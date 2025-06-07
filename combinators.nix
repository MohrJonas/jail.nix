{ pkgs, lib, helpers }: {
  noescape = {
    sig = "String -> NoEscapedString";
    doc = ''
      Prevent the passed string from being automatically shell escaped.

      `escape` and `noescape` aren't combinators, but they are useful helpers
      to expose when defining jails and writing custom combinators, so they are
      exposed with the rest of the combinators for convenience.

      It is the caller's responsibility to ensure anything passed to this is
      correctly escaped.

      ```nix
      # Probably doesn't do what you intended since "~/foo" is shell escaped:
      (readonly "~/foo")

      # This properly makes $HOME/foo readonly in the jail:
      (readonly (noescape "~/foo"))

      # Binds the path specified by the runtime $FOO variable as read only.
      #
      # Note that we must properly quote this to ensure bash correctly keeps it
      # as a single argument, even if it contains spaces:
      (readonly (noescape "\"$FOO\""))
      ```
    '';
    impl = _:
      value: { _noescape = value; }
    ;
  };

  escape = {
    sig = "String -> String";
    doc = ''
      Shell escapes the passed string.

      Use [noescape](#noescape) to prevent escaping.

      `escape` and `noescape` aren't combinators, but they are useful helpers
      to expose when defining jails and writing custom combinators, so they are
      exposed with the rest of the combinators for convenience.

      Example:
      ```nix
      jail-nix.lib.extend {
        inherit pkgs;
        additionalCombinators = combinators: with combinators; {
          # a combinator that binds the passed path to /foo
          my-combinator = path: unsafe-add-raw-args "--bind ''${escape path} /foo"
        };
      }
      ```
    '';
    impl = _:
      rawOrStr:
        if builtins.typeOf rawOrStr == "set" && rawOrStr ? _noescape
        then rawOrStr._noescape
        else lib.strings.escapeShellArg rawOrStr
    ;
  };

  compose = {
    sig = "[Combinator] -> Combinator";
    doc = ''
      Allows combinator composition.

      `compose [ a b c ]` combines `a`, `b`, and `c`.

      This is useful when writing your own combinators, for example when using
      `jail-nix.lib.extend`:

      ```nix
      jail-nix.lib.extend {
        inherit pkgs;
        additionalCombinators = combinators: with combinators; {
          mycombinator = compose [
            (readonly "/foo")
            (readonly "/bar")
            gpu
          ];
        };
      }
      ```
    '';
    impl = _:
      lib.flip lib.pipe
    ;
  };

  include-once = {
    sig = "String -> Combinator -> Combinator";
    doc = ''
      Only run the passed combinator if include-once hasn't been previously
      called with the specified key.

      This is useful when writing your own combinators.

      ```nix
      let
        jail = jail-nix.lib.extend {
          inherit pkgs;
          additionalCombinators = combinators: with combinators; {
            # foo isn't `include-once` so each call to it adds a new echo
            foo = add-runtime "echo foo";
            # bar will only be included once, no matter how many times it is called
            bar = include-once "bar" (add-runtime "echo bar");
          };
        };
      in
        # Prints:
        # foo
        # foo
        # foo
        # bar
        # Hello, world!
        jail "test" pkgs.hello (c: with c; [
          foo
          foo
          foo
          bar
          bar
          bar
        ])
      ```
    '';
    impl = _:
      key: combinator: state:
        if lib.elem key state.included-once
        then state
        else combinator (state // { included-once = state.included-once ++ [ key ]; })
    ;
  };

  unsafe-add-raw-args = {
    sig = "String -> Combinator";
    doc = ''
      Adds the raw string passed into it into the call to bubblewrap.

      Nothing is escaped, it is the caller's responsibility to ensure
      everything is properly escaped.
    '';
    impl = _:
      args: state: state // { cmd = "${state.cmd} ${args}"; }
    ;
  };

  add-path = {
    sig = "String -> Combinator";
    doc = ''
      Appends the passed string to `$PATH`.
    '';
    impl = _:
      path: state: state // { path = "${state.path}:${path}"; }
    ;
  };

   set-argv = {
    sig = "[String] -> Combinator";
    doc = ''
      Overrides the current argv that is passed to the jailed executable.

      By default argv is set to `noescape "$@"` which will forward whatever
      arguments are provided to the wrapper script at runtime. Calling this
      will override the current value.
    '';
    impl = combinators: with combinators;
      argv: state: state // { argv = builtins.concatStringsSep " " (builtins.map escape argv); }
    ;
  };

  add-runtime = {
    sig = "String -> Combinator";
    doc = ''
      Adds arbitrary logic to run at runtime, before the jail starts.

      This can write to `$RUNTIME_ARGS` to push additional bubblewrap flags
      dependant on runtime conditions.

      Note that anything added here is *not* run inside the jail. To run
      arbitrary things at runtime inside the jail see
      [wrap-entry](#wrap-entry).

      Example:
      ```nix
      add-runtime ${"''"}
        # binds /foo only if /bar exists on the host
        if [ -e /bar ]; then
          RUNTIME_ARGS+=(--bind /foo /foo)
        fi
      ${"''"}
      ```
    '';
    impl = _:
      runtime: state: state // { runtime = "${state.runtime}\n${runtime}\n"; }
    ;
  };

  wrap-entry = {
    sig = "(String -> String) -> Combinator";
    doc = ''
      Wraps the binary to be jailed in a bash script that will be the new
      entrypoint to the jail.

      This similar in spirit to the [add-runtime combinator](#add-runtime),
      except that this runs *inside* the jail, while `add-runtime` runs before
      the jail starts.

      Example:
      ```nix
      wrap-entry (entry: ${"''"}
        echo 'Inside the jail!'
        ''${entry}
        echo 'Cleaning up...'
      ${"''"})
      ```
    '';
    impl = _:
      getWrapper: state: state // {
        entry = lib.getExe (pkgs.writeShellApplication {
          name = "${state.name}-jail-wrapper";
          text = getWrapper state.entry;
        });
      }
    ;
  };

  add-pkg-deps = {
    sig = "[Package] -> Combinator";
    doc = ''
      Adds the packages' `bin` directory to `$PATH`.
    '';
    impl = combinators: with combinators;
      pkgs: compose (builtins.map (pkg: add-path "${lib.getBin pkg}/bin") pkgs)
    ;
  };

  no-new-session = {
    sig = "Combinator";
    doc = ''
      Disables `--new-session`

      By default, jail-nix includes the `--new-session` bwrap flag. Doing this
      prevents a jailed application from being able to feed keyboard input to
      the terminal, however this may break some TUI applications.

      See BWRAP(1) for more information and security implications.
    '';
    impl = _:
      state: state // { new-session = false; }
    ;
  };

  set-env = {
    sig = "String -> String -> Combinator";
    doc = ''
      Sets the specified environment variable in the jail.

      This will throw if the variable name is not a valid posix variable name.
    '';
    impl = combinators: with combinators;
      name: value: state: state // { env = state.env // { ${name} = escape value; }; }
    ;
  };

  share-ns = {
    sig = "String -> Combinator";
    doc = ''
      Removes the call to `--unshare-` for the provided namespace.

      By default, jail-nix unshares all namespaces, calling `share-ns "pid"`
      will remove the `--unshare-pid` flag from bwrap which will allow this
      process to share the same pid namespace as the host.

      See BWRAP(1) for more information.
    '';
    impl = _:
      namespace: state: state // { namespaces = state.namespaces // { ${namespace} = true; }; }
    ;
  };

  set-hostname = {
    sig = "String -> Combinator";
    doc = ''
      Sets the hostname to use for the `network` combinator.

      Must be specified before `network`.

      Example:
      ```nix
      [
        (set-hostname "foo")
        network
      ]
      ```
    '';
    impl = _:
      hostname: state: state // { inherit hostname; }
    ;
  };

  tmpfs = {
    sig = "String -> Combinator";
    doc = ''
      Mounts a new tmpfs at the specified location.
    '';
    impl = combinators: with combinators;
      path: unsafe-add-raw-args "--tmpfs ${escape path}"
    ;
  };

  camera = {
    sig = "Combinator";
    doc = ''
      Allows access to webcams and other V4L2 video devices at `/dev/video*`.
    '';
    impl = combinators: with combinators;
      include-once "camera"
      (add-runtime ''
        for v in /dev/video*; do
          [ -e "$v" ] || continue
          RUNTIME_ARGS+=(--dev-bind "$v" "$v")
        done
      '')
    ;
  };

  fwd-env = {
    sig = "String -> Combinator";
    doc = ''
      Forwards the specified environment variable to the underlying process.

      If the env var is not set when the jailed application is run, it will
      exit non-zero.

      If you want to be tolerant of the environment being unset, use
      [try-fwd-env](#try-fwd-env) instead.
    '';
    impl = combinators: with combinators;
      name: set-env name (noescape "\"\$${name}\"")
    ;
  };

  try-fwd-env = {
    sig = "String -> Combinator";
    doc = ''
      Forwards the specified environment variable to the underlying process (if set).
    '';
    impl = combinators: with combinators;
      name: set-env name (noescape "\"\${${name}-}\"")
    ;
  };

  readonly = {
    sig = "String -> Combinator";
    doc = ''
      Binds the specified path in the jail as read-only.
    '';
    impl = combinators: with combinators;
      path: ro-bind path path
    ;
  };

  readwrite = {
    sig = "String -> Combinator";
    doc = ''
      Binds the specified path in the jail as read-write.
    '';
    impl = combinators: with combinators;
      path: rw-bind path path
    ;
  };

  ro-bind = {
    sig = "String -> String -> Combinator";
    doc = ''
      Binds the specified path on the host to a path in the jail as read-only.

      Example:
      ```nix
      # Binds /foo on the host to /bar in the jail
      ro-bind "/foo" "/bar"
      ```
    '';
    impl = combinators: with combinators;
      from: to: unsafe-add-raw-args "--ro-bind ${escape from} ${escape to}"
    ;
  };

  rw-bind = {
    sig = "String -> String -> Combinator";
    doc = ''
      Binds the specified path on the host to a path in the jail as read-write.

      Example:
      ```nix
      # Binds /foo on the host to /bar in the jail
      rw-bind "/foo" "/bar"
      ```
    '';
    impl = combinators: with combinators;
      from: to: unsafe-add-raw-args "--bind ${escape from} ${escape to}"
    ;
  };

  mount-cwd = {
    sig = "Combinator";
    doc = ''
      Bind mounts the runtime working directory as read-write.
    '';
    impl = combinators: with combinators;
      include-once "mount-cwd"
      (unsafe-add-raw-args "--bind \"$PWD\" \"$PWD\"")
    ;
  };

  gui = {
    sig = "Combinator";
    doc = ''
      Exposes everything required to get graphical applications to work.

      This composes [pulse](#pulse), [pipewire](#pipewire),
      [wayland](#wayland), and forwards/binds a few other paths to get fonts
      and cursor to render correctly.
    '';
    impl = combinators: with combinators;
      compose [
        pulse
        pipewire
        wayland
        (fwd-env "XDG_RUNTIME_DIR")
        (readonly (noescape "/etc/fonts"))
        (readonly (noescape "~/.config/dconf"))

        # Cursor
        (fwd-env "XCURSOR_THEME")
        (fwd-env "XCURSOR_PATH")
        (fwd-env "XCURSOR_SIZE")
        (readonly (noescape "/etc/profiles/per-user/\"$USER\"/share/icons")) # TODO - this is from XCURSOR_PATH, maybe readonly these paths?
      ]
    ;
  };

  wayland = {
    sig = "Combinator";
    doc = ''
      Exposes your wayland compositor to the jail.
    '';
    impl = combinators: with combinators;
      compose [
        (fwd-env "WAYLAND_DISPLAY")
        (fwd-env "XDG_RUNTIME_DIR")
        (fwd-env "XDG_SESSION_TYPE")
        (readonly (noescape "\"$XDG_RUNTIME_DIR/$WAYLAND_DISPLAY\""))
      ]
    ;
  };

  xwayland = {
    sig = "Combinator";
    doc = ''
      Safely allow X11 apps to render to a wayland compositor.

      This combinator runs
      [xwayland-satellite](https://github.com/Supreeeme/xwayland-satellite)
      *inside the jail* and only exposes [wayland combinator](#wayland).

      This has the advantage of not allowing multiple jailed X11 applicaitons
      to see each other since each jailed applicaiton gets its own
      xwayland-satelite server.

      However, doing it this way does mean that every jailed applicaiton you
      run with this combinator will spin up its own personal xwayland-satelite
      server, which will consume more resources than having a global one.
    '';
    impl = combinators: with combinators;
      include-once "xwayland"
      (compose [
        wayland
        (set-env "DISPLAY" ":42")
        (wrap-entry (entry: ''
          ${lib.getExe pkgs.xwayland-satellite} :42 &
          ${entry}
        ''))
      ])
    ;
  };

  unsafe-x11 = {
    sig = "Combinator";
    doc = ''
      Exposes X11 to the jailed application.

      Note that applications may be able to break out of the jail because X11
      is not designed to be a security boundary.

      For a safer alternative, consider using the [xwayland](#xwayland)
      combinator inside of a wayland compositor.
    '';
    impl = combinators: with combinators;
      compose [
        (fwd-env "DISPLAY")
        (readwrite "/tmp/.X11-unix")
      ]
    ;
  };

  pulse = {
    sig = "Combinator";
    doc = ''
      Exposes pulseaudio to the jailed application.
    '';
    impl = combinators: with combinators;
      include-once "pulse"
      (compose [
        (fwd-env "XDG_RUNTIME_DIR")
        (try-fwd-env "PULSE_SERVER")
        (unsafe-add-raw-args "--bind-try /run/pulse /run/pulse")
        (unsafe-add-raw-args "--bind-try \"$XDG_RUNTIME_DIR/pulse\" \"$XDG_RUNTIME_DIR/pulse\"")
      ])
    ;
  };

  pipewire = {
    sig = "Combinator";
    doc = ''
      Exposes pipewire to the jailed application.
    '';
    impl = combinators: with combinators;
      include-once "pipewire"
      (compose [
        (fwd-env "XDG_RUNTIME_DIR")
        (unsafe-add-raw-args "--bind-try \"$XDG_RUNTIME_DIR/pipewire-0\" \"$XDG_RUNTIME_DIR/pipewire-0\"")
        (unsafe-add-raw-args "--bind-try /run/pipewire /run/pipewire")
      ])
    ;
  };

  gpu = {
    sig = "Combinator";
    doc = ''
      Exposes the gpu to jailed application.
    '';
    impl = combinators: with combinators;
      include-once "gpu"
      (compose [
        (readwrite (noescape "/run/opengl-driver"))
        (unsafe-add-raw-args "--bind-try /run/opengl-driver-32 /run/opengl-driver-32")
        (readonly (noescape "/sys"))
        (unsafe-add-raw-args "--dev-bind /dev/dri /dev/dri")
      ])
    ;
  };

  time-zone = {
    sig = "Combinator";
    doc = ''
      Exposes your timezone.
    '';
    impl = combinators: with combinators;
      include-once "time-zone"
      (compose [
        (unsafe-add-raw-args "--symlink \"$(readlink /etc/localtime)\" /etc/localtime")
        (readonly "/etc/static/zoneinfo")
        (readonly "/etc/zoneinfo")
      ])
    ;
  };

  network = {
    sig = "Combinator";
    doc = ''
      Grants network access to the jail.

      This also exposes everything required to allow TLS connections.

      You can set your desired hostname with [set-hostname](#set-hostname). The
      default is `jail`.
    '';
    impl = combinators: with combinators;
      include-once "network"
      (state: compose [
        time-zone
        (share-ns "net")
        (readonly "/etc/hosts")
        (readonly "/etc/nsswitch.conf")
        (readonly "/etc/resolv.conf")
        (readonly "/etc/ssl")
        (readonly "/etc/static/nsswitch.conf")
        (readonly "/etc/static/ssl")
        (write-text "/etc/hostname" "${state.hostname}\n")
        (unsafe-add-raw-args "--hostname ${escape state.hostname}")
      ] state)
    ;
  };

  unsafe-dbus = {
    sig = "Combinator";
    doc = ''
      Exposes D-Bus to the jailed program.

      This does no message filtering so it is marked as unsafe. In the future
      this library will include a combinator that uses
      [xdg-dbus-proxy](https://github.com/flatpak/xdg-dbus-proxy) to specify a
      set of allowed messages.
    '';
    impl = combinators: with combinators;
      compose [
        (readonly (noescape "\"$XDG_RUNTIME_DIR/bus\""))
        (set-env "DBUS_SESSION_BUS_ADDRESS" (noescape "\"$DBUS_SESSION_BUS_ADDRESS\""))
      ]
    ;
  };

  bind-pkg = {
    sig = "String -> Package -> Combinator";
    doc = ''
      Bind mounts the passed derivation at a specified location.

      Example:
      ```nix
      bind-pkg "/foo" (pkgs.writeText "foo" "bar")
      ```
    '';
    impl = combinators: with combinators;
      path: pkg: ro-bind (toString pkg) path
    ;
  };

  write-text = {
    sig = "String -> String -> Combinator";
    doc = ''
      Bind mounts a read-only text file at a path.

      Example:
      ```nix
      # This will create a text file in the jail at `/hello.txt`
      write-text "/hello.txt" "Hello, world!"
      ```
    '';
    impl = combinators: with combinators;
      path: contents:
        bind-pkg
          path
          (pkgs.writeText "jail-write-text-${lib.strings.sanitizeDerivationName (escape path)}" contents)
    ;
  };

  persist-home = {
    sig = "String -> Combinator";
    doc = ''
      Persists the home directory across all jails with the specified name.

      This is useful for a lot of software that may want to write arbitrary
      things into your home directory and expect to read them back in a future
      invocation.

      The home directory is persisted in `~/.local/share/jail.nix/home/<name>`.
    '';
    impl = combinators: with combinators;
      name: compose [
        (add-runtime "mkdir -p ${helpers.dataDirSubPath "home/${name}"}")
        (rw-bind (noescape (helpers.dataDirSubPath "home/${name}")) (noescape "~"))
      ]
    ;
  };

  ############################################
  # deprecated
  persisthome = {
    deprecated = true;
    impl = combinators: with combinators;
      name:
        lib.warn "persisthome is deprecated, use persist-home instead. When doing so, rename ~/.local/share/jails/${name} to ${helpers.dataDirSubPath "home/${name}"}"
        (compose [
          (add-runtime "mkdir -p ~/.local/share/jails/${lib.escapeShellArg name}")
          (rw-bind (noescape "~/.local/share/jails/${lib.escapeShellArg name}") (noescape "~"))
        ])
    ;
  };

  dbus-unsafe = {
    deprecated = true;
    impl = combinators:
      lib.warn "dbus-unsafe is deprecated, use unsafe-dbus instead"
      combinators.unsafe-dbus;
  };
}
