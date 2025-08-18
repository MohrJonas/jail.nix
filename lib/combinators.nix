pkgs: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix pkgs;
in rec {
  noescape = {
    sig = "String -> NoEscapedString";
    doc = ''
      Prevent the passed string from being automatically shell escaped.

      `escape` and `noescape` don't return `Permission`s, but they are useful
      helpers to expose when defining jails and writing custom combinators, so
      they are exposed with the rest of the combinators for convenience.

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
    __functor = _: helpers.noescape;
  };

  escape = {
    sig = "String -> String";
    doc = ''
      Shell escapes the passed string.

      Use [noescape](#noescape) to prevent escaping.

      `escape` and `noescape` don't return `Permission`s, but they are useful
      helpers to expose when defining jails and writing custom combinators, so
      they are exposed with the rest of the combinators for convenience.

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
    __functor = _: helpers.escape;
  };

  compose = {
    sig = "[Permission] -> Permission";
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
    __functor = _:
      lib.flip lib.pipe
    ;
  };

  include-once = {
    sig = "String -> Permission -> Permission";
    doc = ''
      Only run the passed permission if include-once hasn't been previously
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
    __functor = _:
      key: combinator: state:
        if lib.elem key state.includedOnce
        then state
        else combinator (state // { includedOnce = state.includedOnce ++ [ key ]; })
    ;
  };

  unsafe-add-raw-args = {
    sig = "String -> Permission";
    doc = ''
      Adds the raw string passed into it into the call to bubblewrap.

      Nothing is escaped, it is the caller's responsibility to ensure
      everything is properly escaped.
    '';
    __functor = _:
      args: state: state // { cmd = "${state.cmd} ${args}"; }
    ;
  };

  add-path = {
    sig = "String -> Permission";
    doc = ''
      Prepends the passed string to `$PATH`.
    '';
    __functor = _:
      path: state: state // { path = [ path ] ++ state.path; }
    ;
  };

   set-argv = {
    sig = "[String] -> Permission";
    doc = ''
      Overrides the current argv that is passed to the jailed executable.

      By default argv is set to `noescape "$@"` which will forward whatever
      arguments are provided to the wrapper script at runtime. Calling this
      will override the current value.
    '';
    __functor = _:
      argv: state: state // { argv = builtins.concatStringsSep " " (builtins.map escape argv); }
    ;
  };

  add-runtime = {
    sig = "String -> Permission";
    doc = ''
      Adds arbitrary logic to run at runtime, before the jail starts.

      You can push additional bubblewrap arguments by appending the bash
      array `$RUNTIME_ARGS`. This allows you to modify the bubblewrap flags
      to be dependent on runtime conditions.

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

      If you create any resources in add-runtime that you want to automatically
      clean up when the jail exits use [add-cleanup](#add-cleanup).
    '';
    __functor = _:
      runtime: state: state // { runtime = "${state.runtime}\n${runtime}\n"; }
    ;
  };

  add-cleanup = {
    sig = "String -> Permission";
    doc = ''
      Adds arbitrary logic to run when the jail exits.

      This is designed to be an easy way to register cleanup actions for things
      created in [add-runtime](#add-runtime). These scripts run in the same
      scope as `add-runtime` so any shell variables defined there will be in
      scope.

      The cleanup actions may run even if the runtime doesn't— for example if a
      previous runtime exits non-zero the jail will exit prematurely, but the
      cleanup actions will still run.

      Example:
      ```nix
      compose [
        (add-runtime ${"''"}
          TMP_FILE=$(mktemp)
          do-something "$TMP_FILE"
        ${"''"})
        (add-cleanup ${"''"}
          if [ -e "''${TMP_FILE-}" ]; then
            rm "$TMP_FILE"
          fi
        ${"''"})
      ]
      ```
    '';
    __functor = _:
      cleanup: state: state // { cleanup = state.cleanup ++ [ cleanup ]; }
    ;
  };

  fake-passwd = {
    sig = "Permission";
    includedInBasePermissions = true;
    doc = ''
      Generates and mounts  fake `/etc/passwd` and `/etc/group` files in the jail.

     The fake `/etc/passwd` and `/etc/group` files contains a root user, and
     forward the calling user's user id, username, group id and group name.

     If you do not want to hide the users and groups that exist on your system,
     you may consider just bind mounting `/etc/passwd` and `/etc/group` inside
     the jail instead.
    '';
    __functor = _:
      compose [
        (add-runtime ''
          if [ ! -e ${helpers.dataDirSubPath "passwd"} ] || [ ! -e ${helpers.dataDirSubPath "group"} ]; then
            NOLOGIN=${pkgs.shadow}/bin/nologin
            mkdir -p ${helpers.dataDir}
            echo "root:x:0:0:System administrator:/root:$NOLOGIN" > ${helpers.dataDirSubPath "passwd"}
            echo "$(id -un):x:$(id -u):$(id -g)::$HOME:$NOLOGIN" >> ${helpers.dataDirSubPath "passwd"}
            echo "root:x:0:" > ${helpers.dataDirSubPath "group"}
            echo "$(id -gn):x:$(id -g):" >> ${helpers.dataDirSubPath "group"}
          fi
        '')
        (ro-bind (noescape (helpers.dataDirSubPath "passwd")) "/etc/passwd")
        (ro-bind (noescape (helpers.dataDirSubPath "group")) "/etc/group")
      ]
    ;
  };

  wrap-entry = {
    sig = "(String -> String) -> Permission";
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
    __functor = _:
      getWrapper: state: state // {
        entry = lib.getExe (pkgs.writeShellApplication {
          name = "${state.name}-jail-wrapper";
          text = getWrapper state.entry;
        });
      }
    ;
  };

  add-pkg-deps = {
    sig = "[Package] -> Permission";
    doc = ''
      Adds the packages' `bin` directory to `$PATH`.
    '';
    __functor = _:
      pkgs: compose (builtins.map (pkg: add-path "${lib.getBin pkg}/bin") pkgs)
    ;
  };

  no-new-session = {
    sig = "Permission";
    doc = ''
      Disables `--new-session`

      By default, jail-nix includes the `--new-session` bwrap flag. Doing this
      prevents a jailed application from being able to feed keyboard input to
      the terminal, however this may break some TUI applications.

      See BWRAP(1) for more information and security implications.
    '';
    __functor = _:
      state: state // { newSession = false; }
    ;
  };

  set-env = {
    sig = "String -> String -> Permission";
    doc = ''
      Sets the specified environment variable in the jail.

      This will throw if the variable name is not a valid posix variable name.
    '';
    __functor = _:
      name: value: state: state // { env = state.env // { ${name} = escape value; }; }
    ;
  };

  share-ns = {
    sig = "String -> Permission";
    doc = ''
      Removes the call to `--unshare-` for the provided namespace.

      By default, jail-nix unshares all namespaces, calling `share-ns "pid"`
      will remove the `--unshare-pid` flag from bwrap which will allow this
      process to share the same pid namespace as the host.

      See BWRAP(1) for more information.
    '';
    __functor = _:
      namespace: state: state // { namespaces = state.namespaces // { ${namespace} = true; }; }
    ;
  };

  set-hostname = {
    sig = "String -> Permission";
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
    __functor = _:
      hostname: state: state // { inherit hostname; }
    ;
  };

  tmpfs = {
    sig = "String -> Permission";
    doc = ''
      Mounts a new tmpfs at the specified location.
    '';
    __functor = _:
      path: unsafe-add-raw-args "--tmpfs ${escape path}"
    ;
  };

  camera = {
    sig = "Permission";
    doc = ''
      Allows access to webcams and other V4L2 video devices at `/dev/video*`.
    '';
    __functor = _:
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
    sig = "String -> Permission";
    doc = ''
      Forwards the specified environment variable to the underlying process.

      If the env var is not set when the jailed application is run, it will
      exit non-zero.

      If you want to be tolerant of the environment being unset, use
      [try-fwd-env](#try-fwd-env) instead.
    '';
    __functor = _:
      name: set-env name (noescape "\"\$${name}\"")
    ;
  };

  try-fwd-env = {
    sig = "String -> Permission";
    doc = ''
      Forwards the specified environment variable to the underlying process (if set).
    '';
    __functor = _:
      name: set-env name (noescape "\"\${${name}-}\"")
    ;
  };

  readonly = {
    sig = "String -> Permission";
    doc = ''
      Binds the specified path in the jail as read-only.
    '';
    __functor = _:
      path: ro-bind path path
    ;
  };

  readwrite = {
    sig = "String -> Permission";
    doc = ''
      Binds the specified path in the jail as read-write.
    '';
    __functor = _:
      path: rw-bind path path
    ;
  };

  ro-bind = {
    sig = "String -> String -> Permission";
    doc = ''
      Binds the specified path on the host to a path in the jail as read-only.

      Example:
      ```nix
      # Binds /foo on the host to /bar in the jail
      ro-bind "/foo" "/bar"
      ```
    '';
    __functor = _:
      from: to: unsafe-add-raw-args "--ro-bind ${escape from} ${escape to}"
    ;
  };

  rw-bind = {
    sig = "String -> String -> Permission";
    doc = ''
      Binds the specified path on the host to a path in the jail as read-write.

      Example:
      ```nix
      # Binds /foo on the host to /bar in the jail
      rw-bind "/foo" "/bar"
      ```
    '';
    __functor = _:
      from: to: unsafe-add-raw-args "--bind ${escape from} ${escape to}"
    ;
  };

  readonly-runtime-args = {
    sig = "Permission";
    doc = ''
      Binds any valid paths passed in as arguments to the jailed program at
      runtime as read-only.
    '';
    __functor = _:
      include-once "readonly-runtime-args"
      (add-runtime ''
        for MAYBE_PATH in "$@"; do
          if [ -e "$MAYBE_PATH" ]; then
            P="$(realpath "$MAYBE_PATH")"
            RUNTIME_ARGS+=(--ro-bind "$P" "$P")
          fi
        done
      '')
    ;
  };

  readwrite-runtime-args = {
    sig = "Permission";
    doc = ''
      Binds any valid paths passed in as arguments to the jailed program at
      runtime as read-write.
    '';
    __functor = _:
      include-once "readwrite-runtime-args"
      (add-runtime ''
        for MAYBE_PATH in "$@"; do
          if [ -e "$MAYBE_PATH" ]; then
            P="$(realpath "$MAYBE_PATH")"
            RUNTIME_ARGS+=(--bind "$P" "$P")
          fi
        done
      '')
    ;
  };

  readonly-paths-from-var = {
    sig = "String -> String -> Permission";
    doc  = ''
      This binds multiple paths as read-only specified by a single runtime
      environment variable.

      The first argument to this combinator is the runtime environment variable
      that contains a list of paths to be bound. The second argument is a
      deliminator to split the paths (Typically either `" "` or `":"`).

      This is useful for variables like `XDG_DATA_DIRS`, `GTK_PATH`,
      `XCURSOR_PATH`, etc.

      Example:
      ```nix
      compose [
        (readonly-paths-from-var "XDG_DATA_DIRS" ":")
        (readonly-paths-from-var "XCURSOR_PATH" " ")
      ]
      ```
    '';
    __functor = _:
      var: separator:
        assert pkgs.lib.isValidPosixName var;
        include-once "readonly-paths-from-var-${var}"
        (add-runtime ''
          while read -rd${lib.escapeShellArg separator} DIR; do
            if [ -e "$DIR" ]; then
              P="$(realpath "$DIR")"
              RUNTIME_ARGS+=(--ro-bind "$P" "$P")
            fi
          done <<< "''${${var}-}"
        '')
    ;
  };

  mount-cwd = {
    sig = "Permission";
    doc = ''
      Bind mounts the runtime working directory as read-write.
    '';
    __functor = _:
      include-once "mount-cwd"
      (unsafe-add-raw-args "--bind \"$PWD\" \"$PWD\"")
    ;
  };

  gui = {
    sig = "Permission";
    doc = ''
      Exposes everything required to get graphical applications to work.

      This composes [pulse](#pulse), [pipewire](#pipewire),
      [wayland](#wayland), and forwards/binds a few other paths to get fonts
      and cursor to render correctly.
    '';
    __functor = _:
      compose [
        (add-runtime "mkdir -p ~/.config/dconf")
        pulse
        pipewire
        wayland
        (readonly (noescape "/etc/fonts"))
        (readonly (noescape "~/.config/dconf"))
        (fwd-env "XDG_RUNTIME_DIR")
        (fwd-env "XDG_DATA_DIRS")
        (readonly-paths-from-var "XDG_DATA_DIRS" ":")

        # Cursor
        (try-fwd-env "XCURSOR_THEME")
        (try-fwd-env "XCURSOR_PATH")
        (try-fwd-env "XCURSOR_SIZE")
        (readonly-paths-from-var "XCURSOR_PATH" " ")
      ]
    ;
  };

  wayland = {
    sig = "Permission";
    doc = ''
      Exposes your wayland compositor to the jail.
    '';
    __functor = _:
      compose [
        (fwd-env "WAYLAND_DISPLAY")
        (fwd-env "XDG_RUNTIME_DIR")
        (fwd-env "XDG_SESSION_TYPE")
        (readonly (noescape "\"$XDG_RUNTIME_DIR/$WAYLAND_DISPLAY\""))
      ]
    ;
  };

  xwayland = {
    sig = "Permission";
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
    __functor = _:
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
    sig = "Permission";
    doc = ''
      Exposes X11 to the jailed application.

      Note that applications may be able to break out of the jail because X11
      is not designed to be a security boundary.

      For a safer alternative, consider using the [xwayland](#xwayland)
      combinator inside of a wayland compositor.
    '';
    __functor = _:
      compose [
        (fwd-env "DISPLAY")
        (readwrite "/tmp/.X11-unix")
      ]
    ;
  };

  pulse = {
    sig = "Permission";
    doc = ''
      Exposes pulseaudio to the jailed application.
    '';
    __functor = _:
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
    sig = "Permission";
    doc = ''
      Exposes pipewire to the jailed application.
    '';
    __functor = _:
      include-once "pipewire"
      (compose [
        (fwd-env "XDG_RUNTIME_DIR")
        (unsafe-add-raw-args "--bind-try \"$XDG_RUNTIME_DIR/pipewire-0\" \"$XDG_RUNTIME_DIR/pipewire-0\"")
        (unsafe-add-raw-args "--bind-try /run/pipewire /run/pipewire")
      ])
    ;
  };

  gpu = {
    sig = "Permission";
    doc = ''
      Exposes the gpu to jailed application.
    '';
    __functor = _:
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
    sig = "Permission";
    doc = ''
      Exposes your timezone.
    '';
    __functor = _:
      include-once "time-zone"
      (compose [
        (unsafe-add-raw-args "--symlink \"$(readlink /etc/localtime)\" /etc/localtime")
        (readonly "/etc/static/zoneinfo")
        (readonly "/etc/zoneinfo")
      ])
    ;
  };

  network = {
    sig = "Permission";
    doc = ''
      Grants network access to the jail.

      This also exposes everything required to allow TLS connections.

      You can set your desired hostname with [set-hostname](#set-hostname). The
      default is `jail`.
    '';
    __functor = _:
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
    sig = "Permission";
    doc = ''
      Exposes D-Bus to the jailed program.

      This does no message filtering so it is marked as unsafe. In the future
      this library will include a combinator that uses
      [xdg-dbus-proxy](https://github.com/flatpak/xdg-dbus-proxy) to specify a
      set of allowed messages.
    '';
    __functor = _:
      compose [
        (readonly (noescape "\"$XDG_RUNTIME_DIR/bus\""))
        (set-env "DBUS_SESSION_BUS_ADDRESS" (noescape "\"$DBUS_SESSION_BUS_ADDRESS\""))
      ]
    ;
  };

  bind-pkg = {
    sig = "String -> Package -> Permission";
    doc = ''
      Bind mounts the passed derivation at a specified location.

      Example:
      ```nix
      bind-pkg "/foo" (pkgs.writeText "foo" "bar")
      ```
    '';
    __functor = _:
      path: pkg: ro-bind (toString pkg) path
    ;
  };

  write-text = {
    sig = "String -> String -> Permission";
    doc = ''
      Bind mounts a read-only text file at a path.

      Example:
      ```nix
      # This will create a text file in the jail at `/hello.txt`
      write-text "/hello.txt" "Hello, world!"
      ```
    '';
    __functor = _:
      path: contents:
        bind-pkg
          path
          (pkgs.writeText "jail-write-text-${lib.strings.sanitizeDerivationName (escape path)}" contents)
    ;
  };

  persist-home = {
    sig = "String -> Permission";
    doc = ''
      Persists the home directory across all jails with the specified name.

      This is useful for a lot of software that may want to write arbitrary
      things into your home directory and expect to read them back in a future
      invocation.

      The home directory is persisted in `~/.local/share/jail.nix/home/<name>`.
    '';
    __functor = _:
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
    sig = "Permission";
    doc = "This was reworked to store data under `~/.local/share/jail.nix` and renamed to [persist-home](#persist-home).";
    __functor = _:
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
    sig = "Permission";
    doc = "This was renamed to [unsafe-dbus](#unsafe-dbus).";
    __functor = _:
      lib.warn "dbus-unsafe is deprecated, use unsafe-dbus instead"
      unsafe-dbus;
  };
}
