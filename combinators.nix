{ pkgs, lib, helpers }: let
  inherit (helpers) escape noescape;
in rec {
  # noescape isn't a combinator, but it's a useful helper to expose when defining jails
  inherit noescape;

  compose = helpers: state: lib.pipe state helpers;

  unsafe-add-raw-args = args: state: state // { cmd = "${state.cmd} ${args}"; };
  add-path = path: state: state // { path = "${state.path}:${path}"; };
  set-argv = argv: state: state // { argv = builtins.concatStringsSep " " (builtins.map escape argv); };
  add-runtime = runtime: state: state // { runtime = "${state.runtime}\n${runtime}\n"; };
  add-pkg-deps = pkgs: compose (builtins.map (pkg: add-path "${pkg}/bin") pkgs);
  no-new-session = state: state // { new-session = false; };
  set-env = name: value: state: state // { env = state.env // { ${name} = escape value; }; };
  share-ns = namespace: state: state // { namespaces = state.namespaces // { ${namespace} = true; }; };

  set-hostname = hostname: state: state // { inherit hostname; };

  tmpfs = path: unsafe-add-raw-args "--tmpfs ${escape path}";
  camera = unsafe-add-raw-args "--dev-bind /dev/video0 /dev/video0";

  # safety: jail/lib.nix asserts that name is a valid shell variable name
  fwd-env = name: set-env name (noescape "\"\$${name}\"");
  try-fwd-env = name: set-env name (noescape "\"\${${name}-}\"");

  readonly = path: ro-bind path path;
  readwrite = path: rw-bind path path;
  ro-bind = from: to: unsafe-add-raw-args "--ro-bind ${escape from} ${escape to}";
  rw-bind = from: to: unsafe-add-raw-args "--bind ${escape from} ${escape to}";
  mount-cwd = unsafe-add-raw-args "--bind \"$PWD\" \"$PWD\"";
  gui = compose [
    (readonly (noescape "/etc/fonts"))
    (readonly (noescape "\"$XDG_RUNTIME_DIR/$WAYLAND_DISPLAY\""))
    (readwrite (noescape "\"$XDG_RUNTIME_DIR/pulse\""))
    (readonly (noescape "~/.config/dconf"))
    (try-fwd-env "DISPLAY")
    (fwd-env "WAYLAND_DISPLAY")
    (fwd-env "XDG_RUNTIME_DIR")
    (fwd-env "XDG_SESSION_TYPE")

    # Cursor
    (fwd-env "XCURSOR_THEME")
    (fwd-env "XCURSOR_PATH")
    (fwd-env "XCURSOR_SIZE")
    (readonly (noescape "/etc/profiles/per-user/\"$USER\"/share/icons")) # TODO - this is from XCURSOR_PATH, maybe readonly these paths?
  ];
  pipewire = compose [
    # TODO - fwd-env XDG_RUNTIME_DIR but first it should update a record in state instead of appending args
    (unsafe-add-raw-args "--bind-try \"$XDG_RUNTIME_DIR/pipewire-0\" \"$XDG_RUNTIME_DIR/pipewire-0\"")
    (unsafe-add-raw-args "--bind-try /run/pipewire /run/pipewire")
  ];
  gpu = compose [
    (readwrite (noescape "/run/opengl-driver"))
    (unsafe-add-raw-args "--bind-try /run/opengl-driver-32 /run/opengl-driver-32")
    (readonly (noescape "/sys"))
    (unsafe-add-raw-args "--dev-bind /dev/dri /dev/dri")
  ];
  time-zone = compose [
    (unsafe-add-raw-args "--symlink \"$(readlink /etc/localtime)\" /etc/localtime")
    (readonly "/etc/static/zoneinfo")
    (readonly "/etc/zoneinfo")
  ];
  network = state: compose [
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
  ] state;
  dbus-unsafe = compose [
    (readonly (noescape "\"$XDG_RUNTIME_DIR/bus\""))
    (set-env "DBUS_SESSION_BUS_ADDRESS" (noescape "\"$DBUS_SESSION_BUS_ADDRESS\""))
  ];
  bind-pkg = path: pkg: ro-bind (toString pkg) path;
  write-text = path: contents:
    bind-pkg
      path
      (pkgs.writeText "jail-write-text-${lib.strings.sanitizeDerivationName (escape path)}" contents);

  persist-home = name: compose [
    (add-runtime "mkdir -p ${helpers.dataDirSubPath "home/${name}"}")
    (rw-bind (noescape (helpers.dataDirSubPath "home/${name}")) (noescape "~"))
  ];

  ############################################
  # deprecated
  persisthome = name: helpers.deprecatedCombinator
    "persisthome is deprecated, use persist-home instead. When doing so, rename ~/.local/share/jails/${name} to ${helpers.dataDirSubPath "home/${name}"}"
    (compose [
      (add-runtime "mkdir -p ~/.local/share/jails/${lib.escapeShellArg name}")
      (rw-bind (noescape "~/.local/share/jails/${lib.escapeShellArg name}") (noescape "~"))
    ]);
}
