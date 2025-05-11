{ pkgs, name, exe, getOpts }: let
  inherit (pkgs) lib;

  exe-str = if builtins.typeOf exe == "string" then lib.escapeShellArg exe else lib.getExe exe;

  combinators = import ./combinators.nix pkgs;

  initial-state = lib.pipe {
    cmd = "${lib.getExe pkgs.bubblewrap}";
    path = "${pkgs.coreutils}/bin";
    argv = "\"$@\"";
    runtime = "";
    new-session = true;
    hostname = "jail";
    env = {};
  } (with combinators; [
    (unsafe-add-raw-args "--proc /proc")
    (unsafe-add-raw-args "--dev /dev")
    (unsafe-add-raw-args "--tmpfs /tmp")
    (unsafe-add-raw-args "--tmpfs ~")
    (unsafe-add-raw-args "--clearenv")
    (unsafe-add-raw-args "--die-with-parent")
    (unsafe-add-raw-args "--unshare-all")
    (readonly "/nix/store")
    (readonly "/bin/sh")
    (fwd-env "LANG")
    (fwd-env "HOME")
    (fwd-env "TERM")

    (add-runtime ''
      if [ ! -e ~/.local/share/jails/passwd ]; then
        echo "root:x:0:0:System administrator:/root:$(which nologin)" > ~/.local/share/jails/passwd
        echo "$USER:x:$(id -u):$(id -g)::$HOME:$(which nologin)" >> ~/.local/share/jails/passwd
      fi
    '')
    (unsafe-add-raw-args "--ro-bind ~/.local/share/jails/passwd /etc/passwd")
  ]);
in lib.pipe combinators [
  # collect opts
  getOpts
  (lib.foldl (acc: el: el acc) initial-state)

  # finalize state
  (with combinators; compose [
    (s: set-env "PATH" s.path s)
    (s: if s.new-session then unsafe-add-raw-args "--new-session" s else s)
    (s: lib.foldr (envVar:
      assert pkgs.lib.isValidPosixName envVar;
      unsafe-add-raw-args "--setenv ${envVar} ${s.env.${envVar}}"
    ) s (builtins.attrNames s.env))
  ])

  # build jailed app from state
  (state: ''
    RUNTIME_ARGS=()
    ${state.runtime}
    exec ${state.cmd} "''${RUNTIME_ARGS[@]}" -- ${exe-str} ${state.argv}
  '')
  (text: pkgs.writeShellApplication { inherit name text; })

  # forward man pages
  (jailed: if exe ? man then jailed // { inherit (exe) man; } else jailed)
]
