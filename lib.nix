{ pkgs, name, exe, getOpts }: let
  inherit (pkgs) lib;

  exe-str = if builtins.typeOf exe == "string" then lib.escapeShellArg exe else lib.getExe exe;

  helpers = rec {
    escape = rawOrStr: if builtins.typeOf rawOrStr == "string" then lib.strings.escapeShellArg rawOrStr else rawOrStr.raw;
    noescape = value: { raw = value; };
    dataDir = "~/.local/share/jail.nix";
    dataDirSubPath = subPath: "${dataDir}/${escape subPath}";
    deprecatedCombinator = message: combinator: lib.warn "jail ${name}: ${message}" combinator;
  };

  combinators = import ./combinators.nix {
    inherit pkgs lib helpers;
  };

  initial-state = {
    cmd = "${lib.getExe pkgs.bubblewrap}";
    path = "${pkgs.coreutils}/bin";
    argv = "\"$@\"";
    runtime = "";
    new-session = true;
    hostname = "jail";
    env = {};
  };
in lib.pipe initial-state (
  # apply pre-user combinators
  (with combinators; [
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
     if [ ! -e ${helpers.dataDirSubPath "passwd"} ] || [ ! -e ${helpers.dataDirSubPath "group"} ]; then
     mkdir -p ${helpers.dataDir}
     echo "root:x:0:0:System administrator:/root:$(which nologin)" > ${helpers.dataDirSubPath "passwd"}
     echo "$(id -un):x:$(id -u):$(id -g)::$HOME:$(which nologin)" >> ${helpers.dataDirSubPath "passwd"}
     echo "root:x:0:" > ${helpers.dataDirSubPath "group"}
     echo "$(id -gn):x:$(id -g):" >> ${helpers.dataDirSubPath "group"}
     fi
     '')
    (ro-bind (noescape (helpers.dataDirSubPath "passwd")) "/etc/passwd")
    (ro-bind (noescape (helpers.dataDirSubPath "group")) "/etc/group")
  ])

  # apply user combinators
  ++ getOpts combinators

  # apply post-user combinators
  ++ (with combinators; [
    (s: set-env "PATH" s.path s)
    (s: if s.new-session then unsafe-add-raw-args "--new-session" s else s)
    (s: lib.foldr (envVar:
      assert pkgs.lib.isValidPosixName envVar;
      unsafe-add-raw-args "--setenv ${envVar} ${s.env.${envVar}}"
    ) s (builtins.attrNames s.env))
  ])

  # build jailed app from state
  ++ [
    (state: ''
      RUNTIME_ARGS=()
      ${state.runtime}
      exec ${state.cmd} "''${RUNTIME_ARGS[@]}" -- ${exe-str} ${state.argv}
    '')
    (text: pkgs.writeShellApplication { inherit name text; })

    # forward man pages
    (jailed: if exe ? man then jailed // { inherit (exe) man; } else jailed)
  ]
)
