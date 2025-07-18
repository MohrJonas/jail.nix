{ pkgs
, additionalCombinators ? _: {}
}: let
  inherit (pkgs) lib;

  helpers = rec {
    dataDir = "~/.local/share/jail.nix";
    dataDirSubPath = subPath: "${dataDir}/${builtinCombinators.escape subPath}";
  };

  builtinCombinators = import ./combinators.nix { inherit pkgs lib helpers; };

  allCombinators = builtinCombinators // additionalCombinators builtinCombinators;

in {
  combinators = allCombinators;

  __functor = _: name: exe: combinatorsToApply: let
    initialState = {
      name = name;
      cmd = "${lib.getExe pkgs.bubblewrap}";
      entry = if builtins.typeOf exe == "string" then lib.escapeShellArg exe else lib.getExe exe;
      path = "${pkgs.coreutils}/bin";
      argv = "\"$@\"";
      runtime = "";
      new-session = true;
      hostname = "jail";
      env = {};
      namespaces = {};
      included-once = []; # See include-once combinator
      cleanup = []; # See cleanup combinator
    };

    userCombinators =
      let t = builtins.typeOf combinatorsToApply; in
        if t == "lambda" then combinatorsToApply allCombinators else
        if t == "list" then combinatorsToApply else
        if t == "null" then []
        else throw "Unknown combinator type ${t}. Must be a function, list, or null";

  in lib.pipe initialState (
    # apply pre-user combinators
    (with builtinCombinators; [
      (unsafe-add-raw-args "--proc /proc")
      (unsafe-add-raw-args "--dev /dev")
      (unsafe-add-raw-args "--tmpfs /tmp")
      (unsafe-add-raw-args "--tmpfs ~")
      (unsafe-add-raw-args "--clearenv")
      (unsafe-add-raw-args "--die-with-parent")
      (readonly "/nix/store")
      (readonly "/bin/sh")
      (fwd-env "LANG")
      (fwd-env "HOME")
      (fwd-env "TERM")

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
    ])

    # apply user combinators
    ++ userCombinators

    # apply post-user combinators
    ++ (with builtinCombinators; [
      (s:
        # See `--unshare-*` in BWRAP(1)
        lib.pipe ["user" "ipc" "pid" "net" "uts" "cgroup"] [
          (lib.filter (ns: !(s.namespaces.${ns} or false)))
          (map (ns: "--unshare-${ns}"))
          (builtins.concatStringsSep " ")
          unsafe-add-raw-args
        ] s
      )
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
        ${if builtins.length state.cleanup > 0 then ''
          function cleanup {
            ${lib.concatStringsSep "\n" state.cleanup}
          }
          trap cleanup EXIT
        '' else ""}
        ${state.runtime}
        ${if builtins.length state.cleanup > 0 then "" else "exec "}${state.cmd} "''${RUNTIME_ARGS[@]}" -- ${state.entry} ${state.argv}
      '')
      (text: pkgs.writeShellApplication { inherit name text; })

      # forward man pages
      (jailed: if exe ? man then jailed // { inherit (exe) man; } else jailed)
    ]
  );
}
