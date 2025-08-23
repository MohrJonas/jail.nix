{ pkgs
, additionalCombinators ? _: {}
, basePermissions ? import ./base-permissions.nix pkgs
, bubblewrapPackage ? pkgs.bubblewrap
}: let
  inherit (pkgs) lib;

  builtinCombinators = import ./combinators.nix pkgs jail;

  allCombinators = builtinCombinators // additionalCombinators builtinCombinators;

  normalizePermissionsToList = combinators:
    let t = builtins.typeOf combinators; in
      if t == "lambda" then combinators allCombinators else
      if t == "list" then combinators else
      if t == "null" then []
      else throw "Unknown combinator type ${t}. Must be a function, list, or null";

  jail = name: exe: permissions: let
    initialState = {
      name = name;
      cmd = lib.getExe bubblewrapPackage;
      entry = if builtins.typeOf exe == "string" then lib.escapeShellArg exe else lib.getExe exe;
      path = [];
      argv = "\"$@\"";
      runtime = "";
      newSession = true;
      hostname = "jail";
      env = {};
      namespaces = {};
      includedOnce = []; # See include-once combinator
      cleanup = []; # See cleanup combinator
      deferredPermissions = []; # See defer combinator
      additionalRuntimeClosures = []; # See bind-nix-store-runtime-closure
    };
  in lib.pipe initialState (
    # Permissions shared by all invocations of jail
    (normalizePermissionsToList basePermissions)

    # Permissions for this specific jail
    ++ (normalizePermissionsToList permissions)

    # Permissions wrapped in `defer` combinator
    ++ [(s: builtinCombinators.compose s.deferredPermissions s)]

    # Finalize everything remaining in state into bwrap args
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
      (s: if builtins.length s.path > 0 then set-env "PATH" (lib.concatStringsSep ":" s.path) s else s)
      (s: if s.newSession then unsafe-add-raw-args "--new-session" s else s)
      (s: lib.foldr (envVar:
        assert pkgs.lib.isValidPosixName envVar;
        unsafe-add-raw-args "--setenv ${envVar} ${s.env.${envVar}}"
      ) s (builtins.attrNames s.env))
    ])

    # Build jailed app from state
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

in {
  combinators = allCombinators;
  __functor = _: jail;
}
