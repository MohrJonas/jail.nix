{
  pkgs,
  additionalCombinators ? _: {},
  basePermissions ? import ./base-permissions.nix pkgs,
  bubblewrapPackage ? pkgs.bubblewrap,
}: let
  inherit (pkgs) lib;

  builtinCombinators = (import ./combinators.nix pkgs jail).combinators;

  allCombinators = builtinCombinators // additionalCombinators builtinCombinators;

  normalizePermissionsToList = combinators: let
    t = builtins.typeOf combinators;
  in
    if t == "lambda"
    then combinators allCombinators
    else if t == "list"
    then combinators
    else if t == "null"
    then []
    else throw "Unknown combinator type ${t}. Must be a function, list, or null";

  desktopHelpers = import ./desktop-helpers.nix lib;

  # Extract the path of the .desktop file from the given package.
  # This assumes that there exists one file in <package>/share/applications and the parent folder actually exists
  getDesktopFilePath = package: let
    applicationsPath = "${package}/share/applications";
  in "${applicationsPath}/${builtins.head (builtins.attrNames (builtins.readDir applicationsPath))}";

  # Check whether the package has a .desktop file that needs to be patched
  hasDesktopFile = package: builtins.pathExists "${package}/share/applications";

  extractIcon = import ./extract-icon.nix;

  patchDesktopFile = exe: let
    desktopFilePath = getDesktopFilePath exe;
    desktopFileText = builtins.readFile desktopFilePath;
    desktopFileContent = desktopHelpers.parseDesktopFile desktopFileText;
    icon = extractIcon lib exe;
    executable = lib.getExe exe;
  in
    desktopHelpers.writeDesktopFile (
      lib.pipe desktopFileContent
      [
        (s: s // { "Desktop Entry" = s."Desktop Entry" // { Exec = executable; }; })
        (s: s // { "Desktop Entry" = s."Desktop Entry" // { Icon = icon; }; })
      ]
      desktopFileContent
    );

  jail = name: exe: permissions: let
    initialState = {
      name = name;
      cmd = lib.getExe bubblewrapPackage;
      entry =
        if builtins.typeOf exe == "string"
        then lib.escapeShellArg exe
        else lib.getExe exe;
      path = [];
      argv = "\"$@\"";
      runtime = "";
      newSession = true;
      dieWithParent = true;
      hostname = "jail";
      env = {};
      namespaces = {};
      includedOnce = []; # See include-once combinator
      cleanup = []; # See cleanup combinator
      deferredPermissions = []; # See defer combinator
      additionalRuntimeClosures = []; # See bind-nix-store-runtime-closure
      dbusPermissions = []; # See dbus combinator
      seccompPermissions = []; # See add-seccomp combinator
      inherit initialState; # See reset combinator
    };
  in
    lib.pipe initialState (
      # Permissions shared by all invocations of jail
      (normalizePermissionsToList basePermissions)
      # Permissions for this specific jail
      ++ (normalizePermissionsToList permissions)
      # Permissions wrapped in `defer` combinator
      ++ [(s: builtinCombinators.compose s.deferredPermissions s)]
      # Finalize everything remaining in state into bwrap args
      ++ (with builtinCombinators; [
        (
          s:
          # See `--unshare-*` in BWRAP(1)
            lib.pipe
            ["user" "ipc" "pid" "net" "uts" "cgroup"]
            [
              (lib.filter (ns: !(s.namespaces.${ns} or false)))
              (map (ns: "--unshare-${ns}"))
              (builtins.concatStringsSep " ")
              unsafe-add-raw-args
            ]
            s
        )
        (s:
          if builtins.length s.path > 0
          then set-env "PATH" (lib.concatStringsSep ":" s.path) s
          else s)
        (s:
          if s.newSession
          then unsafe-add-raw-args "--new-session" s
          else s)
        (s:
          if s.dieWithParent
          then unsafe-add-raw-args "--die-with-parent" s
          else s)
        (
          s:
            lib.foldr (
              envVar:
                assert pkgs.lib.isValidPosixName envVar;
                  unsafe-add-raw-args "--setenv ${envVar} ${s.env.${envVar}}"
            )
            s (builtins.attrNames s.env)
        )
      ])
      # Build jailed app from state
      ++ [
        (state: ''
          RUNTIME_ARGS=()
          ${
            if builtins.length state.cleanup > 0
            then ''
              function cleanup {
                ${lib.concatStringsSep "\n" state.cleanup}
              }
              trap cleanup EXIT
            ''
            else ""
          }
          ${state.runtime}
          ${
            if builtins.length state.cleanup > 0
            then ""
            else "exec "
          }${state.cmd} "''${RUNTIME_ARGS[@]}" -- ${state.entry} ${state.argv}
        '')
        (
          text:
            pkgs.writeShellApplication {
              inherit name text;
              runtimeInputs = [pkgs.coreutils];
            }
        )

        # Add additional properties on the jailed derivation
        (
          jailed:
            jailed
            # forward man pages
            // lib.optionalAttrs (exe ? man) {inherit (exe) man;}
            # forward `override`
            // lib.optionalAttrs (exe ? override) {
              override = overrideFn: jail name (exe.override overrideFn) permissions;
            }
        )

        # Construct derivation consisting of shell script and desktop entry
        (
          jailed:
            if hasDesktopFile exe
            then
              pkgs.buildEnv {
                inherit name;
                paths = [
                  jailed
                  (pkgs.writeTextFile {
                    text = patchDesktopFile exe;
                    destination = "/share/applications/${name}";
                  })
                ];
              }
            else jailed
        )
      ]
    );
in {
  combinators = allCombinators;
  __functor = _: jail;
}
