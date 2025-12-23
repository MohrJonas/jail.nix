lib: derivation:
let
  # Resolutions of icons to check, from best to worst. Scalable are svgs that will be scaled to the required size
  resolutionsToCheck = ["scalable" "512x512" "256x256" "128x128" "64x64" "32x32" "16x16"];
  # Construct folder paths that might exist
  potentialIconFolders = (builtins.map (folder: "${derivation}/share/icons/hicolor/${folder}/apps") resolutionsToCheck) ++ [ "${derivation}/share/pixmaps" ];
  # Filter out folders that do not exist
  actualIconFolders = builtins.filter (path: builtins.pathExists path) potentialIconFolders;
  icons = lib.flatten (
    builtins.map (path:
        builtins.map
          (name: "${path}/${name}")
          (builtins.attrNames (builtins.readDir path))
    )
    actualIconFolders
  );
  icon = builtins.head icons;
in
  icon
