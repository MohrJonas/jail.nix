let
  jailNix = import ./jail.nix;
in {
  init = pkgs: jailNix { inherit pkgs; };
  extend = jailNix;
}
