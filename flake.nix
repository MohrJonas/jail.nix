{
  outputs = _: {
    lib = let
      jailLib = import ./lib.nix;
    in {
      init = pkgs: jailLib { inherit pkgs; };
      extend = jailLib;
    };
  };
}
