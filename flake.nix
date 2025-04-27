{
  outputs = _: {
    lib = let
      runJail = import ./lib.nix;
    in {
      init = pkgs: name: exe: getOpts: runJail {
        inherit pkgs name exe getOpts;
      };

      extend = { pkgs, additionalHelpers }: name: exe: getOpts: runJail {
        inherit pkgs name exe;
        getOpts = helpers: getOpts (helpers // additionalHelpers helpers);
      };
    };
  };
}
