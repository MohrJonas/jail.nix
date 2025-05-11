{
  outputs = _: {
    lib = let
      runJail = import ./lib.nix;
    in {
      init = pkgs: name: exe: getOpts: runJail {
        inherit pkgs name exe getOpts;
      };

      extend = { pkgs, additionalCombinators }: name: exe: getOpts: runJail {
        inherit pkgs name exe;
        getOpts = combinators: getOpts (combinators // additionalCombinators combinators);
      };
    };
  };
}
