{
  outputs = _: {
    lib = let
      runJail = import ./lib.nix;
    in {
      init = pkgs: name: exe: applyCombinators: runJail {
        inherit pkgs name exe applyCombinators;
      };

      extend = { pkgs, additionalCombinators }: name: exe: applyCombinators: runJail {
        inherit pkgs name exe;
        applyCombinators = combinators: applyCombinators (combinators // additionalCombinators combinators);
      };
    };
  };
}
