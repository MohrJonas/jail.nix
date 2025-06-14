{
  # This is a separate flake file from the one at the root because we don't
  # want to depend on nixpkgs
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

  outputs = { self, nixpkgs, ... }: let
    pkgs = import nixpkgs { system = "x86_64-linux"; };
    lib = pkgs.lib;

    formatCombinatorDoc = name: combinatorObj: let
      attrPos = builtins.unsafeGetAttrPos "__functor" combinatorObj;
      file = lib.removePrefix (toString ./..) attrPos.file;
    in ''
      ## ${name}
      **${name} :: ${combinatorObj.sig}**

      ${if self ? rev then "[Source](https://git.sr.ht/~alexdavid/jail.nix/tree/${self.rev}${file}#L${toString attrPos.line})" else ""}

      ${combinatorObj.doc}

      ---
    '';

    combinatorDocs = lib.pipe
      {
        pkgs = throw "Docs must not depend on pkgs";
        lib = throw "Docs must not depend on lib";
        helpers = throw "Docs must not depend on helpers";
      }
      [
        (import ../combinators.nix)
        (lib.filterAttrs (_: v: !(v ? deprecated && v.deprecated)))
        (lib.mapAttrsToList formatCombinatorDoc)
        (lib.concatStringsSep "\n\n")
        (docs: "# Combinators\n\n${docs}")
      ];

    mkdocsSettings = {
      site_name = "jail-nix";
      repo_url = "https://git.sr.ht/~alexdavid/jail.nix";
      theme = {
        name = "readthedocs";
        hljs_languages = [ "nix" ];
      };
    };
  in {
    packages.x86_64-linux.default = pkgs.runCommand "website" { buildInputs = [ pkgs.mkdocs ]; } ''
      mkdir -p docs
      cp ${../README.md} docs/index.md
      echo ${lib.escapeShellArg combinatorDocs} > docs/combinators.md
      echo ${lib.escapeShellArg (builtins.toJSON mkdocsSettings)} > mkdocs.yml
      mkdocs build
      mv site $out
    '';
  };
}
