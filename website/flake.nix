{
  # This is a separate flake file from the one at the root because we don't
  # want to depend on nixpkgs
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

  outputs = { self, nixpkgs, ... }: let
    pkgs = import nixpkgs { system = "x86_64-linux"; };
    lib = pkgs.lib;

    repo = rec {
      rev = if self ? rev then self.rev else lib.warn "Repo is dirty, links will not work!" "???";
      shortRev = lib.substring 0 8 rev;
      urls = {
        base = "https://git.sr.ht/~alexdavid/jail.nix";
        commit = "https://git.sr.ht/~alexdavid/jail.nix/commit/${rev}";
        file = file: lineNr: "https://git.sr.ht/~alexdavid/jail.nix/tree/${rev}${file}#L${toString lineNr}";
      };
    };

    formatCombinatorDoc = name: combinatorObj: let
      attrPos = builtins.unsafeGetAttrPos "__functor" combinatorObj;
      file = lib.removePrefix (toString ./..) attrPos.file;
    in ''
      ### ${name}
      **${name} :: ${combinatorObj.sig}**

      [Source](${repo.urls.file file attrPos.line})

      ${combinatorObj.doc}
    '';

    combinatorDocs = let
      allCombinators = import ../lib/combinators.nix {
        pkgs = throw "Docs must not depend on pkgs";
        lib = throw "Docs must not depend on lib";
        helpers = throw "Docs must not depend on helpers";
      };

      formatSection = filter:
        lib.pipe allCombinators [
          (lib.filterAttrs (_: filter))
          (lib.mapAttrsToList formatCombinatorDoc)
          (lib.concatStringsSep "\n\n---\n")
        ];

      hr = ''<hr style="border: 2px solid #272525">'';
    in ''
      # Combinators

      jail.nix combinators are the building blocks to create `Permission`s,
      which grant a program specific permissions at runtime.

      These permissions can be passed into the third argument to `jail`
      funciton, as well as
      [`basePermissions`](advanced-configuration.md#basepermissions).

      ${formatSection (v: !(v ? deprecated || v ? includedInBasePermissions))}

      ${hr}

      ## Default Included Combinators

      The following combinators are enabled by default, and do not need to be
      explicitly added to your jails unless you [override
      `basePermissions`](advanced-configuration.md#basepermissions).

      ${formatSection (v: v ? includedInBasePermissions)}

      ${hr}

      ## Deprecated Combinators

      The following combinators have been deprecated, and may be removed in the
      future.

      ${formatSection (v: v ? deprecated)}
    '';

    mkdocsSettings = {
      site_name = "jail-nix";
      repo_url = repo.urls.base;
      theme = {
        name = "readthedocs";
        hljs_languages = [ "nix" ];
      };
      # Copyright is placed directly into the page within a <p> tag.
      # Concatenating with </p><p> here is super janky, but good enough for
      # now.
      copyright = lib.concatStringsSep "</p><p>" [
        ''
          Copyright © 2025 Alex David ///
          <a rel="license" href="https://creativecommons.org/licenses/by-nc-sa/4.0/">CC BY-NC-SA 4.0</a>.
        ''
        ''
          This documentation was generated from the <a href="${repo.urls.base}">jail.nix repo</a>,
          commit <a href="${repo.urls.commit}"><code>${repo.shortRev}</code></a>.
        ''
      ];
    };
  in {
    packages.x86_64-linux.default = pkgs.runCommand "website" { buildInputs = [ pkgs.mkdocs ]; } ''
      cp -r ${./static-docs} docs
      chmod -R +w docs
      echo ${lib.escapeShellArg combinatorDocs} > docs/combinators.md
      echo ${lib.escapeShellArg (builtins.toJSON mkdocsSettings)} > mkdocs.yml
      mkdocs build
      mv site $out
    '';
  };
}
