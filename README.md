# jail.nix

A helper to make it easy and ergonomic to wrap your derivations in
[bubblewrap](https://github.com/containers/bubblewrap).

Please send patches, questions and discussions to my [general mailing
list](https://lists.sr.ht/~alexdavid/general).

## Example

```nix
# flake.nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
  inputs.jail-nix.url = "sourcehut:~alexdavid/jail.nix";

  outputs = { nixpkgs, jail-nix, ... }: let
    pkgs = import nixpkgs { system = "x86_64-linux"; };
    jail = jail-nix.lib.init pkgs;

    untrusted-package = pkgs.writeScriptBin "untrusted" ''
      ls -la $HOME
    '';
  in {
    packages.x86_64-linux.jailed = jail "my-jail" untrusted-package (combinators: with combinators; [
      # See combinators section below for more functions that can go here

      # Give program access to the network
      network

      # Allow program to create windows
      gui

      # Give program access to the GPU
      gpu

      # Give program read-only access to /var/log/journal
      (readonly "/var/log/journal")

      # Mount ~/foo to /bar in the jail as read-write
      # (noescape due to `~` — all arguments are shell escaped by default)
      (rw-bind (noescape "~/foo") "/bar")
    ]);
  };
}
```

## Combinators

By default, only the bare minimum permissions are exposed. By passing
combinators into the jail you can modify the jail to expose more permissions.

[Click here](https://alexdav.id/projects/jail-nix/combinators/) for a full list
of combinators and their documentation.
