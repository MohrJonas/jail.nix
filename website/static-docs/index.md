# Getting Started

## Installation

It is currently recommended to install jail.nix as a flake input to your
configuration:

```nix
# flake.nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
  inputs.jail-nix.url = "sourcehut:~alexdavid/jail.nix";
  outputs = { nixpkgs, jail-nix, ... }: {
    # ...
  };
}
```

## Initializing

The core functionality of jail.nix is a `jail` function that wraps derivations
in a bubblewrap jail. To set it up, you need to initialize the library with a
version of nixpkgs.

This is done by either calling `jail-nix.lib.init` for a basic setup, or
`jail-nix.lib.extend` for advanced configuration.

### Basic configuration

For a simple working `jail` function, call `jail-nix.lib.init` with an instantiated nixpkgs:
```nix
pkgs = import nixpkgs { system = "x86_64-linux"; };
jail = jail-nix.lib.init pkgs;
```

### Advanced configuration

You can set more options in the initialization with `jail-nix.lib.extend`.

You must pass in an instantiated nixpkgs attribute `pkgs`.

For example, the below snippet creates a `jail` function where `my-combinator`
is passed to jail definitions.

```nix
pkgs = import nixpkgs { system = "x86_64-linux"; };

jail = jail-nix.lib.extend {
  inherit pkgs;
  additionalCombinators = baseCombinators: with baseCombinators; [
    my-combinator = compose [
      (readonly "/foo")
      (readonly "/bar")
    ];
  ];
};

jailed-hello = jail "jailed-hello" pkgs.hello (c: with c; [
  my-combinator
]);
```

## Usage

Once you have an [initialized](#initializing) `jail` function, it takes the
following positional arguments:

* **name**: (String) - the nix store name of wrapper script that is generated.
* **executable to wrap**: (String or Derivation) - An entrypoint to jail, this can
  be a derivation like `pkgs.hello`, or a string like
  `"${pkgs.hello}/bin/hello"`. If a derivation is passed it will call
  `nixpkgs.lib.getExe` on it to get the entrypoint.
* **combinators**: (List of **Combinators**) - The third argument allows you to
  control the permissions of the jail. By default, the wrapped program runs
  with very few permissions and to get it to even work you will need to define
  what you want it to have access to here. For convenience, if this argument is
  a function, jail.combinators will be passed in, and the return value will be
  used.

## Examples

### Provide jail.nix as a module arg in a Nixos configuration:

It may be more ergonomic to provide the jail function as a module argument in
your Nixos configurations:

```nix
# flake.nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
  inputs.jail-nix.url = "sourcehut:~alexdavid/jail.nix";
  outputs = { nixpkgs, jail-nix, ... }: {
    nixosConfigurations.my-nixos-host = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ({ pkgs, ... }: { _module.args.jail = jail-nix.lib.init pkgs; })

        # Now all modules have `jail` passed into them:
        ({ jail, ... }: {
          environment.systemPackages = [
            (jail "jailed-hello" pkgs.hello [])
          ];
        })
      ];
    };
  };
}
```

### Using jail.nix to jail a flake package
```nix
# flake.nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
  inputs.jail-nix.url = "sourcehut:~alexdavid/jail.nix";
  outputs = { nixpkgs, jail-nix, ... }: {
    packages.x86_64-linux =
      let
        pkgs = import nixpkgs { system = "x86_64-linux"; };
        jail = jail-nix.lib.init pkgs;
      in {
        my-jailed-package = jail "jailed-hello" pkgs.hello [];
      };
  };
}
```
