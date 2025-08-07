# Advanced Configuration

All configuration options on this page can be passed into `jail-nix.lib.extend`.

## additionalCombinators

`additionalCombinators` takes in a list of custom combinators to expose under
`jail.combinators` and in jail definitions. If passed a funciton, jail.nix will
inject the builtin combinators.

For example, this creates a `jail` function that exposes a `my-permission`
combinator:

```nix
jail = jail-nix.lib.extend {
  inherit pkgs;
  additionalCombinators = builtinCombinators: with builtinCombinators; [
    my-permission = compose [
      (readonly "/foo")
      (readonly "/bar")
    ];
  ];
};

# Now my-permission is exposed in all the places the builtin combinators are exposed:

jailed-hello = jail "jailed-hello" pkgs.hello (c: with c; [
  my-permission
]);

# Alternatively, using jail.combinators:

jailed-hello = jail "jailed-hello" pkgs.hello [
  jail.combinators.my-permission
];
```

## basePermissions

By default, jail.nix comes with a base set of permissions that all jails
inherit by default. This configuration option allows you to override these.

The goal of the base permissions are to provide a reasonably secure default
with enough permissions to have most software behave correctly.

Example:
```nix
jail = jail-nix.lib.extend {
  inherit pkgs;
  basePermissions = builtinCombinators: with builtinCombinators; [
    (unsafe-add-raw-args "--proc /proc")
    (unsafe-add-raw-args "--dev /dev")
    (unsafe-add-raw-args "--tmpfs /tmp")
    (unsafe-add-raw-args "--tmpfs ~")
    (unsafe-add-raw-args "--clearenv")
    (unsafe-add-raw-args "--die-with-parent")
    (readonly "/nix/store")
    (readonly "/bin/sh")
    (fwd-env "LANG")
    (fwd-env "HOME")
    (fwd-env "TERM")
  ];
};
```
