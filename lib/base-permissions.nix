pkgs: let helpers = import ./helpers.nix pkgs; in combinators: with combinators; [
  (unsafe-add-raw-args "--proc /proc")
  (unsafe-add-raw-args "--dev /dev")
  (unsafe-add-raw-args "--tmpfs /tmp")
  (unsafe-add-raw-args "--tmpfs ~")
  (unsafe-add-raw-args "--clearenv")
  (unsafe-add-raw-args "--die-with-parent")
  (add-pkg-deps [ pkgs.coreutils ])
  (readonly "/nix/store")
  (readonly "/bin/sh")
  (fwd-env "LANG")
  (fwd-env "HOME")
  (fwd-env "TERM")
  fake-passwd
]
