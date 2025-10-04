{ combinators, ... }:
let
  inherit (combinators)
    compose
    include-once
    noescape
    readonly
    runtime-deep-ro-bind
    unsafe-add-raw-args
    ;
in
{
  sig = "Permission";
  doc = ''
    Exposes the gpu to jailed application.
  '';
  impl = include-once "gpu" (compose [
    (runtime-deep-ro-bind (noescape "/run/opengl-driver"))
    (runtime-deep-ro-bind (noescape "/run/opengl-driver-32"))
    (readonly (noescape "/sys"))
    (unsafe-add-raw-args "--dev-bind /dev/dri /dev/dri")
  ]);
}
