{ combinators, ... }:
let
  inherit (combinators)
    compose
    include-once
    unsafe-add-raw-args
    ;
in
{
  sig = "Permission";
  doc = ''
    Exposes your timezone.
  '';
  impl = include-once "time-zone" (compose [
    (unsafe-add-raw-args "--ro-bind \"$(realpath /etc/localtime)\" \"$(readlink /etc/localtime)\"")
    (unsafe-add-raw-args "--symlink \"$(readlink /etc/localtime)\" /etc/localtime")
  ]);
}
