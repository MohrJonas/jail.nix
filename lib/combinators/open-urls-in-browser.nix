{ combinators, ... }:
let
  inherit (combinators)
    compose
    jail-to-host-channel
    set-env
    ;
in
{
  sig = "Permission";
  doc = ''
    Allows access to open URLs in `$BROWSER`.

    This works by creating a pipe that is mounted into the jail that forwards
    all URLs to the `$BROWSER` outside of the jail. This way the jailed
    program can launch your browser, even if it has a subset of the
    permissions your browser has.
  '';
  impl = compose [
    (jail-to-host-channel "browserchannel" ''"$BROWSER" "$1"'')
    (set-env "BROWSER" "browserchannel")
  ];
}
