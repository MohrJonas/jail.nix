{ combinators, helpers, ... }:
let
  inherit (combinators) set-env;
in
{
  sig = "String -> Permission";
  doc = ''
    Forwards the specified environment variable to the underlying process (if set).
  '';
  impl = name: set-env name (helpers.noescape "\"\${${name}-}\"");
}
