{ ... }:
{
  sig = "String -> Permission";
  doc = ''
    Prepends the passed string to `$PATH`.
  '';
  impl = path: state: state // { path = [ path ] ++ state.path; };
}
