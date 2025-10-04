{ ... }:
{
  sig = "String -> Permission";
  doc = ''
    Removes the call to `--unshare-` for the provided namespace.

    By default, jail-nix unshares all namespaces, calling `share-ns "pid"`
    will remove the `--unshare-pid` flag from bwrap which will allow this
    process to share the same pid namespace as the host.

    See BWRAP(1) for more information.
  '';
  impl =
    namespace: state:
    state
    // {
      namespaces = state.namespaces // {
        ${namespace} = true;
      };
    };
}
