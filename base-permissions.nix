pkgs: let helpers = import ./helpers.nix pkgs; in combinators: with combinators; [
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

  (add-runtime ''
    if [ ! -e ${helpers.dataDirSubPath "passwd"} ] || [ ! -e ${helpers.dataDirSubPath "group"} ]; then
      NOLOGIN=${pkgs.shadow}/bin/nologin
      mkdir -p ${helpers.dataDir}
      echo "root:x:0:0:System administrator:/root:$NOLOGIN" > ${helpers.dataDirSubPath "passwd"}
      echo "$(id -un):x:$(id -u):$(id -g)::$HOME:$NOLOGIN" >> ${helpers.dataDirSubPath "passwd"}
      echo "root:x:0:" > ${helpers.dataDirSubPath "group"}
      echo "$(id -gn):x:$(id -g):" >> ${helpers.dataDirSubPath "group"}
    fi
  '')
  (ro-bind (noescape (helpers.dataDirSubPath "passwd")) "/etc/passwd")
  (ro-bind (noescape (helpers.dataDirSubPath "group")) "/etc/group")
]
