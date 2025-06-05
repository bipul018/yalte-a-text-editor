{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = [
    pkgs.sbcl
    pkgs.sbclPackages.qlot
    pkgs.sbclPackages.qlot-cli
    pkgs.libffi
    pkgs.raylib
  ];
  propagatedBuildInputs = [
    #pkgs.libffi
  ];
  
  buildInputs = [
    #pkgs.libffi
  ];
  inputsFrom = [
    # pkgs.gcc
    pkgs.glibc

    pkgs.gcc
    pkgs.git
  ];

  # inputsFrom = [ pkgs.hello pkgs.gnutar ];
  

  shellHook = ''
  export LD_LIBRARY_PATH=${pkgs.libffi}/lib:$LD_LIBRARY_PATH
  export LD_LIBRARY_PATH=${pkgs.raylib}/lib:$LD_LIBRARY_PATH
  # export LD_LIBRARY_PATH=${pkgs.wayland}/lib:$LD_LIBRARY_PATH
  unset WAYLAND_DISPLAY
  alias sbcl="sbcl --load .qlot/setup.lisp"
  '';
}

  

