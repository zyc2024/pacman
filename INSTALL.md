Requirements: install and initialize opam

### WINDOWS:
  - $ sudo apt-get install pkg-config
  - $ opam switch create pacman ocaml-base-compiler.4.14.0
  - $ eval $(opam env) *ONLY IF IT ASKS YOU TO*
  - $ opam install -y dune merlin utop ounit ocaml-lsp-server ocamlformat yojson graphics bisect_ppx menhir user-setup
  - $ opam user-setup install
  - go to https://sourceforge.net/projects/vcxsrv/, download, install normally.
    - open xlaunch from folder where you downloaded it
    - Select Multiple windows, Display number is -1, Start no client, Disable access control, Finish
  - $ echo "export DISPLAY=localhost:0.0" >> ~/.bashrc
  - Restart WSL

### MAC:
  Make sure XQuartz is installed (https://www.xquartz.org/)  
  Make sure you have Xcode (download from App Store)
  - Requires ONE of the following two Unix package manager
    - MacPorts
    - Homebrew
  - If using Homebrew :
    - $ brew install pkg-config
  - If using MacPorts :
    - $ sudo port install pkg-config
  - $ opam switch create pacman ocaml-base-compiler.4.14.0
  - $ eval $(opam env) *ONLY IF IT ASKS YOU TO*
  - $ opam install -y dune merlin utop ounit ocaml-lsp-server ocamlformat yojson graphics bisect_ppx menhir user-setup
  - $ opam user-setup install

### Both:
  1. Make sure you are on correct switch (i.e. pacman)
    - $ opam switch list
    - you should expect an arrow to the switch (i.e. pacman)
    - Otherwise, for some unknown reason that you are in a different switch run this command
    - $ opam switch [name of switch]
  2. $ utop
  3. #require "graphics";;
  4. Graphics.open_graph " 800x600";;
  5. This should open a new graphical window and ensures graphics module was properly installed.
  6. Close the graphical window
    - you will see an exception that says **Graphics.Graphic_failure("fatal I/O error")**
  7. Download pacman source code
  8. Next, cd into the folder with pacman source code. 
  9. Make sure if on Windows, xlaunch is running in the background!! For mac, make sure XQuartz is running.
  10. $ make build
  11. $ make play  
  Play the game! Move pacman with keys WSAD.
