Instructions to install:

These instructions actually match those found at https://ocaml.org/p/bogue/latest

# Install OPAM packages
```terminal
$ opam update
$ opam upgrade
$ opam install bogue
```

# Run main.ml
```terminal
$ dune build
$ dune exec bin/main.exe
```

You should be able to build and run the project after following the commands above, but below are some possible actions for troubleshooting:

# SDL2 troubleshooting
```terminal
$ opam install dune tsdl tsdl-image tsdl-ttf
$ opam list | grep tsdl
$ sdl2-config --version
```
Check that you have **tsdl version 1.1.0**
Check that you have **SDL >= 2.0.18**

# Installing the latest source of Bogue (if above does not work)
Download the git archive (which can be found at https://ocaml.org/p/bogue/latest), unzip it, cd into the bogue-master dir, and then:
```terminal
$ dune build
$ opam install .
```


