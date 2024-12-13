Instructions to install and run:

# Install OPAM packages
```terminal
$ opam update
$ opam upgrade
$ opam install graphics
```
# Running the program
```terminal
$ dune exec bin/main.exe
```

# For MacOS, if you can't open the display using ```dune exec bin/main.exe```, download xQuartz from https://www.xquartz.org/
# If necessary, restart the computer after downloading xQuartz and then run ```dune exec bin/main.exe```