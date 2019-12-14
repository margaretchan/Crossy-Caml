# How to Install OCaml for Crossy Caml

## Installation instructions

- If OCaml and OPAM are not installed on the machine that the game is being run on, see "Troubleshooting" below
- Install the Graphics and CamlImages libraries using the following commands in terminal: 
`opam install Graphics
opam install camlimages`
- If you are having trouble installing graphics, look at the campuswire post #1607 (copied below)

## Troubleshooting

- Instructions for installing OCaml from the [3110 website](http://www.cs.cornell.edu/courses/cs3110/2019fa/install.html)
- If having further graphics issues even after looking at campuswire post #1607, try uninstalling, reinstalling OCaml, and when you've reached the point of installing all the packages, add in Graphics
- If the window for the game doesn't pop up and you see an exception saying `Cannot open display`, 
install XMing (Windows) or XQuartz (Mac) and before launching the game, and run `export DISPLAY=:0` 

### Campuswire post #1607 (Help for installing Graphics)
###### Credit: Anonymous
Graphics should work fine on Mac OS if XQuartz is installed, but extra steps are needed to use graphic packages on Windows. This brief guide is a summary of what worked for me.

#### Part 0
If the following commands in utop don't give an error, then you can successfully run functions from the Graphics library:
```
#require "graphics";;
Graphics.open_graph "";;
```
#### Part 1
In utop, run the following command: `#require "graphics";;`
If you don't get an error, that means the graphics package is already installed. See part 2.

If you do get an error, try running ``ls `ocamlc -where`/graphics*`` in a terminal (outside of utop). If graphics.cma cannot be found, then that means the graphics package is not installed.

To install the graphics package, try `opam install graphics`. If that fails, run `opam switch create 4.08.1` (WARNING: this will take a significant amount of time. It took nearly 1 hour to finish installing on my computer). Then install the graphics package using `opam install graphics`.

#### Part 2
If `#require "graphics";;` works, but `Graphics.open_graph "";;` gives an error of `Fatal error: exception Graphics.Graphic_failure("Cannot open display ")` then you need a WSL.

[Install Xming](https://www.howtogeek.com/261575/how-to-run-graphical-linux-desktop-applications-from-windows-10s-bash-shell/)
(Note: Steps 2 and 4 are optional. I only did steps 1 and 3.)

At the end, make sure to check that `opam switch show` gives 4.08.1
