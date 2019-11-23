# Welcome to Crossy Caml!

### Installation instructions

- Precondition: OCaml and OPAM are installed on the machine that the game is being run on
- To install the Graphics library, run the following command in terminal: `opam install Graphics`
- If you are having trouble installing graphics, look at the campuswire post @1607
- Install the Camlimages library using `opam install camlimages`


### How to play

- Build the game using: `make build`
- Initialize the GUI using: `make play`

### Troubleshooting

- If having further graphics issues even after looking at campuswire post @1607, 
try uninstalling OCaml and following the guide on the 3110 website. 
When you've reached the point of installing all the packages, add in Graphics
- If the window for the game doesn't pop up and you see an exception saying `Cannot open display`, 
install XMing (Windows) or XQuartz (Mac) and before launching the game, run
`export DISPLAY=:0` 