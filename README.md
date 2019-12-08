<!-- This is the README for Crossy Caml -->

# Crossy Caml
Crossy Caml is a variation of the "Crossy Road" game built entirely in OCaml.
It was built as the final project for Cornell's CS 3110 in Fall 2019.

## Getting Started
Please reference the INSTALL.md document for installation directions.

Documentation can be generated and the directory can be cleaned via the
commands:
```bash
make docs
make clean
```



## Testing
Testing is done via the test.ml file. It is run using:
```bash
make test
```

## Deployment
To run the game, you first must build the files using:

```bash
make build
```
Launching the game window is done by using:
```bash
make play
```

Crossy Caml is played by moving left and right using the 'a' and 'd' keys.
Your camel will move side to side to dodge the enemies (pokemon)
while also attempting to collect items and maximize score. Difficulty can be
selected from the main screen by pressing 's' three times and the game can be
paused at any time by pressing 'p'.

The items are:

- Money Bag: multiplies your score
- Coin: adds to your score
- Snowflake: freezes the game
- Ghost: allows the player to phase through enimies
- Heart: gives the player an extra life
- Red X: clears the game screen
- Lighning Bolt: speeds up the game
- Team Rocket: takes away points from your score
- [Question Mark](https://www.pngfly.com/png-q3e4t8/download.html): 
mystery random item

## Built With
Libraries:
- [CamlImages](http://gallium.inria.fr/camlimages/)
- OCaml Graphics

## Authors
- Margaret Chan
- David Sun
- Kevin Huang
