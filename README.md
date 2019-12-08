<!-- This is the README for Crossy Caml -->

# Crossy Caml
Crossy Caml is a variation of the "Crossy Road" game built entirely in OCaml.
It was built as the final project for CS 3110 in Fall 2019.

## Getting Started
Please reference the INSTALL.md document for installation directions.

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
Your camel will move side to side to dodge the enimies (scorpians, snakes, worms)
while also attempting to collect items. These items include:

- Money Bag: multiplies your score
- Coin: adds to your score
- Snowflake: freezes the game
- Ghost: allows the player to phase through enimies
- Heart: gives the player an extra life

## Built With
Libraries:
- [CamlImages](http://gallium.inria.fr/camlimages/)
- OCaml Graphics

## Authors
- Margaret Chan
- David Sun
- Kevin Huang
