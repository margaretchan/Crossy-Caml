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

- [Money Bag](https://www.stickpng.com/img/icons-logos-emojis/emojis/money-bag-emoji): 
multiplies your score
- [Coin](https://icon-icons.com/icon/coin-pokemon-go-game/67575): 
adds to your score
- [Snowflake](https://www.freeiconspng.com/img/26305): 
freezes the side movement of enimies
- [Ghost](https://www.pinclipart.com/pindetail/xomwJm_ghost-clipart-eyes-halloween-ghost-cute-png-transparent/): 
allows the player to phase through enimies
- [Heart](https://www.pinpng.com/picture/xbxRRR_pixel-clipart-pixel-heart-broken-pixel-heart-hd/): 
gives the player an extra life
- [Red X](https://www.freeiconspng.com/img/35393): 
clears the game screen
- [Lighning Bolt](http://pluspng.com/png-lighting-bolt-2562.html): 
speeds up the game
- [Team Rocket](https://www.nicepng.com/ourpic/u2w7r5a9w7o0i1w7_team-rocket-pokemon-xy-anime-jessie-james/):
takes away points from your score
- [Question Mark](https://www.pngfly.com/png-q3e4t8/download.html): 
mystery random item

for time based effects, each item's effect lasts for 10 seconds. Some effects
have a countdown timer on the bottom right corner.

## Built With
Libraries:
- [CamlImages](http://gallium.inria.fr/camlimages/)
- OCaml Graphics

Font:
- [04b_30](https://fontmeme.com/fonts/04b-30-font/)

## Authors
- Margaret Chan
- David Sun
- Kevin Huang
