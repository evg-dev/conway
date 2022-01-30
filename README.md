# Conway's Game of Life. Haskell version  

Console game with ncurses   

![Image screen](https://raw.githubusercontent.com/evg-dev/conway_game_hs/master/game.png)

 ## Compile
 
Compiled with stack:

```bash
stack build
```

Run in project:

```bash
 stack exec conway-exe
```

Game begin on pause. 

Control the cursor and fill in the points on the grid by pressing the ENTER on the point.

'X' - set "Dead" cell

'@' - set "Live" cell

Unpause by pressing "p" and watch autogeneration, or use manual mode by pressing the SPACE.

You can change points on the grid in the process by pausing the game and switching back to manual mode.

To start new game press "n"

Exit - 'q'