Wumpus World
=======

### General
 * **Field** [N*N], N may be custom
 * Field is devided into **cells**
 * **Agent** starts in bottom left cell 0:0
 * Only one **wumpus** are located somewhere on field

### Objects 
  Agent (actor, player) - only movable object in game
  Pit
  Wumpus
  Gold
 
### Perception
 * Cell containing wumpus and 4 directly adjacent makes agent perceive **stench**
 * In squares adjacent to pit agent will perceive **breeze**
 * In a square with gold agent will perceive **glitter**
 * When agent walks into a wall, it will perceive **bump**
 * When wumpus is killed, **scream** will be perceived
 * Agent __cannot__ perceive its location
 
### Actions
 * **Go forward**
 * **Turn left**
 * **Turn right**
 * **Grab** - pick up an object in the same cell
 * **Shoot** - fire an arrow. Can be used only once
 * **Climb** - makes agent leave the cave. Can be applied when agent in 0:0. Finishes game

### Game endings
 Win is achieved by getting a gold and leaving cave alive
 Fail is achieved by:
 * moving into a cell with pit
 * entering cell with dead wumpus

### Points
 +1000 for winning
 -1 for each action
 -10000 for getting killed



