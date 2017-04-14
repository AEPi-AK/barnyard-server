# Booth Creation Game Server

## JSON Objects

### GameState 
```
{ "currentPhase" : Phase,
  "phaseTime" : int,
  "timeSincePhaseStart" : int,
  "player1" : player,
  "player2" : player,
  "winner"  : Winner,
  "location" : Location,
  "settings" : Settings
}
```

### Phase

The game phase will be one of the following strings:

  * GameJoining
  * GameInstructions
  * GameBiomeSelection
  * GameInProgress
  * GameTimeUp
  * GameScoring
  * GameWinner
  * GameWaiting

### Player
```
{ "slot0" : Head,
  "slot1" : Body,
  "slot2" : Leg,
  "slot0Score" : int,
  "slot1Score" : int,
  "slot2Score" : int,
  "joined" : bool
}
```
#### joined

Set to "True" if the player has joined, otherwise set to "False"

### phaseTime

The amount of time allocated for this GamePhase. For GameWaiting, we say 0
(since it is actually infinite-- it waits unitl a player joins)

### timeSincePhaseStart

The amount of time elapsed since this phase started.


### Head

If an invalid piece is placed at the head, the head will be set to "HeadErr"

If there is no head placed, the head will be set to "NoHead"

Otherwise, it will be set to one of the following:

 * "Head1"
 * "Head2"

### Body 

If an invalid piece is placed at the head, the head will be set to "BodyErr"

If there is no head placed, the head will be set to "NoBody"

Otherwise, it will be set to one of the following:

 * "Body1"
 * "Body2"

### Leg 

If an invalid piece is placed at the head, the head will be set to "LegErr"

If there is no head placed, the head will be set to "NoLeg"

Otherwise, it will be set to one of the following:

 * "Leg1"
 * "Leg2"

### Winner

Only meaningful if the game is in the GameOver state.

Can be one of the following:

  * "Tie"
  * "Player1"
  * "Player2"

### Location

Currently, one of the following:

  * Desert
  * Tundra

### Settings

```
{
  volume : int,
  brightness : int
}
```

Volume is an int between 0 and 100, inclusive.

Brightness is an int between 0 and 255, inclusive.

## Endpoints

### GET /gamestate

Returns a GameState. See above for the definition of GameState.

If the phase is set to GameWaiting, the game is over and no players have joined to start a new one.

If the phase is set to GameJoining, one player has joined and there is a 10 second
countdown until the game begins.

If the phase is set to GameInProgress, then you may place and remove tiles. In all
other phases, the server will reject requests to place or remove tiles.

If the phase is set to GameOver, then there is a X second window until the phase
returns to GameWaiting. X will be set after we know about how long we want to
play lights/sounds/animations to indicate the winner.

### POST /join/PlayerId

PlayerId is an int, 1 for player 1, 2 for player 2.

The server will only accept these requests if it is in the "GameWaiting"
or "GameJoining" states.

If the phase is "GameWaiting" then joining will change the state to "GameJoining"

### POST /place/PlayerId/Slot/TileId

PlayerId is an int, 1 for player 1, 2 for player 2.

Slot should be an integer 0, 1, or 2. 0 is the head slot, 1 is the body slot, 2 is
the leg slot.

TileId is an id for a tile. Currently, acceptable Ids are:

  * "head1"
  * "head2"
  * "body1"
  * "body2"
  * "leg1"
  * "leg2"

This will place a tile in the corresponding slot and update the game state appropriately.

If a tile is placed in an invalid slot, it will be set to the corresponding error
string for that slot. In addition, it will return "status: error" in the JSON.


### POST /remove/PlayerId/Slot

PlayerId is an int, 1 for player 1, 2 for player 2.

Slot should be an integer 0, 1, or 2. 0 is the head slot, 1 is the body slot, 2 is
the leg slot.

This will remove the tile from Player X at slot Y and update the game state appropriately (where X is the player number and Y is the slot number).

### POST /settings/brightness/Int

Sets the brightness to the Int given. Will reject if it is not between 0 and 255 inclusive.

### POST /settings/volume/Int

Sets the volime to the Int given. Will reject if it is not between 0 and 100 inclusive.

### POST /reset

Puts the game back in a GameWaiting state and clears all the slots

### POST /start (TESTING ONLY)

To allow you to test faster, you can reset the game state to a new 
game with both players joined by sending a POST to /start.

## Running Locally

You'll need to install [stack](https://docs.haskellstack.org/en/stable/README/)

Then, follow the Yesod quickstart guide [here](http://www.yesodweb.com/page/quickstart) starting at step 3.
