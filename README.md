# Game :: Dangerous (a tribute to ZZT)

## A project update as of 02/10/2024

Despite my determination to avoid feature creep there are a few last things I feel I can't neglect 
doing to the engine, before moving on to building a game to run on it.  These are explained below.

**Implement map set based game state saving (in progress)**

A common feature in game engines is the ability to load and unload game assets as the player moves 
around, thereby managing CPU and GPU memory usage and allowing for a much larger simulated environment 
then would otherwise be possible.  In Game :: Dangerous environment maps and their associated bundles of 
assets come as packages that can either be completely loaded or not, with no hot swapping of smaller 
asset chunks possible.  Loading these map packages as the player moves between interconnected maps is how 
the engine handles this resource management issue.  The issue of saving game states is coupled 
to this resource management issue as loading another map package involves a complete game logic state 
reset.

The existing game state saving system could already completely save to file and reinitialise a game state tied to 
a single map package. How could this sensibly be extended to cover a set of interconnected maps, given 
that the engine models all non - player objects (models, collision detection elements and scripts) as existing 
within a voxel somewhere in a map at any one time?  The solution I chose involves a save game file referring 
to all the voxels within a set of maps using a single coordinate system.  The engine uses bounds checking 
on these coordinates to determine which map the object referred to is within (and crucially whether 
that map is loaded when the save file is loaded).  This extension doesn't require a change to the save 
file binary format.

**Implement a proper ceiling and sky box system (pending)**

The current single model ceiling system isn't fit for purpose and if I'm going to replace it I might 
as well allow for a basic simulation of both indoor and outdoor environments.

**Allow for multiple player character classes (pending)**

As the image below suggests it's intended that the user will have a choice of two playable characters.  
I've got a rough idea of what the differences will be and believe these can be implemented mostly through 
the engine's scripting language without any major changes to the engine.  They will have to be.

**Fix centipede NPCs (optional)**

In the original ZZT centipedes could swap their heads with their tails in order to escape from dead ends.  
This is supposed to work in Game :: Dangerous but doesn't yet.  I could design maps that avoid the 
risk of centipedes getting stuck but that feels rather limiting.

**Create a launcher for the game (optional)**

The main purpose of a launcher would be to allow users to change engine configuration options without 
having to modify a config file or use command line parameters.

Engine version: 1.1.0

Server version: 2.1.0

![Lore image 1: Ben and Sarah](https://github.com/Mushy-pea/The-Perils-of-Gem-Mining/blob/master/images/Ben_and_Sarah.png)

Lore image 1: Ben and Sarah

![Map development editor](https://github.com/Mushy-pea/The-Perils-of-Gem-Mining/blob/master/images/Editor.png)

The map development editor

[![Preview unavailable](https://img.youtube.com/vi/wOSQ1cYoZXM/default.jpg)](https://youtu.be/wOSQ1cYoZXM)

[![Preview unavailable](https://img.youtube.com/vi/oHMakxQZjlk/default.jpg)](https://youtu.be/oHMakxQZjlk)

[![Preview unavailable](https://img.youtube.com/vi/4Y2er6WZ5qs/default.jpg)](https://youtu.be/4Y2er6WZ5qs)

