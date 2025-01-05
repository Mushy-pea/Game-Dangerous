# Game :: Dangerous (a tribute to ZZT)

## A project update as of 05/01/2025

I'm almost ready to declare a feature freeze for the project, after which my remaining efforts will go into 
maintaining the code base and building a map set to run on the engine.  The most recently completed and in progress 
features are discussed below.

**Implement map set based game state saving (completed 09/11/2024)**

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

**Allow for multiple player character classes (completed 05/01/2025)**

Only some aspects of this feature are tightly bound within the engine, while the rest will be implemented through 
GPLC scripts in the relevant maps modifying their behaviour based on the playerClass value the engine exposes to 
them.  The following table shows how the implementation will be divided.  The available characters will of course 
be Ben Davies and Sarah Shields (see Lore image 1 further down).

![Figure 1: How the player character class implementation will be structured](https://github.com/Mushy-pea/The-Perils-of-Gem-Mining/blob/master/images/Classes%20screenshot.png)

Figure 1: How the player character class implementation will be structured

**Create a launcher for the game (in progress)**

As it stands the launcher works cooperatively with the engine to support the map set based game state saving feature, 
although further work is needed to make this system production ready.  It is also intended that the launcher will 
allow users to change engine configuration options without having to modify a config file or use command line 
parameters.

Engine version: 1.3.0

Server version: 2.2.0

![Lore image 1: Ben and Sarah](https://github.com/Mushy-pea/The-Perils-of-Gem-Mining/blob/master/images/Ben_and_Sarah.png)

Lore image 1: Ben and Sarah

![Map development editor](https://github.com/Mushy-pea/The-Perils-of-Gem-Mining/blob/master/images/Editor.png)

The map development editor

[![Preview unavailable](https://github.com/Mushy-pea/The-Perils-of-Gem-Mining/blob/master/images/Video%20screenshot.png)](https://youtu.be/h-RChZvQUyU&t)

[![Preview unavailable](https://img.youtube.com/vi/wOSQ1cYoZXM/default.jpg)](https://youtu.be/wOSQ1cYoZXM)

[![Preview unavailable](https://img.youtube.com/vi/oHMakxQZjlk/default.jpg)](https://youtu.be/oHMakxQZjlk)

[![Preview unavailable](https://img.youtube.com/vi/4Y2er6WZ5qs/default.jpg)](https://youtu.be/4Y2er6WZ5qs)

