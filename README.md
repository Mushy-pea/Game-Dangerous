# Game :: Dangerous (a tribute to ZZT)

## A project update as of 02/04/2023

I've set myself a target that by the end of September this year I'll have started to create production map content 
using the map development editor.  For much of the seven and a half years over which I've been (on and off) working 
on this project, it's felt like there was still such a mountain of work to do that I didn't even try to estimate when 
this milestone would be reached.  At this stage there's an engine that it appears can viably run the kind of game I'd 
like to build and a substantial part of the editor that would make it feasible to build.  There are still question 
marks over what will happen when I try to implement the game design ideas in my head on this platform, including complex 
puzzles.

I suspect it'll turn into a battle of patience and attrition where I'm forced to squeeze every last ounce of 
capability out of the engine (and myself) to make something anyone might find remotely fun.  That sounds like great fun 
in itself though.

Edit (14/05/2023): Semantic versioning has been introduced for the engine and map development server.  The server API 
is defined through the command interpreter and GPLC compiler implemented in HandleInput and CompileGPLC, respectively.  
The GPLC language also has a specification in the repo.  The engine API is defined through the way it interprets map 
files and the asset files they refer to (model files, sound map and light map) as well as the binary format of game save 
files.

Engine version (14/05/2023): 0.9.0
Server version (14/05/2023): 1.0.0

![Annotated GPLC code example](https://github.com/Mushy-pea/Game-Dangerous/blob/master/images/CodeColouring.png)

Figure 1: Example of annotated GPLC source code that has been compiled using CompileGPLC
Key -> Blue: keyword, Green: readable reference argument, Red: writable reference argument, White: literal constant argument (or anything else)

[![Preview unavailable](https://img.youtube.com/vi/yxnuFl-8j5c/default.jpg)](https://youtu.be/yxnuFl-8j5c)

[![Preview unavailable](https://img.youtube.com/vi/oHMakxQZjlk/default.jpg)](https://youtu.be/oHMakxQZjlk)

[![Preview unavailable](https://img.youtube.com/vi/4Y2er6WZ5qs/default.jpg)](https://youtu.be/4Y2er6WZ5qs)

