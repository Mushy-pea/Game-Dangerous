# Game :: Dangerous (a tribute to ZZT)

## A project update as of 23/03/2021

Work continues to get the game engine and tools to a point where I believe it's ready to build a proper game package with.  The remaining tasks
are as follows.

1.  Fixing the bugs I know about and carrying out correctness testing on the game logic.  The latter point will involve ensuring the 
specification for the engine's GPLC scripting language is correct with reference to the compiler and the engine.  I also intend to carry
out unit testing for each of the GPLC instructions.  Please note that the current implementation of the GPLC compiler is written in C# and
held in the repository Mushy-pea/Game-Dangerous-editor.  The file Mushy-pea/Game-Dangerous/app/assm_gplc.hs is the initial prototype I 
wrote in Haskell and is now considered legacy.  I fancied a bit of language variety, you know?

2.  Adding a small number of quality of life features that would likely be expected for even a free game released on a gaming focussed
platform such as itch.io.  For one thing I intend to make a loader for the application, mainly so that users can change settings without
having to edit the config.txt file.

3.  I may decide to add parallelism to parts of the rendering engine.  The motivation would be to allow the ray casting algorithm to cast
rays at a higher density (while not overloading per thread speed capacity on some systems) and thereby address one of the visual artefact 
issues seen in game.

Additional credits: Samuel Schlesinger has become the second contributor to the Game :: Dangerous project by making the engine and
development tools into a Cabal project.  Nice one!

The in game music bundled with release 3 has kindly been made available for free by Kevin MacLeod under the following license:

Lightless Dawn Kevin MacLeod (incompetech.com)
Licensed under Creative Commons: By Attribution 3.0 License
http://creativecommons.org/licenses/by/3.0/

[![Preview unavailable](https://img.youtube.com/vi/gBaIU4U6eQs/default.jpg)](https://youtu.be/gBaIU4U6eQs)

[![Preview unavailable](https://img.youtube.com/vi/oHMakxQZjlk/default.jpg)](https://youtu.be/oHMakxQZjlk)

[![Preview unavailable](https://img.youtube.com/vi/4Y2er6WZ5qs/default.jpg)](https://youtu.be/4Y2er6WZ5qs)

[![Preview unavailable](https://img.youtube.com/vi/8HuMVTjA138/default.jpg)](https://youtu.be/8HuMVTjA138)


