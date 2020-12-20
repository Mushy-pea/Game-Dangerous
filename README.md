# Game :: Dangerous (a tribute to ZZT)

## A project update as of 11/10/2020

Having had a little break to spend some time on map content, I have now moved back to engine development.  As all the planned rendering 
and game logic features are now implemented the remaining work is going to be largely around refining and fixing what is already in place.
The motivation for this phase of work is to make the engine more robust and the code more comprehensible by other developers.  I've laid 
out below a high level road map of the tasks that lay ahead, which can hopefully be mostly completed by the end of this year.

1.  Refactoring the code in places and moving it closer to conforming with at least some Haskell coding conventions.  This is something 
I've had an intention to do for some time but was put on the back burner in favour of implementing features.  As it may be too much work to 
refactor everything that could benefit from it within the planned time frame, effort will initially be focussed on the parts of the code I 
deem most likely to cause confusion or be difficult to debug.  Some recent feedback from people in the Haskell community has helped guide 
my direction in this area.

2.  Fixing the bugs I know about and carrying out correctness testing on the game logic.  The latter point will involve ensuring the 
specification for the engine's GPLC scripting language is correct with reference to the compiler and the engine.  I also intend to carry
out unit testing for each of the GPLC instructions.  Please note that the current implementation of the GPLC compiler is written in C# and
held in the repository Mushy-pea/Game-Dangerous-editor.  The file Mushy-pea/Game-Dangerous/app/assm_gplc.hs is the initial prototype I 
wrote in Haskell and is now considered legacy.  I fancied a bit of language variety, you know?

3.  Adding a small number of quality of life features that would likely be expected for even a free game released on a gaming focussed
platform such as itch.io.  For one thing I intend to make a loader for the application, mainly so that users can change settings without
having to edit the config.txt file.

4.  I may decide to add parallelism to parts of the rendering engine.  The motivation would be to allow the ray casting algorithm to cast
rays at a higher density (while not overloading per thread speed capacity on some systems) and thereby address one of the visual artefact 
issues seen in game.

Update (as of 19/12/2020): The first phase of refactoring I'm undertaking is to enforce a limit of 160 characters on the existing code base.  This has
already led to an increase of 9.8% in the total lines in the engine and a number of new functions being written to factor out common functionality.
This phase is about 66% complete, after which further passes will be made to deal with other issues with the code such as using more user defind types to
improve readability.  After that I'll move onto stage 2 and a load of bug fixing and correctness testing.

[![Preview unavailable](https://img.youtube.com/vi/gBaIU4U6eQs/default.jpg)](https://youtu.be/gBaIU4U6eQs)

[![Preview unavailable](https://img.youtube.com/vi/oHMakxQZjlk/default.jpg)](https://youtu.be/oHMakxQZjlk)

[![Preview unavailable](https://img.youtube.com/vi/4Y2er6WZ5qs/default.jpg)](https://youtu.be/4Y2er6WZ5qs)

[![Preview unavailable](https://img.youtube.com/vi/8HuMVTjA138/default.jpg)](https://youtu.be/8HuMVTjA138)

