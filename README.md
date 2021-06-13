# Game :: Dangerous (a tribute to ZZT)

## A project update as of 13/06/2021

Release 4 of the project is being made today.  Since release 3 a raft of bug fixes have been made and the packaged demo map is now completable
and contains all its intended features.  No further changes are planned to the map or scripting language specifications so future versions
of the engine should remain backwardly compatible with this map.  There is further work planned on the engine (as detailed below) and
the quality of the packaged map is admittedly only at proof of concept level.  However, for developers interested in the project on a technical level
it provides a demonstration of most of the engine's core features.  It may also be useful as a baseline for getting play tester feedback on the core
game mechanics.  Below are the remaining tasks I intend to complete before turning my attention to developing a production package of game content.

1.  Adding a small number of quality of life features that would likely be expected for even a free game released on a gaming focussed
platform such as itch.io.  For one thing I intend to make a loader for the application, mainly so that users can change settings without
having to edit the config.txt file.

2.  I may decide to add parallelism to parts of the rendering engine.  The motivation would be to allow the ray casting algorithm to cast
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


