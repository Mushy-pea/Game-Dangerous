# Game-Dangerous (a tribute to ZZT)

## A project update as of 29/10/2018

It's been about 33 months since I started working on the Game :: Dangerous project.  As I didn't know what graphics cards did back then,
hadn't written games for existing 3D engines and was only a novice with Haskell, it's been a pretty steep learning curve.  However, it appears
I'm finally getting close to something that looks like an early 1990s demo of virtual reality, with the goal of bringing the key concepts
of ZZT into a 3D space still on course.  In an attempt to avoid complexity creep and get something close to a proper game released within a reasonable
time frame, I've decided the list of features / fixes below are the targets for engine development prior to such a release.

1.  Complete testing / fixing of type 1 NPC logic.

2.  Implement high resolution timing for frame rate management.  Also, make time dependent lighting effects auto adapt to the available frame rate limits (40, 50 and 60 fps).

3.  Complete implementation of type 2 (centipede) NPC logic.  Build 3D models and test.

4.  Perform a multi platform and controls update.  This will involve replacing all WinAPI specific code with (probably) GLFW, thereby hopefully making it possible to build
    the engine for at least Windows, Linux and Mac OS X.  Make the control keys reassignable and (probably) add mouse look.  Also, add a settings menu for changing
    controls and graphics settings.

5.  Implement ceiling collision detection.

6.  Implement visible ceiling at top of environment and a sky box system.

7.  Implement file based game state saving.

An optimistic prediction would be that this list would be completed by the end of 2018.  When it is completed there will still only be a basic set
of prototype tools to aid map development (i.e. DSL compiler and 3D model import tool).  At this stage I intend to start dividing my available time between
building a map set / campaign using hack job tactics (including hand coding map geometry) and writing the C# / Unity Engine based map editor.
Hopefully I'll be able to release a playable "full game" some time in the first half of 2019, but we'll see.  Anyway, I appreciate the interest a few people have
shown so far and I'd like to assure anyone who is interested that I'm determined to press the launch button on this.  If anyone is interested in contributing
to this project please feel free to contact me on the e - mail address on my Github home page.

[![Preview unavailable](https://i.ytimg.com/vi/WzANss7QrXM/hqdefault.jpg?sqp=-oaymwEXCPYBEIoBSFryq4qpAwkIARUAAIhCGAE=&rs=AOn4CLBPQP-qKKtXyBW8QaAcWzpd-T9XMw)](https://www.youtube.com/watch?v=WzANss7QrXM&t=37s)
