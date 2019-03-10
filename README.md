# Game :: Dangerous (a tribute to ZZT)

## A project update as of 10/03/2019

Work continues on building the Game :: Dangerous project.  A significant milestone was reached in month 39 of the project,
with the completion of the core game logic.  There are still known issues and planned features yet to be added.  However,
I've decided there's enough of an engine implemented to go ahead and build the first episode of a campaign to run on it.
There's been a change of plan as to how development will proceed from this stage; I now intend the map editor to be a web
application with a C# back end and Javascript / HTML front end.  Users will be able to create content by accessing an instance
of the editor online and downloading the map files the server side generates based on their input.

I will use this system myself to develop the content for this "Episode 1" release.  The change of direction is partly
motivated by a want to add web development work to my portfolio.  Also, a Unity engine based system may have
excluded Linux users from building that part of the project and I'm all for equal opportunities.  So, the remaining list
of required tasks prior to starting work on Episode 1 are as follows.

## Engine development

1.  Implement high resolution timing for frame rate management.  Also, make time dependent lighting effects auto adapt to the available frame rate limits (40, 50 and 60 fps).

2.  Perform a multi platform and controls update.  This will involve replacing all WinAPI specific code with (probably) GLFW, thereby hopefully making it possible to build
    the engine for at least Windows, Linux and Mac OS X.  Make the control keys reassignable and (probably) add mouse look.  Also, add a settings menu for changing
    controls and graphics settings.

3.  Implement file based game state saving.

## Map editor development

I've made a start on implementing the DSL (game logic scripting) compiler in C#, aiming to mirror the functionality of
the Haskell prototype version but with better error reporting.  Everything else remains to be done.

Looks like I'll be busy for a while then, but I'm still hopeful I can release Episode 1 some time in 2019.

[![Preview unavailable](https://img.youtube.com/vi/8HuMVTjA138/default.jpg)](https://youtu.be/8HuMVTjA138)
