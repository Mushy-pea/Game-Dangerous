# Game :: Dangerous (a tribute to ZZT)

## A project update as of 19/07/2020

About four and a half years after I started I can now announce that every planned feature of the Game :: Dangerous
engine has been implemented.  Why is the version number still only 0.8 then, you might ask.  Well, for those who
have been following the commit history for a while, you may have noticed signs of me being a bit indecisive on the
approach I'd like to take to the game creation system / editor.

To be honest, I've been turning this decision over
for literally years but there was always so much work left to do on the engine that I was able to focus on that and
put off committing to an approach.  Semi - manually editing the text files that contain map descriptions has been just
about viable until now, but as I've got plans to build multiple maps containing up to 30000 voxels each a better
solution is called for.

What I've finally decided is to augment the engine's console mode such that it can be used as a kind of creative mode,
 in the sense that Minecraft has survival and creative modes.  In other words the user will be able to make changes to
the game environment with console commands and test the results in near real time.  I'll need to make some engine changes
and write another tool chain program to allow maps created this way to be saved in the existing map file format, but
much of the required code is already there.  This will be the platform on which I then attempt to build the engine's first
(and possibly only) campaign mode map pack: "The Perils of Gem Mining (a tribute to ZZT)"!

[![Preview unavailable](https://img.youtube.com/vi/oHMakxQZjlk/default.jpg)](https://youtu.be/oHMakxQZjlk)

[![Preview unavailable](https://img.youtube.com/vi/4Y2er6WZ5qs/default.jpg)](https://youtu.be/4Y2er6WZ5qs)

[![Preview unavailable](https://img.youtube.com/vi/8HuMVTjA138/default.jpg)](https://youtu.be/8HuMVTjA138)

