# Game :: Dangerous (a tribute to ZZT)

## A project update as of 31/12/2022

With the engine almost feature complete work has started on the map development editor.  
The architecture I've decided to go with is a client - server approach whereby a server 
written in Haskell will run on the local machine, partnered with a web based client written in TypeScript.  
The server will provide an API that is accessible 
on localhost, which allows changes to a map state to be requested as well as sections 
of the map to be read by the client.

The server will handle loading and saving of map states from and to the file format used 
by the engine.  The several existing Haskell written development tools (such as ImportModel and PreprocessMap) 
will be integrated into the server through static imports.  The C# written GPLC compiler 
(hosted in Mushy-pea/Game-Dangerous-Editor) will be deprecated and a new implementation 
written in Haskell (or AssmGplc updated to meet requirements).  This will then also be 
integrated into the server.

I'm hoping the result will be a much faster and less error prone map development process 
than at present, for me and anyone else who fancies trying their hand at it.

Credits:

The in game music bundled with release 4 has kindly been made available for free by 
Kevin MacLeod under the following license:

Lightless Dawn Kevin MacLeod (incompetech.com)
Licensed under Creative Commons: By Attribution 3.0 License
http://creativecommons.org/licenses/by/3.0/

[![Preview unavailable](https://img.youtube.com/vi/yxnuFl-8j5c/default.jpg)](https://youtu.be/yxnuFl-8j5c)

[![Preview unavailable](https://img.youtube.com/vi/gBaIU4U6eQs/default.jpg)](https://youtu.be/gBaIU4U6eQs)

[![Preview unavailable](https://img.youtube.com/vi/oHMakxQZjlk/default.jpg)](https://youtu.be/oHMakxQZjlk)

[![Preview unavailable](https://img.youtube.com/vi/4Y2er6WZ5qs/default.jpg)](https://youtu.be/4Y2er6WZ5qs)


