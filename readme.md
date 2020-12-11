# chess.cl
The game of chess written in Common Lisp.

My goals for this project are:
1. Practice writing "real" Common Lisp, outisde of textbook exercises
2. Try and write elegant code as best I can
3. Use it as a base for studying SCC.361 - AI & ML

*Features:*
- Singleplayer (versus AI)
- Multiplayer (play & pass)
- Save/load

*Project structure:*
                  +---------------+
 (Depends on all) |    control    | - main logic of the game
                  +---------------+
                  |    player     | - player definition(s) & related functions
                  +-------+-------+
                  | piece | parse | - monadic move parser(s) & piece definition(s)
                  +-------+-------+
(No dependencies) | term  | util  | - terminal functions & misc. utilities
                  +-------+-------+

*TODO:*
- Castle method
- Checkmate test
- Testing, testing, testing!
- Create a setup/main menu screen
- Save/load or ai; have your pick!
