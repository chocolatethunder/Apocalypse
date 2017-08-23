**Built for:** CPSC 449 Programming Paradigms

**Description:** Apocalypse game created in Haskell using two simple AIs

**Methodologies:**
- Test driven development
- Some aspects of Adaptor design inplemented
- Functional and non functional requirements gathering
- Work in a group environment to coordinate various aspects of the program
- Use Git/Github as version control and collaboration tool
- Use Microservices Architecture to overcome the painful integration problem that plagues concurrent software engineering.

**About Apocalypse**

Apocalypse is a board game, a variant of chess, played on an 5x5 grid with only knights and pawns.  For details of the game, see the Wikipedia page on [Apocalypse](https://en.wikipedia.org/wiki/Apocalypse_(chess_variant)#CITEREFPritchard1994).

**Brief**

This project was created to gain a better understanding of Haskell concepts such as - List manipulation, IO operations, Haskell patterns. One key step that was understood is that Concurrent engineering part of this project would make things very difficult to integrate in the end. i.e. when everyone was done their part it would be hard to make each other's code talk to each other. A system was then agreed upon and an API like system was developed where standard input and output standards were agreed upon that will be used to make each piece talk to each other. 

**Overview**

The program can be started in 2 modes. It contains three startegies
- human - Where the user enters the moves
- computer - Smart AI player which scans the entire board for best moves
- random - Dumb AI player which makes a random move

**Command Line Start** ./Apoc.exe \[human|computer|random\] \[human|computer|random\]

![a2](https://user-images.githubusercontent.com/5299394/29585973-44da3202-8746-11e7-8475-2df747befc75.PNG)

**Interactive Mode Start** Simply run the Apoc.exe

![a1](https://user-images.githubusercontent.com/5299394/29585972-44bbb016-8746-11e7-9fc5-b49a0356380d.PNG)

The program displays the new and updated state after a move is made by both parties and displays if the move the player played was valid or if they goofed. The program would then collect penalties. 

![a3](https://user-images.githubusercontent.com/5299394/29586149-fb99e4e2-8746-11e7-8cc3-90b2e544e265.PNG)

There are many ways the game could end. 
- One of the players looses all his/her pawns.  The other player is the winner. 
- One of the players accumulates two penalty points.  The other player is the winner.
- Both players pass on the same round. The one with the most pawns wins.

Here is an example of when the second condition is triggered. 

![a4](https://user-images.githubusercontent.com/5299394/29586305-7055583e-8747-11e7-86b2-7d5444c70177.PNG)
