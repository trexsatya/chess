# Polyglot Chess

This repo is a collection of multiple implementations in different languages and different styles. The program is an implementation of the Chess game.

There are mainly two styles of programming: Object-Oriented (OOP), and Functional (FP). There's imperative style but that's embedded in any OOP program as well. 
FP is declarative as opposed to imperative style.
Declarative means you tell what do you want (of course, you or someone else must provide an implementation of how a computer program will achieve that)
Imperative means you tell do this, do that etc. You tell more of how to do it, than what you really want. Of course, with some refactoring, an imperative program can be made to look declarative.


## Why?
1. There are these two different styles of programming, and there are many programming languages. There's buzz about functional programming nowadays (the evidence for that is
that every other programming languages is trying to include FP elements/structures.
So, naturally I was curious what this buzz is all about.<br>
And, I while looking into Haskell (a purely FP language), I found it somewhat weird.
And, since I am somewhat familiar with different languages, I thought this would be a great idea to embrace the weirdness of FP languages.

2. While trying to compare non-functional and functional languages, on internet I found tons of resources, but they are written from a perspective of demonstrating the best of 
some particular language, there's no common context based on which I can compare.
So, this repo provides a common context (i.e. game of chess, a particular implementation) to see the same logic in different programming languages.

This might be helpful to other people trying to learn another language.

## What?
The current implementation is a very simple implementation of chess game for two human players. So, it just shows a chess board, allows players to make their move, while 
performing the validations required. It also shows the possible moves from a given position to assis the player.

Enhancements TODO:
1. Allow computer vs human play
2. Allow two program implementations to play with each other.
etc.

