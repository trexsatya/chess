# Polyglot Chess

This repo is a collection of multiple implementations of one program in different languages and different styles. The program is an implementation of the Chess game.

There are mainly two styles of programming: Object-Oriented (OOP), and Functional (FP). There's imperative style but that's embedded in any OOP program as well. 
FP is declarative as opposed to imperative style.

Declarative means you tell the program what you want (of course, you or someone else must provide an implementation of how a computer program will achieve that somewhere, at some point of time,  but it's well-encapsulated)

Imperative means you tell the program to do this, then do that etc. You tell more of how to do it, than what you really want. Of course, with some refactoring, an imperative program can be made to look declarative.


## Why?
1. There are these two different styles of programming, but there are many programming languages. There's buzz about functional programming nowadays (the evidence for that is
that every other programming languages is trying to include FP elements/structures).<br>
So, naturally, I was curious what this buzz is all about.<br>
And, while looking into Haskell (a purely FP language), I found it somewhat weird.<br>
And, since I am somewhat familiar with different languages, I thought this would be a great idea to embrace the weirdness of FP languages.

2. While trying to compare non-functional and functional languages, on internet I found tons of resources, but they are written from a perspective of demonstrating the best of 
some particular language, there's no common context based on which I can compare. You get an elegant example out of nowhere, and you don't know what to do with it (at least without some hit and try)<br>
So, this repo provides a common context (i.e. game of chess, a particular implementation) to see the same logic implemented in different programming languages.

This might be helpful to other people trying to learn another language.

## What?
The current implementation is a very simple implementation of chess game for two human players. So, it just shows a chess board, allows players to make their move, while 
performing the validations required. It also shows the possible moves from a given position to assis the player. It doesn't show a GUI, it shows the chessboard on terminal (the terminal must support ANSI characters).
Players can enter input commands to highlight the possible moves, and move piece.

Enhancements TODO:
1. Allow users to undo their move 
2. Allow computer vs human play
2. Allow two program implementations to play with each other.
etc.

The currently targeted implementations are in:-
1. Haskell
2. Kotlin
3. Python
4. TypeScript
5. Go
6. Rust
7. Julia
8. Java

The code is, by no means, excellent. So, contributions towards improvement are welcomed!


### Synopsis of Functional Programming:

In purely functional programming, it's all about functions. There are supporting things as well, like types, expressions etc. But, effectively it's just about functions.
Functions, in mathematical sense. But, programs become obscure when written in terms of functions only, that's why FP languages provide many syntactic-sugar to reduce that.

A program in FP would just be an invocation of a function, which will create/instantiate some values, pass those values around in other functions, even passing some functions to other functions.
We don't say that I am storing this value here, and then anyone can access this value later on in the program. We are just passing values and functions.
<br>So, naturally the scope of accessing a value/data is limited, and the scope of side-effects is limited!


### Synopsis of Object-Oriented Programming:

OOP is based on the observation that everything can be treated as objects. 
What are the objects?<br>
This question is not any less of a problem than defining "what is a particle in Physics", and the answer is similar:- <br>
Anything that has some property, and changes its property based on some interaction.

And, since there are innumerable set of properties in the world, the best way to deal with them is to create an abstraction (which our mind automatically does in the real world) i.e. select a set of properties and interactions
in which you are interested. 

So, a common question that should be/has to be asked in Object-Oriented Programming is "What is X?" "What does it mean to be X?", which is on the borderline of becoming philosophical at any moment. And you know philosophy is  almost insolvable. So, that creates problems and confusions.
OOP seems very appealing given that it is more accurate in simulating our real-world experience with objects. E.g. you see "Smart objects/people", "dumb objects/people",
complex objects made from other simpler objects, etc. All that has been mapped to OOP concepts.


### Synopsis of (Mixed) Imperative Style Programming:

You don't care much "what the heck is X", you just instruct the computer to do this, do that, etc. You use all the tools available to you, so if you are using OOP, you might create class, object where you find it convenient.

This shouldn't suggest that this (mixed) style is *the best*, it's just that you are more relaxed, which can prove to be a good thing, as well as a bad thing! 

<hr>

One advantage of purely functional programming languages over others is based on the following observations:-<br>
*Past mistakes always haunt you back, it takes a huge amount of effort to minimise the effect of the past mistakes*. This is a very general observation, applicable in
software architectures, any sort of system, buildings, societies, life. And this equally applies to programming languages.
In case of programming languages, the mistake is the ability to allow programmers to arbitrarily declare null values, which can blow up at any moment.
Languages later on trying to reduce the potential damage still lag behind those languages which got it right the first time.
