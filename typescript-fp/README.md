
TypeScript claims to follow set-theoretic approach. Types are just sets, and values can belong to one or many sets.
Types are present just at compile time, ultimately type-script code compiles into JavaScript code, and there are no times there after conversion.

An object can be passed where a type (e.g. interface, class) is expected even if the object is not told to be related to the type, IF the object conforms to the structure of that type.
The relationships between types are determined by the properties they contain, not whether they were declared with some particular relationship.

https://immutable-js.github.io/immutable-js/docs/

<hr>
There's another language called PureScript that compiles into JavaScript, and is purely Functional Programming language. But it's lacking a few things: 
<a href="https://www.slant.co/versus/378/389/~typescript_vs_purescript">https://www.slant.co/versus/378/389/~typescript_vs_purescript</a> 

Why Strict Type Checking? - https://dhh.dk/arc/000074.html
