
## Kotlin Synopsis

1. Kotlin is not purely functional programming language like Hakell is, but it provides easy support to write functional code.
And this library enhances Kotlin's FP ability: https://arrow-kt.io/docs/core/
2. Kotlin is very akin to Java, and can inter-operative with Java code
3. The best language features of Kotlin can be listed as:
    1. Null-safety, immutability. By default, any variable you declare, cannot hold a null value. If you want to store null value, you have to declare explicitly. This forces you tto handle scenarios whch by mistake are left in Java programs.
    2. Functions as first class citizens
    3. Syntactic sugar for pattern-matching etc, which makes code concise, more readable.
    4. Operator overloading
   
There is no List Comprehension in Kotlin.

Kotlin Cheat Sheet: https://blog.kotlin-academy.com/kotlin-cheat-sheet-1137588c75a


### Kotlin for Functional Programming
We can easily implement FP-style code in Kotlin. With null-safety built in and with smart-cast feature, monads like Maybe are not needed in Kotlin.
Other monads can be easily implemented though.

And, the code still remains concise after implementing FP-style.
