### Chess Game
Functionalities:

1. Show the chessboard on terminal
2. Accept commands (highlight, move) & update the board accordingly
3. Perform validations before updating the board

## Haskell Guide

1. Haskell seems weird because it is based on completely different approach to computation. It basically tries to eliminate *state* (memory) from the programs.
   
   It tries to encode datastructures using functions. So, you can think that there are only three things in Haskell program. *Functions, Expressions, and Types*. Expressions and Types are basically used for declaring and implementing functions. So, in short it's all about functions alone.
   
   https://www.haskellforall.com/2021/01/the-visitor-pattern-is-essentially-same.html
   
2. Operator `$` is weird, Haskell Lint discourages its usage. https://typeclasses.com/featured/dollar
   In fact, coming from other popular languages, most of the syntax of Haskell will seem weird.
   
3. Prelude is the default package, you can look up the definitions in this package here: https://www.haskell.org/onlinereport/standard-prelude.html
   This might be good for learning, see how the magic works.
   

Haskell Cheat Sheet: https://jutge.org/doc/haskell-cheat-sheet.pdf

Portability of Monad concept into other programming languages: https://www.toptal.com/javascript/option-maybe-either-future-monads-js

<hr>

``` 

toUpper 'a' -- works on character, does not work on string (i.e. List of characters)

What if the argument to toUpper is null?
You don't want to write a function specially to handle that, right?

The solution is that don't take the raw value as an argument,

Instead take a wrapped value as argument
If you take the value wrapped in Maybe, you can apply the toUpper function without any additional effort

fmap toUpper $ Just('c')  -- gives Just 'c'
fmap toUpper $ Nothing  -- gives Nothing

-- Maybe is a pattern, Nothing and Just are instances of that pattern.

Where did this fmap come from? - It is defined when Maybe was defined, 

Now, you can generalise this scenario:-
    we wrapped a value in a "container" (i.e Maybe in this case), 
	  And while building/declaring that container, we declared a function which is special.
	  	It is special because it can take a normal function which acts on a normal value, and it returns another function, which acts on the wrapped value.
This scenario, in itself, is another pattern. This pattern is called Functor.
And Maybe is an instance of this Functor pattern.


So, instead of handling this null-safety in a lot of functions that you might have apart from toUpper,
	you shift the responsibility of null-safety into a special type called Maybe

'x <$> y' is just a different way to write 'fmap x y'


You can look at it from a different (inverse perspective):
	If you have a normal value wrapped in some container like Maybe, Either,
	then fmap is the way to apply a normal function to the wrapped value

	enhancedFunction = (function <$>) -- If you want a function which SHOULD NOT work on normal value, but only on wrapped values


-----------------------------

Can't the function itself be null?
In FP languages, functions are themselves a result of computation, so they can be passed around and returned from another function,
So, it definitely can be null.

So, to make it safer:
You wrap the function itself

toUpperEnhanced = Just(toUpper) <*> -- toUpper is now a wrapped function which will work only on wrapped character values
Where did this <*> come from?
	It is declared when Maybe was declared as an Applicative


What if you have a binary function, and you want a function that SHOULD NOT work on normal values, but only on (both) values wrapped.

Example:
-----------------------------

max -- normal function
maxEnhanced = (max <$>) -- works on wrapped value
maxEnhancedPartiallyApplied = maxEnhanced (Just 3) -- Applicative functor, because as a result of this computation, we get a wrapped function 

maxEnhancedPartiallyApplied <*> (Just 6) => 6

Same as, 'max <$> Just 3 <*> Just 6'


------------------------------

What if you have a function that takes normal value but returns wrapped value?
How would you apply a wrapped value to such a function?

This functionality is provided by Monads

When Maybe is declared as a Monad, it declares this operator/function >>=

wrapped-value >>= function-returning-wrapped-value

This allows us to chain many such functions which return a wrapped value to get the final result

	wrapped-value >>= fn_1 >>= fn_2

Example
-------------------------
foo :: Maybe String  
foo = Just 3   >>= (\x -> 
      Just "!" >>= (\y -> 
      Just (show x ++ y))) 

Same can be written using do notation
-------------------------------------
foo :: Maybe String  
foo = do  
    x <- Just 3  
    y <- Just "!"  
    Just (show x ++ y)  

-------------------------------------------------------

instance Monad ((->) r) where
    f >>= k = \ r -> k (f r) r


instance Functor ((,) a) where
    fmap f (x,y) = (x, f y)


instance  Functor Maybe  where
    fmap _ Nothing       = Nothing
    fmap f (Just a)      = Just (f a)           FUNCTION APPLIED IF VALUE IS PRESENT


instance Applicative Maybe where
    pure = Just

    Just f  <*> m       = fmap f m              FUNCTION APPLIED IF VALUE IS PRESENT
    Nothing <*> _m      = Nothing

    											FUNCTION APPLIED IF BOTH VALUES PRESENT
    liftA2 f (Just x) (Just y) = Just (f x y)
    liftA2 _ _ _ = Nothing

    Just _m1 *> m2      = m2  					SECOND ONE IF THE FIRST IS NOT NULL ELSE NULL
    Nothing  *> _m2     = Nothing


instance  Monad Maybe  where
    (Just x) >>= k      = k x					VALUE OF FIRST APPLIED TO THE SECOND IF PRESENT
    Nothing  >>= _      = Nothing

    (>>) = (*>)




instance Functor (Either a) where
    fmap _ (Left x) = Left x                    FUNCTION APPLIED IF VALUE IS PRESENT
    fmap f (Right y) = Right (f y)              


instance Semigroup (Either a b) where
    Left _ <> b = b
    a      <> _ = a

instance Applicative (Either e) where           FUNCTION APPLIED IF VALUE IS PRESENT
    pure          = Right
    Left  e <*> _ = Left e
    Right f <*> r = fmap f r


instance Monad (Either e) where                 VALUE OF FIRST APPLIED TO THE SECOND IF PRESENT
    Left  l >>= _ = Left l
    Right r >>= k = k r


either                  :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x)     =  f x
either _ g (Right y)    =  g y                  APPLIES APPROPRIATE FUNCTION

Right (\x -> 3) <*> Right(2)  gives Right 3
Left 1 <*> (left or right)    gives Left 1
Left(1) >>= \x -> Left(2)     gives Left 1
Right(1) >>= \x -> Left(2)    gives Left 2
Right(1) >>= \x -> Right(\y -> (x, y)) <*> Right(2)   gives Right (1,2)
A shortcut for above is: Right(1) >>= \x -> Right((x,)) <*> Right(2) using TupleSections


if p then s else pure ()
there's a shortcut for it: when p s      e.g.  when debug (putStrLn "Debugging")


You can use any function that works on the normal value, to work on the wrapped value using liftM
liftM (\x -> 2*x) $ (Just 2)   ; result will be Just 4

There's liftA that works same as above but for applicatives
liftA (\x -> 2*x) $ pure(2)    ; result will be 4 (as an applicative)

join              :: (Monad m) => m (m a) -> m a
join x            =  x >>= id
Join is like flattening the wrapper. So, join (Just(Just (Just 2)))  gives Just (Just 2)


Monoid:
Left(1) <> Right("sa")    gives Right "sa"
Left(1) <> Left(2)        gives Left 2

mappend [1] [2]  gives [1,2]
mconcat ["Hello", " ", "Haskell", "!"]   gives "Hello Haskell!"


Haskell REPL Tool: ghci

```
