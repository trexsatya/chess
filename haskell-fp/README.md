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

Functors, Applicative, and Monads are patterns



Functor:
 something that can work on the value wrapped <$> wrapped-value 
 fmap in Haskell

 fmap (++"!") (Just "wisdom") ; the result will be Just "wisdom!" 
 (++"!") <$> (Just "wisdom")  ; will also give the same result

--------------------------------------------------

Applicative:
 something that can work on the value wrapped, but is itself wrapped <*> wrapped-values

 Just (+3) <*> Just 3  ; the result will be Just 6

--------------------------------------------------

Monad:
 something that can work on the value wrapped, but returns the wrapped values instead of plain values  >>=  wrapped-values


>> is just like ; in imperative languages
the first result is ignored while returning the final result
((*) <$> Just 2 <*> Just 8 ) >> Just (1)     result is Just 1


>>= passes the result of first action to the second
((*) <$> Just 2 <*> Just 8 ) >> Just         result is Just 16


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



```