{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative  ((*>), (<$>), (<*), (<|>))
import           Data.Attoparsec.Text
import           Data.Text            (Text)
import qualified Data.Text            as T

-- | This is the main data structure that we will use to represent the parsed input string
-- It is basically a sequence of elements and each element can be either a single character or
-- a 'choice' - a list of possibilities wrapped in a curly braces.
-- Notice how the type of the data structure is circularly defined in terms of itself (+1 geek points)
data Element = Letter Char | Choices [ [Element] ]

-- Take a note how the whole program is just a bunch of equations allowing one to perform
-- "equational reasoning" [1], i.e. the haskell compiler basically just "solves" the system of
-- equations. Unlike the imperative languages, the meaning of '=' in haskell is the same as 'is'
-- versus 'let' in imperative languages.
-- Something like:
-- x = 1
-- x = 2
-- just does not make sense in haskell (just like in math, because there is no implicit time/state),
-- but it does make sense in imperative programs because every new line represents progression in
-- time and can change the implicit state.

-- | This is the only un-pure function, necessary to communicate with our dirty world.
-- Anything un-pure has 'IO' in their type signature.
main :: IO ()
main =
  -- the direction is backwards: this way <------------
  -- read this like:
  -- 1. get a line from stdin
  -- 2. process the line with the 'expand' pure function
  -- 3. output to stdout
  putStr =<< (T.unpack . expand . T.pack) <$> getLine

-- | Type signatures are rarely necessary, but is a good idea to provide for basic documentation.
-- This particular type signature means that 'expand' is a function that takes a value of type
-- 'Text' and maps it to potentially another value of also type 'Text'. Functions in haskell are
-- pure, which means they are just mappings from one value to another.
-- e.g.: function (+3) would have a type signature Int -> Int, and is an "infinite" mapping that
-- maps 1 to 4, 2 to 5, etc
-- 'expand' runs a parser on the string input, then 'intercalates' the resulting 'words' with
-- spaces, making a single text line out of them.
expand :: Text -> Text
expand t = either
  error
  (T.intercalate " " . render)
  $ parseOnly parseElements t

-- | 'render' does the majority of the work: as seen from the type signature, it takes a list of
-- Elements (see the Element data type above) and recursively 'renders' the data structure into
-- a list of 'words'.
render :: [Element] -> [Text]
render = render' [""]
  where
    -- | This is a helper function that has a recursion friendly type signature.
    -- It takes a list of partially 'rendered' words and augments the words according to the next
    -- element, then recurses until there are no more elements.
    render' :: [Text] -> [Element] -> [Text]
    -- | This is an example of pattern-matching and multiple function definition:
    -- this definition line defines the function only for cases when the second parameter is an
    -- empty list ([]). This is the base case.
    render' prevs [] = prevs
    -- | This one has a different pattern: something like x:xs would match a non-empty list,
    -- would assign the 'head' of the list to 'x' and the 'tail' to 'xs'. (':' is a cons)
    -- In this case, instead of just using a variable like 'x' (btw, variables in haskell are
    -- one time assignment only, once assigned, you can never reassign them to anything else -
    -- remember: no implicit state) we use nested pattern matching (Letter l), so this definition
    -- would only match cases when the 'head' was constructed with the Letter constructor, meaning
    -- it's a letter (not a choice).
    -- Note: (`T.snoc` l) is the equivalent to lambda (\x -> T.snoc x l). The ticks just make a
    -- function infix vs. prefix: so when applying function 'f' to x and y, we normally write
    -- "f x y", which is a prefix notation. To make it infix, we use ticks: "x `f` y". So if we
    -- skip x, we are left with "`f` y", which is the same as lambda (\x -> f x y)
    render' prevs ((Letter l):es) = render' (map (`T.snoc` l) prevs) es
    -- | And this is the case where the 'head' is a choice, and we specify the according behavior.
    -- 'map' is a map-over-list function. It takes a lambda/function and a list and maps the
    -- function over every element of the list. foldMap, is equivalent to "concat . map", i.e.
    -- a composition of 'map' then 'concat' (which concatenates elements in the list).
    render' prevs ((Choices cs):es) = foldMap (`render'` es) $ map (render' prevs) cs


-- And these are the parser equations:

-- | This type signature says: this is a parser that yields a list of Elements.
-- It uses the many' parser combinator and leverages a simpler parser that
-- parses only one Element
parseElements :: Parser [Element]
parseElements = many' parseElement

-- | This parser uses 'alternative' combinator <|>, the semantic meaning of which in this
-- context is: try parsing with parseChar first, if it fails, try the parseChoices
parseElement :: Parser Element
parseElement = parseChar <|> parseChoices

-- | This one uses a primitive parser 'letter' that parses out only one letter (and fails to
-- parse on numbers or any other non-letter symbol). <$> is an infix version of 'fmap' function,
-- which means apply a function inside a "context". The context in this case is the Parser and
-- we are applying the 'Letter' constructor inside the context, constructing a value of type
-- Element.
parseChar :: Parser Element
parseChar = Letter <$> letter

-- | This parser parses out choices. As you can see, it is a composition of primitive parsers
-- and combinators. '*>' means 'don't worry about the stuff on the left, use the stuff on
-- the right'. And '<*' is the opposite. 'sepBy' just uses 'parseElements' parser along with
-- the primitive parser that parses commas to extract a list of lists of Elements from inside
-- the braces.
parseChoices :: Parser Element
parseChoices = Choices <$> (char '{' *> (parseElements `sepBy` (char ',')) <* char '}')

-- As you can see, the parser code is quite simple to comprehend and deconstruct (comparing to
-- alternatives). Haskell is perhaps the best language for writing parsers. What aids the
-- comprehension is the fact that it uses "denotational semantics" vs. "operational semantics"
-- that is used in imperative languages. Denotational answers the question "What", while
-- operational answers "How".
-- To get more in depth of haskell's abstractions like Functor, Applicative, Monad and to
-- get deeper into haskell parsers I cannot recommend [2] highly enough.
--
-- Hope the above made sense. While this ended up being quite an essay, consider that the whole
-- program occupies only 22 lines (if we don't count the comments and type signatures) and could
-- likely be compressed even further without much detriment to readability.


-- Footnotes:
-- [1]: http://www.haskellforall.com/2013/12/equational-reasoning.html
-- [2]: https://www.seas.upenn.edu/~cis194/spring13/lectures.html
--
--
