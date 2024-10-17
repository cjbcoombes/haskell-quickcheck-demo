# Haskell QuickCheck
## Problem 1
Since the assignment mentions "Often times these libraries have the name 'Quickcheck' in their name, after the system, created in 1999, that originated the idea", why not go right to the source and try it out?

The original QuickCheck was written in Haskell, a language even more functional than Racket. I think it will be interesting to see how properties work in Haskell's stricter type system (not contract-based), and how the insistence on purity makes random generators more difficult.

## Problem 2

### Example 1: Basics
First, a simple example to make sense of the most basic functionality of the library:
```haskell
badReverse1 :: [a] -> [a]
badReverse1 = reverse . drop 1

badReverse2 :: [a] -> [a]
badReverse2 = id

prop_reverse_of :: Eq a => ([a] -> [a]) -> [a] -> Bool
prop_reverse_of revFunc l = id l == (revFunc . revFunc) l

go :: IO ()
go = do
    quickCheck (prop_reverse_of reverse :: [Int] -> Bool)
    quickCheck (prop_reverse_of reverse :: [String] -> Bool)
    quickCheck (prop_reverse_of badReverse1 :: [Int] -> Bool)
    quickCheck (prop_reverse_of badReverse1 :: [String] -> Bool)
    quickCheck (prop_reverse_of badReverse2 :: [Int] -> Bool)
    quickCheck (prop_reverse_of badReverse2 :: [String] -> Bool)
```
`quickCheck` takes a `Testable prop`, which represents a more general idea of a testable property but in this case is any function `Arbitrary a => a -> Bool`. A type instance of `Arbitrary` simply provides some way to generate objects of that type, and QuickCheck predefines many such generators for the built-in types, including numbers, strings, and lists. The return type `Bool` is because the property is expected to return `True` if it passes and `False` if it fails, and this return value is what the QuickCheck library will be checking.

For properties acting on parametric types, such as `prop_reverse_of` above, type annotations are required for QuickCheck to know what kind of data to generate, and the type annotation alone can change the behavior of the check.

With this system it is fairly simple both to write and check properties using built-in data types. The example includes a property that checks whether a reverse function composed twice is equivalent to the identity function when called on a list. The property is called on the built-in `reverse` function as well as two incorrect functions, `badReverse1` and `badReverse2`.

The output of the example is:
```
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
*** Failed! Falsified (after 2 tests):
[0]
*** Failed! Falsified (after 2 tests and 1 shrink):
[""]
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
```
The library gives concise, helpful feedback including shrunk counterexamples, however it does not indicate _which_ property is associated with each bit of feedback, so for a more detailed output the user would have to manually add statements such as `putStrLn "Testing (prop_reverse_of reverse :: [Int] -> Bool)"`.

Also, there is no failure on `badReverse2`—a reminder that the library is only as good as the properties you write, and in this case the property does not identify that `badReverse2` is not a valid implementation of `reverse`.

### Example 2: Generation
The following example includes three different ways to generate an ordered list.
```haskell
newtype OrderedList1 a = OrderedList1 [a] deriving Show
newtype OrderedList2 a = OrderedList2 [a] deriving Show
newtype OrderedList3 a = OrderedList3 [a] deriving Show


-- OrderedList1

instance (Arbitrary a, Ord a) => Arbitrary (OrderedList1 a) where
    arbitrary = fmap (OrderedList1 . sort) arbitrary

-- OrderedList2

insertOrdered' :: Ord a => a -> [a] -> [a]
insertOrdered' x [] = [x]
insertOrdered' x (y:ys) | x < y = x:y:ys
                        | otherwise = y:insertOrdered' x ys

insertOrdered :: Ord a => a -> OrderedList2 a -> OrderedList2 a
insertOrdered x (OrderedList2 l) = OrderedList2 $ insertOrdered' x l

instance (Arbitrary a, Ord a) => Arbitrary (OrderedList2 a) where
    arbitrary = sized make
        where make 0 = return (OrderedList2 [])
              make size = insertOrdered <$> arbitrary <*> resize (size - 1) arbitrary
    
-- OrderedList3

instance (Arbitrary a, Ord a) => Arbitrary (OrderedList3 a) where
    arbitrary = sized make
        where make 0 = return (OrderedList3 [])
              make size = do
                rest <- resize (size - 1) arbitrary
                case rest of
                    (OrderedList3 []) -> fmap (OrderedList3 . singleton) arbitrary
                    (OrderedList3 (x:xs)) -> fmap (OrderedList3 . maybe [] (:x:xs)) (suchThatMaybe arbitrary (< x))
    

go :: IO ()
go = do
    generate (arbitrary :: Gen (OrderedList1 Int)) >>= print
    generate (arbitrary :: Gen (OrderedList2 Int)) >>= print
    generate (arbitrary :: Gen (OrderedList3 Int)) >>= print
    -- Plus the built-in OrderedList:
    generate (arbitrary :: Gen (OrderedList Int)) >>= print
```
In order for `quickCheck` to generate some type, it needs to be an instance of `Arbitrary`. An `Arbitrary a` must specify one value `arbitrary :: Gen a`, which is in the `Gen` monad. Chaining within this monad allows QuickCheck to take care of randomness under the hood so the user can write pure code, but it does require some more effort on the part of the user to manage the monad types when writing the generator. 

Starting with `OrderedList1`, we see that the `Arbitrary (OrderedList1 a)` is constrained to `(Arbitrary a, Ord a)`. The `Ord` constraint is necessary in this specific case because it only makes sense to make an ordered list of elements that can be ordered. The `Arbitrary` constraint is fairly common when generating container types, because to generate an instance of the container you also need to be able to generate elements to be inside the container. The implementation simply generates a list (using the built-in instance of `Arbitrary a => Arbitrary [a]`) and sorts it—note that following which `arbitrary` is what comes down to following the types and is best left to a good editor.

`OrderedList2` makes use of the combinator `sized`, so instead of simply `Gen a` we can pass an `Int -> Gen a`, taking a size parameter to determine roughly the size of the generated example. This implementation is recursive and uses the `resize` combinator with `arbitrary :: Gen (OrderedList2 a)` to generate a version of itself with `size - 1`, then generates an arbitrary item and adds it insertion-sort style to the already-ordered list. The functor/applicative maps allow for relatively painless usage of `Gen`, as in `insertOrdered <$> arbitrary <*> resize (size - 1) arbitrary`. A somewhat subtle issue, however, is that this generator populates with very many small values because the decreasing `size` parameter affects the generation of the contained type as well as the length of the list.

The third and most naive generator, `OrderedList3`, is again recursive but this time it generates the rest of the list and then uses another combinator `suchThatMaybe`, to generate an element smaller than the rest of the list. This is one difficulty of generation: the only way to generate an arbitrary value with some constraint is to try randomly, which may be slow or simply fail (hence the `Maybe`). An alternative version, `suchThat`, simply hangs forever when it can't find a valid value.

Overall QuickCheck does provide useful utilities for generating examples, however the monad types can be difficult to navigate and some prudence is required to avoid certain pitfalls in recursive generation.

### Example 3: Tree Prop
To bring some things together, let's look at an example with both a generator and a property:
```haskell
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized gen
        where gen count = frequency [(1, return Leaf), (count, node (count `quot` 2))]
              node count = Node <$> arbitrary <*> gen count <*> gen count
    shrink Leaf = [Leaf]
    shrink (Node x l r) = [Node xs ls rs | xs <- shrink x, ls <- shrink l, rs <- shrink r]

treeMax :: Ord a => Tree a -> Maybe a
treeMax Leaf = Nothing
treeMax (Node x l r) = Just $ maybe x (max x) (max (treeMax l) (treeMax l))

prop_treeMax :: Ord a => Tree a -> a -> Bool
prop_treeMax Leaf _ = True
prop_treeMax tree new = fromMaybe False $
    do m <- treeMax tree
       nml <- treeMax (Node new tree Leaf)
       nmr <- treeMax (Node new Leaf tree)
       return $ max m new == nml && max m new == nmr

go :: IO ()
go = do
    quickCheck (withMaxSuccess 10000 (prop_treeMax :: Tree Int -> Int -> Bool))
```

Three new things here are:
- the tree uses the `frequency` combinator to generate multiple options with a given frequency (decreasing the frequency allows roughly random but still terminating tree structures)
- the tree generator has a `shrink` function, which QuickCheck can use to generate a list of "smaller" examples from a failing one in order to hopefully provide a nicer counterexample to the user
- the property test uses `withMaxSuccess`, which is one of several possible arguments to `quickCheck` and specifies the number of tests to run

This all runs smoothly and `quickCheck` does correctly report a failure! A sneaky typo in `treeMax` means it only ever checks the left side of the tree. This example overall demonstrates an ideal usage of QuickCheck with a fairly simple generator and prop that allow us to find a bug without writing many unit tests.

## Problem 3
Property-based testing (PBT) is, ultimately, another form of testing. Like unit testing, it can assert for certain inputs to a function that certain things are true, but cannot guarantee success for _all_ inputs. The difference is, while unit testing compares the exact output for a few inputs, PBT specifies a more general property of the output and tries it on many inputs. What are the upsides and downsides of this approach?

In terms of pure coverage, PBT gets you big bang for your buck. A single property gets you thousands of test cases far faster than writing the equivalent number of unit tests. For someone actively developing, this provides feedback far faster and easier along the way than requiring many unit tests to be written beforehand. The sheer number of test cases may also find things that were not covered by unit tests, notably with regards to scaling since the random generator can easily make very large examples.

PBT cannot replace unit tests, however, especially with regards to edge cases. The odds of randomly generating a very specific case are low, so those should be taken care of manually. That said, the library does make some effort to generate "small" cases that often contribute to edge cases, for example the number zero or the empty list. A property passing also does not guarantee that a function did the right thing, even for the input that was tested. We saw in Example 1 that `badReverse2` passed our property even though it did not reverse the list. Thus multiple properties are often required, and some additional thought should go into considering what incorrect implementations the properties may allow. This is simplified somewhat in the case of Haskell's purity, because we only care about the inputs and outputs to the function and no side effects (Testing functions within the `IO` or `ST` monad was beyond my exploration for this assignment).

Thus incorporating PBT is useful in terms of efficiency for writing and testing code, as well as for generating massive amounts of test cases and possibly "thinking of" something that a human unit-tester might not. However using _only_ PBT is probably a bad idea, since the assertions it makes about the functions being tested are less strong than unit tests.