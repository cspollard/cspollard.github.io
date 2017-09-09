Putting things into context
===========================

Here are contained a few examples of how powerful abstracting over contexts can
be in real-life situations.
(Where in this case "real-life" means high energy physics)
I'll be using the haskell programming language in the examples, but I'm not
going to discuss haskell syntax carefully here, and I will leave out many
details (e.g. the rigorous definition of `Monad`{.haskell}s).
All the same, this set of examples will hopefully convince you that there is
something to be gained by moving to a more mathematically rigorous
representation of analysis procedures, albeit a slightly more abstract one than
we are used to in traditional programming languages.
Source code is, after all, a representation of a mathematical procedure, and
often a rather difficult one to understand.
Don't worry if not everything contained here makes sense immediately; these
aren't easy concepts to grasp the first time through.

This file is written in literate haskell, so you should be able to
copy-and-paste the text into a local file (e.g. `HEPExample.lhs`) and load it
into the haskell REPL, `ghci`, like so.

< $ ghci HEPExample.lhs

Any lines beginning with "HEPExample>" can be called from within the REPL.
(N.B. you'll need to install the `primitive` and `mwc-probability` packages in
addition to the base libraries, but I'll let google be your friend for
instructions).

Let's start with a module declaration and some imports. (Don't dwell on them if
you're unfamiliar with haskell.)

> module HEPExample where
>
> import           Control.Applicative           (Alternative(..))
> import           Control.Monad                 (guard)
> import           Control.Monad.Primitive
> import           Data.Functor.Identity
> import           Data.Monoid                   (Product (..))
> import           System.Random.MWC.Probability

Then let's define an `Event'`{.haskell} data type which contains only two pieces
of information, an electron pt and a muon pt.

> data Event' = Event' { elPt' :: Double, muPt' :: Double }
>
> evt' :: Event'
> evt' = Event' 25.5 54.2

Now we have a new data type (`Event'`{.haskell}) with one constructor (also
`Event'`{.haskell}) as well as a value inhabiting it (`evt'`{.haskell}).
In this example the electron and muon pts are 25.5 and 54.2, respectively.
(You'll have to forgive me for breaking that fundamental rule of always
specifying units)
If you could read the future you would know that I call this data type
`Event'`{.haskell} (emphasis on the prime) as opposed to `Event`{.haskell}
because we'll shortly be making it a lot _fancier_.
But let's walk before we run, shall we?

Next we define a function that takes as input an `Event'`{.haskell} and
returns an observable, the pt sum of the two leptons in the event.

> sumPt' :: Event' -> Double
> sumPt' evt =
>   let ept = elPt' evt
>       mpt = muPt' evt
>   in ept + mpt

If you've loaded this file into `ghci`, you can check that things work properly:

    HEPExample> sumPt' evt'
    79.7

By my calculation that's exactly the answer we expect.
So far so good.

Now let's make things more interesting: I want my `Event`{.haskell} type to
return some kind of _context_ around the lepton pts.
By "context" I mean that there is some decoration or container that my pt lives
in; there could be a list of pts rather than just one, or the pt could always be
accompanied by some additional information.
Thankfully, we don't have to be specific to start: we can _parameterize_ our
`Event`{.haskell} type by the context that we will (in the future) be using.

> data Event m = Event { elPt :: m Double, muPt :: m Double }

ok, so at this point let's discuss what kind of thing `m`{.haskell} must be.
`m`{.haskell} takes `Double`{.haskell} as an _argument_, so it could be a
decoration or container that holds many types of things: `m a`{.haskell} would
denote an `m`{.haskell} that holds things of type `a`{.haskell}; in this case
that type is `Double`{.haskell}.
(There is some similarity between haskell type parameters and template
parameters in the C++ STL if that helps you understand what we're doing here,
but just remember that there are important differences that we won't go into.)

Let's do something _silly_ to give ourselves a concrete example: let's use a
trivial context, a container that just holds on to the original value and does
nothing else.
In haskell this is called the `Identity`{.haskell} type, and it has only one
constructor, also called `Identity`{.haskell}, which is nothing more than a
wrapper that contains one value.
We can easily make an `Event`{.haskell} with a trivial context:

> evtId :: Event Identity
> evtId = Event (Identity 25.5) (Identity 54.2)

The discerning reader will realize that this is isomorphic to the
`evt'`{.haskell} we originally defined above; there's no new information added
or removed by the `Identity`{.haskell} constructors.
To the discerning reader I say, "Trust me: this is a baby step, but a baby step
in the right direction!"

So far we've (trivially) complicated our original `Event'`{.haskell} type; let's
upgrade the `sumPt'`{.haskell} function so that it can use our new kind of
event.
One common way to _lift_ a "regular" function like `sumPt'`{.haskell} to a
"contextified" function is to use haskell's `do`{.haskell}-notation.
`do`{.haskell}-notation is really just syntactic sugar, but it's well-motivated
by some quite nice (if quite abstract) mathematics.
One should replace `let`{.haskell} bindings by "assignments" (I put this in
quotes because I don't like the word "assignment" very much but can't think of
anything better), which are denoted by `<-`{.haskell}, and make sure to
`return`{.haskell} the final value, like so.

> sumPt :: Monad m => Event m -> m Double
> sumPt evt = do
>   ept <- elPt evt    -- assignment
>   mpt <- muPt evt    -- assignment
>   return (ept + mpt) -- return

You'll notice that the `do`{.haskell}-notation makes our new, fancy function
resemble the original, simple function quite closely: the meaning of what we're
doing can be read easily straight from the code!

Try it:

    HEPExample> sumPt evtId
    Identity 79.7

That yields the same answer that we got earlier, just in the
`Identity`{.haskell} context, which is good because we know we didn't add or
remove any information at all.
Great, we successfully did nothing!
Sarcasm aside, let's dig into what is really going on.

Here's one way to think about our lifted function: each line in the
`do`{.haskell} block yields both a value and a context, but only the _value_ can
be bound to a variable (e.g. `ept`{.haskell}, which has the type
`Double`{.haskell}) through assignment.
We don't have a handle on the contexts at all!
The only way this works is if we have a context that is _composable_, i.e. there
is one and _only_ one well-defined way to combine the contexts from two
consecutive lines.
And this is exactly what happens: contexts are composed line-by-line, so
when we `return`{.haskell} our new observable, it comes with the combined
context of all previous lines.

In fact the `Monad`{.haskell} type constraint is telling us exactly that in the
type signature of `sumPt`{.haskell}.
A `Monad`{.haskell} in informal terms is a context that composes, following some
"obvious" laws that we'll leave for another day.
The type declaration of our function says that it works with _any_ context that
adheres to the `Monad`{.haskell} laws.

What's especially clever about this setup is that we've entirely _abstracted
away_ the context itself.
In other words, we no longer can perform any actions in our function that are
dependent on a particular context; we can only perform actions that _any_
composable context can handle.
How is this powerful?
Well, it means that for each context we want to pass through our function, we
have to show (in advance and exactly once) how this context conforms to the
`Monad`{.haskell} type class.
Once we've done this, we can pass our context into any suitable function, and
any necessary context composition will take place automatically.
Put another way, as long as the `Monad`{.haskell} instance is correctly
implemented for a context `m`{.haskell}, functions that abstract over all
`Monad`{.haskell}s _cannot_ incorrectly propagate the context.
Imagine the number of bugs we've just quashed!

ok, admittedly we've quashed no bugs so far since we've only seen one, trivial
`Monad`{.haskell}.
In that case, the `Identity`{.haskell} context contains no information in
addition to the value it contains, so the composition of two
`Identity`{.haskell}s is just another `Identity`{.haskell}.
For a more complex example, let's think about what happens if perhaps we're
missing an electron or muon from our `Event`{.haskell}; for instance, maybe we
failed to reconstruct one of them.
This "perhaps we have something, perhaps not" kind of data is usually
represented by the `Maybe`{.haskell} type; it has two constructors:
`Just`{.haskell} and `Nothing`{.haskell}.
If we have a value `x`{.haskell}, then the value-with-context will be
`Just x`{.haskell}; if the value is missing, then we have `Nothing`{.haskell}.

You might hear that `Nothing`{.haskell} is similar to a `NULL` pointer in
C++ or `None` in python, but there is an important difference:
`Nothing`{.haskell} always has a concrete type `Maybe a`{.haskell}, where
`a`{.haskell} is a type parameter.
In other words, `Nothing`{.haskell} of type `Maybe Int`{.haskell} is _distinct
from_ `Nothing`{.haskell} of type `Maybe Double`{.haskell}.
`NULL` and `None` can be compared to _any_ pointer or object, respectively,
regardless of their types; `Nothing`{.haskell} cannot.
This might seem tedious at first, but it allows us (and the compiler) to more
clearly reason about the results of a computation.

One use-case of `Maybe`{.haskell} is as the result of some cut: if, during the
computation of an observable `x`{.haskell} of type `a`{.haskell}, some criterion
was not met, then we'll get `Nothing`{.haskell} back; otherwise we'll get
`Just x`{.haskell}.
Let's define a few `Event`{.haskell}s that might be missing some leptons.

> evtElMu, evtElNoMu, evtNoElMu :: Event Maybe
>
> evtElMu = Event (Just 25.5) (Just 54.2) -- both electron and muon identified
>
> evtElNoMu = Event (Just 25.5) Nothing   -- missing a muon
>
> evtNoElMu = Event Nothing (Just 54.2)   -- missing an electron

Before plugging our `Event Maybe`{.haskell}s into our `sumPt`{.haskell}
function, let's discuss how `Maybe`{.haskell}s compose.
Well, if we try to combine two `Just`{.haskell}s, then we get yet another
`Just`{.haskell}; if there are any `Nothing`{.haskell}s involoved, the
combination always yields another `Nothing`{.haskell}.
Translating back to "cut" analagy: as soon as any cut fails, the whole
computation ends in failure, but if every fut passes, then we get a value back.

Try it out:

    HEPExample> sumPt evtElMu
    Just 79.7

    HEPExample> sumPt evtElNoMu
    Nothing

    HEPExample> sumPt evtNoElMu
    Nothing

What happened here?
Well, if we have both an electron and a muon, that's great: we have a
well-defined `sumPt`{.haskell}.
If we're missing either input to the `sumPt`{.haskell} calculation, though,
well... we can't do the calculation, so we get `Nothing`{.haskell} back.
The neat thing here is that we don't have to litter our code with
`if`{.haskell} statements checking that everything we need is there (look back
at the definition of `sumPt`{.haskell} and you will find no mention of cuts!);
we just define the observable and the relevant requirements, and if at any point
some cut fails, `Nothing`{.haskell} will immediately be returned.

Shall we try another context?
Let's take on "scale factors".
What if our `Event`{.haskell}'s electron and muon each come with some scale
factor that ought to be taken into account, i.e. we believe that leptons like
these appear in the data more or less often than in the simulation?
Well, this is clearly just another kind of context: each value carries along
with it an extra `Double`{.haskell}, and these `Double`{.haskell}s are
_multiplicative factors_, i.e. to compose the contexts we multiply the scale
factors.
We've just defined (broken record alert) another composable context that we can
plug into our `sumPt`{.haskell} function, and by black magic all of this already
lives in the haskell `base` libraries (just like `Maybe`{.haskell} did for
cuts).

> type SF = Product Double
>
> evtSF :: Event ((,) SF)
> evtSF = Event (Product 1.1, 25.5) (Product 0.93, 54.2)

If that's not 100\% clear, `evtSF`{.haskell} is an `Event`{.haskell} whose
electrons and muons have some scale factor associated to them: we now have a
context of type `(,) SF`{.haskell}.
(`(,) SF`{.haskell} translates into a 2-tuple in which the first object is
always a scale factor. In haskell `(SF, a)`{.haskell} is just syntactic sugar
for `(,) SF a`{.haskell}.)

The `SF`{.haskell} type (which is just a type-alias for
`Product Double`{.haskell}) already knows how to correctly combine with itself:
multiplication!
Because the real numbers have many ways of combining with each another
(addition, multiplication, etc.) we need to be sure that when we combine them,
they multiply and not one of those many other things; as soon as we use the
`Product`{.haskell} type our intentions become clear.

Let's try this out.

    HEPExample> sumPt evtSF
    (Product {getProduct = 1.0230000000000001},79.7)

_SeemsGood_.
Remember how I said that since `sumPt`{.haskell} abstracts over any
`Monad`{.haskell}, we don't have to worry about contexts being combined
correctly?
What's _really_ great about this is that there is never any question as to which
scale factors apply to the calculation of an observable.

    HEPExample> elPt evtSF
    (Product {getProduct = 1.1},25.5)

The `elPt`{.haskell} function only accesses the information relevant to the
electron in the event, so only the electron scale factor plays a role in the
output because, again, the scale factor here is just a context that is
automatically propagated through the computation.
If we use `muPt`{.haskell} anywhere in the observable definition, then the muon
scale factor context will have to be folded in.
Nifty.

ok, one more example to go (if you're still with me)...
It turns out that probability distributions form another valid, composable
context, and a very useful one at that.

> evtProb :: PrimMonad m => Event (Prob m)
> evtProb = Event (normal 25.5 4.3) (normal 54.2 11.9)

The above defines an event `evtProb`{.haskell} that, instead of having
particular values for the electron and muon pts, holds probability distributions
for each of them.
In this case the electron (muon) pt is normally distributed around a mean of
25.5 (54.2) and standard deviation of 4.3 (11.9).

(For now we'll not discuss the idea behind the `PrimMonad`{.haskell} type
class; just trust me when I say that the `Prob m a`{.haskell} type represents
probability densities as a function of values of type `a`{.haskell}. So
`Prob m Int`{.haskell} means each `Int`{.haskell} has a probability assigned to
it.)

ok, let's give this a whirl:

    HEPExample> sumPt evtProb
    <interactive>:1:1: error:
    • No instance for (Show (Prob m0 Double))
        arising from a use of ‘print’
    • In a stmt of an interactive GHCi command: print it

Woops!
Something's not right here... what's going on?
Well, to answer that question, let's look at the type of
`sumPt evtProb`{.haskell}.

    HEPExample> :t sumPt evtProb
    sumPt evtProb :: PrimMonad m => Prob m Double

We're getting back a probability distribution over all possible
`Double`{.haskell}s, which is what we want...but how does one print such a
thing?
That's a great question, and one which apparently the compiler can't answer!
One thing we can do, even if we can't print it, is sample it:

    HEPExample> withSystemRandom . asGenIO . samples 10 $ sumPt evtProb
    [94.11359301538116,84.77993589158864,92.82674080235154,61.9462895476663
    ,92.13224164142169,88.92614222298857,71.01504548041477,77.92693906507571
    ,109.10381978490473,97.80981038489617]

Now don't get confused by
"`withSystemRandom . asGenIO . samples 10 $`{.haskell}"; that is simply saying,
"please sample the following distribution ten times with the system random
number generator."
What comes back is, of course, the interesting bit: ten samples of the
`sumPt`{.haskell} distribution given the distributions of the lepton pts.
Pretty slick, huh?

Of course in practice, our event data types and analysis procedures are much
more complicated than `Event`{.haskell} and `sumPt`{.haskell}.
Thankfully this is in no way a problem.
Here's a more complex example.

> data Event'' m =
>   Event''
>     { ePts :: m [Double]
>     , mPts :: m [Double]
>     , jPts :: m [Double]
>     }
>
> sumPt'' :: (Monad m, Alternative m) => Event'' m -> m Double
> sumPt'' evt = do
>   epts <- ePts evt         -- access the event's electrons
>   guard $ length epts == 1 -- require exactly 1 electron
>
>   mpts <- mPts evt         -- access the event's muons
>   guard $ length mpts == 1 -- require exactly 1 muon
>
>   jpts <- jPts evt         -- access the event's jets
>   guard $ length jpts == 2 -- require exactly 2 jets
>
>   return . sum $ jpts ++ epts ++ mpts

The above code checks for exactly one electron, one muon, and two jets,
then it calculates the pt sum of these objects.
Any scale factors, systematic variations (which are just approximate probabilty
distributions), or possible failure of cuts can be automatically propagated
through the code just by using an appropriate _context_!
I want to stress here that the type signature of `evtInvM`{.haskell} already
encodes all of this information: it says you need a composable context
(`Monad`{.haskell}) that additionally supports the possibility of failure
(`Alternative`{.haskell}); as long as you have these, then you can use this
function with your context.
We can give it a try on a simple context (`Maybe`{.haskell}) like so:

    HEPExample> sumPt'' $ Event'' (Just [28]) (Just [50]) (Just [10, 15])
    Just 103.0

    HEPExample> sumPt'' $ Event'' (Just [28]) Nothing (Just [10, 15])
    Nothing

    HEPExample> sumPt'' $ Event'' (Just [28]) (Just [50]) (Just [10, 15, 20])
    Nothing

Check for yourself if events with the correct numbers of objects return a
non-`Nothing`{.haskell} value.

Whew: I think that's enough for today.
Hopefully I've given you a taste of what can be achieved with these very
powerful abstractions that mathematicians and computer scientists have known
about for years.
Indeed, many more useful idioms fit in the "composable context" box.
In fact, "stacks" of composable contexts are often themselves composable
contexts.
We can, for instance, construct a combined context that handles scale factors
and the possibility of failure at the same time; no need to change the
`sumPt`{.haskell} function at all!

And this is just the tip of the iceberg: there are many more interesting,
_rigorous_ mathematical concepts at our disposal that already exist, just
waiting for us to use in physics.
At the (small) price of moving away from an imperative style, we can make our
analysis procedures easier to code and, what is more important in my opinion,
easier to _understand_.
