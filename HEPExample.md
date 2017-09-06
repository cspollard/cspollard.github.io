Here are contained a few toy examples of how powerful abstracting over contexts
can be in real-life situations.
Well, at least as "real-life" as high-energy physics is.
I'm not going to discuss haskell syntax carefully here, and I will leave out
many details (e.g. the rigorous definition of `Monad`{.haskell}s).
All the same, this tutorial will hopefully convince you that there is something
to be gained by moving to a more mathematically rigorous representation of
analysis procedures, even if it's a slightly more abstract one than we usually
deal with in traditional programming languages.
(This is exactly what source code is, after all: a representation of a
mathematical procedure)

Don't worry if not everything makes sense immediately: these aren't easy
concepts to grasp the first time through.
This file is written in literate haskell, so you should be able to
copy-and-paste the text into a local file (e.g. `HEPExample.lhs`) and load it
into the haskell REPL, `ghci`, like so.

    $ ghci HEPExample.lhs

Any lines beginning with bird tracks (">") can be called from within the REPL.
(N.B.: you'll need to install the `transformers`, `primitive`, and
`mwc-probability` packages in addition to the base libraries, but I'll let
google be your friend for how to do that).

Let's start with a module declaration and some imports.

> module HEPExample where
>
> import           Control.Monad.Trans.Maybe
> import           Data.Functor.Identity
> import           Data.Monoid                   (Product (..))
> import           System.Random.MWC.Probability
> import           Control.Monad.Primitive

Then let's define an `Event`{.haskell} data type which contains only two pieces
of information: an electron pT and a muon pT.

> data Event' = Event' { elPt' :: Double, muPt' :: Double }
>
> evt' :: Event'
> evt' = Event' 25.5 54.2

Great!
Now we have a new data type (`Event'`{.haskell}) as well as an value inhabiting
it called `evt'`{.haskell}; hopefully this is straightforward so far.
If you could read the future you would know that I call this data type
`Event'`{.haskell} (emphasis on the prime) as opposed to `Event`{.haskell}
because we'll shortly be making everything a _heck of a lot_ fancier.
But let's walk before we run, shall we?

Next let's define a function that takes as input an `Event'`{.haskell} and
calculates an observable: the pT sum of the two leptons in the event.

> sumPt' :: Event' -> Double
> sumPt' evt =
>   let ept = elPt' evt
>       mpt = muPt' evt
>   in ept + mpt

Capisce?
If you've loaded this file into `ghci`, you can check that things work properly:

    HEPExample> sumPt' evt'
    77.9

By my calculation that's exactly the answer we expect.
So far so good?
I'll assume yes.

Now let's make things interesting: I want my `Event`{.haskell} type to return
some kind of _context_ around the lepton pTs.
And by "context" I mean that there is some decoration or container that my pT
lives in: perhaps there is a list of pTs or it is always accompanied by some
additional information.
Thankfully, we don't have to be specific to start: we can _parameterize_ our
`Event`{.haskell} type by the context that we will (in the future) be using.
Why do now what could be done tomorrow?

> data Event m = Event { elPt :: m Double, muPt :: m Double }

For you non-procrasinators, don't worry: we'll have some examples that are
conspicuous in high energy physics soon.

ok, so at this point let's think about what kind of thing `m`{.haskell} must be:
`m`{.haskell} actually takes `Double`{.haskell} as an _argument_, so it looks
like it could be a decoration or container around `Double`{.haskell}, i.e. a
context in which the `Double`{.haskell}s live.
Let's do something _silly_ (and yet surprisingly powerful) to give ourselves a
concrete example: let's use a trivial context, a container that just holds onto
the original value and does nothing else.
In haskell this is called the `Identity`{.haskell} type, and it has only one
constructor, also called `Identity`{.haskell}, which is nothing more than a
wrapper that contains a value.
We can easily make an `Event`{.haskell} with a trivial context:

> evtId :: Event Identity
> evtId = Event (Identity 25.5) (Identity 54.2)

The discerning reader will realize that this is isomorphic to the simple
`evt'`{.haskell} we originally defined above: there's no new information added
or removed by the `Identity`{.haskell} constructors.
To the discerning reader I say, "Trust me: this is a baby step, but a baby step
in the right direction!"

So far we've (trivially) complicated our `Event'`{.haskell} type; let's go
further and complicate the `sumPt'`{.haskell} function.
One common way to _lift_ a "regular" function like `sumPt'`{.haskell} to a
"contextified" function is to use haskell's `do`{.haskell}-notation.
In the end this is really just syntactic sugar, but like many things in the
haskell world, it's well-motivated by some quite nice (if quite abstract)
mathematics.
One rule of thumb to perform this function _lifting_ is to replace
`let`{.haskell} bindings by "monadic assignments" (I put this in quotes because
I don't like the word "assignment" very much, but I can't think of anything
better) and make sure to `return`{.haskell} the final value, like so:

> sumPt :: Monad m => Event m -> m Double
> sumPt evt = do
>   ept <- elPt evt    -- monadic assignment
>   mpt <- muPt evt    -- monadic assignment
>   return (ept + mpt) -- return

Do you see how the `do`{.haskell}-notation makes our new, fancy function
resemble our original, simple function quite closely?
Try it:

    HEPExample> sumPt evtId
    Identity 77.9

That yields (effectively) the same answer that we got earlier, which is good
because we know we didn't add or remove any information at all.
Great, we successfully did nothing!
Sarcasm aside, what is really going on here...?

Here's one way to think about it: each line in the `do`{.haskell} block yields
both a value and a context, but only the _value_ is bound to a variable (e.g.
`ept`{.haskell): we don't have a handle on the contexts at all.
But the only way this works is if we have a _context_ that is _composable_
line-by-line.
Well, that's exactly what the `Monad`{.haskell} type constraint is telling us
in the type signature of `sumPt`{.haskell}: a `Monad`{.haskell} in plain terms
is a context that composes (along with some "obvious" laws that we'll leave for
a rainy day).
With this one function we can use _any_ context that follows the
`Monad`{.haskell} laws.

What's especially clever about this setup is that we've entirely _abstracted
away_ the type of the context.
In other words, we no longer can perform any actions in our function that are
dependent on a particular context `m`{.haskell}; we can only perform actions
that _any_ composable context can handle.
How is this powerful?
Well, it means that we have to tell the compiler how each particular context
conforms to the `Monad`{.haskell} type class _exactly once_; from there we can
pass our context into any suitable function and the compiler _does the
composition for us_: there's really no way for us to mess up the composing
because we aren't handling it at all.
And you'd be surprised how many contexts meet these requirements and can
therefore be fed into this single function!

For example, let's think about the case that perhaps we're missing an electron
or muon from our `Event`{.haskell}; maybe we failed to reconstruct one of them,
or there weren't even any candidates at all.
This "perhaps we have something, perhaps not" kind of data is generally encoded
by the `Maybe`{.haskell} type in haskell.
It has two constructors: `Just`{.haskell} and `Nothing`{.haskell}.
If we have a value x, then the value-with-context will be `Just x`{.haskell};
if the value is missing, then we have `Nothing`{.haskell}.
Some people say that you can think of `Nothing`{.haskell} as similar to a
`NULL` pointer in C++ or `None` in python, but I disagree: `Nothing`{.haskell}
always has a concrete type `Maybe a`{.haskell}, where `a`{.haskell} is a type
parameter.
So `Nothing`{.haskell} of type `Maybe Int`{.haskell} is _distinct from_
`Nothing`{.haskell} of type `Maybe Double`{.haskell}; `NULL` and `None` can be
compared to _any_ pointer or object, respectively, regardless of their types.
This might seem tedious at first, but it allows us (and the compiler) to more
clearly reason about what a computation is doing.

ok, let's try defining some `Event`{.haskell}s that might be missing some
leptons.

> evtElMu, evtElNoMu, evtNoElMu :: Event Maybe
>
> evtElMu = Event (Just 25.5) (Just 54.2) -- both electron and muon identified
>
> evtElNoMu = Event (Just 25.5) Nothing   -- missing a muon
>
> evtNoElMu = Event Nothing (Just 54.2)   -- missing an electron

What's really great about `Maybe`{.haskell} is that it is a
_composable context_, so we can use it in our generic monadic code!
How do `Maybe`{.haskell}s compose?
Well, if we try to compose two `Just`{.haskell}s, then we get yet another
`Just`{.haskell}; if there are any `Nothing`{.haskell}s involoved, the
combination always yields a `Nothing`{.haskell}.

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
Now _some_ people might ask, "why don't I just use a default value in this
situation?"
I'll let you think carefully about that one and come up with your own reasons as
to why default values are, in general, a Really Bad Idea.

Shall we try another context?
How about this: let's take on "scale factors".
What if our `Event`{.haskell}'s electron and muon each come with some scale
factor that ought to be taken into account?
Well, this is clearly just another context: each value carries along with it an
extra `Double`{.haskell}, and these `Double`{.haskell}s are _multiplicative
factors_, i.e. to compose the contexts we multiply the scale factors.
We've just defined (broken record alert) yet another composable context that we
can plug into our `sumPt`{.haskell} function.
And by black magic all of this already lives in the haskell `base` libraries.

> type SF = Product Double
>
> evtSF :: Event ((,) SF)
> evtSF = Event (Product 1.1, 25.5) (Product 0.93, 54.2)

If that's not 100\% clear, `evtSF`{.haskell} is an `Event`{.haskell} whose
electrons and muons have some scale factor associated to them: we now have a
context of type

    (,) SF

which translates into a 2-tuple in which the first object is always a scale
factor (in haskell `(x, y)`{.haskell} is just syntactic sugar for the tuple
constructor `(,) x y`{.haskell}).

The `SF`{.haskell} type (actually the type-aliased `Product Double`{.haskell}
type) already knows how to correctly _combine_ with itself: multiplication!
"But why is this necessary? Why can't I just use regular old Doubles for my
scale factors", you ask.
Well, because the real numbers have many ways of combining with each another:
addition, multiplication, etc.; we need to be sure that when we combine them,
they always multiply and not one of those many other things.
Let's try this out.

    HEPExample> sumPt evtSF
    (Product {getProduct = 1.0230000000000001},79.7)

_SeemsGood_.
In fact, what's _really_ great about this is that there is never any question as
to which scale factors apply to the calculation of an observable.

    HEPExample> elPt evtSF
    (Product {getProduct = 1.1},25.5)

The `elPt`{.haskell} function only accesses the information relevant to the
electron in the event, so only the electron scale factor plays a role in the
output because, again, the scale factor here is just a context that is
automatically propagated through the computation.

ok, one more example to go (if you're still with me)...
It turns out that probability distributions form another valid, composable
context, and a very useful one at that.

> evtProb :: PrimMonad m => Event (Prob m)
> evtProb = Event (normal 25.5 4.3) (normal 54.2 11.9)

The above defines an event that, instead of having particular values for the
electron and muon pTs, holds probability distributions for each of them.
In this case the electron (muon) pT is normally distributed around a mean of
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
One thing we can do _for sure_, though, is sample this distribution:

    HEPExample> withSystemRandom . asGenIO . samples 10 $ sumPt evtProb
    [94.11359301538116,84.77993589158864,92.82674080235154,61.9462895476663,92.13224164142169,88.92614222298857,71.01504548041477,77.92693906507571,109.10381978490473,97.80981038489617]

Now don't get confused by
"`withSystemRandom . asGenIO . samples 10 $`{.haskell}"; that is simply saying,
"please sample the following distribution ten times with the system random
number generator."
What comes back is, of course, the interesting bit: ten samples of the
`sumPt`{.haskell} distribution given the distributions of the lepton pTs.
Pretty slick, huh?

Of course in practice, our event data types and analysis procedures are much
more complicated than `Event`{.haskell} and `sumPt`{.haskell}.
Thankfully this is in no way a problem.

< evtInvM :: (Monad m, Alternative m) => Event m -> m Double
< evtInvM evt = do
<   es <- electrons evt    -- get electrons
<   guard $ length es == 1 -- require exactly 1 electron
<
<   ms <- muons evt        -- get muons
<   guard $ length ms == 1 -- require exactly 1 muon
<
<   js <- jets evt         -- get jets
<   guard $ length js == 2 -- require exactly 2 jets
<
<   return . invM . mconcat $ js ++ es ++ ms

The above code checks for exactly one electron, one muon, two jets,
then calculates the invariant mass of these objects.
Any scale factors, systematic variations, or possible failure of cuts can be
automatically propagated through the code just by using an appropriate
_context_!
I want to stress here that the type signature of `evtInvM`{.haskell} already
encodes all of this information: it says you need a composable context
(`Monad`{.haskell}) that supports the possibility of failure
(`Alternative`{.haskell}); as long as you have these, then you can use this
function.

I think that's enough for today!
Hopefully I've given you a taste of what can be achieved with these very
powerful abstractions that mathematicians have known about for years, but which
only recently have seen use in much "day-to-day" code.
Indeed, many more common idioms fit in the "composable context" box, including
another ubiquitous one in HEP: systematic variations of values.
In fact, so-called "stacks" of these contexts quite often follow the required
rules, so you can construct a combined context that handles scale factors and
the possibility of failure at the same time.
And really this is just the tip of the iceberg: there are many more
interesting, _rigorous_ mathematical concepts at our disposal that already exist
and are just waiting for us to use them in analyses, and by using these concepts
we can make our analysis procedures easier to understand, easier to refactor,
and generally more bug-free!
