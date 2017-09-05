Here are contained a few toy examples of how powerful abstracting over contexts
can be in real-life situations.
Well, as "real-life" as high-energy physics is, at least.
I'm not going to discuss haskell syntax carefully here, and I will leave out
many details (e.g. the rigorous ideas behind `Monad`{.haskell}s).
All the same, this tutorial will hopefully convince you that there is something
to be gained by moving to a more mathematically rigorous representation of
analysis procedures, even if it's a slightly more abstract one than we usually
deal with in traditional programming languages.

Don't worry if not everything makes sense immediately: these aren't easy
concepts for most people to grasp the first time through.
Oh, and this is a literate haskell file, so you should be able to copy-and-paste
the text into a file (e.g. `HEPExample.lhs`) and load it into `ghci` like so.

    $ ghci HEPExample.lhs

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

Now we have a new type as well as an value inhabiting it called
`evt'`{.haskell}: hopefully this is straightforward so far.
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

So far so good?
I'll assume yes.

Let's make things interesting: now I want my `Event`{.haskell} type now to
return some kind of _context_ around the lepton pTs.
What do I mean by context?
I mean that there is some decoration or container that my pT lives in: perhaps
there is a list of pTs or it is always accompanied by some additional
information.
Thankfully, we don't have to be specific in the beginning: we can _parameterize_
our `Event`{.haskell} type by the context that we will (in the future) be using.
Why do now what could be done tomorrow?

> data Event m = Event { elPt :: m Double, muPt :: m Double }

For you non-procrasinators, don't worry: we'll have some examples that are
conspicuous in high energy physics soon.

ok so at this point let's think about what kind of thing `m`{.haskell} must be:
`m`{.haskell} actually takes `Double`{.haskell} as an _argument_, so it looks
like it could be a decoration or container around `Double`{.haskell}, i.e. a
context in which the `Double`{.haskell}s live.
Let's do something _silly_ (and yet surprisingly powerful) to give ourselves a
concrete example: let's use a trivial context, a container that just holds onto
the original value and does nothing else.
In haskell this is called the `Identity`{.haskell} type, and it's nothing more
than a wrapper that contains a value.
We can easily make an `Event`{.haskell} with a trivial context:

> evtId :: Event Identity
> evtId = Event (Identity 25.5) (Identity 54.2)

The discerning reader will realize that this is isomorphic to the
`evt'`{.haskell} we defined above: there's no new information added or removed
by the `Identity`{.haskell} constructors.
To the discerning reader I say, "Trust me: this is a baby step, but a baby step
in the right direction!"

So far we've (trivially) complicated our `Event'`{.haskell} type; let's go
farther and complicate the `sumPt'`{.haskell} function.
One common way to _lift_ a "regular" function like `sumPt'`{.haskell} to a
"contextified" function is to use haskell's "do-notation".
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

Try it:

    HEPExample> sumPt evtId
    Identity 77.9

That yields (effectively) the same answer that we got earlier!
Great, but what is really going on here...?

Here's one way to think about it: each line in the `do`{.haskell} block yields
both a value and a context, but only the _value_ is bound to a variable (e.g.
`ept`{.haskell}); the _contexts_ are implicitly composed line by line: we don't
have a handle on the context itself.
But the only way this works is if we have a _context_ that is _composable_
line-by-line.
Well, that's exactly what the `Monad`{.haskell} type constraint is telling us
in the type signature of `sumPt`{.haskell}: a `Monad`{.haskell} in plain terms
is a contexts that composes (along with some "obvious" laws that we'll leave for
a rainy day).
With this one function we can use _any_ context that follows the
`Monad`{.haskell} laws.

What's especially clever about this setup is that we've entirely _abstracted
away_ the type of the context.
In other words, we no longer can perform any actions that are dependent on
particular contexts; we can only perform actions that _any_ composable context
can handle.
How is this powerful?
Well, it means that we have to tell the compiler how each particular context
conforms to the `Monad`{.haskell} type class _exactly once_; from there we can
pass our context into any suitable function and the compiler _does the
composition for us_.
And you'd be surprised how many contexts meet these requirements and can
therefore be fed into this single function!

I won't talk here about the Monad laws or how one builds a context that
satisfies them (I promise the world doesn't need Yet Another Monad Tutorial),
but in what follows I'll walk through several contexts (other than the
trivial one) that are useful in high energy physics.

For example, let's think about the case that perhaps we're missing an electron
or muon from our `Event`{.haskell}; maybe we failed to reconstruct one of them
or there weren't even any candidates at all.
This "perhaps we have something, perhaps not" kind of data is generally encoded
by the `Maybe`{.haskell} type in haskell.
It has two constructors: `Just`{.haskell} and `Nothing`{.haskell}.
If we have a value x, then the value-with-context will be `Just x`{.haskell};
if the value is missing, then we have `Nothing`{.haskell}.
Some people say that you can think of `Nothing`{.haskell} as similar to a
`NULL` pointer in C++ or `None` in python, but I disagree: any
`Nothing`{.haskell} always has a concrete type `Maybe a`{.haskell}, where
`a`{.haskell} is a type parameter, so `Nothing`{.haskell} of type
`Maybe Int`{.haskell} is _distinct from_ `Nothing`{.haskell} of type
`Maybe Double`{.haskell}.
This might seem tedious at first, but it allows us (and the compiler) to more
clearly reason about what a computation is doing.

ok: let's try defining some `Event`{.haskell}s that might be missing some
leptons.

> evtElMu, evtElNoMu, evtNoElMu :: Event Maybe
>
> evtElMu = Event (Just 25.5) (Just 54.2) -- both electron and muon identified
>
> evtElNoMu = Event (Just 25.5) Nothing   -- missing a muon
>
> evtNoElMu = Event Nothing (Just 54.2)   -- missing an electron

What's really great about `Maybe`{.haskell} is that this context is
_composable_, so we can use it in our generic monadic code!
If we try to compose two `Just`{.haskell}s, then we get yet another
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

which translates into a 2-tuple in which the first part is always a scale
factor (in haskell `(x, y)`{.haskell} is just syntactic sugar for
`(,) x y`{.haskell}).

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

only accesses the information relevant to the electron in the event, so only the
electron scale factor plays a role in the output because, again, the scale
factor here is just a context that is automatically propagated through the
computation.

ok, two more examples to go (if you're still with me)...
It turns out that probability distributions form another valid, composable
context, and a very useful one at that.

> evtProb :: PrimMonad m => Event (Prob m)
> evtProb = Event (normal 25.5 4.3) (normal 54.2 11.9)

The above defines an event that, instead of having definite values for the
electron and muon pTs, holds probabilities for each of them.
In this case the electron (muon) pT is normally distributed around a mean of
25.5 (54.2) and standard deviation of 4.3 (11.9).

(For now we'll not discuss the idea behind the `PrimMonad`{.haskell} type
class; just trust me when I say that the `Prob m a`{.haskell} type represents
probability densities as a function of values of type `a`{.haskell}. So
`Prob m Int`{.haskell} means each `Int`{.haskell} has a probability assigned to
it.

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

Now don't get confused by `withSystemRandom . asGenIO . samples 10`{.haskell};
that is simply saying, "please sample the following distribution ten times with
the system random number generator" and nothing more.
What comes back is, of course, the interesting bit: ten samples of the
`sumPt`{.haskell} distribution given the distributions of the lepton pTs.
Pretty slick, huh?

I think that's enough for today!
Hopefully I've given you a taste of what can be achieved with these very
powerful abstractions.
Honestly, this is just the tip of the iceberg: there are so many more
interesting, _rigorous_ mathematical concepts at our disposal that already exist
and are just waiting for us to use them.
