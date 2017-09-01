
Here are contained a few toy examples of how powerful abstracting over monads
can be in HEP.
I'm not going to discuss haskell syntax carefully here: sorry, and I will leave
a lot of details (e.g. the idea behind monads) for a future post.
What I _will_ recommend now is that you think of any context which is composable
as forming a monad.
Don't worry: it's not an easy concept, but I hope the examples below at least
_motivate_ how these abstractions can be _useful_ in a real setting.
Oh, and this is a literate haskell file, so you should be able to load it into
ghci and run the examples like so.

ghci HEPExample.lhs

Let's start with a module delcaration and some imports:

\begin{code}
module HEPExample where

import           Control.Monad.Trans.Maybe
import           Data.Functor.Identity
import           Data.Monoid               (Product (..))
\end{code}

Then let's define an "Event'" data type which contains only two pieces of
information: an electron pT and a muon pT.

\begin{code}
data Event' = Event' { elPt' :: Double, muPt' :: Double }

evt' :: Event'
evt' = Event' 25.5 52.4
\end{code}

Now we have a new type as well as an value inhabiting it called "evt'": piece of
cake.
If you could read the future you would know that I call this data type "Event'"
as opposed to "Event" because in a few minutes we're going to make everything a
_heck of a lot_ fancier.

Next let's define a function that takes as input an Event' and calculates an
observable: the pT sum of the two leptons in the event.

\begin{code}
sumPt' :: Event' -> Double
sumPt' evt =
  let ept = elPt' evt
      mpt = muPt' evt
  in ept + mpt
\end{code}

Woohoo!
If you've loaded this file into ghci, you can run something like

sumPt' evt'

and you should get the obvious answer back.

ok: so far so good?
I'll assume yes.
Let's make things interesting: I want my "Event" type now to be able to return
some kind of _context_ around the lepton pTs.
What do I mean by context?
Maybe I mean a pT with some scale factor correction, the pT of a lepton that may
have failed some previous identification cut, or a nominal pT with some set of
systematic variations.
All of these are possible if we redefine the "Event" data type to be
parameterized by the _type of context_!

\begin{code}
data Event m = Event { elPt :: m Double, muPt :: m Double }
\end{code}

Wow: what does this even mean?
Let's do something _silly_ to give ourselve a concrete example: let's use a
trivial context, a container that just holds onto the original value.
In haskell this is called the Identity type.
We can easily make an "Event" with a trivial context:

\begin{code}
evtId :: Event Identity
evtId = Event (Identity 25.5) (Identity 52.4)
\end{code}

The discerning reader will realize that this is exactly isomorphic to the
"evt'" we defined above: there's no new information added or removed by the
"Identity" constructors.
To the discerning reader I say, "Trust me: this is a baby step, but a baby step
in the right direction!"

So far we've overcomplicated our "Event'" type for nothing; let's go farther and
overcomplicate the "sumPt'" function.
One common way to _lift_ a "regular" function like "sumPt'" to a "contextified"
function is to use haskell's "do-notation".
In the end this is really just syntactic sugar, but like many things in the
haskell world, it's well-motivated by some quite nice (if quite abstract)
mathematics.
One rule of thumb to perform this function _lifting_ is to replace "let"
bindings by "monadic assignments" (I put this in quotes because I don't like the
word "assignment" very much, but I can't think of anything better) and make sure
to "return" the final value, like so:

\begin{code}
sumPt :: Monad m => Event m -> m Double
sumPt evt = do
  ept <- elPt evt    -- "monadic assignment"
  mpt <- muPt evt    -- "monadic assignment"
  return (ept + mpt) -- "return"
\end{code}

Try it:

sumPt evtId

Hopefully that returns (effectively) the same answer that we got earlier!

Now for the real fun: what if we had a different context?!
Let's try the case that perhaps we're missing an electron or muon.
This "perhaps we have something, perhaps not" is generally encoded by the
"Maybe" type in haskell.
It has two constructors: "Just" and "Nothing".
If we have a value x, then the value-with-context will be "Just x"; if the value
is missing, then we have "Nothing".
Let's try defining some "Event"s that might be missing some leptons.

\begin{code}
evtElMu, evtElNoMu, evtNoElMu :: Event Maybe

evtElMu = Event (Just 25.5) (Just 54.2)

evtElNoMu = Event (Just 25.5) Nothing

evtNoElMu = Event Nothing (Just 54.2)
\end{code}

What's really great about "Maybe" is that this context is _composable_, so we
can use it in our generic monadic code!
Try it out:

sumPt evtElMu
sumPt evtElNoMu
sumPt evtNoElMu

What happened here?
Well, if we have both an electron and a muon, that's great: we have a
well-defined sumPt.
If we're missing either input to the sumPt calculation, though, well... we can't
do the calculation, so we get "Nothing" back!
Now you might say, "why don't I just use a default value".
I'll let you think carefully about that one and come up with your own reasons as
to why default values are, in general, a Really Bad Idea.

Let's take a moment to bask in the glory of our code.
We wrote one function that was polymorphic in the _contexts_ an Event might have
to return, and we've seen that we can make that context trivial (Identity) or
represent some pass-or-fail algebra (Maybe).
But we know that our code is even more general than that: any composable context
("Monad") will do.

Shall we try another?
How about this: let's take on "scale factors".
What if our Event's electron and muon each come with some scale factor that
ought to be taken into account?
Well, it's clear what to do in such cases: we use the electron and muon as
normal, but we need to _multiply_ their scale factors in the computation.
We've just defined (broken record alert) yet another composable context: the
context is some additional information (the scale factor), and we compose scale
factors by multiplying them.
And by black magic all of this already lives in the base haskell libraries.


\begin{code}
type SF = Product Double

evtSF :: Event ((,) SF)
evtSF = Event (Product 1.1, 25.5) (Product 0.93, 54.2)
\end{code}

If that's not 100\% clear, "evtSF" is an "Event" whose electrons and muons have
some scale factor associated to them.
The "Product" type knows how to correctly _combine_ with itself: multiplication!
"But why is this necessary? Why can't I just use regular old Doubles for my
scale factors", you ask.
Well, because the real numbers have many ways of combining with one another:
addition, multiplication, etc.; we need to be sure that when we combine them,
they always multiply.
Let's try this out.

sumPt evtSF

SeemsGood.

ok.
Let's introduce one more wrinkle: what if I want to combine scale factors _and_
the possibility of failure?
Turns out the mathematicians are ahead of us once again: we want a _composition_
of the two contexts discussed above, and such compositions exist and are allowed
in haskell.
(Warning: this is getting pretty advanced even for me).

\begin{code}
type PhysObj = MaybeT ((,) SF)

evtPOElMu, evtPOElNoMu, evtPONoElMu :: Event (MaybeT ((,) (Product Double)))

evtPOElMu = Event (MaybeT (Product 1.1, Just 25.5)) (MaybeT (Product 0.93, Just 54.2))

evtPOElNoMu = Event (MaybeT (Product 1.1, Just 25.5)) (MaybeT (Product 0.93, Nothing))

evtPONoElMu = Event (MaybeT (Product 1.1, Nothing)) (MaybeT (Product 0.93, Just 54.2))
\end{code}

"MaybeT" is called a _monad transformer_ because it _transforms_ the scale
factor context into a context that handles both scale factors and the additional
possibility of failure.

sumPt evtPOElMu
sumPt evtPOElNoMu
sumPt evtPONoElMu

So... our single "sumPt" function is pretty powerful, no?
