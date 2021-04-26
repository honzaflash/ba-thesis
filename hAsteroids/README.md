# hAsteroids

A clone of Atari Asteroids using the Apecs library

This Haskell clone of Asteroids does not strive
to be as accurate of a clone as possible
but I got decently close.

The main difference is that ufos only
come from the left and can't move up and down.
And there is many other details, like different
hitboxes, ufo spawn frequency and many other
some of which I'm sure I'm not even aware of.

It also lacks sound effects.

## About the game implementation

Main goal here is to show how perhaps
surprisingly simple can a game in Haskell be
with Apecs.

Here I have embraced the everything devouring monad
and added my own Reader monad for resources like
the texture maps, the renderer or stateful random generators.

I am not a huge fan of how everything is hidden
and *any* function could technically do *anything*
with the game world. Imo it is nice and it
makes things simpler but it also makes the code
less expressive and less protected by the type system.

Perhaps there is some `State` kung-fu I could
learn to make some functions more isolated
by givign up the `WithResources` reader monad.
But I figured I might as well lean into the monads
and then use this extreme as an example in my thesis
of how too many monads can be bad for your code.

