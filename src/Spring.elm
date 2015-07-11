module Spring
  ( Spring
  , create
  , current
  , map
  , map2
  , map3
  , map4
  , map5
  , andMap
  , connect
  , setDestination
  , connectMany
  , animate
  , animateNested
  ) where
{-| Module for spring-based animations in Elm.

# Create Springs
@docs Spring, create

# Query and Modify Springs
@docs current, setDestination

# Animate Springs
@docs animate

# Connect Springs
@docs connect, connectMany

# All the maps!
@docs map, map2, map3, map4, map5, andMap
-}

import Time exposing (Time)
import Focus exposing (Focus)
{-| Main Spring Type.
A spring's behavior is defined by its stiffness and damping parameters.
-}
type alias Spring a =
  { stiffness   : Float
  , damping     : Float
  , position    : a
  , velocity    : a
  , destination : a
  }

{-| Create a Spring Float given values for stiffness and damping.
The Spring Float is the basic type of Spring and is the only one that
can be animated with the `animate` functon. You can use this as a building
block to create more complex Springs using functions like `map` or `andMap`.
-}
create : Float -> Float -> Spring Float
create stiffness damping =
  { stiffness   = stiffness
  , damping     = damping
  , position    = 0
  , velocity    = 0
  , destination = 0
  }


{-| Get the current value of the spring.
-}
current : Spring a -> a
current {position} = position

{-| Map a function onto a spring.
-}
map : (a -> b) -> Spring a -> Spring b
map f spring =
  { spring | position    <- f spring.position
           , velocity    <- f spring.velocity
           , destination <- f spring.destination
  }

map2 : (a -> b -> c) -> Spring a -> Spring b -> Spring c
map2 f springA springB =
  { stiffness   = (springA.stiffness + springB.stiffness) / 2
  , damping     = (springA.damping  + springB.damping ) / 2
  , position    = f springA.position springB.position
  , velocity    = f springA.velocity springB.velocity
  , destination = f springA.destination springB.destination
  }


map3 : (a -> b -> c -> d) -> Spring a -> Spring b -> Spring c -> Spring d
map3 f springA springB springC =
  f
  `map`    springA
  `andMap` springB
  `andMap` springC

map4 : (a -> b -> c -> d -> e) -> Spring a -> Spring b -> Spring c -> Spring d -> Spring e
map4 f springA springB springC springD =
  f
  `map`    springA
  `andMap` springB
  `andMap` springC
  `andMap` springD

map5 : (a -> b -> c -> d -> e -> f) -> Spring a -> Spring b -> Spring c -> Spring d -> Spring e -> Spring f
map5 f springA springB springC springD springE =
  f
  `map`    springA
  `andMap` springB
  `andMap` springC
  `andMap` springD
  `andMap` springE


andMap : Spring (a -> b) -> Spring a -> Spring b
andMap =
  map2 (<|)




{-| Connect two springs together. This function will modify the second spring
to have the first spring's position as its destination.
-}
connect : Spring a -> Spring a -> Spring a
connect spring1 spring2 =
  { spring2 | destination <- spring1.position }


{-| Set the destination of a spring.
-}
setDestination : a -> Spring a -> Spring a
setDestination destination spring =
  { spring | destination <- destination }

{-| Connect multiple strings end to end. The first spring's destination will be
set to the provided destination. The second spring's destination will be set to
the first spring's position. The third spring's destination will be set to
the second spring's position. And so on...
-}
connectMany : a -> List (Spring a) -> List (Spring a)
connectMany destination list =
  case list of
    [] ->
      []
    s :: ss ->
      setDestination destination s :: connectMany s.position ss


epsilon = 0.0001


{-| Animate a nested field inside a spring with a focus.
-}
animateNested : Focus a Float -> Time -> Spring a -> Spring a
animateNested focus fpms spring =
  let
      fspring : Spring Float
      fspring = map (Focus.get focus) spring

      newFSpring : Spring Float
      newFSpring = animate fpms fspring

  in
      map2 (Focus.set focus) newFSpring spring



{-| Animate a spring given a framerate.

    animate framerate spring
-}
animate : Time -> Spring Float -> Spring Float
animate fpms spring =
  let
      frameRate = fpms / 1000

      fspring = -spring.stiffness * (spring.position - spring.destination)

      fdamper = -spring.damping  * spring.velocity

      a = fspring + fdamper

      newV = spring.velocity + a * frameRate
      newX = spring.position + newV * frameRate


  in
      if
          abs (newV - spring.velocity) < epsilon && abs (newX - spring.position) < epsilon
      then
          { spring | position <- spring.destination
                   , velocity <- 0
          }
      else
          { spring | position <- newX
                   , velocity <- newV
          }
