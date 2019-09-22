# lore

A library for procedurally generated text, for use in games, Twitter bots, and other fun things like [NaNoGenMo](https://github.com/NaNoGenMo/). Name comes from the original idea of a building a "lore engine" into a game, to provide textual backstories on the fly.

The three components of the system are a [Tracery](tracery.io)-inspired substitution system, a nouns/object system, and an action system for defining what actions can happen to those nouns. Events generated by the noun and action system use the grammar (text substitution) system to turn events into narrative. (The nouns and actions systems are inspired by [Sea Duck](https://github.com/aparrish/seaduck), to give due credit, as well as the Component-Entity-Systems pattern used in game engines.)

Lore is written in Fennel and can be used in Lua projects as well. To use in a Lua project, see the [Embedding docs for Fennel](https://fennel-lang.org/tutorial#embedding).


## Usage

### Text expansion / generation

Simple example in Fennel:

```clojure
(local lore (:require lore))

(local grammar {"#name#" ["Hagar" "Conan" "Attila" "Gunthur" "Genghis"]
	        "#title#" ["Sad" "Terrible" "Strong" "Weak" "Feeble"]
		"#origin#" "I am #name# the #title#."})

(for [i 1 5]
  (print (lore.generate-one grammar)))
```

Will output something like:

```
I am Attila the Feeble.
I am Hagar the Sad.
I am Hagar the Strong.
I am Gunthur the Terrible.
I am Conan the Weak.
```


### Nouns and actions

Nouns are simply tables (hashes) with keys and values on them, for setting up "things" that will be useful in your narrative. Nouns can hold values that get updated as the narrative progresses. Here's an example of a person and a banana, as nouns:

```clojure
(local nouns [{:name "Ronald"
	       :hungry-percent 50
	       :person true}
	      {:name "banana"
	       :food true
	       :eaten false}])
```

Often, nouns will have a name field, but it is not required. The attributes on each noun can be used directly for substitution into the grammar of each action.

Actions are a series of functions for manipulating nouns. Actions are implemented as tables that contain at least a filter function (called `filter-fn`) and an `update` function. The `update` function takes as parameters the action that called it and an entity that matched its filter, and can update the state of the scene and/or the entity. Any strings returned by the `update` function get appended to the scene's lines, which are generally used as the narrative output. To pass custom attributes to the grammar, generally pulled from the nouns involved, use the `expand-template` function.

As a shorthand, instead of specifying a filter function, you can use `filter-tags` to list which keys on the entity you want to be present to invoke the action. This saves you from writing the `filter-fn` yourself when tags are sufficient.

For example,

```clojure
(local actions [{:name "eats"
		 :filter-tags [:person]
		 :update (fn [action e]
		           (tset e :hungry false)
			   (lore.expand-template action "#origin# {:name e.name}))
		 :grammar {"#origin#" "#name# eats."}}])
```

An action that writes its own `filter-fn` rather than using `filter-tags` will look like:

```clojure
{:name "eats"
 :filter-fn (fn [e] (and e.hungry-percent) (> e.hungry-percent 75))
 :update (fn [action e] (tset e :hungry-percent (- e.hungry-percent 50)) (lore.expand-template action "#origin#" {:name e.name :hungrypercent e.hungry-percent}))
 :grammar {"#origin#" "#name# eats a banana and is now #hungrypercent#% hungry."}
```

The last concept needed is the scene, or the world. The scene is another table with the lists of `nouns` and `actions` set on it. It must be set up by the `prepare-scene` function, which registers some functions and checks data is consistent.

To perform one round of actions, simply call the `tick` function on the scene. Actions are called in the order that they are defined in the list. Repeated ticks will generate more and more lines in `scene.lines`.

<!-- See [lore example](https://github.com/mathias/lore-example) for an example of Lore used as a history generator for a made-up game. -->

### Putting it all together

Here's a nontrivial example, ported from [Seaduck's Example 4: Rooms with Objects](https://github.com/aparrish/seaduck#examples). The source is in the `example.fnl` file.

```clojure
(local nouns [{:name "kitchen" :room true}
              {:name "living room" :room true}
              {:name "study" :room true}
              {:name "Max" :person true :has-drink false :currently-in "living room"}
              {:name "Rory" :person true :has-drink false :currently-in "study"}
              {:name "coffee" :drink true :currently-in "kitchen"}
              {:name "tea" :drink true :currently-in "kitchen"}])

(local actions [{:name "take"
                 :filter-fn (fn [e]
                              (and e.person
                                   (not e.has-drink)
                                   ;; ensure there is a drink in the room
                                   (lume.any e.scene.nouns (fn [n] (and n.drink (= e.currently-in n.currently-in))))))
                 :update (fn [action person]
                             (let [drinks-for-room (lume.filter person.scene.nouns (fn [n] (and n.drink (= person.currently-in n.currently-in))))
                                   drink (lume.first drinks-for-room)]
                               (tset person :has-drink true)
                               (tset drink :currently-in nil)
                               (lore.expand-template action "#origin#" {:person person.name :drink drink.name})))
                 :grammar {"#origin#" ["#person# took #drink#." "'Oh hey, #drink#!' said #person#, and picked it up."]}}
                {:name "move"
                 :filter-fn (fn [e] (and e.person
                                         e.currently-in))
                 :update (fn [action person]
                             (let [currently-in person.currently-in
                                   destinations (lume.filter person.scene.nouns (fn [e] (and e.room (not (= e.name currently-in)))))
                                   chosen-destination (lume.randomchoice destinations)]
                               (tset person :currently-in chosen-destination.name)
                               (lore.expand-template action "moveto" {:name person.name :room chosen-destination.name})))
                 :grammar {"moveto" ["After awhile, #name# went to #room#."
                                     "#name# decided to go into the #room#."]}}
                {:name "work"
                 :filter-fn (fn [e] (and e.person (= e.currently-in "study") e.has-drink))
                 :update (fn [action person] (lore.expand-template action "isworking" {:name person.name}))
                 :grammar {"isworking" ["#name# typed furiously on their laptop."
                                        "#name# was taking notes while reading a book from the library.",
                                        "#name# sighed as they clicked 'Send' on another e-mail."]}}
                {:name "play video games"
                 :filter-fn (fn [e] (and e.person (= e.currently-in "living room")))
                 :update (fn [action person] (lore.expand-template action "playgames" {:name person.name}))
                 :grammar {"{{videoGame}}" ["Destiny 2" "Splatoon 2" "Skyrim" "Zelda" "Bejeweled"]
                           "playgames" ["#name# sat down to play {{videoGame}} for a while."
                                        "#name# decided to get a few minutes of {{videoGame}} in."
                                        "#name# turned on the video game console. 'Ugh I love {{videoGame}} so much,' said #name#."]}}
                {:name "talks with"
                 :filter-fn (fn [e] (and e.person e.currently-in
                                         (lume.any e.scene.nouns (fn [b] (and b.person b.currently-in (= e.currently-in b.currently-in) (~= e.name b.name))))))
                 :update (fn [action personA]
                             (let [persons (lume.filter personA.scene.nouns (fn [e] (and e.person e.currently-in (~= personA.name e.name) (= personA.currently-in e.currently-in))))
                                   personB (lume.first persons)]
                               (lore.expand-template action "talks-with" {:personA personA.name :personB personB.name})))
                 :grammar {"#topic#" ["the weather" "the garden" "the phase of the moon" "#personA#'s family" "the books they've been reading"]
                           "talks-with" ["#personA# and #personB# chatted for a bit."
                                         "#personA# asked #personB# how their day was going."
                                         "#personB# told #personA# about a dream they had last night."
                                         "#personA# and #personB# talked for a bit about #topic#."]}}])


(local scene {:nouns nouns :actions actions})

(local world (lore.prepare-scene scene))

(for [i 1 5]
     (lore.tick world))

(each [_ line (ipairs world.lines)]
      (print line))

(print "The end.")
```

When run, it will output something like: (depending on seed value)

```
After awhile, Max went to kitchen.
Rory decided to go into the living room.
Rory sat down to play Destiny 2 for a while.
'Oh hey, coffee!' said Max, and picked it up.
Max decided to go into the living room.
Rory decided to go into the study.
Max sat down to play Skyrim for a while.
Max decided to go into the study.
After awhile, Rory went to living room.
Max typed furiously on their laptop.
Rory turned on the video game console. 'Ugh I love Skyrim so much,' said Rory.
Max decided to go into the kitchen.
After awhile, Rory went to study.
Max decided to go into the living room.
After awhile, Rory went to living room.
Max decided to get a few minutes of Bejeweled in.
Rory decided to get a few minutes of Splatoon 2 in.
Max and Rory chatted for a bit.
Rory and Max talked for a bit about the phase of the moon.
The end.
```

## TODO

- [ ] Add an English modifiers function like [Tracery](tracery.io) has.
- [x] Add the ability to track "objects" (nouns) and have arbitrary attributes on them, to further make sophisticated generators.
- [ ] Write validation (data linter) functions `validate-actions` and `validate-nouns`.
- [ ] Unit tests.
- Future functionality ideas:
  - [ ] Actions that can require two or more entities in the world to be in a certain state, and then pass those matched entities to the update fn.
  - [ ] Don't always trigger all actions that can possibly match -- either actions have a percentage likelihood of happening, or they match only one noun randomly each round, but not all nouns.
  - [ ] Keep track of the number of "ticks" on `scene` even if they aren't used by any functions now -- other things could use it, including a goal function.
  - [ ] Allow one action to call another from inside the update function. Not sure whether this would still require the filter-fn to be true or would bypass filter-fn.
  - [ ] Relations between nouns -- `"Matt" :belongs_to "FactionName"` and so on. Relations would allow more complex filters or actions.
  - [ ] A goal state function which ends the scene when reached.
  - [ ] Constraint solver type functionality to help find a set of actions that leads to the goal state.

## License

Copyright (c) 2019 Matt Gauger

MIT License

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.