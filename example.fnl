(local lume (require :lume))
(local lore (require :lore))

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
