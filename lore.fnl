(local lume (require :lume))

;; for dev
(local view (require :fennelview))
(local pp (fn [x] (print (view x))))

;; Setting up random seed
(math.randomseed (os.time))

(fn randomseed [seed]
    (math.randomseed seed))

(lambda smart-val [tbl]
        (if (= (type tbl) "table")
            (. tbl 1)
            tbl))
(lambda random-grammar-match [rule]
        (if (= (type rule) "table")
            (lume.randomchoice rule)
            rule))

(lambda generate-one [grammar ?target]
  (let [target-key (or ?target "#origin#")]
    (var target-val-copy (random-grammar-match (. grammar target-key)))
    (each [key val (pairs grammar)]
          (when (string.find target-val-copy key)
              (set target-val-copy
                   (string.gsub target-val-copy key (lume.randomchoice val)))))
    target-val-copy))

;; Might be a useful utility function to keep around. If not, delete it:
(lambda smart-concat [tbl potential-list]
        (if (= (type potential-list) "string") (table.insert tbl potential-list)
            (= (type potential-list) "table") (lume.concat tbl potential-list)))

(lambda table-contains [list item]
        (lume.any list (fn [list-item] (= list-item item))))

(lambda arrays-all-items-match [lista listb]
        (if (= (# lista) (# listb))
            (lume.all lista (fn [item] (table-contains listb item)))))

(lambda validate-actions [actions]
        (each [_ action (ipairs actions)]
              (assert action.filter-fn "Must have a filter function.")
              ))
(lambda validate-nouns [nouns] nouns)

;; Simplest one for now, more complex later:
(lambda filter-function-tags-for [filter-tags]
        (fn [e]
            (let [entity-keys (lume.keys e)]
              (lume.all filter-tags (fn [filter-key] (table-contains entity-keys filter-key))))))

(lambda prepare-actions [scene]
        (each [_ action (ipairs scene.actions)]
              (tset action :scene scene)
              (tset action :nouns scene.nouns)
              (when (not action.filter-fn)
                (when action.filter-tags
                  (tset action :filter-fn (filter-function-tags-for action.filter-tags))))))

(lambda prepare-scene [scene]
        ;; link each noun back up to the scene so that we can use that association in filter functions
        (each [_ noun (ipairs scene.nouns)]
              (tset noun :scene scene))
        (validate-nouns scene.nouns)

        (prepare-actions scene)
        (validate-actions scene.actions)
        (tset scene :lines [])
        scene)

(lambda tick [scene]
        (each [_ action (ipairs scene.actions)]
              (var matched-entities [])
              (each [_ entity (ipairs scene.nouns)]
                    (when (action.filter-fn entity)
                      ;; filter matches for this entity, perform action:
                      (let [lines (action.update action entity)]
                        (when lines
                            (table.insert scene.lines lines)))))))

(lambda entity-merge-grammar [grammar entity]
        (var tmp-table {})
        (each [k v (pairs entity)]
              (tset tmp-table (string.format "#%s#" k) [v]))
        (lume.merge grammar tmp-table))

(lambda line-for [action entity ?key]
        (let [key (or ?key "#origin#")
              combined-grammar (entity-merge-grammar action.grammar entity)]
          (generate-one combined-grammar)))

(lambda expand-template [action goal mappings]
        (generate-one (entity-merge-grammar action.grammar mappings) goal))

;; (local nouns [{:name "Steve" :hungry-percent 50}
;;               {:name "Bob" :hungry-percent 50}
;;               {:name "Jim" :hungry-percent 50}
;;               {:name "Kevin" :hungry-percent 50}
;;               ])
;; (local actions [{
;;                  :name "gets-more-hungry"
;;                  :filter-tags [:name :hungry-percent]
;;                  :update (fn [action e] (tset e :hungry-percent (+ e.hungry-percent 2)) (line-for action e))
;;                  :grammar {"#origin#" "#name# is #hungrypercent#% hungry."}}
;;                 {:name "eats"
;;                  :filter-fn (fn [e] (and e.hungry-percent) (> e.hungry-percent 75))
;;                  :update (fn [action e] (tset e :hungry-percent (- e.hungry-percent 50)) (line-for action e))
;;                  :grammar {"#origin#" "#name# eats a banana and is now #hungrypercent#% hungry."}}])

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
                               (expand-template action "#origin#" {:person person.name :drink drink.name})))
                 :grammar {"#origin#" ["#person# took #drink#." "'Oh hey, #drink#!' said #person#, and picked it up."]}}
                {:name "move"
                 :filter-fn (fn [e] (and e.person
                                         e.currently-in))
                 :update (fn [action person]
                             (let [currently-in person.currently-in
                                   destinations (lume.filter person.scene.nouns (fn [e] (and e.room (not (= e.name currently-in)))))
                                   chosen-destination (lume.randomchoice destinations)]
                               (tset person :currently-in chosen-destination.name)
                               (expand-template action "moveto" {:name person.name :room chosen-destination.name})))
                 :grammar {"moveto" ["After awhile, #name# went to #room#."
                                     "#name# decided to go into the #room#."]}}
                {:name "work"
                 :filter-fn (fn [e] (and e.person (= e.currently-in "study") e.has-drink))
                 :update (fn [action person] (expand-template action "isworking" {:name person.name}))
                 :grammar {"isworking" ["#name# typed furiously on their laptop."
                                        "#name# was taking notes while reading a book from the library.",
                                        "#name# sighed as they clicked 'Send' on another e-mail."]}}
                {:name "play video games"
                 :filter-fn (fn [e] (and e.person (= e.currently-in "living room")))
                 :update (fn [action person] (expand-template action "playgames" {:name person.name}))
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
                               (expand-template action "talks-with" {:personA personA.name :personB personB.name})))
                 :grammar {"#topic#" ["the weather" "the garden" "the phase of the moon" "#personA#'s family" "the books they've been reading"]
                           "talks-with" ["#personA# and #personB# chatted for a bit."
                                         "#personA# asked #personB# how their day was going."
                                         "#personB# told #personA# about a dream they had last night."
                                         "#personA# and #personB# talked for a bit about #topic#."]}}])


(local scene {:nouns nouns :actions actions})

(local world (prepare-scene scene))

(for [i 1 5]
     (tick world))

(each [_ line (ipairs world.lines)]
      (print line))

(print "The end.")

; Exports
{:randomseed randomseed
 :generate-one generate-one
 :tick tick
 :prepare-scene prepare-scene}
