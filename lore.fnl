;(local rex (require :rex_onig))
(local lume (require :lume))

;; for dev
(local view (require :fennelview))
(local pp (fn [x] (print (view x))))
;; Setting up random seed
(math.randomseed (os.time))

(fn randomseed [seed]
    (math.randomseed seed))

(lambda generate-one [grammar ?target]
  (let [target-key (or ?target "#origin#")]
    (var target-val-copy (let [target (. grammar target-key)]
                           (if (= (type target) "table")
                               (. target 1)
                               target)))
    (each [key val (pairs grammar)]
          (when (string.find target-val-copy key)
              (set target-val-copy
                   (string.gsub target-val-copy key (lume.randomchoice val)))))
    target-val-copy))

(fn story-event [narrative action ...]
    (let [nouns ...]
      (generate-one narrative.grammar action.name)))

(lambda narrative-validator [narrative]
        (assert (= (type narrative) "table") "Narrative must be a table.")
        (assert narrative.nouns "Narrative must have nouns.")
        (assert narrative.actions "Narrative must have actions.")
        (assert narrative.grammar "Narrative must have a grammar.")
        (each [_ noun (ipairs narrative.nouns)]
              (assert noun.properties "Nouns must have properties list."))
        (each [_ action (ipairs narrative.actions)]
              (assert action.name "Actions must have a name field.")
              (assert action.match "Actions must have a match array."))
        (each [name strings (pairs narrative.grammar)]
              (assert (= (type strings) "table") "Grammar must have a list of strings for every key.")
              (each [_ str (ipairs strings)]
                    (assert (= (type str) "string") "Grammar must have a list of strings for every key.")))
        (if narrative.initialize
            (assert (= (type narrative.initialize) "function") "Narrative initializer must be a function."))
        (if narrative.goal-state
            (assert (= (type narrative.goal-state) "function") "Narrative goal must be a function.")))

(lambda events-validator [events]
        (assert (= (type events) "table") "Events must be an array."))

(lambda filter-tag-match [match-str item]
        (pp (.. match-str " ?= " (table.unpack item.tags)))

        (if (= (string.sub match-str 1 1) "#") (lume.find item.tags (string.sub match-str 2))
            (= item.name match-str) true
            false))

(lambda step [narrative events]
        (each [_ action (ipairs narrative.actions)]
              (let [matching-as (lume.filter narrative.nouns (fn [item] (filter-tag-match (. action.match 1) item)))
                    matching-bs (lume.filter narrative.nouns (fn [item] (filter-tag-match (. action.match 2) item)))
                    bound-action action.action
                    bound-when action.when]
                (each [_ obj-a (ipairs matching-as)]
                      (each [_ obj-b (ipairs matching-bs)]
                            (if (bound-when obj-a obj-b)

                                ))))))

(lambda narrative-goal-not-met [narrative]
        (if narrative.goal-state
            (narrative.goal-state narrative)
            false))

(lambda smart-concat [tbl potential-list]
        (if (= (type potential-list) "string") (table.insert tbl potential-list)
            (= (type potential-list) "table") (lume.concat tbl potential-list)))

(lambda i-generate-events [narrative ?max-steps]
        (let [events []
              max-steps (or ?max-steps math.huge)]
          (var steps-count 0)

          (if narrative.initialize
              ;; push any generated events on to events
              (let [possible-events (narrative.initialize)]
                (smart-concat events possible-events)))
          (while (and (<= steps-count max-steps) true) ;; TODO: add goal state checker here
;;            (pp (.. steps-count " < " max-steps))
            (table.insert events (step narrative events))
            (set steps-count (+ steps-count 1)))
          events))

(lambda generate-events [narrative ?max-steps]
        (narrative-validator narrative)
        (let [events (i-generate-events narrative ?max-steps)]
          (events-validator events)
          events))

;; dev data:
(local example-noun {:name "Joe"
                     :properties {:happiness 0
                                  :hungry true}
                     :tags ["person"]})

(local example-action {:name "eat"
                       :match ["person" "food"]
                       :when (fn [a b] (and a.properties.hungry (not b.properties.eaten)))
                       :action (fn [a b]
                                   (let [event (story-event "eat" a b)]
                                     (set a.properties.hungry false)
                                     (set a.properties.eaten true)
                                     (values a b event)))})
(local example-narrative
       {:nouns [example-noun
                {:name "banana"
                 :properties {}
                 :tags ["food"]}]
        :actions [example-action]
        :initialize (fn [] "Once upon a time...")
        :goall-state (fn [narrative] (lume.any narrative.nouns (fn [noun] (not noun.properties.hungry))))
        :grammar {:eat ["#nounA# eats #nounB#"]
                  :sleep ["#nounA# falls asleep."]
                  :see ["#nounA# sees #nounB# out of the corner of their eye."]}})

(local parsed-narrative example-narrative)

(local generated-events (generate-events parsed-narrative 100))

(each [index event (ipairs generated-events)]
      (print event))

; Exports
{:randomseed randomseed
 :generate-one generate-one
 :generate-events generate-events
 :step step}

