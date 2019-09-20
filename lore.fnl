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

(lambda generate-one [grammar ?target]
  (let [target-key (or ?target "#origin#")]
    (var target-val-copy (smart-val (. grammar target-key)))
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
(lambda filter-function-for [filter-definition]
        (fn [e]
            (let [entity-keys (lume.keys e)]
              (lume.all filter-definition (fn [filter-key] (table-contains entity-keys filter-key))))))

(lambda prepare-actions [scene]
        (each [_ action (ipairs scene.actions)]
              (tset action :scene scene)
              (tset action :nouns scene.nouns)
              (tset action :filter-fn (filter-function-for action.filter))))

(lambda prepare-scene [scene]
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
                      (table.insert scene.lines (action.update action entity))))))

(lambda entity-merge-grammar [grammar entity]
        (var tmp-table {})
        (each [k v (pairs entity)]
              (tset tmp-table (string.format "#%s#" k) [v]))
        (lume.merge grammar tmp-table))

(lambda line-for [action entity ?key]
        (let [key (or ?key "#origin#")
              combined-grammar (entity-merge-grammar action.grammar entity)]
          (generate-one combined-grammar)))


(local nouns [{:name "Steve" :mass 150}])
(local actions [{
                 :name "weigh-in"
                 :filter [:name :mass]
                 :update (fn [action e] (tset e :mass (+ e.mass 5)) (line-for action e))
                 :grammar {"#origin#" "#name# who weighs #mass# pounds."}}])

(local scene {:nouns nouns :actions actions})

(local world (prepare-scene scene))

(for [i 1 20]
     (tick world))

(each [_ line (ipairs world.lines)]
      (print line))

; Exports
{:randomseed randomseed
 :generate-one generate-one
 :tick tick
 :prepare-scene prepare-scene}
