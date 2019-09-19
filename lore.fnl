;(local fennel (require :fennel))
;(table.insert (or package.loaders package.searchers) fennel.searcher)
(local rex (require :rex_onig))
(local lume (require :lume))

;; for dev
(local view (require :fennelview))
(global pp (fn [x] (print (view x))))
;; Setting up random seed
(math.randomseed (os.time))

(fn randomseed [seed]
    (math.randomseed seed))

;; (local grammar {"#foo#" ["fuzz" "foop" "fizz"]
;;                 "#bar#" ["barr" "ber" "imp"]
;;                 "#baz#" ["dollars" "cents"]
;;                 "#origin#" "My #foo# is a #bar# with a #baz#."})

(lambda generateOne [grammar ?target]
  (let [target-key (or ?target "#origin#")]
    (var target-val-copy (. grammar target-key))
    (each [key val (pairs grammar)]
          (if (rex.match target-val-copy key)
              (set target-val-copy (rex.gsub target-val-copy key (lume.randomchoice val)))))
    target-val-copy))


{:randomseed randomseed
 :generateOne generateOne}
