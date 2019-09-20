(local rex (require :rex_onig))
(local lume (require :lume))

;; for dev
(local view (require :fennelview))
(global pp (fn [x] (print (view x))))
;; Setting up random seed
(math.randomseed (os.time))

(fn randomseed [seed]
    (math.randomseed seed))

(lambda generate-one [grammar ?target]
  (let [target-key (or ?target "#origin#")]
    (var target-val-copy (. grammar target-key))
    (each [key val (pairs grammar)]
          (if (rex.match target-val-copy key)
              (set target-val-copy (rex.gsub target-val-copy key (lume.randomchoice val)))))
    target-val-copy))

; Exports
{:randomseed randomseed
 :generate-one generate-one}
