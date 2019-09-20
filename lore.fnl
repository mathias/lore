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

; Exports
{:randomseed randomseed
 :generate-one generate-one}
