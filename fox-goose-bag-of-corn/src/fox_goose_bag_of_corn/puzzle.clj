(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set :refer [union intersection difference join]]))

(defn safe-position? [bank-residents]
  (let [residents (into #{} bank-residents)]
    (not (or (= residents #{:fox :goose})
             (= residents #{:goose :corn})))))


;; union: combination
;; intersection: overlap
;; disj: remove an element from the set
;; difference: what's in a but not b

(def end-pos [#{} #{:boat} #{:you :fox :goose :corn}])
(def start-pos [[:fox :goose :corn :you] [:boat] []])
;; [[[:fox :corn] [:boat :goose :you] []]]
(defn river-crossing-plan []
  (let [position (vec (map set start-pos))]
    [position
     [(disj (first position) :you :goose) (union (second position) #{:you :goose}) #{}]
     ]))

;; [[[:fox :goose :corn :you] [:boat] []]]
;; [[[:fox :corn] [:boat :goose :you] []]]
;; [[[:fox :corn] [:boat :you] [:goose]]]
;; [[[:fox] [:boat :you :corn] [:goose]]]
;; [[[:fox] [:boat :you :goose] [:corn]]]
;; [[[:goose] [:boat :you :fox] [:corn]]]
;; [[[:goose] [:boat :you] [:fox :corn]]]
;; [[[] [:boat :you :goose] [:fox :corn]]]
;; [[[] [:boat] [:you :goose :fox :corn]]]

[[#{:fox :goose :corn :you} #{:boat}            #{}]
 [#{:fox :goose}            #{:boat :you :corn} #{}]
 [#{:fox :goose}            #{:boat :you}       #{:corn}]]


[#{:fox :goose :corn :you}
 #{:fox :goose}
 #{:fox :goose}
 #{}
 #{}
 #{:corn}
 ]
