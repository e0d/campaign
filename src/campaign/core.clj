(ns campaign.core)

;; reference to a function that takes a limit and returns
;; a random integer.  Allows for easier changes to the
;; radmonization implementation
(def rand-int-f rand-int)

(defn die [n]
  (+ 1 (rand-int-f n)))

(defn d4
  []
  (die 4))

(defn d6
  []
  (die 6))

(defn d8
  []
  (die 8))

(defn d10
  []
  (die 10))

(defn d12
  []
  (die 12))

(defn d20
  []
  (die 20))

(defn ability
  []
  (reduce + (take 3 (sort (comp - compare) (repeatedly 4 d6)))))


;; Source
;; https://roll20.net/compendium/dnd5e/Ability%20Scores#content
;;
;; <= is syntactic sugar for a range check, inputs must get greater
;; from left to right, = makes it inclusive.
;;
(defn ability-score
  [score]
  (cond 
    (= 1 score) -5
    (<= 2 score 3) -4
    (<= 4 score 5) -3
    (<= 6 score 7) -2
    (<= 8 score 9) -1
    (<= 10 score 11) 0
    (<= 12 score 13) 1
    (<= 14 score 15) 2
    (<= 16 score 17) 3
    (<= 18 score 19) 4
    (<= 20 score 21) 5
    (<= 22 score 23) 6
    (<= 24 score 25) 7
    (<= 26 score 27) 8
    (<= 28 score 29) 9
    (= 30 score) 10))


(defn character-roll []
  (repeatedly 6 
              #(let [s (ability)]
                into [s (ability-score s)])))

                 
(def race-bonuses
  {
   :high-elf
   {
    :dexterity 2
    :intelligence 1
    }
   :teifling
   {
    :charisma 2
    :intelligence 1
    }
   :mountain-dwarf
   {
    :strength 2
    :constitution 2
    }
   }
  )

(def class-priorities
  {
   :barbarian [:strength :constitution :dexterity]
   :bard [:charisma :dexterity :constitution]
   :cleric [:wisdom :strength :constitution]
   })


(def character-attributes
  {
   :strength 0
   :dexterity 0
   :constitution 0
   :intelligence 0
   :wisdom 0
   :charisma 0
   })

(defn model-character
  [dice-roll model-race model-class]
  (
   let [
        priorities (model-class class-priorities)
        ordered-abilities (flatten (conj priorities (keys (apply dissoc character-attributes priorities))))
        ]
   (loop [p ordered-abilities
          roll dice-roll
          result {}]
     (if p
       (let [n (first p)
             r (first roll)
             ]
         (recur (next p)
                (next roll)
                (assoc-in result [n] (update-ability-tuple r (get-in race-bonuses [model-race n] 0)))))
       result
       )
     )
   )
  )

(defn update-ability-tuple
  [current-tuple adjustment]
  (
   let
   [
    score (first current-tuple)
    bonus (second current-tuple)
    new-score (+ adjustment score)
    ]
   into [new-score (ability-score new-score)]))
   

(def my-roll (sort (comp - compare) (character-roll)))

(prn my-roll)

(model-character my-roll :mountain-dwarf :barbarian)

(model-character my-roll :mountain-dwarf :bard)

(model-character my-roll :mountain-dwarf :cleric)


(get-in race-bonuses [:mountain-dwarf :strengh] 0)
