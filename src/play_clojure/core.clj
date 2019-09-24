(ns play-clojure.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(foo 3)

;; problem the big divide, #148

(defn bd [a b c]
  (letfn [(sq [n m]
            (* m (/ (* n (+ n 1)) 2)))]
    (let [a' (dec a)
          b' (quot a' b)
          c' (quot a' c)
          d' (quot a' (* b c))
          ]
      (-
       (+ (sq b' b) (sq c' c))
       (sq d' (* b  c))))))

(defn bd [a b c]
  (letfn [(sq [n m]
            (* m (/ (* n (+ n 1)) 2)))]
    (let [a' (bigint (dec a))
          b' (bigint (quot a' b))
          c' (bigint (quot a' c))
          d' (bigint (quot a' (* b c)))
          ]
      (-
       (+ (sq b' b) (sq c' c))
       (sq d' (* b  c))))))

(defn sq [n m]
  (* m (/ (* n (+ n 1)) 2)))

(defn test []
  (= 0 (bd 3 17 11)))

(bd 15 3 5)

(+ 3 6 9 12 5  10 )

(bd 16 3 5) ;; 75

(+ 3 6 9 12 15 5 10 15)

(+ 3 6 9 15 5 10 15 )

(= 233168 (bd 1000 3 5))


;; Write a function which calculates the sum of all natural numbers under n (first argument) which are evenly divisible by at least one of a and b (second and third argument). Numbers a and b are guaranteed to be coprimes.

;;Note: Some test cases have a very large n, so the most obvious solution will exceed the time limit.

(= 0 (bd 3 17 11))

(= 23 (bd 10 3 5))

;; 3 6 9 5    10  12
;; 23

;; 10 / 3 = 3
;; 3*1 + 3*2 + 3*3
;; 3 + 6 + 9 = 18
;; sum of 1 to n * 3?
;; n(n-1) /2
;; 3(2) = 6
;; 2
;; 2*3 = 6

;;3
;;33
;;333

;;n(n+1)/2
;;3(4)/2 = 6 * 3 = 18



(- 266333 233168)

(= "2333333316666668" (str (bd 100000000 3 5)))

(= "110389610389889610389610"
  (str (bd (* 10000 10000 10000) 7 11)))

(= "1277732511922987429116"
  (str (bd (* 10000 10000 10000) 757 809)))

(= "4530161696788274281"
  (str (bd (* 10000 10000 1000) 1597 3571)))

;;

(defn same-sum [& args]
  true
  )

(= true  (same-sum #{-1 1 99}
             #{-2 2 888}
             #{-3 3 7777})) ; ex. all sets have a subset which sums to zero

;; generate all combinations of summations
;; a b c
;; + a b
;; + a
;; + a b c
;; b c
;; c

;; generation the combinations of a sequence
(defn combs[s acc]
  (if (empty? s)
    acc
    (cons
     (reduce #(cons %2 %1) acc (rests s ()))
     (combs (rest s) acc))))

(defn rests [s acc]
  (if (empty? s)
    acc
    (rests (rest s) (cons s acc))))

(combs [1 2 3] nil)

;; a b c

;; for each element
;;   acc += element
;;   acc += element + each tail element

;; what we want
;; given a b c
;; for each member
;;   for each member of original list
;;     add a, call recusrively and add result of adding a with b


(defn combs2 [s acc]
  (if (empty? s)
    acc
    (let [heads (reduce (fn [acc x]
                          (cons (list x) acc)) acc s)]
      (combs2 (rest s) (conj acc heads)))))

  ;(reduce #(cons %2 %1) acc s))

;; get all the heads
;; for each member and each head also add the head plus the member
;; and so on

(defn combs3 [s acc]
  (reduce #(conj %1 #{%2}) acc s))

(defn combs4 [s acc]
  (if (empty? s)
    acc
    (reduce (fn [acc n ]
              (conj
               (conj acc #{n})
               (combs4 (disj acc n) acc)))
            acc s)))



;;
;; [1,2,3] []
;;   for each thing 1,2,3
;;      add += add thing
;;      map acc [see below]
;;        recurse [2,3] [1]
;;           add thing
;;           [2,3] [[1],[2]]
;;           accum += for each accum add thing
;;              [[1],[2],[1,2],[1,3]]
;;              recurse [3] [[1],[2],[1,2],[1,3]]
;;                 add thing [2,3] [[1],[2],[1,2],[1,3],[3]]
;;                 accum += for each accum add thing
;;                   [3] [[1],[2],[1,2],[1,3],[3],[1,3],[2,3],[1,2,3],[1,3],[3]]
;;                  recurse [] [[1],[2],[1,2],[1,3],[3],[1,3],[2,3],[1,2,3],[1,3],[3]]
;;                    nil so return


(defn combs6 [s acc]
  (if (empty? s)
    acc
    (map inc s)))

(combs6 (1 2 3) ())


(defn combs5 [s acc]
  (if (empty? s)
    acc
    (map (fn [thing]
           (let [a1 (map #(conj %1 thing) acc)
                 a2 (conj acc (list thing) a1)
                 a3 (combs5 (rest s) a2)
                 ]
             a3)) s)))

(combs5 '(1 2 3 4) ())

(map #(conj %1 1) [])






(defn flatten-1 [s]
  (reduce (fn [acc n]
            (if (not (seq? n))
              (conj acc n)
              (apply conj acc n)))
          [] s))


(defn combs6 [s acc]
  (if (empty? s)
    acc
    (map (fn [thing]
           (let [a1 (map #(conj %1 thing) acc)
                 a2 (conj acc (list thing) a1)
                 a3 (combs6 (rest s) a2)
                 ]
             a3)) s)))

(combs6 #{1 2} #{})

(defn combs7 [s acc]
  (if (empty? s)
    acc
    (reduce (fn [acc thing]
              (let [a1 (map #(conj %1 thing) acc)
                    a2 #{thing}]
                (conj acc (combs7 (rest s) (conj acc a1 a2)))))
            acc s)))

(combs7 #{1 2} #{})


(conj #{[1 2 3]} #{[1 2 3] [1 2]})

(comment
 "
def combinations[A](in: Set[A]): Set[Set[A]] = {
    if (in.isEmpty) Set.empty
    else {
      in.foldLeft(Set.empty[Set[A]]) {
        case (acc, cur) =>
          (acc + Set(cur)) ++
          acc.map(_ + cur) ++
          combinations(in - cur)
      }
    }
  }


{val t1 = System.currentTimeMillis
println(combinations(Set(1,2,3,4,5,6,7,8,9)).size)
val t2 = System.currentTimeMillis
println(s\"elapsed ms ${t2 - t1}\")}
"
 )

(defn combinations [in]
  (if (empty? in) #{}
      (reduce (fn [acc cur]
                (clojure.set/union
                 (conj acc #{cur})
                 (into #{} (map #(conj %1 cur) acc))
                 (combinations (disj in cur)))) #{} in)))

(time (combinations #{1 2 3 4 5 6 7 8 9}))



(defn same-sum-1 [& args]
  (letfn [(combs [in]
            (if (empty? in) #{}
                (reduce (fn [acc cur]
                          (clojure.set/union
                           (conj acc #{cur})
                           (into #{} (map #(conj %1 cur) acc))
                           (combs (disj in cur)))) #{} in)))
          (sums [combs]
            (into #{} (map #(reduce + %1) combs)))
          ]
    (not (empty? (clojure.set/intersection (map (comp sums combs) args))))))

(defn same-sum-2 [& args]
  (letfn [(combs [in]
            (if (empty? in) #{}
                (reduce (fn [acc cur]
                          (clojure.set/union
                           (conj acc #{cur})
                           (into #{} (map #(conj %1 cur) acc))
                           (combs (disj in cur)))) #{} in)))
          (sums [combs]
            (into #{} (map #(reduce + %1) combs)))
          ]
    (not (empty? (apply clojure.set/intersection
                        (map (comp sums combs) args))))))

(defn same-sum-3 [& args]
  (letfn [(combs [in]
            (if (empty? in) #{}
                (reduce (fn [acc cur]
                          (clojure.set/union
                           (conj acc #{cur})
                           (into #{} (map #(conj %1 cur) acc))
                           (combs (disj in cur)))) #{} in)))
          (sums [combs]
            (into #{} (map #(reduce + %1) combs)))
          ]
    (map combs args)))

(= false (same-sum-2 #{1}
             #{2}
             #{3}
             #{4}))



(same-sum  #{-1 1 99}
             #{-2 2 888}
             #{-3 3 7777})

(= true  (same-sum #{-1 1 99}
             #{-2 2 888}
             #{-3 3 7777})) ; ex. all sets have a subset which sums to zero


 (same-sum-2 #{-10 9 -8 7 -6 5 -4 3 -2 1}
             #{10 -9 8 -7 6 -5 4 -3 2 -1})

(comment "

a b c

combs [a b c] []
    combs [b c] [[a]]
      [c] [[a] [a b]]
         [c] [[a] [a b] [a c] [a b c]]
         return [[a] [a b] [a c] [a b c]]
      return [[a] [a b] [a c] [a b c]]
    combs [c] [[a] [a b] [a c] [a b c]]
      [[a] [a b] [a c] [a b c]]


combs in a b c,  acc  []
  for n in a b c
    combs in (rest in) (n acc (map acc add n))

")

(defn combinations [in]
  (if (empty? in) #{}
      (reduce (fn [acc cur]
                (clojure.set/union
                 (conj acc #{cur})
                 (into #{} (map #(conj %1 cur) acc))
                 (combinations (disj in cur)))) #{} in)))


(defn combs8 [in in-acc]
  (if (empty? in) in-acc
      (reduce (fn [acc cur]
                (combs8 (rest in)
                        (conj acc (map #(conj %1 cur)) (list cur))))
              in-acc in)))

(combs8 '(\a \b \c) nil)
