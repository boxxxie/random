(ns random
  " random is an amalgamation of a few different random number generating
  functions discovered in the depths of cyberspace.

  main utility is the ability to set a seed for the generators `(set-random-seed!)`
  uuids are done a bit differently and require their own pseudo seed to be set `(reset-uuid-seed!)`
  "
  (:require [clj-uuid :as uuid])
  (:refer-clojure :exclude [rand-int rand rand-nth shuffle]))

(defonce generator (new java.util.Random))

(defn set-random-seed!
  "Sets the seed of the global random number generator."
  [seed]
  (.setSeed generator seed))

(def ^:private uuid-global-state (volatile! 1))
(defn reset-uuid-seed! [] (vreset! uuid-global-state 1))
(def ^:private mock-uuid-ns (uuid/v5 uuid/+namespace-oid+ :mock-uuid))
(defn uuid []
  (uuid/v5 mock-uuid-ns (vswap! uuid-global-state inc)))

(defn rand
  "Returns a random floating point number between 0 (inclusive) and
  n (default 1) (exclusive). Works like clojure.core/rand except it
  uses the seed specified in set-random-seed!."
  ([] (.nextFloat generator))
  ([n] (* n (rand))))

(defn rand-int
  "Returns a random integer between 0 (inclusive) and n (exclusive).
  Works like clojure.core/rand except it uses the seed specified in
  set-random-seed!."
  [n]
  (int (rand n)))

(defn rand-nth
  "Return a random element of the (sequential) collection. Will have
  the same performance characteristics as nth for the given
  collection. Works like clojure.core/rand except it uses the seed
  specified in set-random-seed!."
  [coll]
  (nth coll (rand-int (count coll))))

(defn shuffle
  "Return a random permutation of coll. Works like clojure.core/shuffle
  except it uses the seed specified in set-random-seed!."
  [^java.util.Collection coll]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al generator)
    (clojure.lang.RT/vector (.toArray al))))

(defn rand-nth-weighted
  "Return a random element from the weighted collection.
  A weighted collection can be any seq of [choice, weight] elements.  The
  weights can be arbitrary numbers -- they do not need to add up to anything
  specific.
  Examples:
  (rand-nth-weighted [[:a 0.50] [:b 0.20] [:c 0.30]])
  (rand-nth-weighted {:a 10 :b 200})
  "
  [coll]
  (let [total (reduce + (map second coll))]
    (loop [i                             (rand total)
           [[choice weight] & remaining] (seq coll)]
      (if (>= weight i)
        choice
        (recur (- i weight) remaining)))))

(defn rand-bool
  "Return true or false with a percent chance of being true.
  percent defaults to 50.0
  "
  ([]
   (rand-bool 50.0))
  ([percent]
   (< (rand 100) percent)))

(defn rand-gaussian
  "Return a random float.
  Floats are generated from a Gaussian distribution with the given mean and
  standard deviation.
  A lower and upper bound can be specified if desired, which will clamp the
  output of this function to those bounds.  Note that this clamping does NOT
  adjust the distribution, so if you clamp too tightly you'll get
  a disproportionate number of the boundary values.  It's just here to give you
  a way to prevent garbage values.
  mean defaults to 0.
  standard-deviation defaults to 1.
  "
  ([]
   (.nextGaussian generator))
  ([mean standard-deviation]
   (-> (rand-gaussian)
       (* standard-deviation)
       (+ mean)))
  ([mean standard-deviation lower-bound upper-bound]
   (-> (rand-gaussian mean standard-deviation)
       (max lower-bound)
       (min upper-bound))))

(defn rand-gaussian-int
  "Return a random integer.
  Integers are generated from a Gaussian distribution with the given mean and
  standard deviation.
  A lower and upper bound can be specified if desired, which will clamp the
  output of this function to those bounds.  Note that this clamping does NOT
  adjust the distribution, so if you clamp too tightly you'll get
  a disproportionate number of the boundary values.  It's just here to give you
  a way to prevent garbage values.
  mean defaults to 0.
  standard-deviation defaults to 1.
  "
  ([]
   (int (rand-gaussian)))
  ([mean standard-deviation]
   (int (rand-gaussian mean standard-deviation)))
  ([mean standard-deviation lower-bound upper-bound]
   (-> (rand-gaussian-int mean standard-deviation)
       (max lower-bound)
       (min upper-bound))))
