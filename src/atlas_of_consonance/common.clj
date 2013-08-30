(ns atlas-of-consonance.common
  (:require clojure.set))

;; ======================================================================
;; ideas from Norman Sohl's Atlas of Consonance
;; http://www.sohl.com/mt/maptone.html

(defn overtone-seq
  "create an infinite sequence of overtones, given a tonic freq f"
  [f]
  (map #(* % f) (iterate inc 1)))

(defn tone-seq
  [a b i]
  "given the ends of an octave range [a,b] and an increment i, find
   the tones that lie between and include a and b."
  (range a (+ b i) i))

(defn per-octave-seqs
  "for each octave range (f 2f) (2f 4f) (3f 6f) ... find the base
  frequency multiples in that range.  (e.g. 3f is in the 2nd octave
  range)" [f]
  (let [overtones (overtone-seq f)
        octaves   (overtone-seq (* 2 f))]
    (map (fn [f1 o1] (tone-seq f1 o1 f))
         overtones
         octaves)))

(defn take-per-octave-seqs
  "filter per-octave-seqs to be within the range of n octaves.  So,
  n=7 would filter out some notes in the 6..12 octave range"
  [n f]
  (map #(filter (fn [x] (< x (* (inc n) f))) %)
       (take n (per-octave-seqs f))))

(defn take-norm-per-octave-seqs
  "Normalize per-octave-seqs to the tonic octave range (e.g. 3f -> 3f/2)."
  [n f]
  (let [overtones (overtone-seq f)]
    (map (fn [f1 s]
           (map (fn [s1] (* f (/ s1 f1))) s))
         overtones
         (take-per-octave-seqs n f))))

(defn sorted-freq-map
  "take a sequence of frequency sequences into a sorted histogram map"
  [sfs]
  (let [f-histogram (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} (flatten sfs))]
    (into (sorted-map-by (fn [key1 key2]
                           (compare [(get f-histogram key2) key2]
                                    [(get f-histogram key1) key1])))
          f-histogram)))

(defn count-chord-overtones
  "given 3 notes in a chord, how many overtones do they have in common?"
  [n f0 f1 f2]
  (let [fs       (map #(take n (overtone-seq %)) [f0 f1 f2])
        fs-set   (map set fs)
        fs-flat  (flatten fs)
        f-common (apply clojure.set/intersection fs-set)
        fs-x     (filter #(contains? f-common %) fs-flat)]
    (/ (count fs-x) 3)))

(defn chords-with-overtones
  [n f]
  (let [fs (->> (take-norm-per-octave-seqs n f)
                (sorted-freq-map)
                (sort)
                (keys))]
    (filter #(> (nth % 3) 0)
            (for [i fs
                  j fs]
              [f i j (count-chord-overtones n f i j)]))))
