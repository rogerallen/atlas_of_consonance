Where is the major triad?  My code as of Aug 31, 2013 doesn't highlight the standard C-E-G major triad.  Here are some notes I took as I attempted to figure this out.

```clj
user> (ns atlas-of-consonance.common)
nil
atlas-of-consonance.common> (def notes ["C" "C♯" "D" "D♯" "E" "F" "F♯" "G" "G♯" "A" "A♯" "B" "C"])
#'atlas-of-consonance.common/notes
atlas-of-consonance.common> (for [i (range 13)] (format "%s %.3f" (notes i) (Math/pow 2.0 (/ i 12))))
("C 1.000" "C♯ 1.059" "D 1.122" "D♯ 1.189" "E 1.260" "F 1.335" "F♯ 1.414" "G 1.498" "G♯ 1.587" "A 1.682" "A♯ 1.782" "B 1.888" "C 2.000")

;; okay E is at 1.260 in the chromatic scale for a normalized octave range of 1.0 - 2.0

atlas-of-consonance.common> (for [[n c] (sort (sorted-freq-map (take-norm-per-octave-seqs 12 1.0)))] (format "%d %.3f" c n))
("12 1.000" "1 1.091" "1 1.100" "1 1.111" "1 1.125" "1 1.143" "1 1.167" "2 1.200" "1 1.222" "2 1.250" "1 1.286" "3 1.333" "1 1.375" "1 1.400" "1 1.429" "4 1.500" "1 1.571" "1 1.600" "2 1.667" "1 1.714" "1 1.750" "1 1.800" "1 1.833" "6 2.000")

;; consonant E is at 1.25 and it has 2 matching occurances in a 12 octave sequence
;; consonant G is at 1.50 and it has 4 matching occurances in a 12 octave sequence

atlas-of-consonance.common> (clojure.set/intersection (set (take 12 (overtone-seq 1.0))) (set (take 12 (overtone-seq 1.25))))
#{5.0 10.0}
atlas-of-consonance.common> (clojure.set/intersection (set (take 12 (overtone-seq 1.0))) (set (take 12 (overtone-seq 1.5))))
#{9.0 3.0 6.0 12.0}

;; but the intersection of those 2 sets is null

atlas-of-consonance.common> (clojure.set/intersection (clojure.set/intersection (set (take 12 (overtone-seq 1.0))) (set (take 12 (overtone-seq 1.25)))) (clojure.set/intersection (set (take 12 (overtone-seq 1.0))) (set (take 12 (overtone-seq 1.5)))))
#{}

;; what about consonance between E and G?  there are 2 overtones

atlas-of-consonance.common> (clojure.set/intersection (set (take 12 (overtone-seq 1.5))) (set (take 12 (overtone-seq 1.25))))
#{7.5 15.0}
;; and yet again it is another null set with the earlier set.

;; so, my first attempt at counting the set of overtones, will not count the major chord well...
(defn count-chord-overtones
  "given 3 notes in a chord, how many overtones do they have in common?"
  [n f0 f1 f2]
  (let [fs       (map #(take n (overtone-seq %)) [f0 f1 f2])
        fs-set   (map set fs)
        fs-flat  (flatten fs)
        f-common (apply clojure.set/intersection fs-set)
        fs-x     (filter #(contains? f-common %) fs-flat)]
    (/ (count fs-x) 3)))

atlas-of-consonance.common> (count-chord-overtones 12 1.0 1.25 1.5)
0

;; how about just summing up all of the counts?  That wouldn't lead to
;; any zeros and Norm's graph had zeros.

;; let's try with just the f1 f2 set and ignore the f0 f1, f0 f2 intersection.

(defn new-count-chord-overtones
  "given 3 notes in a chord, how many overtones do they have in common?"
  [n f0 f1 f2]
  (let [fs       (map #(take n (overtone-seq %)) [f1 f2])
        fs-set   (map set fs)
        fs-flat  (flatten fs)
        f-common (apply clojure.set/intersection fs-set)
        fs-x     (filter #(contains? f-common %) fs-flat)]
    (/ (count fs-x) 2)))

```

That leads to something that definitely finds our chords, but looks odd--esp. on the diagonal

![Screenshot](https://github.com/rogerallen/atlas_of_consonance/raw/master/doc/atlas2_f1f2set.png)

```clj
;; Trying again to count all pairs of frequencies, just to see...

(defn new-count-chord-overtones
  "given 3 notes in a chord, how many overtones do they have in common?"
  [n f0 f1 f2]
  (/ (apply +
            (for [pair [[f0 f1] [f0 f2] [f1 f2]]]
              (let [fs       (map #(take n (overtone-seq %)) pair)
                    fs-set   (map set fs)
                    fs-flat  (flatten fs)
                    f-common (apply clojure.set/intersection fs-set)]
                (count f-common))))
     3))
```
But, this now leads to another ugly picture.

![Screenshot](https://github.com/rogerallen/atlas_of_consonance/raw/master/doc/atlas2_f012add.png)

So the problem remains...I sent mail to the author Aug 31, 2013 to ask for clarification.
