(ns atlas-of-consonance.atlas2
  (:use atlas-of-consonance.common)
  (:require [overtone.live :as o]
            [quil.core :as q]))

;; ======================================================================
;; overtone synth that plays a tonic and several overtones, plus
;; another note with it's overtones.
;;
;; FIXME – dynamically use number of octaves to add overtones to sin-osc?
(o/defsynth cons-synth
  [tonic-freq 200
   note1-freq 300
   note2-freq 400
   gate       1]
  (let [num-octaves 5
        a (o/mix (o/sin-osc (map #(* (+ 1 %) tonic-freq) (range num-octaves))))
        b (o/mix (o/sin-osc (map #(* (+ 1 %) note1-freq) (range num-octaves))))
        c (o/mix (o/sin-osc (map #(* (+ 1 %) note2-freq) (range num-octaves))))
        e (o/env-gen (o/asr 0.1 1.0 0.5) :gate gate :action o/FREE)]
  (o/out 0 (o/pan2 (* e (o/mix [a b c])) 0))))

;; ======================================================================
;; "public" state to play with
(defonce tonic-freq-atom (atom 0))
(defonce note1-freq-atom (atom 0))
(defonce note2-freq-atom (atom 0))
(defonce freq-histo-atom (atom ()))
(defonce chord-histo-atom (atom ()))
(defonce synth-atom (atom nil))

(defn set-tonic-freq [f]
  (swap! tonic-freq-atom (fn [_] f))
  (when @synth-atom
    (o/ctl @synth-atom :tonic-freq f))
  nil)

(defn set-note1-freq [f]
  (swap! note1-freq-atom (fn [_] f))
  (when @synth-atom
    (o/ctl @synth-atom :note1-freq f))
  nil)

(defn set-note2-freq [f]
  (swap! note2-freq-atom (fn [_] f))
  (when @synth-atom
    (o/ctl @synth-atom :note2-freq f))
  nil)

(defn set-num-octaves [n]
  (swap! freq-histo-atom
         (fn [x] (sorted-freq-map (take-norm-per-octave-seqs
                                  @tonic-freq-atom n))))
  (swap! chord-histo-atom
         (fn [x] (chords-with-overtones n @tonic-freq-atom)))
  nil)

(defn start-synth []
  (swap! synth-atom (fn [_] (cons-synth @tonic-freq-atom @note1-freq-atom @note2-freq-atom))))
(defn stop-synth []
  (when @synth-atom
    (o/ctl @synth-atom :gate 0)
    (swap! synth-atom (fn [_] nil))))

;; ======================================================================
;; Quil routines
(defn setup []
  (set-tonic-freq  262)
  (set-note1-freq  262)
  (set-note2-freq  262)
  (set-num-octaves 13)
  (q/smooth)
  (q/frame-rate 30))

(defn draw-diatonic-grid
  [b]
  (q/stroke-weight 1.5)
  (q/stroke 0 0 0)
  (q/fill 0 0 0)
  (let [note-str ["C" "C♯" "D" "D♯" "E" "F" "F♯" "G" "G♯" "A" "A♯" "B" "C"]]
    (dotimes [i 13]
      (let [j i
            [x y] (mapv #(- (Math/pow 2 (/ % 12)) 1.0) [i j])
            x (q/lerp b (- (q/width) b) x)
            y (q/lerp b (- (q/height) b) y)
            y (- (q/height) y)
            [xs ys] (mapv note-str [i j])
            xw2 (/ (q/text-width xs) 2)]
        (q/line x (- (q/height) b) x (+ (- (q/height) b) 10))
        (q/line b y (- b 10) y)
        (q/text-align :center :baseline)
        (q/text xs x (- (q/height) (/ b 2)))
        (q/text-align :right :center)
        (q/text ys (/ b 2) y)))))

(defn draw-consonance-grid
  [b]
  (dorun
   (doseq [[f0 f1 f2 w] @chord-histo-atom]
     (let [nw (* 3 (Math/pow w 0.85)) ;; FIXME
           x (q/lerp b (- (q/width) b)
                     (/ (- f1 @tonic-freq-atom) @tonic-freq-atom))
           y (q/lerp b (- (q/height) b)
                     (/ (- f2 @tonic-freq-atom) @tonic-freq-atom))
           y (- (q/height) y)]
       (q/stroke 200 0 0)
       (q/stroke-weight 1.0)
       (q/line x b x (- (q/height) b))
       (q/line b y (- (q/width) b) y)
       (q/fill 0 0 200)
       (q/stroke 0 0 200)
       (q/ellipse x y nw nw)))))

(defn draw-note [b]
  (when @synth-atom
    (let [x (q/lerp b (- (q/width) b)
                    (/ (- @note1-freq-atom @tonic-freq-atom) @tonic-freq-atom))
          y (q/lerp b (- (q/height) b)
                    (/ (- @note2-freq-atom @tonic-freq-atom) @tonic-freq-atom))
          y (- (q/height) y)]
      (q/stroke 0 240 240)
      (q/fill 0 0 0 0)
      (q/stroke-weight 3)
      (q/ellipse x y 20 20))))

(defn draw []
  (let [b 50]
    (q/background 250)
    (draw-consonance-grid b)
    (draw-diatonic-grid b)
    (draw-note b)))

(defn get-closest-seq-freq
  "given sequence of frequencies fs and a goal freq f, return the
  closest freq"
  [f fs]
  (apply min-key (fn [x] (Math/abs (- x f))) fs))

(defn get-closest-diatonic-freq
  [f]
  (get-closest-seq-freq f (map #(* @tonic-freq-atom (Math/pow 2 (/ % 12)))
                               (range 13))))

(defn get-closest-consonance-freq
  [f]
  (get-closest-seq-freq f (keys @freq-histo-atom)))

(defn get-closest-freqs
  [x y]
  (let [b 50
        f1 (q/lerp @tonic-freq-atom (* 2 @tonic-freq-atom)
                  (/ (- x b) (- (q/width) (* 2 b))))
        y (- (q/height) y)
        f1 (q/lerp @tonic-freq-atom (* 2 @tonic-freq-atom)
                   (/ (- x b) (- (q/width) (* 2 b))))
        f2 (q/lerp @tonic-freq-atom (* 2 @tonic-freq-atom)
                   (/ (- y b) (- (q/height) (* 2 b))))]
    ;;(get-closest-consonance-freq f))))
    (vector (get-closest-consonance-freq f1)
            (get-closest-consonance-freq f2))))

(defn mouse-button []
  (let [x (q/mouse-x)
        y (q/mouse-y)
        [snap-freq1 snap-freq2] (get-closest-freqs x y)]
    (set-note1-freq snap-freq1)
    (set-note2-freq snap-freq2)
    (if (q/mouse-state)
      (start-synth)
      (stop-synth))))

(defn run []
  (q/defsketch doodle
    :title          "atlas2"
    :setup          setup
    :draw           draw
    :mouse-pressed  mouse-button
    :mouse-released mouse-button
    :size           [800 800]))

;; exec this to start the window
;; (run)

;; click on the diagram to play notes.
;; FIXME - add grid of

;; use (o/stop) if it gets stuck playing

;; (set-tonic-freq 130)
;; (set-num-octaves 11)
;; (set-num-octaves 17)
;; (set-num-octaves 21)
