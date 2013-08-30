(ns atlas-of-consonance.atlas1
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
   note-freq  300
   gate       1]
  (let [num-octaves 5
        a (o/mix (o/sin-osc (map #(* (+ 1 %) tonic-freq) (range num-octaves))))
        b (o/mix (o/sin-osc (map #(* (+ 1 %) note-freq) (range num-octaves))))
        e (o/env-gen (o/asr 0.1 1.0 0.5) :gate gate :action o/FREE)]
  (o/out 0 (o/pan2 (* e (o/mix [a b])) 0))))

;; ======================================================================
;; "public" state to play with
(defonce tonic-freq-atom (atom 0))
(defonce note-freq-atom (atom 0))
(defonce freq-histo-atom (atom ()))
(defonce synth-atom (atom nil))

(defn set-tonic-freq [f]
  (swap! tonic-freq-atom (fn [_] f))
  (when @synth-atom
    (o/ctl @synth-atom :tonic-freq f))
  nil)

(defn set-note-freq [f]
  (swap! note-freq-atom (fn [_] f))
  (when @synth-atom
    (o/ctl @synth-atom :note-freq f))
  nil)

(defn set-num-octaves [n]
  (swap! freq-histo-atom
         (fn [x] (sorted-freq-map (take-norm-per-octave-seqs
                                  n @tonic-freq-atom))))
  nil)

(defn start-synth []
  (swap! synth-atom (fn [_] (cons-synth @tonic-freq-atom @note-freq-atom))))
(defn stop-synth []
  (when @synth-atom
    (o/ctl @synth-atom :gate 0)
    (swap! synth-atom (fn [_] nil))))

;; ======================================================================
;; Quil routines
(defn setup []
  (set-tonic-freq  262)
  (set-note-freq   262)
  (set-num-octaves 13)
  (q/smooth)
  (q/frame-rate 30))

(defn draw-diatonic-hatches
  [b h2]
  (q/stroke-weight 1.5)
  (q/stroke 0 0 0)
  (q/fill 0 0 0)
  (let [note-str ["C" "C♯" "D" "D♯" "E" "F" "F♯" "G" "G♯" "A" "A♯" "B" "C"]]
    (dotimes [i 13]
      (let [x (- (Math/pow 2 (/ i 12)) 1.0)
            x (q/lerp b (- (q/width) b) x)
            s (note-str i)
            sw2 (/ (q/text-width s) 2)]
        (q/line x h2 x (+ h2 20))
        (q/text s (- x sw2) (- (q/height) (/ b 2)))))))

(defn draw-consonance-hatches
  [b h2 max-freq]
  (dorun
   (doseq [k  (keys @freq-histo-atom)]
     (let [w  (@freq-histo-atom k)
           nw (Math/pow (/ w max-freq) 0.75) ;; a bit non-linear
           x  (q/lerp b (- (q/width) b)
                      (/ (- k @tonic-freq-atom) @tonic-freq-atom))
           a  (q/lerp 0 255 nw)
           sh (+ (/ b 10) (* 9 (/ b 10) nw))]
       (q/stroke 200 0 0 a)
       (q/stroke-weight 1.5)
       (q/line x (- h2 sh) x h2)))))

(defn draw-x-axis
  [b h2]
  (q/stroke 0 0 0)
  (q/stroke-weight 1.5)
  (q/line b h2 (- (q/width) b) h2))

(defn draw-note [b]
  (when @synth-atom
    (let [x (q/lerp b (- (q/width) b)
                    (/ (- @note-freq-atom @tonic-freq-atom) @tonic-freq-atom))]
      (q/stroke 0 0 240)
      (q/stroke-weight 3)
      (q/line x b x (- (q/height) b))
      (q/line b b b (- (q/height) b)))))

(defn draw []
  (let [b 50
        h2 (/ (q/height) 2)
        max-freq (apply max (vals @freq-histo-atom))]
    (q/background 250)
    (draw-consonance-hatches b h2 max-freq)
    (draw-diatonic-hatches b h2)
    (draw-x-axis b h2)
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

(defn get-closest-freq
  [x y]
  (let [b 50
        f (q/lerp @tonic-freq-atom (* 2 @tonic-freq-atom)
                  (/ (- x b) (- (q/width) (* 2 b))))]
    (if (> y (/ (q/height) 2))
      (get-closest-diatonic-freq f)
      (get-closest-consonance-freq f))))

(defn mouse-button []
  (let [x (q/mouse-x)
        y (q/mouse-y)
        snap-freq (get-closest-freq x y)]
    (set-note-freq snap-freq)
    (if (q/mouse-state)
      (start-synth)
      (stop-synth))))

(defn run []
  (q/defsketch doodle
    :title          "Atlas 1"
    :setup          setup
    :draw           draw
    :mouse-pressed  mouse-button
    :mouse-released mouse-button
    :size           [800 150]))

;; exec this to start the window
;; (run)

;; click on the diagram to play notes.
;; - in the top area, it will snap to the nearest consonance note
;; - in the bottom area, it will snap to the nearest diatonic note

;; check out the difference between the values near E and F

;; use (o/stop) if it gets stuck playing

;; (set-tonic-freq 130)
;; (set-num-octaves 7)
;; (set-num-octaves 50)
