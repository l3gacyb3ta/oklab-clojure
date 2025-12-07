(ns oklab
  (:require [nextjournal.clerk :as clerk])
  (:require [clojure.math :as m])
  (:require [nextjournal.clerk.viewer :as v]))

;; OKlab is a color space

;; We need a viewer that can render out arbitrary colors for us. So let's take any set of three numbers and try and
;; render it out as an rgb color. Notably this won't be able to display OKLAB colors, so we'll work on that next.

(defn threenum? [a]
  (and (vector? a) (every? number? a)))

(clerk/add-viewers!
  [{:pred threenum?
    :render-fn '(fn [n] [:div.inline-block {:style {:width 16
                                                    :height 16
                                                    :background-color (str "rgb(" (:nextjournal/value (nth n 0)) ", " (:nextjournal/value (nth n 1)) ", " (:nextjournal/value (nth n 2)) ")")}
                                            :class "border-solid border-2 border-black "}])}])

;; Let's look at some colors!

[[3 255 200] [0xea 0xea 0xea] [50 90 20]]

;; Now we need to be able to define an OKLAB color.
;; It has three values, much like an RBG color, but very different in what they mean:
;; 1. $L$ - perceived lightness
;; 2. $a$ - how green/red the color is
;; 3. $b$ - how blue/yellow the color is

(defn create-oklab [L a b]
  (hash-map :L L :a a :b b))

(def our-color (create-oklab 0.5 0.1 0.6))

;; But maybe, you'd want to look at an OkLCh color, that's
;; 1. $L$ - perceived lightness again
;; 2. $C$ - Chroma as an angle around the color wheel
;; 3. $h$ - h is hue

(defn create-oklch [L C h]
  (hash-map :L L :C C :h h))

(defn oklch-from-oklab [lab]
  (let [L (:L lab) a (:a lab) b (:b lab) square #(* % %)]
    (create-oklch L
                  (m/sqrt (+ (square (:a lab)) (square (:b lab))))
                  (m/atan2 (:b lab) (:a lab)))))


(defn oklab-from-oklch [lch]
  (let [L (:L lch) C (:C lch) h (:h lch)]
    (create-oklab (:L lch)
                  (* C (m/cos (m/to-radians h)))
                  (* C (m/sin (m/to-radians h))))))

;; Now, does it work?
our-color

(oklch-from-oklab our-color)

(oklab-from-oklch (oklch-from-oklab our-color))

;; And, baring rounding point errors, it does work!
;; Now we want actually see `our-color` right?
;; This relies on a bunch of magic numbers, so here we go.

(defn linear-to-gamma [c]
  (if (>= c 0.0031308)
    (- (* 1.055 (Math/pow c (/ 1 2.4))) 0.055)
    (* 12.92 c)))

(defn gamma-to-linear [c]
  (if (>= c 0.04045)
    (Math/pow (/ (+ c 0.055) 1.055) 2.4)
    (/ c 12.92)))

(defn clamp [x min-val max-val]
  (max min-val (min x max-val)))

(defn oklab-to-srgb [lab]
  (let [L (:L lab) a (:a lab) b (:b lab)
        l-tmp (+ L (* a 0.3963377774) (* b 0.2158037573))
        m-tmp (+ L (* a -0.1055613458) (* b -0.0638541728))
        s-tmp (+ L (* a -0.0894841775) (* b -1.2914855480))

        l (* l-tmp l-tmp l-tmp)
        m (* m-tmp m-tmp m-tmp)
        s (* s-tmp s-tmp s-tmp)

        r-lin (+ (* l 4.0767416621) (* m -3.3077115913) (* s 0.2309699292))
        g-lin (+ (* l -1.2684380046) (* m 2.6097574011) (* s -0.3413193965))
        b-lin (+ (* l -0.0041960863) (* m -0.7034186147) (* s 1.7076147010))

        process (fn [v]
                  (-> v
                      linear-to-gamma
                      (* 255)
                      (clamp 0 255)
                      ))]

    ;; 5. Return the resulting map
    [(process r-lin) (process g-lin) (process b-lin)]))

(oklab-to-srgb our-color)

(defn lerp-oklab [pct a b]
  (create-oklab
    (+ (:L a) (* pct (- (:L b) (:L a))))
    (+ (:a a) (* pct (- (:a b) (:a a))))
    (+ (:b a) (* pct (- (:b b) (:b a))))
    ))

(defn lerp-rgb [pct a b]
  (vector
    (+ (nth a 0) (* pct (- (nth b 0) (nth a 0))))
    (+ (nth a 1) (* pct (- (nth b 1) (nth a 1))))
    (+ (nth a 2) (* pct (- (nth b 2) (nth a 2))))
    ))


(def color-1 (create-oklab 0 0.051 -0.207))
(def color-2 (create-oklab 1.5 0.051 -0.207))

(defn little-function [p] (oklab-to-srgb (lerp-oklab p color-1 color-2)))
(defn little-function-2 [p] (lerp-rgb p (oklab-to-srgb color-1) (oklab-to-srgb color-2)))

(map little-function [0.0 0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65 0.7 0.75 0.8 0.85 0.9 0.95 1.0])
(map little-function-2 [0.0 0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65 0.7 0.75 0.8 0.85 0.9 0.95 1.0])

(def color-3 (oklab-from-oklch (create-oklch 0.8397 0.3601 145.59)))

color-3

(defn display-p3-show [lab] (clerk/html [:div.inline-block {:style {:width 16
                                                                    :height 16
                                                                    :background-color (str "oklab(" (:L lab) " " (:a lab) " " (:b lab) ")")}
                                                            :class "border-solid border-2 border-black "}]))


(display-p3-show color-3)