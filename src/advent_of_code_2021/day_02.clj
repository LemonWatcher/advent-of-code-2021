(ns advent-of-code-2021.day-02
  "Advent of Code 2021, Day 2: Dive!"
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [split-lines split]]
            [net.cgrand.xforms :as x]
            [clojure.test :refer [deftest is]]))

;; # Day 2: Dive!
;;
;; Now, you need to figure out how to pilot this thing.
;;
;; It seems like the submarine can take a series of commands like
;; forward 1, down 2, or up 3:
;;
;; forward X increases the horizontal position by X units.
;; down X increases the depth by X units.
;; up X decreases the depth by X units.

(defn submarine-command
  "Process a single submarine command given the current position."
  [[horizontal depth] [command units]]
  (case command
    "forward" [(+ horizontal units) depth]
    "down" [horizontal (+ depth units)]
    "up" [horizontal (- depth units)]))

;; Note that since you're on a submarine, down and up affect your
;; depth, and so they have the opposite result of what you might
;; expect.
;;
;; The submarine seems to already have a planned course (your puzzle
;; input). You should probably figure out where it's going. For
;; example:
;;
;;     forward 5
;;     down 5
;;     forward 8
;;     up 3
;;     down 8
;;     forward 2

(def test-input
  [["forward" 5]
   ["down" 5]
   ["forward" 8]
   ["up" 3]
   ["down" 8]
   ["forward" 2]])

;; Your horizontal position and depth both start at 0.

(def initial-position [0 0])

;; The steps above would then modify them as follows:
;;
;; - forward 5 adds 5 to your horizontal position, a total of 5.
;; - down 5 adds 5 to your depth, resulting in a value of 5.
;; - forward 8 adds 8 to your horizontal position, a total of 13.
;; - up 3 decreases your depth by 3, resulting in a value of 2.
;; - down 8 adds 8 to your depth, resulting in a value of 10.
;; - forward 2 adds 2 to your horizontal position, a total of 15.

(defn navigate-submarine
  "Navigate the submarine from initial position following all the
  supplied commands."
  [initial-position commands]
  (reduce submarine-command initial-position commands))

;; After following these instructions, you would have a horizontal
;; position of 15 and a depth of 10. (Multiplying these together
;; produces 150.)

(deftest navigate-submarine-test
  (= [15 10] (navigate-submarine initial-position test-input)))

;; Calculate the horizontal position and depth you would have after
;; following the planned course. What do you get if you multiply your
;; final horizontal position by your final depth?

(def input
  "Load our test input and separate it into vectors containing the
  command as a string and units as an integer."
  (->> "day_02.txt"
       resource
       slurp
       split-lines
       (mapv (comp #(vector (first %) (Integer. (second %)))
                   #(split % #" ")))))

(defn submarine-answer
  "Calculate the product of horizontal and depth position after
  following commands."
  [initial-position commands]
  (let [[h d] (navigate-submarine initial-position commands)]
    (* h d)))

(deftest submarine-answer-test
  (is (= 2039256 (submarine-answer initial-position input))))

;; # Part Two

;; Based on your calculations, the planned course doesn't seem to make
;; any sense. You find the submarine manual and discover that the
;; process is actually slightly more complicated.
;;
;; In addition to horizontal position and depth, you'll also need to
;; track a third value, aim, which also starts at 0.

(def initial-position-with-aim
  [0 0 0])

;; The commands also mean something entirely different than you first thought:
;;
;; - down X increases your aim by X units.
;; - up X decreases your aim by X units.
;; - forward X does two things:
;;   - It increases your horizontal position by X units.
;;   - It increases your depth by your aim multiplied by X.

(defn navigate-submarine-with-aim
  "Navigate the submarine from initial position including an aim
  component, following all the supplied commands."
  [initial-position commands]
  (reduce (fn [[horizontal depth aim] [command units]]
            (case command
              "forward" [(+ horizontal units) (+ depth (* aim units)) aim]
              "down" [horizontal depth (+ aim units)]
              "up" [horizontal depth (- aim units)]))
          initial-position
          commands))

;; Again note that since you're on a submarine, down and up do the
;; opposite of what you might expect: "down" means aiming in the
;; positive direction.
;;
;; Now, the above example does something different:
;;
;; - forward 5 adds 5 to your horizontal position, a total of
;;   5. Because your aim is 0, your depth does not change.
;; - down 5 adds 5 to your aim, resulting in a value of 5.
;; - forward 8 adds 8 to your horizontal position, a total of
;;   13. Because your aim is 5, your depth increases by 8*5=40.
;; - up 3 decreases your aim by 3, resulting in a value of 2.
;; - down 8 adds 8 to your aim, resulting in a value of 10.
;; - forward 2 adds 2 to your horizontal position, a total of
;;   15. Because your aim is 10, your depth increases by 2*10=20 to a
;;   total of 60.

;; After following these new instructions, you would have a horizontal
;; position of 15 and a depth of 60. (Multiplying these produces 900.)

(deftest navigate-submarine-with-aim-test
  (let [[horizontal depth _] (navigate-submarine-with-aim initial-position-with-aim test-input)]
    (is (= 15 horizontal))
    (is (= 60 depth))))

;; Using this new interpretation of the commands, calculate the
;; horizontal position and depth you would have after following the
;; planned course. What do you get if you multiply your final
;; horizontal position by your final depth?

(defn submarine-aim-answer
  "Calculate the product of horizontal and depth position after
  following commands."
  [initial-position commands]
  (let [[h d _] (navigate-submarine-with-aim initial-position commands)]
    (* h d)))

(deftest submarine-aim-answer-test
  (is (= 1856459736 (submarine-aim-answer initial-position-with-aim input))))
