(ns ^:figwheel-always tictactoe.core
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.test :refer-macros [deftest is run-tests]]))

(enable-console-print!)

(def board-size 3)


(defn create-board [n]
  (vec (repeat n (vec (repeat n "-")))))


(defonce app-state (atom {:text "Welcome to tic tac toe"
                          :board (create-board board-size)
                          :game-status :in-progress}))

(defn computer-move [board]
  (let [moves (for [i (range board-size)
                    j (range board-size)
                    :when (= (get-in board [i j]) "-")]
                [i j])
        move (when (seq moves)
               (rand-nth moves))]
    (if move
      (assoc-in board move "O")
      board)))

(defn full? [board]
  (let [moves (for [i (range board-size)
                    j (range board-size)
                    :when (= (get-in board [i j]) "-")]
                [i j])]
    (= (count moves) 0)))

(defn win? [player board]
  (let [cols (apply mapv vector board)
        diags [[(for [i (range board-size)] (get (get board i) i))]
               [(for [i (range board-size)] (get (get board i) (- (- board-size 1) i)))]]
        combs (apply concat board cols diags)]
    (some true? (for [c combs] (every? (fn [a] (= a player)) c)))))

(defn check-game-status [board]
  (cond
    (win? "X" board) :player-1-win
    (win? "O" board) :player-2-win
    (full? board) :draw
    :else :in-progress))

(defn blank [i j]
  [:rect
   {:width  0.9
    :height 0.9
    :stroke "lightgrey"
    :stroke-width 0.01
    :fill "lightgrey"
    :x      (+ 0.05 i)
    :y      (+ 0.05 j)
    :on-click
      (fn rect-click [e]
        (if (= (:game-status @app-state) :in-progress)
          (do
            (swap! app-state assoc-in [:board j i] "X")
            (swap! app-state assoc-in [:game-status] (check-game-status (:board @app-state)))
            (if (= (:game-status @app-state) :in-progress)
              (swap! app-state update-in [:board] computer-move))
            (swap! app-state assoc-in [:game-status] (check-game-status (:board @app-state))))))}])

(defn circle [i j]
  [:circle
   {:r 0.40
    :stroke "green"
    :stroke-width 0.10
    :stroke-linecap "round"
    :fill "rgba(124, 240, 10, 0)"
    :cx (+ 0.5 i)
    :cy (+ 0.5 j)}])

(defn cross [i j]
  [:g {:stroke "darkred"
       :stroke-width 0.35
       :stroke-linecap "round"
       :transform
       (str "translate(" (+ 0.5 i) "," (+ 0.5 j) ") scale(0.35)")}
   [:line {:x1 -1 :y1 -1 :x2 1 :y2 1}]
   [:line {:x1 1 :y1 -1 :x2 -1 :y2 1}]])

(defn tictactoe []
  [:center
    [:h1 (:text @app-state)]
    [:h3 (name (:game-status @app-state))]
    (into
      [:svg
       {:view-box (str "0 0 " board-size " " board-size)
        :width 400
        :height 400}]
       (for [i (range board-size)
             j (range board-size)]
         (case (get-in @app-state [:board j i])
           "-" [blank i j]
           "X" [cross i j]
           "O" [circle i j])
         ))
   [:p
    {:on-click
     (fn new-game-click [e]
       (swap! app-state assoc :board (create-board board-size))
       (swap! app-state assoc :game-status :in-progress))}
    [:button "New Game"]]])

(reagent/render-component [tictactoe]
                          (. js/document (getElementById "app")))


(deftest win?-test
  (is true (win? "X" [["-" "-" "O"]
                      ["X" "X" "X"]
                      ["O" "-" "O"]]))

  (is nil? (win? "O" [["-" "-" "O"]
                      ["X" "X" "X"]
                      ["O" "-" "O"]]))

  (is true (win? "X" [["-" "O" "X"]
                      ["-" "X" "-"]
                      ["X" "-" "O"]]))

  (is nil? (win? "O" [["-" "O" "X"]
                      ["-" "X" "-"]
                      ["X" "-" "O"]]))

  (is true (win? "X" [["O" "X" "-"]
                      ["-" "X" "-"]
                      ["O" "X" "-"]]))

  (is nil? (win? "O" [["O" "X" "-"]
                      ["-" "X" "-"]
                      ["O" "X" "-"]]))

  (is nil? (win? "X" [["O" "O" "-"]
                      ["X" "X" "-"]
                      ["X" "X" "O"]]))

  (is nil? (win? "O" [["O" "O" "-"]
                      ["X" "X" "-"]
                      ["X" "X" "O"]]))

  (is nil? (win? "X" [["X" "X" "O"]
                      ["O" "X" "O"]
                      ["X" "O" "X"]]))

  (is nil? (win? "O" [["X" "X" "O"]
                      ["O" "X" "O"]
                      ["X" "O" "X"]]))

  (is nil? (win? "X" [["O" "X" "O"]
                      ["O" "O" "X"]
                      ["X" "X" "O"]]))

  (is nil? (win? "O" [["O" "X" "O"]
                      ["O" "O" "X"]
                      ["X" "X" "O"]])))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  ;(swap! app-state assoc-in [:board 0 0] 2)
  (run-tests)
)
