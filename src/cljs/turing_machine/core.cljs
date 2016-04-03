(ns turing-machine.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]))

(enable-console-print!)
;; -------------------------
;; Views

(def blank "\u00a0")

; Wrapper functions to retrieve parts of an instruction
(defn read-symbol [instruction]
  (instruction 0))

(defn next-state [instruction]
  (instruction 1))

(defn write-symbol [instruction]
  (instruction 2))

(defn action [instruction]
  (instruction 3))

(def initial-description {
  :tape {0 "-", 1 "1", 2 "1", 3 "1", 4 ":", 5 "1", 6 "1"}
  :position 0
  :state "S1"
  :instructions {
    ; current state, current symbol (can be :any), next state, next symbol, action

    ; S1, keep moving right until you hit :
    "S1" [["-" "S1" "-" "R"]
          ["1" "S1" "1" "R"]
          ["b" "S1" "b" "R"] ; can probably remove
          [":" "S2" ":" "R"]]
    ; S2, keep moving R until you hit 1, write "a", S3
    "S2" [["a" "S2" "a" "R"]
          ["1" "S3" "a" "L"]
          [blank "S8" blank "L"]]
    ; S3, keep moving L until you hit - -> S4
    "S3" [[":" "S3" ":" "L"]
          ["a" "S3" "a" "L"]
          ["b" "S3" "b" "L"]
          ["1" "S3" "1" "L"]
          ["-" "S4" "-" "R"]]
    ; S4, keep moving R until you hit 1, wtite "b" -> S5
    ; if you hit : -> S2
    "S4" [["b" "S4" "b" "R"]
          ["1" "S5" "b" "L"]
          [":" "S7" ":" "L"]]
    ; S5, move L until you hit " ", write 1 -> S6
    ; could hit b, = or 1
    "S5" [["b" "S5" "b" "L"]
          ["-" "S5" "-" "L"]
          ["1" "S5" "1" "L"]
          [blank "S6" "1" "R"]]
    ; S6 keep moving R until you hit - -> S4
    "S6" [["1" "S6" "1" "R"]
          ["-" "S4" "-" "R"]]
    ; S7 move L overwriting bs with 1s until -
    ; when you hit - -> S1
    "S7" [["b" "S7" "1" "L"]
          ["-" "S1" "-" "R"]]
    ; keep moving L overwriting everything until you get to -
    ; could hit
    "S8" [["a" "S8" blank "L"]
          ["b" "S8" blank "L"]
          ["1" "S8" blank "L"]
          [":" "S8" blank "L"]
          ["-" "halt" blank "L"]]
  }
})

(defn get-tape-value [tape index]
  "Returns the value of the tape map at index if it has one, otherwise Returns
   a space"
  (if-let [val (get tape index)] val blank))

(defn next-instruction [instructions state tape-value]
  (let [state-instructions (get instructions state)
        match-symbol (fn [row]
                      (if (= (read-symbol row) tape-value)
                        row
                        nil))]
    (some match-symbol state-instructions)))

(defn machine-iter [description]
  (if (= (:state description) "halt")
    description
    (let [position (:position description)
        tape (:tape description)
        instructions (:instructions description)
        state (:state description)
        tape-value (get-tape-value tape position)
        next-instruction (next-instruction instructions state tape-value)
        next-state (next-instruction 1)
        next-symbol (next-instruction 2)
        action (next-instruction 3)
        ]
      {
      :tape (assoc tape position next-symbol)
      :position (cond
        (= action "R") (inc position)
        (= action "L") (dec position)
        (= action "none") position)
      :state next-state
      :instructions instructions
      })))

(defn state-row
  "Render an instruction row that is meant to show state."
  [is-curr-state row]
  (let [state (first row)
        instructions (rest row)
        state-selected (if is-curr-state "state-selected" "")]
    [:tr {:class "state-row"} (conj (vector :td {:class state-selected} state)
            (map #(vector :td %) instructions))]))

(defn normal-row
  "Render an instruction row with no state (left pad)."
  [is-curr-state symbol row]
    (let [is-selected (and is-curr-state (= symbol (read-symbol row)))]
      (into [:tr {:class (if is-selected "selected" "")} [:td ""]]
            (map #(vector :td %) row))))

(defn single-state-table
  "Render the instructions for a single state."
  [state symbol state-instructions]
        (let [first-row (into [(first state-instructions)]
                          (first (last state-instructions)))
              rest-rows (rest (last state-instructions))
              state-selected (= state (first state-instructions))]
        (cons (state-row state-selected first-row)
          (map (partial normal-row state-selected symbol)
            rest-rows))))

(defn instruction-table
  "Create table element and then loop over each item in instruction map."
  [symbol state instructions]
  (let [state-table instructions]
    [:table
      {:class "instructions"}
      [:tr
        [:th "State"]
        [:th "Read Symbol"]
        [:th "Next state"]
        [:th "Write symbol"]
        [:th "action"]]
        (let [rows (apply concat
                      (map (partial single-state-table state symbol) instructions))]
          rows)]))

(defn list-tape [items]
  [:ul {:class "tape"}
   (for [item items]
     [:li {:class (if (:selected item) "selected" "")}
       (:val item)])])

(defn enumerate-tape [tape position]
  (let [buffer 10
        tape-range
        (range
          (- position (- buffer 1))
          (+ position buffer))]
    (map (fn [index]
             {
               :val (get-tape-value tape index)
               :selected (= index position)
             })
      tape-range)))

(defn show-machine [description]
  (let [tape (enumerate-tape
          (:tape description) (:position description))
        instructions (:instructions description)
        symbol (get-tape-value (:tape description) (:position description))
        state (:state description)]
    [:div {:class "machine"}
      [instruction-table symbol state instructions]
      [:h3 "State: " (str (:state description))]
      [list-tape tape]]))

(def machine-description
  (reagent/atom initial-description))

(def machine-running
  (reagent/atom false))

(js/setInterval
  #(if @machine-running
    (swap! machine-description machine-iter))
  500)

(defn home-page []
  [:div [:h2 "Turing Machine"]
    (show-machine @machine-description)
    [:div
      [:button {:on-click #(swap! machine-running not)}
        (if @machine-running "Stop" "Run")]
      [:button
        {:on-click (if (not @machine-running)
                      #(swap! machine-description machine-iter))}
                   "Step"]
      [:button
        {:on-click #(do (reset! machine-description initial-description)
                        (reset! machine-running false))} "Reset"]]])

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!)
  (accountant/dispatch-current!)
  (mount-root))
