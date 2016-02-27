(ns turing-machine.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]))

(enable-console-print!)
;; -------------------------
;; Views

(def blank "\u00a0")

(def initial-description {
  :tape {0 "-", 1 "1", 2 "1", 3 "1", 4 "1", 5 ":", 6 "1", 7 "1"}
  :position 0
  :state "init"
  :instructions [
    ; current state, current symbol (can be :any), next state, next symbol, action (:left, :right, :halt)
    ["init" "-" "one" "a" "right"]
    ["one" "1" "one" "m" "right"]
    ["one" ":" "halt" ":" "none"]
  ]
  })

(defn get-tape-value [tape index]
  "Returns the value of the tape map at index if it has one, otherwise Returns
   a space"
  (if-let [val (get tape index)] val blank))

(defn next-instruction [instructions state tape-value]
  (let [instruction-filter
          (fn [instruction]
            (let [instruction-state (instruction 0)
                  instruction-stymbol (instruction 1)]
                  (when
                    (and (= state instruction-state)
                      (= tape-value instruction-stymbol))
                    instruction)))]
    (some instruction-filter instructions)))

(defn machine-iter [description]
  (if (= (:state description) "halt")
    description
    (let [position (:position description)
        tape (:tape description)
        instructions (:instructions description)
        state (:state description)
        tape-value (get-tape-value tape position)
        next-instruction (next-instruction instructions state tape-value)
        next-state (next-instruction 2)
        next-symbol (next-instruction 3)
        action (next-instruction 4)
        ]
      {
      :tape (assoc tape position next-symbol)
      :position (cond
        (= action "right") (inc position)
        (= action "left") (dec position)
        (= action "none") position)
      :state next-state
      :instructions instructions
      })))

(defn instruction-table [instructions]
  [:table
    {:class "instructions"}
    [:tr
      [:th "State"]
      [:th "Symbol"]
      [:th "Next state"]
      [:th "Next symbol"]
      [:th "action"]]
      (for [instruction instructions]
        (let [row (:instruction instruction)
              selected (:selected instruction)]
          [:tr {:class (if selected "selected" "")}
            [:td (row 0)]
            [:td (row 1)]
            [:td (row 2)]
            [:td (row 3)]
            [:td (row 4)]]))])

(defn list-tape [items]
  [:ul {:class "tape"}
   (for [item items]
     [:li {:class (if (:selected item) "selected" "")}
       (:val item)])])

(defn enumerate-tape [tape position]
  (let [buffer 8
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

(defn format-instructions [instructions state tape position]
  (let [tape-value (get-tape-value tape position)
        selected-instruction (next-instruction instructions state tape-value)
        format-instruction
          (fn [instruction]
            {:instruction instruction
             :selected (= selected-instruction instruction)})]
    (map format-instruction instructions)))

(defn show-machine [description]
  (let [tape (enumerate-tape
          (:tape description) (:position description))
        instructions (format-instructions
          (:instructions description)
          (:state description)
          (:tape description)
          (:position description))]
    [:div {:class "machine"}
      [instruction-table instructions]
      [:h3 "Halted: " (str (= (:state description) "halt"))]
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
