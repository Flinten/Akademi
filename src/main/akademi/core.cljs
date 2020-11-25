(ns akademi.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]))
(defonce a (r/atom ["Steen" "Casper"]))
(defonce skin (r/atom :basic))
(defonce debug-mode (r/atom false))
(defonce draw-objs ;
  (r/atom [{:id 1 :type :box :pos [100,100], :size [50,50]}]))

(def render-fns {:default {:box (fn [{[x y] :pos}]
                                  [:rect {:x x :y y
                                          :width 50 :height 20
                                          :style {:fill :green :stroke :blue}}])}
                 :basic {:box (fn [{[x y] :pos [w h] :size :as ob}]
                                [:rect {:x x :y y :on-click #(js/alert (pr-str ob))
                                        :width w :height h
                                        :style {:fill :khaki :stroke :blue}}])}})

;______________tegneflade_________________
 ;Tilføj nyt box objekt der bliver placeret på random pos
(defn new-box [obj-list]
  (conj obj-list {:id (inc (apply max (map :id obj-list))) :type :box :pos [(rand-int 500),(rand-int 500)] :size [20 20]}))

(defn align-left "Flytter objecter til venster" [xs]
  (map (fn [{[_ y] :pos :as obj}]
         (assoc obj :pos [0 y])) xs))

(defn control-area []
  [:div
   [:button {:on-click (fn [_] (swap! draw-objs new-box))} "Ny kasse"]
   [:button {:on-click #(swap! draw-objs align-left)} "Flyt kasser"]
   [:button {:on-click #(swap! debug-mode not)} "debug on/off"]
   [:label "Skin:"
    [:select {:selected (str @skin)
              :on-change (fn [event] (reset! skin (keyword (.-value (.-target event)))))}
     (doall (for [x (sort (keys render-fns))]
              ^{:key (name x)} [:option (name x)]))]]])

(defn render-obj-debug [obj] [:div (pr-str obj)])

(defn draw-area [xs] ;xs er en collection 
  (let [objs (doall
              (for [x xs]
                ^{:key (:id x)} [(or
                                  (when @debug-mode render-obj-debug)
                                  (get-in render-fns [@skin (:type x)])
                                  (get-in render-fns [:default (:type x)])
                                  (fn [_] [:div "Ukendt obj"])) x]))]
    (if @debug-mode
      [:div objs]
      [:svg {:width 500, :height 500, :style {:background-color :linen}} objs])))
(defn mini-app []
  [:div [control-area]
   [draw-area @draw-objs]])

(defn ^:export run []
  (rdom/render [mini-app] (js/document.getElementById "app")))

(defn ^:export reload []
  (.log js/console "reload...")
  (run))