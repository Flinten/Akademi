(ns akademi.core
 (:require [reagent.core :as r]
           [reagent.dom :as rdom]))
(defonce a (r/atom ["Steen" "Casper" ]))
(defonce debug-mode (r/atom false))
(def draw-objs ;
  (r/atom [{:id 1 :type :box :pos [100,100], :size [50,50]}]))
;-------------------simpel tekst eksempel-------
(defn simple-component []
  [:div
   [:p "Jeg er en smuk lille komponent"] 
   [:ul (for[x @a] [:li x])]
   [:button {:onClick (fn[event] 
                        (.log js/console event)
                        (swap! a conj (str "person " (count @a))))}"Jeg er en lille sød knap"]
   [:p.someclass
    "Jeg kan skrive i " [:strong "bold"]
    [:span {:style {:color "red"}} " og rød "] "text."]])

;______________tegneflade_________________
;------Objecter-----
(defn d-obj[{:keys [id type pos size]}]
  (let[[x y ]pos]
  (if @debug-mode 
    [:div (str type " objekt " id)]
    [:rect {:key id, :x x :y y,  :width 50 :height 20 :style {:fill :red :stroke :black} }]))
  )
 ;Tilføj nyt box objekt der bliver placeret på random pos
(defn new-box[obj-list]
  (conj obj-list {:id (inc(apply max (map :id obj-list))) :type :box :pos [(rand-int 500),(rand-int 500)] :size [20 20]})
  )
(defn control-area []
  [:div 
   [:button {:onClick (fn [_] 
                             (swap! draw-objs new-box))} "Ny kasse"]
   [:button {:onClick #(swap! debug-mode not)}"debug on/off"]
   ]
  )
(defn draw-area [xs] ;xs er en collection 
  (let [objs (for [x xs]
               ^{:key (:id x)} [d-obj x])]
    (if @debug-mode
     [:div objs]
      [:svg {:width 500, :height 500, :style {:background-color :black}} objs])))
(defn mini-app[]
[:div [control-area]
 [draw-area @draw-objs]]
)
(defn ^:export run []
 (rdom/render [mini-app] (js/document.getElementById "app")))