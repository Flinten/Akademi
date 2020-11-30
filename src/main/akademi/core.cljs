(ns akademi.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]))
(defonce skin (r/atom :basic))
(defonce debug-mode (r/atom false))
(defonce color-palette (r/atom [:red :green :blue ]))
(defonce color (r/atom :blue))
(defonce selected-ids (r/atom #{104}))

(defonce draw-objs ;
  (r/atom [{:id 1 :type :container :pos [100,50], :size [40,40]}
           {:id 2 :type :container :pos [25,50], :size [75,75]}
           {:id 103 :type :box :pos [0,0], :size [50,50] :parent 1 :text "Sofie"}
           {:id 104 :type :box :pos [50,0], :size [50,50] :parent 2}
           {:id 105 :type :box :pos [100,100], :size [50,50] :parent 2}
           ]))
(defn swap-obj! [id f & args]
  (swap! draw-objs
         (fn [xs]
           (map #_(fn [{i :id :as ob}]
                    (if (= id i) (apply f ob args) ob))
                #(if (= id (:id %)) (apply f % args) %)
                xs))))

(defn render-container [{[x y] :pos [w h] :size}]
       [:rect {:x x :y y
               :width w :height h
               :style {:fill :red :stroke :blue}}]
  )
(def basic-skin {:box (fn [{[x y] :pos [w h] :size c :color :keys [selected id] :as ob}]
            [:rect {:x x :y y 
                    :on-click (fn[_] (swap! selected-ids #(if (% id) #{}  #{id} )))
                    ;(js/alert (pr-str ob))
                    :width w :height h
                    :style {:fill (get @color-palette c :khaki)
                            :stroke (if selected :black :blue)
                            :stroke-width (if selected 4 2)
                            }}])
                 }
  
  )

(defn left-pad
  ([s len]
   (left-pad s len "0"))
  ([s len ch]
   (let [width     (count s)
         pad-width (- len width)]
     (cond->> s
       (pos? pad-width)
       (str (reduce str "" (repeat pad-width ch)))))))

(defonce palettes {:blue (mapv #(str "#0000" (left-pad (.toString % 16) 2)) (range 256))
                   :red (mapv #(str "#" (left-pad (.toString % 16) 2) "0000") (range 256))
                   :green (mapv #(str "#00" (left-pad (.toString % 16) 2) "00") (range 256))})

(defn get-color [idx]
  (let [dx (/ 256 (count @draw-objs))]
    (cond
      (= idx 0) (get (get-in palettes [@color]) idx :red)
      :else (get (get-in palettes [@color]) (- (* dx (+ idx 1)) 1) :red)))
  )

(def render-fns {:default {:box (fn [{[x y] :pos [w h] :size}]
                                  [:rect {:x x :y y
                                          :width w :height h
                                          :style {:fill :green :stroke :blue}}])
                           :container render-container
                           }
                 :basic basic-skin 
                 :presentation (merge basic-skin {:container (constantly nil)})
                 })

;______________tegneflade_________________
 ;Tilføj nyt box objekt der bliver placeret på random pos
(defn new-box [obj-list]
  (conj obj-list 
        {:id (inc (apply max (map :id obj-list)))
         :type :box
         :pos [(rand-int 500),(rand-int 500)]
         :size (let [w (+ 10 (rand-int 80))
                     h (+ 10 (rand-int 40))]
                 [w h])}))

(defn volume-sort [xs]
  (sort-by (fn [{s :size}] (apply * s)) xs))

(defn align-left "Flytter objecter til venster" [xs]
  (let [xs (->> xs 
                volume-sort 
                (map #(dissoc % :i ))
                (map-indexed (fn [i x] (assoc x :color i )) ))]
    (loop [dy 0
           resul []
           [{[_ h] :size :as obj} & rest-xs]
           xs]
      (let [obj (assoc obj :pos [0 dy])]
        (if rest-xs
          (recur (+ dy h 5)
                 (conj resul obj)
                 rest-xs)
          (conj resul obj))))))

(defn organiser-obj "Organiser box og container" [xs]
  (let [m (group-by :parent xs)
        m (into {}
                (for [[id children] m :when id]
                  [id {:n (count children)
                       :size [(apply + (map (comp first :size) children))
                              (apply max (map (comp second :size) children))]}]))] ; m indeholder forhvert contationer id et map med antal children og størrelse
    (js/console.log (clj->js m))
    (tap> m) ; tap> kommado skriver til http://localhost:9631/inspect (tap) evt. kig i cheatsheet
    xs))

(defn control-area []
  [:div
   [:button {:on-click (fn [_] (swap! draw-objs new-box))} "Ny kasse"]
   [:button {:on-click #(swap! draw-objs align-left)} "Flyt kasser"]
   [:button {:on-click #(swap! draw-objs organiser-obj)} "Organiser"]
   [:button {:on-click #(swap! debug-mode not)} "debug on/off"]
   [:label "Skin:"
    [:select {:selected (str @skin)
              :on-change (fn [event] (reset! skin (keyword (.-value (.-target event)))))}
     (doall (for [x (sort (keys render-fns))]
              ^{:key (name x)} [:option (name x)]))]]
   [:label "Color:"
    [:select {:selected (str @color)
              :on-change (fn [event] (reset! color (keyword (.-value (.-target event)))))}
     (doall (for [x (sort (keys palettes))]
              ^{:key (name x)} [:option (name x)]))]]])

(defn render-obj-debug [obj] [:div (pr-str obj)])

(defn draw-area [xs] ;xs er en collection 
  (let [objs (doall
              (for [x xs]
                ^{:key (:id x)} [(or
                                  (when @debug-mode render-obj-debug)
                                  (get-in render-fns [@skin (:type x)])
                                  (get-in render-fns [:default (:type x)])
                                  (fn [_] [:div "Ukendt obj"])) 
                                 (assoc x :selected (@selected-ids (:id x)))]
                ))]
    (if @debug-mode
      [:div objs]
      [:svg {:width 500, :height 500, :style {:background-color :linen}} objs])))

(defn obj-color [{:keys [color]}]
  [:div [:label "Color: "
         [:select {:selected (str color)
                   #_#_:on-change (fn [event] (reset! color (keyword (.-value (.-target event)))))}
          (doall (for [x (sort (keys palettes))]
                   ^{:key (name x)} [:option (name x)]))]]])

(defn obj-text [{:keys [text id]}]
  [:div [:label "Text: "
         [:input {:type :text
                  :value (str text)
                  :placeholder "Her kan der står en tekst."
                  :on-change (fn [ev] (swap-obj! id #(assoc % :a ev)))}]]])

(defn obj-properties [obj]
  [:div "ID: " (:id obj) [:br] 
   [obj-text obj]
   [obj-color obj] [:hr]
   "mere her"]
  )


(defn mini-app []
  [:div [control-area]
   [:table [:tr 
            [:td [draw-area @draw-objs]] 
            [:td {:valign :top}
             (let [[x] (filter (comp @selected-ids :id) @draw-objs)]
               (when x [obj-properties x]))]]]])

(defn ^:export run []
  (rdom/render [mini-app] (js/document.getElementById "app")))

(defn ^:export reload []
  (.log js/console "reload...")
  (run))