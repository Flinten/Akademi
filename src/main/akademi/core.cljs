(ns akademi.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]))
(def initial-objs [{:id 1 :type :container :pos [100,50], :size [40,40]}
                   {:id 2 :type :container :pos [25,50], :size [75,75]}
                   {:id 103 :type :box :pos [0,0], :size [50,50] :parent 1 :text "Sofie"}
                   {:id 104 :type :box :pos [50,0], :size [50,50] :parent 2}
                   {:id 105 :type :box :pos [100,100], :size [50,50] :parent 2}
                   {:id 201 :type :ellipse :pos [200,200], :size [100,50] :parent 2}])
(defonce skin (r/atom :basic))
(defonce debug-mode (r/atom false))
(defonce color-palette (r/atom [:red :green :blue]))
(defonce color (r/atom :blue))
(defonce selected-ids (r/atom #{104}))
(defonce history (r/atom {0 initial-objs})) ; map til vores historik
(defonce current-ts (r/atom 0))
(defonce preview-ts (r/atom nil))

(defonce draw-objs ;
  (r/atom initial-objs))
(add-watch draw-objs :history (fn [_ _ old new]
                                (when-not
                                 (= old new)
                                  (let [ts (.now js/Date)]
                                    (when @current-ts 
                                      (swap! history conj {ts new})
                                      (reset! current-ts ts))))))

(defn swap-obj! [id f & args]
  ; NB! - udnytter at clojurescript er single threaded
  (let [xs @draw-objs
        xs' (map #_(fn [{i :id :as ob}]
                     (if (= id i) (apply f ob args) ob))
                 #(if (= id (:id %)) (apply f % args) %)
                 xs)]
    (when-not (= xs xs')
      (reset! draw-objs xs')))
  nil)
(defn render-container [{[x y] :pos [w h] :size}]
  [:rect {:x x :y y
          :width w :height h
          :style {:fill :red :stroke :blue}}])
(def basic-skin {:box (fn [{[x y] :pos [w h] :size c :color :keys [selected id]}]
                        [:rect {:x x :y y
                                :on-click (fn [_] (swap! selected-ids #(if (% id) #{}  #{id})))
                    ;(js/alert (pr-str ob))
                                :width w :height h
                                :style {:fill (get @color-palette c :khaki)
                                        :stroke (if selected :black :blue)
                                        :stroke-width (if selected 4 2)}}])
                 :ellipse (fn [{[x y] :pos [w h] :size c :color :keys [selected id]}]
                            [:ellipse {:cx (+ (/ w 2) x) :cy (+ (/ h 2) y)
                                       :on-click (fn [_] (swap! selected-ids #(if (% id) #{}  #{id})))
                    ;(js/alert (pr-str ob))
                                       :rx (/ w 2) :ry (/ h 2)
                                       :style {:fill (get @color-palette c :green)
                                               :stroke (if selected :black :blue)
                                               :stroke-width (if selected 4 2)}}])})
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
      :else (get (get-in palettes [@color]) (- (* dx (+ idx 1)) 1) :red))))

(def render-fns {:default {:box (fn [{[x y] :pos [w h] :size}]
                                  [:rect {:x x :y y
                                          :width w :height h
                                          :style {:fill :green :stroke :blue}}])
                           :container render-container}
                 :basic basic-skin
                 :presentation (merge basic-skin {:container (constantly nil)})})

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
                (map #(dissoc % :i))
                (map-indexed (fn [i x] (assoc x :color i))))]
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

(defn target-value [event] 
  (.-value (.-target event))
  )

(defn control-area []
  [:div
   [:button {:on-click (fn [_] (swap! draw-objs new-box))} "Ny kasse"]
   [:button {:on-click #(swap! draw-objs align-left)} "Flyt kasser"]
   [:button {:on-click #(swap! draw-objs organiser-obj)} "Organiser"]
   [:button {:on-click #(swap! debug-mode not)} "debug on/off"]
   [:label "Skin:"
    [:select {:selected (str @skin)
              :on-change (fn [event] (reset! skin (keyword (target-value event))))}
     (doall (for [x (sort (keys render-fns))]
              ^{:key (name x)} [:option (name x)]))]]
   [:label "Color:"
    [:select {:selected (str @color)
              :on-change (fn [event] (reset! color (keyword (target-value event))))}
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
                                 (assoc x :selected (@selected-ids (:id x)))]))]
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
  [:div
   [:label "Text: "
    ^{:key id} [:input {:type :text
                        :default-value (str text)
                        :placeholder "Her kan der stå en tekst"
                        :on-change #(swap-obj! id assoc :text (target-value %))}]]])

(defn obj-type [{:keys [type id]}]
  [:div [:label "Type: "
        ^{:key id} [:select {:value type
                             :on-change #(swap-obj! id assoc :type (keyword (target-value %)))}
          (doall (for [[k v] [[:box "Kasse"] [:ellipse "Ellipse"]]]
                   ^{:key (name k)} [:option {:value k} v]))]]])
(defn goto-history "Går til det angivne tidspunkt i historikken. TIMETRAVEL!!!!"[ts]
  (reset! current-ts nil)
  (reset! draw-objs (get @history ts))
  (reset! current-ts ts))

(defn drawing-history []
  [:div (count @history) " ændring(er)" 
   [:button {:disabled (when (= @current-ts (apply min (keys @history))) :disabled)} "Undo"]
   [:button {:disabled (when (= @current-ts (apply max (keys @history))) :disabled)} "Redo"]
   (doall (for  [[ts x] (reverse (sort @history))] ; sorter history efter tid (reverse = sidtse ændring øverst)
            ^{:key ts} [:div {:style {:cursor :pointer :text-decoration :underline 
                                      :color (cond (= ts @current-ts) :green 
                                                   (= ts @preview-ts) :purple )}
                              :on-click #(goto-history ts)
                              :on-mouse-over #(reset! preview-ts ts)
                              :on-mouse-out #(reset! preview-ts)}
                        (count x) " objekter: " (.substring (.toString (js/Date. ts)) 4 24)]))])

(defn obj-properties [obj]
  [:div "ID: " (:id obj) [:br]
   [obj-type obj]
   [obj-text obj]
   [obj-color obj] [:hr]
   ])

(defn mini-app []
  (let [xs (or (get @history @preview-ts) @draw-objs)] [:div [control-area]
   [:table
    [:tr
     [:td [draw-area xs] (when @debug-mode
                           [:div [:hr] [:pre (with-out-str (cljs.pprint/pprint @history))]])]
        ;consol output fanges og "formateres" råt
     [:td {:valign :top} [drawing-history]]
     [:td {:valign :top}
      (let [[x] (filter (comp @selected-ids :id) xs)]
        (when x [obj-properties x]))]]]]))

(defn ^:export run []
  (rdom/render [mini-app] (js/document.getElementById "app")))

(defn ^:export reload []
  (.log js/console "reload...")
  (run))