(ns akademi.core
  (:require
   [akademi.util :as util]
   [reagent.core :as r]
   [reagent.dom :as rdom]))

(def initial-objs "Map der indeholder Start-up objs"
  {1 {:id 1 :type :container :pos [100,50], :size [40,40]}
   2 {:id 2 :type :container :pos [25,50], :size [75,75]}
 103 {:id 103 :type :box :pos [0,0], :size [50,50] :parent 1 :text "Sofie"}
 104 {:id 104 :type :box :pos [50,0], :size [50,50] :parent 2}
 105 {:id 105 :type :box :pos [100,100], :size [50,50] :parent 2}
 201 {:id 201 :type :ellipse :pos [200,200], :size [100,50] :parent 2}})

(defonce skin (r/atom :basic))
(defonce debug-mode (r/atom false))
(defonce color-palette (r/atom [:red :green :blue]))
(defonce color (r/atom :blue))
(defonce selected-ids (r/atom #{}))
(defonce clipboard (r/atom {}))
(defonce history (r/atom {0 initial-objs})) ; map til vores historik 0 er timestamp
(defonce current-ts (r/atom 0))
(defonce preview-ts (r/atom nil))
(defonce draw-objs (r/atom initial-objs))

(defonce draw-objs (r/atom initial-objs))
(add-watch draw-objs :history (fn [_ _ old new] ;"ser om vores objs ændres og opdaterer da historikken "
                                (when-not
                                 (= old new)
                                  (let [ts (.now js/Date)]
                                    (when @current-ts
                                      (swap! history conj {ts new})
                                      (reset! current-ts ts))))))

(defn swap-obj! [id f & args]
  ; NB! - udnytter at clojurescript er single threaded
  (let [m @draw-objs
        m' (apply update m id f args)] ;snak om den her sener
    (when-not (= m m')
      (reset! draw-objs m'))))
(defn obj-click-handler
  [id]
  (fn [event] (swap! selected-ids #(-> (if (.-shiftKey event) % #{})
                                       (conj id)
                                       (disj (% id))))))
(defn render-container [{[x y] :pos [w h] :size}]
  [:rect {:x x :y y
          :width w :height h
          :style {:fill :red :stroke :blue}}])
(defn add-svg-text [svg-obj {[x y] :pos [w h] :size :keys [text]}]
  [:svg {:x x :y y :overflow :visible}
   svg-obj
   [:text {:x 0  :y (+ 10 (/ h 2)) :font-size 20} text]])
(def basic-skin {:box (fn [{[x y] :pos [w h] :size c :color :keys [selected id] :as obj}]
                        (add-svg-text
                         [:rect {:x 0 :y 0
                                 :on-click (obj-click-handler id)
                                 :width w :height h
                                 :style {:fill (get @color-palette c :khaki)
                                         :stroke (if selected :black :blue)
                                         :stroke-width (if selected 4 2)}}]
                         obj))
                 :ellipse (fn [{[x y] :pos [w h] :size c :color :keys [selected id] :as obj}]

                            (add-svg-text
                             [:ellipse {:cx (/ w 2) :cy (/ h 2)
                                        :on-click (obj-click-handler id)
                                        :rx (/ w 2) :ry (/ h 2)
                                        :style {:fill (get @color-palette c :green)
                                                :stroke (if selected :black :blue)
                                                :stroke-width (if selected 4 2)}}]
                             obj))})
(defn left-pad "Left padder string med nuller, "
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
 (let [id (inc (apply max (keys obj-list)))] (assoc obj-list id
        {:id id 
         :type :box
         :pos [(rand-int 500),(rand-int 500)]
         :size (let [w (+ 10 (rand-int 80))
                     h (+ 10 (rand-int 40))]
                 [w h])})))

(defn volume-sort [xs]
  (sort-by (fn [{s :size}] (apply * s)) xs))

(defn align-left' "Flytter objecter til venster" [xs]
  (let [xs (->> xs 
                volume-sort
                
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

(defn align-left [objs]
  (util/index-by :id (align-left' (vals objs))))



(defn target-value [event]
  (.-value (.-target event)))

(defn delete-select-ids [xs]
  (let [xs' @selected-ids]
    (reset! selected-ids #{})
    (apply dissoc xs xs')))

(defn prepare-for-paste [objs] 
  (let [n (inc (apply max (keys objs)))] 
    (util/index-by :id
                   (map-indexed (fn [i x ] (assoc x :id (+ n i) )) (vals objs)))))
(defn control-area []
  [:div
   [:button {:on-click (fn [_] (swap! draw-objs new-box))} "Ny kasse"]
   [:button {:on-click #(swap! draw-objs align-left)} "Flyt kasser"]
   [:button {:on-click #(swap! debug-mode not)} "debug on/off"]
   [:button {:on-click #(swap! draw-objs delete-select-ids)
             :disabled (when-not  (seq @selected-ids) :disabled)} "Slet"]
   [:button {:on-click #(reset! clipboard (select-keys @draw-objs @selected-ids))
             :disabled (when-not  (seq @selected-ids) :disabled)} "Copy"]
   [:button {:on-click #(swap! draw-objs merge (prepare-for-paste @clipboard))
             :disabled (when-not  (seq @clipboard ) :disabled)} "Paste"]
   [:button {:on-click #(swap! draw-objs merge @clipboard)
             :disabled (when-not  (seq @clipboard) :disabled)} "Merge"]



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
              (for [x (vals xs)]
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
(defn change-text-of-multiple [ids text]
  (doseq [id ids] (swap-obj! id assoc :text text)))
(defn obj-texts [[{:keys [text id]} :as xs]]
  (let [multi (->>
               xs
               (map :text)
               distinct
               next)]
    [:div
     [:label "Text: "
      ^{:key id} [:input {:type :text
                          :default-value (str (when-not multi text))
                          :placeholder (if multi "multi text" "Text")
                          :on-change #(change-text-of-multiple @selected-ids (target-value %))}]]]))


(defn change-type-of-multiple [ids type]
  (doseq [id ids] (swap-obj! id assoc :type type)))

(defn obj-types [[{:keys [type id]} :as xs]]
  (let [multi (next (distinct (map :type xs)))]
    [:div [:label "Type: "
           ^{:key id} [:select {:value (if multi "" type)
                                :on-change #(change-type-of-multiple @selected-ids (keyword (target-value %)))}
                       (doall (concat (when multi (list [:option {:value ""} "Flere typer"]))
                                      (for [[k v] [[:box "Kasse"] [:ellipse "Ellipse"]]]
                                        ^{:key (name k)} [:option {:value k} v])))]]]))
(defn goto-history "Går til det angivne tidspunkt i historikken. TIMETRAVEL!!!!" [ts]
  (reset! current-ts nil)
  (reset! draw-objs (get @history ts))
  (reset! current-ts ts))
#_(let [get-prev-next-ts (fn [order min-max]
                           (when-let [xs (->> @history
                                              keys
                                              (filter #(order % @current-ts))
                                              seq)]
                             (apply min-max xs)))]
    (def get-next-ts (partial get-prev-next-ts > min))
    (def get-previous-ts (partial get-prev-next-ts < max)))
(defn prev-next-fn
  "Returnerer en funktion som kan rejse frem eller tilbage i historiken"
  [direction]
  (fn []
    (when-let [xs (->> @history
                       keys
                       (filter #(direction % @current-ts))
                       seq)]
      (apply ({< max, > min} direction) xs))))
(def get-next-ts (prev-next-fn >))
(def get-previous-ts (prev-next-fn <))
(defn drawing-history []
  [:div (count @history) " ændring(er)"
   [:button {:disabled (when-not (get-previous-ts) :disabled)
             :on-click #(goto-history (get-previous-ts))}
    "Undo"]
   [:button {:disabled (when-not (get-next-ts) :disabled)
             :on-click #(goto-history (get-next-ts))} "Redo"]
   (doall (for  [[ts x] (reverse (sort @history))] ; sorter history efter tid (reverse = sidtse ændring øverst)
            ^{:key ts} [:div {:style {:cursor :pointer :text-decoration :underline
                                      :color (cond (= ts @current-ts) :green
                                                   (= ts @preview-ts) :purple)}
                              :on-click #(goto-history ts)
                              :on-mouse-over #(reset! preview-ts ts)
                              :on-mouse-out #(reset! preview-ts nil)}
                        (count x) " objekter: " (.substring (.toString (js/Date. ts)) 4 24)]))])

(defn obj-properties [[obj :as xs]]
  [:div "ID: " (:id obj) (when (next xs) " med flere") [:br]
   [obj-types xs]

   [obj-texts xs]
   [obj-color obj] [:hr]])

(defn mini-app []
  (let [xs (or (get @history @preview-ts) @draw-objs)
        by-id (util/index-by :id xs)]
    [:div [control-area]
     [:table
      [:tr
       [:td {:valign :top} [draw-area xs] (when @debug-mode
                                            [:div [:hr] [:pre (with-out-str (cljs.pprint/pprint @history))]])]
        ;consol output fanges og "formateres" råt 
       [:td {:valign :top} [:div {:style {:max-height 500 :overflow :auto}} [drawing-history]]]
       [:td {:valign :top}
        (when (seq @selected-ids) [obj-properties (map by-id @selected-ids)])]]]]))

(defn ^:export run []
  (rdom/render [mini-app] (js/document.getElementById "app")))

(defn ^:export reload []
  (.log js/console "reload...")
  (run))