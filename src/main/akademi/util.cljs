(ns akademi.util)

(defn index-by
 "Her skal vi lige have dokumenteret!!!"
  [f xs]
  (into {} (for [x xs] [(f x) x])))

(defn to-tap [x]
  (tap> x)
  x)

(defn map-vals 
  "Anvender funktion på alle værdier i et map og retunerer det opdaterede map"
  [f m]
  (into {} (for [[k v] m] [k (f v)]))) #_ "udfører "

