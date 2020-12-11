(ns akademi.util)

(defn index-by
 "Her skal vi lige have dokumenteret!!!"
  [f xs]
  (into {} (for [x xs] [(f x) x])))