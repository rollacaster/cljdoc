(ns cljdoc.diff)

(defn group-ns [namespaces]
  (map (fn [ns]
         [(:name ns)
          ns])
       namespaces))

(defn diff-ns [namespaces-a namespaces-b]
  (let [namespaces-compare-group-1 (group-ns namespaces-a)
        namespaces-compare-group-2-lookup (into {} (group-ns namespaces-b))]
    (map
     (fn [[k ns]]
       (cond-> ns
         (nil? (namespaces-compare-group-2-lookup k))
         (assoc :diff :cljdoc.diff/removed)))
     namespaces-compare-group-1)))

(defn diff [bundle-a bundle-b]
  (-> bundle-a
      (update :namespaces #(diff-ns % (:namespaces bundle-b)))))
