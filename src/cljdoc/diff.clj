(ns cljdoc.diff
  (:require [clojure.set :refer [difference]]))

(defn group-ns [namespaces]
  (map (fn [ns] [(:name ns) ns]) namespaces))

(defn diff-ns [namespaces-a namespaces-b]
  (let [namespaces-compare-group-1 (group-ns namespaces-a)
        namespaces-compare-group-2-lookup (into {} (group-ns namespaces-b))]
    (map
     (fn [[k ns]]
       (cond-> ns
         (nil? (namespaces-compare-group-2-lookup k))
         (assoc :diff ::removed)))
     namespaces-compare-group-1)))

(defn group-vars [vars]
  (map (fn [var] [((juxt :namespace :name) var) var]) vars))

(defn diff-vars [vars-a vars-b]
  (let [vars-compare-group-1 (group-vars vars-a)
        vars-compare-group-2-lookup (into {} (group-vars vars-b))]
    (map
     (fn [[k var]]
       (let [var-in-group2 (vars-compare-group-2-lookup k)]
         (if (nil? var-in-group2)
           (assoc var :diff ::removed)
           (update var :arglists
                   #(map
                    (fn [arglist]
                      (if ((set (:arglists var-in-group2)) arglist)
                        arglist
                        [::removed-arities arglist]))
                    %)))))
     vars-compare-group-1)))

(defn diff [bundle-a bundle-b]
  (-> bundle-a
      (update :namespaces #(diff-ns % (:namespaces bundle-b)))
      (update :defs #(diff-vars % (:defs bundle-b)))))
