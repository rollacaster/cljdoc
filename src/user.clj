(ns user
  #_(:require [portal.api :as p]))

;; (def p (p/open))
;; (add-tap #'p/submit) 

(def bundle-a
  (dissoc (:cache-bundle (read-string (slurp "./bundle-a.edn"))) :version))

(def bundle-b
  (dissoc (:cache-bundle (read-string (slurp "./bundle-b.edn"))) :version))

(defn group-vars [vars]
  (map (fn [var] [((juxt :namespace :name) var) var]) vars))

(group-vars (:defs bundle-b))

(defn diff-ns [namespaces-a namespaces-b]
  (let [namespaces-compare-group-1 (group-vars namespaces-a)
        namespaces-compare-group-2-lookup (into {} (group-vars namespaces-b))]
    (map
     (fn [[k ns]]
       (cond-> ns
         (nil? (namespaces-compare-group-2-lookup k))
         (assoc :diff :cljdoc.diff/removed)))
     namespaces-compare-group-1)))

(->> (diff-ns (:defs bundle-b) (:defs bundle-a))
     (filter :diff)
     (map :name))
(group-vars (:defs bundle-a))
(group-vars (:defs bundle-b))
(:defs bundle-a)

