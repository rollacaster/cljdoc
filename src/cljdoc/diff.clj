(ns cljdoc.diff
  (:require [clojure.set :refer [difference]]
            [clojure.spec.alpha :as spec]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]))

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

(defn group-vars [vars]
  (map (fn [var] [((juxt :namespace :name) var) var]) vars))

(defn diff-vars [vars-a vars-b]
  (let [vars-compare-group-1 (group-vars vars-a)
        vars-compare-group-2-lookup (into {} (group-vars vars-b))]
    (map
     (fn [[k var]]
       (let [var-in-group2 (vars-compare-group-2-lookup k)
             var-removed (nil? var-in-group2)
             arity-removed (seq (difference (set (:arglists var)) (set (:arglists var-in-group2))))]
         (cond
           var-removed (assoc var :diff ::removed)
           arity-removed (-> var
                             (assoc :diff ::arity-removed)
                             (update :arglists
                                     #(map
                                       (fn [arglist]
                                         (if ((set (:arglists var-in-group2)) arglist)
                                           arglist
                                           [::removed-arities arglist]))
                                       %)))
           :else var)))
     vars-compare-group-1)))

(defn diff [bundle-a bundle-b]
  (-> bundle-a
      (update :namespaces #(diff-ns % (:namespaces bundle-b)))
      (update :defs #(diff-vars % (:defs bundle-b)))))

(spec/fdef compare-versions
  :args (spec/cat :versions (spec/coll-of :cljdoc.spec/version))
  :ret (spec/coll-of
        (spec/tuple :cljdoc.spec/version :cljdoc.spec/version)))

(defn compare-versions [versions]
  (reduce
   (fn [c-versions [idx to]]
     (if (> (dec (count versions)) idx)
       (conj c-versions [(nth versions (inc idx)) to])
       c-versions))
   []
   (map-indexed vector versions)))
