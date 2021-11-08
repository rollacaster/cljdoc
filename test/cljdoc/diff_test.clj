(ns cljdoc.diff-test
  (:require [cljdoc.diff :as sut]
            [clojure.test :as t]))

(t/deftest diff-namespaces
  (t/testing "Removed Namespace"
    (t/is
     (=
      (sut/diff-ns
       #{{:name "reagent.dom.server",
          :platform "cljs",
          :version-entity
          {:id 4, :version "0.10.0", :group-id "reagent", :artifact-id "reagent"}}
         {:name "reagent.impl.protocols",
          :platform "cljs",
          :version-entity
          {:id 3, :version "0.10.0", :group-id "reagent", :artifact-id "reagent"}}}
       #{{:name "reagent.dom.server",
          :platform "cljs",
          :version-entity
          {:id 3, :version "1.1.0", :group-id "reagent", :artifact-id "reagent"}}})
      [{:name "reagent.dom.server",
        :platform "cljs",
        :version-entity
        {:id 4, :version "0.10.0", :group-id "reagent", :artifact-id "reagent"}}
       {:name "reagent.impl.protocols",
        :platform "cljs",
        :version-entity
        {:id 3, :version "0.10.0", :group-id "reagent", :artifact-id "reagent"},
        :diff :cljdoc.diff/removed}]))))
