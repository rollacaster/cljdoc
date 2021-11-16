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

(t/deftest diff-vars
  (t/testing "No Change"
    (t/is
     (= (sut/diff-vars
         #{{:name "reagent-class?",
            :file "reagent/impl/component.cljs",
            :line 47,
            :arglists '([c]),
            :type :var,
            :namespace "reagent.impl.component",
            :platform "cljs"}
           {:name "shallow-obj-to-map",
            :file "reagent/impl/util.cljs",
            :line 193,
            :arglists '([o]),
            :type :var,
            :namespace "reagent.impl.util",
            :platform "cljs"}}
         #{{:name "reagent-class?",
            :file "reagent/impl/component.cljs",
            :line 47,
            :arglists '([c]),
            :type :var,
            :namespace "reagent.impl.component",
            :platform "cljs"}
           {:name "shallow-obj-to-map",
            :file "reagent/impl/util.cljs",
            :line 193,
            :arglists '([o]),
            :type :var,
            :namespace "reagent.impl.util",
            :platform "cljs"}})
        [{:name "reagent-class?",
          :file "reagent/impl/component.cljs",
          :line 47,
          :arglists '([c]),
          :type :var,
          :namespace "reagent.impl.component",
          :platform "cljs"}
         {:name "shallow-obj-to-map",
          :file "reagent/impl/util.cljs",
          :line 193,
          :arglists '([o]),
          :type :var,
          :namespace "reagent.impl.util",
          :platform "cljs"}])))

  (t/testing "Removed vars"
    (t/is
     (= (sut/diff-vars
         #{{:name "reagent-class?",
            :file "reagent/impl/component.cljs",
            :line 47,
            :arglists '([c]),
            :type :var,
            :namespace "reagent.impl.component",
            :platform "cljs"}
           {:name "shallow-obj-to-map",
            :file "reagent/impl/util.cljs",
            :line 193,
            :arglists '([o]),
            :type :var,
            :namespace "reagent.impl.util",
            :platform "cljs"}}
         #{{:name "reagent-class?",
            :file "reagent/impl/component.cljs",
            :line 57,
            :arglists '([c]),
            :type :var,
            :namespace "reagent.impl.component",
            :platform "cljs"}
           {:name "shallow-obj-to-map",
            :file "reagent/impl/component.cljs",
            :line 14,
            :arglists '([o]),
            :type :var,
            :namespace "reagent.impl.component",
            :platform "cljs"}})
        [{:name "reagent-class?",
          :file "reagent/impl/component.cljs",
          :line 47,
          :arglists '([c]),
          :type :var,
          :namespace "reagent.impl.component",
          :platform "cljs"}
         {:name "shallow-obj-to-map",
          :file "reagent/impl/util.cljs",
          :line 193,
          :arglists '([o]),
          :type :var,
          :namespace "reagent.impl.util",
          :platform "cljs",
          :diff :cljdoc.diff/removed}])))

  (t/testing "Removed arities"
    (t/is
     (= (sut/diff-vars
         [{:name "cached-react-class",
           :file "reagent/impl/component.cljs",
           :line 68,
           :arglists '([c]),
           :type :var,
           :namespace "reagent.impl.component",
           :platform "cljs"}]
         [{:name "cached-react-class",
           :file "reagent/impl/component.cljs",
           :line 342,
           :arglists '([compiler c]),
           :type :var,
           :namespace "reagent.impl.component",
           :platform "cljs"}])
        [{:name "cached-react-class",
          :file "reagent/impl/component.cljs",
          :line 68,
          :arglists '([:cljdoc.diff/removed-arities [c]]),
          :type :var,
          :namespace "reagent.impl.component",
          :platform "cljs",
          :diff :cljdoc.diff/arity-removed}]))))
