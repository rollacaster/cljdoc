(ns migrations.010-assoc-cljdoc-tree-to-meta
  (:require [clojure.java.jdbc :as sql]
            [taoensso.nippy :as nippy]))

(defn migrate-meta [db-spec update-doc-tree]
  (doseq [[version-id data] (->> {:row-fn (fn [r]
                                            [(:id r)
                                             (some-> r :meta nippy/thaw)])}
                                 (sql/query db-spec ["select id, meta from versions"])
                                 (map #(update-in % [1 :doc] update-doc-tree)))]
    (sql/execute! db-spec
                  ["UPDATE versions SET meta = ? WHERE id = ?" (nippy/freeze data) version-id])))

(defn up [db-spec]
  (migrate-meta
   db-spec
   (fn [doc-tree]
     (if (:cljdoc.doc/tree doc-tree)
       doc-tree
       {:cljdoc.doc/tree doc-tree}))))

(defn down [db-spec]
  (migrate-meta
   db-spec
   (fn [doc-tree]
     (if (:cljdoc.doc/tree doc-tree)
       (:cljdoc.doc/tree doc-tree)
       doc-tree))))
