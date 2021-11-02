(ns cljdoc.render
  (:require [cljdoc.doc-tree :as doctree]
            [cljdoc.platforms :as platf]
            [cljdoc.render.rich-text :as rich-text]
            [cljdoc.render.layout :as layout]
            [cljdoc.render.articles :as articles]
            [cljdoc.render.sidebar :as sidebar]
            [cljdoc.render.api :as api]
            [cljdoc.util :as util]
            [cljdoc.util.fixref :as fixref]
            [cljdoc.util.scm :as scm]
            [cljdoc.bundle :as bundle]
            [cljdoc.spec]
            [cljdoc.server.routes :as routes]
            [clojure.data :refer [diff]]
            [clojure.string :as string]))

(defmulti render (fn [page-type _route-params _cache-bundle] page-type))

(defmethod render :default
  [page-type _ _]
  (format "%s not implemented, sorry" page-type))

(defmethod render :compare/index
  [_ path-params artifacts-data]
  (layout/page
   {}
   (layout/layout
    {:main-sidebar-contents (sidebar/compare-sidebar path-params artifacts-data)})))

(defn def->str [{[{:keys [name arglists]}] :platforms}]
  (str name arglists))

(defmethod render :compare/namespace
  [_ {:keys [artifact-id-a group-id-a version-a namespace] :as path-params} artifacts-data]
  (def path-params path-params)
  (let [defs-a (bundle/defs-for-ns-with-src-uri (:cache-bundle (first artifacts-data)) namespace)
        defs-b  (bundle/defs-for-ns-with-src-uri (:cache-bundle (second artifacts-data)) namespace)
        [added-var-names removed-var-names unchanged-var-names]
        (diff
         (set (map def->str defs-a))
         (set (map def->str defs-b)))
        removed-vars (remove (fn [def] (unchanged-var-names (def->str def))) defs-b)
        defs (sort-by def->str (concat defs-a removed-vars))
        [[dominant-platf] :as platf-stats] (api/platform-stats defs)]
    (layout/page
     {}
     (layout/layout
      {:main-sidebar-contents (sidebar/compare-sidebar path-params artifacts-data)
       :vars-sidebar-contents (when (seq defs-a)
                                [(api/platform-support-note platf-stats)
                                 (api/definitions-list
                                   (for [def defs
                                         :let [def-name (platf/get-field def :name)
                                               platforms (platf/platforms def)]]
                                     (api/definition
                                       {:name def-name
                                        :platforms platforms
                                        :class (string/join
                                                " "
                                                [(when (and added-var-names (added-var-names (def->str def)))
                                                   "bg-light-green")
                                                 (when (and removed-var-names (removed-var-names (def->str def)))
                                                   "bg-light-red")])
                                        :foreign-platform (= (platf/platforms def) dominant-platf)})))])}))))


(defmethod render :artifact/version
  [_ route-params {:keys [cache-bundle pom last-build]}]
  (let [version-entity (:version-entity cache-bundle)]
    (->> (layout/layout
          ;; TODO on mobile this will effectively be rendered as a blank page
          ;; We could instead show a message and the namespace tree.
          {:top-bar (layout/top-bar version-entity (-> cache-bundle :version :scm :url))
           :main-sidebar-contents (sidebar/sidebar-contents route-params cache-bundle last-build)})
         (layout/page {:title (str (util/clojars-id version-entity) " " (:version version-entity))
                       :description (layout/artifact-description version-entity (:description pom))}))))

(defmethod render :artifact/doc
  [_ route-params {:keys [cache-bundle pom last-build]}]
  (assert (:doc-slug-path route-params))
  (let [version-entity (:version-entity cache-bundle)
        doc-slug-path (:doc-slug-path route-params)
        doc-tree (doctree/add-slug-path (-> cache-bundle :version :doc))
        doc-p (->> doc-tree
                   doctree/flatten*
                   (filter #(= doc-slug-path (:slug-path (:attrs %))))
                   first)
        [doc-type contents] (doctree/entry->type-and-content doc-p)
        top-bar-component (layout/top-bar version-entity (-> cache-bundle :version :scm :url))
        sidebar-contents (sidebar/sidebar-contents route-params cache-bundle last-build)
        articles-block (doctree/get-neighbour-entries (remove #(contains? #{"Readme" "Changelog"}
                                                                          (:title %))
                                                              doc-tree)
                                                      doc-slug-path)
        prev-page (first articles-block)
        next-page (last articles-block)]
    ;; If we can find an article for the provided `doc-slug-path` render that article,
    ;; if there's no article then the page should display a list of all child-pages
    (->> (if doc-type
           (layout/layout
            {:top-bar top-bar-component
             :main-sidebar-contents sidebar-contents
             :content (articles/doc-page
                       {:doc-scm-url (scm/view-uri (bundle/scm-info cache-bundle)
                                                   (-> doc-p :attrs :cljdoc.doc/source-file))
                        :contributors (-> doc-p :attrs :cljdoc.doc/contributors)
                        :doc-type (name doc-type)
                        :doc-html (fixref/fix (rich-text/render-text [doc-type contents])
                                              {:scm-file-path (-> doc-p :attrs :cljdoc.doc/source-file)
                                               :scm (bundle/scm-info cache-bundle)
                                               :uri-map (fixref/uri-mapping version-entity (doctree/flatten* doc-tree))})
                        :version-entity version-entity
                        :prev-page prev-page
                        :next-page next-page})})
           (layout/layout
            {:top-bar top-bar-component
             :main-sidebar-contents sidebar-contents
             :content (articles/doc-overview
                       {:version-entity version-entity
                        :doc-tree (doctree/get-subtree doc-tree doc-slug-path)
                        :prev-page prev-page
                        :next-page next-page})}))

         (layout/page {:title (str (:title doc-p) " — " (util/clojars-id version-entity) " " (:version version-entity))
                       :canonical-url (some->> (bundle/more-recent-version cache-bundle)
                                               (merge route-params)
                                               (routes/url-for :artifact/doc :path-params))
                       :description (layout/artifact-description version-entity (:description pom))
                       :page-features (when doc-type (rich-text/determine-features [doc-type contents]))}))))

(defmethod render :artifact/namespace
  [_ route-params {:keys [cache-bundle pom last-build]}]
  (assert (:namespace route-params))
  (let [version-entity (:version-entity cache-bundle)
        ns-emap route-params
        defs    (bundle/defs-for-ns-with-src-uri cache-bundle (:namespace ns-emap))
        [[dominant-platf] :as platf-stats] (api/platform-stats defs)
        ns-data (bundle/get-namespace cache-bundle (:namespace ns-emap))
        top-bar-component (layout/top-bar version-entity (bundle/scm-url cache-bundle))
        fix-opts {:scm (-> cache-bundle :version :scm)
                  :uri-map (fixref/uri-mapping version-entity
                                               (-> cache-bundle
                                                   :version
                                                   :doc
                                                   doctree/add-slug-path
                                                   doctree/flatten*))}]
    (->> (if ns-data
           (layout/layout
            {:top-bar top-bar-component
             :main-sidebar-contents (sidebar/sidebar-contents route-params cache-bundle last-build)
             :vars-sidebar-contents (when (seq defs)
                                      [(api/platform-support-note platf-stats)
                                       (api/definitions-list
                                         (for [def defs
                                               :let [def-name (platf/get-field def :name)
                                                     platforms (platf/platforms def)]]
                                           (api/definition
                                             {:name def-name
                                              :platforms platforms
                                              :foreign-platform (= (platf/platforms def) dominant-platf)})))])
             :content (api/namespace-page {:ns-entity ns-emap
                                           :ns-data ns-data
                                           :defs defs
                                           :fix-opts fix-opts})})
           (layout/layout
            {:top-bar top-bar-component
             :main-sidebar-contents (sidebar/sidebar-contents route-params cache-bundle last-build)
             :content (api/sub-namespace-overview-page {:ns-entity ns-emap
                                                        :namespaces (bundle/namespaces cache-bundle)
                                                        :defs (bundle/all-defs cache-bundle)
                                                        :fix-opts fix-opts})}))
         (layout/page {:title (str (:namespace ns-emap) " — " (util/clojars-id version-entity) " " (:version version-entity))
                       :canonical-url (some->> (bundle/more-recent-version cache-bundle)
                                               (merge route-params)
                                               (routes/url-for :artifact/namespace :path-params))
                       :description (layout/artifact-description version-entity (:description pom))}))))

(comment

  (defn namespace-hierarchy [ns-list]
    (reduce (fn [hierarchy ns-string]
              (let [ns-path (clojure.string/split ns-string #"\.")]
                (if (get-in hierarchy ns-path)
                  hierarchy
                  (assoc-in hierarchy ns-path nil))))
            {}
            ns-list))

  (namespace-hierarchy (map :name namespaces))

  (-> (doctree/add-slug-path (-> (:cache-bundle cljdoc.bundle/cache) :version :doc))
      first))


