(ns dance.choreography
  (:require [clojure.set          :as    set]
            [shuriken.associative :refer [getsoc]]
            [derrida.core         :refer [deconstruct]]
            [shuriken.sequential  :refer [get-nth assoc-nth-in]]
            [shuriken.spec        :refer [conform!]]
            [threading.core       :refer :all]
            [weaving.core         :refer :all]
            [dance.floor          :refer :all]))

(defdance parent-dance
  "A dance that keeps track of the parent node.
  Context keys: :parent (:next-parent)."
  :scoped [:parent :next-parent]
  :before (fn [form ctx]
            [form (assoc ctx
                    :parent (:next-parent ctx)
                    :next-parent form)]))

(defdance parents-dance
  "A dance that keeps track of the parent nodes, in genealogic order
  (parent, grand-parent, ...).
  Context keys: :parents (:next-parent)."
  :scoped [:parents :next-parent]
  :context {:parents '()}
  :before (fn [form ctx]
            [form (-> ctx
                      (when-> :next-parent
                        (update :parents conj (:next-parent ctx)))
                      (assoc :next-parent form))]))

;; TODO: document
(defdance original-form-dance
  :scoped [:original-form]
  :before (fn [form ctx]
            [form (assoc ctx :original-form form)]))

(defdance depth-dance
  "A dance that keeps track of the depth of the traversal.
  Context key: :depth."
  :context {:debug-depth -1}
  :scoped [:depth]
  :before (fn [x ctx] [x (update ctx :depth (fnil inc -1))]))

;; TODO: does it work with sets ?
;; TODO: use :scoped [:index] ?
(defdance indexed-dance
  "A dance that keeps track of the index the current nodes resides at in
  its parent node. The index can be either a number for list/vectors or
  anything else for maps.
  Context key: :index."
  :step  step-indexed
  :scoped [:index])

(defdance path-dance
  "A dance that keeps track of the path from the root of the datastructure to
  the current node.
  To use a path to find back the node it leads to, use shuriken.core/get-nth-in
  and sister functions since contrary to get-in, assoc, etc ..., they support
  traversing sequences.
  Context key: :path."
  indexed-dance
  :scoped [:path]
  :before (fn [form ctx]
            (let [conj-path (fn [ctx k]
                              (update ctx :path
                                      (comp vec concat)
                                      (if-let [i (get ctx k)]
                                        [i] [])))
                  new-ctx (cond
                            (map-entry? form)
                            (assoc ctx :map-index (key form))

                            (contains? ctx :map-index)
                            (case (:index ctx)
                              0 ctx
                              1 (-> (conj-path ctx :map-index)
                                    (dissoc :map-index)))

                            :else (conj-path ctx :index))]
              [form new-ctx])))

(def-collecting-dance leafs-collecting-dance
  path-dance
  :accumulate #(into %1 [%2])
  :into {}
  :when (not| coll?)
  :what (fn [form ctx]
          [[(:path ctx) form]])
  :at :leafs)

(defn splice [form]
  [::splice form])

(defn- splice? [form]
  (and (vector? form) (-> form first #{::splice})))

(defdance splicing-dance
  :post (fn [form]
          (if-not (and (sequential? form)
                       (some splice? form))
            form
            (let [spliced (reduce (fn [acc x]
                                    (if (splice? x)
                                      (into acc (second x))
                                      (conj acc x)))
                                  []
                                  form)]
              (if (vector? form)
                spliced
                (reverse (into '() spliced)))))))

(let [unquoted? #(not (is-form? 'quote %))]
  (defdance not-processing-quoted-forms-dance
    :pre?  unquoted?
    :walk? unquoted?
    :post? unquoted?))

(defdance reluctant-macroexpanding-dance
  not-processing-quoted-forms-dance
  :before macroexpand)

(defn get-right-context [ctx]
  (-> ctx :right-contexts first))

(defn add-to-right-context [ctx & kvs]
  (assert (even? (count kvs)))
  (reduce (fn [acc [k v]]
            (assoc-nth-in acc [:right-contexts 0 k] v))
          ctx (partition 2 kvs)))

(defn update-right-context [ctx k f & args]
  (-> (get-right-context ctx)
      (get k)
      (as-> $ (apply f $ args))
      (->> (add-to-right-context ctx k))))

(defdance right-context-dance
  :before (fn [form ctx]
            [form (-> ctx
                      (update :right-contexts (fnil #(cons %2 %1) '()) {})
                      (•- (merge (-• :context-from-left)))
                      (dissoc :context-from-left))])
  :after  (fn [form ctx]
            [form (let [ctx (-> ctx
                                (•- (merge (-• :context-from-left)))
                                (dissoc :context-from-left))
                        [s & ss] (:right-contexts ctx)
                        ctx (if (empty? ss)
                              (dissoc ctx :right-contexts)
                              (assoc ctx :right-contexts ss))
                        ctx (if (empty? s)
                              ctx
                              (assoc ctx :context-from-left s))]
                    ctx)]))


(def ^:private rdistinct (comp reverse distinct reverse))

(defn- add-locals-to-current-context [ctx locals]
  (update ctx :locals (comp rdistinct concat) locals))

(defn- add-locals-to-right-context [ctx locals]
  (update-right-context
    ctx :locals-from-left (comp rdistinct concat)
    locals))

(defdance ^:private locals-tracking-dance--top-level
  :context {:locals []}
  :scoped [:locals :binding-form?]
  :before
  (fn [form ctx]
    [form (when-> ctx (<- (is-form? '[let* loop* fn* letfn*] form))
            (update :scoped #(set/union (set %) #{:locals})))]))

(defdance ^:private locals-tracking-dance--handle-locals-from-left
  :before
  (fn [form ctx]
    [form (when-> ctx :locals-from-left
            (-> (update :locals (comp rdistinct concat)
                        (:locals-from-left ctx))
                (dissoc :locals-from-left)))]))


(defdance ^:private locals-tracking-dance--let*-loop*-letfn*-binding-vec
  :before
  (fn [form ctx]
    [form (when-> ctx (-> :parents first
                          (and-> (-> (or->> (is-form? '[let* loop* letfn*])))
                                 (-> second (= form))))
            (update :scoped disj :locals))]))

(defdance ^:private locals-tracking-dance--let*-loop*-letfn*-binding-sym-expr
  :before
  (fn [form ctx]
    [form
     (when-> ctx (and->
                   (-> :parents second (or->> (is-form? '[let* loop* letfn*])))
                   (>-args (-> (= (-> :parents second second)
                                  (-> :parents first)))))
       (if-> (-> :index even?)
         ;; a bound sym, not an expr
         (assoc :binding-form? true)
         ;; a bound expr, not a sym
         (let-> [i :index
                 parent (-> :parents first)
                 prev-form (<- (get parent (dec i)))]
           (add-locals-to-right-context (deconstruct prev-form)))))]))

(defdance ^:private locals-tracking-dance--let*-loop*-letfn*
  locals-tracking-dance--let*-loop*-letfn*-binding-vec
  locals-tracking-dance--let*-loop*-letfn*-binding-sym-expr)


(defdance ^:private locals-tracking-dance--fn*-single-body
  :scoped [:parsed-fn-form]
  :before
  (fn [form ctx]
    [form
     (when-> ctx (<- (is-form? 'fn* form))
       (<- (let [[parsed ctx] (getsoc ctx :parsed-fn-form
                                      #(conform! :shuriken.spec/fn-form form))]
             (when-> ctx (<- (-> parsed :bodies count (= 1)))
               (add-locals-to-current-context
                 (concat (keep identity [(:name parsed)])
                         (->> parsed :bodies first :args deconstruct)))))))]))

(defdance ^:private locals-tracking-dance--fn*-multiple-bodies
  :scoped [:parsed-fn-form]
  :before
  (fn [form ctx]
    [form
     (when-> ctx (some->> :parents first (is-form? 'fn*))
       (<- (let [[parsed ctx] (getsoc ctx :parsed-fn-form
                                      #(conform! :shuriken.spec/fn-form
                                                 (-> ctx :parents first)))]
             (when-> ctx (and-> (<- (-> parsed :bodies count (> 1)))
                                (<- (seq? form)))
               (add-locals-to-current-context
                 (concat (keep identity [(:name parsed)])
                         (-> (conform! :shuriken.spec/args+body form)
                             :args deconstruct)))))))]))

(defdance ^:private locals-tracking-dance--fn*
  locals-tracking-dance--fn*-single-body
  locals-tracking-dance--fn*-multiple-bodies)


(defdance locals-tracking-dance
  reluctant-macroexpanding-dance
  parents-dance
  indexed-dance
  right-context-dance
  locals-tracking-dance--top-level
  locals-tracking-dance--handle-locals-from-left
  locals-tracking-dance--let*-loop*-letfn*
  locals-tracking-dance--fn*)

(defdance free-symbols-tracking-dance
  "Accepts an optional :bound-sym? function in the context."
  locals-tracking-dance
  :scoped [:free-symbol?]
  :before-all (fn [form ctx]
                [form (update ctx :bound-sym?
                              #(when-> % (or-> nil? sequential?) set))])
  :before (fn [form ctx]
            [form
             (when-> ctx (<- (-> form
                                 (and->
                                   symbol?
                                   (<- (not (:binding-form? ctx)))
                                   (not-> (or->> special-symbol?
                                                 (.contains (:locals ctx))
                                                 ((:bound-sym? ctx))
                                                 resolve)))))
               (assoc :free-symbol? true))]))

(def-collecting-dance free-symbols-collecting-dance
  free-symbols-tracking-dance
  :accumulate (comp vec rdistinct conj)
  :when #(:free-symbol? %2)
  :what identity
  :at :free-symbols)
