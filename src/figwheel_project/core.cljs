(ns figwheel-project.core
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [figwheel-project.config :as config]))

(defui Country-list
  Object
  (render [this]
    (apply dom/select nil
      (map  (fn [[k v]] (dom/option nil v))
        (get (om/props this) :FIPS)))))

(def country-list (om/factory Country-list))


(def app-state (atom {:count 0}))

(defn read [{:keys [state] :as env} key params]
  (let [st @state]
    (if-let [[_ value] (find st key)]
      {:value value}
      {:value :not-found})))

(defn mutate [{:keys [state] :as env} key params]
  (if (= 'increment key)
    {:value {:keys [:count]}
     :action #(swap! state update-in [:count] inc)}
    {:value :not-found}))

(defui Counter
  static om/IQuery
  (query [this] [:count]) ;; Defines location of data in pull syntax
  Object
  (render [this]
    (let [{:keys [count]} (om/props this)]
      (dom/div nil
        (dom/span nil (str "Count: " count))
        (dom/button
          #js {:onClick
               (fn [e] (om/transact! this '[(increment)]))}
          "Click me!")))))

(def reconciler
  (om/reconciler
    {:state app-state
     :parser (om/parser {:read read :mutate mutate})}))

(om/add-root! reconciler
  Counter (gdom/getElement "app"))