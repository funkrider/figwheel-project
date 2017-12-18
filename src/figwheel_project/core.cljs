(ns figwheel-project.core
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [figwheel-project.config :as config]))

(def app-state (atom {:count 0}))

(defui Country-list
  Object
  (render [this]
    (apply dom/select nil
      (map  (fn [[k v]] (dom/option nil v))
        (get (om/props this) :FIPS)))))

(def country-list (om/factory Country-list))

(js/ReactDOM.render
  (country-list {:FIPS config/FIPS})
  (gdom/getElement "app"))