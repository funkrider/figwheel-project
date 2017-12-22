(ns figwheel-project.core
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [figwheel-project.config :as config]
            [ajax.core :as ajax]))

(enable-console-print!)

;; ------------------------------------------
;; Constants and app-state

(defonce api-server "https://api.census.gov/data/timeseries/idb/5year?get=NAME,POP,GR,TFR,CBR,E0,IMR,MR0_4,CDR,NMR&FIPS=%1&time=2015")

(defonce constants {:scale-data {"POP"     1500000000 ;; China has max country pop of 1.3 Bln
                                           "GR"      5
                                           "TFR"     10
                                           "CBR"     100
                                           "CBR-POP" 15000000 ;; CBR - per thousand, * POP
                                           "E0"      100
                                           "IMR"     200
                                           "MR0_4"   200
                                           "CDR"     200
                                           "CDR-POP" 15000000 ;; CDR - per thousand, * POP. Typical CDR 10
                                           "NMR"     20}
                    :scale-key {"POP"     "Midyear Population"
                                          "GR"      "Growth Rate"
                                          "TFR"     "Total Fertility Rate"
                                          "CBR"     "Crude Birth Rate"
                                          "CBR-POP" "Births"
                                          "E0"      "Life Expectancy at Birth"
                                          "IMR"     "Infant Mortality Rate"
                                          "MR0_4"   "Under 5 Mortality Rate"
                                          "CDR"     "Crude Death Rate"
                                          "CDR-POP" "Deaths"
                                          "NMR"     "Net Migration Rate"}})

(defonce app-state
  (atom
    {:countries/data       {"POP"     [0 0]
                            "GR"      [0 0]
                            "TFR"     [0 0]
                            "CBR"     [0 0]
                            "CBR-POP" [0 0]
                            "E0"      [0 0]
                            "IMR"     [0 0]
                            "MR0_4"   [0 0]
                            "CDR"     [0 0]
                            "CDR-POP" [0 0]
                            "NMR"     [0 0]
                            "NAME"    ["" ""]}}))


;; ------------------------------------------
;; Om-next read, mutate & reconciler

(defn read
  [{:keys [state] :as env} key params]
  (let [st @state]
    (if-let [[_ value] (find st key)]
      {:value value}
      {:value :not-found})))

(defn mutate
  [{:keys [state] :as env} key params]
  (if (= `update-chart key)
    {:value {:keys [:countries/data]}
     :action #(swap! state assoc :countries/data (:value params))}
    {:value :not-found}))

(def reconciler
  (om/reconciler
    {:state app-state
     :parser (om/parser {:read read :mutate mutate})}))


;; ------------------------------------------
;; Helper methods

;; Rates are per thousand :. Pop * rate / 1000
(defn calc-CBR-POP [x]
  (assoc x "CBR-POP" (.floor js/Math (* (x "POP") (x "CBR") 0.0001))))

(defn calc-CDR-POP [x]
  (assoc x "CDR-POP" (.floor js/Math (* (x "POP") (x "CDR") 0.0001))))

(defn parse-num [x]
  (if (number? x)
    (.parseFloat js/Number x)
    x))

(defn update-target-map [t i m]
  (reduce-kv (fn [o k v]
               (assoc-in o [k i] (parse-num v))) t m))


;; ------------------------------------------
;; AJAX and response methods

(defn handle-data [response i]
  (->> response
    (apply zipmap)
    (calc-CBR-POP)
    (calc-CDR-POP)
    (update-target-map (:countries/data @app-state) i)
    (#(om/transact! reconciler `[(update-chart {:value ~%})]))))

(defn load-country [FIPS i]
  (ajax/GET (.replace api-server "%1" FIPS)
    {:handler (fn [response]
                (handle-data response i))}))

;; ------------------------------------------
;; UI Components

(defui Country-List
  Object
  (render [this]
    (let [key ((om/props this) :key)
          i (if (= key "a") 0 1)]
      (dom/span #js {:className (str "country-selector " key)} (str "Please select a country for column: " key) " "
        (apply dom/select #js {:id       key
                               :onChange #(load-country (.. % -target -value) i)}
          (map
            (fn [[k v]]
              (let [style (if (empty? k) #js {:display "none"} {})]
                (dom/option #js {:value k :style style} v)))
            ((om/props this) :FIPS)))))))

(def country-list (om/factory Country-List {:keyfn :key}))


(defn data-item [data]
  (let [{:keys [k v n] :as props} data
        a (first v)
        b (second v)
        label (get-in constants [:scale-key k])
        range (get-in constants [:scale-data k])
        a-perc (Math/round (/ (Math/abs a) range 0.01))
        a-top (- 100 a-perc)
        b-perc (Math/round (/ (Math/abs b) range 0.01))
        b-top (- 100 b-perc)
        a-name (first n)
        b-name (second n)
        a-negfix (if (< a 0) " negative" "")
        b-negfix (if (< b 0) " negative" "")]

    (dom/div #js {:className "progress-container"}
      (dom/div #js {:className "progress-bar tooltip"}
        (dom/div #js {:className "progress-track"}
          (dom/div #js {:className (str "progress-fill-a" a-negfix) :style #js {:height (str a-perc "%")
                                                                       :top    (str a-top "%")}}
            (dom/span nil (str a-perc "%"))
            (dom/span #js {:className "tooltiptext tooltip-right"} (str a-name " " label " " a " (0-" range ")"))))
        (dom/div #js {:className "vertical-label"} label))

      (dom/div #js {:className "progress-bar tooltip"}
        (dom/div #js {:className "progress-track"}
          (dom/div #js {:className (str "progress-fill-b" b-negfix) :style #js {:height (str b-perc "%") :top (str b-top "%")}}
            (dom/span nil (str b-perc "%"))
            (dom/span #js {:className "tooltiptext tooltip-right"} (str b-name " " label " " b " (0-" range ")")))))

      (dom/div #js {:className "progress-bar-b"}))))

(defui Country-Chart
  static om/IQuery
  (query [this]
    '[:countries/data])
  Object
  (render [this]
    (let [countries-data (om/props this)
          a-name (get-in countries-data ["NAME" 0])
          b-name (get-in countries-data ["NAME" 1])]
      (dom/div #js {:className "container vertical rounded"}

        (map #(data-item {:k %
                          :v (countries-data %)
                          :n (countries-data "NAME")})
          (keys (:scale-key constants)))))))


(def country-chart (om/factory Country-Chart))

(defui Root-View
  Object
  (render [this]
    (let [countries-data (:countries/data (om/props this))
          a-name (get-in countries-data ["NAME" 0])
          b-name (get-in countries-data ["NAME" 1])]

      (dom/div nil

        (country-list (merge {:FIPS config/FIPS} {:key "a"}))
        (country-list (merge {:FIPS config/FIPS} {:key "b"}))

        (country-chart countries-data)))))

;; ------------------------------------------
;; Invoke the app

(om/add-root! reconciler
  Root-View (gdom/getElement "app"))