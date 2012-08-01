(ns hotbook.core
  (:gen-class)
  (:require [net.cgrand.enlive-html :as html]
            [hotbook.prices :as prices])
  (:import [org.jfree.data.xy DefaultXYDataset]
           [org.jfree.data.category CategoryDataset DefaultCategoryDataset]
           [org.jfree.chart.plot XYPlot PlotOrientation]
           [org.jfree.chart ChartFactory ChartPanel]
           [org.jfree.ui ApplicationFrame]
           [java.awt Dimension]))

;;; http://hotline.ua/computer/noutbuki-netbuki/?p=2

;; html body div.conteiner div.content-box div#test.content div#content2 div.col1 table tbody tr td#catalogue table.cat_products_table tbody tr td.misc span

(def pages (range 1 88))

(def prices (atom []))
(def pages-cache (atom {}))

(defn fetch-page
  [n]
  (if-let [p (@pages-cache n)]
    p
    (let [url (->> n
                   (format "http://hotline.ua/computer/noutbuki-netbuki/?p=%d")
                   (java.net.URL.))]
      (swap! pages-cache assoc n
             (html/html-resource url))
      (recur n))))

(defn get-prices
  [page]
  (map html/text
       (html/select page [:td.misc :span])))

(defn string->price
  [s]
  (when-let [avg-price (re-find #"\d[\d ]+\d" s)]
    (-> avg-price
        (.replaceAll " " "")
        Integer/parseInt)))

(defn get-prices-for-page-uncached
  [n]
  (println "Getting page" n)
  (->> n
       fetch-page
       get-prices
       (map string->price)
       (remove nil?)))
(def get-prices-for-page (memoize get-prices-for-page-uncached))

(defn median [ns]
  (let [ns (sort ns)
        cnt (count ns)
        mid (bit-shift-right cnt 1)]
    (if (odd? cnt)
      (nth ns mid)
      (/ (+ (nth ns mid) (nth ns (dec mid))) 2))))

(defn run []
  (reset! prices [])
  (doseq [n pages]
    (swap! prices into (get-prices-for-page n))
    (println "Median: " (median @prices))))

;; (defn -main
;;   []
;;   (run)
;;   (spit "prices.clj" (pr-str @prices))
;;   (spit "pages.clj" (pr-str @pages-cache)))

(def cached-prices prices/prices)

;; visualize
;;; [10 11 11 24 39 39]
;;; 39 - 10 = 29
;;; (quot 29 3) = 9
;;; [10, 19], [20, 29], [30, 39]
;;; (24 - 10) => (quot 14 9) => 1

(defn histogram
  "Display bar chart of prices distribution
   n - number of bars"
  [n]
  (let [ps cached-prices
        min (apply min ps)
        max (apply max ps)
        bucket-size (quot (- max min) n)
        price-to-bucket #(quot (- % min) bucket-size)
        avg #(quot (reduce + %) (count %))
        buckets (vals (group-by price-to-bucket ps))
        xy (map (fn [b] [(avg b) (count b)]) buckets)]
    (sort-by first xy)))


(def series-name "Price")

(defn fill-dataset
  [ds]
  (dorun
   (map-indexed (fn [i [x y]]
                  (.addValue ds y x series-name)) (histogram 40))))

(defn graph
  []
  (let [n 40
        ds (doto (DefaultCategoryDataset.)
             (fill-dataset))
        chart (ChartFactory/createBarChart
               "Bar Chart Demo 1"     ;; chart title
               "Category"             ;; domain axis label
               "Value"                ;; range axis label
               ds                     ;; data
               PlotOrientation/VERTICAL  ;; orientation
               true                      ;; include legend
               true                     ;; tooltips?
               false)                   ;; URLs ?
        chart-panel (doto (ChartPanel. chart)
                      (.setPreferredSize (Dimension. 500 270)))
        af (proxy [ApplicationFrame] ["Title"])]
    (future
      (doto af
        (.setContentPane chart-panel)
        (.setVisible true)))))

(defn -main
  []
  (graph))

(comment
  ;; dump prices to file
  (spit "prices.clj" (pr-str @prices))
  (spit "pages.clj" (pr-str @pages-cache)))