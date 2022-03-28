(ns work-sample-test.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.csv :as csv])
  (:gen-class))

(def config
  {:directory "path/to/data"
   :price-file "prices.csv"
   :output-file "report.csv"})

(defn read-correct-prices []
  (let [filename (str (:directory config)
                      "/"
                      (:price-file config))
        product-prices (->> filename
                            io/file
                            slurp
                            string/split-lines
                            (map #(string/split % #","))
                            (into {}))]
    (zipmap (keys product-prices)
            (map #(Float/parseFloat %) (vals product-prices)))))

(defn clean-receipt-line [line]
  (let [last-char (last line)
        sane-line (apply str (if (Character/isLetter last-char)
                               (butlast line)
                               line))
        sane-line-words (string/split sane-line #" ")
        meaningful-words (filter (comp not empty?) sane-line-words)
        reversed-words (reverse meaningful-words)
        price (first reversed-words)
        product-code (second reversed-words)]
    {:product product-code
     :price (Float/parseFloat price)}))

(defn read-receipt [file]
  (let [lines (->> file
                   slurp
                   string/split-lines)
        first-line (first lines)
        last-line (last lines)
        middle-lines (-> lines rest butlast)
        store-number (-> first-line
                         (string/split #" ")
                         (nth 3)
                         (subs 1))
        void-line? #(string/starts-with? % "***")
        meaningful-lines (->> (map vector middle-lines (rest middle-lines))
                              (filter (fn [[_cl nl]]
                                        (not (void-line? nl))))
                              (map first)
                              (filter (comp not void-line?)))
        sale-entries (map clean-receipt-line meaningful-lines)
        total (-> last-line
                  (string/split #" ")
                  last)]
    {:store (Integer/parseInt store-number)
     :sale-entries sale-entries
     :total (Float/parseFloat total)}))

(defn receipt-discrepancy [file correct-prices]
  (let [receipt (read-receipt file)]
    (update receipt :sale-entries
            (fn [sale-entries]
              (letfn [(calculate-discrepancy [sale-entry]
                        (update sale-entry :price
                                (fn [price]
                                  (- price
                                     (get correct-prices
                                          (:product sale-entry))))))]
                (map calculate-discrepancy sale-entries))))))

(defn list-receipt-files []
  (let [all-files (-> config
                      :directory
                      io/file
                      .listFiles)
        subdirectories (filter #(.isDirectory %) all-files)
        receipt-files (->> subdirectories
                           (map #(.listFiles %))
                           (apply concat))]
    receipt-files))

(defn generate-report []
  (let [report-filename (str (:directory config)
                             "/"
                             (:output-file config))
        correct-prices (read-correct-prices)
        receipt-files (list-receipt-files)
        discrepancies (map #(receipt-discrepancy % correct-prices) receipt-files)
        balances-by-store (->> discrepancies
                               (map #(assoc % :balance
                                            (->> %
                                                 :sale-entries
                                                 (map :price)
                                                 (apply +))))
                               (map #(dissoc % :sale-entries :total))
                               (group-by :store))
        report-content (into [] (zipmap (keys balances-by-store)
                                        (map (fn [balance-entries]
                                               (->> balance-entries
                                                    (map :balance)
                                                    (apply +)))
                                             (vals balances-by-store))))
        sorted-report-content (sort (fn [e1 e2]
                                      (<= (second e1) (second e2)))
                                    report-content)]
    (with-open [writer (io/writer report-filename)]
      (csv/write-csv writer
                     (concat [["store" "plusminus"]]
                             sorted-report-content)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (generate-report))
