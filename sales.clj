(ns sales.core
    (:gen-class)
    (:require [clojure.edn :as edn])
    (:require [clojure.string :as str])
    (:require [clojure.set :as st]))

(def custList (str/split-lines (slurp "cust.txt")))
(def custData {})
(def custNameList '())
(def cuSaData {})
(def custVMap {})
(def prodList (str/split-lines (slurp "prod.txt")))
(def prodData {})
(def prodNameList '())
(def prSaData {})
(def prodVMap {})
(def salesList (str/split-lines (slurp "sales.txt")))
(def salesData {})
(def saleCustTable {})
(def salesVMap {})
(def saPrID {})

(def priceMap {})

;Customer Data Map
(loop [index 0]
    (when (not-empty (get custList index))
        (def custLine (get custList index))
        (def custBreak (str/split custLine #"\|"))
        (def custId (get custBreak 0))
        (def custName (get custBreak 1))
        (def custNameList (conj custNameList custName))
        (def custAddress (get custBreak 2))
        (def custPhone (get custBreak 3))
        (def custInfo (str custName "," custAddress "," custPhone))
        (def valuesCust (str ":[\"" custName "\"" " " "\""custAddress"\"" " " "\""custPhone"\"]"))
        (def custVMap (assoc custVMap custId custInfo))
        (def custData (assoc custData custId valuesCust))
        (def cuSaData (assoc cuSaData custId custName))
        (def custSortData (into (sorted-map) custData))
        (recur (inc index))))

;Product Data Map
(loop [index 0]
    (when (not-empty (get prodList index))
        (def prodLine (get prodList index))
        (def prodBreak (str/split prodLine #"\|"))
        (def prodId (get prodBreak 0))
        (def prodName (get prodBreak 1))
        (def prodNameList (conj prodNameList prodName))
        (def prodCost (get prodBreak 2))
        (def prodInfo (str prodName "," prodCost))
        (def prodVMap (assoc prodVMap prodId prodInfo))
        (def values (str ":[\"" prodName "\"" " " "\"" prodCost "\"]"))
        (def prodData (assoc prodData prodId values))
        (def priceMap (assoc priceMap prodId prodCost))
        (def prodSortData (into (sorted-map) prodData))
        (def prSaData (assoc prSaData prodId prodName))
        (def sortprSaData (into (sorted-map) prSaData))
        (recur (inc index))))

(def salesCountMap {} )

;sales Data Map
(loop [index 0]
    (when (not-empty (get salesList index))
        (def salesLine (get salesList index))
        (def salesBreak (str/split salesLine #"\|"))
        (def salesId (get salesBreak 0))
        (def salesCustId (get salesBreak 1))
        (def cname (get cuSaData salesCustId))
        (def salesProdId (get salesBreak 2))
        (def sname (get prSaData salesProdId))
        (def salesCount (get salesBreak 3))
        (def salesInfo (str cname "," sname "," salesCount))
        ;(def salesVMap (assoc salesVMap salesId salesInfo))
        (def salesVMap (assoc salesVMap salesId sname))
        (def saleCustTable (assoc saleCustTable salesId (get cuSaData salesCustId)))
        (def salesCountMap (assoc salesCountMap salesId (edn/read-string salesCount)))
        (def valuesSales (str ":[\"" cname "\"" " " "\"" sname "\"" " " "\"" salesCount "\"]"))
        (def salesData (assoc salesData salesId valuesSales))
        (def salesSortData (into (sorted-map) salesData))
        (def saPrID (assoc saPrID salesId (get prSaData salesProdId)))
        (def sortsaPrId (into (sorted-map) saPrID))
        (recur (inc index))))

(def countOfProducts {})
(def valNew (vals salesVMap))
(def valCount (vals salesCountMap))
(def entriesCount (count valNew))
(def countOfProducts (assoc countOfProducts (nth valNew 0) (Integer. (nth valCount 0))))

(loop[index 1]
    (when (< index entriesCount)
    (do 
    (def valueTem (keys countOfProducts))
        (if (= (some #(= (nth valNew index) %) valueTem) true)
        (do 
        (def countt (+ (Integer. (nth valCount index))(Integer. (get countOfProducts(nth valNew index)))))
        (def countOfProducts (assoc countOfProducts (nth valNew index) (Integer. countt)))
        )
        (do
            (def countOfProducts (assoc countOfProducts (nth valNew index) (Integer. (nth valCount index))))
        )
        )
        (recur (inc index)))
    ))

(defn mulFloat [x y] (format "%.2f" (* (float x) (Float/parseFloat y))))

(def salesCustomerMap {})
(def salesCustKeys (keys saleCustTable))
(def entries (count salesCustKeys))
(def valSC (vals saleCustTable))

(def pointer (+ 1 (.indexOf (vals sortprSaData) (get saPrID (str 1) )  )))
(def p1 (get salesCountMap (str 1)))
(def p2 (get priceMap (str pointer)))
(def salesCustomerMap (assoc salesCustomerMap (nth valSC 0) (mulFloat p1 p2))) 

(loop[index 2]
    (when (<= index entries)
        (def valueTem (keys salesCustomerMap))
        (if (= (some #(= (nth valSC (dec index)) %) valueTem) true)
        (do
            (def pointer (+ 1 (.indexOf (vals sortprSaData) (get saPrID (str index)))))
            (def ttcost (+ (Float/parseFloat (mulFloat (get salesCountMap (str index)) (get priceMap (str pointer))))  (Float/parseFloat (get salesCustomerMap (str (nth valSC (- index 1))))) ))
            (def salesCustomerMap (assoc salesCustomerMap (str (nth valSC (- index 1))) (format  "%.2f" ttcost )) )
            true
            )
        (do
            (def pointer (+ 1 (.indexOf (vals sortprSaData ) (get saPrID (str index)))))
            (def salesCustomerMap (assoc salesCustomerMap (nth valSC (- index 1)) (mulFloat (get salesCountMap (str index)) (get priceMap (str pointer))) ))
            ))
            (recur (inc index))
            ))

(defn totalCost []
    (def cname (read-line))
    (def tt 0)

    (if (= (some #(= cname %) custNameList) true)
    (do
        (def kk (keys salesCustomerMap))
        (def vv (vals salesCustomerMap))
        (if (= (some #(= cname %) kk) true)
            (println( str cname ": $"(get salesCustomerMap cname)))
            (println (str  cname ": $0") )
        )
        )
        (do (println "Customer Not Found!"))
    )
)

(defn totalProduct []
    (def pname (read-line))
    (def tt 0)

    (if (= (some #(= pname %) prodNameList) true)
        (do
        (def km (keys countOfProducts ))
        (def vm (vals countOfProducts ))
        (if (= (some #(= pname %) km) true)
            (println (str pname ": "(get countOfProducts pname)))
            (println (str pname ": 0"))
        )
        )
    (println "product not found!"))
    )


(defn menu []
    (println "")
    (println "*** Sales Menu ***")
    (println "------------------")
    (println " ")
    (println "1. Display Customer Table") 
    (println "2. Display Product Table")
    (println "3. Display Sales Table")
    (println "4. Total Sales for Customer")
    (println "5. Total Count for Product")
    (println "6. Exit")
    (println " ")
    (println "Enter an option?")
    (let [input (edn/read-string (read-line))]
        (println "")
        (if (number? input)
        (if (== input 6) (println "Bye") 
            (cond 
                (== input 1) (do ;(println custSortData)
                                (def keylist (keys custSortData))
                                ;(def valMap (vals sortCustIDInfo))
                                (def ncount (count keylist))
                                (loop [x 1]
                                    (when (<= x ncount)
                                        (println x "" (get custSortData (str x)))
                                        (recur (+ x 1))))

                                (menu))
                (== input 2) (do ;(println prodSortData)
                                (def keylist (keys prodSortData))
                                (def ncount (count keylist))
                                (loop [x 1]
                                    (when (<= x ncount)
                                        (println x "" (get prodSortData (str x)))
                                        (recur (+ x 1))))
                                (menu))
                (== input 3) (do ;(println salesSortData)
                                (def keylist (keys salesSortData))
                                (def ncount (count keylist))
                                (loop [x 1]
                                    (when (<= x ncount)
                                        (println x "" (get salesSortData (str x)))
                                        (recur (+ x 1))))
                                (menu))
                (== input 4) (do (println "Enter Customer Name ")(totalCost) (menu))
                (== input 5) (do (println "Enter product Name ")(totalProduct) (menu))        
                :else (menu))
        )
        (menu)
        )))
(menu)