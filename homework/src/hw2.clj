(ns hw2
  (:require
            [   incanter.core :refer :all]
 ;;           [ incanter.charts :refer [ histogram set-x-range  xy-plot]]
          ;;  [clojure.core.reducers :as r]
;;            [clojure.core.async :as async ]
            [clojure.core.reducers :as r]
            [clojure.java.io :as io]
            [clojure.string :refer [ split]]))




;;helper functions
(def  toNumbersFn  (fn [arg] (cond (nil? arg) nil
                                  (number? arg) arg
                                  :else
                                  (let [theNumber  (read-string arg)]
                                    (try
                                      (if (number? theNumber  ) theNumber  arg)  (catch NumberFormatException e  arg ))))))


(defn parser
  "for parsing all data files into a map {docId result anotherDocId result}
  where result can be either singleton or coll,
  and numbers are converted from string
  "

  [file]
    (let [result (atom {})
          collToSingleton (fn [arg]
                            (if (coll? arg)
                              (cond (empty? arg)
                                    nil
                                    (= 1  (count arg))
                                    (first arg)
                                    :else
                                    arg)
                              arg))
          lineNum (atom 1) ;;start from 1
          ]

      (with-open [rdr (clojure.java.io/reader file)]
        (doseq [line (line-seq rdr)]
          (let [data  (collToSingleton   (map toNumbersFn  (into [] (.split line  " "))))]
            (swap! result assoc  @lineNum  data))
          (swap! lineNum inc)))
      @result))

(defn dataFn [dataFile]
  (let  [  data (parser  dataFile )]
    (reduce #( assoc-in %1 [ (nth %2 0) (nth %2 1)]   (nth %2 2))  {} (vals data)  )))


(defn myChoose [ n k]
  " the version in incanter will return infinity for large number"
  (let [numerator (map bigdec  (range  (inc  ( - n k))  (inc n)))
        denominator  (map bigdec  (range 1 (inc k)))]
    (/  (reduce *  numerator ) (reduce *  denominator))))

(defn myFactorial
  " the version in incanter will return infinity for large number"
  [n]
  (reduce *  (map bigdec  (range 1 (inc n))) )
  )


;;data

(def Y  (parser (io/resource  "hw2data/newsgrouplabels.txt") ))
(def X  (parser (io/resource  "hw2data/vocabulary.txt") ) )
(def trainLabel  (parser   (io/resource "hw2data/train.label" )))
(def testLabel  (parser  (io/resource  "hw2data/test.label" )))
(def numX (count X))
(def numY (count Y))
(def testData (dataFn (io/resource  "hw2data/test.data") ))
(def trainData  (dataFn (io/resource  "hw2data/train.data") ))





;;calcuations

(def PY "MLE estimate of P(Y), where y corresponds to label id 1 to 20 "
  (memoize (fn [y]
             (double  (/  (count  (for [ [ docId  label ]  trainLabel :when (= label y)] 0 ))     (count  trainLabel) )))))


(def  numTotalWordsInCategoryY
  (memoize
   (fn [ y]     (apply +  (flatten     (map vals  (for [ [ k  v ] trainData :when   (= (get trainLabel k) y) ] v  )))))))


(def numWordXinCategoryY
  (memoize
   (fn [x y]
     (reduce +
             (filter #(not (nil? % ))  (for [ [k v ] trainData :when   (= (get trainLabel k) y) ] (get v x)  )) )
     ;;  filter is way way faster then putting a "contains" condition into :when like below
     ;;(for [ [k v ] trainData :when (and  (not (= -1 (.indexOf  (keys v) x )))  (= (get trainLabel k) y) )] (get v x)  )

     )))


;; check sum over X for a category is 1
;;(reduce +  (pmap   #(PXgivenY % 20) (range 1  (inc  (count X))) ))  --> should be about 1
(def PXgivenY "MAP estimate P(X|Y), x corresponds to wordId x in [1 6xxxx], y corresponds to label id, y in [ 1 20]"
  (memoize  (fn [ x y & {:keys [ l] :or {l 1}} ]
              (let [ J numX

                    ;; v is {wordId count wordId count...} , k is docId
                    numerator  (+ l  (numWordXinCategoryY x y ))

                    denominator  (+   (* l J )  (numTotalWordsInCategoryY y))]
                ;; (prn "numXAndY " numXAndY "numY" numY)
                (with-precision 40  (/  (bigdec numerator) (bigdec denominator)  )) ))))


(defn PYgivenX [testDataId ]
  (let  [currentDoc (get testData testDataId)
         k    (count  currentDoc)  ;;number of different words shown up in a doc
         n (reduce +  (vals currentDoc)) ;; number of total words shown up in a doc
       ;; this is wrong!! -->  n (bigdec  numX)
         aSequenceOfPXgivenY  (fn [y] (flatten   (for [ [ x numX]   currentDoc]     (repeat  numX (PXgivenY x y))   )))

         PYgivenXFn   (fn [y]  (double  (apply *      (myChoose n k )   ( aSequenceOfPXgivenY y)  )))

         PYs (pmap PYgivenXFn (range 1 (inc  numY)) )
         ]

PYs
))
