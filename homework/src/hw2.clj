(ns hw2
  (:require
            [   incanter.core :refer :all]
            [clojure.core.reducers :as r]
            [clojure.java.io :as io]
            [clojure.core.matrix   :refer [join join-along  pm]]
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


;;not needed for this homework
(defn myChoose [ n k]
  " the version in incanter will return infinity for large number"
  (if (= k 1) n
      (let [numerator (map bigdec  (range  (inc  ( - n k))  (inc n)))
            denominator  (map bigdec  (range 1 (inc k)))]
        (/  (reduce *  numerator ) (reduce *  denominator)))))

(defn myFactorial
  " the version in incanter will return infinity for large number"
  [n]
  (reduce *  (map bigdec  (range 1 (inc n))) ))



(defn permutations
  "different number of ways to arrange words
 product of  (choose n k1) (choose (- n k1)  k2) (choose (- n k1 k2) k3) .....
  "
  [aMapColl ]
  (let [ n (reduce + ( vals aMapColl ))
        sortedData (sort-by val > aMapColl  )
        ks     (map last sortedData)

        nAndK     (loop [n n ks ks result []]
                    (if (zero? n) result
                        (recur   (- n (first ks)) (rest ks)  (conj result  [ n (first ks)]) ))
                    )
        ]

    (reduce *  (pmap #( myChoose (first %) (last %)) nAndK))))




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
             (/  (count  (for [ [ docId  label ]  trainLabel :when (= label y)] 0 ))     (count  trainLabel) ))))


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
(def PXgivenY "MAP estimate P(X|Y), x corresponds to wordId x in [1 6xxxx], y corresponds to label id, y in [ 1 20] hNumerator: hallucinated numberator "
  (fn [ x y & {:keys [hNumerator hDenominator ] :or {hNumerator 1  hDenominator numX}} ]
    (let [

          ;; v is {wordId count wordId count...} , k is docId
          numerator  (+ hNumerator  (numWordXinCategoryY x y ))

          denominator  (+  hDenominator  (numTotalWordsInCategoryY y))]

      ;;   (with-precision 40  (/  (bigdec numerator) (bigdec denominator)  ))
      (/ numerator denominator)
      )))


(def predictY
  "input testDataId, output category id "
  (memoize  (fn  [testDataId &  {:keys [hNumerator hDenominator ] :or {hNumerator 1  hDenominator numX}}   ]
              (let  [currentDoc (get testData testDataId)
                     aSequenceOfPXgivenY  (fn [y] (flatten   (for [ [ x numX]   currentDoc]     (repeat  numX (PXgivenY x y))   )))

                     logPYPxGivenY   (fn [y]  (reduce +  (log (PY y)) (map log        ( aSequenceOfPXgivenY y))  ))
                     PYs (pmap logPYPxGivenY  (range 1 (inc  numY)) )
                     maxPy (apply max PYs)]

                (inc  (first   (filter #(not (nil? %) )     (map-indexed #(if ( =  %2 maxPy) %1)   PYs ))))))))


(defn analysisResult
  "overAllaccuratePreditions  5862 with percentage 78.1079%"
  []
  (let [
        dummy (prn "generating results... first time pls be patient for 10 mins")
        predictions   (pmap  #(predictY % ) (range 1 (inc (count testData))))
        dummy (prn "results generated. 2nd time run will be way faster due to memorizing, for around 10 secs")
        numberOfAccuratePredictionsFn (fn [predictions] (reduce +  (pmap #(if (= %1 %2) 1 0) predictions  (pmap last  (sort testLabel)) )))
        overAllaccuratePreditions (numberOfAccuratePredictionsFn predictions)

        ;; (Cfn i j ) return element for matrix C at position i j
        ;; number of times a document with group trutch category j was classified as category i
        Cfn  (fn [ classifiedCategory trueCategory ]
               (let [trueCategoryList  (pmap #(if ( =  % trueCategory)   1 0  )  (pmap last   (sort testLabel)))  ;;filling nil for non current category

                     classifiedList     (pmap #(if ( =  %   classifiedCategory)  1 0 )  predictions   )]
                 (reduce +  (pmap    #(if (= 1 %1 %2 )   1 0)   trueCategoryList classifiedList))))

        categoryIndices   (map first (sort Y))

        matrixResults  (matrix  (pmap #( Cfn (first %) (last %))   (for  [ i   (range 1 (inc numY)) j (range 1 (inc  numY))] [i j]))  numY)
        matrixResultsWithHeader    (join-along  1  (map vector   (conj  categoryIndices 0 ))   (join    (matrix   [ categoryIndices])  matrixResults ))


        accuracyFn (fn [hNumerator hDenominator]
                     (let [predictions   (pmap  #(predictY % :hNumerator  hNumerator :hDenominator hDenominator) (range 1 (inc (count testData))))
                           correctPredictions (numberOfAccuratePredictionsFn predictions)
                           accurayRate (double (/ correctPredictions (count testLabel)))]
                       (prn "for hNumerator" hNumerator "hDenominator" hDenominator "correct predictions" correctPredictions "rate " accurayRate)
                       ))
        ]

    (prn "overAllaccuratePreditions " overAllaccuratePreditions "percentage "  (double (/  (numberOfAccuratePredictionsFn predictions) (count testLabel)))  )
    (prn "labels"  (sort Y))

    ( pm  matrixResultsWithHeader  {:formatter  (fn [x]  (if (number? x) (format "%5d" (int  x)) x)) })

    (accuracyFn 1 0.00001 )
    (accuracyFn 1 1 )
    (accuracyFn 0 1 )
    (accuracyFn 0 0  )
  )

  )
