(ns mlhw2
  (:require
            [   incanter.core :refer :all]
            [ incanter.charts :refer [ histogram set-x-range  xy-plot]]
          ;;  [clojure.core.reducers :as r]
            [clojure.core.async :as async ]
            [clojure.core.reducers :as r]
            [clojure.string :refer [ split]]
          )

  )

(def  toNumbersFn  (fn [arg] (cond (nil? arg) nil
                                  (number? arg) arg
                                  :else
                                  (let [theNumber  (read-string arg)]
                                    (try
                                      (if (number? theNumber  ) theNumber  arg)  (catch NumberFormatException e  arg ))))))




(defn parser
  [file]
    (let [
          result (atom {})


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
          (swap! lineNum inc))

        )
      @result
      )
)

(def Y  (parser "C:\\Users\\zhukev\\Documents\\Optimizer\\Linear Algebra\\machine learnign\\hw\\hw2data/newsgrouplabels.txt" ))
(def X  (parser "C:\\Users\\zhukev\\Documents\\Optimizer\\Linear Algebra\\machine learnign\\hw\\hw2data/vocabulary.txt" ) )
(def trainLabel  (parser "C:\\Users\\zhukev\\Documents\\Optimizer\\Linear Algebra\\machine learnign\\hw\\hw2data/train.label" ))
(def testLabel  (parser "C:\\Users\\zhukev\\Documents\\Optimizer\\Linear Algebra\\machine learnign\\hw\\hw2data/test.label" ))

(defn dataFn [dataFile]
  (let  [  data (parser  dataFile )]
    (reduce #( assoc-in %1 [ (nth %2 0) (nth %2 1)]   (nth %2 2))  {} (vals data)  )))

(def testData (dataFn  "C:\\Users\\zhukev\\Documents\\Optimizer\\Linear Algebra\\machine learnign\\hw\\hw2data/test.data" ))
(def trainData (let [  data (parser "C:\\Users\\zhukev\\Documents\\Optimizer\\Linear Algebra\\machine learnign\\hw\\hw2data/train.data" )
                     ]


                 (quote  (let [result (atom {})]


                           (doseq [ item data]
                             (swap! result assoc (nth item 0 ) ;;docId
                                    {:wordId (nth item 1)
                                     :count (nth item 2)}))
                           @result))
             ;;    (first  (vals data))
                  (reduce #( assoc-in %1 [ (nth %2 0) (nth %2 1)]   (nth %2 2))  {} (vals data)  )
               ;;  (reduce #( assoc-in %1  [ [nth %2 0] (nth %2 1)] (nth %2 2))  {} (vals  data)  )
              ;;   (apply merge  (for [ [ _ v ]  data]    ))
               ;;  (apply merge   (pmap #(when (= 3 (count %))  ( assoc {}  (nth % 0) { (nth % 1)  (nth % 2)}) )   data ))
                 ))


(def PY "MLE estimate of P(Y), where y corresponds to label id 1 to 20 "
  (memoize (fn [y]
             (double  (/  (count  (for [ [ docId  label ]  trainLabel :when (= label y)] 0 ))     (count  trainLabel) )))))

(def numX (count X))

(def PXgivenY "MAP estimate P(X|Y), x corresponds to wordId, y corresponds to label "
  (memoize  (fn [ x y & {:keys [ l] :or {l 1}} ]
              (let [ J numX


                    numY  (+   (* l J )   (count  (for [ [ docId  label ]  trainLabel :when (= label y)] 0 )))
                    ;;   numY (+ (* l J)  (reduce +  (for [ [k v ] trainData :when  (= (get trainLabel k) y) ] 1  )))
                    ;; v is {wordId count wordId count...} , k is docId
                    numXAndY  (+ l (reduce +  (for [ [k v ] trainData :when (and  (not (= -1 (.indexOf  (keys v) x )))  (= (get trainLabel k) y) )] (get v x)  )))


                    ]
                ;; (prn "numXAndY " numXAndY "numY" numY)
                (with-precision 40  (/  (bigdec  numXAndY) (bigdec  numY)  )) ))))



(defn trainedPYgivenX []
  (let  [testData (get testData 2)
         y 20]
    (for [ [ x numX]   testData] (pow   (PXgivenY x y)    numX))


   )

  )
(defn predict "predict based on given testData, for a docId, there's workId (x) and count " [docId ]

  (let [ pFn (fn [y] (let [p (for [[wordId count] (get testData docId)    ]
                               (PXgivenY wordId y ))
                          pMax (apply max p)
                          mostLikelyY (first  (filter  #(not (nil? %)) (map-indexed #(when ( = %2 pMax)  %1) p)))
                          ]
                      mostLikelyY

                      ))
        pYgivenX (fn [y] (reduce * (PY y)  (map #(PXgivenY % y) (range 1  (inc (count  X)))    )  ))

   ;;     pYgivenX (fn [y] (prn "PY" (PY y) "pfn" (pFn y)) (*  (PY y) ( pFn y) ))


        ]
    (    pYgivenX 1)

    )
  )
