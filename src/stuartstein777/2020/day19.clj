(ns stuartstein777.2020.day19)

(defn build-rules-graph [rule-map rules rule-no] 
  )

(let [rules {0 ["a" 1 "b"]
             1 [[2 3] [3 2]]
             2 [["a" "a"] ["b" "b"]]
             3 [["a" "b"] ["b" "a"]]}]
(build-rules-graph rules [] 0))