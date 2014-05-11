(ns n-gram.misc.suffix
  (:require
    [n-gram.misc.misc-functions :refer :all]
    [n-gram.words.file-reader :refer [formattedText]])
  )

(use 'clojure.pprint)




(defrecord suffix_node [children cnt])

(defn build_suffix_node "Builds suffix_node with no children and 0 count" [] (suffix_node. (hash-map)  0))



(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
     nested structure. keys is a sequence of keys. Any empty maps that result
     will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

(defn search_tree "Searches tree for given word" [word node params] (let [children  (keys (get-in node params))  ;get keys of all children of the node
                                                                          results (apply merge (for [[k v] (zipmap children (map #(re-find (re-pattern (str "^" word)) %) children)) :when (not (nil? v))] {k v}))] ;find all non-nil matches for the word in the node's children
                                                                      (if (and (nil? results) (> (count word) 1)) (search_tree (subs word 0 (dec (count word))) node params) results))) ;if no matches are found, repeat with the last character of the word removed

(def search_tree_memo "Memoized search_tree" (memoize search_tree))
;([original_word word node] [original_word (search_tree word node)]))

(defn is_exact_match "Determines whether search results match exactly" [results word]  [((comp not nil?)(re-find (re-pattern (str "^" (key results) "$")) (val results))) ;does the matching pattern in the node's children exactly match a pattern in the beginning of the node?
                                                                                        ((comp not nil?) (re-find (re-pattern (str "^" word "$")) (val results))) ;does the word exactly match the pattern in the node's children?
                                                                                        (val results)]) ;the pattern found

(def is_exact_match_memo "Memoized is_exact_match" (memoize is_exact_match))


(defn add_children "Adds all children to given node" [root_node node children params] (cond (= (count children) 1)
                                                                                            (assoc-in root_node (into params [node :children (key (first children))]) (val (first children))) ;add the first child to the node
                                                                                            (> (count children) 1)
                                                                                            (assoc-in (add_children root_node node (rest children) params) (into params [node :children (key (first children))]) (val (first children))) ;recursively call add_children with one less child
                                                                                            :else root_node))

(def add_children_memo "Memoized add_children" (memoize add_children))


(defn branch_node "Branches node" [root-node params old-node new-node] (let [new-sub-node (subs old-node (count new-node) (count old-node))
                                                                             children (assoc (get-in root-node (into params [old-node :children])) new-sub-node (get-in root-node (into params [old-node])))
                                                                             ;find all children of old node and associate them with the rest of the old node (i.e. child of new node)
                                                                             new-root-node (dissoc-in root-node (into params [old-node]))] ;dissociate old node from root node
                                                                         (add_children_memo (assoc-in new-root-node (into params  [new-node]) (build_suffix_node)) new-node children params))) ;associate new node with root node and add all children

(def branch_node_memo "Memoized branch_node" (memoize branch_node))


;(defn print-function [fun & args] (do (println (str fun args)) (fun args)))



(defn build_suffix_tree "Builds suffix tree for word" ([word] (build_suffix_tree word (build_suffix_node)))
  ([word root_node] (build_suffix_tree word root_node nil))
  ([word root_node params](let [root_node (if (nil? params) (cond (> 2 (count word)) root_node 
                                                                  (= 2 (count word)) (build_suffix_tree (str (reduce str (rest word))) root_node)
                                                                  :else (build_suffix_tree (reduce str (rest word)) root_node))
                                            root_node)
                                params (if (nil? params) [:children] params)
                                match-results (search_tree_memo word root_node params)] ;is there a match for the beginning of the word already in the root node's children?
                            (if (nil? match-results) (update-in (assoc-in root_node (into params [word]) (build_suffix_node)) (into params [word :cnt]) inc) ;if there is no match then add a new child
                              (let [exact-match-results (is_exact_match_memo (first match-results) word)] ;is there an exact match [(node exactly matches beginning of word) (node exactly matches whole word) (pattern matched)]
                                (if  (first exact-match-results) ;if node exactly matches some length of the beginning of the word
                                  (if (second exact-match-results) (update-in root_node (into params [(last exact-match-results) :cnt]) inc) ;if whole word is exactly matched, update count of node
                                    
                                    ;(let [params (if (nil? params) [:children (last exact-match-results) :children (subs word  (count (last exact-match-results)) (count word))]
                                    ; (into params [:children (subs word  (count (last exact-match-results)) (count word))]))]
                                    (build_suffix_tree (subs word  (count (last exact-match-results)) (count word)) root_node
                                                       ;                                                                                             (assoc-in root_node (into params [(last exact-match-results) :children (subs word  (count (last exact-match-results)) (count word))])
                                                       ;                                                                                                       (build_suffix_node)) 
                                                       (into params [(last exact-match-results) :children]))) ;otherwise add a child to node with the end of the word
                                  (update-in (assoc-in (branch_node_memo root_node params      (key (first match-results)) (val (first match-results)) )
                                                       (into params [ (last exact-match-results) :children 
                                                                     (subs word  (count (last exact-match-results)) (count word))]) 
                                                       (build_suffix_node)) (into params [ (last exact-match-results) :children 
                                                                                          (subs word  (count (last exact-match-results)) (count word)) :cnt]) inc))))))) ;if the beginning of the word matches the beginning of the node, branch the node and add the rest of the word as a child

(def build_suffix_tree_memo "Memoized build_suffix_tree" (memoize build_suffix_tree))

;(def tree-text "dorothy")

(defn create-indices "Returns a vector containing the indices of the first and last characters of the sub-word in the word" 
  [word sub-word] (let [first-char-index (.indexOf word sub-word)] [first-char-index (+ first-char-index (count sub-word))]))

(def create-indices-memo "Memoized create-indices" (memoize create-indices))

(defn dereference-indices "Returns the sub-word from word given the first and last character indicies"
  [word indices] (str (subs word (first indices) (second indices))))

(def dereference-indices-memo "Memoized dereference-indices" (memoize dereference-indices))

(defn search_tree_indices "Searches tree for any matches to given word with node keys as a vector of charcater indices"
  [word node params tree-text] (let [children  (keys (get-in node params))  ;get keys of all children of the node
                                     results (apply merge (for [[k v] (zipmap (map #(dereference-indices-memo tree-text %) children) (map #(re-find (re-pattern (str "^" word)) (dereference-indices-memo tree-text %)) children)) :when (not (nil? v))] {k v}))] ;find all non-nil matches for the word in the node's children
                                 (if (and (nil? results) (> (count word) 1)) (search_tree_indices (subs word 0 (dec (count word))) node params tree-text) results))) ;if no matches are found, repeat with the last character of the word removed

(def search_tree_indices_memo "Memoized search_tree_indices" (memoize search_tree_indices))



(defn branch_node_indices "Branches node with node keys as vector of character indices"
  [root-node params old-node new-node tree-text] (let [old-node-indices (create-indices-memo tree-text (clojure.string/reverse old-node)) new-node-indices (create-indices-memo tree-text (clojure.string/reverse new-node)) 
                                                       new-sub-node-indices (create-indices-memo tree-text (clojure.string/reverse (subs old-node (count new-node) (count old-node))))
                                                       children (assoc (get-in root-node (into params [old-node-indices :children])) new-sub-node-indices (get-in root-node (into params [old-node-indices]))) ;find all children of old node and associate them with the rest of the old node (i.e. child of new node)
                                                       new-root-node (dissoc-in root-node (into params [old-node-indices]))] ;dissociate old node from root node
                                                   (add_children_memo (assoc-in new-root-node (into params  [new-node-indices]) (build_suffix_node)) new-node-indices children params))) ;associate new node with root node and add all children

(def branch_node_indices_memo "Memoized branch_node_indices" (memoize branch_node_indices))


;(defn print-function [fun & args] (do (println (str fun args)) (fun args)))



(defn build_suffix_tree_indices "Builds suffix tree with node keys as character indicies" ([word] [word (build_suffix_tree_indices word (build_suffix_node) word)])
  ([word root_node tree-text] (build_suffix_tree_indices word root_node nil tree-text))
  ([word root_node params tree-text](let [root_node (if (nil? params) (cond (> 2 (count word)) root_node 
                                                                            (= 2 (count word)) (build_suffix_tree_indices (str (reduce str (rest word))) root_node tree-text)
                                                                            :else (build_suffix_tree_indices (reduce str (rest word)) root_node tree-text))
                                                      root_node)
                                          params (if (nil? params) [:children] params)
                                          match-results (search_tree_indices_memo word root_node params tree-text)
                                          word-indices (create-indices-memo tree-text word)] ;is there a match for the beginning of the word already in the root node's children?
                                      (if (nil? match-results) (update-in (assoc-in root_node (into params [word-indices]) (build_suffix_node)) (into params [word-indices :cnt]) inc) ;if there is no match then add a new child
                                        (let [exact-match-results (is_exact_match_memo (first match-results) word)] ;is there an exact match [(node exactly matches beginning of word) (node exactly matches whole word) (pattern matched)]
                                          (if  (first exact-match-results) ;if node exactly matches some length of the beginning of the word
                                            (if (second exact-match-results) (update-in root_node (into params [(create-indices-memo tree-text (last exact-match-results)) :cnt]) inc) ;if whole word is exactly matched, update count of node
                                              
                                              ;(let [params (if (nil? params) [:children (last exact-match-results) :children (subs word  (count (last exact-match-results)) (count word))]
                                              ; (into params [:children (subs word  (count (last exact-match-results)) (count word))]))]
                                              (build_suffix_tree_indices (subs word  (count (last exact-match-results)) (count word)) root_node
                                                                         ;                                                                                             (assoc-in root_node (into params [(last exact-match-results) :children (subs word  (count (last exact-match-results)) (count word))])
                                                                         ;                                                                                                       (build_suffix_node)) 
                                                                         (into params [(create-indices-memo tree-text (last exact-match-results)) :children]) tree-text)) ;otherwise add a child to node with the end of the word
                                            (update-in (assoc-in (branch_node_indices_memo root_node params      (key (first match-results)) (val (first match-results)) tree-text)
                                                                 (into params [ (create-indices-memo tree-text (last exact-match-results)) :children 
                                                                               (create-indices-memo tree-text (subs word  (count (last exact-match-results)) (count word)))]) 
                                                                 (build_suffix_node)) (into params [ (create-indices-memo tree-text (last exact-match-results)) :children 
                                                                                                    (create-indices-memo tree-text (subs word  (count (last exact-match-results)) (count word))) :cnt]) inc))))))) ;if the beginning of the word matches the beginning of the node, branch the node and add the rest of the word as a child

(def build_suffix_tree_indices_memo "Memoized build_suffix_tree_indices" (memoize build_suffix_tree_indices))


(defn loop-find-children [node] (let [child (first (keys (get node :children)))] (if ((comp not nil?) child) (str child (loop-find-children (get-in node [:children child]))))))

(defn loop-through-nodes [node key children] (if (> (count children) 0) (loop-through-nodes (get-in node [:children (first (keys children))]) 
                                                                                            (get-in node [:children (first (keys children)) :children]))
                                               key))

(defn find_branch [tree params] (str "{" (first (keys (get-in tree params))) (find_branch tree (into params [(first (keys (get-in tree params))) :children])) "}"))

(defn print-tree ([tree] (print-tree tree [:children]))
  ([tree params] (print-tree tree params ""))
  ([tree params output] (let [children (keys (get-in tree params))](if ((comp not empty?) children) (print-tree tree (into params [(first children) :children]) (str output "{" (first children) ) )
                                                                     
                                                                     (println (str output "{" (first children) "}"))))))

(defrecord restaurant_node [range children restaurant depth]); restaurant with values as a vector with each element representing the number of customers at a table, depth with values [start end]

(defn build_restaurant_node "Builds a new restaurant node object
[] node with empty range, children and restaurant
[range] node with the given range but empty children and restaurant
[range letter] node with the given range, empty children and one table with one customer for the given letter in the restaurant
[range children restaurant] node with the given range, children and restaurant" 
  ([depth] (restaurant_node. [] (hash-map) (hash-map) depth))
  ([range depth] (restaurant_node. range (hash-map) (hash-map) depth))
  ([range letter depth] (restaurant_node. range (hash-map) {letter [1]} depth))
  ([range children restaurant depth](restaurant_node. range children restaurant depth)))

(def d0 0.62)

(def d1 0.69)

(def d2 0.74)

(def d3 0.80)

(def d4 0.95)

(defn return_discount [level] (cond (= 0 level) d0
                                    (= 1 level) d1
                                    (= 2 level) d2
                                    (= 3 level) d3
                                    :else d4))

(def return_discount_memo (memoize return_discount))

(defn find_discount [depth] (if (= (first depth) (second depth))
                              (return_discount_memo (first depth))
                              (* (return_discount_memo (first depth)) (find_discount [(inc (first depth)) (second depth)]))))

(def find_discount_memo (memoize find_discount))

(defn p_new_table [number_of_tables discount number_of_customers]; (let [number_of_tables (count (get-in tree (into params [:restaurant])))
                                                                  ;      discount (find_discount_memo (get-in tree (into params [:depth])))
                                                                   ;     number_of_customers (reduce + (map #(second (val %)) (get-in tree (into params [:restaurant]))))]
                                                                    (/ (* number_of_tables discount) number_of_customers))

(def p_new_table_memo (memoize p_new_table))

(defn p_existing_table [number_of_customers_at_table discount number_of_customers]  ;(let [number_of_customers_at_table (second (get-in tree (into params [:restaurant letter])))
                                                                                     ;     discount (find_discount_memo (get-in tree (into params [:depth]))) 
                                                                                      ;    number_of_customers (reduce + (map #(second (val %)) (get-in tree (into params [:restaurant]))))]
                                                                                      (/ (- number_of_customers_at_table discount) number_of_customers))

(def p_existing_table_memo (memoize p_existing_table))

(defn find_max_prob [tables] (let [maxs (map #(apply max %) tables)]
                               (apply max maxs)))

(def find_max_prob_memo (memoize find_max_prob))

(defn generate_prob_limits_loop [probs_map] (if (= 1 (count probs_map)) (zipmap [(first (keys probs_map))] [(apply vector (cumsum (first (vals probs_map))))])
                                              (let [letters (keys probs_map)
                                                    resulting_map (generate_prob_limits_loop (dissoc probs_map (last letters)))]
                                              (assoc  resulting_map (last letters) (apply vector (map #(+ (find_max_prob_memo (vals resulting_map)) %) (apply vector (cumsum (get probs_map (last letters)) ))))))))

(def generate_prob_limits_loop_memo (memoize generate_prob_limits_loop))

(defn generate_probs_for_table_array [tables discount number_of_customers] (if (> (count tables) 1) (into (generate_probs_for_table_array (rest tables) discount number_of_customers) [(p_existing_table (first tables) discount number_of_customers)])
                                                                                                     [(p_existing_table (first tables) discount number_of_customers)]))

(def generate_probs_for_table_array_memo (memoize generate_probs_for_table_array))

(defn generate_prob_limits ([tree params] (let [restaurant (get-in tree (into params [:restaurant]))
                                                letters (keys restaurant)
                                                discount (find_discount_memo (get-in tree (into params [:depth]))) 
                                                number_of_customers (reduce + (map #(reduce + (val %)) restaurant))
                                                number_of_tables (reduce + (map #(count (val %)) restaurant))
                                                probs_map (assoc  (zipmap letters (map #(generate_probs_for_table_array_memo (get restaurant %) discount number_of_customers) letters)) "" [(p_new_table_memo number_of_tables discount number_of_customers)])]
                                            (generate_prob_limits_loop_memo probs_map)))
  ([tree params letter] (let [restaurant (get-in tree (into params [:restaurant]))
                                                ;letters (keys restaurant)
                                                discount (find_discount_memo (get-in tree (into params [:depth]))) 
                                                number_of_customers (reduce + (map #(reduce + (if (= (key %) letter) (val %) [0])) restaurant))
                                                number_of_tables (reduce + (map #(count (if (= (key %) letter) (val %) [])) restaurant))
                                                probs_map (assoc  (zipmap [letter] [(generate_probs_for_table_array_memo (get restaurant letter) discount number_of_customers)] ) "" [(p_new_table_memo number_of_tables discount number_of_customers)])]
                                            (generate_prob_limits_loop_memo probs_map))))

(def generate_prob_limits_memo (memoize generate_prob_limits))

;(defn find_min_prob [prob_limits] (let [mins_map (zipmap (keys prob_limits) (map #(apply min %) (vals prob_limits)))]
;                                    (apply min-key val mins_map) ))
;
;(def find_min_prob_memo (memoize find_min_prob))

(defn find_min_prob_and_index [prob_limits] (let [mins_map (zipmap (keys prob_limits) (map #(apply min %) (vals prob_limits))) the_min (apply min-key val mins_map)]
                                    [the_min (.indexOf (get prob_limits (key the_min)) (val the_min))]))

(def find_min_prob_and_index_memo (memoize find_min_prob_and_index))

;(defn prob_limits_loop [deciding_prob prob_limits] (let [min_limit (find_min_prob_and_index prob_limits)]
;                                                     (if (<= deciding_prob (val min_limit)) (key min_limit)
;                                                       (if (> (count (get prob_limits (key min_limit))) 1)
;                                                                               (prob_limits_loop deciding_prob (dissoc prob_limits (key min_limit))))))
;
;(def prob_limits_loop_memo (memoize prob_limits_loop))

(defn vec-remove
  "remove elem in coll"
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn prob_limits_and_index_loop [deciding_prob prob_limits] (let [min_limit (find_min_prob_and_index_memo prob_limits)]
                                                               (if (<= deciding_prob (val (first min_limit))) [(key (first min_limit)) (second min_limit)]
                                                                 (if (> (count (get prob_limits (key (first min_limit)))) 1)
                                                                   (let [new_tables (vec-remove (get prob_limits (key (first min_limit))) (second min_limit))]
                                                                     (prob_limits_and_index_loop deciding_prob (assoc prob_limits (key (first min_limit)) new_tables)))
                                                                                         (prob_limits_and_index_loop deciding_prob (dissoc prob_limits (key (first min_limit))))))))

(def prob_limits_and_index_loop_memo (memoize prob_limits_and_index_loop))


(defn loop_find_tables "Cycles through all the children of the node to find the total number of tables for the given letter"
  [children node letter params] (let [result (get-in node (into params [:children (first children) :restaurant letter]))
             tables (if (nil? result) [] result)] 
         (if (>= 1 (count children)) (count tables)
           (+ (count tables) (loop_find_tables (rest children) node letter params)))))

(def loop_find_tables_memo "Memoized loop_find_tables" (memoize loop_find_tables))

(defn find_child_table_count "Returns the total count of all tables for the given letter in the node's children"
  [node letter params] (let [children (keys (get-in node (into params [:children])))]  (loop_find_tables_memo children node letter params)))
                        

(def find_child_table_count_memo "Memoized find_child_table_count" (memoize find_child_table_count))

(defn assign_customer [tree params letter] 
  (prob_limits_and_index_loop_memo (rand) 
                                   (generate_prob_limits_memo tree params letter)))

(defn check_table_customer_consistency "Checks that number of tables for the given letter in the node's children is equal to the number of customers for the letter in the node's restaurant"
  [node letter params](let [table_count (find_child_table_count_memo node letter params) 
                           parent_restaurant (get-in node (into params [:restaurant letter]))
                           parent_restaurant (if (nil? parent_restaurant) [] parent_restaurant)
                           parent_customer_count (reduce + parent_restaurant)
                           difference (- table_count parent_customer_count)]
                        (cond (> difference 1)
                       (if (not (zero? table_count))
                         (if (not (zero? parent_customer_count))
                         (let [assignment (assign_customer node params letter)]
                           (if (= (first assignment) "") 
                             (check_table_customer_consistency (assoc-in node (into params [:restaurant letter]) (into parent_restaurant [1])) letter params)
                             (let [new_tables (first (map #(if (= (.indexOf parent_restaurant %) (second assignment)) (inc %) %) parent_restaurant))]
                               (check_table_customer_consistency (assoc-in node (into params [:restaurant letter]) [new_tables]) letter params))))
                         (check_table_customer_consistency (assoc-in node (into params [:restaurant letter]) [1]) letter params)))
                       (= difference 1)
                       (if (not (zero? table_count))
                        (if (not (zero? parent_customer_count))
                         (let [assignment (assign_customer node params letter)]
                           (if (= (first assignment) "") 
                             (assoc-in node (into params [:restaurant letter]) (into parent_restaurant [1]))
                             (let [new_tables (first (map #(if (= (.indexOf parent_restaurant %) (second assignment)) (inc %) %) parent_restaurant))]
                               (assoc-in node (into params [:restaurant letter]) [new_tables]))))
                         (assoc-in node (into params [:restaurant letter]) [1])))
                       (zero? difference)
                       node)))
                           
(def check_table_customer_consistency_memo "Memoized check_table_customer_consistency" (memoize check_table_customer_consistency))

(defn check_for_children "Checks if there are any children starting with the given letter"
  [letter children] (if (< 0 (count children)) 
                     (if (= (first children) letter) true (check_for_children letter (rest children))) false))

(def check_for_children_memo "Memoize check_for_children" (memoize check_for_children))

(defn check_range "Compares the prefix to the range on the child node and returns a vector with the values [(matching string) (is the match length equal to the range length) (is the prefix longer than the match)]"
  ([range prefix root_word] (check_range range prefix root_word (count (dereference-indices-memo root_word range)))) 
  ([range prefix root_word length] (let [range_letters (clojure.string/reverse (dereference-indices-memo root_word range)) match (re-find (re-pattern (str "^" range_letters)) prefix)] 
                                     (if (nil? match) 
                                       (check_range (create-indices-memo root_word (clojure.string/reverse (subs range_letters 0 (dec (count range_letters))))) prefix root_word length)
                                       [match (= length (count match)) (> (count prefix) (count match))]))))

(def check_range_memo "Memoized check_range" (memoize check_range))

(defn check_consistency_all_tables "Checks that the number of tables in all children is equal to the number of customers in the node's restaurant for all letters"
  [node tables params] (if (<= 1 (count tables)) 
                        (check_table_customer_consistency_memo node (first tables) params)
                        (check_consistency_all_tables (check_table_customer_consistency_memo node (first tables) params) (rest tables) params)))

(def check_consistency_all_tables_memo "Memoized check_consistency_all_tables" (memoize check_consistency_all_tables))

(defn branch "Replaces the node with another with range new_range and children a copy of the old node but with the remaining range"
  [node letter new_range new_letter params] (let [old_range (get-in node (into params [:children letter :range])) 
                                                 restaurant (get-in node (into params [:children letter :restaurant])) 
                                                 children (get-in node (into params [:children letter :children]))
                                                 old_depth (get-in node (into params [:children letter :depth]))
                                                 new_range_length (- (last new_range) (first new_range))]
                                             (check_consistency_all_tables_memo
                                               (assoc-in 
                                                 (assoc-in 
                                                   (dissoc-in node (into params [:children letter]))
                                                   (into params [:children letter]) (build_restaurant_node new_range [(first old_depth) (dec (+ (first old_depth) new_range_length))]))
                                                 (into params [:children letter :children new_letter]) (build_restaurant_node [(first old_range) (- (last old_range) new_range_length)] children restaurant [(+ (first old_depth) new_range_length) (last old_depth)]))
                                               (keys restaurant) (into params [:children letter]))))

(def branch_memo "Memoized branch" (memoize branch))

(defn build_tree "Builds a suffix tree for the given word"
  ([word]  (build_tree word (build_restaurant_node [0 0]) word [] 1)) 
  ([word root_node root_word params new_depth] 
                                                 (let [word_length (count word)
                                                       root_node (if (empty? params) (cond (> 2 word_length) root_node 
                                                                   ;(= 2 (count word)) (build_tree (str (reduce str (rest word))) root_node root_word params new_depth)
                                                                   :else (build_tree (subs word 0 (dec word_length)) root_node root_word params new_depth))
                                                                                 root_node) 
                                                                     prefix (cond (< 1 (count word)) (clojure.string/reverse (subs word 0 (dec word_length)))
                                                                                 ; (= 2 (count word)) (str (reduce str (butlast word)))
                                                                                  :else "")
                                                                     new_depth (if (= prefix "") (dec new_depth) new_depth)
                                                                     prefix_count (if (= prefix "") 1 (count prefix))
                                                                     prefix_start (str (first prefix))
                                                                     letter (str (last word))
                                                                     children (get-in root_node (into params [:children]))]
                                                                 
                                                                   (if (empty? children) 
                                                                     (check_table_customer_consistency_memo
                                                                       (assoc-in root_node (into params [:children prefix_start]) 
                                                                                 (build_restaurant_node (create-indices-memo root_word (clojure.string/reverse prefix)) letter [new_depth (dec (+ new_depth prefix_count))]) 
                                                                                 )
                                                                       letter params)
                                                                     (if (check_for_children_memo prefix_start (keys children))
                                                                       (let [match_results (check_range_memo (get-in root_node (into params [:children prefix_start :range])) prefix root_word)]
                                                                         (if (second match_results)
                                                                           (if (nth match_results 2)
                                                                               (let [new_prefix (subs prefix (count (first match_results)) (count prefix))]
                                                                               (check_table_customer_consistency_memo
                                                                                 (check_table_customer_consistency_memo  
                                                                                   (build_tree (str (clojure.string/reverse new_prefix) letter) root_node root_word (into params [:children prefix_start]) (+ new_depth (- (count prefix) (count new_prefix))))
                                                                                   letter (into params [:children prefix_start]))
                                                                                 letter params))
                                                                             (check_table_customer_consistency_memo root_node letter params))
                                                                           (let [new-range (create-indices-memo root_word (clojure.string/reverse (first match_results))) matching_branch_range (get-in root_node (into params [:children prefix_start :range]))]
                                                                             (check_table_customer_consistency_memo 
                                                                               (check_table_customer_consistency_memo
                                                                                 (assoc-in 
                                                                                   (branch_memo root_node prefix_start new-range 
                                                                                           (subs (clojure.string/reverse (dereference-indices-memo root_word matching_branch_range)) (count (first match_results)) (inc (count (first match_results)))) params)
                                                                                   (into params [:children prefix_start :children (subs prefix (count (first match_results)) (inc (count (first match_results))))]) 
                                                                                   (build_restaurant_node (create-indices-memo root_word (clojure.string/reverse (subs prefix (count (first match_results)) (count prefix)))) letter [(+ new_depth (count (first match_results))) (dec (+ new_depth (count prefix)))]))
                                                                                 letter (into params [:children prefix_start]))
                                                                               letter params)))
                                                                         )
                                                                       (check_table_customer_consistency_memo
                                                                         (assoc-in root_node (into params [:children prefix_start]) (build_restaurant_node (create-indices-memo root_word (clojure.string/reverse prefix)) letter [new_depth (dec (+ new_depth (count prefix)))]))
                                                                         letter params))))))

(def build_tree_memo "Memoized build_tree" (memoize build_tree))


(def uniform_letters_distribution_limits (generate_prob_limits_loop_memo (zipmap (map #(str %) (map char (concat (range 32 33 )(range 48 58) (range 97 123)  ))) (repeat 37 [(float (/ 1 37))]))))

(defn determine_letter [tree params] (let [deciding_prob (rand)
                                           prob_limits (generate_prob_limits_memo tree params)
                                           letter (first (prob_limits_and_index_loop_memo deciding_prob prob_limits))]
                                       (if (= letter "") 
                                         (let [new_params (into [] (butlast (butlast params)))]
                                           (if (empty? new_params) (first (prob_limits_and_index_loop_memo deciding_prob uniform_letters_distribution_limits))
                                           (determine_letter tree new_params)))
                                         letter)))

(defn next_letter [tree context root_text] (let [children (keys (get tree :children))
                                                 context (clojure.string/reverse context)]
                                             (if (check_for_children_memo (str (first context)) children) ;are there any children starting with first letter of context
                                               (let [range_results (check_range_memo (get-in tree [:children (str (first context)) :range]) context root_text)]
                                                   (if (second range_results)
                                                     (if (last range_results)
                                                       (next_letter (get-in tree [:children (str (first context))]) (clojure.string/reverse (subs context (count (first range_results)) (count context))) root_text)  ;context length> match length - recursivley call with increased depth
                                                     (determine_letter tree [:children (str (first context))])) ;match length=range length - search with child params
                                                     ;otherwise
                                                     
                                                       (determine_letter (branch_memo tree (str (first context)) 
                                                                                      (create-indices-memo root_text (clojure.string/reverse (first range_results))) 
                                                                                      (subs (clojure.string/reverse (dereference-indices-memo root_text (get-in tree [:children (str (first context)) :range]))) (count (first range_results)) (inc (count (first range_results))))
                                                                                      []) [:children (str (first context))])));otherwise, instantiate node mid-way and search parent distribution
                                     
                                               (determine_letter tree []))))

(defn find_prob [tree context root_text letter] (let [
                                                        context (clojure.string/reverse context)]
                                             
                                                      (let [range_results (check_range_memo (get-in tree [:children (str (first context)) :range]) context root_text)]
                                                          (if (second range_results)
                                                            (if (last range_results)
                                                              (find_prob (get-in tree [:children (str (first context))]) (clojure.string/reverse (subs context (count (first range_results)) (count context))) root_text letter)  ;context length> match length - recursivley call with increased depth
                                                            (let [probs (generate_probs_for_table_array_memo (get-in tree [:children (str (first context)) :restaurant letter]) (find_discount_memo (get-in tree [:children (str (first context)) :depth]))
                                                                                                             (reduce + (map #(reduce + (val %)) (get-in tree [:children (str (first context)) :restaurant]))))]
                                                              (/ (reduce + probs) (count probs))));match length=range length - search with child params
                                                            ;otherwise
                                                     
                                                              ))))

(def find_prob_memo (memoize find_prob))

(defn loop_predict [tree context root_text length] (if (> length 1) 
                                                     (let [the_next_letter (next_letter tree context root_text)]
                                                       (str the_next_letter (loop_predict tree (str context the_next_letter) root_text (dec length))))
                                                     (next_letter tree context root_text)))

(defn predict_text [tree context root_text length] (str context (loop_predict tree context root_text length)))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn loop_find_probs [tree tree-text root-text] (let [text-count (count tree-text)] 
                                                   (if (> text-count 1) (+ (log2 (find_prob_memo tree (subs tree-text 0 (dec text-count)) root-text (str (last tree-text)))) (loop_find_probs tree (subs tree-text 0 (dec text-count)) root-text))
                                                     (log2 (find_prob_memo tree (subs tree-text 0 (dec text-count)) root-text (str (last tree-text)))))))

(defn ell [tree tree-text] (- (* (/ 1 (count tree-text)) (loop_find_probs tree tree-text tree-text))))

(defn perplexity [tree tree-text] (Math/pow 2 (ell tree tree-text)))
                                       

                                                            ; (defn 
                                                            ;                                            
                                                            ; (defn draw-tree [tree]                       
                                                            ;
                                                            ; ======================================
                                                            ;      Tree construction functions
                                                            ; ======================================
                                                            ;
                                                            ;(defn build_node
                                                            ;  "Creates an empty node"
                                                            ;  []
                                                            ;  (let [next (hash-map)
                                                            ;        idx_start (hash-set)
                                                            ;        cnt 0]
                                                            ;    (node. next idx_start cnt)))
                                                            ;
                                                            ;
                                                            ;(defn build_suffix
                                                            ;  "Generates the sub-tree for the given suffix
                                                            ;
                                                            ;   Input:
                                                            ;        word - the suffix word to generate suffix tree for
                                                            ;               (required)
                                                            ;        idx - index which indicates at which indices
                                                            ;              does the current path correspond to
                                                            ;              (required)
                                                            ;        node - node in which the tree will be rooted.
                                                            ;               If not given - creates an empty node and roots
                                                            ;               the new tree under this node.
                                                            ;
                                                            ;   Returns:
                                                            ;        tree for the given suffix (of type node)
                                                            ;   "
                                                            ;  ([word idx] (build_suffix word (build_node) idx))
                                                            ;  ([word node idx]
                                                            ;    (if (empty? word)
                                                            ;      ; base case (finished reading a suffix)
                                                            ;        (update-in 
                                                            ;          (update-in node [:idx_start] conj idx)
                                                            ;            [:cnt] inc)
                                                            ;
                                                            ;      ; induction (still left to read from suffix)
                                                            ;      (let [next_node (get-in node [:next (first word)] (build_node))]
                                                            ;        (assoc-in 
                                                            ;          (update-in 
                                                            ;            (update-in node [:idx_start] conj idx)
                                                            ;              [:cnt] inc)
                                                            ;          [:next (first word)] 
                                                            ;          (build_suffix (subs word 1) next_node idx))
                                                            ;        ))))
                                                            ;
                                                            ;(defn build_tree
                                                            ;  "Generates the suffix tree for the given word
                                                            ;
                                                            ;   Input:
                                                            ;        word - the word to generate suffix tree for.
                                                            ;               word must be a string. (required)
                                                            ;   Method:
                                                            ;        Constructs the tree by building each suffix
                                                            ;        and appending it to the root node of the tree
                                                            ;   Returns:
                                                            ;        Root node for the suffix tree
                                                            ;   "
                                                            ;  [word]
                                                            ;  (if (not= (string? word))
                                                            ;    (throw (Throwable. "Input must be a string!"))
                                                            ;  (loop [idx 0
                                                            ;         root (build_suffix word idx)
                                                            ;         rest_word (subs word 1)]
                                                            ;    (if (empty? rest_word)
                                                            ;      root
                                                            ;    (recur (inc idx)
                                                            ;           (build_suffix rest_word root (+ idx 1))
                                                            ;           (subs rest_word 1))))))
                                                            ;
                                                            ;
                                                            ;(defn print-subtree [subtree prefix]
                                                            ;  (if (empty? subtree)
                                                            ;    ""
                                                            ;    (apply str prefix (first subtree) "\n"
                                                            ;           (map #(print-subtree %1 (str "\t" prefix)) (rest subtree)))))
                                                            ; 
                                                            ;(defn print-tree [tree]
                                                            ;  (print-subtree tree ""))
                                                            ;
                                                            ;
                                                            ; ======================================
                                                            ;       Helper functions
                                                            ; ======================================
                                                            ;
                                                            ;(defn pred_mismatch
                                                            ;  "A predicate for validating the params to querys with collections
                                                            ;
                                                            ;   Input:
                                                            ;        word - the word to be searched in (string). Required.
                                                            ;        col - collection of patterns (strings), Required.
                                                            ;   Method:
                                                            ;        Validates that word is a non-empty string,
                                                            ;        and col is a non-empty collection of strings.
                                                            ;   Returns:
                                                            ;        true if either of the params type mismatch
                                                            ;        and false otherwise
                                                            ;   "
                                                            ;  [word col]
                                                            ;  (or (or (not (string? word))
                                                            ;          (empty? word)
                                                            ;          (and (not= (list? col))
                                                            ;               (not= (vector? col))))
                                                            ;           (or (empty? col)
                                                            ;               (not-every? string? col))))
                                                            ;
                                                            ;
                                                            ;(defn lazy_query_real
                                                            ;  "Generates the lazy sequence queries
                                                            ;   
                                                            ;   This function is called after parameters were
                                                            ;   validated on 'lazy_query', and it returns the
                                                            ;   lazy sequence of the results.
                                                            ;
                                                            ;  "
                                                            ;  [func col root]
                                                            ;  (if (empty? col)
                                                            ;    nil
                                                            ;  (cons (func root (first col))
                                                            ;        (lazy-seq (lazy_query_real func (rest col) root))))  
                                                            ;  )
                                                            ;
                                                            ;(defn lazy_query
                                                            ;  "A predicate for validating the params to querys with collections
                                                            ;
                                                            ;   Input:
                                                            ;        func - the function to be applied on each
                                                            ;               of the patterns
                                                            ;        word - the word to be searched in (string). Required.
                                                            ;        col - collection of patterns (strings), Required.
                                                            ;
                                                            ;   Method:
                                                            ;        Invokes that word is a string,
                                                            ;        and col is a non-empty collection of strings.
                                                            ;
                                                            ;   Returns:
                                                            ;        A lazy sequence of the patterns evaluated with
                                                            ;        the given func.
                                                            ;
                                                            ;   Throws:
                                                            ;        Exception when parameters mismatch with the error msg.
                                                            ;   "
                                                            ;  
                                                            ;  [func word col]
                                                            ;  (if (pred_mismatch word col)
                                                            ;    (throw (Throwable. (str
                                                            ;      "\nInput must be:\n"
                                                            ;      "col - a non-empty collection of string patterns\n"
                                                            ;      "word - a non-empty string!\n")))
                                                            ;    (lazy_query_real func col (build_tree word)))
                                                            ;  )
                                                            ;
                                                            ;
                                                            ;(defn get_str_path
                                                            ;  "Returns the string formed by concat the path until base case
                                                            ;
                                                            ;   Input:
                                                            ;        node - the current node that correspond to
                                                            ;               the path read so far from the root (required).
                                                            ;        cnt - number of suffixes containing the longest
                                                            ;              repeated pattern (required).
                                                            ;        word - the repeat pattern that was so far discovered
                                                            ;               (required).
                                                            ;
                                                            ;   Method:
                                                            ;        Traverse in the tree as long as the number of
                                                            ;        suffixes found (in the current node) is the same
                                                            ;        as the number of suffixes of the longest repeated substring.
                                                            ;
                                                            ;   Returns:
                                                            ;        The longest repeating substring from the child node
                                                            ;        of the root (supplied as node)
                                                            ;
                                                            ;   Note: this function is TCO optimized
                                                            ;         (last line returns the result of the next call)
                                                            ;
                                                            ;  "
                                                            ;  [node cnt word]
                                                            ;  (if (or (empty? (:next node))
                                                            ;          (> (count (vals (:next node))) 1)
                                                            ;          (< (:cnt (nth (vals (get node :next)) 0)) cnt))
                                                            ;    word
                                                            ;    ; concatenate myself & next call
                                                            ;    (let [ch_cur (nth (keys (get node :next)) 0)
                                                            ;          next_node (nth (vals (get node :next)) 0)]
                                                            ;         (get_str_path next_node cnt (str word ch_cur)))))
                                                            ;
                                                            ;
                                                            ;(defn get_max_child
                                                            ;  "Returns all the longest repeated strings from root's children
                                                            ;
                                                            ;   Input:
                                                            ;        root - the root node of the suffix tree
                                                            ;
                                                            ;   Method:
                                                            ;        Iterates over each of the root children,
                                                            ;        and for each child that has max cnt (most commonly repeated) 
                                                            ;        returns the 'longest repeated substring' that starts
                                                            ;        by traversing from the root node via that child.
                                                            ;   Returns:
                                                            ;        a collection of all the most commonly repeated patterns
                                                            ;
                                                            ;   "
                                                            ;  [root]
                                                            ;  (let [max_cnt (reduce max (map :cnt (vals (:next root))))]
                                                            ;    (for [ch (keys (:next root))
                                                            ;          :when (= (get-in root [:next ch :cnt]) max_cnt)] 
                                                            ;      (str (get_str_path (get-in root [:next ch]) max_cnt ch)))))
                                                            ;
                                                            ;
                                                            ;(defn longer
                                                            ;  "Returns the longer string between s1 & s2"
                                                            ;  [s1 s2]
                                                            ;  (if (> (count s1) (count s2))
                                                            ;    s1 s2))
                                                            ;
                                                            ;
                                                            ;
                                                            ; ======================================
                                                            ;    Query functions (single pattern)
                                                            ; ======================================
                                                            ;
                                                            ;(defn which_ind
                                                            ;  "Retrieves the set of indices where sub is located 
                                                            ;   (and nil otherwise).
                                                            ;
                                                            ;   Input:
                                                            ;        root - the suffix tree (required).
                                                            ;        sub - sub-word to be searched for (required).
                                                            ;   Returns:
                                                            ;        the set of indices where sub is located,
                                                            ;        and nil otherwise
                                                            ;   "
                                                            ;  [root sub]
                                                            ;  (if (empty? sub)
                                                            ;    (get-in root [:idx_start])
                                                            ;    (if-let [next_node (get-in root [:next (first sub)])]
                                                            ;      (which_ind next_node (subs sub 1))
                                                            ;      nil)))
                                                            ;
                                                            ;
                                                            ;(defn is_sub
                                                            ;  "Checks whether sub is a substring of a word 
                                                            ;
                                                            ;   Input:
                                                            ;        root - the suffix tree (required).
                                                            ;        sub - sub-word to be searched for (required).
                                                            ;   Returns:
                                                            ;        true if sub is a substring of root
                                                            ;        false otherwise
                                                            ;   "
                                                            ;  [root sub]
                                                            ;  (not= (which_ind root sub) nil))
                                                            ;
                                                            ;
                                                            ;(defn longest_repeating_substring
                                                            ; "Finds the longest repeating substring 
                                                            ;
                                                            ;   Input:
                                                            ;        word - the word to be searched in (required).
                                                            ;   
                                                            ;   Method:
                                                            ;       1. for all child's of root, find node with max count
                                                            ;       2. traverse the path from the root via the child-node with max
                                                            ;          count, and read letters on the path
                                                            ;       3. stop when:
                                                            ;           a. more than one child (more than one key in the map)
                                                            ;           b. child has lower counter than me 
                                                            ;              (that means the substring repeats twice)
                                                            ;
                                                            ;   Returns:
                                                            ;        The longest repeated substring. 
                                                            ;        If there's more than one substring that appears
                                                            ;        in the same amount of times in the string, returns
                                                            ;        the longest one among them.
                                                            ;        
                                                            ;        Note: if there is no repeating substring, returns
                                                            ;              the string itself
                                                            ;   "
                                                            ;  ([_ word] 
                                                            ;    (longest_repeating_substring (build_tree word)))
                                                            ;  ([root]
                                                            ;    (reduce longer (get_max_child root))))
                                                            ;
                                                            ;
                                                            ;(defn how_many
                                                            ;  "Returns how many times a substring appears inside word 
                                                            ;
                                                            ;   Input:
                                                            ;        root - the suffix tree (required).
                                                            ;        sub - sub-word to be searched for (required).
                                                            ;
                                                            ;   "
                                                            ;  [root sub]
                                                            ;  (if (empty? sub)
                                                            ;    (:cnt root)
                                                            ;    (if-let [next_node (get-in root [:next (first sub)])]
                                                            ;      (how_many next_node (subs sub 1))
                                                            ;      0)))
                                                            ;
                                                            ;
                                                            ;
                                                            ; ======================================
                                                            ;  Query functions with Lazy sequences
                                                            ;    (a coll of patterns is given)
                                                            ; ======================================
                                                            ;
                                                            ;(defn idx_lazy
                                                            ;  "Determines the set of indices for patterns in the text
                                                            ;
                                                            ;   Input:
                                                            ;        word - the word to be searched in (required)
                                                            ;        col - collection of patterns (strings)
                                                            ;   Returns:
                                                            ;        a lazy sequence, for each pattern (in the corresponding index)
                                                            ;        where each element contains the set of indices (if found)
                                                            ;        and nil otherwise.
                                                            ;   "
                                                            ;  [word col]
                                                            ;  (lazy_query which_ind word col))
                                                            ;
                                                            ;
                                                            ;
                                                            ;(defn sub_lazy
                                                            ;  "Checks whether a collection of inputs are substrings of a word
                                                            ;
                                                            ;   Input:
                                                            ;        word - the word to be searched in (required)
                                                            ;        col - collection of (strings) patterns (required).
                                                            ;   Returns:
                                                            ;        a lazy sequence, for each pattern (in the corresponding index)
                                                            ;        returns true if it is a substring of word
                                                            ;        false otherwise
                                                            ;   "
                                                            ;  [word col]
                                                            ;  (lazy_query is_sub word col))
                                                            ;
                                                            ;
                                                            ;(defn longest_repeating_substring_lazy
                                                            ; "Finds the longest repeating substring for each item in the collection
                                                            ;
                                                            ;   Input:
                                                            ;        word - the word to be searched in (required)
                                                            ;        col - collection of (strings) patterns (required).
                                                            ;   
                                                            ;   Returns:
                                                            ;        The longest repeated substring for each pattern in col.
                                                            ;        If there's more than one substring that appears
                                                            ;        in the same amount of times in the string, returns
                                                            ;        the longest one among them.
                                                            ;        
                                                            ;        Note: if there is no repeating substring, returns
                                                            ;              the string itself
                                                            ;   "
                                                            ; [word col]
                                                            ;  (lazy_query longest_repeating_substring word col))
                                                            ;
                                                            ;(defn how_many_lazy
                                                            ;  "Checks how many times each pattern in collection appears
                                                            ;
                                                            ;   Input:
                                                            ;        word - the word to be searched in (required)
                                                            ;        col - collection of (strings) patterns (required).
                                                            ;   Returns:
                                                            ;        a lazy sequence, for each pattern (in the corresponding index)
                                                            ;        returns how many times it appears in the substring
                                                            ;   "
                                                            ;  [word col]
                                                            ;  (lazy_query how_many word col))
                                                            ;
                                                            ;
                                                            ;
                                                            ;
                                                            ; ======================================
                                                            ;        AOT demonstration
                                                            ;    Call Clojure functions from Java
                                                            ; ======================================
                                                            ;(defn -is_sub_java
                                                            ;  "Checks whether sub is a substring of a word 
                                                            ;
                                                            ;   Input:
                                                            ;        root - the suffix tree (required).
                                                            ;        sub - sub-word to be searched for
                                                            ;   Returns:
                                                            ;        true if sub is a substring of root
                                                            ;        false otherwise
                                                            ;   Note:
                                                            ;        this version is to designated to be called
                                                            ;        from java.
                                                            ;
                                                            ;   "
                                                            ;  [word sub]
                                                            ;  (is_sub (build_tree word) sub))
                                                            ;
                                                            ; ======================================
                                                            ;              Main
                                                            ; ======================================
                                                            ;
                                                            ;
                                                            ;(defn -main[]
                                                            ;  )
