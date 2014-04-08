(ns n-gram.misc.suffix
  (:gen-class
    :methods [#^{:static true} [is_sub_java [String String] Boolean]]
    :require clojure.inspector)
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
  [root-node params old-node new-node tree-text] (let [old-node-indices (create-indices-memo tree-text old-node) new-node-indices (create-indices-memo tree-text new-node) 
                                                       new-sub-node-indices (create-indices-memo tree-text (subs old-node (count new-node) (count old-node)))
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

(defrecord restaurant_node [range children restaurant]); restaurant with values [no. tables, no. customers]

(defn build_restaurant_node "Builds a new restaurant node object
[] node with empty range, children and restaurant
[range] node with the given range but empty children and restaurant
[range letter] node with the given range, empty children and one table with one customer for the given letter in the restaurant
[range children restaurant] node with the given range, children and restaurant" 
  ([] (restaurant_node. [] (hash-map) (hash-map)))
  ([range] (restaurant_node. range (hash-map) (hash-map)))
  ([range letter] (restaurant_node. range (hash-map) {letter [1 1]}))
  ([range children restaurant](restaurant_node. range children restaurant)))

(defn loop_find_tables "Cycles through all the children of the node to find the total number of tables for the given letter"
  [children node letter params] (let [result (get-in node (into params [:children (first children) :restaurant letter]))
             tables (if (nil? result) [0 0] result)] 
         (if (>= 1 (count children)) (first tables)
           (+ (first tables) (loop_find_tables (rest children) node letter params)))))

(def loop_find_tables_memo "Memoized loop_find_tables" (memoize loop_find_tables))

(defn find_child_table_count "Returns the total count of all tables for the given letter in the node's children"
  [node letter params] (let [children (keys (get-in node (into params [:children]))) result (loop_find_tables_memo children node letter params)]
                        (if (nil? result) 0 result)))

(def find_child_table_count_memo "Memoized find_child_table_count" (memoize find_child_table_count))

(defn check_table_customer_consistency "Checks that number of tables for the given letter in the node's children is equal to the number of customers for the letter in the node's restaurant"
  [node letter params](let [table_count (find_child_table_count_memo node letter params) 
                           params (into params [:restaurant letter])
                           parent_restaurant (if (nil? (get-in node params)) [0 0] (get-in node params))]
                       (if (and (not (zero? table_count)) (not (= table_count (second parent_restaurant)) ))
                         (if (<= (first parent_restaurant) table_count) 
                           (if (zero? (first parent_restaurant))
                             (assoc-in node params [1 table_count])
                             (assoc-in node params [(first parent_restaurant) table_count]))
                           (assoc-in node params [table_count table_count]))
                         node)))

(def check_table_customer_consistency_memo "Memoized check_table_customer_consistency" (memoize check_table_customer_consistency))

(defn check_for_children "Checks if there are any children starting with the given letter"
  [letter children] (if (< 0 (count children)) 
                     (if (= (first children) letter) true (check_for_children letter (rest children))) false))

(def check_for_children_memo "Memoize check_for_children" (memoize check_for_children))

(defn check_range "Compares the suffix to the range on the child node and returns a vector with the values [(matching string) (is the match length equal to the range length) (is the suffix longer than the match)]"
  ([range suffix root_word] (let [range_letters (dereference-indices-memo root_word range) match (re-find (re-pattern (str "^" range_letters)) suffix)] 
                              (if (nil? match) 
                                (check_range (create-indices-memo root_word (subs range_letters 0 (dec (count range_letters)))) suffix root_word (count range_letters))
                                [match (= (count range_letters) (count match)) (> (count suffix) (count match))])))
  ([range suffix root_word length] (let [range_letters (dereference-indices-memo root_word range) match (re-find (re-pattern (str "^" range_letters)) suffix)] 
                                     (if (nil? match) 
                                       (check_range (create-indices-memo root_word (subs range_letters 0 (dec (count range_letters)))) suffix root_word length)
                                       [match (= length (count match)) (> (count suffix) (count match))]))))

(def check_range_memo "Memoized check_range" (memoize check_range))

(defn check_consistency_all_tables "Checks that the number of tables in all children is equal to the number of customers in the node's restaurant for all letters"
  [node tables params] (if (<= 1 (count tables)) 
                        (check_table_customer_consistency_memo node (first tables) params)
                        (check_consistency_all_tables (check_table_customer_consistency_memo node (first tables) params) (rest tables) params)))

(def check_consistency_all_tables_memo "Memoized check_consistency_all_tables" (memoize check_consistency_all_tables))

(defn branch "Replaces the node with another with range new_range and children a copy of the old node but with the remaining range"
  [node letter new_range new_letter params] (let [old_range (get-in node (into params [:children letter :range])) 
                                                 restaurant (get-in node (into params [:children letter :restaurant])) 
                                                 children (get-in node (into params [:children letter :children]))]
                                             (check_consistency_all_tables_memo
                                               (assoc-in 
                                                 (assoc-in 
                                                   (dissoc-in node (into params [:children letter]))
                                                   (into params [:children letter]) (build_restaurant_node new_range))
                                                 (into params [:children letter :children new_letter]) (build_restaurant_node [(+ (first old_range) (- (last new_range) (first new_range))) (last old_range)] children restaurant))
                                               (keys restaurant) (into params [:children letter]))))

(def branch_memo "Memoized branch" (memoize branch))

(defn build_tree "Builds a suffix tree for the given word"
  ([word] (build_tree word (build_restaurant_node) word []) )
  ([word root_node root_word params] (let [root_node (if (empty? params) (cond (> 2 (count word)) root_node 
                                                                               (= 2 (count word)) (build_tree (str (reduce str (rest word))) root_node root_word params)
                                                                               :else (build_tree (reduce str (rest word)) root_node root_word params))
                                                       root_node) 
                                           suffix (cond (< 2 (count word)) (reduce str (rest word))
                                                        (= 2 (count word)) (str (reduce str (rest word)))
                                                        :else "")
                                           suffix_start (str (first suffix))
                                           letter (str (first word))
                                           children (get-in root_node (into params [:children]))]
                                       (if (empty? children) 
                                         (check_table_customer_consistency_memo
                                           (assoc-in root_node (into params [:children suffix_start]) 
                                                     (build_restaurant_node (create-indices-memo root_word suffix) letter) 
                                                     )
                                           letter params)
                                         (if (check_for_children_memo suffix_start (keys children))
                                           (let [match_results (check_range_memo (get-in root_node (into params [:children suffix_start :range])) suffix root_word)]
                                             (if (second match_results)
                                               (if (nth match_results 2)
                                                 (if (empty? (get-in root_node [:children suffix_start :children]))
                                                   (check_table_customer_consistency_memo 
                                                     (check_table_customer_consistency_memo
                                                       (assoc-in
                                                         (build_tree (str letter (subs suffix (count (first match_results)) (count suffix))) root_node root_word (into params [:children suffix_start]))
                                                         [:children suffix_start :children ""] (build_restaurant_node [0 0] (get-in root_node [:children suffix_start :children]) (get-in root_node [:children suffix_start :restaurant])))
                                                       letter (into params [:children suffix_start]))
                                                     letter params)
                                                   (check_table_customer_consistency_memo
                                                     (check_table_customer_consistency_memo  
                                                       (build_tree (str letter (subs suffix (count (first match_results)) (count suffix))) root_node root_word (into params [:children suffix_start]))
                                                       letter (into params [:children suffix_start]))
                                                     letter params))
                                                 (check_table_customer_consistency_memo root_node letter params))
                                               (let [new-range (create-indices-memo root_word (first match_results)) matching_branch_range (get-in root_node (into params [:children suffix_start :range]))
                                                     new_branch (dereference-indices-memo root_word new-range)]
                                                 (check_table_customer_consistency_memo 
                                                   (check_table_customer_consistency_memo
                                                     (assoc-in 
                                                       (branch_memo root_node suffix_start new-range 
                                                               (subs (dereference-indices-memo root_word matching_branch_range) (count (first match_results)) (inc (count (first match_results)))) params)
                                                       (into params [:children suffix_start :children (subs suffix (count (first match_results)) (inc (count (first match_results))))]) 
                                                       (build_restaurant_node (create-indices-memo root_word (subs suffix (count (first match_results)) (count suffix))) letter))
                                                     letter (into params [:children suffix_start]))
                                                   letter params)))
                                             )
                                           (check_table_customer_consistency_memo
                                             (assoc-in root_node (into params [:children suffix_start]) (build_restaurant_node (create-indices-memo root_word suffix) letter))
                                             letter params))))))

(def build_tree_memo "Memoized build_tree" (memoize build_tree))

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