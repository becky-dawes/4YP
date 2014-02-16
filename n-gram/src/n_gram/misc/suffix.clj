(ns n-gram.misc.suffix
  (:gen-class
   :methods [#^{:static true} [is_sub_java [String String] Boolean]]
   :require clojure.inspector)
  )


; ======================================
;           Tree definition
; ======================================

"=====================
 =  Node definition  =
 =====================

 Node in the tree contains:

 1) next - hash-map of:
        key = <current char>
        value = rest of word, <node object>
    this map will allow us to traverse the tree
    in O(1) for each char read (when reading a pattern).

 2) idx_start - set of indices that correspond to the
                beginning of the pattern read so far,
                relative to the root

 3) cnt - size of idx_starts

"
(defrecord node [next idx_start cnt]) 

(defrecord suffix_node [children cnt])

(defn build_suffix_node [] (suffix_node. (hash-map) 1))
 

;(defrecord suffix_tree [root_node])
;
;(defn build_suffix_tree [] (suffix_tree. (build_suffix_node)))
;
;(defn build-suffix ([word] (build-suffix word (build_suffix_node)))
;  ([word node] 

;(defn search_tree [word root_node] (str (get-in root_node [:value]) (get-in root_node [:children :value])))

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

(defn search_tree [word node params] (let [children  (keys (get-in node params))  ;get keys of all children of the node
                                           results (apply merge (for [[k v] (zipmap children (map #(re-find (re-pattern (str "^" word)) %) children)) :when (not (nil? v))] {k v}))] ;find all non-nil matches for the word in the node's children
                                       (if (and (nil? results) (> (count word) 1)) (search_tree (subs word 0 (dec (count word))) node params) results))) ;if no matches are found, repeat with the last character of the word removed
  ;([original_word word node] [original_word (search_tree word node)]))

(defn is_exact_match [results word]  [((comp not nil?)(re-find (re-pattern (str "^" (key results) "$")) (val results))) ;does the matching pattern in the node's children exactly match a pattern in the beginning of the node?
                                      ((comp not nil?) (re-find (re-pattern (str "^" word "$")) (val results))) ;does the word exactly match the pattern in the node's children?
                                      (val results)]) ;the pattern found
                                

(defn add_children [root_node node children params] (cond (= (count children) 1)
                                                       (assoc-in root_node (into params [node :children (key (first children))]) (val (first children))) ;add the first child to the node
                                                       (> (count children) 1)
                                                       (assoc-in (add_children root_node node (rest children)) (into params [node :children (key (first children))]) (val (first children))) ;recursively call add_children with one less child
                                                       :else root_node))



(defn branch_node [root-node params old-node new-node] (let [children (assoc (get-in root-node (into params [old-node :children])) (subs old-node (count new-node) (count old-node)) (get-in root-node (into params [old-node]))) ;find all children of old node and associate them with the rest of the old node (i.e. child of new node)
                                                             new-root-node (dissoc-in root-node (into params [old-node]))] ;dissociate old node from root node
                                                         (add_children (assoc-in new-root-node (into params  [new-node]) (build_suffix_node)) new-node children params))) ;associate new node with root node and add all children
                                                                





(defn build_suffix_tree ([word] (build_suffix_tree word (build_suffix_node)))
  ([word root_node] (build_suffix_tree word root_node nil))
  ([word root_node params](let [root_node (cond (> 2 (count word)) root_node 
                                         (= 2 (count word)) (build_suffix_tree (str (reduce str (rest word))) root_node)
                                         :else (build_suffix_tree (reduce str (rest word)) root_node))
                                params (if (nil? params) [:children] params)
                                            match-results (search_tree word root_node params)] ;is there a match for the beginning of the word already in the root node's children?
                                                                (if (nil? match-results) (assoc-in root_node (into params [word]) (build_suffix_node)) ;if there is no match then add a new child
                                                                  (let [exact-match-results (is_exact_match (first match-results) word)] ;is there an exact match [(node exactly matches beginning of word) (node exactly matches whole word) (pattern matched)]
                                                                    (if  (first exact-match-results) ;if node exactly matches some length of the beginning of the word
                                                                      (if (second exact-match-results) (update-in root_node (into params [(last exact-match-results) :cnt]) inc) ;if whole word is exactly matched, update count of node
                                                     
                                                                        ;(let [params (if (nil? params) [:children (last exact-match-results) :children (subs word  (count (last exact-match-results)) (count word))]
                                                                                      ; (into params [:children (subs word  (count (last exact-match-results)) (count word))]))]
                                                                          (build_suffix_tree (subs word  (count (last exact-match-results)) (count word)) root_node
;                                                                                             (assoc-in root_node (into params [(last exact-match-results) :children (subs word  (count (last exact-match-results)) (count word))])
;                                                                                                       (build_suffix_node)) 
                                                                                             (into params [(last exact-match-results) :children]))) ;otherwise add a child to node with the end of the word
                                                                        (assoc-in (branch_node root_node params      (key (first match-results)) (val (first match-results)) )
                                                                                               (into params [ (last exact-match-results) :children 
                                                                                             (subs word  (count (last exact-match-results)) (count word))]) 
                                                                                  (build_suffix_node)))))))) ;if the beginning of the word matches the beginning of the node, branch the node and add the rest of the word as a child
;                          (= 2 (count word))
;                          
;                          (let [new_root_node (build_suffix_tree (str (reduce str (rest word))) root_node) match-results (search_tree word new_root_node)] 
;                                               (if (nil? match-results) (assoc-in new_root_node [:children word] (build_suffix_node)) 
;                                                 (let [exact-match-results (is_exact_match (first match-results) word)] 
;                                                   (if (first exact-match-results) 
;                                                     (if (second exact-match-results) (update-in new_root_node [:children (last exact-match-results) :cnt] inc)
;                                                     
;                                                       (assoc-in new_root_node [:children (last exact-match-results) :children 
;                                                                                (subs word (count (last exact-match-results)) (count word))] 
;                                                                 (build_suffix_node)))
;                                                       (assoc-in (branch_node new_root_node (key (first match-results)) (val (first match-results)) )
;                                                                              [:children (last exact-match-results) :children 
;                                                                            (subs word (count (last exact-match-results)) (count word))] 
;                                                                 (build_suffix_node))))))
;                          
;                          ;(assoc-in (build_suffix_tree (str (reduce str (rest word))) root_node)[:children word] (build_suffix_node))
;                          :else 
;                          
;                          (let [new_root_node (build_suffix_tree (reduce str (rest word)) root_node) match-results (search_tree word new_root_node)] 
;                                               (if (nil? match-results) (assoc-in new_root_node [:children word] (build_suffix_node)) 
;                                                 (let [exact-match-results (is_exact_match (first match-results) word)] 
;                                                   (if (first exact-match-results)
;                                                     (if (second exact-match-results) (update-in new_root_node [:children (last exact-match-results) :cnt] inc)
;                                                     
;                                                       (assoc-in new_root_node [:children (last exact-match-results) :children 
;                                                                                (subs word  (count (last exact-match-results)) (count word))] 
;                                                                 (build_suffix_node)))
;                                                       (assoc-in (branch_node new_root_node (key (first match-results)) (val (first match-results)) )
;                                                                              [:children (last exact-match-results) :children 
;                                                                            (subs word (count (last exact-match-results)) (count word))] 
;                                                                 (build_suffix_node)))))))))
                          
                          ;(assoc-in (build_suffix_tree (reduce str (rest word)) root_node)[:children word] (build_suffix_node)))))
 
 (defn loop-find-children [node] (let [child (first (keys (get node :children)))] (if ((comp not nil?) child) (str child (loop-find-children (get-in node [:children child]))))))
 
 (defn loop-through-nodes [node key children] (if (> (count children) 0) (loop-through-nodes (get-in node [:children (first (keys children))]) 
                                                                                             (get-in node [:children (first (keys children)) :children]))
                                                key))
 
; (defn 
;                                            
; (defn draw-tree [tree]                       

; ======================================
;      Tree construction functions
; ======================================

(defn build_node
  "Creates an empty node"
  []
  (let [next (hash-map)
        idx_start (hash-set)
        cnt 0]
    (node. next idx_start cnt)))


(defn build_suffix
  "Generates the sub-tree for the given suffix

   Input:
        word - the suffix word to generate suffix tree for
               (required)
        idx - index which indicates at which indices
              does the current path correspond to
              (required)
        node - node in which the tree will be rooted.
               If not given - creates an empty node and roots
               the new tree under this node.

   Returns:
        tree for the given suffix (of type node)
   "
  ([word idx] (build_suffix word (build_node) idx))
  ([word node idx]
    (if (empty? word)
      ; base case (finished reading a suffix)
        (update-in 
          (update-in node [:idx_start] conj idx)
            [:cnt] inc)

      ; induction (still left to read from suffix)
      (let [next_node (get-in node [:next (first word)] (build_node))]
        (assoc-in 
          (update-in 
            (update-in node [:idx_start] conj idx)
              [:cnt] inc)
          [:next (first word)] 
          (build_suffix (subs word 1) next_node idx))
        ))))

(defn build_tree
  "Generates the suffix tree for the given word

   Input:
        word - the word to generate suffix tree for.
               word must be a string. (required)
   Method:
        Constructs the tree by building each suffix
        and appending it to the root node of the tree
   Returns:
        Root node for the suffix tree
   "
  [word]
  (if (not= (string? word))
    (throw (Throwable. "Input must be a string!"))
  (loop [idx 0
         root (build_suffix word idx)
         rest_word (subs word 1)]
    (if (empty? rest_word)
      root
    (recur (inc idx)
           (build_suffix rest_word root (+ idx 1))
           (subs rest_word 1))))))


(defn print-subtree [subtree prefix]
  (if (empty? subtree)
    ""
    (apply str prefix (first subtree) "\n"
           (map #(print-subtree %1 (str "\t" prefix)) (rest subtree)))))
 
(defn print-tree [tree]
  (print-subtree tree ""))


; ======================================
;       Helper functions
; ======================================

(defn pred_mismatch
  "A predicate for validating the params to querys with collections

   Input:
        word - the word to be searched in (string). Required.
        col - collection of patterns (strings), Required.
   Method:
        Validates that word is a non-empty string,
        and col is a non-empty collection of strings.
   Returns:
        true if either of the params type mismatch
        and false otherwise
   "
  [word col]
  (or (or (not (string? word))
          (empty? word)
          (and (not= (list? col))
               (not= (vector? col))))
           (or (empty? col)
               (not-every? string? col))))


(defn lazy_query_real
  "Generates the lazy sequence queries
   
   This function is called after parameters were
   validated on 'lazy_query', and it returns the
   lazy sequence of the results.

  "
  [func col root]
  (if (empty? col)
    nil
  (cons (func root (first col))
        (lazy-seq (lazy_query_real func (rest col) root))))  
  )

(defn lazy_query
  "A predicate for validating the params to querys with collections

   Input:
        func - the function to be applied on each
               of the patterns
        word - the word to be searched in (string). Required.
        col - collection of patterns (strings), Required.

   Method:
        Invokes that word is a string,
        and col is a non-empty collection of strings.

   Returns:
        A lazy sequence of the patterns evaluated with
        the given func.

   Throws:
        Exception when parameters mismatch with the error msg.
   "
  
  [func word col]
  (if (pred_mismatch word col)
    (throw (Throwable. (str
      "\nInput must be:\n"
      "col - a non-empty collection of string patterns\n"
      "word - a non-empty string!\n")))
    (lazy_query_real func col (build_tree word)))
  )


(defn get_str_path
  "Returns the string formed by concat the path until base case

   Input:
        node - the current node that correspond to
               the path read so far from the root (required).
        cnt - number of suffixes containing the longest
              repeated pattern (required).
        word - the repeat pattern that was so far discovered
               (required).

   Method:
        Traverse in the tree as long as the number of
        suffixes found (in the current node) is the same
        as the number of suffixes of the longest repeated substring.

   Returns:
        The longest repeating substring from the child node
        of the root (supplied as node)

   Note: this function is TCO optimized
         (last line returns the result of the next call)

  "
  [node cnt word]
  (if (or (empty? (:next node))
          (> (count (vals (:next node))) 1)
          (< (:cnt (nth (vals (get node :next)) 0)) cnt))
    word
    ; concatenate myself & next call
    (let [ch_cur (nth (keys (get node :next)) 0)
          next_node (nth (vals (get node :next)) 0)]
         (get_str_path next_node cnt (str word ch_cur)))))


(defn get_max_child
  "Returns all the longest repeated strings from root's children

   Input:
        root - the root node of the suffix tree

   Method:
        Iterates over each of the root children,
        and for each child that has max cnt (most commonly repeated) 
        returns the 'longest repeated substring' that starts
        by traversing from the root node via that child.
   Returns:
        a collection of all the most commonly repeated patterns

   "
  [root]
  (let [max_cnt (reduce max (map :cnt (vals (:next root))))]
    (for [ch (keys (:next root))
          :when (= (get-in root [:next ch :cnt]) max_cnt)] 
      (str (get_str_path (get-in root [:next ch]) max_cnt ch)))))


(defn longer
  "Returns the longer string between s1 & s2"
  [s1 s2]
  (if (> (count s1) (count s2))
    s1 s2))



; ======================================
;    Query functions (single pattern)
; ======================================

(defn which_ind
  "Retrieves the set of indices where sub is located 
   (and nil otherwise).

   Input:
        root - the suffix tree (required).
        sub - sub-word to be searched for (required).
   Returns:
        the set of indices where sub is located,
        and nil otherwise
   "
  [root sub]
  (if (empty? sub)
    (get-in root [:idx_start])
    (if-let [next_node (get-in root [:next (first sub)])]
      (which_ind next_node (subs sub 1))
      nil)))


(defn is_sub
  "Checks whether sub is a substring of a word 

   Input:
        root - the suffix tree (required).
        sub - sub-word to be searched for (required).
   Returns:
        true if sub is a substring of root
        false otherwise
   "
  [root sub]
  (not= (which_ind root sub) nil))


(defn longest_repeating_substring
 "Finds the longest repeating substring 

   Input:
        word - the word to be searched in (required).
   
   Method:
       1. for all child's of root, find node with max count
       2. traverse the path from the root via the child-node with max
          count, and read letters on the path
       3. stop when:
           a. more than one child (more than one key in the map)
           b. child has lower counter than me 
              (that means the substring repeats twice)

   Returns:
        The longest repeated substring. 
        If there's more than one substring that appears
        in the same amount of times in the string, returns
        the longest one among them.
        
        Note: if there is no repeating substring, returns
              the string itself
   "
  ([_ word] 
    (longest_repeating_substring (build_tree word)))
  ([root]
    (reduce longer (get_max_child root))))


(defn how_many
  "Returns how many times a substring appears inside word 

   Input:
        root - the suffix tree (required).
        sub - sub-word to be searched for (required).

   "
  [root sub]
  (if (empty? sub)
    (:cnt root)
    (if-let [next_node (get-in root [:next (first sub)])]
      (how_many next_node (subs sub 1))
      0)))



; ======================================
;  Query functions with Lazy sequences
;    (a coll of patterns is given)
; ======================================

(defn idx_lazy
  "Determines the set of indices for patterns in the text

   Input:
        word - the word to be searched in (required)
        col - collection of patterns (strings)
   Returns:
        a lazy sequence, for each pattern (in the corresponding index)
        where each element contains the set of indices (if found)
        and nil otherwise.
   "
  [word col]
  (lazy_query which_ind word col))



(defn sub_lazy
  "Checks whether a collection of inputs are substrings of a word

   Input:
        word - the word to be searched in (required)
        col - collection of (strings) patterns (required).
   Returns:
        a lazy sequence, for each pattern (in the corresponding index)
        returns true if it is a substring of word
        false otherwise
   "
  [word col]
  (lazy_query is_sub word col))


(defn longest_repeating_substring_lazy
 "Finds the longest repeating substring for each item in the collection

   Input:
        word - the word to be searched in (required)
        col - collection of (strings) patterns (required).
   
   Returns:
        The longest repeated substring for each pattern in col.
        If there's more than one substring that appears
        in the same amount of times in the string, returns
        the longest one among them.
        
        Note: if there is no repeating substring, returns
              the string itself
   "
 [word col]
  (lazy_query longest_repeating_substring word col))

(defn how_many_lazy
  "Checks how many times each pattern in collection appears

   Input:
        word - the word to be searched in (required)
        col - collection of (strings) patterns (required).
   Returns:
        a lazy sequence, for each pattern (in the corresponding index)
        returns how many times it appears in the substring
   "
  [word col]
  (lazy_query how_many word col))




; ======================================
;        AOT demonstration
;    Call Clojure functions from Java
; ======================================
(defn -is_sub_java
  "Checks whether sub is a substring of a word 

   Input:
        root - the suffix tree (required).
        sub - sub-word to be searched for
   Returns:
        true if sub is a substring of root
        false otherwise
   Note:
        this version is to designated to be called
        from java.

   "
  [word sub]
  (is_sub (build_tree word) sub))

; ======================================
;              Main
; ======================================


(defn -main[]
  )
