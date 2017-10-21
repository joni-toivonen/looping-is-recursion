(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [accumulator base exp]
                 (if (zero? exp)
                   accumulator
                   (recur (* accumulator base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq counter]
                 (if (or (== counter 0)
                         (== counter 1))
                   (first a-seq)
                   (recur (rest a-seq) (dec counter))))]
    (helper a-seq (count a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1)
         (empty? seq2)) true
    (or (empty? seq1)
        (empty? seq2)) false
    (== (first seq1)
        (first seq2)) (recur (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [seq-1 a-seq
         predicate pred
         acc 0]
    (cond
      (empty? seq-1) nil
      (pred (first seq-1)) acc
      :else (recur (rest seq-1) predicate (inc acc)))))

(defn avg [a-seq]
  (loop [seq-1 a-seq
         length (count a-seq)
         sum 0]
    (if (empty? seq-1)
      (/ sum length)
      (recur (rest seq-1)
             length
             (+ sum (first seq-1))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [seq1 a-seq
         odd-set #{}]
    (if (empty? seq1)
      odd-set
      (recur (rest seq1)
             (toggle odd-set (first seq1))))))

(defn fast-fibo [n]
  (loop [fibonacci 1
         fibonacci-old 0
         accumulator n]
    (cond
      (= accumulator 0) 0
      (= accumulator 1) fibonacci
      :else
      (recur (+ fibonacci fibonacci-old) fibonacci (dec accumulator)))))

(defn cut-at-repetition [a-seq]
  (loop [seq1 a-seq
         cut-vector []]
    (cond
      (empty? seq1) cut-vector
      (contains? (set cut-vector) (first seq1)) cut-vector
      :else
      (recur (rest seq1) (conj cut-vector (first seq1))))))

