(ns alphabet-cipher.coder)

;; Questions
;; How to represent the cipher square (apparently can just list the chars in a vec)
;; How to generate the cipher square (paste it in? generate it programatically?)
;; How to juxtapose keyword to message

;; repeat keyword N times to match message

(def alphabet (into [] (map char (range 97 (+ 97 26)))))

(defn index-of [element vector] (.indexOf vector element))

(defn encode-letter [direction [letter cipher-rotation]]
  (let [a (index-of letter alphabet)
        b (index-of cipher-rotation alphabet)]
    (get alphabet (mod (direction a b) 26))))

(defn encode-msg [dir]
  (fn [keyword message]
    (apply str (map (partial encode-letter dir)
                  (map vector message (cycle keyword))))))

(def encode (encode-msg +))
(def decode (encode-msg -))

(defn crack-pair [plain-char cipher-char]
  (let [plain (index-of plain-char alphabet)
        cipher (index-of cipher-char alphabet)]
    (first (filter (fn [key-index] (= (mod (+ plain key-index) 26)
                                      cipher))
                   (range 0 26)))))

(defn word-boundaries-align? [word key-seq]
  (= (seq key-seq)
     (take (count key-seq)
           (cycle word))))

(defn keyword-from-key-sequence [key-seq]
  (loop [key-seq key-seq
         kw []]
    (if (and (= (first key-seq) (first kw))
             (word-boundaries-align? kw key-seq))
      (apply str kw)
      (recur (rest key-seq)
             (conj kw (first key-seq))))))

(defn decipher [cipher message]
  (keyword-from-key-sequence
   (map alphabet (map crack-pair message cipher))))

;; scones
;; meetme
;; s: 18
;; m: 12
;; result: e
;; e: 4

;;    ABCDEFGHIJKLMNOPQRSTUVWXYZ
;;  A abcdefghijklmnopqrstuvwxyz A
;;  B bcdefghijklmnopqrstuvwxyza B
;;  C cdefghijklmnopqrstuvwxyzab C
;;  D defghijklmnopqrstuvwxyzabc D
;;  E efghijklmnopqrstuvwxyzabcd E
;;  F fghijklmnopqrstuvwxyzabcde F
;;  G ghijklmnopqrstuvwxyzabcdef G
;;  H hijklmnopqrstuvwxyzabcdefg H
;;  I ijklmnopqrstuvwxyzabcdefgh I
;;  J jklmnopqrstuvwxyzabcdefghi J
;;  K klmnopqrstuvwxyzabcdefghij K
;;  L lmnopqrstuvwxyzabcdefghijk L
;;  M mnopqrstuvwxyzabcdefghijkl M
;;  N nopqrstuvwxyzabcdefghijklm N
;;  O opqrstuvwxyzabcdefghijklmn O
;;  P pqrstuvwxyzabcdefghijklmno P
;;  Q qrstuvwxyzabcdefghijklmnop Q
;;  R rstuvwxyzabcdefghijklmnopq R
;;  S stuvwxyzabcdefghijklmnopqr S
;;  T tuvwxyzabcdefghijklmnopqrs T
;;  U uvwxyzabcdefghijklmnopqrst U
;;  V vwxyzabcdefghijklmnopqrstu V
;;  W wxyzabcdefghijklmnopqrstuv W
;;  X xyzabcdefghijklmnopqrstuvw X
;;  Y yzabcdefghijklmnopqrstuvwx Y
;;  Z zabcdefghijklmnopqrstuvwxy Z
;;    ABCDEFGHIJKLMNOPQRSTUVWXYZ
