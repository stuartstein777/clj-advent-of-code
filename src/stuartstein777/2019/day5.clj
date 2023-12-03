(ns stuartstein777.2019.day5)

;; opcode 3 a, b
;;          a = integer
;;          b = location to save it
;; saves a to location b

;; opcode 4 a
;;          a = address
;; outputs value at address a

;; parameter mode 0 - position mode
;; parameter = 50, value is stored at position 50

;; parameter mode 1 - immediate mode
;; parameter = 50, value is 50

;; opcode 2 digit number
;; 1s and 10s digit
;; rightmost 2 digits
;; parameter modes are in the 100s digit and 1000s digit, 10,000s digit so on

;;    
;;  1002,3,4,33
;; ABCDE 