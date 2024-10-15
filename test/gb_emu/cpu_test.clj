(ns gb-emu.cpu-test
  (:require [clojure.test :refer :all]
            [gb-emu.cpu :refer :all]))

(deftest test-create-cpu
  (testing "CPU initialization"
    (let [cpu @(gb-emu.cpu/create-cpu)]
      (is (contains? cpu :reg-a))
      (is (contains? cpu :reg-b))
      (is (contains? cpu :reg-c))
      (is (contains? cpu :reg-d))
      (is (contains? cpu :reg-e))
      (is (contains? cpu :reg-f))
      (is (contains? cpu :reg-h))
      (is (contains? cpu :reg-l))
      (is (contains? cpu :stack-ptr))
      (is (contains? cpu :pgm-ctr))
      (is (= 0x00 (:reg-a cpu)))
      (is (= 0x00 (:reg-b cpu)))
      (is (= 0x00 (:reg-c cpu)))
      (is (= 0x00 (:reg-d cpu)))
      (is (= 0x00 (:reg-e cpu)))
      (is (= 0x00 (:reg-f cpu)))
      (is (= 0x00 (:reg-h cpu)))
      (is (= 0x00 (:reg-l cpu)))
      (is (= 0x0000 (:stack-ptr cpu)))
      (is (= 0x0000 (:pgm-ctr cpu))))))

(deftest test-get-register
  (testing "Register arithmetic"
    (let [cpu (gb-emu.cpu/create-cpu)]
      (set-register cpu :reg-a 0x0A)
      (is (= 0x0A (get-register cpu :reg-a)))
      (set-register cpu :reg-b 0x0B)
      (is (= 0x0B (get-register cpu :reg-b)))
      (set-register cpu :reg-c 0x0C)
      (is (= 0x0C (get-register cpu :reg-c)))
      (set-register cpu :reg-d 0x0D)
      (is (= 0x0D (get-register cpu :reg-d)))
      (set-register cpu :reg-e 0x0E)
      (is (= 0x0E (get-register cpu :reg-e)))
      (set-register cpu :reg-f 0x0F)
      (is (= 0x0F (get-register cpu :reg-f))))))

(deftest test-opcode-to-op
  (testing "Decode operation from op-code"
    (is (= (name (get op-code-to-op 0x04))
           "inc-b"))
    (is (= (name (get op-code-to-op 0x05))
           "dec-b"))))

(deftest test-lookup-and-exec
  (testing "Lookup and execution of instruction from op-code byte"
    (let [cpu (gb-emu.cpu/create-cpu)]
      ;; All registers should be initialized to zero

      ;; TODO - when these opcodes have handlers
      (lookup-and-exec cpu 0x00)
      (lookup-and-exec cpu 0x01)
      (lookup-and-exec cpu 0x02)
      (lookup-and-exec cpu 0x03)

      ;; FIXME
      ;; Should be "INC B"
      (lookup-and-exec cpu 0x04)
      (is (= (get-register cpu :reg-b) 1))
      
      ;; FIXME
      ;; Should be "DEC B"
      (println cpu)
      (lookup-and-exec cpu 0x05)
      (println cpu)
      (is (= (get-register cpu :reg-b) 0)))))
