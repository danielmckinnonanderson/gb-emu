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

(deftest test-set-flags
  (testing "Set flags based on arbitrary operations"
    (let [cpu (gb-emu.cpu/create-cpu)]
      ;; Assert flags are initially zero
      (is (= (get-register cpu :reg-f) 2r00000000))
      ;; Create an arbitrary operation -- in this case,
      )))

(deftest test-lookup-and-exec
  (testing "Lookup and execution of instruction from op-code byte"
    (testing "INC B"
      (let [cpu (gb-emu.cpu/create-cpu)]
        ;; Incrementing should wrap to zero
        (set-register cpu :reg-b 0xFF)
        (lookup-and-exec cpu 0x04)
        (is (= (:reg-b @cpu) 0x00))
        ;; Assert that the carry flag

        ;; Incrementing again is standard addition
        (lookup-and-exec cpu 0x04)
        (is (= (:reg-b @cpu) 0x01))))

    (testing "DEC B"
        (let [cpu (gb-emu.cpu/create-cpu)]
        ;; Decrementing should wrap to zero
        (set-register cpu :reg-b 0x00)
        (lookup-and-exec cpu 0x05)
        (is (= (:reg-b @cpu) 0xFF))

        ;; Decrementing again is standard subtraction
        (lookup-and-exec cpu 0x05)
        (is (= (:reg-b @cpu) 0xFE))))))
