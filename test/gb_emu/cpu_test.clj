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
      (is (= 0x13 (:reg-c cpu)))
      (is (= 0x00 (:reg-d cpu)))
      (is (= 0xD8 (:reg-e cpu)))
      (is (= 2r00000000 (:reg-f cpu)))
      (is (= 0x01 (:reg-h cpu)))
      (is (= 0x4D (:reg-l cpu)))
      (is (= 0xFFFE (:stack-ptr cpu)))
      (is (= 0x0100 (:pgm-ctr cpu))))))

(deftest test-get-set-register
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

(deftest test-ld-r-r'
  (testing "Loading 8-bit register into other 8-bit register"
    (let [cpu (gb-emu.cpu/create-cpu)]
      (set-register cpu :reg-a 0x0A)
      (set-register cpu :reg-e 0x0E)
      (ld-r-r' cpu :reg-a :reg-e)
      (is (= (get-register cpu :reg-a) (get-register cpu :reg-e))))))

(deftest test-ld-r-n
  (testing "Loading 8-bit register with the immediate data n"
    (let [cpu (gb-emu.cpu/create-cpu)]
      (set-register cpu :reg-a 0x0A)
      (ld-r-n cpu :reg-a 0xFF)
      (is (= (get-register cpu :reg-a) 0xFF)))))
