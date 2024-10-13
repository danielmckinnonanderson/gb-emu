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
  (testing "Get register values"
    (let [cpu (gb-emu.cpu/create-cpu)]
      (swap! cpu assoc :reg-a 0x0A)
      (is (= 0x0A (get-register cpu :reg-a)))
      (swap! cpu assoc :reg-b 0x0B)
      (is (= 0x0B (get-register cpu :reg-b)))
      (swap! cpu assoc :reg-c 0x0C)
      (is (= 0x0C (get-register cpu :reg-c)))
      (swap! cpu assoc :reg-d 0x0D)
      (is (= 0x0D (get-register cpu :reg-d)))
      (swap! cpu assoc :reg-e 0x0E)
      (is (= 0x0E (get-register cpu :reg-e)))
      (swap! cpu assoc :reg-f 0x0F)
      (is (= 0x0F (get-register cpu :reg-f))))))