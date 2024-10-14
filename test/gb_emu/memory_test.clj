(ns gb-emu.memory-test
  (:require [clojure.test :refer :all]
            [gb-emu.memory :refer :all]))

(deftest test-create-wram
  (testing "Create WRAM"
    (let [wram (gb-emu.memory/create-wram)]
      (is (= 8192 (alength wram))))))