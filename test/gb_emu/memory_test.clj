(ns gb-emu.memory-test
  (:require [clojure.test :refer :all]
            [gb-emu.memory :refer :all]))

(deftest test-cartridge-name
  (testing "Getting the cartridge name from a vector of bytes"))
