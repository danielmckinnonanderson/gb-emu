(ns gb-emu.memory-test
  (:require [clojure.test :refer :all]
            [gb-emu.memory :refer :all]))

(deftest test-cartridge-name
  (testing "Getting the cartridge name from a vector of bytes"
    (let [name-bytes [0x50 0x4F 0x4b 0x45 0x4D 0x4F 0x4E 0x20 0x52 0x45 0x44 0x00 0x00 0x00 0x00]
          start-idx 0x134
          rom-bytes (reduce (fn [acc [i v]]
                              (assoc acc (+ start-idx i) v))
                            (vec (repeat 10000 0))
                            (map-indexed vector name-bytes))]
      (is (= (cartridge-name rom-bytes) "POKEMON RED")))))

(deftest test-mmap-cartridge
  (testing "Memory-mapping data from the cartridge into the virtual address bus"
    ;; Set up the test input data
    (let [rom0-sz (region-size rom0-start rom0-end)
          rom1-sz (region-size rom1-start rom1-end)
          rom0-data (vec (repeat rom0-sz 0x0A))
          rom1-data (vec (repeat rom1-sz 0x0B))
          rom-bytes (into rom0-data rom1-data)
          ;; Set up the memory map that will be our test output
          memory (create-memory-map)]
      ;; First verify that memory is filled with zeroes
      (is (every? #(= 0x00 %) (:rom-0 @memory)))
      (is (every? #(= 0x00 %) (:rom-1 @memory)))
      ;; Then mmap the cart
      (mmap-cartridge rom-bytes memory)

      ;; Now, rom-0 should be filled with A's
      (is (every? #(= 0x0A %) (:rom-0 @memory)))
      ;; and rom-1 should be filled with B's
      (is (every? #(= 0x0B %) (:rom-1 @memory))))))
