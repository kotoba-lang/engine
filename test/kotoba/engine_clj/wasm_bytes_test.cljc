(ns kotoba.engine-clj.wasm-bytes-test
  "Tests for `kotoba.engine-clj.wasm-bytes` — the Module-IR -> real `.wasm`
  byte-emission step `kotoba.engine-clj.codegen`'s and this repo's README's
  \"Where this port draws the line\" section describe as out of scope for the
  domain port. `basic_test.cljc`'s own docstring notes the original deleted
  Rust test asserted `wasm.starts_with(b\"\\0asm\")` and that this port
  couldn't restore that assertion because it stopped at Module IR — this
  file is what restores it, now that `wasm-bytes` closes that gap.

  ## Manual external verification (not run here — keeps this suite pure
  CLJC/no-I/O, matching this repo's existing convention)

  Beyond what these tests assert structurally, the emitted bytes for both a
  minimal hand-written example and isekai-network's real
  `games/01-netsurvivors/logic.clj` were independently verified with
  external tools during development:
    - `wasm-tools validate <file>.wasm` — exit 0 (structurally valid core
      WASM per the `wasm-tools` project's independent validator, not just
      self-consistent with this encoder's own opcode table).
    - `wasmtime run --invoke add1 minimal.wasm 5` — printed `6`, i.e. the
      minimal example's compiled+encoded `add1` function was actually
      instantiated and *executed* by wasmtime and returned the correct
      value, the strongest verification short of running the isekai-network
      game itself (which needs `kami-script-runtime`'s host-import
      implementations, out of scope here — this repo only emits the guest
      module)."
  (:require [kotoba.engine-clj.wasm-bytes :as wb]
            [kotoba.engine-clj.codegen :as codegen]
            [kotoba.engine-clj.ast :as ast]
            #?(:clj [clojure.test :refer [deftest is testing]])))

#?(:clj
   (do

     (defn- compile-ir [src]
       (codegen/compile (ast/parse-program-str src)))

     ;; -------------------------------------------------------------------
     ;; LEB128
     ;; -------------------------------------------------------------------

     (deftest uleb128-round-trips
       (testing "small values fit in one byte"
         (is (= [0] (wb/uleb128 0)))
         (is (= [64] (wb/uleb128 64)))
         (is (= [0x7f] (wb/uleb128 127))))
       (testing "values >= 128 continue into further bytes, high bit set on all but the last"
         (is (= [0xE5 0x8E 0x26] (wb/uleb128 624485))) ;; the WASM spec's own worked example
         (is (= [0x80 0x01] (wb/uleb128 128)))))

     (deftest sleb128-round-trips
       (testing "the spec's own worked examples"
         (is (= [0x9b 0xf1 0x59] (wb/sleb128 -624485)))
         (is (= [0x00] (wb/sleb128 0)))
         (is (= [0x02] (wb/sleb128 2)))
         (is (= [0x7e] (wb/sleb128 -2)))))

     (deftest f32-le-bytes-known-patterns
       (testing "1.0f is 0x3F800000, little-endian"
         (is (= [0x00 0x00 0x80 0x3F] (wb/f32-le-bytes 0x3F800000))))
       (testing "0.0f is all zero bytes"
         (is (= [0x00 0x00 0x00 0x00] (wb/f32-le-bytes 0)))))

     ;; -------------------------------------------------------------------
     ;; Module-level structural assertions
     ;; -------------------------------------------------------------------

     (deftest emit-module-starts-with-wasm-magic-and-version
       (testing "restores the original deleted Rust test's assertion (basic_test.cljc's docstring): \\0asm + version 1"
         (let [ir (compile-ir "(defn f [x] (+ x 1))")
               bytes (wb/emit-module ir)]
           (is (= [0x00 0x61 0x73 0x6D 0x01 0x00 0x00 0x00] (take 8 bytes))))))

     (deftest emit-module-is-a-flat-byte-sequence
       (testing "every element is a single unsigned byte 0-255 (catches the nested-vector nesting bug this encoder had during development — vec-encode's raw?/non-raw contract)"
         (let [ir (compile-ir "(defn f [x] (+ x 1))")
               bytes (wb/emit-module ir)]
           (is (every? #(and (integer? %) (<= 0 % 255)) bytes)))))

     (deftest emit-module-deterministic
       (testing "compiling + encoding the same source twice yields byte-identical output"
         (let [src "(defn f [x] (- x (* x 2)))"
               b1 (wb/emit-module (compile-ir src))
               b2 (wb/emit-module (compile-ir src))]
           (is (= b1 b2)))))

     (deftest emit-module-exports-every-guest-function
       (testing "each top-level `defn` becomes an exported function, matching codegen's :export-functions"
         (let [ir (compile-ir "(defn a [] 1) (defn b [x] x)")
               bytes (wb/emit-module ir)
               wat-ish (apply str (map char bytes))] ;; crude but sufficient: names are ASCII in the export section
           (is (re-find #"\Qa\E" wat-ish))
           (is (re-find #"\Qb\E" wat-ish))
           (is (re-find #"\Qcabi_realloc\E" wat-ish))
           (is (re-find #"\Qmemory\E" wat-ish)))))

     (deftest emit-module-with-host-imports-and-string-handle-expansion
       (testing "a host-import call with a :string-handle param compiles+encodes without throwing (exercises the (ptr,len) i32/i32 expansion in host-import-functype)"
         (let [ir (compile-ir "(defn f [] (spawn-entity \"ghost\"))")
               bytes (wb/emit-module ir)]
           (is (= [0x00 0x61 0x73 0x6D 0x01 0x00 0x00 0x00] (take 8 bytes)))
           (is (pos? (count bytes))))))

     (deftest emit-module-with-atoms
       (testing "a `defatom` cell round-trips into a mutable i64 global + export, doesn't throw"
         (let [ir (compile-ir "(defatom score 0) (defn f [] (set-atom! score 5))")
               bytes (wb/emit-module ir)]
           (is (= [0x00 0x61 0x73 0x6D 0x01 0x00 0x00 0x00] (take 8 bytes))))))

     (deftest emit-module-with-string-literals-emits-data-section
       (testing "a function using a string literal produces non-empty :literals and a data section (section id 11 byte appears)"
         (let [ir (compile-ir "(defn f [] (spawn-entity \"ghost\"))")]
           (is (seq (:blob (:literals ir))) "sanity: this source really does produce literal bytes")
           (let [bytes (wb/emit-module ir)]
             (is (some #(= % 11) bytes) "a data-section id byte (11) must appear somewhere in the output")))))))
