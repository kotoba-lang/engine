(ns kotoba.engine-clj.numerics
  "Portable f32 bit-pattern <-> i64 conversions and UTF-8 byte encoding.

  `kami-engine-clj`'s all-i64 ABI stores every f32 value as its IEEE-754
  bit-pattern zero-extended into an i64 (see `ast.rs`'s module docstring).
  Rust gets this for free from `f32::to_bits`/`f32::from_bits` plus an
  `as i64` cast that zero-extends because the source is unsigned (`u32`).
  These two helpers reproduce exactly that: the JVM/JS numeric primitives
  underneath (`Float/floatToRawIntBits`, `DataView`) are 2's-complement
  32-bit *signed* ints, so both branches explicitly mask to the low 32 bits
  to zero-extend rather than sign-extend.")

(defn f32-bits
  "The IEEE-754 bit-pattern of `x` (coerced to f32 precision) as a
  zero-extended i64 — mirrors `f32::to_bits(x) as i64` (ast.rs / codegen.rs)."
  [x]
  #?(:clj (bit-and (long (Float/floatToRawIntBits (float x))) 0xFFFFFFFF)
     :cljs (let [buf (js/ArrayBuffer. 4) view (js/DataView. buf)]
             (.setFloat32 view 0 x true)
             (.getUint32 view 0 true))))

(defn bits->f32
  "Inverse of `f32-bits` — reinterpret the low 32 bits of an i64 as f32.
  Mirrors `f32::from_bits(bits as u32)`. Used only by the reference
  interpreter (`kotoba.engine-clj.interp`), which — unlike the compiler
  itself — needs to actually evaluate f32 arithmetic to a host float."
  [bits]
  #?(:clj (Float/intBitsToFloat (unchecked-int bits))
     :cljs (let [buf (js/ArrayBuffer. 4) view (js/DataView. buf)]
             (.setUint32 view 0 (bit-and bits 0xFFFFFFFF) true)
             (.getFloat32 view 0 true))))

(defn utf8-bytes
  "UTF-8 bytes of `s` as a vector of unsigned ints 0-255 — mirrors Rust's
  `s.into_bytes(): Vec<u8>`."
  [s]
  #?(:clj (into [] (map #(bit-and (long %) 0xff)) (.getBytes ^String s "UTF-8"))
     :cljs (vec (.encode (js/TextEncoder.) s))))
