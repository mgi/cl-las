(in-package :las)

;; Little-endian integers
(define-binary-type integer (bytes sign)
  (:reader (in)
           (loop with unsigned-value = 0
                 with bits-per-byte = 8
                 for low-bit from 0 to (* bits-per-byte (1- bytes)) by bits-per-byte
                 do (setf (ldb (byte bits-per-byte low-bit) unsigned-value) (read-byte in))
                 finally (let ((bits (* bits-per-byte bytes)))
                           (if (and sign (>= unsigned-value (ash 1 (1- bits))))
                               (return (- unsigned-value (ash 1 bits)))
                               (return unsigned-value)))))
  (:writer (out value)
           (loop with bits-per-byte = 8
                 with unsigned-value = (if (plusp value)
                                           value
                                           (- (ash 1 (* bits-per-byte bytes)) value))
                 for low-bit from 0 to (* bits-per-byte (1- bytes)) by bits-per-byte
                 do (write-byte (ldb (byte bits-per-byte low-bit) unsigned-value) out))))

(define-binary-type u1 () (integer :bytes 1))
(define-binary-type u2 () (integer :bytes 2))
(define-binary-type u4 () (integer :bytes 4))
(define-binary-type u8 () (integer :bytes 8))

(define-binary-type s1 () (integer :bytes 1 :sign t))
(define-binary-type s2 () (integer :bytes 2 :sign t))
(define-binary-type s4 () (integer :bytes 4 :sign t))
(define-binary-type s8 () (integer :bytes 8 :sign t))

;; Little-endian IEEE floats
(define-binary-type float (bytes)
  (:reader (in)
           (loop with value = 0
                 with bits-per-byte = 8
                 with decoder = (ecase bytes (4 #'decode-float32) (8 #'decode-float64))
                 for low-bit from 0 to (* bits-per-byte (1- bytes)) by bits-per-byte
                 do (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
                 finally (return (funcall decoder value))))
  (:writer (out value)
           (loop with bits-per-byte = 8
                 with encoded-value = (ecase bytes
                                        (4 (encode-float32 value))
                                        (8 (encode-float64 value)))
                 for low-bit from 0 to (* bits-per-byte (1- bytes)) by bits-per-byte
                 do (write-byte (ldb (byte bits-per-byte low-bit) encoded-value) out))))

(define-binary-type float4 () (float :bytes 4))
(define-binary-type float8 () (float :bytes 8))

;; Vectors
(define-binary-type vector (size type)
  (:reader (in)
           (loop with arr = (make-array size)
                 for i below size
                 do (setf (aref arr i) (read-value type in))
                 finally (return arr)))
  (:writer (out value)
           (loop for e across value
                 do (write-value type out e))))

;; Bitfields:
;;
;; Here is a bitfield stored in a 16bit value where the bit 0 means
;; "a" and bit 1 means "b":
;; (define-bitfield foo (u2)
;;   ((a 0) (b 1)))
(defmacro define-bitfield (name (type) &rest mapping)
  (with-gensyms (in out value symbol bit encval)
    `(define-binary-type ,name ()
       (:reader (,in)
                (let ((,value (read-value ',type ,in)))
                  (loop for (,symbol ,bit) in ',@mapping
                        when (ldb-test (byte 1 ,bit) ,value)
                          collect ,symbol)))
       (:writer (,out ,value)
                (write-value ',type ,out
                             (loop with ,encval = 0
                                   for (,symbol ,bit) in ',@mapping
                                   do (when (member ,symbol ,value)
                                        (setf (ldb (byte 1 ,bit) ,encval) 1))
                                   finally (return ,encval)))))))

;; Fix length null terminated ASCII strings. This codes should work
;; for 8bit character be it ASCII, ISO 8859 or UTF-8 sans extensions.
(define-binary-type 8bit-string (length terminator)
  (:reader (in)
           (let ((string (make-string length)))
             (dotimes (i length)
               (setf (char string i) (code-char (read-byte in))))
             (subseq string 0 (position terminator string :test #'char=))))
  (:writer (out string)
           (let* ((outstring (make-string length :initial-element terminator)))
             (loop for char across string
                   for i from 0
                   do (setf (char outstring i) (char string i)))
             (loop for char across outstring
                   do (write-byte (char-code char) out)))))
