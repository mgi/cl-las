(in-package :las)

(setf binary-types:*endian* :little-endian)

(define-null-terminated-string 32-char-string 32)
(define-unsigned float64 8)
(define-unsigned float32 4)

(define-binary-struct number-of-points-by-return ()
  (npbr-0 0 :binary-type u32)
  (npbr-1 0 :binary-type u32)
  (npbr-2 0 :binary-type u32)
  (npbr-3 0 :binary-type u32)
  (npbr-4 0 :binary-type u32))

(defun number-of-points-by-return (header)
  (with-slots (number-of-points-by-return) header
    (mapcar #'(lambda (slot-name)
                (slot-value number-of-points-by-return slot-name))
            (binary-record-slot-names 'number-of-points-by-return))))

(define-binary-struct project-id-4 ()
  (pid4-0 0 :binary-type u8)
  (pid4-1 0 :binary-type u8)
  (pid4-2 0 :binary-type u8)
  (pid4-3 0 :binary-type u8)
  (pid4-4 0 :binary-type u8)
  (pid4-5 0 :binary-type u8)
  (pid4-6 0 :binary-type u8)
  (pid4-7 0 :binary-type u8))

(defun project-id-4 (header)
  (with-slots (project-id-4) header
    (mapcar #'(lambda (slot-name)
                (slot-value project-id-4 slot-name))
            (binary-record-slot-names 'project-id-4))))

(define-binary-class public-header ()
  ((file-signature :binary-type (define-binary-string file-signature 4) :initform "LASF")
   (file-source-id :binary-type u16 :initform 0)
   (global-encoding :binary-type (define-bitfield global-encoding (u16)
                                   (((:bits)
                                     gps-time 0
                                     internal-wave-data 1
                                     external-wave-data 2
                                     synthetic-return-point 3)))
                    :initform '())
   (project-id-1 :binary-type u32 :initform 0)
   (project-id-2 :binary-type u16 :initform 0)
   (project-id-3 :binary-type u16 :initform 0)
   (project-id-4 :binary-type project-id-4
                 :initform (make-project-id-4))
   (version-major :binary-type u8 :initform 0)
   (version-minor :binary-type u8 :initform 0)
   (system-identifier :binary-type 32-char-string :initform "")
   (generating-software :binary-type 32-char-string :initform "")
   (file-creation-doy :binary-type u16 :initform 0)
   (file-creation-year :binary-type u16 :initform 0)
   (header-size :binary-type u16 :initform 0)
   (offset-to-point-data :binary-type u32 :initform 0 :reader offset-to-point-data)
   (number-of-variable-length-records :binary-type u32 :initform 0
                                      :accessor number-of-variable-length-records)
   (point-data-format-id :binary-type u8 :initform 0 :reader point-data-format-id)
   (point-data-record-length :binary-type u16 :initform 0)
   (number-of-point-records :binary-type u32 :initform 0)
   (number-of-points-by-return :binary-type number-of-points-by-return
                              :initform (make-number-of-points-by-return))
   (x-scale :binary-type float64 :initform 0 :accessor x-scale)
   (y-scale :binary-type float64 :initform 0 :accessor y-scale)
   (z-scale :binary-type float64 :initform 0 :accessor z-scale)
   (x-offset :binary-type float64 :initform 0 :accessor x-offset)
   (y-offset :binary-type float64 :initform 0 :accessor y-offset)
   (z-offset :binary-type float64 :initform 0 :accessor z-offset)
   (max-x :binary-type float64 :initform 0 :accessor max-x)
   (min-x :binary-type float64 :initform 0 :accessor min-x)
   (max-y :binary-type float64 :initform 0 :accessor max-y)
   (min-y :binary-type float64 :initform 0 :accessor min-y)
   (max-z :binary-type float64 :initform 0 :accessor max-z)
   (min-z :binary-type float64 :initform 0 :accessor min-z)
   (start-of-waveform-data-packet :binary-type u64 :initform 0)))

(defmacro def-float64-accessor (class slot)
  (let ((object (gensym))
        (val (gensym)))
    `(progn
       (defmethod ,slot ((,object ,class))
         (decode-float64 (slot-value ,object ',slot)))
       (defmethod (setf ,slot) (,val (,object ,class))
         (setf (slot-value ,object ',slot) (encode-float64 ,val))))))

(def-float64-accessor public-header x-scale)
(def-float64-accessor public-header y-scale)
(def-float64-accessor public-header z-scale)
(def-float64-accessor public-header x-offset)
(def-float64-accessor public-header y-offset)
(def-float64-accessor public-header z-offset)
(def-float64-accessor public-header max-x)
(def-float64-accessor public-header max-y)
(def-float64-accessor public-header max-z)
(def-float64-accessor public-header min-x)
(def-float64-accessor public-header min-y)
(def-float64-accessor public-header min-z)

(defun scaled-min-max-z (header)
  (with-accessors ((min min-z)
                   (max max-z)
                   (zscale z-scale)
                   (zoff z-offset)) header
    (values (+ zoff (* zscale min))
            (+ zoff (* zscale max)))))

(define-binary-class variable-length-record ()
  ((reserved :binary-type u16 :initform 0)
   (user-id :binary-type (define-null-terminated-string user-id 16) :initform "")
   (record-id :binary-type u16 :initform 0)
   (record-length-after-header :binary-type u16 :initform 0 :accessor record-length-after-header)
   (description :binary-type 32-char-string :initform "")))

(defconstant +variable-length-record-fix-size+ 54)

(defun read-headers (fd)
  (let* ((header (read-binary 'public-header fd))
         (vlrecords (loop with pos = (file-position fd)
                          with first-vlr = (read-binary 'variable-length-record fd)
                          for i below (number-of-variable-length-records header)
                          for vlr = first-vlr then (read-binary 'variable-length-record fd)
                          do (progn
                               (incf pos (+ (record-length-after-header vlr)
                                            +variable-length-record-fix-size+))
                               (file-position fd pos))
                          collect vlr)))
    (values header vlrecords)))

(define-binary-class point-data ()
  ((x :binary-type s32 :initform 0)
   (y :binary-type s32 :initform 0)
   (z :binary-type s32 :initform 0)
   (intensity :binary-type u16 :initform 0)
   (gloubiboulga :binary-type u8 :initform 0)
   (classification :binary-type u8 :initform 0)
   (scan-angle-rank :binary-type s8 :initform 0)
   (user-data :binary-type u8 :initform 0)
   (point-source-id :binary-type u16 :initform 0)))

(defmethod return-number ((p point-data))
  (with-slots (gloubiboulga) p
    (ldb (byte 3 0) gloubiboulga)))

(defmethod (setf return-number) (value (p point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 3 0) gloubiboulga) value)))

(defmethod number-of-returns ((p point-data))
  (with-slots (gloubiboulga) p
    (ldb (byte 3 3) gloubiboulga)))

(defmethod (setf number-of-returns) (value (p point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 3 3) gloubiboulga) value)))

(defmethod scan-direction-flag ((p point-data))
  (with-slots (gloubiboulga) p
    (if (zerop (ldb (byte 1 6) gloubiboulga))
        'negative-scan
        'positive-scan)))

(defmethod (setf scan-direction-flag) (value (p point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 1 6) gloubiboulga) (ecase value
                                          (positive-scan 1)
                                          (negative-scan 0)))))

(defmethod edge-of-flight-line-p ((p point-data))
  (with-slots (gloubiboulga) p
    (= 1 (ldb (byte 1 7) gloubiboulga))))

(defmethod (setf edge-of-flight-line-p) (value (p point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 1 7) gloubiboulga) (if value 1 0))))

(defparameter *asprs-classification*
  '(created unclassified ground
    low-vegetation medium-vegetation high-vegetation
    building low-point key-point water
    reserved reserved overlap-point))

(defmethod classification ((p point-data))
  (with-slots (classification) p
    (let ((value (ldb (byte 5 0) classification)))
      (if (< value (length *asprs-classification*))
          (elt *asprs-classification* value)
          'reserved))))

(defmethod (setf classification) (value (p point-data))
  (with-slots (classification) p
    (let ((pos (position value *asprs-classification*)))
      (when pos
        (setf (ldb (byte 5 0) classification) pos)))))

(define-binary-class point-data-gps ()
  ((gps-time :binary-type float64 :initform 0 :accessor gps-time)))

(def-float64-accessor point-data-gps gps-time)

(define-binary-class point-data-color (point-data)
  ((red :binary-type u16 :initform 0)
   (green :binary-type u16 :initform 0)
   (blue :binary-type u16 :initform 0)))

(define-binary-class point-data-color-gps (point-data-color)
  ((gps-time :binary-type float64 :initform 0 :accessor gps-time)))

(def-float64-accessor point-data-color-gps gps-time)

(define-binary-class point-data-gps-waveform ()
  ((x :binary-type s32 :initform 0)
   (y :binary-type s32 :initform 0)
   (z :binary-type s32 :initform 0)
   (intensity :binary-type u16 :initform 0)
   (gloubiboulga :binary-type u8 :initform 0)
   (classification :binary-type u8 :initform 0)
   (scan-angle-rank :binary-type s8 :initform 0)
   (user-data :binary-type u8 :initform 0)
   (point-source-id :binary-type u16 :initform 0)
   (gps-time :binary-type float64 :initform 0 :accessor gps-time)
   (wave-packet-descriptor-index :binary-type u8 :initform 0)
   (byte-offset-to-waveform :binary-type u64 :initform 0)
   (waveform-packet-size :binary-type u32 :initform 0)
   (return-point-waveform-location :binary-type float32 :initform 0)
   (x-t :binary-type float32 :initform 0)
   (y-t :binary-type float32 :initform 0)
   (z-t :binary-type float32 :initform 0)))

;; (defmethod print-object ((p point-data-gps-waveform) stream)
;;   (with-slots (x y z) p
;;     (print-unreadable-object (p stream)
;;       (format stream "~d ~d ~d" x y z))))

(defparameter *point-data-classes*
  '(point-data
    point-data-gps
    point-data-color
    point-data-color-gps
    point-data-gps-waveform
    point-data-color-gps-waveform))

(defun read-first-point (filename)
  (with-open-file (fd filename :element-type '(unsigned-byte 8))
    (let ((h (read-headers fd)))
      (file-position fd (offset-to-point-data h))
      (loop with point-class = (elt *point-data-classes* (point-data-format-id h))
            for i below 10
            collect (read-binary point-class fd)))))
