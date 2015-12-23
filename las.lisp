(in-package :las)

(define-bitfield global-encoding (u2)
  ((gps-time 0)
   (internal-wave-data 1)
   (external-wave-data 2)
   (synthetic-return-point 3)))

(define-binary-type 32char-string () (8bit-string :length 32 :terminator #\Nul))

(define-binary-class public-header ()
  ((file-signature (8bit-string :length 4 :terminator #\Nul))
   (file-source-id u2)
   (global-encoding global-encoding)
   (project-id1 u4)
   (project-id2 u2)
   (project-id3 u2)
   (project-id4 (vector :size 8 :type 'u1))
   (version-major u1)
   (version-minor u1)
   (system-identifier 32char-string)
   (generating-software 32char-string)
   (file-creation-doy u2)
   (file-creation-year u2)
   (header-size u2)
   (offset-to-point-data u4)
   (number-of-variable-length-records u4)
   (point-data-format-id u1)
   (point-data-record-length u2)
   (number-of-point-records u4)
   (number-of-points-by-return (vector :size 5 :type 'u4))
   (x-scale float8)
   (y-scale float8)
   (z-scale float8)
   (x-offset float8)
   (y-offset float8)
   (z-offset float8)
   (max-x float8)
   (min-x float8)
   (max-y float8)
   (min-y float8)
   (max-z float8)
   (min-z float8)
   (start-of-waveform-data-packet u8)))

(define-binary-class variable-length-record ()
  ((reserved u2)
   (user-id (8bit-string :length 16 :terminator #\Nul))
   (record-id u2)
   (record-length-after-header u2)
   (description 32char-string)))

(defconstant +variable-length-record-fix-size+ 54)

(defun read-headers (fd)
  (let* ((header (read-value 'public-header fd))
         (vlrecords (loop with pos = (file-position fd)
                          with first-vlr = (read-value 'variable-length-record fd)
                          for i below (number-of-variable-length-records header)
                          for vlr = first-vlr then (read-value 'variable-length-record fd)
                          do (progn
                               (incf pos (+ (record-length-after-header vlr)
                                            +variable-length-record-fix-size+))
                               (file-position fd pos))
                          collect vlr)))
    (values header vlrecords)))


(define-binary-class point-data ()
  ((x s4)
   (y s4)
   (z s4)
   (intensity u2)
   (gloubiboulga u1)
   (classification u1)
   (scan-angle-rank s1)
   (user-data u1)
   (point-source-id u2)))

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

(defmethod scan-direction ((p point-data))
  (with-slots (gloubiboulga) p
    (if (zerop (ldb (byte 1 6) gloubiboulga))
        'negative-scan
        'positive-scan)))

(defmethod (setf scan-direction) (value (p point-data))
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

(define-binary-class gps-mixin ()
  ((gps-time float8)))

(define-binary-class color-mixin ()
  ((red u2)
   (green u2)
   (blue u2)))

(define-binary-class waveform-mixin ()
  ((wave-packet-descriptor-index u1)
   (byte-offset-to-waveform u8)
   (waveform-packet-size u4)
   (return-point-waveform-location float4)
   (x-t float4)
   (y-t float4)
   (z-t float4)))

(define-binary-class point-data-gps (gps-mixin point-data) ())
(define-binary-class point-data-color (color-mixin point-data) ())
(define-binary-class point-data-color-gps (color-mixin gps-mixin point-data) ())
(define-binary-class point-data-gps-waveform (waveform-mixin gps-mixin point-data) ())
(define-binary-class point-data-color-gps-waveform (waveform-mixin gps-mixin color-mixin point-data) ())

(defmethod print-object ((p point-data) stream)
  (with-slots (x y z) p
    (print-unreadable-object (p stream)
      (format stream "~d ~d ~d" x y z))))

(defparameter *point-data-classes*
  '(point-data
    point-data-gps
    point-data-color
    point-data-color-gps
    point-data-gps-waveform
    point-data-color-gps-waveform))

(defun read-some-points (filename n)
  (with-open-file (fd filename :element-type '(unsigned-byte 8))
    (let ((h (read-headers fd)))
      (file-position fd (offset-to-point-data h))
      (loop with point-class = (elt *point-data-classes* (point-data-format-id h))
            for i below n
            collect (read-value point-class fd)))))

(defun gp-points (lasfile outfile n)
  (with-open-file (fd outfile :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
    (labels ((pp-point (point)
               (with-slots (x y z) point
                 (format fd "~&~d ~d ~d~%" x y z))
               point))
      (mapcar #'pp-point (read-some-points lasfile n)))))
