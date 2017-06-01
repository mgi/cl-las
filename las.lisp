(in-package :las)

(define-bitfield global-encoding (u2)
  ((gps-time 0)
   (internal-wave-data 1)
   (external-wave-data 2)
   (synthetic-return-point 3)
   (wkt 4)))

(define-binary-type 32char-string () (8bit-string :length 32 :terminator #\Nul))

(define-binary-class public-header-legacy ()
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
   (%legacy-number-of-points u4)
   (%legacy-number-of-points-by-return (vector :size 5 :type 'u4))
   (x-scale float8)
   (y-scale float8)
   (z-scale float8)
   (x-offset float8)
   (y-offset float8)
   (z-offset float8)
   (%max-x float8)
   (%min-x float8)
   (%max-y float8)
   (%min-y float8)
   (%max-z float8)
   (%min-z float8)))

(define-binary-class public-header-1.3 (public-header-legacy)
  ((start-of-waveform-data-packet u8)))

(define-binary-class public-header (public-header-1.3)
  ((start-of-first-extended-vlr u8)
   (number-of-extended-vlr u4)
   (%number-of-points u8)
   (%number-of-points-by-return (vector :size 15 :type 'u8))))

(defgeneric number-of-points (header)
  (:documentation "Unified number of points between legacy 32bit
  version and new 64bit one."))

(defmethod number-of-points ((header public-header-legacy))
  (%legacy-number-of-points header))

(defmethod number-of-points ((header public-header))
  (%number-of-points header))

(defgeneric number-of-points-by-return (header)
  (:documentation "Unified number of points by return between legacy
  and new."))

(defmethod number-of-points-by-return ((header public-header-legacy))
  (%legacy-number-of-points-by-return header))

(defmethod number-of-points-by-return ((header public-header))
  (%number-of-points-by-return header))

(defgeneric start-of-evlrs (header)
  (:documentation "Unified start of EVLRs between new and before
  1.4."))

(defmethod start-of-evlrs ((header public-header-1.3))
  (start-of-waveform-data-packet header))

(defmethod start-of-evlrs ((header public-header))
  (start-of-first-extended-vlr header))

(defgeneric number-of-evlrs (header)
  (:documentation "Unified number of EVLRs between new and before
  1.4."))

(defmethod number-of-evlrs ((header public-header-1.3))
  "According to the specs, there is only one extended variable length
  record in LAS 1.3 and it is a waveform data packet."
  1)

(defmethod number-of-evlrs ((header public-header))
  (number-of-extended-vlr header))

(define-binary-class variable-length-record ()
  ((reserved u2)
   (user-id (8bit-string :length 16 :terminator #\Nul))
   (record-id u2)
   (record-length-after-header u2)
   (description 32char-string)))

(define-binary-class waveform-packet-descriptor ()
  ((bits-per-sample u1)
   (waveform-compression-type u1)
   (number-of-samples u4)
   (temporal-sample-spacing u4)
   (digitizer-gain float8)
   (digitizer-offset float8)))

(define-binary-class geokey-directory ()
  ((key-directory-version u2)
   (key-revision u2)
   (minor-revision u2)
   (number-of-keys u2)))

(define-binary-class geokey-key ()
  ((key-id u2)
   (tiff-tag-location u2)
   (char-count u2)
   (value-offset u2)))

(defmethod print-object ((key geokey-key) stream)
  (with-accessors ((id key-id)
		   (tag tiff-tag-location)
		   (val value-offset)) key
    (print-unreadable-object (key stream :type t)
      (format stream "~d ~d ~d" id tag val))))

(define-binary-class extended-variable-length-record ()
  ((reserved u2)
   (user-id (8bit-string :length 16 :terminator #\Nul))
   (record-id u2)
   (record-length-after-header u8)
   (description 32char-string)))

(defun read-public-header (fd)
  (file-position fd 0)
  (let ((h (read-value 'public-header fd)))
    (cond ((and (= (version-major h) 1)
		(= (version-minor h) 3))
	   (file-position fd 0)
	   (read-value 'public-header-1.3 fd))
	  ((and (= (version-major h) 1)
		(< (version-minor h) 3))
	   (file-position fd 0)
	   (read-value 'public-header-legacy fd))
	  (t h))))

(defclass vlr-mixin ()
  ((user-id :initarg :user-id :accessor vlr-user-id)
   (record-id :initarg :record-id :accessor vlr-record-id))
  (:documentation "More user-friendly class for variable length record
  then the underlying binary class."))

(defclass wpd-vlr (vlr-mixin)
  ((content :initarg :content :accessor wpd-vlr-content))
  (:documentation "Waveform packet descriptor VLR"))

(defclass projection-vlr (vlr-mixin)
  ((directory :initarg :directory :accessor projection-vlr-directory)
   (keys :initarg :keys :accessor projection-vlr-keys))
  (:documentation "Projection VLR"))

(defun read-vlr-content (fd user-id record-id)
  (cond ((string= user-id "LASF_Projection")
	 (case record-id
	   (2111 :not-yet-ogc-math-transform-wkt)
	   (2112 :not-yet-ogc-coord-system-wkt)
	   (34735 (let ((directory (read-value 'geokey-directory fd)))
		    (make-instance 'projection-vlr
				   :user-id user-id
				   :record-id record-id
				   :directory directory
				   :keys (loop for i below (number-of-keys directory)
					       collect (read-value 'geokey-key fd)))))
	   (34736 (read-value 'geo-double-params-tag fd))))
	((string= user-id "LASF_Spec")
	 (cond ((and (> record-id 99) (< record-id 355))
		(make-instance 'wpd-vlr :user-id user-id :record-id record-id
					:content (read-value 'waveform-packet-descriptor fd)))))))

;; XXX to be called right after a read-public-header
(defun read-vlrs (header fd)
  (do ((vlr (read-value 'variable-length-record fd)
	    (read-value 'variable-length-record fd))
       (i 0 (1+ i))
       res)
      ((= i (number-of-variable-length-records header)) (nreverse res))
    (let ((next-pos (+ (file-position fd) (record-length-after-header vlr)))
	  (content (read-vlr-content fd (user-id vlr) (record-id vlr))))
      (push content res)
      (file-position fd next-pos))))

(defun read-evlrs (header fd)
  (file-position fd (start-of-evlrs header))
  (do ((evlr (read-value 'extended-variable-length-record fd)
	     (read-value 'extended-variable-length-record fd))
       (i 0 (1+ i))
       res)
      ((= i (number-of-evlrs header)) (nreverse res))
    (push evlr res)
    (let ((pos (+ (file-position fd) (record-length-after-header evlr))))
      (file-position fd pos))))

(defun read-headers (fd)
  (let* ((header (read-public-header fd))
         (vlrecords (read-vlrs header fd))
	 (evlrecords (read-evlrs header fd)))
    (values header vlrecords evlrecords)))

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

(defgeneric return-number (point))
(defgeneric (setf return-number) (value point))
(defgeneric number-of-returns (point))
(defgeneric (setf number-of-returns) (value point))
(defgeneric scan-direction (point))
(defgeneric (setf scan-direction) (value point))
(defgeneric edge-of-flight-line-p (point))
(defgeneric (setf edge-of-flight-line-p) (value point))

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
          (nth value *asprs-classification*)
          'reserved))))

(defmethod (setf classification) (value (p point-data))
  (with-slots (classification) p
    (let ((pos (position value *asprs-classification*)))
      (when pos
        (setf (ldb (byte 5 0) classification) pos)))))

(define-binary-class new-point-data ()
  ((x s4)
   (y s4)
   (z s4)
   (intensity u2)
   (gloubiboulga u2)
   (classification u1)
   (user-data u1)
   (scan-angle s2)
   (point-source-id u2)
   (gps-time float8)))

(defmethod return-number ((p new-point-data))
  (with-slots (gloubiboulga) p
    (ldb (byte 4 0) gloubiboulga)))

(defmethod (setf return-number) (value (p new-point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 4 0) gloubiboulga) value)))

(defmethod number-of-returns ((p new-point-data))
  (with-slots (gloubiboulga) p
    (ldb (byte 4 4) gloubiboulga)))

(defmethod (setf number-of-returns) (value (p new-point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 4 4) gloubiboulga) value)))

(defmethod classification-flags ((p new-point-data))
  (with-slots (gloubiboulga) p
    (ldb (byte 4 8) gloubiboulga)))

(defmethod (setf classification-flags) (value (p new-point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 4 8) gloubiboulga) value)))

(defmethod scanner-channel ((p new-point-data))
  (with-slots (gloubiboulga) p
    (ldb (byte 2 12) gloubiboulga)))

(defmethod (setf scanner-channel) (value (p new-point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 2 12) gloubiboulga) value)))

(defmethod scan-direction ((p new-point-data))
  (with-slots (gloubiboulga) p
    (if (zerop (ldb (byte 1 14) gloubiboulga))
        'negative-scan
        'positive-scan)))

(defmethod (setf scan-direction) (value (p new-point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 1 14) gloubiboulga) (ecase value
					   (positive-scan 1)
					   (negative-scan 0)))))

(defmethod edge-of-flight-line-p ((p new-point-data))
  (with-slots (gloubiboulga) p
    (= 1 (ldb (byte 1 15) gloubiboulga))))

(defmethod (setf edge-of-flight-line-p) (value (p new-point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 1 15) gloubiboulga) (if value 1 0))))

(defparameter *new-asprs-classification*
  '(created unclassified ground
    low-vegetation medium-vegetation high-vegetation
    building low-point reserved water rail road-surface
    reserved wire-guard wire-conductor transmission-tower
    wire-structure-connector bridge-deck high-noise))

(defmethod classification ((p new-point-data))
  (with-slots (classification) p
    (cond ((< classification (length *new-asprs-classification*))
	   (nth classification *new-asprs-classification*))
	  ((< classification 64) 'reserved)
	  (t 'user-definable))))

(defmethod (setf classification) (value (p new-point-data))
  (with-slots (classification) p
    (let ((pos (position value *new-asprs-classification*)))
      (when pos
        (setf classification pos)))))

(define-binary-class gps-mixin ()
  ((gps-time float8)))

(define-binary-class color-mixin ()
  ((red u2)
   (green u2)
   (blue u2)))

(define-binary-class nir-mixin ()
  ((nir u2)))

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

(define-binary-class new-point-data-color (color-mixin new-point-data) ())
(define-binary-class new-point-data-color-nir (nir-mixin color-mixin new-point-data) ())
(define-binary-class new-point-data-waveform (waveform-mixin new-point-data) ())
(define-binary-class new-point-data-color-nir-waveform (waveform-mixin new-point-data-color-nir) ())

(defmethod print-object ((p point-data) stream)
  (with-accessors ((x x) (y y) (z z)) p
    (print-unreadable-object (p stream)
      (format stream "~d ~d ~d" x y z))))

(defmethod print-object ((p new-point-data) stream)
  (with-accessors ((x x) (y y) (z z)) p
    (print-unreadable-object (p stream)
      (format stream "~d ~d ~d" x y z))))

(defparameter *point-data-classes*
  '(point-data
    point-data-gps
    point-data-color
    point-data-color-gps
    point-data-gps-waveform
    point-data-color-gps-waveform
    new-point-data
    new-point-data-color
    new-point-data-color-nir
    new-point-data-waveform
    new-point-data-color-nir-waveform))

(defclass las ()
  ((%stream :initarg :stream :reader las-stream)
   (%header :accessor las-public-header)
   (%vlrecords :accessor las-variable-length-records)
   (%point-class :accessor las-point-class)
   (%evlrecords :accessor las-extended-variable-length-records)))

(defun make-las (stream)
  (let ((las (make-instance 'las :stream stream)))
    (assert (=  (point-data-record-length (las-public-header las)) (object-size (las-point-class las)))
	    () "Point data contains user-specific extra bytes.")
    las))

(defmethod initialize-instance :after ((object las) &key)
  (let ((stream (las-stream object)))
    (when (streamp stream)
      (with-accessors ((public-header las-public-header)
                       (vlrecords las-variable-length-records)
                       (point-class las-point-class)
		       (evlrecords las-extended-variable-length-records)) object
        (multiple-value-bind (pheader vlrs evlrs) (read-headers stream)
          ;; go to data point in stream
          (file-position stream (offset-to-point-data pheader))
          ;; fill missing slots
          (setf public-header pheader
                vlrecords vlrs
                point-class (nth (point-data-format-id pheader) *point-data-classes*)
		evlrecords evlrs))))))

(defun read-point (las &key scale-p)
  "Read a point in the given LAS. XXX position into the LAS stream
should be correct."
  (let ((p (read-value (las-point-class las) (las-stream las))))
    (when scale-p
      (with-accessors ((x x) (y y) (z z)) p
        (with-accessors ((x-scale x-scale) (x-offset x-offset)
                         (y-scale y-scale) (y-offset y-offset)
                         (z-scale z-scale) (z-offset z-offset)) (las-public-header las)
          (setf x (+ x-offset (* x x-scale))
                y (+ y-offset (* y y-scale))
                z (+ z-offset (* z z-scale))))))
    p))

(defun read-point-at (index las &key scale-p)
  "Read a point at a given index."
  (file-position (las-stream las) (+ (offset-to-point-data (las-public-header las))
				     (* index (object-size (las-point-class las)))))
  (read-point las :scale-p scale-p))

(defmacro make-getter (fun-name low-level-name)
  `(defun ,fun-name (las)
     (,low-level-name (las-public-header las))))

(make-getter max-x %max-x)
(make-getter min-x %min-x)
(make-getter max-y %max-y)
(make-getter min-y %min-y)
(make-getter max-z %max-z)
(make-getter min-z %min-z)

(defclass waveform ()
  ((xs :initarg :xs :accessor waveform-xs)
   (ys :initarg :ys :accessor waveform-ys)
   (zs :initarg :zs :accessor waveform-zs)
   (intensities :initarg :intensities :accessor waveform-intensities))
  (:documentation "Spacially positionned waveform."))

(defun type-from-bits (bits)
  (ecase bits
    (8 'u1)
    (16 'u2)
    (32 'u4)))

(defun %get-wave-packet-descriptor-of-point (point las)
  (let ((record-id (+ 99 (wave-packet-descriptor-index point)))
	(vlrs (las-variable-length-records las)))
    (labels ((wpd-key (elt)
	       (and (typep elt 'wpd-vlr)
		    (vlr-record-id elt))))
      (unless (= record-id 99)
	(wpd-vlr-content (find record-id vlrs :key #'wpd-key))))))

(defgeneric waveform-temporal-spacing-of-point (point las)
  (:documentation "Waveform temporal spacing in picoseconds (ps)."))

(defmethod waveform-temporal-spacing-of-point ((point waveform-mixin) las)
  (let ((wpd (%get-wave-packet-descriptor-of-point point las)))
    (when wpd
      (temporal-sample-spacing wpd))))

(defgeneric waveform-of-point (point las))

(defmethod waveform-of-point ((point waveform-mixin) las)
  (let* ((header (las-public-header las))
	 (evlr-pos (start-of-evlrs header))
	 (wpd (%get-wave-packet-descriptor-of-point point las)))
    (when wpd
      (assert (= (* (number-of-samples wpd) (truncate (bits-per-sample wpd) 8))
		 (waveform-packet-size point)))
      (file-position (las-stream las) (+ evlr-pos (byte-offset-to-waveform point)))
      (loop with n = (number-of-samples wpd)
	    with dt = (temporal-sample-spacing wpd)
	    with bps = (bits-per-sample wpd)
	    with xs = (make-array n :element-type 'double-float)
	    with ys = (make-array n :element-type 'double-float)
	    with zs = (make-array n :element-type 'double-float)
	    with intensities = (make-array n :element-type `(unsigned-byte ,bps))
	    for i below n
	    for j downfrom (1- n)
	    do (let ((time (float (- (return-point-waveform-location point) (* i dt)) 1.d0)))
		 (setf (aref xs j) (+ (x point) (* (x-t point) time))
		       (aref ys j) (+ (y point) (* (y-t point) time))
		       (aref zs j) (+ (z point) (* (z-t point) time))
		       (aref intensities j) (read-value (type-from-bits bps) (las-stream las))))
	    finally (return (make-instance 'waveform :xs xs :ys ys :zs zs :intensities intensities))))))

(defmethod projection ((las las))
  (labels ((mykey (elt)
	     (vlr-user-id elt)))
    (let ((vlr-projection (find "LASF_Projection" (las-variable-length-records las) :key #'mykey :test #'string=)))
      vlr-projection)))

(defmacro with-las ((las filename) &body body)
  (alexandria:with-gensyms (stream abort)
    `(let ((,stream (open ,filename :element-type '(unsigned-byte 8)))
           (,abort t))
       (unwind-protect
            (multiple-value-prog1
                (let ((,las (make-las ,stream)))
                  ,@body)
              (setq ,abort nil))
         (when ,stream (close ,stream :abort ,abort))))))

(defmethod las-number-of-points ((object las))
  (number-of-points (las-public-header object)))

;;; Examples
(defun las-to-txt (lasfile outfile &optional n)
  "Example of text convertion."
  (with-open-file (out outfile :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
    (with-las (in lasfile)
      (dotimes (i (or n (las-number-of-points in)))
        (with-accessors ((x x)
                         (y y)
                         (z z)
                         (intensity intensity)
                         (gps-time gps-time)
                         (return-number return-number)) (read-point in :scale-p t)
          (format out "~&~f ~f ~f ~d ~f ~d~%" x y z
                  intensity gps-time return-number))))))

(defun wave-to-txt (lasfile outfile &optional (index 0) n)
  (with-open-file (out outfile :direction :output
			       :if-exists :supersede
			       :if-does-not-exist :create)
    (with-las (in lasfile)
      (dotimes (i (or n (las-number-of-points in)))
	(let* ((point (read-point-at (+ index i) in))
	       (wave (waveform-of-point point in)))
	  (with-accessors ((xs waveform-xs)
			   (ys waveform-ys)
			   (zs waveform-zs)
			   (intensities waveform-intensities)) wave
	    (loop for x across xs
		  for y across ys
		  for z across zs
		  for intensity across intensities
		  do (format out "~&~f ~f ~f ~d~%" x y z intensity)))
	  (terpri out))))))
