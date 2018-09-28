(in-package :las)

(define-bitfield global-encoding (u2)
  ((gps-time 0)
   (internal-wave-data 1)
   (external-wave-data 2)
   (synthetic-return-point 3)
   (wkt 4)))

(define-binary-type 32char-string () (8bit-string :length 32 :terminator #\Nul))

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

(defun current-doy-year ()
  (labels ((leap-year-p (year)
	     (destructuring-bind (fh h f)
		 (mapcar #'(lambda (n) (zerop (mod year n))) '(400 100 4))
	       (or fh (and (not h) f)))))
    (multiple-value-bind (second minute hour date month year day-of-week dst-p tz) (get-decoded-time)
      (declare (ignore second minute hour day-of-week dst-p tz))
      (let ((month-len (list 31 (if (leap-year-p year) 29 28) 31 30 31 30 31 31 30 31 30 31)))
	(values (reduce #'+ (cons date (subseq month-len 0 (1- month))))
		year)))))

(multiple-value-bind (doy year) (current-doy-year)
  (let ((point-data-id 0))
    (define-binary-class public-header-legacy ()
      ((file-signature (8bit-string :length 4 :terminator #\Nul) "LASF")
       (file-source-id u2 0)
       (global-encoding global-encoding '(synthetic-return-point))
       (project-id1 u4 0)
       (project-id2 u2 0)
       (project-id3 u2 0)
       (project-id4 (vector :size 8 :type 'u1) #(0 0 0 0 0 0 0 0))
       (version-major u1 1)
       (version-minor u1 4)
       (system-identifier 32char-string "")
       (generating-software 32char-string "cl-las")
       (file-creation-doy u2 doy)
       (file-creation-year u2 year)
       (header-size u2)
       (offset-to-point-data u4 0)
       (number-of-variable-length-records u4 0)
       (point-data-format-id u1 point-data-id)
       (point-data-record-length u2 (object-size (nth point-data-id *point-data-classes*)))
       (%legacy-number-of-points u4 0)
       (%legacy-number-of-points-by-return (vector :size 5 :type 'u4) #(0 0 0 0 0))
       (x-scale float8 1)
       (y-scale float8 1)
       (z-scale float8 1)
       (x-offset float8 0)
       (y-offset float8 0)
       (z-offset float8 0)
       (%max-x float8 0)
       (%min-x float8 0)
       (%max-y float8 0)
       (%min-y float8 0)
       (%max-z float8 0)
       (%min-z float8 0)))))

(define-binary-class public-header-1.3 (public-header-legacy)
  ((start-of-waveform-data-packet u8 0)))

(define-binary-class public-header (public-header-1.3)
  ((start-of-first-extended-vlr u8 0)
   (number-of-extended-vlr u4 0)
   (%number-of-points u8 0)
   (%number-of-points-by-return (vector :size 15 :type 'u8) (apply #'vector (loop repeat 15 collect 0)))))

(defgeneric number-of-points (header)
  (:documentation "Unified number of points between legacy 32bit
  version and new 64bit one."))

(defgeneric (setf number-of-points) (value header)
  (:documentation "Unified setter for number of points between legacy
  32bit version and new 64bit one."))

(defmethod number-of-points ((header public-header-legacy))
  (%legacy-number-of-points header))

(defmethod (setf number-of-points) (value (header public-header-legacy))
  (setf (%legacy-number-of-points header) value))

(defmethod number-of-points ((header public-header))
  (%number-of-points header))

(defmethod (setf number-of-points) (value (header public-header))
  (setf (%number-of-points header) value))

(defgeneric number-of-points-by-return (header)
  (:documentation "Unified number of points by return between legacy
  and new."))

(defgeneric (setf number-of-points-by-return) (value header)
  (:documentation "Unified setter for number of points by return
  between legacy and new."))

(defmethod number-of-points-by-return ((header public-header-legacy))
  (%legacy-number-of-points-by-return header))

(defmethod (setf number-of-points-by-return) (value (header public-header-legacy))
  (setf (%legacy-number-of-points-by-return header) value))

(defmethod number-of-points-by-return ((header public-header))
  (%number-of-points-by-return header))

(defmethod (setf number-of-points-by-return) (value (header public-header))
  (setf (%number-of-points-by-return header) value))

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

(define-binary-class variable-length-record-header ()
  ((reserved u2 0)
   (user-id (8bit-string :length 16 :terminator #\Nul))
   (record-id u2)
   (record-length-after-header u2 0)
   (description 32char-string "")))

(define-binary-class waveform-packet-descriptor ()
  ((bits-per-sample u1)
   (waveform-compression-type u1)
   (number-of-samples u4)
   (temporal-sample-spacing u4)
   (digitizer-gain float8)
   (digitizer-offset float8)))

(define-binary-class geokey-directory ()
  ((key-directory-version u2 1)
   (key-revision u2 1)
   (minor-revision u2 0)
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

(define-binary-class extended-variable-length-record-header ()
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

(defun write-public-header (fd h)
  (file-position fd 0)
  (cond ((and (= (version-major h) 1)
	      (= (version-minor h) 3))
	 (write-value 'public-header-1.3 fd h))
	((and (= (version-major h) 1)
	      (< (version-minor h) 3))
	 (write-value 'public-header-legacy fd h))
	(t (write-value 'public-header fd h))))

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
  (let (res)
    (dotimes (i (number-of-variable-length-records header) (reverse res))
      (let* ((vlr (read-value 'variable-length-record-header fd))
	     (next-pos (+ (file-position fd) (record-length-after-header vlr)))
	     (content (read-vlr-content fd (user-id vlr) (record-id vlr))))
	(push content res)
	(file-position fd next-pos)))))

(defun read-evlrs (header fd)
  (let ((n (number-of-evlrs header))
	res)
    (unless (zerop n) (file-position fd (start-of-evlrs header)))
    (dotimes (i n (reverse res))
      (let* ((evlr (read-value 'extended-variable-length-record-header fd))
	     (next-pos (+ (file-position fd) (record-length-after-header evlr))))
	(push evlr res)
	(file-position fd next-pos)))))

(defun read-headers (fd)
  (let* ((header (read-public-header fd))
         (vlrecords (read-vlrs header fd))
	 (evlrecords (read-evlrs header fd)))
    (values header vlrecords evlrecords)))

(defun write-vlr (stream vlr)
  (with-accessors ((record-id vlr-record-id)
		   (user-id vlr-user-id)) vlr
    (cond ((string= user-id "LASF_Projection")
	   (with-accessors ((directory projection-vlr-directory)
			    (keys projection-vlr-keys)) vlr
	     (let ((sz (+ (object-size 'geokey-directory)
			  (* (object-size 'geokey-key) (length keys)))))
	       (write-value 'variable-length-record-header stream
			    (make-instance 'variable-length-record-header
					   :record-id record-id :user-id user-id :record-length-after-header sz))
	       (write-value 'geokey-directory stream directory)
	       (dolist (key keys)
		 (write-value 'geokey-key stream key)))))
	  ((string= user-id "LASF_Spec")
	   (cond ((and (> record-id 99) (< record-id 355))
		  (with-accessors ((content wpd-vlr-content)) vlr
		    (let ((sz (object-size content)))
		      (write-value 'variable-length-record-header stream
				   (make-instance 'variable-length-record-header
						  :record-id record-id
						  :user-id user-id
						  :record-length-after-header sz))
		      (write-value 'waveform-packet-descriptor stream content)))))))))

(defun write-evlr (stream evlr)
  (write-value 'extended-variable-length-record-header stream evlr)
  (file-position stream (+ (file-position stream) (record-length-after-header evlr))))

(defun write-headers (las)
  (with-accessors ((stream las-stream)
		   (pheader las-public-header)
		   (vlrs las-variable-length-records)
		   (evlrs las-extended-variable-length-records)) las
    (write-public-header stream pheader)
    (dolist (vlr vlrs)
      (write-vlr stream vlr))
    (file-position stream (start-of-evlrs pheader))
    (dolist (evlr evlrs)
      (write-evlr stream evlr))))

(define-binary-class point-data ()
  ((x s4)
   (y s4)
   (z s4)
   (intensity u2)
   (gloubiboulga u1)
   (classification u1)
   (scan-angle s1)
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
  '(:created
    :unclassified
    :ground
    :low-vegetation
    :medium-vegetation
    :high-vegetation
    :building
    :low-point
    :key-point
    :water
    :reserved
    :reserved
    :overlap-point))

(defmethod classification ((p point-data))
  (with-slots (classification) p
    (ldb (byte 5 0) classification)))

(defmethod (setf classification) (value (p point-data))
  (with-slots (classification) p
    (setf (ldb (byte 5 0) classification) value)))

(defmethod human-readable-classification ((p point-data))
  (let ((value (classification p)))
    (if (< value (length *asprs-classification*))
	(nth value *asprs-classification*)
	:reserved)))

(defmethod (setf human-readable-classification) (value (p point-data))
  (let ((pos (position value *asprs-classification*)))
    (when pos
      (setf (classification p) pos))))

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
  '(:created
    :unclassified
    :ground
    :low-vegetation
    :medium-vegetation
    :high-vegetation
    :building
    :low-point
    :reserved
    :water
    :rail
    :road-surface
    :reserved
    :wire-guard
    :wire-conductor
    :transmission-tower
    :wire-structure-connector
    :bridge-deck
    :high-noise))

(defmethod classification ((p new-point-data))
  (with-slots (classification) p
    (cond ((< classification (length *new-asprs-classification*))
	   (nth classification *new-asprs-classification*))
	  ((< classification 64) :reserved)
	  (t :user-definable))))

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

(defclass las ()
  ((%pathname :initarg :pathname :reader las-pathname)
   (%stream :initarg :stream :reader las-stream)
   (%wpd-stream :initarg :wpd-stream :initform nil :accessor las-wpd-stream
		:documentation "Stream to an external Wave Packet Descriptor.")
   (%header :accessor las-public-header)
   (%point-class :accessor las-point-class)
   (%vlrecords :initform nil :accessor las-variable-length-records)
   (%evlrecords :initform nil :accessor las-extended-variable-length-records)))

(defun make-las (pathname stream)
  (let ((las (make-instance 'las :pathname pathname :stream stream)))
    (when (slot-boundp las '%header)
      (assert (=  (point-data-record-length (las-public-header las)) (object-size (las-point-class las)))
	      () "Point data contains user-specific extra bytes."))
    las))

(defmethod initialize-instance :after ((object las) &key)
  "After las object creation, read slots' data from the input stream
or just instantiate slots in case of an output stream."
  (let ((stream (las-stream object)))
    (when (streamp stream)
      (with-accessors ((pathname las-pathname)
		       (wpd-stream las-wpd-stream)
		       (public-header las-public-header)
                       (vlrecords las-variable-length-records)
                       (point-class las-point-class)
		       (evlrecords las-extended-variable-length-records)) object
	(cond ((input-stream-p stream)
               (multiple-value-bind (pheader vlrs evlrs) (read-headers stream)
		 ;; open external WDP if needed
		 (when (member 'external-wave-data (global-encoding pheader))
		   (let ((wpd-name (merge-pathnames (make-pathname :type "wdp") pathname)))
		     (if (probe-file wpd-name)
			 (setf wpd-stream (open wpd-name :element-type '(unsigned-byte 8)))
			 (error "Can't find ~a waveform file" wpd-name))))
		 ;; go to data point in stream
		 (file-position stream (offset-to-point-data pheader))
		 ;; fill missing slots
		 (setf public-header pheader
                       vlrecords vlrs
                       point-class (nth (point-data-format-id pheader) *point-data-classes*)
		       evlrecords evlrs)))
	      ((output-stream-p stream)
	       (let ((pheader (make-instance 'public-header)))
		 (setf (header-size pheader) (object-size pheader)
		       public-header pheader
		       point-class (nth (point-data-format-id pheader) *point-data-classes*)))))))))

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

(defun write-point (point las &key unscale-p)
  "Write a point in the given LAS. XXX position int the LAS stream
  should be correct."
  (with-accessors ((x x) (y y) (z z)) point
    ;; Update bounding box. Done before unscaling. The spec says: "The
    ;; max and min data fields are the actual unscaled extents of the
    ;; LAS point file data, specified in the coordinate system of the
    ;; LAS data." This was not clear to me but others softwares LAS
    ;; output use a bounding box in the coordinate system unit.
    (with-accessors ((header las-public-header)) las
      (when (< x (%min-x header)) (setf (%min-x header) x))
      (when (> x (%max-x header)) (setf (%max-x header) x))
      (when (< y (%min-y header)) (setf (%min-y header) y))
      (when (> y (%max-y header)) (setf (%max-y header) y))
      (when (< z (%min-z header)) (setf (%min-z header) z))
      (when (> z (%max-z header)) (setf (%max-z header) z)))
    (when unscale-p
      (with-accessors ((x-scale x-scale) (x-offset x-offset)
		       (y-scale y-scale) (y-offset y-offset)
		       (z-scale z-scale) (z-offset z-offset)) (las-public-header las)
        (setf x (/ (- x x-offset) x-scale)
	      y (/ (- y y-offset) y-scale)
	      z (/ (- z z-offset) z-scale)))))
  (write-value (las-point-class las) (las-stream las) point))

(defun write-point-at (point index las &key unscale-p)
  "Write a point at a given index"
  (file-position (las-stream las) (+ (offset-to-point-data (las-public-header las))
				     (* index (object-size (las-point-class las)))))
  (write-point point las :unscale-p unscale-p))

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
   (times :initarg :times :accessor waveform-times)
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
  (:documentation "Waveform temporal spacing in picoseconds (ps).")
  (:method (point las)
    "Defaults to something useful: 1000 ps is a 15cm grid"
    1000))

(defmethod waveform-temporal-spacing-of-point ((point waveform-mixin) las)
  (let ((wpd (%get-wave-packet-descriptor-of-point point las)))
    (when wpd
      (temporal-sample-spacing wpd))))

(defgeneric waveform-of-point (point las))

(defmethod waveform-of-point ((point waveform-mixin) las)
  (let* ((header (las-public-header las))
	 (stream (if (las-wpd-stream las) (las-wpd-stream las) (las-stream las)))
	 (evlr-pos (if (las-wpd-stream las) 0 (start-of-evlrs header)))
	 (wpd (%get-wave-packet-descriptor-of-point point las)))
    (when wpd
      (assert (= (* (number-of-samples wpd) (truncate (bits-per-sample wpd) 8))
		 (waveform-packet-size point)))
      (file-position stream (+ evlr-pos (byte-offset-to-waveform point)))
      (loop with n = (number-of-samples wpd)
	    with dt = (temporal-sample-spacing wpd)
	    with bps = (bits-per-sample wpd)
	    with xs = (make-array n :element-type 'double-float)
	    with ys = (make-array n :element-type 'double-float)
	    with zs = (make-array n :element-type 'double-float)
	    with times = (make-array n)
	    with intensities = (make-array n :element-type `(unsigned-byte ,bps))
	    for i below n
	    for j downfrom (1- n)
	    do (let ((time (float (- (return-point-waveform-location point) (* i dt)) 1.d0)))
		 (setf (aref xs j) (+ (x point) (* (x-t point) time))
		       (aref ys j) (+ (y point) (* (y-t point) time))
		       (aref zs j) (+ (z point) (* (z-t point) time))
		       (aref times j) (* j dt)
		       (aref intensities j) (read-value (type-from-bits bps) stream)))
	    finally (return (make-instance 'waveform :xs xs :ys ys :zs zs :times times
						     :intensities intensities))))))

(defmethod projection ((las las))
  (let ((vlr-projection (find "LASF_Projection" (las-variable-length-records las)
			      :key #'vlr-user-id :test #'string=)))
    (when vlr-projection
      (get-projection-code (projection-vlr-keys vlr-projection)))))

(defmethod (setf projection) (epsg-code (las las))
  (with-accessors ((header las-public-header)
		   (vlrs las-variable-length-records)) las
    (let ((user-id "LASF_Projection"))
      (multiple-value-bind (directory keys) (make-projection-geokey epsg-code)
	(setf vlrs (cons
		    (make-instance 'projection-vlr :record-id 34735 :user-id user-id
						   :directory directory :keys keys)
		    (remove user-id vlrs :key #'vlr-user-id :test #'string=))
	      ;; Update number of variable length record
	      (number-of-variable-length-records header) (length vlrs))))))

(defun open-las-file (filename &key (direction :input) if-exists if-does-not-exist)
  (let ((stream (open filename
		      :element-type '(unsigned-byte 8)
		      :direction direction :if-exists if-exists :if-does-not-exist if-does-not-exist)))
    (make-las filename stream)))

(defmacro with-las ((las filename &rest options) &body body)
  (alexandria:with-gensyms (abort)
    `(let ((,las (open-las-file ,filename ,@options))
	   (,abort t))
       (unwind-protect
            (multiple-value-prog1
              (unwind-protect
		   (progn,@body)
		(when (las-wpd-stream ,las) (close (las-wpd-stream ,las))))
              (setq ,abort nil))
         (when (las-stream ,las) (close (las-stream ,las) :abort ,abort))))))

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

(defun write-test (lasfile nx ny)
  (with-las (las lasfile :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((header (las-public-header las)))
      (setf (number-of-points header) (* nx ny)
	    ;; no VLRS
	    (offset-to-point-data header) (object-size header))
      (loop with off-x = 414800
	    with off-y = -130000
	    with epsilon-pi = (/ pi 100)
	    for i below nx
	    for x from off-x by 1
	    do (loop for j below ny
		     for y from off-y by 1
		     do (let ((z (round (* 5 (sin (/ (- x off-x) epsilon-pi))
					   (sin (/ (- off-y y) epsilon-pi))))))
			  (write-point-at (make-instance (las-point-class las) :x x :y y :z z
									       :intensity 0
									       :gloubiboulga 0
									       :classification 0
									       :scan-angle 0
									       :user-data 0
									       :point-source-id 0)
					  (+ j (* ny i)) las)
			  (when (and (< i 10) (< j 10)) (format t "~@{~a~^ ~}~%" i j x y z)))))
      #+nil(setf (projection las) 2154)
      (write-headers las))))
