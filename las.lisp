(in-package :las)

(define-bitfield global-encoding (:u16)
  ((gps-time 0)
   (internal-wave-data 1)
   (external-wave-data 2)
   (synthetic-return-point 3)
   (wkt 4)))

(define-binary-type 32char-string ()
  (binary-io.common-datatypes:8bit-string :length 32 :terminator #\Nul))

(define-binary-type variable-string ()
  (binary-io.common-datatypes:8bit-string :terminator #\Nul))

(defparameter *point-data-classes*
  '(legacy-point-data
    legacy-point-data-gps
    legacy-point-data-color
    legacy-point-data-color-gps
    legacy-point-data-gps-waveform
    legacy-point-data-color-gps-waveform
    point-data
    point-data-color
    point-data-color-nir
    point-data-waveform
    point-data-color-nir-waveform))

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

(define-binary-class public-header-legacy ()
  ((file-signature
    (binary-io.common-datatypes:8bit-string :length 4 :terminator #\Nul) "LASF")
   (file-source-id :u16 0)
   (global-encoding global-encoding '(synthetic-return-point))
   (project-id1 :u32 0)
   (project-id2 :u16 0)
   (project-id3 :u16 0)
   (project-id4 (:vector :size 8 :type :u8) #(0 0 0 0 0 0 0 0))
   (version-major :u8 1)
   (version-minor :u8 4)
   (system-identifier 32char-string "")
   (generating-software 32char-string "cl-las")
   (file-creation-doy :u16)
   (file-creation-year :u16)
   (header-size :u16)
   (offset-to-point-data :u32 0)
   (number-of-variable-length-records :u32 0)
   (point-data-format-id :u8)
   (point-data-record-length :u16)
   (%legacy-number-of-points :u32 0)
   (%legacy-number-of-points-by-return (:vector :size 5 :type :u32) #(0 0 0 0 0))
   (x-scale :float64 1)
   (y-scale :float64 1)
   (z-scale :float64 1)
   (x-offset :float64 0)
   (y-offset :float64 0)
   (z-offset :float64 0)
   ;; XXX following slots should remain unbound to be correctly
   ;; updated when writing points.
   (%max-x :float64)
   (%min-x :float64)
   (%max-y :float64)
   (%min-y :float64)
   (%max-z :float64)
   (%min-z :float64)))

(define-binary-class public-header-1.3 (public-header-legacy)
  ((start-of-waveform-data-packet :u64 0)))

(define-binary-class public-header (public-header-1.3)
  ((start-of-first-extended-vlr :u64 0)
   (number-of-extended-vlr :u32 0)
   (%number-of-points :u64 0)
   (%number-of-points-by-return (:vector :size 15 :type :u64) (apply #'vector (loop repeat 15 collect 0)))))

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
  ;; set both. old and new.
  (setf (%number-of-points header) value
        (%legacy-number-of-points header) value))

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
  ;; set both.
  (setf (%number-of-points-by-return header) value
        (%legacy-number-of-points-by-return header) (subseq value 0 5)))

(defgeneric start-of-evlrs (header)
  (:documentation "Unified start of EVLRs between new and before
  1.4."))

(defmethod start-of-evlrs ((header public-header-1.3))
  (start-of-waveform-data-packet header))

(defmethod start-of-evlrs ((header public-header))
  (start-of-first-extended-vlr header))

(defgeneric number-of-evlrs (header)
  (:documentation "Unified number of EVLRs between new and before
  1.4.")
  ;; There is no EVLRs by default.
  (:method (header) 0))

(defmethod number-of-evlrs ((header public-header-1.3))
  "According to the specs, there is only one extended variable length
  record in LAS 1.3 and it is a waveform data packet."
  1)

(defmethod number-of-evlrs ((header public-header))
  (number-of-extended-vlr header))

(define-binary-class variable-length-record-header ()
  ((reserved :u16 0)
   (user-id (binary-io.common-datatypes:8bit-string :length 16 :terminator #\Nul))
   (record-id :u16)
   (record-length-after-header :u16 0)
   (description 32char-string "")))

(define-binary-class waveform-packet-descriptor ()
  ((bits-per-sample :u8)
   (waveform-compression-type :u8)
   (number-of-samples :u32)
   (temporal-sample-spacing :u32)
   (digitizer-gain :float64)
   (digitizer-offset :float64)))

(define-binary-class geokey-directory ()
  ((key-directory-version :u16 1)
   (key-revision :u16 1)
   (minor-revision :u16 0)
   (number-of-keys :u16)))

(define-binary-class geokey-key ()
  ((key-id :u16)
   (tiff-tag-location :u16)
   (char-count :u16)
   (value-offset :u16)))

(define-binary-class extended-variable-length-record-header ()
  ((reserved :u16)
   (user-id (binary-io.common-datatypes:8bit-string :length 16 :terminator #\Nul))
   (record-id :u16)
   (record-length-after-header :u64)
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
   (record-id :initarg :record-id :accessor vlr-record-id)
   (disk-size :initarg :disk-size :accessor vlr-disk-size))
  (:documentation "More user-friendly class for variable length record
  then the underlying binary class.  It is used for both VLR and
  Extended VLR (see `read-vlr-content')."))

(defclass wpd-vlr (vlr-mixin)
  ((content :initarg :content :accessor wpd-vlr-content))
  (:documentation "Waveform packet descriptor VLR"))

(defclass geotiff-projection-vlr (vlr-mixin)
  ((directory :initarg :directory :accessor geotiff-projection-vlr-directory)
   (keys :initarg :keys :accessor geotiff-projection-vlr-keys))
  (:documentation "GeoTIFF projection VLR"))

(defclass ogc-cs-projection-vlr (vlr-mixin)
  ((string :initarg :string :accessor ogc-cs-projection-vlr-string))
  (:documentation "OGC Coordinate System projection VLR"))

(defun read-vlr-content (fd user-id record-id)
  (cond ((string= user-id "LASF_Projection")
         (case record-id
           (2111 :not-yet-ogc-math-transform-wkt)
           (2112 (let ((string (read-value 'variable-string fd)))
                   (make-instance 'ogc-cs-projection-vlr
                                  :user-id user-id
                                  :record-id record-id
                                  :disk-size (+ (type-size 'variable-length-record-header)
                                                (length string))
                                  :string string)))
           (34735 (let* ((directory (read-value 'geokey-directory fd))
                         (nkeys (number-of-keys directory)))
                    (make-instance 'geotiff-projection-vlr
                                   :user-id user-id
                                   :record-id record-id
                                   :disk-size (+ (type-size 'variable-length-record-header)
                                                 (type-size directory)
                                                 (* nkeys (type-size 'geokey-key)))
                                   :directory directory
                                   :keys (loop for i below nkeys
                                               collect (read-value 'geokey-key fd)))))
           ((34736 34737) :ditch-optional-geokey)))
        ((string= user-id "LASF_Spec")
         (cond ((and (> record-id 99) (< record-id 355))
                (make-instance 'wpd-vlr :user-id user-id :record-id record-id
                                        :disk-size (+ (type-size 'variable-length-record-header)
                                                      (type-size 'waveform-packet-descriptor))
                                        :content (read-value 'waveform-packet-descriptor fd)))
               ((= record-id 4) :extra-bytes)))))

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
             (next-pos (+ (file-position fd) (record-length-after-header evlr)))
             (content (read-vlr-content fd (user-id evlr) (record-id evlr))))
        (push content res)
        (file-position fd next-pos)))))

(defun read-headers (fd)
  (let ((header (read-public-header fd)))
    (values header (read-vlrs header fd) (read-evlrs header fd))))

(defun write-vlr (stream vlr)
  (with-accessors ((record-id vlr-record-id)
                   (user-id vlr-user-id)
                   (disk-size vlr-disk-size)) vlr
    (let ((sz-after-header (- disk-size (type-size 'variable-length-record-header))))
      (cond ((string= user-id "LASF_Projection")
             (with-accessors ((directory geotiff-projection-vlr-directory)
                              (keys geotiff-projection-vlr-keys)) vlr
               (write-value 'variable-length-record-header stream
                            (make-instance 'variable-length-record-header
                                           :record-id record-id :user-id user-id
                                           :record-length-after-header sz-after-header))
               (write-value 'geokey-directory stream directory)
               (dolist (key keys)
                 (write-value 'geokey-key stream key))))
            ((string= user-id "LASF_Spec")
             (cond ((and (> record-id 99) (< record-id 355))
                    (with-accessors ((content wpd-vlr-content)) vlr
                      (write-value 'variable-length-record-header stream
                                   (make-instance 'variable-length-record-header
                                                  :record-id record-id
                                                  :user-id user-id
                                                  :record-length-after-header sz-after-header))
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
    ;; Just to be safe, enforce valid offset-to-point-data: update it
    ;; after having wrote header+vlrs and then rewrite the updated
    ;; header.
    (setf (offset-to-point-data pheader) (file-position stream))
    (write-public-header stream pheader)
    ;; ELVRS
    (file-position stream (start-of-evlrs pheader))
    (dolist (evlr evlrs)
      (write-evlr stream evlr))))

(define-binary-class legacy-point-data ()
  ((x :s32 0)
   (y :s32 0)
   (z :s32 0)
   (intensity :u16 0)
   (gloubiboulga :u8 0)
   (classification :u8 0)
   (scan-angle :s8 0)
   (user-data :u8 0)
   (point-source-id :u16 0)))

(defgeneric return-number (point))
(defgeneric (setf return-number) (value point))
(defgeneric number-of-returns (point))
(defgeneric (setf number-of-returns) (value point))
(defgeneric scan-direction (point))
(defgeneric (setf scan-direction) (value point))
(defgeneric edge-of-flight-line-p (point))
(defgeneric (setf edge-of-flight-line-p) (value point))

(defmethod return-number ((p legacy-point-data))
  (with-slots (gloubiboulga) p
    (ldb (byte 3 0) gloubiboulga)))

(defmethod (setf return-number) (value (p legacy-point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 3 0) gloubiboulga) value)))

(defmethod number-of-returns ((p legacy-point-data))
  (with-slots (gloubiboulga) p
    (ldb (byte 3 3) gloubiboulga)))

(defmethod (setf number-of-returns) (value (p legacy-point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 3 3) gloubiboulga) value)))

(defmethod scan-direction ((p legacy-point-data))
  (with-slots (gloubiboulga) p
    (if (zerop (ldb (byte 1 6) gloubiboulga))
        :negative-scan
        :positive-scan)))

(defmethod (setf scan-direction) (value (p legacy-point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 1 6) gloubiboulga) (ecase value
                                          (:positive-scan 1)
                                          (:negative-scan 0)))))

(defmethod edge-of-flight-line-p ((p legacy-point-data))
  (with-slots (gloubiboulga) p
    (= 1 (ldb (byte 1 7) gloubiboulga))))

(defmethod (setf edge-of-flight-line-p) (value (p legacy-point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 1 7) gloubiboulga) (if value 1 0))))

(defparameter *legacy-asprs-classification*
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

(defmethod classification ((p legacy-point-data))
  (with-slots (classification) p
    (ldb (byte 5 0) classification)))

(defmethod (setf classification) (value (p legacy-point-data))
  (with-slots (classification) p
    (setf (ldb (byte 5 0) classification) value)))

(defmethod human-readable-classification ((p legacy-point-data))
  (let ((value (classification p)))
    (if (< value (length *legacy-asprs-classification*))
        (nth value *legacy-asprs-classification*)
        :reserved)))

(defmethod (setf human-readable-classification) (value (p legacy-point-data))
  (let ((pos (position value *legacy-asprs-classification*)))
    (when pos
      (setf (classification p) pos))))

(define-binary-class point-data ()
  ((x :s32 0)
   (y :s32 0)
   (z :s32 0)
   (intensity :u16 0)
   (gloubiboulga :u16 0)
   (classification :u8 0)
   (user-data :u8 0)
   (scan-angle :s16 0)
   (point-source-id :u16 0)
   (gps-time :float64 0)))

(defmethod return-number ((p point-data))
  (with-slots (gloubiboulga) p
    (ldb (byte 4 0) gloubiboulga)))

(defmethod (setf return-number) (value (p point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 4 0) gloubiboulga) value)))

(defmethod number-of-returns ((p point-data))
  (with-slots (gloubiboulga) p
    (ldb (byte 4 4) gloubiboulga)))

(defmethod (setf number-of-returns) (value (p point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 4 4) gloubiboulga) value)))

(defmethod classification-flags ((p point-data))
  (with-slots (gloubiboulga) p
    (ldb (byte 4 8) gloubiboulga)))

(defmethod (setf classification-flags) (value (p point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 4 8) gloubiboulga) value)))

(defmethod scanner-channel ((p point-data))
  (with-slots (gloubiboulga) p
    (ldb (byte 2 12) gloubiboulga)))

(defmethod (setf scanner-channel) (value (p point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 2 12) gloubiboulga) value)))

(defmethod scan-direction ((p point-data))
  (with-slots (gloubiboulga) p
    (if (zerop (ldb (byte 1 14) gloubiboulga))
        :negative-scan
        :positive-scan)))

(defmethod (setf scan-direction) (value (p point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 1 14) gloubiboulga) (ecase value
                                           (:positive-scan 1)
                                           (:negative-scan 0)))))

(defmethod edge-of-flight-line-p ((p point-data))
  (with-slots (gloubiboulga) p
    (= 1 (ldb (byte 1 15) gloubiboulga))))

(defmethod (setf edge-of-flight-line-p) (value (p point-data))
  (with-slots (gloubiboulga) p
    (setf (ldb (byte 1 15) gloubiboulga) (if value 1 0))))

(defparameter *asprs-classification*
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
    :high-noise
    :overhead-structure
    :ignored-ground
    :snow
    :temporal-exclusion))

(defmethod classification ((p point-data))
  (with-slots (classification) p
    (cond ((< classification (length *asprs-classification*))
           (nth classification *asprs-classification*))
          ((< classification 64) :reserved)
          (t :user-definable))))

(defmethod (setf classification) (value (p point-data))
  (with-slots (classification) p
    (let ((pos (position value *asprs-classification*)))
      (when pos
        (setf classification pos)))))

(define-binary-class gps-mixin ()
  ((gps-time :float64)))

(define-binary-class color-mixin ()
  ((red :u16 0)
   (green :u16 0)
   (blue :u16 0)))

(defgeneric colorized-p (point)
  (:documentation "Is this point colorized?")
  (:method (point) nil))

(defmethod colorized-p ((point color-mixin)) t)

(define-binary-class nir-mixin ()
  ((nir :u16)))

(define-binary-class waveform-mixin ()
  ((waveform-packet-descriptor-index :u8)
   (byte-offset-to-waveform :u64)
   (waveform-packet-size :u32)
   (return-point-waveform-location :float32)
   (dx :float32)
   (dy :float32)
   (dz :float32)))

(defgeneric waveform-p (point)
  (:documentation "Is this point contains a waveform?")
  (:method (point) nil))

(defmethod waveform-p ((point waveform-mixin)) t)

(define-binary-class legacy-point-data-gps (gps-mixin legacy-point-data) ())
(define-binary-class legacy-point-data-color (color-mixin legacy-point-data) ())
(define-binary-class legacy-point-data-color-gps (color-mixin gps-mixin legacy-point-data) ())
(define-binary-class legacy-point-data-gps-waveform (waveform-mixin gps-mixin legacy-point-data) ())
(define-binary-class legacy-point-data-color-gps-waveform (waveform-mixin gps-mixin color-mixin legacy-point-data) ())

(define-binary-class point-data-color (color-mixin point-data) ())
(define-binary-class point-data-color-nir (nir-mixin point-data-color) ())
(define-binary-class point-data-waveform (waveform-mixin point-data) ())
(define-binary-class point-data-color-nir-waveform (waveform-mixin point-data-color-nir) ())

(defclass las ()
  ((%pathname :initarg :pathname :reader las-pathname)
   (%stream :initarg :stream :reader las-stream)
   (%wdp-stream :initarg :wdp-stream :initform nil :accessor las-wdp-stream
                :documentation "Stream to external Waveform Data Packets.")
   (%header :accessor las-public-header)
   (%point-class :initarg :point-class :accessor las-point-class)
   (%extra-bytes :initarg :extra-bytes :accessor las-extra-bytes)
   (%vlrecords :initform nil :accessor las-variable-length-records)
   (%evlrecords :initform nil :accessor las-extended-variable-length-records)))

(defun make-las (pathname stream point-class)
  (make-instance 'las :pathname pathname :stream stream :point-class point-class))

(defmethod initialize-instance :after ((object las) &key)
  "After las object creation, read slots' data from the input stream
or just instantiate slots in case of an output stream."
  (let ((stream (las-stream object)))
    (when (streamp stream)
      (with-accessors ((pathname las-pathname)
                       (wdp-stream las-wdp-stream)
                       (public-header las-public-header)
                       (vlrecords las-variable-length-records)
                       (point-class las-point-class)
                       (extra-bytes las-extra-bytes)
                       (evlrecords las-extended-variable-length-records)) object
        (cond ((input-stream-p stream)
               (multiple-value-bind (pheader vlrs evlrs) (read-headers stream)
                 ;; open external WDP if needed
                 (when (member 'external-wave-data (global-encoding pheader))
                   (let ((wdp-name (merge-pathnames (make-pathname :type "wdp") pathname)))
                     (if (probe-file wdp-name)
                         (setf wdp-stream (open wdp-name :element-type '(unsigned-byte 8)))
                         (error "Can't find ~a waveform file" wdp-name))))
                 ;; go to data point in stream
                 (file-position stream (offset-to-point-data pheader))
                 ;; fill missing slots
                 (setf public-header pheader
                       vlrecords vlrs
                       point-class (nth (point-data-format-id pheader) *point-data-classes*)
                       extra-bytes (- (point-data-record-length public-header)
                                      (type-size point-class))
                       evlrecords evlrs)))
              ((output-stream-p stream)
               (multiple-value-bind (doy year) (current-doy-year)
                 (let ((pheader (make-instance 'public-header
                                               :file-creation-doy doy
                                               :file-creation-year year
                                               :point-data-format-id (position point-class *point-data-classes*)
                                               :point-data-record-length (type-size point-class))))
                   (setf (header-size pheader) (type-size pheader)
                         public-header pheader)))))))))

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
                                     (* index (+ (type-size (las-point-class las))
                                                 (las-extra-bytes las)))))
  (read-point las :scale-p scale-p))

(defun update-bounding-box (header x y z)
  (macrolet ((update-slot (header slot)
               (let ((min (find-symbol (format nil "%MIN-~a" slot)))
                     (max (find-symbol (format nil "%MAX-~a" slot))))
                 `(progn
                    (when (or (not (slot-boundp ,header ',min))
                              (< ,slot (,min ,header)))
                      (setf (,min ,header) ,slot))
                    (when (or (not (slot-boundp ,header ',max))
                              (> ,slot (,max ,header)))
                      (setf (,max ,header) ,slot))))))
    (update-slot header x)
    (update-slot header y)
    (update-slot header z)))

(defun write-point (point las &key unscale-p)
  "Write a point in the given LAS. XXX position int the LAS stream
  should be correct."
  (with-accessors ((x x) (y y) (z z)) point
    ;; Update bounding box. Done before unscaling. The spec says: "The
    ;; max and min data fields are the actual unscaled extents of the
    ;; LAS point file data, specified in the coordinate system of the
    ;; LAS data." This was not clear to me but in others softwares LAS
    ;; output use a bounding box in the coordinate system unit.
    (update-bounding-box (las-public-header las) x y z)
    (when unscale-p
      (with-accessors ((x-scale x-scale) (x-offset x-offset)
                       (y-scale y-scale) (y-offset y-offset)
                       (z-scale z-scale) (z-offset z-offset)) (las-public-header las)
        (setf x (round (/ (- x x-offset) x-scale))
              y (round (/ (- y y-offset) y-scale))
              z (round (/ (- z z-offset) z-scale))))))
  (write-value (las-point-class las) (las-stream las) point))

(defun write-point-at (point index las &key unscale-p)
  "Write a point at a given index"
  (file-position (las-stream las) (+ (offset-to-point-data (las-public-header las))
                                     (* index (type-size (las-point-class las)))))
  (write-point point las :unscale-p unscale-p))

(defmacro make-get/set (fun-name low-level-name)
  (let ((las (gensym "LAS"))
        (value (gensym)))
    `(progn
       (defun ,fun-name (,las)
         (,low-level-name (las-public-header ,las)))
       (defun (setf ,fun-name) (,value ,las)
         (setf (,low-level-name (las-public-header ,las)) ,value)))))

(make-get/set max-x %max-x)
(make-get/set min-x %min-x)
(make-get/set max-y %max-y)
(make-get/set min-y %min-y)
(make-get/set max-z %max-z)
(make-get/set min-z %min-z)

(defclass waveform ()
  ((xs :initarg :xs :accessor waveform-xs)
   (ys :initarg :ys :accessor waveform-ys)
   (zs :initarg :zs :accessor waveform-zs)
   (times :initarg :times :accessor waveform-times)
   (intensities :initarg :intensities :accessor waveform-intensities))
  (:documentation "Spacially positionned waveform."))

(defun type-from-bits (bits)
  (ecase bits
    (8 :u8)
    (16 :u16)
    (32 :u32)
    (64 :u64)))

(defun %get-waveform-packet-descriptor-of-point (point las)
  (let ((record-id (+ 99 (waveform-packet-descriptor-index point)))
        (all-vlrs (append (las-variable-length-records las)
                          (las-extended-variable-length-records las))))
    (labels ((wpd-key (elt)
               (and (typep elt 'wpd-vlr)
                    (vlr-record-id elt))))
      (unless (= record-id 99)
        (wpd-vlr-content (find record-id all-vlrs :key #'wpd-key))))))

(defgeneric waveform-temporal-spacing-of-point (point las)
  (:documentation "Waveform temporal spacing in picoseconds (ps).")
  (:method (point las)
    "Defaults to something useful: 1000 ps is a 15cm grid"
    1000))

(defmethod waveform-temporal-spacing-of-point ((point waveform-mixin) las)
  (let ((wpd (%get-waveform-packet-descriptor-of-point point las)))
    (when wpd
      (temporal-sample-spacing wpd))))

(defgeneric waveform-of-point (point las))

(defmethod waveform-of-point ((point waveform-mixin) las)
  (let* ((header (las-public-header las))
         (stream (if (las-wdp-stream las) (las-wdp-stream las) (las-stream las)))
         (evlr-pos (if (las-wdp-stream las) 0 (start-of-evlrs header)))
         (wpd (%get-waveform-packet-descriptor-of-point point las)))
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
                 (setf (aref xs j) (+ (x point) (* (dx point) time))
                       (aref ys j) (+ (y point) (* (dy point) time))
                       (aref zs j) (+ (z point) (* (dz point) time))
                       (aref times j) (* j dt)
                       (aref intensities j) (read-value (type-from-bits bps) stream)))
            finally (return (make-instance 'waveform :xs xs :ys ys :zs zs :times times
                                                     :intensities intensities))))))

(defgeneric projection (las)
  (:documentation "Get/set EPSG projection of a LAS."))

(defmethod projection ((las las))
  (let ((vlr-projection (find "LASF_Projection" (las-variable-length-records las)
                              :key #'vlr-user-id :test #'string=)))
    (when vlr-projection
      (get-projection-code (geotiff-projection-vlr-keys vlr-projection)))))

(defmethod (setf projection) (epsg-code (las las))
  (with-accessors ((header las-public-header)
                   (vlrs las-variable-length-records)) las
    (let ((user-id "LASF_Projection"))
      (multiple-value-bind (directory keys) (make-projection-geokey epsg-code)
        (setf vlrs (cons
                    (make-instance 'geotiff-projection-vlr
                                   :record-id 34735 :user-id user-id
                                   :disk-size (+ (type-size 'variable-length-record-header)
                                                 (type-size directory)
                                                 (* (length keys) (type-size 'geokey-key)))
                                   :directory directory :keys keys)
                    (remove user-id vlrs :key #'vlr-user-id :test #'string=))
              ;; Update number of variable length record and offset to data point
              (number-of-variable-length-records header) (length vlrs)
              (offset-to-point-data header) (+ (header-size (las-public-header las))
                                               (reduce #'+ vlrs :key #'vlr-disk-size)))))))

(defun open-las-file (filename &key (direction :input) if-exists if-does-not-exist
                                    (point-class 'point-data))
  (let ((stream (open filename
                      :element-type '(unsigned-byte 8)
                      :direction direction
                      :if-exists if-exists :if-does-not-exist if-does-not-exist)))
    (make-las filename stream point-class)))

(defmacro with-las ((las filename &rest options) &body body)
  (let ((abortp (gensym)))
    `(let ((,las (open-las-file ,filename ,@options))
           (,abortp t))
       (unwind-protect
            (multiple-value-prog1
              (unwind-protect
                   (progn,@body)
                (when (las-wdp-stream ,las) (close (las-wdp-stream ,las))))
              (setq ,abortp nil))
         (when (las-stream ,las) (close (las-stream ,las) :abort ,abortp))))))

(make-get/set las-number-of-points number-of-points)
(make-get/set las-number-of-points-by-return number-of-points-by-return)
(make-get/set las-x-scale x-scale)
(make-get/set las-y-scale y-scale)
(make-get/set las-z-scale z-scale)
(make-get/set las-x-offset x-offset)
(make-get/set las-y-offset y-offset)
(make-get/set las-z-offset z-scale)

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

(defun write-test (lasfile &optional (nx 500) (ny 500))
  (with-las (las lasfile :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :point-class 'point-data-color)
    (setf (las-number-of-points las) (* nx ny)
          (las-number-of-points-by-return las) (vector (* nx ny) 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          ;; Lambert 93… because la France!
          (projection las) 2154)
    (loop with epsilon-pi = (/ pi 100)
          for x below nx
          for red = (rem x 256)
          for green = (rem (+ x 100) 256)
          for blue = (rem (+ x 200) 256)
          do (loop for y below ny
                   do (let ((z (round (* 10 (sin (* x epsilon-pi))
                                         (sin (* y epsilon-pi))))))
                        (write-point-at (make-instance (las-point-class las)
                                                       :x x :y y :z z
                                                       :red red :green green
                                                       :blue blue)
                                        (+ y (* ny x)) las))))
    ;; Bounding box has been updated during point writing so output
    ;; headers last.
    (write-headers las)))
