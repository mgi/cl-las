(in-package :las)

;; GeoTIFF configuration keys
(defparameter +gt-model-type-key+ 1024
  "This GeoKey defines the general type of model Coordinate system
  used, and to which the raster space will be transformed:unknown,
  Geocentric (rarely used), Geographic, Projected Coordinate System,
  or user-defined. If the coordinate system is a PCS, then only the
  PCS code need be specified. If the coordinate system does not fit
  into one of the standard registered PCS'S, but it uses one of the
  standard projections and datums, then its should be documented as a
  PCS model with \"user-defined\" type, requiring the specification of
  projection parameters, etc.")
(defparameter +gt-raster-type-key+ 1025
  "This establishes the Raster Space coordinate system used; there are
  currently only two, namely RasterPixelIsPoint and
  RasterPixelIsArea. No user-defined raster spaces are currently
  supported. For variance in imaging display parameters, such as pixel
  aspect-ratios, use the standard TIFF 6.0 device-space tags
  instead.")
(defparameter +gt-citation-key+ 1026
  "As with all the \"Citation\" GeoKeys, this is provided to give an
  ASCII reference to published documentation on the overall
  configuration of this GeoTIFF file.")

;; Geographic coordinate system parameter keys
(defparameter +geog-type-key+ 2048
  "This key may be used to specify the code for the geographic
  coordinate system used to map lat-long to a specific ellipsoid over
  the earth.")
(defparameter +geog-citation-key+ 2049
  "General citation and reference for all Geographic CS parameters.")
(defparameter +geog-geodetic-datum-key+ 2050
  "This key may be used to specify the horizontal datum, defining the
  size, position and orientation of the reference ellipsoid used in
  user-defined geographic coordinate systems.")
(defparameter +geog-prime-meridian-key+ 2051
  "Allows specification of the location of the Prime meridian for
  user-defined geographic coordinate systems. The default standard is
  Greenwich, England.")
(defparameter +geog-prime-meridian-long-key+ 2061
  "This key allows definition of user-defined Prime Meridians, the
  location of which is defined by its longitude relative to
  Greenwich.")
(defparameter +geog-linear-units-key+ 2052
  "Allows the definition of geocentric CS linear units for
  user-defined GCS.")
(defparameter +geog-linear-unit-size-key+ 2053
  "Allows the definition of user-defined linear geocentric units, as
  measured in meters.")
(defparameter +geog-angular-units-key+ 2054
  "Allows the definition of geocentric CS Linear units for
  user-defined GCS and for ellipsoids.")
(defparameter +geog-angular-unit-size-key+ 2055
  "Allows the definition of user-defined angular geographic units, as
  measured in radians.")
(defparameter +geog-ellipsoid-key+ 2056
  "This key may be used to specify the coded ellipsoid used in the
  geodetic datum of the Geographic Coordinate System.")
(defparameter +geog-semi-major-axis-key+ 2057
  "Allows the specification of user-defined Ellipsoid Semi-Major
  Axis (a).")
(defparameter +geog-semi-minor-axis-key+ 2058
  "Allows the specification of user-defined Ellipsoid Semi-Minor
  Axis (b).")
(defparameter +geog-inv-flattening-key+ 2059
  "Allows the specification of the inverse of user-defined Ellipsoid's
  flattening parameter (f). The eccentricity-squared e^2 of the
  ellipsoid is related to the non-inverted f by:

      e^2  = 2*f  - f^2")
(defparameter +geog-azimuth-units-key+ 2060
  "This key may be used to specify the angular units of measurement
  used to defining azimuths, in geographic coordinate systems. These
  may be used for defining azimuthal parameters for some projection
  algorithms, and may not necessarily be the same angular units used
  for lat-long.")

;; Projected coordinate system parameter keys
(defparameter +projected-cs-type-key+ 3072
  "This code is provided to specify the projected coordinate system.")
(defparameter +projected-cs-citation-key+ 3073
  "As with all the \"Citation\" GeoKeys, this is provided to give an
  ASCII reference to published documentation on the Projected
  Coordinate System particularly if this is a \"user-defined\" PCS.")

;; Projection definition keys
(defparameter +projection-key+ 3074
  "Allows specification of the coordinate transformation method and
  projection zone parameters. Note : when associated with an
  appropriate Geographic Coordinate System, this forms a Projected
  Coordinate System.")
(defparameter +proj-coord-trans-key+ 3075
  "Allows specification of the coordinate transformation method
  used. Note: this does not include the definition of the
  corresponding Geographic Coordinate System to which the projected CS
  is related; only the transformation method is defined here.")
(defparameter +proj-linear-units-key+ 3076
  "Defines linear units used by this projection.")
(defparameter +proj-linear-unit-size-key+ 3077
  "Defines size of user-defined linear units in meters.")
(defparameter +proj-std-parallel-1-key+ 3078
  "Latitude of primary Standard Parallel.")
(defparameter +proj-std-parallel-2-key+ 3079
  "Latitude of second Standard Parallel.")
(defparameter +proj-nat-origin-long-key+ 3080
  "Longitude of map-projection Natural origin.")
(defparameter +proj-nat-origin-lat-key+ 3081
  "Latitude of map-projection Natural origin.")
(defparameter +proj-false-easting-key+ 3082
  "Gives the easting coordinate of the map projection Natural
  origin.")
(defparameter +proj-false-northing-key+ 3083
  "Gives the northing coordinate of the map projection Natural
  origin.")
(defparameter +proj-false-origin-long-key+ 3084
  "Gives the longitude of the False origin.")
(defparameter +proj-false-origin-lat-key+ 3085
  "Gives the latitude of the False origin.")
(defparameter +proj-false-origin-easting-key+ 3086
  "Gives the easting coordinate of the false origin. This is NOT the
  False Easting, which is the easting attached to the Natural
  origin.")
(defparameter +proj-false-origin-northing-key+ 3087
  "Gives the northing coordinate of the False origin. This is NOT the
  False Northing, which is the northing attached to the Natural
  origin.")
(defparameter +proj-center-long-key+ 3088
  "Longitude of Center of Projection. Note that this is not
  necessarily the origin of the projection.")
(defparameter +proj-center-lat-key+ 3089
  "Latitude of Center of Projection. Note that this is not necessarily
  the origin of the projection.")
(defparameter +proj-center-easting-key+ 3090
  "Gives the easting coordinate of the center. This is NOT the False
  Easting.")
(defparameter +proj-false-origin-northing-key+ 3091
  "Gives the northing coordinate of the center. This is NOT the False
  Northing.")
(defparameter +proj-scale-at-nat-origin-key+ 3092
  "Scale at Natural Origin. This is a ratio, so no units are
  required.")
(defparameter +proj-scale-at-center-key+ 3093
  "Scale at Center. This is a ratio, so no units are required.")
(defparameter +proj-azimuth-angle-key+ 3094
  "Azimuth angle east of true north of the central line passing
  through the projection center (for elliptical (Hotine) Oblique
  Mercator). Note that this is the standard method of measuring
  azimuth, but is opposite the usual mathematical convention of
  positive indicating counter-clockwise.")
(defparameter +proj-straight-vert-pole-long-key+ 3095
  "Longitude at Straight Vertical Pole. For polar stereographic.")

;; Vertical CS parameter keys
(defparameter +vertical-cs-type-key+ 4096
  "This key may be used to specify the vertical coordinate system.")
(defparameter +vertical-citation-key+ 4097
  "This key may be used to document the vertical coordinate system
  used, and its parameters.")
(defparameter +vertical-datum-key+ 4098
  "This key may be used to specify the vertical datum for the vertical
  coordinate system.")
(defparameter +vertical-units-key+ 4099
  "This key may be used to specify the vertical units of measurement
  used in the geographic coordinate system, in cases where geographic
  CS's need to reference the vertical coordinate. This, together with
  the Citation key, comprise the only fully implemented keys in this
  section, at present.")

(defun get-projection-code (keys)
  (let ((model (find +gt-model-type-key+ keys :key #'key-id)))
    (when (and model
	       (zerop (tiff-tag-location model)))
      (ecase (value-offset model)
	((1 2) (get-cs-type keys))
	(3 ;; TODO geocentric
	 )))))

(defun get-cs-type (keys)
  (let ((type (find +projected-cs-type-key+ keys :key #'key-id)))
    (when (and type
	       (zerop (tiff-tag-location type)))
      (value-offset type))))

(defun make-projection-geokey (code)
  (let ((keys (list (make-instance 'geokey-key :key-id +gt-model-type-key+ :char-count 1 :tiff-tag-location 0 :value-offset 1)
		    (make-instance 'geokey-key :key-id +gt-raster-type-key+ :char-count 1 :tiff-tag-location 0 :value-offset 2)
		    (make-instance 'geokey-key :key-id +projected-cs-type-key+ :char-count 1 :tiff-tag-location 0 :value-offset code))))
    (values (make-instance 'geokey-directory :number-of-keys (length keys))
	    keys)))
