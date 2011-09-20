;;; -*- package: NOX; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  core-constants.lisp
;;;
;;;  This program is licensed under the terms of the GNU Lesser General Public License
;;;  as published by the Free Software Foundation, version 2.1 of the License. Note
;;;  however that a preamble attached below also applies to this program.
;;; :SEE ../src/LICENSE for details
;;;
;;;   Purpose: This file contains definitions for various constants used by the
;;;   RDF parser (mostly URIs). Given that the XML parser has to deal with the
;;;   issue of RDF M+S vagueness on the namespaces of RDF attributes (such as
;;;   "about"), the definitions in this file are in the NOX package.
;;;
;;;   Generally, I hate this stuff since I never seem to get the constant
;;;   definitions right vis-a-vis compile time vs. load-time. :-(
;;;


(in-package "NOX")


;;; --------------------------------------------------------------------------------------
;;;
;;;   HELPERS
;;;

(eval-when (:compile-toplevel :load-toplevel)
  (defequal -rdf-uri-  "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  (defequal -rdfs-uri- "http://www.w3.org/2000/01/rdf-schema#")
  (defequal -xsd-uri-  "http://www.w3.org/2001/XMLSchema#")
  (defequal -owl-uri-  "http://www.w3.org/2002/07/owl#")
  (defequal -daml-uri- "http://www.daml.org/2000/12/daml+oil#"))

(defmacro rdf-uri (string)  `(concatenate 'string -rdf-uri- ,string))
(defmacro rdfs-uri (string) `(concatenate 'string -rdfs-uri- ,string))
(defmacro xsd-uri (string)  `(concatenate 'string -xsd-uri- ,string))
(defmacro owl-uri (string)  `(concatenate 'string -owl-uri- ,string))
(defmacro daml-uri (string) `(concatenate 'string -daml-uri- ,string))

(defequal -alternate-rdf-uri-
  #."http://www.w3.org/TR/REC-rdf-syntax/")
(defequal -alternate-rdfs-uri-
  #."http://www.w3.org/TR/1999/PR-rdf-schema-19990303#")


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF M+S ATTRIBUTE URIS
;;;


(defequal -rdf-id-uri-              (rdf-uri "ID"))
(defequal -rdf-resource-uri-        (rdf-uri "resource"))
(defequal -rdf-about-uri-           (rdf-uri "about"))
(defequal -rdf-abouteach-uri-       (rdf-uri "aboutEach"))
(defequal -rdf-abouteachprefix-uri- (rdf-uri "aboutEachPrefix"))
(defequal -rdf-bagid-uri-           (rdf-uri "bagID"))
(defequal -rdf-parsetype-uri-       (rdf-uri "parseType"))
(defequal -rdf-datatype-uri-        (rdf-uri "datatype"))
(defequal -rdf-nodeid-uri-          (rdf-uri "nodeID"))
(defequal -xml-lang-attr-           "xml:lang")


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF M+S RESOURCE, PROPERTY, ETC. URIS
;;;


(defequal -rdf-description-uri- (rdf-uri "Description"))
(defequal -rdf-type-uri-        (rdf-uri "type"))
(defequal -rdf-rdf-uri-         (rdf-uri "RDF"))
(defequal -rdf-li-uri-          (rdf-uri "li"))
(defequal -rdf-statement-uri-   (rdf-uri "Statement"))
(defequal -rdf-subject-uri-     (rdf-uri "subject"))
(defequal -rdf-predicate-uri-   (rdf-uri "predicate"))
(defequal -rdf-object-uri-      (rdf-uri "object"))
(defequal -rdf-xmlliteral-uri-  (rdf-uri "XMLLiteral"))
(defequal -rdf-bag-uri-         (rdf-uri "Bag"))
(defequal -rdf-seq-uri-         (rdf-uri "Seq"))
(defequal -rdf-alt-uri-         (rdf-uri "Alt"))
(defequal -rdf-list-uri-        (rdf-uri "List"))
(defequal -rdf-first-uri-       (rdf-uri "first"))
(defequal -rdf-rest-uri-        (rdf-uri "rest"))
(defequal -rdf-nil-uri-         (rdf-uri "nil"))


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF SCHEMA URIS
;;;


(defequal -rdfs-resource-uri-           (rdfs-uri "Resource"))
(defequal -rdfs-class-uri-              (rdfs-uri "Class"))
(defequal -rdfs-subclassof-uri-         (rdfs-uri "subClassOf"))
(defequal -rdfs-subpropertyof-uri-      (rdfs-uri "subPropertyOf"))
(defequal -rdfs-seealso-uri-            (rdfs-uri "seeAlso"))
(defequal -rdfs-isdefinedby-uri-        (rdfs-uri "isDefinedBy"))
(defequal -rdfs-constraintresource-uri- (rdfs-uri "ConstraintResource"))
(defequal -rdfs-constraintproperty-uri- (rdfs-uri "ConstraintProperty"))
(defequal -rdfs-range-uri-              (rdfs-uri "range"))
(defequal -rdfs-domain-uri-             (rdfs-uri "domain"))
(defequal -rdfs-comment-uri-            (rdfs-uri "comment"))
(defequal -rdfs-label-uri-              (rdfs-uri "label"))
(defequal -rdfs-literal-uri-            (rdfs-uri "Literal"))
(defequal -rdfs-datatype-uri-           (rdfs-uri "Datatype"))
(defequal -rdfs-container-uri-          (rdfs-uri "Container "))
(defequal -rdfs-member-uri-             (rdfs-uri "member"))


;;; --------------------------------------------------------------------------------------
;;;
;;;   XSD URIS
;;;


(defequal -xsd-string-uri-             (xsd-uri "string"))
(defequal -xsd-boolean-uri-            (xsd-uri "boolean"))
;;(defequal -xsd-decimal-uri-            (xsd-uri "decimal"))
(defequal -xsd-float-uri-              (xsd-uri "float"))
(defequal -xsd-double-uri-             (xsd-uri "double"))
(defequal -xsd-datetime-uri-           (xsd-uri "dateTime"))
;;(defequal -xsd-time-uri-               (xsd-uri "time"))
(defequal -xsd-date-uri-               (xsd-uri "date"))
;;(defequal -xsd-gyearmonth-uri-         (xsd-uri "gYearMonth"))
;;(defequal -xsd-gyear-uri-              (xsd-uri "gYear"))
;;(defequal -xsd-gmonthday-uri-          (xsd-uri "gMonthDay"))
;;(defequal -xsd-gday-uri-               (xsd-uri "gDay"))
;;(defequal -xsd-gmonth-uri-             (xsd-uri "gMonth"))
;;(defequal -xsd-hexbinary-uri-          (xsd-uri "hexBinary"))
;;(defequal -xsd-base64binary-uri-       (xsd-uri "base64Binary"))
;;(defequal -xsd-anyuri-uri-             (xsd-uri "anyURI"))
(defequal -xsd-normalizedstring-uri-   (xsd-uri "normalizedString"))
;;(defequal -xsd-token-uri-              (xsd-uri "token"))
;;(defequal -xsd-language-uri-           (xsd-uri "language"))
;;(defequal -xsd-nmtoken-uri-            (xsd-uri "NMTOKEN"))
;;(defequal -xsd-name-uri-               (xsd-uri "Name"))
;;(defequal -xsd-ncname-uri-             (xsd-uri "NCName"))
(defequal -xsd-integer-uri-            (xsd-uri "integer"))
;;(defequal -xsd-nonpositiveinteger-uri- (xsd-uri "nonPositiveInteger"))
;;(defequal -xsd-negativeinteger-uri-    (xsd-uri "negativeInteger"))
;;(defequal -xsd-long-uri-               (xsd-uri "long"))
(defequal -xsd-int-uri-                (xsd-uri "int"))
;;(defequal -xsd-short-uri-              (xsd-uri "short"))
;;(defequal -xsd-byte-uri-               (xsd-uri "byte"))
;;(defequal -xsd-nonnegativeinteger-uri- (xsd-uri "nonNegativeInteger"))
;;(defequal -xsd-unsignedlong-uri-       (xsd-uri "unsignedLong"))
;;(defequal -xsd-unsignedint-uri-        (xsd-uri "unsignedInt"))
;;(defequal -xsd-unsignedshort-uri-      (xsd-uri "unsignedShort"))
;;(defequal -xsd-unsignedbyte-uri-       (xsd-uri "unsignedByte"))
;;(defequal -xsd-positiveinteger-uri-    (xsd-uri "positiveInteger"))



;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF ATTRIBUTE LISTS
;;;


(defequal -rdf-attrs- `(,-rdf-id-uri-
                        ,-rdf-resource-uri-
                        ,-rdf-about-uri-
                        ,-rdf-abouteach-uri-
                        ,-rdf-abouteachprefix-uri-
                        ,-rdf-bagid-uri-
                        ,-rdf-parsetype-uri-
                        ,-rdf-datatype-uri-
                        ,-rdf-nodeid-uri-
                        ,-xml-lang-attr-))

(defequal -rdf-attr-map- `((,"ID"              . ,-rdf-id-uri-)
                           (,"resource"        . ,-rdf-resource-uri-)
                           (,"about"           . ,-rdf-about-uri-)
                           (,"aboutEach"       . ,-rdf-abouteach-uri-)
                           (,"aboutEachPrefix" . ,-rdf-abouteachprefix-uri-)
                           (,"bagID"           . ,-rdf-bagid-uri-)
                           (,"parseType"       . ,-rdf-parsetype-uri-)
                           (,"datatype"        . ,-rdf-datatype-uri-)
                           (,"nodeID"          . ,-rdf-nodeid-uri-)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   OWL URIS
;;;


(defequal -owl-list-uri-      (owl-uri "List"))
(defequal -owl-first-uri-     (owl-uri "first"))
(defequal -owl-rest-uri-      (owl-uri "rest"))
(defequal -owl-nil-uri-       (owl-uri "nil"))
(defequal -owl-imports-uri-   (owl-uri "imports"))


;;; --------------------------------------------------------------------------------------
;;;
;;;   DAML+OIL URIS
;;;


(defequal -daml-list-uri-  (daml-uri "List"))
(defequal -daml-first-uri- (daml-uri "first"))
(defequal -daml-rest-uri-  (daml-uri "rest"))
(defequal -daml-nil-uri-   (daml-uri "nil"))
