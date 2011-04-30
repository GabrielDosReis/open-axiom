;; Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
;; All rights reserved.
;; Copyright (C) 2007-2010, Gabriel Dos Reis.
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;     - Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;     - Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in
;;       the documentation and/or other materials provided with the
;;       distribution.
;;
;;     - Neither the name of The Numerical Algorithms Group Ltd. nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;; In order to understand this program you need to understand some details
;; of the structure of the databases it reads. Axiom has 5 databases,
;; the interp.daase, operation.daase, category.daase, compress.daase, and
;; browse.daase. The compress.daase is special and does not follow the
;; normal database format.
;;
;; This documentation refers to KAF files which are random access files.
;; NRLIB files are KAF files (look for NRLIB/index.KAF)
;; The format of a random access file is
;; \begin{verbatim}
;; byte-offset-of-key-table
;; first-entry
;; second-entry
;; ...
;; last-entry
;; ((key1 . first-entry-byte-address)
;;  (key2 . second-entry-byte-address)
;;  ...
;;  (keyN . last-entry-byte-address))
;; \end{verbatim}
;; The key table is a standard lisp alist.
;;
;; To open a database you fetch the first number, seek to that location,
;; and (read) which returns the key-data alist. To look up data you
;; index into the key-data alist, find the ith-entry-byte-address,
;; seek to that address, and (read).
;;
;; For instance, see src/share/algebra/USERS.DAASE/index.KAF
;;
;; One existing optimization is that if the data is a simple thing like a
;; symbol then the nth-entry-byte-address is replaced by immediate data.
;;
;; Another existing one is a compression algorithm applied to the
;; data so that the very long names don't take up so much space.
;; We could probably remove the compression algorithm as 64k is no
;; longer considered 'huge'. The database-abbreviation routine
;; handles this on read and write-compress handles this on write.
;; The squeeze routine is used to compress the keys, the unsqueeze
;; routine uncompresses them. Making these two routines disappear
;; should remove all of the compression.
;;
;; Indeed, a faster optimization is to simply read the whole database
;; into the image before it is saved. The system would be easier to
;; understand and the interpreter would be faster.
;;
;; The system uses another optimization: database contains a stamp
;; (consisting of offset to the main list and build time).  Before
;; saving the image selected data is fetched to memory.  When the
;; saved image starts it checks if the stamp of saved data matches
;; in-core data -- in case of agreement in-core data is used.
;; Parts of the datatabase which was not pre-loaded is still
;; (lazily) fetched from the filesystem.
;;
;; Database files are very similar to KAF files except that there
;; is an optimization (currently broken) which makes the first
;; item a pair of two numbers. The first number in the pair is
;; the offset of the key-value table, the second is a time stamp.
;; If the time stamp in the database matches the time stamp in
;; the image the database is not needed (since the internal hash
;; tables already contain all of the information). When the database
;; is built the time stamp is saved in both the gcl image and the
;; database.


;;TTT 7/2/97
; Regarding the 'ancestors field for a category: At database build
; time there exists a *ancestors-hash* hash table that gets filled
; with CATEGORY (not domain) ancestor information. This later provides
; the information that goes into interp.daase This *ancestors-hash*
; does not exist at normal runtime (it can be made by a call to
; genCategoryTable). Note that the ancestor information in
; *ancestors-hash* (and hence interp.daase) involves #1, #2, etc
; instead of R, Coef, etc. The latter thingies appear in all
; .NRLIB/index.KAF files. So we need to be careful when we )lib
; categories and update the ancestor info.


; This file contains the code to build, open and access the .DAASE
; files this file contains the code to )library NRLIBS and asy files

; There is a major issue about the data that resides in these
; databases.  the fundamental problem is that the system requires more
; information to build the databases than it needs to run the
; interpreter.  in particular, MODEMAP.DAASE is constructed using
; properties like "modemaps" but the interpreter will never ask for
; this information.

; So, the design is as follows:
;  first, the MODEMAP.DAASE needs to be built. this is done by doing
; a )library on ALL of the NRLIB files that are going into the system.
; this will bring in "modemap" information and add it to the
; *modemaps-hash* hashtable.
;  next, database build proceeds, accessing the "modemap" property
; from the hashtables. once this completes this information is never
; used again.
;  next, the interp.daase database is built. this contains only the
; information necessary to run the interpreter. note that during the
; running of the interpreter users can extend the system by do a
; )library on a new NRLIB file. this will cause fields such as "modemap"
; to be read and hashed.

; In the old system each constructor (e.g. LIST) had one library directory
; (e.g. LIST.NRLIB). this directory contained a random access file called
; the index.KAF file. the interpreter needed this KAF file at runtime for
; two entries, the operationAlist and the ConstructorModemap.
; during the redesign for the new compiler we decided to merge all of
; these .NRLIB/index.KAF files into one database, INTERP.DAASE.
; requests to get information from this database are intended to be
; cached so that multiple references do not cause additional disk i/o.
; this database is left open at all times as it is used frequently by
; the interpreter. one minor complication is that newly compiled files
; need to override information that exists in this database.
;   The design calls for constructing a random read (KAF format) file
; that is accessed by functions that cache their results. when the
; database is opened the list of constructor-index pairs is hashed
; by constructor name. a request for information about a constructor
; causes the information to replace the index in the hash table. since
; the index is a number and the data is a non-numeric sexpr there is
; no source of confusion about when the data needs to be read.
;
; The format of this new database is as follows:
;
;first entry:
; an integer giving the byte offset to the constructor alist
; at the bottom of the file
;second and subsequent entries (one per constructor)
; (operationAlist)
; (constructorModemap)
; ....
;last entry: (pointed at by the first entry)
; an alist of (constructor . index) e.g.
;  ( (PI offset-of-operationAlist offset-of-constructorModemap)
;   (NNI offset-of-operationAlist offset-of-constructorModemap)
;    ....)
; This list is read at open time and hashed by the car of each item.

; the system has been changed to use the property list of the
; symbols rather than hash tables. since we already hashed once
; to get the symbol we need only an offset to get the property
; list. this also has the advantage that eq hash tables no longer
; need to be moved during garbage collection.
;  there are 3 potential speedups that could be done. the best
; would be to use the value cell of the symbol rather than the
; property list but i'm unable to determine all uses of the
; value cell at the present time.
;  a second speedup is to guarantee that the property list is
; a single item, namely the database structure. this removes
; an assoc but leaves one open to breaking the system if someone
; adds something to the property list. this was not done because
; of the danger mentioned.
;  a third speedup is to make the getdatabase call go away, either
; by making it a macro or eliding it entirely. this was not done
; because we want to keep the flexibility of changing the database
; forms.

; the new design does not use hash tables. the database structure
; contains an entry for each item that used to be in a hash table.
; initially the structure contains file-position pointers and
; these are replaced by real data when they are first looked up.
; the database structure is kept on the property list of the
; constructor, thus, (get '|DenavitHartenbergMatrix| 'database)
; will return the database structure object.

; each operation has a property on its symbol name called 'operation
; which is a list of all of the signatures of operations with that name.

; -- tim daly

(import-module "macros")
(in-package "AxiomCore")
(import-module "foam_l")
(in-package "BOOT")

(defstruct database
 abbreviation               ; interp.
 ancestors                  ; interp.
 constructor                ; interp.
 constructorcategory        ; interp.
 constructorkind            ; interp.
 constructormodemap         ; interp.
 cosig                      ; interp.
 defaultdomain              ; interp.
 modemaps                   ; interp.
 niladic                    ; interp.
 object                     ; interp.
 operationalist             ; interp.
 documentation              ; browse.
 constructorform            ; browse.
 attributes                 ; browse.
 predicates                 ; browse.
 sourcefile                 ; browse.
 parents                    ; browse.
 users                      ; browse.
 dependents                 ; browse.
 superdomain                ; interp.
 spare                      ; superstition
 ) ; database structure

; there are only a small number of domains that have default domains.
; rather than keep this slot in every domain we maintain a list here.

(defvar *defaultdomain-list* '(
  (|MultisetAggregate| |Multiset|)
  (|FunctionSpace| |Expression|)
  (|AlgebraicallyClosedFunctionSpace| |Expression|)
  (|ThreeSpaceCategory| |ThreeSpace|)
  (|DequeueAggregate| |Dequeue|)
  (|ComplexCategory| |Complex|)
  (|LazyStreamAggregate| |Stream|)
  (|AssociationListAggregate| |AssociationList|)
  (|QuaternionCategory| |Quaternion|)
  (|PriorityQueueAggregate| |Heap|)
  (|PointCategory| |Point|)
  (|PlottableSpaceCurveCategory| |Plot3D|)
  (|PermutationCategory| |Permutation|)
  (|StringCategory| |String|)
  (|FileNameCategory| |FileName|)
  (|OctonionCategory| |Octonion|)))

; this hash table is used to answer the question "does domain x
; have category y?". this is answered by constructing a pair of
; (x . y) and doing an equal hash into this table.

(defvar *operation-hash* nil
  "given an operation name, what are its modemaps?")

(defvar *miss* nil
  "if true print out cache misses on getdatabase calls")

   ; note that constructorcategory information need only be kept for
   ; items of type category. this will be fixed in the next iteration
   ; when the need for the various caches are reviewed

   ; note that the *modemaps-hash* information does not need to be kept
   ; for system files. these are precomputed and kept in modemap.daase
   ; however, for user-defined files these are needed.
   ; currently these are added to the database for 2 reasons:
   ;  there is a still-unresolved issue of user database extensions
   ;  this information is used during database build time



; this are the streams for the databases. they are always open.
; there is an optimization for speeding up system startup. if the
; database is opened and the ..-stream-stamp* variable matches the
; position information in the database then the database is NOT
; read in and is assumed to match the in-core version

(defvar *compressvector* nil
  "a vector of things to compress in the databases")

(defvar *compressVectorLength* 0
  "length of the compress vector")

(defvar *compress-stream* nil 
  "an stream containing the compress vector")

(defvar *compress-stream-stamp* 0 
  "*compress-stream* (position . time)")

(defvar *interp-stream* nil
  "an open stream to the interpreter database")

(defvar *interp-stream-stamp* 0
  "*interp-stream* (position . time)")

; this is indexed by operation, not constructor
(defvar *operation-stream*
  nil "the stream to operation.daase")

(defvar *operation-stream-stamp* 0
  "*operation-stream* (position . time)")

(defvar *browse-stream* nil
  "an open stream to the browser database")

(defvar *browse-stream-stamp* 0
  "*browse-stream* (position . time)")

; this is indexed by (domain . category)
(defvar *category-stream* nil
  "an open stream to the category table")

(defvar *category-stream-stamp* 0 
  "*category-stream* (position . time)")

(defvar *allconstructors* nil
  "a list of all the constructors in the system")

(defvar *allOperations* nil
  "a list of all the operations in the system")

(defvar *asharpflags*
  "-O -laxiom -Fasy -Flsp" "library compiler flags")

(defvar |$ConstructorCache| nil)

(defun asharp (file &optional (flags *asharpflags*))
 "call the asharp compiler"
 (|runProgram|
   (concatenate 'string (|systemRootDirectory|) "/compiler/bin/axiomxl")
    (list flags file)))


(defun |closeAllDatabaseStreams| nil
  (close *interp-stream*)
  (close *operation-stream*)
  (close *category-stream*)
  (close *browse-stream*))


(defun |fillDatabasesInCore| nil
 "set all -hash* to clean values. used to clean up core before saving system"
 (setq *hascategory-hash* (make-hash-table :test #'equal))
 (setq *operation-hash* (make-hash-table))
 (setq *allconstructors* nil)
 (setq *compressvector* nil)
 (setq *compress-stream-stamp* '(0 . 0))
 (compressopen)
 (setq *interp-stream-stamp* '(0 . 0))
 (interpopen)
 (setq *operation-stream-stamp* '(0 . 0))
 (operationopen)
 (setq *browse-stream-stamp* '(0 . 0))
 (browseopen)
 (setq *category-stream-stamp* '(0 . 0))
 (categoryopen) ;note: this depends on constructorform in browse.daase
 (unless |$buildingSystemAlgebra|
   (initial-getdatabase))
#+:AKCL (gbc t)
)

(defun initial-getdatabase ()
  "fetch data we want in the saved system"
  (let (hascategory constructormodemapAndoperationalist operation constr)
    (when |$verbose|
      (format t "Initial getdatabase~%"))
    (setq hascategory '(
			(|Equation| . |Ring|)
			(|Expression| . |CoercibleTo|)
			(|Expression| . |CommutativeRing|)
			(|Expression| . |IntegralDomain|)
			(|Expression| . |Ring|)
			(|Float| . |RetractableTo|)
			(|Fraction| . |Algebra|) 
			(|Fraction| . |CoercibleTo|)
			(|Fraction| . |OrderedSet|)
			(|Fraction| . |RetractableTo|)
			(|Integer| . |Algebra|) 
			(|Integer| . |CoercibleTo|)
			(|Integer| . |ConvertibleTo|)
			(|Integer| . |LinearlyExplicitRingOver|)
			(|Integer| . |RetractableTo|)
			(|List| . |CoercibleTo|)
			(|List| . |FiniteLinearAggregate|)
			(|List| . |OrderedSet|)
			(|Polynomial| . |CoercibleTo|) 
			(|Polynomial| . |CommutativeRing|)
			(|Polynomial| . |ConvertibleTo|)
			(|Polynomial| . |OrderedSet|)
			(|Polynomial| . |RetractableTo|)
			(|Symbol| . |CoercibleTo|) 
			(|Symbol| . |ConvertibleTo|)
			(|Variable| . |CoercibleTo|)))
    (dolist (pair hascategory)
      (|constructorHasCategoryFromDB| pair))
    (setq constructormodemapAndoperationalist 
	  '(|BasicOperator| 
	    |Boolean|
	    |CardinalNumber|
	    |Color|
	    |Complex|
	    |Database|
	    |Equation| 
	    |EquationFunctions2|
	    |Expression|
	    |Float| 
	    |Fraction|
	    |FractionFunctions2|
	    |Integer| 
	    |IntegralDomain|
	    |Kernel|
	    |List|
	    |Matrix|
	    |MappingPackage1|
	    |Operator|
	    |OutputForm|
	    |NonNegativeInteger|
	    |ParametricPlaneCurve|
	    |ParametricSpaceCurve|
	    |Point| 
	    |Polynomial|
	    |PolynomialFunctions2|
	    |PositiveInteger|
	    |Ring|
	    |SetCategory| 
	    |SegmentBinding|
	    |SegmentBindingFunctions2|
	    |DoubleFloat|
	    |SparseMultivariatePolynomial|
	    |SparseUnivariatePolynomial|
	    |Segment|
	    |String|
	    |Symbol|
	    |UniversalSegment|
	    |Variable|
	    |Vector|))
    (dolist (con constructormodemapAndoperationalist)
      (|getConstructorModemapFromDB| con)
      (|getConstructorOperationsFromDB| con))
    (setq operation 
	  '(|+| |-| |*| |/| |**| 
	    |coerce| |convert| |elt| |equation|
	    |float| |sin| |cos| |map| |SEGMENT|))
    (dolist (op operation)
      (|getOperationFromDB| op))
    (setq constr
	  '( ;these are sorted least-to-most freq. delete early ones first
	    |Factored| 
	    |SparseUnivariatePolynomialFunctions2|
	    |TableAggregate&|
	    |RetractableTo&|
	    |RecursiveAggregate&|
	    |UserDefinedPartialOrdering|
	    |None|
	    |UnivariatePolynomialCategoryFunctions2|
	    |IntegerPrimesPackage|
	    |SetCategory&|
	    |IndexedExponents| 
	    |QuotientFieldCategory&|
	    |Polynomial|
	    |EltableAggregate&|
	    |PartialDifferentialRing&| 
	    |Set|
	    |UnivariatePolynomialCategory&|
	    |FlexibleArray|
	    |SparseMultivariatePolynomial|
	    |PolynomialCategory&|
	    |DifferentialExtension&|
	    |IndexedFlexibleArray|
	    |AbelianMonoidRing&|
	    |FiniteAbelianMonoidRing&|
	    |DivisionRing&| 
	    |FullyLinearlyExplicitRingOver&|
	    |IndexedVector|
	    |IndexedOneDimensionalArray| 
	    |LocalAlgebra|
	    |Localize|
	    |Boolean|
	    |Field&|
	    |Vector|
	    |IndexedDirectProductObject|
	    |Aggregate&|
	    |PolynomialRing|
	    |FreeModule|
	    |IndexedDirectProductAbelianGroup|
	    |IndexedDirectProductAbelianMonoid|
	    |SingletonAsOrderedSet|
	    |SparseUnivariatePolynomial|
	    |Fraction| 
	    |Collection&|
	    |HomogeneousAggregate&|
	    |RepeatedSquaring|
	    |IntegerNumberSystem&|
	    |AbelianSemiGroup&|
	    |AssociationList|
	    |OrderedRing&| 
	    |SemiGroup&|
	    |Symbol|
	    |UniqueFactorizationDomain&|
	    |EuclideanDomain&|
	    |IndexedAggregate&|
	    |GcdDomain&|
	    |IntegralDomain&|
	    |DifferentialRing&|
	    |Monoid&|
	    |Reference|
	    |UnaryRecursiveAggregate&| 
	    |OrderedSet&|
	    |AbelianGroup&|
	    |Algebra&|
	    |Module&| 
	    |Ring&| 
	    |StringAggregate&|
	    |AbelianMonoid&|
	    |ExtensibleLinearAggregate&|
	    |PositiveInteger| 
	    |StreamAggregate&|
	    |IndexedString|
	    |IndexedList|
	    |ListAggregate&|
	    |LinearAggregate&|
	    |Character| 
	    |String|
	    |NonNegativeInteger|
	    |SingleInteger|
	    |OneDimensionalArrayAggregate&|
	    |FiniteLinearAggregate&|
	    |PrimitiveArray|
	    |Integer| 
	    |List|
	    |OutputForm|))
    (dolist (con constr)
      (let ((c (|getSystemModulePath| 
		(string (|getConstructorAbbreviationFromDB| con)))))
	(when |$verbose|
	  (format t "   preloading ~a.." c))
	(if (probe-file c)
	    (progn
	      (put con 'loaded c)
	      (|loadModule| c con)
	      (when |$verbose|
		(format t "loaded.~%")))
	  (when |$verbose|
	    (format t "skipped.~%")))))
    (when |$verbose|
      (format t "~%"))))

; format of an entry in interp.daase:
;  (constructor-name
;    operationalist
;    constructormodemap
;    modemaps            -- this should not be needed. eliminate it.
;    object              -- the name of the object file to load for this con.
;    constructorcategory -- note that this info is the cadar of the
;         constructormodemap for domains and packages so it is stored
;         as NIL for them. it is valid for categories.
;    niladic             -- t or nil directly
;    abbrev              -- kept directly
;    cosig               -- kept directly
;    constructorkind     -- kept directly
;    defaultdomain       -- a short list, for %i
;    ancestors           -- used to compute new category updates
;    superdomain         -- valid for domain, NIL for category and package.
;  )
(defun interpOpen ()
  "open the interpreter database and hash the keys"
  (let (constructors pos stamp dbstruct)
    (setq *interp-stream* (open (|pathToDatabase| "interp.daase")))
    (setq stamp (read *interp-stream*))
    (unless (equal stamp *interp-stream-stamp*)
      (when |$verbose|
	(format t "   Re-reading interp.daase"))
      (setq *interp-stream-stamp* stamp)
      (setq pos (car stamp))
      (file-position *interp-stream* pos)
      (setq constructors (read *interp-stream*))
      (dolist (item constructors)
	(setq item (unsqueeze item))
	(setq *allconstructors* (adjoin (first item) *allconstructors*))
	(setq dbstruct (make-database))
	(setf (get (car item) 'database) dbstruct)
	(setf (database-operationalist dbstruct) (second item))
	(setf (database-constructormodemap dbstruct) (third item))
	(setf (database-modemaps dbstruct) (fourth item))
	(setf (database-object dbstruct) (fifth item))
	(setf (database-constructorcategory dbstruct) (sixth item))
	(setf (database-niladic dbstruct) (seventh item))
	(setf (database-abbreviation dbstruct) (eighth item))
	(setf (get (eighth item) 'abbreviationfor) (first item)) ;invert
	(setf (database-cosig dbstruct) (ninth item))
	(setf (database-constructorkind dbstruct) (tenth item))
	(setf (database-ancestors dbstruct) (nth 11 item))
	(setf (database-superdomain dbstruct) (nth 12 item))
	))
    
    (format t "~&")))

; this is an initialization function for the constructor database
; it sets up 2 hash tables, opens the database and hashes the index values

; there is a slight asymmetry in this code. sourcefile information for
; system files is only the filename and extension. for user files it
; contains the full pathname. when the database is first opened the
; sourcefile slot contains system names. the lookup function
; has to prefix the $spadroot information if the directory-namestring is
; null (we don't know the real root at database build time).
; a object-hash table is set up to look up nrlib and ao information.
; this slot is empty until a user does a )library call. we remember
; the location of the nrlib or ao file for the users local library
; at that time. a NIL result from this probe means that the
; library is in the system-specified place. when we get into multiple
; library locations this will also contain system files.


; format of an entry in browse.daase:
; ( constructorname
;     sourcefile
;     constructorform
;     documentation
;     attributes
;     predicates
; )

(defun browseOpen ()
  "open the constructor database and hash the keys"
  (let (constructors pos stamp dbstruct)
    (setq *browse-stream* (open (|pathToDatabase| "browse.daase")))
    (setq stamp (read *browse-stream*))
    (unless (equal stamp *browse-stream-stamp*)
      (when |$verbose|
	(format t "   Re-reading browse.daase"))
      (setq *browse-stream-stamp* stamp)
      (setq pos (car stamp))
      (file-position *browse-stream* pos)
      (setq constructors (read *browse-stream*))
      (dolist (item constructors)
	(setq item (unsqueeze item))
	(unless (setq dbstruct (get (car item) 'database))
	  (format t "browseOpen:~%")
	  (format t "the browse database contains a contructor ~a~%" item)
	  (format t "that is not in the interp.daase file. we cannot~%")
	  (format t "get the database structure for this constructor and~%")
	  (warn "will create a new one~%")
	  (setf (get (car item) 'database) (setq dbstruct (make-database)))
	  (setq *allconstructors* (adjoin item *allconstructors*)))
	(setf (database-sourcefile dbstruct) (second item))
	(setf (database-constructorform dbstruct) (third item))
	(setf (database-documentation dbstruct) (fourth item))
	(setf (database-attributes dbstruct) (fifth item))
	(setf (database-predicates dbstruct) (sixth item))
	(setf (database-parents dbstruct) (seventh item))))
    (format t "~&")))

(defun categoryOpen ()
  "open category.daase and hash the keys"
  (let (pos keys stamp)
    (setq *category-stream* (open (|pathToDatabase| "category.daase")))
    (setq stamp (read *category-stream*))
    (unless (equal stamp *category-stream-stamp*)
      (when |$verbose|
	(format t "   Re-reading category.daase"))
      (setq *category-stream-stamp* stamp)
      (setq pos (car stamp))
      (file-position *category-stream* pos)
      (setq keys (read *category-stream*))
      (setq *hasCategory-hash* (make-hash-table :test #'equal))
      (dolist (item keys)
	(setq item (unsqueeze item))
	(setf (gethash (first item) *hasCategory-hash*) (second item))))
    (format t "~&")))

(defun operationOpen ()
  "read operation database and hash the keys"
  (let (operations pos stamp)
    (setq *operation-stream* (open (|pathToDatabase| "operation.daase")))
    (setq stamp (read *operation-stream*))
    (unless (equal stamp *operation-stream-stamp*)
      (when |$verbose|
	(format t "   Re-reading operation.daase"))
      (setq *operation-stream-stamp* stamp)
      (setq pos (car stamp))
      (file-position *operation-stream* pos)
      (setq operations (read *operation-stream*))
      (dolist (item operations)
	(setq item (unsqueeze item))
	(setf (gethash (car item) *operation-hash*) (cdr item))))
    (format t "~&")))

(defun addoperations (constructor oldmaps)
  "add ops from a )library domain to *operation-hash*"
  (declare (special *operation-hash*))
  (dolist (map oldmaps) ; out with the old
    (let (oldop op)
      (setq op (car map))
      (setq oldop (|getOperationFromDB| op))
      (setq oldop (delete (cdr map) oldop :test #'equal))
      (setf (gethash op *operation-hash*) oldop)))
  (dolist (map (|getOperationModemapsFromDB| constructor)) ; in with the new
    (let (op newmap)
      (setq op (car map))
      (setq newmap (|getOperationFromDB| op))
      (setf (gethash op *operation-hash*) (cons (cdr map) newmap)))))

(defun showdatabase (constructor)
  (format t "~&~a: ~a~%" 'constructorkind
	  (|getConstructorKindFromDB| constructor))
  (format t "~a: ~a~%" 'cosig
	  (|getDualSignatureFromDB| constructor))
  (format t "~a: ~a~%" 'operation
	  (|getOperationFromDB| constructor))
  (format t "~a: ~%" 'constructormodemap)
  (pprint (|getConstructorModemapFromDB| constructor))
  (format t "~&~a: ~%" 'constructorcategory)
  (pprint (|getConstructorCategoryFromDB| constructor))
  (format t "~&~a: ~%" 'operationalist)
  (pprint (|getConstructorOperationsFromDB| constructor))
  (format t "~&~a: ~%" 'modemaps)
  (pprint (|getOperationModemapsFromDB| constructor))
  (format t "~a: ~a~%" 'hascategory
	  (|constructorHasCategoryFromDB| constructor))
  (format t "~a: ~a~%" 'object
	  (|getConstructorModuleFromDB| constructor))
  (format t "~a: ~a~%" 'niladic
	  (|niladicConstructorFromDB| constructor))
  (format t "~a: ~a~%" 'abbreviation
	  (|getConstructorAbbreviationFromDB| constructor))
  (format t "~a: ~a~%" 'constructor?
	  (|getConstructorOperationsFromDB| constructor))
  (format t "~a: ~a~%" 'constructor
	  (|getConstructorFullNameFromDB| constructor))
  (format t "~a: ~a~%" 'defaultdomain
	  (|getConstructorDefaultFromDB| constructor))
  (format t "~a: ~a~%" 'ancestors
	  (|getConstructorAncestorsFromDB| constructor))
  (format t "~a: ~a~%" 'sourcefile
	  (|getConstructorSourceFileFromDB| constructor))
  (format t "~a: ~a~%" 'constructorform
	  (|getConstructorFormFromDB| constructor))
  (format t "~a: ~a~%" 'constructorargs
	  (|getConstructorArgsFromDB| constructor))
  (format t "~a: ~a~%" 'attributes
	  (|getConstructorAttributesFromDB| constructor))
  (format t "~a: ~%" 'predicates)
  (pprint (|getConstructorPredicatesFromDB| constructor))
  (format t "~a: ~a~%" 'documentation
	  (|getConstructorDocumentationFromDB| constructor))
  (format t "~a: ~a~%" 'parents
	  (|getConstructorParentsFromDB| constructor)))

(defun setdatabase (constructor key value)
  (let (struct)
    (when (symbolp constructor)
      (unless (setq struct (get constructor 'database))
	(setq struct (make-database))
	(setf (get constructor 'database) struct))
      (case key
	    (abbreviation
	     (setf (database-abbreviation struct) value)
	     (when (symbolp value)
	       (setf (get value 'abbreviationfor) constructor)))
	    (superdomain
	     (setf (database-superdomain struct) value))
	    (constructorkind
	     (setf (database-constructorkind struct) value))))))

(defun deldatabase (constructor key)
  (when (symbolp constructor)
    (case key
	  (abbreviation
	   (setf (get constructor 'abbreviationfor) nil)))))

(defun getdatabase (constructor key)
  (declare (special *miss*))
  (when (eq *miss* t)
    (format t "getdatabase call: ~20a ~a~%" constructor key))
  (let (data table stream ignore struct)
    (declare (ignore ignore))
    (when (or (symbolp constructor)
	      (and (eq key 'hascategory) (consp constructor)))
      (case key
; note that abbreviation, constructorkind and cosig are heavy hitters
; thus they occur first in the list of things to check
	    (abbreviation
	     (setq stream *interp-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-abbreviation struct))))
	    (constructorkind
	     (setq stream *interp-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-constructorkind struct))))
	    (cosig
	     (setq stream *interp-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-cosig struct))))
	    (operation
	     (setq stream *operation-stream*)
	     (setq data (gethash constructor *operation-hash*)))
	    (constructormodemap
	     (setq stream *interp-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-constructormodemap struct))))
	    (constructorcategory
	     (setq stream *interp-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-constructorcategory struct))
	       (when (null data) ;domain or package then subfield of constructormodemap
		 (setq data (cadar (|getConstructorModemapFromDB| constructor))))))
	    (operationalist
	     (setq stream *interp-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-operationalist struct))))
	    (modemaps
	     (setq stream *interp-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-modemaps struct))))
	    (hascategory
	     (setq table  *hasCategory-hash*)
	     (setq stream *category-stream*)
	     (setq data (gethash constructor table)))
	    (object
	     (setq stream *interp-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-object struct))))
	    (asharp?
	     (setq stream *interp-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-object struct))))
	    (niladic
	     (setq stream *interp-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-niladic struct))))
	    (constructor?
	     (|fatalError| "GETDATABASE called with CONSTRUCTOR?"))
	    (superdomain ; only 2 superdomains in the world
	     (setq stream *interp-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-superdomain struct))))
	    (constructor
	     (when (setq data (get constructor 'abbreviationfor))))
	    (defaultdomain
	      (setq data (cadr (assoc constructor *defaultdomain-list*))))
	    (ancestors
	     (setq stream *interp-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-ancestors struct))))
	    (sourcefile
	     (setq stream *browse-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-sourcefile struct))))
	    (constructorform
	     (setq stream *browse-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-constructorform struct))))
	    (constructorargs
	     (setq data (cdr (|getConstructorFormFromDB| constructor))))
	    (attributes
	     (setq stream *browse-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-attributes struct))))
	    (predicates
	     (setq stream *browse-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-predicates struct))))
	    (documentation
	     (setq stream *browse-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-documentation struct))))
	    (parents
	     (setq stream *browse-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-parents struct))))
	    (users
	     (setq stream *browse-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-users struct))))
	    (dependents
	     (setq stream *browse-stream*)
	     (when (setq struct (get constructor 'database))
	       (setq data (database-dependents struct))))
	    (otherwise  
	     (warn "~%(GETDATABASE ~a ~a) failed~%" constructor key)))
      (when (numberp data)                 ;fetch the real data
	(when *miss*
	  (format t "getdatabase miss: ~20a ~a~%" key constructor))
	(file-position stream data)
	;; Don't attempt to uncompress codes -- they are not compressed.
	(setq data (read stream))
	(unless (eq key 'superdomain)
	  (setq data (unsqueeze data)))
	;;(setq data (unsqueeze (read stream)))
	(case key ; cache the result of the database read
	      (operation           
	       (setf (gethash constructor *operation-hash*) data))
	      (hascategory 
	       (setf (gethash constructor *hascategory-hash*) data))
	      (constructorkind 
	       (setf (database-constructorkind struct) data))
	      (cosig     
	       (setf (database-cosig struct) data))
	      (constructormodemap 
	       (setf (database-constructormodemap struct) data))
	      (constructorcategory 
	       (setf (database-constructorcategory struct) data))
	      (operationalist
	       (setf (database-operationalist struct) data))
	      (modemaps  
	       (setf (database-modemaps struct) data))
	      (object    
	       (setf (database-object struct) data))
	      (niladic   
	       (setf (database-niladic struct) data))
	      (abbreviation 
	       (setf (database-abbreviation struct) data))
	      (constructor 
	       (setf (database-constructor struct) data))
	      (ancestors 
	       (setf (database-ancestors struct) data))
	      (constructorform     
	       (setf (database-constructorform struct) data))
	      (attributes
	       (setf (database-attributes struct) data))
	      (predicates 
	       (setf (database-predicates struct) data))
	      (documentation 
	       (setf (database-documentation struct) data))
	      (parents   
	       (setf (database-parents struct) data))
	      (superdomain
	       (setf (database-superdomain struct) data))
	      (users     
	       (setf (database-users struct) data))
	      (dependents
	       (setf (database-dependents struct) data))
	      (sourcefile
	       (setf (database-sourcefile struct) data))))
      (case key ; fixup the special cases
	    (sourcefile
	     (when (and data (string= (directory-namestring data) "")
			(string= (pathname-type data) "spad"))
	       (setq data
		     (concatenate 'string 
				  (|systemRootDirectory|)
				  "src/algebra/" data))))
	    (asharp?			; is this asharp code?
	     (if (consp data)
		 (setq data (cdr data))
	       (setq data nil)))
	    (object		       ; fix up system object pathname
	     (if (consp data)
		 (setq data
		       (if (string= (directory-namestring (car data)) "")
			   (|getSystemModulePath| (car data))
			 (car data)))
	       (when (and data (string= (directory-namestring data) ""))
		 (setq data (|getSystemModulePath| data)))))))
    data))

;; Current directory
;; Contributed by Juergen Weiss.
#+:cmu
(defun get-current-directory ()
  (namestring (extensions::default-directory)))

#-:cmu
(defun get-current-directory ()
  (namestring (truename "")))

  
; localdatabase tries to find files in the order of:
;  NRLIB/index.KAF
;  .asy
;  .ao, then asharp to .asy

(defun localdatabase (filelist options &optional (make-database? nil))
  "read a local filename and update the hash tables"
  (labels 
   ((processOptions (options)
		    (let (only dir noexpose)
		      (when (setq only (assoc '|only| options))
			(setq options (delete only options :test #'equal))
			(setq only (cdr only)))
		      (when (setq dir (assoc '|dir| options))
			(setq options (delete dir options :test #'equal))
			(setq dir (second dir))
			(when (null dir)
			  (|sayKeyedMsg| 'S2IU0002 nil) ))
		      (when (setq noexpose (assoc '|noexpose| options))
			(setq options (delete noexpose options :test #'equal))
			(setq noexpose 't) )
		      (when options
			(format t "   Ignoring unknown )library option: ~a~%" options))
		      (values only dir noexpose)))
    (processDir (dirarg)
		(let ((indexFiles (|getAllIndexPathnames| dirarg))
		      (aldorFiles (|getAllAldorObjectFiles| dirarg)))
		  (values
		   indexFiles
		   (first aldorFiles)
		   (second aldorFiles)
		   ;; At the moment we will only look for user.lib: others 
		   ;; are taken care of by localasy and localnrlib.
		   nil
		   ))))
   (let (thisdir nrlibs asos asys libs object only dir key 
		 (|$forceDatabaseUpdate| t) noexpose)
     (declare (special |$forceDatabaseUpdate|))
     (setq thisdir (get-current-directory))
     (setq noexpose nil)
     (multiple-value-setq (only dir noexpose) (processOptions options))
     ;don't force exposure during database build
     (if make-database? 
	 (setq noexpose t))
     (if dir 
       (multiple-value-setq (nrlibs asys asos libs)
          (processDir (|ensureTrailingSlash| (string dir)))))
     (dolist (file filelist)
       (let ((filename (pathname-name file))
	     (namedir (directory-namestring file)))
	 (unless namedir 
	   (setq thisdir (concatenate 'string thisdir "/")))
	 (cond
	  ((setq file (probe-file
		       (concatenate 'string 
				    namedir 
				    filename
				    ".NRLIB/"
				    |$IndexFilename|)))
	   (push (namestring file) nrlibs))
	  ((setq file (probe-file
		       (concatenate 'string 
				    namedir 
				    filename
				    ".asy")))
	   (push (namestring file) asys))
	  ((setq file (probe-file
		       (concatenate 'string 
				    namedir 
				    filename
				    ".ao")))
	   (push (namestring file) asos))
	  ('else (format t "   )library cannot find the file ~a.~%" filename)))))
     (dolist (file (|reverse!| nrlibs))
       (setq key (pathname-name (first (last (pathname-directory file)))))
       (setq object (concatenate 'string 
				 (directory-namestring file) 
				 "code." |$faslType|))
       (localnrlib key file object make-database? noexpose))
     (dolist (file (|reverse!| asys))
       (setq object
	     (concatenate 'string 
			  (directory-namestring file)
			  (pathname-name file)))
       (localasy (|astran| file) object only make-database? noexpose))
     (dolist (file (|reverse!| asos))
       (setq object
	     (concatenate 'string 
			  (directory-namestring file)
			  (pathname-name file)))
       (asharp file)
       (setq file (|astran| (concatenate 'string 
					 (pathname-name file) 
					 ".asy")))
       (localasy file object only make-database? noexpose))
     (HCLEAR |$ConstructorCache|))))


(defun localasy (asy object only make-database? noexpose)
  "given an alist from the asyfile and the objectfile update the database"
  (labels (
	   (fetchdata (alist index)
		      (cdr (assoc index alist :test #'string=))))
	  (let (cname kind key alist (systemdir? nil) 
		      oldmaps asharp-name dbstruct abbrev)
   (set-file-getter object)  ; sets the autoload property for G-object
   (dolist (domain asy)
     (setq key (first domain))
     (setq alist (rest domain))
     (setq asharp-name
           (foam::axiomxl-global-name (pathname-name object) key
                                     (lassoc '|typeCode| alist)))
     (if (< (length alist) 4) ;we have a naked function object
         (let ((opname key)
               (modemap (car (LASSOC '|modemaps| alist))) )
           (setq oldmaps (|getOperationFromDB| opname))
           (setf (gethash opname *operation-hash*)
                 (adjoin (subst asharp-name opname (cdr modemap))
                         oldmaps :test #'equal))
           (asharpMkAutoloadFunction object asharp-name))
       (when (if (null only) (not (eq key '%%)) (member key only))
	 (setq *allOperations* nil)        ; force this to recompute
	 (setq oldmaps (|getOperationModemapsFromDB| key))
	 (setq dbstruct (make-database))
	 (setf (get key 'database) dbstruct)
	 (setq *allconstructors* (adjoin key *allconstructors*))
	 (setf (database-constructorform dbstruct)
	       (fetchdata alist "constructorForm"))
	 (setf (database-constructorkind dbstruct)
	       (fetchdata alist "constructorKind"))
	 (setf (database-constructormodemap dbstruct)
	       (fetchdata alist "constructorModemap"))
	 (unless (setf (database-abbreviation dbstruct)
		       (fetchdata alist "abbreviation"))
	   (setf (database-abbreviation dbstruct) key)) ; default
	 (setq abbrev (database-abbreviation dbstruct))
	 (setf (get abbrev 'abbreviationfor) key)
	 (setf (database-constructorcategory dbstruct)
	       (fetchdata alist "constructorCategory"))
	 (setf (database-attributes dbstruct)
	       (fetchdata alist "attributes"))
	 (setf (database-sourcefile dbstruct)
	       (fetchdata alist "sourceFile"))
	 (setf (database-operationalist dbstruct)
	       (fetchdata alist "operationAlist"))
	 (setf (database-modemaps dbstruct)
	       (fetchdata alist "modemaps"))
	 (setf (database-documentation dbstruct)
	       (fetchdata alist "documentation"))
	 (setf (database-predicates dbstruct)
	       (fetchdata alist "predicates"))
	 (setf (database-niladic dbstruct)
	       (fetchdata alist "NILADIC"))
	 (addoperations key oldmaps)
	 (setq cname  (|opOf| (database-constructorform dbstruct)))
	 (setq kind (database-constructorkind dbstruct))
	 (if (null noexpose) (|setExposeAddConstr| (cons cname nil)))
	 (unless make-database?
	   (|updateDatabase| key cname systemdir?) ;makes many hashtables???
	   (|installConstructor| cname kind)
	   ;; following can break category database build
	   (if (eq kind '|category|)
	       (setf (database-ancestors dbstruct)
		     (fetchdata alist "ancestors")))
	   (if (eq kind '|domain|)
	       (dolist (pair (cdr (assoc "ancestors" alist :test #'string=)))
		 (setf (gethash (cons cname (caar pair)) *hascategory-hash*)
		       (cdr pair))))
	   (if |$InteractiveMode| 
	       (setq |$CategoryFrame| |$EmptyEnvironment|)))
	 (setf (database-cosig dbstruct)
	       (cons nil (mapcar #'|categoryForm?|
				 (cddar (database-constructormodemap dbstruct)))))
	 (setf (database-object dbstruct) (cons object asharp-name))
	 (if (eq kind '|category|)
	     (asharpMkAutoLoadCategory object cname asharp-name
				       (database-cosig dbstruct))
	   (asharpMkAutoLoadFunctor object cname asharp-name
				    (database-cosig dbstruct)))
	 (|sayKeyedMsg| 'S2IU0001 (list cname object))))))))

(defun localnrlib (key nrlib object make-database? noexpose)
  "given a string pathname of an index.KAF and the object update the database"
  (labels 
   ((fetchdata (alist in index)
	       (let (pos)
		 (setq pos (third (assoc index alist :test #'string=)))
		 (when pos
		   (file-position in pos)
		   (read in)))))
   (let (alist kind (systemdir? nil) pos
	       constructorform oldmaps abbrev dbstruct)
     (with-open-file (in nrlib)
		     (file-position in (read in))
		     (setq alist (read in))
		     (setq pos (third (assoc "constructorForm" alist :test #'string=)))
		     (file-position in pos)
		     (setq constructorform (read in))
		     (setq key (car constructorform))
		     (setq oldmaps (|getOperationModemapsFromDB| key))
		     (setq dbstruct (make-database))
		     (setq *allconstructors* (adjoin key *allconstructors*))
		     (setf (get key 'database) dbstruct) ; store the struct, side-effect it...
		     (setf (database-constructorform dbstruct) constructorform)
		     (setq *allOperations* nil)   ; force this to recompute
		     (setf (database-object dbstruct) object)
		     (setq abbrev
			   (intern (pathname-name (first (last (pathname-directory object))))))
		     (setf (database-abbreviation dbstruct) abbrev)
		     (setf (get abbrev 'abbreviationfor) key)
		     (setf (database-operationalist dbstruct) nil)
		     (setf (database-operationalist dbstruct)
			   (fetchdata alist in "operationAlist"))
		     (setf (database-constructormodemap dbstruct)
			   (fetchdata alist in "constructorModemap"))
		     (setf (database-modemaps dbstruct) 
			   (fetchdata alist in "modemaps"))
		     (setf (database-sourcefile dbstruct) 
			   (fetchdata alist in "sourceFile"))
		     (when make-database?
		       (setf (database-sourcefile dbstruct)
			     (file-namestring  (database-sourcefile dbstruct))))
		     (setf (database-constructorkind dbstruct)
			   (setq kind (fetchdata alist in "constructorKind")))
		     (setf (database-constructorcategory dbstruct)
			   (fetchdata alist in "constructorCategory"))
		     (setf (database-documentation dbstruct)
			   (fetchdata alist in "documentation"))
		     (setf (database-attributes dbstruct)
			   (fetchdata alist in "attributes"))
		     (setf (database-predicates dbstruct)
			   (fetchdata alist in "predicates"))
		     (setf (database-niladic dbstruct)
			   (when (fetchdata alist in "NILADIC") t))
		     (let ((super (fetchdata alist in "evalOnLoad2")))
		       (setf (database-superdomain dbstruct)
			     (when super 
			       (setq super (cddr super))
			       ;; unquote the domain and predicate.
			       (rplaca super (second (first super)))
			       (rplacd super (cdr (second super)))
			       super)))
		     (addoperations key oldmaps)
		     (unless make-database?
		       (if (eq kind '|category|)
			   (setf (database-ancestors dbstruct)
				 (|applySubst| 
                                      (|pairList| (cdr constructorform) 
				      |$FormalMapVariableList|)
				      (fetchdata alist in "ancestors"))))
		       (|updateDatabase| key key systemdir?) ;makes many hashtables???
		       (|installConstructor| key kind) ;used to be key cname ...
		       (|updateCategoryTable| key kind)
		       (if |$InteractiveMode| 
			   (setq |$CategoryFrame| |$EmptyEnvironment|)))
		     (setf (database-cosig dbstruct)
			   (cons nil (mapcar #'|categoryForm?|
					     (cddar (database-constructormodemap dbstruct)))))
		     (remprop key 'loaded)
		     (if (null noexpose) 
			 (|setExposeAddConstr| (cons key nil)))
		     (setf (symbol-function key) ; sets the autoload property for cname
			   #'(lambda (&rest args)
			       (unless (get key 'loaded)
				 (|startTimingProcess| '|load|)
				 (|loadLibNoUpdate| key key object)) ; used to be cname key
			       (apply key args)))
		     (|sayKeyedMsg| 'S2IU0001 (list key object))))))

; making new databases consists of:
;  1) reset all of the system hash tables
;  *) set up Union, Record and Mapping
;  2) map )library across all of the system files (fills the databases)
;  3) loading some normally autoloaded files
;  4) making some database entries that are computed (like ancestors)
;  5) writing out the databases
;  6) write out 'warm' data to be loaded into the image at build time
; note that this process should be done in a clean image
; followed by a rebuild of the system image to include
; the new index pointers (e.g. *interp-stream-stamp*)
; the system will work without a rebuild but it needs to
; re-read the databases on startup. rebuilding the system
; will cache the information into the image and the databases
; are opened but not read, saving considerable startup time.
; also note that the order the databases are written out is
; critical. interp.daase depends on prior computations and has
; to be written out last.

(defun make-databases (dirlist)
  (labels (
    ;; these are types which have no library object associated with them.
    ;; we store some constructed data to make them perform like library
    ;; objects, the *operationalist-hash* key entry is used by allConstructors
  (withSpecialConstructors ()
   ; note: if item is not in *operationalist-hash* it will not be written
   ; UNION
   (setf (get '|Union| 'database)
     (make-database :operationalist nil :constructorkind '|domain|))
   (push '|Union| *allconstructors*)
   ; RECORD
   (setf (get '|Record| 'database)
    (make-database :operationalist nil :constructorkind '|domain|))
   (push '|Record| *allconstructors*)
   ; MAPPING
   (setf (get '|Mapping| 'database)
    (make-database :operationalist nil :constructorkind '|domain|))
   (push '|Mapping| *allconstructors*)
   ; ENUMERATION
   (setf (get '|Enumeration| 'database)
    (make-database :operationalist nil :constructorkind '|domain|))
   (push '|Enumeration| *allconstructors*)
   )
  (final-name (root) 
	      (concat root ".daase"))
  )
 (let (d)
  (declare (special |$constructorList|))
  (do-symbols (symbol)
   (when (get symbol 'database)
    (setf (get symbol 'database) nil)))
  (setq *hascategory-hash* (make-hash-table :test #'equal))
  (setq *operation-hash* (make-hash-table))
  (setq *allconstructors* nil)
  (setq *compressvector* nil)
  (withSpecialConstructors)
  (localdatabase nil
     (list (list '|dir| (get-current-directory) ))
     'make-database)
  (dolist (dir dirlist)
          (localdatabase nil 
                         (list (list '|dir| 
                                     (namestring (probe-file 
                                                  (concat "./" 
                                                          dir)))))
                         'make-database))
#+:AKCL    (|mkTopicHashTable|)
  (setq |$constructorList| nil) ;; affects buildLibdb
  (|buildLibdb|)
  (|dbSplitLibdb|)
; (|dbAugmentConstructorDataTable|)
  (|mkUsersHashTable|)
  (|saveUsersHashTable|)
  (|mkDependentsHashTable|)
  (|saveDependentsHashTable|)
  (|buildGloss|)
  (write-compress)
  (write-browsedb)
  (write-operationdb)
 ; note: genCategoryTable creates a new *hascategory-hash* table
 ; this smashes the existing table and regenerates it.
 ; write-categorydb does getdatabase calls to write the new information
  (write-categorydb)
  (dolist (con (|allConstructors|))
   (let (dbstruct)
     (when (setq dbstruct (get con 'database))
           (setf (database-cosig dbstruct)
                 (cons nil (mapcar #'|categoryForm?|
                                   (cddar (database-constructormodemap dbstruct)))))
           (when (and (|categoryForm?| con)
                      (= (length (setq d (|domainsOf| (list con) NIL NIL))) 1))
                 (setq d (caar d))
                 (when (= (length d) (length (|getConstructorForm| con)))
                       (format t "   ~a has a default domain of ~a~%" con (car d))
                       (setf (database-defaultdomain dbstruct) (car d)))))))
                                        ; note: genCategoryTable creates *ancestors-hash*. write-interpdb
                                        ; does gethash calls into it rather than doing a getdatabase call.
  (write-interpdb)
#+:AKCL  (write-warmdata)
  (create-initializers)
  (when (probe-file (final-name "compress"))
        (delete-file (final-name "compress")))
  (rename-file "compress.build" (final-name "compress"))
  (when (probe-file (final-name "interp"))
        (delete-file (final-name "interp")))
  (rename-file "interp.build" (final-name "interp"))
  (when (probe-file (final-name "operation"))
        (delete-file (final-name "operation")))
  (rename-file "operation.build" (final-name "operation"))
  (when (probe-file (final-name "browse")) 
        (delete-file (final-name "browse")))
  (rename-file "browse.build" 
               (final-name "browse"))
  (when (probe-file (final-name "category"))
        (delete-file (final-name "category")))
  (rename-file "category.build" 
               (final-name "category")))))

(defun compressOpen ()
 (let (lst stamp pos)
  (setq *compress-stream*
    (open (|pathToDatabase| "compress.daase") :direction :input))
  (setq stamp (read *compress-stream*))
  (unless (equal stamp *compress-stream-stamp*)
    (when |$verbose|
      (format t "   Re-reading compress.daase"))
   (setq *compress-stream-stamp* stamp)
   (setq pos (car stamp))
   (file-position *compress-stream* pos)
   (setq lst (read *compress-stream*))
   (setq *compressVectorLength* (car lst))
   (setq *compressvector*
     (make-array (car lst) :initial-contents (cdr lst))))))

(defun write-compress ()
 (let (compresslist masterpos out)
  (close *compress-stream*)
  (setq out (open "compress.build" :direction :output))
  (princ "                              " out)
  (finish-output out)
  (setq masterpos (file-position out))
  (setq compresslist
        (append (|allConstructors|) (|allOperations|) |$BuiltinAttributes|))
  (push "algebra" compresslist)
  (push "failed" compresslist)
  (push 'signature compresslist)
  (push '|ofType| compresslist)
  (push '|Join| compresslist)
  (push 'and compresslist)
  (push '|nobranch| compresslist)
  (push 'category compresslist)
  (push '|category| compresslist)
  (push '|domain| compresslist)
  (push '|package| compresslist)
  (push 'attribute compresslist)
  (push '|isDomain| compresslist)
  (push '|ofCategory| compresslist)
  (push '|Union| compresslist)
  (push '|Record| compresslist)
  (push '|Mapping| compresslist)
  (push '|Enumeration| compresslist)
  (setq *compressVectorLength* (length compresslist))
  (setq *compressvector*
    (make-array *compressVectorLength* :initial-contents compresslist))
  (print (cons (length compresslist) compresslist) out)
  (finish-output out)
  (file-position out 0)
  (print (cons masterpos (get-universal-time)) out)
  (finish-output out)
  (close out)))

(defun write-interpdb ()
 "build interp.daase from hash tables"
 (declare (special *ancestors-hash*))
 (let (opalistpos modemapspos cmodemappos master masterpos obj *print-pretty*
        concategory categorypos kind niladic cosig abbrev defaultdomain
        ancestors ancestorspos superpos out)
  (print "building interp.daase")
  (setq out (open "interp.build" :direction :output))
  (princ "                              " out)
  (finish-output out)
  (dolist (constructor (|allConstructors|))
   (let (struct)
    (setq struct (get constructor 'database))
    (setq opalistpos (file-position out))
    (print (squeeze (database-operationalist struct)) out)
    (finish-output out)
    (setq cmodemappos (file-position out))
    (print (squeeze (database-constructormodemap struct)) out)
    (finish-output out)
    (setq modemapspos (file-position out))
    (print (squeeze (database-modemaps struct)) out)
    (finish-output out)
    (let ((entry (database-object struct)))
      (cond ((consp entry)
	     (setq obj (cons (pathname-name (car entry))
			     (cdr entry))))
	    (entry
	     (setq obj (pathname-name
			(first (last (pathname-directory entry))))))
	    (t (setq obj nil))))
    (setq concategory (squeeze (database-constructorcategory struct)))
    (if concategory  ; if category then write data else write nil
	(progn
	  (setq categorypos (file-position out))
	  (print concategory out)
	  (finish-output out))
      (setq categorypos nil))
    (setq niladic (database-niladic struct))
    (setq abbrev (database-abbreviation struct))
    (setq cosig (database-cosig struct))
    (setq kind (database-constructorkind struct))
    (setq defaultdomain (database-defaultdomain struct))
    (setq ancestors (squeeze (gethash constructor *ancestors-hash*))) ;cattable.boot
    (if ancestors
	(progn
	  (setq ancestorspos (file-position out))
	  (print ancestors out)
	  (finish-output out))
      (setq ancestorspos nil))
    (setq superpos
	  ;; We do NOT want to compress codes, as we may not be 
	  ;; able to uncompress them to their original form.
	  (let ((super (database-superdomain struct)))
	    (when super
	      (prog1 (file-position out)
		(print super out)
		(finish-output out)))))
    
    (push (list constructor opalistpos cmodemappos modemapspos
		obj categorypos niladic abbrev cosig kind defaultdomain
		ancestorspos superpos) master)))
  (finish-output out)
  (setq masterpos (file-position out))
  (print (|squeezeAll| master) out)
  (finish-output out)
  (file-position out 0)
  (print (cons masterpos (get-universal-time)) out)
  (finish-output out)
  (close out)))

(defun write-browsedb ()
 "make browse.daase from hash tables"
 (let (master masterpos src formpos docpos attpos predpos *print-pretty* out)
  (print "building browse.daase")
  (setq out (open "browse.build" :direction :output))
  (princ "                              " out)
  (finish-output out)
  (dolist (constructor (|allConstructors|))
   (let (struct)
    (setq struct (get constructor 'database))
     ; sourcefile is small. store the string directly
    (setq src (database-sourcefile struct))
    (setq formpos (file-position out))
    (print (squeeze (database-constructorform struct)) out)
    (finish-output out)
    (setq docpos (file-position out))
    (print (database-documentation struct) out)
    (finish-output out)
    (setq attpos (file-position out))
    (print (squeeze (database-attributes struct)) out)
    (finish-output out)
    (setq predpos (file-position out))
    (print (squeeze (database-predicates struct)) out)
    (finish-output out)
    (push (list constructor src formpos docpos attpos predpos) master)))
  (finish-output out)
  (setq masterpos (file-position out))
  (print (|squeezeAll| master) out)
  (finish-output out)
  (file-position out 0)
  (print (cons masterpos (get-universal-time)) out)
  (finish-output out)
  (close out)))

(defun write-categorydb ()
 "make category.daase from scratch. contains the *hasCategory-hash* table"
 (let (out master pos *print-pretty*)
  (print "building category.daase")
  (|genCategoryTable|)
  (setq out (open "category.build" :direction :output))
  (princ "                              " out)
  (finish-output out)
  (maphash #'(lambda (key value)
    (if (or (null value) (eq value t))
     (setq pos value)
     (progn
      (setq pos (file-position out))
      (print (squeeze value) out)
      (finish-output out)))
     (push (list key pos) master))
     *hasCategory-hash*)
  (setq pos (file-position out))
  (print (|squeezeAll| master) out)
  (finish-output out)
  (file-position out 0)
  (print (cons pos (get-universal-time)) out)
  (finish-output out)
  (close out)))

(defun unsqueeze (expr)
  (cond ((atom expr)
         (cond ((and (numberp expr) (<= expr 0))
                (svref *compressVector* (- expr)))
               (t expr)))
        (t (rplaca expr (unsqueeze (car expr)))
           (rplacd expr (unsqueeze (cdr expr)))
           expr)))

(defun squeeze (expr)
 (let (leaves pos (bound (length *compressvector*)))
  (labels (
   (flat (expr)
    (when (and (numberp expr) (< expr 0) (>= expr bound))
     (print expr)
     (break "squeeze found a negative number"))
    (if (atom expr)
     (unless (or (null expr)
                 (and (symbolp expr) (char= (schar (symbol-name expr) 0) #\*)))
      (setq leaves (adjoin expr leaves)))
     (progn
      (flat (car expr))
      (flat (cdr expr))))))
  (setq leaves nil)
  (flat expr)
  (dolist (leaf leaves)
   (when (setq pos (position leaf *compressvector*))
     (|substitute!| (- pos) leaf expr)))
  expr)))

(defun write-operationdb ()
 (let (pos master out)
  (declare (special leaves))
  (setq out (open "operation.build" :direction :output))
  (princ "                              " out)
  (finish-output out)
  (maphash #'(lambda (key value)
   (setq pos (file-position out))
   (print (squeeze value) out)
   (finish-output out)
   (push (cons key pos) master))
   *operation-hash*)
  (finish-output out)
  (setq pos (file-position out))
  (print (|squeezeAll| master) out)
  (file-position out 0)
  (print (cons pos (get-universal-time)) out)
  (finish-output out)
  (close out)))

(defun write-warmdata ()
  "write out information to be loaded into the image at build time"
  (declare (special |$topicHash|))
  (with-open-file (out "warm.data" :direction :output)
		  (format out "(in-package \"BOOT\")~%")
		  (format out "(setq |$topicHash| (make-hash-table))~%")
		  (maphash #'(lambda (k v)
			       (format out "(setf (gethash '|~a| |$topicHash|) ~a)~%" k v)) |$topicHash|)))

(defun |allConstructors| ()
  (declare (special *allconstructors*))
  *allconstructors*)

(defun |allOperations| ()
  (declare (special *allOperations*))
  (unless *allOperations*
    (maphash #'(lambda (k v) (declare (ignore v)) (push k *allOperations*))
	     *operation-hash*))
  *allOperations*)

; the variable NOPfuncall is a funcall-able object that is a dummy
; initializer for libaxiom asharp domains.
(defvar NOPfuncall (cons 'identity nil))

(defun create-initializers ()
;; since libaxiom is now built with -name=axiom following unnecessary
;; (dolist (con (|allConstructors|))
;;   (let ((sourcefile (|getConstructorSourceFileFromDB| con)))
;;     (if sourcefile
;;       (set (foam::axiomxl-file-init-name (pathname-name sourcefile))
;;             NOPfuncall))))
 (setf (symbol-value (foam::axiomxl-file-init-name "axiom")) NOPfuncall)
;; (set (foam::axiomxl-file-init-name "axclique") NOPfuncall)
 (setf (symbol-value (foam::axiomxl-file-init-name "filecliq")) NOPfuncall)
 (setf (symbol-value (foam::axiomxl-file-init-name "attrib")) NOPfuncall)
;; following needs to happen inside restart since $AXIOM may change
 (let ((asharprootlib (strconc (|systemRootDirectory|) "/aldor/lib/")))
   (set-file-getter (strconc asharprootlib "runtime"))
   (set-file-getter (strconc asharprootlib "lang"))
   (set-file-getter (strconc asharprootlib "attrib"))
   (set-file-getter (strconc asharprootlib "axlit"))
   (set-file-getter (strconc asharprootlib "minimach"))
   (set-file-getter (strconc asharprootlib "axextend"))))



;---------------------------------------------------------------------

; how the magic works:
;  when a )library is done on a new compiler file we set up multiple
;  functions (refered to as autoloaders). there is an autoloader
;  stored in the symbol-function of the G-filename (e.g. G-basic)
;  (see set-file-getter function)
;  and an autoloader stored in the symbol-function of every domain
;  in the basic.as file ( asharpMkAutoloadFunctor )
; When a domain is needed the autoloader for the domain is executed.
;  this autoloader invokes file-getter-name to get the name of the
;  file (eg basic) and evaluates the name. the FIRST time this is done
;  for a file the file will be loaded by its autoloader, then it will
;  return the file object. every other time the file is already
;  loaded and the file object is returned directly.
; Once the file object is gotten getconstructor is called to get the
;  domain. the FIRST time this is done for the domain the autoloader
;  invokes the file object. every other time the domain already
;  exists.
;(defvar *this-file* "no-file")

(defmacro |CCall| (fun &rest args)
  (let ((ccc (gensym)) (cfun (gensym)) (cenv (gensym)))
    `(let ((,ccc ,fun))
       (let ((,cfun (|ClosFun| ,ccc))
             (,cenv (|ClosEnv| ,ccc)))
         (funcall ,cfun ,@args ,cenv )))))

(defmacro |ClosFun| (x) `(car ,x))
(defmacro |ClosEnv| (x) `(cdr ,x))

(defun file-runner (name)
 (declare (special foam-user::|G-domainPrepare!|))
  (|CCall| foam-user::|G-domainPrepare!| (|CCall| name)))

(defun getConstructor (file-fn asharp-name)
 (|CCall| file-fn)
; (eval (cdr (assoc file-id (get name 'asharp-name) :test #'equal))))
 (eval asharp-name))

(defun getop (dom op type)
 (declare (special foam-user::|G-domainGetExport!|))
  (|CCall| foam-user::|G-domainGetExport!| dom
      (|hashString| (symbol-name op)) type))

; the asharp compiler will allow both constant domains and domains
; which are functions. localasy sets the autoload property so that
; the symbol-function contains a function that, when invoked with
; the correct number of args will return a domain.

; this function is called if we are given a new compiler domain
; which is a function. the symbol-function of the domain is set
; to call the function with the correct number of arguments.

(defun wrapDomArgs (obj type?)
  (cond ((not type?) obj)
        (t (|makeOldAxiomDispatchDomain| obj))))

(defun asharpMkAutoLoadFunctor (file cname asharp-name cosig)
  (setf (symbol-function cname)
  #'(lambda (&rest args)
     (let ((func (getconstructor (eval (file-getter-name file)) asharp-name)))
      (setf (symbol-function cname)
       (if (vectorp (car func))
        #'(lambda () func) ;; constant domain
        #'(lambda (&rest args)
            (apply (|ClosFun| func)
                   (|append!|
                    (mapcar #'wrapDomArgs args (cdr cosig))
                    (list (|ClosEnv| func)))))))
      (apply cname args)))))

(defun asharpMkAutoLoadCategory (file cname asharp-name cosig)
  (asharpMkAutoLoadFunctor file cname asharp-name cosig)
  (let ((packname (INTERN (STRCONC cname '"&"))))
    (setf (symbol-function packname)
  #'(lambda (self &rest args)
     (let ((func (getconstructor (eval (file-getter-name file)) asharp-name)))
      (setf (symbol-function packname)
       (if (vectorp (car func))
        #'(lambda (self)
            (|CCall| (elt (car func) 5) (cdr func) (wrapDomArgs self t))) ;; constant category
        #'(lambda (self &rest args)
            (let ((precat
                   (apply (|ClosFun| func)
                          (|append!|
                           (mapcar #'wrapDomArgs args (cdr cosig))
                           (list (|ClosEnv| func))))))
              (|CCall| (elt (car precat) 5) (cdr precat) (wrapDomArgs self t))))))
      (apply packname self args))))))

(defun asharpMkAutoLoadFunction (file asharpname)
  (setf (symbol-value asharpname)
   (cons
    #'(lambda (&rest l)
        (let ((args (butlast l))
              (func (getconstructor (eval (file-getter-name file)) asharpname)))
          (apply (car func) (append args (list (cdr func))))))
        ())))

; this function will return the internal name of the file object getter

(defun file-getter-name (filename)
   (foam::axiomxl-file-init-name (pathname-name filename)))

;;need to initialize |G-filename| to a function which loads file
;; and then returns the new value of |G-filename|

(defun set-file-getter (filename)
  (let ((getter-name (file-getter-name filename)))
    (setf (symbol-value getter-name)
         (cons #'init-file-getter  (cons getter-name filename)))))

(defun init-file-getter (env)
  (let ((getter-name (car env))
        (filename (cdr env)))
    (load filename)
    (|CCall| (eval getter-name))))

(defun set-lib-file-getter (filename cname)
  (let ((getter-name (file-getter-name filename)))
    (setf (symbol-value getter-name)
         (cons #'init-lib-file-getter  (cons getter-name cname)))))

(defun init-lib-file-getter (env)
  (let* ((getter-name (car env))
         (cname (cdr env))
         (filename (|getConstructorModuleFromDB| cname)))
    (load filename)
    (|CCall| (eval getter-name))))

;; following 2 functions are called by file-exports and file-imports macros
(defun foam::process-import-entry (entry)
  (let* ((asharpname (car entry))
         (stringname (cadr entry))
         (hcode (caddr entry))
         (libname (cadddr entry))
         (bootname (intern stringname 'boot)))
    (declare (ignore libname))
    (if (and (eq hcode 'foam-user::|initializer|) (not (boundp asharpname)))
        (error (format nil "AxiomXL file ~s is missing!" stringname)))
    (unless (or (not (numberp hcode)) (zerop hcode) (boundp asharpname))
          (when (|constructor?| bootname)
                (setf (symbol-value asharpname)
                     (if (|niladicConstructorFromDB| bootname)
                         (|makeLazyOldAxiomDispatchDomain| (list bootname))
                       (cons '|runOldAxiomFunctor|  bootname))))
          (when (|attribute?| bootname)
                (setf (symbol-value asharpname)
		      (|makeLazyOldAxiomDispatchDomain| bootname))))))
          
                 

;(defun foam::process-export-entry (entry)
;  (let* ((asharpname (car entry))
;        (stringname (cadr entry))
;        (hcode (caddr entry))
;        (libname (cadddr entry))
;        (bootname (intern stringname 'boot)))
;    (declare (ignore libname))
;    (when (numberp hcode)
;         (setf (get bootname 'asharp-name)
;               (cons (cons *this-file* asharpname)
;                     (get bootname 'asharp-name)))
;         )))







