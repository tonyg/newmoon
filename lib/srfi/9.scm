;;; Implementation of SRFI-9 record types.
;;; Taken directly from SRFI-9's defining document, except for the
;;; underlying record representation, which is new code.
;;; Defmacroised 11 Oct 2006.
;;; Tony Garnock-Jones 18 June 2003, for Newmoon

(define (record? x)
  (%assemble (x t f) (x #t #f)
    (c "return(scheme_boolean(isrecord("x")));")
    (dotnet ($ x)
	    (isinst "class [Newmoon]Newmoon.Record")
	    (brtrue ldtrue)
	    ($ f)
	    (br done)
	    ldtrue
	    ($ t)
	    done)))

(define (make-record size)
  (%assemble (size) (size)
    (c "if (!isint("size")) { wrong_type(0); }"
       "return(mkrec(DETAG("size")));")
    (dotnet ($ size)
	    (unbox "int32")
	    (ldind.i4)
	    (newobj "instance void class [Newmoon]Newmoon.Record::.ctor(int32)"))))

(define (record-ref record index)
  (%assemble (record index) (record index)
    (c "int ival = DETAG("index");"
       "if (!isrecord("record")) { wrong_type(0); }"
       "if (!isint("index")) { wrong_type(1); }"
       "if (ival < 0 || ival >= oop_len("record")) { bad_index(); }"
       "return(((vector *) "record")->data[ival]);")
    (dotnet ($ record)
	    (castclass "class [Newmoon]Newmoon.Record")
	    ($ index)
	    (unbox "int32")
	    (ldind.i4)
	    (call "instance object class [Newmoon]Newmoon.Record::get_Item(int32)"))))

(define (record-set! record index value)
  (%assemble (record index value) (record index value)
    (c "int ival = DETAG("index");"
       "if (!isrecord("record")) { wrong_type(0); }"
       "if (!isint("index")) { wrong_type(1); }"
       "if (ival < 0 || ival >= oop_len("record")) { bad_index(); }"
       "return(((vector *) "record")->data[ival] = "value");")
    (dotnet ($ record)
	    (castclass "class [Newmoon]Newmoon.Record")
	    ($ index)
	    (unbox "int32")
	    (ldind.i4)
	    ($ value)
	    (call "instance void class [Newmoon]Newmoon.Record::set_Item(int32, object)")
	    ($ value))))

;; Syntax definitions

; Definition of DEFINE-RECORD-TYPE

(defmacro define-record-type (type ctor pred . fields)
  (if (not (symbol? type)) (error "Bad record type" type))
  (if (not (and (list? ctor)
		(positive? (length ctor))
		(every symbol? ctor)))
      (error "Bad record constructor" type ctor))
  (if (not (symbol? pred)) (error "Bad record predicate" type pred))
  (for-each (lambda (field)
	      (if (not (and (list? field)
			    (every symbol? field)
			    (or (= (length field) 2)
				(= (length field) 3))))
		  (error "Bad field definition" type field)))
	    fields)
  `(begin
     (define ,type (make-record-type ',type ',(map car fields)))
     (define ,(car ctor) (record-constructor ,type ',(cdr ctor)))
     (define ,pred (record-predicate ,type))
     ,@(map (lambda (field) `(define-record-field ,type ,@field)) fields)))

; (define-syntax define-record-type
;   (syntax-rules ()
;     ((define-record-type type
;        (constructor constructor-tag ...)
;        predicate
;        (field-tag accessor . more) ...)
;      (begin
;        (define type
;          (make-record-type 'type '(field-tag ...)))
;        (define constructor
;          (record-constructor type '(constructor-tag ...)))
;        (define predicate
;          (record-predicate type))
;        (define-record-field type field-tag accessor . more)
;        ...))))

; An auxilliary macro for define field accessors and modifiers.
; This is needed only because modifiers are optional.

(defmacro define-record-field (type field-tag accessor . maybe-modifier)
  (if (null? maybe-modifier)
      `(define ,accessor (record-accessor ,type ',field-tag))
      `(begin
	 (define ,accessor (record-accessor ,type ',field-tag))
	 (define ,(car maybe-modifier) (record-modifier ,type ',field-tag)))))

; (define-syntax define-record-field
;   (syntax-rules ()
;     ((define-record-field type field-tag accessor)
;      (define accessor (record-accessor type 'field-tag)))
;     ((define-record-field type field-tag accessor modifier)
;      (begin
;        (define accessor (record-accessor type 'field-tag))
;        (define modifier (record-modifier type 'field-tag))))))

;; Record types

; We define the following procedures:
; 
; (make-record-type <type-name <field-names>)    -> <record-type>
; (record-constructor <record-type<field-names>) -> <constructor>
; (record-predicate <record-type>)               -> <predicate>
; (record-accessor <record-type <field-name>)    -> <accessor>
; (record-modifier <record-type <field-name>)    -> <modifier>
;   where
; (<constructor> <initial-value> ...)         -> <record>
; (<predicate> <value>)                       -> <boolean>
; (<accessor> <record>)                       -> <value>
; (<modifier> <record> <value>)         -> <unspecific>

; Record types are implemented using vector-like records.  The first
; slot of each record contains the record's type, which is itself a
; record.

(define (record-type record)
  (record-ref record 0))

;----------------
; Record types are themselves records, so we first define the type for
; them.  Except for problems with circularities, this could be defined as:
;  (define-record-type :record-type
;    (make-record-type name field-tags)
;    record-type?
;    (name record-type-name)
;    (field-tags record-type-field-tags))
; As it is, we need to define everything by hand.

(define :record-type (make-record 3))
(record-set! :record-type 0 :record-type)	; Its type is itself.
(record-set! :record-type 1 ':record-type)
(record-set! :record-type 2 '(name field-tags))

; Now that :record-type exists we can define a procedure for making more
; record types.

(define (make-record-type name field-tags)
  (let ((new (make-record 3)))
    (record-set! new 0 :record-type)
    (record-set! new 1 name)
    (record-set! new 2 field-tags)
    new))

; Accessors for record types.

(define (record-type-name record-type)
  (record-ref record-type 1))

(define (record-type-field-tags record-type)
  (record-ref record-type 2))

;----------------
; A utility for getting the offset of a field within a record.

(define (field-index type tag)
  (let loop ((i 1) (tags (record-type-field-tags type)))
    (cond ((null? tags)
           (error "record type has no such field" type tag))
          ((eq? tag (car tags))
           i)
          (else
           (loop (+ i 1) (cdr tags))))))

;----------------
; Now we are ready to define RECORD-CONSTRUCTOR and the rest of the
; procedures used by the macro expansion of DEFINE-RECORD-TYPE.

(define (record-constructor type tags)
  (let ((size (length (record-type-field-tags type)))
        (arg-count (length tags))
        (indexes (map (lambda (tag)
                        (field-index type tag))
                      tags)))
    (lambda args
      (if (= (length args)
             arg-count)
          (let ((new (make-record (+ size 1))))
            (record-set! new 0 type)
            (for-each (lambda (arg i)
			(record-set! new i arg))
                      args
                      indexes)
            new)
          (error "wrong number of arguments to constructor" type args)))))

(define (record-predicate type)
  (lambda (thing)
    (and (record? thing)
         (eq? (record-type thing)
              type))))

(define (record-accessor type tag)
  (let ((index (field-index type tag)))
    (lambda (thing)
      (if (and (record? thing)
               (eq? (record-type thing)
                    type))
          (record-ref thing index)
          (error "accessor applied to bad value" type tag thing)))))

(define (record-modifier type tag)
  (let ((index (field-index type tag)))
    (lambda (thing value)
      (if (and (record? thing)
               (eq? (record-type thing)
                    type))
          (record-set! thing index value)
          (error "modifier applied to bad value" type tag thing)))))
