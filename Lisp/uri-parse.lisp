;;; -*- Mode: Lisp -*-
;;; Tommaso Ferrario 869005
;;; -- Main functions --
;; The uri-parse function is the main function for parsing. This function
;; recognizes the scheme, invokes the parsing auxiliary function and
;; returns a list of 8 fields, which represents the structure of the uri.
(defun uri-parse (Uri)
  (let* ((List_of_Chars (coerce Uri 'list))
         (Scheme (scheme List_of_Chars))
         (aux_parse (aux-uri-parse (second Scheme)
				   (string-downcase 
                                    (concatenate 'string
						 (first Scheme))))))
    (append (list 'URI) (list (concatenate 'string (first Scheme)))
	    aux_parse)))
;; The following functions are intended to access the various fields
;; in the list representing the uri. Check that the list is not empty
;; in all functions. Also check that it respects the chosen form to
;; represent Uri.
(defun uri-scheme (URIStruct)
  (if (null URIStruct) 
      NIL
    (if (uri-p URIStruct)
        (second URIStruct)
      NIL)))
(defun uri-userinfo (URIStruct)
  (if (null URIStruct) 
      NIL
    (if (uri-p URIStruct)
        (third URIStruct)
      NIL)))
(defun uri-host (URIStruct)
  (if (null URIStruct) 
      NIL
    (if (uri-p URIStruct)
        (fourth URIStruct)
      NIL)))
(defun uri-port (URIStruct)
  (if (null URIStruct) 
      NIL
    (if (uri-p URIStruct)
        (fifth URIStruct)
      NIL)))
(defun uri-path (URIStruct)
  (if (null URIStruct) 
      NIL
    (if (uri-p URIStruct)
        (sixth URIStruct)
      NIL)))
(defun uri-query (URIStruct)
  (if (null URIStruct) 
      NIL
    (if (uri-p URIStruct)
        (seventh URIStruct)
      NIL)))
(defun uri-fragment(URIStruct)
  (if (null URIStruct) 
      NIL
    (if (uri-p URIStruct)
        (eighth URIStruct)
      NIL)))
;; This function is used to print the uri structure on video. 
;; It input the uri structure and an optional value, the stream. 
;; The optional value is default to *standard-output*.
(defun uri-display (uri-structure &optional (stream *standard-output*))
  (if (streamp stream)
      (let ((out (mapcar #'list (list 'Scheme 'Userinfo 'Host 
				      'Port 'Path 'Query 'Fragment)
                         (rest uri-structure))))
        (format stream  "Display URI~%")
	(print-uri out stream) T)
    NIL))
;;; -- End of main functions --
;;; -- Auxiliary functions --
;; The print-uri function is used to print the various components 
;; of the uri on the stream.
(defun print-uri (List out)
  (cond ((null List) (format out "~%"))
        (t (format out "~VT~S:~T~T~S~%" 1 (first (first List)) 
		   (second (first List))) 
           (print-uri (rest List) out)
	   )))
;; This function is used to parse the scheme. It performs initial checks
;; on characters that are allowed or not in the scheme.
(defun scheme (URI-List)
  (cond ((null URI-List) (error "Invalid scheme."))
        ((and (eq (first URI-List) #\:) (eq (second URI-List) #\@))
         (error "Userinfo not valid"))
        ((eq (first URI-List) #\:) (list () (rest URI-List)))
        (t (let ((char (first URI-List)))
	     (if (or (eq char #\/) (eq char #\?) (eq char #\#)
		     (eq char #\:)) 
		 (error "Scheme not valid.")
               (concat (scheme (rest URI-List)) char))))))
;; This function is used to parse the parts after the scheme. It presents
;; the various cases depending on the value of the scheme, and the 
;; characters immediately following it. 
(defun aux-uri-parse (URI-List Scheme)
  (cond ((equal "" Scheme) (error "Scheme missing."))
        ((null URI-List) (list NIL NIL 80 NIL NIL NIL))
	((equal Scheme "mailto")
	 (let* ((autho (read-authority URI-List))
		(mailto (user (first autho))))
	   (cond ((null (first autho)) 
                  (error "The entered fields are not correct."))
                 ((null (rest mailto))
                  (list (first mailto) NIL 80 NIL NIL NIL))
                 (t
                  (list-to-string (list (first mailto) (second mailto)
                                        80 NIL NIL NIL))))))	       
	((equal Scheme "news")
         (let ((news (first (host URI-List))))
	   (if (check-host news)
	       (list-to-string (list NIL news 80 NIL NIL NIL))
	     (error "Host not valid."))))
	((or (equal Scheme "tel") (equal Scheme "fax"))
	 (let ((utente (first (user URI-List))))
	   (list-to-string (list utente NIL 80 NIL NIL NIL))))
	((equal Scheme "zos") 
         (if (and (eq (first URI-List) #\/) (eq (second URI-List) #\/))
             (let* ((autho (authority (rest (rest URI-List))))
                    (op (path (fourth autho)))
                    (user (first autho))
                    (host (second autho))
                    (port (parse-integer (concatenate 'string
                                                      (third autho))))
                    (zos (first op))
                    (query (second op))
                    (frag (third op)))
               (cond ((null zos)
                      (if (and (null query) (null frag))
                          (if (equal (last URI-List) '(#\/))
                              (error "Path zos is invalid")
                            (if (not (check-host host))
                                (error "Host not valid")
                              (list-to-string (list user host port
						    NIL NIL NIL))))
                        (error "Path zos is invalid")))
                     ((not (check-host host))
                      (error "Host not valid."))
                     ((not (path-zos-p zos 0))
                      (error "Path zos is invalid."))
                     ((not (between 0 65536 port))
                      (error "Port is not valid."))
                     (t (list-to-string (list user host port zos query
                                              frag)))))
           (if (or (null (rest URI-List)) (eq (second URI-List) #\?)
		   (eq (second URI-List) #\#))
               (error "Path zos is invalid.")
             (let* ((op (path (rest URI-List)))
                    (zos (first op))
                    (query (second op))
                    (frag (third op)))
               (if (path-zos-p zos 0)
                   (list-to-string (list NIL NIL 80 zos query frag))
                 (error "Path zos is invalid."))))))
	((and (eq (first URI-List) #\/) (eq (second URI-List) #\/))
	 (let* ((autho (authority (rest (rest URI-List))))
		(op (path (fourth autho)))
		(port (parse-integer (concatenate 'string
						  (third autho))))) 
	   (cond ((not (between 0 65536 port))
		  (error "Port is not valid."))
		 ((not (check-host (second autho)))
		  (error "Host not valid."))
		 ((not (check-path (first op)))
		  (error "Path not valid"))
		 (t (list-to-string (list* (first autho) (second autho)
					   port op))))))
	((eq (first URI-List) #\/) 
         (let ((op (path URI-List)))
	   (if (check-path (first op))
	       (list-to-string (list* NIL NIL 80 op))
	     (error "Path not valid."))))
        (t (error "Uri not valid."))))
;; This function deals with the presence of userinfo.
(defun authority (URI-List)
  (if (eq (first URI-List) #\@)
      (error "Userinfo not valid.")
    (let* ((read (read-authority URI-List))
           (autho (aux-authority (first read))))
      (if (null (second autho))
          (list () (first autho) (third autho) (second read))
        (list (first autho) (second autho) (third autho)
	      (second read))))))
;; This function allows me to recognize the authority by also 
;; performing initial checks on the characters.
(defun aux-authority (URI-List)
  (let ((head (first URI-List))
	(tail (rest URI-List)))
    (cond ((null URI-List) (list () () '(#\8 #\0)))
	  ((eq head #\@) 
           (cond ((null tail) (error "Host missing."))
                 ((eq (first tail) #\:) (error "Host missing."))
                 (t (list* () (host tail)))))
	  ((eq head #\:)
	   (if (null tail)
	       (error "Port not valid.")
	     (list () () (first (port tail)))))
	  (t
	   (if (or (eq head #\/) (eq head #\?) (eq head #\#)
		   (eq head #\:))
	       (error "Invalid character.")
	     (concat (aux-authority tail) head))))))
;; This function is used to parse the user part of the uri. It also 
;; implements initial controls on the user structure.
(defun user (List &optional (scheme "default"))
  (cond ((null List) (list () ()))
        ((and (eq (first List) #\@) (null (rest List)))
         (error "Invalid host"))
        ((and (eq (first List) #\@) (equal scheme "mailto"))
         (list () (host (rest List))))
        ((eq (first List) #\@) (list* () (host (rest List))))
        (t (let ((char (first List)))
             (if (or (eq char #\/) (eq char #\?) (eq char #\#)
		     (eq char #\:))
		 (error "Uerinfo is not valid.")
               (concat (user (rest List) scheme) char))))))
;; This function is used to parse the host part of the uri. It also 
;; implements initial controls on the host structure. 
;; If it finds the ":" it invokes the function that parses the port.
;; Otherwise it assigns to the port the default value. 
(defun host (List)
  (cond ((null List) (list () '(#\8 #\0)))
        ((eq (first List) #\:)
         (if (null (rest List))
             (error "Port is not valid.")
           (list () (first (port (rest List))))))
	((and (eq (first List) #\.) (null (rest List)))
	 (error "Host not valid."))
	((and (eq (first List) #\.) (eq (second List) #\:))
	 (error "Host not valid."))
        (t (let ((char (first List)))
             (if (or (eq char #\/) (eq char #\?) (eq char #\#)
		     (eq char #\:)
		     (eq char #\@)) 
		 (error "Host not valid.")
               (concat (host (rest List)) char))))))
;; This function is used to parse the port, if preset, of the uri.
;; It also implements initial checks on the port structure.
(defun port (List)
  (if (null (first List))
      (list () (rest List))
    (if (not (digit-char-p (first List)))
	(error "Port is not valid.")
      (concat (port (rest List)) (first List)))))
;; This function is used to parse the path of the uri. 
;; It also implements initial controls on the path structure. 
;; If it finds the "?" invokes the function that parses the query, 
;; if it finds "#", it invokes the function that parses the fragment.
(defun path (List_of_Chars)
  (let ((char (first List_of_Chars))
	(tail (rest List_of_Chars)))
    (cond ((null List_of_Chars)(list () () ()))
	  ((eq char #\?)
	   (if (null tail)
	       (error "Query is not valid.")
	     (list* () (query tail))))
	  ((eq char #\#)
	   (if (null tail)
	       (error "Fragment is not valid.")
	     (list () () (first (fragment tail)))))
	  ((and (eq char #\/) (null tail))
	   (error "Illegal path"))
	  (t (if (or (eq char #\@) (eq char #\?) (eq char #\#)
		     (eq char #\:))
		 (error "Path not valid.")
	       (concat (path tail) char))))))
;; This function is used to parse the uri query, if it is present. 
;; It also implements initial controls on the query structure. 
;; If it finds "#" it invokes the function that parses the fragment.
(defun query (List_of_Chars)
  (let ((char (first List_of_Chars))
	(tail (rest List_of_Chars)))
    (cond ((null List_of_Chars) (list () ()))
	  ((and (eq char #\#) (null tail))
	   (error "Fragment is not valid."))
	  ((eq char #\#) (list () (first (fragment tail))))
	  (t (concat (query tail) char)))))
;; This function is used to parse the uri fragment, if it is present.
;; It also implements initial checks on the fragment structure.
(defun fragment (List_of_Chars)
  (if (null List_of_Chars)
      (list () ())
    (concat (fragment (rest List_of_Chars)) (first List_of_Chars))))
;;; -- useful functions --
;; This function is used to convert a string to an integer.
(defun str-to-num (List)
  (if (null List)
      ()
    (append (list (parse-integer (first List)))
	    (str-to-num (rest List)))))
;; These two functions are used to concatenate a value in a list of
;; lists.
(defun concat (List Val)
  (if (null List)
      (list Val)
    (cons (add (first List) Val) (cdr List))))

(defun add (List Val)
  (if (null List)
      (list Val)
    (cons Val List)))
;; These functions are used to check if a number is included in a given
;; range.
(defun between (min max v)
  (and (<= min v) (>= max v)))
;; These functions serve to convert every single item in the list from
;; list to string.
(defun list-to-string (List)
  (cond ((null List) ())
        ((numberp (first List)) 
         (append (list (first List)) (list-to-string (rest List))))
        ((null (first List)) 
         (append (list NIL) (list-to-string (rest List))))
        (t (append (list (concatenate 'string (first List))) 
                   (list-to-string (rest List))))))
;; These functions serve to make the whole authority recognize.
(defun read-authority (Uri_List)
  (cond ((null Uri_list) (list () ()))
	((eq (first Uri_List) #\/) (list () (rest Uri_List)))
	(t (concat (read-authority (rest Uri_List))
		   (first Uri_List)))))
;; This function checks if the input structure is a uri.
(defun uri-p (URI-Struct)
  (and (equal (first URI-Struct) 'URI)
       (= (list-length URI-Struct) 8)))
;;; -- Control functions --
;; This feature allows me to check the host after it has parsed. 
;; In particular, it allows me to manage cases where it is an IPv4
;; address or not.
(defun check-host (Host_List)
  (cond ((null Host_List) (error "Host not valid."))
	((and (only-digit-dots Host_List)
	      (= 15 (list-length Host_List)))
	 (if (ip-p Host_List)
             T
           (aux-check-host Host_List)))
	(t (aux-check-host Host_List))))
;; This function allows me to verify the correctness of the host 
;; in case it is not an IP.
(defun aux-check-host (Host_List)
  (let ((head (first Host_List))
	(tail (rest Host_List)))
    (cond ((null Host_List))
	  ((eq head #\.)
	   (if (null tail)
	       (error "Host not valid.")
	     (aux-check-host tail)))
	  (t (if (or (eq head #\.) (eq head #\/) (eq head #\?)
		     (eq head #\#) (eq head #\@) (eq head #\:))
		 (error "Host not valid.")
	       (aux-check-host tail))))))
;; This function allows me to verify the correctness of the path after 
;; it has been fully recognized.
(defun check-path (List)
  (let ((head (first List))
	(tail (rest List)))
    (cond ((null List) T)
	  ((eq head #\/)
	   (if (null tail)
	       (error "Path not valid.")
	     (check-path tail)))
	  (t (check-path tail)))))
;; These functions allow to verify if the list of characters passed 
;; to the input is a valid Ipv4 address.
(defun ip-p (List)
  (let* ((p (ip-part List))
         (s (ip-part (second p)))
         (te (ip-part (second s)))
         (q (ip-part (second te))))
    (is-ip-p (str-to-num
	      (list-to-string (list (first p) (first s)
				    (first te) (first q)))))))

(defun ip-part (List)
  (cond ((or (eq (first List) #\.) (null List)) (list () (rest List)))
        ((not (digit-char-p (first List))) NIL)
        (t (concat (ip-part (rest List)) (first List)))))

(defun only-digit-dots (List)
  (let ((head (first List)))
    (cond ((null List) T)
	  ((or (eq head #\1) (eq head #\2) (eq head #\3) (eq head #\4)
	       (eq head #\5) (eq head #\6) (eq head #\7) (eq head #\8)
	       (eq head #\9) (eq head #\0) (eq head #\.))
	   (only-digit-dots (rest List)))
	  (t NIL))))

(defun is-ip-p (List)
  (cond ((null List) T)
        (T (if (between 0 255 (first List))
               (is-ip-p (rest List))
             NIL))))
;; This function allows to verify if the list of characters passed 
;; in input respects the specifics of the path for the schema zos.
(defun path-zos-p (List Acc)
  (let ((head (first List))
        (sec (second List))
        (tail (rest List)))
    (cond ((null List) 
           (if (and (> Acc 0) (<= Acc 44))
               T
             (error "Invalid path zos id44.")))
          ((eq head #\.)
           (cond ((null tail) (error "Invalid path zos id44."))
                 ((eq sec #\?) (error "Invalid path zos id44."))
                 ((eq sec #\#) (error "Invalid path zos id44."))
                 (t (path-zos-p tail (1+ Acc)))))
          ((eq head #\() 
           (if (<= Acc 44)
               (id8-p tail 0)
             (error "Invalid path zos id44.")))
          (t (cond ((= 0 Acc)
                    (if (alpha-char-p head)
                        (path-zos-p tail (1+ Acc))
                      (error "Path zos id44 invalid character.")))
                   (t (if (alphanumericp head)
                          (path-zos-p tail (1+ Acc))
                        (error "Path zos id44 invalid character."))))))))

;; This function allows to verify if the list of characters passed 
;; in input respects the specifics of the path for the schema zos, 
;; in the case of id8.
(defun id8-p (List Acc)
  (let ((head (first List))
        (tail (rest List)))
    (cond ((null List) (error "Path zos, id8 invalid."))
          ((eq head #\))
           (if (and (> Acc 0) (<= Acc 8) (null tail))
               T
             (error "Path zos, id8 invalid.")))
          (t (cond ((= 0 Acc)
                    (if (alpha-char-p head)
                        (id8-p tail (1+ Acc))
                      (error "Path zos id8 invalid character.")))
                   (t (if (alphanumericp head)
                          (id8-p tail (1+ Acc))
                        (error "Path zos id8 invalid character."))))))))
