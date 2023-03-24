;;; termef.el --- Query the Termef repository to produce a
;;; pre-formatted org-file for Cosma.

;; Copyright (C) 2023 jmc

;; Author: jmc
;; Maintainer: jmc
;; Created: Thursday, March 23, 2023
;; Keywords: terminologie, CELF
;; URL: 

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Change Log:
;;; Code:
(require 'request)

;;; Next section: importing interactively from Infobox Web Service.

(defvar termef-cosma-org-file "C:\\Users\\jmchauvet\\Documents\\Cosma\\jmc\\termef-out.org")

(defun termef--filter-attributes (item &optional exclusion-list)
  "Creates a third-level org-element for term from the value of
its `Nom' attribute, and returns a list of strings for all other
attributes, excluding any attribute in the `exclusion-list'."
  (let ((atts-array (gethash "attributes" item))
	(str nil)
	)
    (dotimes (i (length atts-array) str)
      (let* ((att (gethash "attribute" (elt atts-array i)))
	     (att-val-array (gethash "values" (elt atts-array i)))
	     (vals nil)
	    )
	(if (string= "Nom" att)
	    (insert (format "*** %s\n" (elt att-val-array 0)))
	  (unless (member att exclusion-list)
	    (setq str (append str
			      (list (if (= 1 (length att-val-array))
					(format "%s : %s\n" (capitalize att) (elt att-val-array 0))
				      (format "%s :\n" (capitalize att))))
			      (if (= 1 (length att-val-array)) nil
				(dotimes (j (length att-val-array) vals)
				  (setq vals (append vals (list (format "%s\n" (elt att-val-array j))))))
				))))
	  )
	)
      )
    )
  )

(defun termef-query (kw)
  "Async query to Termef returning matching terms for `kw' key
string. Returns a properly formatted org-file for Cosma (use the
`cosma.el' function: `cosma-export')."
  (interactive "sKeyword to search: ")
  (write-region "" nil termef-cosma-org-file)
  (with-temp-buffer
    (insert (format "* %s\n" kw))
    (append-to-file (point-min) (point-max) termef-cosma-org-file))
  ;; 
  (request "https://terminologie.finances.gouv.fr/search/infobox"
    ;; "http://httpbin.org/status/200"  ;; success callback will be called.
    ;; "http://httpbin.org/status/400"  ;; you will see "Got 400."
    :params `(("q" . ,kw) ("s" . "1") ("mode" . "exact") ("field" . "label"))
    :parser 'buffer-string
    :success
    (cl-function (lambda (&key data &allow-other-keys)
                   (when data
                     (with-current-buffer (get-buffer-create "*request demo*")
                       (erase-buffer)
		       (let ((jsonobj (json-parse-string data))
			     )
			 ;; (insert (format "%s\n" (gethash "available" jsonobj)))
			 (dotimes (i (length (gethash "items" jsonobj)))
			   (let ((item (termef--filter-attributes (elt (gethash "items" jsonobj) i) '("groupe")))
				 )
			     (insert (format "%s\n\n" (string-join item)))
			     )
			   )
			 )
		       (append-to-file (point-min) (point-max) termef-cosma-org-file)
                       ))))
    :error
    (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                   (message "Got error: %S" error-thrown)))
    :complete (lambda (&rest _) (message "Finished!"))
    :status-code '((400 . (lambda (&rest _) (message "Got 400.")))
                   (418 . (lambda (&rest _) (message "Got 418.")))))
  )

;;; Next section: importing from complete RDF vocabularies.

(defun termef--crop (str)
  (if (> (length str) 2) (substring str 0 -2) str))

(defun termef--search-concept (url buf)
  "Minimalist search for cross-references in same vocabulary. Find
preferred label, in french, for RDF `url' in the RDF vocabulary
in buffer `buf'. Returns `nil' if not found, or label as a
string."
  (save-excursion
    (set-buffer buf)
    (goto-char (point-min))
    (let ((found (search-forward (format "rdf:about=\"%s\"" url) nil t)))
      (if found
	  ;; Subparse enclosing RDF-XML
	  (let* ((beg (or (beginning-of-line) (point)))
		 (end (or (forward-sexp) (point)))
		 (subparse (libxml-parse-xml-region beg end))
		 (res nil)
		 )
	    (dolist (ssbloc subparse res)
	      (if (and (listp ssbloc)
		       (atom (car ssbloc))
		       )
		  (cond
		   ((and (equal 'skos:prefLabel (car ssbloc))
			 (string= "fr" (cdr (assoc 'lang (cadr ssbloc)))))
		    (setq res (string-join (cddr ssbloc))))
		   )
		)
	      )
	    )
	nil)
      )
    )
  )

(defun termef-rdf ()
  "Converts a full RDF vocabulary downloaded from a Termef domain
into a org-file properly formatted for export to Cosma. Applies
to current buffer. Result returned in `*RDF*' buffer."
  (interactive)
  (let ((parse (libxml-parse-xml-region (point-min) (point-max)))
	(rdfbuf (current-buffer))
	)
    (with-current-buffer (get-buffer-create "*RDF*")
      (erase-buffer)
      (org-mode)
      (insert (format "* Source : %s\n" (buffer-file-name rdfbuf)))
      (dolist (bloc parse)
	(if (and (listp bloc) (string= "Description" (car bloc)))
	    (let ((txt nil) (found-def nil))
	      (dolist (ssbloc bloc)
		(if (and (listp ssbloc) (atom (car ssbloc)))
		    (cond
		     ;; The preferred label, in french, is the
		     ;; third-level heading. Immediate printing.
		     ((and (equal 'prefLabel (car ssbloc))
			   (string= "fr" (cdr (assoc 'lang (cadr ssbloc)))))
		      (setq found-def t)
		      (insert (format "*** %s\n---\n" (string-join (cddr ssbloc)))))

		     ;; Delayed printing of body of text under this
		     ;; heading, picked up from RDF description into a
		     ;; prop list.
		     ((and (equal 'definition (car ssbloc))
			   (string= "fr" (cdr (assoc 'lang (cadr ssbloc)))))
		      ;; Definition should be unique?
		      (setq txt
			    (plist-put txt 'definition
				       (append (plist-get txt 'definition)
					       (list (format "%s" (string-join (cddr ssbloc))))))))

		     ((and (equal 'deconseilleOuFrequent (car ssbloc))
			   (string= "fr" (cdr (assoc 'lang (cadr ssbloc)))))
		      (setq txt
			    (plist-put txt 'deconseille
				       (append (plist-get txt 'deconseille)
					       (list (format "%s, " (string-join (cddr ssbloc))))))))

		     ((and (equal 'related (car ssbloc))
			   )
		      (let ((reference
			     (termef--search-concept
			      (format "%s" (cdr (assoc 'resource (cadr ssbloc))))
			      rdfbuf))
			    )
			(if reference
			    (setq txt
				  (plist-put txt 'voiraussi
					     (append (plist-get txt 'voiraussi)
						     (list (format "%s, " reference))))))


			)
		      )
		     )
		  )
		)

	      ;; Print collected information from the plist
	      (if (and found-def txt)
		  (insert (format "Définition : %s\nDéconseillé : %s\nVoir aussi : %s\n\n"
				  (string-join (plist-get txt 'definition))
				  (termef--crop (string-join (plist-get txt 'deconseille)))
				  (termef--crop (string-join (plist-get txt 'voiraussi)))
				  )))
	      )
	  )
	)
      )
    )
  )


(provide 'termef)
;;; termef.el ends here
