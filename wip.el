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


