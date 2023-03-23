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

(provide 'termef)
;;; termef.el ends here
