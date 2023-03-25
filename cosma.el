;;; cosma.el --- Support for the Cosma mind map tool

;; Copyright (C) 2023 jmc

;; Author: jmc
;; Maintainer: jmc
;; Created: Sunday, March 19, 2023
;; Keywords: terminologie, CELF
;; URL: 

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Change Log:
;;; Code:

(defvar cosma-zero-id 240000 "Start beyond 23:59:59 for id generation")
(defconst cosma-yaml-template "---
title: %s
id: %s
type: %s
tags:
  - Collège_Informatique
%s---

"
  "Template for root node and yaml headers of individual .md files.")


(defun cosma--newid ()
  "Generates a new unique id from the current day and incremented
global counter."
  (incf cosma-zero-id)
  (let* ((now (decode-time (current-time)))
	 (str (format "%6d" cosma-zero-id))
	 (end (format "%2s%2s%2s" (substring str 0 2) (substring str 2 4) (substring str 4 6)))
	)
    (format "%04d%02d%02d%6s" (nth 5 now) (nth 4 now) (nth 3 now) end)))

(defun cosma--toc (toc-title &optional save-as-file)
  "Generates a table of content `(term . uuid)` as an association
list and a root .md file from a properly formatted org buffer."
  ;; Init TOC buffer
  (with-current-buffer (get-buffer-create "*COSMA-TOC*")
    (erase-buffer)
    (set-buffer-file-coding-system 'utf-8)
    (insert (format cosma-yaml-template toc-title (cosma--newid) "liste" "")))

  ;; Build TOC as an alist
  (let ((toc nil)
	(ast (org-element-parse-buffer))
	)
    (org-element-map ast 'headline
      (lambda (hl)
	(if (= 3 (org-element-property ':level hl))
	    (let ((txt (buffer-substring (org-element-property ':contents-begin hl)
					 (org-element-property ':contents-end hl)))
		  (newid (cosma--newid))
		  (fn "terme")
		  )
	      ;; Append to TOC buffer
	      (with-current-buffer (get-buffer-create "*COSMA-TOC*")
		(insert
		 (format "%s [[inclut:%s]] *%s*\n"
			 (substring (format "%s" (org-element-property ':title hl)) 1 -1)
			 newid
			 (org-element-property ':title (org-element-property ':parent hl))
			 )))
	      ;; Append to alist
	      (push (cons (substring (format "%s" (org-element-property ':title hl)) 1 -1) newid) toc)
	      )
	  )
	)
      )
    ;; Save TOC
    (if save-as-file
	(with-current-buffer (get-buffer-create "*COSMA-TOC*")
	  (append-to-file (point-min) (point-max) (format "%s\\%s.md" dir toc-title))))
    toc
    )
  )

(defun cosma--links (txt toc config-alist)
  "Replaces the line beginning with key `prefix' of `config-alist'
with annotated lists of pointers (when present in org buffer).
`config-alist' specifies a minima `prefix', `separator' and
`link-type'."
  (with-current-buffer (get-buffer-create "*COSMA-TMP*")
    (erase-buffer)
    (insert (format "%s" txt))
    (goto-char (point-min))
    (if (re-search-forward (cdr (assoc 'prefix config-alist)) nil t)
	(let* ((refs (delete-and-extract-region (point) (point-at-eol)))
	       (terms (split-string refs (cdr (assoc 'separator config-alist)) t))
	       )
	  (insert
	   (format
	    "%s\n"
	    (string-join
	     (mapcar #'(lambda (key)
			 (let ((val (cdr (assoc (string-trim key) toc))))
			   (if val (format "%s [[%s:%s]]" key (cdr (assoc 'link-type config-alist)) val) key)))
		     terms)
	     ", "
	     ))
	   )
	  )
      )
    (buffer-substring-no-properties (point-min) (point-max))
    )
  )
	    
(defun cosma-export ()
  (interactive)
  (let ((ast (org-element-parse-buffer))
	(buf (current-buffer))
	(dir "C:\\Users\\jmchauvet\\Documents\\Cosma\\jmc")
	)

    ;; TOC matters
    (let ((toc (cosma--toc (buffer-name)))
	  )
      ;; Terms are 3rd-level headlines in the master org-file
      (org-element-map ast 'headline
	(lambda (hl)
	  (if (= 3 (org-element-property ':level hl))
	      (let* ((txt (buffer-substring (org-element-property ':contents-begin hl)
					    (org-element-property ':contents-end hl)))
		     (key (substring (format "%s" (org-element-property ':title hl)) 1 -1))
		     (newid (cdr (assoc key toc)))
		     (fn (format "%s" (org-element-property ':SKOSTYPE hl)))
		     )

		;; Create new record file
		(with-current-buffer (get-buffer-create "*COSMA*")
		  (erase-buffer)
		  (set-buffer-file-coding-system 'utf-8)
		  (insert
		   (format
		    cosma-yaml-template
		    (substring (format "%s" (org-element-property ':title hl)) 1 -1)
		    newid
		    fn
		    (format "  - %s\n"
			    (substring
			     (format "%s" (org-element-property ':title (org-element-property ':parent hl))) 1 -1))))
		  (insert
		   (format "%s\n"
			   (let ((transtxt txt))
			     (dolist (transform
				      '(((prefix . "Généralise : ") (separator . ", ") (link-type . "generalise"))
					((prefix . "Spécialise : ") (separator . ", ") (link-type . "specialise"))
					((prefix . "Voir aussi : ") (separator . ", ") (link-type . "voir_aussi"))
					;; ((prefix . "Schémas : ") (separator . ", ") (link-type . "schema"))
					)
				      transtxt)
			       (setq transtxt (cosma--links transtxt toc transform))))))

		  (append-to-file (point-min) (point-max) (format "%s\\%s_%s.md" dir fn newid))
		  )
		)
	    )
	  )
	)

    )
    )
  )

(provide 'cosma)
;;; cosma.el ends here


