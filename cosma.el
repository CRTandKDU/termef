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

(defun jmc/cosma-export ()
  (interactive)
  (let ((ast (org-element-parse-buffer))
	(buf (current-buffer))
	(dir "C:\\Users\\jmchauvet\\Documents\\Cosma\\jmc")
	(yaml-template "---
title: %s
id: %s
type: %s
tags:
  - Collège_Informatique
%s---

")
	)
    ;; TOC
    (with-current-buffer (get-buffer-create "*COSMA-TOC*")
      (erase-buffer)
      (set-buffer-file-coding-system 'utf-8)
      (insert (format yaml-template "Liste17" (jmc/cosma-newid) "liste" "")))
    
    ;; Terms are 3rd-level headlines in the master org-file
    (org-element-map ast 'headline
      (lambda (hl)
	(if (= 3 (org-element-property ':level hl))
	    (let ((txt (buffer-substring (org-element-property ':contents-begin hl)
					 (org-element-property ':contents-end hl)))
		  (newid (jmc/cosma-newid))
		  (fn "terme")
		  )
	      ;; Append to TOC
	      (with-current-buffer (get-buffer-create "*COSMA-TOC*")
		(insert
		 (format "%s [[%s]] *%s*\n"
			 (substring (format "%s" (org-element-property ':title hl)) 1 -1)
			 newid
			 (org-element-property ':title (org-element-property ':parent hl))
			 )))

	      ;; Create new record file
	      (with-current-buffer (get-buffer-create "*COSMA*")
		(erase-buffer)
		(set-buffer-file-coding-system 'utf-8)
		(insert
		 (format yaml-template
			 (substring (format "%s" (org-element-property ':title hl)) 1 -1)
			 newid
			 "terme"
			 (format "  - %s\n" (substring (format "%s" (org-element-property ':title (org-element-property ':parent hl))) 1 -1))))
		(insert (format "\n%s\n" txt))
		(append-to-file (point-min) (point-max) (format "%s\\%s_%s.md" dir fn newid))
		)
	      )
	  )
	)
      )
    ;; Save TOC
    (with-current-buffer (get-buffer-create "*COSMA-TOC*")
      (append-to-file (point-min) (point-max) (format "%s\\Liste.md" dir)))

    )
  )

(defun jmc/cosma-newid ()
  (sleep-for 1)
  (let ((now (decode-time (current-time)))
	)
    (format "%04d%02d%02d%02d%02d%02d"
	    (nth 5 now)
	    (nth 4 now)
	    (nth 3 now)
	    (nth 2 now)
	    (nth 1 now)
	    (car now)
	    ;; (random 10000)
	    )))

;;; cosma.el ends here


