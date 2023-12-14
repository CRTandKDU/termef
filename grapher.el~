(require 'epc)

;; eval each s-exp one by one, such as `eval-last-sexp'

;; start a child process and establish the connection
(setq epc (epc:start-epc "node" '("server.js")))

;; call the echo method
;; (deferred:$
;;   (epc:call-deferred epc 'echo '(hello))
;;   (deferred:nextc it 
;;     (lambda (x) (message "Return : %S" x)))
;;   (deferred:error it
;;     (lambda (err)
;;       (cond
;;        ((stringp err) (message "Application error: %S" err))
;;        ((eq 'epc-error (car err))
;; 	(message "EPC error: %S" (cadr err)))
;;        )))
;;   )

;; (message "%S" (epc:call-sync epc 'echo '((nodes links))))
(message "%S"
	 (epc:call-sync epc 'echo '("A$B$C$Chaînes de caractères$E/A$B$B$E")))
	




(epc:stop-epc epc) ; dispose EPC stack and peer process
