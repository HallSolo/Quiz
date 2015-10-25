(require 'mustache)
(require 'calc-bin)


(defun quiz_1 ( number ) 
( print
  (mustache-render
 "::Вопрос {{number}}:: Переведите число {{digit}} в систему счисления по основанию {{base}}: {= {{#hex}}{{digit}}{{/hex}} }"
 (ht
  ("digit" (random 100))
  ("number" (format "%03d" number))
  ("base" (+ 1 (random 31)))
  ("hex"
   (lambda (templete context)
	 ;;	 ( format "%x" (gethash "digit" context) )
	 ( let ((calc-number-radix (gethash "base" context) ))
	   (math-format-radix (gethash "digit" context) ) )
	 ))
  )
 )
)
)


(loop for i from 1 to 250
	  do (
;;		  (interactive)
		  insert  ( quiz_1 i ) "\n\n"
	  )
	  )


