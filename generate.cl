
(defun shortanswer-print (raw)
  (with-open-file (stream "quiz_1.txt" :direction :output)
	(format stream "~{~{::Вопрос ~a:: Переведите число ~a в систему счисления по основанию ~a: {= ~a }~}~%~%~}" raw)
	)
  )

(defun shortanswer-2-print (raw)
  (with-open-file (stream "quiz_2.txt" :direction :output)
	(format stream "~{~{::Вопрос ~a:: Переведите в десятичную систему число ~a из системы счисления по основанию ~a: {= ~a }~}~%~%~}" raw)
	)
  )

(shortanswer-print '((1 183 4 "2313") (2 145 13 "B2") (3 126 14 "90") (4 176 12 "128") (5 138 3 "12010")))

(defun first-quiz ()
( shortanswer-print
(loop for x = (+ (random 14) 2)
	  for y = (+ 101 (random 99))
	  while (/= size 250)
	  when (/= x 10 )
		counting x into size
		and collecting (list size (write-to-string y :base x) x y ) into array
	  finally (return array) )
)
)

(first-quiz)
