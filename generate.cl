

(defun shortanswer-print (raw)
  (with-open-file (stream "quiz_1.txt" :direction :output)
	(format stream "~{~{::Вопрос ~a:: Переведите число ~a в систему счисления по основанию ~a: {= ~a }~}~%~%~}" raw)
	)
  )

(defun shortanswer-2-print (raw)
  (with-open-file (stream "quiz_2.txt" :direction :output :if-exists :overwrite :external-format :utf-8)
	(format stream "~{~{::Вопрос ~a:: Переведите в десятичную систему счисления число ~a из системы счисления по основанию ~a: {= ~a }~}~%~%~}" raw)
	)
  )

(defun multianswer-print (raw)
  (with-open-file (stream "~/quiz_3.txt" :direction :output)
	(format stream "~{~{::Вопрос ~a:: Выберите символы, которые могут встречаться в числе, записанном в системе счисления по основанию ~a:{~?}~}~2%~}" raw)
	)
  )

(defun multianswer-2-print (raw)
  (with-open-file (stream "~/quiz_4.txt" :direction :output)
	(format stream "~{~{::Вопрос ~d:: Выберите символы, которые <strong>не</strong> могут встречаться в числе, записанном в системе счисления по основанию ~a:{~?}~}~2%~}" raw)
	)
)

(defun shortanswer-3-print (raw)
  (with-open-file (stream "quiz_5.txt" :direction :output :if-exists :overwrite :external-format :utf-8)
	(format stream "~:{::Вопрос ~d:: Переведите в десятичную систему счисления число ~@R из римской системы счисления: {= ~:*~d}~%~%~}" raw)
	)
  )

(defun shortanswer-4-print (raw)
  (with-open-file (stream "quiz_6.txt" :direction :output :if-exists :overwrite :external-format :utf-8)
	(format stream "~:{::Вопрос ~d:: Переведите в римскую систему счисленея число ~d из десятичной системы счисления: {= ~:*~@R}~%~%~}" raw)
	)
  )

(defun shortanswer-5-print (raw)
  (with-open-file (stream "quiz_7.txt" :direction :output :if-does-not-exist :create :if-exists :overwrite :external-format :utf-8)
	(format stream "~:{::Вопрос ~3,'0d:: Вычислите в системе счисления по основанию ~d сумму чисел ~? : {=~@?~:* =~d}~2%~}" raw)
	)
  )

(defun shortanswer-6-print (raw)
  (with-open-file (stream "quiz_8.txt" :direction :output :if-does-not-exist :create :if-exists :overwrite :external-format :utf-8)
	(format stream "~:{::Вопрос ~3,'0d:: Вычислите в системе счисления по основанию ~d разность чисел ~? : {=~@?~:* =~d}~2%~}" raw)
	)
  )

(defun shortanswer-7-print (raw)
  (with-open-file (stream "quiz_9.txt" :direction :output :if-does-not-exist :create :if-exists :overwrite :external-format :utf-8)
	(format stream "~:{::Вопрос ~3,'0d:: Вычислите в системе счисления по основанию ~d произведение чисел ~? : {=~@?~:* =~d}~2%~}" raw)
	)
  )

(multianswer-print '((1 4 "~@:{~:[~~~;~~%33.333%~] ~a~%~}" ((T "2") (NIL "7") (T "2") (T "3"))) (2 8 "~@:{~:[~~~;~~%50%~] ~a~%~}" ((T "3") (NIL "F") (T "2") (NIL "8")))))

(shortanswer-print '((1 183 4 "2313") (2 145 13 "B2") (3 126 14 "90") (4 176 12 "128") (5 138 3 "12010")))



(defun first-quiz ()
( shortanswer-2-print
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

(defun second-quiz ()
( multianswer-2-print
(loop for base = (+ (random 14) 2)
      for answers = (loop for variant = (random (* base 2))
					   while (/= size 4)
					   counting variant into size
					   collecting (list (>= variant base) (write-to-string variant :base (* base 2))) into answers 
					   finally (
								if (and (loop for x in answers thereis (car x))
										(loop for x in answers thereis (null (car x)))
										(= (list-length (remove-duplicates answers :test #'string-equal :key #'cadr)) 4)
										)
								   (return answers)))
   while (/= qsize 250)
   when (and (/= base 10)
			 answers
			 )
   counting base into qsize
   and collecting (list qsize base (format nil "~~@:{~~%~~:[~~~~~~;~[~;=~;~~~~%50%~;~~~~%33.333%~;~~~~%25%~]~~] ~~a~~}" (loop for x in answers when (car x) count x) ) answers) into array
   finally (return array) 
	 )
)
)

(second-quiz)

(defun third-quiz ()
( shortanswer-3-print
(loop for x = (+ 101 (random 499))
	  while (/= size 50)
		counting x into size
		collecting (list size x ) into array
	  finally (return array) )
)
)

(third-quiz)


(defun fourth-quiz ()
( shortanswer-6-print
(loop for base = (+ (random 14) 2)
      for y = (+ 101 (random 99))
      for x = (+ 101 (random 99))
	  while (/= size 250)
      when (and (/= base 10 )
 			    (> x y)
			)
		counting x into size
		and collecting (list size base (format nil "~~@{~~~ar~~^ и ~~}" base) (list x y) (format nil "~~~ar" base) (- x y) ) into array
	  finally (return array) )
)
)

(fourth-quiz)
