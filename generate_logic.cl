
(defmacro → (a b) `(or (not ,a) ,b))

(defmacro ↔ (a b) `(and (→ ,a ,b) (→ ,b ,a)))

(defmacro *list-term-end* () `(list nil t))

(defmacro *list-term-operation* (term) `(list
									   (list 'and ,term ,term)
									   (list 'or ,term ,term)
									   (list '→ ,term ,term)
									   (list '↔ ,term ,term)
									   (list 'not ,term)
									   ))

(defmacro *list-term-middle* (term) `(
									  append *list-term-end*
									  (*list-term-operation* ,term)
									  ))

(defun random-element (x)
  (nth (random (length x)) x)
  )	   

(defun flatten (structure)
  (cond ;((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))

(flatten '(or (and nil t)(or nil (and t t))))

(defun term-end () (random-element *list-term-end*))

(defun term-middle (&optional (level 1))
  (if (< level 2)
	  (random-element (*list-term-middle* (term-middle (incf level))))
	  (random-element (*list-term-middle* (term-end)))
	  )
  )

(defun term-operation () (random-element (*list-term-operation* (term-middle))))

(defun term (stream symb collon etc)
  (cond
	((eq symb 'AND) (format stream "\\wedge" symb))
	((eq symb 'OR ) (format stream "\\vee" symb))
	((eq symb 'NOT) (format stream "\\\\neg" symb))
	((eq symb '→) (format stream "\\Rightarrow" symb))
	((eq symb '↔) (format stream "\\Leftrightarrow" symb))
	)
  )

(defun predicat (stream predicat collon etc)
  (cond
	((or (eq t predicat)
		 (eq nil predicat)) (format stream "~:[False~;True~]" predicat))
	((= (length predicat) 2) (format stream "~{~/term/ ~/predicat/~}" predicat))
	((= (length predicat) 3) (if collon
								 (format stream "~{~*$$ ~/predicat/~2:* ~/term/ ~*~/predicat/ $$~}" predicat)
								 (format stream "~{~*( ~/predicat/~2:* ~/term/ ~*~/predicat/ )~}" predicat)
								 ))
  )
  )

(defun shortanswer-10-print (raw)
  (with-open-file (stream "~/quiz_2_2.txt" :direction :output :if-does-not-exist :create :if-exists :rename-and-delete :external-format :utf-8)
	(format stream "~%$CATEGORY:$course$/Вопросы 2015\\/2016/Булева алгебра/Задание 3 ~2%")
	(format stream "~:{::Вопрос ~3,'0d:: Вычислите выражение: ~:/predicat/ {= ~:[F~;T~]~:* = ~:[false~;true~]~:* = ~:[0~;1~]~:* = ~:[ложь~;истина~] }~2%~}" raw)
	)
  )


(defun seventh-quiz ()
  (shortanswer-10-print
   (loop for x = (term-operation)
		 for lx = (length (flatten x))
	  while (/= size 250)
	  when (and
			(> lx 7)
			(< lx 11))
	  counting x into size
	  and collecting (list size x (eval x)) into array
	  finally (return array)
		)
   )
  )

(seventh-quiz)
