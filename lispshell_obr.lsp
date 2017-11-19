;;;; poczatek wszystkich definicji

;;; funkcja unifikuj
(defun unify (pattern1 pattern2 substitution-list)
	(cond ((equal substitution-list 'failed) 'failed)
		((varp pattern1) (match-var pattern1 pattern2 substitution-list))
		((varp pattern2) (match-var pattern2 pattern1 substitution-list))
		((is-constant-p pattern1)
			(cond ((equal pattern1 pattern2) substitution-list)
				(t 'failed)))
		((is-constant-p pattern2) 'failed)
		(t (unify (cdr pattern1) (cdr pattern2)
			(unify (car pattern1) (car pattern2) substitution-list)))))

;;; funkcja tworzaca pary
(defun match-var (var pattern substitution-list)
	(cond ((equal var pattern) substitution-list)
		(t (let ((binding (get-binding var substitution-list)))
			(cond (binding (unify (get-binding-value binding) pattern substitution-list))
				((occursp var pattern) 'failed)
				(t (add-substitution var pattern substitution-list)))))))

;;; funkcja sprawdzajaca wystepowanie
(defun occursp (var pattern)
	(cond ((equal var pattern) t)
		((or (varp pattern) (is-constant-p pattern)) nil)
		(t (or (occursp var (car pattern))
			(occursp var (cdr pattern))))))

;;; funkcja przypisujaca staly atrybut
(defun is-constant-p (item) (atom item))

;;; funkcja pobierajaca informacje do laczenia
(defun get-binding (var substitution-list) (assoc var substitution-list :test #'equal))

;;; funkcja pobierajaca wartosc do laczenia
(defun get-binding-value (binding) (cdr binding))

;;; funkcja dodajaca nowe podstawienie
(defun add-substitution (var pattern substitution-list) (acons var pattern substitution-list))

;;; definiujemy makro laczenia strumienia
(defmacro cons-stream (exp stream) `(cons ,exp (delay ,stream)))

;;; funkcja dolaczenia naglowka do strumienia
(defun head-stream (stream) (car stream))

;;; funkcja dolaczenia konca do strumienia
(defun tail-stream (stream) (force (cdr stream)))

;;; funkcja oprozniajaca strumien
(defun empty-stream-p (stream) (null stream))

;;; funkcja tworzaca pusty strumien
(defun make-empty-stream () nil)

;;; funkcja laczaca strumienie
(defun combine-streams (stream1 stream2) (append stream1 stream2))

;;; funkcja main
(defun lisp-shell ()
	(setq *case-specific-data* ())
	(print ':)
	(let ((goal (read)))
		(cond ((equal goal 'quit) 'bye)
			(t (print-solutions goal (solve goal (subst-record nil 0)))
				(terpri)
			(lisp-shell)))))

;;; funkcja tworzaca wynik
(defun solve (goal substitutions)
	(filter-stream
		(cond ((conjunctive-goal-p goal)
			(filter-conjunctions
				(cdr (body goal))
				(solve (car (body goal)) substitutions)))
			(t (solve-simple-goal goal substitutions)))
			#'(lambda (x) (< 0.2 (subst-cf x)))))

;;; funkcja tworzaca prosty wynik
(defun solve-simple-goal (goal substitutions)
	(or (infer goal substitutions *assertions*)
		(told goal substitutions *case-specific-data*)
		(ask-for goal substitutions)))

;;; funkcja filtrujaca poprzez strumien
(defun filter-conjunctions (goals substitution-stream)
	(cond ((null goals) substitution-stream)
		((filter-conjunctions
			(cdr goals)
			(filter-through-goal-conj (car goals) substitution-stream)))))

;;; funkcja filtrujaca poprzez cel
(defun filter-through-goal-conj (goal substitution-stream)
	(cond ((empty-stream-p substitution-stream) (make-empty-stream))
		(t (let ((subs (head-stream substitution-stream)))
			(combine-streams (map-stream (solve goal subs)
				#'(lambda (x) (subst-record
						(subst-list x)
						(min (subst-cf x) (subst-cf subs)))))
				(filter-through-goal-conj goal
						(tail-stream substitution-stream)))))))

;;; funkcja wnioskujaca - main engine
(defun infer (goal substitutions kb)
	(cond ((null kb) (make-empty-stream))
		(t (let ((assertion (rename-variables (car kb))))
			(combine-streams
				(cond ((rulep assertion)
					(use-rule goal assertion substitutions))
						(t (use-fact goal assertion substitutions)))
				(infer goal substitutions (cdr kb)))))))

;;; funkcja ustajalaca reguly
(defun use-rule (goal rule substitutions)
	(let ((match (unify goal (conclusion rule) (subst-list substitutions))))
		(cond ((equal match 'failed) (make-empty-stream))
			(t (map-stream (solve (premise rule)
					(subst-record match (subst-cf substitutions)))
				#'(lambda (x) (subst-record
						(subst-list x)
						(* (subst-cf x) (rule-cf rule)))))))))

;;; funkcja ustalajaca wlasciwosci
(defun use-fact (goal pattern substitutions)
	(let ((match (unify goal (fact-pattern pattern) (subst-list substitutions))))
		(cond ((equal match 'failed) (make-empty-stream))
			(t (cons-stream
				(subst-record match (fact-cf pattern))
				(make-empty-stream))))))

;;; funkcja modyfikujaca podstawienia
(defun apply-substitutions (pattern substitution-list)
	(cond ((is-constant-p pattern) pattern)
		((varp pattern)
			(let ((binding (get-binding pattern substitution-list)))
				(cond (binding (apply-substitutions
						(binding-value binding)
						substitution-list))
					(t pattern))))
		(t (cons (apply-substitutions (car pattern) substitution-list)
			(apply-substitutions (cdr pattern) substitution-list)))))

;;; funkcja drukujaca
(defun print-solutions (goal substitution-stream)
	(cond ((empty-stream-p substitution-stream) nil)
		(t (print (apply-substitutions goal
				(subst-list (head-stream substitution-stream))))
		(print " cf = ")
		(print (subst-cf (head-stream substitution-stream)))
		(terpri)
		(print-solutions goal (tail-stream substitution-stream)))))

;;; funkcja filtrujaca strumien
(defun filter-stream (stream filter)
	(cond ((empty-stream-p stream) (make-empty-stream))
		((funcall filter (head-stream stream))
		(cons-stream (head-stream stream)
			(filter-stream (tail-stream stream) filter)))
	(t(filter-stream (tail-stream stream) filter))))

;;; funkcja definiujaca wzor dla zalozen
(defun premise (rule) (nth 2 rule))

;;; funkcja definiujaca wzor dla wnioskow
(defun conclusion (rule) (nth 4 rule))

;;; funkcja definiujaca wzor dla regul
(defun rulep (pattern)
	(and (listp pattern)
		(equal (nth 0 pattern) 'rule)))

;;; funkcja definiujaca dodatkowa regule
(defun rule-cf (rule) (nth 5 rule))

;;; funkcja definiujaca wzor dla faktow
(defun fact-pattern (fact) (car fact))

;;; funkcja definiujaca dodatkowy fakt
(defun fact-cf (fact) (cdr fact))

;;; funkcja definiujaca wzor dla podstawien
(defun subst-list (substitutions) (car substitutions))

;;; funkcja definiujaca dodatkowe podstawienie
(defun subst-cf (substitutions) (cdr substitutions))

;;; definicja calego rekordu podstawien
(defun subst-record (substitutions cf) (cons substitutions cf))

;;; funkcja definiujaca cel
(defun conjunctive-goal-p (goal)
	(and (listp goal)
		(equal (car goal) 'and)))

;;; dodatkowa funkcja
(defun body (goal) (cdr goal))

;;; funkcja zmiany nazw przypisan
(defun rename-variables (assertion)
	(setq *name-list* nil)
	(rename-rec assertion))

;;; funkcja zmiany nazw rekordow
(defun rename-rec (exp)
	(cond ((is-constant-p exp) exp)
		((varp exp) (rename exp))
		(t (cons (rename-rec (car exp))
			(rename-rec (cdr exp))))))

;;; funkcja zmiany nazw zmiennych
(defun rename (var)
	(list 'var (or (cdr (assoc var *name-list* :test #'equal))
			(let ((name (gensym)))
				(setq *name-list* (acons var name *name-list*))
			name))))

;;; funkcja sprawdzajaca czy argument jest staly
(defun is-constant-p (item)
	(atom item))

;;; funkcja sprawdzajaca czy argument jest zmienny
(defun varp (item)
	(and (listp item) (equal (car item) 'var)))

;;; funkcja pobierajaca powiazania
(defun get-binding (var substitution-list) (assoc var substitution-list :test #'equal))

;;; funkcja pobierajaca wartosc (wage) powiazan
(defun binding-value (binding) (cdr binding))

;;; funkcja wyboru konkretnego rekordu podstawien
(defun ask-for (goal substitutions)
	(cond ((askable goal *askables*)
		(let* ((query (apply-substitutions goal (subst-list substitutions)))
			(result (ask-rec query)))
			(setq *case-specific-data* (cons (subst-record query result)
							*case-specific-data*))
			(cons-stream (subst-record (subst-list substitutions) result)
				(make-empty-stream))))))

;;; funkcja wyboru konkretnego rekordu wyszukiwania
(defun ask-rec (query)
	(print query)
	(print '>)
	(let ((answer (read)))
	(cond ((equal answer 'y) 1)
		((equal answer 'n) -1)
		(t (print "answer must be y or n")
			(terpri)
			(ask-rec query)))))

;;; funkcja oznaczajaca mozliwoscia precyzowania
(defun askable (goal askables)
	(cond ((null askables) nil)
		((equal (unify goal (car askables) ()) 'failed) t)
		(t (askable goal (cdr askables)))))

;;; funkcja wyliczajaca podstawienia
(defun told (goal substitutions case-specific-data)
	(cond ((null case-specific-data) (make-empty-stream))
		(t (combine-streams
			(use-fact goal (car case-specific-data) substitutions)
			(told goal substitutions (cdr case-specific-data))))))

;;; laczenie strumieni
(defun combine-streams (stream1 stream2) (append stream1 stream2))

;;; nadpisywanie strumieni
(defun cons-stream (element stream) (cons element stream))

;;; nadpisywanie poczatku strumienia
(defun head-stream (stream) (car stream))

;;; nadpisywanie konca strumienia
(defun tail-stream (stream) (cdr stream))

;;; oproznianie strumienia
(defun empty-stream-p (stream) (null stream))

;;; tworzenie pustego strumienia
(defun make-empty-stream () nil)

;;; mapowanie strumienia
(defun map-stream (stream func)
	(cond ((empty-stream-p stream) (make-empty-stream))
		(t (cons-stream (funcall func (head-stream stream))
					(map-stream (tail-stream stream) func)))))

;;; przygotowanie zestawu zmiennych
(setq *askables* '(
	(rodzaj (var x) (var y))
	(obrabia (var x))
	(narzedzie (var x) (var y))
	(wolne_miejsce (var x))
	(przechodzi_dalej (var x))
	(przyjmuje (var x) (var y))
	(powierzchnia (var x) (var y))))

;;; przygotowanie zestawu regul
(setq *assertions* '(
	(rule
		if(and (rodzaj (var x) toczy) (obrabia (var x)))
		then (tokarka (var x)) .9)
	(rule
		if(and (rodzaj (var x) frezuje) (obrabia (var x)))
		then (frezarka (var x)) .9)
	(rule
		if(and (tokarka (var x)) (wolne_miejsce (var x)) (narzedzie (var x) noz_A))
		then (wynik (var x) przedmiot1) .8)
	(rule
		if(and (tokarka (var x)) (wolne_miejsce (var x)) (narzedzie (var x) noz_B))
		then (wynik (var x) przedmiot2) .8)
	(rule
		if(and (tokarka (var x)) (wolne_miejsce (var x)) (przyjmuje (var x) walek))
		then (toczenie (var x)) 1)
	(rule
		if(and (toczenie (var x)) (narzedzie walek powierzchnia1) (powierzchnia walek dokladnie))
		then (obrobka (var x) dokladna) .8)
	(rule
		if(and (toczenie (var x)) (narzedzie walek powierzchnia2) (powierzchnia walek zgrubnie))
		then (obrobka (var x) zgrubna) .8)))