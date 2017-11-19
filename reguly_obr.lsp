;;; Reguly dla systemu ekspertowego (przykladowe)
;;;
(setq *askables* '(
	(rodzaj (var x) (var y))
	(obrabia (var x))
	(narzedzie (var x) (var y))
	(wolne_miejsce (var x))
	(przechodzi_dalej (var x))
	(przyjmuje (var x) (var y))
	(powierzchnia (var x) (var y))))
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