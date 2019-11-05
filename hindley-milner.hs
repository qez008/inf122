module HindleyMilner where
  {- 
			Hindley-Milner reduseres til Unifikasjon
				
				Substitusjon
			  	Unifikasjon er å finne en substitusjon
			  	En mengde lingninger, a = {x1 = t1 ,..., xn = tn}. der variablene til venstre er forskjellige
					Anvend en substitusjon a på en term t, a(t):
						erstatt samtidig alle forekomster av hver xi i t med ti
				
				Unifikasjon
					(forening)
					substitusjon som gir syntaktis likhet

			Martelli-Montanari

				Unifikasjonsalgoritme
					Ikke-determeninstisk
					Resultatene er ekvivalente
					Inndata: W = {t1 = t1', ... ,tn = tn'}
					Utdata: en unifikasjon eller NO
					Betingelse: utdata skal være en mest generell unifikasjon (mgu) av E (hver annen unifikasjon 
					kan fås vha. susbstiusjon)
					
   -}