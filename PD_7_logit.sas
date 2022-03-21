
/* zbior credit_cards */
data credit_cards;
set dane.credit_cards;
run;


/* Chcemy przewidywac status rachunku kartowego po 2 latach od jego zalozenia.
	Zmienna zalezna (target) przyjmuje trzy mozliwe poziomy: 
		- C (charge off/write off) - zamkniete przez bank, wpisane w straty, 
		- A (attrition) - zamkniete przez klienta, 
		- O (still open) - rachunek wciaz aktywny.

	Dysponujemy piecioma potencjalnymi zmiennymi objasniajacymi: 
	- limit_kredytowy - przecietny limit kredytowy na innych kartach, 
	- liczba_umow - calkowita liczba umow w historii kredytowej klienta
			(karty kredytowe, kredyty gotowkowe, hipoteczne, samochodowe, itp.) 
	- utylizacja - utylizacja (wykorzystanie limitu) pozosta³ych kart kredytowych klienta
		(utylizacja > 1 oznacza przekroczenie limitu)
	- wiek - wiek klienta w latach
	- plec - 0=mezczyzna, 1=kobieta. */


/****************/
/* Cwiczenie #1 */
/*  za 0.4 pkt  */
/****************/

/* 1. Oszacuj wielomianowy model logitowy na postawie wszystkich zmiennych objasniajacych, w którym 
      klasa bazowa rachunkut to "O" (still open). 
      1.a. Zinterpretuj jego parametry (ilorazy szans) oraz istotnosc poszczegolnych zmiennych.
	  1.b. Jaki status rachunku jest bardziej prawdopodobny dla mezczyzn, a jaki dla kobiet?
          (wskazowka -> trzeba zmienic klase bazowa modelu dla zmiennej plec)
      1.c. W jaki sposob wplywa na stan rachunku kartowego liczba wszystkich umow klienta? 
*/
proc logistic data = credit_cards;
	class target (ref="O");
	model target = limit_kredytowy liczba_umow utylizacja wiek plec / link = glogit;
run;
/* 1.a. */
/* Dla stanu A wszystkie zmienne maj¹ dodatni wp³yw, ale w stanie C obserwujê ujemny wp³yw. Zmienne nieistotne dla pvalue=0.05 to utylzacja i wiek.
Natomiast zmienne s¹ ³¹cznie istotne*/

/* 1.b. */

proc logistic data = credit_cards;
	class plec (ref="0");
	model target = limit_kredytowy liczba_umow utylizacja wiek plec / link = glogit;
run;

/* Dla mê¿czyzn jest bardziej prawdopodobny stan C, natomiast dla kobiet stan A*/

/* 1.c. */
/* Liczba umów wp³ywa: na stan A dodatnio, na stan C ujemnie*/

/****************/
/* Cwiczenie #2 */
/*  za 0.4 pkt  */
/****************/

/*
   2. Dla oszacowanego modelu zapisz wartosci dopasowane (te z najwyzszym prawdopodobienstwem)
	    2.a. Wyswietl w formie tabeli zaleznosc miedzy wartosciami dopasowanymi, a wartosciami obserwowanymi (macierz b³êdów).
	    2.b. Ile wynosi miara accuracy dla tego modelu (dla jakiego procenta obserwacji model poprawnie przypisal status rachunku)?
  Wskazówki:
  - Zbiór z wartosciami dopasowanymi mozna otrzymac przeksztalcajac zbior z output out w proc logistic
    tak, zeby zostaly tylko _level_ z najwyzszym prawdopodobienstwem
  - Macierz b³êdów i miara accuracy na podstawie poprzednich zajec (proc freq)
*/

proc logistic data=credit_cards;
class target (ref='O');
model target = limit_kredytowy liczba_umow utylizacja wiek plec / link = glogit;
output out=credit_cards_pred p=p predprobs=individual;
run;

data matrix;
set credit_cards_pred;
where _level_ = _into_;
keep _from_ _into_;
run;

/* 2.b */

data matrix;
set matrix;
if _from_=_into_  then poprawny=1;
				  else poprawny=0;
run;

proc freq data=matrix;
	tables poprawny;
run;

/*accuracy = 87,32% */

/****************/
/* Cwiczenie #3 */
/*  za 0.2 pkt  */
/****************/

/*
   3. Wykorzystujac odpowiedni zbior pomocniczy policz i wyswietl 
	prawdopodobienstwa poszczegolnych stanow rachunku
	dla mezczyzny i kobiety w wieku 35 lat, majacych limit kredytowy 2000 zl,
	5 innych umow i utylizacje na poziomie 20%.
*/
data credit_cards1;
	input limit_kredytowy liczba_umow utylizacja wiek plec;
	datalines;
		2000 5 0.20 35 0
		2000 5 0.20 35 1
		;
run;

data credit_cards2;
set credit_cards credit_cards1;
run;


proc logistic data = credit_cards2;
	model target = limit_kredytowy liczba_umow utylizacja wiek plec / link = glogit;
	output out=credit_cards_pred p=p predprobs=individual;
run;

proc print data=credit_cards_pred;
	where target ="";
run;
