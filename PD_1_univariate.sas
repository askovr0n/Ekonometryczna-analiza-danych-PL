
data samochody;
set dane.samochody;
run;
proc contents data=samochody;
run;
/********************************/
/* ÆWICZENIE #1 				*/
/********************************/
/* Dla zbioru DANE.SAMOCHODY przygotuj wykresy rozrzutu
	dla poszczególnych zmiennych (vs. zuzycie) */

axis1 label = (r=0 a=90);
symbol1 value=dot color=blue pointlabel = ("#marka");

proc gplot data=samochody;
	plot zuzycie*(szerokosc pojemnosc moc masa marka ladownosc dlugosc);
run;quit;

/********************************/
/* ÆWICZENIE #2 				*/
/********************************/
/* Dla zbioru DANE.SAMOCHODY przeprowadŸ analizê jednowymiarow¹…
	dla ka¿dej ze zmiennych wyœwietlaj¹c po 7 obserwacji
	skrajnych z ka¿dej strony rozk³adu.	*/

proc univariate data=samochody nextrobs=7;
var szerokosc pojemnosc moc masa ladownosc dlugosc;
	ods select ExtremeObs;
run;

/********************************/
/* ÆWICZENIE #3 				*/
/********************************/
/* Dla zbioru DANE.SAMOCHODY narysuj histogram ka¿dej ze zmiennych */

proc univariate data=samochody noprint;
var dlugosc ladownosc masa moc pojemnosc szerokosc zuzycie;
	histogram dlugosc ladownosc masa moc pojemnosc szerokosc zuzycie / vscale=count;
run;

/* DLA ZMIENNEJ MARKA NIE DA SIE NARYSOWAÆ HISTOGRAMU */


/********************************/
/* ÆWICZENIE #4 				*/
/********************************/
/* Dla zbioru DANE.SAMOCHODY korzystaj¹c z procedury REG
	wyœwietl wykres statystyk Cooka i dŸwigni.
	Czy s¹ obserwacje potencjalnie nietypowe? (napisz w komentarzu) */

proc reg data=samochody;
model zuzycie = szerokosc pojemnosc moc masa ladownosc dlugosc;
output out=samochody_new (keep = zuzycie marka szerokosc pojemnosc moc masa ladownosc dlugosc lev cd)
h=lev cookd=cd;
run; quit;

proc print data=samochody_new;
run;

proc univariate data=samochody_new plots plotsize=40;
var lev;

run;
proc univariate data=samochody_new plots plotsize=40;
var cd;
run;

/* mog¹ istnieæ potencjalne obserwacje nietypowe
jeœli chodzi o leverage to obserwacje nietypowe bêd, gdy (2k+2)/n
w naszym przypadku gdy lev>0.66 */
proc print data=samochody_new;
  where lev > 0.66;
run;
/* program zaznaczy³ nam obs.12 - Mercedes oraz obs.22 - Passat

 /*Nale¿y równie¿ sprawdziæ warunek, gdy cd > 4/n, w naszym przypadku 4/24 */
proc print data=samochody_new;
  where cd >(4/24);
run;
/* program wskaza³ nam obs.4 czyli Poloneza oraz obs.12 - Mercedesa */


/********************************/
/* ÆWICZENIE #5 				*/
/********************************/
/* Dla zbioru DANE.SAMOCHODY zapisz do zbioru samoch_nietypowe
	statystyki DFBETAs.
	Porównaj wartoœci statystyk z wartoœciami brzegowymi (>2/sqrt(n)).
	Które obserwacje wydaj¹ siê nietypowe? Zapisz te obserwacje do zbioru samoch_nietypowe2
*/
	
proc reg data=samochody;
	model zuzycie = szerokosc pojemnosc moc masa ladownosc dlugosc / influence;
	ods output OutputStatistics=samoch_nietypowe;
	run;
	quit;
/*³¹cze ze sob¹ dwie tabele*/
data samoch_nietypowe;
 set samoch_nietypowe;
 rename HatDiagonal=lev;
run;

proc sort data=samoch_nietypowe;
 by lev;
run;

proc sort data=samochody_new;
 by lev;
run;

data samoch_nietypowe;
 merge samochody_new samoch_nietypowe;
 by lev;
run;


data samoch_nietypowe2;
set samoch_nietypowe;
  where abs(DFB_pojemnosc) > 2/sqrt(24) or abs(DFB_moc) > 2/sqrt(24) or abs(DFB_masa) > 2/sqrt(24)
	or abs(DFB_ladownosc) > 2/sqrt(24) or abs(DFB_dlugosc) > 2/sqrt(24);
run;

proc print
data=samoch_nietypowe2;
run;

proc print data=samochody;
run;

/*Wed³ug statystyki DFBETAs jest wiêcej tych obserwacji odstaj¹cych ni¿ w poprzednim przypadku. W zbiorze mam Forda, Skode,  Mercedesa - który jest wspólny dla ka¿dej ze statystyk, Poloneza i Citroena*/
