
data disks;
 set dane.disks;
run;

proc print data=disks (obs=25);
run;

/*
Serwis komputerowy zatrudnia czterech specjalistow w 
zakresie napraw trzech rodzajow twardych dyskow. 

Kierownik serwisu chce odpowiedziec na pytanie, 
czy czas naprawy dysku zalezy od osoby naprawiajacej 
i rodzaju dysku. Dane znajduja sie w zbiorze DISKS i
zawieraja nastepujace zmienne:

Technician 		imie serwisanta (Bob, Karen, Justin, or Angela)
Brand 			rodzaj dysku twardego (1, 2, or 3)
Time 			czas naprawy (w minutach).
*/

/********************/
/*** Cwiczenie #1 ***/
/***   0.4 pkt    ***/
/********************/


/*
	a. Przeprowadzic dwuczynnikowa analize wariancji, w ktorej regresantem
		bedzie czas naprawy w minutach a regresorami zmienne Technician i Brand. 
		W analizie uwzglednij interakcje miedzy regresorami. */
	
proc glm data=disks;
	class Technician Brand;
	model Time=Technician | Brand;                      
run; quit;

/*
	b. Zakladajac poziom istotnosci na poziomie 0.05 ocenic istotnosc statystyki F.
		Czy interakcja jest istotna?
ODP:
Model jest istotny poniewa¿ p-value < 0.0001

Interakcja jest nieistotna poniewa¿ pvalue < 0.0001


	c. Utworzyc wykres srednich poziomow czynnikow. Czy wykres potwierdza
		wnioski plynace z testu istotnosci interakcji? Dlaczego? */






/********************/
/*** Cwiczenie #2 ***/
/***   0.2 pkt    ***/
/********************/


/*
Przy uzyciu instrukcji LSMEANS wraz z opcja SLICE:

  a. Okreslic czy dla kazdego rodzaju dysku sa istotne roznice pomiedzy serwisantami. 

ODP: Tak, róznice pomiêdzy serwisantami s¹ istotne dla wszystkich typów dysków (0<0.0001)

  b. Zbadac czy dla ka¿dego serwisanta s¹ istotne roznice pomiedzy typami dyskow. 

ODP:
Istotne ró¿nice bêd¹ dla : Angeli (0.0195), Boba (0.0001), Karena (0.0001) 
Nieistotne ró¿nice dla : Justin'a (0.9207)

*/

proc glm data=disks;
	class Brand Technician;
	model Time=Brand|Technician;
	lsmeans Brand*Technician / slice=Brand;
run;quit;

proc glm data=disks;
	class Brand Technician;
	model Time=Brand|Technician;
	lsmeans Brand*Technician / slice=Technician;
run;quit;

proc glm data=school;
	class school gender;
	model reading3=school|gender;
	lsmeans school*gender / slice=gender /* slice=school */;
run;quit;


/********************/
/*** Cwiczenie #3 ***/
/***   0.4 pkt    ***/
/********************/

/*
Sprawdzamy za³o¿enia modelu

	1. Uzywajac instrukcji OUTPUT w PROC GLM zapisac reszty i wartosci teoretyczne
		z modelu w oddzielnym zbiorze danych. Przy pomocy PROC UNIVARIATE
		wyswietlic statystyki opisowe, histogram, wykres prob-prob oraz 
		testy na normalnosc. 
    - Czy reszty z modelu pochodza z rokladu normalnego?
		- Jakie plyna stad wnioski? */

/* ODP: W 3 testach na normalnoœæ reszt okazuje siê, i¿ ich p-value > 0.05, wiec zdecydowanie mamy rozk³ad normalny */

proc glm data=disks;
	class Technician Brand;
	model Time=Technician | Brand;
	output out=check 
		r=residuals 
		p=predicted;
run;

proc univariate data=check;
	var residuals;
	histogram / normal;
	probplot / normal(mu=est sigma=est);
run;


/*
	2. Zbadac czy spelnione jest zalozenie o stalosci wariancji. Za pomoca PROC GPLOT
		przedstawic wykres reszt. W kroku DATA utworzyc pojedyncza zmienna 'group',
		ktora jednoczesnie bedzie uwzgledniac rozroznienie ze wzgledu na serwisanta 
		oraz poziom rodzaju dysku. Oszacowac jednoczynnikowa ANOVE ze zmienna 'group' jako regresorem
		i przeprowadzic test na homogenicznosc wariancji. 
    - Czy wariancja jest homogeniczna?
    - Jakie wnioski?

/* ODP: p-value = 0.9528 wieæ jest wiêksze ni¿ 0.05, wiêc nie mam podstaw do odrzucenia hipotezy h0 o homogenicznoœci wariancji
		Podpowiedz: 
			group=compress(technician || brand);	
*/
proc gplot data=check;
	plot residuals*predicted 
	/ vref=0;
run; quit;

data disks;
	set dane.disks;
	group=compress(Technician || Brand);
run;

proc glm data=disks;
	class group;
	model Time=group;
	means group / hovtest /* Test Levene'a na homogonicznoœæ wariancji. 
                  H0 mówi, ¿e wariancje s¹ równe w grupach. */
;
run;quit;


