
/****************************************/
/* Cwiczenie #1         				*/
/* zbi�r DANE.FITNESS					*/
/****************************************/

data fitness;
 set dane.fitness;
run;

/* zbior dane.fitness */
/* Dane z pewnych zaj�� aerobiku
	Zmienne:
		Name        - imi�  
		Sex         - p�e� (F,M)   
		Runtime     - czas biegu na 1.5 mili
		Age         - wiek w latach
		OxygenCons  - zu�ycie tlenu (ml na kg masy cia�a na minut�)
		Performance - ocena sprawno�ci (w punktach)
		RestPulse   - puls w trakcie odpoczynku   
		RunPulse    - puls w trakcie biegu 
		MaxPulse    - waga w kilogramach   
		Weight      - waga   */

/*Przetestuj wystepowanie wspoliniowosci
  w modelu, w ktorym zmienna objasniajaca
  jest OxygenCons, a wszystkie pozosta�e zmienne
  s� zmiennymi obja�niaj�cymi */

proc reg data=fitness;
model OxygenCons = Runtime Age Performance RestPulse RunPulse MaxPulse Weight /vif tol collinoint;
run; quit;

/* ze statystyki VIF wychodzi, i� zmienne Runtime i Performance s� wsp�liniowe poniewaz ich VIF>10, 
natomiast tolerancja przekracza 1/VIF tylko dla statystyki RunTime, w zwi�zku z tym odrzucam j�
*/

/* Ponowne sprawdzenie */
proc reg data=fitness;
model OxygenCons = Age Performance RestPulse RunPulse MaxPulse Weight /vif tol collinoint;
run; quit;
/* teraz jest w porz�dku */

/********************************/
/* �WICZENIE #2 				*/
/********************************/
/*  Wychodzac od modelu bez wsp�liniowosci zbuduj
	  model zawierajacy tylko istotne zmienne. 
 */
proc reg data=fitness;
model OxygenCons = Age Performance RestPulse RunPulse MaxPulse Weight;
run; quit;
/* p-value dla zmiennej RestPulse wynosi 0.9771, wi�c odrzucam t� zmienn� */

proc reg data=fitness;
	model OxygenCons = Age Performance RunPulse MaxPulse Weight;
run;

/* p-value dla zmiennej Weight wynosi 0.1155, wi�c odrzucam t� zmienn� */

proc reg data=fitness;
	model OxygenCons = Age Performance RunPulse MaxPulse;
run;

/* p-value dla zmiennej Age wynosi 0.1130, wi�c odrzucam t� zmienn� */

proc reg data=fitness;
	model OxygenCons = Performance RunPulse MaxPulse;
run;

/* Zmienne, kt�re pozosta�y s� statystycznie istotne na poziomie pvalue = 0.05 */


/********************************/
/* �WICZENIE #3 				*/
/********************************/

/*Dokonaj analizy normalnosci reszt w modelu z �wiczenia 2.*/

/* korzystam z modelu z zadania drugiego ze zmiennymi istotnymi */

proc reg data=fitness;
	model OxygenCons = Performance RunPulse MaxPulse;
	output out = fitness_resid (keep = OxygenCons Performance RunPulse MaxPulse r fv)
		residual=r predicted=fv;
run; quit;


proc univariate data=fitness_resid normal;
 var r;
 qqplot r / normal(mu=est sigma=est);
run;

/* Bazujac na wynikach wszystkich test�w na normalnosc reszt, nie mam podstaw do odrzucenia hipotezy 0, zakladajacej normalnosc reszt w modelu */

/********************************/
/* �WICZENIE #4 				*/
/********************************/

/*Dokonaj analizy heteroskedastycznosci skladnika losowego
  w modelu z �wiczenia 2.*/

proc reg data=fitness;
	model OxygenCons = Performance RunPulse MaxPulse / spec ;
run;

/* Korzystaj�c z testu White, kt�rego hipoteza zerowa zak�ada homoskedastyczno�� reszt, przy value = 0.2372 nie mam podstaw do odrzucenia H0 */

/********************************/
/* �WICZENIE #5 				*/
/********************************/
/*Przeprowad� test na poprawnosc 
  specyfikacji modelu z �wiczenia 2.*/
/* Test dla sprawdzenia obja�nienia regresanta warto�ciami teoretycznymi i ich kwadratami */
proc reg data=fitness;
 model OxygenCons = Performance RunPulse MaxPulse;
 output out=fitness_fv (keep = OxygenCons Performance RunPulse MaxPulse fv) predicted=fv;
run;
quit;

data fitness_fv;
	set fitness_fv;
	fv2 = fv ** 2;
run;

proc reg data = fitness_fv;
	model OxygenCons = fv fv2;
run; quit;

/* Zmienna fv2 okaza�a si� nieistotna i to jest dobre, ale zmienna fv jest r�wnie� nieistotna, a tak nie powinno by�. 
Specyfikacja modelu jest nieprawid�owa.
W celu poprawy modelu usuwam zmienn� z najwy�szym pvalue czyli MaxPulse */
proc reg data=fitness;
 model OxygenCons = Performance RunPulse;
 output out=fitness_fv2 (keep = OxygenCons Performance RunPulse fv) predicted=fv;
run;
quit;

data fitness_fv2;
	set fitness_fv2;
	fv2 = fv ** 2;
run;


proc reg data = fitness_fv2;
	model OxygenCons = fv fv2;
run; quit;

*/Niestety nie uda�o mi si� uzyska� prawid�owej specyfikacji modelu poniewa� fv i fv2 s� nadal nieistotne. */


