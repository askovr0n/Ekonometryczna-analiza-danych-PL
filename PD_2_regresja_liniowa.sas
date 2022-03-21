
/****************************************/
/* Cwiczenie #1         				*/
/* zbiór DANE.FITNESS					*/
/****************************************/

data fitness;
 set dane.fitness;
run;

/* zbior dane.fitness */
/* Dane z pewnych zajêæ aerobiku
	Zmienne:
		Name        - imiê  
		Sex         - p³eæ (F,M)   
		Runtime     - czas biegu na 1.5 mili
		Age         - wiek w latach
		OxygenCons  - zu¿ycie tlenu (ml na kg masy cia³a na minutê)
		Performance - ocena sprawnoœci (w punktach)
		RestPulse   - puls w trakcie odpoczynku   
		RunPulse    - puls w trakcie biegu 
		MaxPulse    - waga w kilogramach   
		Weight      - waga   */

/*Przetestuj wystepowanie wspoliniowosci
  w modelu, w ktorym zmienna objasniajaca
  jest OxygenCons, a wszystkie pozosta³e zmienne
  s¹ zmiennymi objaœniaj¹cymi */

proc reg data=fitness;
model OxygenCons = Runtime Age Performance RestPulse RunPulse MaxPulse Weight /vif tol collinoint;
run; quit;

/* ze statystyki VIF wychodzi, i¿ zmienne Runtime i Performance s¹ wspó³liniowe poniewaz ich VIF>10, 
natomiast tolerancja przekracza 1/VIF tylko dla statystyki RunTime, w zwi¹zku z tym odrzucam j¹
*/

/* Ponowne sprawdzenie */
proc reg data=fitness;
model OxygenCons = Age Performance RestPulse RunPulse MaxPulse Weight /vif tol collinoint;
run; quit;
/* teraz jest w porz¹dku */

/********************************/
/* ÆWICZENIE #2 				*/
/********************************/
/*  Wychodzac od modelu bez wspó³liniowosci zbuduj
	  model zawierajacy tylko istotne zmienne. 
 */
proc reg data=fitness;
model OxygenCons = Age Performance RestPulse RunPulse MaxPulse Weight;
run; quit;
/* p-value dla zmiennej RestPulse wynosi 0.9771, wiêc odrzucam tê zmienn¹ */

proc reg data=fitness;
	model OxygenCons = Age Performance RunPulse MaxPulse Weight;
run;

/* p-value dla zmiennej Weight wynosi 0.1155, wiêc odrzucam tê zmienn¹ */

proc reg data=fitness;
	model OxygenCons = Age Performance RunPulse MaxPulse;
run;

/* p-value dla zmiennej Age wynosi 0.1130, wiêc odrzucam tê zmienn¹ */

proc reg data=fitness;
	model OxygenCons = Performance RunPulse MaxPulse;
run;

/* Zmienne, które pozosta³y s¹ statystycznie istotne na poziomie pvalue = 0.05 */


/********************************/
/* ÆWICZENIE #3 				*/
/********************************/

/*Dokonaj analizy normalnosci reszt w modelu z æwiczenia 2.*/

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

/* Bazujac na wynikach wszystkich testów na normalnosc reszt, nie mam podstaw do odrzucenia hipotezy 0, zakladajacej normalnosc reszt w modelu */

/********************************/
/* ÆWICZENIE #4 				*/
/********************************/

/*Dokonaj analizy heteroskedastycznosci skladnika losowego
  w modelu z æwiczenia 2.*/

proc reg data=fitness;
	model OxygenCons = Performance RunPulse MaxPulse / spec ;
run;

/* Korzystaj¹c z testu White, którego hipoteza zerowa zak³ada homoskedastycznoœæ reszt, przy value = 0.2372 nie mam podstaw do odrzucenia H0 */

/********************************/
/* ÆWICZENIE #5 				*/
/********************************/
/*PrzeprowadŸ test na poprawnosc 
  specyfikacji modelu z æwiczenia 2.*/
/* Test dla sprawdzenia objaœnienia regresanta wartoœciami teoretycznymi i ich kwadratami */
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

/* Zmienna fv2 okaza³a siê nieistotna i to jest dobre, ale zmienna fv jest równie¿ nieistotna, a tak nie powinno byæ. 
Specyfikacja modelu jest nieprawid³owa.
W celu poprawy modelu usuwam zmienn¹ z najwy¿szym pvalue czyli MaxPulse */
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

*/Niestety nie uda³o mi siê uzyskaæ prawid³owej specyfikacji modelu poniewa¿ fv i fv2 s¹ nadal nieistotne. */


