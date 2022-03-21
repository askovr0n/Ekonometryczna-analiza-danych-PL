
/**************/
/*Cwiczenie #1*/
/*  0.1 pkt   */
/**************/

/* Prosz� uzupe�ni� makro z �wicze� o dodatkowe makro-zmienne: 
  - parametry a i b w liniowej fukcji regresanta wzgl�dem regresora
  - odchylenie standardowe zaburzenia losowego w tej funkcji.
  Makro powinno si� uruchamia� jako np.:
  %MonteCarlo2(LiczbaPowtorzen=100, WielkoscProbki=100, a=10, b=20, err=7);
*/
%macro MonteCarlo1(LiczbaPowtorzen=, WielkoscProbki=, a=, b=, err=);
data ZbiorParametrow;
run;

data dat1;
	do i=1 to &WielkoscProbki.;
		x = 50 + 50 * ranuni(1); 
		output;
	end;
keep x;
run;

%do i=1 %to &LiczbaPowtorzen.;
%put &i.; 				
	data dat1;	
		set dat1;
		e = &err.* rannor(0);
		y = &a. + &b. * x + e;
		keep y x e;
	run;


	proc reg data=dat1 outest=parametry noprint;
		model y = x;
	run; quit;

	data parametry; 	
	 set parametry;
	  b0=Intercept;
	  b1=x;
	  keep b0 b1;
	run;

	data ZbiorParametrow; 			
	 set ZbiorParametrow parametry;
	run;
%end;

data ZbiorParametrow;
 set ZbiorParametrow;
  if b0=. then delete;
run;

proc univariate data=ZbiorParametrow all;
	title 'Symulacja Monte-Carlo2';
	title2 "liczba powtorek = &LiczbaPowtorzen., wielkosc proby = &WielkoscProbki.";
	histogram b0/normal;
	histogram b1/normal;
run;quit;
%mend; 

%MonteCarlo1(LiczbaPowtorzen=100, WielkoscProbki=100, a=10, b=20, err=7);




/**************/
/*Cwiczenie #2*/
/*  0.2 pkt   */
/**************/

/*Napisz makro kt�re przedstawi rozk�ad statystyki F (histogram)
dla regresji liniowej Y=b0+b1*X1+b2*X2+e
Zalozyc, ze:
n=50;
X1~N(50,10^2);
X2~N(100,15^2);
odchylenie standardowe b��du = 5 (e~N(0,5^2));
b0=0.1
b1=0.15
b2=0.1
*/

%macro MonteCarlo2 (LiczbaPowtorzen=, n=, err=, b0=, b1=, b2=);

data ZbiorParametrow;
run;

data dat2;
	do i=1 to &n.;
		X1 = 50 + 10 * rannor(1);
		X2 = 100 + 15 * rannor(1);
		output;
	end;
keep X1 X2;
run;

%do i=1 %to &LiczbaPowtorzen.;
%put &i.;

	data dat2;
		set dat2;
		e = &err. * rannor(0);
		y = &b0. + &b1. * X1 + &b2. * X2 + e;
		keep y e X1 X2;
	run;

	proc reg data=dat2 outest=parametry2 rsquare noprint;
		model y = X1 X2;
	run; quit;

	data parametry2; 	
	 set parametry2;
	  f=(rsq_/_in)/((1-_rsq_)/(edf));
	  keep f;
	run;

	data ZbiorParametrow;
	 set ZbiorParametrow parametry2;
	run;

%end;

data ZbiorParametrow;
 set ZbiorParametrow;
  if f=. then delete;
run;

proc univariate data=ZbiorParametrow all;
	title "Symulacja Monte-Carlo";
	histogram f/normal;
run; quit;

%mend;

%MonteCarlo2 (LiczbaPowtorzen=50, n=50, err=5, b0=0.1, b1=0.15, b2=0.1);

/*
Podpowiedz:
Rozwi�zanie mo�e bazowa� na kodzie z �wicze�, trzeba:
1. poprawi� Proces Generuj�cy Dane:
	-dwa regresory, zgodnie z zadanym rozkladem
	-y jako funkcja dwoch argumentow
2. poprawi� PROC REG
	-opcja rsquare
	-statystyke F mo�na obliczyc dzieki informacjom
	 o wsp. R-square, liczbie parametr�w do oszacowania 
     oraz liczbie st. swobody:
     f=(_rsq_/_in_)/((1-_rsq_)/(_edf_));
3. poprawi� kod tworz�cy zbi�r w kt�rym kumulowane b�d� 
   obliczanie statystyki F
*/



/**************/
/*Cwiczenie #3*/
/*   0.2 pkt  */
/**************/

/*Za pomoca PROC KDE oszacowac gestosc lacznego rozkladu
  estymatorow b1 i b2 w regresji Cwiczenia #2*/

/* Pa�stwa zadaniem jest modyfikacja makra z �wiczenia #2, 
   samo oszacowanie g�sto�ci mo�na przeprowadzi� tak:

ods graphics on;
proc kde data=zbiorF; 
   bivar b1 b2/ plots=all;
run;
ods graphics off;

   przy za�o�eniu, �e w zbiorze zbiorF znajduja si� oceny b1 i b2
   uzyskane w eksperymencie w Monte Carlo
*/

%macro MonteCarlo3 (LiczbaPowtorzen=, n=, err=, b0=, b1=, b2=);
data ZbiorParametrow;
run;

data dat3;
	do i=1 to &n.;
		x1 = 50 + 10 * rannor(0);
		x2 = 100 + 15*rannor(0);
		output;
	end;
keep x1 x2;
run;


%do i=1 %to &LiczbaPowtorzen.;
%put &i.; 				

    
	data dat3;	
		set dat3;
		e = &err. * rannor(0);
		y = &b0. + &b1. * x1 + &b2. * x2 + e;
		keep y x1 x2 e;
	run;


	proc reg data=dat3 outest=parametry noprint rsquare;
		model y = x1 x2;
		output out=zbiorF (keep = x1 x2);
	run; quit;

	data zbiorF; 	
	 set zbiorF;
	  b1=x1;
	  b2=x2;
	  keep b1 b2;
	run;

	
	data parametry; 	
	 set parametry;
	  f=(rsq_/_in)/((1-_rsq_)/(edf));
	  keep f;
	run;

	data ZbiorParametrow; 			
	 set ZbiorParametrow parametry;
	run;
%end;

data ZbiorParametrow;
 set ZbiorParametrow;
  if f=. then delete;
run;

ods graphics on;
proc kde data=zbiorF; 
   bivar b1 b2/ plots=all;
run;
ods graphics off;


%mend; /*koniec makra*/ 

%MonteCarlo3 (LiczbaPowtorzen=50, n=50, err=5, b0=0.1, b1=0.15, b2=0.1); 

/**************/
/*Cwiczenie #4*/
/* za 0.2 pkt */
/**************/

/*Jaka jest moc testu F weryfikuj�cego hipotez�:
  H0: b1=b2=0 */

/*Podpowied�: 
  Uzupelnic makro z Cwiczenia #2
  statystyka - PRZY SPE�NIONEJ H0 - ma rozk�ad F(2,47)
  funkcja, kt�ra wy�wietli odpowiedni� warto�� krytyczn� to finv(0.95,2,47)
*/

%macro MonteCarlo4 (LiczbaPowtorzen=, n=, err=, b0=, b1=, b2=);
data ZbiorParametrow;
run;

data dat4;
	do i=1 to &n.;
		x1 = 50 + 10 * rannor(0);
		x2 = 100 + 15*rannor(0);
		output;
	end;
keep x1 x2;
run;


%do i=1 %to &LiczbaPowtorzen.;
%put &i.; 				

    
	data dat4;	
		set dat4;
		e = &err. * rannor(0);
		y = &b0. + &b1. * x1 + &b2. * x2 + e;
		keep y x1 x2 e;
	run;

	
	proc reg data=dat4 outest=parametry noprint rsquare;
		model y = x1 x2;
	run; quit;

	
	data parametry; 	
	 set parametry;
	  f=(rsq_/_in)/((1-_rsq_)/(edf));
	  keep f;
	run;

	data ZbiorParametrow; 			
	 set ZbiorParametrow parametry;
	run;
%end;

data ZbiorParametrow;
 set ZbiorParametrow;
  if f=. then delete;
run;

data ZbiorParametrow;
 set ZbiorParametrow;
 statystyka=finv(0.95,2,47);
 test=(f>finv(0.95,2,47)); 
run;

proc means data=ZbiorParametrow;
 var test;
 run;

proc univariate data=ZbiorParametrow all;
	title 'Symulacja MonteCarlo4';
	histogram f/normal;
run;quit;


%mend; /*koniec makra*/

%MonteCarlo4 (LiczbaPowtorzen=50, n=50, err=5, b0=0.1, b1=0.15, b2=0.1);


/**************/
/*Cwiczenie #5*/
/* za 0.1 pkt */
/**************/

/*Jak zmieni sie moc testu F jesli zwiekszymy 
  odchylenie standardowego zaburzenia losowego
  z 5 do 7? */

%macro MonteCarlo5 (LiczbaPowtorzen=, n=, err=, b0=, b1=, b2=);
data ZbiorParametrow;
run;

data dat5;
	do i=1 to &n.;
		x1 = 50 + 10 * rannor(0);
		x2 = 100 + 15*rannor(0);
		output;
	end;
keep x1 x2;
run;


%do i=1 %to &LiczbaPowtorzen.;
%put &i.; 				

    
	data dat5;	
		set dat5;
		e = &err. * rannor(0);
		y = &b0. + &b1. * x1 + &b2. * x2 + e;
		keep y x1 x2 e;
	run;

	
	proc reg data=dat5 outest=parametry noprint rsquare;
		model y = x1 x2;
	run; quit;

	
	data parametry; 	
	 set parametry;
	  f=(rsq_/_in)/((1-_rsq_)/(edf));
	  keep f;
	run;

	data ZbiorParametrow; 			
	 set ZbiorParametrow parametry;
	run;
%end;

/*5.*/

data ZbiorParametrow;
 set ZbiorParametrow;
  if f=. then delete;
run;

data ZbiorParametrow;
 set ZbiorParametrow;
 statystyka=finv(0.95,2,47);
 test=(f>finv(0.95,2,47)); 
run;

proc means data=ZbiorParametrow;
 var test;
 run;

proc univariate data=ZbiorParametrow all;
	title 'Symulacja MonteCarlo5';
	histogram f/normal;
run;quit;


%mend; /*koniec makra*/

%MonteCarlo5 (LiczbaPowtorzen=50, n=50, err=7, b0=0.1, b1=0.15, b2=0.1)



/**************/
/*Cwiczenie #6*/
/* za 0.2 pkt */
/**************/

/* Przygotuj makro MonteCarloPi, kt�re dla zadanego
   parametru LiczbaLosowan, wyznaczy oszacowanie liczby pi
   metod� Monte Carlo. W wyniku uruchomienia powinien powsta� 
   dataset pi_&LiczbaLosowan., z jedn� obserwacj� - oszacowaniem liczby pi

   Uruchom makro dla r�nych parametr�w:
	%MonteCarloPi(LiczbaLosowan = 100); -> tworzy pi_100
	%MonteCarloPi(LiczbaLosowan = 10000); -> tworzy pi_10000
	%MonteCarloPi(LiczbaLosowan = 1000000); -> tworzy pi_1000000

   Podpowied�:
	Koncepcja taka sama jak na zaj�ciach:
	1. W data step wylosuj odpowiedni� liczb� wspolrzednych
		x, y i sprawd� czy dany punkt jest wewn�trz okr�gu.
	2. Zsumuj punkty, kt�re by�y w okr�gu (np. proc summary)
	3. Wyznacz pi wg wzoru (jesli promien = 1, dlugosc
		boku kwadratu = 2): pi = punkty_w_okregu / liczba_obserwacji * 4
*/
%macro MonteCarloPi (LiczbaLosowan=);

data dat_pi;
do i=1 to &LiczbaLosowan.;				
		x = ranuni(0);
		y = ranuni(0);
		circle = (y <= sqrt(1-x**2));
		output;
	end;
	keep y x circle;
	run;quit;
proc summary data=dat_pi(where=(circle=1));
var x y;
output out= dat_pi_sum N=N;
run; quit;

data pi_&LiczbaLosowan; 
set dat_pi_sum;
pi=4*N/&LiczbaLosowan.;
keep pi;
run;
proc means data=Dat_pi;
	title 'Symulacja MonteCarloPi';
run;quit;
%mend; 

%MonteCarloPi(LiczbaLosowan = 100); 
%MonteCarloPi(LiczbaLosowan = 10000);
%MonteCarloPi(LiczbaLosowan = 1000000);
