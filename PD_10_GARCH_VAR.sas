
/****************/
/* Æwiczenie #1 */
/*    0,4 pkt   */
/****************/

/*
1. Wygenerowaæ X(t) jako realizacjê procesu GARCH(1,1):
X(t)=res(t)*cvar(t), 
cvar(t)=a0+a1*X(t-1)*X(t-1)+b1*cvar(t-1)
z parametrami a0=0.005, a1=0.1 i b1=0.89
2. Przedstawiæ na wykresie wartoœci realizacji X(t)
3. Obliczyæ i przedstawiæ na wykresie wartoœci ACF dla X(t) i kwadratów X(t)
4. Przedstawiæ statystyki opisowe X(t) i zweryfikowaæ hipotezê o normalnoœci rozk³adu X(t)
5. Oszacowaæ na szeregu X(t) model GARCH(1,1) i porównaæ oceny parametrów z wartoœciami parametrów w Procesie Generuj¹cym Dane.
6. Sprawdziæ, czy reszty z modelu GARCH(1,1) s¹ bia³ym szumem.
*/

/* 1 */

data garch1;
 x = rannor(0);
 cvar = 1;
do i=-100 to 1000;
 res = rannor(0);
 cvar = 0.005 + 0.1*x*x + 0.89*cvar;
 x = res * cvar;
 x2 = x*x;
 if i>0 then output;
end;
keep x x2 cvar res i;
run;

/* 2 */

proc gplot data=garch1;
	 title1 "Proces ARCH(1) z parametrami";
	 title2 h=2 f=cgreek "a0" f=swiss "=0.005,  " 
			f=cgreek "a1" f=swiss "=0.1 i jego warunkowa wariancja";

	axis1 label=('');
	symbol1 i=join v= none w=1 c=red;
	symbol2 i=join v= none w=1 c=green;
	footnote ;
 	plot  x*i/overlay haxis=axis1;
 	plot2 cvar*i ;
run;quit;

/*3 */

proc arima data=garch1;
	identify var=x outcov=x_out nlag=50;
	identify var=x2 outcov=x2_out nlag=50;
run; quit;

data x_out;
	set x_out;
	u95=1.96*stderr; 	/*gorna granica 95% przedzia?u ufno?ci dla ACF i PACF*/
	l95=-u95;		/*dolna granica 95% przedzia?u ufno?ci dla ACF i PACF*/
	keep corr u95 l95 lag;
run;

data x2_out;
	set x2_out;
	u95=1.96*stderr; 	/*gorna granica 95% przedzia?u ufno?ci dla ACF i PACF*/
	l95=-u95;		/*dolna granica 95% przedzia?u ufno?ci dla ACF i PACF*/
	keep corr u95 l95 lag;
run;

/*wykres ACF dla x ... */
proc gplot data=x_out;
  title1 "Funkcja ACF dla realizacji procesu GARCH(1,1)";
  symbol1 v=none i=needle w=10 c=red;
  symbol2 v=none i=join w=1 c=black;
  symbol3 v=none i=join w=1 c=black;
	plot (corr u95 l95)*lag/overlay name='ACF';
run;quit;
/*... i dla x^2*/
proc gplot data=x2_out;
  title1 "Funkcja ACF dla kwadrat?w realizacji procesu GARCH(1,1)";
  symbol1 v=none i=needle w=10 c=red;
  symbol2 v=none i=join w=1 c=black;
  symbol3 v=none i=join w=1 c=black;
	plot (corr u95 l95)*lag/overlay name='ACF';
run;quit;

/* 4 */

goptions reset=all;
proc univariate normal data=garch1;
 var x;
 histogram /normal;
 probplot /normal (mu=est sigma=est);
run;

/*5*/

proc autoreg data=garch1;
 model x=/ garch=(q=1,p=1);
 output out=garch1_out residual=reszty cev=cvar;
run;quit;

/* Okazuje siê, i¿ oceny siê ró¿ni¹ */

/*6 */

data garch1_out;
 set garch1_out;
  reszty_std2=reszty*reszty/cvar;
run;

proc arima data=garch1_out;
identify var=reszty_std2 outcov=garch1_outcov nlag=50; 
run;quit;

data garch1_outcov;
 set garch1_outcov;
u95=1.96*stderr;
l95=-u95;
run;

proc gplot data=garch1_outcov ;
symbol1 interpol=needle value=none color=brown width=15	;
symbol2 interpol=join   value=none color=black			;
symbol3 interpol=join   value=none color=black			;
 title "ACF kwadratow wystandaryzowanych reszt";
 plot (CORR u95 l95)*lag/overlay ;
run;quit;

/* Brak podstaw do odrzucenia hipotezy h0 dla której reszty s¹ bia³ym szumem*/

/****************/
/* Æwiczenie #2 */
/*   0,2 pkt    */
/****************/

/*
Zbiór WIG20 zawiera notowania indeksu WIG20
1. Obliczyæ logarytmiczne stopy zwrotu i przedstawiæ je na wykresie
2. Przedstawiæ korelogram ACF dla kwadratów stóp zwrotu. Czy efekt ARCH jest widoczny?
3. Przedstawiæ statystyki opisowe dla stóp zwrotu i zweryfikowaæ hipotezê o normalnoœci rozk³adu zwrotów.
4. Dokonaæ estymacji modelu GARCH(1,1).
5. Przeprowadziæ diagnostykê kwadratów wystandaryzowanych reszt z modelu GARCH(1,1). 
   Czy model ten mo¿na uznaæ za poprawny?
*/


%include "makra.sas";

/* 1. */
%wczytaj(WIG20);
%wykres(WIG20); 

/* 2. */ 
%acf(WIG20); 

/* Wykres ACF dla kwadratów wygasa geometrycznie */

/*3 */

proc univariate normal data=garch1;
 var x;
 histogram /normal;
 probplot /normal (mu=est sigma=est);
run;

/* brak podstaw do odrzucenia zerowej mowiacej o normalnosci rozkladu reszt */

/* 4. */
proc autoreg data=wig20;
 model wig20=/ garch=(p=1, q=1);
 output out=wig20_out residual=reszty cev=cvar;
run;quit;

/* 5 */

data wig20_out;
 set wig20_out;
  reszty_std2=reszty*reszty/cvar;
run;

proc arima data=wig20_out;
identify var=reszty_std2 outcov=wig20_outcov nlag=50; 
run;quit;

data wig20_outcov;
 set wig20_outcov;
u95=1.96*stderr;
l95=-u95;
run;

proc gplot data=wig20_outcov ;
symbol1 interpol=needle value=none color=brown width=15	;
symbol2 interpol=join   value=none color=black			;
symbol3 interpol=join   value=none color=black			;
 title "ACF kwadratow wystandaryzowanych reszt";
 plot (CORR u95 l95)*lag/overlay ;
run;quit;

/* Model nie mo¿na uznaæ za poprawny, poniewa¿ musimy odrzuciæ hipotezê zerow¹ mówi¹c¹ o bia³ym szumie */

/****************/
/* Æwiczenie #3 */
/*     0,4 pkt  */
/****************/

/* Dla wybranego horyzontu czasowego (proszê napisaæ w komentarzu 
   wybrane okresy in-sample i out-of-sample) przeprowadziæ analizê 1% VaR,
   dla portfela o wartoœci 1 000 000 USD,
   w sk³ad którego wchodzi (ka¿dego dnia):
   - w 20% indeks S&P500
   - w 20% indeks DJIA
   - w 20% indeks FT-SE1000
   - w 20% indeks DAX
   - w 20% indeks NIKKEI
   
   *W analizie wykorzytaj dowolnie wybrany model GARCH.
   *Kwantyl mo¿e byæ obliczane statycznie (bez koniecznoœci przesuwania okna czasowego)
   
*/

/* Rozpatruje indeksy w okresie 23 marca 1990 - 23 marca 2000 */
/* out of sample rozpatruje dla 24 marca 2000 roku */

%wczytaj(SP500); 
%wykres(SP500);  
%acf(SP500);

%wczytaj(DJIA); 
%wykres(DJIA);  
%acf(DJIA);

%wczytaj(FTSE100); 
%wykres(FTSE100);  
%acf(SFTSE100);

%wczytaj(DAX); 
%wykres(DAX);  
%acf(DAX);

%wczytaj(NIKKEI); 
%wykres(NIKKEI);  
%acf(NIKKEI);
   
   
data SP5001;
set SP500;
where Date GE '23mar1990'd AND Date LE '23mar2000'd;
run;

data DJIA2;
set DJIA;
where Date GE '23mar1990'd AND Date LE '23mar2000'd;
run;

data FTSE1001;
set FTSE100;
where Date GE '23mar1990'd AND Date LE '23mar2000'd;
run;

data DAX1;
set DAX;
where Date GE '23mar1990'd AND Date LE '23mar2000'd;
run;

data NIKKEI1;
set NIKKEI;
where Date GE '23mar1990'd AND Date LE '23mar2000'd;
run;

proc surveyselect data=SP5001
	out=SP50011
	method=srs
	samprate=0.2;
run;

proc surveyselect data=DJIA2
	out=DJIA21
	method=srs
	samprate=0.2;
run;

proc surveyselect data=FTSE1001
	out=FTSE10011
	method=srs
	samprate=0.2;
run;

proc surveyselect data=DAX1
	out=DAX11
	method=srs
	samprate=0.2;
run;

proc surveyselect data=NIKKEI1
	out=NIKKEI11
	method=srs
	samprate=0.2;
run;


data table;
set SP50011 DJIA21 FTSE10011 DAX11 NIKKEI11;
run;

 

proc means data=table;
 var r;
 output out=tablestat mean(r)=srednia var(r)=wariancja;
run;

data null; 
set tablestat;
	call symput('srednia',  put(srednia,   best12.));
	call symput('wariancja',put(wariancja, best12.));
run;

data table2;
 set table;
rstd=(r-&srednia)/sqrt(&wariancja);
run;

proc univariate data=table2;
 var rstd;
run;

/*1% kwantyl to -2.755466 a zatem  mniej ni? w przypadku N(0,1)! (-2.327) */ 

data prognoza;
 input rok miesiac dzien;
 datalines;
 2000 03 24
 ;
run;
data prognoza;
 set prognoza;
 date=mdy(miesiac,dzien,rok);
 format date date.;
 keep date;
run;


/*6.*/
/*do??czamy zbi?r z dodatkow? obserwacj? */
data table3;
 set table2 prognoza;
run;

/*7.*/
proc autoreg data=table3;
   	model r = / garch=(q=1,p=1);
	output out=table_out ht=ht; /* zapisujemy warunkowa wariancje do zbioru */
run;

/*8.*/
data table_out;
 set table_out;
  sigma_t=sqrt(ht);
run;


/*10.*/
/*obliczamy warunkowe odchylenie standardowe*/
proc print data=table_out;
where date GE '1mar2000'd;
var date sigma_t;
run;

/*a zatem prognoza odchylenia standardowego na 24 marca 2000 
  wynosi 0.011331 .
 To oznacza, ?e 1% VaR wynosi 0.011331*2.755466*$1000000=$31222,18.
