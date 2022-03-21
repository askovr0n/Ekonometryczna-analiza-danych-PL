
/****************/
/* Æwiczenie #1 */
/*  Za 0,6 pkt  */
/****************/
/*
1. Wygenerowaæ proces ARMA(2,1), 
   tj. y(t)=a1*y(t-1)+a2*y(t-2)+eps(t)+b1*eps(t-1),
 z parametrami a1=0.5, a2=0.3, b1=0.5.
2. Przedstawiæ szereg na wykresie. 
3. Przedstawiæ na wykresie ACF i PACF i dokonaæ ich oceny.
4. Przeprowadziæ estymacjê modelu ARMA(2,1)
5. Sprawdziæ, czy reszty z modelu ARMA(2,1) s¹ bia³ym szumem
*/

data dat01;
  set dane.dat01;
run;

/* 1.1 Generuje proces ARMA(2,1) */

data arma21;
		y=rannor(0);
		y=0;  
		lagy=0;
		lag2y=0;
		lageps=0;
		eps=0;
	do i=-100 to 1000;
		lag2=lagy; 		
		lagy=y;
		lageps=eps;
  		eps=rannor(0);
  		y=0.5*lagy+0.3*lag2y+eps+0.5*lageps; 
  		if i>0 then output;  
 	end;
keep y i;
run;

/* 1.2 Wykres szregu czasowego */

proc gplot data=arma21;
  title1 "Proces ARMA(2,1)";
  symbol1 value=none i=join w=1 c=red;
	plot y*i/vref=0 name='arma21';
run; 

/* 1.3 Oszacowanie ACF i PACF*/

proc arima data=arma21;
	identify var=y outcov=arma21_outcov nlag=50; 
run; quit; 



data arma21_outcov; 
	set arma21_outcov;
	u95=1.96*stderr; 	
	l95=-u95;		
	keep corr partcorr u95 l95 lag;
run;


/*wykres ACF*/

proc gplot data=arma21_outcov;
  title1 "Wykres ACF procesu ARMA(2,1)";
  symbol1 v=none i=needle w=10 c=red;
  symbol2 v=none i=join w=1 c=black;
  symbol3 v=none i=join w=1 c=black;
	plot (corr u95 l95)*lag/overlay name='ACF';
run;quit;

/*wykres PACF*/

proc gplot data=arma21_outcov;
  title1 "Wykres PACF procesu ARMA(2,1)";
  symbol1 v=none i=needle w=10 c=blue;
  symbol2 v=none i=join w=1 c=black;
  symbol3 v=none i=join w=1 c=black;
	plot (partcorr u95 l95)*lag/overlay name='PACF';
run;quit;


/*1.4 Oszacowanie modelu ARMA(2,1)*/

proc arima data=arma21;
	identify var=y nlag=50;
	estimate p=2 q=1 noint;     
	forecast lead=0 out=arma21_r; 
run; quit;

/*1.5 Sprawdzenie bia³ego szumu*/

proc arima data=arma21_r;
	identify var=residual outcov=ar1_r_outcov nlag=50;
run; quit;
/* Brak podstaw do odrzucenia hipotezy 0 -> reszty s¹ bia³ym szumem */


data arma21_r_outcov;
	set arma21_r_outcov;
	u95=1.96*stderr; 	
	l95=-u95;		
	keep corr partcorr u95 l95 lag;
run; 

/*Reszty s¹ bia³ym szumem */

proc gplot data=arma21_r_outcov;
  title1 "Wykres ACF dla reszt";
  symbol1 v=none i=needle w=10 c=red;
  symbol2 v=none i=join w=1 c=black;
  symbol3 v=none i=join w=1 c=black;
	plot (corr u95 l95)*lag/overlay name='ACF';
run;quit;
proc gplot data=arma21_r_outcov;
  title1 "Wykres PACF dla reszt";
  symbol1 v=none i=needle w=10 c=blue;
  symbol2 v=none i=join w=1 c=black;
  symbol3 v=none i=join w=1 c=black;
	plot (partcorr u95 l95)*lag/overlay name='PACF';
run;quit;
/****************/
/* Æwiczenie #2 */
/*  Za 0,4 pkt  */
/****************/

/*
Zbior dat01 zawiera pewien symulowany przebieg procesu ARMA(p,q). 
1. Na podstawie analizy (wizualnej) wykresów ACF i PACF proszê okreœliæ rz¹d parametrów p i q
2. Proszê oszacowaæ parametry takiego modelu z parametrami p i q z punktu 1. Czy oszacowane parametry s¹ istotne?
*/


/*Wykres dla szeregu czasowego*/

proc gplot data=dat01;
  title1 "Proces AR(1)"
  symbol1 value=none i=join w=1 c=red;
	plot y*i/vref=0 name='ar1';
run; 

/* Wyznaczam wartosci ACF i PACF*/
proc arima data=dat01;
	identify var=y outcov=dat01_outcov nlag=50; 
run; quit; 

data dat01_outcov; 
	set dat01_outcov;
	u95=1.96*stderr; 	
	l95=-u95;		
	keep corr partcorr u95 l95 lag;
run;

/*Wykres ACF*/

proc gplot data=dat01_outcov;
  title1 "Wykres ACF procesu AR(1)"
  symbol1 v=none i=needle w=10 c=red;
  symbol2 v=none i=join w=1 c=black;
  symbol3 v=none i=join w=1 c=black;
	plot (corr u95 l95)*lag/overlay name='ACF';
run;quit; 

/*Wykres PACF*/

proc gplot data=dat01_outcov;
  title1 "Wykres PACF procesu AR(1)"
  symbol1 v=none i=needle w=10 c=blue;
  symbol2 v=none i=join w=1 c=black;
  symbol3 v=none i=join w=1 c=black;
	plot (partcorr u95 l95)*lag/overlay name='PACF';
run;quit; 	

/* Wszystkie zmienne statystycznie istotne */

proc arima data=dat01;
	identify var=y nlag=50;
	estimate p=3 q=0; 
	forecast lead=0 
    out=dat01_r;
run; quit;

proc arima data=dat01_r;
	identify var=residual outcov=dat01_r_outcov nlag=50;
run; quit;

/*Brak podstaw do odrzucenia h0 -> reszty s¹ bialym szumem*/

data dat01_r_outcov;
	set dat01_r_outcov;
	u95=1.96*stderr; 	
	l95=-u95;		
	keep corr partcorr u95 l95 lag;
run;

/* Wykres dla reszt */
/* ACF */

proc gplot data=dat01_r_outcov;
  title1 "Wykres ACF dla reszt";
  symbol1 v=none i=needle w=10 c=red;
  symbol2 v=none i=join w=1 c=black;
  symbol3 v=none i=join w=1 c=black;
	plot (corr u95 l95)*lag/overlay name='ACF';
run;quit;

/* PACF */

proc gplot data=dat01_r_outcov;
  title1 "Wykres PACF dla reszt";
  symbol1 v=none i=needle w=10 c=blue;
  symbol2 v=none i=join w=1 c=black;
  symbol3 v=none i=join w=1 c=black;
	plot (partcorr u95 l95)*lag/overlay name='PACF';
run;quit;




