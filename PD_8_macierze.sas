

/**************/
/*Æwiczenie #1*/
/*  0.1 pkt   */
/**************/

/*Rozwi¹¿ poni¿szy uk³ad rownañ 
4x + 2y + 6z = 6
7x + 2y + 4z = 2
1x + 2y + 5z = 7

Wskazówka: statement solve
*/

proc iml;
a = {4 2 6,
	 7 2 4,
	 1 2 5};
b = {6,
	 2,
	 7};
c = solve(a, b);
print c;
quit;

/* ODP:
	x = -0.(6)
    y = 1.(3)
    z = 1 */

/**************/
/*Æwiczenie #2*/
/*    0.2 pkt */
/**************/

/*Uzupe³nij Przyk³ad #2 z æwiczeñ, o œredni¹ liczbê kaw na osobê i na dzieñ
  Na podstawie danych z Przyk³adu #2 o liczbie i koszcie kaw wyœwietl
  macierze:
  1. Ze œredni¹ liczb¹ i œrednim kosztem kaw dla ka¿dego dnia
  2. Ze œredni¹ liczb¹ i œrednim kosztem kaw dla ka¿dej osoby
*/

proc iml;

/*tworzenie macierzy: elemety wierszy oddzielone spacjami, 
  wiersze oddzielone przecinkami. Ca³oœæ macierzy objêta nawiasami klamrowymi*/
kawa={4 2 2 3 2,
	  3 3 1 2 2,
	  2 1 0 4 5};

/*wektor wierszowy*/
dni={"Poniedzia³ek" "Wtorek" "Œroda" "Czwartek" "Pi¹tek"};
/*wektor kolumnowy*/
imiona={"Tytus","Romek","A'Tomek"};

/*wstêpna macierz*/
print kawa[r=imiona c=dni];

koszt_dnia=1.50*kawa;
print koszt_dnia;

/*tworzenie wektora kolumnowego wype³nionego jedynkami*/
ones=j(5,1);
suma_tygodniowo=koszt_dnia*ones;
print ones, suma_tygodniowo; 

/*sumowanie wartoœci elementów macierzy */

srednia_tygodniowo=koszt_dnia[,+] / ncol(koszt_dnia);

suma_dziennie=koszt_dnia[+,];

suma=koszt_dnia[+,+];
print suma_tygodniowo,srednia_tygodniowo,suma_dziennie,suma;

print 'Tygodniowe wydatki na kawê';
print koszt_dnia[r=imiona c=dni]; suma_tygodniowo[format=dollar7.2]; ,
	suma_dziennie[c=dni f=dollar8.2] '  ' suma[f=dollar7.2];

/* Po wstêpnych krokach moge przejœæ do odpowiedzi na dwa pytania */

/* 1 pytanie */

cola = kawa[:,];
colb = koszt_dnia[:,];
colmean = cola//colb;
tab1 = {"Œrednia liczba kaw dla danej osoby",
		   "Œredni koszt kaw dla danej osoby"};
print colmean[r = tab1 c = dni];

/* 2 pytanie */

rowa = kawa[,:];
rowb = koszt_dnia[,:];
rowmean= rowa||rowb;
tab2 = {"Œrednia liczba kaw dla danej osoby" "Œredni koszt kaw dla danej osoby"};
print rowmean[r = imiona c = tab2];
quit;


/**************/
/*Æwiczenie #3*/
/*  0.5 pkt   */
/**************/

/*
1. Za pomoc¹ data step wygeneruj po 20 obs. zmiennych pochodz¹cych z poni¿szych rozk³adów:
X1~N(1,2^2);
X2~N(2,2^2);
X3~N(3,2^2);
X4~N(4,2^2);
X5~N(5,2^2); */

data dat;
 do i=1 to 20;
    a=1+2*rannor(10000);
	b=2+2*rannor(10000);
	c=3++2*rannor(10000);
	d=4++2*rannor(10000);
	e=5++2*rannor(10000);
 	output;
 end;
run;
/*
2. Przerzuæ zawartoœæ zbioru z pkt. 1 do macierzy o wymiarach 20x5 w PROC IML */

proc iml;
START ANOVA;
use dat;
read all;
show names;
matrix = a || b || c || d || e;
print matrix;
FINISH ANOVA;
RUN ANOVA;

/* 3. Za pomoc¹ PROC IML przeprowadŸ jednoczynnikowa analize wariancji, ktora zweryfikuje hipoteze:
H0: E(X1)=E(x2)=E(x3)=E(x4)=E(x5). Czy ró¿nice pomiêdzy zmiennymi s¹ istotne na poziomie istotnoœci 0.05? 

Wskazówki:
- porównaj wynik z wynikami ANOVY przeprowadzonej za pomoc¹ PROC GLM
- zajêcia nr 7 na temat oneway ANOVA (w szczególnoœci plik word z interpretacj¹ PROC GLM) */




/**************/
/*Æwiczenie #4*/
/*   0.2 pkt  */
/**************/

/*Na podstawie Przyk³adu #4 z zajêæ wyœwietl wykres dla 3 zmiennych z geometrycznego bladzenia losowego:
  S_t=S_{t-1}*exp(eps_t), gdzie eps_t~N(0,0.05^2)
*/

proc iml;
  ssize=1000; 
  mean=0;
  std=0.05;       
  free rw;
  do i=1 to 3;
    sample = 0;
    free thisRW;
    do k=2 to ssize;
      sample = sample // exp(normal(0) * std + mean);
      thisRW = thisRW // sample[+, ]; 
    end;
    rw=rw||thisRW; 
  end;
print rw;


create rw from rw; 
append from rw;
close rw; 
quit;


data rw;
 set rw;
 i=_n_;
run;

proc gplot data=rw;
symbol1 i=join v=none w=1;
symbol2 i=join v=none w=1;
symbol3 i=join v=none w=1;

plot(col1--col3)*i/overlay;
run;quit;

