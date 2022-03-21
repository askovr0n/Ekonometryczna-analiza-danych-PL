
/******************************************************************/
/*                         �wiczenia                              */
/******************************************************************/

/*GHQ - General Health Questionnare, Goldberg [1972]
Study of a psychiatric screening questionnaire.
The question of interest is how �caseness� 
is related to gender and GHQ score.

UWAGA!
Zbi�r GHQ to przyk�ad tzw. danych zgrupowanych (ang. grouped data).
Nie mamy tutaj indywidualnych przypadk�w tylko informacje ��cznej 
liczbie wyst�pie� choroby (cases) i ��cznej liczbie przypadk�w, gdzie 
choroba nie wyst�pi�a (noncases).
*/

data ghq;
 set dane.ghq;
run;

/***************/
/* �wiczenie 1 */
/* Za 0,2 pkt  */
/***************/
/* Prosz� rozgrupowa� zbi�r ghq, tak �eby
   otrzyma� tabel� ghq_individual, w kt�rej
   ka�dy wiersz oznacza indywidualny przypadek.
   W wynikowej tabeli powinny by� 3 zmienne: 
   - ghq 
   - sex_01 -> zmiena binarna: 1 je�eli sex = 'F', 0 je�eli sex = 'M'
   - condition -> 1 je�eli dany przypadek by� liczony jako case, 0 je�eli by� liczony jako noncase

  Prosz� zwr�ci� uwag� na nazw� ko�cowej tabeli i zmiennych, �eby odpowiada�y tre�ci polecenia.
  
  wskaz�wki:
  1. W data step trzeba zrobi� 2 p�tle po cases i po noncases,
     w kt�rych b�dzie przypisywany parametr condition.
  2. W wynikowym zbiorze powinno by� 278 obserwacji.
*/

proc contents data=ghq;
run;

data ghq;
 set ghq;
 if sex='F' 	then sex_01=1;
 				else sex_01=0;
run;

data ghq_individual;
run;

data ghq_individual;
set ghq;
do i=1 to cases;
	condition=1;
	output;
end;
do i=1 to noncases;
	condition=0;
	output;
end; 
keep ghq sex_01 condition;
run;



/***************/
/* �wiczenie 2 */
/* Za 0,2 pkt  */
/***************/
/* 

  Prosz� oszacowa� model probitowy z dwoma regresorami: ghq i sex_01. 
  Regresantem jest zmienna condition.

  2.1. Parametry modelu (w dowolnym uk�adzie) prosz� zapisa� do zbioru params_ghq_probit.
  2.2. Warto�ci teoretyczne prosz� zapisa� do zbioru pred_ghq_probit.
  2.3. Kt�re zmienne s� istotne (poziom istotno�ci p=0.05)? 

  Prosz� zwr�ci� uwag� na odpowiedni kierunek przewidywania modelu. Prognozowane wy�sze 
  prawdopodobie�stwo powinno wi�za� si� z wyst�pieniem choroby (condition = 1), a nie na odwr�t.
*/
proc logistic data=ghq_individual simple descending;
	model condition = ghq sex_01 / link = probit;
	output out = pred_ghq_probit p=p;

ods exclude all; 
ods output ParameterEstimates=params_ghq_probit;
ods trace on;   
run;

/* Na poziomie istotno�ci 5% wszystkie zmienne s� istotne */


/***************/
/* �wiczenie 3 */
/* Za 0,3 pkt  */
/***************/
/* 
   Prosz� oceni� dzia�anie modelu z �wiczenia 2. 
   Prosz� za�o�y� stan condition = 1, je�eli jego prawdopodobie�stwo jest wi�ksze ni� 50%.

   3.1. Ile wynosi miara Count R2 (Accuracy)?
   3.2. Ile wynosi miara F1 Score (https://en.wikipedia.org/wiki/F1_score)?

  W wyniku powinien powsta� zbi�r ghq_measures, w ktorym s� dwie kolumny: accuracy i f1_score.

  Wskaz�wka:
   - F1 Score = 2 * (precision * recall) / (precision + recall)
   - precision = true positives / (true positives + false positives)
   - recall = true positives / (true positives + false negatives)
*/


data pred_ghq_probit;
 set pred_ghq_probit;
	if p<=0.5
  then y=0; 
	else y=1;
run;

proc freq data=pred_ghq_probit;
	tables condition * y / out=cross_results_ghq;
run;

data cross_results_ghq;
  set cross_results_ghq;
  if condition = 1 and y = 1 then result_type = 'TP';
  if condition = 1 and y = 0 then result_type = 'FN';
  if condition = 0 and y = 1 then result_type = 'FP';
  if condition = 0 and y = 0 then result_type = 'TN';
run;

proc transpose data=cross_results_ghq out=cross_results__ghq_transposed;
  var count;
  id result_type;
run;

data pred_ghq_probit;
 set pred_ghq_probit;
 if condition=y then poprawny=1;
 			else poprawny=0;
run;

proc freq data=pred_ghq_probit;
 tables poprawny;
run;

data ghq_measures;
set cross_results__ghq_transposed;
R2 = (TP + TN)/(TP + TN + FP + FN);
precision = TP / (TP + FP);
recall = TP / (TP + FN);
F1score = 2 * (precision * recall) / (precision + recall);
keep R2 F1score;
run;

/* Jest 238 poprawnych obserwacji, co przek�ada si� na 86% poprawno�ci modelu, R^2 wynosi 238/278 */

/***************/
/* �wiczenie 4 */
/* Za 0,3 pkt  */
/***************/
/* 
  Wizualizacja efektu zmiennej sex_01 w modelu z �wiczenia 2.

  Prosz� pokaza� na wykresach jaki wp�yw na regresanta ma zmienna sex_01,
  w zale�no�ci od zmiennej poziomu ghq. 

  Prosz� przyj�� na wykrese mo�liwe warto�ci ghq od 1 do 10, co 0.2. (1.0, 1.2, 1.4, ... 9.8, 10.0)  

  W wyniku tego �wiczenia powinny powsta� 2 wizualizajce:
  - wykresy prawdopodobie�stw - pokazuj�ce jak zmienia si� prawodpodobie�stwo condition=1 ze wzrostem ghq dla obu p�ci
  - wykres r�nic prawdopodobie�stw - dla obu p�ci wzgl�dem zmiennej ghq

*/

data _null_;
set params_ghq_probit;
where variable='Intercept';
call symput('beta0', put(estimate, best12.));
run;
%put &beta0.;

data _null_;
	set params_ghq_probit;
	where variable='ghq';
	call symput('beta1', put(estimate, best12.));
run;
%put &beta1.;

data _null_;
	set params_ghq_probit;
	where variable='sex_01';
	call symput('beta2', put(estimate, best12.));
run;
%put &beta2.;

data ghg1;
 do ghq=1 to 10 by 0.2;
 do sex_01=0 to 1 by 1;
  z0=&beta0. + &beta1.*ghq + &beta2.*0; /*mezczyzna*/
  z1=&beta0. + &beta1.*ghq + &beta2.*1;  /*kobieta*/
  predicted0=cdf('normal',z0);
  predicted1=cdf('normal',z1);
  difference=predicted1-predicted0;
 output;
 end;
 end;
run;

/*Wykres dla prawdopodobie�stw*/
proc gplot data=ghq1;
 symbol1 v=dot c=red  i=join w=2;
 symbol2 v=dot c=blue i=join w=2;
  plot (predicted0 predicted1)*ghq/overlay legend;
run;quit;


/*wWykres dla r�nic prawdopodobie�stw*/
proc gplot data=ghq1;
 symbol1 v=dot c=red  i=join w=2;
  plot (difference)*ghq/overlay;
run;quit;
