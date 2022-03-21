
/******************************************************************/
/*                         Æwiczenia                              */
/******************************************************************/

/*GHQ - General Health Questionnare, Goldberg [1972]
Study of a psychiatric screening questionnaire.
The question of interest is how “caseness” 
is related to gender and GHQ score.

UWAGA!
Zbiór GHQ to przyk³ad tzw. danych zgrupowanych (ang. grouped data).
Nie mamy tutaj indywidualnych przypadków tylko informacje ³¹cznej 
liczbie wyst¹pieñ choroby (cases) i ³¹cznej liczbie przypadków, gdzie 
choroba nie wyst¹pi³a (noncases).
*/

data ghq;
 set dane.ghq;
run;

/***************/
/* Æwiczenie 1 */
/* Za 0,2 pkt  */
/***************/
/* Proszê rozgrupowaæ zbiór ghq, tak ¿eby
   otrzymaæ tabelê ghq_individual, w której
   ka¿dy wiersz oznacza indywidualny przypadek.
   W wynikowej tabeli powinny byæ 3 zmienne: 
   - ghq 
   - sex_01 -> zmiena binarna: 1 je¿eli sex = 'F', 0 je¿eli sex = 'M'
   - condition -> 1 je¿eli dany przypadek by³ liczony jako case, 0 je¿eli by³ liczony jako noncase

  Proszê zwróciæ uwagê na nazwê koñcowej tabeli i zmiennych, ¿eby odpowiada³y treœci polecenia.
  
  wskazówki:
  1. W data step trzeba zrobiæ 2 pêtle po cases i po noncases,
     w których bêdzie przypisywany parametr condition.
  2. W wynikowym zbiorze powinno byæ 278 obserwacji.
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
/* Æwiczenie 2 */
/* Za 0,2 pkt  */
/***************/
/* 

  Proszê oszacowaæ model probitowy z dwoma regresorami: ghq i sex_01. 
  Regresantem jest zmienna condition.

  2.1. Parametry modelu (w dowolnym uk³adzie) proszê zapisaæ do zbioru params_ghq_probit.
  2.2. Wartoœci teoretyczne proszê zapisaæ do zbioru pred_ghq_probit.
  2.3. Które zmienne s¹ istotne (poziom istotnoœci p=0.05)? 

  Proszê zwróciæ uwagê na odpowiedni kierunek przewidywania modelu. Prognozowane wy¿sze 
  prawdopodobieñstwo powinno wi¹zaæ siê z wyst¹pieniem choroby (condition = 1), a nie na odwrót.
*/
proc logistic data=ghq_individual simple descending;
	model condition = ghq sex_01 / link = probit;
	output out = pred_ghq_probit p=p;

ods exclude all; 
ods output ParameterEstimates=params_ghq_probit;
ods trace on;   
run;

/* Na poziomie istotnoœci 5% wszystkie zmienne s¹ istotne */


/***************/
/* Æwiczenie 3 */
/* Za 0,3 pkt  */
/***************/
/* 
   Proszê oceniæ dzia³anie modelu z æwiczenia 2. 
   Proszê za³o¿yæ stan condition = 1, je¿eli jego prawdopodobieñstwo jest wiêksze ni¿ 50%.

   3.1. Ile wynosi miara Count R2 (Accuracy)?
   3.2. Ile wynosi miara F1 Score (https://en.wikipedia.org/wiki/F1_score)?

  W wyniku powinien powstaæ zbiór ghq_measures, w ktorym s¹ dwie kolumny: accuracy i f1_score.

  Wskazówka:
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

/* Jest 238 poprawnych obserwacji, co przek³ada siê na 86% poprawnoœci modelu, R^2 wynosi 238/278 */

/***************/
/* Æwiczenie 4 */
/* Za 0,3 pkt  */
/***************/
/* 
  Wizualizacja efektu zmiennej sex_01 w modelu z æwiczenia 2.

  Proszê pokazaæ na wykresach jaki wp³yw na regresanta ma zmienna sex_01,
  w zale¿noœci od zmiennej poziomu ghq. 

  Proszê przyj¹æ na wykrese mo¿liwe wartoœci ghq od 1 do 10, co 0.2. (1.0, 1.2, 1.4, ... 9.8, 10.0)  

  W wyniku tego æwiczenia powinny powstaæ 2 wizualizajce:
  - wykresy prawdopodobieñstw - pokazuj¹ce jak zmienia siê prawodpodobieñstwo condition=1 ze wzrostem ghq dla obu p³ci
  - wykres ró¿nic prawdopodobieñstw - dla obu p³ci wzglêdem zmiennej ghq

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

/*Wykres dla prawdopodobieñstw*/
proc gplot data=ghq1;
 symbol1 v=dot c=red  i=join w=2;
 symbol2 v=dot c=blue i=join w=2;
  plot (predicted0 predicted1)*ghq/overlay legend;
run;quit;


/*wWykres dla ró¿nic prawdopodobieñstw*/
proc gplot data=ghq1;
 symbol1 v=dot c=red  i=join w=2;
  plot (difference)*ghq/overlay;
run;quit;
