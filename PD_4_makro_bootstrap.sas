
/***************/
/* Cwiczenie 1.*/
/* za 0.4 pkt  */
/***************/

/*
  Napisaæ makro, które podobnie jak w przykladzie IV z æwiczeñ
  na podstawie 50-elementowej próbki
  oszacuje za pomoca metod bootstrap 95%-przedial ufnosci
  dla mediany z proby.

  W wyniku dzia³ania makra powinien powstaæ zbior: median_conf_interval,
  zawieraj¹cy dolne i górne oszacowanie przedzia³u ufnoœci.
*/

data dat1;
 do i = 1 to 50;
 	x = 200 + 20*rannor(1);
	output;
 end;
 keep x;
run;

proc print data=dat1;
run;

%macro bootstrap1(n=);

data dat2;
 xmedian=0;
run;

%do i=1 %to &n;
%PUT &i;

proc surveyselect data=dat1 method=urs
n=50 out=dat3 noprint;
run;

proc means data=dat3 noprint median;
	weight NumberHits;
	output out=dat4 median(x)=xmedian;
run;

data dat2;
	set dat2 dat4;
run;

%end;

data dat2;
 set dat2;
  if xmedian=0 then delete;
run;

data dat2;
 set dat2;
 keep xmedian;
run;
%mend;

%bootstrap1(n=50);

proc univariate data=dat2;
	var xmedian;
	output out=median_conf_interval pctlpre=xmedian_ pctlpts=2.5 97.5;
run;






/***************/
/* Cwiczenie 2.*/
/* za 0.6 pkt  */
/***************/

/*
  Podobnie jak w Cwiczeniu 1 ale tym razem szukamy 95%-przedzalu ufnosci
  dla œredniej ucietej (trimmed mean), obliczonej na podstawie 
  50-elemetowej próbki pomniejszonej o wartosc maksymalna i minimalna.

  W wyniku dzia³ania makra powinien powstaæ zbior: tmean_conf_interval,
  zawieraj¹cy dolne i górne oszacowanie przedzia³u ufnoœci.

  WSKAZÓWKA:
  - warto zmieniæ opcjê w procedurze surveyselect
  - zeby wyrzucic skrajne obserwacje mo¿e siê przyszaæ proc sort
*/

%macro bootstrap2(n=);

data datt;
 xtrimmedmean=0;
run;

%do i=1 %to &n;
%PUT &i;

proc sort data=dat1;
by x;
run;
data datt;
	set dat1 NOBS=count;
	if _n_ = 1 then delete;
	if count - _n_ = 1 then delete;
run;

proc surveyselect data=dat1 method=urs
n=50 out=data2a noprint;
run;

proc means data=data2a noprint median;
	weight NumberHits;
	output out=dat3a mean(x)=xtrimmedmean;
run;

data datt;
	set datt dat3a;
run;

%end;

data datt;
 set datt;
  if xtrimmedmean=0 then delete;
run;

data datt;
 set datt;
 keep xtrimmedmean;
run;
%mend;

%bootstrap2(n=50);

proc univariate data=datt;
	var xtrimmedmean;
	output out=tmean_conf_interval pctlpre=xtrimmedmean_ pctlpts=2.5 97.5;
run;

