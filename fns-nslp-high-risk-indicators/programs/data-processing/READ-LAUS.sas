** Read-LAUS.sas - read all series and keep state and county unemployment statistics;

*--------------------------------------------------------;
** READ LIST OF LAUS CODES THAT CORRESPOND TO COUNTIES **;
*--------------------------------------------------------;
proc import out= lauscnty
     datafile= "c:\work\nslpmodel\data\laus\laus-county-list.xls"
     dbms=excel replace;
     sheet="'laus-county-list$'";
     getnames=yes; mixed=no; scantext=yes; usedate=yes; scantime=yes;

data ccd.lauscnty; set lauscnty;
 label areanum  ='LAUS area #'
       areaname ='County name'
       state    ='State FIPS'
       county   ='County FIPS'
       conum    ='State & county FIPS';
  proc sort; by areanum;
run;


*--------------------------------------------------------;
** READ LAUS DATA - MONTHLY UNEMP RATES FOR ALL AREAS  **;
*--------------------------------------------------------;

%macro readit;
 informat series_id $char17. period $char3. fcode $char1.;
 input
   series_id    /*  LASPS36040003 */
   year         /*  1990          */
   period       /*  M01           */
   unemp
   fcode;

   seasonal=(substr(series_id,3,1)='S');
   stcode  = substr(series_id,6,2)*1.0;
   state   = fipstate( stcode );         if stcode=43 then state='PR';
   measure = substr(series_id,13,1)*1.0;
   areatype= substr(series_id,4,2);
   areanum = substr(series_id,4,8);

  label
   seasonal= 'S if seasonally adjusted'
   stcode  = 'State FIPS'
   state   = 'State postal code'
   unemp   = 'Unemployment rate'
   period  = 'M1-M12=monthly, M13=annual'
   measure = 'Measure code (always 3?)'
   areatype= 'Area type code'
   areanum = 'Area number'
   year    = 'Year'

 run;
%mend;


options obs=max;
data laus1; infile 'C:\Work\NSLPmodel\Data\LAUS\laus2000-2004.txt' delimiter='09'x firstobs=2 missover; %readit
data laus2; infile 'C:\Work\NSLPmodel\Data\LAUS\laus2005-2009.txt' delimiter='09'x firstobs=2 missover; %readit

  *------ keep 2004-2008 annual unemployment rates for counties ----;
data laus; set laus1 laus2;
  if measure=3 and period='M13' and 2004<=year<=2008 then output;
 proc sort; by areanum;


data ccd.laus; merge laus (in=a) ccd.lauscnty (in=b keep=areanum areaname conum); by areanum;
  if a & b;
  keep conum unemp areaname year;
 proc sort; by year conum;

 proc print data=ccd.laus; where year=2008;
run;



/*
The series_id (LASPS36040003) can be broken out into:

Code                     Value
--------------------     ------
survey abbreviation   =   LA
seasonal (code)       =   S or U
area_type_code        =   PS
areanum               =   PS360400    << this code maps to the list of counties >>
measure_code          =   03

area_type_code
BS  Balance of State
CA  Combined Statistical Area
CC  Towns in Massachusetts
CN  Counties
CT  Towns/cities
DV  Divisions
ID  NECTA Division
IM  State-specific portion of Metropolitan Statistical Area
MC  Micropolitan Statistical Area
ME  Towns in Maine
MT  Metropolitan Statistical Area
NH  Towns in New Hampsire
PA  Counties, cities, towns          << identify counties through merge to list of counties >>
PS  Counties, cities, towns
PT  County-specific portion of cities
RD  Regional division
SA  LMA
ST  State
VT  Towns in Vermont

measure_codes
03      unemployment rate
04      unemployment
05      employment
06      labor force

period   period_name
M01      JAN      January
M02      FEB      February
M03      MAR      March
M04      APR      April
M05      MAY      May
M06      JUN      June
M07      JUL      July
M08      AUG      August
M09      SEP      September
M10      OCT      October
M11      NOV      November
M12      DEC      December
M13      AN AV      Annual Average


footnote_code      footnote_text
A      Area boundaries do not reflect official OMB definitions.
B      Reflects revised population controls, model reestimation, and new seasonal adjustment.
C      Corrected.
D      Reflects revised population controls and model reestimation.
J      Reflects early 2010 prorating of substate model-based estimate to new state control.
K      Estimates are not model-based as of Sept. 2005 and are published with the other metropolitan areas.
L      Estimates are not model-based as of Sept. 2005 and are published with all other substate areas.
N      Not available.
P      Preliminary.
Q      Annual average is average of 12 months developed using two different methodologies.
Z      Reflects revised seasonal factors.

state code
72            Puerto Rico
80            Census Regions and Divisions
*/
