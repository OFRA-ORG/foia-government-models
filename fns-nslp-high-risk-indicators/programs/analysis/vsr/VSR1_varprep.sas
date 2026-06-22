********************************************************************************************************************************;
********************************************************************************************************************************;
/*

This file uses combines the available years of VSR data and then
revises the variables in preparation for hotdeck imputation. The main type of revision is preparing
variables that should sum to a certain total. Specifically,if some elements of the total are
missing they are filled to be consistent with the other elements. If all elements are missing,
they are left alone and passed along to the hotdecking procedure.

*/
********************************************************************************************************************************;
********************************************************************************************************************************;

%let inlib  = D:\TRANSLATE\sas_data;
%let outlib = D:\TRANSLATE\sas_data;

options nocenter ls=170 noxwait mprint nolabel compress=yes;

libname in  "&inlib.";
libname out "&outlib.";

/*----------------------------------------------------------------*/

data final08 (rename=(temp=typeofverif));
 set in.final08;
 if typeofverif="NULL" then typeofverif="";
 temp = input(typeofverif,1.);
 drop typeofverif;
run;

data out.vsr_varprep;
 set in.final04 (in=_2004)
     in.final05 (in=_2005)
     in.final06 (in=_2006)
     in.final07 (in=_2007)
        final08 (in=_2008);

if _2004 then _year=2004;
if _2005 then _year=2005;
if _2006 then _year=2006;
if _2007 then _year=2007;
if _2008 then _year=2008;

if missing(year) and not missing(matchid) then year = _year;

********************************************************************;
* VSR variables;
********************************************************************;

********************************;
*public;
if missing(puborpriv) then puborpriv=1;
if puborpriv~=1       then puborpriv=0;

********************************;
*use random verification sample;
if year<2007  and not missing(typeofverif) then randverif=(typeofverif=1);
if year>=2007 and not missing(typeofverif) then randverif=(typeofverif=2);

********************************;
*provision;
if missing(numprovisionschools) then numprovisionschools=0;

********************************;
*free counts;

t_freeeligcat           = freeeligcat;
t_freeeligincome        = freeeligincome;
t_freeelignotverified   = freeelignotverified;
t_freereportedprovision = freereportedprovision;

if missing(freeeligcat)           then t_freeeligcat=0;
if missing(freeeligincome)        then t_freeeligincome=0;
if missing(freeelignotverified)   then t_freeelignotverified=0;
if missing(freereportedprovision) then t_freereportedprovision=0;

freetot=sum(t_freeeligcat,t_freeeligincome,t_freeelignotverified,t_freereportedprovision);

if nmiss(t_freeeligcat,t_freeeligincome,t_freeelignotverified,t_freereportedprovision)=4 then freetot=.;

if missing(freeeligtot) and not missing(freetot) then freeeligtot=freetot;
if freeeligtot<freetot  and not missing(freetot) then freeeligtot=freetot;

nmfreemiss=nmiss(freeeligcat,freeeligincome,freeelignotverified,freereportedprovision);

if nmfreemiss=1 & freeeligtot>freetot & not missing(freeeligtot) & not missing(freetot) & missing(freeeligcat)           then freeeligcat           = freeeligtot-freetot;
if nmfreemiss=1 & freeeligtot>freetot & not missing(freeeligtot) & not missing(freetot) & missing(freeeligincome)        then freeeligincome        = freeeligtot-freetot;
if nmfreemiss=1 & freeeligtot>freetot & not missing(freeeligtot) & not missing(freetot) & missing(freeelignotverified)   then freeelignotverified   = freeeligtot-freetot;
if nmfreemiss=1 & freeeligtot>freetot & not missing(freeeligtot) & not missing(freetot) & missing(freereportedprovision) then freereportedprovision = freeeligtot-freetot;

if freeeligtot=freetot & not missing(freeeligtot) & missing(freeeligcat)           then freeeligcat=0;
if freeeligtot=freetot & not missing(freeeligtot) & missing(freeeligincome)        then freeeligincome=0;
if freeeligtot=freetot & not missing(freeeligtot) & missing(freeelignotverified)   then freeelignotverified=0;
if freeeligtot=freetot & not missing(freeeligtot) & missing(freereportedprovision) then freereportedprovision=0;

*98% have 0 for freereportedprov therefore replace with 0 if missing;

if missing(freereportedprovision) then freereportedprovision=0;
nmfreemiss=nmiss(freeeligcat,freeeligincome,freeelignotverified,freereportedprovision);

if nmfreemiss=1 & freeeligtot>freetot & not missing(freeeligtot) & not missing(freetot) & missing(freeeligcat)           then freeeligcat           = freeeligtot-freetot;
if nmfreemiss=1 & freeeligtot>freetot & not missing(freeeligtot) & not missing(freetot) & missing(freeeligincome)        then freeeligincome        = freeeligtot-freetot;
if nmfreemiss=1 & freeeligtot>freetot & not missing(freeeligtot) & not missing(freetot) & missing(freeelignotverified)   then freeelignotverified   = freeeligtot-freetot;
if nmfreemiss=1 & freeeligtot>freetot & not missing(freeeligtot) & not missing(freetot) & missing(freereportedprovision) then freereportedprovision = freeeligtot-freetot;

********************************;
*application vars;

if freeeligcat~=0    then freecatappstu = freecateligapps/freeeligcat;
if freeeligincome~=0 then freeincappstu = freeincomeapps/freeeligincome;
if rpelig~=0         then rpappstu      = rpapps/rpelig;
if rpelig~=0         then rpprovstu     = rpreportedprovision/rpelig;

********************************************************************;
*CCD variables;
********************************************************************;

********************************;
*total students;
if missing(member) then member=enrollment;

********************************;
*migrant students;
if migrnt>member & not missing(migrnt) then migrnt=member;
if member~=0 then pmigrant=migrnt/member;

********************************;
*number of schools;

numsch=max(sch,nschs);
label numsch = "CCD: Number of Schools (max of CCD-US AND CCD-SUS)";

********************************;
*ELL;
if member~=0 then pell=ell/member;

********************************;
*level;

if numsch~=0 then psch_level1=level1/numsch;
if numsch~=0 then psch_level2=level2/numsch;
if numsch~=0 then psch_level3=level3/numsch;
if numsch~=0 then psch_level4=level4/numsch;
if numsch~=0 then psch_level5=level5/numsch;

********************************;
*local;

if numsch~=0 then psch_local1=local1/numsch;
if numsch~=0 then psch_local2=local2/numsch;
if numsch~=0 then psch_local3=local3/numsch;
if numsch~=0 then psch_local4=local4/numsch;
if numsch~=0 then psch_local5=local5/numsch;
if numsch~=0 then psch_local6=local6/numsch;

********************************;
*charter, title 1;

if numsch~=0 then psch_charter=charter/numsch;
if numsch~=0 then psch_st1=st1/numsch;
if numsch~=0 then psch_t1=t1/numsch;

********************************;
*race;

t_race1 = race1;
t_race2 = race2;
t_race3 = race3;
t_race4 = race4;
t_race5 = race5;

if missing(race1) then t_race1=0;
if missing(race2) then t_race2=0;
if missing(race3) then t_race3=0;
if missing(race4) then t_race4=0;
if missing(race5) then t_race5=0;

racetot = sum(of t_race:);

if racetot>99 & not missing(racetot) & missing(race1) then race1=0;
if racetot>99 & not missing(racetot) & missing(race2) then race2=0;
if racetot>99 & not missing(racetot) & missing(race3) then race3=0;
if racetot>99 & not missing(racetot) & missing(race4) then race4=0;
if racetot>99 & not missing(racetot) & missing(race5) then race5=0;


********************************;
*F/RP;

temptot=freeeligtot+rpelig;

if temptot~=0 then tempfpct=freeeligtot/temptot;
if temptot~=0 then temprpct=rpelig/temptot;

if missing(frelch) then frelch = round(tempfpct*totfrl);
if missing(redlch) then redlch = round(temprpct*totfrl);

if missing(frelch) then frelch=freeeligtot;
if missing(redlch) then redlch=rpelig;

totfrl=frelch+redlch;

if member~=0 then pfree=frelch/member;
if member~=0 then prp=redlch/member;
if member~=0 then pfrp=totfrl/member;

drop _year t_:;

run;

proc means data=out.vsr_varprep N NMISS MEAN;
run;

proc contents data=out.vsr_varprep;
run;


