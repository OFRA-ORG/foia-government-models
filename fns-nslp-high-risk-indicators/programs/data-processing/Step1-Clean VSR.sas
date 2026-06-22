* Step1-Clean VSR.sas - clean up SFAID when same ID is used for two clearly different SFANAMES;
libname t 'C:\Data\NSLPmodel\Original';

%include 'C:\Files\My SAS Files\9.1\NSLPmodel\Format-ME.sas';

%macro doit;
 drop recordid;
 length state $ 2;
   state=substr(saname,1,2);
%mend;

*------ year 1 ------------------------------------;
data verif0405a; set t.verif0405; %doit;
 if state='MO' & sfaid='201-201-9048' then delete; * two schools with enrollment<30 in data in 2004-05 & 2008-09;
 if state='TX' & id=8758 then delete; *duplicate record w/diff sfaname (but enroll, is the same);

 if state='NV' & length(sfaid)>4 then do;
    if substr(sfaid,length(sfaid)-4,4)='RCCI' then sfaid=substr(sfaid,1,length(sfaid)-5);
 end;

 * hardcode to fix SFAIDs that appear for multiple SFAnames;
 if state='MO' & id=13892 then sfaid='040-107';
 if state='MO' & id=13344 then sfaid='115-115-6920';
 if state='ND' & id=19231 then sfaid='47019';
 if state='OR' & id=15240 then sfaid='2914002';
 if state='SD' & id=18384 then sfaid='2030200';
 if state='VT' & id=9988  then sfaid='T166';

 * delete/fix duplicate records;
 if id in( 18974, 15444, 742, 6210, 1417, 8878, 8747, 8734, 9398, 8499, 10011) then delete;
 if id=9865 then sfaid='U021';


*------ year 2 ------------------------------------;
data verif0506a; set t.verif0506; %doit;
 if id in(61849,67870) then delete; *SFAID is not unique & this sfaname appears only this year;
 if state='KY' & upcase(sfaname)='KY SCHOOL FOR THE DEAF' then delete; * sfaid not unique, appears in yrs 2,4 only;

 if state='NH' then sfaid=left(sfaname*1.0);

 if state='AK' & id=50589 then sfaid='48';
 if state='AK' & id=50583 then sfaid='43';
 if state='ND' & id=61636 then sfaid='8035';
 if state='NV' & id=61834 then sfaid='7';
 if state='TN' & id=64899 then sfaid='0510';

 * 17 sfas with SFAID='Unknown';
 if state='MS' then do;
   if id=58065 then sfaid='V0000717769';
   if id=58066 then sfaid='V0000717919';
   if id=58068 then sfaid='V0000718179';
   if id=58069 then sfaid='V0000718200';
   if id=58071 then sfaid='V0000718239';
   if id=58072 then sfaid='V0000718289';
   if id=58073 then sfaid='V0000718590';
   if id=58078 then sfaid='V0000719199';
   if id=58085 then sfaid='V0000719630';
   if id=58087 then sfaid='V0000719709';
   if id=58090 then sfaid='V0000724099';
   if id=58098 then sfaid='909';
   if id=58103 then sfaid='V0000721310';
   if id=58108 then sfaid='V0000721789';
   if id=58117 then sfaid='V0000157700';
   if id=58118 then sfaid='V0000722919';
   if id=58124 then sfaid='V0000456159';
 end;

 * delete/fix duplicate records;
 if id in(56203, 58393, 58517, 61643, 64099, 66787, 66960) then delete;
 if id=69168 then sfaid='0100300';


*------ year 3 ------------------------------------;
data verif0607a; set t.verif0607; %doit;
 if state='MS' & id in(104732,104681) then delete; *SFAIDs not unique & sfanames appear only this year;
 if id in(104651,104665) then delete;

 if state='WA' then sfaid=substr(sfaname,1,6);
 if state='ME' then sfaid=put(id,me3id.);  ***** use format to replace sfaid=unknown on all records;

 if state='AK' & id=71610  then sfaid='402';
 if state='DC' & id=77497  then sfaid='NL0763898';
 if state='MS' & id=104691 then sfaid='V0000719200';
 if state='MT' & id=82527  then sfaid='41-0738';
 if state='NV' & id=82550  then sfaid='7';
 if state='SC' & id=92057  then sfaid='1701';

 * delete/fix duplicate records;
 if id in( 71579, 108555, 108641, 77973, 94636, 104689, 104753, 104826, 104744,81974) then delete;
 if id=108379 then sfaid='0100300';
 if id=81975 then sfaname='Seneca R-7';

*------ year 4 -- (ID is not unique, same IDs appear for VT & WA) ----------------------------------;
data verif0708a; set t.verif0708; %doit;
 if state='ID' then sfaid=left(substr(sfaname,length(sfaname)-4,5)*1.0);
 if state='KY' & upcase(sfaname)='KY SCHOOL FOR THE DEAF' then delete; * sfaid not unique, appears in yrs 2,4 only;
 if id= 141875 then delete;

 if state='KY' & id in(122288,122289) then delete; *sfaids not unique, sfanames appear only this yr;
 if state='ND' & id=141875 then sfaid='12345';
 if state='SC' & id=130486 then sfaid='1701';

 * delete/fix duplicate records;
 if id in( 133456, 144264, 144440, 144498) then delete;
 if id=127330 then sfaid='0100300';


*------ year 5 ------------------------------------;
data verif0809a; set t.verif0809; drop recordid;
if state='DE' & id=24957 then delete; * Harvest Christian Academy - not in other years;
if state='MO' & sfaid='201-201-9048' then delete; * two schools with enrollment<30 in data in 2004-05 & 2008-09;

if state='CT' & id=11302 then sfaid='5000';
run;


*------ drop IDAHO dups in 2005-06 --------------------------------------------------------------------;
data id verif0506a; set verif0506a;
 if state='ID' then output id; else output verif0506a;

 proc sort data=id; by sfaid;

data id; set id; by sfaid; if first.sfaid;

data verif0506a; set verif0506a id;



*------ FIX ILLINOIS, 2005-06 -------------------------------------------------------------------------;
** Illinois numeric SFAIDS were compress to an 'e+' format in VSR2005-06 - merge by name to other years;
data il; set verif0506a;
 if state='IL' and puborpriv=1 and index(sfaid,'e+')>0;
  keep id sfaname;
 proc sort; by sfaname;

data il2; set verif0607a; if state='IL' and puborpriv=1; keep sfaid sfaname;
data il3; set verif0708a; if state='IL' and puborpriv=1; keep sfaid sfaname;
data il4; set verif0405a; if state='IL' and puborpriv=1; keep sfaid sfaname;

 proc sort data=il2; by sfaname;
 proc sort data=il3; by sfaname;
 proc sort data=il4; by sfaname;

data match1 nomatch; merge il (in=a) il2 (in=b); by sfaname; if a;
 if b then output match1; else output nomatch;

data match2 nomatch; merge nomatch (in=a) il3 (in=b); by sfaname;if a;
 if b then output match2; else output nomatch;

data match3 nomatch; merge nomatch (in=a) il4 (in=b); by sfaname; if a;
 if b then output match3; else output nomatch;

data ilfix; set match1 match2 match3;
 state='IL';
 rename sfaid=fixid;
                     keep state id sfaid;
 proc sort; by state id;
 proc sort data=verif0506a; by state id;

data verif0506b; merge verif0506a ilfix (in=a); by state id;
  if a then sfaid=fixid; drop fixid;
*------------------------------------------------------------------------------------------------------;

***** SAVE FILES *********;
data t.verif0405a; set verif0405a;
data t.verif0506a; set verif0506a;
data t.verif0607a; set verif0607a;
data t.verif0708a; set verif0708a;
data t.verif0809a; set verif0809a;

run;
