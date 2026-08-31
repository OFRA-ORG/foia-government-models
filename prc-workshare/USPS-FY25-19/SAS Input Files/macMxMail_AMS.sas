*Macros to read mixed mail activity code mapping, and insert new activity codes;

%macro readMxMail(CSVFile,MxMailMap);
*Read file CSVFile (CSV format) with activity codes that belong to mixed mail codes;
*  Prepare final mixed mail mapping dataset;
Data mxmail1;
 infile &CSVfile DLM="," MISSOVER firstobs=2;
 format mixcode $4.;
 format dircode $4.;
 input mixcode $ dircode $;
run;

Data &MxMailMap; set MxMail1;
run;
%mend;

%macro addActvCodeToMxMail(MxMailMap, actvMatch, actvNew);
*insert new activity code actvNew with same mixed mail cosdes as actvMatch;
proc sql;
 insert into MxMailMap
 select mixcode, &actvNew 
 from mxmail1 
 where dircode = &actvMatch;
quit;
%mend;


*Inserts activity code subsets that Nancy needs for processing, these are not official activity codes; 
*AMS 17Nov17;
%macro addActvCodeToMaster(ActivityCodesTable, ActivityCode, NewCRAProd);
*insert new activity code actvNew with same mixed mail cosdes as actvMatch;
proc sql;
 insert into ActivityCodesTable
 set ActivityCode = &ActivityCode.,
     NewCRAProd = &NewCRAProd.;
quit;
%mend;


/*
%macro addMixedMailShapeToMxMail(MxMailMailMap,mxMailNoShape,shapeCode);
*Insert new mixed mail code with shape info, using first digit of activity code
	as last digit of mixed mail code;
proc sql;
  insert into MxMailMap
    select put(&mxMailNoShape. + &shapeCode.,z4.), dirCode
	from mxMail1
	where mixCode = put(&mxMailNoShape,z4.) and substr(dirCode,1,1) = put(&shapeCode.,z1.);
quit;
%mend;*/
%macro addMixedMailShapeToMxMail(MxMailMailMap,mxMailNoShape);
*Insert new mixed mail code with shape info, using first digit of activity code
	as last digit of mixed mail code;
%let shapeCode = 1;
proc sql;
  insert into MxMailMap
    select put(&mxMailNoShape. + &shapeCode.,z4.), dirCode
	from mxMail1
	where mixCode = put(&mxMailNoShape,z4.) and substr(dirCode,1,1) = put(&shapeCode.,z1.);
%let shapeCode = 2;
proc sql;
  insert into MxMailMap
    select put(&mxMailNoShape. + &shapeCode.,z4.), dirCode
	from mxMail1
	where mixCode = put(&mxMailNoShape,z4.) and substr(dirCode,1,1) = put(&shapeCode.,z1.);
%let shapeCode = 3;
proc sql;
  insert into MxMailMap
    select put(&mxMailNoShape. + &shapeCode.,z4.), dirCode
	from mxMail1
	where mixCode = put(&mxMailNoShape,z4.) and substr(dirCode,1,1) = put(&shapeCode.,z1.);
%let shapeCode = 4;
proc sql;
  insert into MxMailMap
    select put(&mxMailNoShape. + &shapeCode.,z4.), dirCode
	from mxMail1
	where mixCode = put(&mxMailNoShape,z4.) and substr(dirCode,1,1) = put(&shapeCode.,z1.);
quit;
%mend;


/*
*test macros;
%let pathData = C:\AraSardar\IOCS\CARMM;
%readMxMail("&pathData\MxMailCodeFY14.csv",MxMailMap)
%addActvCodeToMxMail(MxMailMap,'1060','1061');
%addActvCodeToMxMail(MxMailMap,'1060','1062');
%addActvCodeToMxMail(MxMailMap,'1060','1063');
%addActvCodeToMxMail(MxMailMap,'1310','1311');
%addActvCodeToMxMail(MxMailMap,'1310','1312');
%addActvCodeToMxMail(MxMailMap,'1310','1313');
%addActvCodeToMxMail(MxMailMap,'1310','1314');
%addActvCodeToMxMail(MxMailMap,'1310','1315');
%addActvCodeToMxMail(MxMailMap,'1310','1316');
%addActvCodeToMxMail(MxMailMap,'1310','1317');
%addActvCodeToMxMail(MxMailMap,'1310','1318');
%addActvCodeToMxMail(MxMailMap,'1310','1319');
%addActvCodeToMxMail(MxMailMap,'2310','2311');
%addActvCodeToMxMail(MxMailMap,'2310','2312');
%addActvCodeToMxMail(MxMailMap,'2310','2313');
%addActvCodeToMxMail(MxMailMap,'2310','2314');
%addActvCodeToMxMail(MxMailMap,'2310','2315');
%addActvCodeToMxMail(MxMailMap,'2310','2316');
%addActvCodeToMxMail(MxMailMap,'2310','2317');
%addActvCodeToMxMail(MxMailMap,'2310','2318');
%addActvCodeToMxMail(MxMailMap,'2310','2319');
%addActvCodeToMxMail(MxMailMap,'3310','3311');
%addActvCodeToMxMail(MxMailMap,'3310','3312');
%addActvCodeToMxMail(MxMailMap,'3310','3313');
%addActvCodeToMxMail(MxMailMap,'3310','3314');
%addActvCodeToMxMail(MxMailMap,'3310','3315');
%addActvCodeToMxMail(MxMailMap,'3310','3316');
%addActvCodeToMxMail(MxMailMap,'3310','3317');
%addActvCodeToMxMail(MxMailMap,'3310','3318');
%addActvCodeToMxMail(MxMailMap,'3310','3319');
%addActvCodeToMxMail(MxMailMap,'4310','4311');
%addActvCodeToMxMail(MxMailMap,'4310','4312');
%addActvCodeToMxMail(MxMailMap,'4310','4313');
%addActvCodeToMxMail(MxMailMap,'4310','4314');
%addActvCodeToMxMail(MxMailMap,'4310','4315');
%addActvCodeToMxMail(MxMailMap,'4310','4316');
%addActvCodeToMxMail(MxMailMap,'4310','4317');
%addActvCodeToMxMail(MxMailMap,'4310','4318');
%addActvCodeToMxMail(MxMailMap,'4310','4319');
proc sort data=MxMailMap nodup;
by mixcode dircode;
run;
*/

******* End macMxMail;
