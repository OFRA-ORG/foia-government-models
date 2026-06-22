*Step5-Read LinkFile.sas;
* read linkfiles from Excel file and save as SAS in FinalDat directory;


PROC IMPORT OUT= fd.CCDlink
     DATAFILE= "C:\Work\NSLPmodel\Data\LinkFile\VSR-CCD LinkFile.xls"
     DBMS=EXCEL REPLACE;
     SHEET="CCDlink$";
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

PROC IMPORT OUT= fd.VSRlink
     DATAFILE= "C:\Work\NSLPmodel\Data\LinkFile\VSR-CCD LinkFile.xls"
     DBMS=EXCEL REPLACE;
     SHEET="VSRlink$";
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
