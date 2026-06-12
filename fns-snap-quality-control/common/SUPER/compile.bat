@echo off


set COMPILE_FLAGS=-std=f2008 -Wall -Wextra -Werror -Wpedantic -O3 -fcheck=all -g -fbacktrace

mkdir mod
mkdir lib

pushd src
pushd modules


gfortran %COMPILE_FLAGS% -J..\..\mod -c^
 Globparm.f90^
 States.f90^
 Userparm.f90^
 Global.f90^
 Maindata.f90^
 Addvar.f90^
 Iodata.f90^
 Free_mem.f90^
 Locvar.f90^
 Mathrand.f90^
 Mr.f90^
 Utils.f90^
 Read_hh.f90^
 Wrhead.f90^
 Write_hh.f90 2> compile.log || goto ERR 

ar cr ..\..\lib\libsuper_mod.a *.o || goto ERR

popd

gfortran %COMPILE_FLAGS% -J..\mod -c^
 Delvars.f90^
 GP.f90^
 Parmtab.f90^
 Print_hh.f90^
 Readhead.f90^
 Readparm.f90^
 Supervis.f90^
 Timing.f90  2> compile.log || goto ERR

ar cr ..\lib\libsuper.a *.o || (goto ERR)

popd

echo Compiling and linking executed successfully.

exit /B 0


:ERR

echo An error occured when compiling:  check compile.log for more information.


exit /B %errorlevel%