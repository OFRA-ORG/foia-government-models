@echo off


set COMPILE_FLAGS=-std=f2008 -Wall -Wextra -Werror  -Wpedantic -O3 -fcheck=all -g -fbacktrace

mkdir mod
mkdir lib

pushd src
pushd modules


call gfortran %COMPILE_FLAGS% -I..\..\..\..\Common\SUPER\mod -I..\..\..\..\Common\fstamp\mod  -J..\..\mod -c^
 dblocs.f90^
 dbdefine.f90^
 dbparm.f90^
 dbwork.f90 2> compile.log || (goto ERR)

ar cr ..\..\lib\libqcfstamp_mod.a *.o 2>> compile.log || (goto ERR)

popd

gfortran %COMPILE_FLAGS% -I..\..\..\Common\SUPER\mod -I..\..\..\Common\fstamp\mod -J..\mod -c^
 DBPART.f90^
 DBVARDBG.f90^
 DBLOCS.f90^
 DBVARS.f90^
 DBPARM.f90^
 DBDEFINE.f90^
 DBFSU.f90^
 DBCNTS.f90^
 DBASSET.f90^
 DBGENCODE.f90 2> compile.log || (goto ERR)
 
ar cr ..\lib\libqcfstamp.a *.o 2>> compile.log || (goto ERR)

popd

echo Compiling and linking executed successfully.

exit /B 0


:ERR

echo An error occured when compiling:  check compile.log for more information.

exit /B 1
