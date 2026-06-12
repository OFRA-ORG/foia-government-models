@echo off


set COMPILE_FLAGS=-std=f2008 -Wall -Wextra -Werror -Wpedantic -O3 -fcheck=all -g -fbacktrace

call gfortran %COMPILE_FLAGS% -I..\..\..\Common\SUPER\mod  ^
 dummy.f90^
 tally.f90^
 "..\..\..\common\display\src\Mathcon.f90"^
 -L ..\..\..\common\super\lib^
 -l super^
 -l super_mod^
 -o mathpc.exe  2> complink.log || (goto ERR)
 
echo Compiling and linking executed successfully.

exit /B 0


:ERR

echo An error occured when compiling:  check complink.log for more information.

exit /B 1
