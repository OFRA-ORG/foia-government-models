@echo off


set COMPILE_FLAGS=-std=f2008 -Wall -Wcompare-reals -Wunused-parameter -Werror -Wpedantic -O3 -fcheck=all -g -fbacktrace

mkdir mod
mkdir lib

pushd src
pushd modules


call gfortran %COMPILE_FLAGS% -I..\..\..\SUPER\mod -J..\..\mod -c^
 Fssizes.f90^
 Fslocs.f90^
 Fswork.f90^
 Fscnts.f90^
 Fsparm.f90^
 Fsrefalg.f90^
 fsutils.f90 2> compile.log || (goto ERR)

ar cr ..\..\lib\libfstamp_mod.a *.o 2>> compile.log || (goto ERR)

popd

gfortran %COMPILE_FLAGS% -I..\..\SUPER\mod -J..\mod -c^
 Fscnts.f90^
 Fselgdbg.f90^
 Fselig.f90^
 Fslocs.f90^
 Fsparm.f90^
 Fsrefalg.f90^
 Fsrefdbg.f90^
 Fssetdbg.f90^
 Fsstats_gl.f90^
 Fsstats_summ.f90^
 Fstab_characteristics.f90^
 Fstab_deductions.f90^
 Fstab_gl.f90^
 fstab_gl_pers.f90^
 Fstab_gl_st.f90^
 Fstab_povrat_size.f90^
 Fstab_protect_ben.f90^
 Fstab_protect_gl.f90^
 fstab_protect_summ.f90^
 Fstab_summ.f90^
 Fstab_summ_st.f90^
 Fstab_welfare_status.f90^
 fstab1x.f90^
 Fstables.f90^
 Fstamp.f90^
 Fstbldbg.f90 2> compile.log || (goto ERR)
 
ar cr ..\lib\libfstamp.a *.o 2>> compile.log || (goto ERR)

popd

echo Compiling and linking executed successfully.

exit /B 0


:ERR

echo An error occured when compiling:  check compile.log for more information.

exit /B 1
