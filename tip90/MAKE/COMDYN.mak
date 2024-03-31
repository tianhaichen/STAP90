# Microsoft Developer Studio Generated NMAKE File, Based on COMDYN.dsp
!IF "$(CFG)" == ""
CFG=COMDYN - Win32 Debug
!MESSAGE No configuration specified. Defaulting to COMDYN - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "COMDYN - Win32 Release" && "$(CFG)" != "COMDYN - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "COMDYN.mak" CFG="COMDYN - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "COMDYN - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "COMDYN - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "COMDYN - Win32 Release"

OUTDIR=.\..\EXE
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\..\EXE
# End Custom Macros

ALL : "$(OUTDIR)\COMDYN.exe"


CLEAN :
	-@erase "$(INTDIR)\cksep.obj"
	-@erase "$(INTDIR)\CloseOutFiles.obj"
	-@erase "$(INTDIR)\InquireFile.obj"
	-@erase "$(INTDIR)\MacroNumber.obj"
	-@erase "$(INTDIR)\module_data.mod"
	-@erase "$(INTDIR)\module_data.obj"
	-@erase "$(INTDIR)\module_ioport.mod"
	-@erase "$(INTDIR)\module_ioport.obj"
	-@erase "$(INTDIR)\module_parameter.mod"
	-@erase "$(INTDIR)\module_parameter.obj"
	-@erase "$(INTDIR)\OpenOutFiles.obj"
	-@erase "$(INTDIR)\pcomp.obj"
	-@erase "$(INTDIR)\PropForce.obj"
	-@erase "$(INTDIR)\pstrip.obj"
	-@erase "$(INTDIR)\ReadData.obj"
	-@erase "$(INTDIR)\ReadPara.obj"
	-@erase "$(INTDIR)\TIM_Central.obj"
	-@erase "$(INTDIR)\TIM_GW3.OBJ"
	-@erase "$(INTDIR)\TIM_GW4.OBJ"
	-@erase "$(INTDIR)\TIM_Houbolt.obj"
	-@erase "$(INTDIR)\TIM_Newmark.obj"
	-@erase "$(INTDIR)\TIM_PREC.OBJ"
	-@erase "$(INTDIR)\TIM_Wilson.obj"
	-@erase "$(INTDIR)\TimeIntegration.obj"
	-@erase "$(INTDIR)\TIP90.obj"
	-@erase "$(INTDIR)\WriteToFile.obj"
	-@erase "$(OUTDIR)\COMDYN.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

F90_PROJ=/compile_only /include:"$(INTDIR)\\" /nologo /warn:nofileopt /module:"Release/" /object:"Release/" 
F90_OBJS=.\Release/
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\COMDYN.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\COMDYN.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=SMATHS.LIB SMATHD.LIB SF90MP.LIB kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\COMDYN.pdb" /machine:I386 /out:"$(OUTDIR)\COMDYN.exe" 
LINK32_OBJS= \
	"$(INTDIR)\cksep.obj" \
	"$(INTDIR)\CloseOutFiles.obj" \
	"$(INTDIR)\InquireFile.obj" \
	"$(INTDIR)\MacroNumber.obj" \
	"$(INTDIR)\module_data.obj" \
	"$(INTDIR)\module_ioport.obj" \
	"$(INTDIR)\module_parameter.obj" \
	"$(INTDIR)\OpenOutFiles.obj" \
	"$(INTDIR)\pcomp.obj" \
	"$(INTDIR)\PropForce.obj" \
	"$(INTDIR)\pstrip.obj" \
	"$(INTDIR)\ReadData.obj" \
	"$(INTDIR)\ReadPara.obj" \
	"$(INTDIR)\TIM_Central.obj" \
	"$(INTDIR)\TIM_GW3.OBJ" \
	"$(INTDIR)\TIM_GW4.OBJ" \
	"$(INTDIR)\TIM_Houbolt.obj" \
	"$(INTDIR)\TIM_Newmark.obj" \
	"$(INTDIR)\TIM_PREC.OBJ" \
	"$(INTDIR)\TIM_Wilson.obj" \
	"$(INTDIR)\TimeIntegration.obj" \
	"$(INTDIR)\TIP90.obj" \
	"$(INTDIR)\WriteToFile.obj"

"$(OUTDIR)\COMDYN.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "COMDYN - Win32 Debug"

OUTDIR=.\..\EXE
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\..\EXE
# End Custom Macros

ALL : "$(OUTDIR)\COMDYN.exe" ".\Debug\module_data.mod" ".\Debug\module_ioport.mod" ".\Debug\module_parameter.mod"


CLEAN :
	-@erase "$(INTDIR)\cksep.obj"
	-@erase "$(INTDIR)\CloseOutFiles.obj"
	-@erase "$(INTDIR)\InquireFile.obj"
	-@erase "$(INTDIR)\MacroNumber.obj"
	-@erase "$(INTDIR)\module_data.mod"
	-@erase "$(INTDIR)\module_data.obj"
	-@erase "$(INTDIR)\module_ioport.mod"
	-@erase "$(INTDIR)\module_ioport.obj"
	-@erase "$(INTDIR)\module_parameter.mod"
	-@erase "$(INTDIR)\module_parameter.obj"
	-@erase "$(INTDIR)\OpenOutFiles.obj"
	-@erase "$(INTDIR)\pcomp.obj"
	-@erase "$(INTDIR)\PropForce.obj"
	-@erase "$(INTDIR)\pstrip.obj"
	-@erase "$(INTDIR)\ReadData.obj"
	-@erase "$(INTDIR)\ReadPara.obj"
	-@erase "$(INTDIR)\TIM_Central.obj"
	-@erase "$(INTDIR)\TIM_GW3.OBJ"
	-@erase "$(INTDIR)\TIM_GW4.OBJ"
	-@erase "$(INTDIR)\TIM_Houbolt.obj"
	-@erase "$(INTDIR)\TIM_Newmark.obj"
	-@erase "$(INTDIR)\TIM_PREC.OBJ"
	-@erase "$(INTDIR)\TIM_Wilson.obj"
	-@erase "$(INTDIR)\TimeIntegration.obj"
	-@erase "$(INTDIR)\TIP90.obj"
	-@erase "$(INTDIR)\WriteToFile.obj"
	-@erase "$(OUTDIR)\COMDYN.exe"
	-@erase "$(OUTDIR)\COMDYN.ilk"
	-@erase "$(OUTDIR)\COMDYN.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

F90_PROJ=/check:bounds /compile_only /debug:full /include:"$(INTDIR)\\" /nologo /warn:argument_checking /warn:nofileopt /module:"Debug/" /object:"Debug/" /pdbfile:"..\EXE/DF60.PDB" 
F90_OBJS=.\Debug/
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\COMDYN.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\COMDYN.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=SMATHS.LIB SMATHD.LIB SF90MP.LIB kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\COMDYN.pdb" /debug /machine:I386 /out:"$(OUTDIR)\COMDYN.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\cksep.obj" \
	"$(INTDIR)\CloseOutFiles.obj" \
	"$(INTDIR)\InquireFile.obj" \
	"$(INTDIR)\MacroNumber.obj" \
	"$(INTDIR)\module_data.obj" \
	"$(INTDIR)\module_ioport.obj" \
	"$(INTDIR)\module_parameter.obj" \
	"$(INTDIR)\OpenOutFiles.obj" \
	"$(INTDIR)\pcomp.obj" \
	"$(INTDIR)\PropForce.obj" \
	"$(INTDIR)\pstrip.obj" \
	"$(INTDIR)\ReadData.obj" \
	"$(INTDIR)\ReadPara.obj" \
	"$(INTDIR)\TIM_Central.obj" \
	"$(INTDIR)\TIM_GW3.OBJ" \
	"$(INTDIR)\TIM_GW4.OBJ" \
	"$(INTDIR)\TIM_Houbolt.obj" \
	"$(INTDIR)\TIM_Newmark.obj" \
	"$(INTDIR)\TIM_PREC.OBJ" \
	"$(INTDIR)\TIM_Wilson.obj" \
	"$(INTDIR)\TimeIntegration.obj" \
	"$(INTDIR)\TIP90.obj" \
	"$(INTDIR)\WriteToFile.obj"

"$(OUTDIR)\COMDYN.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.SUFFIXES: .fpp

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.fpp{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("COMDYN.dep")
!INCLUDE "COMDYN.dep"
!ELSE 
!MESSAGE Warning: cannot find "COMDYN.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "COMDYN - Win32 Release" || "$(CFG)" == "COMDYN - Win32 Debug"
SOURCE=..\SRC\cksep.f90

"$(INTDIR)\cksep.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\SRC\CloseOutFiles.f90

"$(INTDIR)\CloseOutFiles.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\module_parameter.mod" "$(INTDIR)\module_ioport.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\SRC\InquireFile.f90

"$(INTDIR)\InquireFile.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\SRC\MacroNumber.f90

"$(INTDIR)\MacroNumber.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\SRC\module_data.f90

!IF  "$(CFG)" == "COMDYN - Win32 Release"

F90_MODOUT=\
	"module_data"


"$(INTDIR)\module_data.obj"	"$(INTDIR)\module_data.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "COMDYN - Win32 Debug"

F90_MODOUT=\
	"module_data"


"$(INTDIR)\module_data.obj"	"$(INTDIR)\module_data.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\SRC\module_ioport.f90

!IF  "$(CFG)" == "COMDYN - Win32 Release"

F90_MODOUT=\
	"module_ioport"


"$(INTDIR)\module_ioport.obj"	"$(INTDIR)\module_ioport.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "COMDYN - Win32 Debug"

F90_MODOUT=\
	"module_ioport"


"$(INTDIR)\module_ioport.obj"	"$(INTDIR)\module_ioport.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\SRC\module_parameter.f90

!IF  "$(CFG)" == "COMDYN - Win32 Release"

F90_MODOUT=\
	"module_parameter"


"$(INTDIR)\module_parameter.obj"	"$(INTDIR)\module_parameter.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "COMDYN - Win32 Debug"

F90_MODOUT=\
	"module_parameter"


"$(INTDIR)\module_parameter.obj"	"$(INTDIR)\module_parameter.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\SRC\OpenOutFiles.f90

"$(INTDIR)\OpenOutFiles.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\module_parameter.mod" "$(INTDIR)\module_ioport.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\SRC\pcomp.f90

"$(INTDIR)\pcomp.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\SRC\PropForce.f90

"$(INTDIR)\PropForce.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\module_parameter.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\SRC\pstrip.f90

"$(INTDIR)\pstrip.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\SRC\ReadData.f90

"$(INTDIR)\ReadData.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\module_parameter.mod" "$(INTDIR)\module_ioport.mod" "$(INTDIR)\module_data.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\SRC\ReadPara.f90

"$(INTDIR)\ReadPara.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\module_parameter.mod" "$(INTDIR)\module_ioport.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\SRC\TIM_Central.f90

"$(INTDIR)\TIM_Central.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\module_parameter.mod" "$(INTDIR)\module_data.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\SRC\TIM_GW3.F90

"$(INTDIR)\TIM_GW3.OBJ" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\module_parameter.mod" "$(INTDIR)\module_ioport.mod" "$(INTDIR)\module_data.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\SRC\TIM_GW4.F90

"$(INTDIR)\TIM_GW4.OBJ" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\module_parameter.mod" "$(INTDIR)\module_ioport.mod" "$(INTDIR)\module_data.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\SRC\TIM_Houbolt.f90

"$(INTDIR)\TIM_Houbolt.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\module_parameter.mod" "$(INTDIR)\module_ioport.mod" "$(INTDIR)\module_data.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\SRC\TIM_Newmark.f90

"$(INTDIR)\TIM_Newmark.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\module_parameter.mod" "$(INTDIR)\module_ioport.mod" "$(INTDIR)\module_data.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\SRC\TIM_PREC.F90

"$(INTDIR)\TIM_PREC.OBJ" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\module_parameter.mod" "$(INTDIR)\module_ioport.mod" "$(INTDIR)\module_data.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\SRC\TIM_Wilson.f90

"$(INTDIR)\TIM_Wilson.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\module_parameter.mod" "$(INTDIR)\module_ioport.mod" "$(INTDIR)\module_data.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\SRC\TimeIntegration.f90

"$(INTDIR)\TimeIntegration.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\module_parameter.mod" "$(INTDIR)\module_ioport.mod" "$(INTDIR)\module_data.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\SRC\TIP90.f90

"$(INTDIR)\TIP90.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\module_ioport.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\SRC\WriteToFile.f90

"$(INTDIR)\WriteToFile.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\module_parameter.mod" "$(INTDIR)\module_ioport.mod"
	$(F90) $(F90_PROJ) $(SOURCE)



!ENDIF 

