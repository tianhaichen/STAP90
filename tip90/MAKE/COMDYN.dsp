# Microsoft Developer Studio Project File - Name="COMDYN" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=COMDYN - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "COMDYN.mak".
!MESSAGE 
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

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "COMDYN - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\EXE"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x804 /d "NDEBUG"
# ADD RSC /l 0x804 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 SMATHS.LIB SMATHD.LIB SF90MP.LIB kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "COMDYN - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\EXE"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /include:"Debug/" /nologo /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /debug:full /include:"Debug/" /nologo /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x804 /d "_DEBUG"
# ADD RSC /l 0x804 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 SMATHS.LIB SMATHD.LIB SF90MP.LIB kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "COMDYN - Win32 Release"
# Name "COMDYN - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=..\SRC\cksep.f90
# End Source File
# Begin Source File

SOURCE=..\SRC\CloseOutFiles.f90
DEP_F90_CLOSE=\
	".\Release\module_ioport.mod"\
	".\Release\module_parameter.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\SRC\InquireFile.f90
# End Source File
# Begin Source File

SOURCE=..\SRC\MacroNumber.f90
# End Source File
# Begin Source File

SOURCE=..\SRC\module_data.f90

!IF  "$(CFG)" == "COMDYN - Win32 Release"

!ELSEIF  "$(CFG)" == "COMDYN - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"module_data"

# End Source File
# Begin Source File

SOURCE=..\SRC\module_ioport.f90

!IF  "$(CFG)" == "COMDYN - Win32 Release"

!ELSEIF  "$(CFG)" == "COMDYN - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"module_ioport"

# End Source File
# Begin Source File

SOURCE=..\SRC\module_parameter.f90

!IF  "$(CFG)" == "COMDYN - Win32 Release"

!ELSEIF  "$(CFG)" == "COMDYN - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"module_parameter"

# End Source File
# Begin Source File

SOURCE=..\SRC\OpenOutFiles.f90
DEP_F90_OPENO=\
	".\Release\module_ioport.mod"\
	".\Release\module_parameter.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\SRC\pcomp.f90
# End Source File
# Begin Source File

SOURCE=..\SRC\PropForce.f90
DEP_F90_PROPF=\
	".\Release\module_parameter.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\SRC\pstrip.f90
# End Source File
# Begin Source File

SOURCE=..\SRC\ReadData.f90
DEP_F90_READD=\
	".\Release\module_data.mod"\
	".\Release\module_ioport.mod"\
	".\Release\module_parameter.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\SRC\ReadPara.f90
DEP_F90_READP=\
	".\Release\module_ioport.mod"\
	".\Release\module_parameter.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\SRC\TIM_Central.f90
DEP_F90_TIM_C=\
	".\Release\module_data.mod"\
	".\Release\module_parameter.mod"\
	{$(INCLUDE)}"linear_operators.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\SRC\TIM_GW3.F90
DEP_F90_TIM_G=\
	".\Release\module_data.mod"\
	".\Release\module_ioport.mod"\
	".\Release\module_parameter.mod"\
	{$(INCLUDE)}"linear_operators.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\SRC\TIM_GW4.F90
DEP_F90_TIM_GW=\
	".\Release\module_data.mod"\
	".\Release\module_ioport.mod"\
	".\Release\module_parameter.mod"\
	{$(INCLUDE)}"linear_operators.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\SRC\TIM_Houbolt.f90
DEP_F90_TIM_H=\
	".\Release\module_data.mod"\
	".\Release\module_ioport.mod"\
	".\Release\module_parameter.mod"\
	{$(INCLUDE)}"linear_operators.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\SRC\TIM_Newmark.f90
DEP_F90_TIM_N=\
	".\Release\module_data.mod"\
	".\Release\module_ioport.mod"\
	".\Release\module_parameter.mod"\
	{$(INCLUDE)}"linear_operators.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\SRC\TIM_PREC.F90
DEP_F90_TIM_P=\
	".\Release\module_data.mod"\
	".\Release\module_ioport.mod"\
	".\Release\module_parameter.mod"\
	{$(INCLUDE)}"linear_operators.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\SRC\TIM_Wilson.f90
DEP_F90_TIM_W=\
	".\Release\module_data.mod"\
	".\Release\module_ioport.mod"\
	".\Release\module_parameter.mod"\
	{$(INCLUDE)}"linear_operators.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\SRC\TimeIntegration.f90
DEP_F90_TIMEI=\
	".\Release\module_data.mod"\
	".\Release\module_ioport.mod"\
	".\Release\module_parameter.mod"\
	{$(INCLUDE)}"linear_operators.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\SRC\TIP90.f90
DEP_F90_TIP90=\
	".\Release\module_ioport.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\SRC\WriteToFile.f90
DEP_F90_WRITE=\
	".\Release\module_ioport.mod"\
	".\Release\module_parameter.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
