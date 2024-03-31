# Microsoft Developer Studio Project File - Name="MBSim" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=MBSim - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "MBSim.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "MBSim.mak" CFG="MBSim - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "MBSim - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "MBSim - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "MBSim - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x804 /d "NDEBUG"
# ADD RSC /l 0x804 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "MBSim - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /GZ /c
# ADD BASE RSC /l 0x804 /d "_DEBUG"
# ADD RSC /l 0x804 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "MBSim - Win32 Release"
# Name "MBSim - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\BODY.F90
# End Source File
# Begin Source File

SOURCE=.\DATA_MODULE.F90
# End Source File
# Begin Source File

SOURCE=.\HINGE.F90
DEP_F90_HINGE=\
	".\Debug\module_body.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\HINGE_REVOLUTE.F90
DEP_F90_HINGE_=\
	".\Debug\module_body.mod"\
	".\Debug\module_hinge.mod"\
	".\Debug\module_system.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IO.F90
DEP_F90_IO_F9=\
	".\Debug\module_file.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\MAIN.F90
DEP_F90_MAIN_=\
	".\Debug\module_file.mod"\
	".\Debug\module_sol_af.mod"\
	".\Debug\module_system.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\MARK.F90
DEP_F90_MARK_=\
	".\Debug\module_body.mod"\
	".\Debug\module_system.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\RIGID.F90
DEP_F90_RIGID=\
	".\Debug\module_body.mod"\
	".\Debug\module_system.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SOL_AF.F90
DEP_F90_SOL_A=\
	".\Debug\module_file.mod"\
	".\Debug\module_system.mod"\
	{$(INCLUDE)}"imsl.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SYSTEM.F90
DEP_F90_SYSTE=\
	".\Debug\module_body.mod"\
	".\Debug\module_hinge.mod"\
	".\Debug\module_mark.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\USER.F90
DEP_F90_USER_=\
	".\Debug\module_mark.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\UTILITY.F90
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
