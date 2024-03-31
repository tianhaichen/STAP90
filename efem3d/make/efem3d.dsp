# Microsoft Developer Studio Project File - Name="efem3d" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=efem3d - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "efem3d.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "efem3d.mak" CFG="efem3d - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "efem3d - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "efem3d - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "efem3d - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
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
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386 /out:"../exe/efem3d.exe"

!ELSEIF  "$(CFG)" == "efem3d - Win32 Debug"

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
# ADD BASE F90 /check:bounds /compile_only /debug:full /include:"Debug/" /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /browser /check:bounds /compile_only /debug:full /include:"Debug/" /nologo /real_size:64 /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /GZ /c
# ADD BASE RSC /l 0x804 /d "_DEBUG"
# ADD RSC /l 0x804 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /out:"../exe/efem3d.exe" /pdbtype:sept
# SUBTRACT LINK32 /profile

!ENDIF 

# Begin Target

# Name "efem3d - Win32 Release"
# Name "efem3d - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=..\src\DataIn.f90
DEP_F90_DATAI=\
	".\Release\DataOut.mod"\
	".\Release\ElementData.mod"\
	".\Release\FFI.mod"\
	".\Release\MaterialData.mod"\
	".\Release\ParticleData.mod"\
	".\Release\Simulation.mod"\
	".\Release\TimeFunctionData.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src\DataOut.f90
DEP_F90_DATAO=\
	".\Release\ElementData.mod"\
	".\Release\FFI.mod"\
	".\Release\MaterialData.mod"\
	".\Release\ParticleData.mod"\
	".\Release\Simulation.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src\Element.f90
# End Source File
# Begin Source File

SOURCE=..\src\FEMCalc.f90
DEP_F90_FEMCA=\
	".\Release\ElementData.mod"\
	".\Release\MaterialData.mod"\
	".\Release\ParticleData.mod"\
	".\Release\Simulation.mod"\
	{$(INCLUDE)}"imsl.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src\FFI.F90
# End Source File
# Begin Source File

SOURCE=..\src\main.f90
DEP_F90_MAIN_=\
	".\Release\DataIn.mod"\
	".\Release\DataOut.mod"\
	".\Release\ElementData.mod"\
	".\Release\FFI.mod"\
	".\Release\ParticleData.mod"\
	".\Release\Simulation.mod"\
	".\Release\TimeFunctionData.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src\Material.f90
DEP_F90_MATER=\
	".\Release\Simulation.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src\Particle.f90
# End Source File
# Begin Source File

SOURCE=..\src\Simulation.f90
# End Source File
# Begin Source File

SOURCE=..\src\TimeFunction.f90
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
