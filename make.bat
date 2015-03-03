@rem This batch file builds zymake using the Microsoft Visual C port of 
@rem Objective Caml
@rem 
@rem Adjust the VCINSTALLDIR and OCAMLPATH variables below if your installations
@rem are not in the default locations.
@
@SETLOCAL
@
@rem set path and other environment variables for Visual Studio
@SET VCINSTALLDIR=C:\Program Files\Microsoft Visual Studio 8
@call "%VCINSTALLDIR%\Common7\Tools\vsvars32.bat"
@
@rem set path for ocaml
@SET OCAMLPATH=C:\Program Files\Objective Caml\bin
@SET PATH=%OCAMLPATH%;%PATH%
@
nmake /f Makefile RM=del %1
