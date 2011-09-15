- Open the file openaxiom.nsi and edit
  the variable BUILD_VERSION "X.Y.Z"
  with the version you are packaging. Also
  you can specify the Architecture by setting
  the ARCH variable.
  
- Copy the "OpenAxiom" install directory
  next to the openaxiom.nsi file.

- Place inside the "bin" directory any extra 
  libraries that need to be included in the
  OpenAxiom path.

  Mingw:
  
  * libgcc_s_dw2-1.dll
  * mingwm10.dll
  * libstdc++-6.dll
 
  QT:
  
  * QtCore4.dll
  * QtGui4.dll
  
  Command Line:
  
  * cat.exe
  * rm.exe
  
- Compile the openaxiom.nsi file using NSIS.

- After compilation, you will have an installer
  file open-axiom-X.Y.Z-windows-i386.exe