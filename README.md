This is a repository of select code written from the book "Teach Yourself COBOL in 21 Days", Third Edition (published 1999).

This code was compiled and tested using the free Micro Focus "Visual COBOL for Visual Studio Personal Edition" product, which includes a command line compiler. The code was written in VS Code (with a COBOL extension) and compiled and ran from the command line.

To compile and link, launch the "Visual COBOL Command Prompt" after setting up Visual COBOL.

To compile:

`cobol add01.cbl`

To compile and suppress additional input:

`cobol add01.cbl;`

To link:

`cbllink add01.obj`

To compile and link in one step with no additional input:

`cbllink add01.cbl`

These programs can be run from the Visual COBOL Command Prompt (ideally). To run from a different command prompt, you'll need to copy `cblrtsm.dll` from Visual COBOL's install directory. Some of these programs display output with no pause at the end, so launching them from a file explorer will open and immediately close a command prompt window.

The programs that use printing will fail with a runtime error (unless you happen to be running them on MS-DOS, or a very old version of Windows). Some of these programs have been edited to display to the screen instead of printing.

To compile a program that calls another program - `stcmnt04` as the main program, and `stcrpt02` as the called program:

`cbllink -mSTCMNT04 stcmnt04.cbl stcrpt02.cbl`