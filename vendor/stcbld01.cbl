       IDENTIFICATION DIVISION.
       PROGRAM-ID. STCBLD01.
      *------------------------------------
      * Create an Empty State Code File
      *------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
           COPY "slstate.cbl".

       DATA DIVISION.
       FILE SECTION.

           COPY "fdstate.cbl".
           
       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           OPEN OUTPUT STATE-FILE.
           CLOSE STATE-FILE.

       PROGRAM-DONE.
           STOP RUN.
           