       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATE01.
      *--------------------------------------------
      * Demo of Date Entry
      *--------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       77  DATE-ENTRY-FIELD   PIC Z9/99/9999.
       77  DATE-MMDDYYYY      PIC 9(8).
       77  DATE-YYYYMMDD      PIC 9(8).

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           PERFORM OPENING-PROCEDURE.
           PERFORM MAIN-PROCESS.
           PERFORM CLOSING-PROCEDURE.

       PROGRAM-EXIT.
           EXIT PROGRAM.

       PROGRAM-DONE.
           STOP RUN.
           
       OPENING-PROCEDURE.

       CLOSING-PROCEDURE.

       MAIN-PROCESS.
           PERFORM GET-A-DATE.
           PERFORM DISPLAY-AND-GET-DATE
               UNTIL DATE-MMDDYYYY = ZEROES.

       GET-A-DATE.
           DISPLAY "ENTER A DATE (MM/DD/YYYY)".
           ACCEPT DATE-ENTRY-FIELD.
           MOVE DATE-ENTRY-FIELD TO DATE-MMDDYYYY.
           COMPUTE DATE-YYYYMMDD =
                   DATE-MMDDYYYY * 10000.0001.

       DISPLAY-AND-GET-DATE.
           PERFORM DISPLAY-A-DATE.
           PERFORM GET-A-DATE.

       DISPLAY-A-DATE.
           MOVE DATE-MMDDYYYY TO DATE-ENTRY-FIELD.
           DISPLAY "FORMATTED DATE IS " DATE-ENTRY-FIELD.
           DISPLAY "DATE-MMDDYYYY IS " DATE-MMDDYYYY.
           DISPLAY "DATE-YYYYMMDD IS " DATE-YYYYMMDD.
           