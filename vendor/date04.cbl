       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATE04.
      *--------------------------------------------
      * Demo of Date Entry and validation
      *--------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

           COPY "wsdate.cbl".

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

           COPY "pldate.cbl".
           
       DISPLAY-AND-GET-DATE.
           PERFORM DISPLAY-A-DATE.
           PERFORM GET-A-DATE.

       DISPLAY-A-DATE.
           MOVE DATE-MMDDYYYY TO DATE-ENTRY-FIELD.
           DISPLAY "FORMATTED DATE IS " DATE-ENTRY-FIELD.
           DISPLAY "DATE-MMDDYYYY IS " DATE-MMDDYYYY.
           DISPLAY "DATE-YYYYMMDD IS " DATE-YYYYMMDD.
           