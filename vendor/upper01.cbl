       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPPER01.
      *--------------------------------------------
      * Converts input to upper case.
      *--------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       77  UPPER-ALPHA           PIC X(26) VALUE
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
       77  LOWER-ALPHA           PIC X(26) VALUE
           "abcdefghijklmnopqrstuvwxyz".

       77 TEST-FIELD             PIC X(30) VALUE SPACE.
       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           PERFORM ENTER-TEST-FIELD.
           PERFORM CONVERT-AND-ENTER
               UNTIL TEST-FIELD = SPACES.

       PROGRAM-DONE.
           STOP RUN.

       ENTER-TEST-FIELD.
           DISPLAY "Enter upper or lower case data".
           DISPLAY "Leave blank to end".
           ACCEPT TEST-FIELD.
       CONVERT-AND-ENTER.
           PERFORM CONVERT-TEST-FIELD.
           PERFORM ENTER-TEST-FIELD.

       CONVERT-TEST-FIELD.
           INSPECT TEST-FIELD CONVERTING
               LOWER-ALPHA TO UPPER-ALPHA.
           DISPLAY TEST-FIELD.