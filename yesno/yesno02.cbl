       IDENTIFICATION DIVISION.
       PROGRAM-ID. YESNO02.
      *--------------------------------------------------
      * This program asks for a Y or N answer, and then
      * displays whether the user chose yes or no.
      * The edit logic allows for entry of Y, y, N, or n.
      *--------------------------------------------------
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  YES-OR-NO      PIC X.

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.

           PERFORM GET-THE-ANSWER.
           PERFORM EDIT-THE-ANSWER.
           PERFORM DISPLAY-THE-ANSWER.

       PROGRAM-DONE.
           STOP RUN.
       
       GET-THE-ANSWER.
           DISPLAY "Is the answer Yes or No? (Y/N)".
           ACCEPT YES-OR-NO.

       EDIT-THE-ANSWER.
           IF YES-OR-NO IS EQUAL "y"
               MOVE "Y" TO YES-OR-NO.
           IF YES-OR-NO IS EQUAL "n"
               MOVE "N" TO YES-OR-NO.

       DISPLAY-THE-ANSWER.
           IF YES-OR-NO IS EQUAL "Y"
               DISPLAY "You answered Yes.".
           
           IF YES-OR-NO IS EQUAL "N"
               DISPLAY "You answered No.".