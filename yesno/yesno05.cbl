       IDENTIFICATION DIVISION.
       PROGRAM-ID. YESNO05.
      *--------------------------------------------------
      * This program asks for a Y or N answer, and then
      * displays whether the user chose yes or no
      * or an invalid entry.
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
           IF YES-OR-NO = "y"
               MOVE "Y" TO YES-OR-NO.
           IF YES-OR-NO = "n"
               MOVE "N" TO YES-OR-NO.

       DISPLAY-THE-ANSWER.
           IF YES-OR-NO = "Y" OR
              YES-OR-NO = "N"
               PERFORM DISPLAY-YES-OR-NO
           ELSE
               DISPLAY "Your entry was invalid.".
           
       DISPLAY-YES-OR-NO.
           IF YES-OR-NO = "Y"
               DISPLAY "You answered Yes."
           ELSE
               DISPLAY "You answered No.".
       