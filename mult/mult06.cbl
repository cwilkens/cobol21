       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULT06.
      *--------------------------------------------------
      * This program asks the user for a number for a 
      * multiplication table, and a table size
      * and then displays a table for that number times 
      * the values 1 through HOW-MANY.
      *
      * The display is paused after each 15 lines.
      * -------------------------------------------------
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  THE-NUMBER         PIC 99.
       01  THE-MULTIPLIER     PIC 999.
       01  THE-PRODUCT        PIC 9999.
       01  HOW-MANY           PIC 99.
       01  SCREEN-LINES       PIC 99.

       01  A-DUMMY            PIC X.

       PROCEDURE DIVISION.
      * LEVEL 1 ROUTINES
       PROGRAM-BEGIN.
           PERFORM PROGRAM-INITIALIZATION.
           PERFORM GET-TABLE-DATA.
           PERFORM DISPLAY-THE-TABLE.

       PROGRAM-DONE.
           STOP RUN.
       
      * LEVEL 2 ROUTINES
       PROGRAM-INITIALIZATION.
           MOVE 0 TO SCREEN-LINES.

       GET-TABLE-DATA.
           DISPLAY "Which multiplication table (01-99)?".
           ACCEPT THE-NUMBER.

           DISPLAY "How many entries would you like (01-99)?".
           ACCEPT HOW-MANY.

       DISPLAY-THE-TABLE.
           DISPLAY "The " THE-NUMBER "s table is:".
           PERFORM CALCULATE-AND-DISPLAY
               VARYING THE-MULTIPLIER
                FROM 1 BY 1
                UNTIL THE-MULTIPLIER > HOW-MANY.

      * LEVEL 3 ROUTINES.
       CALCULATE-AND-DISPLAY.
           IF SCREEN-LINES = 15
               DISPLAY "Press ENTER to continue . . ."
               ACCEPT A-DUMMY
               MOVE 0 TO SCREEN-LINES.

           COMPUTE THE-PRODUCT = THE-NUMBER * THE-MULTIPLIER.
           DISPLAY
               THE-NUMBER " * " THE-MULTIPLIER " = " THE-PRODUCT.
           
           ADD 1 TO SCREEN-LINES.
           