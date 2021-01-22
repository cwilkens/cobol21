       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULT02.
      *--------------------------------------------------
      * This program asks the user for a number for a 
      * multiplication table,
      * and then displays a table for that number times 
      * the values 1 through 12.
      * -------------------------------------------------
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  THE-NUMBER         PIC 99.
       01  THE-MULTIPLIER     PIC 999.
       01  THE-PRODUCT        PIC 9999.

       PROCEDURE DIVISION.
      * LEVEL 1 ROUTINES
       PROGRAM-BEGIN.
           PERFORM PROGRAM-INITIALIZATION.
           PERFORM GET-TABLE-NUMBER.
           PERFORM DISPLAY-THE-TABLE.

       PROGRAM-DONE.
           STOP RUN.
       
      * LEVEL 2 ROUTINES
       PROGRAM-INITIALIZATION.
           MOVE 0 TO THE-MULTIPLIER.

       GET-TABLE-NUMBER.
           DISPLAY "Which multiplication table (01-99)?".
           ACCEPT THE-NUMBER.

       DISPLAY-THE-TABLE.
           DISPLAY "The " THE-NUMBER "s table is:".
           PERFORM CALCULATE-AND-DISPLAY 12 TIMES.

      * LEVEL 3 ROUTINES.
       CALCULATE-AND-DISPLAY.
           ADD 1 TO THE-MULTIPLIER.
           COMPUTE THE-PRODUCT = THE-NUMBER * THE-MULTIPLIER.
           DISPLAY
               THE-NUMBER " * " THE-MULTIPLIER " = " THE-PRODUCT.
           