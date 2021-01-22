       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULT03.
      *--------------------------------------------------
      * This program asks the user for a number for a 
      * multiplication table, and a table size
      * and then displays a table for that number times 
      * the values 1 through HOW-MANY.
      * -------------------------------------------------
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  THE-NUMBER         PIC 99.
       01  THE-MULTIPLIER     PIC 999.
       01  THE-PRODUCT        PIC 9999.
       01  HOW-MANY           PIC 99.

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
           MOVE 0 TO THE-MULTIPLIER.

       GET-TABLE-DATA.
           DISPLAY "Which multiplication table (01-99)?".
           ACCEPT THE-NUMBER.

           DISPLAY "How many entries would you like (01-99)?".
           ACCEPT HOW-MANY.

       DISPLAY-THE-TABLE.
           DISPLAY "The " THE-NUMBER "s table is:".
           PERFORM CALCULATE-AND-DISPLAY HOW-MANY TIMES.

      * LEVEL 3 ROUTINES.
       CALCULATE-AND-DISPLAY.
           ADD 1 TO THE-MULTIPLIER.
           COMPUTE THE-PRODUCT = THE-NUMBER * THE-MULTIPLIER.
           DISPLAY
               THE-NUMBER " * " THE-MULTIPLIER " = " THE-PRODUCT.
           