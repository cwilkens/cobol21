       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO04.

      * This program illustrates the use of a PERFORM

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       PROCEDURE DIVISION.
       
       PROGRAM-BEGIN.    
           DISPLAY "Today's message is:".
           PERFORM SAY-HELLO.
                   
       PROGRAM-DONE.
           STOP RUN.

       SAY-HELLO.
           DISPLAY "Hello world".