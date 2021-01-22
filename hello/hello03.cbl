       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO03.
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  THE-MESSAGE      PIC X(20).
       01  THE-NAME         PIC X(10).
       01  THE-NUMBER       PIC 99.

       PROCEDURE DIVISION.
       
       PROGRAM-BEGIN.
           
           DISPLAY "Enter someone's name.".

           ACCEPT THE-NAME.

           MOVE "Hello" TO THE-MESSAGE.

           MOVE 1 TO THE-NUMBER.

           DISPLAY "Message "
                   THE-NUMBER
                   ": "
                   THE-MESSAGE
                   THE-NAME.
           
           MOVE "Say Goodnight," TO THE-MESSAGE.

           MOVE 2 TO THE-NUMBER.

           DISPLAY "Message "
                   THE-NUMBER
                   ": "
                   THE-MESSAGE
                   THE-NAME.

                   
       PROGRAM-DONE.
           STOP RUN.