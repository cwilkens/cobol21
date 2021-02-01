       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILMNU01.
      *--------------------------------------------
      * Menu for the bill payment system.
      *--------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       77  MENU-PICK                     PIC 9.
           88  MENU-PICK-IS-VALID        VALUES 0 THRU 2.

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
           PERFORM GET-MENU-PICK.
           PERFORM DO-THE-PICK
               UNTIL MENU-PICK = 0.

      *---------------------------------
      * MENU
      *---------------------------------
       GET-MENU-PICK.
           PERFORM DISPLAY-THE-MENU.
           PERFORM ACCEPT-MENU-PICK.
           PERFORM RE-ACCEPT-MENU-PICK
               UNTIL MENU-PICK-IS-VALID.

       DISPLAY-THE-MENU.
           PERFORM CLEAR-SCREEN.
           DISPLAY "    PLEASE SELECT:".
           DISPLAY " ".
           DISPLAY "          1. STATE CODE MAINTENANCE".
           DISPLAY "          2. VENDOR CODE MAINTENANCE".
           DISPLAY " ".
           DISPLAY "          0. EXIT".
           PERFORM SCROLL-LINE 8 TIMES.

       ACCEPT-MENU-PICK.
           DISPLAY "YOUR CHOICE (0-2)?".
           ACCEPT MENU-PICK.

       RE-ACCEPT-MENU-PICK.
           DISPLAY "INVALID SELECTION - PLEASE RE-TRY.".
           PERFORM ACCEPT-MENU-PICK.

       CLEAR-SCREEN.
           PERFORM SCROLL-LINE 25 TIMES.

       SCROLL-LINE.
           DISPLAY " ".
       
       DO-THE-PICK.
           IF MENU-PICK = 1
               PERFORM STATE-MAINTENANCE
           ELSE
           IF MENU-PICK = 2
               PERFORM VENDOR-MAINTENANCE.

           PERFORM GET-MENU-PICK.

      *---------------------------------
      * STATE
      *---------------------------------
       STATE-MAINTENANCE.
           CALL "stcmnt04".

      *---------------------------------
      * VENDOR
      *---------------------------------
       VENDOR-MAINTENANCE.
           CALL "vndmnt04".
      