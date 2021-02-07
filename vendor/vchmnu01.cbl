       IDENTIFICATION DIVISION.
       PROGRAM-ID. VCHMNU01.
      *--------------------------------------------
      * Menu for Voucher Processing
      *--------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       77  MENU-PICK                     PIC 9.
           88  MENU-PICK-IS-VALID        VALUES 0 THRU 7.

       77  A-DUMMY        PIC X.

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
           DISPLAY "          1. VOUCHER ENTRY".
           DISPLAY "          2. BILLS REPORT".
           DISPLAY "          3. SELECT VOUCHER BY DUE DATE RANGE".
           DISPLAY "          4. SELECT INDIVIDUAL VOUCHERS".
           DISPLAY "          5. CLEAR PREVIOUS SELECTIONS".
           DISPLAY "          6. CASH REQUIREMENTS REPORT".
           DISPLAY "          7. PAID BILLS ENTRY".
           DISPLAY " ".
           DISPLAY "          0. EXIT".
           PERFORM SCROLL-LINE 8 TIMES.

       ACCEPT-MENU-PICK.
           DISPLAY "YOUR CHOICE (0-7)?".
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
               PERFORM VOUCHER-ENTRY
           ELSE
           IF MENU-PICK = 2
               PERFORM BILLS-REPORT
           ELSE
           IF MENU-PICK = 3
               PERFORM DATE-SELECTION
           ELSE
           IF MENU-PICK = 4
               PERFORM SINGLE-SELECTION
           ELSE
           IF MENU-PICK = 5
               PERFORM CLEAR-SELECTIONS
           ELSE
           IF MENU-PICK = 6
               PERFORM CASH-REQUIREMENTS
           ELSE
           IF MENU-PICK = 7
               PERFORM PAID-ENTRY.

           PERFORM GET-MENU-PICK.

       VOUCHER-ENTRY.
           CALL "vchmnt01".

       BILLS-REPORT.
           CALL "bilrpt03".
           DISPLAY "PRESS ENTER TO CONTINUE. . .".
           ACCEPT A-DUMMY.

       DATE-SELECTION.
           CALL "vchsel01".

       SINGLE-SELECTION.
           CALL "vchpic01".

       CLEAR-SELECTIONS.
           CALL "vchclr01".

       CASH-REQUIREMENTS.
           CALL "cshreq01".
           DISPLAY "PRESS ENTER TO CONTINUE. . .".
           ACCEPT A-DUMMY.

       PAID-ENTRY.
           CALL "vchpay02".
