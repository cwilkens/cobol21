       IDENTIFICATION DIVISION.
       PROGRAM-ID. CTLMNT01.
      *------------------------------------
      * Change and Inquire only
      * for the bills system control
      * file.
      *------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
           COPY "slcontrl.cbl".

       DATA DIVISION.
       FILE SECTION.

           COPY "fdcontrl.cbl".
           
       WORKING-STORAGE SECTION.

       77  MENU-PICK                   PIC 9.
           88 MENU-PICK-IS-VALID       VALUES 0 THRU 2.
       
       77  THE-MODE                    PIC X(7).
       77  RECORD-FOUND                PIC X.
       77  WHICH-FIELD                 PIC 9.
       77  A-DUMMY                     PIC X.
       
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
           OPEN I-O CONTROL-FILE.
       
       CLOSING-PROCEDURE.
           CLOSE CONTROL-FILE.


       MAIN-PROCESS.
           PERFORM GET-MENU-PICK.
           PERFORM MAINTAIN-THE-FILE
               UNTIL MENU-PICK = 0.

      *--------------------------------
      * MENU
      *--------------------------------
       GET-MENU-PICK.
           PERFORM DISPLAY-THE-MENU.
           PERFORM ACCEPT-MENU-PICK.
           PERFORM RE-ACCEPT-MENU-PICK
               UNTIL MENU-PICK-IS-VALID.

       DISPLAY-THE-MENU.
           PERFORM CLEAR-SCREEN.
           DISPLAY "    PLEASE SELECT:".
           DISPLAY " ".
           DISPLAY "        1.  CHANGE  CONTROL INFORMATION".
           DISPLAY "        2.  DISPLAY CONTROL INFORMATION".
           DISPLAY " ".
           DISPLAY "        0.  EXIT".
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

       MAINTAIN-THE-FILE.
           PERFORM DO-THE-PICK.
           PERFORM GET-MENU-PICK.

       DO-THE-PICK.
           IF MENU-PICK = 1
               PERFORM CHANGE-MODE
           ELSE
           IF MENU-PICK = 2
               PERFORM INQUIRE-MODE.

      *--------------------------------
      * CHANGE
      *--------------------------------
       CHANGE-MODE.
           MOVE "CHANGE" TO THE-MODE.
           PERFORM GET-CONTROL-RECORD.
           IF RECORD-FOUND = "Y"
               PERFORM CHANGE-RECORDS.

       CHANGE-RECORDS.
           PERFORM GET-FIELD-TO-CHANGE.
           PERFORM CHANGE-ONE-FIELD.

           PERFORM GET-CONTROL-RECORD.

       GET-FIELD-TO-CHANGE.
           PERFORM DISPLAY-ALL-FIELDS.
           PERFORM ASK-WHICH-FIELD.

       ASK-WHICH-FIELD.
           MOVE 1 TO WHICH-FIELD.

       CHANGE-ONE-FIELD.
           PERFORM CHANGE-THIS-FIELD.

       CHANGE-THIS-FIELD.
           IF WHICH-FIELD = 1
               PERFORM ENTER-CONTROL-LAST-VOUCHER.
           
           PERFORM REWRITE-CONTROL-RECORD.

      *--------------------------------
      * INQUIRE
      *--------------------------------
       INQUIRE-MODE.
           MOVE "DISPLAY" TO THE-MODE.
           PERFORM GET-CONTROL-RECORD.
           IF RECORD-FOUND = "Y"
               PERFORM INQUIRE-RECORDS.

       INQUIRE-RECORDS.
           PERFORM DISPLAY-ALL-FIELDS.
           PERFORM PRESS-ENTER.

       PRESS-ENTER.
           DISPLAY " ".
           DISPLAY "PRESS ENTER TO CONTINUE".
           ACCEPT A-DUMMY.

      *--------------------------------
      * Routines for Change
      *--------------------------------
       ENTER-CONTROL-LAST-VOUCHER.
           PERFORM ACCEPT-CONTROL-LAST-VOUCHER.

       ACCEPT-CONTROL-LAST-VOUCHER.
           DISPLAY "ENTER LAST VOUCHER NUMBER".
           ACCEPT CONTROL-LAST-VOUCHER.

      *--------------------------------
      * Routines shared by Change and Inquire
      *--------------------------------
       INIT-CONTROL-RECORD.
           MOVE ZEROES TO CONTROL-RECORD.

       ENTER-CONTROL-KEY.
           MOVE 1 TO CONTROL-KEY.

       GET-CONTROL-RECORD.
           PERFORM INIT-CONTROL-RECORD.
           PERFORM ENTER-CONTROL-KEY.
           MOVE "N" TO RECORD-FOUND.
           PERFORM FIND-CONTROL-RECORD.

       FIND-CONTROL-RECORD.
           PERFORM READ-CONTROL-RECORD.
           IF RECORD-FOUND = "N"
               DISPLAY "RECORD NOT FOUND"
               DISPLAY "YOU MUST RUN CTLBLD01"
               DISPLAY "TO CREATE THIS FILE".

       DISPLAY-ALL-FIELDS.
           DISPLAY " ".
           PERFORM DISPLAY-CONTROL-LAST-VOUCHER.
           DISPLAY " ".

       DISPLAY-CONTROL-LAST-VOUCHER.
           DISPLAY "1. LAST VOUCHER NUMBER: " 
                       CONTROL-LAST-VOUCHER.

      *--------------------------------
      * File I-O Routines
      *--------------------------------
       READ-CONTROL-RECORD.
           MOVE "Y" TO RECORD-FOUND.
           READ CONTROL-FILE RECORD
               INVALID KEY
               MOVE "N" TO RECORD-FOUND.
       
       REWRITE-CONTROL-RECORD.
           REWRITE CONTROL-RECORD
               INVALID KEY
               DISPLAY "ERROR REWRITING CONTROL RECORD".
               