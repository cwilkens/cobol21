       IDENTIFICATION DIVISION.
       PROGRAM-ID. VNDMNT01.
      *------------------------------------
      * Add, Change, Inquire and Delete
      * for the Vendor File.
      *------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
           COPY "slvnd01.cbl".

       DATA DIVISION.
       FILE SECTION.

           COPY "fdvnd02.cbl".
           
       WORKING-STORAGE SECTION.

       77  MENU-PICK                   PIC 9.
           88 MENU-PICK-IS-VALID       VALUES 0 THRU 4.
       
       77  THE-MODE                    PIC X(7).
       77  WHICH-FIELD                 PIC 9.
       77  OK-TO-DELETE                PIC X.
       77  RECORD-FOUND                PIC X.
       77  VENDOR-NUMBER-FIELD         PIC Z(5).

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           PERFORM OPENING-PROCEDURE.
           PERFORM MAIN-PROCESS.
           PERFORM CLOSING-PROCEDURE.
           
       PROGRAM-DONE.
           STOP RUN.

       OPENING-PROCEDURE.
           OPEN I-O VENDOR-FILE.
       
       CLOSING-PROCEDURE.
           CLOSE VENDOR-FILE.


       MAIN-PROCESS.
           PERFORM GET-MENU-PICK.
           PERFORM MAINTAIN-THE-FILE
               UNTIL MENU-PICK = 0.

      *--------------------------------
      * MENU
      *--------------------------------
       GET-MENU-PICK.
           PERFORM DISPLAY-THE-MENU.
           PERFORM GET-THE-PICK.
           PERFORM MENU-RETRY
               UNTIL MENU-PICK-IS-VALID.

       DISPLAY-THE-MENU.
           PERFORM CLEAR-SCREEN.
           DISPLAY "    PLEASE SELECT:".
           DISPLAY " ".
           DISPLAY "          1.  ADD RECORDS".
           DISPLAY "          2.  CHANGE A RECORD".
           DISPLAY "          3.  LOOK UP A RECORD".
           DISPLAY "          4.  DELETE A RECORD".
           DISPLAY " ".
           DISPLAY "          0.  EXIT".
           PERFORM SCROLL-LINE 8 TIMES.

       GET-THE-PICK.
           DISPLAY "YOUR CHOICE (0-4)?".
           ACCEPT MENU-PICK.
       MENU-RETRY.
           DISPLAY "INVALID SELECTION - PLEASE RE-TRY.".
           PERFORM GET-THE-PICK.
       CLEAR-SCREEN.
           PERFORM SCROLL-LINE 25 TIMES.

       SCROLL-LINE.
           DISPLAY " ".

       MAINTAIN-THE-FILE.
           PERFORM DO-THE-PICK.
           PERFORM GET-MENU-PICK.

       DO-THE-PICK.
           IF MENU-PICK = 1
               PERFORM ADD-MODE
           ELSE
           IF MENU-PICK = 2
               PERFORM CHANGE-MODE
           ELSE
           IF MENU-PICK = 3
               PERFORM INQUIRE-MODE
           ELSE
           IF MENU-PICK = 4
               PERFORM DELETE-MODE.

      *--------------------------------
      * ADD
      *--------------------------------
       ADD-MODE.
           MOVE "ADD" TO THE-MODE.
           PERFORM GET-NEW-VENDOR-NUMBER.
           PERFORM ADD-RECORDS
               UNTIL VENDOR-NUMBER = ZEROES.
       
       GET-NEW-VENDOR-NUMBER.
           PERFORM INIT-VENDOR-RECORD.
           PERFORM ENTER-VENDOR-NUMBER.
           MOVE "Y" TO RECORD-FOUND.
           PERFORM FIND-NEW-VENDOR-RECORD
               UNTIL RECORD-FOUND = "N" OR
                     VENDOR-NUMBER = ZEROES.
       
       FIND-NEW-VENDOR-RECORD.
           PERFORM READ-VENDOR-RECORD.
           IF RECORD-FOUND = "Y"
               DISPLAY "RECORD ALREADY ON FILE".
               PERFORM ENTER-VENDOR-NUMBER.
       
       ADD-RECORDS.
           PERFORM ENTER-REMAINING-FIELDS.
           PERFORM WRITE-VENDOR-RECORD.
           PERFORM GET-NEW-VENDOR-NUMBER.

       ENTER-REMAINING-FIELDS.
           PERFORM ENTER-VENDOR-NAME.
           PERFORM ENTER-VENDOR-ADDRESS-1.
           PERFORM ENTER-VENDOR-ADDRESS-2.
           PERFORM ENTER-VENDOR-CITY.
           PERFORM ENTER-VENDOR-STATE.
           PERFORM ENTER-VENDOR-ZIP.
           PERFORM ENTER-VENDOR-CONTACT.
           PERFORM ENTER-VENDOR-PHONE.
       
      *--------------------------------
      * CHANGE
      *--------------------------------
       CHANGE-MODE.
           MOVE "CHANGE" TO THE-MODE.
           PERFORM GET-VENDOR-RECORD.
           PERFORM CHANGE-RECORDS
               UNTIL VENDOR-NUMBER = ZEROES.

       CHANGE-RECORDS.
           PERFORM GET-FIELD-TO-CHANGE.
           PERFORM CHANGE-ONE-FIELD
               UNTIL WHICH-FIELD = ZERO.
           PERFORM GET-VENDOR-RECORD.

       GET-FIELD-TO-CHANGE.
           PERFORM DISPLAY-ALL-FIELDS.
           PERFORM ASK-WHICH-FIELD.

       ASK-WHICH-FIELD.
           DISPLAY "ENTER THE NUMBER OF THE FIELD".
           DISPLAY "TO CHANGE (1-8) OR 0 TO EXIT".
           ACCEPT WHICH-FIELD.
           IF WHICH-FIELD > 8
               DISPLAY "INVALID ENTRY".

       CHANGE-ONE-FIELD.
           PERFORM CHANGE-THIS-FIELD.
           PERFORM GET-FIELD-TO-CHANGE.

       CHANGE-THIS-FIELD.
           IF WHICH-FIELD = 1
               PERFORM ENTER-VENDOR-NAME.
           IF WHICH-FIELD = 2
               PERFORM ENTER-VENDOR-ADDRESS-1.
           IF WHICH-FIELD = 3
               PERFORM ENTER-VENDOR-ADDRESS-2.
           IF WHICH-FIELD = 4
               PERFORM ENTER-VENDOR-CITY.
           IF WHICH-FIELD = 5
               PERFORM ENTER-VENDOR-STATE.
           IF WHICH-FIELD = 6
               PERFORM ENTER-VENDOR-ZIP.
           IF WHICH-FIELD = 7
               PERFORM ENTER-VENDOR-CONTACT.
           IF WHICH-FIELD = 8
               PERFORM ENTER-VENDOR-PHONE.
           
           PERFORM REWRITE-VENDOR-RECORD.

      *--------------------------------
      * INQUIRE
      *--------------------------------
       INQUIRE-MODE.
           MOVE "DISPLAY" TO THE-MODE.
           PERFORM GET-VENDOR-RECORD.
           PERFORM INQUIRE-RECORDS
               UNTIL VENDOR-NUMBER = ZEROES.

       INQUIRE-RECORDS.
           PERFORM DISPLAY-ALL-FIELDS.
           PERFORM GET-VENDOR-RECORD.

      *--------------------------------
      * DELETE
      *--------------------------------
       DELETE-MODE.
           MOVE "DELETE" TO THE-MODE.
           PERFORM GET-VENDOR-RECORD.
           PERFORM DELETE-RECORDS
               UNTIL VENDOR-NUMBER = ZEROES.

       DELETE-RECORDS.
           PERFORM DISPLAY-ALL-FIELDS.
           MOVE "X" TO OK-TO-DELETE.

           PERFORM ASK-TO-DELETE
              UNTIL OK-TO-DELETE = "Y" OR "N".
           
           IF OK-TO-DELETE = "Y"
               PERFORM DELETE-VENDOR-RECORD.

           PERFORM GET-VENDOR-RECORD.

       ASK-TO-DELETE.
           DISPLAY "DELETE THIS RECORD (Y/N)?".
           ACCEPT OK-TO-DELETE.
           IF OK-TO-DELETE = "y"
               MOVE "Y" TO OK-TO-DELETE.
           IF OK-TO-DELETE = "n"
               MOVE "N" TO OK-TO-DELETE.
           IF OK-TO-DELETE NOT = "Y" AND
              OK-TO-DELETE NOT = "N"
               DISPLAY "YOU MUST ENTER YES OR NO".
      
      *--------------------------------
      * Rountines shared by all modes
      *--------------------------------
       INIT-VENDOR-RECORD.
           MOVE SPACE TO VENDOR-RECORD.
           MOVE ZEROES TO VENDOR-NUMBER.

       ENTER-VENDOR-NUMBER.
           DISPLAY " ".
           DISPLAY "ENTER VENDOR NUMBER OF THE VENDOR".
           DISPLAY "TO " THE-MODE " (1-99999)".
           DISPLAY "ENTER 0 TO STOP ENTRY".
           ACCEPT VENDOR-NUMBER-FIELD.
           MOVE VENDOR-NUMBER-FIELD TO VENDOR-NUMBER.

       GET-VENDOR-RECORD.
           PERFORM INIT-VENDOR-RECORD.
           PERFORM ENTER-VENDOR-NUMBER.
           MOVE "N" TO RECORD-FOUND.
           PERFORM FIND-VENDOR-RECORD
               UNTIL RECORD-FOUND = "Y" OR
                     VENDOR-NUMBER = ZEROES.

      *------------------------------------
      * Routines shared by Add and Change
      *------------------------------------
       FIND-VENDOR-RECORD.
           PERFORM READ-VENDOR-RECORD.
           IF RECORD-FOUND = "N"
               DISPLAY "RECORD NOT FOUND"
               PERFORM ENTER-VENDOR-NUMBER.
       
       ENTER-VENDOR-NAME.
           DISPLAY "ENTER VENDOR-NAME".
           ACCEPT VENDOR-NAME.

       ENTER-VENDOR-ADDRESS-1.
           DISPLAY "ENTER VENDOR-ADDRESS-1".
           ACCEPT VENDOR-ADDRESS-1.

       ENTER-VENDOR-ADDRESS-2.
           DISPLAY "ENTER VENDOR-ADDRESS-2".
           ACCEPT VENDOR-ADDRESS-2.

       ENTER-VENDOR-CITY.
           DISPLAY "ENTER VENDOR-CITY".
           ACCEPT VENDOR-CITY.

       ENTER-VENDOR-STATE.
           DISPLAY "ENTER VENDOR-STATE".
           ACCEPT VENDOR-STATE.

       ENTER-VENDOR-ZIP.
           DISPLAY "ENTER VENDOR-ZIP".
           ACCEPT VENDOR-ZIP.

       ENTER-VENDOR-CONTACT.
           DISPLAY "ENTER VENDOR-CONTACT".
           ACCEPT VENDOR-CONTACT.

       ENTER-VENDOR-PHONE.
           DISPLAY "ENTER VENDOR-PHONE".
           ACCEPT VENDOR-PHONE.

      *------------------------------------
      * Routines shared by Change,
      * Inquire, and Delete
      *------------------------------------
       DISPLAY-ALL-FIELDS.
           DISPLAY " ".
           PERFORM DISPLAY-VENDOR-NUMBER.
           PERFORM DISPLAY-VENDOR-NAME.
           PERFORM DISPLAY-VENDOR-ADDRESS-1.
           PERFORM DISPLAY-VENDOR-ADDRESS-2.
           PERFORM DISPLAY-VENDOR-CITY.
           PERFORM DISPLAY-VENDOR-STATE.
           PERFORM DISPLAY-VENDOR-ZIP.
           PERFORM DISPLAY-VENDOR-CONTACT.
           PERFORM DISPLAY-VENDOR-PHONE.
           DISPLAY " ".

       DISPLAY-VENDOR-NUMBER.
           DISPLAY "   VENDOR NUMBER: " VENDOR-NUMBER.

       DISPLAY-VENDOR-NAME.
           DISPLAY "1. VENDOR NAME: " VENDOR-NAME.

       DISPLAY-VENDOR-ADDRESS-1.
           DISPLAY "2. VENDOR ADDRESS-1: " VENDOR-ADDRESS-1.

       DISPLAY-VENDOR-ADDRESS-2.
           DISPLAY "3. VENDOR ADDRESS-2: " VENDOR-ADDRESS-2.

       DISPLAY-VENDOR-CITY.
           DISPLAY "4. VENDOR CITY: " VENDOR-CITY.

       DISPLAY-VENDOR-STATE.
           DISPLAY "5. VENDOR STATE: " VENDOR-STATE.

       DISPLAY-VENDOR-ZIP.
           DISPLAY "6. VENDOR ZIP: " VENDOR-ZIP.

       DISPLAY-VENDOR-CONTACT.
           DISPLAY "7. VENDOR CONTACT: " VENDOR-CONTACT.

       DISPLAY-VENDOR-PHONE.
           DISPLAY "8. VENDOR PHONE: " VENDOR-PHONE.

      *--------------------------------
      * File I-O Routines
      *--------------------------------
       READ-VENDOR-RECORD.
           MOVE "Y" TO RECORD-FOUND.
           READ VENDOR-FILE RECORD
               INVALID KEY
               MOVE "N" TO RECORD-FOUND.
       
       WRITE-VENDOR-RECORD.
           WRITE VENDOR-RECORD
               INVALID KEY
               DISPLAY "RECORD ALREADY ON FILE".

       REWRITE-VENDOR-RECORD.
           REWRITE VENDOR-RECORD
               INVALID KEY
               DISPLAY "ERROR REWRITING VENDOR RECORD".

       DELETE-VENDOR-RECORD.
           DELETE VENDOR-FILE RECORD
               INVALID KEY
               DISPLAY "ERROR DELETING VENDOR RECORD".
               