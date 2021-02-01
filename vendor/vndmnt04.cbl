       IDENTIFICATION DIVISION.
       PROGRAM-ID. VNDMNT04.
      *------------------------------------
      * Add, Change, Inquire and Delete
      * for the Vendor File.
      * Menu includes inquire by name,
      * the vendor report, and the vendor
      * report in name order.
      *------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
           COPY "slvnd02.cbl".

           COPY "slstate.cbl".

       DATA DIVISION.
       FILE SECTION.

           COPY "fdvnd04.cbl".

           COPY "fdstate.cbl".
           
       WORKING-STORAGE SECTION.

       77  MENU-PICK                   PIC 9.
           88 MENU-PICK-IS-VALID       VALUES 0 THRU 7.
       
       77  THE-MODE                    PIC X(7).
       77  WHICH-FIELD                 PIC 9.
       77  OK-TO-DELETE                PIC X.
       77  VENDOR-RECORD-FOUND         PIC X.
       77  STATE-RECORD-FOUND          PIC X.
       77  A-DUMMY                     PIC X.

       77  VENDOR-NUMBER-FIELD         PIC Z(5).

       77  ERROR-MESSAGE               PIC X(79) VALUE SPACE.

           COPY "wscase01.cbl".

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
           OPEN I-O VENDOR-FILE.
           OPEN I-O STATE-FILE.
       
       CLOSING-PROCEDURE.
           CLOSE VENDOR-FILE.
           CLOSE STATE-FILE.

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
           DISPLAY "          1.  ADD RECORDS".
           DISPLAY "          2.  CHANGE A RECORD".
           DISPLAY "          3.  LOOK UP A RECORD".
           DISPLAY "          4.  DELETE A RECORD".
           DISPLAY "          5.  LOOK UP BY NAME".
           DISPLAY "          6.  PRINT RECORDS".
           DISPLAY "          7.  PRINT IN NAME ORDER".
           DISPLAY " ".
           DISPLAY "          0.  EXIT".
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
               PERFORM DELETE-MODE
           ELSE
           IF MENU-PICK = 5
               PERFORM INQUIRE-BY-NAME
           ELSE
           IF MENU-PICK = 6
               PERFORM PRINT-VENDOR-REPORT
           ELSE
           IF MENU-PICK = 7
               PERFORM PRINT-BY-NAME.

      *--------------------------------
      * ADD
      *--------------------------------
       ADD-MODE.
           MOVE "ADD" TO THE-MODE.
           PERFORM GET-NEW-RECORD-KEY.
           PERFORM ADD-RECORDS
               UNTIL VENDOR-NUMBER = ZEROES.
       
       GET-NEW-RECORD-KEY.
           PERFORM ACCEPT-NEW-RECORD-KEY.
           PERFORM RE-ACCEPT-NEW-RECORD-KEY
               UNTIL VENDOR-RECORD-FOUND = "N" OR
                     VENDOR-NUMBER = ZEROES.

       ACCEPT-NEW-RECORD-KEY.
           PERFORM INIT-VENDOR-RECORD.
           PERFORM ENTER-VENDOR-NUMBER.
           IF VENDOR-NUMBER NOT = ZEROES
               PERFORM READ-VENDOR-RECORD.
           
       RE-ACCEPT-NEW-RECORD-KEY.
           DISPLAY "RECORD ALREADY ON FILE".
           PERFORM ACCEPT-NEW-RECORD-KEY.
       
       ADD-RECORDS.
           PERFORM ENTER-REMAINING-FIELDS.
           PERFORM WRITE-VENDOR-RECORD.
           PERFORM GET-NEW-RECORD-KEY.

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
           PERFORM GET-EXISTING-RECORD.
           PERFORM CHANGE-RECORDS
               UNTIL VENDOR-NUMBER = ZEROES.

       CHANGE-RECORDS.
           PERFORM GET-FIELD-TO-CHANGE.
           PERFORM CHANGE-ONE-FIELD
               UNTIL WHICH-FIELD = ZERO.
           PERFORM GET-EXISTING-RECORD.

       GET-FIELD-TO-CHANGE.
           PERFORM DISPLAY-ALL-FIELDS.
           PERFORM ASK-WHICH-FIELD.

       ASK-WHICH-FIELD.
           PERFORM ACCEPT-WHICH-FIELD.
           PERFORM RE-ACCEPT-WHICH-FIELD
               UNTIL WHICH-FIELD < 9.

       ACCEPT-WHICH-FIELD.
           DISPLAY "ENTER THE NUMBER OF THE FIELD".
           DISPLAY "TO CHANGE (1-8) OR 0 TO EXIT".
           ACCEPT WHICH-FIELD.

       RE-ACCEPT-WHICH-FIELD.
           DISPLAY "INVALID ENTRY".
           PERFORM ACCEPT-WHICH-FIELD.

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
           PERFORM GET-EXISTING-RECORD.
           PERFORM INQUIRE-RECORDS
               UNTIL VENDOR-NUMBER = ZEROES.

       INQUIRE-RECORDS.
           PERFORM DISPLAY-ALL-FIELDS.
           PERFORM GET-EXISTING-RECORD.

      *--------------------------------
      * DELETE
      *--------------------------------
       DELETE-MODE.
           MOVE "DELETE" TO THE-MODE.
           PERFORM GET-EXISTING-RECORD.
           PERFORM DELETE-RECORDS
               UNTIL VENDOR-NUMBER = ZEROES.

       DELETE-RECORDS.
           PERFORM DISPLAY-ALL-FIELDS.

           PERFORM ASK-OK-TO-DELETE.
           
           IF OK-TO-DELETE = "Y"
               PERFORM DELETE-VENDOR-RECORD.

           PERFORM GET-EXISTING-RECORD.

       ASK-OK-TO-DELETE.
           PERFORM ACCEPT-OK-TO-DELETE.

           PERFORM RE-ACCEPT-OK-TO-DELETE
               UNTIL OK-TO-DELETE = "Y" OR "N".

       ACCEPT-OK-TO-DELETE.
           DISPLAY "DELETE THIS RECORD (Y/N)?".
           ACCEPT OK-TO-DELETE.
           INSPECT OK-TO-DELETE
               CONVERTING LOWER-ALPHA TO UPPER-ALPHA.
       
       RE-ACCEPT-OK-TO-DELETE.
           DISPLAY "YOU MUST ENTER YES OR NO".
           PERFORM ACCEPT-OK-TO-DELETE.
           
      *--------------------------------
      * Routines shared by all modes
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

      *--------------------------------
      * INQUIRE BY NAME
      *--------------------------------
       INQUIRE-BY-NAME.
           PERFORM CLOSING-PROCEDURE.
           CALL "vninnm03".
           PERFORM OPENING-PROCEDURE.

      *--------------------------------
      * PRINT
      *--------------------------------
       PRINT-VENDOR-REPORT.
           PERFORM CLOSING-PROCEDURE.
           DISPLAY "VENDOR REPORT IN PROGRESS".
           CALL "vndrpt04".
           DISPLAY "PRESS ENTER TO CONTINUE . . .".
           ACCEPT A-DUMMY.
           PERFORM OPENING-PROCEDURE.

      *--------------------------------
      * PRINT BY NAME
      *--------------------------------
       PRINT-BY-NAME.
           PERFORM CLOSING-PROCEDURE.
           DISPLAY "REPORT BY NAME IN PROGRESS".
           CALL "vnbynm02".
           DISPLAY "PRESS ENTER TO CONTINUE . . .".
           ACCEPT A-DUMMY.
           PERFORM OPENING-PROCEDURE.

      *------------------------------------
      * Routines shared by Add and Change
      *------------------------------------
       ENTER-VENDOR-NAME.
           PERFORM ACCEPT-VENDOR-NAME.
           PERFORM RE-ACCEPT-VENDOR-NAME
               UNTIL VENDOR-NAME NOT = SPACE.

       ACCEPT-VENDOR-NAME.
           DISPLAY "ENTER VENDOR NAME".
           ACCEPT VENDOR-NAME.
           INSPECT VENDOR-NAME
               CONVERTING LOWER-ALPHA
               TO         UPPER-ALPHA.

       RE-ACCEPT-VENDOR-NAME.
           DISPLAY "VENDOR NAME MUST BE ENTERED".
           PERFORM ACCEPT-VENDOR-NAME.

       ENTER-VENDOR-ADDRESS-1.
           PERFORM ACCEPT-VENDOR-ADDRESS-1.
           PERFORM RE-ACCEPT-VENDOR-ADDRESS-1
               UNTIL VENDOR-ADDRESS-1 NOT = SPACE.

       ACCEPT-VENDOR-ADDRESS-1.
           DISPLAY "ENTER VENDOR ADDRESS-1".
           ACCEPT VENDOR-ADDRESS-1.
           INSPECT VENDOR-ADDRESS-1
               CONVERTING LOWER-ALPHA
               TO         UPPER-ALPHA.

       RE-ACCEPT-VENDOR-ADDRESS-1.
           DISPLAY "VENDOR ADDRESS-1 MUST BE ENTERED".
           PERFORM ACCEPT-VENDOR-ADDRESS-1.

       ENTER-VENDOR-ADDRESS-2.
           DISPLAY "ENTER VENDOR ADDRESS-2".
           ACCEPT VENDOR-ADDRESS-2.
           INSPECT VENDOR-ADDRESS-2
               CONVERTING LOWER-ALPHA
               TO         UPPER-ALPHA.

       ENTER-VENDOR-CITY.
           PERFORM ACCEPT-VENDOR-CITY.
           PERFORM RE-ACCEPT-VENDOR-CITY
               UNTIL VENDOR-CITY NOT = SPACE.

       ACCEPT-VENDOR-CITY.
           DISPLAY "ENTER VENDOR CITY".
           ACCEPT VENDOR-CITY.
           INSPECT VENDOR-CITY
               CONVERTING LOWER-ALPHA
               TO         UPPER-ALPHA.

       RE-ACCEPT-VENDOR-CITY.
           DISPLAY "VENDOR CITY MUST BE ENTERED".
           PERFORM ACCEPT-VENDOR-CITY.

       ENTER-VENDOR-STATE.
           PERFORM ACCEPT-VENDOR-STATE.
           PERFORM RE-ACCEPT-VENDOR-STATE
               UNTIL VENDOR-STATE NOT = SPACES AND
                     STATE-RECORD-FOUND = "Y".

       ACCEPT-VENDOR-STATE.
           DISPLAY "ENTER VENDOR STATE".
           ACCEPT VENDOR-STATE.
           PERFORM EDIT-CHECK-VENDOR-STATE.
           
       RE-ACCEPT-VENDOR-STATE.
           DISPLAY ERROR-MESSAGE.
           PERFORM ACCEPT-VENDOR-STATE.

       EDIT-CHECK-VENDOR-STATE.
           PERFORM EDIT-VENDOR-STATE.
           PERFORM CHECK-VENDOR-STATE.

       EDIT-VENDOR-STATE.
           INSPECT VENDOR-STATE
               CONVERTING LOWER-ALPHA
               TO         UPPER-ALPHA.

       CHECK-VENDOR-STATE.
           PERFORM VENDOR-STATE-REQUIRED.
           IF VENDOR-STATE NOT = SPACES
               PERFORM VENDOR-STATE-ON-FILE.

       VENDOR-STATE-REQUIRED.
           IF VENDOR-STATE = SPACE
               MOVE "VENDOR STATE MUST BE ENTERED"
                 TO ERROR-MESSAGE.
       
       VENDOR-STATE-ON-FILE.
           MOVE VENDOR-STATE TO STATE-CODE.
           PERFORM READ-STATE-RECORD.
           IF STATE-RECORD-FOUND = "N"
               MOVE "STATE CODE NOT FOUND IN CODES FILE"
                 TO ERROR-MESSAGE.

       ENTER-VENDOR-ZIP.
           PERFORM ACCEPT-VENDOR-ZIP.
           PERFORM RE-ACCEPT-VENDOR-ZIP
               UNTIL VENDOR-ZIP NOT = SPACE.

       ACCEPT-VENDOR-ZIP.
           DISPLAY "ENTER VENDOR ZIP".
           ACCEPT VENDOR-ZIP.
           INSPECT VENDOR-ZIP
               CONVERTING LOWER-ALPHA
               TO         UPPER-ALPHA.

       RE-ACCEPT-VENDOR-ZIP.
           DISPLAY "VENDOR ZIP MUST BE ENTERED".
           PERFORM ACCEPT-VENDOR-ZIP.

       ENTER-VENDOR-CONTACT.
           DISPLAY "ENTER VENDOR CONTACT".
           ACCEPT VENDOR-CONTACT.
           INSPECT VENDOR-CONTACT
               CONVERTING LOWER-ALPHA
               TO         UPPER-ALPHA.

       ENTER-VENDOR-PHONE.
           PERFORM ACCEPT-VENDOR-PHONE.
           PERFORM RE-ACCEPT-VENDOR-PHONE
               UNTIL VENDOR-PHONE NOT = SPACE.

       ACCEPT-VENDOR-PHONE.
           DISPLAY "ENTER VENDOR PHONE".
           ACCEPT VENDOR-PHONE.
           INSPECT VENDOR-PHONE
               CONVERTING LOWER-ALPHA
               TO         UPPER-ALPHA.

       RE-ACCEPT-VENDOR-PHONE.
           DISPLAY "VENDOR PHONE MUST BE ENTERED".
           PERFORM ACCEPT-VENDOR-PHONE.

      *------------------------------------
      * Routines shared by Change,
      * Inquire, and Delete
      *------------------------------------
       GET-EXISTING-RECORD.
           PERFORM ACCEPT-EXISTING-KEY.
           PERFORM RE-ACCEPT-EXISTING-KEY
               UNTIL VENDOR-RECORD-FOUND = "Y" OR
                     VENDOR-NUMBER = ZEROES.

       ACCEPT-EXISTING-KEY.
           PERFORM INIT-VENDOR-RECORD.
           PERFORM ENTER-VENDOR-NUMBER.
           IF VENDOR-NUMBER NOT = ZEROES
               PERFORM READ-VENDOR-RECORD.

       RE-ACCEPT-EXISTING-KEY.
           DISPLAY "RECORD NOT FOUND".
           PERFORM ACCEPT-EXISTING-KEY.

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
           PERFORM VENDOR-STATE-ON-FILE.
           IF STATE-RECORD-FOUND = "N"
               MOVE "**Not Found**" TO STATE-NAME.
           DISPLAY "5. VENDOR STATE: " 
                   VENDOR-STATE " "
                   STATE-NAME.

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
           MOVE "Y" TO VENDOR-RECORD-FOUND.
           READ VENDOR-FILE RECORD
               INVALID KEY
               MOVE "N" TO VENDOR-RECORD-FOUND.
       
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
       
       READ-STATE-RECORD.
           MOVE "Y" TO STATE-RECORD-FOUND.
           READ STATE-FILE RECORD
               INVALID KEY
               MOVE "N" TO STATE-RECORD-FOUND.
               