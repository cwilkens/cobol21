       IDENTIFICATION DIVISION.
       PROGRAM-ID. VCHMNT01.
      *------------------------------------
      * Add, Change, Inquire and Delete
      * for the Voucher File.
      * All fields are displayed, but
      * SELECTED, PAID-AMOUNT, PAID-DATE and
      * CHECK-NO are not modifiable
      *------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
           COPY "slvnd02.cbl".

           COPY "slvouch.cbl".

           COPY "slcontrl.cbl".

       DATA DIVISION.
       FILE SECTION.

           COPY "fdvnd04.cbl".

           COPY "fdvouch.cbl".

           COPY "fdcontrl.cbl".
           
       WORKING-STORAGE SECTION.

       77  MENU-PICK                   PIC 9.
           88 MENU-PICK-IS-VALID       VALUES 0 THRU 4.
       
       77  THE-MODE                    PIC X(7).
       77  WHICH-FIELD                 PIC 9.
       77  OK-TO-DELETE                PIC X.
       77  VOUCHER-RECORD-FOUND        PIC X.
       77  CONTROL-RECORD-FOUND        PIC X.
       77  VENDOR-RECORD-FOUND         PIC X.
       77  A-DUMMY                     PIC X.
       77  ADD-ANOTHER                 PIC X.

       77  VENDOR-NUMBER-FIELD         PIC Z(5).
       77  VOUCHER-AMOUNT-FIELD        PIC ZZZ,ZZ9.99-.
       77  VOUCHER-PAID-AMOUNT-FIELD   PIC ZZZ,ZZ9.99-.

       77  ERROR-MESSAGE               PIC X(79) VALUE SPACE.

           COPY "wscase01.cbl".

           COPY "wsdate01.cbl".

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
           OPEN I-O VOUCHER-FILE.
           OPEN I-O VENDOR-FILE.
           OPEN I-O CONTROL-FILE.

       CLOSING-PROCEDURE.
           CLOSE VOUCHER-FILE.
           CLOSE VENDOR-FILE.
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
           DISPLAY "          1.  ADD RECORDS".
           DISPLAY "          2.  CHANGE A RECORD".
           DISPLAY "          3.  LOOK UP A RECORD".
           DISPLAY "          4.  DELETE A RECORD".
           DISPLAY " ".
           DISPLAY "          0.  EXIT".
           PERFORM SCROLL-LINE 8 TIMES.

       ACCEPT-MENU-PICK.
           DISPLAY "YOUR CHOICE (0-4)?".
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
               PERFORM DELETE-MODE.

      *--------------------------------
      * ADD
      *--------------------------------
       ADD-MODE.
           MOVE "ADD" TO THE-MODE.
           MOVE "Y" TO ADD-ANOTHER.
           PERFORM GET-NEW-RECORD-KEY.
           PERFORM ADD-RECORDS
               UNTIL ADD-ANOTHER = "N".
       
       GET-NEW-RECORD-KEY.
           PERFORM ACCEPT-NEW-RECORD-KEY.
           PERFORM RE-ACCEPT-NEW-RECORD-KEY
               UNTIL VOUCHER-RECORD-FOUND = "N".

           PERFORM DISPLAY-VOUCHER-NUMBER.

       ACCEPT-NEW-RECORD-KEY.
           PERFORM INIT-VOUCHER-RECORD.
           PERFORM RETRIEVE-NEXT-VOUCHER-NUMBER.
           
           PERFORM READ-VOUCHER-RECORD.
           
       RE-ACCEPT-NEW-RECORD-KEY.
           PERFORM ACCEPT-NEW-RECORD-KEY.

       RETRIEVE-NEXT-VOUCHER-NUMBER.
           PERFORM READ-CONTROL-RECORD.
           ADD 1 TO CONTROL-LAST-VOUCHER.
           MOVE CONTROL-LAST-VOUCHER TO VOUCHER-NUMBER.
           PERFORM REWRITE-CONTROL-RECORD.
       
       ADD-RECORDS.
           PERFORM ENTER-REMAINING-FIELDS.
           PERFORM WRITE-VOUCHER-RECORD.
           PERFORM GET-ADD-ANOTHER.

       GET-ADD-ANOTHER.
           PERFORM ACCEPT-ADD-ANOTHER.
           PERFORM RE-ACCEPT-ADD-ANOTHER
               UNTIL ADD-ANOTHER = "Y" OR "N".

       ACCEPT-ADD-ANOTHER.
           DISPLAY "ADD ANOTHER VOUCHER(Y/N)?".
           ACCEPT ADD-ANOTHER.

           INSPECT ADD-ANOTHER
               CONVERTING LOWER-ALPHA
               TO         UPPER-ALPHA.

           IF ADD-ANOTHER = "Y"
               PERFORM GET-NEW-RECORD-KEY.

       RE-ACCEPT-ADD-ANOTHER.
           DISPLAY "YOU MUST ENTER YES OR NO".
           PERFORM ACCEPT-ADD-ANOTHER.

       ENTER-REMAINING-FIELDS.
           PERFORM ENTER-VOUCHER-VENDOR.
           PERFORM ENTER-VOUCHER-INVOICE.
           PERFORM ENTER-VOUCHER-FOR.
           PERFORM ENTER-VOUCHER-AMOUNT.
           PERFORM ENTER-VOUCHER-DATE.
           PERFORM ENTER-VOUCHER-DUE.
           PERFORM ENTER-VOUCHER-DEDUCTIBLE.
           PERFORM ENTER-VOUCHER-SELECTED.
       
      *--------------------------------
      * CHANGE
      *--------------------------------
       CHANGE-MODE.
           MOVE "CHANGE" TO THE-MODE.
           PERFORM GET-EXISTING-RECORD.
           PERFORM CHANGE-RECORDS
               UNTIL VOUCHER-NUMBER = ZEROES.

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
               UNTIL WHICH-FIELD < 8.

       ACCEPT-WHICH-FIELD.
           DISPLAY "ENTER THE NUMBER OF THE FIELD".
           DISPLAY "TO CHANGE (1-7) OR 0 TO EXIT".
           ACCEPT WHICH-FIELD.

       RE-ACCEPT-WHICH-FIELD.
           DISPLAY "INVALID ENTRY".
           PERFORM ACCEPT-WHICH-FIELD.

       CHANGE-ONE-FIELD.
           PERFORM CHANGE-THIS-FIELD.
           PERFORM GET-FIELD-TO-CHANGE.

       CHANGE-THIS-FIELD.
           IF WHICH-FIELD = 1
               PERFORM ENTER-VOUCHER-VENDOR.
           IF WHICH-FIELD = 2
               PERFORM ENTER-VOUCHER-INVOICE.
           IF WHICH-FIELD = 3
               PERFORM ENTER-VOUCHER-FOR.
           IF WHICH-FIELD = 4
               PERFORM ENTER-VOUCHER-AMOUNT.
           IF WHICH-FIELD = 5
               PERFORM ENTER-VOUCHER-DATE.
           IF WHICH-FIELD = 6
               PERFORM ENTER-VOUCHER-DUE.
           IF WHICH-FIELD = 7
               PERFORM ENTER-VOUCHER-DEDUCTIBLE.
           
           PERFORM REWRITE-VOUCHER-RECORD.

      *--------------------------------
      * INQUIRE
      *--------------------------------
       INQUIRE-MODE.
           MOVE "DISPLAY" TO THE-MODE.
           PERFORM GET-EXISTING-RECORD.
           PERFORM INQUIRE-RECORDS
               UNTIL VOUCHER-NUMBER = ZEROES.

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
               UNTIL VOUCHER-NUMBER = ZEROES.

       DELETE-RECORDS.
           PERFORM DISPLAY-ALL-FIELDS.

           PERFORM ASK-OK-TO-DELETE.
           
           IF OK-TO-DELETE = "Y"
               PERFORM DELETE-VOUCHER-RECORD.

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
       INIT-VOUCHER-RECORD.
           MOVE SPACE TO VOUCHER-INVOICE
                         VOUCHER-FOR
                         VOUCHER-DEDUCTIBLE
                         VOUCHER-SELECTED.
           MOVE ZEROES TO VOUCHER-NUMBER
                          VOUCHER-VENDOR
                          VOUCHER-AMOUNT
                          VOUCHER-DATE
                          VOUCHER-DUE
                          VOUCHER-PAID-AMOUNT
                          VOUCHER-PAID-DATE
                          VOUCHER-CHECK-NO.

      *------------------------------------
      * Routines shared by Add and Change
      *------------------------------------
       ENTER-VOUCHER-VENDOR.
           PERFORM ACCEPT-VOUCHER-VENDOR.
           PERFORM RE-ACCEPT-VOUCHER-VENDOR
               UNTIL VOUCHER-VENDOR NOT = ZEROES AND
                     VENDOR-RECORD-FOUND = "Y".
       
       ACCEPT-VOUCHER-VENDOR.
           DISPLAY "ENTER VENDOR".
           ACCEPT VOUCHER-VENDOR.
           PERFORM EDIT-CHECK-VOUCHER-VENDOR.
           PERFORM DISPLAY-VOUCHER-VENDOR.

       RE-ACCEPT-VOUCHER-VENDOR.
           DISPLAY ERROR-MESSAGE.
           PERFORM ACCEPT-VOUCHER-VENDOR.
       
       EDIT-CHECK-VOUCHER-VENDOR.
           PERFORM EDIT-VOUCHER-VENDOR.
           PERFORM CHECK-VOUCHER-VENDOR.

       EDIT-VOUCHER-VENDOR.

       CHECK-VOUCHER-VENDOR.
           PERFORM VOUCHER-VENDOR-REQUIRED.
           IF VOUCHER-VENDOR NOT = ZEROES
               PERFORM VOUCHER-VENDOR-ON-FILE.
       
       VOUCHER-VENDOR-REQUIRED.
           IF VOUCHER-VENDOR = ZEROES
               MOVE "VENDOR MUST BE ENTERED"
                 TO ERROR-MESSAGE.

       VOUCHER-VENDOR-ON-FILE.
           MOVE VOUCHER-VENDOR TO VENDOR-NUMBER.
           PERFORM READ-VENDOR-RECORD.
           IF VENDOR-RECORD-FOUND = "N"
               MOVE "VENDOR NOT ON FILE"
                 TO ERROR-MESSAGE.

       ENTER-VOUCHER-INVOICE.
           PERFORM ACCEPT-VOUCHER-INVOICE.
           PERFORM RE-ACCEPT-VOUCHER-INVOICE
               UNTIL VOUCHER-INVOICE NOT = SPACE.
       
       ACCEPT-VOUCHER-INVOICE.
           DISPLAY "ENTER INVOICE NUMBER".
           ACCEPT VOUCHER-INVOICE.
           INSPECT VOUCHER-INVOICE
               CONVERTING LOWER-ALPHA
               TO         UPPER-ALPHA.

       RE-ACCEPT-VOUCHER-INVOICE.
           DISPLAY "INVOICE MUST BE ENTERED".
           PERFORM ACCEPT-VOUCHER-INVOICE.
       
       ENTER-VOUCHER-FOR.
           PERFORM ACCEPT-VOUCHER-FOR.
           PERFORM RE-ACCEPT-VOUCHER-FOR
               UNTIL VOUCHER-FOR NOT = SPACE.
       
       ACCEPT-VOUCHER-FOR.
           DISPLAY "WHAT FOR?".
           ACCEPT VOUCHER-FOR.
           INSPECT VOUCHER-FOR
               CONVERTING LOWER-ALPHA
               TO         UPPER-ALPHA.

       RE-ACCEPT-VOUCHER-FOR.
           DISPLAY "A DESCRIPTION MUST BE ENTERED".
           PERFORM ACCEPT-VOUCHER-FOR.
       
       ENTER-VOUCHER-AMOUNT.
           PERFORM ACCEPT-VOUCHER-AMOUNT.
           PERFORM RE-ACCEPT-VOUCHER-AMOUNT
               UNTIL VOUCHER-AMOUNT NOT = ZEROES.
       
       ACCEPT-VOUCHER-AMOUNT.
           DISPLAY "ENTER INVOICE AMOUNT".
           ACCEPT VOUCHER-AMOUNT-FIELD.
           MOVE VOUCHER-AMOUNT-FIELD TO VOUCHER-AMOUNT.

       RE-ACCEPT-VOUCHER-AMOUNT.
           DISPLAY "AMOUNT MUST NOT BE ZERO".
           PERFORM ACCEPT-VOUCHER-AMOUNT.
       
       ENTER-VOUCHER-DATE.
           MOVE "N" TO ZERO-DATE-IS-OK.
           MOVE "ENTER INVOICE DATE(MM/DD/YYYY)?"
               TO DATE-PROMPT.
           MOVE "AN INVOICE DATE IS REQUIRED"
               TO DATE-ERROR-MESSAGE.
           PERFORM GET-A-DATE.
           MOVE DATE-YYYYMMDD TO VOUCHER-DATE.
       
       ENTER-VOUCHER-DUE.
           MOVE "N" TO ZERO-DATE-IS-OK.
           MOVE "ENTER DUE DATE(MM/DD/YYYY)?"
               TO DATE-PROMPT.
           MOVE "A DUE DATE IS REQUIRED"
               TO DATE-ERROR-MESSAGE.
           PERFORM GET-A-DATE.
           MOVE DATE-YYYYMMDD TO VOUCHER-DUE.
       

       ENTER-VOUCHER-DEDUCTIBLE.
           PERFORM ACCEPT-VOUCHER-DEDUCTIBLE.
           PERFORM RE-ACCEPT-VOUCHER-DEDUCTIBLE
               UNTIL VOUCHER-DEDUCTIBLE = "Y" OR "N".
       
       ACCEPT-VOUCHER-DEDUCTIBLE.
           DISPLAY "IS THIS TAX DEDUCTIBLE?".
           ACCEPT VOUCHER-DEDUCTIBLE.
           INSPECT VOUCHER-DEDUCTIBLE
               CONVERTING LOWER-ALPHA
               TO         UPPER-ALPHA.

       RE-ACCEPT-VOUCHER-DEDUCTIBLE.
           DISPLAY "MUST BE YES OR NO".
           PERFORM ACCEPT-VOUCHER-DEDUCTIBLE.
       
       ENTER-VOUCHER-SELECTED.
           MOVE "N" TO VOUCHER-SELECTED.

      *------------------------------------
      * Routines shared by Change,
      * Inquire, and Delete
      *------------------------------------
       GET-EXISTING-RECORD.
           PERFORM ACCEPT-EXISTING-KEY.
           PERFORM RE-ACCEPT-EXISTING-KEY
               UNTIL VOUCHER-RECORD-FOUND = "Y" OR
                     VOUCHER-NUMBER = ZEROES.

       ACCEPT-EXISTING-KEY.
           PERFORM INIT-VOUCHER-RECORD.
           PERFORM ENTER-VOUCHER-NUMBER.
           IF VOUCHER-NUMBER NOT = ZEROES
               PERFORM READ-VOUCHER-RECORD.

       RE-ACCEPT-EXISTING-KEY.
           DISPLAY "RECORD NOT FOUND".
           PERFORM ACCEPT-EXISTING-KEY.

       ENTER-VOUCHER-NUMBER.
           DISPLAY "ENTER VOUCHER NUMBER TO "
                   THE-MODE.
           ACCEPT VOUCHER-NUMBER.

       DISPLAY-ALL-FIELDS.
           DISPLAY " ".
           PERFORM DISPLAY-VOUCHER-NUMBER.
           PERFORM DISPLAY-VOUCHER-VENDOR.
           PERFORM DISPLAY-VOUCHER-INVOICE.
           PERFORM DISPLAY-VOUCHER-FOR.
           PERFORM DISPLAY-VOUCHER-AMOUNT.
           PERFORM DISPLAY-VOUCHER-DATE.
           PERFORM DISPLAY-VOUCHER-DUE.
           PERFORM DISPLAY-VOUCHER-DEDUCTIBLE.
           IF VOUCHER-PAID-DATE = ZEROES
               PERFORM DISPLAY-VOUCHER-SELECTED.
           IF VOUCHER-PAID-DATE NOT = ZEROES
               PERFORM DISPLAY-VOUCHER-PAID-AMOUNT
               PERFORM DISPLAY-VOUCHER-PAID-DATE
               PERFORM DISPLAY-VOUCHER-CHECK-NO.
           DISPLAY " ".
       
       DISPLAY-VOUCHER-NUMBER.
           DISPLAY "   VOUCHER NUMBER: " VOUCHER-NUMBER.

       DISPLAY-VOUCHER-VENDOR.
           PERFORM VOUCHER-VENDOR-ON-FILE.
           IF VENDOR-RECORD-FOUND = "N"
               MOVE "**Not Found**" TO VENDOR-NAME.
           DISPLAY "1. VENDOR: " 
                   VOUCHER-VENDOR " "
                   VENDOR-NAME.

       DISPLAY-VOUCHER-INVOICE.
           DISPLAY "2. INVOICE: " VOUCHER-INVOICE.

       DISPLAY-VOUCHER-FOR.
           DISPLAY "3. FOR: " VOUCHER-FOR.

       DISPLAY-VOUCHER-AMOUNT.
           MOVE VOUCHER-AMOUNT TO VOUCHER-AMOUNT-FIELD.
           DISPLAY "4. AMOUNT: " VOUCHER-AMOUNT-FIELD.

       DISPLAY-VOUCHER-DATE.
           MOVE VOUCHER-DATE TO DATE-YYYYMMDD.
           PERFORM FORMAT-THE-DATE.
           DISPLAY "5. INVOICE DATE: " FORMATTED-DATE.

       DISPLAY-VOUCHER-DUE.
           MOVE VOUCHER-DUE TO DATE-YYYYMMDD.
           PERFORM FORMAT-THE-DATE.
           DISPLAY "6. DUE DATE: " FORMATTED-DATE.

       DISPLAY-VOUCHER-DEDUCTIBLE.
           DISPLAY "7. DEDUCTIBLE: " VOUCHER-DEDUCTIBLE.

       DISPLAY-VOUCHER-SELECTED.
           DISPLAY "   SELECTED FOR PAYMENT: " VOUCHER-SELECTED.

       DISPLAY-VOUCHER-PAID-AMOUNT.
           MOVE VOUCHER-PAID-AMOUNT TO VOUCHER-PAID-AMOUNT-FIELD.
           DISPLAY "   PAID: " VOUCHER-PAID-AMOUNT-FIELD.

       DISPLAY-VOUCHER-PAID-DATE.
           MOVE VOUCHER-PAID-DATE TO DATE-YYYYMMDD.
           PERFORM FORMAT-THE-DATE.
           DISPLAY "   PAID ON: " FORMATTED-DATE.

       DISPLAY-VOUCHER-CHECK-NO.
           DISPLAY "   CHECK: " VOUCHER-CHECK-NO.

      *--------------------------------
      * File I-O Routines
      *--------------------------------
       READ-VOUCHER-RECORD.
           MOVE "Y" TO VOUCHER-RECORD-FOUND.
           READ VOUCHER-FILE RECORD
               INVALID KEY
               MOVE "N" TO VOUCHER-RECORD-FOUND.
       
      * or READ VOUCHER-FILE RECORD WITH LOCK



      * or READ VOUCHER-FILE RECORD WITH HOLD



       WRITE-VOUCHER-RECORD.
           WRITE VOUCHER-RECORD
               INVALID KEY
               DISPLAY "RECORD ALREADY ON FILE".

       REWRITE-VOUCHER-RECORD.
           REWRITE VOUCHER-RECORD
               INVALID KEY
               DISPLAY "ERROR REWRITING VOUCHER RECORD".

       DELETE-VOUCHER-RECORD.
           DELETE VOUCHER-FILE RECORD
               INVALID KEY
               DISPLAY "ERROR DELETING VOUCHER RECORD".
       
       READ-VENDOR-RECORD.
           MOVE "Y" TO VENDOR-RECORD-FOUND.
           READ VENDOR-FILE RECORD
               INVALID KEY
               MOVE "N" TO VENDOR-RECORD-FOUND.

       READ-CONTROL-RECORD.
           MOVE 1 TO CONTROL-KEY.
           MOVE "Y" TO CONTROL-RECORD-FOUND.
           READ CONTROL-FILE RECORD
               INVALID KEY
               MOVE "N" TO CONTROL-RECORD-FOUND
               DISPLAY "CONTROL FILE IS INVALID".

       REWRITE-CONTROL-RECORD.
           REWRITE CONTROL-RECORD
               INVALID KEY
               DISPLAY "ERROR REWRITING CONTROL RECORD".

           COPY "pldate01.cbl".
