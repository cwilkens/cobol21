       IDENTIFICATION DIVISION.
       PROGRAM-ID. VCHPIC01.
      *------------------------------------
      * Change only.
      * Allows the user to change
      * the VOUCHER-SELECTED flag
      * for unpaid vouchers
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
       
       77  WHICH-FIELD                 PIC 9.
       77  VOUCHER-RECORD-FOUND        PIC X.
       77  VENDOR-RECORD-FOUND         PIC X.
       
       77  VOUCHER-NUMBER-FIELD        PIC Z(5).
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

       CLOSING-PROCEDURE.
           CLOSE VOUCHER-FILE.
           CLOSE VENDOR-FILE.

       MAIN-PROCESS.
           PERFORM CHANGE-MODE.

      *--------------------------------
      * CHANGE
      *--------------------------------
       CHANGE-MODE.
           PERFORM GET-EXISTING-RECORD.
           PERFORM CHANGE-RECORDS
               UNTIL VOUCHER-NUMBER = ZEROES.

       CHANGE-RECORDS.
           PERFORM GET-FIELD-TO-CHANGE.
           IF VOUCHER-PAID-DATE = ZEROES
               PERFORM CHANGE-ONE-FIELD.
           PERFORM GET-EXISTING-RECORD.

       GET-FIELD-TO-CHANGE.
           PERFORM DISPLAY-ALL-FIELDS.
           PERFORM ASK-WHICH-FIELD.

       ASK-WHICH-FIELD.
           MOVE 1 TO WHICH-FIELD.

       CHANGE-ONE-FIELD.
           PERFORM CHANGE-THIS-FIELD.

       CHANGE-THIS-FIELD.
           IF WHICH-FIELD = 1
               PERFORM ENTER-VOUCHER-SELECTED.
           
           PERFORM REWRITE-VOUCHER-RECORD.

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
       ENTER-VOUCHER-SELECTED.
           PERFORM ACCEPT-VOUCHER-SELECTED.
           PERFORM RE-ACCEPT-VOUCHER-SELECTED
               UNTIL VOUCHER-SELECTED = "Y" OR "N".
       
       ACCEPT-VOUCHER-SELECTED.
           DISPLAY "SELECT THIS VOUCHER (Y/N)?".
           ACCEPT VOUCHER-SELECTED.

           INSPECT VOUCHER-SELECTED
               CONVERTING LOWER-ALPHA
               TO UPPER-ALPHA.

       RE-ACCEPT-VOUCHER-SELECTED.
           DISPLAY "YOU MUST ENTER YES OR NO".
           PERFORM ACCEPT-VOUCHER-SELECTED.
       
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
           DISPLAY "ENTER VOUCHER NUMBER TO SELECT OR CLEAR ".
           ACCEPT VOUCHER-NUMBER.

       DISPLAY-ALL-FIELDS.
           DISPLAY " ".
           IF VOUCHER-PAID-DATE NOT = ZEROES
               DISPLAY " !!! THIS VOUCHER IS ALREADY PAID !!!".
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
           DISPLAY "   VENDOR: " 
                   VOUCHER-VENDOR " "
                   VENDOR-NAME.

       VOUCHER-VENDOR-ON-FILE.
           MOVE VOUCHER-VENDOR TO VENDOR-NUMBER.
           PERFORM READ-VENDOR-RECORD.
           IF VENDOR-RECORD-FOUND = "N"
               MOVE "VENDOR NOT ON FILE"
                   TO ERROR-MESSAGE.

       DISPLAY-VOUCHER-INVOICE.
           DISPLAY "   INVOICE: " VOUCHER-INVOICE.

       DISPLAY-VOUCHER-FOR.
           DISPLAY "   FOR: " VOUCHER-FOR.

       DISPLAY-VOUCHER-AMOUNT.
           MOVE VOUCHER-AMOUNT TO VOUCHER-AMOUNT-FIELD.
           DISPLAY "   AMOUNT: " VOUCHER-AMOUNT-FIELD.

       DISPLAY-VOUCHER-DATE.
           MOVE VOUCHER-DATE TO DATE-YYYYMMDD.
           PERFORM FORMAT-THE-DATE.
           DISPLAY "   INVOICE DATE: " FORMATTED-DATE.

       DISPLAY-VOUCHER-DUE.
           MOVE VOUCHER-DUE TO DATE-YYYYMMDD.
           PERFORM FORMAT-THE-DATE.
           DISPLAY "   DUE DATE: " FORMATTED-DATE.

       DISPLAY-VOUCHER-DEDUCTIBLE.
           DISPLAY "   DEDUCTIBLE: " VOUCHER-DEDUCTIBLE.

       DISPLAY-VOUCHER-SELECTED.
           DISPLAY "1. SELECTED FOR PAYMENT: " VOUCHER-SELECTED.

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



       REWRITE-VOUCHER-RECORD.
           REWRITE VOUCHER-RECORD
               INVALID KEY
               DISPLAY "ERROR REWRITING VOUCHER RECORD".

       READ-VENDOR-RECORD.
           MOVE "Y" TO VENDOR-RECORD-FOUND.
           READ VENDOR-FILE RECORD
               INVALID KEY
               MOVE "N" TO VENDOR-RECORD-FOUND.
               
           COPY "pldate01.cbl".
