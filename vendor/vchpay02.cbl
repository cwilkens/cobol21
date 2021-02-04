       IDENTIFICATION DIVISION.
       PROGRAM-ID. VCHPAY01.
      *------------------------------------
      * Change only.
      * User can request a voucher.
      * If the voucher is already paid,
      * the user is asked if they
      * would like to clear the payment
      * and reopen the voucher.
      * If the voucher is not paid,
      * the user is required to enter
      * a payment date, amount and check
      * number.
      * Only maintains PAID-DATE
      * CHECK-NO and PAID-AMOUNT.
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

       77  WHICH-FIELD                 PIC 9.
       77  OK-TO-PROCESS               PIC X.
       77  FULL-PAYMENT                PIC X.
       77  NEW-VOUCHER                 PIC X.

       77  VOUCHER-RECORD-FOUND        PIC X.
       77  VENDOR-RECORD-FOUND         PIC X.
       77  CONTROL-RECORD-FOUND        PIC X.
       77  VOUCHER-NUMBER-FIELD        PIC Z(5).
       77  AN-AMOUNT-FIELD             PIC ZZZ,ZZ9.99-.
       77  CHECK-NO-FIELD              PIC Z(6).

       77  PROCESS-MESSAGE             PIC X(79) VALUE SPACE.

       77  SAVE-VOUCHER-RECORD         PIC X(103).

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
           PERFORM CHANGE-MODE.

      *--------------------------------
      * CHANGE
      *--------------------------------
       CHANGE-MODE.
           PERFORM GET-EXISTING-RECORD.
           PERFORM CHANGE-RECORDS
               UNTIL VOUCHER-NUMBER = ZEROES.

       CHANGE-RECORDS.
           PERFORM DISPLAY-ALL-FIELDS.
           IF VOUCHER-PAID-DATE = ZEROES
               PERFORM CHANGE-TO-PAID
           ELSE
               PERFORM CHANGE-TO-UNPAID.

           PERFORM GET-EXISTING-RECORD.

      *--------------------------------
      * Ask if the user wants to pay this
      * voucher and if so:
      * Change the voucher to paid status
      * by getting PAID-DATE, PAID-AMOUNT
      * and CHECK-NO.
      *--------------------------------
       CHANGE-TO-PAID.
           PERFORM ASK-OK-TO-PAY.
           IF OK-TO-PROCESS = "Y"
               PERFORM CHANGE-ALL-FIELDS.

       ASK-OK-TO-PAY.
           MOVE "PROCESS THIS VOUCHER AS PAID (Y/N)?"
               TO PROCESS-MESSAGE.
           PERFORM ASK-OK-TO-PROCESS.

       CHANGE-ALL-FIELDS.
           PERFORM CHANGE-THIS-FIELD
               VARYING WHICH-FIELD FROM 1 BY 1
               UNTIL WHICH-FIELD > 3.

           PERFORM REWRITE-VOUCHER-RECORD.

           IF NEW-VOUCHER = "Y"
               PERFORM GENERATE-NEW-VOUCHER.

       CHANGE-THIS-FIELD.
           IF WHICH-FIELD = 1
               PERFORM ENTER-VOUCHER-PAID-DATE.
           IF WHICH-FIELD = 2
               PERFORM ENTER-VOUCHER-PAYMENT.
           IF WHICH-FIELD = 3
               PERFORM ENTER-VOUCHER-CHECK-NO.

      *--------------------------------
      * Ask if the user wants to re-open
      * this voucher and if so:
      * Move zeroes to PAID-DATE, 
      * PAID-AMOUNT and CHECK-NO.
      *--------------------------------
       CHANGE-TO-UNPAID.
           PERFORM ASK-OK-TO-OPEN.
           IF OK-TO-PROCESS = "Y"
               PERFORM CLEAR-PAID-AND-REWRITE
               DISPLAY "VOUCHER HAS BEEN RE OPENED".

       CLEAR-PAID-AND-REWRITE.
           PERFORM CLEAR-PAID-FIELDS.
           PERFORM REWRITE-VOUCHER-RECORD.

       CLEAR-PAID-FIELDS.
           MOVE ZEROES TO VOUCHER-PAID-DATE
                          VOUCHER-PAID-AMOUNT
                          VOUCHER-CHECK-NO.

       ASK-OK-TO-OPEN.
           MOVE "RE-OPEN THIS VOUCHER (Y/N)?" TO PROCESS-MESSAGE.
           PERFORM ASK-OK-TO-PROCESS.

      *--------------------------------
      * This routine is used by both
      * ASK-OK-TO-PAY which is part of
      * the CHANGE-TO-PAID logic, and 
      * ASK-OK-TO-OPEN which is part 
      * of the CHANGE-TO-UNPAID logic.
      *--------------------------------
       ASK-OK-TO-PROCESS.
           PERFORM ACCEPT-OK-TO-PROCESS.

           PERFORM RE-ACCEPT-OK-TO-PROCESS
               UNTIL OK-TO-PROCESS = "Y" OR "N".

       ACCEPT-OK-TO-PROCESS.
           DISPLAY PROCESS-MESSAGE.
           ACCEPT OK-TO-PROCESS.
           INSPECT OK-TO-PROCESS
               CONVERTING LOWER-ALPHA TO UPPER-ALPHA.

       RE-ACCEPT-OK-TO-PROCESS.
           DISPLAY "YOU MUST ENTER YES OR NO".
           PERFORM ACCEPT-OK-TO-PROCESS.

      *--------------------------------
      * Field entry routines.
      *--------------------------------
       ENTER-VOUCHER-PAID-DATE.
           MOVE "N" TO ZERO-DATE-IS-OK.
           MOVE "ENTER PAID DATE(MM/DD/YYYY)?"
               TO DATE-PROMPT.
           MOVE "A VALID PAID DATE IS REQUIRED"
               TO DATE-ERROR-MESSAGE.
           PERFORM GET-A-DATE.
           MOVE DATE-YYYYMMDD TO VOUCHER-PAID-DATE.

      *--------------------------------
      * Voucher payment is entered by
      * asking if the payment is for
      * the exact amount of the voucher.
      * If it is, VOUCHER-AMOUNT
      * is moved in to VOUCHER-PAID-AMOUNT.
      * If it is not, then the user is
      * asked to enter the amount
      * to be paid.
      * If the paid amount is less than
      * the voucher amount, the user
      * is also asked if a new voucher
      * should be generated for
      * the balance. This allows
      * for partial payments.
      *--------------------------------
       ENTER-VOUCHER-PAYMENT.
           MOVE "N" TO NEW-VOUCHER.
           PERFORM ASK-FULL-PAYMENT.
           IF FULL-PAYMENT = "Y"
               MOVE VOUCHER-AMOUNT TO VOUCHER-PAID-AMOUNT
           ELSE
               PERFORM ENTER-VOUCHER-PAID-AMOUNT
               IF VOUCHER-PAID-AMOUNT < VOUCHER-AMOUNT
                   PERFORM ASK-NEW-VOUCHER.

       ASK-FULL-PAYMENT.
           PERFORM ACCEPT-FULL-PAYMENT.
           PERFORM RE-ACCEPT-FULL-PAYMENT
               UNTIL FULL-PAYMENT = "Y" OR "N".

       ACCEPT-FULL-PAYMENT.
           MOVE VOUCHER-AMOUNT TO AN-AMOUNT-FIELD.
           DISPLAY "PAY THE EXACT AMOUNT "
                   AN-AMOUNT-FIELD
                   " (Y/N)?".
           ACCEPT FULL-PAYMENT.
           INSPECT FULL-PAYMENT
               CONVERTING LOWER-ALPHA TO UPPER-ALPHA.

       RE-ACCEPT-FULL-PAYMENT.
           DISPLAY "YOU MUST ENTER YES OR NO".
           PERFORM ACCEPT-FULL-PAYMENT.

       ASK-NEW-VOUCHER.
           PERFORM ACCEPT-NEW-VOUCHER.
           PERFORM RE-ACCEPT-NEW-VOUCHER
               UNTIL NEW-VOUCHER = "Y" OR "N".

       ACCEPT-NEW-VOUCHER.
           MOVE VOUCHER-AMOUNT TO AN-AMOUNT-FIELD.
           DISPLAY "GENERATE A NEW VOUCHER".
           DISPLAY " FOR THE BALANCE (Y/N)?".
           ACCEPT NEW-VOUCHER.
           INSPECT NEW-VOUCHER
               CONVERTING LOWER-ALPHA TO UPPER-ALPHA.

       RE-ACCEPT-NEW-VOUCHER.
           DISPLAY "YOU MUST ENTER YES OR NO".
           PERFORM ACCEPT-NEW-VOUCHER.

       ENTER-VOUCHER-PAID-AMOUNT.
           PERFORM ACCEPT-VOUCHER-PAID-AMOUNT.
           PERFORM RE-ACCEPT-VOUCHER-PAID-AMOUNT
               UNTIL VOUCHER-PAID-AMOUNT NOT = ZEROES.


       ACCEPT-VOUCHER-PAID-AMOUNT.
           DISPLAY "ENTER AMOUNT PAID".
           ACCEPT AN-AMOUNT-FIELD.
           MOVE AN-AMOUNT-FIELD TO VOUCHER-PAID-AMOUNT.

       RE-ACCEPT-VOUCHER-PAID-AMOUNT.
           MOVE VOUCHER-AMOUNT TO AN-AMOUNT-FIELD.
           DISPLAY "A PAYMENT MUST BE ENTERED".
           DISPLAY "AGAINST " AN-AMOUNT-FIELD.
           PERFORM ACCEPT-VOUCHER-PAID-AMOUNT.

       ENTER-VOUCHER-CHECK-NO.
           PERFORM ACCEPT-VOUCHER-CHECK-NO.

       ACCEPT-VOUCHER-CHECK-NO.
           DISPLAY "ENTER THE CHECK NUMBER".
           DISPLAY "ENTER 0 FOR CASH PAYMENT".
           ACCEPT CHECK-NO-FIELD.
           MOVE CHECK-NO-FIELD TO VOUCHER-CHECK-NO.

      *--------------------------------
      * A new voucher is generated by
      * 1. Saving the exusting voucher
      *    record.
      * 2. Locating a new voucher number
      *    that is not in use by using
      *    the control file and attempting
      *    to read a voucher with the
      *    number offered by the control
      *    file.
      * 3. Restoring the saved voucher record
      *    but using the new voucher number.
      * 4. Setting the new voucher amount
      *    to the original amount minus
      *    the amount paid.
      * 5. Resetting the paid date,
      *    paid amount and check number.
      * 6. Setting the selected flag to "N".
      * 7. Writing this new record.
      *--------------------------------
       GENERATE-NEW-VOUCHER.
           MOVE VOUCHER-RECORD TO SAVE-VOUCHER-RECORD.
           PERFORM GET-NEW-RECORD-KEY.
           PERFORM CREATE-NEW-VOUCHER-RECORD.
           PERFORM DISPLAY-NEW-VOUCHER.

       CREATE-NEW-VOUCHER-RECORD.
           MOVE SAVE-VOUCHER-RECORD TO VOUCHER-RECORD.
           MOVE CONTROL-LAST-VOUCHER TO VOUCHER-NUMBER.
           SUBTRACT VOUCHER-PAID-AMOUNT FROM VOUCHER-AMOUNT.
           MOVE "N" TO VOUCHER-SELECTED.
           PERFORM CLEAR-PAID-FIELDS.
           PERFORM WRITE-VOUCHER-RECORD.

       DISPLAY-NEW-VOUCHER.
           MOVE VOUCHER-NUMBER TO VOUCHER-NUMBER-FIELD.
           MOVE VOUCHER-AMOUNT TO AN-AMOUNT-FIELD.
           DISPLAY "VOUCHER " VOUCHER-NUMBER-FIELD
                   " CREATED FOR " AN-AMOUNT-FIELD.

      *--------------------------------
      * Standard change mode routines to 
      * get a voucher number, read the 
      * voucher record.
      *--------------------------------
       GET-NEW-RECORD-KEY.
           PERFORM ACCEPT-NEW-RECORD-KEY.
           PERFORM RE-ACCEPT-NEW-RECORD-KEY
               UNTIL VOUCHER-RECORD-FOUND = "N".



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
           DISPLAY "ENTER VOUCHER NUMBER TO PROCESS".
           ACCEPT VOUCHER-NUMBER.

      *--------------------------------
      * Standard routines to display
      * voucher fields.
      *--------------------------------
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
           PERFORM DISPLAY-VOUCHER-SELECTED.
           PERFORM DISPLAY-VOUCHER-PAID-DATE.
           PERFORM DISPLAY-VOUCHER-PAID-AMOUNT.
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

       DISPLAY-VOUCHER-INVOICE.
           DISPLAY "   INVOICE: " VOUCHER-INVOICE.

       DISPLAY-VOUCHER-FOR.
           DISPLAY "   FOR: " VOUCHER-FOR.

       DISPLAY-VOUCHER-AMOUNT.
           MOVE VOUCHER-AMOUNT TO AN-AMOUNT-FIELD.
           DISPLAY "   AMOUNT: " AN-AMOUNT-FIELD.

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
           DISPLAY "   SELECTED FOR PAYMENT: " VOUCHER-SELECTED.

       DISPLAY-VOUCHER-PAID-DATE.
           MOVE VOUCHER-PAID-DATE TO DATE-YYYYMMDD.
           PERFORM FORMAT-THE-DATE.
           DISPLAY "1. PAID ON: " FORMATTED-DATE.

       DISPLAY-VOUCHER-PAID-AMOUNT.
           MOVE VOUCHER-PAID-AMOUNT TO AN-AMOUNT-FIELD.
           DISPLAY "2. PAID: " AN-AMOUNT-FIELD.

       DISPLAY-VOUCHER-CHECK-NO.
           DISPLAY "3. CHECK: " VOUCHER-CHECK-NO.

      *--------------------------------
      * File activity Routines
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

       VOUCHER-VENDOR-ON-FILE.
           MOVE VOUCHER-VENDOR TO VENDOR-NUMBER.
           PERFORM READ-VENDOR-RECORD.
       
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

      *--------------------------------
      * General utility routines
      *--------------------------------
           COPY "pldate01.cbl".
