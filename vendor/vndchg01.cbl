       IDENTIFICATION DIVISION.
       PROGRAM-ID. VNDCHG01.
      *------------------------------------
      * Change records in Vendor File.
      *------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
           COPY "slvnd01.cbl".

       DATA DIVISION.
       FILE SECTION.

           COPY "fdvnd02.cbl".
           
       WORKING-STORAGE SECTION.

       77  WHICH-FIELD         PIC 9.
       77  RECORD-FOUND        PIC X.
       77  VENDOR-NUMBER-FIELD PIC Z(5).

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           OPEN I-O VENDOR-FILE.
           PERFORM GET-VENDOR-RECORD.
           PERFORM CHANGE-RECORDS
               UNTIL VENDOR-NUMBER = ZEROES.
           CLOSE VENDOR-FILE.

       PROGRAM-DONE.
           STOP RUN.
           
      *--------------------------------
      * TO GET A VENDOR RECORD, ASK FOR
      * VENDOR NUMBER, AND THEN TRY TO 
      * READ THE RECORD.
      *--------------------------------
       GET-VENDOR-RECORD.
           PERFORM INIT-VENDOR-RECORD.
           PERFORM ENTER-VENDOR-NUMBER.
           MOVE "N" TO RECORD-FOUND.
           PERFORM FIND-VENDOR-RECORD
               UNTIL RECORD-FOUND = "Y" OR
                     VENDOR-NUMBER = ZEROES.
       
       INIT-VENDOR-RECORD.
           MOVE SPACE TO VENDOR-RECORD.
           MOVE ZEROES TO VENDOR-NUMBER.

       ENTER-VENDOR-NUMBER.
           DISPLAY " ".
           DISPLAY "ENTER VENDOR NUMBER OF THE VENDOR".
           DISPLAY "TO CHANGE (1-99999)".
           DISPLAY "ENTER 0 TO STOP ENTRY".
           ACCEPT VENDOR-NUMBER-FIELD.
           MOVE VENDOR-NUMBER-FIELD TO VENDOR-NUMBER.

       FIND-VENDOR-RECORD.
           PERFORM READ-VENDOR-RECORD.
           IF RECORD-FOUND = "N"
               DISPLAY "RECORD NOT FOUND"
               PERFORM ENTER-VENDOR-NUMBER.

       READ-VENDOR-RECORD.
           MOVE "Y" TO RECORD-FOUND.
           READ VENDOR-FILE RECORD
             INVALID KEY
               MOVE "N" TO RECORD-FOUND.
       
       CHANGE-RECORDS.
           PERFORM GET-FIELD-TO-CHANGE.
           PERFORM CHANGE-ONE-FIELD
               UNTIL WHICH-FIELD = ZERO
           PERFORM GET-VENDOR-RECORD.

      *--------------------------------
      * DISPLAY ALL FIELDS, ASK THE USER
      * WHICH TO CHANGE
      *--------------------------------
       GET-FIELD-TO-CHANGE.
           PERFORM DISPLAY-ALL-FIELDS.
           PERFORM ASK-WHICH-FIELD.
       
      *--------------------------------
      * DISPLAY ALL FIELDS WITH BLANK
      * LINES ABOVE AND BELOW
      *--------------------------------
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

       ASK-WHICH-FIELD.
           DISPLAY "ENTER THE NUMBER OF THE FIELD".
           DISPLAY "TO CHANGE (1-8) OR 0 TO EXIT".
           ACCEPT WHICH-FIELD.
           IF WHICH-FIELD > 8
               DISPLAY "INVALID ENTRY".
       
      *--------------------------------
      * GET DATA ENTRY FOR THE NEW FIELD.
      * THEN ASK FOR THE NEXT FIELD
      * TO CHANGE.
      * CONTINUE UNTIL USER ENTERS 0
      *--------------------------------
       CHANGE-ONE-FIELD.
           PERFORM CHANGE-THIS-FIELD.
           PERFORM GET-FIELD-TO-CHANGE.

      *--------------------------------
      * ACCEPT DATA ENTRY FOR THE FIELD
      * SPECIFIED, AND THEN REWRITE
      * RECORD.
      *--------------------------------
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

       REWRITE-VENDOR-RECORD.
           REWRITE VENDOR-RECORD
               INVALID KEY
               DISPLAY "ERROR REWRITING VENDOR RECORD".
