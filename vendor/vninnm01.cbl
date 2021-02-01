       IDENTIFICATION DIVISION.
       PROGRAM-ID. VNINNM01.
      *------------------------------------
      * Inquire for the Vendor File
      * using vendor name.
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

       77  VENDOR-FILE-AT-END          PIC X.
       77  STATE-RECORD-FOUND          PIC X.


       77  VENDOR-NAME-FIELD           PIC X(30).
           
           COPY "wscase01.cbl".

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           PERFORM OPENING-PROCEDURE.
           PERFORM MAIN-PROCESS.
           PERFORM CLOSING-PROCEDURE.
           
       PROGRAM-DONE.
           STOP RUN.

       OPENING-PROCEDURE.
           OPEN I-O VENDOR-FILE.
           OPEN I-O STATE-FILE.
       
       CLOSING-PROCEDURE.
           CLOSE VENDOR-FILE.
           CLOSE STATE-FILE.

       MAIN-PROCESS.
           PERFORM INQUIRE-BY-NAME.
      *--------------------------------
      * INQUIRE
      *--------------------------------
       INQUIRE-BY-NAME.
           PERFORM GET-EXISTING-RECORD.
           PERFORM INQUIRE-RECORDS
               UNTIL VENDOR-NAME = SPACES.

       INQUIRE-RECORDS.
           PERFORM DISPLAY-ALL-FIELDS.
           PERFORM GET-EXISTING-RECORD.

      *--------------------------------
      * Locate a record logic
      *--------------------------------
       GET-EXISTING-RECORD.
           PERFORM ACCEPT-EXISTING-KEY.
           PERFORM RE-ACCEPT-EXISTING-KEY
               UNTIL VENDOR-FILE-AT-END NOT = "Y".
       
       ACCEPT-EXISTING-KEY.
           PERFORM INIT-FOR-KEY-ENTRY.
           PERFORM ENTER-VENDOR-NAME.
           IF VENDOR-NAME NOT = SPACES
               PERFORM READ-FIRST-VENDOR-RECORD.

       RE-ACCEPT-EXISTING-KEY.
           DISPLAY "RECORD NOT FOUND".
           PERFORM ACCEPT-EXISTING-KEY.

      *--------------------------------
      * Field Entry logic
      *--------------------------------
       ENTER-VENDOR-NAME.
           PERFORM ACCEPT-VENDOR-NAME.

       ACCEPT-VENDOR-NAME.
           DISPLAY "ENTER VENDOR NAME".
           ACCEPT VENDOR-NAME.
           INSPECT VENDOR-NAME
               CONVERTING LOWER-ALPHA
               TO         UPPER-ALPHA.

      *--------------------------------
      * Display logic
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
           MOVE VENDOR-STATE TO STATE-CODE.
           PERFORM READ-STATE-RECORD.
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
      * File Related Routines
      *--------------------------------
       INIT-FOR-KEY-ENTRY.
           MOVE SPACE TO VENDOR-RECORD.
           MOVE ZEROES TO VENDOR-NUMBER.
           MOVE "N" TO VENDOR-FILE-AT-END.

       READ-FIRST-VENDOR-RECORD.
           MOVE "N" TO VENDOR-FILE-AT-END.
           START VENDOR-FILE
               KEY NOT < VENDOR-NAME
               INVALID KEY
               MOVE "Y" TO VENDOR-FILE-AT-END.
           
           IF VENDOR-FILE-AT-END NOT = "Y"
               PERFORM READ-NEXT-VENDOR-RECORD.
       
       READ-NEXT-VENDOR-RECORD.
           READ VENDOR-FILE NEXT RECORD
               AT END
               MOVE "Y" TO VENDOR-FILE-AT-END.
               
       READ-STATE-RECORD.
           MOVE "Y" TO STATE-RECORD-FOUND.
           READ STATE-FILE RECORD
               INVALID KEY
               MOVE "N" TO STATE-RECORD-FOUND.
               