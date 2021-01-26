       IDENTIFICATION DIVISION.
       PROGRAM-ID. VNDNEW02.
      *----------------------------------------------
      * Add a record to an indexed Vendor File.
      *----------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
           SELECT VENDOR-FILE
               ASSIGN TO "vendor"
               ORGANIZATION IS INDEXED
               RECORD KEY IS VENDOR-NUMBER
               ACCESS MODE IS DYNAMIC.

       DATA DIVISION.
       FILE SECTION.
       
       FD  VENDOR-FILE
           LABEL RECORDS ARE STANDARD.
       01  VENDOR-RECORD.
           05  VENDOR-NUMBER                   PIC 9(5).
           05  VENDOR-NAME                     PIC X(30).
           05  VENDOR-ADDRESS-1                PIC X(30).
           05  VENDOR-ADDRESS-2                PIC X(30).
           05  VENDOR-CITY                     PIC X(20).
           05  VENDOR-STATE                    PIC X(2).
           05  VENDOR-ZIP                      PIC X(10).
           05  VENDOR-CONTACT                  PIC X(30).
           05  VENDOR-PHONE                    PIC X(15).
           
       WORKING-STORAGE SECTION.

       01  VENDOR-NUMBER-FIELD                 PIC Z(5).

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           OPEN I-O VENDOR-FILE.
           PERFORM GET-NEW-VENDOR-NUMBER.
           PERFORM ADD-RECORDS
               UNTIL VENDOR-NUMBER = ZEROES.
           CLOSE VENDOR-FILE.

       PROGRAM-DONE.
           STOP RUN.
           
       GET-NEW-VENDOR-NUMBER.
           PERFORM INIT-VENDOR-RECORD.
           PERFORM ENTER-VENDOR-NUMBER.

       INIT-VENDOR-RECORD.
           MOVE SPACE TO VENDOR-RECORD.
           MOVE ZEROES TO VENDOR-NUMBER.

       ENTER-VENDOR-NUMBER.
           DISPLAY "ENTER VENDOR NUMBER (1-99999)".
           DISPLAY "ENTER 0 TO STOP ENTRY".
           ACCEPT VENDOR-NUMBER-FIELD.
           MOVE VENDOR-NUMBER-FIELD TO VENDOR-NUMBER.

       ADD-RECORDS.
           PERFORM ENTER-REMAINING-FIELDS.
           PERFORM WRITE-VENDOR-RECORD.
           PERFORM GET-NEW-VENDOR-NUMBER.

       WRITE-VENDOR-RECORD.
           WRITE VENDOR-RECORD
               INVALID KEY
               DISPLAY "RECORD ALREADY ON FILE".

       ENTER-REMAINING-FIELDS.
           PERFORM ENTER-VENDOR-NAME.
           PERFORM ENTER-VENDOR-ADDRESS-1.
           PERFORM ENTER-VENDOR-ADDRESS-2.
           PERFORM ENTER-VENDOR-CITY.
           PERFORM ENTER-VENDOR-STATE.
           PERFORM ENTER-VENDOR-ZIP.
           PERFORM ENTER-VENDOR-CONTACT.
           PERFORM ENTER-VENDOR-PHONE.

       ENTER-VENDOR-NAME.
           DISPLAY "ENTER VENDOR NAME".
           ACCEPT VENDOR-NAME.

       ENTER-VENDOR-ADDRESS-1.
           DISPLAY "ENTER VENDOR ADDRESS-1".
           ACCEPT VENDOR-ADDRESS-1.

       ENTER-VENDOR-ADDRESS-2.
           DISPLAY "ENTER VENDOR ADDRESS-2".
           ACCEPT VENDOR-ADDRESS-2.

       ENTER-VENDOR-CITY.
           DISPLAY "ENTER VENDOR CITY".
           ACCEPT VENDOR-CITY.

       ENTER-VENDOR-STATE.
           DISPLAY "ENTER VENDOR STATE".
           ACCEPT VENDOR-STATE.

       ENTER-VENDOR-ZIP.
           DISPLAY "ENTER VENDOR ZIP".
           ACCEPT VENDOR-ZIP.

       ENTER-VENDOR-CONTACT.
           DISPLAY "ENTER VENDOR CONTACT".
           ACCEPT VENDOR-CONTACT.

       ENTER-VENDOR-PHONE.
           DISPLAY "ENTER VENDOR PHONE".
           ACCEPT VENDOR-PHONE.
