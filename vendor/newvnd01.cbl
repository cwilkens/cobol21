       IDENTIFICATION DIVISION.
       PROGRAM-ID. NEWVND01.
      *------------------------------------------------
      * Create new Vendor File with Alt key from old.
      *------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY "slovnd01.cbl".

           COPY "slvnd02.cbl".
       DATA DIVISION.
       FILE SECTION.

           COPY "fdovnd01.cbl".

           COPY "fdvnd04.cbl".

       WORKING-STORAGE SECTION.

       77  OLD-VENDOR-FILE-AT-END   PIC X VALUE "N".

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           PERFORM OPENING-PROCEDURE.
           PERFORM MAIN-PROCESS.
           PERFORM CLOSING-PROCEDURE.

       PROGRAM-DONE.
           STOP RUN.

       OPENING-PROCEDURE.
           OPEN OUTPUT VENDOR-FILE.
           OPEN I-O OLD-VENDOR-FILE.

       CLOSING-PROCEDURE.
           CLOSE VENDOR-FILE.
           CLOSE OLD-VENDOR-FILE.

       MAIN-PROCESS.
           PERFORM READ-NEXT-OLD-VENDOR-RECORD.
           PERFORM PROCESS-ONE-RECORD
               UNTIL OLD-VENDOR-FILE-AT-END = "Y".

       READ-NEXT-OLD-VENDOR-RECORD.
           MOVE "N" TO OLD-VENDOR-FILE-AT-END.
           READ OLD-VENDOR-FILE NEXT RECORD
               AT END
               MOVE "Y" TO OLD-VENDOR-FILE-AT-END.

       PROCESS-ONE-RECORD.
           MOVE OLD-VENDOR-RECORD TO VENDOR-RECORD.
           PERFORM WRITE-VENDOR-RECORD.

           PERFORM READ-NEXT-OLD-VENDOR-RECORD.

       WRITE-VENDOR-RECORD.
           WRITE VENDOR-RECORD
               INVALID KEY
               DISPLAY "ERROR WRITING VENDOR RECORD".
           