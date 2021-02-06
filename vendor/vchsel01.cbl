       IDENTIFICATION DIVISION.
       PROGRAM-ID. VCHSEL01.
      *------------------------------------
      * Asks the user for a cutoff
      * date
      *
      * 1. Searches the voucher file for
      *    unpaid vouchers that are
      *    within the cut off date
      *    and reflags them as selected
      *------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY "slvouch.cbl".

       DATA DIVISION.
       FILE SECTION.

           COPY "fdvouch.cbl".

       WORKING-STORAGE SECTION.

       77  OK-TO-PROCESS               PIC X.
       77  VOUCHER-FILE-AT-END         PIC X.

       77  CUT-OFF-DATE                PIC 9(8).

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

       CLOSING-PROCEDURE.
           CLOSE VOUCHER-FILE.

       MAIN-PROCESS.
           PERFORM GET-OK-TO-PROCESS.
           IF OK-TO-PROCESS = "Y"
               PERFORM GET-CUT-OFF-DATE
               PERFORM PROCESS-VOUCHERS.

       GET-OK-TO-PROCESS.
           PERFORM ACCEPT-OK-TO-PROCESS.
           PERFORM RE-ACCEPT-OK-TO-PROCESS
               UNTIL OK-TO-PROCESS = "Y" OR "N".

       ACCEPT-OK-TO-PROCESS.
           DISPLAY "SELECT VOUCHER BY DATE RANGE (Y/N)?".
           ACCEPT OK-TO-PROCESS.
           INSPECT OK-TO-PROCESS
               CONVERTING LOWER-ALPHA
               TO         UPPER-ALPHA.


       RE-ACCEPT-OK-TO-PROCESS.
           DISPLAY "YOU MUST ENTER YES OR NO".
           PERFORM ACCEPT-OK-TO-PROCESS.

       GET-CUT-OFF-DATE.
           MOVE "N" TO ZERO-DATE-IS-OK.
           MOVE "SELECT ON OR BEFORE (MM/DD/YYYY)?"
               TO DATE-PROMPT.
           PERFORM GET-A-DATE.
           MOVE DATE-YYYYMMDD TO CUT-OFF-DATE.

      *--------------------------------
      * Select vouchers with previous dates
      *--------------------------------
       PROCESS-VOUCHERS.
           PERFORM READ-FIRST-VALID-VOUCHER.
           PERFORM PROCESS-ALL-VOUCHERS
               UNTIL VOUCHER-FILE-AT-END = "Y".

       PROCESS-ALL-VOUCHERS.
           PERFORM PROCESS-THIS-VOUCHER.
           PERFORM READ-NEXT-VALID-VOUCHER.

       PROCESS-THIS-VOUCHER.
           MOVE "Y" TO VOUCHER-SELECTED.
           PERFORM REWRITE-VOUCHER-RECORD.

      *--------------------------------
      * Read first, read next routines
      *--------------------------------
       READ-FIRST-VALID-VOUCHER.
           PERFORM READ-NEXT-VALID-VOUCHER.

       READ-NEXT-VALID-VOUCHER.
           PERFORM READ-NEXT-VOUCHER-RECORD.
           PERFORM READ-NEXT-VOUCHER-RECORD
               UNTIL VOUCHER-FILE-AT-END = "Y"
                   OR (    VOUCHER-PAID-DATE = ZEROES
                       AND VOUCHER-DUE NOT > CUT-OFF-DATE).

       READ-NEXT-VOUCHER-RECORD.
           MOVE "N" TO VOUCHER-FILE-AT-END.
           READ VOUCHER-FILE NEXT RECORD
               AT END
               MOVE "Y" TO VOUCHER-FILE-AT-END.

      *--------------------------------
      * Other File I-O routines.
      *--------------------------------
       REWRITE-VOUCHER-RECORD.
           REWRITE VOUCHER-RECORD
               INVALID KEY
               DISPLAY "ERROR REWRITING VENDOR RECORD".
      *--------------------------------
      * Utility routines.
      *--------------------------------
           COPY "pldate01.cbl".