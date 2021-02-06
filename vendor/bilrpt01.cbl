       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILRPT01.
      *--------------------------------------------
      * Bills report in Due Date order
      *--------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY "slvouch.cbl".

           COPY "slvnd02.cbl".

           SELECT PRINTER-FILE
               ASSIGN TO PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

           COPY "fdvouch.cbl".

           COPY "fdvnd04.cbl".

       FD  PRINTER-FILE
           LABEL RECORDS ARE OMITTED.
       01  PRINTER-RECORD             PIC X(80).

       WORKING-STORAGE SECTION.

       77  OK-TO-PROCESS         PIC X.

           COPY "wscase01.cbl".

       01  DETAIL-LINE.
           05  PRINT-NUMBER      PIC ZZZZ9.
           05  FILLER            PIC X(3) VALUE SPACE.
           05  PRINT-NAME        PIC X(30).
           05  FILLER            PIC X(1) VALUE SPACE.
           05  PRINT-DUE-DATE    PIC Z9/99/9999.
           05  FILLER            PIC X(1) VALUE SPACE.
           05  PRINT-AMOUNT      PIC ZZZ,ZZ9.99.
           05  FILLER            PIC X(1) VALUE SPACE.
           05  PRINT-INVOICE     PIC X(15).
           05  FILLER            PIC X(1) VALUE SPACE.
           05  PRINT-SELECTED    PIC X(1) VALUE SPACE.

       01  TOTAL-THRU.
           05  FILLER            PIC X(20) VALUE SPACE.
           05  FILLER            PIC X(10) VALUE "TOTAL THRU".

       01  COLUMN-LINE.
           05  FILLER            PIC X(7)  VALUE "VOUCHER".
           05  FILLER            PIC X(1)  VALUE SPACE.
           05  FILLER            PIC X(10) VALUE "VENDOR/For".
           05  FILLER            PIC X(23) VALUE SPACE.
           05  FILLER            PIC X(8)  VALUE "DUE DATE".
           05  FILLER            PIC X(1)  VALUE SPACE.
           05  FILLER            PIC X(10) VALUE "AMOUNT DUE".
           05  FILLER            PIC X(1)  VALUE SPACE.
           05  FILLER            PIC X(7)  VALUE "INVOICE".
           05  FILLER            PIC X(9)  VALUE SPACE.
           05  FILLER            PIC X(1)  VALUE "S".

       01  TITLE-LINE.
           05  FILLER              PIC X(30) VALUE SPACE.
           05  FILLER              PIC X(12)
               VALUE "BILLS REPORT".
           05  FILLER              PIC X(19) VALUE SPACE.
           05  FILLER              PIC X(5)  VALUE "PAGE:".
           05  FILLER              PIC X(1)  VALUE SPACE.
           05  PRINT-PAGE-NUMBER   PIC ZZZ9.

       77  VOUCHER-FILE-AT-END     PIC X.
       77  VENDOR-RECORD-FOUND     PIC X.

       77  LINE-COUNT              PIC 999 VALUE ZERO.
       77  PAGE-NUMBER             PIC 9999 VALUE ZERO.
       77  MAXIMUM-LINES           PIC 999 VALUE 55.

       77  RECORD-COUNT            PIC 9999 VALUE ZEROES.

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
           OPEN I-O VENDOR-FILE.

      *     OPEN OUTPUT PRINTER-FILE.

       MAIN-PROCESS.
           PERFORM GET-OK-TO-PROCESS.
           IF OK-TO-PROCESS = "Y"
               PERFORM PRINT-THE-REPORT.

       CLOSING-PROCEDURE.
           CLOSE VENDOR-FILE.
           PERFORM END-LAST-PAGE.
      *     CLOSE PRINTER-FILE.

       GET-OK-TO-PROCESS.
           PERFORM ACCEPT-OK-TO-PROCESS.
           PERFORM RE-ACCEPT-OK-TO-PROCESS
               UNTIL OK-TO-PROCESS = "Y" OR "N".

       ACCEPT-OK-TO-PROCESS.
           DISPLAY "PRINT BILLS REPORT (Y/N)?".
           ACCEPT OK-TO-PROCESS.
           INSPECT OK-TO-PROCESS
               CONVERTING LOWER-ALPHA
               TO         UPPER-ALPHA.

       RE-ACCEPT-OK-TO-PROCESS.
           DISPLAY "YOU MUST ENTER YES OR NO".
           PERFORM ACCEPT-OK-TO-PROCESS.

       PRINT-THE-REPORT.
           OPEN INPUT VOUCHER-FILE.
           PERFORM START-ONE-REPORT.
           PERFORM PROCESS-VOUCHERS.
           PERFORM END-ONE-REPORT.
           CLOSE VOUCHER-FILE.

       START-ONE-REPORT.
           PERFORM INITIALIZE-REPORT.
           PERFORM START-NEW-PAGE.

       INITIALIZE-REPORT.
           MOVE ZEROES TO LINE-COUNT PAGE-NUMBER.

       END-ONE-REPORT.
           IF RECORD-COUNT = ZEROES
               MOVE "NO RECORDS FOUND" TO PRINTER-RECORD
               PERFORM WRITE-TO-PRINTER.

       PROCESS-VOUCHERS.
           PERFORM READ-FIRST-VALID-VOUCHER.
           PERFORM PROCESS-ALL-VOUCHERS
               UNTIL VOUCHER-FILE-AT-END = "Y".

       PROCESS-ALL-VOUCHERS.
           PERFORM PROCESS-THIS-VOUCHER.
           PERFORM READ-NEXT-VALID-VOUCHER.

       PROCESS-THIS-VOUCHER.
           ADD 1 TO RECORD-COUNT.
           IF LINE-COUNT > MAXIMUM-LINES
               PERFORM START-NEXT-PAGE.
           PERFORM PRINT-THE-RECORD.

       PRINT-THE-RECORD.
           PERFORM PRINT-LINE-1.
           PERFORM PRINT-LINE-2.
           PERFORM LINE-FEED.

       PRINT-LINE-1.
           MOVE SPACE TO DETAIL-LINE.
           MOVE VOUCHER-NUMBER TO PRINT-NUMBER.

           MOVE VOUCHER-VENDOR TO VENDOR-NUMBER.
           PERFORM READ-VENDOR-RECORD.
           IF VENDOR-RECORD-FOUND = "Y"
               MOVE VENDOR-NAME TO PRINT-NAME
           ELSE
               MOVE "*VENDOR NOT ON FILE*" TO PRINT-NAME.

           MOVE VOUCHER-DUE TO DATE-YYYYMMDD.
           PERFORM CONVERT-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO PRINT-DUE-DATE.

           MOVE VOUCHER-AMOUNT TO PRINT-AMOUNT.
           MOVE VOUCHER-INVOICE TO PRINT-INVOICE.

           IF VOUCHER-SELECTED = "Y"
               MOVE VOUCHER-SELECTED TO PRINT-SELECTED
           ELSE
               MOVE SPACE TO PRINT-SELECTED.
           
           MOVE DETAIL-LINE TO PRINTER-RECORD.
           PERFORM WRITE-TO-PRINTER.

       PRINT-LINE-2.
           MOVE SPACE TO DETAIL-LINE.
           MOVE VOUCHER-FOR TO PRINT-NAME.
           MOVE DETAIL-LINE TO PRINTER-RECORD.
           PERFORM WRITE-TO-PRINTER.

       WRITE-TO-PRINTER.
      *    WRITE PRINTER-RECORD BEFORE ADVANCING 1.
           DISPLAY PRINTER-RECORD.
           ADD 1 TO LINE-COUNT.

       LINE-FEED.
           MOVE SPACE TO PRINTER-RECORD.
           PERFORM WRITE-TO-PRINTER.

       START-NEXT-PAGE.
           PERFORM END-LAST-PAGE.
           PERFORM START-NEW-PAGE.

       START-NEW-PAGE.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO PRINT-PAGE-NUMBER.
           MOVE TITLE-LINE TO PRINTER-RECORD.
           PERFORM WRITE-TO-PRINTER.
           PERFORM LINE-FEED.
           MOVE COLUMN-LINE TO PRINTER-RECORD.
           PERFORM WRITE-TO-PRINTER.
           PERFORM LINE-FEED.

       END-LAST-PAGE.
           PERFORM FORM-FEED.
           MOVE ZERO TO LINE-COUNT.

       FORM-FEED.
           MOVE SPACE TO PRINTER-RECORD.
      *    WRITE PRINTER-RECORD BEFORE ADVANCING PAGE.
           DISPLAY PRINTER-RECORD.

      *--------------------------------
      * Read first, read next routines
      *--------------------------------
       READ-FIRST-VALID-VOUCHER.
           PERFORM READ-NEXT-VALID-VOUCHER.

       READ-NEXT-VALID-VOUCHER.
           PERFORM READ-NEXT-VOUCHER-RECORD.
           PERFORM READ-NEXT-VOUCHER-RECORD
               UNTIL VOUCHER-FILE-AT-END = "Y"
                  OR VOUCHER-PAID-DATE = ZEROES.

       READ-NEXT-VOUCHER-RECORD.
           MOVE "N" TO VOUCHER-FILE-AT-END.
           READ VOUCHER-FILE NEXT RECORD
               AT END MOVE "Y" TO VOUCHER-FILE-AT-END.

      *--------------------------------
      * Other file IO routines
      *--------------------------------
       READ-VENDOR-RECORD.
           MOVE "Y" TO VENDOR-RECORD-FOUND.
           READ VENDOR-FILE RECORD
               INVALID KEY
               MOVE "N" TO VENDOR-RECORD-FOUND.

      *--------------------------------
      * Utility routines
      *--------------------------------
           COPY "pldate01.cbl".
