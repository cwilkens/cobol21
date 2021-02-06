       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILRPT03.
      *--------------------------------------------
      * Bills Report with totals by day.
      *--------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY "slvouch.cbl".

           COPY "slvnd02.cbl".

           COPY "slstate.cbl".

           SELECT WORK-FILE
               ASSIGN TO "work"
               ORGANIZATION IS SEQUENTIAL.

           SELECT SORT-FILE
               ASSIGN TO "SORT".

           SELECT PRINTER-FILE
               ASSIGN TO PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

           COPY "fdvouch.cbl".

           COPY "fdvnd04.cbl".

           COPY "fdstate.cbl".

       FD  WORK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WORK-RECORD.
           05  WORK-NUMBER           PIC 9(5).
           05  WORK-VENDOR           PIC 9(5).
           05  WORK-INVOICE          PIC X(15).
           05  WORK-FOR              PIC X(30).
           05  WORK-AMOUNT           PIC S9(6)V99.
           05  WORK-DATE             PIC 9(8).
           05  WORK-DUE              PIC 9(8).
           05  WORK-DEDUCTIBLE       PIC X.
           05  WORK-SELECTED         PIC X.
           05  WORK-PAID-AMOUNT      PIC S9(6)V99.
           05  WORK-PAID-DATE        PIC 9(8).
           05  WORK-CHECK-NO         PIC 9(6).

       SD  SORT-FILE.

       01  SORT-RECORD.
           05  SORT-NUMBER           PIC 9(5).
           05  SORT-VENDOR           PIC 9(5).
           05  SORT-INVOICE          PIC X(15).
           05  SORT-FOR              PIC X(30).
           05  SORT-AMOUNT           PIC S9(6)V99.
           05  SORT-DATE             PIC 9(8).
           05  SORT-DUE              PIC 9(8).
           05  SORT-DEDUCTIBLE       PIC X.
           05  SORT-SELECTED         PIC X.
           05  SORT-PAID-AMOUNT      PIC S9(6)V99.
           05  SORT-PAID-DATE        PIC 9(8).
           05  SORT-CHECK-NO         PIC 9(6).

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

       77  WORK-FILE-AT-END        PIC X.
       77  VENDOR-RECORD-FOUND     PIC X.

       77  LINE-COUNT              PIC 999 VALUE ZERO.
       77  PAGE-NUMBER             PIC 9999 VALUE ZERO.
       77  MAXIMUM-LINES           PIC 999 VALUE 55.

       77  RECORD-COUNT            PIC 9999 VALUE ZEROES.

       77  SAVE-DUE                PIC 9(8).

       77  RUNNING-TOTAL           PIC S9(6)V99.

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
               PERFORM SORT-DATA-FILE
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

      *--------------------------------
      * Sorting logic
      *--------------------------------
       SORT-DATA-FILE.
           SORT SORT-FILE
               ON ASCENDING KEY SORT-DUE
               USING VOUCHER-FILE
               GIVING WORK-FILE.

       PRINT-THE-REPORT.
           OPEN INPUT WORK-FILE.
           PERFORM START-ONE-REPORT.
           PERFORM PROCESS-VOUCHERS.
           PERFORM END-ONE-REPORT.
           CLOSE WORK-FILE.

       START-ONE-REPORT.
           PERFORM INITIALIZE-REPORT.
           PERFORM START-NEW-PAGE.
           MOVE ZEROES TO RUNNING-TOTAL.

       INITIALIZE-REPORT.
           MOVE ZEROES TO LINE-COUNT PAGE-NUMBER.

       END-ONE-REPORT.
           IF RECORD-COUNT = ZEROES
               MOVE "NO RECORDS FOUND" TO PRINTER-RECORD
               PERFORM WRITE-TO-PRINTER.

       PROCESS-VOUCHERS.
           PERFORM READ-FIRST-VALID-WORK.
           PERFORM PROCESS-ALL-DATES
               UNTIL WORK-FILE-AT-END = "Y".

       PROCESS-ALL-DATES.
           PERFORM START-ONE-DATE.

           PERFORM PROCESS-ALL-VOUCHERS
               UNTIL WORK-FILE-AT-END = "Y"
                  OR WORK-DUE NOT = SAVE-DUE.

           PERFORM END-ONE-DATE.

       START-ONE-DATE.
           MOVE WORK-DUE TO SAVE-DUE.

       END-ONE-DATE.
           PERFORM PRINT-RUNNING-TOTAL.

       PRINT-RUNNING-TOTAL.
           MOVE SPACE TO DETAIL-LINE.
           MOVE SAVE-DUE TO DATE-YYYYMMDD.
           PERFORM CONVERT-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO PRINT-DUE-DATE.
           MOVE RUNNING-TOTAL TO PRINT-AMOUNT.
           MOVE TOTAL-THRU TO PRINT-NAME.
           MOVE DETAIL-LINE TO PRINTER-RECORD.
           PERFORM WRITE-TO-PRINTER.
           PERFORM LINE-FEED 2 TIMES.

       PROCESS-ALL-VOUCHERS.
           PERFORM PROCESS-THIS-VOUCHER.
           PERFORM READ-NEXT-VALID-WORK.

       PROCESS-THIS-VOUCHER.
           ADD 1 TO RECORD-COUNT.
           IF LINE-COUNT > MAXIMUM-LINES
               PERFORM START-NEXT-PAGE.
           PERFORM PRINT-THE-RECORD.
           ADD WORK-AMOUNT TO RUNNING-TOTAL.

       PRINT-THE-RECORD.
           PERFORM PRINT-LINE-1.
           PERFORM PRINT-LINE-2.
           PERFORM LINE-FEED.

       PRINT-LINE-1.
           MOVE SPACE TO DETAIL-LINE.
           MOVE WORK-NUMBER TO PRINT-NUMBER.

           MOVE WORK-VENDOR TO VENDOR-NUMBER.
           PERFORM READ-VENDOR-RECORD.
           IF VENDOR-RECORD-FOUND = "Y"
               MOVE VENDOR-NAME TO PRINT-NAME
           ELSE
               MOVE "*VENDOR NOT ON FILE*" TO PRINT-NAME.

           MOVE WORK-DUE TO DATE-YYYYMMDD.
           PERFORM CONVERT-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO PRINT-DUE-DATE.

           MOVE WORK-AMOUNT TO PRINT-AMOUNT.
           MOVE WORK-INVOICE TO PRINT-INVOICE.

           IF WORK-SELECTED = "Y"
               MOVE VOUCHER-SELECTED TO PRINT-SELECTED
           ELSE
               MOVE SPACE TO PRINT-SELECTED.
           
           MOVE DETAIL-LINE TO PRINTER-RECORD.
           PERFORM WRITE-TO-PRINTER.

       PRINT-LINE-2.
           MOVE SPACE TO DETAIL-LINE.
           MOVE WORK-FOR TO PRINT-NAME.
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
       READ-FIRST-VALID-WORK.
           PERFORM READ-NEXT-VALID-WORK.

       READ-NEXT-VALID-WORK.
           PERFORM READ-NEXT-WORK-RECORD.
           PERFORM READ-NEXT-WORK-RECORD
               UNTIL WORK-FILE-AT-END = "Y"
                  OR WORK-PAID-DATE = ZEROES.

       READ-NEXT-WORK-RECORD.
           MOVE "N" TO WORK-FILE-AT-END.
           READ WORK-FILE NEXT RECORD
               AT END MOVE "Y" TO WORK-FILE-AT-END.

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
