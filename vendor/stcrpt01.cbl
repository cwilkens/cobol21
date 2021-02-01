       IDENTIFICATION DIVISION.
       PROGRAM-ID. STCRPT01.
      *--------------------------------------------
      * Report on the STATE File.
      *--------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY "slstate.cbl".
      * DISPLAY instead of PRINTER for testing
           SELECT PRINTER-FILE
               ASSIGN TO DISPLAY
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

           COPY "fdstate.cbl".

       FD  PRINTER-FILE
           LABEL RECORDS ARE OMITTED.
       01  PRINTER-RECORD             PIC X(80).

       WORKING-STORAGE SECTION.

       01  DETAIL-LINE.
           05  PRINT-CODE           PIC XX.
           05  FILLER               PIC XXXX   VALUE SPACE.
           05  PRINT-NAME           PIC X(20).

       01  COLUMN-LINE.
           05  FILLER          PIC X(4) VALUE "CODE".
           05  FILLER          PIC X(2) VALUE SPACE.
           05  FILLER          PIC X(4) VALUE "NAME".

       01  TITLE-LINE.
           05  FILLER              PIC X(25) VALUE SPACE.
           05  FILLER              PIC X(11) VALUE "STATE CODES".
           05  FILLER              PIC X(15) VALUE SPACE.
           05  FILLER              PIC X(5) VALUE "PAGE:".
           05  FILLER              PIC X(1) VALUE SPACE.
           05  PRINT-PAGE-NUMBER   PIC ZZZZ9.


       77  FILE-AT-END         PIC X.
       77  LINE-COUNT          PIC 999 VALUE ZERO.
       77  PAGE-NUMBER         PIC 99999 VALUE ZERO.
       77  MAXIMUM-LINES       PIC 999 VALUE 55.

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.

           PERFORM OPENING-PROCEDURE.
           MOVE ZEROES TO LINE-COUNT
                          PAGE-NUMBER.

           PERFORM START-NEW-PAGE.

           MOVE "N" TO FILE-AT-END.
           PERFORM READ-NEXT-RECORD.
           IF FILE-AT-END = "Y"
               MOVE "NO RECORDS FOUND" TO PRINTER-RECORD
               PERFORM WRITE-TO-PRINTER
           ELSE
               PERFORM PRINT-STATE-FIELDS
                   UNTIL FILE-AT-END = "Y".

           PERFORM CLOSING-PROCEDURE.

       PROGRAM-DONE.
           STOP RUN.
           
       OPENING-PROCEDURE.
           OPEN I-O STATE-FILE.
           OPEN OUTPUT PRINTER-FILE.

       CLOSING-PROCEDURE.
           CLOSE STATE-FILE.
           PERFORM END-LAST-PAGE.
           CLOSE PRINTER-FILE.

       PRINT-STATE-FIELDS.
           IF LINE-COUNT > MAXIMUM-LINES
               PERFORM START-NEXT-PAGE.
           PERFORM PRINT-THE-RECORD.
           PERFORM READ-NEXT-RECORD.

       PRINT-THE-RECORD.
           MOVE SPACE TO DETAIL-LINE.
           MOVE STATE-CODE TO PRINT-CODE.
           MOVE STATE-NAME TO PRINT-NAME.
           MOVE DETAIL-LINE TO PRINTER-RECORD.
           PERFORM WRITE-TO-PRINTER.

       READ-NEXT-RECORD.
           READ STATE-FILE NEXT RECORD
               AT END MOVE "Y" TO FILE-AT-END.

       WRITE-TO-PRINTER.
           WRITE PRINTER-RECORD BEFORE ADVANCING 1.
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
           WRITE PRINTER-RECORD BEFORE ADVANCING PAGE.
