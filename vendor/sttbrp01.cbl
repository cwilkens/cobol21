       IDENTIFICATION DIVISION.
       PROGRAM-ID. STTBRP01.
      *------------------------------------
      * Report on the STATE File.
      *------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
           COPY "slstate.cbl".
           
      * assign to DISPLAY instead of PRINTER for testing
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
           05  PRINT-CODE             PIC XX.
           05  FILLER                 PIC XXXX  VALUE SPACE.
           05  PRINT-NAME             PIC X(20).

       01  COLUMN-LINE.
           05  FILLER             PIC X(4) VALUE "CODE".
           05  FILLER             PIC X(2) VALUE SPACE.
           05  FILLER             PIC X(4) VALUE "NAME".

       01  TITLE-LINE.
           05  FILLER             PIC X(25) VALUE SPACE.
           05  FILLER             PIC X(11) VALUE "STATE CODES".
           05  FILLER             PIC X(15) VALUE SPACE.
           05  FILLER             PIC X(5) VALUE "PAGE:".
           05  FILLER             PIC X(1) VALUE SPACE.
           05  PRINT-PAGE-NUMBER  PIC ZZZZ9.
           
       77  FILE-AT-END            PIC X.
       77  LINE-COUNT             PIC 999 VALUE ZERO.
       77  PAGE-NUMBER            PIC 99999 VALUE ZERO.
       77  MAXIMUM-LINES          PIC 999 VALUE 55.

       01  TABLE-STATE-RECORD OCCURS 50 TIMES
            INDEXED BY STATE-INDEX.
           05  TABLE-STATE-CODE          PIC XX.
           05  TABLE-STATE-NAME          PIC X(20).
       
       PROCEDURE DIVISION.
       PROGRAM-BEGIN.

           PERFORM OPENING-PROCEDURE.
           MOVE ZEROES TO LINE-COUNT
                          PAGE-NUMBER.

           PERFORM START-NEW-PAGE.

           SET STATE-INDEX TO 1.
           PERFORM PRINT-STATE-FIELDS
               UNTIL STATE-INDEX > 50 OR
                     TABLE-STATE-RECORD(STATE-INDEX) = SPACE.

           PERFORM CLOSING-PROCEDURE.
           
       PROGRAM-DONE.
           STOP RUN.

       OPENING-PROCEDURE.
           OPEN I-O STATE-FILE.
           PERFORM LOAD-STATE-TABLE.
           CLOSE STATE-FILE.

           OPEN OUTPUT PRINTER-FILE.

       LOAD-STATE-TABLE.
           PERFORM CLEAR-TABLE.
           SET STATE-INDEX TO 1.
           PERFORM READ-NEXT-RECORD.
           PERFORM LOAD-ONE-STATE-RECORD
               UNTIL FILE-AT-END = "Y" OR 
                     STATE-INDEX > 50.

       CLEAR-TABLE.
           PERFORM CLEAR-ONE-TABLE-ROW
               VARYING STATE-INDEX FROM 1 BY 1
               UNTIL STATE-INDEX > 50.

       CLEAR-ONE-TABLE-ROW.
           MOVE SPACE TO TABLE-STATE-RECORD(STATE-INDEX).

       LOAD-ONE-STATE-RECORD.
           MOVE STATE-CODE TO TABLE-STATE-CODE(STATE-INDEX).
           MOVE STATE-NAME TO TABLE-STATE-NAME(STATE-INDEX).

           PERFORM READ-NEXT-RECORD.

           IF FILE-AT-END NOT = "Y"
               SET STATE-INDEX UP BY 1
               IF STATE-INDEX > 50
                   DISPLAY "TABLE FULL".
       
       CLOSING-PROCEDURE.
           PERFORM END-LAST-PAGE.
           CLOSE PRINTER-FILE.

       PRINT-STATE-FIELDS.
           IF LINE-COUNT > MAXIMUM-LINES
               PERFORM START-NEXT-PAGE.
           PERFORM PRINT-THE-RECORD.
           SET STATE-INDEX UP BY 1.

       PRINT-THE-RECORD.
           MOVE SPACE TO DETAIL-LINE.
           MOVE TABLE-STATE-CODE(STATE-INDEX) TO PRINT-CODE.
           MOVE TABLE-STATE-NAME(STATE-INDEX) TO PRINT-NAME.
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
