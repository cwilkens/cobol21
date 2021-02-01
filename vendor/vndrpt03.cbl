       IDENTIFICATION DIVISION.
       PROGRAM-ID. VNDRPT03.
      *--------------------------------------------
      * Report on the Vendor File. Displays to screen.
      *--------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY "slvnd01.cbl".

           COPY "slstate.cbl".

           SELECT PRINTER-FILE
               ASSIGN TO PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

           COPY "fdvnd02.cbl".

           COPY "fdstate.cbl".

       FD  PRINTER-FILE
           LABEL RECORDS ARE OMITTED.
       01  PRINTER-RECORD             PIC X(80).

       WORKING-STORAGE SECTION.

       01  DETAIL-LINE.
           05  PRINT-NUMBER      PIC 9(5).
           05  FILLER            PIC X     VALUE SPACE.
           05  PRINT-NAME        PIC X(30).
           05  FILLER            PIC X(15) VALUE SPACE.
           05  PRINT-CONTACT     PIC X(30).

       01  CITY-STATE-LINE.
           05  FILLER            PIC X(6)  VALUE SPACE.
           05  PRINT-CITY        PIC X(20).
           05  FILLER            PIC X     VALUE SPACE.
           05  PRINT-STATE       PIC X(2).
           05  FILLER            PIC X     VALUE SPACE.
           05  PRINT-STATE-NAME  PIC X(20).
           05  FILLER            PIC X(1)  VALUE SPACE.
           05  PRINT-ZIP         PIC X(10).

       01  COLUMN-LINE.
           05  FILLER      PIC X(2)  VALUE "NO".
           05  FILLER      PIC X(4)  VALUE SPACE.
           05  FILLER      PIC X(12)  VALUE "NAME-ADDRESS".
           05  FILLER      PIC X(33)  VALUE SPACE.
           05  FILLER      PIC X(17)  VALUE "CONTACT-PHONE-ZIP".

       01  TITLE-LINE.
           05  FILLER      PIC X(25)  VALUE SPACE.
           05  FILLER      PIC X(11)  VALUE "VENDOR LIST".
           05  FILLER      PIC X(19)  VALUE SPACE.
           05  FILLER      PIC X(5)   VALUE "PAGE1".
           05  FILLER      PIC X(1)   VALUE SPACE.
           05  PRINT-PAGE-NUMBER  PIC ZZZZ9.

       77  FILE-AT-END             PIC X.
       77  STATE-FILE-AT-END       PIC X VALUE "N".
       77  LINE-COUNT              PIC 999 VALUE ZERO.
       77  PAGE-NUMBER             PIC 99999 VALUE ZERO.
       77  MAXIMUM-LINES           PIC 999 VALUE 55.

       01  TABLE-STATE-RECORD OCCURS 50 TIMES
            INDEXED BY STATE-INDEX.
           05  TABLE-STATE-CODE        PIC XX.
           05  TABLE-STATE-NAME        PIC X(20).

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
               PERFORM PRINT-VENDOR-FIELDS
                   UNTIL FILE-AT-END = "Y".
           
           PERFORM CLOSING-PROCEDURE.

       PROGRAM-DONE.
           STOP RUN.

       OPENING-PROCEDURE.
           OPEN I-O VENDOR-FILE.

           OPEN I-O STATE-FILE.
           PERFORM LOAD-STATE-TABLE.
           CLOSE STATE-FILE.

      *     OPEN OUTPUT PRINTER-FILE.

       LOAD-STATE-TABLE.
           PERFORM CLEAR-TABLE.
           SET STATE-INDEX TO 1.
           PERFORM READ-NEXT-STATE-RECORD.
           PERFORM LOAD-ONE-STATE-RECORD
               UNTIL STATE-FILE-AT-END = "Y" OR
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

           PERFORM READ-NEXT-STATE-RECORD.

           IF STATE-FILE-AT-END NOT = "Y"
               SET STATE-INDEX UP BY 1
               IF STATE-INDEX > 50
                   DISPLAY "TABLE FULL".

       CLOSING-PROCEDURE.
           CLOSE VENDOR-FILE.
           PERFORM END-LAST-PAGE.
      *     CLOSE PRINTER-FILE.
       
       PRINT-VENDOR-FIELDS.
           IF LINE-COUNT > MAXIMUM-LINES
               PERFORM START-NEXT-PAGE.
           PERFORM PRINT-THE-RECORD.
           PERFORM READ-NEXT-RECORD.

       PRINT-THE-RECORD.
           PERFORM PRINT-LINE-1.
           PERFORM PRINT-LINE-2.
           PERFORM PRINT-LINE-3.
           PERFORM PRINT-LINE-4.
           PERFORM LINE-FEED.

       PRINT-LINE-1.
           MOVE SPACE TO DETAIL-LINE.
           MOVE VENDOR-NUMBER TO PRINT-NUMBER.
           MOVE VENDOR-NAME TO PRINT-NAME.
           MOVE VENDOR-CONTACT TO PRINT-CONTACT.
           MOVE DETAIL-LINE TO PRINTER-RECORD.
           PERFORM WRITE-TO-PRINTER.

       PRINT-LINE-2.
           MOVE SPACE TO DETAIL-LINE.
           MOVE VENDOR-ADDRESS-1 TO PRINT-NAME.
           MOVE VENDOR-PHONE TO PRINT-CONTACT.
           MOVE DETAIL-LINE TO PRINTER-RECORD.
           PERFORM WRITE-TO-PRINTER.

       PRINT-LINE-3.
           MOVE SPACE TO DETAIL-LINE.
           MOVE VENDOR-ADDRESS-2 TO PRINT-NAME.
           IF VENDOR-ADDRESS-2 NOT = SPACE
               MOVE DETAIL-LINE TO PRINTER-RECORD
               PERFORM WRITE-TO-PRINTER.
       
       PRINT-LINE-4.
           MOVE SPACE TO CITY-STATE-LINE.
           MOVE VENDOR-CITY TO PRINT-CITY.
           MOVE VENDOR-STATE TO PRINT-STATE.

           PERFORM LOOK-UP-STATE-CODE.

           MOVE VENDOR-ZIP TO PRINT-ZIP.
           MOVE CITY-STATE-LINE TO PRINTER-RECORD.
           PERFORM WRITE-TO-PRINTER.

       LOOK-UP-STATE-CODE.
           SET STATE-INDEX TO 1.
           SEARCH TABLE-STATE-RECORD
               AT END
                MOVE "***Not Found***" TO PRINT-STATE-NAME
               WHEN VENDOR-STATE = TABLE-STATE-CODE(STATE-INDEX)
                MOVE TABLE-STATE-NAME(STATE-INDEX)
                  TO PRINT-STATE-NAME.
           
           IF STATE-NAME = SPACE
               MOVE "*State is Blank*" TO PRINT-STATE-NAME.

       READ-NEXT-RECORD.
           READ VENDOR-FILE NEXT RECORD
               AT END MOVE "Y" TO FILE-AT-END.
       
       WRITE-TO-PRINTER.
      *     WRITE PRINTER-RECORD BEFORE ADVANCING 1.
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
      *     WRITE PRINTER-RECORD BEFORE ADVANCING PAGE.
           DISPLAY PRINTER-RECORD.

       READ-NEXT-STATE-RECORD.
           MOVE "N" TO STATE-FILE-AT-END.
           READ STATE-FILE NEXT RECORD
               AT END
               MOVE "Y" TO STATE-FILE-AT-END.