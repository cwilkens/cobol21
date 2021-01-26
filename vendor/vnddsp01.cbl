       IDENTIFICATION DIVISION.
       PROGRAM-ID. VNDDSP01.
      *---------------------------------------------------
      * Display vendor number and name in order.
      *---------------------------------------------------
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



       01  DETAIL-LINE.
           05  DISPLAY-NUMBER      PIC 9(5).
           05  FILLER              PIC X     VALUE SPACE.
           05  DISPLAY-NAME        PIC X(30).
           05  FILLER              PIC X     VALUE SPACE.
           05  DISPLAY-CONTACT     PIC X(30).

       01  CITY-STATE-DETAIL.
           05  DISPLAY-CITY        PIC X(20).
           05  FILLER              PIC X VALUE SPACE.
           05  DISPLAY-STATE       PIC X(2).

       01  COLUMN-LINE.
           05  FILLER         PIC X(2)  VALUE "NO".
           05  FILLER         PIC X(4)  VALUE SPACE.
           05  FILLER         PIC X(12) VALUE "NAME-ADDRESS".
           05  FILLER         PIC X(19) VALUE SPACE.
           05  FILLER         PIC X(17) VALUE "CONTACT-PHONE-ZIP".

       01  TITLE-LINE.
           05  FILLER              PIC X(15) VALUE SPACE.
           05  FILLER              PIC X(11)
               VALUE "VENDOR LIST".
           05  FILLER              PIC X(15) VALUE SPACE.
           05  FILLER              PIC X(5) VALUE "PAGE:".
           05  FILLER              PIC X(1) VALUE SPACE.
           05  DISPLAY-PAGE-NUMBER PIC ZZZZ9.

       77  FILE-AT-END             PIC X.
       77  A-DUMMY                 PIC X.
       77  LINE-COUNT              PIC 999 VALUE ZERO.
       77  PAGE-NUMBER             PIC 99999 VALUE ZERO.
       77  MAXIMUM-LINES           PIC 999 VALUE 15.

       77  DISPLAY-RECORD          PIC X(79).

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           
           PERFORM OPENING-PROCEDURE.
           MOVE ZEROES TO LINE-COUNT
                          PAGE-NUMBER.

           PERFORM START-NEW-PAGE.
           
           MOVE "N" TO FILE-AT-END.
           PERFORM READ-NEXT-RECORD.
           IF FILE-AT-END = "Y"
               MOVE "NO RECORDS FOUND" TO DISPLAY-RECORD
               PERFORM WRITE-DISPLAY-RECORD
           ELSE
               PERFORM DISPLAY-VENDOR-FIELDS
                   UNTIL FILE-AT-END = "Y".
           
           PERFORM CLOSING-PROCEDURE.


       PROGRAM-DONE.
           STOP RUN.
           
       OPENING-PROCEDURE.
           OPEN I-O VENDOR-FILE.

       CLOSING-PROCEDURE.
           CLOSE VENDOR-FILE.

       DISPLAY-VENDOR-FIELDS.
           IF LINE-COUNT > MAXIMUM-LINES
               PERFORM START-NEXT-PAGE.
           PERFORM DISPLAY-THE-RECORD.
           PERFORM READ-NEXT-RECORD.

       DISPLAY-THE-RECORD.
           PERFORM DISPLAY-LINE-1.
           PERFORM DISPLAY-LINE-2.
           PERFORM DISPLAY-LINE-3.
           PERFORM DISPLAY-LINE-4.
           PERFORM LINE-FEED.

       DISPLAY-LINE-1.
           MOVE SPACE TO DETAIL-LINE.
           MOVE VENDOR-NUMBER TO DISPLAY-NUMBER.
           MOVE VENDOR-NAME TO DISPLAY-NAME.
           MOVE VENDOR-CONTACT TO DISPLAY-CONTACT.
           MOVE DETAIL-LINE TO DISPLAY-RECORD.
           PERFORM WRITE-DISPLAY-RECORD.

       DISPLAY-LINE-2.
           MOVE SPACE TO DETAIL-LINE.
           MOVE VENDOR-ADDRESS-1 TO DISPLAY-NAME.
           MOVE VENDOR-PHONE TO DISPLAY-CONTACT.
           MOVE DETAIL-LINE TO DISPLAY-RECORD.
           PERFORM WRITE-DISPLAY-RECORD.

       DISPLAY-LINE-3.
           MOVE SPACE TO DETAIL-LINE.
           MOVE VENDOR-ADDRESS-2 TO DISPLAY-NAME.
           IF VENDOR-ADDRESS-2 NOT = SPACE
               MOVE DETAIL-LINE TO DISPLAY-RECORD
               PERFORM WRITE-DISPLAY-RECORD.
       
       DISPLAY-LINE-4.
           MOVE SPACE TO DETAIL-LINE.
           MOVE VENDOR-CITY TO DISPLAY-CITY.
           MOVE VENDOR-STATE TO DISPLAY-STATE.
           MOVE CITY-STATE-DETAIL TO DISPLAY-NAME.
           MOVE VENDOR-ZIP TO DISPLAY-CONTACT.
           MOVE DETAIL-LINE TO DISPLAY-RECORD.
           PERFORM WRITE-DISPLAY-RECORD.
           
       READ-NEXT-RECORD.
           READ VENDOR-FILE NEXT RECORD
               AT END MOVE "Y" TO FILE-AT-END.
       
       WRITE-DISPLAY-RECORD.
           DISPLAY DISPLAY-RECORD.
           ADD 1 TO LINE-COUNT.

       LINE-FEED.
           MOVE SPACE TO DISPLAY-RECORD.
           PERFORM WRITE-DISPLAY-RECORD.

       START-NEXT-PAGE.

           PERFORM END-LAST-PAGE.
           PERFORM START-NEW-PAGE.

       START-NEW-PAGE.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO DISPLAY-PAGE-NUMBER.
           MOVE TITLE-LINE TO DISPLAY-RECORD.
           PERFORM WRITE-DISPLAY-RECORD.
           PERFORM LINE-FEED.
           MOVE COLUMN-LINE TO DISPLAY-RECORD.
           PERFORM WRITE-DISPLAY-RECORD.
           PERFORM LINE-FEED.

       END-LAST-PAGE.
           PERFORM PRESS-ENTER.
           MOVE ZERO TO LINE-COUNT.

       PRESS-ENTER.
           DISPLAY "PRESS ENTER TO CONTINUE. . .".
           ACCEPT A-DUMMY.