       IDENTIFICATION DIVISION.
       PROGRAM-ID. VNDERR01.
      *---------------------------------------------------
      * Forces an error by reading past the end of a file.
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

       77  FILE-AT-END            PIC X.

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           PERFORM OPENING-PROCEDURE.
           PERFORM MAIN-PROCESS.
           PERFORM CLOSING-PROCEDURE.

       PROGRAM-DONE.
           STOP RUN.
           
       OPENING-PROCEDURE.
           OPEN I-O VENDOR-FILE.

       CLOSING-PROCEDURE.
           CLOSE VENDOR-FILE.

       MAIN-PROCESS.
           MOVE "N" TO FILE-AT-END.
           PERFORM READ-NEXT-RECORD.
           PERFORM READ-AND-DISPLAY
                   UNTIL FILE-AT-END = "Y".
           
       READ-AND-DISPLAY.
           DISPLAY VENDOR-NUMBER.
           PERFORM READ-NEXT-RECORD.

       READ-NEXT-RECORD.
           READ VENDOR-FILE NEXT RECORD.
      *         AT END MOVE "Y" TO FILE-AT-END.