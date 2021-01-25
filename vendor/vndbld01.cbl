       IDENTIFICATION DIVISION.
       PROGRAM-ID. VNDBLD01.
      *------------------------------------
      * Create an Empty Vendor File
      *------------------------------------
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

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           OPEN OUTPUT VENDOR-FILE.
           CLOSE VENDOR-FILE.

       PROGRAM-DONE.
           STOP RUN.
           