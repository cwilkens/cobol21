       IDENTIFICATION DIVISION.
       PROGRAM-ID. VCHBLD01.
      *--------------------------------------------
      * Create a Voucher file for the 
      * bills payment system
      *--------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY "slvouch.cbl".

       DATA DIVISION.
       FILE SECTION.

           COPY "fdvouch.cbl".

       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           OPEN OUTPUT VOUCHER-FILE.
           CLOSE VOUCHER-FILE.

       PROGRAM-EXIT.
           EXIT PROGRAM.

       PROGRAM-DONE.
           STOP RUN.
           