      *---------------------------------
      * working storage copy file for Date routines.
      *---------------------------------
       77  FORMATTED-DATE     PIC Z9/99/9999.
       77  DATE-MMDDYYYY      PIC 9(8).
       77  DATE-QUOTIENT      PIC 9999.
       77  DATE-REMAINDER     PIC 9999.

       77  VALID-DATE-FLAG    PIC X.
           88  DATE-IS-INVALID  VALUE "N".
           88  DATE-IS-ZERO     VALUE "0".
           88  DATE-IS-VALID    VALUE "Y".
           88  DATE-IS-OK       VALUES "Y" "0".
       
       01  DATE-YYYYMMDD      PIC 9(8).
       01  FILLER REDEFINES DATE-YYYYMMDD.
           05  DATE-YYYY      PIC 9999.

           05  DATE-MM        PIC 99.
           05  DATE-DD        PIC 99.

      *---------------------------------
      * User can set these values before
      * performing GET-A-DATE.
      *---------------------------------
       77  DATE-PROMPT        PIC X(50) VALUE SPACE.
       77  DATE-ERROR-MESSAGE PIC X(50) VALUE SPACE.
      *---------------------------------
      * User can set this value before
      * performing GET-A-DATE or CHECK-DATE.
      *---------------------------------
       77  ZERO-DATE-IS-OK    PIC X VALUE "N".