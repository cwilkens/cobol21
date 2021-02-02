       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATE05.
      *--------------------------------------------
      * Testing Date Entry and handling
      *--------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       77  ANY-DATE           PIC 9(8) VALUE ZEROES.
       77  REQUIRED-DATE      PIC 9(8) VALUE ZEROES.

      *---------------------------------
      * Fields for date routines.
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

       CLOSING-PROCEDURE.

       MAIN-PROCESS.
           PERFORM GET-TWO-DATES.
           PERFORM DISPLAY-AND-GET-DATES
               UNTIL REQUIRED-DATE = 00010101.

       GET-TWO-DATES.
           PERFORM GET-ANY-DATE.
           PERFORM GET-REQUIRED-DATE.

       GET-ANY-DATE.
           MOVE "Y" TO ZERO-DATE-IS-OK.
           MOVE "ENTER AN OPTIONAL MM/DD/YYYY?" TO DATE-PROMPT.
           MOVE "MUST BE ANY VALID DATE" TO DATE-ERROR-MESSAGE.
           PERFORM GET-A-DATE.
           MOVE DATE-YYYYMMDD TO ANY-DATE.

       GET-REQUIRED-DATE.
           MOVE "N" TO ZERO-DATE-IS-OK.
           MOVE SPACE TO DATE-PROMPT.
           MOVE "MUST ENTER A VALID DATE" TO DATE-ERROR-MESSAGE.
           PERFORM GET-A-DATE.
           MOVE DATE-YYYYMMDD TO REQUIRED-DATE.
           
       DISPLAY-AND-GET-DATES.
           PERFORM DISPLAY-THE-DATES.
           PERFORM GET-TWO-DATES.

       DISPLAY-THE-DATES.
           MOVE ANY-DATE TO DATE-YYYYMMDD.
           PERFORM FORMAT-THE-DATE.
           DISPLAY "ANY DATE IS " FORMATTED-DATE.
           MOVE REQUIRED-DATE TO DATE-YYYYMMDD.
           PERFORM FORMAT-THE-DATE.
           DISPLAY "REQUIRED DATE IS " FORMATTED-DATE.

      *-----------------------------------
      * USAGE:
      *  MOVE "Y" (OR "N") TO ZERO-DATE-IS-OK. (optional)
      *  MOVE prompt TO DATE-PROMPT            (optional)
      *  MOVE message TO DATE-ERROR-MESSAGE    (optional)
      *  PERFORM GET-A-DATE
      * RETURNS:
      *  DATE-IS-OK (ZERO OR VALID)
      *  DATE-IS-VALID (VALID)
      *  DATE-IS-INVALID (BAD DATE)
      * 
      *  IF DATE IS VALID IT IS IN
      *     DATE-YYYYMMDD AND
      *     DATE-MMDDYYYY AND
      *     FORMATTED-DATE (formatted)
      *-----------------------------------
       GET-A-DATE.
           PERFORM ACCEPT-A-DATE.
           PERFORM RE-ACCEPT-A-DATE
               UNTIL DATE-IS-OK.

       ACCEPT-A-DATE.
           IF DATE-PROMPT = SPACE
               DISPLAY "ENTER A DATE (MM/DD/YYYY)"
           ELSE
               DISPLAY DATE-PROMPT.

           ACCEPT FORMATTED-DATE.

           PERFORM EDIT-CHECK-DATE.

       RE-ACCEPT-A-DATE.
           IF DATE-ERROR-MESSAGE = SPACE
               DISPLAY "INVALID DATE"
           ELSE
               DISPLAY DATE-ERROR-MESSAGE.

           PERFORM ACCEPT-A-DATE.
       
       EDIT-CHECK-DATE.
           PERFORM EDIT-DATE.
           PERFORM CHECK-DATE.
           MOVE DATE-MMDDYYYY TO FORMATTED-DATE.

       EDIT-DATE.
           MOVE FORMATTED-DATE TO DATE-MMDDYYYY.
           PERFORM CONVERT-TO-YYYYMMDD.

      *-----------------------------------
      * USAGE:
      *  MOVE date(YYYYMMDD) TO DATE-YYYYMMDD.
      *  PERFORM CONVERT-TO-MMDDYYYY.
      * 
      * RETURNS:
      *  DATE-MMDDYYYY.
      *-----------------------------------
       CONVERT-TO-MMDDYYYY.
           COMPUTE DATE-MMDDYYYY = 
                   DATE-YYYYMMDD * 10000.0001.

      *-----------------------------------
      * USAGE:
      *  MOVE date(MMDDYYYY) TO DATE-MMDDYYYY.
      *  PERFORM CONVERT-TO-YYYYMMDD.
      * 
      * RETURNS:
      *  DATE-YYYYMMDD.
      *-----------------------------------
       CONVERT-TO-YYYYMMDD.
           COMPUTE DATE-YYYYMMDD =
                   DATE-MMDDYYYY * 10000.0001.

      *-----------------------------------
      * USAGE:
      *  MOVE date(YYYYMMDD) TO DATE-YYYYMMDD.
      *  MOVE "Y" (OR "N") TO ZERO-DATE-IS-OK.
      *  PERFORM CHECK-DATE.
      * 
      * RETURNS:
      *  DATE-IS-OK      (ZERO OR VALID)
      *  DATE-IS-VALID   (VALID)
      *  DATE-IS-INVALID (BAD DATE)
      * 
      * Assume that the date is good, then
      * test the date in the following
      * steps. The routine stops if any
      * of these conditions are true,
      * and sets the valid date flag.
      * Condition 1 returns the valid date 
      * flag set to "0" if ZERO-DATE-IS-OK
      * is "Y", otherwise it sets the
      * valid date flag to "N".
      * If any other condition is true,
      * the valid date flag is set to "N".
      * 1.  Is the date zeroes
      * 2.  Month > 12 or < 1
      * 3.  Day < 1 or > 31
      * 4.  Day > 30 and
      *     Month = 2 (February)  or
      *             4 (April)     or
      *             6 (June)      or
      *             9 (September) or
      *            11 (November)
      *     Day > 29 and
      *     Month = 2 (February)
      * 5.  Day = 29 and Month = 2 and
      *     Not a leap year
      * ( A leap year is any year evenly
      *   divisible by 4, but does not
      *   end in 00 and that is 
      *   not evenly divisible by 400).
      *-----------------------------------
       CHECK-DATE.
           MOVE "Y" TO VALID-DATE-FLAG.
           IF DATE-YYYYMMDD = ZEROES
               IF ZERO-DATE-IS-OK = "Y"
                   MOVE "0" TO VALID-DATE-FLAG
               ELSE
                   MOVE "N" TO VALID-DATE-FLAG
           ELSE
           IF DATE-MM < 1 OR DATE-MM > 12
               MOVE "N" TO VALID-DATE-FLAG
           ELSE
           IF DATE-DD < 1 OR DATE-DD > 31
               MOVE "N" TO VALID-DATE-FLAG
           ELSE
           IF (DATE-DD > 30) AND
              (DATE-MM = 2 OR 4 OR 6 OR 9 OR 11)
               MOVE "N" TO VALID-DATE-FLAG
           ELSE
           IF DATE-DD > 29 AND DATE-MM = 2
               MOVE "N" TO VALID-DATE-FLAG
           ELSE
           IF DATE-DD = 29 AND DATE-MM = 2
               DIVIDE DATE-YYYY BY 400 GIVING DATE-QUOTIENT
                      REMAINDER DATE-REMAINDER
               IF DATE-REMAINDER = 0
                   MOVE "Y" TO VALID-DATE-FLAG
               ELSE
                   DIVIDE DATE-YYYY BY 100 GIVING DATE-QUOTIENT
                          REMAINDER DATE-REMAINDER
                   IF DATE-REMAINDER = 0
                       MOVE "N" TO VALID-DATE-FLAG
                   ELSE
                       DIVIDE DATE-YYYY BY 4 GIVING DATE-QUOTIENT
                              REMAINDER DATE-REMAINDER
                       IF DATE-REMAINDER = 0
                           MOVE "Y" TO VALID-DATE-FLAG
                       ELSE
                           MOVE "N" TO VALID-DATE-FLAG.
      *---------------------------------
      * USAGE:
      *  MOVE date(YYYYMMDD) TO DATE-YYYYMMDD.
      *  PERFORM FORMAT-THE-DATE.
      * 
      * RETURNS:
      *  FORMATTED-DATE
      *  DATE-MMDDYYYY.
      *-----------------------------------
       FORMAT-THE-DATE.
           PERFORM CONVERT-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO FORMATTED-DATE.
           