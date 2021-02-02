      *-----------------------------------
      * procedures copy file for Date routines.
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
           