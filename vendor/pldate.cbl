      *-----------------------------------
      * pldate.cbl - program logic copy file
      *-----------------------------------
       GET-A-DATE.
           PERFORM ACCEPT-A-DATE.
           PERFORM RE-ACCEPT-A-DATE
               UNTIL DATE-IS-OK.

       ACCEPT-A-DATE.
           DISPLAY "ENTER A DATE (MM/DD/YYYY)".
           ACCEPT DATE-ENTRY-FIELD.

           PERFORM EDIT-CHECK-DATE.

       RE-ACCEPT-A-DATE.
           DISPLAY "INVALID DATE".
           PERFORM ACCEPT-A-DATE.
       
       EDIT-CHECK-DATE.
           PERFORM EDIT-DATE.
           PERFORM CHECK-DATE.

       EDIT-DATE.
           MOVE DATE-ENTRY-FIELD TO DATE-MMDDYYYY.
           COMPUTE DATE-YYYYMMDD =
                   DATE-MMDDYYYY * 10000.0001.

      *-----------------------------------
      * Assume that the date is good, then
      * test the date in teh following
      * steps. The routine stops if any
      * of these conditions are true,
      * and sets the valid date flag.
      * Condition 1 returns the valid date 
      * flag set to "0".
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
               MOVE "0" TO VALID-DATE-FLAG
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
                           