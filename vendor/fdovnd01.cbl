      *--------------------------------------------------------
      * FDOVND01.CBL
      * Primary Key - OLD-VENDOR-NUMBER
      * Original before alt path added
      * NAME, ADDRESS-1, CITY, STATE,
      *   and PHONE are required fields.
      *
      * OLD-VENDOR-STATE must be looked up and must exist in
      *  the STATE-FILE to be valid.
      * OLD-VENDOR-ADDRESS-2 not always used so may be SPACES
      * OLD-VENDOR-PHONE is usually the number for OLD-VENDOR-CONTACT
      * All fields should be entered in UPPER case.
      *--------------------------------------------------------
       FD  OLD-VENDOR-FILE
           LABEL RECORDS ARE STANDARD.
       01  OLD-VENDOR-RECORD.
           05  OLD-VENDOR-NUMBER                   PIC 9(5).
           05  OLD-VENDOR-NAME                     PIC X(30).
           05  OLD-VENDOR-ADDRESS-1                PIC X(30).
           05  OLD-VENDOR-ADDRESS-2                PIC X(30).
           05  OLD-VENDOR-CITY                     PIC X(20).
           05  OLD-VENDOR-STATE                    PIC X(2).
           05  OLD-VENDOR-ZIP                      PIC X(10).
           05  OLD-VENDOR-CONTACT                  PIC X(30).
           05  OLD-VENDOR-PHONE                    PIC X(15).
           