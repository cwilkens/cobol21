      *----------------------------------------------
      * SLVND01.CBL
      * Use under FILE-CONTROL to define vendor file.
      *----------------------------------------------
           SELECT VENDOR-FILE
               ASSIGN TO "vendor"
               ORGANIZATION IS INDEXED
               RECORD KEY IS VENDOR-NUMBER
               ALTERNATE KEY
                   IS VENDOR-NAME WITH DUPLICATES
               ACCESS MODE IS DYNAMIC.
