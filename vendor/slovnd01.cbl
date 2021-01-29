      *----------------------------------------------
      * SLOVND01.CBL
      * Use under FILE-CONTROL to define vendor file.
      *----------------------------------------------
           SELECT OLD-VENDOR-FILE
               ASSIGN TO "ovendor"
               ORGANIZATION IS INDEXED
               RECORD KEY IS OLD-VENDOR-NUMBER
               ACCESS MODE IS DYNAMIC.
