      *----------------------------------------------
      * SLVND01.CBL
      * Use under FILE-CONTROL to define vendor file.
      *----------------------------------------------
           SELECT VENDOR-FILE
               ASSIGN TO "vendor"
               ORGANIZATION IS INDEXED
               RECORD KEY IS VENDOR-NUMBER
               ACCESS MODE IS DYNAMIC.
