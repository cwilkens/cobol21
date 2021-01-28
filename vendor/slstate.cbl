      *--------------------------
      * SLSTATE.CBL
      *--------------------------
           SELECT STATE-FILE
               ASSIGN TO "state"
               ORGANIZATION IS INDEXED
               RECORD KEY IS STATE-CODE
               ACCESS MODE IS DYNAMIC.
               