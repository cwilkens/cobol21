      *--------------------------------
      * slvouch.cbl
      *--------------------------------
           SELECT VOUCHER-FILE
               ASSIGN TO "voucher"
               ORGANIZATION IS INDEXED
               RECORD KEY IS VOUCHER-NUMBER
               ACCESS MODE IS DYNAMIC.