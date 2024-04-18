        REM              As of 04/03/97 - Dave Dulong
        REM - (APCMKFIL) - Create Data File Utility
        REM                Opens a data file and creates it - if it doesn't 
        REM                exist.
        REM
        REM                Formulated after APCCLEAN.BAS
        REM

        dim rslt$20, file$8

        init(" ") rslt$, file$

        REM - Applicable Select Statement for File.

            file$ = "APCPCRQ"

            select #1,  "APCPCRQ",                                       ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =  1,   keylen = 20,                      ~
                        alt key  1, keypos  =     2, keylen = 19, dup

            call "OPENCHCK" (#1, x%, y%,  500%, rslt$)

         display                                                         ~
            at(01,21), "(APC) Data File Create Utility Program",         ~
            at(03,21), "          For Data File",                        ~
            at(05,32), hex(84), file$

        scan_done

            call "SHOSTAT" ("Finished!")
            stop
        end

