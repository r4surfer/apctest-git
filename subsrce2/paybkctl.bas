        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP    AAA   Y   Y  BBBB   K   K   CCC   TTTTT  L       *~
            *  P   P  A   A  Y   Y  B   B  K  K   C   C    T    L       *~
            *  PPPP   AAAAA   YYY   BBBB   KKK    C        T    L       *~
            *  P      A   A    Y    B   B  K  K   C   C    T    L       *~
            *  P      A   A    Y    BBBB   K   K   CCC     T    LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PAYBKCTL - Checks for Posting in Process (maybe)          *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/01/94 ! Original                                 ! KB2 *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

            sub "PAYBKCTL" (#3, ret%)

        dim                                                              ~
            ask1$80,                     /* Ask User Message           */~
            plowkey$99                   /* Miscellaneous Read/Plow Key*/~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.01 07/28/94 CMS Patch Release R6.03.01      "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! PAYJRNTF ! PayJournal Transitnal file,from PAYUPDTE *~
            * #02 ! PAYHNYRF ! Accounts Payable Inventory Report File   *~
            * #03 ! PAYBUFFR ! Accounts Payable Invoice Buffer          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "PAYJRNTF",                                      ~
                        varc,     indexed,  recsize =  688,              ~
                        keypos =    1, keylen =  36                      ~

            select #02, "PAYHNYRF",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  61                      ~


            call "OPENCHCK" (#01, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#01, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

            call "EXTRACT" addr ("ID", str(plowkey$,,3%))

            init (hex(00)) str(plowkey$,4%)
            call "PLOWNEXT" (#1, plowkey$, 3%, f1%)
               if f1% = 0% then L10200
            ask1$ = "You have unposted payables G/L transactions"
            goto L10300

L10200:     init (hex(00)) str(plowkey$,4%)
            call "PLOWNEXT" (#2, plowkey$, 3%, f1%)
               if f1% = 0% then L10500
            ask1$ = "You have unprocessed payables/inventory records"

L10300:     ask% = 0%
            call "ASKUSER" (ask%, "* * * S O R R Y * * *", ask1$, " ",   ~
                "Press any PF Key to EXIT.")
            goto exit_program

L10500:     init (hex(00)) str(plowkey$,4%)
            call "PLOWNEXT" (#3, plowkey$, 3%, f1%)
               if f1% = 0% then exit_program

            ask% = 0%
            call "ASKUSER" (ask%, "* * * S O R R Y * * *",               ~
              "You have active records in the data entry holding file",  ~
              "Please be sure no background posting is in process",      ~
              "Press PF32 to continue, any other PF Key to EXIT")
            if ask% <> 32% then exit_program
               f1% = 0%
               goto exit_program

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            ret% = f1%
            end
