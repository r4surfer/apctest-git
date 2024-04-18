        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   CCC    OOO   RRRR    AAA   RRRR   U   U  PPPP   DDDD    *~
            *  C   C  O   O  R   R  A   A  R   R  U   U  P   P  D   D   *~
            *  C      O   O  RRRR   AAAAA  RRRR   U   U  PPPP   D   D   *~
            *  C   C  O   O  R   R  A   A  R   R  U   U  P      D   D   *~
            *   CCC    OOO   R   R  A   A  R   R   UUU   P      DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CORARUPD - Process Core AR Buffer Into Core Bank          *~
            *            Written as stand alone in case we need to sort *~
            *            or perform some such pre-processing            *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/14/92 ! Original                                 ! KAB *~
            * 10/09/92 ! Respond to Update Core from A/R Flag     ! KAB *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            delkey$50,                   /* Work Area                  */~
            record$(4)256,               /* Work Area                  */~
            readkey$50, plowkey$50,      /* Work Area                  */~
            userid$3                     /* User Id                    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! CORARITF ! Core A/R Interface File                  *~
            * #02 ! CORPARNT ! Core Parent Cross Reference              *~
            * #03 ! SYSFILE2 ! System Control File                      *~
            * #50 ! WORKFILE ! Work File                                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "CORARITF",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  24

            select #02, "CORPARNT",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   10, keylen =   9,                     ~
                        alt key  1, keypos = 1, keylen =  18

            select #03, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #50, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =   25,              ~
                        keypos =    1, keylen =  25                      ~

            exit% = 0%
            call "OPENCHCK" (#03, fs%, 0%, 0%, " ")
                 if fs% < 1% then exit_program_end
*        Check for Core Bank
            plowkey$ = "SWITCHS.COR"
            call "READ100" (#03, plowkey$, core_bank%)
               if core_bank% = 0% then exit_program_end
            get #03 using L04570, str(plowkey$,1%,1%)
L04570:         FMT POS(128), CH(1)
            if str(plowkey$,1%,1%) <> "Y" then exit_program_end

            call "OPENCHCK" (#01, fs%, 0%, 0%, " ")
                 if fs% < 1% then exit_program_end
            call "SHOSTAT" ("Updating Core Bank")

            call "OPENCHCK" (#02, fs%, 0%, 0%, " ")

            call "CORJNLSB" ("JS", -1%, 0%, 0%, 0%, " ", " ", " ", " ",  ~
                             exit%)
               if exit% = 0% then L09000
               if exit% = 99% then exit% = 0%
                  goto exit_program_end

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)

            call "WORKOPEN" (#50, "SHARE", 500%, f1%)

L10000: REM *************************************************************~
            * Pre - Processing Section                                  *~
            *-----------------------------------------------------------*~
            * Perform any sorting, etc. necessary.                      *~
            *************************************************************

            init (hex(00)) readkey$ : str(readkey$,,3%) = str(userid$,,3%)
            call "PLOWNEXT" (#1, readkey$, 3%, f1%)
               if f1% = 0% then exit_program
            delkey$ = str(readkey$,,9%)
            init (hex(00)) str(delkey$,10%)
            goto L10170

L10140:     call "PLOWNEXT" (#1, readkey$, 9%, f1%)
               if f1% = 0% then L11000

L10170:     get #1 using L10180, plowkey$, temp
L10180:         FMT POS(51), CH(9), POS(143), PD(14,4)
            if temp = 0 then L10140
            call "READ100" (#2, plowkey$, f1%)
               if f1% = 0% then parent
            if str(key(#2,1%),,9%) = str(plowkey$,,9%) then parent
*        Its a Child
            if temp > 0 then write #50 using L10260, "2", readkey$        ~
                        else write #50 using L10260, "3", readkey$
               goto L10140
L10260:           FMT CH(1), CH(24)
        parent
            if temp > 0 then write #50 using L10260, "1", readkey$        ~
                        else write #50 using L10260, "4", readkey$
               goto L10140

L11000: REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

            init (hex(00)) plowkey$

L11090:     call "PLOWNEXT" (#50, plowkey$, 0%, f1%)
               if f1% = 0% then L11370

            call "READ100" (#1, str(plowkey$,2%,24%), f1%)
               if f1% = 0% then L11090

            init(" ") record$()
            get #1 using L11250, str(record$())
L11250:         FMT POS(51), CH(900)

            call "CORARCB" ("I ", "Y", record$(), f1%)

            goto L11090

L11370:     call "DELETE" (#1, delkey$, 9%)
            init (hex(00)) delkey$
            call "DELETE" (#50, delkey$, 0%)
            goto L10000

        REM Format for ARICORTF                                          ~
            CH(3)         /*   1/3    /* User Id                       /*~
            CH(6)         /*   4/6    /* Session Id                    /*~
            CH(6)         /*  10/6    /* A/R Post Date                 /*~
            CH(1)         /*  16/1    /* A/R G/L Post Flag             /*~
            CH(1)         /*  17/1    /* Hex(00)                       /*~
            CH(1)         /*  18/7    /* Date/Time Sequence            /*~
            CH(750)       /*  25/26   /* Filler                        /*~
            CH(750)       /*  51/750  /* Invoice Line Record           /*~
            CH(38)        /* 801/38   /* PO, SO, Invoice Date          /*~
            CH(162)       /* 839/162  /* Filler                        /*

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
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "CORJNLSB" ("JE", -1%, 0%, 0%, 0%, " ", " ", " ", " ",  ~
                             f1%)
        exit_program_end
            end exit%
