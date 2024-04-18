        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  M   M  L       QQQ   N   N  EEEEE  X   X  TTTTT          *~
            *  MM MM  L      Q   Q  NN  N  E       X X     T            *~
            *  M M M  L      Q   Q  N N N  EEEE     X      T            *~
            *  M   M  L      Q Q Q  N  NN  E       X X     T            *~
            *  M   M  LLLLL   QQQ   N   N  EEEEE  X   X    T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * MLQNEXT  - Passes the next Multi-Line Quotation Number to *~
            *            the caller. Call this subroutine ONLY when     *~
            *            you KNOW you need a number, please.            *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/02/93 ! Original                                 ! JIM *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "MLQNEXT" (quotenbr$)

        dim                                                              ~
            quotenbr$8,                  /* Next Quotation Number      */~
            sys2key$20                   /* Key to SYSFILE2            */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! MLQMASTR ! Quotation Master file                    *~
            * #03 ! MLQCROSS ! Quotation-to-S.O. Cross Reference file   *~
            * #04 ! MLQNUMBR ! Duplicate Quotation Number Check file    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #02,  "MLQMASTR",                                     ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =   10, keylen =   8,                     ~
                        alt key  1, keypos =    1, keylen =  17

            select #03,  "MLQCROSS",                                     ~
                        varc,     indexed,  recsize =    41,             ~
                        keypos =    1, keylen =  33,                     ~
                        alt key  1, keypos =    1, keylen =   8, dup,    ~
                            key  2, keypos =    9, keylen =  25, dup,    ~
                            key  3, keypos =   18, keylen =  16, dup,    ~
                            key  4, keypos =    9, keylen =   9, dup

            select #04,  "MLQNUMBR",                                     ~
                        varc,     indexed,  recsize =    8,              ~
                        keypos =    1, keylen =   8

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            if beenherebefore% <> 0% then goto L10000
                beenherebefore% = 1%
                mat f2% = con
                sys2key$ = "SWITCHS.MLQ"
                call "OPENCHCK" (#01, u3%, f2%(1%), 0%, " ")
                xrefyorn$ = "N"
                call "READ100" (#01, sys2key$, f1%(1%))    /* SYSFILE2 */
                if f1%(1%) <> 0% then get #01 using L09140, xrefyorn$
L09140:              FMT POS(41), CH(1)
                call "OPENCHCK" (#02, u3%, f2%(2%),   0%, " ")
                call "OPENCHCK" (#04, u3%, f2%(4%), 100%, " ")
                if xrefyorn$ = "Y"                                       ~
                     then call "OPENCHCK" (#03, u3%, f2%(3%), 100%, " ")

L10000: REM *************************************************************~
            *              L E T'S   G O   T O   W O R K                *~
            *************************************************************

            call "READ101" (#01, sys2key$, f1%(1%))        /* SYSFILE2 */
*        Derive the next Quote # for the current caller.
            if f1%(1%) = 0%                                              ~
                then quotenbr% = 1001%            /* The default value */~
                else get #01 using L35170, quotenbr% /* Else actual val */
L10090
*        A series of tests for a valid, usable Quote # starts here.
            convert quotenbr% to quotenbr$, pic (00000000)
*        Toss the Quote # derived if it already exists, either in the
*        MLQMASTR, MLQNUMBR or MLQCROSS files.
            call "READ100" (#02, quotenbr$, f1%(2%))       /* MLQMASTR */
            if f1%(2%) <> 0% then goto L10210     /* Exist in MLQMASTR? */
            call "READ100" (#04, quotenbr$, f1%(4%))       /* MLQNUMBR */
            if f1%(4%) <> 0% then goto L10210     /* Exist in MLQNUMBR? */
            if xrefyorn$ <> "Y" then goto L10250/* Does MLQCROSS exist? */
                call "PLOWALTS" (#03, quotenbr$, 1%, 8%,        /* Yup */~
                     f1%(3%))                              /* MLQCROSS */
                if f1%(3%) = 0% then goto L10250  /* Exist in MLQCROSS? */
L10210
*        The derived Quote # exists somewhere ... get another one.
                     quotenbr% = quotenbr% + 1% /* Compute next number */
                     goto L10090                    /* ... & re-test it */

L10250
*        Got a Quote # for the caller (it's in QUOTENBR$). Compute the
*        NEXT Quote # & put it in SYSFILE2, then write it to MLQNUMBR.
            quotenbr% = quotenbr% + 1%      /* Compute the next number */
            if quotenbr% > 99999999% then quotenbr% = 1001% /* Default */
            if f1%(1%) = 0%                                              ~
                then put #01 using L35040, sys2key$, "nxt", date, "Y",    ~
                     quotenbr%, 30%, "Y", "N", "Y", " "   /* Create it */~
                else put #01 using L35170, quotenbr% /* Else actual val */
            if f1%(1%) = 0% then write #01 else rewrite #01/* SYSFILE2 */
            write #04, quotenbr$  /* MLQNUMBR- Not there by definition */
            goto exit_program

        REM *************************************************************~
            *       R E C O R D   F O R M A T   S T A T E M E N T       *~
            *************************************************************

L35040:     FMT          /* File #01- SYSFILE2; the SWITCHS.MLQ record */~
                CH(20), /*  1/ 20 Key "SWITCHS.MLQ"                    */~
                CH(03), /* 21/  3 User who last modified               */~
                CH(06), /* 24/  6 Date last modified                   */~
                XX(02), /* 30/  2 Filler for alignment                 */~
                CH(01), /* 32/  1 Quotation module in effect? (Y/N)    */~
                BI(04), /* 33/  4 Next Quotation Number                */~
                BI(04), /* 37/  4 Default Expiration Period in days    */~
                CH(01), /* 41/  1 Create/Maint Cross-Ref file? (Y/N)   */~
                CH(01), /* 42/  1 Edit Awarded Quotes or Warn? (Y/N/W) */~
                CH(01), /* 43/  1 Delete Quote after Cutover? (Y/N/A)  */~
                CH(457) /* 44/457 Filler                               */

L35170:     fmt/* SYSFILE2- SWITCHS.MLQ- Next Quotation Number only    */~
                pos(33), bi(04)/* Next Quotation Number                */

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
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            end
