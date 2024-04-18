        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  W   W  RRRR    AAA   W   W   SSS   U   U  PPPP   DDDD    *~
            *  W   W  R   R  A   A  W   W  S      U   U  P   P  D   D   *~
            *  W   W  RRRR   AAAAA  W   W   SSS   U   U  PPPP   D   D   *~
            *  W W W  R   R  A   A  W W W      S  U   U  P      D   D   *~
            *   W W   R   R  A   A   W W    SSS    UUU   P      DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * WRAWSUPD - This subroutine writes serialized parts to the *~
            *            Warranty Service master file if the WSS sub-   *~
            *            module is installed.                           *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/15/91 ! Original                                 ! JIM *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "WRAWSUPD" (serialnr$, partnmbr$, invoicnr$, invcdate$,      ~
            dealcode$)

        dim                                                              ~
            dealcode$9,                  /* Dealer Code                */~
            dealnadr$(6)30,              /* Dealer Name and Address    */~
            invcdate$6,                  /* Invoice Date               */~
            invoicnr$8,                  /* Invoice Number             */~
            partnmbr$25,                 /* Part Number                */~
            serialnr$20,                 /* Serial Number              */~
            wssactve$1                   /* Is WSS installed?          */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! WTYMASTR ! Warranty System Maintenance Master file  *~
            * #03 ! CUSTOMER ! Customer Master File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #02, "WTYMASTR",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =    1, keylen =  45,                     ~
                        alt key  1, keypos =   21, keylen =  45

            select #03, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            if been_here_before% > 0% then goto L02380
                been_here_before% = 1%
                call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
                call "READ100" (#01, "SWITCHS.WRA", f1%(1))
                    if f1%(1) = 0% then exit_program
                get #01 using L02360, wssactve$
L02360:             FMT POS(21), CH(1)

L02380:     if wssactve$ <> "Y" then exit_program

            if been_here_before% > 1% then goto L09000
                been_here_before% = 2%
                call "OPENCHCK" (#02, fs%(02), f2%(02), 300%, rslt$(02))
                call "OPENCHCK" (#03, fs%(03), f2%(03),   0%, rslt$(03))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

        REM *************************************************************~
            *  S U B R O U T I N E   L O G I C   O C C U R S   H E R E  *~
            *************************************************************

            if serialnr$ = " " then exit_program
            if partnmbr$ = " " then exit_program
            call "READ100" (#02, str(serialnr$) & str(partnmbr$), f1%(2))
                if f1%(2) <> 0% then exit_program /* Already exist? */
            init (" ") dealnadr$()
            call "READ100" (#03, dealcode$, f1%(3)) /* CUSTOMER */
                if f1%(3) <> 0% then get #03 using L10110, dealnadr$()
L10110:         FMT POS(40), 6*CH(30)
            write #02 using L35040, serialnr$, partnmbr$, serialnr$,      ~
                invoicnr$, invcdate$, dealcode$, dealnadr$(), " ", 0%, " "
            goto exit_program

        REM *************************************************************~
            *          F I L E   R E C O R D   L A Y O U T S            *~
            *************************************************************

L35040:     FMT /* File #02 -- WTYMASTR record layout                  */~
                CH(20),                  /* Serial Number              */~
                CH(25),                  /* Part Number                */~
                CH(20),                  /* Serial Number again        */~
                CH(08),                  /* Invoice Number             */~
                CH(06),                  /* Invoice Date               */~
                CH(09),                  /* Dealer (CUSTOMER) code     */~
                6*CH(30),                /* Dealer Name and Address    */~
                CH(06),                  /* Date purchased             */~
                BI(02),                  /* Warranty period in months  */~
                CH(236)                  /* Filler                     */

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
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            end
