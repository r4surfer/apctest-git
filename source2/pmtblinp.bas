        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP   M   M  TTTTT  BBBB   L      IIIII  N   N  PPPP    *~
            *  P   P  MM MM    T    B   B  L        I    NN  N  P   P   *~
            *  PPPP   M M M    T    BBBB   L        I    N N N  PPPP    *~
            *  P      M   M    T    B   B  L        I    N  NN  P       *~
            *  P      M   M    T    BBBB   LLLLL  IIIII  N   N  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PMTBLINP - Input the Part/ Precious Metal Item association*~
            *            table data.                                    *~
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
            * 11/17/94 ! Original                                 ! RJ1 *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim f2%(64),                     /* = 0 if the file is open    */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
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
            * #03 ! HNYMASTR ! Inventory Master File                    *~
            * #06 ! GENCODES ! General Codes File                       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #03, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #06, "GENCODES",                                      ~
                        varc, indexed, recsize = 128,                    ~
                        keypos =    1,  keylen = 24

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#03, fs%(03%), f2%(03%),   0%, rslt$(03%))
            call "OPENCHCK" (#06, fs%(06%), f2%(06%),   0%, rslt$(06%))

        REM *************************************************************~
            *                M A I N   L O G I C                        *~
            *-----------------------------------------------------------*~
            * Call subroutine...                                        *~
            *************************************************************

            call "PMPTBLSB"(" ",         /* Part Number                */~
                             #3, #6, "S")
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
            call "SHOSTAT" ("One Moment Please")

            end
