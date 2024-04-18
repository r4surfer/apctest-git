        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   M   M   SSS   M   M   AAA   IIIII  N   N  PPPP    *~
            *  C   C  MM MM  S      MM MM  A   A    I    NN  N  P   P   *~
            *  C      M M M   SSS   M M M  AAAAA    I    N N N  PPPP    *~
            *  C   C  M   M      S  M   M  A   A    I    N  NN  P       *~
            *   CCC   M   M   SSS   M   M  A   A  IIIII  N   N  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CMSMACHK - Checks to see if current user is a Module      *~
            *            and/or Data Base Administrator.                *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/26/85 ! ORIGINAL                                 ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "CMSMACHK" (module$, ma$, dba$)

        dim                                                              ~
            dba$1,                       /* Bata Base Admin? (Y/N)     */~
            ma$1,                        /* Module Admin? (Y/ /N)      */~
            malist$45,                   /* Admin list for module      */~
            module$3,                    /* Module to maintain list for*/~
            p%(1),                       /* Receiver for Search        */~
            readkey$20,                  /* Read Key                   */~
            userid$3                     /* User ID of current user    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.15.08 02/14/86 VBK & VENDOR enhancements       "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

*        Don't open file if DBA check only.
            if open1% <> 0 then L10000
                call "OPENCHCK" (#1, open1%, 0%, 0%, " ")
                call "EXTRACT" addr("ID", userid$)

L10000: REM *************************************************************~
            *                M A I N   P R O G R A M                    *~
            * --------------------------------------------------------- *~
            * Check out what we've been requested to.                   *~
            *************************************************************

            ma$, dba$ = "N"
            if module$ = " " then ma$ = " "
            if module$ = " " then L10170
                readkey$ = "MODULE_ADMIN." & module$
                call "READ100" (#1, readkey$, f1%)
                if f1% = 0% then L10170
                     get #1 using L10130, malist$
L10130:                   FMT XX(20), CH(45)
                search malist$ = str(userid$) to p%() step 3
                if p%(1) <> 0% then ma$ = "Y"

L10170:     call "CMSDBADM" (dba$, " ")
            if dba$ <> "Y" then dba$ = "N"

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * Closes all the files currently open, and also displays    *~
            * a message (ONLY if in foreground) while linking to the    *~
            * next program.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            end
