        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   L      N   N  FFFFF  L       AAA    GGG    SSS    *~
            *  P   P  L      NN  N  F      L      A   A  G      S       *~
            *  PPPP   L      N N N  FFFF   L      AAAAA  G GGG   SSS    *~
            *  P      L      N  NN  F      L      A   A  G   G      S   *~
            *  P      LLLLL  N   N  F      LLLLL  A   A   GGG    SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLNFLAGS - INPUT OF USER SELECTABLE SWITCHES AND OPTIONS  *~
            *            TO CONTROL THEN ACTIONS OF PLANSUB. 3 SECTIONS *~
            *            DIVIDED INTO 1) SYSTEM WIDE DECISIONS, 2) MORE *~
            *            DETAILED DECISIONS (DT,PR, TYPE) AND 3A AND B) *~
            *            WHAT TO DO WHEN A CONSTRAINT IS HIT.           *~
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
            * 06/28/84 ! ORIGINAL                                 ! KEN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim planflags$(25)20

        dim f2%(1),                      /* = 0 IF THE FILE IS OPEN    */~
            rslt$20                      /* TEXT FROM FILE OPENING     */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.17.01 11/20/86 Order process & planning #2     "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select # 1, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            call "SHOSTAT" ("Opening File, One Moment Please")

            call "OPENCHCK" (#1, fs%, f2%(1), 0%, rslt$)
                 if fs% < 0% then L65000

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************
            init (" ") str(planflags$(),1,500)

            gosub L31000

            call "PLNFLSUB" (planflags$(), err%)

            if err% > 0% then L65000

            gosub L32000
            goto L65000

L31000: REM *************************************************************~
            *       S E T  D E F A U L T S  A N D  R E A D  D A T A     *~
            *                                                           *~
            * SETS UP, IF NO RECORD, DEFAULTS ARE SET FOR INPUT         *~
            *************************************************************

            call "READ100" (#1, "PLANNING SYSTEM FLAG", f1%)
            if f1% = 0% then L31110
            get #1, using L31090, str(planflags$(),1,480)
L31090:         FMT XX(20), CH(480)

L31110:     call "PIPINDEX" (#1, " ", err%, f1%)

            return

L32000: REM *************************************************************~
            *       W R  I T E  R E C O R D  T O  F I L E               *~
            *                                                           *~
            *                                                           *~
            *************************************************************
            if str(planflags$(),1,1) = " " then return
            call "READ101" (#1, "PLANNING SYSTEM FLAG", f1%)
            if f1% <> 0% then delete #1

            write #1, using L33050, "PLANNING SYSTEM FLAG",               ~
                    str(planflags$(),1,480)
            return

L33050:         FMT CH(20), CH(480)

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please.")

            end
