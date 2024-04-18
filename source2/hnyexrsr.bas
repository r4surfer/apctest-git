        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  EEEEE  X   X  RRRR    SSS   RRRR    *~
            *  H   H  NN  N  Y   Y  E       X X   R   R  S      R   R   *~
            *  HHHHH  N N N   YYY   EEEE     X    RRRR    SSS   RRRR    *~
            *  H   H  N  NN    Y    E       X X   R   R      S  R   R   *~
            *  H   H  N   N    Y    EEEEE  X   X  R   R   SSS   R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYEXRSR - Plow HNYQUAN and call HNYEXSUB.  Easy.         *~
            *            This is clearly not a standard program.        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/13/87 ! Original                                 ! KEN *~
            * 05/11/87 ! Std Cost Changes (fmt @ 19100)           ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            date$8,                      /* Date for screen display    */~
            exdate$6,                    /* Expiration Date            */~
            inpmessage$79,               /* Informational Message      */~
            line2$79,                    /* Second Line of Screen Headr*/~
            lot$16,                      /* Lot                        */~
            mode$6,                      /* GETPARTM Response          */~
            part$25,                     /* Part                       */~
            store$3,                     /* Store                      */~
            userid$3                     /* Current User Id            */~

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.00 09/08/87 Standard costs to 12            "
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
            * # 2 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * # 5 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 6 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 2, "PIPMASTR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select # 5, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select # 6, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

               rslt$( 2) = "REQUIRED"
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
               rslt$( 5) = "REQUIRED"
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, rslt$( 5))
               rslt$( 6) = "REQUIRED"
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6), 0%, rslt$( 6))

            if min(fs%()) < 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            inpmessage$  = "To Exit Program Press PF1.  To Begin Processi~
        ~ng Press PF32."

            REM Check out the mode we're in via a 'hidden' GETPARM
            mode$ = "ONLINE" /* If ran without procedure, you get this.*/
            call "GETPARM" addr ("ID", "S", "RUNMODE ", "@", "0001",     ~
                  "SCREEN", 0%, "K", "INVOICES", mode$, 6%, 5%, 32%, "U")

            if mode$ = "AUTO" then L19000
            if mode$ <> "ONLINE" then exit_program

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

L10140:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then exit_program
                      if keyhit%  = 32 then       L19000
                         goto L10140

L19000: REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

            call "SHOWMSG" ("Processing . . .")

L19060:     call "READNEXT" (#6, f1%(6))
               if f1%(6) = 0% then exit_program

            get #6, using L19100, lot$, part$, store$, qtyoh, exdate$
L19100:         FMT CH(16), CH(25), CH(3), POS(69), PD(14,4),            ~
                    POS(404), CH(6)

            call "HNYEXSUB" (lot$, store$, part$, exdate$, qtyoh)
               goto L19060

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              str(line2$,62%) = "HNYEXRSR: " & str(cms2v$,,8%)

L40170:     accept                                                       ~
               at (01,02),                                               ~
                  "Balance Expiration Details With Inventory Records.",  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (10,02), "This Program will Balance Expiration Date Inf~
        ~ormation in the Inventory Master",                               ~
               at (11,02), "With the Planning System Detail Files.",     ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Return to Menu",                          ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(32)Process",                                ~
                                                                         ~
               keys(hex(20010d0f)),                                      ~
               key (keyhit%)

               if keyhit% <> 13 then L40430
                  call "MANUAL" ("HNYEXRSR")
                  goto L40170

L40430:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40170

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
