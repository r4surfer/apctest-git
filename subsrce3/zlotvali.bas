        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  L       OOO   TTTTT  V   V   AAA   L      IIIII  DDDD    *~
            *  L      O   O    T    V   V  A   A  L        I    D   D   *~
            *  L      O   O    T    V   V  AAAAA  L        I    D   D   *~
            *  L      O   O    T     V V   A   A  L        I    D   D   *~
            *  LLLLL   OOO     T      V    A   A  LLLLL  IIIII  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * LOTVALID - Validate new Lot Number passed in to ensure    *~
            *            that; A) it is a unique lot number, and        *~
            *            B) that the format is valid as per the format  *~
            *            defined by HNYFLAGS, and C) that lot numbers   *~
            *            (tracking) is allowed for the given part.      *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/07/87 ! Original                                 ! LDJ *~
            * 11/23/93 ! Added HNYLOCNS file for further test of  ! MLJ *~
            *          !  lot uniqueness.                         !     *~
            * 09/19/97 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "ZLOTVALI" (part$,           /* Part Number for Lot        */~
                        store$,          /* Store to Add Lot in        */~
                        lot$,            /* Lot to Add                 */~
                        #1,              /* SYSFILE2 UFB               */~
                        #2,              /* HNYMASTR UFB               */~
                        #3,              /* HNYQUAN  UFB               */~
                        errormsg$)       /* Returned error message.    */
                                         /* Value will be blank if Lot */
                                         /* number OK, otherwise will  */
                                         /* contain appropriate error  */
                                         /* error message text.        */
                                         /* Pass as LOT-CHECK to cause */
                                         /* part flag to be ignored    */

        dim                                                              ~
            emsg$25,                     /* Part # for Error Message   */~
            errormsg$,                   /* Returned error message.    */~
            format$16,                   /* Lot Number Format Code     */~
            formatmsg$16,                /* Lot Number Format Code     */~
            lastpart$25,                 /* Last Part NUmber Processed */~
            lot$,                        /* Lot to Add                 */~
            lottrack$1,                  /* Lot Tracking Flag          */~
            lot_sys$1,                   /* System Lot Track Flag      */~
            lot_uniq$1,                  /* Lot Uniqueness Flag        */~
            lpart$25,                    /* HNYLOCNS Part              */~
            lstore$3,                    /* HNYLOCNS Store             */~
            max$2,                       /* Max Lot Number Length      */~
            min$2,                       /* Min Lot Number Length      */~
            part$,                       /* Part Number for Lot        */~
            readkey$64,                  /* File Read Key Variable     */~
            store$,                      /* Store / Warehouse number   */~
	    tdate$8,                     /* Temp date                  */~
	    udate$8                      /* Unformated date            */

        dim f2%(04),                     /* = 0 if the file is open    */~
            f1%(04),                     /* = 1 if READ was successful */~
            fs%(04),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(04)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            select  #4, "HNYLOCNS",                                      ~
                        varc, indexed,  recsize =  700,                  ~
                        keypos = 1, keylen = 42,                         ~
                        alternate key 1, keypos = 443, keylen = 42,      ~
                                  key 2, keypos = 485, keylen = 42,      ~
                                  key 3, keypos = 527, keylen = 42,      ~
                                  key 4, keypos = 590, keylen = 42

            if fs%(1) = 0% then                                          ~
               call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))
            if fs%(4%) = 0% then                                         ~
               call "OPENCHCK" (#4,  fs%(4%), f2%(4%), 0%, rslt$(4%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            lot_check% = 0%
            if errormsg$ = "LOT-CHECK" then lot_check% = 1%

            errormsg$ = " "  :  if lot$ = " " then end
            if readkey$ > " " then L09230
                max$ = "6" : min$ = "6" : format$ = all("#")
                lot_sys$ = "N" : lot_uniq$ = "U"
                readkey$ = "SWITCHS.HNY"
                call "READ100" (#1, readkey$, f1%(1))
                if f1%(1) = 0% then L09210
                get #1 using L09200, max$, min$, format$, lot_sys$,lot_uniq$
L09200:         FMT POS(21), 2*CH(2), CH(16), POS(92), 2*CH(1)
L09210:         max% = 6% : convert max$ to max%, data goto L09220
L09220:         min% = 6% : convert min$ to min%, data goto L09221
L09221:         formatmsg$ = format$
                tran(formatmsg$,"A#N+Y%")replacing

L09230:     REM *** Retrieve Part Specific Flags From HNYMASTR ***
                if part$ = lastpart$ then L10000
                lastpart$ = part$
                lottrack$ = "N"          /* Default          */
                REM *** Check to See if we can save a read ***
                if fs(#2) <"10" and part$ <> " " and part$ = key(#2)     ~
		   then L09290
                call "READ100" (#2, lastpart$, f1%(2))
                if f1%(2) = 0% then L10000
L09290:         get #2 using L09300, lottrack$
L09300:         FMT POS(130), CH(1)

L10000: REM *************************************************************~
            *          M A I N   R O U T I N E   B E G I N S            *~
            *-----------------------------------------------------------*~
            *   First test to see if Lot number allowed/required.       *~
            *   Then test format to see if valid.  Then check Lot       *~
            *   number to see if unique or if not if same part & store. *~
            *************************************************************

            if lottrack$ = "N" and lot$ = " " then exit_routine
            if lot_check% = 1% then test_format
            if lottrack$ = "N" then L10140
            if lot$ = "?" then gosub find_lots
            if lot_sys$  = "N" then exit_routine
            if lot$ > " " then test_format
L10140:     if lottrack$ = "N" then errormsg$ =                          ~
             "Sorry, Lot Tracking is not Enabled for this Part " & part$ ~
            else errormsg$ =                                             ~
             "Sorry, a non-blank Lot Number in the format " & formatmsg$ ~
              & " is required for this part"
            goto exit_routine

        test_format
            if len(lot$) >= min% and len(lot$) <= max% then L10260
               errormsg$ = "Sorry, but a Lot number must be between " &  ~
                           min$ & " and " & max$ & " characters long."
               goto exit_routine
L10260:     for x% = 1% to len(lot$)
                if str(format$,x%,1%) = "#" then L10420
                if str(format$,x%,1%) = "+" then L10400
                if x% > 15% then L10340
                if str(format$,x%,2%) <> "%%" then L10340
		   tdate$ = date
		   call "DATEFMT" (tdate$, 0%, udate$)
                   str(lot$,x%,2%) = str(udate$,3%,2%)
                   x% = x% + 1%
                   goto L10420
L10340:         if str(format$,x%,1%) = str(lot$,x%,1%) then L10420
L10350:            errormsg$ = "Sorry, but a Lot Number MUST be in the " ~
                             & "format " & formatmsg$
                   x% = 99%
                   goto L10420
                /* Test for Digits Only */
L10400:         if str(lot$,x%,1%) < "0" or str(lot$,x%,1%) > "9" then   ~
                   L10350
L10420:     next x%
            if errormsg$ > " " then exit_routine
            if lot_uniq$ = "U" then gosub test_uniqueness
            goto exit_routine

        test_uniqueness   /* Must be unique to this part number        */
            readkey$ = lot$
            if lot_check% = 1% and part$ = " " then return
            call "PLOWALTS" (#3, readkey$, 1%, 16%, f1%(3))
            if f1%(3) = 0% then hnylocns_uniqueness
            if str(readkey$,17%,25%) = part$ then return
                errormsg$ = "Lot " & lot$ & " is already assigned" &     ~
                            " to Part " & str(readkey$,17%,25%)
                return

        hnylocns_uniqueness
            if lot_check% = 1% then return
            readkey$ = all(hex(00))
            call "REDALT4" (#4, readkey$, 4%, f1%(4%))
                if f1%(4%) = 0% then return
            get #4 using L10551, lstore$
L10551:         FMT POS(590), CH(3)
L10552:     readkey$ = str(lstore$) & lot$
            call "PLOWALTS" (#4, readkey$, 4%, 9%, f1%(4%))
                if f1%(4%) = 0% then L10562
            get #4 using L10556, lpart$
L10556:         FMT POS(599), CH(25)
            if lpart$ = part$ then return
                emsg$ = str(readkey$,10%,25%)
                errormsg$ = "Lot " & lot$ & " is already assgined to pa"&~
                            "rt " & emsg$ & " (HNYLOCNS)"
                return
L10562:     readkey$ = all(hex(ff)) : str(readkey$,1%,3%) = lstore$
            call "REDALT4" (#4, readkey$, 4%, f1%(4%))
                if f1%(4%) = 0% then return
            get #4 using L10551, lstore$
            goto L10552

        find_lots
            readkey$ = str(part$,,25%) & str(store$,,3%) & lot$
            errormsg$ = hex(06) & "Below are the LOTS under Part " &     ~
                        part$ & " and Store " & store$
            call "ZPLOWCOD" (#3, readkey$, errormsg$, 28%, 0.0, f1%(3))
            errormsg$ = " "
            if f1%(3) > 0% then lot$ = str(readkey$,29%)
            return

        exit_routine
            end
