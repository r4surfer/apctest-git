        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS   EEEEE  RRRR   V   V   AAA   L      IIIII  DDDD    *~
            *  S      E      R   R  V   V  A   A  L        I    D   D   *~
            *   SSS   EEEE   RRRR   V   V  AAAAA  L        I    D   D   *~
            *      S  E      R  R    V V   A   A  L        I    D   D   *~
            *   SSS   EEEEE  R   R    V    A   A  LLLLL  IIIII  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SERVALID - Validate new Serial Numbers passed in to ensure*~
            *            that; A) it is a unique Serial number, and     *~
            *            B) that the format is valid as per the format  *~
            *            defined by HNYFLAGS, and C) that Serial numbers*~
            *            (tracking) is allowed for the given part.      *~
            *            If the Quantity is > 1 then the assumption is  *~
            *            that a range of S/N's are to be assigned.      *~
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
            * 03/26/87 ! Added support for 'IP' Transaction Type  ! LDJ *~
            *          ! (Direct Inventory Maintenance - HNYQIPUT)!     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "SERVALID" (part$,           /* Part Number                */~
                        store$,          /* Store Number               */~
                        lot$,            /* Lot                        */~
                        serial$,         /* Serial Number to Add       */~
                        qty,             /* Qty to Add                 */~
                        index%,          /* Item to assign S/N's to    */~
                        l%,              /* Current Subscript in SER$()*/~
                        #1,              /* SYSFILE2 UFB               */~
                        #2,              /* HNYMASTR UFB               */~
                        #3,              /* SERMASTR UFB               */~
                        ser$(),          /* Work List of S/N's         */~
                                         /* assigned to passed in index*/~
                        mode$,           /* Add or Change Mode (A/C)   */~
                        trantype$,       /* Source Transaction Type.   */~
                        trankey$,        /* Source Transaction Key.    */~
                        errormsg$)       /* Returned error message.    */
                                         /* Value will be blank if Lot */
                                         /* number OK, otherwise will  */
                                         /* contain appropriate error  */
                                         /* error message text.        */

        dim                                                              ~
            auto$1,                      /* Auto Assignment of S/N's ? */~
            datetime$7,                  /* Date Time Stamp            */~
            errormsg$,                   /* Returned error message.    */~
            format$20,                   /* Serial Number Format Code  */~
            formatmsg$20,                /* Serial Number Format Msg   */~
            key$40,                      /* Source Transaction Key.    */~
            lastpart$25,                 /* Last Part NUmber Processed */~
            location$31,                 /* S/N Status & Location Test */~
            lot$16,                      /* Lot                        */~
            max$2,                       /* Max Serial Number Length   */~
            min$2,                       /* Min Serial Number Length   */~
            mode$1,                      /* Add or Change Mode (A/C)   */~
            next$14,                     /* Next Serial Number Digits  */~
            p%(15),                      /* Search Receiver array      */~
            part$,                       /* Part Number                */~
            plowkey$80,                  /* Misc. Plow Key             */~
            readkey$64,                  /* File Read Key Variable     */~
            ser$(1)20,                   /* Passed in Serial #'s       */~
            serial$,                     /* Serial Number to Add       */~
            serial$(2)20,                /* Serial Number(s) to Add    */~
            sertrack$1,                  /* Serial Tracking Flag       */~
            ser_unique$1,                /* Serial Numbers Uniqueness  */~
            status$1,                    /* Serial Number Status Flag  */~
            store$3,                     /* Store Number               */~
	    tdate$8,                     /* Temp formated date         */~
            trankey$40,                  /* Source Transaction Key.    */~
            trantype$2,                  /* Source Transaction Type.   */~
            type$2,                      /* Source Transaction Type.   */~
	    udate$8,                     /* Unformated date CCYYMMDD   */~
            userid$3                     /* Current User ID            */~

        dim f2%(04),                     /* = 0 if the file is open    */~
            f1%(04),                     /* = 1 if READ was successful */~
            fs%(04),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(04)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L01342
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
L01342: REM *************************************************************
            if fs%(1) = 0% then                                          ~
               call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            errormsg$ = " "
            qty = int(qty)
            if readkey$ > " " then L09230
                REM *** Extract User ID & Workstation Device # ***
                call "EXTRACT" addr("ID", userid$)
                REM *** Get System Switches ***
                auto$ = "N" : max$ = "6" : min$ = "6" : format$ = all("#")
                filesize% = 1000% : ser_unique$ = "U"
                readkey$ = "SWITCHS.HNY"
                call "READ100" (#1, readkey$, f1%(1))
                if f1%(1) = 0% then L09210
                get #1 using L09200, auto$, max$, min$, format$, filesize%
L09200:         FMT POS(41), CH(1), 2*CH(2), CH(20), POS(88), BI(3)
L09210:         max% = 6% : convert max$ to max%, data goto L09220
L09220:         min% = 6% : convert min$ to min%, data goto L09221
L09221:         formatmsg$ = format$
                tran(formatmsg$,"A#N+Y%")replacing
                call "OPENCHCK" (#3, fs%(3), f2%(3), filesize%, rslt$(3))

L09230:     REM *** Retrieve Part Specific Flags From HNYMASTR ***
                if part$ = lastpart$ then L10000
                lastpart$ = part$
                sertrack$ = "N"          /* Default          */
                REM *** Check to See if we can save a read ***
                if fs(#2) <"10" and part$ <> " " and part$ = key(#2)     ~
		   then L09290
                call "READ100" (#2, lastpart$, f1%(2))
                if f1%(2) = 0% then L10000
L09290:         get #2 using L09300, sertrack$
L09300:         FMT POS(131), CH(1)

L10000: REM *************************************************************~
            *          M A I N   R O U T I N E   B E G I N S            *~
            *-----------------------------------------------------------*~
            *   First test to see if Serial number allowed/required.    *~
            *   Then test format to see if valid.  Then check Serial    *~
            *   number to see if unique.                                *~
            *************************************************************

            if qty >= 1 then L10110
               errormsg$ = "Quantity to Assign CANNOT be less than 1!"
               goto exit_routine
L10110:     if sertrack$ = "N" and serial$ = " " then exit_routine
            if sertrack$ = "Y" and serial$ > " " then test_format
            if sertrack$ = "N" then errormsg$ =                          ~
             "Sorry, Serial Tracking is not Enabled for this Part " &    ~
             part$                                                       ~
            else errormsg$ =                                             ~
             "Sorry, a non-blank Serial Number in the format " &         ~
              formatmsg$ & " is required for this part"
            goto exit_routine

        test_format
            serial$(1) = serial$
            if len(serial$(1%)) >= min% and len(serial$(1%)) <= max%     ~
               then L10280
               errormsg$ = "Sorry, but a Serial number must be between " ~
                         & min$ & " and " & max$ & " characters long."
               goto exit_routine
L10280:     for x% = 1% to len(serial$(1%))
                if str(format$,x%,1%) = "#" then L10440
                if str(format$,x%,1%) = "+" then L10420
                if x% > 19% then L10360
                if str(format$,x%,2%) <> "%%" then L10360
		   tdate$ = date
		   call "DATEFMT" ( tdate$, 0%, udate$ )
                   str(serial$(1%),x%,2%) = str(udate$,3%,2%)
                   x% = x% + 1%
                   goto L10440
L10360:         if str(format$,x%,1%) = str(serial$(1%),x%,1%) then L10440
L10370:            errormsg$ = "Sorry, but a Serial Number MUST be in "  ~
                             & "the format " & formatmsg$
                   x% = 99%
                   goto L10440
                /* Test for Digits Only */
L10420:         if str(serial$(1%),x%,1%) < "0" or str(serial$(1%),x%,1%)~
                                          > "9" then L10370
L10440:     next x%
            if errormsg$ > " " then exit_routine

        REM Everything Hunk-Dorey - Reassign the S/N passed in
            serial$ = serial$(1%)

        REM Test QTY & Assign S/N's to Range if Applicable
            i% = 1%
L10520: REM Start Sequentially Assigning & Reserving S/N's
            gosub reserve_serialnbr      /* Reserve Serial Number      */
            if errormsg$ > " " then L10580
            if i% >= qty then L10640
            f1%(3) = 0%
            gosub increment_number
            if errormsg$ = " " then L10600
L10580:        gosub unreserve_serialnbrs
               goto exit_routine
L10600:     if l% >= dim(ser$(),1)-1% then L10640
            i% = i% + 1%
            l% = l% + 1%
            goto L10520
L10640:     if auto$ <> "Y" then exit_routine
            REM *** Update Next Avail Number ***
            serial$(1%) = ser$(l%)
            gosub increment_number
            errormsg$ = " "
            call "READ101" (#1,"SWITCHS.HNY         ", f1%(1))
            if f1%(1) = 0% then exit_routine
            get #1 using L10730, serial$(2%)
            if serial$(2%) > serial$(1%) then L10710
            put #1 using L10730, serial$(1%)
L10710:     rewrite #1
            goto exit_routine
L10730:     FMT POS(66), CH(20)

        reserve_serialnbr
            gosub check_for_uniqueness
            ser$(l%) = " "
            if errormsg$ > " " then L11350
            call "GETDTTM" addr(datetime$)
            put #3 using L35060,                                          ~
            "6",            /* Current Status Of a Serial Numbered Part*/~
            userid$,        /* Reserved by User ID                     */~
            index%,         /* Document Line assigned to               */~
            "SN Temporarily Assigned",         /* Text                 */~
            serial$(1%),    /* Serial Number                           */~
            part$,          /* Part code                               */~
            serial$(1%),    /* SERIAL NUMBER                           */~
            " ",            /* Job Number                              */~
            " ",            /* Vendor code                             */~
            " ",            /* Receiver Control Number                 */~
            " ",            /* Purchase Order Number                   */~
            " ",            /* Purchase Line Sequence Number (not ITEM */~
            " ",            /* Vendor Lot                              */~
            " ",            /* The specific BOM identifier for a Bill. */~
            " ",            /* The specific routing to use for a Bill. */~
            " ",            /* Date production job actually started    */~
            " ",            /* Date Job Completed                      */~
            store$,         /* Warehouse or Store                      */~
            lot$,           /* Which lot in inventory                  */~
            datetime$,      /* Date & Time                             */~
            userid$,        /* user-id of specific user                */~
            hex(ffffffff),  /* Internal ID to text in TXTFILE.         */~
            trantype$,      /* Transaction Type Code                   */~
            trankey$,       /* Transaction or Document Line Key value  */~
            " "             /* Filler (Internal, unused space)         */~

            write #3, eod goto L11320
L11320:     ser$(l%) = serial$(1)
            return

L11350:     if i% = 1% and auto$ <> "Y" then return
            if l% > 1% and i% = 1% then return
            if mode$ = "C" then return  /* Can't change */
            if qty = 1 then return
            errormsg$ = " "
            gosub increment_number
            if errormsg$ > " " then return
            goto reserve_serialnbr

        increment_number
            mat p% = zer
            search -format$ = "+" to p%()
            if p%(1%) > 0% then L11500
               errormsg$ = "Serial Number Format is not set up for " &   ~
                           "sequential numbering of S/N's!"
               return
L11500:     x%,y%,b% = 0%

            REM *** Construct the Number to Increment ***
L11530:     if p%(y%+1%) = 0% then L11610
            a% = 0%
            y% = y% + 1%
            convert str(serial$(1),p%(y%),1%) to a%, data goto L11570
L11570:     b% = b% + a% * 10%^x%
            x% = x% + 1%
            if y% >= 14% then L11610
            goto L11530
L11610:     nxt = b% + 1%
            call "CONVERT" (nxt,0,str(next$,,y%))
            tran(str(next$,,y%),"0 ")replacing
            for x% = 1% to y%
                str(serial$(1),p%(1%+y%-x%),1%) = str(next$,x%,1%)
            next x%
            if serial$(1) > serial$ then return
               errormsg$ = "Unable to auto-increment S/N's for the " &   ~
                           "given quantity."
               return

        unreserve_serialnbrs
            if i% = 1% and f1%(3) = 1% then return
            for x% = l% - (i% - 1%) to l%
                plowkey$ = str(part$,,25%) & ser$(x%)
                call "DELETE" (#3, plowkey$, 45%)
                if x% > l% - (i% - 1%) then ser$(x%) = " "
            next x%
            l% = l% - (i% - 1%)
            return

        check_for_uniqueness   /* Test for Uniqueness Across the Board */
            readkey$, ser$(l%) = serial$(1%)
            call "PLOWALTS" (#3, readkey$, 1%, 20%, f1%(3))
            if f1%(3) = 0% then return
            status$ = str(key(#3,2),,1%) and hex(3f)
            if status$ = "1" then L12100
            if trantype$ <> "IP" then L12090
               location$ = "2" & str(store$,,3%) & lot$
               if str(key(#3,2),,31%) = location$ then L12121
L12090:     if status$ <> "6" and str(key(#3,2),,1%) < hex(40) then L12170
L12100:     get #3 using L12110, type$, key$
L12110:     FMT POS(216), CH(2), CH(40)
            if type$ <> trantype$ or key$ <> trankey$ then L12170
L12121:     if ser_unique$ = "U" and                                     ~
               str(readkey$,21%,25%) <> part$ then L12170
            search ser$() = str(readkey$,,20%) to p%() step 20%
            if p%(1) = 0% then L12170
            if p%(2) = 0% then return
L12170:     errormsg$ = "Sorry but S/N " & serial$(1%) & " is already " &~
                        "assigned to Part " & str(readkey$,21%,25%)
            return

        REM *************************************************************~
            *                  F I L E   F O R M A T S                  *~
            *-----------------------------------------------------------*~
            *  File Layout for SERMASTR.                                *~
            *************************************************************

L35060: FMT                 /* FILE: SERMASTR                          */~
            CH(1),          /* Current Status Of a Serial Numbered Part*/~
            CH(3),          /* Reserved by User ID                     */~
            BI(3),          /* Document Line assigned to               */~
            CH(24),         /* Free text                               */~
            CH(20),         /* Serial Number                           */~
            CH(25),         /* Part code                               */~
            CH(20),         /* Serial Number                           */~
            CH(8),          /* Job Number                              */~
            CH(9),          /* Vendor code                             */~
            CH(16),         /* Receiver Control Number                 */~
            CH(16),         /* Purchase Order Number                   */~
            CH(3),          /* Purchase Line Sequence Number (not ITEM */~
            CH(16),         /* Lot Number                              */~
            CH(3),          /* The specific BOM identifier for a Bill o*/~
            CH(3),          /* The specific routing to use for a Bill. */~
            CH(6),          /* Date production job actually started    */~
            CH(6),          /* Date Job Completed                      */~
            CH(3),          /* Warehouse or Store                      */~
            CH(16),         /* Which lot in inventory - always used wit*/~
            CH(7),          /* Date & Time record was setup on the syst*/~
            CH(3),          /* user-id of specific user                */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(2),          /* Transaction Type Code                   */~
            CH(40),         /* Transaction or Document Line Key value  */~
            CH(43)          /* Filler (Internal, unused space)         */~

        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_routine
            end
