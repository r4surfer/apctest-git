        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M   SSS   EEEEE  TTTTT  TTTTT  L       *~
            *  A   A  R   R  MM MM  S      E        T      T    L       *~
            *  AAAAA  RRRR   M M M   SSS   EEEE     T      T    L       *~
            *  A   A  R   R  M   M      S  E        T      T    L       *~
            *  A   A  R   R  M   M   SSS   EEEEE    T      T    LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMSETTL - Program purges zero balancing transactions     *~
            *            from the Trial Balance file.  Items purged are *~
            *            written to the report file ARMSTLRF.           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/17/86 ! Original                                 ! ERN *~
            * 01/27/88 ! Multi-currency mods                      ! KAB *~
            * 05/17/88 ! Clean up on selects                      ! JDH *~
            * 10/17/88 ! Warning ASKUSER prior to opening files.  ! JDH *~
            * 07/13/89 ! Allow settling of balances less than .01 ! JDH *~
            * 11/10/89 ! Fixed crash @ workfile PUT with big numbr! JDH *~
            * 11/16/89 ! ARMSTLRF file now gets each items currncy! JDH *~
            *          !  and amount in that currency for printing!     *~
            * 04/09/92 ! PRR 12335. Warning if Cutoff in future.  ! JDH *~
            * 03/08/93 ! PRRs 12571&12572 Settlement LEVEL$ = 'B'.! JIM *~
            * 03/08/93 ! PRR 12804 Use a logically arbitrary (or, ! JIM *~
            *          ! arbitrarily logical) # recs for WORKFILE.! JIM *~
            * 03/18/93 ! OPEN Currency files only if CURR$ = 'Y'. ! JIM *~
            * 03/18/93 ! ASKUSER if user's date < Last Settled.   ! JIM *~
            * 07/29/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**
        dim                                                              ~
            bals(3), sbals(3), cbals(3), /* A/R Balances               */~
            billto$9,                    /* Bill-to Customer Number    */~
            billto_stl$1,                /* Bill-to Settling Allowed?  */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            currency$4,                  /* Transaction Currency       */~
            cursor%(2),                  /* Cursor location for edit   */~
            cutoff$8,                    /* Cutoff Date for Settling   */~
            date$8,                      /* Date for screen display    */~
            datetime$7,                  /* Date/Time Stamp            */~
            delkey$50,                   /* Key For Delete             */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            last_stld$8,                 /* Date of last Settling      */~
            level$1,                     /* Settlement Level           */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            paymnt$10,                   /* Settlement Payment Code    */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            plowkey$50,                  /* Miscellaneous Read/Plow Key*/~
            stlmnt$8,                    /* Settlement Group Code      */~
            stl_msg$41,                  /* Settlement Message         */~
            tbkey$21, tbdata$235,        /* Trial Balance Key and Data */~
            userid$3                     /* Current User Id            */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            axd$4,                       /* AXD Block for OPENFILE     */~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con
                     /* The variable F2%() should not be modified.     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! ARMTRIAL ! Accounts Receivable Trial Balance        *~
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 9 ! ARMSTLRF ! A/R Settling Report File                 *~
            * #10 ! CRCBUFFR ! Cash Receipts Buffer File                *~
            * #11 ! ARIBUFFR ! Invoicing Buffer File                    *~
            * #43 ! CRCLNCUR ! Currency Shadow Line info                *~
            * #44 ! ARMTBCEX ! Currency Shadow Exposure file            *~
            * #45 ! CRCL2CUR ! Currency Shadow Line info                *~
            * #46 ! ARMTBCRC ! Currency Shadow Trial Balance Recon.     *~
            * #47 ! CUSTOMER ! Customer Master File (need # records)    *~
            * #60 ! WORKFILE ! Workfile                                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  21                      ~

            select # 2, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select # 9, "ARMSTLRF",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  1,   keylen = 28                       ~

            select #10, "CRCBUFFR",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen = 17,                      ~
                        alt key  1, keypos =  201, keylen =  23

            select #11, "ARIBUFFR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  17,                     ~
                        alt key  1, keypos = 2001, keylen =  24

            select #43, "CRCLNCUR",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 21,                      ~
                        alt key  1, keypos =  1, keylen =  25

            select #44, "ARMTBCEX",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 21,                      ~
                        alt key  1, keypos =  1, keylen =  25

            select #45, "CRCL2CUR",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 21,                      ~
                        alt key  1, keypos =  1, keylen =  25

            select #46, "ARMTBCRC",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 21,                      ~
                        alt key  1, keypos =  1, keylen =  25

            select #47,  "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #60, "WORKFILE",                                      ~
                        varc,     indexed,  recsize = 21,                ~
                        keypos =    1, keylen = 13

L02610:     u3% = 0% /* Window in the middle */
            call "ASKUSER" (u3%, "*** EXCLUSIVE FILE USAGE ***",         ~
                "This program (ARMSETTL) requires the exclusive use of "&~
                "certain files.", "No other users may run A/R programs "&~
                "while ARMSETTL is running.", "Press (RETURN) to Contin"&~
                "ue; Press PF(16) to exit to menu.")
            if u3% = 16% then goto exit_program
            if u3% <> 0% then goto L02610

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENFILE" (#1, "IO   ", f2%(1%), rslt$(1%), axd$)
            if f2%(1%) <> 0% then exit_program
            get rslt$(1%) using L05024, armrecs%
            call "OPENFILE" (#47%, "VALID", f2%(47%), rslt$(47%), axd$)
            if f2%(47%) = 0% then get rslt$(47%), using L05024, cusrecs%
L05024:         FMT POS(17), BI(4)
            if armrecs% = 0% or cusrecs% = 0%                            ~
                then wrkrecs% = 100%                      /* Arbitrary */~
                else wrkrecs% = armrecs% / cusrecs% * 3%   /* Logical? */
            call "WORKOPEN" (#60, "IO   ", max(100%, wrkrecs%), f2%(60%))
            call "OPENFILE" (#2, "SHARE", f2%(2), rslt$(2), axd$)
            if f2%(2) <> 0% then exit_program
            call "OPENFILE" (#9, "IO   ", f2%(9), rslt$(9), axd$)
            if f2%(9)  = 0% then L05110
                call "OPENFILE" (#9, "OUTPT", f2%(9), rslt$(9), axd$)
                close #9 : f2%(9) = 1%
                call "OPENFILE" (#9, "IO   ", f2%(9), rslt$(9), axd$)
L05110:     call "OPENFILE" (#10, "IO   ", f2%(10), rslt$(10),  axd$)
            if f2%(10) <> 0% then L05212
                get str(rslt$(10)) using L05140, records%
L05140:              FMT POS(17), BI(4)
                if records% = 0% then L05212
                     call "ASKUSER" (2%, "A/R SETTLING",                 ~
                          "Transactions still exist in Cash Buffers.",   ~
                          "They must be processed to run this program.", ~
                          "Press any PF Key to abort...")
                     goto exit_program
L05212:     call "OPENFILE" (#11, "IO   ", f2%(11), rslt$(11), axd$)
            if f2%(11) <> 0% then L09000
                get str(rslt$(11)) using L05140, records%
                if records% = 0% then L09000
                     call "ASKUSER" (2%, "A/R SETTLING",                 ~
                          "Transactions still exist in Invoice Buffers.",~
                          "They must be processed to run this program.", ~
                          "Press any PF Key to abort...")
                     goto exit_program


L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date  :  call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            save$, billto_stl$ = "Y"  :  last_stld$ = " "
            call "READ100" (#2, "ARM.LAST.SETTLING   ", f1%(2))
            if f1%(2) = 0% then L09210
                get #2 using L09160, last_stld$, billto_stl$
L09160:              FMT XX(20), CH(6), CH(1)
                call "DATEFMT" (last_stld$)
                if pos("YN" = billto_stl$) = 0% then billto_stl$ = "Y"
                save$ = billto_stl$

L09210:     stl_msg$ = "Allow Settling at Bill-to Level?"
            call "READ100" (#02, "SWITCHS.CUR", f1%(2))
            if f1%(2) <> 0% then get #02 using L09240, curr$
L09240:         FMT POS(21), CH(1)
            if curr$ <> "Y" then L10000
                stl_msg$ = "Allow Settling at Bill-to/Currency Level?"
                call "OPENFILE" (#44, "IO   ", f2%(44%), rslt$(44%), axd$)
                call "OPENFILE" (#43, "SHARE", f2%(43%), rslt$(43%), axd$)
                call "OPENFILE" (#45, "SHARE", f2%(45%), rslt$(45%), axd$)
                call "OPENFILE" (#46, "IO   ", f2%(46%), rslt$(46%), axd$)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf16$ = "(16)Exit Program"
            billto_stl$ = save$
            init(" ") errormsg$, inpmessage$
            if cutoff$ = " " or cutoff$ = blankdate$ then cutoff$ = date$

            for fieldnr% = 1% to 2%
                gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10180
L10140:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% = 16 and fieldnr% = 1 then exit_program
                      if keyhit% <>  0 then       L10140
L10180:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10140
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            pf16$ = "(16)Settle TB"
            inpmessage$ = edtmessage$
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       settling_routine
                  if keyhit% <>  0 then       editpg1
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1% or fieldnr% > 2% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
                  pf16$ = " "
L11170:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
            goto editpg1


        REM *************************************************************~
            *            S E T T L I N G   R O U T I N E                *~
            * --------------------------------------------------------- *~
            * Remove zero balance items from trial balance.  Levels of  *~
            * settling are (1)Payment, (2)Settlement Group, and         *~
            * (3)Bill-to (if allowed).  All items removed from the      *~
            * Trial Balance are written to the file ARMSTLRF for later  *~
            * reporting on items settled.                               *~
            *************************************************************

        settling_routine
            call "SHOSTAT" ("Settling A/R Trial Balance")
            billto$ = all(hex(00))
            call "DATUNFMT" (cutoff$)


        next_billto
            plowkey$ = str(billto$) & hex(ff)
            call "PLOWNEXT" (#1, plowkey$, 0%, f1%(1))
            if f1%(1) = 0% then end_routine
                          billto$ = str(plowkey$, 1, 9) : billto = 0
            next_stlmnt:  stlmnt$ = str(plowkey$,10, 8) : stlmnt = 0
            next_paymnt:  paymnt$ = str(plowkey$,10,10)
            call "ARMCBLNC" (billto$, paymnt$, cutoff$, 10%, "s", #1,    ~
                             #10, sbals(), #44, #43, #45, currency$,     ~
                             " ", 0, 0, cbals())

            if currency$ = " " then L12300
               mat bals = cbals
               goto L12320
L12300:     mat bals = sbals

L12320:     billto = min(billto + bals(1), 9999999999.9999)
            stlmnt = min(stlmnt + bals(1), 9999999999.9999)
            if abs(sbals(1)) < .01 then L12350
            if abs(bals(1)) >= .01 then L12370
L12350:         level$ = "P"  :  gosub settle
                if str(paymnt$,9,2) = "00" then L12530
L12370:     if str(paymnt$,9,2) = "00" then L12430 /* Payment = Stlment */
                plowkey$ = str(billto$) & str(paymnt$) & hex(ffff)
                call "PLOWNEXT" (#1, plowkey$, 17%, f1%(1))
                if f1%(1) = 1% then next_paymnt
                     if abs(stlmnt) >= .01 then L12430
                          level$ = "S"  :  gosub settle : goto L12530
L12430:              if billto_stl$ = "N" then L12530
                     plowkey$ = str(billto$) & currency$ : temp = 0
                     call "READ101" (#60, plowkey$, f1%(60))
                        if f1%(60) = 0% then L12490
                     get #60 using L12480, temp
L12480:                  FMT POS(14), PD(14,4)
L12490:              temp = temp + stlmnt
                     temp = min(temp, 9999999999.9999)
                     put #60 using L12510, plowkey$, temp
L12510:                  FMT CH(13), PD(14,4)
                     if f1%(60) = 0% then write #60 else rewrite #60
L12530:              plowkey$ = str(billto$) & str(stlmnt$) & hex(ff)
                     call "PLOWNEXT" (#1, plowkey$, 9%, f1%(1))
                     if f1%(1) = 1% then next_stlmnt

                          if billto_stl$ = "N" then L12830
                     plowkey$ = str(billto$) & hex(00000000)
                     stlmnt$ = hex(00)
                     call "PLOWNEXT" (#60, plowkey$, 9%, f1%(60))
                        goto L12630
L12620:              call "READNEXT" (#60, f1%(60))
L12630:                 if f1%(60) = 0% then L12830
                     get #60 using L12650, currency$, temp
L12650:                     FMT POS(10), CH(4), PD(14,4)
                        if abs(temp) >= .01 then L12620
                     if currency$ <> " " then L12760
L12680:                plowkey$ = str(billto$) & str(stlmnt$) & hex(ff)
L12690:                call "PLOWNEXT" (#1, plowkey$, 9%, f1%(1))
                          if f1%(1) = 0% then L12620
                       call "READ100" (#44, key(#1), f1%(44))
                          if f1%(44) = 0% then L12730
                             str(plowkey$,18) = hex(ff)
                             goto L12690
L12730:                stlmnt$ = str(plowkey$,10,8)
                       level$ = "B": gosub settle: goto L12680

L12760:              plowkey$ = str(currency$) & str(billto$) & hex(00)
                     call "PLOWALTS" (#44, plowkey$, 1%, 13%, f1%(44))
                        if f1%(44) = 0% then L12620
                     get #44 using L12800, stlmnt$
L12800:                  FMT POS(14), CH(8)
                     level$ = "C": gosub settle: goto L12760

L12830:              plowkey$ = str(billto$) & hex(00000000)
                     call "DELETE" (#60, plowkey$, 9%)
                     goto next_billto

            settle   /* Toss zero balance items into report file       */
                if level$ <> "P" then L14050
                     plowkey$ = str(billto$) & str(paymnt$) & hex(00)
                     break%   = 19%
                     goto settling_loop
L14050:         if level$  = "C" then L14070
                if level$ <> "S" then L14100
L14070:              plowkey$ = str(billto$) & str(stlmnt$) & hex(00)
                     break%   = 17%
                     goto settling_loop
L14100:         if level$ <> "B" then return
                     plowkey$ = str(billto$) & hex(00)
                     break%   =  9%

              settling_loop
                call "PLOWNXT1" (#1, plowkey$, break%, f1%(1))
                if f1%(1) = 0% then return
                     get #1 using L14180, tbkey$, tbdata$
L14180:                   FMT CH(21), CH(235)
                     delete #1
                     call "READ101" (#44, key(#1), f1%(44))
                        if f1%(44) = 0% then L14250
                     get #44 using L14230, str(tbdata$,47,8)
L14230:                  FMT POS(26), CH(8)
                     delete #44
                     delkey$ = key(#1)
*                   CALL "DELETE" (#46, DELKEY$, 21%)
                     call "READ101" (#46, key(#1), f1%(46))
                        if f1%(46) = 0% then L14250
                     get #46 using L14246, currency$, str(tbdata$,47,8)
L14246:                  FMT CH(4), POS(26), CH(8)
                     delete #46
L14250:              call "GETDTTM" addr(datetime$)
                     write #9 using L14300, tbkey$, datetime$,            ~
                                           cutoff$, level$, currency$,   ~
                                           " ", tbkey$, tbdata$,         ~
                                           eod goto L14250
L14300:                   FMT CH(21), CH(7), CH(6), CH(1), CH(4), CH(5), ~
                              CH(21), CH(235)
                     goto settling_loop


        end_routine
            call "READ101" (#2, "ARM.LAST.SETTLING   ", f1%(2))
            put #2 using L14390, "ARM.LAST.SETTLING   ",                  ~
                                cutoff$, billto_stl$, userid$, " ", " "
L14390:         FMT CH(20), CH(6), CH(1), CH(3), CH(250), CH(220)
            if f1%(2) = 0% then write #2 else rewrite #2
            call "FILEBGON" (#60)
            goto exit_program


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub      L20120,          /* Bill-to Settling?*/~
                                   L20162           /* Cutoff date      */
            return

L20120
*        Allow Settling at Bill-to Level?           BILLTO_STL$
            inpmessage$ = "Enter 'Y' to allow settling of all a"  &      ~
                          " Bill-to's items if they zero balance."
            return

L20162
*        Settling Cutoff Date                       CUTOFF$
            inpmessage$ = "Enter Cutoff Date. Only Details Up T"  &      ~
                          "o And Including This Date Will Be Settled."
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
            str(line2$,62%) = "ARMSETTL: " & str(cms2v$,,8%)
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr% gosub       L40130,         /* Bill-to Settling?*/~
                                    L40130          /* Cutoff Date      */
            goto L40160
                  lfac$(fieldnr%) = hex(80)  :  return   /* UpLow      */
L40130:           lfac$(fieldnr%) = hex(81)  :  return   /* UPPER Only */
                  lfac$(fieldnr%) = hex(82)  :  return   /* Numeric    */

L40160:     accept                                                       ~
               at (01,02), "A/R Settling Routine",                       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(8c)),   stl_msg$             , ch(41),~
               at (06,45), fac(lfac$( 1)), billto_stl$          , ch(01),~
               at (06,56), "Last Settled On",                            ~
               at (06,72), fac(hex(8c)),   last_stld$           , ch(08),~
               at (07,02), "Settling Cutoff Date",                       ~
               at (07,38), fac(lfac$( 2)), cutoff$              , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$,                          ~
                     keys(hex(000104050d0f10)), key(keyhit%)

               if keyhit% <> 13% then L40390
                  call "MANUAL" ("ARMSETTL")
                  goto L40160

L40390:        if keyhit% <> 15% then L40430
                  call "PRNTSCRN"
                  goto L40160

L40430:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr%  gosub      L50110,         /* Bill-to Settling?*/~
                                    L50160          /* Cutoff Date      */
            return

L50110
*        Allow Settling at Bill-to Level?      BILLTO_STL$
            if pos("YN" = billto_stl$) > 0% then return
                errormsg$ = "Enter 'Y' -or- 'N'."
                return

L50160
*        Settling Cut Off Date                 CUTOFF$
            call "DATEOK" (cutoff$, 0%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUNFMT" (date$)
                call "DATUNFMT" (cutoff$)
                call "DATUNFMT" (last_stld$)
                if cutoff$ > date$ then L50250
                if cutoff$ < last_stld$ then L50350
L50220:              call "DATEFMT" (date$)
                     call "DATEFMT" (cutoff$)
                     call "DATEFMT" (last_stld$)
                     return
L50250:         u3% = 2%
                call "ASKUSER" (u3%, "*** WARNING ***",                  ~
                     "The Settling Cutoff Date is in the Future.",       ~
                     "Press RETURN to Continue  -or-",                   ~
                     "Press PF1 to Re-enter Date.")
                if u3% = 0% then L50220
                if u3% <> 1% then L50250
                errormsg$ = "Re-enter Settling Cutoff Date"
                     goto L50220

L50350:         u3% = 2%
                call "ASKUSER" (u3%, "*** WARNING ***",                  ~
                     "The Settling Cutoff Date is prior to the Date Last ~
        ~Settled.", "Nothing will be settled AND the current date will be ~
        ~lost.", "Press RETURN to Continue -OR- Press PF1 to Re-enter Date~
        ~.")
                if u3% = 0% then L50220
                if u3% <> 1% then L50350
                errormsg$ = "Re-enter Settling Cutoff Date"
                     goto L50220

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
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
