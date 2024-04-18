        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   EEEEE  RRRR   IIIII  N   N   SSS   U   U  BBBB    *~
            *  S      E      R   R    I    NN  N  S      U   U  B   B   *~
            *   SSS   EEEE   RRRR     I    N N N   SSS   U   U  BBBB    *~
            *      S  E      R   R    I    N  NN      S  U   U  B   B   *~
            *   SSS   EEEEE  R   R  IIIII  N   N   SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SERINSUB - Handles assignment of new serial numbers to    *~
            *            parts entering inventory.                      *~
            *                                                           *~
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
            * 01/19/87 ! Original                                 ! LDJ *~
            * 03/25/87 ! Disabled Range Option if no digits       ! LDJ *~
            *          !   defined in S/N format (less than 3).   !     *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "SERINSUB" (partcode$,       /* Part code                  */~
                        storeno$,        /* Store Number               */~
                        lotno$,          /* Lot                        */~
                        qty,             /* Qty to Assign S/N's To     */~
                        index%,          /* Pointer For Work File Use  */~
                        avg_lines%,      /* Average # Lines per Documnt*/~
                        trantype$,       /* Source Transaction Type.   */~
                                         /*   IA = Inventory Additions */~
                                         /*   PO = P.O. Receipts, etc. */~
                        trankey$,        /* Source Transaction Key.    */~
                        errormsg$,       /* Returned Error Message     */~
                        #1,              /* SYSFILE2 UFB               */~
                        #2,              /* HNYMASTR UFB               */~
                        #3,              /* SERMASTR UFB               */~
                        #4)              /* SERWORK  UFB               */

        dim                                                              ~
            auto$1,                      /* Auto S/N Assignment Flag   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            format$20,                   /* Serial Number Format Code  */~
            function$1,                  /* Function Code (Add,Chg,Del)*/~
            i$(24)80,                    /* Screen Image               */~
            infomsg$79,                  /* Informational Message      */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            line7$79,                    /* Column Header on Line 7    */~
            lot$16,                      /* Lot                        */~
            mfac$(1001)1,                /* Field Attribute Characters */~
            n$(1001)6,                   /* Screen Sequence Numbers    */~
            next$14,                     /* Next Serial Number Digits  */~
            nfac$(1001)1,                /* Field Attribute Characters */~
            p%(15),                      /* Search Receiver array      */~
            part$25,                     /* Part code                  */~
            pf16$16,pf11$16,pf12$16,     /* PF Key Screen Literals     */~
            pf2$8,pf3$8,pf4$8,pf5$8,     /* PF Key Screen Literals     */~
            pf6$8,pf7$8,pf1$20,pf8$,pf9$,/* PF Key Screen Literals     */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            qty$10,                      /* Total Quantity to Assign   */~
            qtyassigned$10,              /* Serial Numbers Assigned    */~
            range$1,                     /* Add Range? (Y/N)           */~
            remainder$10,                /* Remainder to Assign        */~
            s$(1001)1,                   /* Selection Block            */~
            serial$(1001)20,             /* Serial Numbers             */~
            sfac$(1001)1,                /* Field Attribute Characters */~
            store$3,                     /* Store Number               */~
            temp$20,                     /* Temporary Place Holder     */~
            trantype$2,                  /* Source Transaction Type.   */~
            trankey$40,                  /* Source Transaction Key.    */~
            userid$3                     /* Current User Id            */~

        dim f1%(06)                      /* = 1 if READ was successful */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.04 05/19/92 UNIX Compatibility Changes      "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #5  ! SERDETAL ! Serial Number Parent/Component Relations *~
            *************************************************************

            select #5,  "SERDETAL",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =   46, keylen =  48,                     ~
                        alt key  1, keypos =    1, keylen =  93

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            part$ = partcode$ : store$ = storeno$ : lot$ = lotno$ : o%=1%
            if userid$ > " " then L09290
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            line7$ = "   Seq #    Serial Number"
            str(line7$,44%) = "Seq #    Serial Number"
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            for x% = 1% to dim(n$(),1)
                put n$(x%) using L09170, x%
            next x%
L09170:     %#####)
            auto$ = "N" : max_qty% = 100%
            if fs(#1) = "00" and key(#1) = "SWITCHS.HNY" then L09230
            plowkey$ = "SWITCHS.HNY"
            call "READ100" (#1, plowkey$, f1%(1))
            if f1%(1) = 0% then L09250
L09230:     get #1 using L09240, auto$, format$, max_qty%
L09240:     FMT POS(41), CH(1), POS(46),CH(20),POS(86),BI(2)
L09250:     max_qty% = min(max_qty%, dim(serial$(),1) - 1%)
            mat p% = zer
            search -format$ = "+" to p%()

L09290:     call "SERENABL" (part$,           /* Part Number to Check  */~
                             enabled%,        /* Enable Flag to Set    */~
                             ll%,             /* Maximum S/N Field Len */~
                             #1,              /* SYSFILE2 UFB          */~
                             #2)              /* HNYMASTR UFB          */

            if enabled% = 0% then exit_routine
            if fs% = 0% then call "OPENCHCK" (#5, fs%, 0%, 0%, " ")
            gosub initialize_variables
            if qty <= max_qty% then L09420
               errormsg$ = "Quantity may not exceed##### on Parts " &    ~
                           "requiring Serial Numbers!"
               convert max_qty% to str(errormsg$,24%,5%),pic(#####)
               goto exit_routine
L09420:     call "SERMKWRK" (#1, avg_lines%, #4)
            call "CONVERT" (qty,-.0001,qty$)
            gosub dataload
            gosub'051(1%)
            if function$ = "A" then L10380
            if function$ = "D" then L10400

        REM *************************************************************~
            *       E D I T     M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        editmode
            pf16$ = "(16)Return"
            errormsg$, pf1$,pf8$,pf9$ = " "
            function$ = "C"
            gosub'051(3%)
            gosub set_pf_prompts
            if remainder > 0 then pf11$ = "(11)Add S/N's"                ~
                             else pf11$ = " "
            if qtyassigned>0 then pf12$ = "(12)Delete S/N's"             ~
                             else pf12$ = " "
L10130:     gosub'101(3%)     /* Display & Accept Screen    */
               if keyhit% = 2% then o% = 1%
               if keyhit% = 3% then o% = max(1%,m%-23%)
               if keyhit% = 4% then o% = max(1%,o%-23%)
               if keyhit% = 5% then o% = max(1%,min(m%-23%,o%+23%))
               if keyhit% = 6% then o% = max(1%,o%-1%)
               if keyhit% = 7% then o% = max(1%,min(m%-23%,o%+1%))
               if keyhit% =11% and remainder > 0 then L10380
               if keyhit% =12% and qtyassigned > 0 then L10400
               if keyhit% =16% then datasave
               if keyhit% <>0% then editmode
               if cursor%(1) < 8% or cursor%(1) > 19% then editmode
               l% = o% + cursor%(1) - 8%
               if cursor%(2) > 38% then l% = l% + 12%
               if l% > m% then editmode
               if serial$(l%) = " " then editmode
             gosub'051(4%)
               if enabled% = 0% and errormsg$ > " " then L10130
               if enabled% = 0% then editmode
               pf1$,pf2$,pf3$,pf4$,pf5$,pf6$,pf7$,pf11$,pf12$,pf16$ = " "
            plowkey$ = str(part$) & serial$(l%)
            call "READ101" (#3, plowkey$, f1%(3))
            if f1%(3) = 0% then L10320
            if str(key(#3,2),,1%) = "6" then delete #3                   ~
            else call "STRTRLSE" (#3)

L10320:     gosub'101(fieldnr%)     /* Display & Accept Screen    */
                if keyhit% <> 0% then L10320
            gosub'151(4%)     /* Edit Field for Valid Entry */
            if errormsg$ > " " then L10320
            goto editmode

L10380:     function$ = "A" : gosub add_serial_numbers : goto editmode

L10400:     function$ = "D" : gosub del_serial_numbers : goto editmode


        add_serial_numbers
            pf11$, pf12$, pf16$ = " "
            pf1$ = "(1)Exit Add Mode"
            gosub'051(2%)      /* Add Range ?  */
            if enabled% = 0% then L10520  /* Normal Adds      */
L10470:     gosub'101(fieldnr%)     /* Display & Accept Screen    */
               if keyhit%  = 1% then return
               if keyhit% <> 0% then L10470
            gosub'151(2%)     /* Edit Field for Valid Entry */
               if errormsg$ > " " then L10470

L10520:     gosub'051(4%)      /* Check Qty's, set input message   */
               if enabled% = 0% then return
               if enabled% = 2% then call "SHOSTAT" (                    ~
                  "Auto Serial Number Assignment in Progress")
               if enabled% = 2% then L10570
L10540:     gosub'101(fieldnr%)     /* Display & Accept Screen    */
               if keyhit% = 1% then serial$(l%) = " "
               if keyhit% = 1% then return
               if keyhit% <> 0% then L10540
L10570:     gosub'151(4%)     /* Edit Field for Valid Entry */
               if errormsg$ > " " then L10540
            qtyassigned = l%
            call "CONVERT" (qtyassigned,-.0001,qtyassigned$)
            remainder = qty - qtyassigned
            call "CONVERT" (remainder,-.0001,remainder$)
            m% = l%
            goto L10520


        set_pf_prompts
            if o% = 1% then L10710
               pf2$ = "(2)First" : pf4$ = "(4)Prev" : pf6$ = "(6)Up"
                goto L10720
L10710:        pf2$, pf4$, pf6$ = " "
L10720:     if o% >= m% - 23% then L10750
               pf3$ = "(3)Last " : pf5$ = "(5)Next" : pf7$ = "(7)Down"
               return
L10750:        pf3$, pf5$, pf7$ = " "
            return

        del_serial_numbers
            gosub'051(3%)      /* Check if anything to Delete      */
               if enabled% = 0% then return
L10810:        gosub set_pf_prompts
               pf1$ = "(1)Exit Delete Mode"
               pf11$, pf12$, pf16$ = " "
               pf8$ = "(8)'X' All S/N's"
               pf9$ = "(9)Reset to ' '"
            gosub'101(3%)     /* Display & Accept Screen    */
               if keyhit% = 1% then return
               if keyhit% = 2% then o% = 1%
               if keyhit% = 3% then o% = max(1%,m%-23%)
               if keyhit% = 4% then o% = max(1%,o%-23%)
               if keyhit% = 5% then o% = max(1%,min(m%-23%,o%+23%))
               if keyhit% = 6% then o% = max(1%,o%-1%)
               if keyhit% = 7% then o% = max(1%,min(m%-23%,o%+1%))
               if keyhit% = 8% then s$() = all("X")
               if keyhit% = 9% then s$() = all(" ")
               if keyhit% <>0% then L10810
               x% = m%
               for l% = 1% to m%
                   if s$(l%) <> "X" then L11130
                   plowkey$ = str(part$) & serial$(l%)
                   call "READ101" (#3, plowkey$, f1%(3))
                   if f1%(3) = 0% then L11110
                   if str(key(#3,2),,1%) = "6" then L11102
                   if trantype$ <> "PO" then L11105
L11102:               delete #3
L11105:            call "DELETE" (#5, plowkey$, 45%)
L11110:            serial$(l%) = " "
                   x% = x% - 1%
L11130:         next l%
                call "STRTRLSE" (#3)
                call "LINSMASH" (serial$())
                init(" ")s$()
                m% = x%
                qtyassigned = m%
                call "CONVERT" (qtyassigned,-.0001,qtyassigned$)
                remainder = qty - qtyassigned
                call "CONVERT" (remainder,-.0001,remainder$)
                if o% > m%-23% then o% = max(1%, m%-23%)
                return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto exit_routine

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20140,         /* Function Selection */    ~
                              L20190,         /* Add Range? (Y/N)   */    ~
                              L20280,         /* Selection Block    */    ~
                              L20400          /* Serial Number      */
            return
L20140: REM Def/Enable Function Code (Add,Chg,Del) FUNCTION$
            function$ = "C"
            if qtyassigned < qty then function$ = "A"
            if qtyassigned > qty then function$ = "D"
            return
L20190: REM Def/Enable Add Range? (Y/N)            RANGE$
            range$ = "N"
            if p%(1) * p%(2) * p%(3) = 0% then enabled% = 0%
            if remainder < 2 then enabled% = 0%
            if enabled% = 0% then return
            inpmessage$ = "Enter 'Y' to Assign a Range of S/N's " &      ~
                          "or 'N' to enter them 1 at a time."
            range$ = "Y"
            if auto$ = "Y" and remainder = qty then enabled% = 0%
            return
L20280: REM Def/Enable Selection Block             S$(12)
            if m% = 0% then enabled% = 0%
            if function$ = "D" then inpmessage$ =                        ~
               "Enter an 'X' in the Selection Block Next to each S/N " & ~
               "to DELETE and press RETURN."
            if function$ = "C" then inpmessage$ =                        ~
               "Position cursor to the S/N to CHANGE and press RETURN" & ~
               " or PF16 to return"
            if function$ = "C" then init(hex(0b))str(s$(),,m%)           ~
                               else init(" ")str(s$(),,m%)
            range$ = "N"
            return
L20400: REM Def/Enable Serial Number               SERIAL$(12)
            if function$ <> "C" then L20500
               inpmessage$ = "Change the Serial Number Above"
               plowkey$ = str(part$) & str(serial$(l%)) & hex(000000)
               call "PLOWNEXT" (#5, plowkey$, 45%, f1%(5))
               if f1%(5) = 0% then return
               enabled% = 0%
               errormsg$ = "Sorry, You Cannot Change a S/N that has " &  ~
                           "Components Kitted to It!"
               inpmessage$ = "Press RETURN to Acknowlege & Continue"
               return
L20500:     if m% >= qty then enabled% = 0%
            if m% >= dim(serial$(),1)-1% then enabled% = 0%
            if enabled% = 0% then return
            o% = max(1%,m%-22%)
            l% = max(1%, m%+1%)
            inpmessage$ = "Enter the Serial Number to Assign"
            if auto$ = "Y" and remainder = qty then auto_assign
            if auto$ <> "Y" then L20640
               r% = remainder
               if range$ = "N" then remainder = 0
               gosub auto_assign
               remainder = r%
               enabled% = 1%
               return
L20640:     if serial$(l%) = " " and l% > 1% then                        ~
               serial$(l%) = serial$(l%-1%)
            q% = 1%
            if serial$(l%) > " " then gosub increment_number
            return

        auto_assign
            call "READ101" (#1,"SWITCHS.HNY         ", f1%(1))
            if f1%(1) = 0% then L20640
            get #1 using L20820, serial$(l%)
            temp$ = serial$(l%)
            q% = remainder
            gosub increment_number
            put #1 using L20820, serial$(l%)
            rewrite #1
            serial$(l%) = temp$
            enabled% = 2%
            return
L20820:     FMT POS(66), CH(20)

        increment_number
            if p%(1%) = 0% then return
            x%,y%,b% = 0%

            REM *** Construct the Number to Increment ***
L20890:     if p%(y%+1%) = 0% then L20970
            a% = 0%
            y% = y% + 1%
            convert str(serial$(l%),p%(y%),1%) to a%, data goto L20930
L20930:     b% = b% + a% * 10%^x%
            x% = x% + 1%
            if y% >= 14% then L20970
            goto L20890
L20970:     nxt = b% + q%
            call "CONVERT" (nxt,0,str(next$,,y%))
            tran(str(next$,,y%),"0 ")replacing
            for x% = 1% to y%
                str(serial$(l%),p%(1%+y%-x%),1%) = str(next$,x%,1%)
            next x%
            return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      range$                 , /* Add Range? (Y/N)   */  ~
                      s$()                   , /* Selection Block    */  ~
                      serial$()              , /* Serial Number      */  ~
                      pf1$,pf2$,pf3$,pf4$,pf5$,pf6$,pf7$,pf11$,pf12$
            mat redim mfac$(max_qty%+1%)1,                               ~
                      n$(max_qty%+1%)6,                                  ~
                      nfac$(max_qty%+1%)1,                               ~
                      s$(max_qty%+1%)1,                                  ~
                      serial$(max_qty%+1%)20,                            ~
                      sfac$(max_qty%+1%)1
            return


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
            *   L O A D   E N T R I E S   F R O M   W O R K    F I L E  *~
            *-----------------------------------------------------------*~
            * Reads passed in Work File for any S/N's previously entered*~
            * for the passed in index pointer and loads them into an    *~
            * array automatically sorted!                               *~
            *************************************************************
        dataload
            plowkey$ = bin(index%,3)
            m% = 0%
            l% = 1%
L30120:     call "PLOWNEXT" (#4, plowkey$, 3%, f1%(4))
            if f1%(4) = 0% then L30180
            m% = m% + 1%
            if m% > dim(serial$(),1)-1% then L30180
               serial$(m%) = str(plowkey$,4%)
               goto L30120
L30180:     qtyassigned = m%
            call "CONVERT" (qtyassigned,-.0001,qtyassigned$)
            remainder = qty - qtyassigned
            call "CONVERT" (remainder,-.0001,remainder$)
            return

        REM *************************************************************~
            *   L O A D   E N T R I E S   I N T O   W O R K    F I L E  *~
            *-----------------------------------------------------------*~
            * Deletes Prior Contents of Work File for this Index pointer*~
            * and then writes to the work file the current contents of  *~
            * the serial number array.                                  *~
            *************************************************************
        dataput
            plowkey$ = bin(index%,3)
            call "DELETE" (#4, plowkey$, 3%)
            if m% < 1% then L31130
            for l% = 1% to m%
                write #4 using L31170, index%, serial$(l%), part$
            next l%
L31130:     if qtyassigned <> qty then errormsg$ =                       ~
               "Qty differs from S/N's assigned by " & remainder$
            return

L31170:     FMT BI(3), CH(20), CH(25)

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              line2$ = "For Part " & part$
              if store$ > " " then line2$ = line2$ &                     ~
                 " Store " & store$
              if lot$ > " " then line2$ = line2$ &                       ~
                 " Lot " & lot$
              if store$ > " " then L40090
              if trantype$ = "WP" then line2$ = line2$ &                 ~
                 " Job " & trankey$
L40090:       o% = max(1%, o%)
              str(line2$,62%) = "SERINSUB: " & str(cms2v$,,8%)
              infomsg$ = " "
              if function$ = "D" then infomsg$ =                         ~
                "Please Note:  You Are Now in DELETE Mode"
              init(hex(8c)) lfac$(),mfac$()
              init(hex(9c)) sfac$(),nfac$(), sfac$()
              if function$ = "A" then init(hex(8c))str(nfac$(),,m%+1%)   ~
                                 else init(hex(8c))str(nfac$(),,m%)
              on fieldnr% gosub      ,         /* Function Code     */   ~
                                L40200,         /* Add Range? (Y/N)  */   ~
                                L40230,         /* Selection Blocks  */   ~
                                L40220          /* Serial Numbers    */
              goto L40270

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */
L40220:           mfac$(l%)       = hex(81)  :  return  /* Upper Only */
L40230:           if function$ = "C" then init(hex(86))str(sfac$(),,m%)  ~
                                     else init(hex(81))str(sfac$(),,m%)
                  return

L40270:     accept                                                       ~
               at (01,02),                                               ~
                  "Serial Number Assignment",                            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,24), fac(hex(84)), infomsg$               , ch(50),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02),                                               ~
        "Total Quantity To Assign =",                                    ~
               at (05,29), fac(hex(8c)), qty$                   , ch(06),~
               at (05,36),                                               ~
        "S/N's Assigned =",                                              ~
               at (05,53), fac(hex(8c)), qtyassigned$           , ch(06),~
               at (05,60),                                               ~
        "Remainder =",                                                   ~
               at (05,73), fac(hex(84)), remainder$             , ch(06),~
               at (06,02),                                               ~
        "(Add Range? (Y/N):",                                            ~
               at (06,21), fac(lfac$(02)), range$               , ch(01),~
               at (06,23),                                               ~
        ")",                                                             ~
               at (07,02), fac(hex(ac)), line7$,                         ~
               at (08,04), fac(nfac$(o%+00%)), n$(o%+00%)       , ch(06),~
               at (08,12), fac(sfac$(o%+00%)), s$(o%+00%)       , ch(01),~
               at (08,14), fac(mfac$(o%+00%)), str(serial$(o%+00%),,ll%),~
               at (09,04), fac(nfac$(o%+01%)), n$(o%+01%)       , ch(06),~
               at (09,12), fac(sfac$(o%+01%)), s$(o%+01%)       , ch(01),~
               at (09,14), fac(mfac$(o%+01%)), str(serial$(o%+01%),,ll%),~
               at (10,04), fac(nfac$(o%+02%)), n$(o%+02%)       , ch(06),~
               at (10,12), fac(sfac$(o%+02%)), s$(o%+02%)       , ch(01),~
               at (10,14), fac(mfac$(o%+02%)), str(serial$(o%+02%),,ll%),~
               at (11,04), fac(nfac$(o%+03%)), n$(o%+03%)       , ch(06),~
               at (11,12), fac(sfac$(o%+03%)), s$(o%+03%)       , ch(01),~
               at (11,14), fac(mfac$(o%+03%)), str(serial$(o%+03%),,ll%),~
               at (12,04), fac(nfac$(o%+04%)), n$(o%+04%)       , ch(06),~
               at (12,12), fac(sfac$(o%+04%)), s$(o%+04%)       , ch(01),~
               at (12,14), fac(mfac$(o%+04%)), str(serial$(o%+04%),,ll%),~
               at (13,04), fac(nfac$(o%+05%)), n$(o%+05%)       , ch(06),~
               at (13,12), fac(sfac$(o%+05%)), s$(o%+05%)       , ch(01),~
               at (13,14), fac(mfac$(o%+05%)), str(serial$(o%+05%),,ll%),~
               at (14,04), fac(nfac$(o%+06%)), n$(o%+06%)       , ch(06),~
               at (14,12), fac(sfac$(o%+06%)), s$(o%+06%)       , ch(01),~
               at (14,14), fac(mfac$(o%+06%)), str(serial$(o%+06%),,ll%),~
               at (15,04), fac(nfac$(o%+07%)), n$(o%+07%)       , ch(06),~
               at (15,12), fac(sfac$(o%+07%)), s$(o%+07%)       , ch(01),~
               at (15,14), fac(mfac$(o%+07%)), str(serial$(o%+07%),,ll%),~
               at (16,04), fac(nfac$(o%+08%)), n$(o%+08%)       , ch(06),~
               at (16,12), fac(sfac$(o%+08%)), s$(o%+08%)       , ch(01),~
               at (16,14), fac(mfac$(o%+08%)), str(serial$(o%+08%),,ll%),~
               at (17,04), fac(nfac$(o%+09%)), n$(o%+09%)       , ch(06),~
               at (17,12), fac(sfac$(o%+09%)), s$(o%+09%)       , ch(01),~
               at (17,14), fac(mfac$(o%+09%)), str(serial$(o%+09%),,ll%),~
               at (18,04), fac(nfac$(o%+10%)), n$(o%+10%)       , ch(06),~
               at (18,12), fac(sfac$(o%+10%)), s$(o%+10%)       , ch(01),~
               at (18,14), fac(mfac$(o%+10%)), str(serial$(o%+10%),,ll%),~
               at (19,04), fac(nfac$(o%+11%)), n$(o%+11%)       , ch(06),~
               at (19,12), fac(sfac$(o%+11%)), s$(o%+11%)       , ch(01),~
               at (19,14), fac(mfac$(o%+11%)), str(serial$(o%+11%),,ll%),~
               at (08,44), fac(nfac$(o%+12%)), n$(o%+12%)       , ch(06),~
               at (08,52), fac(sfac$(o%+12%)), s$(o%+12%)       , ch(01),~
               at (08,54), fac(mfac$(o%+12%)), str(serial$(o%+12%),,ll%),~
               at (09,44), fac(nfac$(o%+13%)), n$(o%+13%)       , ch(06),~
               at (09,52), fac(sfac$(o%+13%)), s$(o%+13%)       , ch(01),~
               at (09,54), fac(mfac$(o%+13%)), str(serial$(o%+13%),,ll%),~
               at (10,44), fac(nfac$(o%+14%)), n$(o%+14%)       , ch(06),~
               at (10,52), fac(sfac$(o%+14%)), s$(o%+14%)       , ch(01),~
               at (10,54), fac(mfac$(o%+14%)), str(serial$(o%+14%),,ll%),~
               at (11,44), fac(nfac$(o%+15%)), n$(o%+15%)       , ch(06),~
               at (11,52), fac(sfac$(o%+15%)), s$(o%+15%)       , ch(01),~
               at (11,54), fac(mfac$(o%+15%)), str(serial$(o%+15%),,ll%),~
               at (12,44), fac(nfac$(o%+16%)), n$(o%+16%)       , ch(06),~
               at (12,52), fac(sfac$(o%+16%)), s$(o%+16%)       , ch(01),~
               at (12,54), fac(mfac$(o%+16%)), str(serial$(o%+16%),,ll%),~
               at (13,44), fac(nfac$(o%+17%)), n$(o%+17%)       , ch(06),~
               at (13,52), fac(sfac$(o%+17%)), s$(o%+17%)       , ch(01),~
               at (13,54), fac(mfac$(o%+17%)), str(serial$(o%+17%),,ll%),~
               at (14,44), fac(nfac$(o%+18%)), n$(o%+18%)       , ch(06),~
               at (14,52), fac(sfac$(o%+18%)), s$(o%+18%)       , ch(01),~
               at (14,54), fac(mfac$(o%+18%)), str(serial$(o%+18%),,ll%),~
               at (15,44), fac(nfac$(o%+19%)), n$(o%+19%)       , ch(06),~
               at (15,52), fac(sfac$(o%+19%)), s$(o%+19%)       , ch(01),~
               at (15,54), fac(mfac$(o%+19%)), str(serial$(o%+19%),,ll%),~
               at (16,44), fac(nfac$(o%+20%)), n$(o%+20%)       , ch(06),~
               at (16,52), fac(sfac$(o%+20%)), s$(o%+20%)       , ch(01),~
               at (16,54), fac(mfac$(o%+20%)), str(serial$(o%+20%),,ll%),~
               at (17,44), fac(nfac$(o%+21%)), n$(o%+21%)       , ch(06),~
               at (17,52), fac(sfac$(o%+21%)), s$(o%+21%)       , ch(01),~
               at (17,54), fac(mfac$(o%+21%)), str(serial$(o%+21%),,ll%),~
               at (18,44), fac(nfac$(o%+22%)), n$(o%+22%)       , ch(06),~
               at (18,52), fac(sfac$(o%+22%)), s$(o%+22%)       , ch(01),~
               at (18,54), fac(mfac$(o%+22%)), str(serial$(o%+22%),,ll%),~
               at (19,44), fac(nfac$(o%+23%)), n$(o%+23%)       , ch(06),~
               at (19,52), fac(sfac$(o%+23%)), s$(o%+23%)       , ch(01),~
               at (19,54), fac(mfac$(o%+23%)), str(serial$(o%+23%),,ll%),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)), pf1$                           ,~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf2$                           ,~
               at (24,20), fac(hex(8c)), pf3$                           ,~
               at (23,30), fac(hex(8c)), pf4$                           ,~
               at (24,30), fac(hex(8c)), pf5$                           ,~
               at (23,40), fac(hex(8c)), pf6$                           ,~
               at (24,40), fac(hex(8c)), pf7$                           ,~
               at (23,49), fac(hex(8c)), pf8$                           ,~
               at (24,49), fac(hex(8c)), pf9$                           ,~
               at (22,23), fac(hex(8c)), pf11$                          ,~
               at (22,43), fac(hex(8c)), pf12$                          ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(000102030405060708090b0c0d0f10)),                ~
               key (keyhit%)

               if keyhit% <> 13 then L41360
                  call "MANUAL" ("SERINSUB")
                  goto L40270

L41360:        if keyhit% <> 15 then L41400
                  call "PRNTSCRN"
                  goto L40270

L41400:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return
                  u3% = u3%
        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub      ,         /* Function Code     */     ~
                              L50300,         /* Add Range? (Y/N)  */     ~
                              L50350,         /* Selection Block   */     ~
                              L50400          /* Serial Number     */
            return
L50300: REM Test for Add Range? (Y/N)             RANGE$
            if pos("YN" = range$) > 0% then return
            errormsg$ = "Must be Y or N"
            return
L50350: REM Test for Selection Block              S$(12)
            return
L50400: REM Test for Serial Number                SERIAL$(12)
            if range$ = "N" then q = 1 else q = max(1, remainder)
            call "SERVALID" (part$, store$, lot$, serial$(l%), q, index%,~
                             l%, #1, #2, #3, serial$(), function$,       ~
                             trantype$, trankey$, errormsg$)
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_routine

            end
