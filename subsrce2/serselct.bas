        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   EEEEE  RRRR    SSS   EEEEE  L       CCC   TTTTT   *~
            *  S      E      R   R  S      E      L      C   C    T     *~
            *   SSS   EEEE   RRRR    SSS   EEEE   L      C        T     *~
            *      S  E      R   R      S  E      L      C   C    T     *~
            *   SSS   EEEEE  R   R   SSS   EEEEE  LLLLL   CCC     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SERSELCT - Handles assignment of existing serial numbers  *~
            *            to transactions.                               *~
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
            * 01/27/87 ! Original                                 ! LDJ *~
            * 09/13/89 ! Re-Dimensioned INCL_EXCL$ to 25.         ! JDH *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "SERSELCT" (partcode$,       /* Part code                  */~
                        locin$,          /* S/N Location to Select from*/~
                        qty,             /* Qty to Assign S/N's To     */~
                        index%,          /* Pointer For Work File Use  */~
                        avg_lines%,      /* Average # Lines per Documnt*/~
                        trantypein$,     /* Source Transaction Type.   */~
                                         /*   IA = Inventory Additions */~
                                         /*   PO = P.O. Receipts, etc. */~
                        trankeyin$,      /* Source Transaction Key.    */~
                        status$,         /* Status to Change S/N to.   */~
                        source$,         /* Status to Select/Chg from  */~
                        errormsg$,       /* Returned Error Message     */~
                                         /* (If 1st char = '*' then    */~
                                         /* remainder assumed to be an */~
                                         /* info message).             */~
                        #1,              /* SYSFILE2 UFB               */~
                        #2,              /* HNYMASTR UFB               */~
                        #3,              /* SERMASTR UFB               */~
                        #4)              /* SERWORK  UFB               */

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr$80,                    /* PLOWCODE Argument          */~
            descr_map(06),               /* PLOWCODE Argument          */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            format$20,                   /* Serial Number Format Code  */~
            from_serial$20,              /* Beginning S/N Range        */~
            function$1,                  /* Function Code (Add,Chg,Del)*/~
            header$(2)80,                /* PLOWCODE Argument          */~
            i$(24)80,                    /* Screen Image               */~
            incl_excl(3),                /* PLOWCODE Argument          */~
            incl_excl$(3)25,             /* PLOWCODE Argument          */~
            infomsg$79,                  /* Informational Message      */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            line7$79,                    /* Column Header on Line 7    */~
            location$30,                 /* S/N Location to Select from*/~
            locin$30,                    /* S/N Location to Select from*/~
            mfac$(1001)1,                /* Field Attribute Characters */~
            n$(1001)6,                   /* Screen Sequence Numbers    */~
            next$14,                     /* Next Serial Number Digits  */~
            nfac$(1001)1,                /* Field Attribute Characters */~
            p%(15),                      /* Search Receiver array      */~
            part$25,                     /* Part code                  */~
            pf16$16,pf11$16,pf12$16,     /* PF Key Screen Literals     */~
            pf2$8,pf3$8,pf4$8,pf5$8,     /* PF Key Screen Literals     */~
            pf6$8,pf7$8,pf1$20,pf8$,pf9$,/* PF Key Screen Literals     */~
            plowkey$96,                  /* Miscellaneous Read/Plow Key*/~
            qty$10,                      /* Total Quantity to Assign   */~
            qtyassigned$10,              /* Serial Numbers Assigned    */~
            range$1,                     /* Add Range? (Y/N)           */~
            readkey$96,                  /* Miscellaneous Read/Plow Key*/~
            remainder$10,                /* Remainder to Assign        */~
            s$(1001)1,                   /* Selection Block            */~
            serial$(1001)20,             /* Serial Numbers             */~
            serial$20,                   /* Temporary Serial Number    */~
            sfac$(1001)1,                /* Field Attribute Characters */~
            source$1,                    /* Status to Select/Chg from  */~
            status$1,                    /* Status to Change S/N to.   */~
            tkey$40,                     /* Source Transaction Key.    */~
            to_serial$20,                /* Ending    S/N Range        */~
            trantype$2,                  /* Source Transaction Type.   */~
            trantypein$2,                /* Source Transaction Type.   */~
            trankey$40,                  /* Source Transaction Key.    */~
            trankeyin$40,                /* Source Transaction Key.    */~
            ttype$2,                     /* Source Transaction Type.   */~
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
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 2 ! HNYMASTR ! Inventory Master File                    *~
            * # 3 ! SERMASTR ! Serial Number Tracking Master File       *~
            * # 4 ! WORKFILE ! Temporary System Workfile                *~
            * # 5 ! TEMPFILE ! Not Used - Place holder for PLOWCODE     *~
            *************************************************************~

            select #5,  "TEMPFILE",                                      ~
                        varc,     indexed,  recsize =  4  ,              ~
                        keypos =    1, keylen =  4

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            part$ = partcode$ : o%=1% : location$ = locin$
            trantype$ = trantypein$ : trankey$ = trankeyin$
            if userid$ > " " then L09290
            call "EXTRACT" addr("ID", userid$)
            call "OPENCHCK" (#3, 0%, 0%, 0%, "                      ")
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
            max_qty% = 100%
            if fs(#1) = "00" and key(#1) = "SWITCHS.HNY" then L09230
            plowkey$ = "SWITCHS.HNY"
            call "READ100" (#1, plowkey$, f1%(1))
            if f1%(1) = 0% then L09250
L09230:     get #1 using L09240, format$, max_qty%
L09240:     FMT POS(46), CH(20), POS(86),BI(2)
L09250:     max_qty% = min(max_qty%, dim(serial$(),1) - 1%)
            mat p% = zer
            search -format$ = "+" to p%()
            descr_map(1) = 97.08 : descr_map (2) =  3     /* Job       */
            descr_map(3) =105.09 : descr_map (4) = 17     /* Vendor    */
            descr_map(5) =177.061: descr_map (6) = 37     /* Date Built*/
            header$(1) = "  Serial Number         Built By Job  Purch Fro~
        ~m Vendor  Date Built/Purch"

L09290:     call "SERENABL" (part$,           /* Part Number to Check  */~
                             enabled%,        /* Enable Flag to Set    */~
                             ll%,             /* Maximum S/N Field Len */~
                             #1,              /* SYSFILE2 UFB          */~
                             #2)              /* HNYMASTR UFB          */

            if str(errormsg$,,1%) = "*" then infomsg$ = str(errormsg$,2%)~
                                        else infomsg$ = " "
            errormsg$ = " "
            if enabled% = 0% then exit_routine
            gosub initialize_variables
            if qty <= max_qty% then L09420
               errormsg$ = "Quantity may not exceed##### on Parts " &    ~
                           "requiring Serial Numbers!"
               convert max_qty% to str(errormsg$,24%,5%),pic(#####)
               goto exit_routine
L09420:     call "SERMKWRK" (#1, avg_lines%, #4)
            call "CONVERT" (qty,-.0001, qty$)
            gosub dataload
            if qty = 0 and m% = 0% then exit_routine /* nuthin to do */
            gosub'051(1%)
            if function$ = "A" then L11020
            if function$ = "D" then L11060

        REM *************************************************************~
            *       E D I T     M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        editmode
            pf16$ = "(16)Return"
            errormsg$, pf1$,pf8$,pf9$ = " "
            function$ = "C"
            gosub'051(5%)
            gosub set_pf_prompts
            if remainder > 0 then pf11$ = "(11)Add S/N's"                ~
                             else pf11$ = " "
            if qtyassigned>0 then pf12$ = "(12)Delete S/N's"             ~
                             else pf12$ = " "
            pf1$ = "(1)Restore/Undo"
            gosub'101(5%)     /* Display & Accept Screen    */
               if keyhit% = 1% then gosub undo
               if keyhit% = 2% then o% = 1%
               if keyhit% = 3% then o% = max(1%,m%-23%)
               if keyhit% = 4% then o% = max(1%,o%-23%)
               if keyhit% = 5% then o% = max(1%,min(m%-23%,o%+23%))
               if keyhit% = 6% then o% = max(1%,o%-1%)
               if keyhit% = 7% then o% = max(1%,min(m%-23%,o%+1%))
               if keyhit% =11% and remainder > 0 then L11020
               if keyhit% =12% and qtyassigned > 0 then L11060
               if keyhit% =16% then datasave
               if keyhit% <>0% then editmode
               if cursor%(1) < 8% or cursor%(1) > 19% then editmode
               l% = o% + cursor%(1) - 8%
               if cursor%(2) > 38% then l% = l% + 12%
               if l% > m% then editmode
               if serial$(l%) = " " then editmode
             gosub'051(6%)
               if enabled% = 0% then editmode
               pf1$,pf2$,pf3$,pf4$,pf5$,pf6$,pf7$,pf11$,pf12$,pf16$ = " "
            plowkey$ = str(part$) & serial$(l%)
            call "READ101" (#3, plowkey$, f1%(3))
            if f1%(3) = 0% then L10920
            if str(key(#3,2),,1%) = "7" then L10840 /* Is it in TIF yet?*/
            call "STRTRLSE" (#3) /* If not = 7 then must be in TIF file*/
            goto L10920
L10840:     put #3 using L10860, source$, " ", " "  /* back into source */
L10860:     FMT CH(1), POS(216), CH(2), CH(40)
            rewrite #3

L10920:     gosub'101(6%)           /* Display & Accept Screen    */
            gosub'151(6%)     /* Edit Field for Valid Entry */
            if errormsg$ > " " then L10920
            goto editmode

L11020:     function$ = "A" : gosub add_serial_numbers : goto editmode

L11060:     function$ = "D" : gosub del_serial_numbers : goto editmode


        add_serial_numbers
            pf11$, pf12$, pf16$ = " "
            pf1$ = "(1)Exit Add Mode"
            gosub'051(2%)      /* Add Range ?  */
            if enabled% = 0% then L11660  /* Normal Adds      */
L11220:     gosub'101(fieldnr%)     /* Display & Accept Screen    */
               if keyhit% = 1% then return
            gosub'151(2%)     /* Edit Field for Valid Entry */
               if errormsg$ > " " then L11220
L11320:        for fieldnr% = 3% to 4%
L11340:            gosub'051(fieldnr%)
                      if enabled% = 0% then L11580
                      if fieldnr% > 2% then pf4$ = "(4)Prior"            ~
                                       else pf4$ = " "
L11380:            gosub'101(fieldnr%)
                      if keyhit% <> 1% then L11480
                         from_serial$, to_serial$ = " "
                         fieldnr% = 99%
                         goto L11580
L11480:               if keyhit% <> 4% then L11522
                         fieldnr% = max(2%, fieldnr% - 1%)
                         goto L11340
L11522:               if keyhit% <> 0% then L11380
                   gosub'151(fieldnr%)
                      if errormsg$ > " " then L11380
L11580:         next fieldnr%
                from_serial$, to_serial$, pf4$ = " "
                if fieldnr% = 99% then return
                if range$ = "Y" then L11320
*          one at a time entry/selection
L11660:     gosub'051(6%)      /* Check Qty's, set input message   */
               if enabled% = 0% then return
L11760:     gosub'101(fieldnr%)     /* Display & Accept Screen    */
               if keyhit% = 1% then serial$(l%) = " "
               if keyhit% = 1% then return
            gosub'151(6%)     /* Edit Field for Valid Entry */
               if errormsg$ > " " then L11760
            qtyassigned = l%
            call "CONVERT" (qtyassigned,-.0001,qtyassigned$)
            remainder = qty - qtyassigned
            call "CONVERT" (remainder,-.0001,remainder$)
            m% = l%
            goto L11660


        set_pf_prompts
            if o% = 1% then L12100
               pf2$ = "(2)First" : pf4$ = "(4)Prev" : pf6$ = "(6)Up"
                goto L12120
L12100:        pf2$, pf4$, pf6$ = " "
L12120:     if o% >= m% - 23% then L12180
               pf3$ = "(3)Last " : pf5$ = "(5)Next" : pf7$ = "(7)Down"
               return
L12180:        pf3$, pf5$, pf7$ = " "
            return

        del_serial_numbers
            gosub'051(5%)      /* Check if anything to Delete      */
               if enabled% = 0% then return
L12300:        gosub set_pf_prompts
               pf1$ = "(1)Exit Delete Mode"
               pf11$, pf12$, pf16$ = " "
               pf8$ = "(8)'X' All S/N's"
               pf9$ = "(9)Reset to ' '"
            gosub'101(5%)     /* Display & Accept Screen    */
               if keyhit% = 1% then return
               if keyhit% = 2% then o% = 1%
               if keyhit% = 3% then o% = max(1%,m%-23%)
               if keyhit% = 4% then o% = max(1%,o%-23%)
               if keyhit% = 5% then o% = max(1%,min(m%-23%,o%+23%))
               if keyhit% = 6% then o% = max(1%,o%-1%)
               if keyhit% = 7% then o% = max(1%,min(m%-23%,o%+1%))
               if keyhit% = 8% then s$() = all("X")
               if keyhit% = 9% then s$() = all(" ")
               if keyhit% <>0% then L12300

        begin_delete
               x% = m%
               for l% = 1% to m%
                   if s$(l%) <> "X" then L12800
                   plowkey$ = str(part$) & serial$(l%)
                   call "READ101" (#3, plowkey$, f1%(3))
                   if f1%(3) = 0% then L12760
                   if str(key(#3,2),,1%) <> "7" then L12760
                      put #3 using L12750, source$, " ", " "
L12750:               FMT CH(1), POS(216), CH(2), CH(40)
                      rewrite #3
L12760:            serial$(l%) = " "
                   x% = x% - 1%
L12800:         next l%
                call "STRTRLSE" (#3)
                call "LINSMASH" (serial$())
                init(" ")s$()
                m% = x%
                qtyassigned = m%
                call "CONVERT" (qtyassigned,-.0001,qtyassigned$)
                remainder = qty - qtyassigned
                call "CONVERT" (remainder,-.0001,remainder$)
                if o% > m% - 23% then o% = max(1%, m%-23%)
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
            on fieldnr% gosub L20320,         /* Function Selection */    ~
                              L20420,         /* Add Range? (Y/N)   */    ~
                              L20600,         /* From Range         */    ~
                              L20760,         /* To Range           */    ~
                              L20900,         /* Selection Block    */    ~
                              L21140          /* Serial Number      */
            return
L20320: REM Def/Enable Function Code (Add,Chg,Del) FUNCTION$
            function$ = "C"
            if qtyassigned < qty then function$ = "A"
            if qtyassigned > qty then function$ = "D"
            return
L20420: REM Def/Enable Add Range? (Y/N)            RANGE$
            range$ = "N"
            if remainder < 2 then enabled% = 0%
            if remainder < 2 then return
            inpmessage$ = "Enter 'Y' to Assign a Range of S/N's " &      ~
                          "or 'N' to enter them 1 at a time."
            range$ = "Y"
            return
L20600: REM Def/Enable From S/N Range              FROM_SERIAL$
            if remainder < 1 then L20640
            if range$ = "Y" then L20700
L20640:        from_serial$ = " "
               range$ = "N"
               enabled% = 0%
               return
L20700:     inpmessage$ = "Enter the beginning S/N in the Range " &      ~
                          "to Select"
            return
L20760: REM Def/Enable To S/N Range                TO_SERIAL$
            if range$ = "Y" then L20842
               to_serial$ = " "
               enabled% = 0%
               return
L20842:     if m% >= qty then enabled% = 0%
            if m% >= dim(serial$(),1)-1% then enabled% = 0%
            if enabled% = 0% then return
            o% = max(1%,m%-22%)
            l% = max(1%, m%+1%)
            inpmessage$ = "Enter the ending S/N in the Range to Select"
            if remainder > 1 then L20882
               to_serial$ = from_serial$
               return
L20882:     serial$(l%) = from_serial$
            q% = remainder - 1
            gosub increment_number
            to_serial$ = serial$(l%)
            serial$(l%) = " "
            return
L20900: REM Def/Enable Selection Block             S$(12)
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
L21140: REM Def/Enable Serial Number               SERIAL$(12)
            inpmessage$ = "Change the Serial Number Above"
            if function$ = "C" then return
            if m% >= qty then enabled% = 0%
            if m% >= dim(serial$(),1)-1% then enabled% = 0%
            if enabled% = 0% then return
            o% = max(1%,m%-22%)
            l% = max(1%, m%+1%)
            inpmessage$ = "Enter the Serial Number to Assign"
            if serial$(l%) = " " and l% > 1% then                        ~
               serial$(l%) = serial$(l%-1%)
            q% = 1%
            if serial$(l%) > " " then gosub increment_number
            return

        increment_number
            if p%(1%) = 0% then return
            x%,y%,b% = 0%

            REM *** Construct the Number to Increment ***
L21980:     if p%(y%+1%) = 0% then L22140
            a% = 0%
            y% = y% + 1%
            convert str(serial$(l%),p%(y%),1%) to a%, data goto L22060
L22060:     b% = b% + a% * 10%^x%
            x% = x% + 1%
            if y% >= 14% then L22140
            goto L21980
L22140:     nxt = b% + q%
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
            mat redim mfac$(max_qty%+1%)1,                               ~
                      n$(max_qty%+1%)6,                                  ~
                      nfac$(max_qty%+1%)1,                               ~
                      s$(max_qty%+1%)1,                                  ~
                      serial$(max_qty%+1%)20%,                           ~
                      sfac$(max_qty%+1%)1

            init(" ") inpmessage$,                                       ~
                      range$                 , /* Add Range? (Y/N)   */  ~
                      from_serial$,to_serial$, /* Range Values       */  ~
                      s$()                   , /* Selection Block    */  ~
                      serial$()              , /* Serial Numbers     */  ~
                      pf1$,pf2$,pf3$,pf4$,pf5$,pf6$,pf7$,pf11$,pf12$
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
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************
        undo
            u3% = 2%
            call "ASKUSER" (u3%, "*** UNDO ***",                         ~
                 "Press PF (1) to Return to Display", "- OR -",          ~
            "Press RETURN to UnDo the Effects of The Current Entries")
            if u3% = 1% then return
            if u3% <> 0% then undo
            init("X")s$() : undo% = 1%
            gosub begin_delete   /* erase contents of current array */
            gosub dataload       /* restore from work file          */
            undo% = 0%
            return

        REM *************************************************************~
            *   L O A D   E N T R I E S   F R O M   W O R K    F I L E  *~
            *-----------------------------------------------------------*~
            * Reads passed in Work File for any S/N's previously entered*~
            * for the passed in index pointer and loads them into an    *~
            * array automatically sorted.                               *~
            *************************************************************
        dataload
            plowkey$ = bin(index%,3)
            m% = 0%
            l% = 1%
L30110:     call "PLOWNEXT" (#4, plowkey$, 3%, f1%(4))
            if f1%(4) = 0% then L30340
            m% = m% + 1%
            if m% > dim(serial$(),1)-1% then L30340
            serial$(m%) = str(plowkey$,4%)
            if undo% = 0% then L30110

            REM *** Reset Status Flag to '7' if need to ***
            readkey$ = str(part$) & serial$(m%)
            call "READ101" (#3, readkey$, f1%(3))
            if f1%(3) = 0% then L30240  /* shouldn't happen! */
            if str(key(#3,2),,1%) <> source$ then L30210
            put #3 using L30205, "7", trantype$, trankey$, source$,       ~
                                location$
            rewrite #3
            goto L30110
L30205:     FMT CH(1), POS(216), CH(2), CH(40), CH(1), CH(30)
L30210:     REM *** Is S/N now owned by another transaction ? ***
            if str(key(#3,2),,1%) < hex(40) then L30240
            get #3 using L30235, ttype$, tkey$
            if ttype$ <> trantype$ or tkey$ <> trankey$ then L30240
            goto L30110
L30235:     FMT POS(216), CH(2), CH(40)
L30240:     REM *** This one's no longer available - grabbed by ? ***
            serial$(m%) = " "
            m% = m% - 1%
            call "STRTRLSE" addr(#3)
            goto L30110

L30340:     qtyassigned = m%
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
              if function$ = "D" and errormsg$=" " then str(errormsg$,22)~
                = hex(84) & "Please Note:  You Are Now in DELETE Mode"
              if source$ = "1" then line2$ = line2$ & "  Job " & location$
              if source$ = "2" then line2$ = line2$ & " Store " &        ~
                 str(location$,,3%) & " Lot " & str(location$,4%)
              if source$ = "2" and str(location$,4%,16%) = " " then      ~
                 line2$ = "For Part " & part$ & " Store " &              ~
                          str(location$,,3%)
              if source$ = "3" and trantype$ <> "RJ" then line2$ = line2$~
                 & " Job: " & str(location$,,8%)                         ~
                 & " S/N " & str(location$,9%,20%)
              if source$ = "3" and trantype$ = "RJ" then line2$ = line2$ ~
                 & " From Job: " & str(location$,,8%)
              if source$ = "4" then line2$ = line2$ & " Customer "  &    ~
                 str(location$,,9%)
              if source$ = "4" and str(location$,10%) > " " then         ~
                 line2$ = line2$ & " Invoice " & str(location$,10%,8%)
              if source$ = "4" and str(location$,18%) > " " then         ~
                 line2$ = line2$ & " Line " & str(location$,18%)
              o% = max(1%, o%)
              str(line2$,62%) = "SERSELCT: " & str(cms2v$,,8%)
              init(hex(8c)) lfac$(),mfac$()
              init(hex(9c)) sfac$(),nfac$(), sfac$()
              if function$ = "A" then init(hex(8c))str(nfac$(),,m%+1%)   ~
                                 else init(hex(8c))str(nfac$(),,m%)
              on fieldnr% gosub      ,         /* Function Code     */   ~
                                L40410,         /* Add Range? (Y/N)  */   ~
                                L40410,         /* From Range        */   ~
                                L40410,         /* To Range          */   ~
                                L40440,         /* Selection Blocks  */   ~
                                L40430          /* Serial Numbers    */
              goto L40480

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40410:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */
L40430:           mfac$(l%)       = hex(81)  :  return  /* Upper Only */
L40440:           if function$ = "C" then init(hex(86))str(sfac$(),,m%)  ~
                                     else init(hex(81))str(sfac$(),,m%)
                  return

L40480:     accept                                                       ~
               at (01,02),                                               ~
                  "Enter/Select Serial Numbers Used",                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(84)), infomsg$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02),                                               ~
        "Total Quantity To Select =",                                    ~
               at (05,29), fac(hex(8c)), qty$                   , ch(06),~
               at (05,36),                                               ~
        "S/N's Selected =",                                              ~
               at (05,53), fac(hex(8c)), qtyassigned$           , ch(06),~
               at (05,60),                                               ~
        "Remainder =",                                                   ~
               at (05,73), fac(hex(84)), remainder$             , ch(06),~
               at (06,02),                                               ~
        "Enter Ranges? (Y/N):",                                          ~
               at (06,23), fac(lfac$(02)), range$               , ch(01),~
               at (06,26), "From:"                              ,        ~
               at (06,32), fac(lfac$(03)), str(from_serial$,,ll%),       ~
               at (06,53), "To:"                                ,        ~
               at (06,57), fac(lfac$(04)), str(to_serial$,,ll%) ,        ~
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

               if keyhit% <> 13 then L41680
                  call "MANUAL" ("SERSELCT")
                  goto L40480

L41680:        if keyhit% <> 15 then L41720
                  call "PRNTSCRN"
                  goto L40480

L41720:           close ws
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
                              L50150,         /* Add Range? (Y/N)  */     ~
                              L50190,         /* From Range        */     ~
                              L50460,         /* To   Range        */     ~
                              L50770,         /* Selection Block   */     ~
                              L50790          /* Serial Number     */
            return
L50150: REM Test for Add Range? (Y/N)             RANGE$
            if pos("YN" = range$) > 0% then return
            errormsg$ = "Must be Y or N"
            return
L50190: REM Test for From S/N Range               FROM_SERIAL$
            mat incl_excl = zer  : incl_excl$() = " "
            brk% = 31% : kee = 2.80 : dlen = 20
            incl_excl(2) = 52.25 : incl_excl$(2) = part$  /* Part Nbr  */
            if trantype$ = "IR" or trantype$ = "RJ" then                 ~
               gosub'88(from_serial$)                                    ~
            else                                                         ~
            plowkey$ = source$ & str(location$) & str(from_serial$) &    ~
                       part$
            descr$ = hex(06) & "Select the FROM Serial Number Range Entry"
            f1%(3) = -(cursor%(1))
            call "PLOWCODE" (#3, plowkey$, descr$,9000%+brk%, kee,f1%(3),~
                             header$(),dlen,0,incl_excl(), incl_excl$(), ~
                             "Y", " ", #5, descr_map())
            if f1%(3) = 0% then errormsg$ = "S/N Not Found in Inventory "~
                     & "for the given Location!"
            from_serial$ = str(plowkey$,brk%+1%,20%)
            return
L50460: REM test for To S/N Range                 TO_SERIAL$
            mat incl_excl = zer  : incl_excl$() = " "
            incl_excl(1) = 32.20 : incl_excl$(1) = "<z>" & from_serial$
            incl_excl(2) = 52.25 : incl_excl$(2) = part$  /* Part Nbr  */
            test% = 30%
            if trantype$ = "IR" or trantype$ = "RJ" then                 ~
               gosub'88(to_serial$)                                      ~
            else                                                         ~
            plowkey$ = source$ & str(location$) & str(to_serial$) &      ~
                       part$
            descr$ = hex(06) & "Select the TO Serial Number Range Entry"
            f1%(3) = -(cursor%(1))
            call "PLOWCODE" (#3, plowkey$, descr$,9000%+brk%,kee, f1%(3),~
                             header$(),dlen,0,incl_excl(), incl_excl$(), ~
                             "Y", " ", #5, descr_map())
            if f1%(3) = 0% then errormsg$ = "S/N Not Found or Invalid "  ~
                     & "for the given Source"
            if f1%(3) = 0% then return
            to_serial$ = str(plowkey$,brk%+1%)
            q% = remainder : serial$(l%) = from_serial$
            gosub assign_serial_numbers
            if q% < remainder then L50690
               serial$(l%) = " "
               l% = max(1%, m%)
               errormsg$ = "Unable to assign any S/N's within the given "~
                         & "Range!"
               goto L50750
L50690:     m% = l%
            o% = max(1%, m%-22%)
            remainder = q%
            call "CONVERT" (remainder, -.0001, remainder$)
            qtyassigned = l%
            call "CONVERT" (qtyassigned,-.0001,qtyassigned$)
L50750:     call "STRTRLSE" addr(#3)
            return
L50770: REM Test for Selection Block              S$(12)
            return
L50790: REM Test for Serial Number                SERIAL$(12)
            test% = 30%
            REM *** Still in Work File? ***
            plowkey$ = bin(index%,3) & serial$(l%)
            call "READ100" (#4, plowkey$, f1%(4))
            if f1%(4) = 0% then L50960
            plowkey$ = str(part$) & serial$(l%)
            call "READ100" (#3, plowkey$, f1%(3)) /* Still Available? */
            if f1%(3) = 0% then L50960
            if str(key(#3,2),,1%) < hex(40) then L50960
            to_serial$ = serial$(l%)
            get #3 using L50940, ttype$, tkey$
            if ttype$ = trantype$ and tkey$ = trankey$ then L51110
            if str(key(#3,2),,1%) = source$ then L51110
            to_serial$ = " "
L50940:     FMT POS(216), CH(2), CH(40)

L50960:     REM *** Look for it in Master File in current Source Location
            mat incl_excl = zer  : incl_excl$() = " "
            brk% = 31% : kee = 2.80 : dlen = 20
            incl_excl(2) = 52.25 : incl_excl$(2) = part$  /* Part Nbr  */
            if trantype$ = "IR" or trantype$ = "RJ" then                 ~
                gosub'88(serial$(l%))                                    ~
            else                                                         ~
            plowkey$ = source$ & str(location$) & str(serial$(l%)) &     ~
                       part$
            descr$ = hex(06) & "Select the Serial Number to Assign"
            f1%(3) = -(cursor%(1))
            call "PLOWCODE" (#3, plowkey$, descr$,9000%+brk%,kee, f1%(3),~
                             header$(),dlen,0,incl_excl(), incl_excl$(), ~
                             "Y", " ", #5, descr_map())
            if f1%(3) = 0% then errormsg$ = "S/N Not Found or Invalid "  ~
                     & "for the given Source"
            if f1%(3) = 0% then return
            serial$(l%), to_serial$ = str(plowkey$,brk% + 1%)
L51110:     q% = 1%
            gosub assign_serial_numbers
            to_serial$ = " "
            if q% > 0% then errormsg$ = "Unable to Assign this S/N " &   ~
                            "(Somebody just snatched it!)"
            call "STRTRLSE" addr(#3)
            return

        assign_serial_numbers
            first% = 1%
            plowkey$ = str(part$) & serial$(l%)
            call "READ101" (#3, plowkey$, f1%(3))
            if f1%(3) = 0% then return
            get #3 using L50940, ttype$, tkey$
            if ttype$ <> trantype$ or tkey$ <> trankey$ then L51270
            if str(key(#3,2),,1%) > hex(40) then L51320
L51270:     if str(key(#3,2),,1%) <> source$ then L51370
            if test% < 2% then L51290
            if str(key(#3,2),2%,test%) <> location$ then L51370
L51290:     put #3 using L51410, "7", trantype$, trankey$, source$,       ~
                                     location$
            rewrite #3
L51320:     if first% = 0% then l% = l% + 1%
            serial$(l%) = str(plowkey$,26%)
            q% = q% - 1%
            if q% < 1% then return
            first% = 0%
L51370:     call "PLOWNXT1" (#3, plowkey$, 25%, f1%(3))
            if f1%(3) = 0% then return
            if str(plowkey$,26%) > to_serial$ then return
            goto L51270
L51410:     FMT CH(1), POS(216), CH(2), CH(40), CH(1), CH(30)

        deffn'88(serial$)
            REM *** Special Handling for IR & RJ Transaction Types ***
            incl_excl(2) = 01.01 : incl_excl$(2) = source$ /* Source   */
            plowkey$ = str(part$) & serial$
            if trantype$ <> "RJ" then L51490 /* Returns from Job to HNY */
               incl_excl(3) = 02.08 : incl_excl$(3) = str(location$,,8%)
                                            /* Job Number    */
L51490:     if source$ <> "4" then L51560
            incl_excl(3) = 02.09 : incl_excl$(3) = str(location$,,9%)
                                            /* Customer Nbr (Required) */
            if str(location$,10%,8%) > " " then  /* Invoice # Present */ ~
            incl_excl(3) = 02.17 : incl_excl$(3) = str(location$,,17%)
            if str(location$,18%,3%) > " " then  /* Invoice Line #    */ ~
            incl_excl(3) = 02.20 : incl_excl$(3) = str(location$,,20%)
L51560:     brk% = 25% : kee = 0.80 : test% = len(incl_excl$(3))
            dlen = 0
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
