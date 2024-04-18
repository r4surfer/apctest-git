        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   EEEEE  RRRR   K   K  IIIII  TTTTT                 *~
            *  S      E      R   R  K  K     I      T                   *~
            *   SSS   EEEE   RRRR   KKK      I      T                   *~
            *      S  E      R   R  K  K     I      T                   *~
            *   SSS   EEEEE  R   R  K   K  IIIII    T                   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SERKIT   - Handles kitting of component parts (lot tracked*~
            *            or serial numbered) to serialized parent parts *~
            *            (part to build in a job).                      *~
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
            * 02/06/87 ! Original                                 ! LDJ *~
            * 04/03/87 ! Initialized ERRORMSG$ to blank on early  ! LDJ *~
            *          !   exits (not a S/N part).                !     *~
            * 08/27/87 ! Fixed 'TYPO' Bug in line 9611 - was      ! LDJ *~
            *          !   exiting routine if component was Lot   !     *~
            *          !   Tracked but was invoking routine if    !     *~
            *          !   component was neither Lot Tracked or   !     *~
            *          !   S/N Tracked - the reverse of how it    !     *~
            *          !   Should Work! (chnaged one (1) byte!).  !     *~
            * 03/15/88 ! Changed to return Error Message if user  ! LDJ *~
            *          !   attempts to kit (or dekit) a serialized!     *~
            *          !   component part to a non-serialized     !     *~
            *          !   parent part.                           !     *~
            * 03/21/89 ! Changed Line 9760 to allow fractional    ! RJM *~
            *          !   Quantities of non-serialized parts to  !     *~
            *          !   be kitted to a serialized Parent part. !     *~
            * 09/01/89 ! Changed max qty allowed for range ea. qty! JDH *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "SERKIT"   (partcode$,       /* Part code to Kit/DeKit To  */~
                                         /* (Part to build in Job).    */~
                        comp_partcode$,  /* Component Part Code to Kit */~
                                         /* or issue to this Job.      */~
                        storeno$,        /* Store to Select From/Issue */~
                                         /* To.                        */~
                        lotno$,          /* Lot to Select From/Issue To*/~
                                         /* (If TRANTYPE$ = JJ then    */~
                                         /*  this arg should be the    */~
                                         /*  Xfer To Job code).        */~
                        qty_build,       /* Qty to Build in Job minus  */~
                                         /* the Qty Reported Complete  */~
                                         /* if Kitting, otherwise must */~
                                         /* contain the total Qty to   */~
                                         /* Build in the Job.          */~
                        qty,             /* Comp.  Qty to Issue / kit. */~
                        index%,          /* Pointer For Work File Use  */~
                                         /* (Line or Sequence Number)  */~
                        avg_lines%,      /* Average # Lines per Documnt*/~
                                         /* (For use in creating Files)*/~
                        trantype$,       /* Source Transaction Type.   */~
                                         /* (JK = Job Kitting).        */~
                                         /* (RJ = Job Returns).        */~
                                         /* (JJ = Job to Job ).        */~
                        jobno$,          /* Job Number to Issue To/From*/~
                        status$,         /* Status of Parent S/N's to  */~
                                         /* Kit To.                    */~
                        source$,         /* Status of Component S/N's  */~
                                         /* to Kit From.               */~
                        op%,             /* Operation to Perform       */~
                                         /* 0% = Normal Input/Edit,    */~
                                         /* 1% = Save Data Pointed to  */~
                                         /*      by INDEX%,            */~
                                         /* 2% = Start Over, if INDEX% */~
                                         /*      > 0% then only the    */~
                                         /*      line/data pointed to  */~
                                         /*      by INDEX% is cleared, */~
                                         /*      else ALL is cleared.  */~
                        errormsg$,       /* Returned Error Message     */~
                                         /* (If 1st char = '*' then    */~
                                         /* remainder assumed to be an */~
                                         /* info message).             */~
                        #1,              /* SYSFILE2 UFB               */~
                        #2,              /* HNYMASTR UFB               */~
                        #3,              /* SERMASTR UFB               */~
                        #7,              /* SERTIF   UFB               */~
                        #6)              /* SERWORK  UFB               */

        dim                                                              ~
            comp_loc$30,                 /* Component S/N's Location   */~
            comp_part$25,                /* Component Part             */~
            comp_partcode$,              /* Component Part             */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr$80,                    /* PLOWCODE Argument          */~
            errormsg$79,                 /* Error message              */~
            format$20,                   /* Serial Number Format Code  */~
            from_serial$20,              /* Beginning S/N Range        */~
            function$1,                  /* Function Code (Add,Chg,Del)*/~
            header$(2)80,                /* PLOWCODE Argument          */~
            i$(24)80,                    /* Screen Image               */~
            incl_excl(1002),             /* PLOWCODE Argument          */~
            index%(1001),                /* SERWORK Index Pointers     */~
            infomsg$79,                  /* Informational Message      */~
            inpmessage$79,               /* Informational Message      */~
            job$8,                       /* Production Job Number      */~
            jobno$8,                     /* Production Job Number      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            line7$79,                    /* Column Header on Line 7    */~
            location$30,                 /* S/N Location to Select from*/~
            lot$16,                      /* Lot Code                   */~
            lotno$16,                    /* Lot Code                   */~
            mfac$(1001)1,                /* Field Attribute Characters */~
            msg5$25,                     /* Line 5 Message text        */~
            msg6$17,                     /* Line 6 Message text        */~
            n$(1001)6,                   /* Screen Sequence Numbers    */~
            next$14,                     /* Next Serial Number Digits  */~
            nfac$(1001)1,                /* Field Attribute Characters */~
            p%(15),                      /* Search Receiver array      */~
            part$25,                     /* Part code                  */~
            partcode$25,                 /* Part code                  */~
            pf16$16,pf11$16,pf12$16,     /* PF Key Screen Literals     */~
            pf2$8,pf3$8,pf4$8,pf5$8,     /* PF Key Screen Literals     */~
            pf6$8,pf7$8,pf1$16,pf8$,pf9$,/* PF Key Screen Literals     */~
            plowkey$100,                 /* Miscellaneous Read/Plow Key*/~
            qfac$(1001)1,                /* Field Attribute Characters */~
            qty$10,                      /* Total Quantity to Assign   */~
            qty$(1001)8,                 /* Quantity Distribution      */~
            qty_kitted$10,               /* Serial Numbers Assigned    */~
            qty_each$8,                  /* Each qty for range asgnmnt */~
            range$1,                     /* Add Range? (Y/N)           */~
            readkey$100,                 /* Miscellaneous Read/Plow Key*/~
            remainder$10,                /* Remainder to Assign        */~
            s$(1001)1,                   /* Selection Block            */~
            serial$(1002)20,             /* Serial Numbers             */~
            sfac$(1001)1,                /* Field Attribute Characters */~
            source$1,                    /* Status of Component S/N's  */~
            status$1,                    /* Status to Change S/N to.   */~
            stat$1,                      /* Temporary Status Code      */~
            store$3,                     /* Store Code                 */~
            storeno$,                    /* Store Code                 */~
            tojob$8,                     /* Production Job Number      */~
            to_serial$20,                /* Ending    S/N Range        */~
            trantype$2,                  /* Source Transaction Type.   */~
            trankey$40,                  /* Source Transaction Key.    */~
            userid$3                     /* Current User Id            */~

        dim f1%(08)                      /* = 1 if READ was successful */~

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
            * # 4 ! SERWORK2 ! Temporary System Workfile                *~
            * # 5 ! TEMPFILE ! Not Used - Place holder for PLOWCODE     *~
            * # 6 ! SERWORK  ! Temporary System Workfile                *~
            * # 7 ! SERTIF   ! Temporary Image File for S/N's Trans     *~
            *************************************************************~

            select #4,  "SERWORK2",                                      ~
                        varc,     indexed,  recsize =  60 ,              ~
                        keypos =    1, keylen =  23

            select #5,  "TEMPFILE",                                      ~
                        varc,     indexed,  recsize =  4  ,              ~
                        keypos =    1, keylen =  4

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            part$ = partcode$ : o%=1% : job$, location$ = jobno$
            comp_part$ = comp_partcode$
            store$ = storeno$ : lot$, tojob$ = lotno$
            if userid$ > " " then L09290
            call "EXTRACT" addr("ID", userid$)
            call "OPENCHCK" (#3, 0%, 0%, 0%, "                      ")
            date$ = date
            call "DATEFMT" (date$)
            line7$ = "Seq #  Parent Serial Nbr.     Dist Qty  Seq #  " & ~
                     "Parent Serial Nbr.     Dist Qty"
            for x% = 1% to dim(n$(),1)
                put n$(x%) using L09180 , x%
            next x%
L09180:     %#####)
            max_qty% = 100%
            if fs(#1) = "00" and key(#1) = "SWITCHS.HNY" then L09240
            plowkey$ = "SWITCHS.HNY"
            call "READ100" (#1, plowkey$, f1%(1))
            if f1%(1) = 0% then L09260
L09240:     get #1 using L09250 , format$, max_qty%
L09250:     FMT POS(46), CH(20), POS(86),BI(2)
L09260:     max_qty% = min(max_qty%, dim(serial$(),1) - 1%)
            mat p% = zer
            search -format$ = "+" to p%()
L09290:     if trantype$ <> "RJ" and trantype$ <> "JJ" then L09350
            header$(1) = "  Serial Numbers Currently in Process or Built"~
                       & " by Job: " & job$
            msg5$ = "Total Quantity To DEKIT ="
            msg6$ = "Quantity Entered="
            goto L09400
L09350:     header$(1) = "  Serial Numbers Currently in Process in Job: "~
                       & job$
            msg5$ = "Total Quantity To ISSUE ="
            msg6$ = "Quantity Issued ="

L09400:     call "SERENABL" (part$,           /* Part Number to Check  */~
                             enabled%,        /* Enable Flag to Set    */~
                             ll%,             /* Maximum S/N Field Len */~
                             #1,              /* SYSFILE2 UFB          */~
                             #2)              /* HNYMASTR UFB          */

            call "SERENABL" (comp_part$,      /* Part Number to Check  */~
                             c_enabled%,      /* Enable Flag to Set    */~
                             ll%,             /* Maximum S/N Field Len */~
                             #1,              /* SYSFILE2 UFB          */~
                             #2)              /* HNYMASTR UFB          */

            if enabled% = 1% then L09560
               if c_enabled% = 1% then L09541
                  errormsg$ = " " : goto exit_routine
L09541:        errormsg$ = "Sorry, you cannot KIT a SERIALIZED " &       ~
                           "Component to a NON-SERIALIZED Parent Part!"
               goto exit_routine

L09560:     if c_enabled% = 1% then L09670

            call "LOTENABL" (comp_part$,      /* Part Number to Check  */~
                             enabled%,        /* Enable Flag to Set    */~
                             u3%,             /* Maximum Lot Field Len */~
                             #1,              /* SYSFILE2 UFB          */~
                             #2)              /* HNYMASTR UFB          */

            if enabled% <> 0% or op% = 2% or index% = 0% then L09670
               errormsg$ = " " : goto exit_routine

L09670:     if str(errormsg$,,1%) = "*" then infomsg$ = str(errormsg$,2%)~
                                        else infomsg$ = " "
            gosub initialize_variables
            if qty <= max_qty% or c_enabled% = 0% then L09750
               errormsg$ = "Quantity may not exceed##### on Parts " &    ~
                           "requiring Serial Numbers!"
               convert max_qty% to str(errormsg$,24%,5%),pic(#####)
               goto exit_routine
L09750:     call "SERMKWRK" (#1, avg_lines%, #4)
        REM      CALL "CONVERT" (QTY,-.0001,QTY$)
            if c_enabled% = 1% then call "CONVERT" (qty,-0.001,qty$)     ~
               else call "CONVERT" (qty,-0.2,qty$)
            gosub dataload
            on op% goto savedata, startover
            if qty = 0 then startover
            gosub'051(1%)
            if function$ = "A" then L11440
            if function$ = "D" then L11460

        REM *************************************************************~
            *       E D I T     M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        editmode
            pf16$ = "(16)Return"
            errormsg$, pf1$,pf8$,pf9$ = " "
            function$ = "C"
            gosub'051(6%)
            gosub set_pf_prompts
            if remainder > 0 then pf11$ = "(11)Add S/N's"                ~
                             else pf11$ = " "
            if qty_kitted >0 then pf12$ = "(12)Delete S/N's"             ~
                             else pf12$ = " "
            gosub'101(6%)     /* Display & Accept Screen    */
               if keyhit% = 2% then o% = 1%
               if keyhit% = 3% then o% = max(1%,m%-23%)
               if keyhit% = 4% then o% = max(1%,o%-23%)
               if keyhit% = 5% then o% = max(1%,min(m%-23%,o%+23%))
               if keyhit% = 6% then o% = max(1%,o%-1%)
               if keyhit% = 7% then o% = max(1%,min(m%-23%,o%+1%))
               if keyhit% =11% and remainder > 0 then L11440
               if keyhit% =12% and qty_kitted  > 0 then L11460
               if keyhit% =16% then datasave
               if keyhit% <>0% then editmode
               if cursor%(1) < 8% or cursor%(1) > 19% then editmode
               l% = o% + cursor%(1) - 8%
               if cursor%(2) > 40% then l% = l% + 12%
               if l% > m% then editmode
               if serial$(l%) = " " then editmode
               if cursor%(2) < 31% then fieldnr% = 7% else fieldnr% = 8%
               if cursor%(2) >= 48% and cursor%(2) < 70% then fieldnr%=7%

            gosub'051(fieldnr%)
               if enabled% = 0% then editmode
               pf1$,pf2$,pf3$,pf4$,pf5$,pf6$,pf7$,pf11$,pf12$,pf16$ = " "
            REM *** No going back (No StartOver or Cancel)
L11390:     gosub'101(fieldnr%)     /* Display & Accept Screen    */
            gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
            if errormsg$ > " " then L11390
            goto editmode

L11440:     function$ = "A" : gosub add_serial_numbers : goto editmode

L11460:     function$ = "D" : gosub del_serial_numbers : goto editmode


        add_serial_numbers
               pf11$, pf12$, pf16$ = " "
               pf1$ = "(1)Exit Add Mode"
            gosub'051(2%)      /* Add Range ?  */
               if enabled% = 0% then L11840  /* Normal Adds      */
L11540:     gosub'101(fieldnr%)     /* Display & Accept Screen    */
               if keyhit% = 1% then return
            gosub'151(2%)     /* Edit Field for Valid Entry */
               if errormsg$ > " " then L11540
               if range$ = "N" then L11840  /* Normal Adds      */

            REM *** Add Range(s) ***
L11610:     for fieldnr% = 3% to 5%
L11620:         gosub'051(fieldnr%)
                   if enabled% = 0% then L11690
                      if fieldnr% > 2% then pf4$ = "(4)Prior"            ~
                                       else pf4$ = " "
L11660:         gosub'101(fieldnr%)
                   if keyhit% <> 1% then L11730
                      fieldnr% = 99%
L11690:               range$,from_serial$, to_serial$, qty_each$ = " "
                      if enabled% = 0% and fieldnr% = 2% then            ~
                         fieldnr% = 99%
                      goto L11790
L11730:            if keyhit% <> 4% then L11760
                      fieldnr% = max(2%, fieldnr% - 1%)
                      goto L11620
L11760:            if keyhit% <> 0% then L11660
                gosub'151(fieldnr%)
                   if errormsg$ > " " then L11660
L11790:      next fieldnr%
             from_serial$, to_serial$, pf4$ = " "
             if range$ = "Y" then L11610
             if fieldnr% = 99% then return

L11840:     REM ****   one at a time entry/selection  ***
            for fieldnr% = 7% to 8%
L11860:         gosub'051(fieldnr%)
                   if enabled% = 0% then L11930
                      if fieldnr% > 7% then pf4$ = "(4)Prior"            ~
                                       else pf4$ = " "
L11900:         gosub'101(fieldnr%)
                   if keyhit% <> 1% then L11980
                      serial$(l%), qty$(l%) = " "
L11930:               fieldnr% = 99%
                      readkey$ = str(trantype$,,2%) & str(job$)          ~
                                         & str(plowkey$,,23%)
                      gosub clear_components_loop
                      goto L12070
L11980:            if keyhit% <> 4% then L12040
                      readkey$ = str(trantype$,,2%) & str(job$)          ~
                                         & str(plowkey$,,23%)
                      gosub clear_components_loop
                      fieldnr% = max(7%, fieldnr% - 1%)
                      goto L11860
L12040:            if keyhit% <> 0% then L11900
                gosub'151(fieldnr%)
                   if errormsg$ > " " then L11900
L12070:      next fieldnr%
             if fieldnr% = 99% then return
             goto L11840


        set_pf_prompts
            if o% = 1% then L12160
               pf2$ = "(2)First" : pf4$ = "(4)Prev" : pf6$ = "(6)Up"
                goto L12170
L12160:        pf2$, pf4$, pf6$ = " "
L12170:     if o% >= m% - 23% then L12200
               pf3$ = "(3)Last " : pf5$ = "(5)Next" : pf7$ = "(7)Down"
               return
L12200:        pf3$, pf5$, pf7$ = " "
            return

        del_serial_numbers
            gosub'051(6%)      /* Check if anything to Delete      */
               if enabled% = 0% then return
L12260:        gosub set_pf_prompts
               pf1$ = "(1)CANCEL Delete"
               pf11$, pf12$, pf16$ = " "
               pf8$ = "(8)'X' All S/N's"
               pf9$ = "(9)Reset to ' '"
            gosub'101(fieldnr%)     /* Display & Accept Screen    */
               if keyhit% = 1% then return
               if keyhit% = 2% then o% = 1%
               if keyhit% = 3% then o% = max(1%,m%-23%)
               if keyhit% = 4% then o% = max(1%,o%-23%)
               if keyhit% = 5% then o% = max(1%,min(m%-23%,o%+23%))
               if keyhit% = 6% then o% = max(1%,o%-1%)
               if keyhit% = 7% then o% = max(1%,min(m%-23%,o%+1%))
               if keyhit% = 8% then s$() = all("X")
               if keyhit% = 9% then s$() = all(" ")
               if keyhit%<> 0% then L12260
               x% = m%
               for l% = 1% to m%
                   if s$(l%) <> "X" then L12530
                   q = 0 : convert qty$(l%) to q, data goto L12460
L12460:            qty_kitted = qty_kitted - q
                   if c_enabled% = 0% then L12510
                      readkey$ = str(trantype$,,2%) & str(job$) &        ~
                                 bin(index%,3) & str(serial$(l%))
                      gosub clear_components_loop
L12510:            serial$(l%), qty$(l%) = " "
                   x% = x% - 1%
L12530:         next l%
                call "LINSMASH" (serial$())
                call "LINSMASH" (qty$())
                init(" ")s$()
                m% = x%
                call "CONVERT" (qty_kitted ,-.2, qty_kitted$ )
                remainder = qty - qty_kitted
                call "CONVERT" (remainder,-.2, remainder$)
                if o% > m% - 23% then o% = max(1%, m%-23%)
                return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves Parent S/N's and distribution quantities to SERWORK2*~
            *************************************************************

        datasave
            errormsg$ = " "
            gosub dataput
            goto exit_routine

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data in SERWORK2 to SERTIF file.  Note that only    *~
            * entries for components that are NOT Serialized are        *~
            * processed here.  By this time Serialized components are   *~
            * already in the SERTIF file.                               *~
            *************************************************************

        savedata
            if c_enabled% = 0% then L19250
               plowkey$ = str(trantype$,,2%) & str(job$) & bin(index%,3)
               stat$ = "3" or hex(40)
               gosub chg_components_loop
               goto exit_routine
L19250:     plowkey$ = bin(index%,3)
            gosub save_parents_loop
            goto exit_routine

        save_parents_loop
            call "PLOWNXT1" (#4, plowkey$, 3%, f1%(4))
            if f1%(4) = 0% then return
            get #4 using L19620, qty_issued
            delete #4
            trankey$ = str(job$) & bin(index%,3) & str(plowkey$,4%,20%)
            if trantype$ = "JK" then serial$(1%) = " "                   ~
                                else serial$(1%) = str(store$) & lot$
            call "SERMKWRK" (#1, avg_lines%, #7)      /* Make SERTIF ? */

L19390:     put #7 using L19680,                                          ~
               trantype$,   /* Identifies type of transaction.         */~
               trankey$,    /* Key to Original Transaction Item        */~
                            /* record.                                 */~
               serial$(1%), /* blank goes here because we're only      */~
                            /* kitting store/lots to a parent S/N.     */~
                            /* (unless we're de-kitting, then it       */~
                            /* contains the orig store & lot codes!).  */~
               comp_part$,  /* Part code                               */~
                            /* Part this S/N is associated with.       */~
               qty_issued,  /* Quantity of Something                   */~
                            /* Quantity = 0     if S/N field contains  */~
                            /* a real S/N. If S/N field contains       */~
                            /* Store&Lot then = Qty Issued.            */~
               " "          /* Filler (Internal, unused space)         */

            write #7, eod goto L19580
            goto save_parents_loop

L19580:     readkey$ = str(trantype$,,2%) & str(trankey$) & serial$(1%)
            call "DELETE" (#7, readkey$, 62%)
            goto L19390

L19620: FMT                 /* FILE: SERWORK2                          */~
            POS(49),        /* Position for Field QTYTOJOB             */~
            PD(14,4)        /* quantity moved to job                   */~
                            /* Quantity of Component Issued/Kitted to  */~
                            /* this Parent Serial Number.              */

L19680: FMT                 /* FILE: SERTIF                            */~
            CH(2),          /* Identifies type of transaction.         */~
            CH(40),         /* Key to Original Transaction Item        */~
                            /* record.                                 */~
            CH(20),         /* Serial Number                           */~
                            /* Serial Number affected by Transaction   */~
            CH(25),         /* Part code                               */~
                            /* Part this S/N is associated with.       */~
            PD(14,4),       /* Quantity of Something                   */~
                            /* Quantity = 0     if S/N field contains  */~
                            /* a real S/N. If S/N field contains       */~
                            /* Store&Lot then = Qty Issued.            */~
            CH(5)           /* Filler (Internal, unused space)         */~

            goto exit_routine

        chg_components_loop
            call "PLOWNEXT" (#7, plowkey$, 13%, f1%(7))
            if f1%(7) = 0% then return
            get #7 using L19940, readkey$              /* Part Code */
            str(readkey$,26%) = str(plowkey$,43%,20%) /* Serial Number */
            call "READ101" (#3, readkey$, f1%(3))
            if f1%(3) = 0% then chg_components_loop
            put #3 using L19950, stat$
            rewrite #3
            goto chg_components_loop
L19940:     FMT POS(63), CH(25)
L19950:     FMT CH(1)

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20180,         /* Function Selection */    ~
                              L20220,         /* Add Range? (Y/N)   */    ~
                              L20330,         /* From Range         */    ~
                              L20440,         /* To Range           */    ~
                              L20620,         /* Qty Each           */    ~
                              L20700,         /* Selection Block    */    ~
                              L20820,         /* Serial Number      */    ~
                              L21030          /* Distribution Qty   */
            return
L20180: REM Def/Enable Function Code (Add,Chg,Del) FUNCTION$
            function$ = "C"
            if qty_kitted  = 0 then function$ = "A"
            return
L20220: REM Def/Enable Add Range? (Y/N)            RANGE$
            range$ = "N"
            if remainder < 2 then L20310
            if qty_build - m% < 2 then L20310
            if c_enabled% = 1% then L20310
            inpmessage$ = "Enter 'Y' to Issue to a Range of S/N's " &    ~
                          "or 'N' to enter them 1 at a time."
            range$ = "Y"
            return
L20310:        enabled% = 0%
               return
L20330: REM Def/Enable From S/N Range              FROM_SERIAL$
            if remainder < 1 then L20370
            if m% >= qty_build then L20370
            if range$ = "Y" then L20410
L20370:        from_serial$ = " "
               range$ = "N"
               enabled% = 0%
               return
L20410:     inpmessage$ = "Enter the beginning Parent S/N in the Range " ~
                        & "to Kit To"
            return
L20440: REM Def/Enable To S/N Range                TO_SERIAL$
            if range$ = "Y" then L20490
               to_serial$ = " "
               enabled% = 0%
               return
L20490:     o% = max(1%,m%-22%)
            l% = max(1%, m%+1%)
            inpmessage$ = "Enter the ending Parent S/N in the Range to " ~
                        & "Kit To"
            if qty_build - m% > 1 then L20560
               to_serial$ = from_serial$
               return
L20560:     serial$(l%) = from_serial$
            q% = qty_build - (m% + 1%)
            gosub increment_number
            to_serial$ = serial$(l%)
            serial$(l%) = " "
            return
L20620: REM Def/Enable Qty Each                    QTY_EACH$
            if range$ = "Y" then L20670
               qty_each$ = " "
               enabled% = 0%
               return
L20670:     inpmessage$ = "Enter the Qty to Issue to Each S/N in the " & ~
                          "given Range"
            return
L20700: REM Def/Enable Selection Block             S$()
            if m% = 0% then enabled% = 0%
            if function$ = "D" then inpmessage$ =                        ~
               "Enter an 'X' in the Selection Block Next to each S/N " & ~
               "to DELETE and press RETURN."
            if function$ = "C" then inpmessage$ =                        ~
               "Position cursor to the S/N or Qty to CHANGE and press " &~
               "RETURN or PF16 to exit"
            if function$ = "C" then init(hex(0b))str(s$(),,m%)           ~
                               else init(" ")str(s$(),,m%)
            range$ = "N"
            return
L20820: REM Def/Enable Serial Number               SERIAL$()
            if function$ = "A" then L20910
            if c_enabled% = 0% then L20890
               errormsg$ = "Sorry, you cannot change a Parent S/N "    & ~
                           "when the Component is also Serialized!"
               enabled% = 0%
               return
L20890:     inpmessage$ = "Change the Serial Number Above"
            return
L20910:     if m% >= qty_build then enabled% = 0%
            if m% >= dim(serial$(),1)-1% then enabled% = 0%
            if remainder <= 0 then enabled% = 0%
            if enabled% = 0% then return
            o% = max(1%,m%-22%)
            l% = max(1%, m%+1%)
            inpmessage$ = "Enter the Serial Number to Assign"
            if serial$(l%) = " " and l% > 1% then                        ~
               serial$(l%) = serial$(l%-1%)
            q% = 1%
            if serial$(l%) > " " then gosub increment_number
            return
L21030: REM Def/Enable Distribution Qty            QTY$()
            call "STRING" addr("LJ", qty$(l%), 8%)
            if function$ = "A" then L21110
            inpmessage$ = "Change the Distribution Quantity as needed"
            qty_issued=0 : convert qty$(l%) to qty_issued,data goto L21080
L21080:     qty_kitted = qty_kitted - qty_issued
            remainder = qty - qty_kitted
            return
L21110:     inpmessage$ = "Enter the Quantity to Distribute"
            if qty$(l%) = " " and l% > 1% then qty$(l%) = qty$(l%-1%)
            return

        increment_number
            if p%(1%) = 0% then return
            x%,y%,b% = 0%

            REM *** Construct the Number to Increment ***
L21200:     if p%(y%+1%) = 0% then L21280
            a% = 0%
            y% = y% + 1%
            convert str(serial$(l%),p%(y%),1%) to a%, data goto L21240
L21240:     b% = b% + a% * 10%^x%
            x% = x% + 1%
            if y% >= 14% then L21280
            goto L21200
L21280:     nxt = b% + q%
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
            maxqty% = min(max_qty%, max(24%,qty_build))
            maxqty% = maxqty% + 1%
            mat redim mfac$(maxqty%)1,                                   ~
                      n$(maxqty%)6,                                      ~
                      nfac$(maxqty%)1,                                   ~
                      qty$(maxqty%)8,                                    ~
                      s$(maxqty%)1,                                      ~
                      serial$(maxqty%)20%,                               ~
                      index%(maxqty%),                                   ~
                      incl_excl(maxqty%),                                ~
                      sfac$(maxqty%)1

            init(" ") errormsg$, inpmessage$,                            ~
                      range$                 , /* Add Range? (Y/N)   */  ~
                      from_serial$,to_serial$, /* Range Values       */  ~
                      qty_each$              , /* Range Values       */  ~
                      s$()                   , /* Selection Block    */  ~
                      serial$()              , /* Serial Numbers     */  ~
                      qty$()                 , /* Qty Issued         */  ~
                      pf1$,pf2$,pf3$,pf4$,pf5$,pf6$,pf7$,pf11$,pf12$
            mat index% = zer
            return

        REM *************************************************************~
            *   C L E A R   E N T R I E S   F O R   S T A R T O V E R   *~
            *-----------------------------------------------------------*~
            * Clears Work File entries for the given line or for all    *~
            * lines depending on the value of INDEX% passed in.         *~
            * Releases any component serial numbers held also.          *~
            * Routine is also invoked if user changes Issue Qty on      *~
            * Calling Screen to Zero.                                   *~
            *************************************************************
        startover
            if index% <> 0% then L29410
               plowkey$ = all(hex(00)) : break% = 0% : goto L29420
L29410:     plowkey$ = bin(index%,3)   : break% = 3%
L29420:     gosub clear_parents_loop
            if index% = 0% then call "FILEBGON" (#4)
            goto exit_routine

        clear_parents_loop
            call "PLOWNXT1" (#4, plowkey$, break%, f1%(4))
            if f1%(4) = 0% then return
            if index% <> 0% then delete #4
            readkey$ = str(trantype$,,2%) & str(job$) & str(plowkey$,,3%)~
                     & str(plowkey$,4%,20%)
            gosub clear_components_loop
            goto clear_parents_loop

        clear_components_loop
            call "PLOWNXT1" (#7, readkey$, 42%, f1%(7))
            if f1%(7) = 0% then return
            get #7 using L29680, comp_part$
            delete #7
            inpmessage$ = str(comp_part$) & str(readkey$,43%,20%)
            call "READ101" (#3, inpmessage$, f1%(3))
            if f1%(3) = 0% then clear_components_loop
            get #3 using L29700, prior_status$, prior_location$
            put #3 using L29690, prior_status$, prior_location$," "," ",  ~
                                " ", " "
            rewrite #3
            goto clear_components_loop
L29680:     FMT POS(63), CH(25)
L29690:     FMT CH(1), CH(30), POS(216),CH(2), CH(40), CH(1), CH(30)
L29700:     FMT POS(258), CH(1), CH(30)

        REM *************************************************************~
            *   L O A D   E N T R I E S   F R O M   W O R K    F I L E  *~
            *-----------------------------------------------------------*~
            * Reads passed in Work File for any S/N's previously entered*~
            * for the passed in index pointer and loads them into an    *~
            * array automatically sorted.                               *~
            *************************************************************
        dataload
            plowkey$ = bin(index%,3)
            m%, l%, indx% = 0%
            qty_kitted = 0
L30110:     call "PLOWNEXT" (#4, plowkey$, 3%, f1%(4))
            if f1%(4) = 0% then L30220
            m% = m% + 1%
            if m% > dim(serial$(),1)-1% then L30220
               serial$(m%) = str(plowkey$,4%)
               get #4 using L30210, qty_issued, index%(m%)
               call "CONVERT" (qty_issued, 0.2, qty$(m%))
               qty_kitted = qty_kitted + qty_issued
               if index%(m%) > indx% then indx% = index%(m%)
               goto L30110
L30210:        FMT POS(49), PD(14,4), BI(3)
L30220:     call "CONVERT" (qty_kitted ,-.2, qty_kitted$ )
            remainder = qty - qty_kitted
            call "CONVERT" (remainder,-.2, remainder$)
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
            qty_kitted = 0
            if m% < 1% then L31190
            for l% = 1% to m%
                qty_issued = 0
                convert qty$(l%) to qty_issued, data goto L31150
L31150:         write #4 using L35000, index%, serial$(l%), part$,        ~
                                      qty_issued, index%(l%), " "
                qty_kitted = qty_kitted + qty_issued
            next l%
L31190:     if qty_kitted  <> qty then errormsg$ =                       ~
               "Qty distributed differs from Qty Issued by " & remainder$
            return

L35000: FMT                 /* FILE: SERWORK2                          */~
            BI(3),          /* General purpose sequence number         */~
                            /* Index pointer to Document Line Item     */~
                            /* that S/N's w/this pointer belong to.    */~
                            /* NOT the same as Item #!                 */~
            CH(20),         /* Serial Number                           */~
                            /* Parent Serial Number (S/N of Part to    */~
                            /* Build).                                 */~
            CH(25),         /* Part code                               */~
                            /* Part Code this S/N is associated with.  */~
            PD(14,4),       /* quantity moved to job                   */~
                            /* Quantity of Component Issued/Kitted to  */~
                            /* this Parent Serial Number.              */~
            BI(3),          /* another gen purpose sequence number     */~
                            /* Component S/Ns Index pointer for        */~
                            /* component S/N's kitted to this Parent   */~
                            /* S/N.                                    */~
            CH(01)          /* Filler (Internal, unused space)         */~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              line2$ = "For Job: " & job$ & " Component: " & comp_part$  ~
                     & " Store: " & store$
              if lot$ > " " then line2$ = line2$ & " Lot: " & lot$
              if trantype$ = "JK" then L40180
              line2$ = "From Job: " & job$ & " Component: " & comp_part$ ~
                     & " To Store: " & store$
              if lot$ > " " then line2$ = line2$ & " Lot: " & lot$
              if trantype$ = "RJ" then L40180
              REM *** Must be JJ ***
              line2$ = "From Job: " & job$ & " To Job: " & tojob$ &      ~
                       " Component: " & comp_part$
L40180:       if function$ = "D" then errormsg$ = hex(84) & "            ~
        ~         Please Note:  You Are Now in DELETE Mode"
              o% = max(1%, o%)
              if len(line2$) < 64% then                                  ~
              str(line2$,64%) = "SERKIT: " & str(cms2v$,,8%)
              if errormsg$ = " " then errormsg$ = hex(84) & infomsg$
              init(hex(8c)) lfac$(),mfac$(), qfac$()
              init(hex(9c)) sfac$(),nfac$(), sfac$()
              if function$ = "A" then init(hex(8c))str(nfac$(),,m%+1%)   ~
                                 else init(hex(8c))str(nfac$(),,m%)
              on fieldnr% gosub      ,         /* Function Code     */   ~
                                L40390,         /* Add Range? (Y/N)  */   ~
                                L40390,         /* From Range        */   ~
                                L40390,         /* To Range          */   ~
                                L40400,         /* Range Qty Each    */   ~
                                L40430,         /* Selection Blocks  */   ~
                                L40410,         /* Serial Numbers    */   ~
                                L40420          /* Distribution Qty  */
              goto L40480

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40390:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40400:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */
L40410:           mfac$(l%)       = hex(81)  :  return  /* Upper Only */
L40420:           qfac$(l%)       = hex(82)  :  return  /* Numeric    */
L40430:           if function$ = "C" then init(hex(86))str(sfac$(),,m%), ~
                                                       str(qfac$(),,m%)  ~
                                     else init(hex(81))str(sfac$(),,m%)
                  return

L40480:     accept                                                       ~
               at (01,02),                                               ~
                  "Distribute Quantities to Serial Numbers",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), "Parent Part to Build = ",                    ~
               at (04,25), fac(hex(8c)), part$                  , ch(25),~
               at (04,51), "Qty to Build =",                             ~
               at (04,66), fac(hex(8c)), qty_build         , pic(######),~
               at (05,02), fac(hex(8c)), msg5$,                          ~
               at (05,28), fac(hex(8c)), qty$                   , ch(06),~
               at (05,35), fac(hex(8c)), msg6$,                          ~
               at (05,53), fac(hex(8c)), qty_kitted$            , ch(06),~
               at (05,60), "Remainder =",                                ~
               at (05,73), fac(hex(84)), remainder$             , ch(06),~
               at (06,02), "Range?:",                                    ~
               at (06,10), fac(lfac$(02)), range$               , ch(01),~
               at (06,13), "From:"                              ,        ~
               at (06,19), fac(lfac$(03)), str(from_serial$,,ll%),       ~
               at (06,40), "To:"                                ,        ~
               at (06,44), fac(lfac$(04)), str(to_serial$,,ll%) ,        ~
               at (06,65), "Qty Ea:"                            ,        ~
               at (06,73), fac(lfac$(05)), qty_each$            , ch(08),~
               at (07,02), fac(hex(ac)), line7$,                         ~
               at (08,02), fac(nfac$(o%+00%)), n$(o%+00%)       , ch(06),~
               at (08,09), fac(sfac$(o%+00%)), s$(o%+00%)       , ch(01),~
               at (08,11), fac(mfac$(o%+00%)), str(serial$(o%+00%),,ll%),~
               at (08,32), fac(qfac$(o%+00%)), qty$(o%+00%)     , ch(08),~
               at (09,02), fac(nfac$(o%+01%)), n$(o%+01%)       , ch(06),~
               at (09,09), fac(sfac$(o%+01%)), s$(o%+01%)       , ch(01),~
               at (09,11), fac(mfac$(o%+01%)), str(serial$(o%+01%),,ll%),~
               at (09,32), fac(qfac$(o%+01%)), qty$(o%+01%)     , ch(08),~
               at (10,02), fac(nfac$(o%+02%)), n$(o%+02%)       , ch(06),~
               at (10,09), fac(sfac$(o%+02%)), s$(o%+02%)       , ch(01),~
               at (10,11), fac(mfac$(o%+02%)), str(serial$(o%+02%),,ll%),~
               at (10,32), fac(qfac$(o%+02%)), qty$(o%+02%)     , ch(08),~
               at (11,02), fac(nfac$(o%+03%)), n$(o%+03%)       , ch(06),~
               at (11,09), fac(sfac$(o%+03%)), s$(o%+03%)       , ch(01),~
               at (11,11), fac(mfac$(o%+03%)), str(serial$(o%+03%),,ll%),~
               at (11,32), fac(qfac$(o%+03%)), qty$(o%+03%)     , ch(08),~
               at (12,02), fac(nfac$(o%+04%)), n$(o%+04%)       , ch(06),~
               at (12,09), fac(sfac$(o%+04%)), s$(o%+04%)       , ch(01),~
               at (12,11), fac(mfac$(o%+04%)), str(serial$(o%+04%),,ll%),~
               at (12,32), fac(qfac$(o%+04%)), qty$(o%+04%)     , ch(08),~
               at (13,02), fac(nfac$(o%+05%)), n$(o%+05%)       , ch(06),~
               at (13,09), fac(sfac$(o%+05%)), s$(o%+05%)       , ch(01),~
               at (13,11), fac(mfac$(o%+05%)), str(serial$(o%+05%),,ll%),~
               at (13,32), fac(qfac$(o%+05%)), qty$(o%+05%)     , ch(08),~
               at (14,02), fac(nfac$(o%+06%)), n$(o%+06%)       , ch(06),~
               at (14,09), fac(sfac$(o%+06%)), s$(o%+06%)       , ch(01),~
               at (14,11), fac(mfac$(o%+06%)), str(serial$(o%+06%),,ll%),~
               at (14,32), fac(qfac$(o%+06%)), qty$(o%+06%)     , ch(08),~
               at (15,02), fac(nfac$(o%+07%)), n$(o%+07%)       , ch(06),~
               at (15,09), fac(sfac$(o%+07%)), s$(o%+07%)       , ch(01),~
               at (15,11), fac(mfac$(o%+07%)), str(serial$(o%+07%),,ll%),~
               at (15,32), fac(qfac$(o%+07%)), qty$(o%+07%)     , ch(08),~
               at (16,02), fac(nfac$(o%+08%)), n$(o%+08%)       , ch(06),~
               at (16,09), fac(sfac$(o%+08%)), s$(o%+08%)       , ch(01),~
               at (16,11), fac(mfac$(o%+08%)), str(serial$(o%+08%),,ll%),~
               at (16,32), fac(qfac$(o%+08%)), qty$(o%+08%)     , ch(08),~
               at (17,02), fac(nfac$(o%+09%)), n$(o%+09%)       , ch(06),~
               at (17,09), fac(sfac$(o%+09%)), s$(o%+09%)       , ch(01),~
               at (17,11), fac(mfac$(o%+09%)), str(serial$(o%+09%),,ll%),~
               at (17,32), fac(qfac$(o%+09%)), qty$(o%+09%)     , ch(08),~
               at (18,02), fac(nfac$(o%+10%)), n$(o%+10%)       , ch(06),~
               at (18,09), fac(sfac$(o%+10%)), s$(o%+10%)       , ch(01),~
               at (18,11), fac(mfac$(o%+10%)), str(serial$(o%+10%),,ll%),~
               at (18,32), fac(qfac$(o%+10%)), qty$(o%+10%)     , ch(08),~
               at (19,02), fac(nfac$(o%+11%)), n$(o%+11%)       , ch(06),~
               at (19,09), fac(sfac$(o%+11%)), s$(o%+11%)       , ch(01),~
               at (19,11), fac(mfac$(o%+11%)), str(serial$(o%+11%),,ll%),~
               at (19,32), fac(qfac$(o%+11%)), qty$(o%+11%)     , ch(08),~
               at (08,41), fac(nfac$(o%+12%)), n$(o%+12%)       , ch(06),~
               at (08,48), fac(sfac$(o%+12%)), s$(o%+12%)       , ch(01),~
               at (08,50), fac(mfac$(o%+12%)), str(serial$(o%+12%),,ll%),~
               at (08,71), fac(qfac$(o%+12%)), qty$(o%+12%)     , ch(08),~
               at (09,41), fac(nfac$(o%+13%)), n$(o%+13%)       , ch(06),~
               at (09,48), fac(sfac$(o%+13%)), s$(o%+13%)       , ch(01),~
               at (09,50), fac(mfac$(o%+13%)), str(serial$(o%+13%),,ll%),~
               at (09,71), fac(qfac$(o%+13%)), qty$(o%+13%)     , ch(08),~
               at (10,41), fac(nfac$(o%+14%)), n$(o%+14%)       , ch(06),~
               at (10,48), fac(sfac$(o%+14%)), s$(o%+14%)       , ch(01),~
               at (10,50), fac(mfac$(o%+14%)), str(serial$(o%+14%),,ll%),~
               at (10,71), fac(qfac$(o%+14%)), qty$(o%+14%)     , ch(08),~
               at (11,41), fac(nfac$(o%+15%)), n$(o%+15%)       , ch(06),~
               at (11,48), fac(sfac$(o%+15%)), s$(o%+15%)       , ch(01),~
               at (11,50), fac(mfac$(o%+15%)), str(serial$(o%+15%),,ll%),~
               at (11,71), fac(qfac$(o%+15%)), qty$(o%+15%)     , ch(08),~
               at (12,41), fac(nfac$(o%+16%)), n$(o%+16%)       , ch(06),~
               at (12,48), fac(sfac$(o%+16%)), s$(o%+16%)       , ch(01),~
               at (12,50), fac(mfac$(o%+16%)), str(serial$(o%+16%),,ll%),~
               at (12,71), fac(qfac$(o%+16%)), qty$(o%+16%)     , ch(08),~
               at (13,41), fac(nfac$(o%+17%)), n$(o%+17%)       , ch(06),~
               at (13,48), fac(sfac$(o%+17%)), s$(o%+17%)       , ch(01),~
               at (13,50), fac(mfac$(o%+17%)), str(serial$(o%+17%),,ll%),~
               at (13,71), fac(qfac$(o%+17%)), qty$(o%+17%)     , ch(08),~
               at (14,41), fac(nfac$(o%+18%)), n$(o%+18%)       , ch(06),~
               at (14,48), fac(sfac$(o%+18%)), s$(o%+18%)       , ch(01),~
               at (14,50), fac(mfac$(o%+18%)), str(serial$(o%+18%),,ll%),~
               at (14,71), fac(qfac$(o%+18%)), qty$(o%+18%)     , ch(08),~
               at (15,41), fac(nfac$(o%+19%)), n$(o%+19%)       , ch(06),~
               at (15,48), fac(sfac$(o%+19%)), s$(o%+19%)       , ch(01),~
               at (15,50), fac(mfac$(o%+19%)), str(serial$(o%+19%),,ll%),~
               at (15,71), fac(qfac$(o%+19%)), qty$(o%+19%)     , ch(08),~
               at (16,41), fac(nfac$(o%+20%)), n$(o%+20%)       , ch(06),~
               at (16,48), fac(sfac$(o%+20%)), s$(o%+20%)       , ch(01),~
               at (16,50), fac(mfac$(o%+20%)), str(serial$(o%+20%),,ll%),~
               at (16,71), fac(qfac$(o%+20%)), qty$(o%+20%)     , ch(08),~
               at (17,41), fac(nfac$(o%+21%)), n$(o%+21%)       , ch(06),~
               at (17,48), fac(sfac$(o%+21%)), s$(o%+21%)       , ch(01),~
               at (17,50), fac(mfac$(o%+21%)), str(serial$(o%+21%),,ll%),~
               at (17,71), fac(qfac$(o%+21%)), qty$(o%+21%)     , ch(08),~
               at (18,41), fac(nfac$(o%+22%)), n$(o%+22%)       , ch(06),~
               at (18,48), fac(sfac$(o%+22%)), s$(o%+22%)       , ch(01),~
               at (18,50), fac(mfac$(o%+22%)), str(serial$(o%+22%),,ll%),~
               at (18,71), fac(qfac$(o%+22%)), qty$(o%+22%)     , ch(08),~
               at (19,41), fac(nfac$(o%+23%)), n$(o%+23%)       , ch(06),~
               at (19,48), fac(sfac$(o%+23%)), s$(o%+23%)       , ch(01),~
               at (19,50), fac(mfac$(o%+23%)), str(serial$(o%+23%),,ll%),~
               at (19,71), fac(qfac$(o%+23%)), qty$(o%+23%)     , ch(08),~
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
               at (22,20), fac(hex(8c)), pf11$                          ,~
               at (22,40), fac(hex(8c)), pf12$                          ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(000102030405060708090b0c0d0f10)),                ~
               key (keyhit%)

               if keyhit% <> 13 then L41930
                  call "MANUAL" ("SERKIT")
                  goto L40480

L41930:        if keyhit% <> 15 then L41970
                  call "PRNTSCRN"
                  goto L40480

L41970:           close ws
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
                              L50170 ,         /* Add Range? (Y/N)  */    ~
                              L50210 ,         /* From Range        */    ~
                              L50570 ,         /* To   Range        */    ~
                              L51000 ,         /* Range Dist Qty    */    ~
                              L51190 ,         /* Selection Block   */    ~
                              L51210 ,         /* Serial Number     */    ~
                              L51600          /* Qty Per Parent    */
            return
L50170: REM Test for Add Range? (Y/N)             RANGE$
            if pos("YN" = range$) > 0% then return
            errormsg$ = "Must be Y or N"
            return
L50210: REM Test for From S/N Range               FROM_SERIAL$
            if trantype$ = "JK" then L50410
            mat redim incl_excl(max(m%+1%, 1%)),                         ~
                      serial$(max(m%+1%, 1%))20
            mat incl_excl = con
            mat incl_excl = (-32.20) * incl_excl
            incl_excl(dim(incl_excl(),1)) = 97.08
            serial$(dim(incl_excl(),1)) = job$
            plowkey$ = str(part$) & from_serial$
            descr$ = hex(06) & "Select the FROM Serial Number Range Entry"
            f1%(3) = -(cursor%(1))
            call "PLOWCODE" (#3, plowkey$, descr$,  6025%, 0.80, f1%(3), ~
                             header$(), 0, 0, incl_excl(), serial$(),    ~
                             "Y")
            if f1%(3) = 0% then errormsg$="S/N Not Found in the current "~
                     & "Job!"
            from_serial$ = str(plowkey$,26%)
            serial$(dim(incl_excl(),1)) = " "
            mat redim incl_excl(maxqty%), serial$(maxqty%)20
            return
L50410:     mat redim incl_excl(max(m%, 1%)),                            ~
                      serial$(max(m%, 1%))20
            mat incl_excl = con
            mat incl_excl = (-32.20) * incl_excl
            plowkey$ = status$ & str(location$) & from_serial$

            descr$ = hex(06) & "Select the FROM Serial Number Range Entry"
            f1%(3) = -(cursor%(1))
            call "PLOWCODE" (#3, plowkey$, descr$,  6031%, 2.80, f1%(3), ~
                             header$(),20, 0, incl_excl(), serial$(),    ~
                             "Y")
            if f1%(3) = 0% then errormsg$="S/N Not Found in the current "~
                     & "Job!"
            from_serial$ = str(plowkey$,32%)
            mat redim incl_excl(maxqty%), serial$(maxqty%)20
            return
L50570: REM test for To S/N Range                 TO_SERIAL$
            if trantype$ = "JK" then L50810
            mat redim incl_excl(max(m%+2%, 1%)),                         ~
                      serial$(max(m%+2%, 1%))20
            mat incl_excl = con
            mat incl_excl = (-32.20) * incl_excl
            incl_excl(dim(incl_excl(),1)) = 97.08
            serial$(dim(incl_excl(),1)) = job$
            incl_excl(dim(incl_excl(),1)-1) = 32.20
            serial$(dim(incl_excl(),1)-1) = "<z>" & from_serial$
            serial$(dim(incl_excl(),1)-1) = addc hex(01)
            plowkey$ = str(part$) & to_serial$
            descr$ = hex(06) & "Select the TO Serial Number Range Entry"
            f1%(3) = -(cursor%(1))
            call "PLOWCODE" (#3, plowkey$, descr$,  6025%, 0.0 , f1%(3), ~
                             header$(), 0, 0, incl_excl(), serial$(),    ~
                             "Y")
            if f1%(3) = 0% then errormsg$ = "S/N Not Found or Invalid "  ~
                     & "for the current Job"
            to_serial$ = str(plowkey$,26%)
            serial$(dim(incl_excl(),1)),serial$(dim(incl_excl(),1)-1)=" "
            mat redim incl_excl(maxqty%), serial$(maxqty%)20
            return
            REM *** Kitting logic ***
L50810:     mat redim incl_excl(max(m%+1%, 1%)),                         ~
                      serial$(max(m%+1%, 1%))20
            mat incl_excl = con
            mat incl_excl = (-32.20) * incl_excl
            incl_excl(dim(incl_excl(),1)) = 32.20
            serial$(dim(incl_excl(),1)) = "<z>" & from_serial$
            serial$(dim(incl_excl(),1)) = addc hex(01)
            plowkey$ = status$ & str(location$) & to_serial$
            descr$ = hex(06) & "Select the TO Serial Number Range Entry"
            f1%(3) = -(cursor%(1))
            call "PLOWCODE" (#3, plowkey$, descr$,  6031%, 2.00, f1%(3), ~
                             header$(),20, 0, incl_excl(), serial$(),    ~
                             "Y")
            if f1%(3) = 0% then errormsg$ = "S/N Not Found or Invalid "  ~
                     & "for the current Job"
            to_serial$ = str(plowkey$,32%)
            serial$(dim(incl_excl(),1)) = " "
            mat redim incl_excl(maxqty%), serial$(maxqty%)20
            return
L51000: REM test for Range Each Quantity          QTY_EACH$
            call "NUMTEST" (qty_each$,.01, remainder, errormsg$, .2,     ~
                                 qty_each)
            if errormsg$ > " " then return
            q = remainder : serial$(l%) = from_serial$
            gosub assign_serial_numbers
            if q < remainder then L51120
               serial$(l%) = " "
               l% = max(1%, m%)
               errormsg$ = "Unable to assign any S/N's within the given "~
                         & "Range!"
               return
L51120:     m% = l%
            o% = max(1%, m%-22%)
            remainder = q
            call "CONVERT" (remainder, -.2, remainder$)
            qty_kitted  = qty - remainder
            call "CONVERT" (qty_kitted ,-.2,qty_kitted$ )
            return
L51190: REM Test for Selection Block              S$(12)
            return
L51210: REM Test for Serial Number                SERIAL$(12)
            if trantype$ = "JK" then L51430
            mat redim incl_excl(max(m%+1%, l%+1%)),                      ~
                      serial$(max(m%+1%, l%+1%))20
            mat incl_excl = con
            mat incl_excl = (-32.20) * incl_excl
            incl_excl(dim(incl_excl(),1)) = 97.08
            serial$(dim(incl_excl(),1)) = job$
            plowkey$ = str(part$) & serial$(l%)
            serial$(l%) = " "
            descr$ = hex(06) & "Select the Serial Number to Assign"
            f1%(3) = -(cursor%(1))
            call "PLOWCODE" (#3, plowkey$, descr$,  6025%, 0.00, f1%(3), ~
                             header$(), 0, 0, incl_excl(), serial$(),    ~
                             "Y")
            if f1%(3) = 0% then errormsg$ = "S/N Not Found or Invalid "  ~
                     & "for the given Job!"
            serial$(dim(incl_excl(),1)) = " "
            mat redim incl_excl(maxqty%), serial$(maxqty%)20
            serial$(l%) = str(plowkey$,26%)
            return

L51430:     mat redim incl_excl(max(m%, l%)),                            ~
                      serial$(max(m%, l%))20
            mat incl_excl = con
            mat incl_excl = (-32.20) * incl_excl
            plowkey$ = status$ & str(location$) & serial$(l%)
            serial$(l%) = " "
            descr$ = hex(06) & "Select the Serial Number to Assign"
            f1%(3) = -(cursor%(1))
            call "PLOWCODE" (#3, plowkey$, descr$,  6031%, 2.00, f1%(3), ~
                             header$(),20, 0, incl_excl(), serial$(),    ~
                             "Y")
            if f1%(3) = 0% then errormsg$ = "S/N Not Found or Invalid "  ~
                     & "for the given Job!"
            mat redim incl_excl(maxqty%), serial$(maxqty%)20
            serial$(l%) = str(plowkey$,32%)
            return

L51600: REM Test for Quantity                     QTY$()
            call "NUMTEST" (qty$(l%), .01, remainder, errormsg$,-.2,     ~
                            qty_issued)
            if errormsg$ > " " then return
            comp_loc$ = str(store$) & lot$
            trankey$ = str(job$) & bin(index%,3) & serial$(l%)
            lines% = qty_build
            if function$ = "C" then L51740
            if index%(l%) <> 0% then L51740
               indx% = indx% + 1%
               index%(l%) = indx%
               trankey$ = str(job$) & bin(index%,3) & serial$(l%)
               goto L51870

L51740:     REM *** Transfer Component Entries from TIF to WORK ***
            call "SERLOAD"  (index%(l%), /* Line Item Pointer.         */~
                             trantype$,  /* Source Transaction Type    */~
                             trankey$,   /* Source Transaction Key     */~
                             lines%,     /* Average # Lines per Documnt*/~
                             " ",        /* If non-blank Load Serial   */~
                             " ",        /* Numbers from this Source & */~
                                         /* Location, Else from TIF.   */~
                             #1,         /* SYSFILE2 UFB               */~
                             #7,         /* SERTIF UFB                 */~
                             #3,         /* SERMASTR UFB               */~
                             #6, u3%)    /* SERWORK  UFB               */

L51870:     call "SERSELCT" (comp_part$, /* Part code                  */~
                             comp_loc$,  /* S/N Location to Select from*/~
                             qty_issued, /* Qty to Assign S/N's To     */~
                             index%(l%), /* Pointer For Work File Use  */~
                             lines%,     /* Average # Lines per Documnt*/~
                             trantype$,  /* Source Transaction Type.   */~
                             trankey$,   /* Source Transaction Key.    */~
                             "3",        /* Status to Change S/N to.   */~
                             source$,    /* Status to Select/Chg from  */~
                             errormsg$,  /* Returned Error Message     */~
                             #1,         /* SYSFILE2 UFB               */~
                             #2,         /* HNYMASTR UFB               */~
                             #3,         /* SERMASTR UFB               */~
                             #6)         /* SERWORK  UFB               */

            REM *** Transfer Component Entries from WORK to TIF ***
            call "SERSAVE" (index%(l%),  /* Line Item Pointer.         */~
                            trantype$,   /* Source Transaction Type    */~
                            trankey$,    /* Source Transaction Key     */~
                            lines%,      /* # Trans to Create File for */~
                            comp_part$,  /* Part Code (Component)      */~
                            userid$,     /* Current User ID            */~
                            "3",         /* Change Status to ...       */~
                            source$,     /* Change Status from ...     */~
                            2%,          /* Clear TIF after Save (NO)  */~
                                         /* (Don't change status from 7*/~
                            #1,          /* SYSFILE2 UFB               */~
                            #7,          /* SERTIF UFB                 */~
                            #3,          /* SERMASTR UFB               */~
                            #6)          /* SERWORK  UFB               */


            if errormsg$ > " " then return
            qty_kitted = qty_kitted + qty_issued
            remainder  = qty - qty_kitted
            call "CONVERT" (remainder, -.2, remainder$)
            call "CONVERT" (qty_kitted ,-.2,qty_kitted$ )
            m% = max(m%,l%)
            return

        assign_serial_numbers
            first% = 1%
            plowkey$ = str(part$) & serial$(l%)
            call "READ100" (#3, plowkey$, f1%(3))
            if f1%(3) = 0% then return
L52320:     if trantype$ <> "JK" then L52350
            if str(key(#3,2),,1%) <> status$ then L52460
            if str(key(#3,2),2%,30%) <> location$ then L52460
L52350:     if first% = 0% then l% = l% + 1%
            serial$(l%) = str(plowkey$,26%)
            if q - qty_each >= 0 then L52410
               qty_issued = q
               q = 0
               goto L52430
L52410:     qty_issued = qty_each
            q = q - qty_each
L52430:     call "CONVERT" (qty_issued, .2, qty$(l%))
            if q = 0 then return
            first% = 0%
L52460:     call "PLOWNEXT" (#3, plowkey$, 25%, f1%(3))
            if f1%(3) = 0% then return
            if str(plowkey$,26%) > to_serial$ then return
            goto L52320
            FMT CH(1), POS(216), CH(2), CH(40)

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
