        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB   K   K  IIIII  TTTTT   SSS   U   U  BBBB    *~
            *    J    B   B  K  K     I      T    S      U   U  B   B   *~
            *    J    BBBB   KKK      I      T     SSS   U   U  BBBB    *~
            *  J J    B   B  K  K     I      T        S  U   U  B   B   *~
            *   J     BBBB   K   K  IIIII    T     SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBKITSUB - Control the issuing of materials to jobs       *~
            *            via the background tasks                       *~
            *----------------------------------------------------------Q*~
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
            * 01/15/87 ! Original                                 ! KEN *~
            * 02/09/87 ! Enhanced Lot Tracking                    ! ERN *~
            * 02/10/87 ! Changes for Serial Number Handling       ! LDJ *~
            * 05/13/87 ! Bug fix - when coming back out of Lot    ! LDJ *~
            *          ! Distribution (LOTDISN), if there was only!     *~
            *          ! 1 entry in the distribution table it     !     *~
            *          ! zapped the table - no distribution needed!     *~
            *          ! This had an unfortunate effect on Serial !     *~
            *          ! Numbered parts as their detailed S/N     !     *~
            *          ! distribution was lost.  The fix was to   !     *~
            *          ! retain the distribution table entries if !     *~
            *          ! the Job Part is a Serial Numbered part.  !     *~
            * 06/15/87 ! Standard Costing Changes                 ! ERN *~
            * 10/28/87 ! Dont' require lot to be on file (51970)  ! HES *~
            * 05/17/88 ! Sort Pick List Using Sysflag             ! HES *~
            * 08/09/88 ! Fixed MAXLINES% being incremented twice  ! TLJ *~
            * 02/13/89 ! Added 'ASKUSER' Warning when Qty Issued  ! MJB *~
            *          !  is Greater Than Qty Required            !     *~
            * 09/21/89 ! Cnvrtd QTYISSUED$ to QTY rtrn frm SERKIT.! JDH *~
            * 02/14/90 ! Added PF(24) access to HNYLCSUB for Loc. ! MLJ *~
            *          !  control, also HNYLOCNS & LOCATION files.!     *~
            * 02/27/90 ! Added call to FILEBGON prior to WORKOPEN.! MLJ *~
            * 03/09/90 ! Changed LOCATION from 200 to 400.        ! MLJ *~
            * 06/13/90 ! Error if serial numbered components      ! JDH *~
            *          !  kitted in whole numbers.                !     *~
            * 11/29/90 ! Added FILEBGON for #17                   ! MJB *~
            * 11/29/90 ! Added LOTVALID & LOTUNQ2E to enforce lot ! MJB *~
            *          !  tracking specifications.                !     *~
            * 09/04/90 ! G/L Export file modifications.           ! RAC *~
            * 06/25/91 ! PRR 12060.  Enter in location order.     ! JDH *~
            * 11/21/91 ! Now allows kit complete to serialized    ! WPH *~
            *          ! parent if all parts not lot or S/N trackd!     *~
            * 04/14/92 ! PRR 12108. Flag Job as In-use.           ! JDH *~
            * 10/01/92 ! Ensure In-use Flag is Relieved.          ! JDH *~
            * 08/25/93 ! PRR 11966, 12656 - Corrected Kit Complete! MLJ *~
            *          !  by re-establishing store default.       !     *~
            *          ! PRR 12908 - Added '(22)Qty Info' in input!     *~
            *          !  for call to HNYQDISP (#56 now passed    !     *~
            *          !  from callers).                          !     *~
            *          ! PRR 12909 - Added '(26)Find' in input and!     *~
            *          !  edit mode to locate and position to a   !     *~
            *          !  part in the kit list.                   !     *~
            * 11/08/93 ! Keep Option Parts Consistant w/JBCMPSUB  ! KAB *~
            *          !  Lots of cosmetic help.  This routine    !     *~
            *          !  loops or falls thru validation in       !     *~
            *          !  at least three modes.  Care must be     !     *~
            *          !  taken to make sure that -               !     *~
            *          !    1)Variables normally set during       !     *~
            *          !      Default/Enable are set when needed. !     *~
            *          !    2)Upon an Error return, an appropriate!     *~
            *          !      field is enabled.                   !     *~
            *          ! (Translation - A convoluted mess.  Be    !     *~
            *          !  careful with even simple changes.)      !     *~
            * 10/18/94 ! PRR 13304. Fixed bad branch.             ! JDH *~
            *          ! PRR 13255. Added UOM on toggled display. !     *~
            * 12/12/95 ! Correctly determines line for Locations. ! JDH *~
            * 09/04/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "JBKITSUB" (modno$, jnlid$,          /* G/L Control Stuff  */~
                        #3,                      /* HNYMASTR File UFB  */~
                        #4,                      /* JBMASTR2 File UFB  */~
                        #34,                     /* PIPOUT   File UFB  */~
                        #52,                     /* HNYQUAN  File UFB  */~
                        #54,                     /* SYSFILE2 File UFB  */~
                        #59,                     /* STORNAME File UFB  */~
                        #36,                     /* JBPIPXRF File UFB  */~
                        #7,                      /* JBCREDIT File UFB  */~
                        #20,                     /* USERINFO File UFB  */~
                        #61,                     /* SERTIF   File UFB  */~
                        #62,                     /* SERMASTR File UFB  */~
                        #63,                     /* SERWORK  File UFB  */~
                        djb$,                    /* Dflt Job / Caller  */~
                        f2%())

        dim                                                              ~
            basedate$8,                  /* Planning Base Date         */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bkgpct$10,                   /* Background Percent         */~
            bkgstr$3,                    /* Background Store           */~
            bkglot$6,                    /* Background Lot             */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            distindex%(2000),            /* Dist. Index                */~
            distinptr%(2000),            /* Dist. Index Ptr for S/N's  */~
            distlot$(2000)6,             /* Dist. Lots                 */~
            distqty$(2000)10,            /* Dist. Quantity             */~
            diststore$(2000)3,           /* Dist. Stores               */~
            dfac$1,                      /* Special Display FAC        */~
            djb$8,                       /* Default Job From Caller    */~
            duedate$8,                   /* Issue Components Due By    */~
            edtindex%(100),              /* S/N Ptrs for   (Lot Distr) */~
            edtmessage$79,               /* Edit screen message        */~
            edtlot$(100)6,               /* Edit  Lots     (Lot Distr) */~
            edtqty$(100)10,              /* Edit  Quantity (Lot Distr) */~
            edtstore$(100)3,             /* Edit  Stores   (Lot Distr) */~
            errormsg$79,                 /* Error message              */~
            fac$(10,10)1,                /* Tabular FAC's              */~
            fpart$25, savefpart$25,      /* Display Variable           */~
            hdr$(2)79,                   /* Screen Header              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            jbcrkey$50,                  /* Plowkey for jbcredit       */~
            jnlid$3,                     /* GL Journal Id              */~
            job$8,                       /* Job Order No.              */~
            jobdescr$30,                 /* Job Order No.              */~
            jobend$8,                    /* Job Complete               */~
            jobpart$25,                  /* Building Part              */~
            jobpartdesc$34,              /* Part Descr.                */~
            jobqty$10,                   /* Qty. to Build              */~
            jobstart$8,                  /* Job Started                */~
            lastdate$8,                  /* Planning Last Date         */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            lot$(600)6,                  /* From Lot                   */~
            modno$2,                     /* GL Module                  */~
            moduleno$2,                  /* GL Module                  */~
            optqty(600),                 /* Options Currently Avail.   */~
            part$25,                     /* Part Code and Descr. Array */~
            part$(600,2)32,              /* Part Code and Descr. Array */~
            pipoutrec$64,                /* Part Code and Descr. Array */~
            pipoutplow$99,               /* Part Code and Descr. Array */~
            pfkey$32,                    /* PF Keys Enabled            */~
            pfmsg$(2)79,                 /* PF Screen Literals         */~
            pipout$(600)64,              /* PIPOUT to honor and obey   */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            postdate$8,                  /* Posting Date               */~
            p3msg1$33,                   /* Line 24 1st Half Msg       */~
            p3msg2$18,                   /* Line 24 2nd Half Msg       */~
            qtyissued$(600)10,           /* Issued Quantity            */~
            qtyrem$(600,2)13,            /* Open   Quantity            */~
            qtyreq$(600)10,              /* Pipout Quantity            */~
            readkey$50,                  /* Misc Read/Plow key         */~
            rftg$19,                     /* Generic Xref Tag           */~
            savelot$6,                   /* Lot field before Edit      */~
            savestore$3,                 /* Store field before Edit    */~
            store$(600)3,                /* From Warehouse             */~
            storelot$(600)19,            /* From Warehouse (Options)   */~
            summary$1,                   /* GL Summary Flag            */~
            tran$79,                     /* Translation String         */~
            tttle$40,                    /* Work Variable              */~
            twoflags$2,                  /* Lottrack/Serial flags      */~
            type%(600),                  /* Part Types                 */~
            uom$4,                       /* Unit of Measure            */~
            userid$3,                    /* Current User Id            */~
            warning$79                   /* Incomplete Display         */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * # 3 ! HNYMASTR ! Inventory Master File                    *~
            * # 4 ! JBMASTR2 ! Job  Master File                         *~
            * #34 ! PIPOUT   ! Planned Withdrawal Detail                *~
            * #52 ! HNYQUAN  ! Inventory Store/Lot Record               *~
            * #54 ! SYSFILE2 ! System Control File                      *~
            * #59 ! STORNAME ! Warehouse File                           *~
            * #36 ! JBPIPXRF ! Type 000 Pegging                         *~
            * # 7 ! JBCREDIT ! Job Completions Detail                   *~
            * #20 ! USERINFO ! User's Posting Dates                     *~
            * #61 ! SERTIF   ! Additions buffer for inventory S/N's     *~
            * #62 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #63 ! SERWORK  ! Temporary Serial #'s Work File           *~
            * #17 ! WORKFILE ! For Sorting Pick List                    *~
            * #18 ! HNYLOCNS ! Location Quantity Detail File            *~
            * #19 ! LOCATION ! Location Master File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #17, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =  80,               ~
                        keypos = 1, keylen = 72

            select #18, "HNYLOCNS",                                      ~
                         varc,  indexed,  recsize =  700,                ~
                         keypos =  1,  keylen =  42,                     ~
                         alt key 1, keypos =  443, keylen =  42,         ~
                             key 2, keypos =  485, keylen =  42,         ~
                             key 3, keypos =  527, keylen =  42,         ~
                             key 4, keypos =  590, keylen =  42

            select #19, "LOCATION",                                      ~
                         varc,  indexed,  recsize =  400,                ~
                         keypos =  1,  keylen =  11,                     ~
                         alt key 1, keypos =    4, keylen =  11

            call "OPENCHCK" (#18, 0%, 0%, 1%, " " )
            call "OPENCHCK" (#19, 0%, 0%, 1%, " " )

            file% = 34% : sort$ = "P"
            call "READ100" (#54, "SWITCHS.SFC", f1%(54))
                if f1%(54) = 0 then L09000
            get #54, using L02260, sort$, warehouse$
L02260:     FMT POS(31), CH(1), POS(37), CH(3)
            if sort$ < "A" then sort$ = "P"
            if sort$ = "P" then L09000
                   call "FILEBGON" (#17)
                   call "WORKOPEN" (#17, "IO   ", 300%, 1%)
                   file% = 17%

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            ll% = 6%
            call "EXTRACT" addr("ID", userid$)
            call "READ100" (#20, userid$, f1%(20))
               if f1%(20)=0 then L65000
            get #20, using L09075, postdate$, savestore$
L09075:         FMT XX(33), CH(6), XX(24), CH(3)
            call "WHICHPER" (#54, postdate$, whichperiod%)
               if whichperiod% = 0% then L65000

            call "DATEFMT" (postdate$)

            call "READ100" (#54, "MONTHS OPEN", f1%(54))
                if f1%(54) = 0 then L65000
            get #54, using L09120, basedate$
L09120:         FMT XX(32), CH(6)
            call "DATE" addr("G+", str(basedate$,,6), 489%,              ~
                                   str(lastdate$,,6), u3%)
            call "DATEFMT" (lastdate$)
            call "DATEFMT" (basedate$)

            if warehouse$ = " " then warehouse$ = savestore$
            savestore$ = " "

            hdr$(1) = "Component Part Code            Qty Required Qty Is~
        ~sued Str Lot        Remaining"

            hdr$(2) = "Component Part Description     Qty Required Qty Is~
        ~sued Str Lot    Date Req UOM"

            linemax% = dim(qtyreq$(),1):distmax% = dim(diststore$(),1)

            init (hex(00)) tran$
            init (hex(01)) str(tran$,2)
            init (hex(02)) str(tran$,35)
            init (hex(03)) str(tran$,46)
            init (hex(04)) str(tran$,58)
            init (hex(05)) str(tran$,63)
            init (hex(06)) str(tran$,70)

            str(line2$,61%) = "JBKITSUB: " & str(cms2v$,,8%)

            if modno$ = " " then modno$ = "03"
            if jnlid$ = " " then jnlid$ = "MPR"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables : job$ = djb$

            for fieldnr% = 1 to  2
L10100:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10220
L10120:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         if fieldnr% = 1% then call "JBINUSE" (job$, 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% =  16 then exit_program
                      if keyhit% = 10% then bkg_complete
                      if keyhit% <> 0  then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%

            gosub dataload

            if maxlines% = 0% then editpg1

L10300:     s% = s% + 1%
            c% = s% + l%
            if c% > maxlines% then editpg1
L10330:     if s% < 11% then L10360
               s% = 10%
               l% = l% + 1%
L10360:     gosub input_line
              if keyhit% = 16% then editpg1
            goto L10300

        input_line
            fullvalid% = 0%
            for fieldnr% = 1% to 6%
L10420:       gosub'052(fieldnr%)
                if enabled% = 0% then L10670
L10440:       if fieldnr% = 1% or fieldnr% = 6% then t% = 0%
              gosub'102(fieldnr%, s%)
                  warning$ = " "
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10530
L10476:                  if fieldnr% < 4% or fieldnr% > 5% then L10480
                            savestore$ = store$(c%) : savelot$ = lot$(c%)
                            gosub clear_ser_distr
L10480:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10440
                         if fieldnr% = 1% then L10420
                         goto L10476
L10530:               if keyhit% <>  6 then       L10620
                         if c% = 1% then L10620
                         if fieldnr% <> 1% then L10580
                            part$(c%,1%) = part$(c%-1%,1%)
                            part$(c%,2%) = part$(c%-1%,2%)
L10580:                  if fieldnr% =  2% then                          ~
                               qtyreq$(c%) = qtyreq$(c%-1%)
                         if fieldnr% =  3% then                          ~
                               qtyissued$(c%) = qtyissued$(c%-1%)
                         if fieldnr% =  4% then                          ~
                               store$ (c%) = store$ (c%-1%)
                         if fieldnr% =  5% then                          ~
                               lot$   (c%) = lot$   (c%-1%)
                         if fieldnr% =  6% then                          ~
                               qtyrem$(c%,1%) = qtyrem$(c%-1%,1%)
                         goto L10420

L10620:               if keyhit%  =  8 then t% = mod(t%+1%,2%)
                      if keyhit% <> 16 then       L10660
                         if edit% = 0% and fieldnr% = 3% then L10720
                         if edit% <> 0% and fieldnr% = 1% then L10760
L10660:               if keyhit% <>  0 then       L10440
L10670:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10440
            next fieldnr%
            if complete% = 0% or complete% > 1% then return
            if store$(c%) = " " then return
            compstore$ = store$(c%):complot$ = lot$(c%):complete% = 2%
            call "SHOSTAT" ("Kitting Remaining Eligible Items.")
            return

L10720:     if auto% = 0% then L10744
            auto% = 2%
            gosub'152(3%)
            if errormsg$ <> " " then L10744
            fieldnr% = fieldnr% + 1%
            gosub'152(4%)
            if errormsg$ <> " " then L10744
            fieldnr% = fieldnr% + 1%
            gosub'152(5%)
            if errormsg$ <> " " then L10744
            return

L10744:     qtyissued$(c%)= "0"
            gosub'152(3%)
            errormsg$ = " "
            return

L10760:     part$(c%,1%), part$(c%,2%), qtyreq$(c%), qtyissued$(c%),     ~
            store$(c%), lot$(c%), qtyrem$(c%,1%), qtyrem$(c%,2%) = " "
            errormsg$ = " "
            return

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            c% = 0%
            fullvalid%, edit% = 1%
            inpmessage$ = edtmessage$
            lasts%, lastfieldnr%, complete%, auto% = 0%
            gosub'102(0%,0%)            /* Display Screen - No Entry   */
               warning$, errormsg$ = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then l% = 0%
                  if keyhit%  =  3 then l% = maxlines% - 10%
                  if keyhit%  =  4 then l% = l% - 10%
                  if keyhit%  =  5 then l% = l% + 10%
                  if keyhit%  =  6 then l% = l% - 1%
                  if keyhit%  =  7 then l% = l% + 1%
                                   l% = max(0%, min(l%, maxlines% - 10%))
                  if keyhit%  =  8 then t% = mod(t% + 1%, 2%)
                  if keyhit%  =  9 then       L11260
                  if keyhit%  = 10 then       kit_complete
                  if keyhit%  = 11 then       append_mode
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editpg1
L11260:     if cursor%(1) < 11 or cursor%(1) > 20 then editpg1
            s% = cursor%(1) - 10
            c% = s% + l%
            if c% > maxlines% then editpg1
            if keyhit%  <>  9 then L11300
               edit% = 0%:auto% = 1%
               goto L10330
L11300:     fieldnr% = val(str(tran$, cursor%(2),1))
            if fieldnr% = 0% then fieldnr% = 3%
                                  /* Not Likely, darn that tab stop */
            if fieldnr% > 1% then L11320
               if pipout$(c%) <> " " then fieldnr% = 3%
L11320:     if fieldnr% = lastfieldnr% and s% = lasts% then editpg1
        editpg1a
            if fieldnr% <> 3% then L11400
               temp = 0:convert qtyissued$(c%) to temp, data goto L11360
L11360:        if temp > 0 then L11400
                  edit% = 0%
                  gosub input_line
                    goto editpg1

L11400:     gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
L11420:     if fieldnr% = 1% or fieldnr% = 6% then t% = 0%
L11430:     gosub'102(fieldnr%, s%)
                  warning$ = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  8 then t% = mod(t% + 1%, 2%)
                  if keyhit% <>  0 then L11420
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11430
                  lastfieldnr% = fieldnr%:lasts% = s%
            goto L11260

L11520: append_mode
            append% = 1%
            c% = maxlines% + 1%
            if c% <= linemax% then L11550
                append% = 0%
                goto editpg1
L11550:     l% = max(0%, maxlines% - 9%)
            s% = c% - l%

            gosub input_line
                if keyhit% <> 16 then L11620
                   l% = max(0%, l% - 1%)
                   append% = 0%
                   goto editpg1
L11620:     maxlines% = maxlines% + 1%
            goto L11520
            if cursor%(1) < 11 or cursor%(1) > 20 then editpg1
        kit_complete
            if maxlines% = 0% then editpg1
            st% = 1%
            if cursor%(1) < 11 or cursor%(1) > 20 then L11672
               st% = l% + (cursor%(1) - 10%)

L11672:     if p_enabled% = 0% then L11690
               for j% = st% to maxlines%
                  call"READ100"(#3,str(part$(j%,1%),1,25), f1%(3%))
                     if f1%(3%) = 0% then L11684
                  get #3, using L11677, twoflags$
L11677:              FMT  POS(130), CH(2)
                  if twoflags$ <> "NN" then L11684
               next j%
               goto L11690  /* Ok to proceed, no components lot or s/n */

L11684:        errormsg$ = "Sorry, Can't Kit Complete. Parent Serial" &  ~
                     "ized & component(s) serial or lot tracked."
               goto editpg1

L11690:     for i% = st% to maxlines%
                if pipout$(i%) = " " then editpg1
                   temp = 0:convert qtyreq$(i%) to temp, data goto L11720
L11720:            if temp = 0 then L11770

                   temp = 0:convert qtyissued$(i%) to temp,              ~
                                                       data goto L11760
L11760:            if temp <> 0 then L11770
                   c% = i% : gosub'052(3%)
                   if enabled% = 0% then L11770
                      goto L11800
L11770:     next i%
            goto editpg1

L11800:     c% = i%
            l% = max(0%, min(maxlines% - 10%, c% - 1%))
            s% = c% - l%
            edit% = 0%:complete% = 1%
            goto L10330

        REM *************************************************************~
            * Background Kit Complete                                   *~
            *   Note - No Input Mode, Just Edit                         *~
            *************************************************************
        bkg_complete

            bkgpct = 100
            call "CONVERT" (bkgpct, 0.2, bkgpct$)
            bkgstr$ = warehouse$
            bkglot$ = " "
            bkgcomp% = 1%
            duedate$ = "ALL"
            if p_enabled% <> 0% then                                     ~
               warning$ = "Warning - Job Part is Serial #'ed."
            errormsg$ = " "
L12170:     lastfieldnr% = 0%
            gosub'103(0%)
                warning$ = " "
                   if keyhit%  =  1% then gosub startover
                   if keyhit%  = 16% then datasave
                   if keyhit% <>  0% then L12170

L12240:     fieldnr% = cursor%(1%) - 7%
            if fieldnr% < 3% or fieldnr% > 4% then L12170
            if fieldnr% = lastfieldnr% then L12170
L12270:     gosub'103(fieldnr%)
                   if keyhit%  =  1% then gosub startover
                   if keyhit% <>  0% then L12270
            gosub'153(fieldnr%)
                if errormsg$ <> " " then L12270
                lastfieldnr% = fieldnr%
                goto L12240

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20120,         /* Job Number         */    ~
                              L20140          /* Due By             */
            return
L20120: REM Def/Enable Job Order No.               JOB$
            inpmessage$ = "Enter Job Number, or blank for search."
            return
L20140: REM Def/Enable Issue Components Due By     DUEDATE$
            duedate$ = jobend$
            inpmessage$ = "Components due on or before this date will be"
            inpmessage$ = inpmessage$ & " processed."
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(fieldnr%)
            enabled% = 1%
            inpmessage$ = " "
            on fieldnr% gosub     L21170, /* Part                       */~
                                  L21220, /* Quantity Required          */~
                                  L21270, /* Quantity Issued            */~
                                  L21480, /* Store/Warehouse            */~
                                  L21630, /* Lot                        */~
                                  L21750  /* Quantity Open/Date Req.    */
            return

L21170: REM DEFAULT/ENABLE FOR PART
            if pipout$(c%) <> " " then enabled% = 0%
            if complete% > 0% then enabled% = 0%
            return

L21220: REM DEFAULT/ENABLE FOR QUANTITY REQUIRED
            if qtyreq$(c%) = " " then call "CONVERT" (0, 0.2, qtyreq$(c%))
            enabled% = 0%
            return

L21270: REM DEFAULT/ENABLE FOR QUANTITY ISSUED
            inpmessage$ = "Enter the Quantity Actually Issued to the Job"
            savestore$ = store$(c%) : savelot$ = lot$(c%)
            if p_enabled% <> 0% then L21290
            call "SERENABL" (part$(c%,1%), c_enabled%, u3%, #54, #3)
               if c_enabled% <> 0% then L21330
L21290:     temp  = 0: convert qtyissued$(c%) to temp , data goto L21291
L21291:     temp1 = 0: convert qtyreq$   (c%) to temp1, data goto L21292
L21292:     temp2 = 0: convert qtyrem$(c%,1%) to temp2, data goto L21300
L21300:     if temp <> 0 then L21430
            if type%(c%) <> 0% then L21420
               if optqty(c%) > 0 then L21360
L21330:           enabled% = 0%
                  return
L21360:        temp = min(temp1, optqty(c%))
               store$(c%) = str(storelot$(c%),1,3)
               lot$  (c%) = str(storelot$(c%),4,6)
               goto L21430

L21420:        temp = temp1               /* TEMP = MIN(TEMP1, TEMP2) */
L21430:        call "CONVERT" (temp, -0.2, qtyissued$(c%))
            if keyhit% = 4% then return
            if complete% > 0% then enabled% = 0%
            return

L21480: REM DEFAULT/ENABLE FOR STORE
            inpmessage$ = "Enter the Store the Material was Issued from "~
                        & "or '*' for multiple store/lots."
            savestore$ = store$(c%) : savelot$ = lot$(c%)
            if type%(c%) = 0% then L21530
            temp = 0: convert qtyissued$(c%) to temp, data goto L21520
L21520:     if temp > 0 then L21550
L21530:        enabled% = 0%
               return
L21550:     if complete% > 1% then L21580
            if store$(c%) = " " then store$(c%) = warehouse$
               return
L21580:     if store$(c%) = " " then store$(c%) = compstore$
               if keyhit% = 4% then return
               goto L21530

L21630: REM DEFAULT/ENABLE FOR LOT
            inpmessage$ = "Enter the Lot from which the Material was "   ~
                        & "Issued"
            savestore$ = store$(c%) : savelot$ = lot$(c%)
            if type%(c%)  = 0%    then L21720
            if store$(c%) = "***" then L21720
            call "LOTENABL" (part$(c%,1), lot_enabl%, ll%, #54, #3)
            if lot_enabl% = 0% then L21720
            temp = 0: convert qtyissued$(c%) to temp, data goto L21700
L21700:     if temp <= 0 then L21720
            if complete% < 2% then return
               if lot_enabl% =  1% then L21720   /* Memo only         */
               if complot$   = " " then return  /* Blank, Must Enter */
               lot$(c%) = complot$              /* Not Blank, Try It */
L21720:        enabled% = 0%
               return

L21750: REM DEFAULT/ENABLE FOR QUANTITY OPEN/DATE REQUIRED
            if qtyrem$(c%,2%) = " " or ~
               qtyrem$(c%,2%) = blankdate$ then qtyrem$(c%,2%) = postdate$
            if qtyrem$(c%,1%) <> " " then L21800
               call "CONVERT" (0, 0.2, qtyrem$(c%,1%))
               goto L21820
L21800:     if t% <> 0% then L21820
            if edit% <> 0% and pipout$(c%) <> " " then L21840
L21820:        enabled% = 0%
               return
L21840:     call "STRING" addr("LJ", qtyrem$(c%,1%), 10%)
            if complete% > 0% then L21820
               return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
        init(" ")                                                        ~
            bkgpct$,                     /* Background Percent         */~
            bkgstr$,                     /* Background Store           */~
            bkglot$,                     /* Background Lot             */~
            diststore$(),                /* Dist. Stores               */~
            distlot$(),                  /* Dist. Lots                 */~
            distqty$(),                  /* Dist. Quantity             */~
            duedate$,                    /* Issue Components Due By    */~
            errormsg$,                   /* Error message              */~
            fpart$, savefpart$,          /* Display Variable           */~
            inpmessage$,                 /* Informational Message      */~
            job$,                        /* Job Order No.              */~
            jobdescr$,                   /* Job Order No.              */~
            jobpart$,                    /* Building Part              */~
            jobpartdesc$,                /* Part Descr.                */~
            jobqty$,                     /* Qty. to Build              */~
            jobstart$,                   /* Job Started                */~
            jobend$,                     /* Job Complete               */~
            lot$(),                      /* From Lot                   */~
            part$(),                     /* Part Code and Descr. Array */~
            p3msg1$,                     /* Line 24 1st Half Msg       */~
            p3msg2$,                     /* Line 24 2nd Half Msg       */~
            pipout$(),                   /* PIPOUT to honor and obey   */~
            qtyreq$(),                   /* Pipout Quantity            */~
            qtyissued$(),                /* Issued Quantity            */~
            qtyrem$(),                   /* Open   Quantity            */~
            rftg$,                       /* Generic Xref Tag           */~
            store$(),                    /* From Warehouse             */~
            storelot$(),                 /* From Warehouse (Options)   */~
            warning$

            auto%, complete%, maxlines%, c%, s%, t%, l%, edit%,          ~
            maxdist%, lastkey%, bkgcomp% = 0%

            mat distindex% = zer
            mat type%      = con:mat type% = (-1) * type%
            mat optqty     = zer

            call "JBINUSE" (" ", 1%)  /* Clears In-use Flag for User */
            call "ALLFREE"

            dfac$ = hex(9c)

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

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            REM *** Check to See if we must clear Serial Nbrs distribution
            qty = 0
            call "SERKIT" (jobpart$,     /* Part code to Kit To        */~
                           " ",          /* Component Part Code to Kit */~
                           " ",          /* S/N Location to Select from*/~
                           " ",          /* (Store and Lot Codes).     */~
                           jobrmdr,      /* Qty to Build in Job        */~
                           qty,          /* Comp.  Qty to Issue / kit. */~
                           0%,           /* Index Ptr For Work File Use*/~
                           maxlines%,    /* Average # Lines per Documnt*/~
                           "JK",         /* Source Transaction Type.   */~
                                         /* (JK = Job Kitting).        */~
                           job$,         /* Source Transaction Key     */~
                           "1",          /* Status of Parent S/N's to  */~
                                         /* Kit To.                    */~
                           "2",          /* Status of Component S/N's  */~
                                         /* to Kit From.               */~
                           2%,           /* Operation to Perform       */~
                                         /* 0% = Normal Input/Edit,    */~
                                         /*      by INDEX%,            */~
                                         /* 2% = Start Over, if INDEX% */~
                                         /*      > 0% then only the    */~
                                         /*      line/data pointed to  */~
                                         /*      by INDEX% is cleared, */~
                                         /*      else ALL is cleared.  */~
                           errormsg$,    /* Returned Error Message     */~
                           #54,          /* SYSFILE2 UFB               */~
                           #3,           /* HNYMASTR UFB               */~
                           #62,          /* SERMASTR UFB               */~
                           #61,          /* SERTIF   UFB               */~
                           #63)          /* SERWORK  UFB               */
            goto inputmode

        clear_ser_distr
            REM *** Clear if PF4 Pressed or Store/Lot Changed or Qty = 0**
            savestore$=store$(c%) : savelot$=lot$(c%)
            if str(store$(c%),,1%) = "*" then clear_lot_sn_distr
            qty = 0 : convert qtyissued$(c%) to qty, data goto L29505
            index% = c%

        clear_serial_numbers
L29505:     call "SERKIT" (jobpart$,     /* Part code to Kit To        */~
                           part$(c%,1%), /* Component Part Code to Kit */~
                           store$(c%),   /* S/N Location to Select from*/~
                           lot$(c%),     /* (Store and Lot Codes).     */~
                           jobrmdr,      /* Qty to Build in Job        */~
                           qty,          /* Comp.  Qty to Issue / kit. */~
                           index%,       /* Pointer For Work File Use  */~
                           maxlines%,    /* Average # Lines per Documnt*/~
                           "JK",         /* Source Transaction Type.   */~
                                         /* (JK = Job Kitting).        */~
                           job$,         /* Source Transaction Key     */~
                           "1",          /* Status of Parent S/N's to  */~
                                         /* Kit To.                    */~
                           "2",          /* Status of Component S/N's  */~
                                         /* to Kit From.               */~
                           2%,           /* Operation to Perform       */~
                                         /* 0% = Normal Input/Edit,    */~
                                         /*      by INDEX%,            */~
                                         /* 2% = Start Over, if INDEX% */~
                                         /*      > 0% then only the    */~
                                         /*      line/data pointed to  */~
                                         /*      by INDEX% is cleared, */~
                                         /*      else ALL is cleared.  */~
                           errormsg$,    /* Returned Error Message     */~
                           #54,          /* SYSFILE2 UFB               */~
                           #3,           /* HNYMASTR UFB               */~
                           #62,          /* SERMASTR UFB               */~
                           #61,          /* SERTIF   UFB               */~
                           #63)          /* SERWORK  UFB               */
            return

        clear_lot_sn_distr
            if maxdist% = 0% then return
            for j% = 1% to maxdist%
                if distindex%(j%) <> u3% then L29710
                   store$(c%) = diststore$(j%)
                   lot$  (c%) = distlot$  (j%)
                   index%     = distinptr%(j%)
                   convert distqty$(j%) to qty, data goto L29710
                   gosub clear_serial_numbers
L29710:     next j%
            store$(c%) = savestore$ : lot$(c%) = savelot$
            return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            hits%, maxlines% = 0%
            call "DATUNFMT" (basedate$)
            init (hex(00)) str(plowkey$,20)
            gosub sort_da_pip
            break% = 19%
            if file% = 34% then L30150
                plowkey$ = hex(00)
                break% = 0%

L30150:     call "PLOWNEXT" (#file%, plowkey$, break%, f1%(file%))
               if f1%(file%) = 0 then exit_dataload
            temp% = maxlines% + 1%
            if file% = 17% then L30230

            get #34, using L30200, pipout$(temp%)
L30200:     FMT CH(64)
            goto L30250

L30230:     get #17, using L30240, pipout$(temp%)
L30240:     FMT POS(17), CH(64)

L30250:     get pipout$(temp%), using L30260, datedue%, qty
L30260:         FMT POS(45), BI(4), POS(57), PD(14,4)
            if qty <= 0 then L30150

            call "READ100" (#3, str(pipout$(temp%),20,25), f1%(3))
               if f1%(3) = 0% then L30150
            get #3, using L30320, temp$
L30320:         FMT POS(180), CH(3)
            convert temp$ to type%, data goto L30150
            if type% > 489% and type% < 500% then L30150
            if type% > 789% and type% < 800% then L30150

            hits% = hits% + 1%

            if datedue% > duedate% then L30150

            if maxlines% = 0% then call "SHOSTAT" ("Loading Kit List")

            call "CONVERT" (qty, 0.2, qtyreq$(temp%))
            call "CONVERT" (  0, 0.2, qtyissued$(temp%))
            call "CONVERT" (qty, 0.2, qtyrem$(temp%,1%))
            type%(temp%) = type%

            part$(temp%, 1%) = str(pipout$(temp%),20,25)
            get #3, using L30500, part$(temp%, 2%), uom$
L30500:         FMT POS(26), CH(32), POS(74), CH(4)
            call "DATE"addr("G+", str(basedate$,,6), datedue% - 1%,      ~
                                  str(qtyrem$(temp%, 2%),,6), u3%)
            call "DATEFMT" (qtyrem$(temp%, 2%))
            str(qtyrem$(temp%, 2%),10%) = uom$
            maxlines% = temp%
            if type%(temp%) = 0% then gosub L30710
            if maxlines% < linemax% then L30150

        exit_dataload
            if maxlines% < linemax% then pipout$(maxlines% + 1%) = " "
            call "DATEFMT" (basedate$)
            if maxlines% < hits% then                                    ~
               warning$ = "Warning, not all parts are due by the date spe~
        ~cified."
            if maxlines% = linemax% then                                 ~
               warning$ = "There may be more components to be kitted. Rep~
        ~eat this function as needed."
            return

        REM SOME CODE TO TAKE CARE OF TYPE 000 PARTS

L30710:     if type% <> 0% then return
            if temp% = 1% then L30740
            if part$(temp%,1%) = part$(temp%-1%,1%) then L30750
L30740:     init (" ") rftg$
L30750:     call "PLOWNEXT" (#36, str(pipout$(temp%),1,44) & rftg$,      ~
                                                            44%, f1%(36))
            if f1%(36) <> 0 then L30790
               type%(temp%) = 500%:return

L30790:     get #36, using L30800, rftg$
L30800:         FMT POS(45), CH(19)
            if str(rftg$,1,2) <> "JO" then return
            init (hex(00)) jbcrkey$ : str(jbcrkey$,,8) = str(rftg$,12,8)

L30830:     optqty(temp%) = 0
            call "PLOWNEXT"(#7, jbcrkey$, 8%, f1%(7))
               if f1%(7)=0 then L30950
            get #7,using L30930, storelot$(temp%)
            if str(storelot$(temp%),1,1) <> "*" then L30870
L30862:        storelot$(temp%) = " "
               goto L30830
L30870:     str(storelot$(temp%),10) = " "
            call"READ100"(#52,str(part$(temp%,1%),1,25) &                ~
                                     str(storelot$(temp%),1,19), f1%(52))
               if f1%(52)=0 then L30862
            get #52, using L30920, optqty(temp%)
L30920:         FMT POS(69), PD(14,4)
L30930:         FMT POS(48), CH(9)
            if optqty(temp%) > 0 then return
            goto L30862

L30950:     call"PLOWNEXT"(#36, str(pipout$(temp%),1,44) & rftg$,        ~
                                                           44%, f1%(36))
            if f1%(36) = 0 then return else goto L30790

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            if bkgcomp% <> 0% then dataputb
            if maxlines% = 0% then return
            call "SHOSTAT" ("Updating Transaction Image File")
            call "DATUNFMT" (postdate$)
            hits% = 0%

            for i%=1 to maxlines%

            qty = 0:convert qtyissued$(i%) to qty, data goto L31140
L31140:     temp = 0:convert qtyrem$(i%,1%) to temp, data goto L31150
L31150:     temp1 = 0:convert qtyreq$(i%) to temp1, data goto L31170

L31170:     if temp1 = 0 then type% = 2% else type% = 1%
               adjust =  temp1 - (qty + temp)
                 if adjust = 0 and qty = 0 then L31470

            if postflag% <> 0% then L31280
                return% = 0
                moduleno$ = modno$
                call "JNLINFO" (moduleno$, jnlid$, pstseq%, summary$,    ~
                                tttle$, postdate$, #54, f2%(54), return%)
                postflag% = 99%

L31280:     init ("0") tttle$
            call "CONVERT" (adjust, -0.2, str(tttle$,,10))

            if store$(i%) = "***" then L31360
               index% = i%
               gosub post_to_tif
               hits% = hits% + 1%
               goto L31470

L31360:     if maxdist% = 0% then L31470

            for j% = 1% to maxdist%
                if distindex%(j%) <> i% then L31460
                   store$(i%) = diststore$(j%)
                   lot$  (i%) = distlot$  (j%)
                   index% = distinptr%(j%)
                   convert distqty$(j%) to qty, data goto L31460
                      gosub post_to_tif
                      hits% = hits% + 1%
                   init("0") tttle$
L31460:     next j%
L31470:     next i%

            if hits% = 0% then L31510
            call "JB2TIF" ("J1", 2%, return%)  /* Process entries */
L31510:     call "DATEFMT" (postdate$)
            return

        post_to_tif

            call "HNYHOLD" (#52, part$(i%,1%), store$(i%), lot$(i%),     ~
                                                            qty, return%)

            REM *** Check to See if we must distribute to Serial Nbrs ***
            call "SERKIT" (jobpart$,     /* Part code to Kit To        */~
                           part$(i%,1%), /* Component Part Code to Kit */~
                           store$(i%),   /* S/N Location to Select from*/~
                           lot$(i%),     /* (Store and Lot Codes).     */~
                           jobrmdr,      /* Qty to Build in Job        */~
                           qty,          /* Comp.  Qty to Issue / kit. */~
                           index%,       /* Pointer For Work File Use  */~
                           maxlines%,    /* Average # Lines per Documnt*/~
                           "JK",         /* Source Transaction Type.   */~
                                         /* (JK = Job Kitting).        */~
                           job$,         /* Source Transaction Key     */~
                           "1",          /* Status of Parent S/N's to  */~
                                         /* Kit To.                    */~
                           "2",          /* Status of Component S/N's  */~
                                         /* to Kit From.               */~
                           1%,           /* Operation to Perform       */~
                                         /* 0% = Normal Input/Edit,    */~
                                         /*      by INDEX%,            */~
                                         /* 1% = Save Specified Line   */~
                                         /* 2% = Start Over, if INDEX% */~
                                         /*      > 0% then only the    */~
                                         /*      line/data pointed to  */~
                                         /*      by INDEX% is cleared, */~
                                         /*      else ALL is cleared.  */~
                           errormsg$,    /* Returned Error Message     */~
                           #54,          /* SYSFILE2 UFB               */~
                           #3,           /* HNYMASTR UFB               */~
                           #62,          /* SERMASTR UFB               */~
                           #61,          /* SERTIF   UFB               */~
                           #63)          /* SERWORK  UFB               */

            str(tttle$,38%,3%) = bin(index%,3)

            call "JB2TIF"               /* Writes to Transaction Image */~
                 ("J1",                 /* Send transaction to JBPOST1 */~
                  0%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  type%,                /* Transaction type (1 = kit), */~
                                        /*        (2 = manual)         */~
                  hex(80),              /* Priority                    */~
                  job$,                 /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  postdate$,            /* G/L posting date            */~
                  part$(i%,1%),         /* Inventory Part Code         */~
                  store$(i%),           /* Inventory Store Code        */~
                  lot$(i%),             /* Inventory Lot Id.           */~
                  qty,                  /* Quantity to process         */~
                  str(pipout$(i%),,56), /* PIPOUT tag if kitting       */~
                  tttle$, " ")          /* Passes Adjustment, N/A      */

            return

        REM *************************************************************~
            * Distribute Withdrawal                                     *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************
        dist_quantity

            init (" ") edtstore$(), edtlot$(), edtqty$()
            hole%, edtmax% = 0%
            mat edtindex% = zer

            if maxdist% = 0% then L32120

            for i% = 1% to maxdist%
                if distindex%(i%) <> c% then L32105
                   distindex%(i%) =  0%
                   edtmax% = edtmax% + 1%
                   edtstore$(edtmax%) = diststore$(i%)
                   edtlot$  (edtmax%) = distlot$  (i%)
                   edtqty$  (edtmax%) = distqty$  (i%)
                   edtindex%(edtmax%) = distinptr%(i%)
L32105:         if distindex%(i%) = 0% then hole% = hole% + 1%
            next i%

L32120:     edtqty = 0: convert qtyissued$(c%) to edtqty, data goto L32130

L32130:     maxallowed% = min(100% - edtmax%, distmax% + hole% - maxdist%)
            errormsg$ = "Issue Components to Job: " & job$

            call "LOTDISN" (part$(c%,1%),/* Part Code                  */~
                           edtstore$(),  /* Passed Stores              */~
                           edtlot$(),    /* Passed Lots                */~
                           edtqty$(),    /* Passed Quantities          */~
                           errormsg$,    /* Oops                       */~
                           1%,           /* Withdraw                   */~
                           edtmax%,      /* Current Maxlines           */~
                           maxallowed%,  /* Maximum Desired            */~
                           edtqty,       /* Quantity Desired           */~
                           edtindex%(),  /* S/N's index pointers       */~
                           jobpart$,     /* Part code to Kit To        */~
                           jobrmdr,      /* Qty to Build in Job        */~
                           c%,           /* Pointer For Work File Use  */~
                           maxlines%*10%,/* Average # Lines per Documnt*/~
                           "JK",         /* Source Transaction Type.   */~
                                         /* (JK = Job Kitting).        */~
                           job$,         /* Source Transaction Key     */~
                           "1",          /* Status of Parent S/N's to  */~
                                         /* Kit To.                    */~
                           "2",          /* Status of Component S/N's  */~
                                         /* to Kit From.               */~
                           "3",          /* Destination Status of      */~
                                         /* Component S/N's.           */~
                           #3,           /* HNYMASTR                   */~
                           #52,          /* HNYQUAN                    */~
                           #59,          /* STORNAME                   */~
                           #54,          /* SYSFILE2                   */~
                           #62,          /* SERMASTR UFB               */~
                           #61,          /* SERTIF   UFB               */~
                           #63)          /* SERWORK  UFB               */~

*          IF ERRORMSG$ <> " " THEN RETURN

            if edtmax% > 1% or p_enabled% = 1% then L32320
               store$(c%) = edtstore$(1%)
               lot$  (c%) = edtlot$  (1%)
               return

L32320:     placed% = 0%

            if maxdist% = 0% then L32390

            for i% = 1% to maxdist%
                if distindex%(i%) <> 0% then L32380
                   placed% = placed% + 1%
                   diststore$(i%) = edtstore$(placed%)
                   distlot$  (i%) = edtlot$  (placed%)
                   distqty$  (i%) = edtqty$  (placed%)
                   distinptr%(i%) = edtindex%(placed%)
                   distindex%(i%) = c%
                if placed% = edtmax% then L32430
L32380:     next i%

L32390:     for i% = placed% + 1% to edtmax%
                maxdist% = maxdist% + 1%
                diststore$(maxdist%) = edtstore$(i%)
                distlot$  (maxdist%) = edtlot$  (i%)
                distqty$  (maxdist%) = edtqty$  (i%)
                distinptr%(maxdist%) = edtindex%(i%)
                distindex%(maxdist%) = c%
            next i%

L32430:     if errormsg$ <> " " then return

            for i% = 1% to edtmax%
            temp = 0
            if edtqty$(i%) = " " then L33100
            for j% = i% to edtmax%
                if edtqty$  (j%) = " " then L32490
                if edtlot$  (j%) <> edtlot$  (i%) then L32490
                if edtstore$(j%) <> edtstore$(i%) then L32490
                   temp1 = 0:convert edtqty$(j%) to temp1, data goto L32485
                   temp = temp + temp1
L32485:            edtqty$(j%) = " "
L32490:     next j%

            for test% = 1% to maxlines%
                if test% = c% then L33030
                if part$(c%,1%) <> part$(test%,1%) then L33030
                if store$(test%) = "***" then L32555
                if edtlot$(i%) <> lot$(test%) then L33030
                if edtstore$(i%) <> store$(test%) then L33030
                    temp1 = 0:convert qtyissued$(test%) to temp1,        ~
                                                     data goto L33030
                    temp = temp + temp1
                    goto L33030

L32555:         for testd% = 1% to maxdist%
                    if distindex%(testd%) <> test% then L33010
                    if diststore$(testd%) <> edtstore$(i%) then L33010
                    if distlot$(testd%) <> edtlot$(i%) then L33010
                       temp1 = 0:convert distqty$(testd%) to temp1,      ~
                                                     data goto L33010
                       temp = temp + temp1
L33010:         next testd%

L33030:     next test%

            if temp = 0 then L33100
               call "HNYAVAIL" (#3, #52, part$(c%,1%), edtstore$(i%),    ~
                      edtlot$(i%), errormsg$, temp, temp1, f1%(52))
                  if errormsg$ = " " then L33100
            errormsg$ = "Distribution Conflict.  Store:" & edtstore$(i%)
            errormsg$ = errormsg$ & " Lot:" & edtlot$(i%)
            return

L33100:     next i%
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        sort_da_pip
            REM Clear Work File, sort PIPOUTS if required...
            if file% = 34% then return /* No Sort */
                call "SHOSTAT" ("Kit List Sort in Process. . .")
                call "DELETE" (#17, " ", 0%)
                pipoutplow$ = plowkey$
        sort_loop
                call "PLOWNEXT" (#34,pipoutplow$,19%,f1%(34))
                   if f1%(34) = 0 then return
                datedue% = val(str(pipoutplow$,45,4),4)
                if datedue% > duedate% then sort_loop
                get #34, using L35100, pipoutrec$
L35100:         FMT CH(64)
                part$ = str(pipoutrec$,20,25)

                REM Generate Work File To Get Desired Sort Order...
                on pos("TLCSB"=sort$) goto L35250, /* Part Type Order   */~
                                           L35350, /* Bin Location Order*/~
                                           L35530, /* Part Category Orde*/~
                                           L35620, /* Part Class Order  */~
                                           L35710  /* BOM Order         */

                REM Contingency Plan...
                    file% = 34%  /* No Sort */
                    sort$ = "P"
                    return

L35250:         REM Sort into Part Type order...
                    parttype$ = "????"
                    call "READ100" (#3, part$, f1%(3))
                        if f1%(3) = 0 then L35310
                    get #3, using L35300, parttype$
L35300:             FMT POS(180), CH(3)
L35310:             write #17, using L35320, parttype$, pipoutrec$
L35320:             FMT CH(16), CH(64)
                    goto sort_loop  /* Go Get Next One */

L35350:         REM Sort into Bin Location order...
                    bin$ = " "
                    call "READ100" (#3, part$, f1%(3))
                        if f1%(3) = 0 then L35500
                    get #3, using L35400, bin$
L35400:             FMT POS(155), CH(8)
                    if bin$ <> " " then L35500

                    REM Check HNYQUAN Files For Bin Location...
                    readkey$ = part$
L35450:             call "PLOWNEXT" (#52, readkey$, 25%, f1%(52))
                        if f1%(52) = 0 then L35500
                    get #52, using L35480, bin$
L35480:             FMT POS(61), CH(8)
                    if bin$ = " " then L35450
L35500:             write #17, using L35320, bin$, pipoutrec$
                    goto sort_loop  /* Go Get Next One */

L35530:         REM Sort into Part Category order...
                    pcat$ = "????"
                    call "READ100" (#3, part$, f1%(3))
                        if f1%(3) = 0 then L35590
                    get #3, using L35580, pcat$
L35580:             FMT POS(90), CH(4)
L35590:             write #17, using L35320, pcat$, pipoutrec$
                    goto sort_loop  /* Go Get Next One */

L35620:         REM Sort into Part Class order...
                    pclass$ = "????"
                    call "READ100" (#3, part$, f1%(3))
                        if f1%(3) = 0 then L35680
                    get #3, using L35670, pclass$
L35670:             FMT POS(133), CH(4)
L35680:             write #17, using L35320, pclass$, pipoutrec$
                    goto sort_loop  /* Go Get Next One */

L35710:         REM Sort into BOM order (via SYSTIME stamp on PIPOUT)...
                    write #17,using L35320,str(pipoutrec$,49,8),pipoutrec$
                    goto sort_loop  /* Go Get Next One */


        REM *************************************************************~
            *   A C C E S S    L O C A T I O N    M A N A G E M E N T   *~
            *___________________________________________________________*~
            * Call HNYLCSUB and pass the Part, Store, Lot and Quantity  *~
            * so that the user does not have to re-enter them.          *~
            *************************************************************

         locations
            if qtyreq$(tempc%) <> " " then L36120
               qty = 0
               goto L36140

L36120:     convert qtyreq$(tempc%) to qty

L36140:     call "HNYLCSUB"  (part$(tempc%,1%),                          ~
                              store$(tempc%),                            ~
                              lot$(tempc%),                              ~
                              qty,                                       ~
                              4%,    /*  Withdrawl Mode                */~
                              #54,   /*  SYSFILE2                      */~
                              #59,   /*  STORNAME                      */~
                              #20,   /*  USERINFO                      */~
                              #3,    /*  HNYMASTR                      */~
                              #18,   /*  HNYLOCNS                      */~
                              #52,   /*  HNYQUAN                       */~
                              #19)   /*  LOCATION                      */
            return

        REM *************************************************************~
            *        F I N D   P A R T   I N   K I T   L I S T          *~
            *************************************************************

            find_part

        REM Reset issued & remaining if in input mode, not accepted yet...
                if fieldnr% = 0% then L38200
                   if c% > maxlines% then L38140
                      qtyissued$(c%) = "0"
                      gosub'152(3%)
                      goto L38200

L38140:     part$(c%,1%), part$(c%,2%), qtyreq$(c%), qtyissued$(c%),     ~
            store$(c%), lot$(c%), qtyrem$(c%,1%), qtyrem$(c%,2%) = " "
            append% = 0%
            l% = max(0%, min(l%, maxlines% - 10%))

L38200:     errormsg$, warning$ = " "
            if fpart$ = " " then L38380
            st% = 1%
            if savefpart$ = " " then L38280
            if savefpart$ = fpart$ then L38260
               savefpart$ = " " : goto L38280

L38260:        st% = savep% + 1%
               if st% > maxlines% then L38340
L38280:     flen% = len(fpart$)
            for p% = st% to linemax%
              if part$(p%,1%) = " " then L38340
                 if str(part$(p%,1%),,flen%) = fpart$ then L38440
            next p%

L38340:     errormsg$ = "Part: " & fpart$ & " Not Found In Kit List"
            if savefpart$ = " " then L38380
               errormsg$ = "Part: " & fpart$
               errormsg$ = errormsg$ & " No More Occurances In Kit List"
L38380:     lastkey% = 0%
            dfac$ = hex(9c)
            fpart$ = " " : savefpart$ = " " : savep% = 1%
            warning$ = " "
            return clear all
            goto editpg1

L38440:     l% = max(0, min(p% - 1%, maxlines% - 10%))
            c% = p%
            s% = c% - l%
            if savefpart$ = " " then L38510
               put warning$ using L38490, savep%, p%
L38490: %Last Occurance at line ####: Currently Found at line ####
               goto L38540
L38510:     put warning$ using L38520, p%
L38520: %Part Found at line ####

L38540:     savefpart$ = fpart$ : savep% = p%
            lastkey% = 0% : edit% = 1% : fullvalid% = 1%
            dfac$ = hex(9c)
            fieldnr% = 3%
            return clear all
            goto editpg1a

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataputb
            if bkgcomp%  = 0% then return
            call "SHOSTAT" ("Updating Transaction Image File")
            call "DATUNFMT" (postdate$)

            if postflag% <> 0% then L39220
                return% = 0
                moduleno$ = modno$
                call "JNLINFO" (moduleno$, jnlid$, pstseq%, summary$,    ~
                                tttle$, postdate$, #54, f2%(54), return%)
                postflag% = 99%

L39220:     bkgpct = 100 * bkgpct

            call "JB2TIF"               /* Writes to Transaction Image */~
                 ("J1",                 /* Send transaction to JBPOST1 */~
                  0%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  3%,                   /* Transaction type (3=kit cmp)*/~
                  hex(35),              /* Priority                    */~
                  job$,                 /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  postdate$,            /* G/L posting sequence number */~
                  " ",                  /* Inventory Part Code         */~
                  bkgstr$,              /* Inventory Store Code        */~
                  bkglot$,              /* Inventory Lot Id.           */~
                  bkgpct,               /* Quantity to process         */~
                  " ",                  /* Not used                    */~
                  " ",                  /* Not used                    */~
                  " ")                  /* Not used                    */~

            call "JB2TIF" ("J1", 2%, return%)  /* Process entries */
            call "DATEFMT" (postdate$)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              str(line2$,,40%) = " "
              init (hex(9c)) fac$()
              init (hex(84)) lfac$()

              pfmsg$(1) = "(1)Start Over"
              str(pfmsg$(1),64) = "(13)Instructions"

              pfmsg$(2) = " "
              str(pfmsg$(2),64) = "(15)Print Screen"

              p3msg1$ = " "
              p3msg2$ = "  (16)Exit Program"

              pfkey$ = hex(0001ffff0d0f10)

              if fieldnr% > 1% then L40260
                 str(pfmsg$(1),20,44) = " "
                 str(pfkey$,3,2) = hex(ffff)
                 goto L40290

L40260:          str(pfmsg$(1),20,18) = "(4)Previous Field"
                 str(pfkey$,3,1) = hex(04)

                 str(pfmsg$(1),40,23) = "(10)Backgound Kit Comp."
                 str(pfkey$,4,1) = hex(0a)

L40290:       on fieldnr% gosub L40340,         /* Job Number        */   ~
                                L40340          /* Due By            */
              goto L45000

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40340:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen. (Lines)                   *~
            *************************************************************

        deffn'102(fieldnr%, screen%)

              sc1%, sc2% = 0%
              if maxlines% = 0% then L41065
              sc1% = l% + 1% : sc2% = min(l% + 10%, maxlines%)
L41065:       put str(line2$,,40%) using L41066, sc1%, sc2%, maxlines%
L41066:           %Current: Lines #### thru #### of ####

              init (hex(84)) lfac$()
              if fieldnr% > 0% then L41220
                 init(hex(86)) fac$()

              pfmsg$(1%)= "(1)St Over  (4)Prev Page  (6)Prev Line  (8)Chg~
        ~ Disp  (9) Auto  (13)Instructions"
              pfmsg$(2%)= "(2)First    (5)Next Page  (7)Next Line (10)Kit~
        ~ Compl (11)Appnd (15)Print Screen"
              p3msg1$   = "(3)Last     (24)Locns    (26)Find"
              p3msg2$   = "  (16)Save Data"
              dfac$   = hex(9c) :  lastkey% = 0%
              pfkey$ = hex(000102030405060708090a0b0d0f10181a)

              if l% > 0% then L41188
                 str(pfmsg$(1%),13%,27%) = " "
                 str(pfmsg$(2%), 1%,12%) = " "
                 str(pfkey$,3%,1%) = hex(ff)
                 str(pfkey$,5%,1%) = hex(ff)
                 str(pfkey$,7%,1%) = hex(ff)

L41188:      if l% + 10% < maxlines% then L45000
                 str(pfmsg$(2%),13%,27%) = " "
                 str(p3msg1$   , 1%,12%) = " "
                 str(pfkey$,4%,1%) = hex(ff)
                 str(pfkey$,6%,1%) = hex(ff)
                 str(pfkey$,8%,1%) = hex(ff)
                 goto L45000

L41220:       pfmsg$(1) = "(1)Start Over"
              str(pfmsg$(1),64) = "(13)Instructions"

              pfmsg$(2) = " "
              str(pfmsg$(2),41) = "(8)Change Display"
              str(pfmsg$(2),64) = "(15)Print Screen"

              p3msg1$ = "(22)Qty Info  (24)Locns  (26)Find"
              p3msg2$ = "  (16)Edit Mode"
*            FPART$  = " "
              dfac$ = hex(9c)  :  lastkey% = 0%

              pfkey$ = hex(0001ffff080d0f1018161a)

              init(hex(84)) fac$()
              if edit% <> 0% then L41570
                 if fieldnr% > 3% then L41430
L41370:             str(pfmsg$(1),20,18) = " "
                    str(pfkey$,3,1) = hex(ff)
                    p3msg2$ = "  (16)Edit Mode"
                    str(pfkey$,8,1) = hex(10)
                      goto L41480

L41430:             str(pfmsg$(1),20,18) = "(4)Previous Field"
                    str(pfkey$,3,1) = hex(04)
                    p3msg2$ = " "
                    str(pfkey$,8,1) = hex(ff)

L41480:          if c% > 1% then L41530
                    str(pfmsg$(2),20,18) = " "
                    str(pfkey$,4,1) = hex(ff)
                       goto L41630

L41530:             str(pfmsg$(2),20,18) = "(6)Previous Line"
                    str(pfkey$,4,1) = hex(06)
                       goto L41630

L41570:       if c% <= maxlines% then L41600
                 if fieldnr% > 1% then L41430 else goto L41370

L41600:             p3msg2$ = " "
                    str(pfkey$,8,1) = hex(ff)

L41630:       on fieldnr% gosub L41720,         /* Part              */   ~
                                L41730,         /* Quantity Req.     */   ~
                                L41730,         /* Quantity Iss.     */   ~
                                L41720,         /* Store             */   ~
                                L41720,         /* Lot               */   ~
                                L41720          /* Quantity Open     */
              goto L45000

              fac$(screen%,fieldnr%) = hex(80):return   /* Up / Low   */
L41720:       fac$(screen%,fieldnr%) = hex(81):return   /* Upper Only */
L41730:       fac$(screen%,fieldnr%) = hex(82):return   /* Numeric    */

L45000: REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

L45060:     accept                                                       ~
               at (01,02),                                               ~
                  "Issue Materials to Jobs",                             ~
               at (01,58), "Posting Date:",                              ~
               at (01,72), fac(hex(8c)), postdate$              , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), warning$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Job Order No.:",                             ~
               at (06,17), fac(lfac$( 1)), job$                 , ch(08),~
               at (06,28), fac(hex(8c))  , jobdescr$            , ch(30),~
                                                                         ~
               at (07,02), "Building Part:",                             ~
               at (07,17), fac(hex(84))  , jobpart$             , ch(25),~
               at (07,44), fac(hex(8c))  , jobpartdesc$         , ch(34),~
                                                                         ~
               at (08,02), "Qty. to Build:",                             ~
               at (08,17), fac(hex(84))  , jobqty$              , ch(10),~
                                                                         ~
               at (08,28), "Started On:",                                ~
               at (08,40), fac(hex(84))  , jobstart$            , ch(08),~
                                                                         ~
               at (08,50), "Sch. Complete:",                             ~
               at (08,65), fac(hex(84))  , jobend$              , ch(08),~
                                                                         ~
               at (09,02), "Issue Components Due By:",                   ~
               at (09,30), fac(lfac$( 2)), duedate$             , ch(08),~
                                                                         ~
               at (10,02), fac(hex(ac))  , hdr$(t% + 1%)        , ch(79),~
                                                                         ~
               at (11,02), fac(fac$( 1,1)), part$(l%+ 1%,t%+1%) , ch(32),~
               at (12,02), fac(fac$( 2,1)), part$(l%+ 2%,t%+1%) , ch(32),~
               at (13,02), fac(fac$( 3,1)), part$(l%+ 3%,t%+1%) , ch(32),~
               at (14,02), fac(fac$( 4,1)), part$(l%+ 4%,t%+1%) , ch(32),~
               at (15,02), fac(fac$( 5,1)), part$(l%+ 5%,t%+1%) , ch(32),~
               at (16,02), fac(fac$( 6,1)), part$(l%+ 6%,t%+1%) , ch(32),~
               at (17,02), fac(fac$( 7,1)), part$(l%+ 7%,t%+1%) , ch(32),~
               at (18,02), fac(fac$( 8,1)), part$(l%+ 8%,t%+1%) , ch(32),~
               at (19,02), fac(fac$( 9,1)), part$(l%+ 9%,t%+1%) , ch(32),~
               at (20,02), fac(fac$(10,1)), part$(l%+10%,t%+1%) , ch(32),~
                                                                         ~
               at (11,35), fac(fac$( 1,2)), qtyreq$(l%+ 1%)     , ch(10),~
               at (12,35), fac(fac$( 2,2)), qtyreq$(l%+ 2%)     , ch(10),~
               at (13,35), fac(fac$( 3,2)), qtyreq$(l%+ 3%)     , ch(10),~
               at (14,35), fac(fac$( 4,2)), qtyreq$(l%+ 4%)     , ch(10),~
               at (15,35), fac(fac$( 5,2)), qtyreq$(l%+ 5%)     , ch(10),~
               at (16,35), fac(fac$( 6,2)), qtyreq$(l%+ 6%)     , ch(10),~
               at (17,35), fac(fac$( 7,2)), qtyreq$(l%+ 7%)     , ch(10),~
               at (18,35), fac(fac$( 8,2)), qtyreq$(l%+ 8%)     , ch(10),~
               at (19,35), fac(fac$( 9,2)), qtyreq$(l%+ 9%)     , ch(10),~
               at (20,35), fac(fac$(10,2)), qtyreq$(l%+10%)     , ch(10),~
                                                                         ~
               at (11,46), fac(fac$( 1,3)), qtyissued$(l%+ 1%)  , ch(10),~
               at (12,46), fac(fac$( 2,3)), qtyissued$(l%+ 2%)  , ch(10),~
               at (13,46), fac(fac$( 3,3)), qtyissued$(l%+ 3%)  , ch(10),~
               at (14,46), fac(fac$( 4,3)), qtyissued$(l%+ 4%)  , ch(10),~
               at (15,46), fac(fac$( 5,3)), qtyissued$(l%+ 5%)  , ch(10),~
               at (16,46), fac(fac$( 6,3)), qtyissued$(l%+ 6%)  , ch(10),~
               at (17,46), fac(fac$( 7,3)), qtyissued$(l%+ 7%)  , ch(10),~
               at (18,46), fac(fac$( 8,3)), qtyissued$(l%+ 8%)  , ch(10),~
               at (19,46), fac(fac$( 9,3)), qtyissued$(l%+ 9%)  , ch(10),~
               at (20,46), fac(fac$(10,3)), qtyissued$(l%+10%)  , ch(10),~
                                                                         ~
               at (11,57), fac(fac$( 1,4)), store$ (l%+ 1%)     , ch(03),~
               at (12,57), fac(fac$( 2,4)), store$ (l%+ 2%)     , ch(03),~
               at (13,57), fac(fac$( 3,4)), store$ (l%+ 3%)     , ch(03),~
               at (14,57), fac(fac$( 4,4)), store$ (l%+ 4%)     , ch(03),~
               at (15,57), fac(fac$( 5,4)), store$ (l%+ 5%)     , ch(03),~
               at (16,57), fac(fac$( 6,4)), store$ (l%+ 6%)     , ch(03),~
               at (17,57), fac(fac$( 7,4)), store$ (l%+ 7%)     , ch(03),~
               at (18,57), fac(fac$( 8,4)), store$ (l%+ 8%)     , ch(03),~
               at (19,57), fac(fac$( 9,4)), store$ (l%+ 9%)     , ch(03),~
               at (20,57), fac(fac$(10,4)), store$ (l%+10%)     , ch(03),~
                                                                         ~
               at (11,61), fac(fac$( 1,5)), str(lot$(l%+ 1%),,ll%),      ~
               at (12,61), fac(fac$( 2,5)), str(lot$(l%+ 2%),,ll%),      ~
               at (13,61), fac(fac$( 3,5)), str(lot$(l%+ 3%),,ll%),      ~
               at (14,61), fac(fac$( 4,5)), str(lot$(l%+ 4%),,ll%),      ~
               at (15,61), fac(fac$( 5,5)), str(lot$(l%+ 5%),,ll%),      ~
               at (16,61), fac(fac$( 6,5)), str(lot$(l%+ 6%),,ll%),      ~
               at (17,61), fac(fac$( 7,5)), str(lot$(l%+ 7%),,ll%),      ~
               at (18,61), fac(fac$( 8,5)), str(lot$(l%+ 8%),,ll%),      ~
               at (19,61), fac(fac$( 9,5)), str(lot$(l%+ 9%),,ll%),      ~
               at (20,61), fac(fac$(10,5)), str(lot$(l%+10%),,ll%),      ~
                                                                         ~
               at (11,68), fac(fac$( 1,6)), qtyrem$(l%+ 1%,t%+1%),ch(13),~
               at (12,68), fac(fac$( 2,6)), qtyrem$(l%+ 2%,t%+1%),ch(13),~
               at (13,68), fac(fac$( 3,6)), qtyrem$(l%+ 3%,t%+1%),ch(13),~
               at (14,68), fac(fac$( 4,6)), qtyrem$(l%+ 4%,t%+1%),ch(13),~
               at (15,68), fac(fac$( 5,6)), qtyrem$(l%+ 5%,t%+1%),ch(13),~
               at (16,68), fac(fac$( 6,6)), qtyrem$(l%+ 6%,t%+1%),ch(13),~
               at (17,68), fac(fac$( 7,6)), qtyrem$(l%+ 7%,t%+1%),ch(13),~
               at (18,68), fac(fac$( 8,6)), qtyrem$(l%+ 8%,t%+1%),ch(13),~
               at (19,68), fac(fac$( 9,6)), qtyrem$(l%+ 9%,t%+1%),ch(13),~
               at (20,68), fac(fac$(10,6)), qtyrem$(l%+10%,t%+1%),ch(13),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg$(1%)             , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg$(2%)             , ch(79),~
               at (24,02), fac(hex(8c)), p3msg1$                , ch(33),~
               at (24,36), fac(dfac$),   fpart$                 , ch(25),~
               at (24,63), fac(hex(8c)), p3msg2$                , ch(18),~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 13% then L46170
                  call "MANUAL" ("JBKITSUB")
                  goto L45060

L46170:        if keyhit% <> 15% then L46210
                  call "PRNTSCRN"
                  goto L45060

L46210:         if keyhit% <> 22% then L46250
                    call "HNYQDISP" (part$(c%,1%), #3, #52, #52, #54)
                    goto L45060

L46250:         if keyhit% <> 24% then L46290
                    if c% = 0% then L46258
                          tempc% = c%
                          goto L46268
L46258:             call "GETSCRN" ("C", " ", cursor%(), 0%)
                    if cursor%(1%) < 11% or cursor%(1%) > 20% then L45060
                          temps% = cursor%(1%) - 10%
                          tempc% = temps% + l%
                          if tempc% > maxlines% then L45060
L46268:             gosub locations
                    goto L45060

L46290:         if lastkey% = 0% then L46340
                    gosub find_part
                    goto L45060

L46340:         if keyhit% <> 26% then L46410
                    init(hex(84)) fac$()
                    init(hex(81)) dfac$
                    lastkey% = 26%
                    inpmessage$ = "Enter Part Number and Press RETURN."
                    goto L45060

L46410:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%)
              str(line2$,,40%) = "Background Kit Complete"

              init (hex(84)) lfac$()

              pfmsg$(1) = "(1)Start Over"
              str(pfmsg$(1),64) = "(13)Instructions"

              pfmsg$(2) = " "
              str(pfmsg$(2),64) = "(15)Print Screen"

              p3msg1$ = " "
              p3msg2$ = "  (16)Save Data"

              pfkey$ = hex(00010d0f10)
              if fieldnr% > 0% then L47206
                 init (hex(86)) str(lfac$(),3%)
                 inpmessage$ = edtmessage$
                 goto L47420

L47206:       p3msg2$ = " "
              str(pfkey$,5%,1%) = hex(ff)

              on fieldnr% gosub     ,,         /* Skip 1 & 2        */   ~
                                L47371,         /* Quantity          */   ~
                                L47374          /* Store/Lot         */
              goto L47420

L47371:       inpmessage$ = "Enter Percentage of remaning kit list to"
              inpmessage$ = inpmessage$ & " process (Up To 100%)."
              goto L47390
L47374:       inpmessage$ = "Enter Store and Lot for Kitting."
              goto L47390

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L47390:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L47420:     accept                                                       ~
               at (01,02),                                               ~
                  "Issue Materials to Jobs",                             ~
               at (01,58), "Posting Date:",                              ~
               at (01,72), fac(hex(8c)), postdate$              , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), warning$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Job Order No.:",                             ~
               at (06,17), fac(lfac$( 1)), job$                 , ch(08),~
               at (06,28), fac(hex(8c))  , jobdescr$            , ch(30),~
                                                                         ~
               at (07,02), "Building Part:",                             ~
               at (07,17), fac(hex(84))  , jobpart$             , ch(25),~
               at (07,44), fac(hex(8c))  , jobpartdesc$         , ch(34),~
                                                                         ~
               at (08,02), "Qty. to Build:",                             ~
               at (08,17), fac(hex(84))  , jobqty$              , ch(10),~
                                                                         ~
               at (08,28), "Started On:",                                ~
               at (08,40), fac(hex(84))  , jobstart$            , ch(08),~
                                                                         ~
               at (08,50), "Sch. Complete:",                             ~
               at (08,65), fac(hex(84))  , jobend$              , ch(08),~
                                                                         ~
               at (09,02), "Issue Components Due By        :",           ~
               at (09,40), fac(lfac$( 2)), duedate$             , ch(08),~
                                                                         ~
               at (10,02), "Issue % of Remaining Kit List  :",           ~
               at (10,40), fac(lfac$( 3)), bkgpct$              , ch(10),~
                                                                         ~
               at (11,02), "Issue Components from Store/Lot:",           ~
               at (11,40), fac(lfac$( 4)), bkgstr$              , ch(03),~
               at (11,44), fac(lfac$( 4)), bkglot$              , ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg$(1%)             , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg$(2%)             , ch(79),~
               at (24,02), fac(hex(8c)), p3msg1$                , ch(33),~
               at (24,36), fac(dfac$),   fpart$                 , ch(25),~
               at (24,63), fac(hex(8c)), p3msg2$                , ch(18),~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 13% then L47940
                  call "MANUAL" ("JBKITSUB")
                  goto L47420

L47940:        if keyhit% <> 15% then L47980
                  call "PRNTSCRN"
                  goto L47420

L47980:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50110,         /* Job Number        */     ~
                              L50320          /* Due By            */
            return
L50110: REM Test for Job Order No.                JOB$
               call "GETCODE" (#4, job$, jobdescr$, 0%, 0, f1%(4))
               if f1%(4) <> 0 then L50160
                  errormsg$ = "Invalid Entry For Job Order Number"
                  return
L50160:        u3% = 2%   /* Check In-use & Write Flag if Free */
               call "JBINUSE" (job$, u3%)
                     if u3% = 0 then L50200
                     errormsg$ = hex(00)
                     return
L50200:        get #4, using L50220, str(plowkey$,,19), jobpart$, job_qty,~
                                    qty_comp, jobstart$, temp$, jobend$
L50220:        FMT POS(39), CH(19), CH(25), PD(14,4), PD(14,4), POS(147),~
                       CH(6), CH(6), POS(174), CH(6)
               call "SERENABL" (jobpart$, p_enabled%, u3%, #54, #3)
               jobrmdr = job_qty - qty_comp
               call "DATEFMT" (jobstart$)
               call "DATEFMT" (jobend$)
               call "GETCODE" (#3, jobpart$, jobpartdesc$, 1%,99, f1%(3))
                 if f1%(3) = 0% then jobpartdesc$ = "* * Not on File * *"
               call "CONVERT" (job_qty, -.2, jobqty$)
               if temp$ = " " or temp$ = blankdate$ then return
                  errormsg$ = "This Job is Closed."
                  call "JBINUSE" (job$, 1%)  /* Clears In-use for Job */
                  return

L50320: REM Test for Issue Components Due By      DUEDATE$
               call "DATEOK" (duedate$, 0%, errormsg$)
                  if errormsg$ <> " " then return
               call "DATUNFMT" (duedate$)
               call "PIPINDEX" (#54, duedate$, duedate%, u3%)
                   if u3% <> 0 then errormsg$ = "Date Must Be Between " &~
                                         basedate$ & " and " & lastdate$
               call "DATEFMT" (duedate$)
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub     L51200, /* Part                       */~
                                  L51400, /* Quantity Required          */~
                                  L51500, /* Quantity Issued            */~
                                  L52000, /* Store/Warehouse            */~
                                  L52400, /* Lot                        */~
                                  L53400  /* Quantity Open/Date Req.    */
            return

L51200: REM TEST DATA FOR PART
            if pipout$(c%) <> " " then L51320
               /* Done at data load, avoid problems with INP loop */
            call "GETCODE" (#3, part$(c%,1%), part$(c%,2%), 0%, 0, f1%(3))
               if f1%(3) <> 0% then L51250
                  errormsg$ = "Part Code Not on File."
                  return
L51250:     if part$(c%,1%) <> jobpart$ then L51280
               errormsg$ = "Cannot Issue the End Item."
               return
L51280:     if p_enabled% <> 0% then L51320
            call "SERENABL" (part$(c%,1%), c_enabled%, u3%, #54, #3)
               if c_enabled% = 0% then L51320
                  errormsg$ = "Serial Numbered Part, Cannot Kit to non-"
                  errormsg$ = errormsg$ & "Serial Numbered Parent."
                  return
L51320:     if fullvalid% = 0% then return

L51400: REM TEST DATA FOR QUANTITY REQUIRED
            /* SHOULD NEVER BE EDITED */
            if fullvalid% = 0% then return

L51500: REM TEST DATA FOR QUANTITY ISSUED
            if type%(c%) = 0 then temp1 = optqty(c%) else temp1 = 9e7
            call "NUMTEST" (qtyissued$(c%), 0, temp1, errormsg$, -0.2,   ~
                                                                    temp)
               if errormsg$ <> " " then return
            call "SERENABL" (part$(c%, 1%), enabled%, u3%, #54, #3)
            if enabled% = 0% or temp = int(temp) then L51600
                errormsg$ = "Serial numbered component parts must be " & ~
                            "kitted in whole numbers."
                return
L51600:     temp1 = 0: convert qtyreq$(c%) to temp1, data goto L51610
L51610:     if temp1 >= temp or append% = 1% then L51670
L51620:         u3% = 2%
                call "ASKUSER" (u3%, "***** OVER ISSUE WARNING *****",   ~
                     "The Issue Quantity is Greater Than The Required" & ~
                     " Quantity", " ", "Press RETURN to Continue...")
                if u3% <> 0% then L51620
L51670:     temp1 = max(0, temp1 - temp)
            call "CONVERT" (temp1, 0.2, qtyrem$(c%,1%))
            if temp > 0 then L51740
               store$(c%), lot$(c%) = " "
               gosub clear_ser_distr
               errormsg$ = " "
               return
L51740:     if fullvalid% = 0% then return
               savestore$ = store$(c%) : savelot$ = lot$(c%)

L52000: REM TEST DATA FOR STORE
            if savestore$ = " " or savestore$ = store$(c%) then L52030
               gosub clear_ser_distr
L52030:     temp = 0:convert qtyissued$(c%) to temp, data goto L52040
L52040:     if temp > 0 then L52070
               store$(c%), lot$(c%) = " "
               return
L52070:     if type%(c%) = 0% then L52190
            if auto% = 2% then L52190
            if str(store$(c%),,1) <> "*" then L52190
               store$(c%) = "***"
               gosub dist_quantity
                 if errormsg$ <> " " then return
               if store$(c%) = "***" then L52170
                  if fullvalid% <> 0% then L52260
                  if fieldnr% = 4% then fieldnr% = 5%
                  goto L52260
L52170:        lot$(c%) = " "
               return
L52190:     if auto% = 2 and store$(c%) = "***" then L52250
            if auto% < 2 then temp = 0 else temp = 99
            call "GETCODE" (#59, store$(c%), temp$, 0%, temp, f1%(59))
               if f1%(59) <> 0% then L52250
                  errormsg$ = "Store Not on File."
                  return
L52250:     if fullvalid% = 0% then return
L52260:        savestore$ = store$(c%) : savelot$ = lot$(c%)
               call "LOTENABL" (part$(c%,1), lot_enabl%, ll%, #54, #3)

L52400: REM TEST DATA FOR LOT
            if store$(c%) = "***" then L52470
            if savelot$ = " " or savelot$ = lot$(c%) then L52440
               gosub clear_ser_distr
L52440:     qty = 0 : convert qtyissued$(c%) to qty, data goto L52450
L52450:     if qty > 0 then L52500
               store$(c%) = " "
L52470:        lot$(c%) = " "
               return

L52500:     if lot_enabl% > 0% then L52540
               lot$(c%) = " "
               goto L52680

L52540:     if lot_enabl% > 1% then L52553
               if lot$(c%) = "?" then L52620
                  goto L52680

L52553:        if lot$(c%) <> " " then L52590
                  errormsg$ = "Lot Cannot be Blank."
                  return

L52590:     readkey$ = str(part$(c%,1%),,25) & str(store$(c%)) & lot$(c%)
            call "READ100" (#52, readkey$, f1%(52))
               if f1%(52) = 1% then L52680
L52620:     call "LOTVALID" (part$(c%,1%), store$(c%), lot$(c%), #54,    ~
                             #3, #52, errormsg$)
                if errormsg$ <> " " then return
            call "LOTUNQ2E" (part$(), lot$(), c%, #54, errormsg$)
                if errormsg$ <> " " then return

L52680:     temp = 0
            for test% = 1% to maxlines%
                if test% = c% then L52890
                if part$(c%,1%) <> part$(test%,1%) then L52890
                if store$(test%) = "***" then L52800
                if lot$(c%) <> lot$(test%) then L52890
                if store$(c%) <> store$(test%) then L52890
                    temp1 = 0:convert qtyissued$(test%) to temp1,        ~
                                                     data goto L52890
                    temp = temp + temp1
                    goto L52890

L52800:         for testd% = 1% to maxdist%
                    if distindex%(testd%) <> test% then L52870
                    if diststore$(testd%) <> store$(c%) then L52870
                    if distlot$(testd%) <> lot$(c%) then L52870
                       temp1 = 0:convert distqty$(testd%) to temp1,      ~
                                                     data goto L52870
                       temp = temp + temp1
L52870:         next testd%

L52890:     next test%
               temp2 = temp : temp = temp + qty
               call "HNYAVAIL" (#3, #52, part$(c%,1%), store$(c%),       ~
                      lot$(c%), errormsg$, temp, temp1, f1%(52))
               if errormsg$ = " " then L52970
                  if temp2 = 0 then L52940
                  errormsg$ = errormsg$ & " : ("
                  call "CONVERT" (temp2, -0.2,                           ~
                                str(errormsg$, len(errormsg$) + 1%, 10%))
                  errormsg$ = errormsg$ & " kitted)"
L52940:           if lot_enabl% = 0% then fieldnr% = min(fieldnr%, 4%)
                  return

L52970:     REM *** Check to See if we must distribute to Serial Nbrs ***

            call "SERKIT" (jobpart$,     /* Part code to Kit To        */~
                           part$(c%,1%), /* Component Part Code to Kit */~
                           store$(c%),   /* S/N Location to Select from*/~
                           lot$(c%),     /* (Store and Lot Codes).     */~
                           jobrmdr,      /* Qty to Build in Job        */~
                           qty,          /* Comp.  Qty to Issue / kit. */~
                           c%,           /* Pointer For Work File Use  */~
                           maxlines%,    /* Average # Lines per Documnt*/~
                           "JK",         /* Source Transaction Type.   */~
                                         /* (JK = Job Kitting).        */~
                           job$,         /* Source Transaction Key     */~
                           "1",          /* Status of Parent S/N's to  */~
                                         /* Kit To.                    */~
                           "2",          /* Status of Component S/N's  */~
                                         /* to Kit From.               */~
                           0%,           /* Operation to Perform       */~
                                         /* 0% = Normal Input/Edit,    */~
                                         /*      by INDEX%,            */~
                                         /* 2% = Start Over, if INDEX% */~
                                         /*      > 0% then only the    */~
                                         /*      line/data pointed to  */~
                                         /*      by INDEX% is cleared, */~
                                         /*      else ALL is cleared.  */~
                           errormsg$,    /* Returned Error Message     */~
                           #54,          /* SYSFILE2 UFB               */~
                           #3,           /* HNYMASTR UFB               */~
                           #62,          /* SERMASTR UFB               */~
                           #61,          /* SERTIF   UFB               */~
                           #63)          /* SERWORK  UFB               */
            if errormsg$ = " " then return
            if lot_enabl% = 0% then fieldnr% = min(fieldnr%, 4%)
            return

L53400: REM TEST DATA FOR QUANTITY OPEN/DATE REQUIRED
            temp1 = 0:convert qtyreq$(c%) to temp1, data goto L53420
L53420:     temp2 = 0:convert qtyissued$(c%) to temp2, data goto L53430
L53430:     temp1 = max(0, temp1 - temp2)
            temp1 = 999999999 /* REM TO DISABLE INCREMENTING */
            if pipout$(c%) = " " then temp1 = 0
            if temp1 <> 0 then L53490
               call "CONVERT" (0, 0.2, qtyrem$(c%,1%))
               return
L53490:     call "NUMTEST" (qtyrem$(c%,1%),0,temp1,errormsg$,-0.2,temp)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub     ,,         /* Skip 1 & 2        */     ~
                              L54200,         /* Quantity          */     ~
                              L54400          /* Store/Lot         */
            return

L54200: REM Test for Quantity                     BKGQTY$
            call "NUMTEST" (bkgpct$, 0, 100, errormsg$, -0.4, bkgpct)
            if errormsg$ <> " " then return
            if bkgpct > 0 then return
               errormsg$ = "Percent must be greater than zero."
               return

L54400: REM Test for Store & Lot                  BKGSTR$ & BKGLOT$
            call "GETCODE" (#59, bkgstr$, temp$, 0%, 0, f1%(59))
               if f1%(59%) <> 0% then L54460
            errormsg$ = "Select a valid store."
            return

L54460:     if bkglot$ = " " then return
            errormsg$ = "LOT-CHECK"
            call "LOTVALID" (" ", bkgstr$, bkglot$, #54, #3, #52,        ~
                             errormsg$)
            if str(errormsg$,,1%) = "L" then errormsg$ = " "
            return    /* Bypass Uniqueness catch 22 */

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution.                                     *~
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
            if postflag% = 99% then call "JBJNLCLS"                      ~
               ("J1", userid$, moduleno$, jnlid$, pstseq%, return%)
            call "JBINUSE" (" ", 1%)  /* Clears In-use Flag for User */
            end
