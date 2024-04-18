        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   L      N   N  TTTTT  RRRR   V   V  L      RRRR    *~
            *  P   P  L      NN  N    T    R   R  V   V  L      R   R   *~
            *  PPPP   L      N N N    T    RRRR   V   V  L      RRRR    *~
            *  P      L      N  NN    T    R   R   V V   L      R   R   *~
            *  P      LLLLL  N   N    T    R   R    V    LLLLL  R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLNTRVLR - Creates an array containing a jobs traveler    *~
            *            based on WCOUT data.                           *~
            *            Also returns the phantomized routing for the   *~
            *            assembly.                                      *~
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
            * 10/23/86 ! ORIGINAL                                 ! HES *~
            * 03/04/88 ! Fixed Bug with act code being tied to    ! HES *~
            *          ! the wrong (prev) step                    !     *~
            * 03/23/89 ! Fixed Bug; now accumulates Setup & Run   ! RJM *~
            *          !   Units correctly.                       !     *~
            * 01/17/91 ! Changed large array initialize to INIT   ! MJB *~
            * 11/03/93 ! Purchase Jobs Project - Added Support for! JBK *~
            *          !   'BW' type tags.                        !     *~
            * 07/19/96 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "PLNTRVLR" (tag$,            /* TAG NUMBER                 */~
                        #1,              /* RTEMASTR FILE              */~
                        #2,              /* WCOUT FILE                 */~
                        #3,              /* JBCROSS2 FILE              */~
                        #4,              /* BOMMASTR FILE              */~
                        #5,              /* ENGMASTR FILE              */~
                        #6,              /* SYSFILE2 FILE              */~
                        #7,              /* PIPIN or JBMASTR2          */~
                        rte$,            /* Route For Tag (Returned)   */~
                        bom$,            /* Bill For Tag (Returned)    */~
                        rte$(),          /* Phantomized Routing        */~
                        hits%,           /* Number Of Elements in Array*/~
                        travel$(),       /* Traveler Array             */~
                        ptr%)            /* Number Of Elements in Array*/

        dim                                                              ~
            acode$4,                     /* Activity Code Last Read    */~
            actcode$4,                   /* Activity Code For Save     */~
            basedate$6,                  /* Planning Calendar Base Date*/~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bomused$3,                   /* EFFECTIVE BOM              */~
            datein$6,                    /* Start Date In Work Center  */~
            dateout$6,                   /* End Date In Work Center    */~
            part$25,                     /* PART CODE                  */~
            record$100,                  /* Work Variable              */~
            rte$(255)200,                /* ROUTING ARRAY              */~
            rteused$3,                   /* EFFECTIVE RTE              */~
            step$7,                      /* STEP LABEL                 */~
            tag$19,                      /* JOB ORDER OR WORK ORDER    */~
            travel$(256)67,              /* FORMATTED WCOUT DATA       */~
            wc$(4)4,                     /* WORK CENTERS AND CONCURENTS*/~
            wcoutplow$50                 /* Work Variable              */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            if basedate$ <> " " and basedate$ <> blankdate$ then L09000
            basedate$ = "X"
            call "READ100" (#6, "MONTHS OPEN", f1%)
                if f1% = 0 then L65000
            get #6, using L02170, basedate$
L02170:     FMT XX(32), CH(6)

L09000: REM *************************************************************~
            *              I N I T I A L I Z A T I O N                  *~
            *                                                           *~
            * Do prepritory work for printing of traveler               *~
            *************************************************************

            REM Determine Bill And Route Used For Planning...
            init(" ") travel$(), rte$()
            rteused$, bomused$ = " "
            call "READ100" (#3, tag$, f1%)
                if f1% = 0 then L09160
            get #3, using L09120, rteused$, bomused$
L09120:     FMT XX(25), CH(3), XX(44), CH(3)
            rte$ = rteused$
            bom$ = bomused$

L09160:     REM Get effectivity date from JBMASTR2 file...
            if str(tag$,,11) <> "JOB ORDER: " then L09260
            call "READ100" (#7, str(tag$,12), f1%)
                if f1% = 0 then L65000  /* Say What?? */
            get #7, using L09210, part$, qtymake, closed$, plend$
L09210:     FMT POS(58), CH(25), PD(14,4), POS(153), CH(6),POS(174),CH(6)
            if closed$ <> " " or closed$ = blankdate$ then end
            call "PIPINDEX" (#6, plend$, effday%, 0%)
            goto L09330

L09260:     REM Get effectivity date from PIPIN file...
            if str(tag$,,2%)  = "BW" then L09280
            if str(tag$,,2%) <> "WO" then L65000
L09280:     call "READ100" (#7, tag$, f1%)
                if f1% = 0 then L65000
            get #7, using L09310, part$, effday%, qtymake
L09310:     FMT CH(25), BI(4), XX(19), PD(14,4)

L09330:     REM Use SUB to load route including phantom steps...
            call "PLANRTE" (part$, rteused$, bomused$, " ", rte$(),      ~
                                              effday%, hits%, #4, #1, #5)

        REM *************************************************************~
            *                  L O A D    D A T A                       *~
            *                                                           *~
            * The rather complicated process of gathering up data...    *~
            *************************************************************

            wc$(), datein$, dateout$ = " "
            laststep$ = hex(ff) /* Place Holder */
            ptr%, totbmq%, totamq%, totsu%, totrun% = 0
            wcoutplow$ = all(hex(00))
            str(wcoutplow$,,19) = tag$

            REM Build Array For Print...
L10130:     call "PLOWNEXT" (#2, wcoutplow$, 19%, f1%)
                if f1% <> 0 then L10180
                if wc$() = " " then calc_in_and_out
                ptr% = ptr% + 1 : gosub put_record
                goto calc_in_and_out
L10180:     get #2, using L10490, wc$, day%, seq%, setup%, run%, step$,   ~
                                 pstep%, alt%, con%, acode$
            REM Determine slot in array...
            if pstep% = 0 then L10240
                step$ = step$ & "-"
                convert pstep% to str(step$,pos(step$="-")+1), pic(00)
L10240:     if step$ = laststep$ then L10330
                if laststep$ = hex(ff) then L10280
                ptr% = ptr% + 1
                gosub put_record
L10280:         rteseq% = seq%/100
                if con% > 9 then rteseq% = 0%   /* Off Planned Routing */
                wc$(), datein$, dateout$, actcode$ = " "
                totbmq%, totamq%, totsu%, totrun%, lalt%   = 0

L10330:     REM Accumulate data for this step.
            con% = mod(con%+10,10)
            if con% > 0% and con% < 5% then L10400
                wc$(1%) = wc$
                if con% = 0% then totbmq% = totbmq% + 1%
                if con% = 9% then totamq% = totamq% + 1%
                goto L10430
L10400:     wc$(con%) = wc$
            lalt% = alt% : if actcode$ = " " then actcode$ = acode$
*          TOTSU%  = TOTSU%  + SETUP%
*          TOTRUN% = TOTRUN% + RUN%
*          IF CON% = 1% AND TOTBMQ% = 0% THEN TOTSU% = SETUP%
*          IF CON% = 1% AND TOTBMQ% = 0% THEN TOTRUN% = RUN%
            if con% = 1% then totsu% = totsu% + setup%
            if con% = 1% then totrun% = totrun% + run%
L10430:     if datein$ = " " or datein$ = blankdate$ then ~
                  call "DATE" addr("G+", basedate$, day%-1%, datein$, 0%)
            call "DATE" addr("G+", basedate$, day%-1%, dateout$, 0%)
            laststep$ = step$
            goto L10130

L10490:     FMT                    /* FILE: WCOUT                      */~
                CH(4),             /* Work Center code                 */~
                XX(2),             /* Date out of PIP in date subscript*/~
                XX(2),             /* Route Array Sequence Number      */~
                XX(19),            /* Tag number in level 2 planning   */~
                BI(2),             /* Date out of PIP in date subscript*/~
                BI(2),             /* Route Array Sequence Number      */~
                BI(4),             /* Set Up time in WC units          */~
                BI(4),             /* Run time in hours per part       */~
                CH(4),             /* RTE Step To Identify A Route Line*/~
                BI(1),             /* Phantom Route Step Level         */~
                BI(2),             /* alternate part seq. no.          */~
                BI(1),             /* Concurrent Workcenter Seq. relati*/~
                CH(04),            /* Activity to be performed         */~
                CH(17)             /* Filler (Internal, unused space)  */

        put_record
            REM Save info, this record now represents a routing step...
            record$ = " "
            ptr% = min(ptr%, 256%)
            put record$, using L10730, laststep$, rteseq%, wc$(1), wc$(2),~
                wc$(3), wc$(4), totbmq%, totsu%, totrun%, totamq%,       ~
                actcode$, datein$, dateout$, 0, 0, lalt%
            travel$(ptr%) = record$   /* This Allows Flexible DIM */
L10730:     FMT CH(7), BI(2), 4*CH(4), 4*BI(2), CH(4), 2*CH(6),          ~
                                                        2*PD(14,4), BI(2)
        return

        calc_in_and_out
            if ptr% = 0 then L10970
            for c% = 1 to ptr%
                yield_pct = 100
                get travel$(c%), using L10820, rteseq%
L10820:         FMT POS(8), BI(2)
                if rteseq% = 0 or rteseq% > hits% then L10870
                get rte$(rteseq%), using L10850, yield_pct
L10850:         FMT POS(80), PD(14,7)

L10870:         REM Factor yield into quantity, then save info...
                if yield_pct < .1 then yield_pct = 100
                parts_in = round(qtymake * 100/yield_pct, 2)
                put travel$(c%), using L10910, parts_in
L10910:         FMT POS(50), PD(14,4)    /* Quan into Step */
                if c% < 2% then L10950
                   put travel$(c%-1%), using L10940, parts_in
L10940:            FMT POS(58), PD(14,4)    /* Quan out of Step */
L10950:     next c%
            put travel$(ptr%), using L10940, qtymake
L10970:     goto L65000

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
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
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
