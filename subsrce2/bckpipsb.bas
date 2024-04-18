        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    CCC   K   K  PPPP   IIIII  PPPP    SSS   BBBB    *~
            *  B   B  C   C  K  K   P   P    I    P   P  S      B   B   *~
            *  BBBB   C      KKK    PPPP     I    PPPP    SSS   BBBB    *~
            *  B   B  C   C  K  K   P        I    P          S  B   B   *~
            *  BBBB    CCC   K   K  P      IIIII  P       SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKPIPSB - Handles Sales Order - Planning Interface       *~
            *            regarding Demands and PIPs. PIPs are aligned   *~
            *            per the current SO line data in BCKLINES.      *~
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
            * 09/26/86 ! Original                                 ! ERN *~
            * 08/21/89 ! Cancelld SOs now remove any planning data! JDH *~
            * 10/01/89 ! Don't Unplan if PCD < DUE                ! KAB *~
            * 04/29/91 ! PRR 11782  Fixed the program to properly ! SID *~
            *          !     update Forecast File when allocation !     *~
            *          !     method is 'C'.                       !     *~
            * 01/03/92 ! Now doesn't write the planned completion ! JDH *~
            *          !   date to DEMMASTR if demand has not been!     *~
            *          !   planned.                               !     *~
            * 03/08/94 ! Added support for BOMSPHDR               ! WPH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "BCKPIPSB" (so$,             /* Sales Order Number         */~
                        seq$,            /* Line Item Sequence         */~
                        store$,          /* Store                      */~
                        crhold$,         /* Credit Status (H= On-hold) */~
                        #1,              /* BCKLINES                   */~
                        #3,              /* DEMMASTR                   */~
                        #4,              /* SYSFILE2                   */~
                        #5,              /* PIPMASTR                   */~
                        #6,              /* PIPIN                      */~
                        #7,              /* PIPOUT                     */~
                        #8,              /* BOMSPEC                    */~
                        #9,              /* WCMASTR                    */~
                        #10,             /* WCOUT                      */~
                        #11,             /* SFMASTR2                   */~
                        #12,             /* SFCUM2                     */~
                        #13,             /* JBCROSS2                   */~
                        #14,             /* PIPCROSS                   */~
                        #15 )            /* JBPIPXRF                   */~


        dim                                                              ~
            alloc$1,                     /* Allocation flag from SO    */~
            bomid$3,                     /* Specific Bom Id.           */~
            crhold$1,                    /* CR Hold Status (H/blank)   */~
            cumf%(490),                  /* Cummlative Forecast Area   */~
            cuscode$9,                   /* Customer Number from SO    */~
            demand$123,                  /* Entire DEMMASTR Record     */~
            dempart$25,                  /* Part Number on Demand      */~
            dempriority$1,               /* Demand Priority            */~
            demqty$10,                   /* Demand Quantity            */~
            demship$6,                   /* Ship date on Demand        */~
            demstatus$1,                 /* Demand Status              */~
            demtype$1,                   /* Demand Type                */~
            hex00$10,                    /* All Hex 00's               */~
            orderqty$10,                 /* Order Qty- Alphanumeric    */~
            part$25,                     /* Part Number from SO        */~
            pcd$6,                       /* Planned Completion Date    */~
            pippart$25,                  /* Part on PIP record         */~
            readkey$100,                 /* Read Key Variable          */~
            reqdship$6,                  /* Required Ship Date (SO)    */~
            rslt$20,                     /* Result for OPENCHCK        */~
            seq$3,                       /* Line Item Sequence         */~
            so$16,                       /* Sales Order Number         */~
            sodemtype$1,                 /* Demand Type on SO          */~
            sopriority$1,                /* Priority Code from SO      */~
            store$3,                     /* Store Code                 */~
            store1$1,                    /* 1st Character of Store     */~
            unplanopt$1,                 /* Unplan Option Switch       */~
            unplanrpt$1                  /* Unplan Report Switch       */

        dim f1%(32)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00   "
        REM *************************************************************
            f2% = 1%

            select #18, "BOMSPHDR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  53,                     ~
                        alt key  1, keypos =   35, keylen =  19

            call "OPENCHCK" (#18, f1%(18), f2%, 0%, rslt$)

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            store1$ = str(store$,,1)

            if alreadybeenherebefore% = 1% then L10000
               alreadybeenherebefore% = 1%

                hex00$ = all(hex(00))

                call "PIPINDEX" (#4, " ", today%, ret%)

                call "READ100" (#4, "PLANNING SYSTEM FLAG", f1%(4))
                unplanopt$ = "Y"  :  unplanrpt$ = "N"
                if f1%(4) = 0% then L10000
                     get #4 using L09190, unplanopt$, unplanrpt$
L09190:                   FMT XX(23), CH(1), XX(13), CH(1)


L10000: REM *************************************************************~
            *                M A I N   L O G I C                        *~
            *-----------------------------------------------------------*~
            * See remarks in code.                                      *~
            *************************************************************

*        Load the SO Line, the Demand (if any) and test the Part
            gosub load_data

*        First see if line exists in BCKLINES.  If it doesn't, a delete
*        is assumed and EVERYTHING needs to be undone.

            if soonfile% = 1% then L10200
                gosub unplan
                gosub remove_demand
                gosub remove_pips
                gosub remove_bomspec
                goto  exit_program


L10200
*        Next see if the Store is a non-planned store.  If so, remove
*        any planning information that may exist
            if store1$ >= "0" and store1$ <= "9" then L10270
                gosub unplan
                gosub remove_demand
                gosub remove_pips
                goto  exit_program


L10270
*        If Order has been cancelled, then remove any planning data.
            if crhold$ <> "C" then L10350
                gosub unplan
                gosub remove_demand
                gosub remove_pips
                goto  exit_program


L10350
*        If Part is non-stocked or is an unplanned part then remove
*        any planning data.
            if partok% = 1% then L10440
                gosub unplan
                gosub remove_demand
                gosub remove_pips
                goto  exit_program


L10440
*        If Line is allocated complete we need to undo any planning
*        data and just write a PIPOUT for the Open Order quantity.
            if alloc$ <> "C" then L10540
                alloc = openqty   /* Just in case     */
                gosub unplan
                gosub remove_demand
                gosub write_pips
                gosub adjust_forecast
                goto  exit_program


L10540
*        Now we know the following:
*         (1) We have a valid Sales Order line,
*         (2) The Part is a Stocked Item and a Planned Part;
*         (3) The Store is a planned Store.
*         (4) The part is at worst only partially allocated.

*        If the Demand is unplanned then we just need to undo
*        everything and rewrite the demand and the PIPs
            if demstatus$ >= "2" then L10680        /* Planned          */
                gosub write_pips
                gosub write_demand
                goto  exit_program


L10680
*        Demand is planned.  If the Required Ship Date has moved outside
*        of the "Planning Window" (Old Ship Date to Planned Completion
*        Date) -or- the Part and/or Order Quantity has changed, then
*        we need to unplan and re-establish the PIPs.  If all the
*        above is OK-fine we just need to adjust the PIPOUT (maybe).

            if pcd% >= demship% then L10740
            if reqdship% < pcd%     or                                   ~
               reqdship% > demship% then undo_plan
               goto L10760
L10740:     if reqdship%  < demship% or                                  ~
               reqdship%  > pcd%    then undo_plan
L10760:     if dempart$  <> part$   then undo_plan
            if orderqty  <> demqty  then undo_plan

*        Just realign PIPOUT(s)
            gosub align_pipouts
            gosub adjust_forecast
            gosub write_demand
            goto  exit_program


        undo_plan    /* Undo plan and rewrite planning data            */
            gosub unplan
            gosub write_pips
            gosub write_demand
            goto  exit_program


        REM *************************************************************~
            *              S U B R O U T I N E S                        *~
            *-----------------------------------------------------------*~
            * Routines called by the driver to do the dirty work.       *~
            *************************************************************

        unplan       /* Call 'UNPLAN' to unplan demand.                */
            if demstatus$ < "2" then return
                call "UNPLAN"                                            ~
                    (so$,                /* Sales Order Number         */~
                     seq$,               /* Demand Line                */~
                     demtype$,           /* Demand Type                */~
                     pcd%,               /* Date Planned for SO/SF Only*/~
                     demship%,           /* Date Due                   */~
                     dempart$,           /* Part to Unplan             */~
                     demqty,             /* Quantity to Unplan         */~
                     today%,             /* Index for Today            */~
                     unplanopt$,         /* Unplanning Option          */~
                     unplanrpt$,         /* Unplanning Report          */~
                     #13,                /* JBCROSS2                   */~
                     #5,                 /* PIPMASTR                   */~
                     #9,                 /* WCMASTR                    */~
                     #10,                /* WCOUT                      */~
                     #6,                 /* PIPIN                      */~
                     #7,                 /* PIPOUT                     */~
                     #11,                /* SFMASTR2                   */~
                     #12,                /* SFCUM2                     */~
                     #14,                /* PIPCROSS                   */~
                     #15 )               /* JBPIPXRF                   */
                demstatus$ = " "
                str(demand$,77,12) = " " /* Last Planned and Planned   */
                pcd% = 0%                /* Completion Date            */
                return


        remove_demand     /* Delete Demand from the Demand Master      */
            if demand% = 0% then return            /* Doesn't exist    */
                readkey$ = str(so$,,16) & seq$
                call "REDALT1" (#3, readkey$, 1%, f1%(3))
                if f1%(3) = 1% then delete #3
                return


        remove_pips       /* Get PIPIN and PIPOUTs out of PIP files    */
*        First the PIPIN
            readkey$ = str(so$,,16) & seq$
            call "READ101" (#6, readkey$, f1%(6))
            if f1%(6) = 0% then L12530
                get #6, using L12490, pippart$, pip%, pipqty
L12490:              FMT CH(25), BI(4), XX(19), PD(14,4)
                delete #6
                call "PIPFLAGS" (pippart$, 1%, pip%, -pipqty, #5, #12)

L12530
*        And next the PIPOUT
        remove_pipouts
            readkey$ = str(so$,,16) & str(seq$,,3) & hex(00)
L12560:     call "PLOWNXT1" (#7, readkey$, 19%, f1%(7))
            if f1%(7) = 0% then return
                get #7, using L12590, pippart$, pip%, pipqty
L12590:              FMT XX(19), CH(25), BI(4), XX(8), PD(14,4)
                delete #7
                call "PIPFLAGS" (pippart$, 1%, pip%, pipqty, #5, #12)
                if alloc$ = "C" then orderqty = orderqty - pipqty
                goto L12560


        remove_bomspec    /* Delete Options for a delete SO line       */
            readkey$ = str(so$,,16) & str(seq$,,3) & hex(00000000)
            /* first check header file - don't delete if found */
            call "REDALT1"(#18, readkey$, 1%, f1%(18))  /* header */
                if f1%(18) <> 0% then return

L12670:     call "PLOWAL1" (#8, readkey$, 1%, 19%, f1%(8))
            if f1%(8) = 0% then return
                delete #8
                goto L12670


        write_pips        /* Write out PIPOUT and PIPIN Records        */
            gosub remove_pips
            if openqty = 0 then return

*        Do PIPOUT First
          pipoutqty = max(0,  openqty - preinv)
          if pipoutqty = 0 then return
            write #7 using L12820, so$, seq$, part$, reqdship%,           ~
                                                         time, pipoutqty
L12820:         FMT CH(16), CH(3), CH(25), BI(4), CH(8), PD(14,4)
            call "PIPFLAGS" (part$, 1%, reqdship%, -pipoutqty, #5, #12)
            if alloc$ = "C" then return  /* Means no PIPIN   */

*        And now the PIPIN
            pipinqty = max(0, openqty - alloc - max(0, preinv - alloc))
            if pipinqty = 0 then return
                write #6 using L12910, part$, reqdship%, so$, seq$,       ~
                                                     pipinqty, reqdship%
L12910:              FMT CH(25), BI(4), CH(16), CH(3), PD(14,4), BI(4)
                call "PIPFLAGS" (part$, 1%, reqdship%, pipinqty, #5, #12)
            return


        write_demand      /* Update the Demand and write to file       */
*        First reset the Demand Status
            if demstatus$ = " " then demstatus$ = "1"
            if demstatus$ = "6" then demstatus$ = "7"
            if demstatus$ = "8" then demstatus$ = "9"
            if crhold$ <> "H" then L13060
                if demstatus$ = "1" then demstatus$ = " "
                if demstatus$ = "7" then demstatus$ = "6"
                if demstatus$ = "9" then demstatus$ = "8"

L13060:     gosub remove_demand

            call "CONVERT" (orderqty, 2.2, orderqty$)
            if demstatus$ < "2" then str(demand$,77,12) = " "
            put str(demand$) using L13120,                                ~
                demstatus$, sodemtype$, sopriority$, reqdship$,          ~
                so$, seq$, part$, orderqty$, bomid$, store$, cuscode$
L13120:              FMT 3*CH(1), CH(6), CH(16), CH(3), CH(25), CH(10),  ~
                         POS(68), CH(3), POS(74), CH(3), POS(89), CH(9)
            put #3 using L13150, str(demand$)
L13150:         FMT CH(123)
            write #3
            return


        adjust_forecast   /* Adjust forecast if Type has changed       */
            mat cumf% = zer
            call "READ101" (#12, part$, f1%(12))
            if f1%(12) = 0% then goto L13270
                get #12 using L13250, part$, cumf%()
L13250:              FMT CH(25), 490*BI(4)

L13270:     if alloc$ = "C" then L13320
            if demtype$ = "2" then L13320           /* Old Type         */
                for i% = pcd% to 490%
                     cumf%(i%) = cumf%(i%) + demqty
                next i%

L13320:     if sodemtype$ = "2" then L13370         /* New Type         */
                for i% = pcd% to 490%
                     cumf%(i%) = cumf%(i%) - orderqty
                next i%

L13370:     put #12 using L13250, part$, cumf%()
            if f1%(12) = 0% then write #12  else  rewrite #12
            return



        align_pipouts
*        Sum the PIPOUTs and see if their quantity is correct.  If not,
*        then they must be adjusted to reflect the proper quantity.
*        Also, if the Required Ship Date on the Order is after the first
*        PIPOUT, we need to adjust the date on that PIPOUT.  If it runs
*        into the second PIPOUT, then we combine the two.
            pipoutqty, qty1, qty2 = 0
            dateout%  = reqdship%
            readkey$  = str(so$,,16) & str(seq$,,3) & hex(00)
            call "PLOWNEXT" (#7, readkey$, 19%, f1%(7))  /* Get 1st    */
            if f1%(7) = 0% then L13640              /* Ooops! Rewrite   */
                get #7 using L13550, dateout%, qty1
L13550:              FMT POS(45), BI(4), POS(57), PD(14,4)
                pipoutqty = pipoutqty + qty1

                call "PLOWNEXT" (#7, readkey$, 19%, f1%(7)) /* Get 2nd */
                if f1%(7) = 0% then L13640
                     get #7 using L13610, qty2
L13610:                   FMT POS(57), PD(14,4)
                     pipoutqty = pipoutqty + qty2

L13640
*        Now Test to see if anything needs to be done
            qty  = max(0, openqty - preinv) /* What PIPOUTs should be  */
            chngqty = pipoutqty - qty       /* Old - New               */
            on sgn(chngqty) + 2%  goto L13690, L13780, L13740

L13690
*        Case 1: Quantity has increased.  Put entire QTY on PCD.
            qty1 = 0
            qty2 = qty
            goto L13780

L13740
*        Case 2: Quantity has Decreased.  Remove from 1st then 2nd
            qty1 = max(0, qty1 - chngqty)
            qty2 = max(0, qty  - qty1   )

L13780
*        Has the Required Ship Date Moved?
            if dateout% >= reqdship% and chngqty = 0 then return
                dateout% = reqdship%
                if dateout% < pcd% then L13850
                     qty1 = 0
                     qty2 = qty

L13850
*        Change Required.  Kill old PIPOUTS and write New Ones.
            gosub remove_pipouts            /* Remove current PIPOUTS  */

            if qty1 = 0 then L13930          /* Implies only one PIPOUT */
                write #7 using L13910, so$, seq$, part$, dateout%, time,  ~
                                                                     qty1
L13910:              FMT CH(16), CH(3), CH(25), BI(4), CH(8), PD(14,4)
                call "PIPFLAGS" (part$, 1%, dateout%, -qty1, #5, #12)
L13930:     if qty2 = 0 then return
                write #7 using L13910, so$, seq$, part$, pcd%, time, qty2
                call "PIPFLAGS" (part$, 1%, pcd%    , -qty2, #5, #12)
                return



        REM *************************************************************~
            *                D A T A   L O A D S                        *~
            *-----------------------------------------------------------*~
            * Subroutines to bring data in from files.                  *~
            *************************************************************

        load_data

*        First get the Sales Order Line
            init (" ") cuscode$, part$, reqdship$, sodemtype$,           ~
                       sopriority$, alloc$
            orderqty, openqty, alloc, preinv = 0
            readkey$ = str(so$,,16) & seq$
            call "READ100" (#1, readkey$, soonfile%)
            if soonfile% = 0% then load_demand
                get #1 using L30180, cuscode$, part$, orderqty, openqty,  ~
                                    alloc, preinv, reqdship$, sodemtype$,~
                                    sopriority$, alloc$, bomid$
L30180:             FMT CH(9), POS(32), CH(25), POS(93), PD(14,4),       ~
                        POS(109), PD(14,4), POS(125), 2*PD(14,4),        ~
                        POS(212), CH(6), POS(240), 2*CH(1), POS(246),    ~
                        CH(1), POS(263), CH(3)
                call "PIPINDEX" (#4, reqdship$, reqdship%, ret%)
                if pos("12" = sodemtype$) = 0% then sodemtype$ = "1"
                if sopriority$ = " " then sopriority$ = "A"


        load_demand  /* Bring in the current demand for this line item.*/
                     /* If no demand exists, dummy one up.             */
            readkey$ = str(so$,,16) & seq$
            call "REDALT0" (#3, readkey$, 1%, demand%)
            if demand% = 1% then get #3 using L30320, str(demand$)
L30320:         FMT CH(123)
            if demand% = 1% then L30410
                demstatus$ = "1" : if crhold$ = "H" then demstatus$ = " "
                put str(demand$) using L30390,                            ~
                          demstatus$, sodemtype$, sopriority$, " ",      ~
                          so$, seq$, " ", "0", " ", " ", " ", store$,    ~
                          " ", reqdship$, cuscode$, " ", hex00$
L30390:              FMT 3*CH(1), CH(6), CH(16), CH(3), CH(25), CH(10),  ~
                         CH(4), 3*CH(3), 2*CH(6), CH(9), CH(16), CH(10)
L30410:     get str(demand$) using L30440,                                ~
                     demstatus$, demtype$, dempriority$, demship$,       ~
                     dempart$, demqty$, pcd$
L30440:         FMT 3*CH(1), CH(6), POS(29), CH(25), CH(10), POS(83),    ~
                    CH(6)
            demqty = 0  :  convert demqty$ to demqty, data goto L30480
L30480:     demship%, pcd% = 0%
            if demship$ <> " " then                                      ~
                        call "PIPINDEX" (#4, demship$, demship%, ret%)
            if pcd$ <> " " then call "PIPINDEX" (#4, pcd$, pcd%, ret%)


*        See that the Part Number is a Stocked and Planned Part
            call "READ100" (#5, part$, partok%)
            return


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

        exit_program      /* Pip Pip Hooray!!!! */

            end
