        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  JJJJJ  BBBB   PPPP   IIIII   CCC   K   K   SSS   L       *~
            *    J    B   B  P   P    I    C   C  K  K   S      L       *~
            *    J    BBBB   PPPP     I    C      KKK     SSS   L       *~
            *  J J    B   B  P        I    C   C  K  K       S  L       *~
            *   J     BBBB   P      IIIII   CCC   K   K   SSS   LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBPICKSL - This is a subroutine to print a pick slip for a*~
            *            range of Job Numbers.  Its a subroutine so that*~
            *            it can be called from the release or as a      *~
            *            stand alone for reprints without a lot of      *~
            *            hassles.                                       *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/07/83 ! ORIGINAL                                 ! KEN *~
            * 11/11/85 ! SHOW STORE, LOT, & BIN LOCATIONS         ! HES *~
            * 02/12/86 ! Expanded BIN Location from 4 to 8 bytes. ! LDJ *~
            * 02/24/87 ! Add component, parent part serial numbers! JRH *~
            * 03/16/87 ! Removed obsolete subroutines.            ! ERN *~
            * 04/28/87 ! Corrected to print Stocking UOM          ! MJB *~
            * 06/10/87 ! Minor HNYMASTR format mod (PARTTYPE$).   ! JIM *~
            * 02/08/88 ! Corrected Report if parent ser & comp not! HES *~
            * 02/12/88 ! Added Sort Options!! Plus some TLC...    ! HES *~
            * 06/16/88 ! Added Summary Opt, for Pick List by Part ! KAB *~
            * 07/05/88 ! Re-Added Bar Code Printing Capabilities  ! KAB *~
            * 02/14/89 ! Happy Valentine's Day...  Added Job Desc ! MJB *~
            *          !   & Control Number & reformatted header  !     *~
            * 03/06/89 ! Added Multiple Bin Sort & Print Options. ! JEF *~
            * 07/26/91 !(QC FIXES) Corrected the setting of the   ! RJB *~
            *          !   Default Store Code and the Print No Bin!     *~
            *          !   Locations Option.                      !     *~
            * 08/31/93 ! Purchased Jobs - Vendor name & addr now  ! MLJ *~
            *          !  part of heading - 'PJ' jobs only.       !     *~
            *          !  Added VENDOR, channle 14.               !     *~
            *          ! PRR 10967 - Job text can now be included !     *~
            *          !  on pick Lists (JB0016).  Added TXTFILE, !     *~
            *          !  channel 15.                             !     *~
            *          ! PRRs 12241, 12704 - BOM Header Text can  !     *~
            *          !  now be included when printin pick lists.!     *~
            *          !  added BOMMASTR, channel 16.             !     *~
            *          ! PRR 12935 - Now prints '(*)' beside comp !     *~
            *          !  part if alternates exist. Added HNYALTRS!     *~
            *          !  channel 18.                             !     *~
            *          ! MISC - Renumbered and modkilled, lacked  !     *~
            *          !  room to work. Corrected implied integers!     *~
            * 08/29/94 ! No longer supports Xerox 4045 printer bar! JDH *~
            *          !  codes.  Now it's HP 256X & 2300 series. !     *~
            * 10/07/94 ! Added SO Line Text.  Added Rev To hdr.   ! HES *~
            *          ! Implement new barcoding scheme via sub.  !     *~
            *          ! Bug fixes and MAJOR internal overhaul.   !     *~
            * 03/08/95 ! Blank line no longer follows unprinted   ! JDH *~
            *          !  text.                                   !     *~
            * 02/25/98 ! Y2K mods.                                ! RJH *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "JBPICKSL" (firstjob$,       /* FIRST JOB TO PRINT         */~
                         lastjob$,       /* LAST JOB TO PRINT          */~
                               #1,       /* JBMASTR2 CHANNEL #         */~
                               #2,       /* PIPOUT CHANNEL #           */~
                               #3,       /* WCOUT CHANNEL #            */~
                               #4,       /* HNYMASTR CHANNEL #         */~
                               #5,       /* HNYQUAN CHANNEL #          */~
                               #6,       /* SYSFILE2 CHANNEL #         */~
                          exline%,       /* Extra lines to print on    */~
                                         /*   Pick List                */~
                        calstart$,       /*  FIRST DAY IN PROD CAL     */~
                          cutoff%)       /*  END DATE FOR PICK (OFFSET)*/

        dim alt$3,                       /* Alternates Exist Indicator */~
            bin$8,                       /* INVENTORY BIN LOCATION     */~
            bin_loc$(100)20,             /* Array of Bins              */~
            bar_flag$1,                  /* Bar Code Flag in SFC       */~
            binlit$8,                    /* Bin Title Literal          */~
            bindsh$8,                    /* Bin Title Dashes           */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bomid$3,                     /* BOM Job Was Plenned To Use */~
            btxtid$4,                    /* BOM Header Text ID         */~
            calstart$6,                  /* FIRST DAY IN PLANNING CAL  */~
            ctlnbr$19,                   /* Shop Floor Control Number  */~
            currentjob$8,                /* JOB BEING PROCESSED        */~
            date$8,                      /* TODAYS DATE                */~
            def_store$3,                 /* Default Store Code         */~
            firstjob$8,                  /* FIRST JOB TO PRINT FOR     */~
            hddate$45,                   /* Header Date                */~
            header$80,                   /* Header For Report          */~
            inc_loc$1,                   /* Include HNYLOCNS for Bins  */~
            jbpart$25,                   /* PART JOB IS BUILDING       */~
            jbpartdescr$60,              /* PART DESCRIPTION           */~
            jbquant$10,                  /* QUANTITY TO BUILD          */~
            jbstore$3,                   /* Store From Job             */~
            jbworkcenter$4,              /* FIRST WC FOR JOB           */~
            jobdescr$30,                 /* Job Description            */~
            jtxtid$4,                    /* Job Text ID                */~
            lastjob$8,                   /* LAST JOB TO PRINT FOR      */~
            lot$6,                       /* LOT NUMBER                 */~
            part$25,                     /* COMPONENT PART             */~
            partdescr$32,                /* PART DESCRIPTION           */~
            parttype$3,                  /* HNYMASTR Part type         */~
            pickby$8,                    /* PICKING CUTOFF DATE        */~
            pipoutplow$80,               /* Work Variable              */~
            pipoutrec$64,                /* Work Variable              */~
            prtbin$1,                    /* PRINT LOCATIONS FLAG       */~
            psum$1,                      /* PART SUMMARY OPTION        */~
            quantity$9,                  /* QUANTITY OF COMP NEEDED    */~
            readkey$50,                  /* WORK VARIABLE              */~
            rev$2,                       /* BOM Revision Level         */~
            rslt$20,                     /* Work Variable              */~
            rteid$3,                     /* Route Job Was Planned To Us*/~
            ser$(2,2)20,                 /* Serial Lits & dashes       */~
            sercomp$1,                   /* COMPONENT SERIAL # FLAG    */~
            stdate$8,                    /*                            */~
            stxtid$4,                    /* SO line Text ID            */~
            stlot$10,                    /* Print Literal              */~
            str$3,                       /* STORE NUMBER               */~
            tagnr$19,                    /*                            */~
            temp$6,                      /* WORK VARIABLE              */~
            textid$4,                    /* Text ID                    */~
            tim$8,                       /* Time For Title             */~
            unit$4,                      /* UNIT of STOCKING           */~
            userid$3,                    /* Da Operator                */~
            vendor$9,                    /* Vendor Number              */~
            ven_name$30,                 /* Vendor Name                */~
            ven_addr$(5)30,              /* Vendor Address             */~
            wcoutplow$35                 /* WORK VARIABLE              */

        dim f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            f2%(64)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto L02000
            cms2v$ = "R6.04.01 06/23/95 Patch Finalization of R6.04.01  "

L02000: REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #11 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #12 ! JBCROSS2 ! Job Cross-Reference file                 *~
            * #13 ! HNYLOCNS ! Stock Location Master File               *~
            * #14 ! VENDOR   ! Vendor Master File                       *~
            * #15 ! TXTFILE  ! System Text File                         *~
            * #16 ! BOMMASTR ! Bill Of Materials Relationship File      *~
            * #17 ! WORKFILE ! For Sorting Pick List                    *~
            * #18 ! HNYALTRS ! Alternate Parts File                     *~
            * #19 ! BCKLINES ! Sales Order Line Item Detail File        *~
            *************************************************************

            select #11, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #12, "JBCROSS2",                                      ~
                        varc,     indexed,  recsize =  94,               ~
                        keypos =29, keylen = 19,                         ~
                        alternate key 1, keypos = 1 , keylen = 47,       ~
                                  key 2, keypos = 48, keylen = 47

            select #13, "HNYLOCNS",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =  443, keylen =  42,         ~
                            key  2, keypos =  485, keylen =  42,         ~
                            key  3, keypos =  527, keylen =  42,         ~
                            key  4, keypos =  590, keylen =  42

            select #14, "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos = 10, keylen = 30, dup


            select #15, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11

            select #16, "BOMMASTR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =  1, keylen = 56

            select #17, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =  80,               ~
                        keypos = 1, keylen = 72

            select #18, "HNYALTRS",                                      ~
                        varc,     indexed,  recsize =  60,               ~
                        keypos =    1, keylen =  33

            select #19, "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            if firstjob$ > lastjob$ then end

        REM *************************************************************~
            *              I N I T I A L I Z A T I O N                  *~
            *                                                           *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            btxtid$, jtxtid$ = " "
            if prtbin$ <> " " then L10000

            REM Following code only executed first time in...
            date$ = date
            call "DATEFMT" (date$)
            call "DATE" addr("HD", hddate$)
            call "SPCESMSH" (hddate$, 2%)
            call "STRING" addr("CT", hddate$, 45%)
            call "EXTRACT" addr("ID", userid$)

            select printer (134)

            prtbin$ = "N"
            sort$ = "P" : psum$ = "F" : file% = 2%
            binlit$, bindsh$ = " "
            bar_flag$ = "N"
            pge% = 56%

            call "OPENCHCK" (#11, 0%, 0%, 0%, rslt$)
            call "OPENCHCK" (#12, 0%, 0%, 0%, rslt$)
            call "OPENCHCK" (#13, 0%, 0%, 0%, rslt$)
            call "OPENCHCK" (#14, 0%, 0%, 0%, rslt$)
            call "OPENCHCK" (#15, 0%, 0%, 0%, rslt$)
            call "OPENCHCK" (#16, 0%, 0%, 0%, rslt$)
            call "OPENCHCK" (#18, 0%, 0%, 0%, rslt$)
            call "OPENCHCK" (#19, 0%, 0%, 0%, rslt$)

            call "READ100" (#6, "SWITCHS.SFC", f1%(6))
            if f1%(6) = 0 then L09520
               get #6, using L09370, prtbin$, sort$, psum$, bar_flag$,     ~
                                   def_store$, inc_loc$, qty_loc%
L09370:        FMT POS(26), CH(1), XX(4), CH(1), XX(1), 2*CH(1), POS(37),~
                   CH(3), CH(1), BI(1)

               if sort$ < "A" then sort$ = "P"
               if psum$ < "A" then psum$ = "F"
               if sort$ = "P" then L09480
                  call "WORKOPEN" (#17, "IO   ", 200%, 1%)
                  file% = 17%
                  if sort$ = "L" and prtbin$ = "N" then prtbin$ = "Y"
L09480:        if prtbin$ = "N" then L09520
                  binlit$ = "LOC ID."
                  bindsh$ = "--------"

L09520:        REM Setup subroutine for processing...
               call "BARCODES" ("I", bar_flag$, " ", " ", " ", " ", " ", ~
                                " ", 134%, barsize%)

L10000: REM *************************************************************~
            *               P R I N T   S E C T I O N                   *~
            *-----------------------------------------------------------*~
            * Set up Job related things here                            *~
            *************************************************************

            REM Plow/load JBMASTR2 data...
            currentjob$=firstjob$
            currentjob$=currentjob$ addc all(hex(ff))

        next_job
            call "PLOWNEXT" (#1, currentjob$, 0%, f1%(1))
                if f1%(1) = 0 then L65000
            get #1, using L10160, currentjob$, jobdescr$, tagnr$, jbpart$,~
                    qt1, vendor$,temp$, stdate$, jtxtid$, ctlnbr$
            init(" ") jbstore$      /* Field may be added to JBMASTR2 */
L10160:     FMT CH(8), CH(30), CH(19), CH(25), PD(14,4), POS(108), CH(9),~
                POS(153), CH(6), POS(168), CH(6), POS(228), CH(4),       ~
                POS(1120), CH(19)
            if temp$ <> " " and temp$ <> str(blankdate$,,6%) ~
                                          then next_job   /* Closed Job */
            if currentjob$ > lastjob$ then L65000

            stxtid$ = " "
            if ctlnbr$ = " " then L10300
               REM Try to Get Sales Order Text
               call "READ100" (#19, ctlnbr$, f1%(19%))
               if f1%(19) = 0% then L10300
                  get #19, using L10280, stxtid$
L10280:           FMT POS(242), CH(4)

L10300:     f1%(14%) = 0%
            if vendor$ = " " then L10390
               init(hex(00)) readkey$
               str(readkey$,1%,9%) = str(vendor$)
               call "READ100" (#14, readkey$, f1%(14%))
               if f1%(14%) = 0% then L10390
                  get #14 using L10370, ven_name$, ven_addr$()
L10370:           FMT POS(40), CH(30), 5*CH(30)

L10390:     call "CONVERT" (qt1, -0.2, jbquant$)
            call "GETCODE" (#4, jbpart$, pipoutplow$, 1%, 99, f1%(4))
                if f1%(4) = 0 then pipoutplow$ = "**(Not On File)**"
            jbpartdescr$ = jbpart$
            jbpartdescr$ = jbpartdescr$ & " " & pipoutplow$
            if jobdescr$ = pipoutplow$ then jobdescr$ = " "
            if def_store$ = " " then L10470
                if jbstore$ = " " then jbstore$ = def_store$
L10470:     call "DATEFMT" (stdate$)
            header$ = "JOB: " & currentjob$
            if jobdescr$ = " " then L10510
                header$ = header$ & " (" & jobdescr$ & ")"
L10510:     if ctlnbr$ = " " then L10530
                header$ = header$ & " Ctl # " & ctlnbr$
L10530:     call "STRING" addr("CT", header$, 80%)
            page%, maxlines% = 0%
            tim$ = " " : call "TIME" (tim$)
            date$ = date : call "DATEFMT" (date$)
            line% = 99%

        REM Retrieve Bom and Route Id...
            rteid$, bomid$ = " "
            call "READ100" (#12, tagnr$, f1%(12))
                if f1%(12) = 0 then L10740
            get #12, using L10640, rteid$, bomid$
L10640:         FMT POS(26), CH(3), XX(44), CH(3)

        REM Retrieve BOM Header Text ID...
            init(hex(00)) readkey$
            readkey$ = str(jbpart$) & str(bomid$) & "  0"
            call "READ100" (#16, readkey$, f1%(16%))
                if f1%(16%) = 0% then L10740
            get #16 using L10720, btxtid$, rev$
L10720:         FMT POS(90), CH(4), XX(1), CH(2)

L10740:     REM Retrieve Initial Work Center For Kit...
            jbworkcenter$=" "
            init (hex(00)) wcoutplow$
            init ("_") str$, lot$
            str(wcoutplow$,,19)=tagnr$
            call "PLOWNEXT" (#3, wcoutplow$, 19%, f1%(3))
                if f1%(3) = 0 then L10840
            get #3, using L10820, jbworkcenter$
L10820:         FMT CH(4)

L10840:     length%, enabled% = 0%
            pmode% = 1%   /* No Serial Numbers */
            call "SERENABL" (jbpart$, enabled%, length%, #6, #4)
            init (" ") ser$()
            if enabled% = 0% then L11060
                ser$(1,1) = "ASSY SERIAL NUMBER"
                init ("_") ser$(2,1)
                maxlines% = qt1
                pmode% = 2%   /* Parent Serial Numbers Only */

        REM *************************************************************~
            *                S O R T    P I P O U T S                   *~
            *-----------------------------------------------------------*~
            * create sorted work file if so requested                   *~
            *************************************************************

L11060:     REM Clear Work File, sort PIPOUTS if required...
            if file% = 2% then L12000  /* No Sort */
                call "DELETE" (#17, " ", 0%)
                init (hex(00)) pipoutplow$
                str(pipoutplow$,1,19) = tagnr$
                sort_loop: call "PLOWNEXT" (#2, pipoutplow$, 19%, f1%(2))
                     if f1%(2) = 0 then L12000
                v% = val(str(pipoutplow$,45,4),4)
                if v% > cutoff% then sort_loop  /* Beyond cutoff */
                get #2, using L11160, pipoutrec$
L11160:         FMT CH(64)
                part$ = str(pipoutrec$,20,25)

                REM Generate Work File To Get Desired Sort Order...
                on pos("TLCSB"=sort$) goto L11310, /* Part Type Order   */~
                                           L11410, /* Bin Location Order*/~
                                           L11520, /* Part Category Orde*/~
                                           L11610, /* Part Class Order  */~
                                           L11700  /* BOM Order         */

                REM Contingency Plan...
                    file% = 2%  /* No Sort */
                    sort$ = "P"
                    goto L12000

L11310:         REM Sort into Part Type order...
                    parttype$ = "????"
                    call "READ100" (#4, part$, f1%(4))
                        if f1%(4) = 0 then L11370
                    get #4, using L11360, parttype$
L11360:                 FMT POS(180), CH(3)
L11370:             write #17, using L11380, parttype$, pipoutrec$
L11380:                 FMT CH(16), CH(64)
                    goto sort_loop  /* Go Get Next One */

L11410:         REM Sort into Bin Location order...
                    bin$ = " "
                    call "HNYBINSB" ("S", part$, prtbin$, inc_loc$,      ~
                                      jbstore$, qty_loc%, bin_loc$(),    ~
                                      #4, #5, #13)

                    bin$ = str(bin_loc$(1),11,8)

                    write #17, using L11380, bin$, pipoutrec$
                    goto sort_loop  /* Go Get Next One */

L11520:         REM Sort into Part Category order...
                    pcat$ = "????"
                    call "READ100" (#4, part$, f1%(4))
                        if f1%(4) = 0 then L11580
                    get #4, using L11570, pcat$
L11570:                 FMT POS(90), CH(4)
L11580:             write #17, using L11380, pcat$, pipoutrec$
                    goto sort_loop  /* Go Get Next One */

L11610:         REM Sort into Part Class order...
                    pclass$ = "????"
                    call "READ100" (#4, part$, f1%(4))
                        if f1%(4) = 0 then L11670
                    get #4, using L11660, pclass$
L11660:                 FMT POS(133), CH(4)
L11670:             write #17, using L11380, pclass$, pipoutrec$
                    goto sort_loop  /* Go Get Next One */

L11700:         REM Sort into BOM order (via SYSTIME stamp on PIPOUT)...
                    write #17,using L11380,str(pipoutrec$,49,8),pipoutrec$
                    goto sort_loop  /* Go Get Next One */

L12000: REM *************************************************************~
            *        P R I N T   C O M P O N E N T   L I S T            *~
            *-----------------------------------------------------------*~
            * First get a component of the job and get it ready!        *~
            *************************************************************

            init (hex(00)) pipoutplow$
            break% = 19%
            str(pipoutplow$,1,19) = tagnr$
            if file% = 2% then comp_loop
                pipoutplow$ = hex(00)
                break% = 0%

        comp_loop
            mode% = pmode%  /* Set To JB Part Ser# status */
            call "PLOWNEXT" (#file%, pipoutplow$, break%, f1%(file%))
               if f1%(file%) = 0 then L16000 /* End this one gracefully */
            if file% = 17% then L13220

            qty = 0 : tempv% = 999999%
L13070:     v% = val(str(pipoutplow$,45,4),4)   /* On PIPOUT */
            tempv% = min(tempv%, v%)
            if v% > cutoff% then L13170      /* Beyond cutoff date */
            get #2, using L13110, part$, temp
L13110:         FMT XX(19), CH(25), XX(12), PD(14,4)
            qty = qty + temp
            if psum$ = "F" then L13190
            if psum$ = "D" then break% = 48% else break% = 44%
            call "PLOWNEXT" (#file%, pipoutplow$, break%, f1%(file%))
               if f1%(file%) <> 0% then L13070
L13170:     break% = 19%
            v% = tempv%
L13190:     if qty <= 0 then comp_loop
            goto L13280

L13220:     v% = val(str(pipoutplow$,61,4),4)   /* On WORKFILE */
            if v% > cutoff% then comp_loop  /* Shouldn't Happen */
            get #17, using L13250, part$, qty
L13250:         FMT XX(35), CH(25), XX(12), PD(14,4)
            if qty < 0 then comp_loop

L13280:     call "DATE" addr ("G+", calstart$, v%-1%, temp$, err%)
            pickby$ = temp$
            call "DATEFMT" (pickby$)
            if err% <> 0 then pickby$ = " "

            partdescr$ = "  ** NOT ON FILE **" : ptype$ = " "
            call "READ100" (#4, part$, f1%(4))
                if f1%(4) = 0 then L13500
            get #4, using L13380, partdescr$, unit$, pcat$, sercomp$,     ~
                                 pclass$, parttype$
L13380:         FMT POS(26), CH(32), POS(74), CH(4), POS(90), CH(4),     ~
                    POS(131), CH(1), POS(133), CH(4), POS(180), CH(3)

            ptype$ = "Type:" & parttype$
            if sort$ = "C" then ptype$ = "Cat:" & pcat$
            if sort$ = "S" then ptype$ = "Cls:" & pclass$
            convert parttype$ to type%, data go to L13500
            if type% > 489% and type% < 500% then L13480
                if type% > 789% and type% < 800% then L13480
                    goto L13500
L13480:     partdescr$ = str(partdescr$,,18) & ":TOOL NO ISSUE"

L13500:     bincntr% = 0%
            if sercomp$ <> "Y" then L13570
                REM NOTE-Caelus doesn't support ser comp & non ser parent
                ser$(1,2) = "COMP SERIAL NUMBER"
                init ("_") ser$(2,2)
                mode% = 3%   /* Parent AND child Serial Numbers */
                maxlines% = int(qty)
L13570:     call "CONVERT" (qty, 0.2, quantity$)
            readkey$ = str(part$) & hex(00)
            call "PLOWNEXT" (#18, readkey$, 25%, f1%(18%))

        REM *************************************************************~
            * Now decide what print mode we're in and go to it!!        *~
            *************************************************************

            stlot$ = str(str$) & "/" & str(lot$)

*       * Stay here for no ser #'s

            needed% = 2% + barsize%
            if mode% > 1% then needed% = 5% + barsize%
            if line% + needed% > pge% then gosub page_head
*        Text printing resulting for heading logic may still have left
*        us without enough space, so call again (text only prints once)
            if line% + needed% > pge% then gosub page_head

            if f1%(18%) = 0% then alt$ = " " else alt$ = "(*)"
            print using L60350, str(part$ & alt$), unit$, pickby$, " "
            line% = line% + 1%
            call"BARCODES"("P", part$, "1", " "," "," "," "," ",0%,line%)
            if mode% > 1% then ser_nos
               gosub print_bin_locations
               if exline% = 0% then comp_loop
                  REM Print Requested Extra Lines...
                  for z% = 0% to exline%
                      if line% > pge% then gosub page_head
                         print using L60390, " ", " ", " ", stlot$, bin$
                         print skip(1)
                         line% = line% + 2%
                  next z%
            goto comp_loop

        ser_nos                         /* Assy ser #'s only here */
                print using L60470, partdescr$, ptype$, quantity$
                print using L60500, ser$(1,1), ser$(1,2), binlit$
                print skip(1)
                line% = line% + 3%

                if prtbin$ <> "N" then L15110
                   init("_") stlot$, lot$, str$
                   init(" ") bin$
                   if jbstore$ <> " " then str$ = jbstore$
                   goto L15120
L15110:         gosub print_bin_locations       /* we have Bin #'s */
L15120:         stlot$ = str(str$) & "/" & str(lot$)
                k% = maxlines% - bincntr%
                if k% < 0% then comp_loop
                   REM Put out enough more lines to allow writing all S/Ns
                   for z% = 1% to k%
                       gosub print_extra_ser_lines
                   next z%
            goto comp_loop

L16000: REM end it all here
            if line% > pge% then gosub page_head
            print skip(2)
            print using L60230
            goto next_job


        print_bin_locations
            bincntr% = 0% : bin_loc_cnt% = 0% : init (" ") bin_loc$()
            if prtbin$ = "N" then L17060   /* Still want descr line */
               call "HNYBINSB" ("P", part$, prtbin$, inc_loc$, jbstore$, ~
                                qty_loc%, bin_loc$(), #4, #5, #13)

L17060:     REM Read Next BIN_LOC$ element to see if candidate for print.
            bin_loc_cnt% = bin_loc_cnt% + 1%
            if bin_loc_cnt%>1% and bin_loc$(bin_loc_cnt%)=" " then return
               if bin_loc_cnt% > 100% then return
                  get bin_loc$(bin_loc_cnt%), using L17110,str$,lot$,bin$
L17110:           FMT POS(2), CH(3), CH(6), CH(8)
                  if str$ = " " then init ("_") str$
                  if lot$ = " " then init ("_") lot$
                  if bin$ = " " then init ("_") bin$

            REM Will Print This One...
            bincntr% = bincntr% + 1%
            stlot$ = str(str$) & "/" & str(lot$)

*       * Stay here for no serial #'s
            if mode% = 1% then L17240
               gosub print_extra_ser_lines
               goto L17060
L17240:     if line% + barsize% > pge% then gosub page_head
            if bincntr% = 1% then                                        ~
                print using L60390, partdescr$, ptype$, quantity$,        ~
                                   stlot$, bin$                          ~
                             else                                        ~
                print using L60430, stlot$, bin$
            line% = line% + 1%
            saveline% = line%
            call"BARCODES" ("P",str$,"50",lot$,"62"," "," "," ",1%,line%)
            if saveline% <> line% then L17360
               print skip(1)
               line% = line% + 1%
L17360:     if prtbin$ = "N" then return
               goto L17060

        print_extra_ser_lines
            if line% + barsize% > pge% then gosub page_head
            print using L60530, ser$(2,1), ser$(2,2), stlot$, bin$
            line% = line% + 1%
            if stlot$ = last_stlot$ then L18100  /* Save Space */
               last_stlot$ = stlot$
               saveline% = line%
               call"BARCODES" ("P",str$,"50",lot$,"62"," "," "," ",1%,   ~
                               line%)
               if saveline% <> line% then return
L18100:           print skip(1)
                  line% = line% + 1%
                  return

        page_head
            tim$ = " " : call "TIME" (tim$)
            page% = page% + 1%
            print page
            print using L60040, userid$, page%
            print using L60090, hddate$
            line% = 4% : last_stlot$ = hex(ff)
            print using L60060, header$
            call "BARCODES" ("P",currentjob$,"23"," "," "," "," "," ",1%,~
                             line%)
            if line% <> 4% then L40110
               print skip(1) : line% = 5%
L40110:     if f1%(14%) = 0% or page% > 1% then L40230
                REM Print Vendor Name For Purchased Job...
                print using L60060, ven_name$
                line% = line% + 1%
                for v% = 1% to 5%
                    if ven_addr$(v%) = " " then L40190
                         print using L60060, ven_addr$(v%)
                         line% = line% + 1%
L40190:         next v%
                line% = line% + 1%
                print skip(1)

L40230:     if page% > 1% then L40290
               print using L60170, jbpartdescr$, rev$
               print using L60200, jbquant$, jbworkcenter$, bomid$,       ~
                                  rteid$, stdate$
               print skip(1)
               line% = line% + 3%
L40290:     print using L60250, binlit$
            print using L60280, bindsh$
            line% = line% + 2%
            if page% > 1% then return
               textid$ = jtxtid$ : gosub print_text
               textid$ = btxtid$ : gosub print_text
               textid$ = stxtid$ : gosub print_text
            return

        print_text
            if textid$ = hex(ffffffff) then textid$ = " "
            if textid$ = hex(00000000) then textid$ = " "
            if textid$ = " " then return
               comp% = 0%
               inline% = line%
L40430:        call "TXTPRINT" (#15, f2%(15%), 134%, textid$, "JB0016",  ~
                               1%, line%, pge%, "Y", " ", comp%)
               if comp% = 0% then L40480
                  gosub page_head
                  goto L40430
L40480:        if inline% = line% then L40500
               print
               line% = line% + 1%
L40500:        return

        REM *************************************************************~
            *    I M A G E   S T A T E M E N T S                        *~
            *************************************************************

L60040: %Run By: ###           * * * Materials Requirement List * * *    ~
        ~         PAGE: ###
L60060: %################################################################~
        ~################

L60090: %                   #############################################

        %JOB ORDER NUMBER: ########################

        %          ################################

        %##########################

L60170: %Part No: #######################################################~
        ~#####  Revision:##

L60200: %Quantity To Make: ##########First Work Center:####  BOM:### RTE:~
        ~### Start:########

L60230: %                   * * * END OF MATERIALS REQUIREMENT LIST * * *

L60250: %PART NUMBER/DESCRIPTION    UOM   PICK BY  QTY REQ'D  STR/LOT    ~
        ~#######  QTY ISS'D

L60280: %-------------------------------- -------- ---------- ---------- ~
        ~#######  ---------

        %________________________________________________________________~
        ~__________________

*       * This line for all cases, part & UOM
L60350: %########################### #### ########                       ~
        ~########

*       * For bin turned on - 1st line detail  (no ser #)
L60390:  %################################ ######## ########## ##########~
        ~ ######## _________

*       * For bin turned on - all rest details   (no ser #)
L60430:  %                                                     ##########~
        ~ ######## _________

*       * For no bin printed  (with ser #)  No quantity line here
L60470: %################################ ######## ##########

*       * Now a header line for ser #'s with or w/o bins
L60500: %    ####################  ***  ####################  STR/LOT    ~
        ~######## QTY ISS'D

L60530:  %    ####################  ***  ####################  ##########~
        ~ ######## _________

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            call "BARCODES" ("E"," "," "," "," "," "," "," ",0%,0%)
            end
