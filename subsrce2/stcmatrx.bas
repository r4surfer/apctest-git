        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   TTTTT   CCC   M   M   AAA   TTTTT  RRRR   X   X   *~
            *  S        T    C   C  MM MM  A   A    T    R   R   X X    *~
            *   SSS     T    C      M M M  AAAAA    T    RRRR     X     *~
            *      S    T    C   C  M   M  A   A    T    R   R   X X    *~
            *   SSS     T     CCC   M   M  A   A    T    R   R  X   X   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCMATRX - Displays Standard Costing Matrix for the Part/ *~
            *            Cost Set requested.                            *~
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
            * 06/17/87 ! Original (JRH and KAB)                   ! ERN *~
            * 12/19/96 ! PRR 13674 Keep Fold-in MSD on big numbers! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "STCMATRX"   (part$,         /* Part to Display            */~
                          set$,          /* Cost Set to Display. Blank */~
                                         /*   for current (returned)   */~
                          #2,            /* SYSFILE2                   */~
                          #4 )           /* HNYMASTR                   */

        dim                                                              ~
            bkt_ids$(12)10,              /* Short bucket names         */~
            bkt_descrs$(12)20,           /* Longg bucket names         */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            fin_bom(12), fin_bom$(12)10, /* Fold-in BOM costs          */~
            fib$10,                      /* Edited Fold-in BOM total   */~
            fit$11,                      /* Edited Fold-in cost total  */~
            fold_in(12), fold_in$(12)11, /* Fold-in total costs        */~
            hny_bom$3,                   /* Bill of Materials          */~
            hny_bom(12),                 /* BOM cost from STCHNY       */~
            hny_map$8,                   /* Mapping to Use             */~
            hny_rte$3,                   /* Route                      */~
            hny_rte(12),                 /* RTE cost from STCHNY       */~
            hny_srq$10,                  /* Standard Run Qty           */~
            hny_tlv(12),                 /* This level costs (STCHNY)  */~
            hny_typ$3,                   /* Part type                  */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            line2$79,                    /* Second Line of Screen Headr*/~
            line7$79,                    /* Screen Line #7 (Col Hdrs)  */~
            map%(12),                    /* Fold-in cost mapping       */~
            message$50,                  /* Roll-up Pending Message    */~
            mfac$(5)1,                   /* Cost matrix FACs           */~
            mfac1$(12)1,                 /* Cost matrix FACs           */~
            mfac2$(12)1,                 /* Cost matrix FACs           */~
            mfac3$(12)1,                 /* Cost matrix FACs           */~
            mfac4$(12)1,                 /* Cost matrix FACs           */~
            mfac5$(12)1,                 /* Cost matrix FACs           */~
            part$25, part_descr$60,      /* Part number & description  */~
            pf8$18,                      /* PF 8 Screen Literal        */~
            rol_bom$3,                   /* BOM after last roll up     */~
            rol_rte$3,                   /* RTE after last roll up     */~
            roll_up(12), roll_up$(12)12, /* Roll-up cost totals        */~
            rte$10,                      /* Edited RTE costs           */~
            rub$10,                      /* Edited Roll-up BOM costs   */~
            rup$12,                      /* Edited Roll-up total costs */~
            rup_bom$(12)10,              /* Edit Roll-up BOM costs     */~
            rup_rte$(12)10,              /* Edit Roll-up RTE costs     */~
            rup_tlv$(12)10,              /* Edit Roll-up This Lvl costs*/~
            set$8, setid$4, setdescr$30, /* Cost set descr from caller */~
            tlv$10                       /* Edited This Level costs    */

        dim f1%(32)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 4 ! HNYMASTR ! Inventory Master File                    *~
            * # 6 ! STCHNY   ! Standard Cost Set- Inventory Standards   *~
            * # 7 ! STCMAPNG ! Standard Cost Set Mappings               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 6, "STCHNY",                                        ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =  1,   keylen = 25

            select # 7, "STCMAPNG",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =  1,   keylen =  8

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date : call "DATEFMT" (date$)

            str(line7$, 1) = "Bucket ID "
            str(line7$,11) = hex(ac) & "RollUp Cst" & hex(8c) & "!"
            str(line7$,24) = hex(ac) & "RollUp BOM"
            str(line7$,35) = hex(ac) & "  RTE Cost"
            str(line7$,46) = hex(ac) & "This Level"
            str(line7$,57) = hex(ac) & "FoldIn BOM" & hex(8c) & "!"
            str(line7$,70) = "FoldIn Cst"

            call "READ100" (#4, part$, f1%(4))
            if f1%(4) = 0% then exit_program
                get #4 using L09190, part_descr$, hny_typ$
L09190:              FMT POS(26), CH(32), POS(180), CH(3)
                part_descr$ = part$ & "  (" & part_descr$ & ")"

*        Get descriptors for Cost Set
            buckets% = 3%
            call "STCSETID" (buckets%, #2, set$, setid$, bkt_ids$(),     ~
                             bkt_descrs$(), setdescr$)
            if buckets% > 0% then L09320
                call "ASKUSER" (0%, "STANDARD COST MATRIX",              ~
                                "The Standard Cost Set Requested",       ~
                                "was not found on file.",                ~
                                "Press RETURN to exit display...")
                goto exit_program
L09320:     line2$ = "Cost Set: " & set$ & " (" & setdescr$ & ")"

*        Open STCHNY and STCMAPNG for Cost Set
            call "STCFOPEN" (set$, "SxxxSx", #2, errormsg$,              ~
                                                 #6, #6, #6, #6, #7, #7)
            if errormsg$ = " " then L09430
                call "ASKUSER" (0%, "STANDARD COST MATRIX",              ~
                                "Unable to open Cost Set:", errormsg$,   ~
                                "Press RETURN to exit display...")
                goto exit_program

L09430
*        Get Part Information required for display
            call "READ100" (#6, part$, f1%(6))
            if f1%(6) = 1% then L09510
                call "ASKUSER" (0%, "STANDARD COST MATRIX",              ~
                                "Part " & part$ & "not in Cost Set.",    ~
                                "(All costs are therefore zero).",       ~
                                "Press RETURN to exit display...")
                goto exit_program
L09510:     get #6 using L09540, hny_map$, hny_bom$, hny_rte$, hny_srq,    ~
                               hny_bom(), hny_rte(), hny_tlv(),          ~
                               rol_bom$, rol_rte$, rol_srq
L09540:         FMT XX(25), CH(8), XX(4), CH(3), CH(3), PD(14,4), XX(8), ~
                    36*PD(14,4), CH(3), CH(3), PD(14,4)
            message$ = " "
            if hny_bom$ <> rol_bom$ or hny_rte$ <> rol_rte$ or           ~
               hny_srq  <> rol_srq  then message$ =                      ~
                                "Costs are out-of-date pending roll-up."
            call "CONVERT" (hny_srq, -0.2, hny_srq$)

        REM *************************************************************~
            *             D I S P L A Y   D R I V E R                   *~
            *-----------------------------------------------------------*~
            * Control Screen Display.                                   *~
            *************************************************************

            rup, rub, rte, tlv, fib, fit = 0
            mat roll_up = zer : mat fold_in = zer : mat fin_bom = zer
            mat map% = zer
            init (hex(8c)) mfac$(), mfac1$(), mfac2$(), mfac3$(),        ~
                           mfac4$(), mfac5$()
            init (" ") inpmessage$, rup$, rub$, rte$, tlv$, fib$, fit$,  ~
                       roll_up$(), fold_in$(), fin_bom$(), rup_bom$(),   ~
                       rup_rte$(), rup_tlv$()

            inpmessage$ = "Position Cursor and Press (RETURN) to see" &  ~
                          " Total Derivation."

            call "READ100" (#7, hny_map$, f1%(7))
            if f1%(7) = 1% then get #7 using L10200, map%()
L10200:         FMT  /* File #7- STCMAPNG (STCnnnnM) */ POS(39), 12*BI(1)
            mat roll_up = hny_bom + hny_rte
            mat roll_up = roll_up + hny_tlv
            mat fold_in = hny_rte + hny_tlv
            for n% = 1% to 12%
                if map%(n%) = 0% then map%(n%) = n%
                fin_bom(map%(n%)) = fin_bom(map%(n%)) + hny_bom(n%)
                rup = rup + roll_up(n%)
                rub = rub + hny_bom(n%)
                rte = rte + hny_rte(n%)
                tlv = tlv + hny_tlv(n%)
                fib = fib + hny_bom(n%)
                fit = fit + hny_bom(n%) + hny_rte(n%) + hny_tlv(n%)
            next n%
            mat fold_in = fold_in + fin_bom
            for n% = 1% to 12%
                if n% > buckets% then goto L10440
                     call "CONVERT" (roll_up(n%), 4.4,                   ~
                          str(roll_up$(n%),,10))
                     call "CONVERT" (fold_in(n%), 4.4,                   ~
                                                    str(fold_in$(n%),2%))
                     call "CONVERT" (fin_bom(n%), 4.4, fin_bom$(n%))
                     call "CONVERT" (hny_bom(n%), 4.4, rup_bom$(n%))
                     call "CONVERT" (hny_rte(n%), 4.4, rup_rte$(n%))
                     call "CONVERT" (hny_tlv(n%), 4.4, rup_tlv$(n%))
L10440:         str(roll_up$(n%),12,1) = "!"
                str(fold_in$(n%), 1,1) = "!"
            next n%
            call "CONVERT" (rup, 4.4, str(rup$,,10))
            str(rup$,12,1) = "!"
            call "CONVERT" (rub, 4.4, rub$)
            call "CONVERT" (rte, 4.4, rte$)
            call "CONVERT" (tlv, 4.4, tlv$)
            call "CONVERT" (fib, 4.4, fib$)
            call "CONVERT" (fit, 4.4, str(fit$,2%))
            str(fit$,1,1) = "!"
            toggle% = 1%
            gosub pf8_toggler

        display_the_matrix
            str(line7$,11,1) = mfac$(1) or hex(20)
            str(line7$,22,1) = mfac$(1)
            str(line7$,24,1) = mfac$(2) or hex(20)
            str(line7$,35,1) = mfac$(3) or hex(20)
            str(line7$,46,1) = mfac$(3) or hex(20)
            str(line7$,57,1) = mfac$(4) or hex(20)
            str(line7$,68,1) = mfac$(5) or hex(20)
            gosub'105
            if u3% =   0% then gosub hilite_map
            if u3% =   8% then gosub pf8_toggler
            if u3% <> 16% then display_the_matrix
            goto exit_program

        pf8_toggler
          on toggle% goto L10740, L10810
L10740:     init (hex(84)) mfac$(1), mfac$(2), mfac$(3), mfac1$(),       ~
                           mfac2$(), mfac3$()
            init (hex(8c)) mfac$(4), mfac$(5), mfac4$(), mfac5$()
            pf8$ = "(8)HiLite Fold-in"
            toggle% = 2%
            return

L10810:     init (hex(84)) mfac$(3), mfac$(4), mfac$(5), mfac3$(),       ~
                           mfac4$(), mfac5$()
            init (hex(8c)) mfac$(1), mfac$(2), mfac1$(), mfac2$()
            pf8$ = "(8)HiLite Roll-up"
            toggle% = 1%
            return

        hilite_map
            cursor%(1%) = cursor%(1%) - 7%
            if cursor%(1%) < 1% or cursor%(1%) > 12% then reset_toggle
            if cursor%(1%) > buckets% then reset_toggle
            init (hex(8c)) mfac1$(), mfac2$(), mfac3$(), mfac4$(),       ~
                                                                 mfac5$()
            on toggle% goto L10990, L10950          /* FOLD-IN, ROLL-UP */
L10950:         mfac1$(cursor%(1%)), mfac2$(cursor%(1%)),                ~
                                     mfac3$(cursor%(1%)) = hex(84)
                return

L10990:         mfac3$(cursor%(1%)), mfac5$(cursor%(1%)) = hex(84)
                mfac$(4) = hex(8c) : mfac$(2) = hex(84)
                for n% = 1% to 12%
                    if map%(n%) = cursor%(1%) then mfac2$(n%) = hex(84)
                next n%
                return

        reset_toggle
            toggle% = 1% + mod(toggle%, 2%)
            goto pf8_toggler

        REM *************************************************************~
            *               M A T R I X   S C R E E N                   *~
            *-----------------------------------------------------------*~
            * Display of Cost Matrix Screen.                            *~
            *************************************************************

        deffn'105
            str(line2$,62%) = "STCMATRX: " & str(cms2v$,,8%)

L40090:     accept                                                       ~
               at (01,02),                                               ~
                  "Part Standard Cost Roll-up/Fold-in Matrix"   ,        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), "Part:",                                      ~
               at (04,08), fac(hex(8c))  , part_descr$          , ch(60),~
               at (04,71), "Type:",                                      ~
               at (04,77), fac(hex(8c))  , hny_typ$             , ch(03),~
                                                                         ~
               at (05,02), "BOM:",                                       ~
               at (05,07), fac(hex(8c))  , hny_bom$             , ch(03),~
               at (05,15), "Route:",                                     ~
               at (05,22), fac(hex(8c))  , hny_rte$             , ch(03),~
               at (05,31), "Std Run Qty:",                               ~
               at (05,44), fac(hex(8c))  , hny_srq$             , ch(10),~
               at (05,61), "   Mapping:",                                ~
               at (05,73), fac(hex(8c))  , hny_map$             , ch(08),~
                                                                         ~
               at (07,02), fac(hex(ac))  , line7$               , ch(79),~
                                                                         ~
               at (08,02), fac(hex(8e))   , bkt_ids$( 1)        , ch(10),~
               at (08,13), fac(mfac1$( 1)), roll_up$( 1)        , ch(12),~
               at (08,26), fac(mfac2$( 1)), rup_bom$( 1)        , ch(10),~
               at (08,37), fac(mfac3$( 1)), rup_rte$( 1)        , ch(10),~
               at (08,48), fac(mfac3$( 1)), rup_tlv$( 1)        , ch(10),~
               at (08,59), fac(mfac4$( 1)), fin_bom$( 1)        , ch(10),~
               at (08,70), fac(mfac5$( 1)), fold_in$( 1)        , ch(11),~
                                                                         ~
               at (09,02), fac(hex(8e))   , bkt_ids$( 2)        , ch(10),~
               at (09,13), fac(mfac1$( 2)), roll_up$( 2)        , ch(12),~
               at (09,26), fac(mfac2$( 2)), rup_bom$( 2)        , ch(10),~
               at (09,37), fac(mfac3$( 2)), rup_rte$( 2)        , ch(10),~
               at (09,48), fac(mfac3$( 2)), rup_tlv$( 2)        , ch(10),~
               at (09,59), fac(mfac4$( 2)), fin_bom$( 2)        , ch(10),~
               at (09,70), fac(mfac5$( 2)), fold_in$( 2)        , ch(11),~
                                                                         ~
               at (10,02), fac(hex(8e))   , bkt_ids$( 3)        , ch(10),~
               at (10,13), fac(mfac1$( 3)), roll_up$( 3)        , ch(12),~
               at (10,26), fac(mfac2$( 3)), rup_bom$( 3)        , ch(10),~
               at (10,37), fac(mfac3$( 3)), rup_rte$( 3)        , ch(10),~
               at (10,48), fac(mfac3$( 3)), rup_tlv$( 3)        , ch(10),~
               at (10,59), fac(mfac4$( 3)), fin_bom$( 3)        , ch(10),~
               at (10,70), fac(mfac5$( 3)), fold_in$( 3)        , ch(11),~
                                                                         ~
               at (11,02), fac(hex(8e))   , bkt_ids$( 4)        , ch(10),~
               at (11,13), fac(mfac1$( 4)), roll_up$( 4)        , ch(12),~
               at (11,26), fac(mfac2$( 4)), rup_bom$( 4)        , ch(10),~
               at (11,37), fac(mfac3$( 4)), rup_rte$( 4)        , ch(10),~
               at (11,48), fac(mfac3$( 4)), rup_tlv$( 4)        , ch(10),~
               at (11,59), fac(mfac4$( 4)), fin_bom$( 4)        , ch(10),~
               at (11,70), fac(mfac5$( 4)), fold_in$( 4)        , ch(11),~
                                                                         ~
               at (12,02), fac(hex(8e))   , bkt_ids$( 5)        , ch(10),~
               at (12,13), fac(mfac1$( 5)), roll_up$( 5)        , ch(12),~
               at (12,26), fac(mfac2$( 5)), rup_bom$( 5)        , ch(10),~
               at (12,37), fac(mfac3$( 5)), rup_rte$( 5)        , ch(10),~
               at (12,48), fac(mfac3$( 5)), rup_tlv$( 5)        , ch(10),~
               at (12,59), fac(mfac4$( 5)), fin_bom$( 5)        , ch(10),~
               at (12,70), fac(mfac5$( 5)), fold_in$( 5)        , ch(11),~
                                                                         ~
               at (13,02), fac(hex(8e))   , bkt_ids$( 6)        , ch(10),~
               at (13,13), fac(mfac1$( 6)), roll_up$( 6)        , ch(12),~
               at (13,26), fac(mfac2$( 6)), rup_bom$( 6)        , ch(10),~
               at (13,37), fac(mfac3$( 6)), rup_rte$( 6)        , ch(10),~
               at (13,48), fac(mfac3$( 6)), rup_tlv$( 6)        , ch(10),~
               at (13,59), fac(mfac4$( 6)), fin_bom$( 6)        , ch(10),~
               at (13,70), fac(mfac5$( 6)), fold_in$( 6)        , ch(11),~
                                                                         ~
               at (14,02), fac(hex(8e))   , bkt_ids$( 7)        , ch(10),~
               at (14,13), fac(mfac1$( 7)), roll_up$( 7)        , ch(12),~
               at (14,26), fac(mfac2$( 7)), rup_bom$( 7)        , ch(10),~
               at (14,37), fac(mfac3$( 7)), rup_rte$( 7)        , ch(10),~
               at (14,48), fac(mfac3$( 7)), rup_tlv$( 7)        , ch(10),~
               at (14,59), fac(mfac4$( 7)), fin_bom$( 7)        , ch(10),~
               at (14,70), fac(mfac5$( 7)), fold_in$( 7)        , ch(11),~
                                                                         ~
               at (15,02), fac(hex(8e))   , bkt_ids$( 8)        , ch(10),~
               at (15,13), fac(mfac1$( 8)), roll_up$( 8)        , ch(12),~
               at (15,26), fac(mfac2$( 8)), rup_bom$( 8)        , ch(10),~
               at (15,37), fac(mfac3$( 8)), rup_rte$( 8)        , ch(10),~
               at (15,48), fac(mfac3$( 8)), rup_tlv$( 8)        , ch(10),~
               at (15,59), fac(mfac4$( 8)), fin_bom$( 8)        , ch(10),~
               at (15,70), fac(mfac5$( 8)), fold_in$( 8)        , ch(11),~
                                                                         ~
               at (16,02), fac(hex(8e))   , bkt_ids$( 9)        , ch(10),~
               at (16,13), fac(mfac1$( 9)), roll_up$( 9)        , ch(12),~
               at (16,26), fac(mfac2$( 9)), rup_bom$( 9)        , ch(10),~
               at (16,37), fac(mfac3$( 9)), rup_rte$( 9)        , ch(10),~
               at (16,48), fac(mfac3$( 9)), rup_tlv$( 9)        , ch(10),~
               at (16,59), fac(mfac4$( 9)), fin_bom$( 9)        , ch(10),~
               at (16,70), fac(mfac5$( 9)), fold_in$( 9)        , ch(11),~
                                                                         ~
               at (17,02), fac(hex(8e))   , bkt_ids$(10)        , ch(10),~
               at (17,13), fac(mfac1$(10)), roll_up$(10)        , ch(12),~
               at (17,26), fac(mfac2$(10)), rup_bom$(10)        , ch(10),~
               at (17,37), fac(mfac3$(10)), rup_rte$(10)        , ch(10),~
               at (17,48), fac(mfac3$(10)), rup_tlv$(10)        , ch(10),~
               at (17,59), fac(mfac4$(10)), fin_bom$(10)        , ch(10),~
               at (17,70), fac(mfac5$(10)), fold_in$(10)        , ch(11),~
                                                                         ~
               at (18,02), fac(hex(8e))   , bkt_ids$(11)        , ch(10),~
               at (18,13), fac(mfac1$(11)), roll_up$(11)        , ch(12),~
               at (18,26), fac(mfac2$(11)), rup_bom$(11)        , ch(10),~
               at (18,37), fac(mfac3$(11)), rup_rte$(11)        , ch(10),~
               at (18,48), fac(mfac3$(11)), rup_tlv$(11)        , ch(10),~
               at (18,59), fac(mfac4$(11)), fin_bom$(11)        , ch(10),~
               at (18,70), fac(mfac5$(11)), fold_in$(11)        , ch(11),~
                                                                         ~
               at (19,02), fac(hex(8e))   , bkt_ids$(12)        , ch(10),~
               at (19,13), fac(mfac1$(12)), roll_up$(12)        , ch(12),~
               at (19,26), fac(mfac2$(12)), rup_bom$(12)        , ch(10),~
               at (19,37), fac(mfac3$(12)), rup_rte$(12)        , ch(10),~
               at (19,48), fac(mfac3$(12)), rup_tlv$(12)        , ch(10),~
               at (19,59), fac(mfac4$(12)), fin_bom$(12)        , ch(10),~
               at (19,70), fac(mfac5$(12)), fold_in$(12)        , ch(11),~
                                                                         ~
               at (20,02), "Totals:",                                    ~
               at (20,13), fac(mfac$(1)) , rup$                 , ch(12),~
               at (20,26), fac(mfac$(2)) , rub$                 , ch(10),~
               at (20,37), fac(mfac$(3)) , rte$                 , ch(10),~
               at (20,48), fac(mfac$(3)) , tlv$                 , ch(10),~
               at (20,59), fac(mfac$(4)) , fib$                 , ch(10),~
               at (20,70), fac(mfac$(5)) , fit$                 , ch(11),~
                                                                         ~
               at (22,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), fac(hex(8c)),   message$,                     ~
               at (23,65), "(13)Instructions",                           ~
               at (24,45), "(15)Print Screen",                           ~
               at (24,20), fac(hex(8c)), pf8$                           ,~
               at (24,65), "(16)Return",                                 ~
                     keys (hex(00080d0f10)), key (u3%)

               if u3% <> 13% then L41480
                  call "MANUAL" ("STCMATRX")
                  goto L40090

L41480:        if u3% <> 15% then L41520
                  call "PRNTSCRN"
                  goto L40090

L41520:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Return to Caller.                                         *~
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
            end
