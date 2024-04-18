        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   TTTTT   CCC   H   H  N   N  Y   Y   SSS   BBBB    *~
            *  S        T    C   C  H   H  NN  N  Y   Y  S      B   B   *~
            *   SSS     T    C      HHHHH  N N N   YYY    SSS   BBBB    *~
            *      S    T    C   C  H   H  N  NN    Y        S  B   B   *~
            *   SSS     T     CCC   H   H  N   N    Y     SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCHNYSB - Edits, Displays and prints standard cost infor-*~
            *            mation based on passed parameters              *~
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
            * 03/23/87 ! Original                                 ! JIM *~
            * 03/24/88 ! Added ability to edit RTE                ! RJM *~
            * 05/09/88 ! Removed Call To STCENGSB, put in STCINPUT! HES *~
            * 05/13/88 ! Added Ability To Input Materials Bucket  ! HES *~
            * 09/20/88 ! Fixed Edit of Materials Bucket           ! KAB *~
            * 10/25/88 ! Field By Field of BOM, RTE, etc. line    ! KAB *~
            *          ! No PLOWCODE if RTE Id. is left blank     !     *~
            *          ! Lifted JDH's Init line for report (thanx)!     *~
            * 04/03/89 ! Added second Return code to calling arg  ! MJB *~
            *          !  list of sub STCSPROL.                   !     *~
            * 11/13/90 ! Added Management DMC to screen & save.   ! JDH *~
            * 06/13/91 ! Added Management DMC calc to PF17 opt.   ! KAB *~
            * 11/07/91 ! CMS/DEC 'MASK' Project                   ! SID *~
            * 02/21/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 10/20/92 ! Removed FACs from page zero.             ! JIM *~
            * 12/19/96 ! PRR 13675 Keep significant fold-in digit ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "STCHNYSB" (costset$,        /* Cost set code              */~
                        setdesc$,        /* Cost set description       */~
                        pnfgflag$,       /* Frozen, Global flag        */~
                        buckets%,        /* Number of cost buckets     */~
                        bkt_ids$(),      /* Short buckt names          */~
                        bkt_des$(),      /* Cost bucket descriptions   */~
                        #1,              /* HNYMASTR file              */~
                        #2,              /* BOMMASTR file              */~
                        #3,              /* STCHNY (STCnnnnH) file     */~
                        #4,              /* STCDETAL (STCnnnnD) file   */~
                        #5,              /* STCCHNGS (STCnnnnC) file   */~
                        #6,              /* STCBOMXF file              */~
                        #7,              /* STCMAPNG (STCnnnnM) file   */~
                        #8,              /* SYSFILE2 file              */~
                        #9,              /* TXTFILE file               */~
                        #10,             /* RTEMASTR file              */~
                        #11,             /* STCWCACT (STCnnnnW) file   */~
                        #12,             /* STCLABOR (STCnnnnL) file   */~
                        #13,             /* WCMASTR file               */~
                        #17)             /* MGTFCTR1 file              */

        dim                                                              ~
            allparts$1,                  /* Print all Inventory parts? */~
            beg$25,                      /* Begin Part # for print     */~
            beg_part$25, beg_desc$34,    /* Beginning Part # for print */~
            bkt$3,                       /* Print bucket #             */~
            bkt_des$(12)20,              /* Cost bucket descriptions   */~
            bkt_ids$(12)10,              /* Short bucket names         */~
            bomsave$3,                   /* BOM save area              */~
            brite$1,                     /* Bright FAC                 */~
            buckets$2,                   /* Edited number of buckets   */~
            compname$60,                 /* Company name               */~
            cost$(3)96,                  /* Packed costs for output    */~
            costset$8,                   /* Cost set code from caller  */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descmsg$25,                  /* Optional description msg   */~
            dtl_bkt%(12),                /* Bucket Number              */~
            dtl_des$(12)40,              /* Description                */~
            dtl_fix(12), dtl_fix$(12)10, /* Fixed $                    */~
            dtl_ids$(12)10,              /* Bucket IDs                 */~
            dtl_per(12), dtl_per$(12)10, /* $ per Part                 */~
            dtl_rec$(99)70,              /* Std cost detail records    */~
            dtl_seq$(12)3,               /* Line                       */~
            dtldes$40,                   /* Print detail description   */~
            dtlseq$3,                    /* Print detail sequence #    */~
            edtmessage$79,               /* Edit screen message        */~
            edttran$80,                  /* Edit Translation String    */~
            end$25,                      /* End Part # for print       */~
            end_part$25, end_desc$34,    /* Ending Part # for print    */~
            errormsg$79,                 /* Error message              */~
            fin_bom(12), fin_bom$(12)10, /* Fold-in BOM costs          */~
            fib$10,                      /* Edited Fold-in BOM total   */~
            fit$11,                      /* Edited Fold-in cost total  */~
            fix$10,                      /* Print detail fixed amount  */~
            fold_in(12), fold_in$(12)11, /* Fold-in total costs        */~
            hny_bom$3,                   /* Bill of Materials          */~
            hny_bom(12), hny_bom$(12)12, /* BOM cost from STCHNY       */~
            hny_map$8,                   /* Mapping to Use             */~
            hny_mat$2,                   /* Default Materials Bucket   */~
            hny_rte$3,                   /* Route                      */~
            hny_rte(12), hny_rte$(12)12, /* RTE cost from STCHNY       */~
            hny_seq$(12)3,               /* Screen sequence numbers    */~
            hny_srq$10,                  /* Standard Run Qty           */~
            hny_tlv(12), hny_tlv$(12)12, /* This level costs (STCHNY)  */~
            hny_typ$3,                   /* Part type                  */~
            i$(24)80,                    /* Screen Image               */~
            incl(1), incl$(1)1,          /* Dummy PLOWCODE params      */~
            inpmessage$79,               /* Informational Message      */~
            keytab$33,                   /* PF Keys available          */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            line6$79,                    /* Screen Line 6              */~
            map%(12),                    /* Fold-in cost mapping       */~
            mapdflt$8,                   /* Default Map code           */~
            mdmc$12,                     /* Management DMC Value       */~
            mdmc_prompt$5,               /* Management DMC Prompt Msg  */~
            mdmc_print$18,               /* Management DMC Print Msg   */~
            mdmc_totals(12),             /* Management DMC Totals      */~
            mfac$(5)1,                   /* Cost matrix FACs           */~
            mfac1$(12)1,                 /* Cost matrix FACs           */~
            mfac2$(12)1,                 /* Cost matrix FACs           */~
            mfac3$(12)1,                 /* Cost matrix FACs           */~
            mfac4$(12)1,                 /* Cost matrix FACs           */~
            mfac5$(12)1,                 /* Cost matrix FACs           */~
            mgtrpt_on$1,                 /* Is Management Reporting on?*/~
            misc$50,                     /* Junk                       */~
            msg$(3)79,                   /* PLOWCODE parameter         */~
            nfgflag$1, pnfgflag$1,       /* Frozn/Globl flag from callr*/~
            nmric$1,                     /* Numeric FAC                */~
            partnbr$25, partdesc$34,     /* Part number & description  */~
            per$10,                      /* Print detail amt per part  */~
            pf10$18,                     /* PF 10 Screen Literal       */~
            pf11$18,                     /* PF 11 Screen Literal       */~
            pf12$18,                     /* PF 12 Screen Literal       */~
            pf14$18,                     /* PF 14 Screen Literal       */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf2$21,                      /* PF 2 Screen Literal        */~
            pf25$18,                     /* PF 25 Screen Literal       */~
            pf3$18,                      /* PF 3 Screen Literal        */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$18,                      /* PF 5 Screen Literal        */~
            pf6$18,                      /* PF 6 Screen Literal        */~
            pf7$18,                      /* PF 7 Screen Literal        */~
            pf8$18,                      /* PF 8 Screen Literal        */~
            pf9$40,                      /* PF 9 Screen Literal        */~
            pg2_l7$79,                   /* Screen 2 Line 7 headers    */~
            pg3_l7$79,                   /* Screen 3 Line 7 headers    */~
            pg5_l7$79,                   /* Screen 5 Line 7 headers    */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            prt_typ$3,                   /* Print part type            */~
            rol_bom$3,                   /* BOM after last roll up     */~
            rol_rte$3,                   /* RTE after last roll up     */~
            roll_up(12), roll_up$(12)12, /* Roll-up cost totals        */~
            rptid$6,                     /* Report ID                  */~
            rte$10,                      /* Edited RTE costs           */~
            rub$10,                      /* Edited Roll-up BOM costs   */~
            rup$12,                      /* Edited Roll-up total costs */~
            rup_bom$(12)10,              /* Edit Roll-up BOM costs     */~
            rup_rte$(12)10,              /* Edit Roll-up RTE costs     */~
            rup_tlv$(12)10,              /* Edit Roll-up This Lvl costs*/~
            seq$3,                       /* Output Seq #s (STCDETAL)   */~
            setdesc$30,                  /* Cost set descr from caller */~
            subhdr$60,                   /* Edited cost set & descript */~
            sum_desc$9,                  /* Summary/Detail description */~
            sum_detl$1,                  /* Summary or Detail          */~
            text$(196, 1)70, textid$4,   /* Text & Text ID             */~
            time$8,                      /* Time of day                */~
            tlv$10,                      /* Edited This Level costs    */~
            tot_bom$12,                  /* Edited total BOM cost      */~
            tot_dtl$10,                  /* Edited total Detail cost   */~
            tot_fix$10,                  /* Edited total Fixed cost    */~
            tot_hny$12,                  /* Edited total cost          */~
            tot_per$10,                  /* Edited total cost per part */~
            tot_rte$12,                  /* Edited total RTE cost      */~
            tot_tlv$12,                  /* Edited total Val add cost  */~
            uplow$1,                     /* Up / Low FAC               */~
            upper$1,                     /* Upper case FAC             */~
            userid$3,                    /* Current User ID            */~
            xfac$(12,4)1                 /* FACs for Page 3 (details)  */

        dim f1%(32),                     /* = 1 if READ was successful */~
            f2%(32)                      /* = 0 if the file is open    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! HNYMASTR ! Inventory Master file                    *~
            * #2  ! BOMMASTR ! BOM Relationship file                    *~
            * #3  ! STCnnnnH ! Std Cost Set- Inventory Stds (STCHNY)    *~
            * #4  ! STCnnnnD ! Standard Cost Details (STCDETAL)         *~
            * #5  ! STCnnnnC ! Tickler file of changed parts (STCCHNGS) *~
            * #6  ! STCBOMXF ! Std Cost Cross Reference of BOMs & RTEs  *~
            * #7  ! STCnnnnM ! Standard Cost Set Mappings (STCMAPNG)    *~
            * #8  ! SYSFILE2 ! Caelus Management System Information     *~
            * #9  ! TXTFILE  ! System Text File                         *~
            * #10 ! RTEMASTR ! Production routing master file           *~
            *-----+----------+------------------------------------------*~
            *  where 'nnnn' is a unique cost set identifier generated   *~
            *  by the caller of this subroutine (STCINPUT).             *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            if been_here_before% <> 0% then goto initial_every_time
                been_here_before% = 1%
                call "EXTRACT" addr("ID", userid$)
                date$ = date : call "DATEFMT" (date$)

        REM See if we can determine a default mapping code **************
                mapdflt$ = " "
L09130:         call "PLOWNEXT" (#7, mapdflt$, 0%, f1%(7))
                if f1%(7) = 0% then goto L09190
                     map% = map% + 1%
                     if map% < 2% then goto L09130
                          mapdflt$ = " "

L09190:         str(pg2_l7$, 4) = hex(ac) & "Bucket ID " & hex(8c)
                str(pg2_l7$,16) = hex(ac) & "Bucket Description  "&hex(8c)
                str(pg2_l7$,38) = hex(ac) & "    From BOM" & hex(8c)
                str(pg2_l7$,52) = hex(ac) & "    From RTE" & hex(8c)
                str(pg2_l7$,66) = hex(ac) & "  This Level" & hex(8c)

                str(pg3_l7$, 1) = "Seq" & hex(8c)
                str(pg3_l7$, 5) = hex(ac) & "Description               "&~
                     "              " & hex(8c)
                str(pg3_l7$,46) = hex(ac) & " Fixed Amt" & hex(8c)
                str(pg3_l7$,57) = hex(ac) & " Amt/ Part" & hex(8c)
                str(pg3_l7$,68) = hex(ac) & "Bucket ID " & hex(8c)

                str(pg5_l7$, 1) = "Bucket ID "
                str(pg5_l7$,11) = hex(ac) & "RollUp cst" & hex(8c) & "!"
                str(pg5_l7$,24) = hex(ac) & "RollUp BOM"
                str(pg5_l7$,35) = hex(ac) & "  RTE cost"
                str(pg5_l7$,46) = hex(ac) & "This Level"
                str(pg5_l7$,57) = hex(ac) & "FoldIn BOM" & hex(8c) & "!"
                str(pg5_l7$,70) = "FoldIn cst"

                call "TXTFUTIL" (#9, f2%(9), "INTL", textid$)
                call "COMPNAME" (12%, compname$, u3%)
                rptid$ = "STC001"

            /* See if Management Reporting is on */
            call "READ100" (#8, "SWITCHS.GL", f1%(8))
            if f1%(8) = 1% then get #8 using L09426, mgtrpt_on$
L09426:         FMT POS(59), CH(1)
            if mgtrpt_on$ = "Y" then mdmc_prompt$ = "MDMC:" else         ~
                                     mdmc_prompt$ = " "

        initial_every_time
            convert buckets% to buckets$, pic (##)
            init (" ") hny_seq$()
            for i% = 1% to 12%
                if i% > buckets% then bkt_ids$(i%), bkt_des$(i%) = " "   ~
                     else convert i% to hny_seq$(i%), pic (##.)
            next i%
            init (hex(ff)) textid$, keytab$
            str(keytab$,1,16) = hex(01ffff04ffffffffffffffff0dff0f10)
            str(keytab$,33,1) = hex(00)  /* Enable (RETURN) */

            nfgflag$ = pnfgflag$

            init (hex(00)) edttran$
            init (hex(01)) str(edttran$, 2)
            init (hex(02)) str(edttran$,13)
            init (hex(03)) str(edttran$,26)
            init (hex(04)) str(edttran$,50)
            init (hex(05)) str(edttran$,64)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode1
            gosub pf4_off : gosub pf5_off : gosub pf14_on : gosub pf9_on
            pf16$ = "(16)Exit Routine" : str(keytab$, 16, 1) = hex(10)
            gosub pf2_off : gosub pf3_off : gosub pf7_off : gosub pf8_off
            gosub pf11_12_off : gosub pf25_off : gosub pf10_off
            gosub pf6_on : if nfgflag$ = "F" then gosub pf6_off

            gosub L29000

            for fieldnr% = 1% to 1%
                gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% =  0% then goto L10125
L10090:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  9% then goto L10125
                      if keyhit%  = 14% then goto inputmode4
                      if keyhit%  = 16% then goto L65000
                      if keyhit% <>  0% then goto L10090
L10125:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then goto L10090
            next fieldnr%
            if f1%(3) <> 1% then goto test_frozen_set
                if nfgflag$ <> "F" then goto editpg2
                     brite$, uplow$, upper$, nmric$ = hex(8c)
                     edtmessage$="Cost set is frozen. Edit not allowed."&~
                          " Press (RETURN) to continue."
                     goto editpg2

        test_frozen_set
            if nfgflag$ <> "F" then goto inputmode2
            plowkey$ = partnbr$
L10177:     call "PLOWALTS" (#2, plowkey$, 1%, 25%, f1%(2))
               if f1%(2) = 0% then L10200
            if str(plowkey$,29,3) = "  0" then L10177
            str(plowkey$,54,8) = costset$
            call "REDALT0" (#6, str(plowkey$,26,36), 1%, whereused%)
               if whereused% = 0% then L10177

L10186:         u3% = 2%
                call "ASKUSER" (u3%,                                     ~
               "*** ADD WARNING (FROZEN COST SET) ***", hex(94) &        ~
               "Part: " & partnbr$ & " is used as a Component" & hex(84),~
               "Press PF(17) to add above part to frozen cost set" &     ~
                          " " & str(costset$),                           ~
               "-- OR --      Press PF(1) to ABORT add")
                if u3% =   1% then goto inputmode1
                if u3% <> 17% then goto L10186
                goto L10216

L10200:         u3% = 2%
                call "ASKUSER" (u3%, "*** ADD TO FROZEN COST SET? ***",  ~
                     "Part: " & partnbr$ & " " & str(partdesc$),         ~
                     "Press PF(17) to add above part to frozen cost set"&~
                          " " & str(costset$),                           ~
                     "-- OR --      Press PF(1) to ABORT add")
                if u3% =   1% then goto inputmode1
                if u3% <> 17% then goto L10200
L10216:         pf17sw% = 1%

        inputmode2
            mdmc$ = " "
            gosub pf14_off : gosub pf5_off : gosub pf10_off
            pf16$ = "(16)Edit Costs" : str(keytab$, 16, 1) = hex(10)
            gosub pf2_off : gosub pf3_off : gosub pf6_off : gosub pf25_off
            gosub pf11_12_off : gosub pf7_off : gosub pf9_off
            gosub pf8_on : pf8$ = "(8)Enter Details"
            editmode% = 0%
            f% = 1%
            for fieldnr% = f% to 5% + buckets%
L10275:         gosub'052(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% =  0% then goto L10365
                gosub pf4_off : if fieldnr% = f% then L10295
                     gosub pf4_on : pf4$ = "(4)Previous Field"
L10295:         gosub'102(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then goto L10335
L10310:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then goto L10275
                         if fieldnr% = 1% then goto L10275
                         goto L10310
L10335:               if keyhit% <>  8% then goto L10350
                          gosub test_this_level
                          if u3% <>  0% then goto inputmode3
L10350:               if keyhit% <> 16% then goto L10360
                          errormsg$ = " " : goto editpg2
L10360:               if keyhit% <>  0% then goto L10275
L10365:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10275
            next fieldnr%
            goto editpg2

        inputmode3
            pf16$ = " " : str(keytab$, 16, 1) = hex(ff)
            gosub pf3_off : gosub pf5_off : gosub pf6_off : gosub pf25_off
            gosub pf11_12_off : gosub pf8_off : gosub pf7_off
            gosub pf14_off : gosub pf10_off : gosub pf9_off
            descmsg$ = " or blank to Edit costs"

        inputmode3_continue
            init (" ") dtl_seq$(), dtl_des$(), dtl_fix$(), dtl_per$(),   ~
                dtl_ids$()
            mat dtl_fix = zer : mat dtl_per = zer : mat dtl_bkt% = zer
            for linenr% = 1% to 12%
            for fieldnr% = 1% to 4%
L10450:         gosub'053(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% =  0% then goto L10545
                gosub pf2_off : if fieldnr% < 2 then goto L10470
                     gosub pf2_on : pf2$ = "(2)Line Over"
L10470:         gosub pf4_off : if fieldnr% = 1 then L10480
                     gosub pf4_on : pf4$ = "(4)Previous Field"
L10480:         gosub'103(linenr%, fieldnr%) /* Display & Accept Screen */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  2% then goto L10500
                          fieldnr% = 1% : goto L10450
L10500:               if keyhit% <>  4% then goto L10530
L10505:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'053(fieldnr%)
                         if enabled% = 1% then goto L10470
                         if fieldnr% = 1% then goto L10450
                         goto L10505
L10530:               if keyhit% <>  0% then goto L10470
                          if fieldnr% = 1% and dtl_des$(linenr%) = " "   ~
                               then goto editpg2
L10545:         gosub'153(linenr%,fieldnr%)/*Edit Field for Valid Entry*/
                      if errormsg$ <> " " then goto L10470
            next fieldnr%
            if cnt% = dim(dtl_rec$(), 1) then goto editpg2
                cnt% = cnt% + 1% : dtl% = cnt%
                put dtl_rec$(cnt%) using L16130, "   ", dtl_des$(linenr%),~
                     dtl_fix(linenr%), dtl_per(linenr%),                 ~
                     dtl_bkt%(linenr%), dtl_ids$(linenr%)
            next linenr%
            goto inputmode3_continue

        inputmode4
            gosub pf4_off : gosub pf5_off : gosub pf14_off : gosub pf9_off
            pf16$ = "(16)Return" : str(keytab$, 16, 1) = hex(10)
            gosub pf2_off : gosub pf3_off : gosub pf6_off : gosub pf8_off
            gosub pf11_12_off : gosub pf25_off : gosub pf10_off
            gosub pf7_off

            beg$, end$, beg_desc$, end_part$, end_desc$, errormsg$ = " "
            sum_detl$ = "S" : sum_desc$ = " " : allparts$ = "N"
            beg_part$ = "ALL"
            printsw% = 1%

            for fieldnr% = 1% to 4%
L10665:         gosub'054(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% =  0% then goto L10735
                gosub pf4_off : if fieldnr% = 1 then L10685
                     gosub pf4_on : pf4$ = "(4)Previous Field"
L10685:         gosub'104(fieldnr%)     /* Display & Accept Screen  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then goto L10725
L10700:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'054(fieldnr%)
                         if enabled% = 1% then goto L10665
                         if fieldnr% = 1% then goto L10665
                         goto L10700
L10725:               if keyhit% =  16% then goto inputmode1
                      if keyhit% <>  0% then goto L10665
L10735:         gosub'154(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10665
            next fieldnr%
            goto editpg4

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg2
            errormsg$ = " "
        editpg2a
            gosub pf11_12_off : gosub pf14_off
            gosub pf2_off : gosub pf3_off : gosub pf25_on  : gosub pf4_off
            gosub pf8_on  : gosub pf10_on : gosub pf9_off  : gosub pf5_off
            gosub pf7_off : if hny_rte$ <> " " then gosub pf7_on
            gosub pf6_off : if hny_bom$  = " " then goto L11070
                gosub pf6_on : pf6$ = "(6)Show BOM"

L11070:     if nfgflag$ = "F" then goto editpg2_frozen
                pf12$ = "(12)Delete Part" : str(keytab$, 12, 1) = hex(0c)
L11080:         pf16$ = "(16)Save Data"   : str(keytab$, 16, 1) = hex(10)
                pf2$  = "(17)ReCalc This Level"
                str(keytab$, 2, 1) = hex(11)
                goto L11105
        editpg2_frozen
            if pf17sw% = 1% then goto L11080
                pf8$ = "(8)Show Details" : pf25$ = "(25)Display Text"
                pf16$ = "(16)Return"     : str(keytab$, 16, 1) = hex(10)
L11105:     inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            editmode% = 1%
            gosub'102(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  6% then                                 ~
                     call "BOMXPLOD" (partnbr$, hny_bom$, #1, #2)
                  if keyhit%  =  7% then                                 ~
                     call "RTEDSPLY" (partnbr$, hny_rte$, #10, #1)
                  if keyhit%  =  8% then goto editpg3
                  if keyhit%  = 10% then gosub matrix_display
                  if keyhit%  = 12% then gosub delete_part
                  if keyhit%  = 17% then gosub roll_up_part
                  if keyhit% <> 16% then goto L11165
                     if nfgflag$ <> "F" then goto datasave
                     if pf17sw% = 1% then goto datasave_check
                     goto inputmode1
L11165:           if keyhit%  = 25% then gosub manage_text
                  if keyhit% <>  0% then goto editpg2
            if nfgflag$ = "F" and pf17sw% = 0% then goto inputmode1
            on cursor%(1) goto editpg2, editpg2, editpg2, editpg2, L11200,~
                editpg2, editpg2, L11210, L11210, L11210, L11210, L11210,     ~
                L11210, L11210, L11210, L11210, L11210, L11210, L11210
            goto editpg2
L11200:     lo% = 1% : hi% =  5%
            if cursor%(2) < 2 then goto L11230
            lo% = val(str(edttran$,cursor%(2),1)) : hi% = lo% : goto L11230
L11210:     gosub test_detail_cost
            if u3% <> 0% then goto editpg3
            lo% = cursor%(1) - 2% : hi% = lo%
            if lo% > buckets% + 5% then goto editpg2
L11230:     for fieldnr% = lo% to hi%
                if fieldnr% < 1% or fieldnr% > 17% then goto editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then L11290
            pf16$ = " " : str(keytab$, 16, 1) = hex(ff)
            gosub pf4_off : gosub pf5_off : gosub pf6_off : gosub pf7_off
            gosub pf8_off : gosub pf10_off : gosub pf11_12_off
            gosub pf25_off: gosub pf2_off
L11265:     gosub'102(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then goto L11265
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then goto L11265
L11290:     next fieldnr%
            goto editpg2

        editpg3
            cnt% = 1%
            descmsg$ = " "
        editpg3_continue
            if cnt% = 1% then gosub pf2_off else gosub pf2_on
            if dtl% > 12% and cnt% < ((dtl% / 12%) * 12%) +1% then L11335
                gosub pf3_off : gosub pf5_off : goto L11340
L11335:     gosub pf3_on : gosub pf5_on
L11340:     if cnt% > 12% then gosub pf4_on else gosub pf4_off
            gosub pf8_off
            gosub pf6_off : gosub pf7_off : gosub pf10_off : gosub pf9_off
            gosub pf11_12_on : gosub pf25_on : gosub pf14_off
            pf16$ = "(16)Part Summary" : str(keytab$, 16, 1) = hex(10)
            if nfgflag$ = "F" then gosub pf11_12_off

            gosub detail_extractor
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'103(0%, 0%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  2% then goto L11410
                     cnt% = 1% : goto editpg3_continue
L11410:           if keyhit% <>  3% then goto L11425
                     cnt% = ((dtl% / 12%) * 12%) + 1%
                     goto editpg3_continue
L11425:           if keyhit% <>  4% then goto L11440
                     cnt% = max(1%, cnt% - 12%)
                     goto editpg3_continue
L11440:           if keyhit% <>  5% then goto L11455
                     cnt% = min(((dtl% / 12%) * 12%) + 1%, cnt% + 12%)
                     goto editpg3_continue
L11455:           if keyhit%  = 11% then goto insert_detail
                  if keyhit%  = 12% then goto delete_detail
                  if keyhit%  = 16% then goto editpg2
                  if keyhit%  = 25% then gosub manage_text
                  if keyhit% <>  0% then goto editpg3
                if nfgflag$ = "F" and pf17sw% = 0% then goto inputmode1
                gosub test_this_level
                if u3% <> 1% then goto editpg2
                gosub get_line_number
        editpg3_capture
            for fieldnr% = 1% to 4%
            gosub'053(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then goto L11625
                  pf16$ = " " : str(keytab$, 16, 1) = hex(ff)
                  gosub pf3_off : gosub pf4_off : gosub pf5_off
                  gosub pf8_off : gosub pf25_off : gosub pf11_12_off
                  gosub pf2_on
                  if fieldnr% > 1% then pf2$ = "(2)Line Over"            ~
                     else pf2$ = "(2)Cancel Line"
L11555:     gosub'103(linenr%, fieldnr%)    /* Display & Accept Screen */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  2% then goto L11610
                     if fieldnr% > 1% then goto editpg3_capture
                       if str(dtl_rec$(d%), 1, 3) <> " "                 ~
                          then goto editpg3_continue
                          for i% = d% to dtl%
                               dtl_rec$(i%) = dtl_rec$(i% + 1%)
                          next i%
                          dtl% = dtl% - bumper%
                          goto editpg3_continue
L11610:           if keyhit% <>  0 then goto L11555
            gosub'153(linenr%, fieldnr%)/* Edit Field for Valid Entry */
                  if errormsg$ <> " " then goto L11555
L11625:     next fieldnr%
            put dtl_rec$(d%) using L16130, dtl_seq$(linenr%),             ~
                dtl_des$(linenr%), dtl_fix(linenr%), dtl_per(linenr%),   ~
                dtl_bkt%(linenr%), dtl_ids$(linenr%)
            goto editpg3_continue

        insert_detail
            gosub test_this_level
            if u3% <> 1% then goto editpg2
            if dtl% = dim(dtl_rec$(), 1) then goto editpg3_continue
            gosub get_line_number
            if d% >= dtl% then goto L11710
                for i% = dtl% to d% step - 1%
                     dtl_rec$(i% + 1%) = dtl_rec$(i%)
                next i%
                dtl_rec$(d%) = " " : dtl% = dtl% + 1%
                dtl_fix(linenr%), dtl_per(linenr%) = 0
L11710:     gosub detail_extractor
            goto editpg3_capture

        delete_detail
            linenr% = cursor%(1) - 7%
            if linenr% < 1% or linenr% > 12% then goto editpg3_continue
            d% = cnt% + linenr% - 1%
            if d% > dtl% then goto editpg3_continue
L11750:         u3% = 2%
                call "ASKUSER" (u3%, "*** CONFIRM DETAIL DELETION ***",  ~
                     "Press (RETURN) to DELETE detail cost line # " &    ~
                          dtl_seq$(linenr%) & ":",                       ~
                     dtl_des$(linenr%) & " " & dtl_fix$(linenr%) & " " & ~
                          dtl_per$(linenr%) & " " &                      ~
                          str(dtl_ids$(linenr%)),                        ~
                     "-- OR --    Press PF(1) to ABORT Deletion")
                if u3% = 1% then goto editpg3_continue
                if u3% <> 0% then goto L11750
                tot_hny = tot_hny - tot_tlv
                gosub'200(dtl_fix(linenr%), dtl_per(linenr%),            ~
                     dtl_bkt%(linenr%))
                tot_hny = tot_hny + tot_tlv
                call "CONVERT" (tot_hny, 4.4, tot_hny$)
                for i% = d% to dtl%
                     dtl_rec$(i%) = dtl_rec$(i% + 1%)
                next i%
                dtl% = dtl% - 1%
                dtlsw% = 1%
                goto editpg3_continue

        editpg4
            gosub pf4_off : gosub pf5_off : gosub pf10_off : gosub pf9_off
            gosub pf6_off : gosub pf7_off
            pf16$ = "(16)Print List" : str(keytab$, 16, 1) = hex(10)
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'104(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then goto print_listings
                  if keyhit% <>  0% then goto editpg4
L11910:     fieldnr% = cursor%(1) - 5%
            if fieldnr% < 1% or fieldnr% >  4% then goto editpg4
            if fieldnr% = lastfieldnr% then goto editpg4
            gosub'054(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then goto editpg4
                  pf16$ = " " : str(keytab$, 16, 1) = hex(ff)
L11940:     gosub'104(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then goto L11940
            gosub'154(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then goto L11940
                  lastfieldnr% = fieldnr%
            goto L11910

        REM *************************************************************~
            *       L I S T I N G   P R I N T   R O U T I N E           *~
            *************************************************************

        print_listings
            call "SHOSTAT" ("Standard Cost Part listing in process   " & ~
                "... Please stand by")
            page_nbr% = 0% : line_nbr% = 99%
            select printer(134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            time$ = " " : call "TIME" (time$)

        REM PRINT 'PAGE ZERO' -- THE SELECTION SCREEN *******************
            pagesw% = 1%
            gosub page_0_heading
            print skip (4)
L13152:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then goto L13160
                str(i$(), i%, 1%) = " "
                goto L13152
L13160:     print using L60370, "    ---- SELECTION CRITERIA ----"
            print
            for n% = 6% to 19%
                print using L60370, i$(n%)
            next n%
            hny%, pagesw% = 0%

        report_main_loop
            prt_typ$ = " "
            if allparts$ <> "Y" then goto read_stchny_loop
            call "PLOWNEXT" (#1, beg$, 0%, f1%(1))
            if f1%(1) = 0% then goto print_done
            if beg$ > end$ then goto print_done
            get #1 using L13300, partnbr$, partdesc$, prt_typ$
L13300:         FMT /*File #1- HNYMASTR*/ CH(25), CH(32), POS(180), CH(3)
            init ("*") hny_bom$, hny_rte$, hny_srq$, hny_map$, hny_mat$
            init (" ") tot_bom$, tot_rte$, tot_tlv$, tot_hny$, mdmc$
            call "READ100" (#3, beg$, f1%(3))
            goto report_continue_1

        read_stchny_loop
            call "PLOWNEXT" (#3, beg$, 0%, f1%(3))
            if f1%(3) = 0% then goto print_done
            if beg$ > end$ then goto print_done
            prt_typ$ = "***"
            call "READ100" (#1, beg$, f1%(1))
            if f1%(1) <> 0% then get #1 using L13420, prt_typ$
L13420:         FMT /*File #1- HNYMASTR*/ POS(180), CH(3)

        report_continue_1
            if f1%(3) <> 0% then gosub L29000
            colsw% = 0% : one%, printsw% = 1%
            if f1%(3) <> 0% then gosub dataload
            call "DESCRIBE" (#1, partnbr$, partdesc$, 0%, f1%(1))
            if line_nbr% < 58% then goto L13500
                gosub page_heading
                hny% = 0%
L13500:     if sum_detl$ <> "D" then goto L13570
                if hny% = 1% then goto L13570
                     gosub print_part_columns
                     hny% = 1%
L13570:     gosub print_part_line
            if sum_detl$ = "S" or f1%(3) = 0% then                       ~
                goto report_main_loop
            hny% = 0%

        REM Print standard cost detail records, if any ******************
            plowkey$ = str(partnbr$,,25) & hex(00)
            onetime% = 0%
            colsw% = 1%
        read_stcdetal_loop
            call "PLOWNEXT" (#4, plowkey$, 25%, f1%(4))
            if f1%(4) = 0% then goto print_stcdetal_totals
            get #4 using L13700, dtlseq$, dtldes$, fix, per, bkt%

L13700:         FMT  /* File STCDETAL (STCnnnnD)                       */~
                     POS(26), CH(3), CH(40), PD(14,4), PD(14,4), BI(1)

            if str(dtlseq$,1,1) <> "0" then goto L13770
                str(dtlseq$,1,1) = " "
                if str(dtlseq$,2,1) <> "0" then goto L13770
                     str(dtlseq$,2,1) = " "
L13770:     tot_per = tot_per + per
            call "CONVERT" (per, 4.4, per$)
            if hny_srq <> 0 then tot_fix = tot_fix + round(fix/hny_srq,4)
            call "CONVERT" (fix, 4.4, fix$)
            convert bkt% to bkt$, pic (##)
            if onetime% <> 0% then goto L13860
                if line_nbr% > 57% then gosub page_heading else          ~
                     gosub print_detail_columns
                onetime% = 1%
L13860:     if line_nbr% > 59% then gosub page_heading
            print using L60270, dtlseq$, dtldes$, fix$, per$,             ~
                bkt_ids$(bkt%), bkt$
            line_nbr% = line_nbr% + 1%
            goto read_stcdetal_loop

        print_stcdetal_totals
            onetime% = 0%
            tot_dtl = tot_per + tot_fix
            if tot_dtl = 0 then goto print_stchny_buckets
            if line_nbr% > 59% then gosub page_heading
            call "CONVERT" (tot_fix, 4.4, tot_fix$)
            call "CONVERT" (tot_per, 4.4, tot_per$)
            call "CONVERT" (tot_dtl, 4.4, tot_dtl$)
            print using L60270, "***", "                      TOTALS: " & ~
                tot_dtl$, tot_fix$, tot_per$
            line_nbr% = line_nbr% + 1%

        print_stchny_buckets
            colsw% = 2%
            for i% = 1% to buckets%
                convert i% to bkt$, pic (##.)
                if onetime% <> 0% then goto L14120
                     if line_nbr% > 57% then gosub page_heading else     ~
                          gosub print_bucket_columns
                     onetime% = 1%
L14120:         if line_nbr% > 59% then gosub page_heading
                print using L60350, bkt$, bkt_ids$(i%), bkt_des$(i%),     ~
                     hny_bom$(i%), hny_rte$(i%), hny_tlv$(i%)
                line_nbr% = line_nbr% + 1%
            next i%
            if line_nbr% > 59% then gosub page_heading
            mdmc_print$ = " "
            if mgtrpt_on$ = "Y" then mdmc_print$ = "MDMC: " & mdmc$
            print using L60350, "***", " ", "TOTALS: " & tot_hny$,        ~
                tot_bom$, tot_rte$, tot_tlv$, mdmc_print$
            print
            line_nbr% = line_nbr% + 2%
            goto report_main_loop

        print_done
            print
            print using L60390                        /* END OF REPORT */
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            goto inputmode4

        page_heading
            page_nbr% = page_nbr% + 1%  :  line_nbr% = 4%
        page_0_heading
            print page
            print using L60040, date$, time$, compname$, "-" & rptid$
            print using L60070, page_nbr%
            print using L60100, subhdr$
            if pagesw% <> 0% then return
            print
            if sum_detl$ = "D" then goto L14430
                gosub print_part_columns
                return
L14430:     on colsw% goto L14460, L14520
            return

L14460: REM Re-print the detail cost column headers *********************
            gosub print_part_columns
            gosub print_part_line
            gosub print_detail_columns
            return

L14520: REM Re-print the STCHNY cost column headers *********************
            gosub print_part_columns
            gosub print_part_line
            gosub print_bucket_columns
            return

        REM *************************************************************~
            *    M I S C E L L A N E O U S   S U B R O U T I N E S      *~
            *************************************************************

        print_part_columns
            print using L60120
            print using L60150
            line_nbr% = line_nbr% + 2%
            return

        print_part_line
            print using L60180, partnbr$, partdesc$, prt_typ$, hny_bom$,  ~
                hny_rte$, hny_srq$, str(hny_map$) & str(tot_bom$,2,11) & ~
                str(tot_rte$,2,11) & str(tot_tlv$,2,11) &                ~
                str(tot_hny$,2,11)
            line_nbr% = line_nbr% + 1%
            return

        print_detail_columns
            print
            print using L60210
            print using L60230, "#"
            print using L60250
            line_nbr% = line_nbr% + 4%
            return

        print_bucket_columns
            print
            print using L60290
            print using L60310, "#"
            print using L60330
            line_nbr% = line_nbr% + 4%
            return

        pf2_off
            pf2$ = " " : str(keytab$, 2, 1) = hex(ff) : return
        pf2_on
            pf2$ = "(2)First" : str(keytab$, 2, 1) = hex(02) : return

        pf3_off
            pf3$ = " " : str(keytab$, 3, 1) = hex(ff) : return
        pf3_on
            pf3$ = "(3)Last" : str(keytab$, 3, 1) = hex(03) : return

        pf4_off
            pf4$ = " " : str(keytab$, 4, 1) = hex(ff) : return
        pf4_on
            pf4$ = "(4)Prev" : str(keytab$, 4, 1) = hex(04) : return

        pf5_off
            pf5$ = " " : str(keytab$, 5, 1) = hex(ff) : return
        pf5_on
            pf5$ = "(5)Next" : str(keytab$, 5, 1) = hex(05) : return

        pf6_off
            pf6$ = " " : str(keytab$, 6, 1) = hex(ff) : return
        pf6_on
            pf6$ = " " : str(keytab$, 6, 1) = hex(06) : return

        pf7_off
            pf7$ = " " : str(keytab$, 7, 1) = hex(ff) : return
        pf7_on
            pf7$ = "(7)Show Route" : str(keytab$, 7, 1) = hex(07) : return

        pf8_off
            pf8$ = " " : str(keytab$, 8, 1) = hex(ff) : return
        pf8_on
            pf8$ = "(8)Edit Details" : str(keytab$, 8, 1) = hex(08)
            return

        pf9_off
            pf9$ = " " : str(keytab$, 9, 1) = hex(ff) : return
        pf9_on
            pf9$ = "(9)Select from Parts in this Cost Set"
            str(keytab$, 9, 1) = hex(09) : return

        pf10_off
            pf10$ = " " : str(keytab$,10, 1) = hex(ff) : return
        pf10_on
            pf10$ = "(10)Cost Matrix" : str(keytab$,10, 1) = hex(0a)
            return

        pf11_12_off
            pf11$, pf12$ = " " : str(keytab$,11, 2) = hex(ffff) : return
        pf11_12_on
            pf11$ = "(11)Insert/Add" : pf12$ = "(12)Delete Line"
            str(keytab$,11, 2) = hex(0b0c) : return

        pf14_off
            pf14$ = " " : str(keytab$,14, 1) = hex(ff) : return
        pf14_on
            pf14$ = "(14)Print Listings" : str(keytab$,14, 1) = hex(0e)
            return

        pf25_off
            pf25$ = " " : str(keytab$,25, 1) = hex(ff) : return
        pf25_on
            pf25$ = "(25)Manage Text" : str(keytab$,25, 1) = hex(19)
            return

        detail_extractor
            m% = 0%
            init (" ") dtl_seq$(), dtl_des$(), dtl_fix$(), dtl_per$(),   ~
                dtl_ids$()
            mat dtl_fix = zer : mat dtl_per = zer : mat dtl_bkt% = zer
            for i% = cnt% to min(dim(dtl_rec$(), 1), cnt% + 11%)
                m% = m% + 1%
                if dtl_rec$(i%) = " " then L16180
                get dtl_rec$(i%) using L16130, dtl_seq$(m%), dtl_des$(m%),~
                     dtl_fix(m%), dtl_per(m%), dtl_bkt%(m%), dtl_ids$(m%)

L16130:         FMT  /* Format of Array DTL_REC$()                     */~
                     CH(3), CH(40), PD(14,4), PD(14,4), BI(1), CH(10)

                call "CONVERT" (dtl_fix(m%), 4.4, dtl_fix$(m%))
                call "CONVERT" (dtl_per(m%), 4.4, dtl_per$(m%))
L16180:     next i%
            return

        get_line_number
            bumper% = 0%
            linenr% = cursor%(1) - 7%
            if linenr% < 1% or linenr% > 12% then L16320
                d% = cnt% + linenr% - 1%
                if d% <= dtl% then return
                     bumper% = 1%
                     linenr% = linenr% - (d% - dtl% - 1%)
                     dtl% = dtl% + bumper%
                     d% = dtl%
                     return
L16320:     return clear all
            goto editpg3_continue

        manage_text
            if nfgflag$ = "F" and pf17sw% <> 1% then goto display_text
            call "TXTINSUB" (#9, f2%(9), "023",                          ~
                "Cost Set: " & costset$ & ", Part: " & partnbr$ & " " &  ~
                partdesc$, textid$, text$())
            return
        display_text
            call "TXTDSPLY" (#9, f2%(9), "023",                          ~
                "Cost Set: " & costset$ & ", Part: " & partnbr$ & " " &  ~
                partdesc$, textid$, text$())
            return

        kill_detail_costs
            plowkey$ = str(partnbr$,,25) & hex(000000)
            call "DELETE" (#4, plowkey$, 25%)
            dtlsw%, dtl% = 0%
            return

        get_bom_cross_ref
            plowkey$ = str(costset$,,8) & str(partnbr$,,25)
            call "READ101" (#6, plowkey$, f1%(6))
            return

        test_detail_cost
            u3% = 0%
            if dtl% < 1% then return
            if tot_dtl = 0 then return
            goto cost_mode_conflict
        test_this_level
            u3% = 1%
            if dtl% > 0% then return
            if tot_tlv = 0 then return
        cost_mode_conflict
L16620:     u3% = 2%
            call "ASKUSER" (u3%, "*** COST MODE CONFLICT ***",           ~
                "Costs must be -EITHER- 'This Level' -OR- the sum of " & ~
                     "the Details:",                                     ~
                "Press (RETURN) to use 'This Level' costs (and zero/DEL"&~
                     "ETE the Details)",                                 ~
                "-- OR --    Press PF(1) to enter/use Detail cost " &    ~
                     "records")
            if u3% <> 1% then goto L16730
                gosub clear_this_level
                return
L16730:     if u3% <> 0% then goto L16620
            gosub clear_detail_cost
            return

        delete_part
            u3% = 2%
            call "ASKUSER" (u3%, "*** CONFIRM PART DELETION ***",        ~
                "Press (RETURN) to DELETE Standard Costs, Detail costs"& ~
                     " and BOM X-ref for:",                              ~
                "Part: " & partnbr$ & " " & str(partdesc$),              ~
                "-- OR --      Press PF(1) to ABORT delete")
            if u3% =  1% then return
            if u3% <> 0% then goto delete_part
            call "READ101" (#3, partnbr$, f1%(3))
            if f1%(3) <> 0% then delete #3
            gosub kill_detail_costs
            gosub get_bom_cross_ref
            if f1%(6) <> 0% then delete #6
            call "TXTFUTIL" (#9, f2%(9), "DELE", textid$)
            plowkey$ = "H" & partnbr$
            call "READ101" (#5, plowkey$, f1%(5))
            if f1%(5) <> 0% then delete #5
            return clear all
            goto inputmode1

        deffn'200(f, p, b%)
            by1% = -1%
            goto compute_detail
        deffn'201(f, p, b%)
            by1% = 1%
        compute_detail
            tot_per = tot_per + (p * by1%)
            tot_dtl = tot_dtl + (p * by1%)
            if b% = 0% then L17090
                hny_tlv(b%) = hny_tlv(b%) + (p * by1%)
                tot_tlv = tot_tlv + (p * by1%)
L17090:     if hny_srq = 0 then goto L17160
                temp = round((f / hny_srq) * by1%, 4%)
                tot_fix = tot_fix + temp
                tot_dtl = tot_dtl + temp
                if b% = 0% then L17160
                     hny_tlv(b%) = hny_tlv(b%) + temp
                     tot_tlv = tot_tlv + temp
L17160:     call "CONVERT" (tot_dtl, 4.4, tot_dtl$)
            call "CONVERT" (tot_fix, 4.4, tot_fix$)
            call "CONVERT" (tot_per, 4.4, tot_per$)
            call "CONVERT" (tot_tlv, 4.4, tot_tlv$)
            if b% = 0% then return
                call "CONVERT" (hny_tlv(b%), 4.4, hny_tlv$(b%))
                return

        clear_this_level
            tot_hny = tot_hny - tot_tlv
            mat hny_tlv = zer
            tot_tlv = 0
            for i% = 1% to buckets%
                call "CONVERT" (hny_tlv(i%), 4.4, hny_tlv$(i%))
            next i%
            call "CONVERT" (tot_tlv, 4.4, tot_tlv$)
            if dtl% < 1% then L17420
            call "SHOSTAT" ("Re-calculating Detail Cost amounts ... " &  ~
                "Please stand by")
            tot_fix, tot_per, tot_dtl = 0
            for i% = 1% to dtl%
                get dtl_rec$(i%) using L17380, fix, per, bkt%
L17380:              FMT /*DTL_REC$()*/ POS(44), PD(14,4), PD(14,4), BI(1)
                gosub'201(fix, per, bkt%)
            next i%
            tot_hny = tot_hny + tot_tlv
L17420:     call "CONVERT" (tot_hny, 4.4, tot_hny$)
            return

        clear_detail_cost
            init (" ") dtl_rec$()
            tot_dtl, tot_fix, tot_per, cnt%, dtl% = 0
            mat dtl_fix = zer : mat dtl_per = zer : mat dtl_bkt% = zer
            call "CONVERT" (tot_fix, 4.4, tot_fix$)
            call "CONVERT" (tot_per, 4.4, tot_per$)
            call "CONVERT" (tot_dtl, 4.4, tot_dtl$)
            gosub kill_detail_costs
            return

        matrix_display
            rup, rub, rte, tlv, fib, fit = 0
            mat roll_up = zer : mat fold_in = zer : mat fin_bom = zer
            mat map% = zer
            init (hex(8c)) mfac$(), mfac1$(), mfac2$(), mfac3$(),        ~
                mfac4$(), mfac5$()
            init (" ") inpmessage$, rup$, rub$, rte$, tlv$, fib$, fit$,  ~
                roll_up$(), fold_in$(), fin_bom$(), rup_bom$(),          ~
                rup_rte$(), rup_tlv$()

            inpmessage$ = "Position Cursor and Press (RETURN) to see Tota~
        ~l Derivation."

            call "READ100" (#7, hny_map$, f1%(7))
            if f1%(7) = 1% then get #7 using L17700, map%()
L17700:         FMT  /* File #7- STCMAPNG (STCnnnnM) */ POS(39), 12*BI(1)
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
                if n% > buckets% then goto L17940
                     call "CONVERT" (roll_up(n%), 4.4,                   ~
                          str(roll_up$(n%),,10))
                     call "CONVERT" (fold_in(n%), 4.4,                   ~
                                                    str(fold_in$(n%),2%))
                     call "CONVERT" (fin_bom(n%), 4.4, fin_bom$(n%))
                     call "CONVERT" (hny_bom(n%), 4.4, rup_bom$(n%))
                     call "CONVERT" (hny_rte(n%), 4.4, rup_rte$(n%))
                     call "CONVERT" (hny_tlv(n%), 4.4, rup_tlv$(n%))
L17940:         str(roll_up$(n%),12,1) = "!"
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
            str(pg5_l7$,11,1) = mfac$(1) or hex(20)
            str(pg5_l7$,22,1) = mfac$(1)
            str(pg5_l7$,24,1) = mfac$(2) or hex(20)
            str(pg5_l7$,35,1) = mfac$(3) or hex(20)
            str(pg5_l7$,46,1) = mfac$(3) or hex(20)
            str(pg5_l7$,57,1) = mfac$(4) or hex(20)
            str(pg5_l7$,68,1) = mfac$(5) or hex(20)
            gosub'105
            if u3% =   0% then gosub hilite_map
            if u3% =   1% then gosub startover
            if u3% =   8% then gosub pf8_toggler
            if u3% <> 16% then display_the_matrix
            return

        pf8_toggler
            on toggle% goto L18250, L18310
L18250:     init (hex(84)) mfac$(1), mfac$(2), mfac$(3), mfac1$(),       ~
                mfac2$(), mfac3$()
            init (hex(8c)) mfac$(4), mfac$(5), mfac4$(), mfac5$()
            pf8$ = "(8)HiLite Fold-in"
            toggle% = 2%
            return
L18310:     init (hex(84)) mfac$(3), mfac$(4), mfac$(5), mfac3$(),       ~
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
            on toggle% goto L18490, L18450          /* FOLD-IN, ROLL-UP */
L18450:     mfac1$(cursor%(1%)), mfac2$(cursor%(1%)),                    ~
                mfac3$(cursor%(1%)) = hex(84)
            return

L18490:     mfac3$(cursor%(1%)), mfac5$(cursor%(1%)) = hex(84)
            mfac$(4) = hex(8c) : mfac$(2) = hex(84)
            for n% = 1% to 12%
                if map%(n%) = cursor%(1%) then mfac2$(n%) = hex(84)
            next n%
            return

        reset_toggle
            toggle% = 1% + mod(toggle%, 2%)
            goto pf8_toggler

        roll_up_part
            call "STCSPROL" (partnbr$, hny_bom$, hny_rte$, hny_srq,      ~
                             hny_bom(), hny_rte(),                       ~
                             #2,          /* BOMMASTR                  */~
                             #3,          /* STCHNY                    */~
                             #11,         /* STCWCACT                  */~
                             #12,         /* STCLABR                   */~
                             #10,         /* RTEMASTR                  */~
                             #13,         /* WCMASTR                   */~
                             erte%,       /* Ret Code for RTE Error    */~
                             ebom%)       /* Ret Code for BOM Error    */

            if erte% = 0% then L18830
               errormsg$ = "Cost Roll-Up Failed. Check Route ID " &      ~
                           hny_rte$ & " for Part " & partnbr$
               goto editpg2a
L18830:     if ebom% = 0% then L18850
               errormsg$ = "Cost Roll-Up Failed. Check BOM ID "   &      ~
                           hny_bom$ & " for Part " & partnbr$
               goto editpg2a

L18850:     mdmc = -1 : if mgtrpt_on$ <> "Y" then L18865
            mat mdmc_totals = hny_bom
            mat mdmc_totals = mdmc_totals + hny_rte
            mat mdmc_totals = mdmc_totals + hny_tlv
            call "STCMDMC" (partnbr$, costset$, mdmc_totals(), mdmc,     ~
                            #1, #17, #8)
L18865:     rol_bom$ = hny_bom$:rol_rte$ = hny_rte$:rol_srq = hny_srq
            tot_bom, tot_rte = 0
            gosub data_edit
            goto editpg2

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave_check
           if hny_bom$ <> rol_bom$ then L19047
           if hny_rte$ <> rol_rte$ then L19047
           if hny_srq   = rol_srq  then L19055

L19047:         u3% = 2%
                call "ASKUSER" (u3%, "*** ROLL UP REQUIRED? ***",        ~
                "A roll up is required to be consistent with BOM data",  ~
                "Press PF(16) to add above part WITHOUT roll.",          ~
                "-- OR --      Press PF(1) to RETURN to EDIT")
                if u3% =   1% then goto editpg2
                if u3% <> 16% then goto L19047

L19055:    if tot_hny = 0 or whereused% = 0% then datasave

L19057:         u3% = 2%
                call "ASKUSER" (u3%, "*** CONSISTENCY WARNING ***",      ~
                "Assemblies using this part will not reflect its costs", ~
                "Press PF(16) to CONTINUE ADD",                          ~
                "-- OR --      Press PF(1) to RETURN to EDIT")
                if u3% =   1% then goto editpg2
                if u3% <> 16% then goto L19057

        datasave
            if dtlsw% = 0% then goto stchny_save
                gosub kill_detail_costs
                mat hny_tlv = zer : hnysw% = 1%
                for i% = 1% to dim(dtl_rec$(), 1)
                     if dtl_rec$(i%) = " " then goto stchny_save
                     get dtl_rec$(i%) using L17380, fix, per, bkt%
                     hny_tlv(bkt%) = hny_tlv(bkt%) + per
                     if hny_srq = 0 then goto L19170
                          hny_tlv(bkt%) = hny_tlv(bkt%) +                ~
                               round((fix / hny_srq), 4%)
L19170:              convert i% to seq$, pic (000)
                     put #4 using L19210, partnbr$, seq$,                 ~
                          str(dtl_rec$(i%), 4, 57), " "

L19210:              FMT  /*  File #4- STCDETAL (STCnnnnD)             */~
                     CH(25), CH(3), CH(57), CH(255)

                     write #4
                next i%

        stchny_save
            call "PACKZERO" (hny_bom(), cost$(1))
            call "PACKZERO" (hny_rte(), cost$(2))
            call "PACKZERO" (hny_tlv(), cost$(3))
            call "READ101" (#3, partnbr$, f1%(3))

            put #3 using L19480, partnbr$, hny_map$, textid$, hny_bom$,   ~
                hny_rte$, hny_srq, tot_hny, cost$(1), cost$(2), cost$(3),~
                rol_bom$, rol_rte$, rol_srq, hny_mat$, mdmc, " "

L19480:         FMT      /*  File #3- STCHNY (STCnnnnH)                */~
                CH(25), CH(8), CH(4), CH(3), CH(3), PD(14,4), PD(14,4),  ~
                CH(96), CH(96), CH(96), CH(3), CH(3), PD(14,4), CH(2),   ~
                PD(14,4), CH(129)

            if f1%(3) = 0% then write #3 else rewrite #3

            gosub get_bom_cross_ref
            if hny_bom$ <> " " or hny_rte$ <> " " then L19540
               if f1%(6) <> 0% then delete #6
               goto text_file_save
L19540:     put #6 using L19570, partnbr$, hny_bom$, costset$, partnbr$,  ~
                hny_rte$, costset$

L19570:         FMT  /*  File #6- STCBOMXF                             */~
                CH(25), CH(3), CH(8), CH(25), CH(3), CH(8)

            if f1%(6) = 0% then write #6 else rewrite #6

        text_file_save
            call "TXTFUTIL" (#9, f2%(9), "TOS2", textid$)
            if nfgflag$ = "Y" then goto inputmode1
            if nfgflag$ = "F" then goto L19640
            if hnysw% + dtlsw% = 0% then goto inputmode1
L19640:         put #5 using L19660, "H", partnbr$

L19660:         FMT  /* File #5- STCCHNGS ( STCnnnnC) */ CH(1), CH(25)

                write #5, eod goto inputmode1
                goto inputmode1

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%

        REM Def/Enable Part No.                    PARTNBR$
            inpmessage$ = "Enter Part # or partial to see available "   &~
                "codes.  Then press (RETURN) or PF(9)."
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L21260,         /* Bill of Materials  */    ~
                              L21450,         /* Route              */    ~
                              L21310,         /* Standard Run Qty   */    ~
                              L21350,         /* Mapping to Use     */    ~
                              L21500,         /* Default Mater Bkt  */    ~
                              L21400,         /* This Level bckt  1 */    ~
                              L21400,         /* This Level bckt  2 */    ~
                              L21400,         /* This Level bckt  3 */    ~
                              L21400,         /* This Level bckt  4 */    ~
                              L21400,         /* This Level bckt  5 */    ~
                              L21400,         /* This Level bckt  6 */    ~
                              L21400,         /* This Level bckt  7 */    ~
                              L21400,         /* This Level bckt  8 */    ~
                              L21400,         /* This Level bckt  9 */    ~
                              L21400,         /* This Level bckt 10 */    ~
                              L21400,         /* This Level bckt 11 */    ~
                              L21400          /* This Level bckt 12 */
            return

L21260: REM Def/Enable Bill of Materials           HNY_BOM$
            inpmessage$ = "Enter the Bill of Materials for this part or"&~
                " partial to see list of BOMs"
            return

L21310: REM Def/Enable Standard Run Qty            HNY_SRQ$
            inpmessage$ = "Enter the Standard Run Quantity for this part"
            return

L21350: REM Def/Enable Mapping to Use              HNY_MAP$
            inpmessage$ = "Enter the Map for this part or partial to " & ~
                "to see list of Maps"
            return

L21400: REM Def/Enable This Level                  HNY_TLV$
            inpmessage$ = "Enter the 'This Level' cost amount for this "&~
                "bucket"
            return

L21450: REM Def/Enable Routing                     HNY_RTE$
            if editmode% = 0% then enabled% = 0%
            inpmessage$ = "Enter the Route for this part or partial to "&~
                "see list of ROUTEs"
            return

L21500: REM Def/Enable Default Materials Bucket    HNY_MAT$
            if hny_typ$ = "000" and editmode% = 0% then enabled% = 0%
            if hny_typ$ > "499" and editmode% = 0% then enabled% = 0%
            inpmessage$ = "Enter 'D' to use Cost Set Default, P = Prorate~
        ~, 1-12 for specific bucket Id."
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  3  of Input. *~
            *************************************************************

        deffn'053(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L22150,         /* Std Cost Dtl descr */    ~
                              L22190,         /* Std Cost Dtl fixed$*/    ~
                              L22230,         /* Std Cost Dtl $/part*/    ~
                              L22280          /* Std Cost Dtl bucket*/
            return

L22150: REM Def/Enable Description                 DTL_DES$
            inpmessage$ = "Enter the Standard Cost detail description" & ~
                descmsg$
            return

L22190: REM Def/Enable Fixed $                     DTL_FIX$
            inpmessage$ = "Enter the Standard Cost detail Fixed amount "&~
                "per SRQ of " & hny_srq$
            return

L22230: REM Def/Enable $ per Part                  DTL_PER$
            inpmessage$ = "Enter the Standard Cost detail amount per " & ~
                "part"
            return

L22280: REM Def/Enable Std Cost Bucket             DTL_IDS$
            inpmessage$ = "Enter the Standard Cost detail bucket ID or "&~
                "partial to see list of codes"
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   4     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  4  of Input. *~
            *************************************************************

        deffn'054(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L23140,         /* Beginning Part No  */    ~
                              L23190,         /* Ending Part No.    */    ~
                              L23240,         /* Summary or Detail  */    ~
                              L23290          /* All Parts switch   */
            return

L23140: REM Def/Enable Beginning Part No.          BEG_PART$
            inpmessage$ = "Enter beginning Part #, 'ALL', 'FIRST' or "  &~
                "partial to see list of codes"
            return

L23190: REM Def/Enable Ending Part No.             END_PART$
            inpmessage$ = "Enter ending Part #, 'LAST', or partial "    &~
                "to see list of codes"
            return

L23240: REM Def/Enable Summary or Detail           SUM_DETL$
            inpmessage$ = "Enter 'S' to print Summary listing; 'D' to p"&~
                "rint Detail"
            return

L23290: REM Def/Enable All parts switch            ALLPARTS$
            inpmessage$ = "'Y' = Print all parts in Inventory Master; '"&~
                "N' = Cost Set parts only"
            return

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, line6$, line2$, subhdr$,   ~
                      partnbr$, partdesc$    , /* Part # & descript  */  ~
                      hny_bom$, bomsave$     , /* Bill of Materials  */  ~
                      hny_srq$               , /* Standard Run Qty   */  ~
                      hny_rte$, hny_mat$     , /* Route              */  ~
                      hny_typ$               , /* Part type          */  ~
                      hny_tlv$(), tot_tlv$   , /* This Level costs   */  ~
                      hny_bom$(), tot_bom$   , /* BOM costs          */  ~
                      hny_rte$(), tot_rte$   , /* RTE costs          */  ~
                      rol_bom$, rol_rte$     , /* Roll-up BOM, RTE   */  ~
                      tot_hny$               , /* STCHNY total cost  */  ~
                      mdmc$                  , /* Management DMC     */  ~
                      dtl_seq$()             , /* Line/seq #         */  ~
                      dtl_des$()             , /* Description        */  ~
                      dtl_fix$(), tot_fix$   , /* Fixed $            */  ~
                      dtl_per$(), tot_per$   , /* $ per Part         */  ~
                      dtl_ids$()             , /* Bucket IDs         */  ~
                      dtl_rec$()             , /* Std cost details   */  ~
                      tot_dtl$               , /* STDETAL total cost */  ~
                      textid$, text$()       , /* Text fields        */  ~
                      mdmc$                    /* Management DMC     */
            hny_map$ = mapdflt$

            one% = -1%
            brite$=hex(86):uplow$=hex(80):upper$=hex(81):nmric$=hex(82)
            tot_bom, tot_rte, tot_tlv, tot_hny, tot_fix, tot_per,        ~
                tot_dtl, mdmc = 0
            hny_srq, rol_srq = 1
            pf17sw%, cnt%, dtl%, hnysw%, dtlsw%, printsw%, onetime%,     ~
            whereused% = 0%
            mat hny_bom = zer : mat hny_tlv = zer : mat hny_rte = zer
            mat dtl_fix = zer : mat dtl_per = zer : mat dtl_bkt% = zer
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                " to Desired Value & Press (RETURN)."
            line2$ = "Cost set: " & costset$ & " (" & setdesc$ & ")"
            if nfgflag$ = "Y" then str(line2$, len(line2$) + 1%) =       ~
                ", Global"
            if nfgflag$ = "F" then str(line2$, len(line2$) + 1%) =       ~
                ", Frozen"
            subhdr$ = line2$
            call "FMTTITLE" (subhdr$, " ", 2%)
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
            goto inputmode1

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload
            get #3 using L30110, partnbr$, hny_map$, textid$, hny_bom$,   ~
                hny_rte$, hny_srq, tot_hny, hny_bom(), hny_rte(),        ~
                hny_tlv(), rol_bom$, rol_rte$, rol_srq, hny_mat$, mdmc

L30110:     FMT      /*  File #3- STCHNY (STCnnnnH)                    */~
                CH(25), CH(8), CH(4), CH(3), CH(3), PD(14,4), PD(14,4),  ~
                12*PD(14,4), 12*PD(14,4), 12*PD(14,4), CH(3), CH(3),     ~
                PD(14,4), CH(2), POS(364), PD(14,4)

            if mdmc > 20000000000 then mdmc = 0

            for i% = 1% to 12%
                if i% > buckets% then hny_tlv(i%) = 0
                tot_tlv = tot_tlv + hny_tlv(i%)
            next i%

            if printsw% <> 0% then goto data_edit
            srqsave  = hny_srq
            bomsave$ = hny_bom$
            call "TXTFUTIL" (#9, f2%(9), "LOAD", textid$)
            if rol_bom$ = hny_bom$ then goto L30280
                line6$ = "BOM"
                mat hny_bom = zer
L30280:     if rol_rte$ = hny_rte$ and rol_srq = hny_srq then goto L30320
                if line6$ = " " then line6$ = "RTE" else                 ~
                     str(line6$, len(line6$)+2%) = "and RTE"
                mat hny_rte = zer
L30320:     if line6$ = " " then goto L30360
                str(line6$, len(line6$)+2%) = "costs not shown -- they "&~
                "are out of date pending roll-up"

L30360: REM Now Gather up all Std Cost Detail records, if any ***********
            plowkey$ = str(partnbr$,,25) & hex(000000)
L30380:     call "PLOWNEXT" (#4, plowkey$, 25%, f1%(4))
            if f1%(4) = 0% then goto data_edit
            if dtl% = dim(dtl_rec$(), 1) then goto data_edit
            dtl% = dtl% + 1%
            get #4 using L30430, dtl_rec$(dtl%)
L30430:         FMT  /* File #4- STCDETAL (STCnnnnD)  */ POS(26), CH(60)
            if str(dtl_rec$(dtl%), 1, 1) <> "0" then goto L30480
                str(dtl_rec$(dtl%), 1, 1) = " "
                if str(dtl_rec$(dtl%), 2, 1) <> "0" then goto L30480
                     str(dtl_rec$(dtl%), 2, 1) = " "
L30480:     get dtl_rec$(dtl%) using L30490, fix, per, bkt%
L30490:         FMT /* DTL_REC$() */  POS(44), PD(14,4), PD(14,4), BI(1)
            str(dtl_rec$(dtl%), 61) = bkt_ids$(bkt%)
            if onetime% <> 0% then goto L30550
                mat hny_tlv = zer
                tot_tlv = 0
                onetime% = 1%
L30550:     gosub'201(fix, per, bkt%)
            goto L30380

        data_edit
            for i% = 1% to buckets%
                if rol_bom$ <> hny_bom$ and printsw% = 0% then goto L30630
                     call "CONVERT" (hny_bom(i%), 4.4, hny_bom$(i%))
                     tot_bom = tot_bom + hny_bom(i%)
L30630:         if (rol_rte$ <> hny_rte$ or rol_srq <> hny_srq) and      ~
                     printsw% = 0% then goto L30670
                          call "CONVERT" (hny_rte(i%), 4.4, hny_rte$(i%))
                          tot_rte = tot_rte + hny_rte(i%)
L30670:         call "CONVERT" (hny_tlv(i%), 4.4, hny_tlv$(i%))
            next i%
            tot_hny = tot_bom + tot_rte + tot_tlv
            call "CONVERT" (tot_bom, 4.4, tot_bom$)
            call "CONVERT" (tot_rte, 4.4, tot_rte$)
            call "CONVERT" (tot_tlv, 4.4, tot_tlv$)
            call "CONVERT" (tot_hny, 4.4, tot_hny$)
            call "CONVERT" (hny_srq, 0.2*one%, hny_srq$)
            call "CONVERT" (tot_fix, 4.4, tot_fix$)
            call "CONVERT" (tot_per, 4.4, tot_per$)
            call "CONVERT" (tot_dtl, 4.4, tot_dtl$)
                mdmc$ = " "
                if mgtrpt_on$ <> "Y" then return
                call "CONVERT" (mdmc, 4.4, mdmc$)
                if mdmc > 1000000 then mdmc$ = " "
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              str(line2$,62%) = "STCHNYSB: " & str(cms2v$,,8%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(brite$) lfac$()

                lfac$(fieldnr%) = upper$             /* Upper Only */

L40130:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Part Standard Costs"                  ,        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Part:",                                      ~
               at (04,08), fac(lfac$( 1)), partnbr$             , ch(25),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf6$                           ,~
               at (23,42), fac(hex(8c)), pf14$                          ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,20), fac(hex(84)), pf9$                           ,~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys (keytab$), key (keyhit%)

               if keyhit% <> 13 then L40390
                  call "MANUAL" ("STCHNYSB")
                  goto L40130

L40390:        if keyhit% <> 15 then L40430
                  call "PRNTSCRN"
                  goto L40130

L40430:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%)
              str(line2$,62%) = "STCHNYSB: " & str(cms2v$,,8%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(brite$) lfac$()
              on fieldnr% gosub L41140,         /* Bill of Materials */   ~
                                L41140,         /* Route ID          */   ~
                                L41145,         /* Standard Run Qty  */   ~
                                L41140,         /* Mapping to Use    */   ~
                                L41140,         /* Purchase Bucket   */   ~
                                L41145,         /* This Level  1     */   ~
                                L41145,         /* This Level  2     */   ~
                                L41145,         /* This Level  3     */   ~
                                L41145,         /* This Level  4     */   ~
                                L41145,         /* This Level  5     */   ~
                                L41145,         /* This Level  6     */   ~
                                L41145,         /* This Level  7     */   ~
                                L41145,         /* This Level  8     */   ~
                                L41145,         /* This Level  9     */   ~
                                L41145,         /* This Level 10     */   ~
                                L41145,         /* This Level 11     */   ~
                                L41145          /* This Level 12     */
              goto L41155

                  lfac$(fieldnr%) = uplow$  :  return  /* Up / Low   */
L41140:           lfac$(fieldnr%) = upper$  :  return  /* Upper Only */
L41145:           lfac$(fieldnr%) = nmric$  :  return  /* Numeric    */

L41155:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Part Standard Costs"                  ,        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Part:",                                      ~
               at (04,08), fac(hex(8c))  , partnbr$             , ch(25),~
               at (04,34), fac(hex(8c))  , partdesc$            , ch(34),~
               at (04,71), "Type:",                                      ~
               at (04,77), fac(hex(8c))  , hny_typ$             , ch(03),~
                                                                         ~
               at (05,02), "BOM:",                                       ~
               at (05,07), fac(lfac$( 1)), hny_bom$             , ch(03),~
               at (05,13), "Route:",                                     ~
               at (05,20), fac(lfac$( 2)), hny_rte$             , ch(03),~
               at (05,26), "Std Run Qty:",                               ~
               at (05,39), fac(lfac$( 3)), hny_srq$             , ch(10),~
               at (05,50), "Map:",                                       ~
               at (05,55), fac(lfac$( 4)), hny_map$             , ch(08),~
               at (05,64), "Purchase Bkt:",                              ~
               at (05,78), fac(lfac$( 5)), hny_mat$             , ch(02),~
                                                                         ~
               at (06,02), fac(hex(8c))  , line6$               , ch(79),~
               at (07,02), fac(hex(8c))  , pg2_l7$              , ch(79),~
                                                                         ~
               at (08,02), fac(hex(8c))  , hny_seq$(1)          , ch(03),~
               at (08,06), fac(hex(8c))  , bkt_ids$(1)          , ch(10),~
               at (08,18), fac(hex(8c))  , bkt_des$(1)          , ch(20),~
               at (08,40), fac(hex(8c))  , hny_bom$(1)          , ch(12),~
               at (08,54), fac(hex(8c))  , hny_rte$(1)          , ch(12),~
               at (08,68), fac(lfac$( 6)), hny_tlv$(1)          , ch(12),~
                                                                         ~
               at (09,02), fac(hex(8c))  , hny_seq$(2)          , ch(03),~
               at (09,06), fac(hex(8c))  , bkt_ids$(2)          , ch(10),~
               at (09,18), fac(hex(8c))  , bkt_des$(2)          , ch(20),~
               at (09,40), fac(hex(8c))  , hny_bom$(2)          , ch(12),~
               at (09,54), fac(hex(8c))  , hny_rte$(2)          , ch(12),~
               at (09,68), fac(lfac$( 7)), hny_tlv$(2)          , ch(12),~
                                                                         ~
               at (10,02), fac(hex(8c))  , hny_seq$(3)          , ch(03),~
               at (10,06), fac(hex(8c))  , bkt_ids$(3)          , ch(10),~
               at (10,18), fac(hex(8c))  , bkt_des$(3)          , ch(20),~
               at (10,40), fac(hex(8c))  , hny_bom$(3)          , ch(12),~
               at (10,54), fac(hex(8c))  , hny_rte$(3)          , ch(12),~
               at (10,68), fac(lfac$( 8)), hny_tlv$(3)          , ch(12),~
                                                                         ~
               at (11,02), fac(hex(8c))  , hny_seq$(4)          , ch(03),~
               at (11,06), fac(hex(8c))  , bkt_ids$(4)          , ch(10),~
               at (11,18), fac(hex(8c))  , bkt_des$(4)          , ch(20),~
               at (11,40), fac(hex(8c))  , hny_bom$(4)          , ch(12),~
               at (11,54), fac(hex(8c))  , hny_rte$(4)          , ch(12),~
               at (11,68), fac(lfac$( 9)), hny_tlv$(4)          , ch(12),~
                                                                         ~
               at (12,02), fac(hex(8c))  , hny_seq$(5)          , ch(03),~
               at (12,06), fac(hex(8c))  , bkt_ids$(5)          , ch(10),~
               at (12,18), fac(hex(8c))  , bkt_des$(5)          , ch(20),~
               at (12,40), fac(hex(8c))  , hny_bom$(5)          , ch(12),~
               at (12,54), fac(hex(8c))  , hny_rte$(5)          , ch(12),~
               at (12,68), fac(lfac$(10)), hny_tlv$(5)          , ch(12),~
                                                                         ~
               at (13,02), fac(hex(8c))  , hny_seq$(6)          , ch(03),~
               at (13,06), fac(hex(8c))  , bkt_ids$(6)          , ch(10),~
               at (13,18), fac(hex(8c))  , bkt_des$(6)          , ch(20),~
               at (13,40), fac(hex(8c))  , hny_bom$(6)          , ch(12),~
               at (13,54), fac(hex(8c))  , hny_rte$(6)          , ch(12),~
               at (13,68), fac(lfac$(11)), hny_tlv$(6)          , ch(12),~
                                                                         ~
               at (14,02), fac(hex(8c))  , hny_seq$(7)          , ch(03),~
               at (14,06), fac(hex(8c))  , bkt_ids$(7)          , ch(10),~
               at (14,18), fac(hex(8c))  , bkt_des$(7)          , ch(20),~
               at (14,40), fac(hex(8c))  , hny_bom$(7)          , ch(12),~
               at (14,54), fac(hex(8c))  , hny_rte$(7)          , ch(12),~
               at (14,68), fac(lfac$(12)), hny_tlv$(7)          , ch(12),~
                                                                         ~
               at (15,02), fac(hex(8c))  , hny_seq$(8)          , ch(03),~
               at (15,06), fac(hex(8c))  , bkt_ids$(8)          , ch(10),~
               at (15,18), fac(hex(8c))  , bkt_des$(8)          , ch(20),~
               at (15,40), fac(hex(8c))  , hny_bom$(8)          , ch(12),~
               at (15,54), fac(hex(8c))  , hny_rte$(8)          , ch(12),~
               at (15,68), fac(lfac$(13)), hny_tlv$(8)          , ch(12),~
                                                                         ~
               at (16,02), fac(hex(8c))  , hny_seq$(9)          , ch(03),~
               at (16,06), fac(hex(8c))  , bkt_ids$(9)          , ch(10),~
               at (16,18), fac(hex(8c))  , bkt_des$(9)          , ch(20),~
               at (16,40), fac(hex(8c))  , hny_bom$(9)          , ch(12),~
               at (16,54), fac(hex(8c))  , hny_rte$(9)          , ch(12),~
               at (16,68), fac(lfac$(14)), hny_tlv$(9)          , ch(12),~
                                                                         ~
               at (17,02), fac(hex(8c))  , hny_seq$(10)         , ch(03),~
               at (17,06), fac(hex(8c))  , bkt_ids$(10)         , ch(10),~
               at (17,18), fac(hex(8c))  , bkt_des$(10)         , ch(20),~
               at (17,40), fac(hex(8c))  , hny_bom$(10)         , ch(12),~
               at (17,54), fac(hex(8c))  , hny_rte$(10)         , ch(12),~
               at (17,68), fac(lfac$(15)), hny_tlv$(10)         , ch(12),~
                                                                         ~
               at (18,02), fac(hex(8c))  , hny_seq$(11)         , ch(03),~
               at (18,06), fac(hex(8c))  , bkt_ids$(11)         , ch(10),~
               at (18,18), fac(hex(8c))  , bkt_des$(11)         , ch(20),~
               at (18,40), fac(hex(8c))  , hny_bom$(11)         , ch(12),~
               at (18,54), fac(hex(8c))  , hny_rte$(11)         , ch(12),~
               at (18,68), fac(lfac$(16)), hny_tlv$(11)         , ch(12),~
                                                                         ~
               at (19,02), fac(hex(8c))  , hny_seq$(12)         , ch(03),~
               at (19,06), fac(hex(8c))  , bkt_ids$(12)         , ch(10),~
               at (19,18), fac(hex(8c))  , bkt_des$(12)         , ch(20),~
               at (19,40), fac(hex(8c))  , hny_bom$(12)         , ch(12),~
               at (19,54), fac(hex(8c))  , hny_rte$(12)         , ch(12),~
               at (19,68), fac(lfac$(17)), hny_tlv$(12)         , ch(12),~
                                                                         ~
               at (20,02), fac(hex(8c))  , mdmc_prompt$         , ch(05),~
               at (20,08), fac(hex(8c))  , mdmc$                , ch(12),~
               at (20,21), "Tot:",                                       ~
               at (20,26), fac(hex(8c))  , tot_hny$             , ch(12),~
               at (20,40), fac(hex(8c))  , tot_bom$             , ch(12),~
               at (20,54), fac(hex(8c))  , tot_rte$             , ch(12),~
               at (20,68), fac(hex(8c))  , tot_tlv$             , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,24), fac(hex(8c)), pf6$                   , ch(16),~
               at (22,42), fac(hex(8c)), pf10$                          ,~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), fac(hex(8c)), pf2$                           ,~
               at (23,24), fac(hex(8c)), pf7$                   , ch(16),~
               at (23,42), fac(hex(8c)), pf12$                          ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,02), fac(hex(8c)), pf4$                           ,~
               at (24,24), fac(hex(8c)), pf8$                   , ch(16),~
               at (24,42), fac(hex(8c)), pf25$                          ,~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys (keytab$), key (keyhit%)

               if keyhit% <> 13 then L41830
                  call "MANUAL" ("STCHNYSB")
                  goto L41155

L41830:        if keyhit% <> 15 then L41850
                  call "PRNTSCRN"
                  goto L41155

L41850:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(linenr%, fieldnr%)
              str(line2$,62%) = "STCHNYSB: " & str(cms2v$,,8%)
              if fieldnr% > 0% then init(hex(8c)) xfac$()                ~
                               else init(brite$) xfac$()
              on fieldnr% gosub L42080,         /* Std Cost dtl descr */  ~
                                L42090,         /* Std Cost dtl fix $ */  ~
                                L42090,         /* Std Cost dtl $/part*/  ~
                                L42085          /* Std Cost dtl bucket*/
            goto L42100

L42080:      xfac$(linenr%, fieldnr%) = uplow$ : return /* Up / Low  */
L42085:      xfac$(linenr%, fieldnr%) = upper$ : return /* Upper only*/
L42090:      xfac$(linenr%, fieldnr%) = nmric$ : return /* Numeric   */

L42100:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Part Standard Cost Details"           ,        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Part:",                                      ~
               at (04,08), fac(hex(8c))  , partnbr$             , ch(25),~
               at (04,34), fac(hex(8c))  , partdesc$            , ch(34),~
               at (04,71), "Type:",                                      ~
               at (04,77), fac(hex(8c))  , hny_typ$             , ch(03),~
                                                                         ~
               at (05,02), "BOM:",                                       ~
               at (05,07), fac(hex(8c)),   hny_bom$             , ch(03),~
               at (05,13), "Route:",                                     ~
               at (05,20), fac(hex(8c)),   hny_rte$             , ch(03),~
               at (05,26), "Std Run Qty:",                               ~
               at (05,39), fac(hex(8c)),   hny_srq$             , ch(10),~
               at (05,50), "Map:",                                       ~
               at (05,55), fac(hex(8c)),   hny_map$             , ch(08),~
               at (05,64), "Purchase Bkt:",                              ~
               at (05,78), fac(hex(8c)),   hny_mat$             , ch(02),~
                                                                         ~
               at (07,02), fac(hex(ac))  , pg3_l7$              , ch(79),~
                                                                         ~
               at (08,02), fac(hex(8c))     , dtl_seq$(1)       , ch(03),~
               at (08,07), fac(xfac$( 1, 1)), dtl_des$(1)       , ch(40),~
               at (08,48), fac(xfac$( 1, 2)), dtl_fix$(1)       , ch(10),~
               at (08,59), fac(xfac$( 1, 3)), dtl_per$(1)       , ch(10),~
               at (08,70), fac(xfac$( 1, 4)), dtl_ids$(1)       , ch(10),~
                                                                         ~
               at (09,02), fac(hex(8c))     , dtl_seq$(2)       , ch(03),~
               at (09,07), fac(xfac$( 2, 1)), dtl_des$(2)       , ch(40),~
               at (09,48), fac(xfac$( 2, 2)), dtl_fix$(2)       , ch(10),~
               at (09,59), fac(xfac$( 2, 3)), dtl_per$(2)       , ch(10),~
               at (09,70), fac(xfac$( 2, 4)), dtl_ids$(2)       , ch(10),~
                                                                         ~
               at (10,02), fac(hex(8c))     , dtl_seq$(3)       , ch(03),~
               at (10,07), fac(xfac$( 3, 1)), dtl_des$(3)       , ch(40),~
               at (10,48), fac(xfac$( 3, 2)), dtl_fix$(3)       , ch(10),~
               at (10,59), fac(xfac$( 3, 3)), dtl_per$(3)       , ch(10),~
               at (10,70), fac(xfac$( 3, 4)), dtl_ids$(3)       , ch(10),~
                                                                         ~
               at (11,02), fac(hex(8c))     , dtl_seq$(4)       , ch(03),~
               at (11,07), fac(xfac$( 4, 1)), dtl_des$(4)       , ch(40),~
               at (11,48), fac(xfac$( 4, 2)), dtl_fix$(4)       , ch(10),~
               at (11,59), fac(xfac$( 4, 3)), dtl_per$(4)       , ch(10),~
               at (11,70), fac(xfac$( 4, 4)), dtl_ids$(4)       , ch(10),~
                                                                         ~
               at (12,02), fac(hex(8c))     , dtl_seq$(5)       , ch(03),~
               at (12,07), fac(xfac$( 5, 1)), dtl_des$(5)       , ch(40),~
               at (12,48), fac(xfac$( 5, 2)), dtl_fix$(5)       , ch(10),~
               at (12,59), fac(xfac$( 5, 3)), dtl_per$(5)       , ch(10),~
               at (12,70), fac(xfac$( 5, 4)), dtl_ids$(5)       , ch(10),~
                                                                         ~
               at (13,02), fac(hex(8c))     , dtl_seq$(6)       , ch(03),~
               at (13,07), fac(xfac$( 6, 1)), dtl_des$(6)       , ch(40),~
               at (13,48), fac(xfac$( 6, 2)), dtl_fix$(6)       , ch(10),~
               at (13,59), fac(xfac$( 6, 3)), dtl_per$(6)       , ch(10),~
               at (13,70), fac(xfac$( 6, 4)), dtl_ids$(6)       , ch(10),~
                                                                         ~
               at (14,02), fac(hex(8c))     , dtl_seq$(7)       , ch(03),~
               at (14,07), fac(xfac$( 7, 1)), dtl_des$(7)       , ch(40),~
               at (14,48), fac(xfac$( 7, 2)), dtl_fix$(7)       , ch(10),~
               at (14,59), fac(xfac$( 7, 3)), dtl_per$(7)       , ch(10),~
               at (14,70), fac(xfac$( 7, 4)), dtl_ids$(7)       , ch(10),~
                                                                         ~
               at (15,02), fac(hex(8c))     , dtl_seq$(8)       , ch(03),~
               at (15,07), fac(xfac$( 8, 1)), dtl_des$(8)       , ch(40),~
               at (15,48), fac(xfac$( 8, 2)), dtl_fix$(8)       , ch(10),~
               at (15,59), fac(xfac$( 8, 3)), dtl_per$(8)       , ch(10),~
               at (15,70), fac(xfac$( 8, 4)), dtl_ids$(8)       , ch(10),~
                                                                         ~
               at (16,02), fac(hex(8c))     , dtl_seq$(9)       , ch(03),~
               at (16,07), fac(xfac$( 9, 1)), dtl_des$(9)       , ch(40),~
               at (16,48), fac(xfac$( 9, 2)), dtl_fix$(9)       , ch(10),~
               at (16,59), fac(xfac$( 9, 3)), dtl_per$(9)       , ch(10),~
               at (16,70), fac(xfac$( 9, 4)), dtl_ids$(9)       , ch(10),~
                                                                         ~
               at (17,02), fac(hex(8c))     , dtl_seq$(10)      , ch(03),~
               at (17,07), fac(xfac$(10, 1)), dtl_des$(10)      , ch(40),~
               at (17,48), fac(xfac$(10, 2)), dtl_fix$(10)      , ch(10),~
               at (17,59), fac(xfac$(10, 3)), dtl_per$(10)      , ch(10),~
               at (17,70), fac(xfac$(10, 4)), dtl_ids$(10)      , ch(10),~
                                                                         ~
               at (18,02), fac(hex(8c))     , dtl_seq$(11)      , ch(03),~
               at (18,07), fac(xfac$(11, 1)), dtl_des$(11)      , ch(40),~
               at (18,48), fac(xfac$(11, 2)), dtl_fix$(11)      , ch(10),~
               at (18,59), fac(xfac$(11, 3)), dtl_per$(11)      , ch(10),~
               at (18,70), fac(xfac$(11, 4)), dtl_ids$(11)      , ch(10),~
                                                                         ~
               at (19,02), fac(hex(8c))     , dtl_seq$(12)      , ch(03),~
               at (19,07), fac(xfac$(12, 1)), dtl_des$(12)      , ch(40),~
               at (19,48), fac(xfac$(12, 2)), dtl_fix$(12)      , ch(10),~
               at (19,59), fac(xfac$(12, 3)), dtl_per$(12)      , ch(10),~
               at (19,70), fac(xfac$(12, 4)), dtl_ids$(12)      , ch(10),~
                                                                         ~
               at (20,29), "Totals:",                                    ~
               at (20,37), fac(hex(8c))  , tot_dtl$             , ch(10),~
               at (20,48), fac(hex(8c))  , tot_fix$             , ch(10),~
               at (20,59), fac(hex(8c))  , tot_per$             , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,20), fac(hex(8c)), pf4$                           ,~
               at (22,42), fac(hex(8c)), pf11$                          ,~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), fac(hex(8c)), pf2$                   , ch(18),~
               at (23,20), fac(hex(8c)), pf5$                           ,~
               at (23,42), fac(hex(8c)), pf12$                          ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,02), fac(hex(8c)), pf3$                           ,~
               at (24,20), fac(hex(8c)), pf8$                           ,~
               at (24,42), fac(hex(8c)), pf25$                          ,~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys (keytab$), key (keyhit%)

               if keyhit% <> 13 then L42705
                  call "MANUAL" ("STCHNYSB")
                  goto L42100

L42705:        if keyhit% <> 15 then L42725
                  call "PRNTSCRN"
                  goto L42100

L42725:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   4                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'104(fieldnr%)
              str(line2$,62%) = "STCHNYSB: " & str(cms2v$,,8%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L43080,         /* Beginning Part No */   ~
                                L43080,         /* Ending Part No.   */   ~
                                L43080,         /* Summary or Detail */   ~
                                L43080          /* All Parts switch  */
              goto L43095

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low    */
L43080:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only  */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric     */

L43095:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Part Standard Cost Listings",                   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Begin Part No.:",                            ~
               at (06,18), fac(lfac$( 1)), beg_part$            , ch(25),~
               at (06,44), fac(hex(8c))  , beg_desc$            , ch(34),~
                                                                         ~
               at (07,04), "End Part No.:",                              ~
               at (07,18), fac(lfac$( 2)), end_part$            , ch(25),~
               at (07,44), fac(hex(8c))  , end_desc$            , ch(34),~
                                                                         ~
               at (08,02), "Summary/Detail:",                            ~
               at (08,18), fac(lfac$( 3)), sum_detl$            , ch(01),~
               at (08,44), fac(hex(8c))  , sum_desc$            , ch(09),~
                                                                         ~
               at (09,02), "All Inv Parts?:",                            ~
               at (09,18), fac(lfac$( 4)), allparts$            , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys (keytab$), key (keyhit%)

               if keyhit% <> 13 then L43260
                  call "MANUAL" ("STCHNYSB")
                  goto L43095

L43260:        if keyhit% <> 15 then L43280
                  call "PRNTSCRN"
                  goto L43095

L43280:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   5                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen (cost matrix).             *~
            *************************************************************

        deffn'105
              str(line2$,62%) = "STCHNYSB: " & str(cms2v$,,8%)

L44045:     accept                                                       ~
               at (01,02),                                               ~
                  "Part Standard Cost Roll-up/Fold-in Matrix"   ,        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), "Part:",                                      ~
               at (04,08), fac(hex(8c))  , partnbr$             , ch(25),~
               at (04,34), fac(hex(8c))  , partdesc$            , ch(34),~
               at (04,71), "Type:",                                      ~
               at (04,77), fac(hex(8c))  , hny_typ$             , ch(03),~
                                                                         ~
               at (05,02), "BOM:",                                       ~
               at (05,07), fac(hex(8c)),   hny_bom$             , ch(03),~
               at (05,13), "Route:",                                     ~
               at (05,20), fac(hex(8c)),   hny_rte$             , ch(03),~
               at (05,26), "Std Run Qty:",                               ~
               at (05,39), fac(hex(8c)),   hny_srq$             , ch(10),~
               at (05,50), "Map:",                                       ~
               at (05,55), fac(hex(8c)),   hny_map$             , ch(08),~
               at (05,64), "Purchase Bkt:",                              ~
               at (05,78), fac(hex(8c)),   hny_mat$             , ch(02),~
                                                                         ~
               at (07,02), fac(hex(ac))  , pg5_l7$              , ch(79),~
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
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,20), fac(hex(8c)), pf8$                           ,~
               at (24,65), "(16)Return",                                 ~
                                                                         ~
               keys (hex(0001080d0f10)), key (u3%)

               if u3% <> 13 then L44745
                  call "MANUAL" ("STCHNYSB")
                  goto L44045

L44745:        if u3% <> 15 then L44765
                  call "PRNTSCRN"
                  goto L44045

L44765:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "

        REM Test for Part No.                     PARTNBR$
            if keyhit% <> 9% then goto L50180
            partdesc$ = hex(06) & "Select a Part Number"
            init (" ") msg$()
            call "PLOWCODE" (#3, partnbr$, partdesc$, 8000%, .32, f1%(3),~
                msg$(), 0, 0, incl(), incl$(), " ", " ", #1)
            if f1%(3) = 1% then goto L50180
                errormsg$ = "Part Number not found in Standard Cost " &  ~
                     "Part file.  Try again." : return
L50180:     call "GETCODE" (#1, partnbr$, partdesc$, 0%, 0.32, f1%(1))
            if f1%(1) <> 0% then goto L50221
                errormsg$ = "Part Number not found in Part Master file."&~
                     "  Try again."
                return
L50221:     get #1 using L50222, hny_typ$
L50222:         FMT  /* File #1- HNYMASTR  */  POS(180), CH(3)
            call "PUTPAREN" (partdesc$)
            call "READ100" (#3, partnbr$, f1%(3))
            if f1%(3) = 0% then gosub data_edit else gosub dataload
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51250,         /* Bill of Materials */     ~
                              L51780,         /* Route             */     ~
                              L51440,         /* Standard Run Qty  */     ~
                              L51540,         /* Mapping to Use    */     ~
                              L51965,         /* Materials Bucket  */     ~
                              L51630,         /* This Level  1     */     ~
                              L51630,         /* This Level  2     */     ~
                              L51630,         /* This Level  3     */     ~
                              L51630,         /* This Level  4     */     ~
                              L51630,         /* This Level  5     */     ~
                              L51630,         /* This Level  6     */     ~
                              L51630,         /* This Level  7     */     ~
                              L51630,         /* This Level  8     */     ~
                              L51630,         /* This Level  9     */     ~
                              L51630,         /* This Level 10     */     ~
                              L51630,         /* This Level 11     */     ~
                              L51630          /* This Level 12     */
            return

L51250: REM Test for Bill of Materials            HNY_BOM$
            if hny_bom$ = bomsave$ then goto L51290
                bomsave$ = hny_bom$
                hny_rte$ = " "
L51290:     if hny_bom$ = " " then goto L51410
            plowkey$ = str(partnbr$,,25) & str(hny_bom$,,3) & hex(00)
            init (" ") msg$()
            misc$ = hex(06) & "Select a BOM for Part " & partnbr$
            call "PLOWCODE" (#2, plowkey$, misc$,  2025%,  0.3, f1%(2),  ~
                msg$(), 3)
            if f1%(2) <> 0% then goto L51380
                errormsg$ = "Bill of Materials not on file.  Try again."
                return
L51380:     hny_bom$ = str(plowkey$, 26, 3)
            get #2 using L51400, hny_rte$
L51400:         FMT  /*  File #2- BOMMASTR  */  POS(87), CH(3)
L51410:     hnysw% = 1%
            return

L51440: REM Test for Standard Run Qty             HNY_SRQ$
            call "NUMTEST" (hny_srq$, 1, 9e7, errormsg$, 0.2, hny_srq)
            if errormsg$ <> " " then return
            if abs(hny_srq - srqsave) < .0001 then return
            srqsave = hny_srq
            call "CONVERT" (hny_srq, -0.2, hny_srq$)
            hnysw% = 1%
            if dtl% > 0% then gosub clear_this_level
            return

L51540: REM Test for Mapping to Use               HNY_MAP$
            hnysw% = 1%
            mapdflt$ = hny_map$
            if hny_map$ = " " then return
            call "GETCODE" (#7, hny_map$, misc$, 0%, 0, f1%(7))
            if f1%(7) = 1% then return
                errormsg$ = "Std Cost Set Mapping record not on file." & ~
                     "  Try again."
                return

L51630: REM Test for This Level                   HNY_TLV$
            i% = fieldnr% - 5%
            call "NUMTEST" (hny_tlv$(i%), 0, 9e7, errormsg$, 4.4, tlv)
            if errormsg$ <> " " then return
                tot_hny = tot_hny - tot_tlv
                tot_tlv = tot_tlv - hny_tlv(i%)
                hny_tlv(i%) = tlv
                tot_tlv = tot_tlv + hny_tlv(i%)
                tot_hny = tot_hny + tot_tlv
                call "CONVERT" (tot_tlv, 4.4, tot_tlv$)
                call "CONVERT" (tot_hny, 4.4, tot_hny$)
                call "CONVERT" (hny_tlv(i%), 4.4, hny_tlv$(i%))
                hnysw% = 1%
                return

L51780: REM Test for Route                        HNY_RTE$
            if hny_rte$ = " " then return
            plowkey$ = str(partnbr$,,25) & str(hny_rte$,,3) & hex(00)
            init (" ") msg$()
            misc$ = hex(06) & "Select a ROUTE for Part " & partnbr$
            call "PLOWCODE" (#10, plowkey$, misc$, 2025%, 0.0, f1%(10),  ~
                msg$(), 3)
            if f1%(10) <> 0% then goto L51910
                hny_rte$ = " "
                return
L51910:     hny_rte$ = str(plowkey$, 26, 3)
            return

L51965: REM Test for Default Materials Bucket     HNY_MAT$
            if str(hny_mat$,1,1) = " " then hny_mat$ = str(hny_mat$,2)
            if hny_mat$ = " " then hny_mat$ = "P"
            if pos("PD" = str(hny_mat$,1,1)) = 0% then L51974
               str(hny_mat$,2) = " "
               return
L51974:     temp = buckets%
            call "NUMTEST" (hny_mat$, 1, temp, errormsg$, 0.0, 0)
            if errormsg$ = " " then return
            errormsg$ = "Enter 'D', 'P', or Bucket '1' thru 'xx'"
            convert buckets% to str(errormsg$,37,2), pic(##)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************

        deffn'153(linenr%, fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L52140,         /* Std Cost dtl descr */    ~
                              L52180,         /* Std Cost dtl fixed$*/    ~
                              L52360,         /* Std Cost dtl $/part*/    ~
                              L52540          /* Std Cost dtl bucket*/
            return

L52140: REM Test for Description                  DTL_DES$
            dtlsw% = 1%
            return

L52180: REM Test for Fixed $                      DTL_FIX$
            call "NUMTEST" (dtl_fix$(linenr%), 0, 9e7, errormsg$, 4.4,   ~
                fix)
            if errormsg$ <> " " then return
                if dtl_bkt%(linenr%) <> 0% then                          ~
                     tot_hny = tot_hny - hny_tlv(dtl_bkt%(linenr%))
                gosub'200(dtl_fix(linenr%), dtl_per(linenr%),            ~
                     dtl_bkt%(linenr%))
                dtl_fix(linenr%) = fix
                gosub'201(dtl_fix(linenr%), dtl_per(linenr%),            ~
                     dtl_bkt%(linenr%))
                if dtl_bkt%(linenr%) = 0% then goto L52320
                     tot_hny = tot_hny + hny_tlv(dtl_bkt%(linenr%))
                     call "CONVERT" (tot_hny, 4.4, tot_hny$)
L52320:         call "CONVERT" (dtl_fix(linenr%), 4.4, dtl_fix$(linenr%))
                dtlsw% = 1%
                return

L52360: REM Test for $ per Part                   DTL_PER$
            call "NUMTEST" (dtl_per$(linenr%), 0, 9e7, errormsg$, 4.4,   ~
                per)
            if errormsg$ <> " " then return
                if dtl_bkt%(linenr%) <> 0% then                          ~
                     tot_hny = tot_hny - hny_tlv(dtl_bkt%(linenr%))
                gosub'200(dtl_fix(linenr%), dtl_per(linenr%),            ~
                     dtl_bkt%(linenr%))
                dtl_per(linenr%) = per
                gosub'201(dtl_fix(linenr%), dtl_per(linenr%),            ~
                     dtl_bkt%(linenr%))
                if dtl_bkt%(linenr%) = 0% then goto L52500
                     tot_hny = tot_hny + hny_tlv(dtl_bkt%(linenr%))
                     call "CONVERT" (tot_hny, 4.4, tot_hny$)
L52500:         call "CONVERT" (dtl_per(linenr%), 4.4, dtl_per$(linenr%))
                dtlsw% = 1%
                return

L52540: REM Test for Std Cost Bucket              DTL_IDS$
            if dtl_ids$(linenr%) = " " or dtl_ids$(linenr%) = "?"        ~
                then L52640
                plowkey$ = "STC.BD." & str(userid$) & "." & hex(00)
L52580:         call "PLOWNEXT" (#8, plowkey$, 11%, f1%(8))
                if f1%(8) = 0% then L52640
                     get #8 using L52610, misc$
L52610:                   FMT  /* File #8- SYSFILE2  */ POS(21), CH(10)
                     if dtl_ids$(linenr%)=misc$ then goto bucket_in_n_out
                     goto L52580
L52640:     misc$ = hex(06) & "Select a Standard Cost bucket"
            plowkey$ = "STC.BD." & str(userid$) & "." & hex(00)
            call "PLOWCODE" (#8, plowkey$, misc$, 11%, .3, f1%(8))
            if f1%(8) <> 0% then goto bucket_in_n_out
                errormsg$ = "Cost bucket not found.  Try again."
                return
        bucket_in_n_out
            tot_hny = tot_hny - tot_tlv
            gosub'200(dtl_fix(linenr%),dtl_per(linenr%),dtl_bkt%(linenr%))
            convert str(plowkey$, 12, 2) to dtl_bkt%(linenr%),           ~
                data goto L52750
L52750:     gosub'201(dtl_fix(linenr%),dtl_per(linenr%),dtl_bkt%(linenr%))
            if dtl_bkt%(linenr%) = 0% then goto L52790
                tot_hny = tot_hny + tot_tlv
                call "CONVERT" (tot_hny, 4.4, tot_hny$)
L52790:     dtl_ids$(linenr%) = str(misc$,,10)
            dtlsw% = 1%
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 4.                      *~
            *************************************************************

        deffn'154(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L53130,         /* Beginning Part No */     ~
                              L53330,         /* Ending Part No.   */     ~
                              L53510,         /* Summary or Detail */     ~
                              L53610          /* All Parts switch  */
            return

L53130: REM Test for Beginning Part No.           BEG_PART$
            if beg_part$ <> "ALL" then goto L53180
                beg_desc$, end_part$, end_desc$ = " "
                fieldnr% = fieldnr% + 1%
                goto part_test_range
L53180:     if beg_part$ <> "FIRST" then goto L53210
                beg_desc$ = " "
                return
L53210:     beg_desc$ = hex(06) & "Select a beginning Part No."
            plowkey$ = str(beg_part$,,25) & hex(00)
            init (" ") msg$()
            call "PLOWCODE" (#3, plowkey$, beg_desc$, 8000%, .32, f1%(3),~
                msg$(), 0, 0, incl(), incl$(), " ", " ", #1)
            if f1%(3) <> 0% then goto L53290
                errormsg$ = "Part Number not found in Standard Cost " &  ~
                     "Part file.  Try again." : return
L53290:     beg_part$ = str(plowkey$,,25)
            call "PUTPAREN" (beg_desc$)
            return

L53330: REM Test for Ending Part No.              END_PART$
            if end_part$ <> "LAST" then goto L53370
                end_desc$ = " "
                goto part_test_range
L53370:     end_desc$ = hex(06) & "Select an ending Part No."
            plowkey$ = str(end_part$,,25) & hex(00)
            init (" ") msg$()
            call "PLOWCODE" (#3, plowkey$, end_desc$, 8000%, .32, f1%(3),~
                msg$(), 0, 0, incl(), incl$(), " ", " ", #1)
            if f1%(3) <> 0% then goto L53450
                errormsg$ = "Part Number not found in Standard Cost " &  ~
                     "Part file.  Try again." : return
L53450:     end_part$ = str(plowkey$,,25)
            call "PUTPAREN" (end_desc$)
        part_test_range
            call "TESTRNGE" (beg_part$, end_part$, beg$, end$, errormsg$)
            return

L53510: REM Test for Summary or Detail           SUM_DETL$
            sum_desc$ = "(Summary)"
            if sum_detl$ = "S" then return
            sum_desc$ = "(Detail)"
            if sum_detl$ = "D" then return
                sum_desc$ = " "
                errormsg$ = "You must enter 'S' for Summary or 'D' for "&~
                     "Detail"
                return

L53610: REM Test all inventory parts switch       ALLPARTS$
            if allparts$ = "Y" or allparts$ = "N" then return
            errormsg$ = "Response must be either 'Y' for Yes or 'N' " &  ~
                "for No"
            return

        REM *************************************************************~
            *             I M A G E   S T A T E M E N T S               *~
            *************************************************************

L60040: %RUN DATE: ######## @ ########      #############################~
        ~###############################                      STCHNYSB####~
        ~###
L60070: %                                                    PART STANDAR~
        ~D COSTS LISTING                                           PAGE: #~
        ~###
L60100: %                                   #############################~
        ~###############################
L60120: %PART NUMBER               DESCRIPTION                    TYP BOM~
        ~ RTE ST RUN QTY MAPPING    BOM COST   RTE COST THIS LEVEL TOTAL C~
        ~OST
L60150: %------------------------- ------------------------------ --- ---~
        ~ --- ---------- -------- ---------- ---------- ---------- -------~
        ~---
L60180: %######################### ############################## ### ###~
        ~ ### ########## #################################################~
        ~###
L60210: %         *---------------------------- STANDARD COST DETAILS ---~
        ~------------------------*
L60230: %         SEQ DESCRIPTION                               FIXED AMT~
        ~   PER PART BUCKET ID  B#
L60250: %         --- ---------------------------------------- ----------~
        ~ ---------- ---------- --
L60270: %         ### ######################################## ##########~
        ~ ########## ########## ##
L60290: %         *------------------------- PART STANDARD COSTS --------~
        ~------------------*
L60310: %          B# BUCKET ID  DESCRIPTION              FROM BOM     FR~
        ~OM RTE   THIS LEVEL
L60330: %         --- ---------- -------------------- ------------ ------~
        ~------ ------------
L60350: %         ### ########## #################### ############ ######~
        ~###### ############  ##################
L60370: %                          ######################################~
        ~##########################################
L60390: %** END OF REPORT **

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
