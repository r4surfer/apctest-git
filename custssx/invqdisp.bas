        REM *************************************************************~
            *                                                           *~
            *  IIIII  N   N  V   V   QQQ   DDDD   IIIII   SSS   PPPP    *~
            *    I    NN  N  V   V  Q   Q  D   D    I    S      P   P   *~
            *    I    N N N  V   V  Q   Q  D   D    I     SSS   PPPP    *~
            *    I    N  NN   V V   Q   Q  D   D    I        S  P       *~
            *  IIIII  N   N    V     QQQ   DDDD   IIIII   SSS   P       *~
            *                           Q                               *~
            *-----------------------------------------------------------*~
            * INVQDISP - Display INVQUAN Info for Part Specified.       *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/29/86 ! Original                                 ! ERN *~
            * 11/14/86 ! Added 'zero lot' option                  ! WPH *~
            * 04/08/86 ! Standard Cost Project Modifications      ! ERN *~
            * 05/27/88 ! Added Inv Val Method to HNYPDISP call    ! MJB *~
            * 08/24/93 ! PRR 10850. UOM to EOD line. Only Room!   ! JDH *~
            *          ! PRR 12913. PF9 lists Locations.          !     *~
            *************************************************************~
            * 03/03/06 ! (PAR000) CR347 Mod for New Part Number   ! RHH *~
            *************************************************************

        sub "INVQDISP"   (part$,         /* Part Number to Display     */~
                          #1,            /* INVMASTR Channel           */~
                          #2,            /* INVQUAN  Channel           */~
                          #3,            /* INVYPOOL  Channel           */~
                          #4 )           /* SYSFILE2 Channel           */

*        Part Number must be on file or the subroutine whips right
*        back to the caller.  Also the Caller must open the files.


        dim                                                              ~
            bid$(12%)10, bdescr$(12%)20, /* Cost Bucket IDs and Descrs */~
            company$60,                  /* Company Name for Report    */~
            conv$10,                     /* UOM Conversion Factor      */~
            costs$(12%)50,               /* Cost Breakdown Display     */~
            cursor%(2%),                 /* Cursor Position            */~
            date$8,                      /* Date for screen display    */~
            descr$34,                    /* Part Decsription           */~
            descrp$79,                   /* PLOWCODE Param             */~
            descr_map(4%),               /* PLOWCODE Param             */~
            dfac$(15%)1,                 /* Display Facs               */~
            dsply$(16%)79,               /* Display Strings            */~
            errormsg$79,                 /* Error message              */~
            hdr$79, hdr2$50,             /* Screen Column Headings     */~
            hdr$(3%)79,                  /* PLOWCODE Param             */~
            i$(24%)80,                   /* Screen Image               */~
            i_e(1%),                     /* PLOWCODE Param             */~
            i_e$(1%)1,                   /* PLOWCODE Param             */~
            inpmessage$79,               /* Informational Message      */~
            lfac$1,                      /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            lot$6, lot$(4%,2%)6,         /* Lot Info                   */~
            method$(16%)1,               /* Inv Val Method array       */~
            mthd$1,                      /* Inv Val Method from file   */~
            part$45,                     /* Part Number        (PAR000)*/~
            pf$(3%)79, pfkeys$20,        /* PF Keys                    */~
            plowkey$99, plowkey1$99,     /* Misc Use Plow keys         */~
            qtys(8%), qtys$(8%)11,       /* Qtys for Report            */~
            readkey$99,                  /* Misc Use Read Key          */~
            rptdescr$128,                /* Report Select Criteria     */~
            runtime$8,                   /* Report Run Time            */~
            setid$8,                     /* Current Cost Set           */~
            stds(12%),                   /* Inventory Values           */~
            str$3, str$(4%,2%)5,         /* Store Info                 */~
            ttls(8%),                    /* Report Totals              */~
            uomp$4,                      /* Pricing Unit of Measure    */~
            uoms$4                       /* Stocking Unit of Measure   */

        dim f1%(32%)                     /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "REV:01.00 03/03/06 New Part Number Purchase Jobs  "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! INVMASTR ! Inventory Master File            (PAR000)*~
            * #2  ! INVQUAN  ! Inventory Quantity File          (PAR000)*~
            * #3  ! INVPOOL  ! Inventory Pool File              (PAR000)*~
            * #4  ! SYSFILE2 ! System Information Repository            *~
            * #5  ! HNYLOCNS ! Stock Location Detail File               *~
            *************************************************************~

            select #5,  "HNYLOCNS",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =  443, keylen =  42,         ~
                            key  2, keypos =  485, keylen =  42,         ~
                            key  3, keypos =  527, keylen =  42,         ~
                            key  4, keypos =  590, keylen =  42

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)                          /* (PAR000) */
            str(line2$,62) = "INVQDISP: " & str(cms2v$,,8)

            hdr$ = "Str" & hex(ac) & "Lot No" & hex(ac) & "   On-Hand" & ~
                   hex(ac) & " Open S.O." & hex(ac) & "  On-Order" &     ~
                   hex(ac) & "     In QC" & hex(ac) & "Value Each" &     ~
                   hex(ac) & "Val On-Hand" & hex(8c)
            call "COMPNAME" (12%, company$, u3%)
                                                           /* (PAR000)  */ 
            call "READ100" (#1, part$, f1%(1%))
            if f1%(1%) = 0% then exit_program
                get #1 using L09190, part$, descr$, uoms$, uomp$, conv
L09190:              FMT CH(45), CH(32), POS(94), 2*CH(4), PD(14,7)
                call "CONVERT" (conv, -0.7, conv$)
                str(line2$,,61) = "Part: " & part$ & " (" & str(descr$,1%,15%) & ")"

            plowkey$ = part$
            call "PLOWNEXT" (#2, plowkey$, 45%, f1%(2%))
            if f1%(2) = 1% then L09310
                u3% = 2%
                call "ASKUSER" (u3%, "INVQUAN DISPLAY",                  ~
                          "There are no Quantity Details for this Part", ~
                          "Press RETURN to continue...", " ")
                goto exit_program

L09310:     call "GETNAMES" addr(#3, file$, temp$, temp$)

*        Set Standard Costing Info
            hdr2$ = "##" & hex(8cac) & "Bucket ID " &                    ~
                    hex(8cac) & "Bucket Description  " &                 ~
                    hex(8cac) & "       Value" & hex(8c)
            call "READ100" (#4, "STC.CONTROL", f1%(4))
            if f1%(4) = 1% then L09410
L09390:         setid$ = "--NONE--"
                goto L10000
L09410:     get #4 using L09420, setid$
L09420:         FMT POS(28), CH(8)
            if setid$ = " " then L09390
            readkey$ = "STC.HDR." & setid$
            call "READ100" (#4, readkey$, f1%(4))
            if f1%(4) = 1% then get #4 using L09470, bid$(), bdescr$()
L09470:         FMT POS(60), 12*CH(10), 12*CH(20)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, dsply$(), str$(), lot$()
            str$(1,1), lot$(1,1) = "ALL"
            init(hex(00)) str$(2,1), lot$(2,1)
            init(hex(ff)) str$(2,2), lot$(2,2)
            plowkey$ = str(part$,,45) & str(str$(2,1)) & lot$(2,1)
            gosub load_screen

*        Main Screen: Get Display Options
        main_screen
            gosub set_pf_main_screen
            gosub'101               /* Display & Accept Screen    */
                if keyhit% <>  6% then L10220
                     nozero% = nozero% + 1%
                     if nozero% = 2% then nozero% = 0%
                     goto inputmode
L10220:         if keyhit%  =  1% then inputmode
                if keyhit%  =  2% then first_screen
                if keyhit%  =  5% then next_screen
                if keyhit%  =  8% then calc_totals
                if keyhit%  = 10% then modify_criteria
                if keyhit%  = 14% then print_report
                if keyhit%  = 16% then exit_program

*        RETURN/PF-9/PF-12 implies Show Pool Info, Locations, or Costs
            c% = cursor%(1%) - 4%
            if c% < 1% or c% > 15% then main_screen
            if str(dsply$(c%),,6%) = "** END" then main_screen
            if str(dsply$(c%),,3%) = " " then main_screen
            if keyhit% =  9% then show_locations
            if keyhit% = 12% then show_costs
            if file$ <> "INVPOOL" then main_screen         /* (PAR000) */
                call "INVPDISP" (part$, str(dsply$(c%),,3%),             ~
                              str(dsply$(c%),5%,6%), #1, #3, method$(c%))
                goto main_screen
                                                     /* (PAR000)   */
            show_costs    /* Display Cost Breakdown for line indicated */
                readkey$ = str(part$,,45%) & str(dsply$(c%),,3%) &       ~
                                             str(dsply$(c%),5%,6%)
                call "READ100" (#2, readkey$, f1%(2%))
                if f1%(2%) = 0% then main_screen   /* Queer Indeed */
                gosub costs_screen
                goto  main_screen

            show_locations /* Display Locations and Quantities */
                if opened_this% = 1% then L10440
                    call "OPENCHCK" (#5, 0%, 0%, 0%, " ")
                    opened_this% = 1%
L10440:         readkey$ = str(dsply$(c%),,3%) & str(part$,,25%) &       ~
                                                 str(dsply$(c%),5%,6%)
                descrp$ = hex(06) & "Part: " & str(part$,1%,25%) & " (" & descr$ &   ~
                          ")  Str/Lot: " & str(dsply$(c%),,10%)
                hdr$(1%) = "    Location     Quantity"
                descr_map(1%) =   4.08  : descr_map(2%) = 03.00
                descr_map(3%) = 573.08  : descr_map(4%) = 14.1042

                call "PLOWCODE" (#5, readkey$, descrp$, 9034%, 1.99, 0%, ~
                                 hdr$(), 0, 0, i_e(), i_e$(), "d", "Y",  ~
                                 #5, descr_map())
                goto  main_screen

        first_screen
            plowkey$ = str(part$,,45) & str(str$(2,1)) & lot$(2,1)
        next_screen
            gosub load_screen
            goto  main_screen


        calc_totals  /* Display totals for criteria specified          */
            inpmessage$ = "Enter Selection Criteria for Totals."
            opt% = 1%
            gosub save_display
            gosub get_criteria
                if keyhit%  =  1% then restore_display
            gosub load_totals
            gosub set_pf_totals
L10640:     gosub'101
                if keyhit%  =  0% then restore_display
                if keyhit%  = 16% then exit_program
                goto L10640


        modify_criteria   /* Change Display Parameters                 */
            inpmessage$ = "Enter new Display Criteria."
            opt% = 2%
            gosub save_display
            gosub get_criteria
                if keyhit%  =  1% then restore_display
                goto first_screen

        print_report
            inpmessage$ = "Enter Selection Criteria for Report."
            opt% = 3%
            gosub save_display
            gosub get_criteria
                if keyhit%  =  1% then restore_display
            gosub report_printing
            goto restore_display


        get_criteria  /* Get Selection Criteria and Return   */
            gosub set_pf_criteria
L10900:     gosub'101
                if keyhit%  =  1% then return
                if keyhit%  = 16% then return clear all
                if keyhit%  = 16% then exit_program
                if keyhit% <>  0% then L10900
            gosub'151
                if errormsg$ <> " " then L10900
            return


        save_display   /* Save Display and Criteria for start over     */
            dsply$(16) = dsply$(1)
            str$(3,1) = str$(1,1) : str$(3,2) = str$(1,2)
            str$(4,1) = str$(2,1) : str$(4,2) = str$(2,2)
            lot$(3,1) = lot$(1,1) : lot$(3,2) = lot$(1,2)
            lot$(4,1) = lot$(2,1) : lot$(4,2) = lot$(2,2)
            return

        restore_display   /* Restore last display                      */
            dsply$(1) = dsply$(16)
            str$(1,1) = str$(3,1) : str$(1,2) = str$(3,2)
            str$(2,1) = str$(4,1) : str$(2,2) = str$(4,2)
            lot$(1,1) = lot$(3,1) : lot$(1,2) = lot$(3,2)
            lot$(2,1) = lot$(4,1) : lot$(2,2) = lot$(4,2)
            goto main_screen


        report_printing   /* Produce Report per Criteria specified     */
            page% = 0% : line% = 857%
            mat ttls = zer
            call "SHOSTAT" ("Printing Quantities Report")
            call "SETPRNT" ("HNY030", " ", 0%, 0%)
            select printer (134)
            call "TIME" (runtime$)
            plowkey1$ = str(part$,,45) & str(str$(2,1))
            rptdescr$ = "PART: " & part$ & " DESCRIPTION: " & descr$
            if str$(1,1) = "ALL" then                                    ~
                rptdescr$ = rptdescr$ & ".  FOR ALL STORES"
            if str$(1,1) <> "ALL" and str$(1,1) = str$(1,2) then         ~
                rptdescr$ = rptdescr$ & ".  FOR STORE " & str$(1,1)
            if str$(1,1) <> "ALL" and str$(1,1) <> str$(1,2) then        ~
                rptdescr$ = rptdescr$ & ".  FOR STORES " & str$(1,1) &   ~
                            " TO " & str$(1,2)
            if lot$(1,1) = "ALL" then                                    ~
                rptdescr$ = rptdescr$ & ";  FOR ALL LOTS"
            if lot$(1,1) <> "ALL" and lot$(1,1) = lot$(1,2) then         ~
                rptdescr$ = rptdescr$ & ";  FOR LOTS " & lot$(1,1)
            if lot$(1,1) <> "ALL" and lot$(1,1) <> lot$(1,2) then        ~
                rptdescr$ = rptdescr$ & ";  FOR LOTS " & lot$(1,1) &     ~
                            " TO " & lot$(1,2)
            call "STRING" addr("CT", rptdescr$, 128%)

        report_loop                                         /* (PAR000) */
            call "PLOWNEXT" (#2, plowkey1$, 45%, f1%(2%))
            if f1%(2) = 0% then end_report

            get #2 using L11490, str$, lot$, qtys(1), qtys(2), qtys(3),   ~
                                qtys(4), qtys(5), qtys(6), stds()
L11490:         FMT POS(62), CH(3), CH(6), POS(89), 5*PD(14,4),          ~
                                          POS(137), 13*PD(14,4)
            if str$ >  str$(2,2) then end_report
            if lot$ <= lot$(2,1) or lot$ > lot$(2,2) then report_loop
                qtys(7) = round(qtys(1) * qtys(6), 2)
                for x%  = 1% to 7%
                     call "CONVERT" (qtys(x%), 2.2, qtys$(x%))
                     ttls(x%) =  ttls(x%) + qtys(x%)
                next x%
                if line% > 55% then gosub page_heading
                print using L12140, str$, lot$, qtys$(1), qtys$(2),       ~
                                   qtys$(3), qtys$(5), qtys$(6), qtys$(7)
                line% = line% + 1%
                goto report_loop

        end_report
            if line% > 55% then gosub page_heading
            for x% = 1% to 7%
                call "CONVERT" (ttls(x%), 2.2, qtys$(x%))
            next x%
            print using L12170
            print using L12200, qtys$(1), qtys$(2), qtys$(3),             ~
                               qtys$(5), qtys$(7)
            print : print "** END OF REPORT **"
            close printer
            call "SETPRNT" ("HNY030", " ", 0%, 1%)
            return


        page_heading
            page% = page% + 1%
            line% = 8%
            print page
            print using L11960, date$, runtime$, company$
            print using L11990, page%
            print using L12020, rptdescr$
            print
            print using L12050
            print using L12080
            print using L12110
            return


L11960: %RUN DATE: ######## ########            #########################~
        ~###################################               HNYQDISP-HNY030

L11990: %                                                       INVENTORY~
        ~ QUANTITIES LISTING                                   PAGE: ###

L12020: %################################################################~
        ~#################################################################

L12050: %                               LOT                              ~
        ~                                    ON-HAND

L12080: %                        STORE NUMBER     ON-HAND   OPEN S.O.    ~
        ~ON-ORDER   QTY IN QC  VALUE EACH   VALUATION

L12110: %                        ----- ------ ----------- ----------- ---~
        ~-------- ----------- ----------- -----------

L12140: %                         ###  ###### ########### ########### ###~
        ~######## ########### ########### ###########

L12170: %                                     ----------- ----------- ---~
        ~-------- -----------             -----------

L12200: %                        * TOTALS *   ########### ########### ###~
        ~######## ###########             ###########


        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads the Next Screen's Worth of Data.                    *~
            *************************************************************
        load_screen
            init (" ") dsply$()
            l% = 0%
                                                           /* (PAR000)  */
L30090:     call "PLOWNEXT" (#2, plowkey$, 45%, f1%(2))
            if f1%(2) = 1% then L30140
L30110:         l% = l% + 1%
                dsply$(l%) = "** END OF DATA **     " & conv$ & " " &    ~
                        uoms$ & " (Stocking) = 1 " & uomp$ & " (Pricing)"
                return
L30140:     get #2 using L30160, str$, lot$, oh, bo, oo, cm, qc,          ~
                                each, stds(), mthd$
L30160:         FMT POS(62), CH(3), CH(6), POS(89), 5*PD(14,4),          ~
                    POS(137), 13*PD(14,4), POS(423), CH(1)
            if str$ > str$(2,2) then L30110         /* DONE   */
                                                           /* (PAR000) */
            if nozero% = 1%  and  oh + bo + oo + cm + qc = 0 then L30090


            if lot$ <= lot$(2,1) or lot$ > lot$(2,2) then L30090
                l% = l% + 1%  :  ext = 0
                ext = round(each * oh, 2)
                dsply$(l%) = str(str$) & " " & lot$
                if oh <> 0 then                                          ~
                    call "CONVERT" (oh, 2.2, str(dsply$(l%),12,10))
                if bo <> 0 then                                          ~
                    call "CONVERT" (bo, 2.2, str(dsply$(l%),23,10))
                if oo <> 0 then                                          ~
                    call "CONVERT" (oo, 2.2, str(dsply$(l%),34,10))
                if qc <> 0 then                                          ~
                    call "CONVERT" (qc, 2.2, str(dsply$(l%),45,10))
                call "CONVERT" (each  , 2.2, str(dsply$(l%),56,10))
                if oh <> 0 then                                          ~
                    call "CONVERT" (ext, 2.2, str(dsply$(l%),67,11))
                method$(l%) = mthd$
                if l% = 15% then return else goto L30090


        load_totals
            init (" ") dsply$(1)
            t1, t2, t3, t4, t6 = 0
            plowkey1$ = str(part$,,45) & str(str$(2,1)) & hex(00)
                                                           /* (PAR000) */
L30490:     call "PLOWNEXT" (#2, plowkey1$, 45%, f1%(2%))
            if f1%(2) = 0% then L30660
                get #2 using L30530, str$, lot$, oh, bo, oo, cm, qc,      ~
                                                             each, stds()
L30530:              FMT POS(62), CH(3), CH(6), POS(89), 5*PD(14,4),     ~
                         POS(137), 13*PD(14,4)
                if str$ >  str$(2,2) then L30660         /* DONE   */
                if lot$ <= lot$(2,1) or lot$ > lot$(2,2) then L30490
                     t1 = t1 + oh
                     t2 = t2 + bo
                     t3 = t3 + oo
                     t4 = t4 + qc
                     t6 = t6 + round(oh * each, 2)
                     goto L30490

L30660:     dsply$(1%) = "*TOTALS*"
            call "CONVERT" (t1 , 2.2, str(dsply$(1%),12,10))
            call "CONVERT" (t2 , 2.2, str(dsply$(1%),23,10))
            call "CONVERT" (t3 , 2.2, str(dsply$(1%),34,10))
            call "CONVERT" (t4 , 2.2, str(dsply$(1%),45,10))
            call "CONVERT" (t6 , 2.2, str(dsply$(1%),67,11))
            return



        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101                                  /* Display Screen   */

L40080:     accept                                                       ~
               at (01,02), "Display Part Quantity Information",          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), fac(hex(ac)), hdr$                   , ch(79),~
                                                                         ~
               at (05,02), fac(dfac$( 1)), dsply$( 1)           , ch(79),~
               at (06,02), fac(dfac$( 2)), dsply$( 2)           , ch(79),~
               at (07,02), fac(dfac$( 3)), dsply$( 3)           , ch(79),~
               at (08,02), fac(dfac$( 4)), dsply$( 4)           , ch(79),~
               at (09,02), fac(dfac$( 5)), dsply$( 5)           , ch(79),~
               at (10,02), fac(dfac$( 6)), dsply$( 6)           , ch(79),~
               at (11,02), fac(dfac$( 7)), dsply$( 7)           , ch(79),~
               at (12,02), fac(dfac$( 8)), dsply$( 8)           , ch(79),~
               at (13,02), fac(dfac$( 9)), dsply$( 9)           , ch(79),~
               at (14,02), fac(dfac$(10)), dsply$(10)           , ch(79),~
               at (15,02), fac(dfac$(11)), dsply$(11)           , ch(79),~
               at (16,02), fac(dfac$(12)), dsply$(12)           , ch(79),~
               at (17,02), fac(dfac$(13)), dsply$(13)           , ch(79),~
               at (18,02), fac(dfac$(14)), dsply$(14)           , ch(79),~
               at (19,02), fac(dfac$(15)), dsply$(15)           , ch(79),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               at (23,48), fac(lfac$    ), str$(1,1)            , ch(05),~
               at (23,57), fac(lfac$    ), str$(1,2)            , ch(05),~
               at (24,48), fac(lfac$    ), lot$(1,1)            , ch(06),~
               at (24,57), fac(lfac$    ), lot$(1,2)            , ch(06),~
                                                                         ~
                   keys(pfkeys$),  key (keyhit%)

               if keyhit% <> 13 then L40480
                  call "MANUAL" ("HNYQDISP")
                  goto L40080

L40480:        if keyhit% <> 15 then L40520
                  call "PRNTSCRN"
                  goto L40080

L40520:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf_main_screen
           inpmessage$ = "Position Cursor and Press PF-12 to see" &      ~
                         " Breakdown of Inventory Values."
                                                            /* (PAR000) */
           if file$ = "INVPOOL" then      /* #3 TEST */                  ~
           inpmessage$ = "Pos Cursor & Press: RETURN for Pool Info; " &  ~
                         "PF-9 for Locations; PF-12 for Costs."
           init (hex(86)) dfac$(), lfac$
           pf$(1) = "(1)Start Over   !( 8)Display Totals  --Selection C"&~
                    "riteria---  !(13)Instructions"
           pf$(2) = "(2)First (5)Next!(10)Change Display  Stores: XXX  "&~
                    "  - XXX     !(15)Print Screen"
           pf$(3) = "(6)Ignore 0 Lots!(14)Print Report      Lots: XXXXX"&~
                    "X - XXXXXX  !(16)Exit Display"
           pfkeys$ = hex(0102ffff0506ff08090aff0c0d0e0f10ffffff00)
           if nozero% = 1% then str(pf$(3),,16) = "(6)Show 0 Lots"
           return

        set_pf_criteria
           init (hex(8c)) dfac$()
           init (hex(81)) lfac$
           pf$(1) = "(1)Start Over   !    Display Totals  --Selection C"&~
                    "riteria---  !(13)Instructions"
           pf$(2) = "                !    Change Display  Stores: XXX  "&~
                    "  - XXX     !(15)Print Screen"
           pf$(3) = "                !    Print Report      Lots: XXXXX"&~
                    "X - XXXXXX  !(16)Exit Display"
           pfkeys$ = hex(01ffffffffffffffffffffff0dfe0f10ffffff00)
           str(pf$(opt%),21,1) = hex(84)
           str(pf$(opt%),37,1) = hex(8c)
           return

        set_pf_totals
           inpmessage$ = "Press (RETURN) to Continue with Display."
           init (hex(9c)) dfac$()  :  dfac$(1) = hex(84)
           init (hex(84)) lfac$
           pf$(1) = "                !    Display Totals  --Selection C"&~
                    "riteria---  !(13)Instructions"
           pf$(2) = "                !                    Stores: XXXxx"&~
                    "  - XXXxx   !(15)Print Screen"
           pf$(3) = "                !                      Lots: XXXXX"&~
                    "X - XXXXXX  !(16)Exit Display"
           pfkeys$ = hex(01ffffffffffffffffffffff0dfe0f10ffffff00)
           str(pf$(1%),21,1) = hex(84)
           str(pf$(1%),37,1) = hex(8c)
           return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        costs_screen                               /* Display Screen   */
            gosub set_pf_costs
            get #2 using L41090, stds()
L41090:         FMT POS(145), 12*PD(14,4)
            init (" ") costs$()
            for x% = 1% to 12%
                put str(costs$(x%)) using L41160, x%, bid$(x%),           ~
                                                 bdescr$(x%), stds(x%)
                if stds(x%) = 0 and bid$(x%) = " " then costs$(x%) = " "
            next x%
L41160:         %##  ##########  #################### -###,###.####

L41180:     accept                                                       ~
               at (01,02), "Display Part Quantity Information",          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,15), "Store:", fac(hex(84)), str(dsply$(c%),1,3),  ~
               at (04,27), "Lot:"  , fac(hex(84)), str(dsply$(c%),5,6),  ~
               at (04,40), "Cost Set: ", fac(hex(84)), setid$,           ~
                                                                         ~
               at (06,15), fac(hex(ac)), hdr2$,                          ~
               at (07,15), fac(dfac$( 1)), costs$( 1)           , ch(50),~
               at (08,15), fac(hex(84))  , costs$( 2)           , ch(50),~
               at (09,15), fac(hex(84))  , costs$( 3)           , ch(50),~
               at (10,15), fac(hex(84))  , costs$( 4)           , ch(50),~
               at (11,15), fac(hex(84))  , costs$( 5)           , ch(50),~
               at (12,15), fac(hex(84))  , costs$( 6)           , ch(50),~
               at (13,15), fac(hex(84))  , costs$( 7)           , ch(50),~
               at (14,15), fac(hex(84))  , costs$( 8)           , ch(50),~
               at (15,15), fac(hex(84))  , costs$( 9)           , ch(50),~
               at (16,15), fac(hex(84))  , costs$(10)           , ch(50),~
               at (17,15), fac(hex(84))  , costs$(11)           , ch(50),~
               at (18,15), fac(hex(84))  , costs$(12)           , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                   keys(pfkeys$),  key (keyhit%)

               if keyhit% <> 13 then L41530
                  call "MANUAL" ("HNYQDISP")
                  goto L41180

L41530:        if keyhit% <> 15 then L41570
                  call "PRNTSCRN"
                  goto L41180

L41570:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf_costs
           inpmessage$ = "Press RETURN to Continue...."
           pf$(1) = "                                                  "&~
                    "            !(13)Instructions"
           pf$(2) = "                                                  "&~
                    "            !(15)Print Screen"
           pf$(3) = "                                                  "&~
                    "            !(16)Exit Display"
           pfkeys$ = hex(ffffffffffffffffffffffff0dff0f10ffffff00)
           return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test Store and Lot Ranges entered.                        *~
            *************************************************************

        deffn'151
            errormsg$ = " "

*        First Test Store Range
            call "TESTRNGE" (str$(1,1), str$(1,2), str$(2,1), str$(2,2), ~
                             errormsg$)
            if errormsg$ = " " then L50160
                errormsg$ = "Stores: " & errormsg$
                return

L50160
*        Next Test Lot Range
            if lot$(1,1) <> " " or lot$(1,2) <> " " then L50210
                lot$(2,1) = hex(20202020201f) /* Lot blank is a valid  */
                lot$(2,2) = " "               /* lot number.           */
                return
L50210:     call "TESTRNGE" (lot$(1,1), lot$(1,2), lot$(2,1), lot$(2,2), ~
                             errormsg$)
            if errormsg$ = " " then return
                errormsg$ = "Lots: " & errormsg$
                return

        exit_program

            end
