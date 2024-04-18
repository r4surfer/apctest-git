        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  FFFFF   AAA    AAA   M   M  TTTTT  RRRR   PPPP   TTTTT   *~
            *  F      A   A  A   A  MM MM    T    R   R  P   P    T     *~
            *  FFFF   AAAAA  AAAAA  M M M    T    RRRR   PPPP     T     *~
            *  F      A   A  A   A  M   M    T    R   R  P        T     *~
            *  F      A   A  A   A  M   M    T    R   R  P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FAAMTRPT - Calculates and displays Alternate Minimum Taxes*~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/27/88 ! Original                                 ! TLJ *~
            * 08/28/89 ! Corrected range selection on Asset Code  ! MJB *~
            *          !  and Date formatting.                    !     *~
            * 01/11/93 ! Page 0 Facs, Header, & End Report Time.  ! RJH *~
            * 08/06/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            accumdepr(3),                /* Accumulative Depreciation  */~
            adj$1,                       /* AMT Adjustment Y or N      */~
            amt$1,                       /* AMT Flag                   */~
            amt_class$5,                 /* AMT Class Life             */~
            amt_group$10,                /* AMT Group Code             */~
            atype$1,                     /* Asset Type                 */~
            assetcd$10,                  /* Asset Code                 */~
            assdescr$30,                 /* Asset Description          */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bonus(3),                    /* Bonus Depreciation         */~
            cnv$(3)1,                    /* Proration Convention Method*/~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            curdepr(3),                  /* Current Depreciation       */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dispdate$8,                  /* Disposal Date              */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            expded(3),                   /* Expense Deduction          */~
            fed_meth$14,                 /* Federal Book Method        */~
            fmasset$10,                  /* Assets                     */~
            fmdate$10,                   /* Date Range                 */~
            group$(3)10,                 /* Table Group Code           */~
            hiasset$10,                  /* Assets                     */~
            hidate$10,                   /* Date Range                 */~
            i$(24)80,                    /* Screen Image               */~
            inserv$(3)8,                 /* Date put in service        */~
            inpmessage$79,               /* Informational Message      */~
            itc_red(3),                  /* ITC Reduction              */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            life$(3)5,                   /* Life/Recovery in years     */~
            line2$79,                    /* Screen Line #2             */~
            loasset$10,                  /* Assets                     */~
            lodate$10,                   /* Date Range                 */~
            meth$(3)2,                   /* Depr. Method               */~
            oldt_descr$30,               /* Old Property Type Descr.   */~
            old_type$1,                  /* Old Property Type          */~
            orgbasis(3),                 /* Original Basis             */~
            otherred(3),                 /* Other Reduction            */~
            per(3),                      /* Percent for Percent Method */~
            percent$6,                   /* Percent for Percent Method */~
            period$(3)2,                 /* Period Put in Service      */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            pref$1,                      /* AMT Preference Y or N      */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rpttitle$60,                 /* Report Title               */~
            ptype$1,                     /* Property Type              */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            salv(3),                     /* Salvage Value              */~
            swyr$(3)4,                   /* Year Swithed               */~
            time$8,                      /* System Time                */~
            toasset$10,                  /* Assets                     */~
            todate$10,                   /* Date Range                 */~
            type$7,                      /* Property Types             */~
            ttype$7,                     /* Property Types             */~
            userid$3                     /* Current User Id            */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

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
            * #01 ! FAMASTER ! Fixed Assets Master File                 *~
            * #02 ! SYSFILE2 ! Caelus Management System Information     *~
            * #03 ! FATABLE  ! Fixed Assets ACRS Depreciation Tables Fi *~
            * #08 ! WORKFILE ! Workfile for sorting report              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "FAMASTER",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  120, keylen =  10,                     ~
                        alt key  1, keypos =   58, keylen =   1, dup     ~

            select #02, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos = 1,    keylen = 20                       ~

            select #03, "FATABLE",                                       ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    1, keylen =  18                      ~

            select #08, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  12

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, u3%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "Fixed Assets Alternative Minim" &               ~
                        "um Tax Report                 "
            call "STRING" addr("CT", rpttitle$, 60%)

            str(columnttl$, 1) = "Beginning Code"
            str(columnttl$,27) = "Ending Code"

            str(line2$,62) = "FAAMTRPT: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  5%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10201
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10201:               if keyhit% =  6% then type$ = "RPLAEXO"
                      if keyhit% =  7% then type$ = "       "
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       generate_report
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% then fieldnr% = 1%
            if fieldnr% > 5% then fieldnr% = 5%
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  6% then type$ = "RPLAEXO"
                  if keyhit%  =  7% then type$ = "       "
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *           E X T R A C T   R E P O R T   D A T A           *~
            *-----------------------------------------------------------*~
            * Data Extraction section for report.                       *~
            *************************************************************
        extract_data:
            call "SHOSTAT" ("Extracting Data From FAMASTER")
            call "WORKOPEN" (#08, "IO   ", 1000%, f2%(2))
                if f2%(2) = 1% then inputmode      /* ERROR */
            call "DATUFMTC" (fmdate$)
            call "DATUFMTC" (todate$)
            init(hex(00)) plowkey$  :  plowkey$ = loasset$

        get_next
            call "PLOWNEXT" (#01, plowkey$, 0%, f1%(1))
                if f1%(1) = 0% then extract_done
            get #01, using L35030, meth$(), inserv$(), ptype$, dispdate$, ~
                                  assetcd$, assdescr$, atype$, group$(), ~
                                  life$(), cnv$(), per(), swyr$(),       ~
                                  orgbasis(), salv(), itc_red(),         ~
                                  bonus(), expded(), otherred(),         ~
                                  accumdepr(), curdepr(), period$(),     ~
                                  amt$, amt_group$, amt_class$, amt_adj, ~
                                  amt_cur, amt_accum

            if assetcd$ > hiasset$ then extract_done


*       ** Test Record Against Specified Criteria
            if amt$ = " " then get_next
            if adj$ = "N" and amt$ = "1" then get_next
            if pref$ = "N" and amt$ = "2" then get_next
            if pos(type$=ptype$) = 0% then get_next

            extracted% = 1%
*       ** Get Federal Depreciation
                 call "FADEPR" ( assetcd$, ptype$, dispdate$, inserv$(2),~
                                 group$(2), cnv$(2), period$(2),         ~
                                 life$(2), meth$(2), per(2), swyr$(2),   ~
                                 orgbasis(2), salv(2), itc_red(2),       ~
                                 bonus(2),                               ~
                                 expded(2), otherred(2), accumdepr(2),   ~
                                 curdepr(2), fmdate$, todate$, ret%, #03)
*       ** Get AMT Depreciation
                 if amt$ = "1" then amtmeth$ = "7" else amtmeth$ = "S"
                 call "FADEPR" ( assetcd$, ptype$, dispdate$, inserv$(2),~
                                 amt_group$, cnv$(2), period$(2),        ~
                                 amt_class$, amtmeth$, per(2), swyr$(2), ~
                                 amt_adj, salv(2), itc_red(2), bonus(2), ~
                                 expded(2), otherred(2), amt_accum,      ~
                                 amt_cur, fmdate$, todate$, ret%, #03)

                 fed_basis = orgbasis(2) - itc_red(2) - expded(2)        ~
                                                 - bonus(2) - otherred(2)

                 call "CONVERT" (per(2), 2.2, percent$)
                 call "STRING" addr("LJ", percent$, 6%)

                                       /* Key = AMT$, PTYPE$, ASSETCD$ */
                 write #08 using L13700,amt$, ptype$, assetcd$, assdescr$,~
                                       meth$(2), life$(2), amt_class$,   ~
                                       fed_basis, amt_adj, curdepr(2),   ~
                                       amt_cur, accumdepr(2), amt_accum, ~
                                       percent$, group$(2), amt_group$

L13700:                FMT             CH(1), CH(1), CH(10), CH(30),     ~
                                       CH(2), CH(5), CH(5),              ~
                                       PD(14,4), PD(14,4), PD(14,4),     ~
                                       PD(14,4), PD(14,4), PD(14,4),     ~
                                       CH(6), CH(10), CH(10)
                goto get_next

        extract_done
            call "DATFMTC" (fmdate$)
            call "DATFMTC" (todate$)
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20140,         /* Assets                 */~
                              L20190,         /* Date Range             */~
                              L20230,         /* Property Types         */~
                              L20280,         /* AMT Adjustments        */~
                              L20320          /* AMT Tax Pref.          */
            return

L20140: REM Def/Enable Assets                      FMASSET$
            if fmasset$            = " " then                            ~
               fmasset$            = "ALL"
            return

L20190: REM Def/Enable Date Range                  FMDATE$
            if fmdate$ <> " " and fmdate$ <> blankdate$ then L20220
              readkey$ = "SWITCHS.FA "
              call "READ101" (#02, readkey$, f1%(2))
              if f1%(2) = 0% then L20220
                get #2 using L20215, fmdate$, todate$
L20215:             FMT    POS(21), CH(8), CH(8)
                call "DATFMTC" (fmdate$)
                call "DATFMTC" (todate$)
L20220:     return

L20230: REM Def/Enable Property Types              TYPE$
            if type$ = " " then type$ = "RPLAEXO"
            return


L20280: REM Def/Enable AMT Adjustments             ADJ$
            if adj$ = " " then adj$ = "Y"
            return

L20320: REM Def/Enable AMT Tax Preference          PREF$
            if pref$ = " " then pref$ = "Y"
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Asset Range                                            ",~
         "Enter Date Range                                             ",~
         "R-Real, P-Personal, L-Low Inc., A-Amortized, E-Lease, X-Rental,~
        ~ O-Other.",                                                      ~
         "Print AMT for Assets Using TABLES, Enter Y (yes) or N (no).  ",~
         "Print AMT Tax Preference Assets, Enter Y (yes) or N (no).    "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      fmasset$, fmdate$, pref$, type$, adj$,             ~
                      hiasset$, hidate$,                                 ~
                      loasset$, lodate$,                                 ~
                      toasset$, todate$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
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
            goto inputmode

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************
        generate_report:
            extracted% = 0%
            gosub extract_data
            if extracted% = 1% then L30200
                call "FILEBGON" (#08)
L30100:         u3% = 2%
                call "ASKUSER" ( u3%, " ", "Report Not Printed.",        ~
                       "The specified criteria was not met.",            ~
                       "Press RETURN to EDIT  -OR-  PF(1) to STARTOVER.")
            if u3% <> 1% then L30180
                gosub L30160
L30160:         return clear all
                goto inputmode
L30180:     if u3% = 0% then editpg1
                goto L30100
L30200:     call "SHOSTAT" ("Printing Alternate Minimum Tax Report")
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("F/A008", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
*       ** CALL "DATFMTC" (FMDATE$)
*       ** CALL "DATFMTC" (TODATE$)
            init(" ") old_amt$

            plowkey$ = all(hex(00))
        get_next_rec
            call "PLOWNEXT" (#08, plowkey$, 0%, f1%(8))
            if f1%(8) = 0% then end_report
            get  #08 using L30370, amt$, ptype$, assetcd$, assdescr$,     ~
                                  meth$(2), life$(2), amt_class$,        ~
                                  fed_basis, amt_adj, fed_cur,           ~
                                  amt_cur, fed_accum, amt_accum,         ~
                                  percent$, group$(2), amt_group$

L30370:         FMT CH(1), CH(1), CH(10), CH(30), CH(2), CH(5), CH(5),   ~
                    PD(14,4), PD(14,4), PD(14,4), PD(14,4), PD(14,4),    ~
                    PD(14,4), CH(6), CH(10), CH(10)



            gosub get_property_type_descr
            gosub get_depr_methods_descr
            depr_adj = fed_cur - amt_cur
            gosub new_property_type   /* Print Total, Start New Page */
            gosub print_lines
*       ** Calculate Subtotals */
            adj_subtot = adj_subtot + depr_adj         /* AMT Adjust.  */
            f_acc_subtot = f_acc_subtot + fed_accum    /* Accum. Depr  */
            a_acc_subtot = a_acc_subtot + amt_accum
            f_cur_subtot = f_cur_subtot + fed_cur      /* Current Depr */
            a_cur_subtot = a_cur_subtot + amt_cur
            f_bas_subtot = f_bas_subtot + fed_basis    /* Adj. Basis   */
            a_bas_subtot = a_bas_subtot + amt_adj
*       ** Calculate Totals    */
            adj_tot = adj_tot + depr_adj               /* AMT Adjust.  */
            f_acc_tot = f_acc_tot + fed_accum          /* Accum. Depr  */
            a_acc_tot = a_acc_tot + amt_accum
            f_cur_tot = f_cur_tot + fed_cur            /* Current Depr */
            a_cur_tot = a_cur_tot + amt_cur
            f_bas_tot = f_bas_tot + fed_basis          /* Adj. Basis   */
            a_bas_tot = a_bas_tot + amt_adj
            goto get_next_rec

        end_report
            assetcd$ = " "
            gosub print_totals
            if lcntr% > 53% then gosub page_head
            print skip(2)
            time$ = " "  :  call "TIME" (time$)
            print using L64990, time$       /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            call "FILEBGON" (#08)
            goto inputmode

        get_property_type_descr:
                 ptype_descr$ = " "
                 if ptype$ = "R" then ptype_descr$ = "Real"
                 if ptype$ = "P" then ptype_descr$ = "Personal"
                 if ptype$ = "L" then ptype_descr$ = "Low Income"
                 if ptype$ = "A" then ptype_descr$ = "Amortized"
                 if ptype$ = "E" then ptype_descr$ = "Lease"
                 if ptype$ = "X" then ptype_descr$ = "Rental"
                 if ptype$ = "O" then ptype_descr$ = "Other "
                 return

        get_depr_methods_descr:
                 convert meth$(2) to meth%
                 if meth% = 2% then fed_meth$ = "S/L"
                 if meth% = 3% then fed_meth$ = percent$ & "% DB"
                 if meth% = 4% then fed_meth$ = percent$ & "% DBwSW"
                 if meth% = 5% then fed_meth$ = "SUM YRS DIGTS"
                 if meth% = 6% or meth% = 7% then fed_meth$=group$(2)
                 if meth% = 8% then fed_meth$ = percent$ & "%"

                 if amt$ = "1" then amt_meth$ = amt_group$               ~
                               else amt_meth$ = "S/L"
                 return

        new_property_type:
           if old_amt$ = amt$ then L31170
             if old_amt$ <> " " then gosub print_totals
             old_type$ = " "
             old_amt$ = amt$
             /* Initialize Totals    */
             f_bas_tot, f_cur_tot, f_acc_tot,                            ~
             a_bas_tot, a_cur_tot, a_acc_tot = 0
             gosub page_head
L31170:    if old_type$ = ptype$ then L31240
              if old_type$ <> " " then gosub print_subtotals
              old_type$ = ptype$
              oldt_descr$ = ptype_descr$
              /* Initialize Subtotals */
              adj_subtot, f_bas_subtot, f_cur_subtot, f_acc_subtot,      ~
              a_bas_subtot, a_cur_subtot, a_acc_subtot, adj_subtot = 0
L31240:    return

        print_lines:
            if lcntr% > 56% then gosub page_head
            print
            print using L60430, ptype_descr$, assetcd$, assdescr$,        ~
                               fed_meth$, life$(2), fed_basis, fed_accum,~
                               fed_cur

            print using L60460,                         amt_meth$,        ~
                               amt_class$, amt_adj, amt_accum, amt_cur,  ~
                               depr_adj
            lcntr% = lcntr% + 3%
            return

        print_subtotals:
            if lcntr% > 51% then gosub page_head
            print
            print using L60550
            print using L60590, oldt_descr$, f_bas_subtot, f_acc_subtot,  ~
                               f_cur_subtot
            print using L60620, a_bas_subtot, a_acc_subtot,               ~
                                              a_cur_subtot, adj_subtot
            print
            lcntr% = lcntr% + 5%
            return

        print_totals:
            gosub print_subtotals
            if lcntr% > 52% then gosub page_head
            print
            print using L60661
            if old_amt$ = "1"                                            ~
                  then print using L60721, f_bas_tot, f_acc_tot, f_cur_tot~
                  else print using L60700, f_bas_tot, f_acc_tot, f_cur_tot
            print using L60730, a_bas_tot, a_acc_tot, a_cur_tot,          ~
                               adj_tot
            lcntr% = lcntr% + 4%
            return

        page_head:             /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            if pcntr% = 0% then gosub print_params
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "FAAMTRPT"
            print using L60120, rpttitle$, pcntr%
            print
            if old_amt$ <> "1" then L34091 /* TABLES Headings          */
              print using L60170, fmdate$, todate$
              print
              print using L60240
              print using L60340
              goto L34110
L34091:     if old_amt$ <> "2" then L34130 /* Tax Preference Headings */
            print using L60200, fmdate$, todate$
            print
            print using L60290
            print using L60390

L34110:     print using L60500
L34130:     lcntr% = 8%
            return

        print_params:          /* Print Page Zero */
            print page
L34175:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L34195
                str(i$(), i%, 1%) = hex(20)
                goto L34175
L34195:     print using L60070, date$, time$, company$, "FAAMTRPT"
            print using L60120, rpttitle$, pcntr%
            print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            pcntr% = pcntr% + 1%
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: FAMASTER                          */~
            3*CH(2),        /* Depreciation Method                     */~
            3*CH(8),        /* Date Placed In Service                  */~
            XX(9),          /* Depreciation Expense G/L Account Code   */~
            XX(9),          /* Accumulated Depreciation G/L Account Cod*/~
            XX(9),          /* Asset G/L Account Code                  */~
            CH(1),          /* Property Type                           */~
            CH(8),          /* Disposal Date                           */~
            XX(8),          /* Purchase Date                           */~
            XX(30),         /* Location of fixed asset                 */~
            XX(15),         /* Identification Code                     */~
            CH(10),         /* Asset Code                              */~
            CH(30),         /* Fixed Asset Description Line #1         */~
            XX(30),         /* Fixed Asset Description Line #2         */~
            CH(1),          /* Fixed Asset Record Type Code            */~
            XX(8),          /* Purchase Price   PD(14,4)               */~
            XX(8),          /* Investment Tax Credit Taken  PD(14,4)   */~
            XX(8),          /* Disposal Price  PD(14,4)                */~
            XX(30),         /* Disposal Description                    */~
            XX(9),          /* Vendor Code                             */~
            XX(16),         /* Invoice Number                          */~
            3*CH(10),       /* General Group Name for categorizing tabl*/~
            3*CH(5),        /* Asset Life (Years)                      */~
            3*CH(1),        /* Proration Convention                    */~
            3*PD(14,4),     /* Percent for Declining Balance or Percent*/~
            3*CH(4),        /* Year depr method was switch to straight */~
            3*PD(14,4),     /* Original Basis                          */~
            3*PD(14,4),     /* Salvage Value                           */~
            3*PD(14,4),     /* ITC Basis Reduction                     */~
            3*PD(14,4),     /* Bonus Depreciation Taken                */~
            3*PD(14,4),     /* Expense Deduction Taken                 */~
            3*PD(14,4),     /* Other Basis Reduction                   */~
            XX(150),        /* Other Basis Reduction Descr. 3*CH(50)   */~
            3*PD(14,4),     /* Accumulated Depreciation                */~
            3*PD(14,4),     /* Current Depreciation                    */~
            3*CH(2),        /* In service period.                      */~
            CH(1),          /* AMT Flag                                */~
            CH(10),         /* AMT Group                               */~
            CH(5),          /* AMT Class Life                          */~
            PD(14,4),       /* AMT Adjusted Basis                      */~
            PD(14,4),       /* AMT Current Depr                        */~
            PD(14,4)        /* AMT Accum Depr                          */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40190,         /* Assets            */   ~
                                L40190,         /* Date Range        */   ~
                                L40190,         /* Property Types    */   ~
                                L40190,         /* AMT Adjustments   */   ~
                                L40190          /* AMT Tax Pref.     */
              goto L40220

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40190:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40220:     accept                                                       ~
               at (01,02),                                               ~
                  "Input Report Selection Criteria",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), "Assets",                                     ~
               at (07,30), fac(lfac$( 1)), fmasset$             , ch(10),~
               at (07,56), fac(lfac$( 1)), toasset$             , ch(10),~
                                                                         ~
               at (08,02), "Fiscal Year",                                ~
               at (08,30), fac(lfac$( 2)), fmdate$              , ch(10),~
               at (08,56), fac(lfac$( 2)), todate$              , ch(10),~
                                                                         ~
               at (09,02), "Property Types",                             ~
               at (09,30), fac(lfac$( 3)), type$                , ch(07),~
                                                                         ~
               at (10,02), "AMT Using TABLES",                           ~
               at (10,30), fac(lfac$( 4)), adj$                 , ch(01),~
                                                                         ~
               at (11,02), "AMT Tax Preferences",                        ~
               at (11,30), fac(lfac$( 5)), pref$                , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40590
                  call "MANUAL" ("FAAMTRPT") : goto L40220

L40590:        if keyhit% <> 15 then L40620
                  call "PRNTSCRN" : goto L40220

L40620:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40890     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (6)Allow All           " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (7)Allow None          " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ff0607ffffffffff0dff0f1000)
            if fieldnr% = 1% then L40770
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40770:     if fieldnr% > 1% then L40790
                str(pf$(1),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40790:     if fieldnr% = 3% then L40830
              str(pf$(2),18,26) = " "  :  str(pfkeys$, 6,1) = hex(ff)
              str(pf$(3),18,26) = " "  :  str(pfkeys$, 7,1) = hex(ff)
              goto L40870
L40830:     if str(type$,1,7) <> "       " then L40850
              str(pf$(3),18,26) = " "  :  str(pfkeys$, 7,1) = hex(ff)
L40850:     if str(type$,1,7) <> "RPLAEXO" then L40870
              str(pf$(2),18,26) = " "  :  str(pfkeys$, 6,1) = hex(ff)
L40870:     return

L40890: if fieldnr% > 0% then L41070  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L41070:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (6)Allow All           " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (7)Allow None          " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffff0607ffffffffff0dff0fff00)
            if fieldnr% = 3% then L41145
              str(pf$(2),18,26) = " "  :  str(pfkeys$, 6,1) = hex(ff)
              str(pf$(3),18,26) = " "  :  str(pfkeys$, 7,1) = hex(ff)
              goto L41150
L41145:     if str(type$,1,7) <> "       " then L41148
              str(pf$(3),18,26) = " "  :  str(pfkeys$, 7,1) = hex(ff)
              goto L41150
L41148:     if str(type$,1,7) <> "RPLAEXO" then L41150
              str(pf$(2),18,26) = " "  :  str(pfkeys$, 6,1) = hex(ff)
L41150:     return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50150,         /* Assets                 */~
                              L50220,         /* Date Range             */~
                              L50410,         /* Property Types         */~
                              L50540,         /* AMT Adjustments        */~
                              L50580          /* AMT Tax Pref.          */
            return

L50150: REM Test for Assets                       FMASSET$
            call "TESTRNGE"                                              ~
                  (fmasset$            , toasset$            ,           ~
                   loasset$            , hiasset$            ,           ~
                   errormsg$)
            return

L50220: REM Test for Date Range                   FMDATE$
            if fmdate$ <> "ALL" then L50260
                todate$ = " "
                return
L50260:     call "DATEOKC" (fmdate$, u5%, errormsg$)
                if errormsg$ <> " " then return
            if todate$ = " " or todate$ = blankdate$ then todate$ = fmdate$
            call "DATEOKC" (todate$, u6%, errormsg$)
                if errormsg$ <> " " then return
            if u5% <= u6% then return
                errormsg$ = "Invalid date range selection.  "  &         ~
                            "Please re-enter"
                return






L50410: REM Test for Property Types               TYPE$
            str(ttype$,1,7) = str(type$,1,7)
            type$ = "       "
            if pos(ttype$="R") <> 0% then str(type$,1,1) = "R"
            if pos(ttype$="P") <> 0% then str(type$,2,1) = "P"
            if pos(ttype$="L") <> 0% then str(type$,3,1) = "L"
            if pos(ttype$="A") <> 0% then str(type$,4,1) = "A"
            if pos(ttype$="A") <> 0% then str(type$,4,1) = "A"
            if pos(ttype$="E") <> 0% then str(type$,5,1) = "E"
            if pos(ttype$="X") <> 0% then str(type$,6,1) = "X"
            if pos(ttype$="O") <> 0% then str(type$,7,1) = "O"
            return

L50540: REM Test for AMT Adjustments              ADJ$
            if adj$<>"Y" and adj$<>"N" then errormsg$="Enter Y or N."
            return

L50580: REM Test for AMT Tax Preference           PREF$
            if pref$<>"Y" and pref$<>"N" then errormsg$="Enter Y or N."
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                  ########: F/A0~
        ~08

*       * Header Line 2
L60120: %                                     ###########################~
        ~#################################                      PAGE:  ###~
        ~##

*       * Header Line 3, TABLES
L60170: %Alternate Minimum Tax Comparison Using TABLES (Federal Book / AM~
        ~T) for the FISCAL YEAR ########## to ##########

*       * Header Line 3, Tax Preference
L60200: %Alternate Minimum Tax Comparison for TAX PREFERENCE ITEMS (Feder~
        ~al Book / AMT) for the FISCAL YEAR ########## to ##########

*       * Header Line 4, TABLES
L60240: %PROPERTY                                                        ~
        ~    RECOVERY      ADJUSTED       ACCUM.      CURRENT     ADJUSTME~
        ~NT

*       * Header Line 4, TAX PREFERENCE
L60290: %PROPERTY                                             DEPR. METHO~
        ~D       LIFE      ADJUSTED       ACCUM.      CURRENT     ADJUSTME~
        ~NT

*       * Header Line 5, TABLES
L60340: %TYPE       ASSET CODE ASSET DESCRIPTION              TABLE GROUP~
        ~      /CLASS         BASIS        DEPR.        DEPR.    (Fed - AM~
        ~T)

*       * Header Line 5, TAX PREFERENCE
L60390: %TYPE       ASSET CODE ASSET DESCRIPTION              or TABLE GR~
        ~OUP   /CLASS         BASIS        DEPR.        DEPR.    (Fed - AM~
        ~T)

L60430: %########## ########## ############################## ###########~
        ~###    ##### ##########.## #########.## #########.##

L60460: %                                                     ###########~
        ~###    ##### ##########.## #########.## #########.##  -#########.~
        ~##

L60500: %__________ __________ ______________________________ ___________~
        ~___ ________ _____________ ____________ ____________ ____________~
        ~__

*       * Subtotal Line
L60550: %                                                                ~
        ~             _____________ ____________ ____________ ____________~
        ~__

L60590: %**  SUBTOTALS For Property Type ##########                      ~
        ~             $#########.## $########.## $########.##

L60620: %                                                                ~
        ~             $#########.## $########.## $########.## -$#########.~
        ~##

*       * Total Lines
L60661: %                                                                ~
        ~             _____________ ____________ ____________ ____________~
        ~__

L60700: %*** AMT TOTALS for TAX PREFERENCE ITEMS                         ~
        ~             $#########.## $########.## $########.##

L60721: %*** AMT TOTALS using TABLES                                     ~
        ~             $#########.## $########.## $########.##

L60730: %                                                                ~
        ~             $#########.## $########.## $########.## -$#########.~
        ~##

*       * Report Title for page 0
        %############################################################

L64990:       %                            * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   @  ########  * * * * * * * * * *

        REM THISPROGRAMWASGENERATEDBYGENRPPGMAPROPRIETRYPRODUCTOFCAELUS**~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
