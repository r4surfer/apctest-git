        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCEMP2B                             *~
            *  Creation Date     - 04/17/96                             *~
            *  Last Modified Date- 04/17/96                             *~
            *  Description       - This Program provides Employee       *~
            *                      Benefits Entry/Update and Inquiry.   *~
            *                                                           *~
            *  Special Comments  - Subroutine for APCEMPED              *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/17/96 ! New Program for (APC) - Last Mod Date    ! JBF *~
            * 05/03/20 ! CR2490 New employee number size          ! RDB *~
            *************************************************************

        sub "APCEMP2B"   (e_no$)         /* Employee No.               */

        dim e_no$5,                      /* Employee No.               */~
            e_med_ins$2,                 /* Employee Medical Insurance */~
            e_med_date$8,                /* Med. Ins. Effective Date   */~
            e_den_ins$2,                 /* Employee Dental  Insurance */~
            e_den_date$8,                /* Den. Ins. Effective Date   */~
            e_dis_ins$2,                 /* Employee Disab.  Insurance */~
            e_dis_date$8,                /* Dis. Ins. Effective Date   */~
            e_lif_ben_name$30,           /* Life Ins. Beneficiary      */~
            e_lif_relation$10,           /* Beneficiary Relation       */~
            e_lif_date$8,                /* Life Ins. Effective Date   */~
            e_lif_add_name$30,           /* Add. Life Ins. Beneficiary */~
            e_lif_add_rel$10,            /* Add. Beneficiary Relation  */~
            e_lif_add_date$8,            /* Add. Life Effective Date   */~
            e_cu_total$8,                /* Credit Union Total Check   */~
            e_cu_savings$8,              /* Credit Union Savings       */~
            e_cu_checking$8,             /* Credit Union Checking      */~
            e_401k_total$2,              /* 401(K) Total % (Employee)  */~
            e_401k_total_co$2,           /* 401(K) Total % (Company)   */~
            e_401k_fixed$2,              /* 401(K) Fixed % (Employee)  */~
            e_401k_fixed_co$2,           /* 401(K) Fixed % (Company)   */~
            e_401k_bond$2,               /* 401(K) Bond  % (Employee)  */~
            e_401k_bond_co$2,            /* 401(K) Bond  % (Company)   */~
            e_401k_stock$2,              /* 401(K) Stock % (Employee)  */~
            e_401k_stock_co$2,           /* 401(K) Stock % (Company)   */~
            e_401k_us$2,                 /* 401(K) U.S.  % (Employee)  */~
            e_401k_us_co$2,              /* 401(K) U.S.  % (Company)   */~
            e_uw_caring$1,               /* United Way Caring Share    */~
            e_pay_deduct(3),             /* Other P/R Deductions (3)   */~
            e_pay_deduct_re$(3%)25,      /* Other P/R Ded. Reason (3)  */~
            e_userid$3,                  /* Userid of Last Modifier    */~
            e_mod_dte$8,                 /* Last Mod. Date             */~
            e_fill2$6,                   /* APCEMPBN Filler Area       */~
            med_cost$8,                  /* Medical Ins. Cost (Display)*/~
            den_cost$8,                  /* Dental  Ins. Cost (Display)*/~
            dis_cost$8,                  /* Disability Ins. Cost (Disp)*/~
            lif_ins$9,                   /* Life Insurance Value       */~
            lif_add_ins$9,               /* Life Insurance Add. Value  */~
            cu_xmas$8,                   /* Credit Union (Christmas)   */~
            cu_vac$8,                    /* Credit Union (Vacation)    */~
            cu_other$8,                  /* Credit Union (Other)       */~
            uw_amount$8,                 /* United Way Contribution    */~
            uniform$8,                   /* Uniform P/R Deduction      */~
            pay_deduct$(3%)8,            /* Other P/R Deductions (3)   */~
            line13_2$5,                  /* Total (Display)            */~
            line13_3$5,                  /* Fixed (Display)            */~
            line13_4$4,                  /* Bond  (Display)            */~
            line13_5$5,                  /* Stock (Display)            */~
            line13_6$8,                  /* US Stock (Display)         */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$60,                 /* Error message              */~
            i$(24%)80,                   /* Detail Line(10) Array Area */~
            inpmessage$79,               /* Informational Message      */~
            cursor%(2%),                 /*                            */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            progid$18,                   /* Screen Line #2 Program ID  */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(4%),                                                     ~
            f1%(4%),                                                     ~
            fs%(4%),                                                     ~
            rslt$(4%)20

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "01.00.00 04/17/96 Employee Benefits Inq./Update   "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #04 ! APCEMPBN ! Employee Benefits Detail File            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #4,  "APCEMPBN",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos = 1,    keylen =  5                       ~

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#4,  fs%(4%),  f2%(4%), 100%, rslt$(4%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            progid$   = "APCEMP2B: " & str(cms2v$,,8)
            line13_2$ = "Total"
            line13_3$ = "Fixed"
            line13_4$ = "Bond"
            line13_5$ = "Stock"
            line13_6$ = "US Stock"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        inputmode
            gosub initialize_variables

            fieldnr% = 1%: goto L11230
            for fieldnr% = 1% to 1%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                     if enabled% = 0% then L10260

L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit% =  1% then gosub startover
                     if keyhit% <> 4% then L10230 /* Previous Field */
L10160:                   fieldnr% = max(1%, fieldnr% - 1%)

                          gosub'051(fieldnr%)
                               if enabled% = 1% then L10130
                               if fieldnr% = 1% then L10100
                               goto L10160

L10230:              if keyhit% = 16% and fieldnr% = 1% then exit_sub
                     if keyhit% <> 0%           then L10130

L10260:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " "        then L10130

            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                if keyhit% =  1%       then gosub startover
                if keyhit% = 14%       then gosub dataput
                if keyhit% = 16%       then exit_sub
                if keyhit% <> 0%       then editpg1

            fieldnr% = 1%
            if fieldnr% = lastfieldnr% then editpg1

            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled% = 0%       then editpg1

L11190:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit% =  1%       then gosub startover
                if keyhit% <> 0%       then L11190

L11230:     gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " "    then L11190
                lastfieldnr% = fieldnr%
            goto editpg1

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************
        deffn'051(fieldnr%)
            enabled% = 1%
        return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, e_med_ins$, e_med_date$,   ~
                e_den_ins$, e_den_date$, e_dis_ins$, e_dis_date$,        ~
                e_lif_ben_name$, e_lif_relation$, e_lif_date$,           ~
                e_lif_add_name$, e_lif_add_rel$, e_lif_add_date$,        ~
                e_cu_total$, e_cu_savings$, e_cu_checking$, e_401k_bond$,~
                e_401k_total$, e_401k_total_co$, e_401k_fixed_co$,       ~
                e_401k_fixed$, e_401k_bond_co$, e_401k_stock_co$,        ~
                e_401k_stock$, e_401k_us$, e_401k_us_co$, e_uw_caring$,  ~
                e_pay_deduct_re$(), e_userid$, e_mod_dte$, e_fill2$

            e_med_cost       = 0.00 : e_den_cost = 0.00
            e_dis_cost       = 0.00 : e_lif_ins  = 0.00
            e_cu_xmas        = 0.00 : e_cu_vac   = 0.00
            e_lif_add_ins    = 0.00 : e_cu_other = 0.00
            e_uw_amount      = 0.00 : e_uniform  = 0.00
            med_cost         = round(e_med_cost, 2)
            convert med_cost        to med_cost$,      pic(####0.00)

            den_cost         = round(e_den_cost, 2)
            convert den_cost        to den_cost$,      pic(####0.00)

            dis_cost         = round(e_dis_cost, 2)
            convert dis_cost        to dis_cost$,      pic(####0.00)

            lif_ins          = round(e_lif_ins, 2)
            convert lif_ins         to lif_ins$,       pic(#####0.00)

            lif_add_ins      = round(e_lif_add_ins, 2)
            convert lif_add_ins     to lif_add_ins$,   pic(#####0.00)

            cu_xmas          = round(e_cu_xmas, 2)
            convert cu_xmas         to cu_xmas$,       pic(#,##0.00)

            cu_vac           = round(e_cu_vac, 2)
            convert cu_vac          to cu_vac$,        pic(#,##0.00)

            cu_other         = round(e_cu_other, 2)
            convert cu_other        to cu_other$,      pic(#,##0.00)

            uniform          = round(e_uniform, 2)
            convert uniform         to uniform$,       pic(####0.00)

            uw_amount        = round(e_uw_amount, 2)
            convert uw_amount       to uw_amount$,     pic(####0.00)

            pay_deduct       = round(e_pay_deduct(1%), 2)
            convert pay_deduct      to pay_deduct$(1%), pic(####0.00)

            pay_deduct       = round(e_pay_deduct(2%), 2)
            convert pay_deduct      to pay_deduct$(2%), pic(####0.00)

            pay_deduct       = round(e_pay_deduct(3%), 2)
            convert pay_deduct      to pay_deduct$(3%), pic(####0.00)

            edit% = 0%
            return

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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            rec% = 0%
            read #4,key = e_no$, eod goto L30972

            get #4, using L35030,                                         ~
                     e_no$,              /* Employee No.               */~
                     e_med_ins$,         /* Employee Medical Insurance */~
                     e_med_cost,         /* Employee Medical Ins. Cost */~
                     e_med_date$,        /* Med. Ins. Effective Date   */~
                     e_den_ins$,         /* Employee Dental  Insurance */~
                     e_den_cost,         /* Employee Dental  Ins. Cost */~
                     e_den_date$,        /* Den. Ins. Effective Date   */~
                     e_dis_ins$,         /* Employee Disab.  Insurance */~
                     e_dis_cost,         /* Employee Disab.  Ins. Cost */~
                     e_dis_date$,        /* Dis. Ins. Effective Date   */~
                     e_lif_ins,          /* Employee Life Ins. Value   */~
                     e_lif_ben_name$,    /* Life Ins. Beneficiary      */~
                     e_lif_relation$,    /* Beneficiary Relation       */~
                     e_lif_date$,        /* Life Ins. Effective Date   */~
                     e_lif_add_ins,      /* Add. Life Ins. Value       */~
                     e_lif_add_name$,    /* Add. Life Ins. Beneficiary */~
                     e_lif_add_rel$,     /* Add. Beneficiary Relation  */~
                     e_lif_add_date$,    /* Add. Life Effective Date   */~
                     e_cu_total$,        /* Credit Union Total Check   */~
                     e_cu_savings$,      /* Credit Union Savings       */~
                     e_cu_checking$,     /* Credit Union Checking      */~
                     e_cu_xmas,          /* Credit Union Christmas     */~
                     e_cu_vac,           /* Credit Union Vacation      */~
                     e_cu_other,         /* Credit Union Other Accounts*/~
                     e_401k_total$,      /* 401(K) Total % (Employee)  */~
                     e_401k_total_co$,   /* 401(K) Total % (Company)   */~
                     e_401k_fixed$,      /* 401(K) Fixed % (Employee)  */~
                     e_401k_fixed_co$,   /* 401(K) Fixed % (Company)   */~
                     e_401k_bond$,       /* 401(K) Bond  % (Employee)  */~
                     e_401k_bond_co$,    /* 401(K) Bond  % (Company)   */~
                     e_401k_stock$,      /* 401(K) Stock % (Employee)  */~
                     e_401k_stock_co$,   /* 401(K) Stock % (Company)   */~
                     e_401k_us$,         /* 401(K) U.S.  % (Employee)  */~
                     e_401k_us_co$,      /* 401(K) U.S.  % (Company)   */~
                     e_uw_caring$,       /* United Way Caring Share    */~
                     e_uw_amount,        /* United Way Contribution    */~
                     e_uniform,          /* Uniform Deduction          */~
                     e_pay_deduct(),     /* Other P/R Deductions (3)   */~
                     e_pay_deduct_re$(), /* Other P/R Ded. Reason (3)  */~
                     e_userid$,          /* Userid of Last Modifier    */~
                     e_mod_dte$,         /* Last Mod. Date             */~
                     e_fill2$            /* APCEMPBN Filler Area       */

            med_cost        = round(e_med_cost, 2)
            convert med_cost       to med_cost$,      pic(####0.00)

            den_cost        = round(e_den_cost, 2)
            convert den_cost       to den_cost$,      pic(####0.00)

            dis_cost        = round(e_dis_cost, 2)
            convert dis_cost       to dis_cost$,      pic(####0.00)

            lif_ins         = round(e_lif_ins, 2)
            convert lif_ins        to lif_ins$,       pic(#####0.00)

            lif_add_ins     = round(e_lif_add_ins, 2)
            convert lif_add_ins    to lif_add_ins$,   pic(#####0.00)

            cu_xmas         = round(e_cu_xmas, 2)
            convert cu_xmas        to cu_xmas$,       pic(#,##0.00)

            cu_vac          = round(e_cu_vac, 2)
            convert cu_vac         to cu_vac$,        pic(#,##0.00)

            cu_other        = round(e_cu_other, 2)
            convert cu_other       to cu_other$,      pic(#,##0.00)

            uniform         = round(e_uniform, 2)
            convert uniform        to uniform$,       pic(####0.00)

            uw_amount       = round(e_uw_amount, 2)
            convert uw_amount      to uw_amount$,     pic(####0.00)

            pay_deduct      = round(e_pay_deduct(1%), 2)
            convert pay_deduct     to pay_deduct$(1%), pic(####0.00)

            pay_deduct      = round(e_pay_deduct(2%), 2)
            convert pay_deduct     to pay_deduct$(2%), pic(####0.00)

            pay_deduct      = round(e_pay_deduct(3%), 2)
            convert pay_deduct     to pay_deduct$(3%), pic(####0.00)

            if e_med_date$  = " "    then L30855
            call "DATEFMT" (e_med_date$)

L30855:     if e_den_date$  = " "    then L30882
            call "DATEFMT" (e_den_date$)

L30882:     if e_dis_date$  = " "    then L30909
            call "DATEFMT" (e_dis_date$)

L30909:     if e_lif_date$  = " "    then L30936
            call "DATEFMT" (e_lif_date$)

L30936:     if e_lif_add_date$ = " " then L30963
            call "DATEFMT" (e_lif_add_date$)

L30963:     rec% = 1%
L30972:     return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "SHOSTAT" ( "Updating APCEMPPR Data" )
            read #4,hold,key = e_no$, eod goto L31180

            delete #4

L31180:     convert med_cost$      to e_med_cost,      data goto L31200

L31200:     convert den_cost$      to e_den_cost,      data goto L31220

L31220:     convert dis_cost$      to e_dis_cost,      data goto L31240

L31240:     convert lif_ins$       to e_lif_ins,       data goto L31260

L31260:     convert lif_add_ins$   to e_lif_add_ins,   data goto L31280

L31280:     convert cu_xmas$       to e_cu_xmas,       data goto L31300

L31300:     convert cu_vac$        to e_cu_vac,        data goto L31320

L31320:     convert cu_other$      to cu_other,        data goto L31340

L31340:     convert uniform$       to e_uniform,       data goto L31360

L31360:     convert uw_amount$     to e_uw_amount,     data goto L31380

L31380:     convert pay_deduct$(1%) to e_pay_deduct(1%), data goto L31400

L31400:     convert pay_deduct$(2%) to e_pay_deduct(2%), data goto L31420

L31420:     convert pay_deduct$(3%) to e_pay_deduct(3%), data goto L31440

L31440:     if e_med_date$     = " " then L31470
            call "DATUNFMT" (e_med_date$)

L31470:     if e_den_date$     = " " then L31500
            call "DATUNFMT" (e_den_date$)

L31500:     if e_dis_date$     = " " then L31530
            call "DATUNFMT" (e_dis_date$)

L31530:     if e_lif_date$     = " " then L31560
            call "DATUNFMT" (e_lif_date$)

L31560:     if e_lif_add_date$ = " " then L31590
            call "DATUNFMT" (e_lif_add_date$)

L31590:     e_userid$  = userid$
            e_mod_dte$ = date$
            put #4, using L35030,                                         ~
                     e_no$,              /* Employee No.               */~
                     e_med_ins$,         /* Employee Medical Insurance */~
                     e_med_cost,         /* Employee Medical Ins. Cost */~
                     e_med_date$,        /* Med. Ins. Effective Date   */~
                     e_den_ins$,         /* Employee Dental  Insurance */~
                     e_den_cost,         /* Employee Dental  Ins. Cost */~
                     e_den_date$,        /* Den. Ins. Effective Date   */~
                     e_dis_ins$,         /* Employee Disab.  Insurance */~
                     e_dis_cost,         /* Employee Disab.  Ins. Cost */~
                     e_dis_date$,        /* Dis. Ins. Effective Date   */~
                     e_lif_ins,          /* Employee Life Ins. Value   */~
                     e_lif_ben_name$,    /* Life Ins. Beneficiary      */~
                     e_lif_relation$,    /* Beneficiary Relation       */~
                     e_lif_date$,        /* Life Ins. Effective Date   */~
                     e_lif_add_ins,      /* Add. Life Ins. Value       */~
                     e_lif_add_name$,    /* Add. Life Ins. Beneficiary */~
                     e_lif_add_rel$,     /* Add. Beneficiary Relation  */~
                     e_lif_add_date$,    /* Add. Life Effective Date   */~
                     e_cu_total$,        /* Credit Union Total Check   */~
                     e_cu_savings$,      /* Credit Union Savings       */~
                     e_cu_checking$,     /* Credit Union Checking      */~
                     e_cu_xmas,          /* Credit Union Christmas     */~
                     e_cu_vac,           /* Credit Union Vacation      */~
                     e_cu_other,         /* Credit Union Other Accounts*/~
                     e_401k_total$,      /* 401(K) Total % (Employee)  */~
                     e_401k_total_co$,   /* 401(K) Total % (Company)   */~
                     e_401k_fixed$,      /* 401(K) Fixed % (Employee)  */~
                     e_401k_fixed_co$,   /* 401(K) Fixed % (Company)   */~
                     e_401k_bond$,       /* 401(K) Bond  % (Employee)  */~
                     e_401k_bond_co$,    /* 401(K) Bond  % (Company)   */~
                     e_401k_stock$,      /* 401(K) Stock % (Employee)  */~
                     e_401k_stock_co$,   /* 401(K) Stock % (Company)   */~
                     e_401k_us$,         /* 401(K) U.S.  % (Employee)  */~
                     e_401k_us_co$,      /* 401(K) U.S.  % (Company)   */~
                     e_uw_caring$,       /* United Way Caring Share    */~
                     e_uw_amount,        /* United Way Contribution    */~
                     e_uniform,          /* Uniform Deduction          */~
                     e_pay_deduct(),     /* Other P/R Deductions (3)   */~
                     e_pay_deduct_re$(), /* Other P/R Ded. Reason (3)  */~
                     e_userid$,          /* Userid of Last Modifier    */~
                     e_mod_dte$,         /* Last Mod. Date             */~
                     e_fill2$            /* APCEMPBN Filler Area       */

            write #4, eod goto L32230

            if e_med_date$     = " " then L32100
            call "DATEFMT" (e_med_date$)

L32100:     if e_den_date$     = " " then L32130
            call "DATEFMT" (e_den_date$)

L32130:     if e_dis_date$     = " " then L32160
            call "DATEFMT" (e_dis_date$)

L32160:     if e_lif_date$     = " " then L32190
            call "DATEFMT" (e_lif_date$)

L32190:     if e_lif_add_date$ = " " then L32250
            call "DATEFMT" (e_lif_add_date$)

            goto L32250
L32230:     call "SHOSTAT" ( "ERROR - Unable to Update APCEMPBN"  ) : stop

L32250: return clear all
        goto editpg1

        REM *************************************************************~
            *               F O R M A T    S T A T E M E N T S          *~
            *************************************************************
L35030:         FMT  CH(05),             /* Employee No.               */~
                     CH(02),             /* Employee Medical Insurance */~
                     PD(14,4),           /* Medical Ins. Cost          */~
                     CH(08),             /* Med. Ins. Effective Date   */~
                     CH(02),             /* Employee Dental  Insurance */~
                     PD(14,4),           /* Dental  Ins. Cost          */~
                     CH(08),             /* Den. Ins. Effective Date   */~
                     CH(02),             /* Employee Disab.  Insurance */~
                     PD(14,4),           /* Disability Ins. Cost       */~
                     CH(08),             /* Dis. Ins. Effective Date   */~
                     PD(14,4),           /* Life Insurance Value       */~
                     CH(30),             /* Life Ins. Beneficiary      */~
                     CH(10),             /* Beneficiary Relation       */~
                     CH(08),             /* Life Ins. Effective Date   */~
                     PD(14,4),           /* Life Insurance Add. Value  */~
                     CH(30),             /* Add. Life Ins. Beneficiary */~
                     CH(10),             /* Add. Beneficiary Relation  */~
                     CH(08),             /* Add. Life Effective Date   */~
                     CH(08),             /* Credit Union Total Check   */~
                     CH(08),             /* Credit Union Savings       */~
                     CH(08),             /* Credit Union Checking      */~
                     PD(14,4),           /* Credit Union (Christmas)   */~
                     PD(14,4),           /* Credit Union (Vacation)    */~
                     PD(14,4),           /* Credit Union (Other)       */~
                     CH(02),             /* 401(K) Total % (Employee)  */~
                     CH(02),             /* 401(K) Total % (Company)   */~
                     CH(02),             /* 401(K) Fixed % (Employee)  */~
                     CH(02),             /* 401(K) Fixed % (Company)   */~
                     CH(02),             /* 401(K) Bond  % (Employee)  */~
                     CH(02),             /* 401(K) Bond  % (Company)   */~
                     CH(02),             /* 401(K) Stock % (Employee)  */~
                     CH(02),             /* 401(K) Stock % (Company)   */~
                     CH(02),             /* 401(K) U.S.  % (Employee)  */~
                     CH(02),             /* 401(K) U.S.  % (Company)   */~
                     CH(01),             /* United Way Caring Share    */~
                     PD(14,4),           /* United Way Contribution    */~
                     PD(14,4),           /* Uniform P/R Deduction      */~
                     3*PD(14,4),         /* Other P/R Deductions (3)   */~
                     3*CH(25),           /* Other P/R Ded. Reason (3)  */~
                     CH(03),             /* Userid of Last Modifier    */~
                     CH(08),             /* Last Mod. Date             */~
                     CH(06)              /* APCEMPBN Filler Area       */~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************
        deffn'101(fieldnr%, edit%)
            inpmessage$ = edtmessage$
            gosub set_pf1

            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            if fieldnr% > 0% then gosub L40140
            goto accept_screen
                lfac$(fieldnr%) = hex(80)  :  return   /* Up / Low   */
L40140:         lfac$(fieldnr%) = hex(81)  :  return   /* Upper Only */
                lfac$(fieldnr%) = hex(82)  :  return   /* Numeric    */

        accept_screen
            accept                                                       ~
                at (01,02),                                              ~
                   "Employee Master Benefits Information"       ,        ~
                                                                         ~
                at (01,66), "Today:"                            ,        ~
                at (01,73), fac(hex(8c)),   date$               , ch(08),~
                                                                         ~
                at (02,02), fac(hex(94)),   errormsg$           , ch(60),~
                at (02,63), fac(hex(8c)),   progid$             , ch(18),~
                                                                         ~
                at (03,02), "Medical   :"                       ,        ~
                at (03,14), fac(lfac$(1%)), e_med_ins$          , ch(02),~
                at (03,21), "Cost:"                             ,        ~
                at (03,30), fac(lfac$(1%)), med_cost$           , ch(08),~
                at (03,41), "Effective Date:"                   ,        ~
                at (03,57), fac(lfac$(1%)), e_med_date$         , ch(08),~
                                                                         ~
                at (04,02), "Dental    :"                       ,        ~
                at (04,14), fac(lfac$(1%)), e_den_ins$          , ch(02),~
                at (04,21), "Cost:"                             ,        ~
                at (04,30), fac(lfac$(1%)), den_cost$           , ch(08),~
                at (04,41), "Effective Date:"                   ,        ~
                at (04,57), fac(lfac$(1%)), e_den_date$         , ch(08),~
                                                                         ~
                at (05,02), "Disability:"                       ,        ~
                at (05,14), fac(lfac$(1%)), e_dis_ins$          , ch(02),~
                at (05,21), "Cost:"                             ,        ~
                at (05,30), fac(lfac$(1%)), dis_cost$           , ch(08),~
                at (05,41), "Effective Date:"                   ,        ~
                at (05,57), fac(lfac$(1%)), e_dis_date$         , ch(08),~
                                                                         ~
                at (06,02), "Life      -"                       ,        ~
                at (06,20), "Value:"                            ,        ~
                at (06,29), fac(lfac$(1%)), lif_ins$            , ch(09),~
                at (06,41), "Effective Date:"                   ,        ~
                at (06,57), fac(lfac$(1%)), e_lif_date$         , ch(08),~
                                                                         ~
                at (07,02), " Benefic. :"                       ,        ~
                at (07,14), fac(lfac$(1%)), e_lif_ben_name$     , ch(30),~
                at (07,47), "Relation:"                         ,        ~
                at (07,57), fac(lfac$(1%)), e_lif_relation$     , ch(10),~
                                                                         ~
                at (08,02), "Add. Life -"                       ,        ~
                at (08,20), "Value:"                            ,        ~
                at (08,29), fac(lfac$(1%)), lif_add_ins$        , ch(09),~
                at (08,41), "Effective Date:"                   ,        ~
                at (08,57), fac(lfac$(1%)), e_lif_add_date$     , ch(08),~
                                                                         ~
                at (09,02), " Benefic. :"                       ,        ~
                at (09,14), fac(lfac$(1%)), e_lif_add_name$     , ch(30),~
                at (09,47), "Relation:"                         ,        ~
                at (09,57), fac(lfac$(1%)), e_lif_add_rel$      , ch(10),~
                                                                         ~
                at (11,02), "C/U Deduct-"                       ,        ~
                at (11,14), "Total:"                            ,        ~
                at (11,21), fac(lfac$(1%)), cu_total$           , ch(08),~
                at (11,33), "Savings :"                         ,        ~
                at (11,43), fac(lfac$(1%)), cu_savings$         , ch(08),~
                at (11,55), "Checking:"                         ,        ~
                at (11,65), fac(lfac$(1%)), cu_checking$        , ch(08),~
                                                                         ~
                at (12,14), "Xmas :"                            ,        ~
                at (12,21), fac(lfac$(1%)), cu_xmas$            , ch(08),~
                at (12,33), "Vacation:"                         ,        ~
                at (12,43), fac(lfac$(1%)), cu_vac$             , ch(08),~
                at (12,55), "Other   :"                         ,        ~
                at (12,65), fac(lfac$(1%)), cu_other$           , ch(08),~
                                                                         ~
                at (14,02), "401(K) %  -"                       ,        ~
                at (14,14), fac(hex(ac)),   line13_2$           , ch(05),~
                at (14,21), fac(hex(ac)),   line13_3$           , ch(05),~
                at (14,28), fac(hex(ac)),   line13_4$           , ch(04),~
                at (14,34), fac(hex(ac)),   line13_5$           , ch(05),~
                at (14,41), fac(hex(ac)),   line13_6$           , ch(08),~
                at (14,53), "United Way:"                       ,        ~
                at (14,65), fac(lfac$(1%)), uw_amount$          , ch(08),~
                at (14,74), "C/S:"                              ,        ~
                at (14,79), fac(lfac$(1%)), e_uw_caring$        , ch(01),~
                                                                         ~
                at (15,02), "  Employee:"                       ,        ~
                at (15,16), fac(lfac$(1%)), e_401k_total$       , ch(02),~
                at (15,23), fac(lfac$(1%)), e_401k_fixed$       , ch(02),~
                at (15,29), fac(lfac$(1%)), e_401k_bond$        , ch(02),~
                at (15,36), fac(lfac$(1%)), e_401k_stock$       , ch(02),~
                at (15,44), fac(lfac$(1%)), e_401k_us$          , ch(02),~
                                                                         ~
                at (16,02), "  Company :"                       ,        ~
                at (16,16), fac(lfac$(1%)), e_401k_total_co$    , ch(02),~
                at (16,23), fac(lfac$(1%)), e_401k_fixed_co$    , ch(02),~
                at (16,29), fac(lfac$(1%)), e_401k_bond_co$     , ch(02),~
                at (16,36), fac(lfac$(1%)), e_401k_stock_co$    , ch(02),~
                at (16,44), fac(lfac$(1%)), e_401k_us_co$       , ch(02),~
                                                                         ~
                at (18,02), "P/R:"                              ,        ~
                at (18,07), fac(lfac$(1%)), pay_deduct$(1%)     , ch(08),~
                at (18,17), fac(lfac$(1%)), e_pay_deduct_re$(1%), ch(25),~
                at (18,44), fac(lfac$(1%)), pay_deduct$(2%)     , ch(08),~
                at (18,54), fac(lfac$(1%)), e_pay_deduct_re$(2%), ch(25),~
                                                                         ~
                at (19,07), fac(lfac$(1%)), pay_deduct$(3%)     , ch(08),~
                at (19,17), fac(lfac$(1%)), e_pay_deduct_re$(3%), ch(25),~
                at (19,44), "Uniform:"                          ,        ~
                at (19,54), fac(lfac$(1%)), uniform$            , ch(08),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1)              , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2)              , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3)              , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                if keyhit% <> 15% then L41330
                     call "PRNTSCRN"

                     goto accept_screen
L41330:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

            return

        set_pf1
        if edit% = 2% then L41520     /*  Input Mode             */
            pf$(1) = "(1)Start Over                            " &       ~
                     "                                      "
            pf$(2) = "                                         " &       ~
                     "                      (15)Print Screen"
            pf$(3) = "                                         " &       ~
                     "                      (16)Return      "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)

            if fieldnr% = 1% then L41500
              str(pf$(3%),64%)     = " " : str(pfkeys$,16%,1%) = hex(ff)
L41500:     return

L41520: if fieldnr% > 0% then L41620  /*  Edit Mode - Select Field */
            pf$(1) = "(1)Start Over                            " &       ~
                     "                      (14)Update Data "
            pf$(2) = "                                         " &       ~
                     "                      (15)Print Screen"
            pf$(3) = "                                         " &       ~
                     "                      (16)Return      "
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
                                     /*  Edit Mode - Enabled    */
L41620:     pf$(1) = "(1)Start Over                            " &       ~
                     "                                      "
            pf$(2) = "                                         " &       ~
                     "                      (15)Print Screen"
            pf$(3) = "                                         " &       ~
                     "                                      "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0fff00)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************
        deffn'151(fieldnr%)
            errormsg$ = " "
            gosub field_edits

        return

        field_edits
        REM Employee No.                        E_NO$
            if edit% = 2%           then L50180
            gosub dataload

        return

L50180: REM Medical Insurance                   E_MED_INS$
            if e_med_ins$ = "EO"    then L50270
            if e_med_ins$ = "EC"    then L50270
            if e_med_ins$ = "ES"    then L50270
            if e_med_ins$ = "EF"    then L50270
            if e_med_ins$ = "NN"    then L50270
            errormsg$    = "(Error) - Invalid MEDICAL Ins. ?"
        return

L50270: REM Medical Insurance Cost              E_MED_COST
            convert med_cost$ to med_cost,  data goto L50330

            convert med_cost  to med_cost$, pic (####0.00)

            goto edit_med_date
L50330:     errormsg$ = "(Error) - Invalid Medical COST ?"
        return

        REM Medical Insurance Effective Date    E_MED_DATE$
        edit_med_date
            if e_med_date$     = " " then edit_den_ins
            call "DATEOK" (e_med_date$, date%, errormsg$)

            if date% <> 0%          then edit_den_ins
            init (" ") e_med_date$
        return

        REM Dental Insurance                    E_DEN_INS$
        edit_den_ins
            if e_den_ins$ = "EO"    then L50550
            if e_den_ins$ = "EC"    then L50550
            if e_den_ins$ = "ES"    then L50550
            if e_den_ins$ = "EF"    then L50550
            if e_den_ins$ = "NN"    then L50550
            errormsg$    = "(Error) - Invalid DENTAL Ins. ?"
        return

L50550: REM Dental Insurance Cost               E_DEN_COST
            convert den_cost$ to den_cost,  data goto L50610

            convert den_cost  to den_cost$, pic (####0.00)

            goto edit_den_date
L50610:     errormsg$ = "(Error) - Invalid Dental COST ?"
        return

        REM Dental Insurance Effective Date     E_DEN_DATE$
        edit_den_date
            if e_den_date$     = " " then edit_dis_ins
            call "DATEOK" (e_den_date$, date%, errormsg$)

            if date% <> 0%          then edit_dis_ins
            init (" ") e_den_date$
        return

        REM Disability Insurance                E_DIS_INS$
        edit_dis_ins
            if e_dis_ins$ = "EO"    then L50830
            if e_dis_ins$ = "EC"    then L50830
            if e_dis_ins$ = "ES"    then L50830
            if e_dis_ins$ = "EF"    then L50830
            if e_dis_ins$ = "NN"    then L50830
            errormsg$     = "(Error) - Invalid DISABILITY Ins. ?"
        return

L50830: REM Disability Insurance Cost           E_DIS_COST
            convert dis_cost$ to dis_cost,  data goto L50890

            convert dis_cost  to dis_cost$, pic (####0.00)

            goto edit_dis_date
L50890:     errormsg$ = "(Error) - Invalid Disability COST ?"
        return

        REM Disability Insurance Effective Date E_DIS_DATE$
        edit_dis_date
            if e_dis_date$     = " " then edit_lif_ins
            call "DATEOK" (e_dis_date$, date%, errormsg$)

            if date% <> 0%          then edit_lif_ins
            init (" ") e_dis_date$
        return

        REM Life Insurance Value                E_LIF_INS$
        edit_lif_ins
            convert lif_ins$  to lif_ins,   data goto L51080

            convert lif_ins   to lif_ins$,  pic (#####0.00)

            goto edit_lif_date
L51080:     errormsg$ = "(Error) - Invalid Life Ins. VALUE ?"
        return

        REM Life Insurance Effective Date       E_LIF_DATE$
        edit_lif_date
            if e_lif_date$    = " " then edit_add_ins
            call "DATEOK" (e_lif_date$, date%, errormsg$)

            if date% <> 0%          then edit_add_ins
            init (" ") e_lif_date$
        return

        REM Additional Life Insurance Value     E_LIF_ADD_INS$
        edit_add_ins
            convert lif_add_ins$ to lif_add_ins,  data goto L51270

            convert lif_add_ins  to lif_add_ins$, pic (#####0.00)

            goto edit_add_date
L51270:     errormsg$ = "(Error) - Invalid Additional Life Ins. VALUE ?"
        return

        REM Add. Life Insurance Effective Date  E_LIF_ADD_DATE$
        edit_add_date
            if e_lif_add_date$ = " " then edit_cu_total
            call "DATEOK" (e_lif_add_date$, date%, errormsg$)

            if date% <> 0%           then edit_cu_total
            init (" ") e_lif_add_date$
        return

        REM Credit Union Total                  E_CU_TOTAL$
        edit_cu_total
            if cu_total$ = "NET     "  then edit_cu_savings
            convert cu_total$ to cu_total,  data goto L51470

            convert cu_total  to cu_total$, pic (#,##0.00)

            goto edit_cu_savings
L51470:     errormsg$ = "(Error) - Invalid Credit Union TOTAL ?"
        return

        REM Credit Union Savings                E_CU_SAVINGS$
        edit_cu_savings
            if cu_savings$ = "BALANCE "    then edit_cu_checking
            convert cu_savings$ to cu_savings,  data goto L51580

            convert cu_savings  to cu_savings$, pic (#,##0.00)

            goto edit_cu_checking
L51580:     errormsg$ = "(Error) - Invalid Credit Union SAVINGS ?"
        return

        REM Credit Union Checking               E_CU_CHECKING$
        edit_cu_checking
            if cu_checking$ = "BALANCE " then edit_cu_xmas
            convert cu_checking$ to cu_checking,  data goto L51690

            convert cu_checking  to cu_checking$, pic (#,##0.00)

            goto edit_cu_xmas
L51690:     errormsg$ = "(Error) - Invalid Credit Union CHECKING ?"
        return

        REM Credit Union Christmas              E_CU_XMAS$
        edit_cu_xmas
            convert cu_xmas$     to cu_xmas,      data goto L51790

            convert cu_xmas      to cu_xmas$,     pic (#,##0.00)

            goto edit_cu_vac
L51790:     errormsg$ = "(Error) - Invalid Credit Union XMAS ?"
        return

        REM Credit Union Vacation               E_CU_VAC$
        edit_cu_vac
            convert cu_vac$      to cu_vac,       data goto L51890

            convert cu_vac       to cu_vac$,      pic (#,##0.00)

            goto edit_cu_other
L51890:     errormsg$ = "(Error) - Invalid Credit Union VACATION ?"
        return

        REM Credit Union Other                  E_CU_OTHER$
        edit_cu_other
            convert cu_other$    to cu_other,     data goto L51990

            convert cu_other     to cu_other$,    pic (#,##0.00)

            goto edit_401k_total
L51990:     errormsg$ = "(Error) - Invalid Credit Union OTHER ?"
        return

        REM 401(K) Total                        E_401K_TOTAL$
        edit_401k_total
            convert e_401k_total$ to e_401k_total,  data goto L52090

            convert e_401k_total  to e_401k_total$, pic (##)

            goto edit_401k_fixed
L52090:     errormsg$ = "(Error) - Invalid 401(K) TOTAL ?"
        return

        REM 401(K) Fixed                        E_401K_FIXED$
        edit_401k_fixed
            convert e_401k_fixed$ to e_401k_fixed,  data goto L52190

            convert e_401k_fixed  to e_401k_fixed$, pic (##)

            goto edit_401k_bond
L52190:     errormsg$ = "(Error) - Invalid 401(K) FIXED ?"
        return

        REM 401(K) Bond                         E_401K_BOND$
        edit_401k_bond
            convert e_401k_bond$  to e_401k_bond,   data goto L52290

            convert e_401k_bond   to e_401k_bond$,  pic (##)

            goto edit_401k_stock
L52290:     errormsg$ = "(Error) - Invalid 401(K) BOND ?"
        return

        REM 401(K) Stock                        E_401K_STOCK$
        edit_401k_stock
            convert e_401k_stock$ to e_401k_stock,  data goto L52390

            convert e_401k_stock  to e_401k_stock$, pic (##)

            goto edit_401k_us
L52390:     errormsg$ = "(Error) - Invalid 401(K) STOCK ?"
        return

        REM 401(K) US Stock                     E_401K_US$
        edit_401k_us
            convert e_401k_us$    to e_401k_us,     data goto L52490

            convert e_401k_us     to e_401k_us$,    pic (##)

            goto edit_401k_total_co
L52490:     errormsg$ = "(Error) - Invalid 401(K) US STOCK ?"
        return

        REM 401(K) Total Company                E_401K_TOTAL_CO$
        edit_401k_total_co
            convert e_401k_total_co$ to e_401k_total_co,  data goto L52590

            convert e_401k_total_co  to e_401k_total_co$, pic (##)

            goto edit_401k_fixed_co
L52590:     errormsg$ = "(Error) - Invalid 401(K) Company TOTAL ?"
        return

        REM 401(K) Fixed Company                E_401K_FIXED_CO$
        edit_401k_fixed_co
            convert e_401k_fixed_co$ to e_401k_fixed_co,  data goto L52690

            convert e_401k_fixed_co  to e_401k_fixed_co$, pic (##)

            goto edit_401k_bond_co
L52690:     errormsg$ = "(Error) - Invalid 401(K) Company FIXED ?"
        return

        REM 401(K) Bond Company                 E_401K_BOND_CO$
        edit_401k_bond_co
            convert e_401k_bond_co$  to e_401k_bond_co,   data goto L52790

            convert e_401k_bond_co   to e_401k_bond_co$,  pic (##)

            goto edit_401k_stock_co
L52790:     errormsg$ = "(Error) - Invalid 401(K) Company BOND ?"
        return

        REM 401(K) Stock Company                E_401K_STOCK_CO$
        edit_401k_stock_co
            convert e_401k_stock_co$ to e_401k_stock_co,  data goto L52890

            convert e_401k_stock_co  to e_401k_stock_co$, pic (##)

            goto edit_401k_us_co
L52890:     errormsg$ = "(Error) - Invalid 401(K) Company STOCK ?"
        return

        REM 401(K) US Stock Company             E_401K_US_CO$
        edit_401k_us_co
            convert e_401k_us_co$    to e_401k_us_co,     data goto L52990

            convert e_401k_us_co     to e_401k_us_co$,    pic (##)

            goto edit_uw_caring
L52990:     errormsg$ = "(Error) - Invalid 401(K) Company US STOCK ?"
        return

        REM United Way Caring Share Flag        E_UW_CARING$
        edit_uw_caring
            if e_uw_caring$ = "Y"  then edit_uw_amount
            if e_uw_caring$ = "N"  then edit_uw_amount
            errormsg$    = "(Error) - Invalid CARING SHARE (Y/N) ?"
        return

        REM United Way Contribution Amount      E_UW_AMOUNT
        edit_uw_amount
            convert uw_amount$       to uw_amount,        data goto L53160

            convert uw_amount        to uw_amount$,       pic (####0.00)

            goto edit_uniform
L53160:     errormsg$ = "(Error) - Invalid United Way Amount ?"
        return

        REM Uniform Amount                      E_UNIFORM
        edit_uniform
            convert uniform$         to uniform,          data goto L53260

            convert uniform          to uniform$,         pic (####0.00)

            goto edit_pay_deduct1
L53260:     errormsg$ = "(Error) - Invalid UNIFORM Amount ?"
        return

        REM Other P/R Deductions (1)            E_PAY_DEDUCT(1)
        edit_pay_deduct1
            convert pay_deduct$(1%) to pay_deduct,       data goto L53360

            convert pay_deduct      to pay_deduct$(1%),  pic (####0.00)

            goto edit_pay_deduct2
L53360:     errormsg$ = "(Error) - Invalid P/R Deduction(1) ?"
        return

        REM Other P/R Deductions (2)            E_PAY_DEDUCT(2)
        edit_pay_deduct2
            convert pay_deduct$(2%)  to pay_deduct,      data goto L53460

            convert pay_deduct       to pay_deduct$(2%), pic (####0.00)

            goto edit_pay_deduct3
L53460:     errormsg$ = "(Error) - Invalid P/R Deduction (2) ?"
        return

        REM Other P/R Deductions (3)            E_PAY_DEDUCT(3)
        edit_pay_deduct3
            convert pay_deduct$(3%)  to pay_deduct,      data goto L53560

            convert pay_deduct       to pay_deduct$(3%), pic (####0.00)

            goto L53570
L53560:     errormsg$ = "(Error) - Invalid P/R Deduction (3) ?"
L53570: return

        REM *************************************************************~
            *           S p e c i a l   S u b r o u t i n e s           *~
            *************************************************************

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*
        exit_sub
            call "SHOSTAT" ("One Moment Please")
            close #4

        end
