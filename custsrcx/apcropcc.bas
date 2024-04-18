        REM *************************************************************~
            *      ( Special PF(9) Key to Turn on Debug Screen )        *~
            *   AAA   PPPP    CCC   RRRR    OOO   PPPP    CCC    CCC    *~
            *  A   A  P   P  C   C  R   R  O   O  P   P  C   C  C   C   *~
            *  AAAAA  PPPP   C      RRRR   O   O  PPPP   C      C       *~
            *  A   A  P      C   C  R   R  O   O  P      C   C  C   C   *~
            *  A   A  P       CCC   R  ER   OOO   P       CCC    CCC    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCROPCC - (A) Enter Valid Selection Criteria to find     *~
            *                those Raw Materials which need to be       *~
            *                Calculated. (Part,Date,Class,Type,Option)  *~
            *                                                           *~
            *            (B) All Three (3) Calc Methods are Active for  *~
            *                each Specified Class.                      *~
            *                                                           *~
            *                (1) - Calc Usage and Safety Stock / Day    *~
            *                (2) - Safety Stock and Lead Time           *~
            *                (3) - Calc Usage and On-Hand Usage / Day   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/22/94 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/21/97 ! Mod for Upgrade to new Release R6.04.03  ! RHH *~
            * 04/01/98 ! Y2K modifications                        ! ERN *~
            * 04/09/98 ! SYSFILE2 ROP PARAM format changes        ! ERN *~
            *************************************************************

        dim                              /* File = (ROPHNY)            */~
            r_key$25,                    /* Primary Key - ROPHNY       */~
            r_part$25,                   /* Part Number (Raw Material) */~
            r_rop$10,                    /* Re-Order Point             */~
            r_eoq$10,                    /* Economic Order Quantity    */~
            r_avgu$8,                    /* Average Useage - HNYDETAL  */~
            r_stdd$10,                   /* Standard Deviation         */~
            rs_avgu$8,                   /* Saved Average Usage        */~
            rs_rop$10,                   /* Saved Re-order Point       */~
            rs_eoq$10,                   /* Saved Economic Order Qty   */~
            rs_ss$10, c_ss$10,           /* Saved Standard Stocking Lv */~
            rs_stdd$10,                  /* Saved Standatd Deduction   */~
            r_date$6, r_datef$10,        /* Last Date Part Changed     */~
            r_class$4,                   /* ROP Part Class             */~
            r_fill$149,                  /* ROP Filler Area            */~
            r_source$1,                  /* C = Calc, M = Manual       */~
                                         /* File = (ROPCLASS)          */~
            rc_class$4,                  /* ROP Part Class             */~
            rc_desc$30,                  /* ROP Class Description      */~
            rc_excess$1,                 /* ROP Excess Flag - Y/N      */~
            rc_calc$1,                   /* ROP Calc Flag - Y/N        */~
            rc_formula$1,                /* Calc Method 1, 2, 3, Etc.  */~
            rc_fill$13,                  /* Class Filler Area          */~
                                         /* File = (SYSFILE2)          */~
            p_smco$8,                    /* Safety Stock +/- Adj %     */~
            p_purch$8,                   /* Not Used - Today           */~
            p_work$8,                    /* Not Used - Today           */~
            p_int$8,                     /* Economic Ord Qty +/- Adj % */~
            p_devr$8,                    /* Safety Stock +/- Adj %     */~
            p_vpcnt$8,                   /* ROP Std Deviation +/- Adj %*/~
            p_part$25,                   /* Last Part Processed-Restar */~
            p_yymm$4,                    /* Last Calc Year/Month       */~
            p_flag$4,                    /* Process Flag               */~
                                         /* File = (HNYMASTR)          */~
            pp_lead$10, c_ll$4,          /* Lead time in Days          */~
            pp_type$3,                   /* Part Type Code             */~
            pp_ss$10,                    /* Safty Stock Level          */~
                                         /* File = (HNYDETAL)          */~
            hny_key$42,                  /* Primary Key                */~
            hny_dte$6,                   /* Detail Posting Date        */~
            hny_tc$2,                    /* Detail Trans. Code         */~
            hny_qty$8,                   /* Detail Usage Quantity      */~
            hny_cd$3,                    /* Special Code From Text     */~
                                         /* Screen Display             */~
            scr_msg$25,                  /* Screen Message             */~
            used$10,                     /* Inventory Removed from INV */~
            issued$10,                   /* Inv Put Into Inventory     */~
            on_hand_tot$10,              /* On-Hand Total              */~
            usage_ss$8, cc_usage$8,      /* Safety Stock Usage         */~
            usage_cc$8,                  /* Calculated Usage           */~
            usage_oh$8,                  /* On-Hand Usage              */~
            x_ss$10, rr$4,               /* New Safety Stock           */~
                                         /* Program - Variables        */~
            part$25, part_d$30,          /* Part Number Lookup         */~
            bg_part$25, bg_part_d$30,    /* Beg Part Number            */~
            ed_part$25, ed_part_d$30,    /* End Part Number            */~
            bg_dte$10, bg_date$10,       /* Beg Date for Usage Data    */~
            ed_dte$10, ed_date$10,       /* End Date for Usage Data    */~
            usage$4, rop$5,              /* Usage Number of Days       */~
            class$4, classd$30,          /* Lookup ROP Class Code      */~
            bg_class$4, bg_class_d$30,   /* Beg ROP Class Code         */~
            ed_class$4, ed_class_d$30,   /* End ROP Class Code         */~
            bg_type$3, bg_type_d$30,     /* Beg Part Type Code         */~
            ed_type$3, ed_type_d$30,     /* End Part Type Code         */~
            type$3, typed$30,            /* Lookup Part Type Code      */~
            r_options$1, r_options_d$30, /* ROP Calc Options 0, 1, 2   */~
            hdr$(4%)132,                 /* ROP LOOKUP                 */~
            incl(2%), incl$(2%),         /* ROP LOOKUP                 */~
            print_title$40, company$40,  /* REPORT TITLE               */~
            rpt_time$8,                  /* REPORT RUN TIME            */~
            readkey$50, descr$30,        /* Generic Key                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            date$10, ck_dte$6,           /* SYSTEM DATE                */~
            temp1$10, temp2$10,          /* Holding cells              */~
            userid$3                     /* Current User Id            */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Re-Order Point Calculation Utility"
            pname$ = "APCROPCC - Rev: R6.04"

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
            * #1  ! ROPHNY   ! Master ROP Data                          *~
            * #2  ! GENCODES ! Master Code Table File                   *~
            * #3  ! ROPCLASS ! ROP Class Data File                      *~
            * #4  ! HNYMASTR ! Inventory Master File                    *~
            * #5  ! HNYDETAL ! Inventory Movement Detail File           *~
            * #6  ! SYSFILE2 ! Master System File - Tables              *~
            * #7  ! APCROPWK ! ROP Calc Work File                       *~
            * #8  ! HNYQUAN  ! INVENTORY QUANTITIES MASTER FILE         *~
            * #9  !          !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "ROPHNY",                                       ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    1, keylen =   25,                    ~
                        alt key  1, keypos =  104, keylen =   4, dup


            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #3,   "ROPCLASS",                                     ~
                        varc,     indexed,  recsize =   50,              ~
                        keypos =    1, keylen =   4

            select #4,   "HNYMASTR",                                     ~
                        varc,     indexed,  recsize = 900,               ~
                        keypos =    1, keylen =   25,                    ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #5,   "HNYDETAL",                                     ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos =    1, keylen =   42,                    ~
                        alt key  1, keypos =   43, keylen =   6, dup,    ~
                            key  2, keypos =   49, keylen =   2, dup

            select #6,   "SYSFILE2",                                     ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =   20

            select #7,   "APCROPWK",                                     ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  25

            select #8,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =   650,             ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),  0%, rslt$(6%))
            call "OPENCHCK" (#7, fs%(7%), f2%(7%),  0%, rslt$(7%))
            call "OPENCHCK" (#8, fs%(8%), f2%(8%),  0%, rslt$(8%))
            if fs%(7%) = 0% then goto L02750
               gosub lookup_parameters            /* Check for Restart */
               if p_flag% = 0% then goto L02730     /* No Restart        */
                  gosub update_rophny

L02730:        call "FILEBGON" addr(#7)

L02750:     call "OPENCHCK" (#7, fs%(7%), f2%(7%),500%, rslt$(7%))
            close #7

            call "OPENFILE" (#7, "IO   ", f2%(7%), rslt$(7%), axd$ )

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)
            date$ = date : u3% = 0%
            ck_dte$ = date
            call "DATFMTC" (date$)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 9%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub process_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 9% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Valid Beginning Part Number?                         ",~
         "Enter a Valid Ending Part Number?                            ",~
         "Enter a Valid Beginning Product/Part Usage Date?             ",~
         "Enter a Valid Ending Product/Part Usage Date?                ",~
         "Enter a Valid Beginning ROP Class Code?                      ",~
         "Enter a Valid Ending ROP Class Code?                         ",~
         "Enter a Valid Beginning Part Type Code?                      ",~
         "Enter a Valid Ending Part Type Code?                         ",~
         "Enter a Valid Calculation Option - 0, 1, 2 ?                 "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, r_part$, r_rop$, r_eoq$,   ~
                      r_avgu$, r_stdd$, rs_avgu$, rs_rop$, rs_eoq$,      ~
                      rs_ss$, rs_stdd$, r_date$, r_class$, r_fill$,      ~
                      r_source$, rc_class$, rc_desc$, rc_excess$,        ~
                      rc_calc$, rc_formula$, rc_fill$, bg_dte$, ed_dte$, ~
                      bg_date$, ed_date$, rpt_time$, readkey$,           ~
                      descr$, bg_class$, bg_class_d$, ed_class$,         ~
                      ed_class_d$, r_options$, r_options_d$, bg_type$,   ~
                      bg_type_d$, ed_type$, ed_type_d$, type$, typed$,   ~
                      bg_part$, bg_part_d$, ed_part$, ed_part_d$,        ~
                      p_smco$, p_purch$, p_work$, p_int$, p_devr$,       ~
                      p_vpcnt$, p_part$, p_yymm$, p_flag$, usage$,       ~
                      hny_qty$, print_title$, rop$
        debug% = 0%
        return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
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
            get #1, using L32970,         /* Primary Key - ROPHNY       */~
                          r_part$,       /* Part Number (Raw Material) */~
                          r_rop,         /* Re-Order Point             */~
                          r_eoq,         /* Economic Order Quantity    */~
                          r_avgu,        /* Average Useage - HNYDETAL  */~
                          r_stdd,        /* Standard Deviation         */~
                          rs_avgu,       /* Saved Average Usage        */~
                          rs_rop,        /* Saved Re-order Point       */~
                          rs_eoq,        /* Saved Economic Order Qty   */~
                          rs_ss,         /* Saved Standard Stocking Lv */~
                          rs_stdd,       /* Saved Standatd Deduction   */~
                          r_date$,       /* Last Date Part Changed     */~
                          r_class$,      /* ROP Part Class             */~
                          r_source$,     /* C = Calc, M = Manual       */~
                          r_userid$,     /* Last Modified by           */~
                          r_fill$        /* ROP Filler Area            */

            rs_avgu     = r_avgu
            rs_rop      = r_rop
            rs_eoq      = r_eoq
            rs_ss       = pp_ss
            rs_stdd     = r_stdd
            r_date$     = date
            r_source$   = "C"
            r_fill$     = " "
            p_part$     = r_part$
        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            read #1,hold,key = r_part$, eod goto L31090
               delete #1

L31090:     put #1, using L32970,         /* Primary Key - ROPHNY       */~
                          r_part$,       /* Part Number (Raw Material) */~
                          r_rop,         /* Re-Order Point             */~
                          r_eoq,         /* Economic Order Quantity    */~
                          r_avgu,        /* Average Useage - HNYDETAL  */~
                          r_stdd,        /* Standard Deviation         */~
                          rs_avgu,       /* Saved Average Usage        */~
                          rs_rop,        /* Saved Re-order Point       */~
                          rs_eoq,        /* Saved Economic Order Qty   */~
                          rs_ss,         /* Saved Standard Stocking Lv */~
                          rs_stdd,       /* Saved Standatd Deduction   */~
                          r_date$,       /* Last Date Part Changed     */~
                          r_class$,      /* ROP Part Class             */~
                          r_source$,     /* C = Calc, M = Manual       */~
                          r_userid$,     /* Last Modified by           */~
                          r_fill$        /* ROP Filler Area            */
            write #1

            read #4,hold,key = r_part$, eod goto L31320
               put #4 using L31300, rs_ss
L31300:          FMT POS(318), PD(14,4)
            rewrite #4
L31320: return

        REM *************************************************************~
            *              U p d a t e   W o r k   F i l e              *~
            *-----------------------------------------------------------*

        dataput_wrk
            r_formula$ = rc_formula$
            cc_usage = usage_ss
            if r_formula$ = "1" then cc_usage = usage_cc
            if r_formula$ = "3" then cc_usage = usage_oh
            read #7,hold,key = r_part$, eod goto L32120
               delete #7

L32120:     put #7, using L33220,         /* Primary Key - APCROPWK     */~
                          r_part$,       /* Part Number (Raw Material) */~
                          r_rop,         /* Re-Order Point             */~
                          r_eoq,         /* Economic Order Quantity    */~
                          r_avgu,        /* Average Useage - HNYDETAL  */~
                          r_stdd,        /* Standard Deviation         */~
                          rs_avgu,       /* Saved Average Usage        */~
                          rs_rop,        /* Saved Re-order Point       */~
                          rs_eoq,        /* Saved Economic Order Qty   */~
                          rs_ss,         /* Saved Standard Stocking Lv */~
                          rs_stdd,       /* Saved Standatd Deduction   */~
                          r_date$,       /* Last Date Part Changed     */~
                          r_class$,      /* ROP Part Class             */~
                          r_source$,     /* C = Calc, M = Manual       */~
                          userid$,       /* Last Modified By           */~
                          r_formula$,    /* Equation Number-for Report */~
                          c_ll,          /* Part Lead Time             */~
                          c_pp,          /* Part Pan Size              */~
                          c_ss,          /* New Safety Stock           */~
                          cc_usage,      /* Calc Usage for Formula     */~
                          used,          /* Usage Count for Period     */~
                          r_fill$        /* ROP Filler Area            */
            write #7
        return

        dataload_wrk
            get #7, using L33220,        /* Primary Key - APCROPWK     */~
                          r_part$,       /* Part Number (Raw Material) */~
                          r_rop,         /* Re-Order Point             */~
                          r_eoq,         /* Economic Order Quantity    */~
                          r_avgu,        /* Average Useage - HNYDETAL  */~
                          r_stdd,        /* Standard Deviation         */~
                          rs_avgu,       /* Saved Average Usage        */~
                          rs_rop,        /* Saved Re-order Point       */~
                          rs_eoq,        /* Saved Economic Order Qty   */~
                          rs_ss,         /* Saved Standard Stocking Lv */~
                          rs_stdd,       /* Saved Standatd Deduction   */~
                          r_date$,       /* Last Date Part Changed     */~
                          r_class$,      /* ROP Part Class             */~
                          r_source$,     /* C = Calc, M = Manual       */~
                          r_userid$,     /* Last Modified By           */~
                          r_formula$,    /* Equation Number            */~
                          c_ll,          /* Part Lead Time             */~
                          c_pp,          /* Part Pan Size              */~
                          c_ss,          /* New Safety Stock           */~
                          cc_usage,      /* Calc Usage for Formula     */~
                          used,          /* Usage Count for Period     */~
                          r_fill$        /* ROP Filler Area            */

            if p_flag% = 1% then return          /* Updating (ROPHNY ) */

               convert r_rop  to r_rop$,    pic(#######.##)

               convert r_eoq  to r_eoq$,    pic(#######.##)

               convert r_avgu to r_avgu$,   pic(#####.##)

               convert r_stdd to r_stdd$,   pic(#######.##)

               convert rs_avgu to rs_avgu$, pic(#####.##)

               convert rs_rop  to rs_rop$ , pic(#######.##)

               convert rs_eoq  to rs_eoq$ , pic(#######.##)

               convert rs_ss   to rs_ss$  , pic(#######.##)

               convert c_ll    to c_ll$   , pic(####)

               convert c_pp    to c_pp$   , pic(#######)

               convert c_ss    to c_ss$   , pic(#######.##)

               convert cc_usage to cc_usage$, pic(#####.##)

               if used = 0.0 and r_formula$ <> "2" then                  ~
                                                   cc_usage$ = "* None *"
               r_datef$ = r_date$
               call "DATFMTC" (r_datef$)
        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* File = (ROPHNY  )          */
L32970: FMT CH(25),                      /* Part Number                */~
            PD(14,4),                    /* Re-Oder Point              */~
            PD(14,4),                    /* Economic Order Quantity    */~
            PD(14,4),                    /* Average Usage              */~
            PD(14,7),                    /* Standard Deviation         */~
            PD(14,4),                    /* SV Average Usage           */~
            PD(14,4),                    /* SV ROP                     */~
            PD(14,4),                    /* SV EOQ                     */~
            PD(14,4),                    /* SV Safety Stock Last Calc  */~
            PD(14,7),                    /* SV Standard Deviation      */~
            CH(06),                      /* Last Mod Date              */~
            CH(04),                      /* ROP Class                  */~
            CH(1),                       /* Soure of Last ROP C or M   */~
            CH(3),                       /* Last Modified By           */~
            CH(145)                      /* Filler Area                */

                                         /* File = (ROPCLASS)          */
        FMT CH(04),                      /* ROP Part Class Code        */~
            CH(30),                      /* Class Code Description     */~
            CH(01),                      /* ROP Excess Test Flag       */~
            CH(01),                      /* ROP Calc Flag              */~
            CH(01),                      /* ROP Formula Flag           */~
            CH(13)                       /* Filler Area                */

                                         /* File = (APCROPWK)          */
L33220: FMT CH(25),                      /* Part Number                */~
            PD(14,4),                    /* Re-Oder Point              */~
            PD(14,4),                    /* Economic Order Quantity    */~
            PD(14,4),                    /* Average Usage              */~
            PD(14,7),                    /* Standard Deviation         */~
            PD(14,4),                    /* SV Average Usage           */~
            PD(14,4),                    /* SV ROP                     */~
            PD(14,4),                    /* SV EOQ                     */~
            PD(14,4),                    /* SV Safety Stock Last Calc  */~
            PD(14,7),                    /* SV Standard Deviation      */~
            CH(06),                      /* Last Mod Date              */~
            CH(04),                      /* ROP Class                  */~
            CH(1),                       /* Soure of Last ROP C or M   */~
            CH(3),                       /* Last Modified By           */~
            CH(1),                       /* Formula Used for Calc      */~
            PD(14,4),                    /* Save Part Lead Time        */~
            PD(14,4),                    /* Save Part Pan Size         */~
            PD(14,4),                    /* Save New Safety Stock Val  */~
            PD(14,4),                    /* Save Calculated Usage/ Day */~
            PD(14,4),                    /* Save Usage Count           */~
            CH(104)                      /* Filler Area                */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub set_pf1

              gosub'050(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40250,         /* Beg Part Number      */~
                                L40250,         /* End Part Number      */~
                                L40250,         /* Beg Usage Date       */~
                                L40250,         /* End Usage Date       */~
                                L40250,         /* Beg ROP Class Code   */~
                                L40250,         /* End Rop Class Code   */~
                                L40250,         /* Beg Part Type Code   */~
                                L40250,         /* End Part Type Code   */~
                                L40260          /* Options 0, 1, 2      */

              goto L40280

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40250:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40260:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40280:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,24), fac(hex(a4)), apc$                   , ch(38),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Beg Part Number:",                           ~
               at (03,20), fac(lfac$( 1)), bg_part$             , ch(25),~
               at (03,50), fac(hex(84)), bg_part_d$             , ch(30),~
                                                                         ~
               at (04,02), "End Part Number:",                           ~
               at (04,20), fac(lfac$( 2)), ed_part$             , ch(25),~
               at (04,50), fac(hex(84)), ed_part_d$             , ch(30),~
                                                                         ~
               at (05,02), "Mat'l Usage Beg Date:",                      ~
               at (05,25), fac(lfac$( 3)), bg_date$             , ch(10),~
                                                                         ~
               at (06,02), "            End Date:",                      ~
               at (06,25), fac(lfac$( 4)), ed_date$             , ch(10),~
               at (06,50), "No. of Usage Days :",                        ~
               at (06,70), fac(hex(84)), usage$                 , ch(04),~
                                                                         ~
               at (07,02), "ROP Class Code Beg  :",                      ~
               at (07,25), fac(lfac$( 5)), bg_class$            , ch(04),~
               at (07,50), fac(hex(84)), bg_class_d$            , ch(30),~
                                                                         ~
               at (08,02), "               End  :",                      ~
               at (08,25), fac(lfac$( 6)), ed_class$            , ch(04),~
               at (08,50), fac(hex(84)), ed_class_d$            , ch(30),~
                                                                         ~
               at (09,02), "Part Type Code Beg  :",                      ~
               at (09,25), fac(lfac$( 7)), bg_type$             , ch(03),~
               at (09,50), fac(hex(84)), bg_type_d$             , ch(30),~
                                                                         ~
               at (10,02), "               End  :",                      ~
               at (10,25), fac(lfac$( 8)), ed_type$             , ch(03),~
               at (10,50), fac(hex(84)), ed_type_d$             , ch(30),~
                                                                         ~
               at (11,02), "ROP Calc Opt's 0,1,2:",                      ~
               at (11,25), fac(lfac$( 9)), r_options$           , ch(01),~
               at (11,50), fac(hex(84)), r_options_d$           , ch(30),~
                                                                         ~
               at (14,21),                                               ~
                  "(0) - ROP Calculation Report Only?      ",            ~
               at (16,21),                                               ~
                  "(1) - ROP Calculation Report and Update?",            ~
               at (18,21),                                               ~
                  "(2) - ROP Calculation Update Only?      ",            ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9% then goto L40890
                  debug% = 1%
                  gosub process_data

L40890:        if keyhit% <> 15% then goto L40930
                  call "PRNTSCRN"
                  goto L40280

L40930:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41120     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L41080
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L41080:     if fieldnr% > 1% then L41100
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L41100:     return

L41120: if fieldnr% > 0% then L41220  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "(9)Display Screen                      "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Process Data"
            pfkeys$ = hex(01ffffffffffffff09ffffffffff0f1000)
            return

L41220:
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *              D i s p l a y   S c r e e n                  *~
            *************************************************************

        display_screen                        /* PF(9) Used to Process */
            if r_options$ <> "0" then return
            scr_msg$ = "(After ) - Calculations?"
            inpmessage$ = "Press <Return> To Continue, PF(16) To Exit?"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)

            convert pp_ss       to pp_ss$       , pic(#######.##)

            convert c_ss        to c_ss$        , pic(#######.##)

            convert c_ll        to c_ll$        , pic(####)

            convert c_pp        to c_pp$        , pic(#######)

            convert usage%      to usage$       , pic(####)

            convert used        to used$        , pic(#######.##)

            convert issued      to issued$      , pic(#######.##)

            convert on_hand_tot to on_hand_tot$ , pic(#######.##)

            convert usage_ss    to usage_ss$    , pic(#####.##)

            convert usage_cc    to usage_cc$    , pic(#####.##)

            convert usage_oh    to usage_oh$    , pic(#####.##)

            convert x_ss        to x_ss$        , pic(#######.##)

            convert r_rop       to r_rop$       , pic(#######.##)

            convert r_eoq       to r_eoq$       , pic(#######.##)

            convert r_avgu      to r_avgu$      , pic(#####.##)

            convert rr          to rr$          , pic(##.#)

            convert rs_avgu     to rs_avgu$     , pic(#####.##)

            convert rs_rop      to rs_rop$      , pic(#######.##)

            convert rs_eoq      to rs_eoq$      , pic(#######.##)

            convert rs_ss       to rs_ss$       , pic(#######.##)

            accept                                                       ~
               at (01,02),                                               ~
                  "APC Building Products - ROP Display Screen ",         ~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (02,02), fac(hex(94)), scr_msg$               , ch(25),~
               at (02,40), "ROP Part No.",                               ~
               at (02,55), fac(hex(84)), str(r_part$,1%,15%)    , ch(15),~
               at (02,71), "Incr:",                                      ~
               at (02,77), fac(hex(84)), rr$                    , ch(04),~
                                                                         ~
               at (04,02), "Loaded Data (1) Safety Stock    :",          ~
               at (05,02), "            (2) Lead Time       :",          ~
               at (06,02), "            (3) Order Increment :",          ~
               at (07,02), "            (4) Usage Days      :",          ~
               at (08,02), "            (5) ROP Calc Flag   :",          ~
               at (09,02), "            (6) ROP Formula     :",          ~
               at (10,02), "            (7) ROP Class       :",          ~
                                                                         ~
               at (12,02), "Calculated  (1) Taken From Inv. :",          ~
               at (13,02), "            (2) Put Into Inv.   :",          ~
               at (14,02), "            (3) On-Hand Qty     :",          ~
               at (15,02), "            (4) S.S Usage / Day :",          ~
               at (16,02), "            (5) C.C Usage / Day :",          ~
               at (17,02), "            (6) O.H Usage / Day :",          ~
                                                                         ~
               at (18,60), "Prev Value",                                 ~
               at (19,02), "Chg Values  (1) ROP Usage / Day :",          ~
               at (20,02), "            (2) New ROP Value   :",          ~
               at (21,02), "            (3) New EOQ Value   :",          ~
               at (22,02), "            (4) New Safety Stock:",          ~
                                                                         ~
               at (04,40), fac(hex(84)), pp_ss$                 , ch(10),~
               at (05,46), fac(hex(84)), c_ll$                  , ch(04),~
               at (06,43), fac(hex(84)), c_pp$                  , ch(07),~
               at (07,46), fac(hex(84)), usage$                 , ch(04),~
               at (08,49), fac(hex(84)), rc_calc$               , ch(01),~
               at (09,49), fac(hex(84)), rc_formula$            , ch(01),~
               at (10,46), fac(hex(84)), rc_class$              , ch(04),~
                                                                         ~
               at (12,40), fac(hex(84)), used$                  , ch(10),~
               at (13,40), fac(hex(84)), issued$                , ch(10),~
               at (14,40), fac(hex(84)), on_hand_tot$           , ch(10),~
               at (15,42), fac(hex(84)), usage_ss$              , ch(08),~
               at (16,42), fac(hex(84)), usage_cc$              , ch(08),~
               at (17,42), fac(hex(84)), usage_oh$              , ch(08),~
                                                                         ~
               at (19,42), fac(hex(84)), r_avgu$                , ch(08),~
               at (20,40), fac(hex(84)), r_rop$                 , ch(10),~
               at (21,40), fac(hex(84)), r_eoq$                 , ch(10),~
               at (22,40), fac(hex(84)), x_ss$                  , ch(10),~
                                                                         ~
               at (19,62), fac(hex(84)), rs_avgu$               , ch(08),~
               at (20,60), fac(hex(84)), rs_rop$                , ch(10),~
               at (21,60), fac(hex(84)), rs_eoq$                , ch(10),~
               at (22,60), fac(hex(84)), rs_ss$                 , ch(10),~
                                                                         ~
               at (24,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L43150
                  call "PRNTSCRN"
                  goto display_screen

L43150:        if keyhit% <> 16% then goto L43180
                  goto exit_program

L43180: close ws
        call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            call "SHOSTAT" ("Processing Part - ( "&r_part$&" )")
        return
        REM *************************************************************~
            *                     E D I T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Edit data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50200,         /* Beg Part Number       */ ~
                              L50390,         /* End Part Number       */ ~
                              L50590,         /* Beg Part Usage Date   */ ~
                              L50750,         /* End Part Usage Date   */ ~
                              L51040,         /* Beg ROP Class         */ ~
                              L51230,         /* End Rop Class         */ ~
                              L51430,         /* Beg Part Type         */ ~
                              L51580,         /* End Part Type         */ ~
                              L51740          /* ROP Calc Options      */

            return

L50200: REM Beginning Part Number                 BG_PART$
            if bg_part$ <> " " then goto L50300
               gosub lookup_part
               if f1%(1%) <> 0% then goto L50270
L50240:           bg_part$ = "ALL"
                  bg_part_d$ = "(All) - Valid Material Parts"
                  return
L50270:        bg_part$   = part$
               bg_part_d$ = part_d$
               return
L50300:    if str(bg_part$,1%,1%) = "A" then goto L50240
           read #4,key = bg_part$, using L50320,bg_part_d$, eod goto L50350
L50320:       FMT POS(26), CH(30)
           if len(bg_part$) > 14% then goto L50350
        return
L50350:    errormsg$ = "(Error) - Invalid Beginning Part Number? "
           bg_part$, bg_part_d$ = " "
        return

L50390: REM Ending Part Number                    ED_PART$
            if ed_part$ <> " " then goto L50500
               if str(bg_part$,1%,1%) = "A" then goto L50440
               gosub lookup_part
               if f1%(1%) <> 0% then goto L50470
L50440:           ed_part$   = bg_part$
                  ed_part_d$ = bg_part_d$
                  return
L50470:        ed_part$   = part$
               ed_part_d$ = part_d$
               return
L50500:    if str(bg_part$,1%,1%) = "A" then goto L50440
           read #4,key = ed_part$, using L50520,ed_part_d$, eod goto L50550
L50520:       FMT POS(26), CH(30)
           if len(ed_part$) > 14% then goto L50550
        return
L50550:    errormsg$ = "(Error) - Invalid Ending Part Number? "
           ed_part$, ed_part_d$ = " "
        return

L50590: REM Beginning Part Usage Date             BG_DATE$
            bg_dte$ = " "
            if bg_date$ <> " " then goto L50640
               bg_date$ = "0101" & str(date$,7%,4%)

L50640:     call "DATEOKC" (bg_date$, date%, errormsg$ )
            if date% = 0% then return
       REM  convert date% to bg_dte$, pic(######)
            bg_dte$ = bg_date$
            call "DATUFMTC" (bg_dte$)

            if bg_dte$ > ck_dte$ then goto L50700
        return
L50700:     errormsg$ = "(Error) - Beginning Date Cannot be Greater than ~
        ~Todays Date?"
            bg_date$, bg_dte$ = " "
        return

L50750: REM Ending Part Usage Date                ED_DATE$
            ed_dte$ = " "
            if ed_date$ <> " " then goto L50800
               ed_date$ = date

L50800:     call "DATEOKC" (ed_date$, date%, errormsg$)
            if date% = 0% then return
        REM convert date% to ed_dte$, pic(######)
            ed_dte$ = ed_date$
            call "DATUFMTC" (ed_dte$)

            if bg_dte$ > ed_dte$ then goto L50930
            if ed_dte$ > ck_dte$ then goto L50990
            call "DATE" addr("G-", bg_dte$, ed_dte$, usage%, ret% )
            if ret% <> 0% then goto L50930
               usage% = usage% + 1%
               convert usage% to usage$, pic(####)

               if usage% < 14% then goto L50960
        return
L50930:     errormsg$ = "(Error) - Invalid Ending Usage Date?"
            ed_dte$, ed_date$, usage$ = " "
        return
L50960:     errormsg$ = "(Error) - Date Range Must be (14) or More Days?"
            ed_dte$, ed_date$, usage$ = " "
        return
L50990:     errormsg$ = "(Error) - Ending Date Cannot be Greater than Tod~
        ~ays Date?"
            ed_dte$, ed_date$, usage$ = " "
        return

L51040: REM Beginning ROP Class Code              BG_CLASS$
           if bg_class$ <> " " then goto L51140
              gosub lookup_class
              if f1%(3%) <> 0% then goto L51110
L51080:          bg_class$ = "ALL"
                 bg_class_d$ = "(All) - Valid ROP Class Codes"
                 return
L51110:       bg_class$   = class$
              bg_class_d$ = classd$
              return
L51140:    if str(bg_class$,1%,1%) = "A" then goto L51080
              read #3,key = bg_class$, using L51170, bg_class_d$,         ~
                                                    eod goto L51190
L51170:          FMT POS(5), CH(30)
        return
L51190:    errormsg$ = "(Error) - Invalid Beginning ROP Class Code?"
           bg_class$, bg_class_d$ = " "
        return

L51230: REM Ending ROP Class Code                 ED_CLASS$
           if ed_class$ <> " " then goto L51340
              if str(bg_class$,1%,1%) = "A" then goto L51280
              gosub lookup_class
              if f1%(3%) <> 0% then goto L51310
L51280:          ed_class$   = bg_class$
                 ed_class_d$ = bg_class_d$
                 return
L51310:        ed_class$   = class$
               ed_class_d$ = classd$
               return
L51340:    if str(bg_class$,1%,1%) = "A" then goto L51280
              read #3,key = ed_class$, using L51170, ed_class_d$,         ~
                                                    eod goto L51390
              if bg_class$ > ed_class$ then goto L51390
        return
L51390:    errormsg$ = "(Error) - Invalid Ending ROP Class Code?"
           ed_class$, ed_class_d$ = " "
        return

L51430: REM Begining Part Type Code               BG_TYPE$
           if bg_type$ <> " " then goto L51480
L51450:       bg_type$   = "ALL"
              bg_type_d$ = "(A)ll Valid Part Type Codes?"
              return
L51480:    if str(bg_type$,1%,1%) = "A" then goto L51450
              type$ = bg_type$
              gosub lookup_type
              if type% = -1% then goto L51540
              bg_type_d$ = typed$
        return
L51540:    errormsg$ = "(Error) - Invalid Beginning Part Type Code?"
           bg_type$, bg_type_d$ = " "
        return

L51580: REM Ending Part Type Code                      PT_TYPE$
           if ed_type$ <> " " then goto L51630
L51600:       ed_type$   = bg_type$
              ed_type_d$ = bg_type_d$
              return
L51630:    if str(bg_type$,1%,1%) = "A" then goto L51600
              type$ = ed_type$
              gosub lookup_type
              if type% = -1% then goto L51700
              ed_type_d$ = typed$
              if bg_type$ > ed_type$ then goto L51700
        return
L51700:    errormsg$ = "(Error) - Invalid Ending Part Type Code?"
           ed_type$, ed_type_d$ = " "
        return

L51740: REM ROP Calculation Options                  R_OPTIONS$
            if r_options$ <> " " then goto L51780
               r_options$ = "0"
L51780:     if r_options$ = "0" then                                     ~
               r_options_d$ = "ROP Calculation Report Only"
            if r_options$ = "1" then                                     ~
               r_options_d$ = "ROP Calculation Report and Update"
            if r_options$ = "2" then                                     ~
               r_options_d$ = "ROP Calculation Update Only"

            if r_options$ <> "0" and r_options$ <> "1" and               ~
                                     r_options$ <> "2" then goto L51880
        return
L51880:     errormsg$ = "(Error) - Invalid ROP Calculation Option?"
            r_options$, r_options_d$ = " "
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~+
L55070: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~!
L55100: %!########## @ ########                       ###################~
        ~#####################                                  Page: ####~
        ~!
L55130: %!                                                               ~
        ~                                                                 ~
        ~!
L55160: %!Beg Part: ######################### Beg Date: ########## Beg Cl~
        ~ass: ####  Beg Type: ###   Option: ##############################~
        ~!
L55190: %!End Part: ######################### End Date: ########## End Cl~
        ~ass: ####  End Type: ###   Period: (####) Days  Rop Calc's: #####~
        ~!
L55220: %!<-Part Number->!<New  ROP>!<New  EOQ>!<New S.S.>!New  Use!<Old ~
        ~ ROP>!<Old  EOQ>!<Old S.S.>!Old  Use!Lead!Pan Sze!Cl's!F!Usage CC~
        ~!
L55250: %!###############!##########!##########!##########!########!#####~
        ~#####!##########!##########!########!####!#######!####!#!########~
        ~!
L55280: %!---------------!----------!----------!----------!--------!-----~
        ~-----!----------!----------!--------!----!-------!----!-!--------~
        ~!
        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            page_no% = 0%
            lcnt%    = 99%
            print_title$="ROP - Re-Order Point Calculation Report "
            date$ = date  :  call "DATFMTC" (date$)
            rpt_time$ = " "
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCROP", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            print using L55040
            call "SETPRNT" ("APCROP", " ", 0%, 1%)
        return

        print_header
           if lcnt% <> 99% then print using L55040
           print page
           page_no% = page_no% + 1%
           print using L55040
           print using L55100, date$, rpt_time$, print_title$, page_no%
           print using L55130
           print using L55160, bg_part$, bg_date$, bg_class$, bg_type$,   ~
                                                  r_options_d$
           print using L55190, ed_part$, ed_date$, ed_class$, ed_type$,   ~
                                                  usage$, rop$
           print using L55070
           print using L55220
           lcnt% = 7%
        return

        print_detail
           if lcnt% > 56% then gosub print_header
           print using L55280
           print using L55250, str(r_part$,1%,15%), r_rop$, r_eoq$, c_ss$,~
                              r_avgu$, rs_rop$, rs_eoq$, rs_ss$,         ~
                              rs_avgu$, c_ll$, c_pp$, r_class$,          ~
                              r_formula$, cc_usage$
           lcnt% = lcnt% + 2%
        return

        lookup_type
            type% = -1%
            convert type$ to type%, data goto L60500
L60500:
            typed$ = "** Invalid **"
            if type% = 0   then typed$ = "Generic          "
            if type% > 0   then typed$ = "Nonplanned       "
            if type% > 199 then typed$ = "Purchased        "
            if type% > 489 then typed$ = "Purchased Tool   "
            if type% > 499 then typed$ = "Manufactured     "
            if type% > 789 then typed$ = "Manufactured Tool"
            if type% > 799 then typed$ = "Manufactured     "
            if type% > 999 then typed$ = "** Invalid **    "
        return

        lookup_part
           init(" ") hdr$(), part$, part_d$
           part_d$ = hex(06) & "ROP Part Records"
           call "PLOWCODE" (#1,part$,part_d$,8000%,0.32, f1%(1%), hdr$(),~
                            0, -1.0026, incl(), incl$(), " ", " ", #4%)
           if f1%(1%) = 0% then part_d$ = " "
        return

        lookup_class
           init(" ") errormsg$, class$, classd$
           classd$ = hex(06) & "Select a Valid ROP Class"
           call "GETCODE" (#3, class$, classd$, 0%, 1.30, f1%(3%))
           if f1%(3%) = 0 then classd$ = " "
        return

        lookup_parameters
           call "SHOSTAT" ("Looking Up ROP Parameters")
           p_int, p_vpcnt = 0.0
           p_flag% = 0%                      /* (SYSFILE2) - 0% NORMAL */
           readkey$ = "ROP PARAM"
           read #6,key = readkey$, using L60840, p_smco, p_purch, p_work, ~
                        p_int, p_devr, p_vpcnt, p_part$, p_yymm$, p_flag%
L60840:       FMT POS(21), PD(14,7), 5*PD(14,4), CH(25), CH(4), BI(1)
           p_purch, p_work = 1.0
           if p_flag% = 0% then goto L60930
                                                     /* (1%) = RESTART */
L60880:       hit% = 2%
              call "ASKUSER" (hit%, "***** Restart *****", "Press PF1 to ~
        ~Restart Aborted Run", "- or -", "Press PF16 to Exit.")
              if hit% = 16% then goto exit_program
              if hit% <> 1% then goto L60880
              temp1$ = date
              call "DATFMTC" (temp1$, temp%, temp2$)
L60930:       p_yymm$ = str(temp2$,2,4)
        return
           stop "(ERROR)-(1) No System Parameters Found. Run 'ROPSYSIN'"
           close ws
        return clear all
        goto exit_program

        update_parameters                          /* Used for Restart */
           str(readkey$,1,20) = "ROP PARAM"
           read #6,hold,key = readkey$, eod goto L61070
           convert p_yymm$ to p_yymm%, data goto L61038
L61038:    put #6, using L61040, p_part$, p_yymm%, p_flag%, str(date)
L61040:          FMT POS(69), CH(25), BI(4), BI(1), CH(6)
           rewrite #6
        return
L61070:    stop "(Error)-(2) - Updating 'SYSFILE2' "
        return

        load_rop_class                             /* Load (ROPCLASS)  */
            class% = 0%
            read #3,key = r_class$, using L61150, rc_class$, rc_desc$,    ~
                                    rc_excess$, rc_calc$, rc_formula$,   ~
                                    eod goto L61180
L61150:        FMT CH(4), CH(30), 3*CH(1)
            if rc_calc$ <> "Y" then goto L61180
               class% = 1%
L61180: return

        load_part                              /* Load (HNYMASTR) Data */
            part% = 1%
            r_part$ = r_key$
            read #4,key = r_part$, using L61250, pp_lead$, pp_type$,      ~
                                                pp_ss, pp_pan
L61250:        FMT POS(170), CH(10), CH(3), POS(318), 2*PD(14,4)
            pp_lead = 1.0
            convert pp_lead$ to pp_lead, data goto L61280
L61280:
            if pp_lead = 0.0 or pp_pan = 0.0 then part% = 0%
        return

        load_usage                             /* Load (HNYDETAL) Data */
            c_ss, c_ll, c_pp, used, issued            = 0.0
            on_hand_tot, usage_ss, usage_cc, usage_oh = 0.0
            x_ss, r_rop, r_eoq, r_avgu, rr            = 0.0
            hny_key$ = all(hex(00))
            str(hny_key$,1%,25%) = r_part$
            read #5,key > hny_key$, using L61400, hny_key$, hny_dte$,     ~
                                    hny_tc$, eod goto load_usage_done
L61400:        FMT CH(42), CH(6), CH(2)
            goto L61460
        load_usage_nxt
            read #5, using L61400, hny_key$, hny_dte$, hny_tc$,           ~
                                                 eod goto load_usage_done
                                              /* Check Part Number     */
L61460:     if str(hny_key$,1%,25%) <> r_key$ then goto load_usage_done
                                              /* Check Posting Date Rg */
            if hny_dte$ > ed_dte$ then goto load_usage_nxt
            if hny_dte$ < bg_dte$ then goto load_usage_nxt
                                              /* Check Trans Type Code */
                                              /* Don't Check PO'S      */
            if hny_tc$ <> "IW" and hny_tc$ <> "IA"                       ~
                                                 then goto load_usage_nxt
                                              /* Check Store Usage     */
                                              /* 000,300,400 Not Applic*/
                                              /* Turn Off Check for Now*/
               get #5, using L61580, hny_qty, hny_cd$
L61580:           FMT POS(51), PD(14,4), POS(83), CH(3)
                                              /*(COR) + Corr. to Inv.  */
                                              /*(ADJ) - Corr. to Inv.  */
               rhh = abs(hny_qty)             /*(REG) - Corr. to Inv.  */
        REM    IF HNY_TC$ = "PO"  THEN GOTO 61680
               if hny_cd$ = "COR" then goto L61700
               if hny_cd$ = "ADJ" then goto L61680
               if hny_cd$ = "REJ" then goto L61720
               if hny_tc$ = "IA"  then goto L61700
                                              /* 'ADJ', 'IW' - Codes   */
L61680:           used   = used   + rhh : goto load_usage_nxt
                                              /* 'COR', 'IA', 'PO' Cod */
L61700:           issued = issued + rhh : goto load_usage_nxt
                                              /* 'REJ' - Codes         */
L61720:           issued = issued - rhh : goto load_usage_nxt
        load_usage_done
           gosub load_onhand
           used        = round(used, 4)         /* Date Range Usage    */
           issued      = round(issued, 4)       /* Date Range Recivied */
           on_hand_tot = round(on_hand_tot, 4)  /* Today's ON-HAND Qty */
        return

        load_onhand
          if rc_formula$ <> "3" then return     /* Not Applic. 1, 2    */
             readkey$ = all(hex(00))
             str(readkey$,1%,25%) = r_part$
             read #8,key > readkey$, using L61850, readkey$,eod goto L61980
L61850:         FMT POS(17), CH(44)
             goto L61900
        load_onhand_nxt
             read #8, using L61850, readkey$, eod goto L61980

L61900:      if str(readkey$,1%,25%) <> r_part$ then goto L61980
                str$ = str(readkey$,26%,3%)
                if str$ = "000" or str$ = "300" or str$ = "400" then     ~
                                                     goto load_onhand_nxt
                get #8, using L61950, on_hand
L61950:            FMT POS(69), PD(14,4)
                on_hand_tot = on_hand_tot + on_hand
                goto load_onhand_nxt
L61980: return

        do_calc           /* P_SMCO  - Safety Stock Zero Adj (SYSFILE2)*/
           rr = 0.0       /* P_INT   - EOQ +/- Adj %         (SYSFILE2)*/
                          /* P_DEVR  - Safety Stock +/- Adj %(SYSFILE2)*/
                          /* P_VPCNT - ROP STD Dev +/- Adj % (SYSFILE2)*/
                          /* R_STDD  - ROP STD Dev +/- Adj % (ROPHNY  )*/
                          /* RC_FORMULA$ - 1, 2, 3, Etc.               */
                          /* PP_LEAD - Lead Time in Days     (HNYMASTR)*/
                          /* C_LL    - Lead Time in Days(Calc Variable)*/
                          /* PP_SS   - Safty Stock    (ROPHNY,HNYMASTR)*/
                          /* C_SS    - Safty Stock ( Calc Variable)    */
                          /* PP_PAN  - Purchasing Increment  (HNYMASTR)*/
                          /* USED    - Prod Issued for Dates- Withdraws*/
                          /* ISSUED  - Prod Received for Dates-Addition*/
                          /* USAGE%  - Number of Days in Usage Period  */
                          /* USAGE_CC- Calculated Usage Per Day        */
                          /* USAGE_SS- Standard Usage Per Day          */
                          /* USAGE_OH- On-Hand Usage Per Day           */
                          /* R_AVGU  - ROP Average Usage Per Day       */
                          /* ON_HAND_TOT - Current On-Hand Total       */
                          /* SS%     - 1% = No Safety Stock (0.0)      */
           gosub dataload           /* (ROPHNY) Load all Values        */
           gosub calc_standard      /* Calc of ( USAGE_SS)             */
           if rc_formula$ <> "1" then goto L62370      /* Formula - (1) */
              usage_cc  = round( used / usage%, 4)
              if usage_cc <= usage_ss then goto L62290
                 zz%       = int((c_ll * usage_cc) + .5)
                 x_ss,c_ss = zz%    /* Calc (New) Safety Stock(Increas)*/
                 goto L62510
                                    /* Standard Greater than Calc Usage*/
L62290:       zz   = round( c_ss * p_devr, 4)
              zz%       = int((c_ss - zz) + .5)
              x_ss,c_ss = zz%       /* Calc Safety Stock    (Reduction)*/
              if c_ss > ( pp_pan * p_devr ) then goto L62510
                                    /* Set Minimum Safety Stock Level  */
                 zz%       = int( (pp_pan * p_devr) + .5 )
                 x_ss,c_ss = zz%
                 goto L62510
L62370:    if rc_formula$ <> "2" then goto L62410      /* Formula - (2) */
              usage_cc = usage_ss                 /*Set Calc to STD    */
              goto L62510
                                                      /* Formula - (3) */
L62410:       usage_cc  = round( used / usage%, 4)     /* Calc Usage   */
              usage_oh  = round(on_hand_tot / c_ll, 4) /* On-Hand Usage*/
              if usage_cc <= usage_oh then goto L62480
                 zz%       = int((c_ll * usage_cc) + .5)
                 x_ss,c_ss = zz%    /* Calc (New) Safety Stock(Increas)*/
                 goto L62510
                                    /* On-Hand Greater than Calc Usage */
L62480:  x_ss,c_ss = on_hand_tot    /* Set Safety Stock To On-Hand Qty */

                                    /* Calc New ROP Value - For Usage  */
L62510:       rop_a = int((pp_pan * r_stdd) + .5) /*ROP Adjustment +   */
L62520:         rr = rr + 1.0                     /*Deduct Safety Stock*/
                rop_c = ( rr * pp_pan ) - c_ss + rop_a
                r_avgu = round(rop_c/c_ll, 4) /* ROP Avg Usage Per Day */
                if r_avgu > usage_cc then goto do_calc_done
                   goto L62520
        do_calc_done
           yy%   = int(rop_c + .5)              /* Convert to Integer  */
           r_rop = yy%                          /* New R.O.P Value     */
           r_eoq = round(rr * pp_pan, 4)        /* New E.O.Q for Order */
           c_ll = pp_lead                       /* RESET LEADTIME      */
           gosub dataput_wrk                    /* Unchanged - PP_LEAD */
                                                /*           - PP_PAN  */
        return

        calc_standard          /* USAGE_SS- Calc Standard Usage Per Day*/
                               /* C_SS    - Safety Stock    (Calc Var.)*/
                               /* PP_SS   - Safety Stock     (HNYMASTR)*/
                               /* PP_PAN  - Pan Size         (HNYMASTR)*/
           ss% = 0%            /* P_SMCO  - S.S Zero Adj %   (SYSFILE2)*/
           c_ss = pp_ss        /* PP_LEAD - Vendor Lead Time (HNYMASTR)*/
           c_ll = pp_lead      /* C_LL    - Vendor Lead Time(Calc Var.)*/
           c_pp = pp_pan
           if c_ss > .1 then goto L62780
              zz% = int((pp_pan * p_smco) + .5)
              c_ss = zz%
              ss% = 1%                        /* Safety Stock Zero  */
L62780:    usage_ss = round(c_ss / c_ll, 4)   /* Safety Stock Usage */
        return

        REM *************************************************************~
            *           M A I N L I N E   P R O C E S S I N G           *~
            *************************************************************

        process_data
            call "SHOSTAT" ("Re-Order Point Calculations in Progress")
            rop%, cnt% = 0%
            r_key$ = all(hex(00))
            if str(bg_part$,1%,1%) = "A" then goto process_next
               r_key$ = bg_part$
               read #1,key = r_key$, using L63160, r_key$, r_class$,      ~
                                                     eod goto end_process
               goto L63170
        process_next
            read #1,key > r_key$, using L63160, r_key$, r_class$,         ~
                                                     eod goto end_process
L63160:        FMT CH(25), POS(104), CH(4)
L63170:     cnt% = cnt% + 1%
            print at(02,35);hex(84);"Scanned [ ";cnt%;" ]";
                                              /* Check Part Range      */
            if str(bg_part$,1%,1%) = "A" then goto L63240
               if r_key$ > ed_part$ then goto end_process

                                              /* Check ROP Class Range */
L63240:     if str(bg_class$,1%,1%) = "A" then goto L63270
               if r_class$ < bg_class$ or r_class$ > ed_class$ then      ~
                                                        goto process_next
L63270:     gosub load_rop_class         /* (ROPCLASS) - Info. Data    */
                                         /* RC_FORMULA$ = Eq. No.      */
            if class% = 0% then goto process_next
            gosub load_part              /* (HNYMASTR) - Info. Data    */
                                         /* PP_LEAD - Leadtime in Days */
                                         /* pp_TYPE$- Part Type Code   */
                                         /* PP_SS - Safety Stock Value */
                                         /* PP_PAN - Std order Incremen*/
                                         /* Check Part Type Range      */
            if str(bg_type$,1%,1%) = "A" then goto L63390
               if pp_type$ < bg_type$ or pp_type$ > ed_type$ then        ~
                                                        goto process_next
L63390:     if part% = 0% then goto process_next
                                              /* Begin Calc Process    */
            gosub load_usage                  /* From (HNYDETAL) Data  */
            gosub do_calc
            if debug% = 1% then gosub display_screen
            rop% = rop% + 1%
            goto process_next
        end_process
            convert rop% to rop$, pic(#####)

            gosub generate_report
            gosub update_rophny
        return clear all
        goto exit_program

        generate_report
            if r_options$ = "2" then return               /* NO REPORT  */
            gosub select_printer
            cnt% = 0%
            call "SHOSTAT" ("Re-Order Report Creation in Progress")
            r_key$ = all(hex(00))
            read #7,key > r_key$, eod goto generate_done
               goto L63640
        generate_next
            read #7, eod goto generate_done
L63640:     gosub dataload_wrk
            gosub print_detail
            goto generate_next
        generate_done
            gosub close_printer
        return

        update_rophny
            if p_flag% = 0% then goto L63760
               r_key$ = p_part$
               goto L63790

L63760:     if r_options$ = "0" then return               /* NO UPDATE  */
            r_key$ = all(hex(00))

L63790:     cnt% = 0%
            p_flag% = 1%
            call "SHOSTAT" ("ROP - Updating in Progress")
            read #7,key > r_key$, eod goto update_done
               goto L63860
        update_next
            read #7, eod goto update_done
L63860:     cnt% = cnt% + 1%
            print at(02,35);hex(84);"Updated [ ";cnt%;" ]";
            gosub dataload_wrk
            if used = 0.0 and r_formula$ <> "2" then goto update_next
            if rs_ss = 0.0 then c_ss = 0.0
               rs_ss = c_ss              /* Set New Safety Stock Value */
               gosub dataput
               gosub update_parameters
            goto update_next
        update_done
            p_flag% = 0%
            p_part$ = " "
            gosub update_parameters
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")
            if p_flag% = 0% then call "FILEBGON" addr(#7)
            end
