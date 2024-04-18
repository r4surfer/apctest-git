        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDRGA02                             *~
            *  Creation Date     - 05/16/05                             *~
            *  Last Modified Date- 01/01/06                             *~
            *  Written By        - Roy H. Hoffman                       *~
            *  Modifications By  - Roy H. Hoffman                       *~ 
            *                                                           *~
            *  Description       - New RGA Report                       *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                      (RGASTATUS) '---' Save Last Serial   *~
            *                                  Number Assigned          *~
            *        (RGAREASON) RGA Reason Code Associated with Return *~
            *        (RGASTATUS) RGA Salvage Tracking Status Code       *~
            *        (RGATR-LOC) RGA Trailer/Location                   *~
            *        (RGASURGE ) RGA Surge Sku's for use by Planning    *~
            *        (RGASALVAG) 2005 Salvage Costs by Department       *~  
            *                                                           *~
            *  Subroutine Used   - AWDRGB01 (Print RGA Barcode Labels)  *~
            *                                                           *~
            *                                                           *~
            *  Special gosubs -                                         *~
            *                                                           *~
            *                                                           *~ 
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/16/05 ! (New) Report for RGA System              ! RHH *~
            * 01/01/06 ! (PAR000) - CR347 Mod for new Sub Part No.! RHH *~
            * 01/30/23 ! CR3233 Add new product type code salvage ! RDB *~ 
            *************************************************************

        dim                                                              ~
            mode$5,                      /* Use for File Open          */~
            readkey$50, descr$32,        /* GENCODES Lookup            */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            sc_sort$1, sc_sortd$30,      /* Report sort Selection      */~
            sc_data_type$1, sc_data_typed$30, /* RGA report Data       */~  
            dte1$10, beg_dte$10,         /* Beg     Sales Order Date   */~
            dte2$10, end_dte$10,         /* End     Sales Order Date   */~
            beg_ser$8, end_ser$8,        /* Beg/End Serial Number      */~
            sc_rga_status$3,             /* Screen RGA Status Code     */~
            sc_rga_statusd$32,           /* Screen RGA Status Descr    */~
            sc_trailer$6,                /* RGA Trailer Number         */~
            sc_trailerd$32,              /* RGA Trailer Description    */~
            sc_type$1, sc_typed$32,      /* RGA Report Type Summ,Detail*/~
            scr$(10%)40,                 /* Sort Selection Display     */~
            rga_rec$(2%)256,             /* RGA Header Record          */~
            rga_key1$17,                 /* RGA Alt Key (1)            */~ 
            wrk_key$40,                  /* Work File Key              */~
            code$10,                     /* Lookup Code                */~
            company$40,                  /* For Report Company Name    */~
            print_title$40,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */



                                         /* (AWDRGAHD) - Header Data   */
        dim rga_status$3, rga_statusd$30,/* RGA Status/Store(RGASTATUS)*/~
            rga_serial$8,                /* RGA Serial No. (Barcode)   */~
            rga_warranty_id$8,           /* Warranty Id from Sales Ord */~
            rga_so_barcode$18,           /* Barcode assoc. with S. O.  */~
            rga_comp_no$8,               /* Complaint No. with S. O.   */~
            rga_cust$9,                  /* Sales Order Customer No.   */~
            rga_number$4,                /* RGA No. Assigned to Prod.  */~
            rga_number_ln$2,             /* RGA Line Item No.          */~  
            rga_sales_ord$8,             /* Sales Ord. Assoc. W/Prod.  */~		 
            rga_sales_ln$3,              /* Line Item Assoc. W/S.O.    */~
            rga_part$25,                 /* RGA Part No. / MFG Part No.*/~
            rga_sub_part$20,             /* New Sub Part No.   (PAR000)*/~
            rga_descr$30,                /* Part No. Description       */~			
	        rga_comp_code$7,             /* Compalint Code W/Complaint */~
            rga_comp_reas$3,             /* Reason Code W/ Complaint   */~ 
            rga_trailer_loc$6,           /* Trailer/Location Assigned  */~
            rga_trailer$6,               /* Actual RGA Trailer Number  */~  
            rga_reason_cd$3,             /* RGA Reason Code (RGAREASON)*/~
            rga_reason_cdd$30,           /* Description                */~                            
            rga_comment$32,              /* Additional Info. Assoc RGA */~			
            rga_qty_chk$1,               /* Flagged for QTY Check Y/N  */~
            rga_qty_dte$10,              /* RGA Date Checked by Quality*/~
            rga_value$10,                /* RGA Sales Value            */~
            rga_sal_value$10,            /* RGA Salvage Total Value    */~
            rga_sal_mat$10,              /* RGA Salvage Material Value */~
            rga_sal_labor$10,            /* RGA Salvage Labor Value    */~
            rga_sal_over$10,             /* RGA Salvage Overhead Value */~
            rga_credit_amt$10,           /* Credit Amt Assoc. W/S.O.   */~
            rga_credit_dte$10,           /* Date Assoc. with Credit Amt*/~
            rga_orig_date$10,            /* Original Order Date of S.O.*/~
            rga_return_dte$10,           /* Date Assoc. w. RGA Return  */~
            rga_return_tme$5,            /* Time 1st Entere into System*/~
            rga_init_usr$3,              /* User Id for Initial Entry  */~
            rga_dept$3,                  /* Production Department Code */~
            rga_stat_dte$10,             /* Data of Last Status Change */~
            rga_status_seq$2,            /* Add. Data for Same Status  */~
			rga_p_type$4,                /* Salvage product type CR3233*/~
			rga_p_type_desc$20           /* Product Type DescriptCR3233*/

                                         /* (AWDRGADT)- Tracking Detail*/
        dim rga_dtkey$10,                /* Detail Primary Key         */~
            rga_dtrec$128,               /* Detail Record              */~
            rga_status_date$10,          /* Detail Status Change Date  */~
            rga_status_time$5,           /* Detail Status Change Time  */~
            rga_status_seqn$2,           /* Detail Seq. Number For Ser */~
            rga_status_code$3,           /* Detail Status Code         */~
            rga_status_coded$30,         /* Detail Status Code Descr.  */~
            rga_reason$3,                /* Detail Reason Code         */~
            rga_reasond$30,              /* Detail Reason Code Descr   */~
            rga_comm$32,                 /* Detail Comment             */~
            rga_status_usr$3,            /* User Id Assoc w/Stat Chg   */~
            cnt$5,                       /* Selected Data Count        */~
            t1$10, t2$10, t3$10, t4$10,  /* Report Totals              */~
            t5$10, t6$                   /* Report Totals              */ 
 
        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$35, pname$21                                   
            apc$   = "RGA-Special Analysis Report        "
            pname$ = "AWDRGA02 - 06/16/2005"

        REM *************************************************************

            mat f2% = con

            mat f1% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! AWDRGAHD ! RGA Tracking Header File                 *~
            * #02 ! AWDRGADT ! RGA Tracking Detail File                 *~
            * #03 ! GENCODES ! System Master Code Table Files           *~
            * #10 ! RGARPTWK ! Report Work File                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #01, "AWDRGAHD",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 512,                                   ~
                        keypos = 10, keylen = 8,                          ~
                        alt key 1, keypos =  1, keylen =  17,            ~
                            key 2, keypos = 18, keylen =   8, dup,       ~
                            key 3, keypos = 26, keylen =  18, dup,       ~
                            key 4, keypos = 44, keylen =   5, dup,       ~
                            key 5, keypos = 49, keylen =   6, dup,       ~
                            key 6, keypos = 55, keylen =  11, dup,       ~
                            key 7, keypos = 66, keylen =  25, dup  
 

            select #02, "AWDRGADT",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos =  7, keylen = 10,                        ~
                        alt key 1, keypos  =  1, keylen = 16

            select #03,  "GENCODES",                                     ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


            select #10, "RGARPTWK",                                      ~
                        varc,     indexed,  recsize =   552,             ~
                        keypos =    1, keylen =  40

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#02, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#03, fs%(3%), f2%(3%),  0%, rslt$(3%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATFMTC" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            scr$( 1%) = "****************************************"
            scr$( 2%) = "*       Report Sort Selections         *"
            scr$( 3%) = "* 1. By RGA Return Date                *"
            scr$( 4%) = "* 2. By Department Code                *"
            scr$( 5%) = "* 3. By Sales Order Line Item          *"
            scr$( 6%) = "* 4. By Complaint Number               *"
            scr$( 7%) = "* 5. By Original Order Date            *"
            scr$( 8%) = "* 6. By Complaint Reason Code          *"
            scr$( 9%) = "* 7. By Customer Code                  *"
            scr$(10%) = "****************************************"
        

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 7%
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
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 7% then editpg1
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

        print_report
            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work

        REM    gosub open_work_file

            gosub generate_report
        return clear all
        REM GOTO INPUTMODE
        goto exit_program

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
         "Enter a Valid Report Sort Selection (1 thru 7)?              ",~
         "RGA Data Select 1=All,2=(No)Salvage&Scrap,3=Salvage Only,4=Scarp Only?",~ 
         "Enter a Beginning and Ending RGA Return Date or 'ALL'?       ",~
         "Enter a Beginning and Ending Serial Number or 'ALL'?         ",~
         "Enter a Valid RGA Status Code or 'ALL'?                      ",~
         "Enter a Valid Trailer Number or 'ALL'?                       ",~
         "Enter an (S) for Summary, or (D) for Detail?                 "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, dte1$, dte2$, beg_dte$,    ~
                      end_dte$, sc_rga_status$, sc_rga_statusd$, sc_trailer$,~
                      sc_trailerd$, sc_type$, sc_typed$, beg_ser$, end_ser$, ~
                      sc_sort$, sc_sortd$, sc_data_type$, sc_data_typed$
        return

        REM *************************************************************~
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        REM DATALOAD
        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT
        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40180,         /* Sort Selection    */  ~
                                L40180,         /* RGA Data Type     */  ~
                                L40180,         /* Beg/End Date      */  ~
                                L40180,         /* Beg/End Serial No.*/  ~
                                L40180,         /* RGA Status Code   */  ~
                                L40180,         /* RGA Trailer Number*/  ~
                                L40180          /* RGA Rpt Type S or D*/

              goto L40210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,63), "Today:",                                     ~
               at (01,70), fac(hex(8c)), date$                  , ch(10),~
               at (01,24), fac(hex(a4)), apc$                   , ch(35),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Enter Sort Sel. (1-7)? :",                   ~
               at (04,28), fac(lfac$(1%)), sc_sort$             , ch(01),~
               at (04,45), fac(hex(84)), sc_sortd$              , ch(30),~
                                                                         ~
               at (05,02), "Enter RGA Data Type?   :",                   ~
               at (05,28), fac(lfac$(2%)), sc_data_type$        , ch(01),~
               at (05,45), fac(hex(84)), sc_data_typed$         , ch(30),~
                                                                         ~ 
               at (06,02), "Beg/End RGA Return Date:",                   ~
               at (06,28), fac(lfac$(3%)), dte1$                , ch(10),~
               at (06,45), fac(lfac$(3%)), dte2$                , ch(10),~
                                                                         ~
               at (07,02), "Beg/End Serial No, ALL :",                   ~
               at (07,28), fac(lfac$(4%)), beg_ser$             , ch(08),~
               at (07,45), fac(lfac$(4%)), end_ser$             , ch(08),~
                                                                         ~
               at (08,02), "RGA Status Code or ALL :",                   ~
               at (08,28), fac(lfac$(5%)), sc_rga_status$       , ch(03),~
               at (08,45), fac(hex(84)), sc_rga_statusd$        , ch(30),~
                                                                         ~
               at (09,02), "RGA Trailer Number(ALL):",                   ~
               at (09,28), fac(lfac$(6%)), sc_trailer$          , ch(06),~
               at (09,45), fac(hex(84)), sc_trailerd$           , ch(30),~
                                                                         ~
               at (10,02), "RGA S=Summary,D=Detail :",                   ~
               at (10,28), fac(lfac$(7%)), sc_type$             , ch(01),~
               at (10,45), fac(hex(84)), sc_typed$              , ch(30),~
                                                                         ~
               at (11,21), fac(hex(84)), scr$( 1%)              , ch(40),~
               at (12,21), fac(hex(84)), scr$( 2%)              , ch(40),~
               at (13,21), fac(hex(84)), scr$( 3%)              , ch(40),~
               at (14,21), fac(hex(84)), scr$( 4%)              , ch(40),~
               at (15,21), fac(hex(84)), scr$( 5%)              , ch(40),~
               at (16,21), fac(hex(84)), scr$( 6%)              , ch(40),~
               at (17,21), fac(hex(84)), scr$( 7%)              , ch(40),~
               at (18,21), fac(hex(84)), scr$( 8%)              , ch(40),~
               at (19,21), fac(hex(84)), scr$( 9%)              , ch(40),~
               at (20,21), fac(hex(84)), scr$(10%)              , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9% then goto L40215
                  tab% = 26%
                  gosub display_codes
                  goto L40210

L40215:        if keyhit% <> 10% then goto L40220
                  tab% = 28%
                  gosub display_codes
                  goto L40210

L40220:        if keyhit% <> 11% then goto L40230
                  tab% = 27%
                  gosub display_codes
                  goto L40210

L40230:        if keyhit% <> 15% then goto L40560
                  call "PRNTSCRN"
                  goto L40210

L40560:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40750     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &       ~
                     "                       (11)RGA Reason  "
            pf$(2%) = "                 (9)RGA Status          " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                 (10)RGA Trailer        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffff090a0bffffff0f1000)
            if fieldnr% = 1% then L40710
                str(pf$(3%),64%)    = " "  :  str(pfkeys$,16%,1%) = hex(ff)
L40710:     if fieldnr% > 1% then L40730
                str(pf$(1%),18%,26%) = " "  :  str(pfkeys$,4%,1%) = hex(ff)
L40730:     return

L40750: if fieldnr% > 0% then L40840  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (14)Print Report"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40840:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                                       "
            pf$(2%) = "                                        " &       ~
                     "                                       "
            pf$(3%) = "                                        " &       ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50000,         /* Report Sort Selection */~
                              L50600,         /* RGA Data Type Sel.    */~ 
                              L50150,         /* Beg/End Rack Prod Date*/~
                              L50200,         /* Beg/End Serial No.    */~
                              L50300,         /* RGA Status Code       */~
                              L50400,         /* RGA Trailer Number    */~
                              L50500          /* RPT Type S or D       */
            return

L50000: Rem Report Sort Selection                     sc_sort$, sc_sortd$
            sc_sort% = 1%
            convert sc_sort$ to sc_sort%, data goto L50010
L50010:
            convert sc_sort% to sc_sort$, pic(#)

            if sc_sort% < 1% or sc_sort% > 7% then goto L50020
               if sc_sort% = 1% then sc_sortd$ = "By RGA Return Date    "
               if sc_sort% = 2% then sc_sortd$ = "By Department Code    "
               if sc_sort% = 3% then sc_sortd$ = "By Sales Order Ln     "
               if sc_sort% = 4% then sc_sortd$ = "By Complaint No       "
               if sc_sort% = 5% then sc_sortd$ = "By Orig Order Date    "
               if sc_sort% = 6% then sc_sortd$ = "By Complaint Reason Cd"
               if sc_sort% = 7% then sc_sortd$ = "By Customer Code      "
        return
L50020:     errormsg$ = "(Error) - Invalid Report Sort Selection (1 - 6)?"
            gosub error_prompt
            init(" ") sc_sort$, sc_sortd$
        return

L50150: REM Beginning and Ending Rack Prod Date       dte1$,dte2$
            if str(dte1$,1%,3%) = "ALL" then goto L50190
            if dte1$ <> " " then goto L50160
               goto L50190

L50160:        date% = 0%
               call "DATEOKC" (dte1$, date%, errormsg$)
               if errormsg$ <> " " then goto L50185
               if dte2$ <> " " then goto L50180
                  dte2$ = dte1$

L50180:           call "DATEOKC" (dte2$, date%, errormsg$)
                  if errormsg$ <> " " then L50185
               beg_dte$ = dte1$
               end_dte$ = dte2$
               call "DATUFMTC" (beg_dte$)
               call "DATUFMTC" (end_dte$)
               if beg_dte$ > end_dte$ then goto L50195

        return
L50185:        errormsg$ = "(Error) Invalid Order Date."
               init(" ") dte1$, dte2$, beg_dte$, end_dte$
               gosub error_prompt
        return
L50190:     str(dte1$,1%,3%) = "ALL"
            str(dte2$,1%,3%) = "ALL"
            beg_dte$ = "ALL" : end_dte$ = "ALL"
        return
L50195:        errormsg$ = "(Error) Invalid beginning date?"
               init(" ") dte1$, dte2$, beg_dte$, end_dte$
               gosub error_prompt
        return

L50200: REM Beginning and Ending Serial Number          beg_ser$, end_ser$
            if str(beg_ser$,1%,3%) = "ALL" then goto L50290
            if beg_ser$ <> " " then goto L50260
               goto L50290

L50260:        beg_ser% = 0%
               convert beg_ser$ to beg_ser%, data goto L50280

               convert beg_ser% to beg_ser$, pic(00000000)

               read #1,key 0% = beg_ser$, eod goto L50280

               if end_ser$ <> " " then goto L50270
                  end_ser$ = beg_ser$

L50270:        end_ser% = 0%
               convert end_ser$ to end_ser%, data goto L50285

               convert end_ser% to end_ser$, pic(00000000)

               read #1,key 0% = end_ser$, eod goto L50285

               if beg_ser$ > end_ser$ then goto L50295

        return
L50280:        errormsg$ = "(Error) invalid Beginning Serial Number?"
               init(" ") beg_ser$, end_ser$
               gosub error_prompt
        return
L50285:        errormsg$ = "(Error) Invalid Ending Serial Number?"
               init(" ") beg_ser$, end_ser$
               gosub error_prompt
        return
L50290:     str(beg_ser$,1%,3%) = "ALL"
            str(end_ser$,1%,3%) = "ALL"
        return
L50295:        errormsg$ = "(Error) Invalid Beginning Serial Number?"
               init(" ") beg_ser$, end_ser$
               gosub error_prompt
        return

L50300: REM RGA Status Code                         sc_rga_status$, sc_rga_statusd$
            if sc_rga_status$ <> "ALL" then goto L50320
L50310:        sc_rga_statusd$ = "(ALL) RGA Status Codes"
               return

L50320:     if sc_rga_status$ <> " " then goto L50330
               sc_rga_status$ = "ALL"
               goto L50310

L50330:     init(" ") code$
            str(code$,1%,3%) = sc_rga_status$
            gosub lookup_rga_status
            if check% = 0% then goto L50340
            sc_rga_statusd$ = descr$
 
        return
L50340:     errormsg$ = "(Error) Invalid RGA Status Code?"
            init(" ") sc_rga_status$, sc_rga_statusd$
            gosub error_prompt
        return

L50400: REM RGA Trailer                            sc_trailer$, sc_trailerd$
            if str(sc_trailer$,1%,3%) <> "ALL" then goto L50420
L50410:        sc_trailerd$ = "(ALL) RGA trailers"
               return

L50420:     if sc_trailer$ <> " " then goto L50430
               sc_trailer$ = "ALL"
               goto L50410

L50430:     init(" ") code$
            str(code$,1%,6%) = sc_trailer$
            gosub lookup_rga_trailer
            if check% = 0% then goto L50440
            sc_trailerd$ = descr$
 
        return
L50440:     errormsg$ = "(Error) Invalid RGA Trailer?"
            init(" ") sc_trailer$, sc_trailerd$
            gosub error_prompt
        return

L50500: REM RGA Report Type                          sc_type$, sc_typed$
            if sc_type$ <> " " then goto L50510
               sc_type$ = "S"
   
L50510:     if sc_type$ <> "S" and sc_type$ <> "D" then goto L50520

               if sc_type$ = "S" then sc_typed$ = "(S)ummary RGA Report W/Totals "
               if sc_type$ = "D" then sc_typed$ = "(D)etail RGA Report W/Totals  " 
        return
L50520:     errormsg$ = "(Error) Invalid report Type, Only (S or D) Valid?"
            init(" ") sc_type$, sc_typed$
            gosub error_prompt
        return

L50600: REM RGA Data Type                            sc_data_type$, sc_data_typed$
            sc_data_type% = 1%
            convert sc_data_type$ to sc_data_type%, data goto L50610
L50610:
            convert sc_data_type% to sc_data_type$, pic(#)

            if sc_data_type% < 1% or sc_data_type% > 4% then goto L50620
               if sc_data_type$ = "1" then sc_data_typed$ = "(All) RGA Data            "
               if sc_data_type$ = "2" then sc_data_typed$ = "(No) Salvage & Scrap Data "
               if sc_data_type$ = "3" then sc_data_typed$ = "Salvage Data (Only)       "
               if sc_data_type$ = "4" then sc_data_typed$ = "Scrape Data (Only)        "

        return
L50620:    errormsg$ = "(Error) Invalid RGA Data Type (1,2,3,4)?"
           gosub error_prompt
           init(" ") sc_data_type$, sc_data_typed$
        return
 
        lookup_rga_status
           init(" ") readkey$, descr$
           check% = 0%
           str(readkey$,1%,9%) = "RGASTATUS"
           str(readkey$,10%,3%) = str(code$,1%,3%) 
           read #3,key = readkey$, using LOOK_1, descr$, eod goto LOOK_2
LOOK_1:       FMT POS(25), CH(32)
             check% = 1%
LOOK_2: return

        lookup_rga_trailer
           init(" ") readkey$, descr$, rga_trailer$
           check% = 0%
           str(readkey$,1%,9%) = "RGATR-LOC"
           str(readkey$,10%,6%) = str(code$,1%,6%) 
           read #3,key = readkey$, using LOOK_1, descr$, eod goto LOOK_2
             check% = 1%
             if sc_trailer$ = "T00000" then descr$ = "Trailer No. Not Assigned"
                                                 /* Save Actuak Trailer No. */
             rga_trailer$ = str(descr$,21%,6%)

        return

        lookup_rga_reason
           init(" ") readkey$, descr$
           check% = 0%
           str(readkey$,1%,9%) = "RGAREASON"
           str(readkey$,10%,3%) = str(code$,1%,3%) 
           read #3,key = readkey$, using LOOK_1, descr$, eod goto LOOK_2
             check% = 1%
        return
		
/* CR3233 */		
		lookup_p_type_desc
		   init(" ") readkey$, descr$, rga_p_type_desc$

           str(readkey$,1%,9%) = "RGAPRDTYP"
           str(readkey$,10%,4%) = rga_p_type$ 
           read #3,key = readkey$, using LOOK_1, descr$, eod goto LOOK_2
			 rga_p_type_desc$ = str(descr$,1%,20%)
		
		return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */

L55020: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~+

L55050: %!########## ########                           #################~
        ~#######################         Sort:################## AWDRGA02:~
        ~!

L55090: %!Return DTE Beg: ########## End: ##########  ###################~
        ~#####################   Beg Ser:######## End:########  Page: ####~
        ~!

L55130: %!Status: ### ############################## Trailer: ###### ####~
        ~#####################  Rpt Type: # ##############################~
        ~!

L55140: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~!
                                          /* Summary Records           */
L55200: %!Ser:########  Cust:######### Sales:######## ###  Comp:######## ~
        ~Warr:######## RGA:#### ## Stat:### ##############################~
        ~!

L55300: %!  Return Dte:########## Trailer:###### Prod Bar:###############~
        ~### RGA Comp Reason: ###  Reas:### ##############################~
        ~!

L55400: %!  Part:######################### ##############################~
        ~ Comm: ################################ Qual:# ########## Dpt:###~
        ~!

L55500: %!  Prod Val:########## Salvage:########## Mat:########## Lab:###~
        ~####### OvH:########## Cred Amt:########## ########## ###########~	
        ~!

L55600: %!  Ord Dte:########## RGA Comp Cd: ####### Last Date: ##########~
        ~ Last Time:##### Last User: ### Last Seq. ##                     ~
        ~!

                                          /* Detail Records             */
L55700: %! * ########## ##### Seq:## St:###[#########################]Rs:~
        ~###[#########################]############################### ###~
        ~!
                            
                                           /* Total Record               */
L55800: %!CT##### Value:########## Salvage:########## Material:##########~
        ~ Labor:########## Over Head:##########   Credit Amount:##########~
        ~!
        
        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            call "SETPRNT" ("RGARPT", " ",25000%, 0%)
            select printer (134)

            page_no% = 0%
            lcnt%    = 99%
  
            print_title$ = " RGA Summary/Detail Analysis Report  "
            call "FMTTITLE" (print_title$, " ", 12%)
            date$ = date  :  call "DATFMTC" (date$)
            call "TIME" (rpt_time$)
            company$ = "ATRIUM Windows and Doors"
            call "FMTTITLE" (company$, " ", 12%)
        return

        close_printer
            call "SETPRNT" ("RGARPT", " ",0%, 1%)
        return

        generate_report
                                         /* Check label file for data */ 
            gosub selecting_data
                                         /* Print sorted report       */

            call "SHOSTAT" ("PRINTING REPORT")
            call "PAUSE" ADDR(100%)


            gosub select_printer

            t1, t2, t3, t4,t5, t6 = 0.0
            init(" ") wrk_key$
            count% = 0%
            wrk_key$ = all(hex(00))

        generate_report_next
            init(" ") rga_serial$, rga_number$, rga_number_ln$, rga_sales_ord$,     ~
                      rga_sales_ln$, rga_warranty_id$, rga_comp_no$, rga_status$,   ~
                      rga_statusd$, rga_return_dte$, rga_trailer_loc$,              ~
                      rga_so_barcode$, rga_comp_reas$, rga_reason_cd$,              ~
                      rga_reason_cdd$, rga_part$, rga_descr$, rga_comment$,         ~
                      rga_qty_chk$, rga_qty_dte$, rga_dept$, rga_value$,            ~
                      rga_sal_value$, rga_sal_mat$, rga_sal_labor$, rga_sal_over$,  ~
                      rga_credit_amt$, rga_credit_dte$, rga_orig_date$,             ~
                      rga_comp_code$, rga_stat_dte$, rga_return_tme$, rga_init_usr$,~
                      rga_status_seq$, rga_rec$(), rga_cust$, rga_sub_part$, ~
                      rga_p_type$					  
                                                                   /* (PAR000)     */       
            read #10,key > wrk_key$, using GEN_1, wrk_key$, rga_rec$(),  ~
                                                  eod goto generate_done
GEN_1:         FMT CH(40), 2*CH(256)
            count% = count% + 1%
            convert count% to cnt$, pic(#####)
                                               /* Serial Number      */
            rga_serial$ = str(rga_rec$(),10%,8%)
                                               /* Customer Number    */
            rga_cust$   = str(rga_rec$(),257%,9%) 
                                               /* RGA Number         */
            rga_number$ = str(rga_rec$(),49%,4%)
                                               /* RGA Line Item      */
            rga_number_ln$ = str(rga_rec$(),53%,2%)
                                               /* RGA Sales Order    */
            rga_sales_ord$ = str(rga_rec$(),55%,8%)
                                               /* RGA Sales Ord Line */
            rga_sales_ln$  = str(rga_rec$(),63%,3%)
                                               /* RGA Warranty Id    */
            rga_warranty_id$ = str(rga_rec$(),18%,8%)
                                               /* RGA Complaint No.  */
            str(rga_comp_no$,1%,5%) = str(rga_rec$(),44%,5%)
            gosub unpack_comp_number
                                               /* RGQA Status Code   */
            rga_status$ = str(rga_rec$(),7%,3%)
            init(" ") code$
            str(code$,1%,3%) = rga_status$

            gosub lookup_rga_status
            rga_statusd$ = descr$
                                               /* End (S) - 1        */
     
                                               /* RGA Return Date    */
            str(rga_return_dte$,1%,6%) = str(rga_rec$(),1%,6%) 
            call "DATFMTC" (rga_return_dte$)
                                               /* RGA Trailer        */
            rga_trailer_loc$ = str(rga_rec$(),131%,6%)
            init(" ") code$
            str(code$,1%,6%) = rga_trailer_loc$
            gosub lookup_rga_trailer
            rga_trailer_loc$ = rga_trailer$    /* Actual Trailer No. */

                                               /* Prod. Barcode      */
            rga_so_barcode$  = str(rga_rec$(),26%,18%)
                                               /* RGA Complaint Reas */
            rga_comp_reas$   = str(rga_rec$(),128%,3%)
                                               /* RGA Reason Code    */
            rga_reason_cd$   = str(rga_rec$(),137%,3%)
            init(" ") code$
            str(code$,1%,3%) = rga_reason_cd$
            gosub lookup_rga_reason
            rga_reason_cdd$ = descr$
                                               /* End of (S) - 2     */
/* CR3233 */
            rga_p_type$ = str(rga_rec$(),286%,4%)
            gosub lookup_p_type_desc
                                               /* MFG Part Number    */
            rga_part$ = str(rga_rec$(),66%,25%)
                                               /* (PAR000)           */
            rga_sub_part$ = str(rga_rec$(),266%,20%)
                                               /* (PAR000)           */

                                               /* Part Description   */
            rga_descr$= str(rga_rec$(),91%,30%)
                                               /* RGA Comment        */
            rga_comment$ = str(rga_rec$(),140%,32%)
                                               /* RGA Quaklity Check */
            rga_qty_chk$ = str(rga_rec$(),172%,1%)
                                               /* RGA Quality Date   */
            str(rga_qty_dte$,1%,6%) = str(rga_rec$(),173%,6%)
            call "DATFMTC" (rga_qty_dte$)
                                               /* Department Code    */
            rga_dept$ = str(rga_rec$(),246%,3%)
                                               /* End of (S) Line 3  */  

                                               /* Product Value      */
            rga_value, rga_sal_value, rga_sal_mat, rga_sal_labor, rga_sal_over = 0.0
            get str(rga_rec$(),179%,8%), using GEN_2, rga_value
GEN_2:         FMT PD(14,4)
            t1 = t1 + rga_value
            convert rga_value to rga_value$, pic(####.####-)

                                               /* Salvage Value      */
            get str(rga_rec$(),187%,8%), using GEN_2, rga_sal_value
            t2 = t2 + rga_sal_value
            convert rga_sal_value to rga_sal_value$, pic(######.##-)

                                               /* Salvage Value Mat  */
            get str(rga_rec$(),195%,8%), using GEN_2, rga_sal_mat
            t3 = t3 + rga_sal_mat
            convert rga_sal_mat to rga_sal_mat$, pic(######.##-)

                                               /* Salvage Value Lab  */
            get str(rga_rec$(),203%,8%), using GEN_2, rga_sal_labor
            t4 = t4 + rga_sal_labor
            convert rga_sal_labor to rga_sal_labor$, pic(######.##-)

                                               /* Salvage Value Ovh  */
            get str(rga_rec$(),211%,8%), using GEN_2, rga_sal_over
            t5 = t5 + rga_sal_over
            convert rga_sal_over to rga_sal_over$, pic(######.##-)

                                               /* Credit Amt         */
            rga_credit_amt = 0.0
            get str(rga_rec$(),219%,8%), using GEN_2, rga_credit_amt
            t6 = t6 + rga_credit_amt
            convert rga_credit_amt to rga_credit_amt$, pic(######.##-)

                                               /* Credit Amt Dte     */
            str(rga_credit_dte$,1%,6%) = str(rga_rec$(),227%,6%)
            call "DATFMTC" (rga_credit_dte$)
                                               /* End of (S) Line 4  */
 
                                               /* Original Ord Date  */
            str(rga_orig_date$,1%,6%) = str(rga_rec$(),233%,6%)
            call "DATFMTC" (rga_orig_date$)

                                               /* RGA Complaint Code */
            rga_comp_code$ = str(rga_rec$(),121%,7%)

                                               /* Date Last Changed  */
            str(rga_stat_dte$,1%,6%) = str(rga_rec$(),251%,6%)
            call "DATFMTC" (rga_stat_dte$)

                                               /* Time of Last Change*/
            rga_return_tme$ = str(rga_rec$(),239%,2%) & ":" &          ~
                              str(rga_rec$(),241%,2%)
    
                                               /* User id for Last Chg*/
            rga_init_usr$ = str(rga_rec$(),243%,3%)

                                               /* Last Detail Seq. No.*/
            rga_status_seq$ = str(rga_rec$(),249%,2%)
                                               /* End of (S) Line 5   */

            gosub print_summary

            if sc_type$ = "D" then gosub print_detail

            goto generate_report_next
        generate_done
            gosub print_totals

            gosub close_printer

            gosub delete_work

        REM    call "FILEBGON" addr(#10)
        return

        print_header                        /* Page Header       */
          page_no% = page_no% + 1%
          if lcnt% <> 99% then print using L55020
          lcnt% = 0%
          print page
          print using L55020
          print using L55050, date$, rpt_time$, company$, str(sc_sortd$,1%,18%)
          print using L55090, dte1$, dte2$, print_title$, beg_ser$,~
                              end_ser$, page_no%
          print using L55130, sc_rga_status$, sc_rga_statusd$,     ~
                              sc_trailer$, str(sc_trailerd$,1%,25%),~
                              sc_type$, sc_typed$
          lcnt% = lcnt% + 4%
        return

        print_summary                       /* Summary Data      */

          if lcnt% = 99% or lcnt% > 58% then gosub print_header

          print using L55140

                                            /* Print Data Ln-1   */
          print using L55200, rga_serial$, rga_cust$, rga_sales_ord$, rga_sales_ln$,~
                              rga_comp_no$, rga_warranty_id$, rga_number$,          ~
                              rga_number_ln$, rga_status$, rga_statusd$
                                           
                                            /* Print Data Ln-2   */
          print using L55300, rga_return_dte$, rga_trailer_loc$, rga_so_barcode$,~
                              rga_comp_reas$, rga_reason_cd$, rga_reason_cdd$

                                            /* Print Data Ln-3   */ 
          print using L55400, rga_part$, rga_descr$, rga_comment$, rga_qty_chk$,~
                              rga_qty_dte$, rga_dept$

                                            /* Print Data Ln-4   */
											/* CR3233            */
          print using L55500, rga_value$, rga_sal_value$, rga_sal_mat$,       ~
                              rga_sal_labor$, rga_sal_over$, rga_credit_amt$, ~
                              rga_credit_dte$, rga_p_type_desc$

                                            /* Print Data Ln-5   */
          print using L55600, rga_orig_date$, rga_comp_code$, rga_stat_dte$,    ~
                              rga_return_tme$, rga_init_usr$, rga_status_seq$

          lcnt% = lcnt% + 6%

        return

        print_detail
            if sc_type$ = "S" then return
 
             init(" ") rga_dtkey$

             str(rga_dtkey$,1%,8%) = rga_serial$
PRT_1:  
             if lcnt% = 99% or lcnt% > 58% then gosub print_header

             read #2,key 0% > rga_dtkey$, using PRT_2, rga_dtrec$, eod goto PRT_3
PRT_2:         FMT CH(128)

             if rga_serial$ <> str(rga_dtrec$,7%,8%) then goto PRT_3
                rga_dtkey$ = str(rga_dtrec$,7%,10%)
 
                init(" ") rga_status_date$, rga_status_time$, rga_status_seqn$,  ~
                          rga_status_code$, rga_status_coded$, rga_reason$,      ~
                          rga_reasond$, rga_comm$, rga_status_usr$
  
                                           /* Date Assoc. with Status Change */
                str(rga_status_date$,1%,6%) = str(rga_dtrec$,1%,6%)
                call "DATFMTC" (rga_status_date$)
                                           /* Time Assoc. with Status Change */
                rga_status_time$ = str(rga_dtrec$,26%,2%) & ":" &             ~
                                   str(rga_dtrec$,28%,2%)
                                           /* Detail Seq Number              */
                rga_status_seqn$ = str(rga_dtrec$,15%,2%)
                                           /* Detail Status Code             */
                rga_status_code$ = str(rga_dtrec$,17%,3%)
                init(" ") code$
                str(code$,1%,3%) = rga_status_code$
                gosub lookup_rga_status
                                           /* Detail Status Code Descript    */
                rga_status_coded$ = descr$
                                           /* Detail Reason Code             */
                rga_reason$ = str(rga_dtrec$,20%,3%)
                init(" ") code$
                str(code$,1%,3%) = rga_reason$
                gosub lookup_rga_reason
                                           /* Detail Reason Code Descript    */
                rga_reasond$ = descr$
                                           /* Detail Comment                 */
                rga_comm$ = str(rga_dtrec$,36%,32%)
                                           /* User Id for Status Change      */
                rga_status_usr$ = str(rga_dtrec$,23%,3%)
    
                print using L55700, rga_status_date$, rga_status_time$,~
                                    rga_status_seqn$, rga_status_code$,~
                                    str(rga_status_coded$,1%,25%), rga_reason$,~
                                    str(rga_reasond$,1%,25%), str(rga_comm$,1%,31%),~
                                    rga_status_usr$
  
                lcnt% = lcnt% + 1%
                goto PRT_1
PRT_3:
        return 

        print_totals
            print using L55140
                                  /* Total of Product Value    */
            convert t1 to t1$, pic(######.##-)

                                  /* Total of Salvage Value    */
            convert t2 to t2$, pic(######.##-)

                                  /* Total of Salvage Material */
            convert t3 to t3$, pic(######.##-)

                                  /* Total of Salvage Labor    */
            convert t4 to t4$, pic(######.##-)

                                  /* Total of Salvage Over     */
            convert t5 to t5$, pic(######.##-)

                                  /* Total Value of Credits    */
            convert t6 to t6$, pic(######.##-)

            print using L55800, cnt$, t1$, t2$, t3$, t4$, t5$, t6$

            print using L55020

        return


        selecting_data
             if sc_type$ = "S" then call "SHOSTAT" ("Selecting RGA Summary Data ")
             if sc_type$ = "D" then call "SHOSTAT" ("Selecting RGA Detail Data")

             count% = 0%

             rga_key1$ = all(hex(00))

             if str(dte1$,1%,3%) <> "ALL"                                    ~
                              then str(rga_key1$,1%,6%) = str(beg_dte$,1%,6%)
        selecting_data_next  
             read #1,key 1% > rga_key1$, using SEL_1, rga_rec$(),            ~
                                                 eod goto selecting_data_done
SEL_1:          FMT 2*CH(256)

             if mod(count%,25%) <> 0 then goto SEL_2
                convert count% to count$, pic(######)
                call "SHOSTAT" ("Rga Header Records Scanned ("&count$&")")

SEL_2:       count% = count% + 1%
             convert count% to cnt$, pic(#####)
                                         /* Set Alt Key (1)            */
             rga_key1$ = str(rga_rec$(),1%,17%)
                                         /* Check Data Type            */
             gosub select_data_type
             if select% = 0% then goto selecting_data_next
 
                                         /* Check RGA Return Date      */
             if str(dte1$,1%,3%) = "ALL" then goto SEL_2A
                if str(rga_rec$(),1%,6%) > str(end_dte$,1%,6%) then       ~
                                               goto selecting_data_done
SEL_2A:
                                         /* Check RGA Serial Number    */
             if str(beg_ser$,1%,3%) = "ALL" then goto SEL_4
                if str(rga_rec$(),10%,8%) < beg_ser$ or                   ~
                   str(rga_rec$(),10%,8%) > end_ser$ then                 ~
                                               goto selecting_data_next   

                                         /* Check RGA Status Code      */
SEL_4:       if sc_rga_status$ = "ALL" then goto SEL_5
                if str(rga_rec$(),7%,3%) <> sc_rga_status$ then          ~
                                              goto selecting_data_next

                                         /* Check RGA Trailer Number   */
SEL_5:       if str(sc_trailer$,1%,3%) = "ALL" then goto SEL_6
                if sc_trailer$ <> str(rga_rec$(),131%,6%) then          ~
                                              goto selecting_data_next
                                         /* Check Report Type          */
SEL_6: 
               init(" ") wrk_key$        /* Sort Selection             */
                                         /* Return Date                */
               if sc_sort% = 1% Then                                     ~
                            str(wrk_key$,1%,11%) = str(rga_rec$(),1%,6%) & "     "

                                         /* Department Code            */
               if sc_sort% = 2% Then                                     ~
                            str(wrk_key$,1%,11%) = str(rga_rec$(),246%,3%) & "        "

                                         /* Sales Order Line Item      */
               if sc_sort% = 3% Then                                     ~
                            str(wrk_key$,1%,11%) = str(rga_rec$(),55%,11%)

                                         /* Complaint number           */
               if sc_sort% = 4% then rga_comp_no$ = str(rga_rec$(),44%,5%)
               if sc_sort% = 4% then gosub unpack_comp_number
               if sc_sort% = 4% and rga_comp_no$ = "00000000" then       ~
                                                rga_comp_no$ = "99999999"
               if sc_sort% = 4% Then                                     ~
                            str(wrk_key$,1%,11%) = rga_comp_no$ & "   "

                                         /* Original Sales Order Date  */
               if sc_sort% = 5% then                                     ~
                            str(wrk_key$,1%,11%) = str(rga_rec$(),233%,6%) & "     " 

                                         /* Complaint Reason Code      */ 
               if sc_sort% = 6% Then                                     ~
                            str(wrk_key$,1%,11%) = str(rga_rec$(),128%,3%) & "        "

                                         /* Sales Order Customer Code  */
               if sc_sort% = 7% then                                     ~
                            str(wrk_key$,1%,11%) = str(rga_rec$(),257%,9%) & "   "
                                         /* Put No Data At the End     */
               if str(wrk_key$,1%,11%) <> " " then goto SEL_7
                  str(wrk_key$,1%,11%) = "99999999999"
SEL_7:
                                        /* Sales Ord. & Line           */
               str(wrk_key$,12%,11%) = str(rga_rec$(),55%,11%)
                                        /* Warranty Id                 */
               str(wrk_key$,23%,8%) = str(rga_rec$(),18%,8%)
                                        /* Serial Number               */
               str(wrk_key$,31%,8%) = str(rga_rec$(),10%,8%) 
                                        /* Set Count                   */
               gosub update_work
               goto selecting_data_next
        selecting_data_done
            if count% = 0% then goto SEL_8

        return
SEL_8:
            errormsg$ = "No RGA Data Found"
            gosub error_prompt
        return 

        update_work
            write #10, using UPD_1, wrk_key$, rga_rec$(), eod goto UPD_2

UPD_1:         FMT CH(40), 2*CH(256)
        return
UPD_2:
            call "SHOSTAT" ("(Error) Serial Number - " & str(wrk_key$,1%,8%) )
            stop
        return

        REM open_work_file
        REM    call "OPENCHCK" (#10, fs%(10%), f2%(10%), 500%, rslt$(10%))
        REM return

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        unpack_comp_number
            rga_comp_no% = 0%
            get str(rga_comp_no$,1%,4%), using PACK_2, rga_comp_no%
PACK_2:             FMT BI(4)
            convert rga_comp_no% to rga_comp_no$, pic(00000000)

        return 

        display_codes
            call "APCPLN1B" (tab%, #3)
        return

        select_data_type
            select% = 0%
                                      /* Selection for All Data         */
            if sc_data_type$ = "1" then goto DATA_3
                                      /* Selection No Salavage or Scrap */
            if sc_data_type$ <> "2" then goto DATA_1
               if str(rga_rec$(),7%,3%) <> "010" and               ~
                  str(rga_rec$(),7%,3%) <> "014" then goto DATA_3
               return
DATA_1:                               /* Salvage Data (Only)            */
            if sc_data_type$ <> "3" then goto DATA_2
               if str(rga_rec$(),7%,3%) = "010" then goto DATA_3
               return
DATA_2:                               /* Scrap Data (Only)              */
               if str(rga_rec$(),7%,3%) = "014" then goto DATA_3  
        return
DATA_3:     select% = 1%
        return 

       open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#10,mode$, 500%, f2%)
            if f2% <> 0% then goto WORK_1
        return
WORK_1:     call "SHOSTAT" ("Error - Cannot Open (RGARPTWK)") : stop
        return
        delete_work
            call "FILEBGON" (#10)
        return


        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program

            end
