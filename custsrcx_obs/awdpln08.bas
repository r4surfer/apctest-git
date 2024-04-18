        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN08                             *~
            *  Creation Date     - 09/24/03                             *~
            *  Last Mod Date     - 04/26/2010                            *~
            *  Written By        - Christie Gregory                     *~
            *                                                           *~
            *  Description       - Analysis Special Scheduled Remake    *~
            *                      Glass                                *~
            *                                                           *~
            *  Code Tables Used  - (PLAN SCH1), (PLAN SCH2)             *~
            *                                                           *~
            *  Subroutine Used   - (EWDPLA58) Display Customer Info     *~
            *                      (APCPLN3B) Calculate New Due Date    *~
            *                      (EWDGLSSB) Calculate Glass Cuts      *~
            *                      (EWDPLB58) Verify Delete             *~
            *                      (EWDPLC58) Print Labels on Zebra     *~
            *                                                           *~
            *  Special Comments  - gen_rpt1 - Create Tempered Glass Rpt *~
            *                      print_dtl- Print Report Detail       *~
            *                                                           *~
            *                      gen_labels     - Print Glas Labels   *~
            *                      print_lab_temp - Write to file       *~
            *                      build_lab_temp - Build Label Record  *~
            *                                                           *~
            *                      update_specials - Place or Release   *~
            *                      edit_schedule   - Check Records      *~
            *                      modify_schedule - Modify Records     *~
            *                      update_apcplnor - Update             *~
            *                      update_apcplnsc - Update             *~
            *                                                           *~
            *                      delete_specials - Delete Type '0's   *~
            *                                                           *~
            *                      release_specials - Release to Plan   *~
            *                      update_apcplnor_rel - Update         *~
            *                      update_apcplnsc_rel - Update         *~
            *                                                           *~
            *  sp_flag% = (1%) Place Items On Order                     *~
            *             (2%) Receive Items On Order                   *~
            *             (3%) Release Items On-Order                   *~
            *             (4%) Delete Special Orders (Type = '0' Only)  *~
            *                                                           *~
            *  rpt%     = (0%) Regular Report based on Selections       *~
            *             (1%) Tempered Glass Labels Only               *~
            *                                                           *~
            *  report%  = (0%) Glass Report/Labels from Display Screen  *~
            *             (1%) Glass Reports/Labels from Main Screen    *~
            *                                                           *~
            *             single strength                 spType% = 1%  *~
            *             double strength                 spType% = 2%  *~
            *             tempered double strength        spType% = 3%  *~
            *             SDL Single strength             spType% = 4%  *~
            *             SDL Double strength             spType% = 5%  *~
            *             SDL Temp Double strength        spType% = 6%  *~
            *             Triple Strength > 25SqFt 3/16   spType% = 7%  *~ 
            *             Door 5/32 Triple Strength       spType% = 8%  *~
            *             No Sandwich PLANCSAND                   998%  *~
            *             Not Tempered                            999%  *~            
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/24/03 ! (New) Program                            ! CMG *~
            * 11/16/05 ! (AWD001) Mod for Cardinal Glass File     ! CMG *~
            * 01/01/06 ! (PAR000) CR347 Mod for sub part          ! CMG *~
            * 03/16/06 ! (AWD002) - modification for North East   ! CMG *~
            *05/30/2007! (AWD003) - modification for 18mm         ! DES *~
            *10/28/2008! (AWD004) - mod to add extra field to     ! CMG *~
            *          !      EWDPLA58 call                       !     *~
            *11/30/2009! (AWD005) - mod for ULTRA Intercept       ! CMG *~
            *04/26/2010! (AWD006) - mod for tempered order file   ! CMG *~
            *03/22/2011! (AWD007) - mod for intercept systems     ! CMG *~
            *************************************************************

        dim f2%(30%),                    /* = 0 if the file is open    */~
            filename$8,                  /* File Name for Open         */~
            f1%(30%),                    /* = 1 if READ was successful */~
            fs%(30%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
            schema$8,                    /*   not yet checked (OPENCHCK*/~
            rslt$(30%)20                 /* Text from file opening     */

        
        dim                                                              ~
            title$45, company$40,        /* Analysis Title and Time    */~
            beg_dte$6, beg_date$10,      /* Beg/End Delivery Date      */~
            end_dte$6, end_date$10,      /*                            */~
            beg_cut$2, end_cut$2,        /* Beg/End Cut-Off            */~
            beg_cut_d$35, end_cut_d$35,  /* Beg/End Cut-Off DESC        */~
            cutoff_a$2, cutoff_b$,       /* Customer Cut-Off            */~
            x$10, pf_txt$20,             /* Date Buffer                */~
            sc_status$1, sc_status_d$30, /* Special Analysis Process Cd*/~
            sc_type$1, sc_type_d$30,     /* Analysis Type Process Code */~
            sc_process$1,sc_process_d$35,/* Process Selection Codes    */~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            readkey1$50,                 /* GENCODES Lookup 1          */~
            sp_status_dte$6,             /* Special Analysis Status Dte*/~
            sp_status_date$8,            /* Formated                   */~
            sp_status$1, sp_st$1,        /* Special Analysis Process Cd*/~
            sp_type$1, or_st$2,          /* Analysis Type Process Code */~
            sp_route$5, or_hows$2,       /* Route code                 */~
            sp_cutoff$2,                 /* cut off Day 1 thru 7       */~
            sp_cust$9,                   /* Customer Code              */~
            sp_so$8,                     /* Sales order number         */~
            sp_ln$2,                     /* S.O. Line item No.         */~
            sp_ln_item$4,                /* Line Item Piece            */~
            sp_due$6, sp_due_date$8,     /* Sales order Due Date       */~
            sp_part$25, sp_part_d$32,    /* Part Number                */~
            sp_qty$4,                    /* line Item Quantity         */~
            sp_stat$2,                   /* (PLAN STAT) Planning       */~
            sp_usr$3,                    /* Last Mod User Id           */~
            sp_dte$6, sp_date$8,         /* Last Mod Date              */~
            sp_time$8,                   /* Last Mod Time              */~
            sp_text$4, sp_rec$128,       /* Line Item Text Id          */~
            sp_primary$3,                /* Primary Dept               */~
            sp_fil$17,                   /* Filler Area                */~
            sp_warr$9,                   /* Glass Barcode              */~
            sp_num$3,                    /* Glass Remake Num           */~
            sp_sand1$2,                  /* First Sandwich Code        */~
            sp_sand2$2,                  /* Sec Sandwich Code          */~
            sandwich$(2%)30,             /* Data to write File         */~
            sp_lits$4,                   /* Number of Lites            */~
            sp_temp$2,                   /* Tempered Field             */~
            sp_temp1$2,                  /* Tempered Field             */~
            sp_temp2$2,                  /* Tempered Field             */~
            sp_rack$1,                   /* Rack Type '0' or '1'       */~
            spType$3,                    /* Spacer Type (AWD006)       */~
            strength$1,                  /* Glass Strength (AWD006)    */~
            tpSand$10,                   /* Tempered Sandwich (AWD006) */~
            tp_comma$1,                  /* File Delimiter             */~
            sp_view$1,                   /* Top/Bot View               */~
            tp_item$4,                   /* Tempered Item              */~
            tp_model$3,                  /* Tempered Model             */~
            tp_glass$2,                  /* Tempered Glass             */~
            tp_color$1,                  /* Tempered Color             */~
            tp_so$8,                     /* Tempered So                */~
            tp_ln$2,                     /* Tempered Line              */~
            tp_view$3,                   /* Tempered View              */~
            tp_size$20,                  /* Tempered Size              */~
            tp_type$30,                  /* Tempered Glas Type         */~
            tp_lite$1,                   /* Tempered Lites             */~
            tp_spacer$10,                /* Tempered Spacer            */~
            tp_sp_th$10,                 /* Tempered Spacer Thickness  */~
            sav_spacer$10,               /* Tempered Spacer            */~
            sp_temp_key$100,             /* Read key Sort File         */~
            sd1$24, sd2$24,              /* Beg/End Date Screen Text   */~
            sp_key$50, sav_key$35,       /* SP Primary Key and Record  */~
            sav_ord$8, sav_order$17,     /* Save Sales Order           */~
            sav_line$10,                 /* Sales order Line item      */~
            txt$(5%)30,                  /* Shostat Text               */~
            apc_scr$120,                 /* Screen Description         */~
            apc_prt$60,                  /* Print Description          */~
            apc_sze$20,                  /* Size Long Form             */~
            s_23m$3,                     /* Model Code for Series      */~
            s_23$8,                      /* New Series Code            */~
            s_so$8,                      /* Sales Order                */~
            s_ln$3,                      /*** Line Item (3)            */~
            s_prv$30,                    /* Private Label Name         */~
            s_1$2,                       /* Private Label Code         */~
            code$3, tab$(10%)9,          /* Tables for Lookup          */~
            h1$12, h2$12, h3$12, h4$12,  /* Summary Screen Display     */~
            h5$12, h6$12, h7$25, h8$25,  /*                            */~
            h9$12,                       /*                            */~
            sel$(6%)20,                  /* Display Text               */~
            textid$4,                    /* Text Id's                  */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            dsp_msg$79,                  /* Screen Display Message     */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3,                    /* Current User Id            */~
            t_k$6,                       /* Thickness                  */~
            gr_time$8                    /* Same Status Time           */


        dim ctt$(10%,3%)9, dept$3,       /* 1-5=Top, 6-10=Bot          */~
            gdd$(12%)10, view$3,         /* 1-6=Top, 7-12=Bot          */~
            ajj$(10%,2%)6,               /* 1-5=Top, 6-10=Bot          */~
            dcc$(10%,2%)8,               /* 1-5=Top, 6-10=Bot          */~
            wd$7, wd1$9,                 /* Actual Width               */~
            ht$6, ht1$8,                 /* Actual Height              */~
            rk_barcode$9, rk_seq$        /* Glass/Rack Barcode         */

        dim dt$(600%)80,                 /* Analysis Display           */~
            cc$(600%)1                   /* Selection                  */

        dim                              /* FILE - LABEL PRINT FILES   */~
            muttin$8, lits$1, mut$8,     /* Muttin Code Vert/Horiz     */~
            t1$3, chg$1,                 /* Model Code                 */~
            t2$6,                        /* Color Code                 */~
            t3$15,                       /* Glass Description          */~
            t4$6,                        /* Liting                     */~
            t5$20,                       /* Width and Height           */~
            t6$3,                        /* View Top/Bot               */~
            t7$11,                       /* Sales Order Line Item      */~
            t8$8,                        /* Due Date                   */~
            t9$1,                        /* Contour Grid               */~
            t10$5,                       /* (AWD005) Ultra Intercept   */~
            ff$8,                        /* Label Print File Name      */~
            library$8,                   /* Library Name = 'APCDATA'   */~
            volume$6,                    /* Volume Name = 'CARLOS'     */~
            l_lt$6, lt$2,                /* Liting Left Descr          */~
            r_lt$6,                      /* Liting Right Descr         */~
            cl$1,                        /* Color Code                 */~
            lk$1                         /* Lock Code                  */

        dim                              /*                            */~
            gr_key$33,                   /* AWDPLNGR readkey           */~
/*PAR000*/  gr_rec$(2%)192               /* AWDPLNGR record            */


        dim                              /* (PAR000)                   */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4,            /* Additional Info Fields     */~
            field1$1,                    /* New Part Field 1 GRDTYPE   */~
            field2$1,                    /* New Part Field 2 GRDSIZE   */~
            field3$1,                    /* New Part Field 3 GRDCOLOR  */~
            field4$1,                    /* New Part Field 4 HARDWARE  */~
            field5$1,                    /* New Part Field 5 FOAM      */~
            field6$1,                    /* Casing        (AWD006)     */~
            field7$1,                    /* Sample Color  (AWD006)     */~
            field8$1,                    /* EXT Grid Type (AWD006)     */~
            field9$1,                    /* EXT Grid Size (AWD006)     */~
            field10$1,                   /* INT Grid Color (AWD006)     */~
            field11$1,                   /* EXT Grid COLOR (AWD006)     */~
            subpart$20,                  /* Subpart Number   AWD006    */~
            infopart$20                  /* Info Part Number AWD006    */            

        dim                              /* (AWD001)                   */~
            csandwich$(2%)30,            /* Cardinal Sandwich  (AWD001)*/~
            tp_ctype$30,                 /* Cardinal Glass Type(AWD001)*/~
            header1$15,                  /* Header Information 1       */~
            header2$15,                  /* Header Information 2       */~
            header3$15,                  /* Header Information 3(AWD006)*/~
            header4$30                   /* Header Information 4(AWD006)*/

/* (AWD007) */
        dim intercept$2,                 /* Intercept                  */~
            sav_intercept$2,             /* Save Intercept for racks   */~
            interdesc$(9%)5,             /* Intercept Descriptions     */~
            model$3, ty$2,               /* Model and Glass Type       */~
            spTypeDesc$(999%)30          /* SP Type Descriptions       */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Special Analysis of Remake Glass "
            pname$ = "AWDPLN08 - Rev: R7.00"

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
            * #1  ! EWDSCRMK ! Special Temp Glass Remakes               *~
            * #2  ! AMTBOMCD ! Master Equation File                     *~
            * #3  !          !                                          *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            * #5  ! APCPLNOR ! Planning Header Histroy                  *~
            * #6  ! APCPLNSC ! Planning Master Schedule File            *~
            * #7  ! CUSTOMER ! Customer Master File                     *~
            * #8  ! TXTFILE  ! Sales Master Text File                   *~
            * #9  ! AMTBOMIF ! Master VALIDITY FILE                     *~
            * #11 ! BCKMASTR !                                          *~
            * #12 ! BCKLINES ! S.O. Detail                              *~
            * #15 ! EWDPLNRK ! Master Glass Rack File                   *~
            * #18 ! AWDSTEMP ! Tempered Sort File                       *~
            * #19 ! AWDTEMP  ! Tempered FTP File                        *~
            * #20 ! AWDSRPT  ! Sort File for Labels and Report          *~
            * #21 ! AWDPLNGR ! Glass Sched/Remake Tempered File         *~
            * #22 ! AWDTEMP1 ! Mod for Cardinal Glass          (AWD001) *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "EWDSCRMK",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,   keylen = 50,                       ~
                        alt key 1, keypos =  16, keylen = 35

            select #2,  "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


            select #5,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos = 1,    keylen = 51,                      ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup

            select #6,  "APCPLNSC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 24,   keylen = 10,                      ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33

            select #7,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos = 1,    keylen =  9,                      ~
                        alt key 1, keypos = 10, keylen = 30, dup,        ~
                            key 2, keypos =424, keylen =  9, dup,        ~
                            key 3, keypos =771, keylen =  9, dup,        ~
                            key 4, keypos =780, keylen =  9, dup,        ~
                            key 5, keypos = 1049, keylen = 9, dup

            select #8,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

            select #9, "AMTBOMIF",                                       ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32

            select #11, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #12, "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #15, "EWDPLNRK",                                      ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  9,                      ~
                        alt key  1, keypos = 10, keylen =  14

            select #18, "AWDPLNWK",                                       ~
/*AWD001*/              varc,     indexed, recsize = 256,                 ~
                        keypos = 1,    keylen = 93

            select #19, "AWDRMK  ",                                        ~
                        varc,     indexed, recsize = 128,                 ~
                        keypos = 1,    keylen = 90

            select #20, "AWDPLNW1",                                       ~
                        varc,     indexed, recsize = 240,                 ~
                        keypos = 1,    keylen = 81

            select #21,  "AWDPLNGR",                                     ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 22,   keylen =  12,                     ~
                        alt key  1, keypos  =     7, keylen = 27,        ~
                            key  2, keypos  =     1, keylen = 33,        ~
                            key  3, keypos  =    13, keylen = 21

/* (AWD001) */
            select #22, "AWDRMK1 ",                                       ~
                        consec, recsize = 128

/* (AWD001) */

            select #63, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup




	    skip_it$ = "0"
             call "SHOSTAT" ("Opening Files, One Moment Please")



            filename$ = "AMTBOMCD" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNOR" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSC" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CUSTOMER" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "TXTFILE"  : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AMTBOMIF" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKMASTR" : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#12, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDPLNRK" : call "EWDOPEN" (#15, filename$, err%)
            if err% <> 0% then gosub open_error


            filename$ = "AWDPLNGR" : call "EWDOPEN" (#21, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 100%, rslt$(1%))


            mat f1% = zer

            wrk%, wrk1% = 0%
        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            ret% = 0%
            call "COMPNAME" (12%, company$, ret%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            init(" ") txt$(), tab$(), ff$, volume$, library$
            tab$(1%) = "PLAN SCH1"
            tab$(2%) = "PLAN SCH2"
            tab$(3%) = "COLOR    "
            tab$(4%) = "GLASS    "
            tab$(5%) = "LITING   "
            tab$(6%) = "PLAN CUTO"
            tab$(7%) = "PLAN SAND"
            tab$(8%) = "TEMP GED "
            tab$(9%) = "PLANCSAND"                            /* (AWD001) */

            ff$      = "MMDD@DPT"
            volume$  = "CARLOS"
            library$ = "APCDATA "

            txt$(1%) = "  Placing Specials On-Order   "
            txt$(2%) = "  Receive Specials On-Order   "
            txt$(3%) = "Releasing Specials to Planning"
            txt$(4%) = "Deleting Selected Special Ords"

            tp_comma$ = ","

* (AWD002) Next 3 lines
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #4, schema_err%)
            
            
/* (AWD007) */
            gosub load_interdesc
            init(" ") spTypeDesc$()
REM                                   10        20        30
REM                           123456789012345678901234567890
            spTypeDesc$(1) = "single strength"
            spTypeDesc$(2) = "double strength"
            spTypeDesc$(3) = "tempered double strength"
            spTypeDesc$(4) = "SDL Single strength"
            spTypeDesc$(5) = "SDL Double strength"
            spTypeDesc$(6) = "SDL Temp Double strength"
            spTypeDesc$(7) = "Triple Strength>25SqFt 3/16"
            spTypeDesc$(8) = "Door 5/32 Triple Strength"
            spTypeDesc$(998) = "NO SANDWICH IN PLANCSAND "
            spTypeDesc$(999) = "NOT TEMPERED"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   7%
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
                  if keyhit%  =  9% then gosub process_data
                  if keyhit%  = 10% then gosub process_data
                  if keyhit%  = 14% then gosub process_data
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
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
            *             P R O C E S S   D A T A                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************
                                                 /* Analyize (EWDSCHED)*/
        process_data                             /* Production Data    */
            report% = 1%                         /* No Screeen Display */
            init(" ") sp_key$, dt$()
            cnt% = 0% : dt% = 0% : dt_max% = 0%
            str(sp_key$,1%,6%) = beg_dte$
            str(sp_key$,7%,1%) = sc_status$
            str(sp_key$,8%,1%) = sc_type$

            call "SHOSTAT" ("Searching (EWDSCHED) Data")
        process_data_ewd
            read #1,key > sp_key$, using L19000, sp_key$,                ~
                                               eod goto process_data_done
L19000:        FMT CH(50)
            if str(sp_key$,1%,6%) > end_dte$ then goto process_data_done
            if str(sp_key$,7%,1%) <> sc_status$ then goto process_data_ewd
            if str(sp_key$,8%,1%) <> sc_type$ then goto process_data_ewd

            if str(beg_cut$,1%,2%) = "AL" then goto no_cut_off
               gosub check_cutoff
               if cutoff% = 0% then goto process_data_ewd
            no_cut_off
               gosub load_screen_dt
               goto process_data_ewd

        process_data_done
            dt_max% = dt%
            if keyhit% =  9% then gosub gen_labels
            if keyhit% = 10% then gosub display_analysis
            if keyhit% = 14% then gosub gen_rpt1
        return clear all
        goto inputmode

        load_screen_dt
            gosub dataload
            sav_ord$ = sp_so$
            gosub check_apcplnor

REM         if or_st$ > "01" and or_st$ < "90" then sp_cutoff$ = "**"
            if or_st$ = "99" then sp_cutoff$ = "HH"  /* Credit Hold    */
            if or_hows$ = "20" then sp_cutoff$ = "RR" /* Back Order Rep*/

            if or_hows$ = "26" then sp_cutoff$ = "BO" /* Back Order Rep*/
            if or_hows$ = "22" then sp_cutoff$ = "SR" /* Sample Repair */


            if or_hows$ = "23" then sp_cutoff$ = "FX"
            if str(sp_cust$,1%,6%) = "SA0999" then sp_cutoff$ = "SA"


            dt% = dt% + 1%
            if dt% > 300% then dt% = 300%
            str(dt$(dt%),1%,8%)  = sp_status_date$   /* Analysis Date  */
            str(dt$(dt%),10%,2%) = sp_cutoff$        /* cutoff Day     */
            str(dt$(dt%),13%,6%) = sp_cust$          /* Customer Code  */
            str(dt$(dt%),20%,8%) = sp_so$            /* cust Sales Ord */
            str(dt$(dt%),29%,2%) = sp_ln$            /* Line Item      */
            str(dt$(dt%),32%,9%) = sp_warr$          /* Glass Barcode  */
            str(dt$(dt%),42%,3%) = sp_num$           /* Glass Remake No*/
            str(dt$(dt%),46%,25%)= sp_part$          /* part No        */
            str(dt$(dt%),72%,4%) = sp_qty$           /* Quantity       */
            str(dt$(dt%),76%,4%) = sp_ln_item$       /* Line Item      */
        return


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
         "Enter a Valid Analysis Process Code from (PLAN SCH1)?        ",~
         "Enter a Valid Analysis Type of Process Code from (PLAN SCH2)?",~
         "Enter a Update Code '0'= Selected, '1'= All Except Selected? ",~
         "Enter a Valid Beginning Date?                                ",~
         "Enter a Valid Ending Date?                                   ",~
         "Enter a Valid Beginning Customer Cut-Off?                    ",~
         "Enter a Valid Ending Customer Cut-Off?                       "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, beg_date$, beg_dte$,       ~
                      end_date$, end_dte$, x$, sc_status$, sc_status_d$, ~
                      sc_type$, sc_type_d$, sc_process$, sc_process_d$,  ~
                      cc$(), dt$(), beg_cut$, end_cut$, beg_cut_d$, end_cut_d$

            rpt% = 0% : sp_flag% = 0%

            init (hex(ff)) textid$
            call "TXTFUTIL" (#8, f2%(8%), "INTL", textid$)
            sd1$="Beg Date-Special Orders"
            sd2$="End Date-Special Orders"
            report% = 0%
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
            init(" ") sp_status_date$, sp_due_date$, sp_date$

            get #1, using L35000, sp_status_dte$,  /* Spec Anal St     */~
                                  sp_status$,      /* (PLAN SCH1)      */~
                                  sp_type$,        /* (PLAN SCH2)      */~
                                  sp_cutoff$,      /* Cut Off Day 1-7  */~
                                  sp_route$,       /* Route code       */~
                                  sp_cust$,        /* Customer Code    */~
                                  sp_so$,          /* Sales order No.  */~
                                  sp_ln$,          /* S.O. Line item No*/~
                                  sp_ln_item$,     /* Line Item Piece  */~
                                  sp_warr$,        /* Barcode          */~
                                  sp_num$,         /* Rmk Num          */~
                                  sp_due$,         /* Sales order Due  */~
                                  sp_part$,        /* Part Number      */~
                                  sp_qty$,         /* line Item Qty    */~
                                  sp_stat$,        /* (PLAN STAT) Plann*/~
                                  sp_usr$,         /* Last Mod User Id */~
                                  sp_dte$,         /* Last Mod Date    */~
                                  sp_time$,        /* Last Mod Time    */~
                                  sp_text$,        /* Line Item Text Id*/~
                                  sp_primary$,     /* Primary Dep      */~
                                  sp_fil$          /* Filler Area      */


            if str(sp_primary$,1%,3%) = "   " then sp_primary$ = "099"
            sp_status_date$ = sp_status_dte$
            call "DATEFMT" (sp_status_date$)

            sp_due_date$ = sp_due$
            call "DATEFMT" (sp_due_date$)

            sp_date$ = sp_dte$
            call "DATEFMT" (sp_date$)

        return

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

L35000:     FMT                          /* (EWDCHED) File             */~
                CH(6),                   /* sp_status_dte$ Spec Anal St*/~
                CH(1),                   /* sp_status$ (PLAN SCH1)     */~
                CH(1),                   /* sp_type$ (PLAN SCH2)       */~
                CH(2),                   /* sp_cutoff$ Cut Off Day 1-7 */~
                CH(5),                   /* sp_route$ Route code       */~
                CH(9),                   /* sp_cust$ Customer Code     */~
                CH(8),                   /* sp_so$ Sales order number  */~
                CH(2),                   /* sp_ln$ S.O. Line item No.  */~
                CH(4),                   /* sp_ln_item$ Line Item Piece*/~
                CH(9),                   /* Glass Barcode              */~
                CH(3),                   /* Glass Remake Num           */~
                CH(6),                   /* sp_due$ Sales order Due Dat*/~
                CH(25),                  /* sp_part$ Part Number       */~
                CH(4),                   /* sp_qty$ line Item Quantity */~
                CH(2),                   /* sp_stat$ (PLAN STAT) Plann */~
                CH(3),                   /* sp_usr$ Last Mod User Id   */~
                CH(6),                   /* sp_dte$ Last Mod Date      */~
                CH(8),                   /* sp_time$ Last Mod Time     */~
                CH(4),                   /* sp_text$ Line Item Text Id */~
                CH(3),                   /* sp_primary$ Primary Dept   */~
                CH(17)                   /* sp_fil$ Filler Area        */

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
              on fieldnr% gosub L40160,          /* beg_date$          */~
                                L40160,          /* end_date$          */~
                                L40160,          /* Analysis Process   */~
                                L40160,          /* Process Type Code  */~
                                L40160,          /* Process Selection  */~
                                L40160,          /* Beg Cut-Off EWD013 */~
                                L40160           /* End Cut-Off EWD013 */

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Special Selection Code :",                   ~
               at (03,27), fac(lfac$(1%)), sc_status$           , ch(01),~
               at (03,40), fac(hex(84)),   sc_status_d$         , ch(30),~
                                                                         ~
               at (04,02), "Special Type Code      :",                   ~
               at (04,27), fac(lfac$(2%)), sc_type$             , ch(01),~
               at (04,40), fac(hex(84)),   sc_type_d$           , ch(30),~
                                                                         ~
               at (05,02), "Special Process Code   :",                   ~
               at (05,27), fac(lfac$(3%)), sc_process$          , ch(01),~
               at (05,40), fac(hex(84)),   sc_process_d$        , ch(35),~
                                                                         ~
               at (06,02), fac(hex(84)),   sd1$                 , ch(24),~
               at (06,27), fac(lfac$(4%)), beg_date$            , ch(10),~
                                                                         ~
               at (07,02), fac(hex(84)),   sd2$                 , ch(24),~
               at (07,27), fac(lfac$(5%)), end_date$            , ch(10),~
                                                                         ~
               at (08,02), "Customer Cut-Off Code A:",                   ~
               at (08,27), fac(lfac$(6%)), beg_cut$             , ch(02),~
               at (08,40), fac(hex(84)),   beg_cut_d$           , ch(35),~
                                                                         ~
               at (09,02), "Customer Cut-Off Code B:",                   ~
               at (09,27), fac(lfac$(7%)), end_cut$             , ch(02),~
               at (09,40), fac(hex(84)),   end_cut_d$           , ch(35),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1) = "(1)Start Over     (4)Previous Field     " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(2),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1),19,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over     (4)Previous Field     " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                  (9)Print Labels       " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (10)Select Data        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffff0affffff0e0f1000)
            return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return


        REM *************************************************************~
            *           D I S P L A Y   S U M M A R Y   S C R E E N     *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_analysis
            k% = 0%
L41000:     gosub set_pf2
            accept                                                       ~
               at (01,02), fac(hex(84)), sel$(1%)               , ch(15),~
               at (01,64), fac(hex(84)), pageno$                , ch(14),~
               at (02,02), fac(hex(84)), sel$(2%)               , ch(15),~
               at (02,64), fac(hex(84)), sel$(4%)               , ch(16),~
               at (03,02), fac(hex(84)), sel$(3%)               , ch(15),~
               at (03,64), fac(hex(84)), sel$(5%)               , ch(16),~
                                                                         ~
               at (02,18), fac(hex(a4)), title$                 , ch(45),~
                                                                         ~
               at (05,04), fac(hex(a4))  , h1$                  , ch(08),~
               at (05,13), fac(hex(a4))  , h2$                  , ch(02),~
               at (05,16), fac(hex(a4))  , h3$                  , ch(05),~
               at (05,23), fac(hex(a4))  , h4$                  , ch(08),~
               at (05,32), fac(hex(a4))  , h5$                  , ch(02),~
               at (05,35), fac(hex(a4))  , h6$                  , ch(09),~
               at (05,45), fac(hex(a4))  , h7$                  , ch(03),~
               at (05,49), fac(hex(a4))  , h8$                  , ch(25),~
               at (05,75), fac(hex(a4))  , h9$                  , ch(04),~
                                                                         ~
               at (06,02), fac(hex(81))  , cc$(k% + 1%)         , ch(01),~
               at (06,04), fac(hex(84))  , dt$(k% + 1%)         , ch(75),~
                                                                         ~
               at (07,02), fac(hex(81))  , cc$(k% + 2%)         , ch(01),~
               at (07,04), fac(hex(84))  , dt$(k% + 2%)         , ch(75),~
                                                                         ~
               at (08,02), fac(hex(81))  , cc$(k% + 3%)         , ch(01),~
               at (08,04), fac(hex(84))  , dt$(k% + 3%)         , ch(75),~
                                                                         ~
               at (09,02), fac(hex(81))  , cc$(k% + 4%)         , ch(01),~
               at (09,04), fac(hex(84))  , dt$(k% + 4%)         , ch(75),~
                                                                         ~
               at (10,02), fac(hex(81))  , cc$(k% + 5%)         , ch(01),~
               at (10,04), fac(hex(84))  , dt$(k% + 5%)         , ch(75),~
                                                                         ~
               at (11,02), fac(hex(81))  , cc$(k% + 6%)         , ch(01),~
               at (11,04), fac(hex(84))  , dt$(k% + 6%)         , ch(75),~
                                                                         ~
               at (12,02), fac(hex(81))  , cc$(k% + 7%)         , ch(01),~
               at (12,04), fac(hex(84))  , dt$(k% + 7%)         , ch(75),~
                                                                         ~
               at (13,02), fac(hex(81))  , cc$(k% + 8%)         , ch(01),~
               at (13,04), fac(hex(84))  , dt$(k% + 8%)         , ch(75),~
                                                                         ~
               at (14,02), fac(hex(81))  , cc$(k% + 9%)         , ch(01),~
               at (14,04), fac(hex(84))  , dt$(k% + 9%)         , ch(75),~
                                                                         ~
               at (15,02), fac(hex(81))  , cc$(k% + 10%)        , ch(01),~
               at (15,04), fac(hex(84))  , dt$(k% + 10%)        , ch(75),~
                                                                         ~
               at (16,02), fac(hex(81))  , cc$(k% + 11%)        , ch(01),~
               at (16,04), fac(hex(84))  , dt$(k% + 11%)        , ch(75),~
                                                                         ~
               at (17,02), fac(hex(81))  , cc$(k% + 12%)        , ch(01),~
               at (17,04), fac(hex(84))  , dt$(k% + 12%)        , ch(75),~
                                                                         ~
               at (18,02), fac(hex(81))  , cc$(k% + 13%)        , ch(01),~
               at (18,04), fac(hex(84))  , dt$(k% + 13%)        , ch(75),~
                                                                         ~
               at (19,02), fac(hex(81))  , cc$(k% + 14%)        , ch(01),~
               at (19,04), fac(hex(84))  , dt$(k% + 14%)        , ch(75),~
                                                                         ~
               at (21,02), fac(hex(a4)),   dsp_msg$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L41040             /* First    */
L41020:           k% = 0%
                  goto L41000

L41040:        if keyhit% <> 3% then goto L41080             /* Last      */
L41060:           x% = int(val_max% / 14%)
                  k% = (x%*14%)
                  if (k% + 1%) > val_max% then k% = k% - 14%
                  goto L41000

L41080:        if keyhit% <> 4% then goto L41100             /* Previous */
                  if k% < 15% then goto L41020
                  k% = k% - 14%
                  if k% <= 1% then goto L41020
                  goto L41000

L41100:        if keyhit% <> 5% then goto L41120             /* Next     */
                  k% = k% + 14%
                  if k% < val_max% then goto L41000
                  goto L41060

L41120:        if keyhit% <> 7% then goto L41110
                  report% = 0%
                  gosub build_file
                  goto L41000

L41110:        if keyhit% <> 9% then goto L41115
                  report% = 0%                 /* Based on Selection  */
                  gosub gen_labels

L41115:        if keyhit% <> 0% then goto L41130
                  gosub display_detail
                  goto L41000

L41130:        if keyhit% <> 11% then goto L41135
                  gosub update_specials

L41135:        if keyhit% <> 12% then goto L41145
                  call "EWDPLB58" (switch%)
                  if switch% = 0% then goto L41000
                     sp_flag% = 4%
                     gosub delete_specials


L41145:        if keyhit% <> 14% then goto L41150
                  report% = 0%                 /* Based on Selection */
                  gosub gen_rpt1

L41150:        if keyhit% <> 15 then goto L41155
                  call "PRNTSCRN"
                  goto L41000

L41155:        if keyhit% <> 16% then goto L41000

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return clear all
        goto inputmode

        set_pf2
            init(" ") h1$, h2$, h3$, h4$, h5$, h6$, h7$, h8$,x$
            dsp_msg$=                                                     ~
             "Use 'X' to Display Detail Information, followed by <Return>?"
            str(dsp_msg$,62%,15%) = "Total [ xxxxx ]"
            convert dt_max% to str(dsp_msg$,70%,5%), pic(#####)

            sel$(1%) = "Beg: xxxxxxxxxx"
            sel$(2%) = "End: xxxxxxxxxx"
            sel$(3%) = "Typ: xxxxxxxxx "
            sel$(4%) = "Date: xxxxxxxxxx"
            sel$(5%) = "Time: xxxxxxxx  "
            str(sel$(1%),6%,10%) = beg_date$
            str(sel$(2%),6%,10%) = end_date$
            p% = pos(sc_type_d$ = "-")
            str(sel$(3%),6%,9%) = str(sc_type_d$,p% + 1%,9%)
            x$ = date
            call "DATFMTC" (x$)
            str(sel$(4%),7%,10%) = x$
            init(" ") x$
            call "TIME" (x$)
            str(sel$(5%),7%,8%) = str(x$,1%,8%)
            p% = pos(sc_status_d$ = "-")
            title$ = "Display of Items that are " &                      ~
                      str(sc_status_d$,p%+1%,18%)
            pageno$ = "Page: XX of XX"             /* k% = Array Values */
            h1$ = "Proc Dte"
            h2$ = "CO"
            h3$ = "Cust. "
            h4$ = "< S.O. >"
            h5$ = "Ln"
            h6$ = "Warranty"
            h7$ = "Num"
            h8$ = "<----- Part Number ----->"
            h9$ = " Qty"

            val_max% = dt_max%
            if val_max% > (300% - 14%) then val_max% = 300% - 14%
                                                        /* Display Max */
            x = val_max%
            yy% = ( val_max% / 14% )
            if mod(x,14) <> 0 then yy% = yy% + 1%

            xx% = (k% / 14%) +1%
            if xx% > yy% then xx% = yy%

            convert xx% to str(pageno$,7%,2%), pic(##) /* Current Page*/

            convert yy% to str(pageno$,13%,2%), pic(##)/* Total Pages */

               pf_txt$ = "(11)Place On Order  "
            if sc_status$ = "2" then                                     ~
               pf_txt$ = "(11)Recived Specials"
            if sc_status$ = "4" then                                     ~
               pf_txt$ = "(11)Release Specials"
            if sc_status$ = "Z" then pf_txt$ = "                    "

            pf$(1) = "(2)First           (5)Next              " &        ~
                               pf_txt$
            str(pf$(1%),64%,16%) = "(14)Print Report"
            pf$(2) = "(3)Last            (7)Build Temp File   " &        ~
                     "(12)Delete(Sel Only)   (15)Print Screen"
            pf$(3) = "(4)Previous        (9)Print Labels      " &        ~
                     "                       (16)Exit Display"

            pfkeys$ = hex(ff02030405ff07ff09ff0b0c0dff0f1000)
            if sc_status$ = "Z" then str(pfkeys$,11%,1%) = hex(ff)
            if sc_status$ = "0" then goto L41850
               str(pf$(2%),40%,21%) = " " : str(pfkeys$,12%,1%)=hex(ff)
L41850:     gosub check_screen
            return

        check_screen
            if val_max% > 14% then goto L41860
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L41860:      if k% >= 14% then goto L41870
                gosub no_first
                gosub no_prev
L41870:      if (k% + 14%) <= val_max% then goto L41880
                gosub no_last
L41880:      if k% <= (val_max% - 14%) then goto L41900
                gosub no_next
L41900: return
        no_first
            str(pf$(1%),1%, 9%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(1%),20%, 9%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
            str(pf$(2%),1%,9%)   = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(3%),1%,12%)  = " " : str(pfkeys$,4%,1%) = hex(ff)
        return


        REM *************************************************************~
            *           D I S P L A Y   S U M M A R Y   S C R E E N     *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_detail
            for dt% = 1% to dt_max%
                if cc$(dt%) <> " " then goto L42000
            next dt%
            goto L42010                          /* No Selection Found */

L42000:     sp_so$ = str(dt$(dt%),20%,8%)        /* Sales order        */
            sp_ln$ = str(dt$(dt%),29%,2%)        /* Line Item          */
            rk_barcode$ = "         "
            rk_seq$     = "     "
       call "EWDPLA58" (1%,              /* o%=Info Only, 1%=Info+Glass*/~
                        sp_so$,          /* Sales Order Number         */~
                        sp_ln$,          /* Sales order Line Item      */~
                        rk_barcode$,     /* Glass/Rack Barcode         */~
                        rk_seq$,         /* Production Seq. No         */~
                        " ",                                             ~
                        " ",                                             ~
                        " ",                                             ~
                        " ",                                             ~
                        " ",                                             ~
                        " ",                                             ~
                        " ",             /* (AWD004)                   */~
                        #5,              /* (APCPLNOR) Planning Header */~
                        #6,              /* (APCPLNSC) Planning Ln Item*/~
                        #7,              /* (CUSTOMER) Customer Master */~
                        #4,              /* (GENCODES) Master Tables   */~
                        #9,              /* (AMTBOMIF) Master Validity */~
                        #8,              /* (TXTFILE ) Sales Text File */~
                        #2,              /* (AMTBOMCD) Master Equation */~
                        #12,             /* (BCKLINES) S.O. Dtl(EWD003)*/~
                        #15,             /* (EWDPLNRK) - (EWD005)      */~
                        er% )            /* 0% = Ok, Non-Zero = Error  */
L42010: init(" ") cc$()
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            init(" ") errormsg$, code$
            on fieldnr% gosub L50000,                 /* sc_status$   */  ~
                              L50100,                 /* sc_type$     */  ~
                              L50200,                 /* sc_process$  */  ~
                              L50300,                 /* beg_date$    */  ~
                              L50400,                 /* end-date$    */  ~
                              L50500,                 /* beg cut-off  */  ~
                              L50600                  /* end cut-off  */
            return

L50000: Rem Special Selection Code                    sc_status$
            if sc_status$ = " " then sc_status$ = "0"
            tab% = 1%                            /* (PLAN SCH1)      */
            code$ = sc_status$
            gosub check_code
            if code% = 0% then goto L50010
               sc_status_d$ = desc$

            sd1$="Beg Date-Special Orders"
            sd2$="End Date-Special Orders"
            p% = pos(sc_status_d$ = "-")
            str(sd1$,10%,14%) = str(sc_status_d$,p%+1%,14%)
            str(sd2$,10%,14%) = str(sc_status_d$,p%+1%,14%)

        return
L50010:     init(" ") sc_status$, sc_status_d$
            errormsg$ = "(Error) Invalid Special Selection Code?"
            gosub error_prompt
            sd1$="Beg Date-Special Orders"
            sd2$="End Date-Special Orders"
        return

L50100: Rem Special Type Code                          sc_type$
            if sc_type$ = " " then sc_type$ = "5"
            tab% = 2%                            /* (PLAN SCH2)      */
            code$ = sc_type$
            gosub check_code
            if code% = 0% then goto L50110
               sc_type_d$ = desc$
        return
L50110:     init(" ") sc_type$, sc_type_d$
            errormsg$ = "(Error) Special Type Code?"
            gosub error_prompt
        return

L50200: Rem Special Selection Process Code             sc_process$
            if sc_process$ = " " then sc_process$ = "0"
            if sc_process$ = "0" then                                  ~
               sc_process_d$ = "Only Process 'X's Selected?"
            if sc_process$ = "1" then                                  ~
               sc_process_d$ = "Only Process 'Blank' Selections?"
            p% = pos("01" = sc_process$)
            if p% = 0% then goto L50210
        return
L50210:     init(" ") sc_process$, sc_process_d$
            errormsg$ = "(Error) Invalid Process Selection Code?"
            gosub error_prompt
        return

L50300: REM Beginning search Date                    BEG_DTE$, BEG_DATE$
            if beg_date$ <> " " then goto L50310
               beg_date$ = date

L50310:     date% = 0%                       /* Formatted with Cent   */
            call "DATEOKC" (beg_date$, date%, errormsg$)

            if date% = 0% then goto L50320
            x$ = beg_date$
            call "DATUFMTC"(x$)              /* Unformatted with Cent */
            beg_dte$ = str(x$,1%,6%)
        return
L50320:     init(" ") beg_dte$, beg_date$, x$
            errormsg$ = "(Error) Invalid Begining Re-make Date?"
            gosub error_prompt
        return

L50400: REM Ending search Make Date              END_DTE$, END_DATE$
            if end_date$ <> " " then goto L50410
               end_date$ = beg_date$
L50410:     date% = 0%                       /* Formatted with Cent    */
            call "DATEOKC" (end_date$, date%, errormsg$)
            if date% = 0% then goto L50420
            x$ = end_date$
            call "DATUFMTC"(x$)              /* Unformatted with Cent  */
            end_dte$ = str(x$,1%,6%)
            if end_dte$ < beg_dte$ then goto L50420
        return
L50420:     init(" ") end_dte$, end_date$, x$
            errormsg$ = "(Error) Invalid Ending Re-make Date?"
            gosub error_prompt
        return

L50500: REM Beginning Customer Cut-Off Code A    BEG_CUT$, BEG_CUT_D$
            if beg_cut$ <> " " then goto L50510
               beg_cut$ = "AL"
            return
L50510:     if str(beg_cut$,1%,2%) = "AL" then return
            init(" ") beg_cut_d$, code$
            tab% = 6%
            str(code$,1%,2%) = beg_cut$
            gosub check_code
            if code% <> 1% then goto L50550
            str(beg_cut_d$,1%,35%) = desc$

        return
L50550:     init(" ") beg_cut$, beg_cut_d$
            errormsg$ = "(Error) Invalid Customer Cut-Off Code A?"
            gosub error_prompt
        return

L50600: REM Ending Customer Cut-Off Code B       END_CUT$, END_CUT_D$
            if end_cut$ <> " " then goto L50610
               end_cut$ = "AL"
            return
L50610:     if str(beg_cut$,1%,2%) = "AL" then return
            init(" ") end_cut_d$, code$
            tab% = 6%
            str(code$,1%,2%) = end_cut$
            gosub check_code
            if code% <> 1% then goto L50650
            str(end_cut_d$,1%,35%) = desc$

        return
L50650:     init(" ") end_cut$, end_cut_d$
            errormsg$ = "(Error) Invalid Customer Cut-Off Code B?"
            gosub error_prompt
        return


        check_code
            code% = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = tab$(tab%)
            str(readkey$,10%,15%) = code$
            read #4,key = readkey$, using L54000, desc$,                ~
                                                eod goto check_code_done
L54000:        FMT POS(25), CH(30)
            code% = 1%
        check_code_done
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55000: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

L55010: %!######## @ ########                      ######################~
        ~##################                              Page:       ### !

L55020: %! Begin Date: ##########                  ######################~
        ~##################                       End Date : ########### !

L55030: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

L55040: %!Customer !SalesOrd!Ln!Due Date!<----- Part Number ----->!<---- ~
        ~Description ------------->! Qty !*! T/B ! Cut Width ! Cut Height!

L55050: %!---------!--------!--!--------!-------------------------!------~
        ~--------------------------!-----!-!-----!-----------!-----------!

L55060: %!#########!########!##!########!#########################!######~
        ~##########################!#### !#! ### ! ######### ! ########  !


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        calc_glass_size
            opts% = 0%
            dept$ = "000"
            g_cnt% = 0%
            if str(sp_part$,1%,1%) = "8" then dept$ = "008"
            call "EWDGLSSB" (opts%,      /* Options 0% = No Calc       */~
                             sp_part$,   /* MFG Part Number            */~
                             subpart$,   /* (AWD006) Subpart           */~
                             dept$,      /* Department Code            */~
                             ct%,        /* Glass Piece Count          */~
                             ctt$(),     /* 1-5 = Top, 6-10 = Bot      */~
                             gdd$(),     /* 1-6 = Top, 7-12 = Bot      */~
                             ajj$(),     /* Window Adjustment (GED) Top*/~
                             dcc$(),     /* Decimal 1-5=Top, 6-10=Bot  */~
                             wd$,        /* Window width Eights        */~
                             ht$,        /* window Height Eights       */~
                             g_cnt%,     /* Glass Piece Cut Count      */~
                             spType%,    /* Spacer Type (AWD006)       */~
                             #4,         /* (GENCODES) Master Tables   */~
                             #2,         /* (AMTBOMCD) Equations       */~
                             er% )       /* 0%=Ok, Non-Zero = Error    */

/*(AWD006) */
            init(" ") strength$
            if spType% = 1% or spType% = 4% then strength$ = "3"
            if spType% = 2% or spType% = 3% then strength$ = "4"
            if spType% = 5% or spType% = 6% then strength$ = "4"
            if spType% = 7% then strength$ = "6"
            if spType% = 8% then strength$ = "5"                             
        return

        deffn'099(textid$)
            txt% = 0%
            if textid$ = hex(00000000) or textid$ = hex(ffffffff)        ~
                                           or textid$ = " " then return
            txt% = 1%
        return


        open_error
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        print_header
            init(" ") rpt_time$ : call "TIME" (rpt_time$)
            pageno% = pageno% + 1%
            if lcnt% <> 99% then print using L55000
            print page
            print using L55000
            print using L55010, date$, rpt_time$, company$, pageno%
            print using L55020, beg_date$, title$, end_date$
            print using L55030
            print using L55040
            lcnt% = 5%

        return


        print_dtl_temp
            gosub build_dtl_rpt
        return
        print_dtl_temp_sort
            sc_type$ = " "

            init(" ") sp_temp_key$
            read #20, hold, key > sp_temp_key$, eod goto print_sort_done

                       goto print_sort_first
        print_sort_nxt
            read #20, hold, eod goto print_sort_done

print_sort_first
/* (AWD006) */
             get #20, using L60210, intercept$, spType$, sp_rack$, gdd$(view%),~
                                 gdd$(spac%), str(sp_part$,1%,3%), wd$,     ~
                                 ht$, str(sp_part$,5%,2%),                  ~
                                 str(sp_part$,4%,1%), t_k$, muttin$,        ~
                                 sp_warr$, sp_so$, sp_ln$, sp_temp$,        ~
                                 sp_temp1$, t5$, sandwich$(k%),             ~
                                 sp_lits$, view$, sp_due_date$, sp_part$,   ~
                                 sp_part_d$, sp_qty$, chg$, sp_cust$, wd1$, ht1$


                      delete #20

            if rpt% = 1% then goto print_temp_labels
               if lcnt% > 55% then gosub print_header
               prt_flag% = 0%
               print using L55050
               lcnt% = lcnt% + 1%


                if prt_flag% <> 0% then goto L60155
                   print using L55060, sp_cust$,sp_so$,sp_ln$,sp_due_date$,~
                                    sp_part$,sp_part_d$, sp_qty$, chg$,    ~
                                    view$, wd1$, ht1$
                prt_flag% = 1%
                goto L60220
L60155:     print using L55060, " ", " ", " ", " ", " ", " ", " ", " ",   ~
                               view$, wd1$, ht1$
L60220:         lcnt% = lcnt% + 1%
                     goto print_sort_nxt
        print_temp_labels
         if rpt% = 1% then gosub print_lab_temp
                     goto print_sort_nxt
        print_sort_done
        return

        lookup_description
            init(" ") sp_part_d$
            call "APCDESCR" (sp_part$, apc_scr$, apc_prt$, apc_sze$,   ~
                                                           #9, x_er% )

            s_23% = 0%                        /* Find Private Label  */
            s_23m$ = str(sp_part$,1%,3%)
            s_so$  = sp_so$                   /*                     */
            s_ln$  = sp_ln$                   /*                     */
            init(" ") s_prv$, s_1$, s_23$
            prv% = 1%
            call "APCPRZSB" (prv%, s_1$, sp_cust$, s_23m$, s_so$, s_ln$, ~
                           s_prv$, s_23$, s_23%, #7, #4, #12, #12, x_er% )
              if x_er% <> 0% then L60180
              if len(sp_part$) < 18% then L60180
                  str(apc_prt$,1%,8%)   = s_23$
                                                /* MFG Description     */
L60180:     sp_part_d$ = str(apc_prt$,1%,32%)
        return

        gen_rpt1
            mode% = 1% : gosub open_work1
            mode% = 3% : gosub open_work1
            cnt% = 0%
            call "SHOSTAT" ("Special Tempered Glass Report")
            gosub select_printer

            for dt% = 1% to dt_max%
                cnt% = cnt% + 1%
                if report% = 1% then goto L60185  /* Print All           */

                if sc_process$ = "0" and cc$(dt%) <> "X" then goto L60190
                                                  /* Only Print 'x's     */
                if sc_process$ = "1" and cc$(dt%) = "X" then goto L60190
                                                  /* Only Print 'Blanks' */
L60185:            init(" ") sp_key$
                   str(sp_key$,1%,9%)  = str(dt$(dt%),13%,6%)
                   str(sp_key$,10%,8%) = str(dt$(dt%),20%,8%)
                   str(sp_key$,18%,2%) = str(dt$(dt%),29%,2%)
                   str(sp_key$,20%,4%) = str(dt$(dt%),76%,4%)
                   str(sp_key$,24%,9%) = str(dt$(dt%),32%,9%)
                   read #1,key 1% > sp_key$, eod goto L60190
                      gosub dataload
                      gosub calc_glass_size
                      gosub lookup_description
                      gosub print_dtl_temp
L60190:     next dt%

           gosub print_dtl_temp_sort
           print using L55000
           gosub close_printer
        return clear all
        goto inputmode

        select_printer
            init(" ") title$
            p% = pos(sc_type_d$ = "-")
            title$ = str(sc_type_d$,p% + 1%,9%)& " Special Glass Report"
            pageno% = 0%
            lcnt%    = 99%
            call "FMTTITLE" (title$, " ", 12%)
            date$ = date  :  call "DATEFMT" (date$)
REM            company$ = "Ellison Window and Door"
            call "FMTTITLE" (company$, " ", 12%)
            call "SETPRNT" ("EWDGLS", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("EWDGLS", " ", 0%, 1%)
        return

        gen_labels
            mode% = 1% : gosub open_work1
            mode% = 3% : gosub open_work1
            rpt% = 1%
            cnt% = 0%
            call "SHOSTAT" ("Special Tempered Glass Labels")

            for dt% = 1% to dt_max%
                cnt% = cnt% + 1%
                if report% = 1% then goto L60200  /* Print All           */

                if sc_process$ = "0" and cc$(dt%) <> "X" then goto L61000
                                                  /* Only Print 'x's     */
                if sc_process$ = "1" and cc$(dt%) = "X" then goto L61000
                                                  /* Only Print 'Blanks' */
L60200:            init(" ") sp_key$
                   str(sp_key$,1%,9%)  = str(dt$(dt%),13%,6%)
                   str(sp_key$,10%,8%) = str(dt$(dt%),20%,8%)
                   str(sp_key$,18%,2%) = str(dt$(dt%),29%,2%)
                   str(sp_key$,20%,4%) = str(dt$(dt%),76%,4%)
                   str(sp_key$,24%,9%) = str(dt$(dt%),32%,9%)
                   read #1,key 1% > sp_key$, eod goto L61000
                      gosub dataload
                      gosub calc_glass_size
                      gosub print_dtl_temp
L61000:     next dt%
            gosub print_dtl_temp_sort
        return clear all
        goto inputmode

        print_lab_temp
           error%  = 0%
           yy_max% = 1%
           convert sp_qty$ to yy_max%, data goto L61010
L61010
           for yy% = 1% to yy_max%
               gosub build_lab_temp


               call "EWDPLC58" (t1$,               /* Model        (03)*/~
                                t2$,               /* Color Descr  (06)*/~
                                t3$,               /* Glass Type   (15)*/~
                                t4$,               /* Litin/Grid   (06)*/~
                                t5$,               /* Width & Heig (20)*/~
                                t6$,               /* Top/Bot      (03)*/~
                                t7$,               /* SO/Ln        (11)*/~
                                t8$,               /* SO Due Date  (08)*/~
                                t9$,               /* Contour Grid (01)*/~
                                t10$,              /* (AWD005) Ultra   */~
                                #4,                /* (GENCODES)       */~
                                error%)            /* Return Code      */

                if error% <> 0% then gosub L61020

           next yy%
        return
L61020:    errormsg$ = "(Error) Printing Glass Label for " & t7$
           gosub error_prompt
        return

        build_lab_temp
           t1$ = str(sp_part$,1%,3%)                /* Model Code     */
           tab% = 3%
           init(" ") code$ : code$ = str(sp_part$,4%,1%)/* Color      */
           gosub check_code
           t2$ = str(desc$,6%,6%)
           tab% = 4%
           init(" ") code$ : code$ = str(sp_part$,5%,2%)/* Glass      */
           gosub check_code
           t3$ = str(desc$,1%,15%)
           tab% = 5%
           init(" ") code$ : code$ = str(sp_part$,7%,2%)/* Liting     */
           gosub check_code
           p% = pos(desc$ = "-")
           if p% = 0% then p% = 4%
           l_lt$ = str(desc$,1%,p%-2%) & "   "   /* LEFT MUTTIN - TOP */
           r_lt$ = str(desc$,p%+2%,6%) & "   "   /* RIGHT MUTTIN - BOT*/

           gosub get_muttin
           gosub change_muttin
           t4$ = str(mut$,1%,6%)

           t5$ = wd1$ & " x " & ht1$                /* Width & Height */
           t6$ = view$                              /* View           */
           t7$ = sp_so$ & "-" & sp_ln$              /* Sales Ord/Line */
           t8$ = sp_due_date$                       /* Due Date       */

/*(PAR000) */
           init(" ") lk$
REM        lk$ = str(sp_part$,12%,1%)               /* Set Lock code  */
           init(" ") so_inv$, item_no$
           so_inv$  = sp_so$                        /* Set SO         */
           item_no$ = sp_ln$                        /* Set Line       */

           gosub lookup_sub_part                    /* Lookup Sub Part*/
           field1$ = str(flds$(12%),1%,1%)          /* Grdtype        */
           field2$ = str(flds$(13%),1%,1%)          /* GrdSize        */
           field3$ = str(flds$(14%),1%,1%)          /* GrdColor       */
           field4$ = str(flds$(15%),1%,1%)          /* Hardware       */
           field5$ = str(flds$(16%),1%,1%)          /* Foam           */

REM        p%  = pos("0123456789" = lk$)
           if field1$ = "2" then p% = 1%            /* Contour */
           t9$ = " "
           if p% = 1% then t9$ = "C"                /*Has Contour Grid*/
                                                    /*  (EWD014)      */
           cl$ = " "
REM        cl$ = str(sp_part$,4%,1%)
           p% = 0%
REM        p% = pos("GHIJ" = cl$)
           if field2$ = "2" then p% = 1%            /* 3/4 Inch Grid */
           if p% <> 0% then t9$ = "W"

/* <AWD003> */
           if field1$ = "2" and field2$ = "1" then t9$ = "E"
/* </AWD003> */

/* (AWD006) */
REM  !            gosub check_ultra
            t10$ = " "
REM  !            if ultra% = 1% then t10$ = "ultra"
/* (AWD007) */
            convert intercept$ to intercept%, data goto error4
error4:

            t10$ = interdesc$(intercept%)


        return

        edit_schedule                     /* Process Sales Order Line */
                                          /* Items (1) at a time.     */
           init(" ") sp_key$, sav_key$, sav_ord$, sp_time$
           call "TIME" (sp_time$)         /* Same time all Pieces     */
           str(sp_key$,1%,9%)  = str(dt$(dt%),13%,6%)
           str(sp_key$,10%,8%) = str(dt$(dt%),20%,8%)
           str(sp_key$,18%,2%) = str(dt$(dt%),29%,2%)
           str(sp_key$,20%,4%) = str(dt$(dt%),76%,4%)
           str(sp_key$,24%,9%) = str(dt$(dt%),32%,9%)
           sav_key$   = str(sp_key$,1%,32%)  /* Cust, S.O., Line Item */
           sav_ord$   = str(sp_key$,10%,8%)  /* Sales Order           */
           sav_line$  = str(sp_key$,10%,10%) /* Sales Order/Line Item */
           hit% = 0%

L61030:    read #1,hold,key 1% > sp_key$, using L61040, sp_rec$,       ~
                                                        eod goto L61045
L61040:       FMT CH(128)
           sp_key$ = str(sp_rec$,16%,35%)
           if sav_key$ <> str(sp_key$,1%,32%) then goto L61045
              gosub check_apcplnor          /* See if Planned       */

              sp_st$ = str(sp_rec$,7%,1%)
           if sp_flag% = 4% and sp_st$ <> "0" then goto L61030
                                            /* Can Only Delete      */
                                            /* special Sales Orders */

           if sp_flag% = 1% and sp_st$ <> "0" then goto L61030
                                            /* Can only Change      */
                                            /* Special Sales orders */

           if sp_flag% = 2% and sp_st$ <> "2" then goto L61030
                                            /* Can only Receive     */
                                            /* Placed On-Orders     */

           if sp_flag% = 3% and sp_st$ <> "4" then goto L61030
                                            /* Can only Release     */
                                            /* Received Orders      */
          REM Debug
          REM call "SHOSTAT" ("(1)Deleting --> " & sp_key$)
          REM stop
           if or_st$ = "99" and sp_flag% <> 4% then goto L61045
                                             /* Credit Hold, cannot */
                                             /* Place on Order, but */
                                             /* can be deleted      */
              delete #1
              if sp_flag% <> 4% then gosub modify_schedule
              hit% = 1%                     /* Line Item Modified   */
L61045: return

        modify_schedule                   /* Each Line Item of S.O. */
           if sp_flag% <> 1% then goto L61048
              str(sp_rec$,7%,1%)  = "2"   /* Place items On-Order   */
              goto L61055

L61048:    if sp_flag% <> 2% then goto L61050
              str(sp_rec$,7%,1%)  = "4"   /* Received Items On-Order*/
              goto L61055

L61050:    if sp_flag% <> 3% then goto L61060
              str(sp_rec$,7%,1%)  = "Z"   /*Release Orders to Planning*/
              gosub update_awdplngr

                                          /* Common to all Three (3)  */
L61055:    str(sp_rec$,1%,6%)  = date
           str(sp_rec$,88%,3%) = userid$
           str(sp_rec$,91%,6%) = date
           str(sp_rec$,97%,8%) = sp_time$
           put #1, using L61040, sp_rec$
        REM debug
        REM call "SHOSTAT" ("(2)Updating --> " & sp_key$ )
        REM stop

           write #1, eod goto L61060
        return
L61060:    errormsg$ = "(Error)-Modifying (EWDSCHED)" & sav_key$
           gosub error_prompt
        return

        delete_specials
            sp_flag% = 4%
            call "SHOSTAT" (txt$(sp_flag%))         /* Only Selected    */
            for dt% = 1% to dt_max%
                if cc$(dt%) <> "X" then goto L61065 /* Only Delete 'X's */
                   gosub edit_schedule              /* Delete all of the*/
L61065:     next dt%                                /* Line item        */
        return clear all
        goto inputmode

        update_specials           /* Process Place On-Order and Release */
            if sc_status$ = "0" then sp_flag% = 1% /* Place On-Order    */
            if sc_status$ = "2" then sp_flag% = 2% /* Received Specials */
            if sc_status$ = "4" then sp_flag% = 3% /* Release SO To Plan*/
            init(" ") gr_time$
            gr_time$ = time

            call "SHOSTAT" (txt$(sp_flag%))
            for dt% = 1% to dt_max%
                if sc_process$ = "0" and cc$(dt%) <> "X" then goto L61100
                                               /* Only Process 'X's     */
                if sc_process$ = "1" and cc$(dt%) = "X" then goto L61100
                                               /* Only process 'Blanks' */
                   gosub edit_schedule         /* Each Line Item        */
                   if hit% = 0% then goto L61100
        REM debug
        REM call "SHOSTAT" ("(3)Update Planning --> " & sav_ord$ &"  "&sav_line$)
        REM stop

L61100:     next dt%
            if sp_flag% = 3% then gosub release_specials
        return clear all
        goto inputmode

        check_apcplnor                        /* Check Planning Status */
            init(" ") or_st$, or_hows$
            read #5,key 4% = sav_ord$, using L61105, or_st$, or_hows$,   ~
                                                     eod goto L61108
L61105:        FMT POS(60), CH(2), POS(92), CH(2)

L61108: return


        release_specials
            init(" ") sav_order$
            for dt% = 1% to dt_max%
                if sc_process$ = "0" and cc$(dt%) <> "X" then goto L61300
                                               /* Only Process 'X's     */
                if sc_process$ = "1" and cc$(dt%) = "X" then goto L61300
                                               /* Only process 'Blanks' */
                   init(" ") sp_key$
                   str(sp_key$,1%,9%)  = str(dt$(dt%),13%,6%)
                   str(sp_key$,10%,8%) = str(dt$(dt%),20%,8%)
                   if str(sp_key$,1%,17%) = sav_order$ then goto L61300
                                               /* All Lines on S.O. must */
                                               /* be Released before S.O.*/
                      sav_order$ = str(sp_key$,1%,17%)
                      check% = 0%
L61200:               read #1,key 1% > sp_key$, using L61210, sp_status$, ~
                                       sp_key$, sp_stat$, eod goto L61250
L61210:                  FMT POS(7), CH(1), POS(16), CH(35), POS(86), CH(2)

                      if sav_order$ <> str(sp_key$,1%,17%) then goto L61250
                         if sp_status$ <> "Z" then check% = 1%
                         goto L61200

L61250:               if check% = 1% then goto L61310
        REM Debug
        REM call "SHOSTAT" ("(4)All Line Items Released --> " & sav_order$)
        REM stop

L61300:     next dt%
        return

L61310:     errormsg$ = "(Error) S.O. Not Released--> "& sav_order$
            gosub error_prompt
            goto L61300



        update_awdplngr
             init(" ") gr_key$
             str(gr_key$,1%,9%)  = str(sp_key$,24%,9%)
             str(gr_key$,10%,3%) = str(sp_key$,33%,3%)
             read #21, hold, key = gr_key$, using L61535, gr_rec$(),       ~
                                                      eod goto no_glass

L61535:           FMT 2*CH(192)                     /* PAR000 */

                  str(gr_rec$(),13%,1%) = "0"
                  str(gr_rec$(),14%,8%) = gr_time$
                  delete #21

                  put #21, using L61535, gr_rec$()       /* PAR000 */

                  write #21, eod goto no_glass
        return
        no_glass
            errormsg$ = "(Error) Updating (AWDPLNGR)-> " & sp_so$
            gosub error_prompt
        return

        get_muttin
            vert% = 0% : horz% = 0% : er% = 0% : err% = 0%
            if str(sp_part$,7%,2%) <> "00" then goto L61700
               muttin$ = "        " : lits$ = "0"
               return

L61700:     call "APCGSLIT" ( sp_part$,         /* MFG Part Number     */~
                              muttin$,          /* Grid Vert/Horiz Code*/~
                              lits$,            /* No. of Lits         */~
                              str(view$,1%,1%), /* T or B              */~
                              vert%,            /* Number of Verticals */~
                              horz%,            /* Number of Horizontal*/~
                              #4,               /* (GENCODES)          */~
                              er% )             /* Error Code          */
            if er% = 0% then return
               if er% = 1% then err% = 7%       /* GED Lits Error      */
               if er% = 2% then err% = 8%       /* GED Hinge Error     */
               if er% = 3% then err% = 9%       /* GED Muttin Error    */
        return

        change_muttin
            lt% = 0%
            lt$ = str(sp_part$,7%,2%)
            convert lt$ to lt%, data goto L61800
L61800:
            mut$ = " "                         /* SWITCH VERT - HORZ */
            if len(muttin$) < 5 then goto L61810
               xx% = pos(muttin$ = "x")
               cc% = pos(muttin$ = "C")
               ii% = (cc% - xx%)
            mut$ = str(muttin$,xx%+1%,ii%) & "x" & str(muttin$,1%,xx%-1%)
L61810:     if lt% > 82% then mut$ = l_lt$
        return


        check_cutoff
            cutoff%, beg_cut%, end_cut%, cutoff_code% = 0%
            init(" ") cutoff_a$, cutoff_b$, readkey1$
            str(readkey1$,1%,9%) = str(sp_key$,16%,9%)

            read #7, key = readkey1$, using L62000, cutoff_a$, cutoff_b$,~
                                                        eod goto no_cutoff
L62000:          FMT POS(860), CH(2), POS(900), CH(2)


            convert beg_cut$ to beg_cut%, data goto no_cutoff

            convert end_cut$ to end_cut%, data goto no_cutoff

            beg_cut% = beg_cut% + 5%
            end_cut% = end_cut% + 5%

            convert cutoff_a$ to cutoff_code%, data goto no_cutoff

            if beg_cut% = cutoff_code% then cutoff% = 1%

            convert cutoff_b$ to cutoff_code%, data goto no_cutoff

            if end_cut% = cutoff_code% then cutoff% = 1%


        no_cutoff
        return

        open_file
            init(" ") library$, volume$, file$
            library$        = "TEMPERED"
* (AWD002) - Next two lines
            volume$         = "CARLO2"
REM         if schema% = 1% then volume$         = "CARLO2"
REM         if schema% = 2% then volume$         = "NE2"

            if skip_it$ = "0" then goto skip_it       
            close #19
            close #22

skip_it:
            skip_it$ = "1"
            file$   = "AWDRMK  "
             open nodisplay #19, output, space = 100%,                   ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%

/* (AWD001) Cardinal File */
            init(" ") library$, volume$, file$
            library$        = "TEMPERED"
            volume$         = "CARLO2"
            file$   = "AWDRMK1 "

             open nodisplay #22, output, space = 100%,                   ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
/* (AWD001) Cardinal File */
 skip_open:

        return

        build_file
            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work
            rpt% = 1%
            cnt% = 0%
            call "SHOSTAT" ("Build EDI File For Special Tempered Glass")

            for dt% = 1% to dt_max%
                cnt% = cnt% + 1%
                if report% = 1% then goto L60300  /* Build All           */

                if sc_process$ = "0" and cc$(dt%) <> "X" then goto L60400
                                                  /* Only Build 'x's     */
                if sc_process$ = "1" and cc$(dt%) = "X" then goto L60400
                                                  /* Only Build 'Blanks' */
L60300:            init(" ") sp_key$
                   str(sp_key$,1%,9%)  = str(dt$(dt%),13%,6%)
                   str(sp_key$,10%,8%) = str(dt$(dt%),20%,8%)
                   str(sp_key$,18%,2%) = str(dt$(dt%),29%,2%)
                   str(sp_key$,20%,4%) = str(dt$(dt%),76%,4%)
                   str(sp_key$,24%,9%) = str(dt$(dt%),32%,9%)
                   str(sp_key$,33%,3%) = str(dt$(dt%),42%,3%)
                   read #1,key 1% = sp_key$, eod goto L61000
                      gosub dataload
                      gosub calc_glass_size
                      gosub build_dtl
L60400:     next dt%

            gosub write_dtl

        return clear all
        goto inputmode


        build_dtl
             init(" ") wd1$, ht1$, t5$, sp_lits$, sp_temp$
             gosub get_muttin
             gosub change_muttin
             gosub check_thickness
             sp_lits$ = "1"
             sp_qty% = 1%
             savType% = spType%                 /* (AWD006) */

             convert str(sp_warr$,9%,1%) to sp_num%, data goto L60600

L60600:
             convert sp_qty$ to sp_qty%, data goto L60550
L60550:
             for j% = 1% to sp_qty%
               for n% = 1% to ct%

                  if (sp_num% + 1%) <> n% then goto L60590
                  view% = 6%
                  if n% > 5% then view% = 12%              /* view% for spacer desc */
                  spac% = 3%
                  if n% > 5% then spac% = 9%              /* view% for spacer desc */
                  view$ = "TOP"
                  if n% > 5% then view$ = "BOT"
                  if len(ctt$(n%,1%)) < 3 then goto L60590 /* n% for number of panels */
                  wd1$ = ctt$(n%,1%)
                  ht1$ = ctt$(n%,2%)
                  t5$ = wd1$ & " x " & ht1$                /* Width & Height */
REM                  gosub check_ultra                        /* (AWD005) */
/* (AWD007) */
                  gosub lookup_intercept
                  gosub check_size
                  gosub lookup_sandwich
/* (AWD006) */
REM                  if p% = 0% then goto L60590
                  spType$ = "000"
                  convert spType% to spType$, pic(000)
/* (AWD006\) */                  

                  for k% = 1% to 2%
                      convert n%    to sp_temp$, pic(00)
                      convert j%    to sp_temp1$, pic(00)
                      convert k%    to sp_temp2$, pic(00)

                      init(" ") sp_temp_key$
                      str(sp_temp_key$,1%,2%)  = intercept$       /* (AWD007) */
                      str(sp_temp_key$,3%,3%)  = spType$          /* (AWD006) */
                      str(sp_temp_key$,6%,1%)  = sp_rack$         /* Rack Type 0 or 1 */
                      str(sp_temp_key$,7%,10%) = gdd$(view%)      /* Spacer Desc      */
                      str(sp_temp_key$,17%,10%)= gdd$(spac%)      /* Spacer thichness */
                      str(sp_temp_key$,27%,3%) = str(sp_part$,1%,3%)

                      str(sp_temp_key$,30%,9%) = wd$
                      str(sp_temp_key$,39%,8%) = ht$
                      str(sp_temp_key$,47%,2%) = str(sp_part$,5%,2%)
                      str(sp_temp_key$,49%,1%) = str(sp_part$,4%,1%)
                      sp_view$ = "0"
                      if str(view$,1%,1%) = "B" then sp_view$ = "1"
                      str(sp_temp_key$,50%,1%) = sp_view$

                      str(sp_temp_key$,51%,6%) = t_k$
                      str(sp_temp_key$,57%,8%) = muttin$
                      str(sp_temp_key$,65%,9%) = sp_warr$
                      str(sp_temp_key$,74%,8%) = sp_so$
                      str(sp_temp_key$,82%,2%) = sp_ln$
                      str(sp_temp_key$,84%,2%) = sp_temp$               /* Count Glass Panels */
                      str(sp_temp_key$,86%,2%) = sp_temp1$              /* Piece */
                      str(sp_temp_key$,88%,2%) = sp_temp2$              /* Piece */
                      str(sp_temp_key$,90%,3%) = sp_num$

                      read #18, hold, key = sp_temp_key$, eod goto no_temp
                             delete #18
                      no_temp

/* (AWD006) add type */                      
                      write #18, using L60500, intercept$, spType$, sp_rack$, ~
                                 gdd$(view%),~
                                 gdd$(spac%), str(sp_part$,1%,3%), wd$,       ~
                                 ht$,str(sp_part$,5%,2%),str(sp_part$,4%,1%), ~
                                 sp_view$, t_k$, muttin$, sp_warr$, sp_so$,   ~
                                 sp_ln$,sp_temp$,sp_temp1$, sp_temp2$, sp_num$,~
                                 t5$, sandwich$(k%), sp_lits$, view$, sp_fil$,~
                                 wd1$, ht1$, csandwich$(k%), " "
                                                   /* (AWD001) */
/* Put Original valuTe back in case of error (AWD006)*/
                    spType% = savType%                                                           
                   next k%
L60590:        next n%

L60500:            FMT CH(2), CH(3), CH(1), CH(10), CH(10), CH(3), CH(9), CH(8),~
                       CH(2),~
                       CH(1), CH(1), CH(6), CH(8), CH(9), CH(8), CH(2), CH(2), ~
                       CH(2), CH(2), CH(3), CH(20), CH(30), CH(1), CH(3),      ~
                       CH(7), CH(9), CH(8), CH(30), CH(58)


             next j%
        return

        check_size
             sp_rack$ = "1"   /* (AWD001) Change default from 1 to 0*/
             wd, ht = 0.00
             convert dcc$(n%,1%) to wd, data goto L60510

L60510:
             convert dcc$(n%,2%) to ht, data goto L60520

L60520:
REM             if wd <= 33.5 and ht <= 35.0 then sp_rack$ = "0"
REM             if wd <= 35.0 and ht <= 33.5 then sp_rack$ = "0"

             if wd <= 38.0 and ht <= 68.0 then sp_rack$ = "0"
             if wd <= 68.0 and ht <= 38.0 then sp_rack$ = "0"

             if wd <= 6.0 and ht <= 12.0 then sp_rack$ = "1"
             if wd <= 12.0 and ht <= 6.0 then sp_rack$ = "1"


/*(AWD001) Change Rack Sizes */
/* (AWD005) */
/* (AWD007) */
REM  !              if ultra% = 0% then return
REM  !                if sp_rack$ = "0" then sp_rack$ = "2"
REM  !                if sp_rack$ = "1" then sp_rack$ = "3"
        return

        lookup_sandwich
             init(" ") code$, sp_sand1$, sp_sand2$, sandwich$()
             init(" ") csandwich$()              /* (AWD001) */
             temp%     = 2%
             if view$ = "BOT" then temp% = 8%

             sp_sand1$ = str(gdd$(temp%),4%,2%)
             sp_sand2$ = str(gdd$(temp%),7%,2%)
             
/* (AWD006) */
             tpSand$ = gdd$(temp%)             
             gosub check_special_sand
/* (AWD006) remove not tempered from order file */
             p% = pos(tpSand$ = "T")
             if p% = 0% then goto notTempered
             
             tab% = 7%                  /* PLAN SAND */
             code$ = sp_sand1$
             gosub check_code
             str(sandwich$(1%),1%,30%) = str(desc$,1%,30%)
REM          if str(sp_sand1$,1%,2%) = str(sp_sand2$,1%,2%) then return


             init(" ") code$
             code$ = sp_sand2$
             gosub check_code
             str(sandwich$(2%),1%,30%) = str(desc$,1%,30%)

/* (AWD001)  */

             tab% = 9%
             init(" ") code$
             code$ = strength$ & sp_sand1$     /* (AWD006) */
             gosub check_code
             str(csandwich$(1%),1%,30%) = str(desc$,1%,30%)

             init(" ") code$
             code$ = strength$ & sp_sand2$     /* (AWD006) */
             gosub check_code
             str(csandwich$(2%),1%,30%) = str(desc$,1%,30%)

             if str(csandwich$(1%),1%,30%) = " " then goto noSandwich
             if str(csandwich$(2%),1%,30%) = " " then goto noSandwich

        return
/*(AWD006) */        
noSandwich:
           spType% = 998%
           if str(csandwich$(1%),1%,30%) = " " then ~
               str(csandwich$(1%),1%,30%) = str(sp_part$,5,2) & "  " & tpSand$

           if str(csandwich$(2%),1%,30%) = " " then ~
               str(csandwich$(2%),1%,30%) = str(sp_part$,5,2) & "  " & tpSand$
        
        return
notTempered
REM             call "SHOSTAT" (" Not Tempered ")
REM             stop
             spType% = 999%

             str(csandwich$(1%),1%,30%) = str(sp_part$,5,2) & "  " & tpSand$
             str(csandwich$(2%),1%,30%) = str(sp_part$,5,2) & "  " & tpSand$
        return
/* (AWD006\) */        
        check_special_sand
             temp_spec% = 0%
             tab% = 8%
             code$ = str(sp_part$,5%,2%)
             gosub check_code
             if code% = 0% then return
             sp_sand1$ = str(desc$,4%,2%)
             sp_sand2$ = str(desc$,7%,2%)
/* (AWD006) */             
             tpSand$ = str(desc$,1,10)             

             if view$ <> "BOT" then return
             sp_sand1$ = str(desc$,21%,2%)
             sp_sand2$ = str(desc$,24%,2%)
/* (AWD006) */             
             tpSand$ = str(desc$,18,10)             
             temp_spec% = 1%
        return



        write_dtl
            gosub open_file
            gosub write_headers              /* (AWD001) Cardinal Head */
            tp_citem% = 0%                   /* (AWD001) Cardinal Item */
            tp_item% = 0%
            neuSpacer% = 1%                  /* (AWD006) */
            rack% = 0%                       /* (AWD006) */
            patioRack% = 0%                  /* (AWD006) */            
            
                      /* (AWD005) flag to know when ultra rack started */
            ultra_rack% = 0%
            init(" ") sp_temp_key$                        /* (AWD007) */
            read #18, hold, key > sp_temp_key$, using L60580, intercept$,     ~
                                      tp_spacer$, eod goto write_dtl_done

L60580:             FMT CH(2), XX(4), CH(10)       /*(AWD006) */
                    sav_spacer$ = tp_spacer$
                    sav_intercept$ = intercept$
                       goto write_dtl_first
        write_dtl_nxt
            read #18, hold, eod goto write_dtl_done

write_dtl_first
/*(AWD006) add spType$ */
             get #18, using L60530, intercept$, spType$, sp_rack$, tp_spacer$,~
                                 tp_sp_th$, ~
                                 tp_model$, wd$, ht$, tp_glass$, tp_color$,   ~
                                 sp_view$, t_k$, muttin$, sp_warr$, tp_so$,   ~
                                 tp_ln$, sp_temp$, sp_temp1$, sp_temp2$,      ~
                                 sp_num$, tp_size$, tp_type$, tp_lite$,      ~
                                 tp_view$, sp_fil$, wd1$, ht1$, tp_ctype$
                                                            /* (AWD001) */


                      delete #18
                      
L60530:            FMT CH(2), CH(3),CH(1), CH(10), CH(10), CH(3), CH(9), CH(8),~
                       CH(2),                                                  ~
                       CH(1), CH(1), CH(6), CH(8), CH(9), CH(8), CH(2),  CH(2),~ 
                       CH(2), CH(2), CH(3), CH(20), CH(30), CH(1), CH(3),      ~
                       CH(7), CH(9), CH(8), CH(30)

/* (AWD005) */
             if sp_rack$ = "2" and ultra_rack% = 0% then gosub reset_rack

             if str(tp_spacer$,1%,10%) = str(sav_spacer$,1%,10%) then goto same_sp
                tp_item% = tp_item% + 4%
                sav_spacer$ = tp_spacer$
                neuSpacer% = 1%
same_sp
/* (AWD007) */
             if intercept$ = sav_intercept$ then goto same_intercept
                tp_item% = tp_item% + 4%
                sav_intercept$ = intercept$
                neuSpacer% = 1%

same_intercept


             tp_item% = tp_item% + 1%
             if tp_item% > 180% then tp_item% = 1%
             convert tp_item% to tp_item$, pic(0000)


REM             if sp_rack$ = "1" then tp_item$ = "    "
REM             if sp_rack$ = "3" then tp_item$ = "    "    /* (AWD005) */

             write #19, using L60540, tp_comma$, sp_warr$, tp_comma$,     ~
                        tp_item$, tp_comma$,     ~
                        tp_model$, tp_comma$, tp_so$, tp_comma$,          ~
                        tp_ln$, tp_comma$, sp_temp$, tp_comma$, sp_temp1$,~
                        tp_comma$, sp_temp2$,tp_comma$,tp_view$,tp_comma$,~
                        tp_size$, tp_comma$,tp_type$, tp_comma$, tp_lite$,~
                        tp_comma$, sp_rack$, tp_comma$, tp_spacer$, tp_comma$,~
                        sp_rack$, tp_comma$, intercept$, tp_comma$, ~
                        tp_spacer$, tp_comma$

L60540:            FMT CH(1), CH(9), CH(1), CH(4), CH(1), CH(3), CH(1), CH(8), ~
                       CH(1), CH(2), CH(1), CH(2), CH(1), CH(2), CH(1), CH(2), ~
                       CH(1), CH(3), CH(1), CH(20), CH(1), CH(30), CH(1),      ~
                       CH(1), CH(1), CH(1), CH(1), CH(10), CH(1), CH(1),       ~
                       CH(1), CH(2), CH(1), CH(10), CH(1)


/* (AWD001)  */

/* (AWD028) */
              if neuSpacer% = 1% then gosub newSpacer
              if tp_citem% = 104% then gosub newSpacer              
              
              tp_citem% = tp_citem% + 1%
              if tp_citem% > 104% then tp_citem% = 1%

              convert tp_citem% to tp_item$, pic(###0)


REM              if sp_rack$ = "1" then tp_item$ = "    "

              call "STRING" addr("LJ", wd1$, len(wd1$), wd1$)
              call "STRING" addr("LJ", ht1$, len(ht1$), ht1$)

              call "SPCESMSH" (wd1$, 1%)
              call "SPCESMSH" (ht1$, 1%)

              convert intercept$ to intercept%, data goto error2

error2:


              write #22, using L60560,                                      ~
/*Order */            tp_item$ , tp_comma$, ~
/*Item  */            tp_model$,      tp_comma$,                            ~
/*Series*/            tp_so$,         tp_comma$,                            ~
/*Width */                            wd1$, tp_comma$,                      ~
                                      "x", tp_comma$,                       ~
/*Height*/                            ht1$, tp_comma$,                      ~
/*GlsType*/                           tp_ctype$, tp_comma$,                 ~
/*Shape*/                             tp_comma$,                            ~
/*Qty*/                               tp_lite$, tp_comma$, sp_rack$,        ~
                                      tp_comma$,                            ~
                                  intercept$ & "-" & interdesc$(intercept%),~
                                      tp_comma$, " "

L60560:                 FMT CH(4), CH(1), CH(3), CH(1), CH(8), CH(1), CH(9), ~
                            CH(1), CH(1), CH(1), CH(8), CH(1), CH(30), CH(1), ~
                            CH(1), CH(1), CH(1), CH(1), CH(1),                ~
/*(AWD007) */               CH(08), CH(1), CH(42)
/* (AWD001) */

                     goto write_dtl_nxt
        write_dtl_done
        return

/* (AWD005) */
        reset_rack
          ultra_rack% = 1%
          tp_item% = 0%
          tp_citem% = 0%
        return

/*(AWD006)*/
        newSpacer
            init(" ") header1$, header2$, header3$, header4$, header5$
            str(header1$,1,14) = "Remake XX"
            str(header2$,1,14) = "Spacer  XXXX  "
            str(header3$,1,14) = "Type    XXX   "

            str(header2$,9,4) = tp_spacer$
            str(header3$,9,3) = spType$
            
            spType% = 0%
            convert spType$ to spType%, data goto error3

error3:
            str(header4$,1,30) =  spTypeDesc$(spType%)


REM            if sp_rack$ = "1" or sp_rack$ = "3" then goto patioRack
REM not patio
              rack% = rack% + 1%
              convert rack% to str(header1$,8,2), pic(00)  
              goto rackFinished

REM patioRack:   REM Patio Racks
              patioRack% = patioRack% + 1%
              str(header1$,1,7) = "Picture XX"
              convert patioRack% to str(header1$,9,2), pic(00)

rackFinished:

               gosub write_header_rack
               tp_citem% = 0%
               neuSpacer% = 0%
        return
/*(AWD006\) */           
        
/* (AWD001) */
        write_headers
               init(" ") header1$, header2$
               header1$ = "RELEASE DATE:"
               header2$ = "P.O.#."

               gosub write_header_rec

               init(" ") header1$, header2$
               header1$ = "DELIVERY DATE:"
               header2$ = "VENDOR:"

               gosub write_header_rec


               init(" ") header1$, header2$
               header1$ = "TO:"
               header2$ = "FROM:"

               gosub write_header_rec

               gosub write_item_header

        return

        write_header_rec
               write #22, using HEADER1,                                     ~
                         tp_comma$,                                          ~
                         tp_comma$,                                          ~
                         header1$,                                           ~
                         tp_comma$,                                          ~
                         tp_comma$,                                          ~
                         header2$,                                           ~
                         tp_comma$,                                          ~
                         tp_comma$,                                          ~
                         tp_comma$
HEADER1:             FMT CH(1), CH(1), CH(15), CH(1), CH(1), CH(15),     ~
                         CH(1), CH(1), CH(1)

        return

/* (AWD006) */
        write_header_rack
               write #22, using HEADER3, header1$, tp_comma$, header2$,    ~
                      tp_comma$, header3$, tp_comma$, header4$, tp_comma$, ~
                      header5$, tp_comma$


HEADER3:             FMT CH(15), CH(01), CH(15), CH(01), CH(15), CH(01), ~
                         CH(30), CH(01), CH(15), CH(01)

        return
/* (AWD006\) */        

        write_item_header
               write #22, using HEADER2,                                      ~
                         "ORDER #",     tp_comma$,                            ~
                         "ITEM",        tp_comma$,                            ~
                         "SERIES",      tp_comma$,                            ~
                         "SIZE",        tp_comma$, tp_comma$, tp_comma$,      ~
                         "GLASS TYPE",  tp_comma$,                            ~
                         "SHAPE *",     tp_comma$,                            ~
                         "QTY",         tp_comma$,                            ~
                         "RACK",        tp_comma$,                            ~
                         "INTERCEPT",   tp_comma$

HEADER2:                  FMT   CH(7), CH(1), CH(4), CH(1), CH(6), CH(1),  ~
                                CH(4), CH(1), CH(1), CH(1), CH(10), CH(1), ~
                                CH(7), CH(1), CH(3), CH(1),                ~
                                CH(4), CH(1), CH(9), CH(1)
        return
/* (AWD001) */




        build_dtl_rpt
             savType% = spType%                  /* (AWD006) */
             gosub get_muttin
             gosub change_muttin
             gosub check_thickness

             convert str(sp_warr$,9%,1%) to sp_num%, data goto L60250

L60250:

            for kk% = 1% to ct%
                if (sp_num% + 1%) <> kk% then goto L60175
                if len(ctt$(kk%,1%)) < 3 then goto L60175
                wd1$ = ctt$(kk%,1%)
                ht1$ = ctt$(kk%,2%)
                view$ = "TOP"
                chg$ = "N"
                if sp_stat$ > "00" then chg$ = "C"

                if kk% > 5% then view$ = "BOT"
                n% = kk%
                view% = 6%
                if view$ = "BOT" then view% = 12%
                spac% = 3%
                if n% > 5% then spac% = 9%
                convert kk%    to sp_temp$, pic(00)
/* (AWD006) */                
                sp_view$ = "0"
                if str(view$,1%,1%) = "B" then sp_view$ = "1"
               
REM  !                gosub check_ultra
/* (AWD006\) */
/* (AWD007)  */
                gosub lookup_intercept
                gosub check_size
/* (AWD006) */
                gosub lookup_sandwich
REM do not print clear label but are in order file */
                if p% = 0% then goto L60175
                spType$ = "000"
                convert spType% to spType$, pic(000)
/*(AWD006\) */                
                write #20, using L60210, intercept$, spType$, sp_rack$,     ~
                                 gdd$(view%),                               ~
                                 gdd$(spac%), str(sp_part$,1%,3%),wd$,ht$,  ~
                                 str(sp_part$,5%,2%), str(sp_part$,4%,1%),  ~
                                 t_k$, muttin$,  sp_warr$, sp_so$, sp_ln$,  ~
                                 sp_temp$,sp_temp1$, t5$, sandwich$(k%),    ~
                                 sp_lits$, view$, sp_due_date$, sp_part$,   ~
                                 sp_part_d$, sp_qty$, chg$, sp_cust$, wd1$, ht1$

L60210:          FMT CH(2), CH(3), CH(1), CH(10), CH(10), CH(3), CH(9), CH(8), ~
                     CH(2), ~
                     CH(1), CH(6), CH(8), CH(9), CH(8), CH(2), CH(2), CH(2),   ~
                     CH(20), CH(30),CH(1), CH(3), CH(8), CH(25), CH(32), CH(4),~
                     CH(1), CH(9), CH(9), CH(8)
/* (AWD006) put original value back in case of error */                     
                spType% = savType%  
L60175:     next kk%

        return

        check_thickness                            /* Thickness/Spacer */
           init(" ") readkey$, t_k$
           str(readkey$,1%,9%)   = "GED 002  "
           str(readkey$,10%,15%) = str(sp_part$,1%,3%)
           read #4,key = readkey$, using L54000, desc$, eod goto no_thickness

           t_k$ = str(desc$,1%,6%)          /* Thickness of Spacer    */
        return
        no_thickness
           t_k$="ERR-01"
        return

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#18,mode$, 500%, f2%)
            if f2% <> 0% then goto L62619
            wrk% = 1%
        return
L62619:     call "SHOSTAT" ("Error - Cannot Open (APCPLNWK)") : stop
        return
        open_work1
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#20,mode$, 500%, f2%)
            if f2% <> 0% then goto L62619
            wrk1% = 1%
        return
        delete_work
            if wrk%  = 1% then call "FILEBGON" (#18)
            if wrk1% = 1% then call "FILEBGON" (#20)
        return

        lookup_sub_part                              /* (PAR000) - BEG */
            init(" ") flag$, pgm$, bcksubpt_rec$, flds$(), info_flds$(),~
                       subpart$, infopart$, field1$, field2$, field3$, ~
                       field4$, field5$, field6$, field7$, field8$,    ~
                       field9$, field10$, field11$
            flag$ = "0"                  /* Sales Order Info         */
            pgm$  = "1"


            convert so_inv$ to so_inv%, data goto convert_alpha

            convert so_inv% to so_inv$, pic(00000000)

            goto order_converted

convert_alpha:
            convert str(so_inv$,2%,7%) to so_inv%, data goto sub_part1
sub_part1:
            convert so_inv% to str(so_inv$,2%,7%), pic(0000000)


order_converted:
            convert item_no$ to item_no%, data goto sub_part2
sub_part2:

            convert item_no% to item_no$, pic(###)

        call "AWDBKSUB"   (flag$,        /* Flag 0=SalesOrder 1=Invoice*/~
                          pgm$,          /* Calling Program 0=BCKUPDTE */~
                                         /* 1=Any Other 2=Delete       */~
                                         /* 3=Invoice                  */~
                          so_inv$,       /* SO or Invoice Num to lookup*/~
                          item_no$,      /* Item Number                */~
                          bcksubpt_rec$, /* Record If BCKUPDTE then    */~
                                         /* pass in else pass out      */~
                          flds$(),       /* Part Number Fields         */~
                          info_flds$(),  /* Information Fields         */~
                          #63,           /* BCKSUBPT File              */~
                          suberr1%)      /* Error Code                 */

/* (AWD006) */
            subpart$ = str(bcksubpt_rec$,48%,20%)
            
            if suberr1% = 0% then return
/*(AWD006) */            
            str(bcksubpt_rec$,48%,20%) = "00000000000000000000"
            subpart$ = str(bcksubpt_rec$,48%,20%)
/*(AWD006\) */            
            str(bcksubpt_rec$,48%,20%) = "00000                    "
            errormsg$ = "AWDBKSUB ERROR = "&so_inv$ & " Line= " & item_no$
            gosub error_prompt
            suberr1% = 0%

        return

/* (AWD006) */
        check_ultra
           init(" ") readkey$
           ultra% = 0%
           str(readkey$,1%,9%)   = "PLANULTRA"
           str(readkey$,10%,3%)  = str(sp_part$,1,3)
           str(readkey$,13%,2%)  = "**"
           read #4,key = readkey$, eod goto no_ultra_all
              ultra% = 1%
              return
        no_ultra_all
           str(readkey$,1%,9%)   = "PLANULTRA"
           str(readkey$,10%,3%)  = str(sp_part$,1,3)
           str(readkey$,13%,2%)  = str(sp_part$,5,2)
           read #4,key = readkey$, eod goto no_ultra
              ultra% = 1%
        no_ultra
        return

/* (AWD007) */
        lookup_intercept
          init(" ") readkey$, desc$, intercept$, model$, ty$
          model$ = str(sp_part$,1,3)
          ty$    = str(sp_part$,5,2)
          intercept% = 1%                  /* DEFAULT !! */
          str(readkey$,1,9)  = "INTERCEPT"
          str(readkey$,10,3) = model$
          str(readkey$,13,2) = ty$

           read #4, key = readkey$, using L54000, desc$,  ~
                                   eod goto no_intercept_glass

               convert str(desc$,1,2) to intercept%, data goto intercept_done
           goto intercept_done

        no_intercept_glass
          str(readkey$,1,9)  = "INTERCEPT"
          str(readkey$,10,3) = model$
          str(readkey$,13,2) = str(ty$,1,1) & "*"

           read #4, key = readkey$, using L54000, desc$,  ~
                                   eod goto no_intercept_all

               convert str(desc$,1,2) to intercept%, data goto intercept_done
           goto intercept_done

         no_intercept_all
           init(" ") readkey$
           str(readkey$,1,9)  = "INTERCEPT"
           str(readkey$,10,3) = model$
           str(readkey$,13,2) = "**"

           read #4, key = readkey$, using L54000, desc$,  ~
                                   eod goto intercept_done

               convert str(desc$,1,2) to intercept%, data goto intercept_done

        intercept_done
          convert intercept% to intercept$,pic(00)
        return

        load_interdesc
          init(" ") readkey$, desc$, interdesc$()
          str(readkey$,1,9) = "INTERDESC"
        interdesc_next
          read #4, key > readkey$, using INTERDESC_FMT, readkey$, desc$, ~
                                              eod goto interdesc_done
INTERDESC_FMT:            FMT CH(24), CH(30)
                  if str(readkey$,1,9) <> "INTERDESC" then goto interdesc_done

                  intercept% = 9%
                  convert str(readkey$,10,2) to intercept%,      ~
                                         data goto interdesc_done

                  if intercept% > 9% then goto interdesc_error

                  interdesc$(intercept%) = str(desc$,1,5)
                  goto interdesc_next

         interdesc_done
         return
         interdesc_error
            errormsg$ = "INTERDESC ERROR = Error Loading data from INTERDESC"
            gosub error_prompt
         goto exit_program
/* (AWD007\) */



        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            gosub delete_work

            end

