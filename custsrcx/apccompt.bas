        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC    CCC    OOO   M   M  PPPP   TTTTT   *~
            *  A   A  P   P  C   C  C   C  O   O  MM MM  P   P    T     *~
            *  AAAAA  PPPP   C      C      O   O  M M M  PPPP     T     *~
            *  A   A  P      C   C  C   C  O   O  M   M  P        T     *~
            *  A   A  P       CCC    CCC    OOO   M   M  P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCCOMPT - COMPLAINT TRACKING ENTRY/LOOKUP/REPORTING      *~
            *                                                           *~
            *   ( Security TABLE = 'CSECURITY'- User Id's With Access ) *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/20/92 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 06/24/92 ! Mod to set Screen defaults, Add new      ! RHH *~
            *          ! new Routine 'SET_DEFAULTS'               !     *~
            * 10/15/93 ! Mod to Blank Out Customer Code and Name  ! RHH *~
            *          ! for Stress Cracks. Routine 'PT_1'        !     *~
            *          ! Line (60130). Removed ( 10/19/93 )       !     *~
            * 09/29/97 ! Mod Remove the (APCORDER) file, nolonger ! RHH *~
            *          !   used.                                  !     *~
	    * 11/26/97 ! Check for 60403 Revision Complaince      ! DJD *~
            * 03/24/98 ! Y2K Project                              ! ERN *~
            * 03/23/99 ! Mod to usage of CSECURITY Table to allow ! BWS *~
            *          !  display only & delete (EWD001).         !     *~
            * 10/05/00 ! Mod to fix bug with call to TXTPRINT     ! CMG *~
            *          !                        (EWD002).         !     *~
            * 10/05/00 ! Mod to get number of max pages to print  ! CMG *~
            *          !     from "..." in CSECURITY table.       !     *~
            *          !                        (EWD003).         !     *~
            * 10/30/00 ! Mod to change tracking code to credit    ! CMG *~
            *          !     code. (EWD004)                       !     *~
            * 12/18/00 ! Additional mods to for inside sales entry! CMG *~
            *          !           (EWD004)                       !     *~
            * 01/04/01 ! Numerous mods to revamp complaint system ! CMG *~
            * 01/18/01 ! Mod to pack the complaint number to      ! CMG *~
            *          !  utilize all eight digits. (EWD005)      !     *~
            * 12/06/04 ! (EWD006) Use Order Date as default for   ! RHH *~
            *          !  error date.                             !     *~
            * 12/14/04 ! (EWD007) Mod to Look Sales Order up in   ! RHH *~
            *          !  History for the Oder Date, when not in  !     *~
            *          !  the current Database.                   !     *~
            * 01/07/05 ! (EWD008) Mod to prevent the changing of  ! RHH *~
            *          !  Sales Order Date, When found.           !     *~ 
            * 01/18/05 ! (AWD009) Mod to add new complaint codes  ! CMG *~
            * 02/15/05 ! (AWD010) Mod to save Order Header Date   ! RHH *~
            *          !          use ord_flag%                   !     *~  
            * 01/17/06 ! (PAR000) CR347 mods for sub part         ! CMG *~
            * 08/03/06 ! (AWD011) Mod to add info part            ! DES *~
            * 02/14/07 ! (AWD012) Mod to add replacement so/line  ! DES *~
            *10/04/2007! (AWD013) Mod add AWDHIST and stop comp.  ! DES *~
	    *          !          code 117 & 117 from entering a  !     *~
	    *          !          "z" in col1 & 2.                !     *~
            * 04/18/08 ! (AWD014) mod to make all sub columns     ! CMG *~
            *          !     to Z except 133 and 117              !     *~
            *************************************************************

        dim                              /* (APCCOMPT) COMPLAINT FILE  */~
            hdr$40,                      /* ASKUSER HEADER             */~
            msg$(3%)79,                  /* ASKUSER TEXT               */~
            blankdate$10,                /* PD empty date              */~
            blankfield$1,                /* Blank Field                */~
/*(EWD005)*/comp_number$8,               /* COMPLAINT NUMBER           */~
/* AWD011 */ part_info$20,               /* Part Information Number    */~
/* AWD012 */ replace_so$8,               /* replacement sales order    */~
/* AWD012 */ replace_ln$8,               /* replacement sales order line*/~
            comp_mfg_plant$1,            /* Mfg Plant                  */~
            comp_cuscode$9,              /* CUSTOMER CODE              */~
            cust_name$30,                /* CUSTOMER NAME              */~
            comp_so$8,                   /* SALES ORDER NUMBER         */~
            comp_so2$7,                  /* SALES ORDER NUMBER         */~
            comp_line$2,                 /* S.O. LINE ITEM             */~
            comp_po$16,                  /* CUSTOMER PO NUMBER         */~
            comp_rga$8,                  /* APC RGA NUMBER             */~
            comp_part$25,                /* PART NUMBER                */~
            comp_part_d$40,              /* PART NUMBER DESCRIPTION    */~
            comp_quan$4,                 /* QUANTITY                   */~
            comp_code$3,                 /* COMPLAINT CODE             */~
            comp_code_desc$32,           /* COMPLAINT CODE DESCRIPTION */~
            comp_init$2,                 /* INITIATOR CODE (INIT CODE) */~
            comp_init_desc$32,           /* INITIATOR CODE DESCRIPTION */~
            comp_init_dte$8,             /* DATE OF COMPLAINT          */~
            comp_init_txt$4,             /* INITIATOR TEXT ID          */~
            comp_init_txt1$3,            /* INITIATOR TEXT ID  YES/NO  */~
            comp_svcs$2,                 /* SERVICE CODE (SVCS CODE)   */~
            comp_svcs_desc$32,           /* SERVICE CODE DESCRIPTION   */~
/*EWD004*/  comp_crd$2,                  /* Credit   CODE              */~
/*EWD004*/  comp_crd_desc$32,            /* Credit   CODE DESCRIPTION  */~
            comp_cost$10,                /* COST OF COMPLAINT          */~
            comp_userid$3,               /* LAST MODIFIED BY           */~
            comp_dte$6,                  /* DATE LAST MODIFIED         */~
            comp_slmn$4,                 /* SALESMAN                   */~
            comp_slmn_desc$32,           /* SALESMAN Name              */~
/*PAR000*/  comp_filler$86,              /* FILLER AREA                */~
/*PAR000*/  comp_rec$256,                /* COMPLAINT RECORD           */~
/*PAR000*/  comp_subp$20,                /* COMPLAINT SUBPART          */~
            comp_key$5,                  /* PRIMARY KEY                */~
            comp_key1$10,                /* ALT (1) KEY                */~
            comp_key2$16,                /* ALT (2) KEY                */~
            comp_key3$8,                 /* ALT (3) KEY                */~
            comp_mod$3, comp_mod_d$30,   /* Model and Description      */~
            textid$4,                    /* TEXT ID                    */~
            txt$4,                       /* TEXT ID                    */~
            readkey$50,                  /* GENCODES PRIMARY KEY       */~
            store_key$3,                 /* STORE (000)                */~
            comp_no$8,                   /* NEXT COMPLAINT NUMBER      */~
            next_ref$16,                 /* NEXT REFERENCE NUMBER      */~
            descr$64,                    /* PLOWCODE DESCRIPTION       */~
            text$(113%,1%)70,            /* Text Buffer area           */~
            header$79,                   /*                            */~
/*(EWD005)*/next_number$28,              /* Next Complaint Number      */~
            bck_key$25,                  /* BCKMASTR-BCKLINES KEY      */~
            scr$(10%)40,                 /* Report Selection Menu      */~
            beg_date$10, end_date$10,    /* Beginning/Ending Dates     */~
            beg_dte$10, end_dte$10,      /*                            */~
            tst_dte$10,                  /* Test Date                  */~
            beg_mod$3, end_mod$3,        /* Beg/End Model Codes        */~
            type$1,type_desc$32,         /* Summary or Detail          */~
            sel$1,                       /* Report Selection           */~
            sel_desc$34,                 /* Report Selection Descript  */~
            wrk$5,                       /* WRK REC NUMBER             */~
            wrk_key1$22,                 /* WORK SORT KEY              */~
            txt_1$20,                    /* PRINT TEXT DESCRIPTION     */~
            mask$80,                     /* PRINT TEXT MASK            */~
            company$60,                  /* For Report Company Name    */~
            print_title$43,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(23%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3,                    /* Current User Id            */~
/*EWD003*/  pages$30,                    /* Max number of Pages        */~
/*EWD004*/  comp_err_dte$8,              /* DATE OF PSC/CSC Error      */~
/*EWD008*/  comp_err_sav$8,              /* Save Date Cannot Change    */~ 
/*EWD004*/  comp_ed_id$3,                /* Complaint Editor ID        */~
/*EWD004*/  comp_en_id$3,                /* Complaint Entry ID         */~
/*EWD004*/  comp_pr_id$3,                /* Complaint Proofer ID       */~
            ord_date$8,                  /* (EWD006) Order Date        */~
            ship_date$8,                 /* (EWD006) Ship Date         */~ 
            comp_wizd$2,                 /* Window Wizard(COMP WIZD)   */~
            comp_wizd_desc$32,           /* Window Wizard Complaint    */~
            rpt_test$10                  /* Test Var for report        */

        dim f2%(64%),                    /* = 0 if the file is open    */~
            f1%(64%),                    /* = 1 if READ was successful */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64%)20                 /* Text from file opening     */

                                         /* (EWD007)                   */
        dim                              /* (EWDHIST) S.O. History     */~
            hs_key1$15,                  /* History Alt Key (1)        */~
            hs_po$16,                    /* History P.O Number         */~
            hs_so$8,                     /* History S.O. Number        */~
            hs_ln$3,                     /* History Line No.           */~
            hs_cust$9,                   /* History Customer Code      */~
            hs_order$6,                  /* History S.O. Date          */~
            hs_part$25                   /* History Part Number        */

                                         /* (EWD007)                   */ 
        dim tm_po$16,                    /* History P.O Number         */~
            tm_so$8,                     /* History S.O. Number        */~
            tm_ln$3,                     /* History Line No.           */~
            tm_cust$9,                   /* History Customer Code      */~
            tm_order$6,                  /* History S.O. Date          */~
            tm_part$25                   /* History Part Number        */

        dim tx_po$16,                    /* History P.O Number         */~
            tx_so$8,                     /* History S.O. Number        */~
            tx_ln$3,                     /* History Line No.           */~
            tx_cust$9,                   /* History Customer Code      */~
            tx_order$6,                  /* History S.O. Date          */~
            tx_part$25                   /* History Part Number        */

        dim                              /*  (AWD009)                  */~
            col_key$6,                   /* Generic Readkey            */~
            col_desc$60,                 /* Generic Description        */~
            col_no$1,                    /* Column Number              */~
            col1$1,                      /* New Column 1               */~
            col1_desc$60,                /* Generic Description        */~
            col2$1,                      /* New Column 2               */~
            col2_desc$60,                /* Generic Description        */~
            col3$1,                      /* New Column 3               */~
            col3_desc$60,                /* Generic Description        */~
            col4$1,                      /* New Column 4               */~
            col4_desc$60,                /* Generic Description        */~
            col5$1,                      /* New Column 5               */~
            col5_desc$60,                /* Generic Description        */~
            col6$1,                      /* New Column 6               */~
            col6_desc$60,                /* Generic Description        */~
            col7$1,                      /* New Column 7               */~
            col7_desc$60                 /* Generic Description        */


        dim                              /* PAR000                     */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
/*AWD011*/  bcksubpt_key$11,             /* BCKSUBPT Key               */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4             /* Additional Info Fields     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 02/15/05 Complaint Tracking System      "
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
            * #1  ! APCCOMPT ! COMPLAINT TRACKING MASTER FILE           *~
            * #2  ! EWDHIST  ! Master Sales Order History File  (EWD007)*~
            * #3  ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            * #4  ! GENCODES ! SYSTEM MASTER TABLE FILES                *~
            * #5  ! TXTFILE  ! MASTER SYSTEM TEXT FILE                  *~
            * #6  ! STORNAME ! MASTER STORE FILE (COMPLAINT NO ASSGN)   *~
            * #7  ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
            * #8  ! BCKMASTR ! S.O. HEADER MASTER FILE                  *~
            * #9  ! AMTBOMIF ! VALIDTY FILE FOR DESCRIPTIONS            *~
            * #10 ! APCCOMP1 ! APC Work File                            *~
            * #11 ! HNYMASTR ! Part Master File                         *~
            * #15 ! SLMMASTR ! SALESMAN CODES                           *~
            * #16 ! TXTCOMPL ! Complaint Text File                      *~
            * #18 ! COMPCOL1 ! Complaint Reason Validation Column 1     *~
            * #19 ! COMPCOL2 ! Complaint Reason Validation Column 2     *~
            * #20 ! COMPCOL3 ! Complaint Reason Validation Column 3     *~
            * #21 ! COMPCOL4 ! Complaint Reason Validation Column 4     *~
            * #22 ! COMPCOL5 ! Complaint Reason Validation Column 5     *~
            * #23 ! COMPCOL6 ! Complaint Reason Validation Column 6     *~
            * #24 ! COMPCOL7 ! Complaint Reason Validation Column 7     *~
            * #42 ! AWDHIST  ! MASTER SALES HIST FILE        (AWD013)   *~
            * #63 ! BCKSUBPT ! Sub Part File                 (PAR000)   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "APCCOMPT",                                     ~
/*PAR000*/              varc,     indexed,  recsize = 256,               ~
                        keypos =    1, keylen =    5,                    ~
                        alt key  1, keypos =    6, keylen =  10,         ~
                            key  2, keypos =   16, keylen =  16, dup,    ~
                            key  3, keypos =   32, keylen =   8, dup,    ~
                            key  4, keypos =   40, keylen =  13, dup,    ~
			    key  5, keypos =  191, keylen =  10, dup 

                                                         /* (EWD007)    */
            select #2,  "EWDHIST",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   17, keylen =  32,                     ~
                        alt key  1, keypos =   54, keylen =  15,         ~
                            key  2, keypos =   49, keylen =  20,         ~
                            key  3, keypos =   77, keylen =  26,         ~
                            key  4, keypos =    1, keylen =  25, dup,    ~
                            key  5, keypos =  193, keylen =   8, dup 
                                                         /* (EWD007)    */

            select #3,   "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =    9,                    ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #5,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

            select #6,  "STORNAME",                                      ~
                        varc,     indexed,  recsize =   300,             ~
                        keypos =    1, keylen =   3

            select #7,   "BCKLINES",                                     ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =   19

            select #8,   "BCKMASTR",                                     ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =   25,                    ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #9,   "AMTBOMIF",                                     ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =   32

            select #10,  "APCCOMP1",                                     ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos =    1, keylen =    5,                    ~
                        alt key  1, keypos =    6, keylen = 22, dup

            select #11, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1,   keylen =  25,                     ~
                        alt key  1, keypos  =   102, keylen =  9, dup,   ~
                            key  2, keypos  =    90, keylen =  4, dup,   ~
                            key  3, keypos  =    26, keylen = 32, dup

            select #15, "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4


            select #16, "TXTCOMPL",                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

/*(EWD004)*/select #17,  "USERINFO",                                     ~
                        varc,     indexed,  recsize =   150,            ~
                        keypos =    1, keylen =  3 




            select #18, "COMPCOL1",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  4 

            select #19,"COMPCOL2",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  5 

            select #20,"COMPCOL3",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  6

            select #21,"COMPCOL4",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  6

            select #22,"COMPCOL5",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  6

            select #23,"COMPCOL6",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  6

            select #24,"COMPCOL7",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  6

                                                         /* (AWD013)    */
            select #42, "AWDHIST",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   17, keylen =  32,                     ~
                        alt key  1, keypos =   54, keylen =  15,         ~
                            key  2, keypos =   49, keylen =  20,         ~
                            key  3, keypos =   77, keylen =  26,         ~
                            key  4, keypos =    1, keylen =  25, dup,    ~
                            key  5, keypos =  193, keylen =   8, dup 
/*PAR000*/
            select #63, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~ 
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup    

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1%), f2%(1%),100%, rslt$(1%))

           filename$ = "CUSTOMER" : call "EWDOPEN" (#3, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "TXTFILE" : call "EWDOPEN" (#5, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "STORNAME" : call "EWDOPEN" (#6, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "BCKLINES" : call "EWDOPEN" (#7, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "BCKMASTR" : call "EWDOPEN" (#8, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "AMTBOMIF" : call "EWDOPEN" (#9, filename$, err%)
           if err% <> 0% then gosub open_error
           
           call "OPENCHCK" (#10, fs%(10%), f2%(10%),  0%, rslt$(10%))

                                                   /* (EWD007)         */
           call "OPENCHCK" (#2,  fs%(2%),  f2%(2%),   0%, rslt$(2%))
                                                   /* (AWD013)         */
           call "OPENCHCK" (#42,  fs%(42%),  f2%(42%),   0%, rslt$(42%))
                                                   /* (EWD007)         */
           
           filename$ = "HNYMASTR" : call "EWDOPEN" (#11, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "SLMMASTR" : call "EWDOPEN" (#15, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "TXTCOMPL" : call "EWDOPEN" (#16, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "USERINFO" : call "EWDOPEN" (#17, filename$, err%)
           if err% <> 0% then gosub open_error

           filename$ = "COMPCOL1" : call "EWDOPEN" (#18, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "COMPCOL2" : call "EWDOPEN" (#19, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "COMPCOL3" : call "EWDOPEN" (#20, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "COMPCOL4" : call "EWDOPEN" (#21, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "COMPCOL5" : call "EWDOPEN" (#22, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "COMPCOL6" : call "EWDOPEN" (#23, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "COMPCOL7" : call "EWDOPEN" (#24, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
           if err% <> 0% then gosub open_error


            mat f1% = zer

            if fs%(10%) = 0 then goto L09000
               call "FILEBGON" addr(#10)

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "DATUFMTC" (blankdate$)
            blankfield$ = " "
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            next_number$ = "Next Complaint No.: None    "  /* (EWD005) */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            page% = 1%                                       /* (AWD009) */
            edit% = 1%
            gosub initialize_variables
/* <AWD012> */
REM         for fieldnr% = 1% to  20%
REM             if fieldnr% =  9% then goto L10235            /* (AWD009) */
REM             if fieldnr% = 11% then goto L10235            /* (AWD009) */
            for fieldnr% = 1% to  22%
                if fieldnr% = 11% then goto L10235            /* (AWD009) */
                if fieldnr% = 13% then goto L10235            /* (AWD009) */
/* </AWD012> */
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
                      if keyhit% = 14% then goto inputmode_report
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
L10235:     next fieldnr%


                                                /* (AWD009) - Second Page */
          inputmode_columns
            if comp_rec% = 1% then goto editpg1              /* (AWD009)  */
            for fieldnr% = 1% to 9%
            page% = 2%                                       /* (AWD009) */
REM                gosub'071 (fieldnr%) 
REM                     if fieldnr% = 1% and account% = 1% then L10500
REM                     if fieldnr% = 2% and billto%  = 1% then L10500
REM                     if fieldnr% = 3% and parent%  = 1% then L10500
                     if enabled% = 0% then L10480
L10360:         gosub'071(fieldnr%)     /* Set Defaults               */
                      if enabled% = 0% then L10480
L10380:         gosub'103(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10460
L10410:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'071(fieldnr%)
                         if enabled% = 1% then L10380
                         if fieldnr% = 1% then L10360
                         goto L10410
L10460:               if keyhit%  = 16% then       exit_program
                      if keyhit% <>  0% then       L10380
L10480:         gosub'153(fieldnr%)
                      if errormsg$ <> " " then L10380
REM L10500:     next fieldnr%




        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
REM         if fieldnr% =  9% and page% = 1% then goto editpg1   /* (AWD009) */
REM         if fieldnr% = 11% and page% = 1% then goto editpg1   /* (AWD009) */
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  5% then editpg3
                  if keyhit%  = 12% then gosub delete_complaint
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11140:     if cursor%(1%) <= 13 then fieldnr% = cursor%(1%) - 2%
            if cursor%(1%) = 10  then fieldnr% = 0%
            if cursor%(1%) = 11  then fieldnr% = cursor%(1%) - 3%
            if cursor%(1%) >  4  then fieldnr% = fieldnr% + 1%   
            if cursor%(1%) >  5  then fieldnr% = fieldnr% + 1%   
            if cursor%(2%) > 40% then fieldnr% = fieldnr% + 1%
            
            if cursor%(1%) > 13 then fieldnr% = cursor%(1%) - 1%
            if cursor%(1%) > 15 then fieldnr% = cursor%(1%)
            if cursor%(1%) = 15% and cursor%(2%) > 40% then    ~
                     fieldnr% = 15%

/* <AWD012> */
REM         if fieldnr% =  9% and page% = 1% then goto editpg1   /* (AWD009) */
REM         if fieldnr% = 11% and page% = 1% then goto editpg1   /* (AWD009) */
REM         if fieldnr% < 1% or fieldnr% > 20% then editpg1
            if fieldnr% = 11% and page% = 1% then goto editpg1   /* (AWD009) */
            if fieldnr% = 13% and page% = 1% then goto editpg1   /* (AWD009) */
            if fieldnr% < 1% or fieldnr% > 22% then editpg1
/* </AWD012> */

            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11410:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11410
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11410
                  lastfieldnr% = fieldnr%
            goto L11140


        REM *************************************************************~
            *       I N P U T   M O D E   R E P O R T   S C R E E N     *~
            *************************************************************

        inputmode_report
            gosub initialize_variables

            for fieldnr% = 1% to 10%
L12080:         gosub'061(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12200
L12100:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L12180
L12130:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'061(fieldnr%)
                         if enabled% = 1% then L12100
                         if fieldnr% = 1% then L12080
                         goto L12130
L12180:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L12100
L12200:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12100
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   R E P O R T   S C R E E N      *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg2
L13110:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 10% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'061(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L13160:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13160
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13160
                  lastfieldnr% = fieldnr%
            goto L13110




        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  5% then editpg1
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0 then       editpg3
L11860:     fieldnr% = cursor%(1) - 2%
            if fieldnr% < 1 or fieldnr% > 9 then editpg3
            if fieldnr% = lastfieldnr% then editpg3
            gosub'071(fieldnr%)
                  if enabled% = 0% then       editpg3
L11970:     gosub'103(fieldnr%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11970
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11970
                     lastfieldnr% = fieldnr%
REM                     gosub check_fields
                     goto L11860


        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
           call "SHOSTAT" ("Selecting & Sorting Data")
           call "OPENCHCK" (#10,  fs%(10%), f2%(10%),500%, rslt$(10%))
           wrk% = 0%
           comp_key$ = all(hex(00))
           read #1,key > comp_key$, using L19080, comp_rec$,              ~
                                                      eod goto print_done
           goto L19085
        print_next
           read #1, using L19080, comp_rec$, eod goto print_done
L19080:      FMT CH(256)                            /*PAR000*/
L19085:    check% = 0%
           comp_number$ = str(comp_rec$,1%,5%)
           part_info$ = str(comp_rec$,171%,20%)
/*AWD012*/ replace_so$ = str(comp_rec$,191,8)  
/*AWD012*/ replace_ln$ = str(comp_rec$,199,2)  
           gosub unpack_comp_number
           gosub check_data
           if check% = 0% then goto print_next
           wrk_key1$ = all(hex(00))
           on sel% gosub upd_1, upd_2, upd_3, upd_4, upd_5, upd_6, upd_7
           goto print_next
        print_done
           gosub generate_report
           call "FILEBGON" addr(#10)
        return clear all
        goto inputmode

        check_data                                   /* CUSTOMER CODE  */
          if str(comp_cuscode$,1%,3%) = "ALL" then goto L19170
             init(" ") rpt_test$  :  rpt_test$ = str(comp_rec$,44%,9%)
          if str(rpt_test$,1%,9%) = str(comp_cuscode$,1%,9%) then goto L19170
             return
                                                     /* COMPLAINT CODE */
L19170:   if str(comp_code$,1%,1%) = "A" then goto L19190
             init(" ") rpt_test$  :  rpt_test$ = str(comp_rec$,82%,3%)
          if str(rpt_test$,1%,3%) = comp_code$ then goto L19190
             return
                                                     /* INITIATOR CODE */
L19190:   if str(comp_init$,1%,1%) = "A" then goto L19210
             init(" ") rpt_test$  :  rpt_test$ = str(comp_rec$,85%,2%)
          if str(rpt_test$,1%,2%) = comp_init$ then goto L19210
             return
                                                     /* CSC/PSC CODE   */
L19210:   if str(comp_svcs$,1%,1%) = "A" then goto L19230
             init(" ") rpt_test$  : rpt_test$ = str(comp_rec$,97%,2%)
          if str(rpt_test$,1%,2%) = comp_svcs$ then goto L19230
             return
                                                     /* Credit   CODE EWD004 */
L19230:   if str(comp_crd$,1%,1%) = "A" then goto L19250
             init(" ") rpt_test$  : rpt_test$ = str(comp_rec$,109%,2%)
          if str(rpt_test$,1%,2%) = comp_crd$ then goto L19250
             return
                                                     /* Wizard CODE    */
L19250:   if str(comp_wizd$,1%,1%) = "A" then goto L19270
             init(" ") rpt_test$  :  rpt_test$ = str(comp_rec$,121%,2%)
          if str(rpt_test$,1%,2%) = comp_wizd$ then goto L19270
              return
                                                     /* COMPLAINT DATE */
L19270:   if str(beg_date$,1%,1%) = "A" then goto L19295
             init(" ") rpt_test$  :  rpt_test$ = str(comp_rec$,87%,6%)
          if str(rpt_test$,1%,6%) < str(beg_dte$,1%,6%)        ~
                or str(rpt_test$,1%,6%) > str(end_dte$,1%,6%) then return

                                                     /* Salesman Code */
L19295:   if str(comp_slmn$,1%,1%) = "A" then goto L19320
             init(" ") rpt_test$  :  rpt_test$ = str(comp_rec$,40%,4%)
          if str(rpt_test$,1%,4%) = comp_slmn$ then goto L19320
             return

                                                     /* MODEL CODES    */
L19320:   if str(beg_mod$,1%,1%) = "A" then goto L19340
          if str(comp_rec$,53%,3%) < beg_mod$ or str(comp_rec$,53%,3%) > ~
                                            end_mod$ then return

L19340:   check% = 1%
        return

        upd_1
           str(wrk_key1$,1%,3%) = str(comp_rec$,82%,3%)  /* COMPLAINT  */
           str(wrk_key1$,4%,9%) = str(comp_rec$,44%,9%)  /* CUSTOMER   */
           str(wrk_key1$,13%,6%)= str(comp_rec$,87%,6%)  /* DATE       */
           goto update_work

        upd_2
           str(wrk_key1$,1%,9%) = str(comp_rec$,44%,9%)  /* CUSTOMER   */
           str(wrk_key1$,10%,3%)= str(comp_rec$,82%,3%)  /* COMPLAINT  */
           str(wrk_key1$,13%,6%)= str(comp_rec$,87%,6%)  /* DATE       */
           goto update_work

        upd_3
           str(wrk_key1$,1%,2%) = str(comp_rec$,121%,2%) /* SOURCE     */
           str(wrk_key1$,3%,3%) = str(comp_rec$,82%,3%)  /* COMPLAINT  */
           str(wrk_key1$,6%,9%) = str(comp_rec$,44%,9%)  /* CUSTOMER   */
           str(wrk_key1$,15%,6%)= str(comp_rec$,87%,6%)  /* DATE       */
           goto update_work

        upd_4
           str(wrk_key1$,1%,9%) = str(comp_rec$,44%,9%)  /* CUSTOMER   */
           str(wrk_key1$,10%,2%)= str(comp_rec$,121%,2%) /* SOURCE     */
           str(wrk_key1$,12%,3%)= str(comp_rec$,82%,3%)  /* COMPLAINT  */
           str(wrk_key1$,15%,6%)= str(comp_rec$,87%,6%)  /* DATE       */
           goto update_work

        upd_5
           str(wrk_key1$,1%,6%) = str(comp_rec$,87%,6%)  /* DATE       */
           str(wrk_key1$,7%,9%) = str(comp_rec$,44%,9%)  /* CUSTOMER   */
           str(wrk_key1$,16%,3%)= str(comp_rec$,82%,3%)  /* COMPLAINT  */
           goto update_work

        upd_6
           str(wrk_key1$,1%,4%) = str(comp_rec$,40%,4%)  /* SALESMAN   */
           str(wrk_key1$,5%,9%) = str(comp_rec$,44%,9%)  /* CUSTOMER   */
           str(wrk_key1$,14%,3%)= str(comp_rec$,82%,3%)  /* COMPLAINT  */
           str(wrk_key1$,17%,6%)= str(comp_rec$,87%,6%)  /* DATE       */
           goto update_work

        upd_7
           str(wrk_key1$,1%,3%) = str(comp_rec$,53%,3%)  /* Model      */
           str(wrk_key1$,4%,3%) = str(comp_rec$,82%,3%)  /* COMPLAINT  */
           str(wrk_key1$,7%,9%) = str(comp_rec$,44%,9%)  /* CUSTOMER   */
           str(wrk_key1$,16%,6%)= str(comp_rec$,87%,6%)  /* DATE       */
           goto update_work

        update_work
           wrk% = wrk% + 1%
           convert wrk% to wrk$, pic(00000)
           write #10, using L19605, wrk$, wrk_key1$, str(comp_rec$,1%,5%)
L19605:      FMT CH(5), CH(22), CH(5)
        return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
           if edit% <> 2% then return
/* <AWD0122> */
REM            if fieldnr% > 5% then return
               if fieldnr% = 3% then return
               if fieldnr% = 5% then return
               if fieldnr% > 6% then return
/* </AWD0122> */
                  enabled% = 0%
        return

        deffn'061(fieldnr%)
            enabled% = 1%
        return


        deffn'071(fieldnr%)                                /* (AWD009) */
            enabled% = 1%

REM            if edit% = 2% then gosub check_fields
            if fieldnr% < 5% then return
               if fieldnr% = 5% then col3$ = "Z"
               if fieldnr% = 6% then col4$ = "Z"
               if fieldnr% = 7% then col5$ = "Z"
               if fieldnr% = 8% then col6$ = "Z"
               if fieldnr% = 9% then col7$ = "Z"
        return                                   
        check_fields
              for i% = 1% to 9%
                    sav_fieldnr% = fieldnr%
                    gosub '153(i%)
                    if errormsg$ = " " then goto next_field
                       init(" ") comp_code$, comp_init_txt1$, col1$,   ~
                            col1_desc$, col2$, col2_desc$, col3$,      ~
                            col3_desc$, col4$, col4_desc$, col5$,      ~
                            col5_desc$, col6$, col6_desc$, col7$,      ~
                            col7_desc$
                       edit% = 1%

                       return clear all
                       goto inputmode_columns 
                   
next_field

                    fieldnr% = sav_fieldnr%
              next i% 
        return                                              /* (AWD009) */

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

        scrn1_msg  :  data                /* EWD001 - Minor Mods */      ~
         "Enter Complaint Number for Look-up, or Leave Blank for Entry.",~
         "Enter a S.O.,'?' to Scan,' ' for LkUp or 'R' for System.(Req)",~
         "Enter a S.O.,'?' to Scan,' ' for LkUp or 'R' for System.(Req)",~
         "Enter a Valid S.O. Line No or Leave Blank for Look-Up.  (Req)",~
         "Enter a Valid S.O. Line No or Leave Blank for Look-Up.  (Req)",~
         "Enter a Valid Customer PO Number.                  (Optional)",~
         "Enter a Valid APC RGA Code.                        (Optional)",~
         "Enter a Valid Customer Code or '?' for Look-Up     (Optional)",~
         "Enter a Valid Part Number or '?' for Look-Up       (Optional)",~
         "Enter a Valid Quantity.                            (Optional)",~
         "Enter Applicable Complaint Code.                        (Req)",~
         "Enter Applicable Initiator Code.                        (Req)",~
         "Enter, Edit or View Initiator Text. Use PF(8) Key.      (Req)",~
         "Enter Initiator Date.                                   (Req)",~
         "Enter Applicable CSC/PSC Error Code.                    (Req)",~
         "Enter Applicable Credit Code.                           (Req)",~
         "Enter Editor ID Initials.                               (Req)",~
         "Enter Date Error Occurred.                              (Req)",~
         "Enter Entry ID Initials.                                (Req)",~
         "Enter Proofer ID Initials.                              (Req)",~         
         "Enter The Applicable Cost of Complaint.                      "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28440
                inpmessage$ = edtmessage$
                return

L28440
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                /* EWD001 - Minor Mods */      ~
         "Enter a Valid Report Selection.                              ",~
         "Enter (S) for Summary or (D) for Detail Report.              ",~
         "Enter a Valid Customer Code, (A) for ALL or '?' for Look-Up. ",~
         "Enter a Valid Complaint Code or (A) for All.                 ",~
         "Enter a Valid Initiator Code or (A) for All.                 ",~
         "Enter a Valid PSC/CSC Code or (A) for All.                   ",~
         "Enter a Valid Credit Code or (A) for All.                    ",~
         "Enter a Valid Window Wizard Code or (A) for All.             ",~
         "Enter a Valid Beginning and Ending Complaint Date or (A) All.",~
         "Enter a Valid Beginning and Ending Model Code or (A) for All."


        deffn'070(scrnr%, fieldnr%)                /*  (AWD009) - Beg */
            if fieldnr% <> 0% then L28540
                inpmessage$ = edtmessage$
                return

L28540
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn3_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn3_msg  :  data                                               ~
         "Enter a Complaint Code                                       ",~
         "Enter Complaint Text Yes or No                               ",~
         "Enter a Valid Sub Part                                       ",~
         "Enter a Valid Part or Error Code                             ",~
         "Enter a Comment Code                                         ",~
         "N/A                                                          ",~
         "N/A                                                          ",~
         "N/A                                                          ",~
         "N/A                                                          "  
                                                    /* (AWD009) - END */

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
                                                       /* (EWD006)     */
        initialize_variables
            init(" ") errormsg$, inpmessage$, comp_number$, comp_so$,    ~
                      comp_cuscode$, comp_line$, comp_po$, comp_rga$,    ~
                      comp_part$, comp_quan$, comp_code$, comp_init$,    ~
                      comp_init_dte$, comp_init_txt$, comp_svcs$,        ~
                      comp_crd$, comp_userid$, comp_wizd$, comp_wizd_desc$,~
                      comp_dte$, comp_filler$, readkey$, comp_code_desc$,~
                      comp_init_desc$, comp_svcs_desc$, comp_crd_desc$,  ~
                      comp_init_txt1$, comp_no$, descr$, cust_name$,     ~
                      comp_key$, comp_key1$, comp_key2$, comp_key3$,     ~
                      comp_part_d$, sel_desc$, comp_cost$, type$,        ~
                      type_desc$, sel$, sel_desc$, beg_date$,            ~
                      end_date$, beg_dte$, end_dte$, comp_mod$,          ~
                      comp_mod_d$, comp_ed_id$, comp_en_id$, comp_pr_id$,~
                      comp_err_dte$, beg_mod$, end_mod$, ord_date$,      ~
                      ship_date$, comp_err_sav$, comp_subp$, part_info$, ~
		      comp_mfg_plant$, replace_so$, replace_ln$
                                                        /* (EWD008)     */
                                                                /*PAR000*/
                      beg_dte$   = blankdate$
                      end_dte$   = blankdate$
                      tst_dte$   = blankdate$
                      ord_date$  = blankdate$ 
                      ship_date$ = blankdate$
                      ord_flag%  = 0%                   /* (AWD010)     */
                                                        /* (EWD006)     */
            comp_slmn$ = "0000"
            comp_cost  = 0.0
            store_key$ = "000"
            gosub next_complaint_no
            init (hex(ff)) textid$, comp_init_txt$, text$()
            call "TXTFUTIL" (#16, f2%(16%), "INTL", textid$)

            scr$(1%) = "*********< Report Sort Options >********"
            scr$(2%) = "* (1) By Complaint Code,Customer,Date  *"
            scr$(3%) = "* (2) By Customer,Complaint Code,Date  *"
            scr$(4%) = "* (3) By Wizard,Complaint,Customer,Date*"
            scr$(5%) = "* (4) By Customer,Wizard,Complaint,Date*"
            scr$(6%) = "* (5) By Date,Customer,Complaint       *"
            scr$(7%) = "* (6) By Salesman,Customer,Complaint,DT*"
            scr$(8%) = "* (7) By Model,Complaint,Customer,Date *"
            scr$(9%) = "****************************************"


* EWD001    if userid$ = "ERN" or userid$ = "ern" then return

            readkey$ = all(hex(00))
            readkey$ = "CSECURITY" & userid$
            call "DESCRIBE" (#4, readkey$, comp_code_desc$, 0%, f1%(4))
            if f1%(4%) = 0% then gosub check_security /* ACCESS DENIED */

/*EWD001*/  security% = 1%  /*0=Display Only; 1=Normal; 2=Delete Access*/  
/*EWD001*/  convert str(comp_code_desc$,,1%) to security%,               ~
/*EWD001*/      data goto L20500

L20500:     comp_code_desc$ = " "

                                                     /*  (AWD009)  */
            init(" ") col_key$, col_desc$, col1$, col1_desc$, col2$,     ~ 
                      col2_desc$, col3$, col3_desc$, col4$, col4_desc$,  ~
                      col5$, col5_desc$, col6$, col6_desc$, col7$,       ~
                      col7_desc$
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
            *************************************************************
        dataload
             get #1, using L35040, str(comp_number$,1%,4%), blankfield$, ~
                                   comp_so$, comp_line$, comp_po$,       ~
                                   comp_rga$, comp_slmn$, comp_cuscode$, ~
                                   comp_part$, comp_quan$, comp_code$,   ~
                                   comp_init$, str(comp_init_dte$,1%,6%),~
                                   comp_init_txt$, comp_svcs$,           ~
                                   str(tst_dte$,1%,6%), comp_ed_id$,     ~
                                   comp_crd$, str(comp_err_dte$,1%,6%),  ~
                                   comp_en_id$,                          ~
                                   comp_wizd$, comp_pr_id$, comp_cost,   ~
                                   comp_userid$, comp_dte$,              ~
                                   col1$, col2$, col3$, col4$, col5$,    ~
                                   col6$, col7$, comp_subp$, part_info$, ~
				   replace_so$, replace_ln$
            comp_mfg_plant$ = str(part_info$,7,1)
                                                  /* PAR000 */

            gosub unpack_comp_number               /*  (EWD005)  */
            
            if comp_init_dte$ = blankdate$ then comp_init_dte$ = " "
            if comp_err_dte$  = blankdate$ then comp_err_dte$  = " "
            if comp_dte$      = blankdate$ then comp_dte$      = " "
            if comp_err_sav$  = blankdate$ then comp_err_sav$  = " "
                                                   /* (EWD008)   */

/* AWD011 */
           init(" ") bcksubpt_rec$, bcksubpt_key$
	   str(bcksubpt_key$,1,8) = comp_so$
           str(bcksubpt_key$,10,11) = comp_line$
	   if str(bcksubpt_key$,10,1) = "0" then str(bcksubpt_key$,10,1) = " "
	   if str(bcksubpt_key$,11,1) = "0" then str(bcksubpt_key$,11,1) = " "
           read #63, key = bcksubpt_key$, USING L50604, bcksubpt_rec$,  ~
                     eod goto L50604
L50604:    FMT CH(256)
           part_info$ = str(bcksubpt_rec$,132,20)  
            comp_mfg_plant$ = str(part_info$,7,1)
/* END AWD011 */

            gosub L50615                            /* CUSTOMER NAME    */
            gosub L50715                            /* PART DESCRIPTION */
            gosub L50895                            /* COMPLAINT CODE   */
            gosub L50975                            /* INITIATOR CODE   */
            gosub L51055                            /* INITIATOR DATE   */
            gosub L51110                            /* INITIATOR TEXT   */
            gosub L51140                            /* SERVICE CODE     */
            gosub L51260                            /* SERVICE TEXT     */
            gosub L51260                            /* Credit Code      */
            gosub L51350                            /* Editor ID        */
            gosub L51410                            /* Entry ID         */
            gosub L51520                            /* Proofer ID       */
            gosub L51380                            /* Date of Error    */
            gosub L51600                            /* Wizard Code      */

            if col1$ = " " then col1$ = "Z"
            if col2$ = " " then col2$ = "Z"
            if col3$ = " " then col3$ = "Z"
            if col4$ = " " then col4$ = "Z"
            if col5$ = " " then col5$ = "Z"
            if col6$ = " " then col6$ = "Z"
            if col7$ = " " then col7$ = "Z"

            gosub L50895
            gosub L51110
            gosub L53000
            gosub L53100
            gosub L53200
            gosub L53300
            gosub L53400
            gosub L53500
            gosub L53600 

            gosub lookup_model                     /* Model Lookup     */
            errormsg$ = " "
            assign% = 0% : lookup% = 1%
            convert comp_cost to comp_cost$, pic(##,###.##-)

        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *************************************************************
        dataput
 /*EWD001*/   if security% = 0% then return      /* For Safety... */
              if assign%  <> 0% then gosub assign_complaint
              gosub check_fields
              call "DATUNFMT" (comp_init_dte$)
              call "DATUNFMT" (comp_err_dte$)
              gosub pack_comp_number                /*  (EWD005)   */
              
              read #1,hold,key = comp_key$, eod goto L31140
                 delete #1
L31140:       comp_userid$ = userid$ : comp_dte$ = date
              put #1, using L35040, str(comp_number$,1%,4%), blankfield$,~
                                   comp_so$, comp_line$, comp_po$,       ~
                                   comp_rga$, comp_slmn$, comp_cuscode$, ~
                                   comp_part$, comp_quan$, comp_code$,   ~
                                   comp_init$, str(comp_init_dte$,1%,6%),~
                                   comp_init_txt$, comp_svcs$,           ~
                                   str(blankdate$,1%,6%), comp_ed_id$,   ~
                                   comp_crd$, str(comp_err_dte$,1%,6%),  ~
                                   comp_en_id$,                          ~
                                   comp_wizd$, comp_pr_id$, comp_cost,   ~
                                   comp_userid$, comp_dte$,              ~
                                   col1$, col2$, col3$, col4$, col5$,    ~
                                   col6$, col7$, comp_subp$, part_info$, ~
				   replace_so$, replace_ln$
                                                       /* PAR000 */
              write #1
        return clear all
        goto inputmode

        REM *************************************************************~
            *       A S S I G N   C O M P L A I N T   N U M B E R       *~
            *************************************************************

        assign_complaint
            assign%, lookup% = 0%
            read #6,hold,key = store_key$, using L32080, next_ref$,       ~
                                                 comp_no$, eod goto L32250
L32080:        FMT POS(162), CH(16), POS(209), CH(8)
            convert comp_no$ to comp_no%, data goto L32250

            convert comp_no% to comp_number$, pic(00000000)
            comp_no% = comp_no% + 1%
            convert comp_no% to comp_no$, pic(00000000)
            if ref% = 0% then goto L32220
               comp_so$   = "R" & "0" & str(next_ref$,2%,6%)
               comp_line$ = "01"
               convert str(next_ref$,2%,6%) to next_ref%,data goto L32250

               next_ref% = next_ref% + 1%
               convert next_ref% to str(next_ref$,2%,6%), pic(000000)

L32220:     put #6, using L32080, next_ref$, comp_no$
            rewrite #6
        return
L32250:     stop "(Error) Updating Store (" & store_key$ &               ~
                  ") Complaint No. " & comp_no$
        return clear all
        goto inputmode

        next_complaint_no
            comp_no$ = " "
            read #6,key = store_key$, using L32080, next_ref$, comp_no$,  ~
                                                   eod goto L32350
            str(next_number$,21%,8%) = str(comp_no$,1%,8%)
L32350: return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35040: FMT                              /* (APCCOMPT) COMPLAINT FILE  */~
            CH(04),                      /* COMPLAINT NUMBER  (EWD005) */~
            CH(01),                      /* FILLER            (EWD005) */~
            CH(08),                      /* SALES ORDER NUMBER         */~
            CH(02),                      /* S.O. LINE ITEM             */~
            CH(16),                      /* CUSTOMER PO NUMBER         */~
            CH(08),                      /* APC RGA NUMBER             */~
            CH(04),                      /* SALESMAN                   */~
            CH(09),                      /* CUSTOMER CODE              */~
            CH(25),                      /* PART NUMBER                */~
            CH(04),                      /* QUANTITY                   */~
            CH(03),                      /* COMPLAINT CODE             */~
            CH(02),                      /* INITIATOR CODE (INIT CODE) */~
            CH(06),                      /* DATE OF COMPLAINT          */~
            CH(04),                      /* INITIATOR TEXT ID          */~
            CH(02),                      /* CSC/PSC CODE (SVCS CODE)   */~
            CH(06),                      /* BLANK   DATE               */~
            CH(04),                      /* ENTRY ID                   */~
            CH(02),                      /* Credit CODE                */~
            CH(06),                      /* COMPLAINT ERROR DATE       */~
            CH(04),                      /* COMPLAINT ENTRY ID         */~
            CH(02),                      /* COMP WIZARD CDE (COMP WIZD)*/~
            CH(04),                      /* COMPLAINT PROOFER ID       */~
            PD(14,4),                    /* COST OF COMPLAINT          */~
            CH(03),                      /* LAST MODIFIED BY           */~
            CH(06),                      /* DATE LAST MODIFIED         */~
            CH(01),                      /* Column1           (AWD009) */~
            CH(01),                      /* Column2           (AWD009) */~
            CH(01),                      /* Column3           (AWD009) */~
            CH(01),                      /* Column4           (AWD009) */~
            CH(01),                      /* Column5           (AWD009) */~
            CH(01),                      /* Column6           (AWD009) */~
            CH(01),                      /* Column7           (AWD009) */~
            CH(20),                      /* Subpart           (AWD011) */~
            CH(20),                      /* Part info         (AWD011) */~
            CH(08),                      /* replacement so    (AWD012) */~
            CH(02)                       /* replacement line  (AWD012) */


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
REM.........................    L40330,         /* S.O. Line Item    */ 
              on fieldnr% gosub L40330,         /* Complaint Code    */   ~
                                L40320,         /* Sales Order No.   */   ~
                                L40320,         /* Replace SO        */   ~
                                L40320,         /* S.O. Line Item    */   ~
                                L40320,         /* Replace line      */   ~
                                L40320,         /* Customer PO No.   */   ~
                                L40320,         /* APC RGA Number    */   ~
                                L40320,         /* Customer Number   */   ~
                                L40320,         /* Part Number       */   ~
                                L40330,         /* Quantity          */   ~
                                L40320,         /* Complaint Code    */   ~
                                L40320,         /* Initiator Code    */   ~
                                L40320,         /* Initiator Text    */   ~
                                L40320,         /* Initiator Date    */   ~
                                L40320,         /* CSC/PSC Error Code*/   ~
                                L40320,         /* Editor ID         */   ~
                                L40320,         /* Initiator Date    */   ~                                
                                L40320,         /* Entry ID          */   ~
                                L40320,         /* Proofer ID        */   ~
                                L40320,         /* Wizard Code       */   ~                                
                                L40320,         /* Credit Code       */   ~                                
                                L40330          /* Cost of complaint */
              goto L40350

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40320:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40330:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40350:     accept                                                       ~
               at (01,02),                                               ~
                  "Complaint Tracking - Entry and Lookup - (APCCOMPT)",  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "* Complaint Number  :",                      ~
/*(EWD005)*/   at (03,25), fac(lfac$(1%)), comp_number$         , ch(08),~
/*(EWD005)*/   at (03,40), fac(hex(a4))  , next_number$         , ch(28),~
                                                                         ~
               at (04,02), "* Sales Order Number:",                      ~
               at (04,25), fac(lfac$(2%)), comp_so$             , ch(08),~
               at (04,42), "    Replacement S.O.:",                      ~
/* <AWD012> */ at (04,65), fac(lfac$(3%)), replace_so$          , ch(08),~
                                                                         ~
               at (05,02), "* S.O. Line Item No.:",                      ~
               at (05,25), fac(lfac$(4%)), comp_line$           , ch(02),~
/* <AWD012> */ at (05,65), fac(lfac$(5%)), replace_ln$          , ch(02),~
                                                                         ~
               at (06,02), "* Customer PO Number:",                      ~
               at (06,25), fac(lfac$(6%)), comp_po$             , ch(16),~
                                                                         ~
               at (07,02), "* APC RGA Number    :",                      ~
               at (07,25), fac(lfac$(7%)), comp_rga$            , ch(08),~
                                                                         ~
               at (08,02), "  Customer Code     :",                      ~
               at (08,25), fac(lfac$(8%)), comp_cuscode$        , ch(09),~
               at (08,40), fac(hex(84)),   cust_name$           , ch(30),~
               at (08,75), fac(hex(84)),   comp_slmn$           , ch(04),~
                                                                         ~
               at (09,02), "  Part Number       :",                      ~
               at (09,25), fac(lfac$(9%)), comp_part$           , ch(25),~
/*PAR000*/     at (09,52), fac(lfac$(9%)), comp_subp$           , ch(20),~
/*PAR000*/     at (10,52), fac(hex(84)),str(comp_part_d$,1%,28%), ch(28),~
                                                                         ~
/* AWD011 */   at (10,02), "  Part Information 	:",                      ~
/* AWD011 */   at (10,25), fac(hex(84)), part_info$             , ch(20),~
                                                                         ~
               at (11,02), "  Quantity-No.Items :",                      ~
               at (11,25), fac(lfac$(10%)), comp_quan$           , ch(04),~
                                                                         ~
               at (12,02), "  Initiator Code    :",                      ~
               at (12,25), fac(lfac$(12%)), comp_init$          , ch(02),~
               at (12,40), fac(hex(84)),   comp_init_desc$      , ch(32),~
                                                                         ~
               at (13,40), "Date:",                                      ~
               at (13,46), fac(lfac$(14%)), comp_init_dte$      , ch(08),~
                                                                         ~
               at (14,02), "  CSC/PSC Error Code:",                      ~
               at (14,25), fac(lfac$(15%)), comp_svcs$          , ch(02),~
               at (14,40), fac(hex(84)),   comp_svcs_desc$      , ch(32),~
                                                                         ~
               at (15,02), "  Editor Initials   :",                      ~
               at (15,25), fac(lfac$(16%)), comp_ed_id$         , ch(03),~
                                                                         ~
               at (15,40), "Date of Err:",                               ~
               at (15,53), fac(lfac$(17%)), comp_err_dte$       , ch(08),~
                                                                         ~
               at (16,02), "  Entry Initials    :",                      ~
               at (16,25), fac(lfac$(18%)), comp_en_id$         , ch(03),~
                                                                         ~
               at (17,02), "  Proofer Initials  :",                      ~
               at (17,25), fac(lfac$(19%)), comp_pr_id$         , ch(03),~
                                                                         ~
               at (18,02), "  Window Wizard Code: ",                     ~
               at (18,25), fac(lfac$(20%)), comp_wizd$          , ch(02),~
               at (18,40), fac(hex(84)),   comp_wizd_desc$      , ch(32),~
                                                                         ~
               at (19,02), "  Credit  Code      :",                      ~
               at (19,25), fac(lfac$(21%)), comp_crd$           , ch(02),~
               at (19,40), fac(hex(84)),   comp_crd_desc$       , ch(32),~
                                                                         ~
               at (20,02), "  Cost of Complaint :",                      ~
               at (20,25), fac(lfac$(22%)), comp_cost$          , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
                                                  /* (AWD009)  */
REM               if keyhit% = 8% and fieldnr% = 11% then                   ~
                                                  gosub edit_init_text
               if keyhit% <> 15 then goto L41190
                  call "PRNTSCRN"
                  goto L40350

L41190:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41410     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                 (8)Add/Edit Text       " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffff08ffffffffff0e0f1000)
            if fieldnr% = 1% then L41340
               str(pf$(1%),64%) = " " : str(pfkeys$,14%,1%) = hex(ff)
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L41340:     if fieldnr% > 1% then L41360
               str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L41360:     
                                                     /*  (AWD009)   */
            REM  if fieldnr% = 11% or fieldnr% = 14% or fieldnr% = 17% or     ~
                                           fieldnr% = 20% then goto L41390
               if fieldnr% = 14% or fieldnr% = 17% or                      ~
                                           fieldnr% = 20% then goto L41390
               str(pf$(3%),18%,26%) = " " : str(pfkeys$,8%,1%) = hex(ff)
L41390:     return

L41410: if fieldnr% > 0% then L41550  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over    (5)Page 2              " &        ~
                     "                                       "
/*EWD001*/  pf$(2%)= "                 (8)Add/Edit/Display Tex" &        ~
/*EWD001*/           "t                      (15)Print Screen"
            pf$(3%)= "                 (12)Delete Complaint   " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffff05ffff08ffffff0cffff0f1000)
                                                   /*  (AWD009)   */
REM            if fieldnr% = 11% or fieldnr% = 14% or fieldnr% = 17% or     ~
                                           fieldnr% = 20% then goto L41520
            if fieldnr% = 14% or fieldnr% = 17% or                          ~
                                           fieldnr% = 20% then goto L41520
               str(pf$(2%),18%,26%) = " " : str(pfkeys$,8%,1%) = hex(ff)
L41520:     if assign% = 0% and security% = 2% then goto L41530 /*EWD001*/
               str(pf$(3%),18%,26%) = " " : str(pfkeys$,12%,1%) = hex(ff)
L41530:     if security% <> 0% then goto L41540:                /*EWD001*/
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L41540:     return                                            /*EWD001 ^*/
L41550:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
/*EWD001*/  pf$(2%)= "                 (8)Add/Edit/Display Tex" &        ~
/*EWD001*/           "t                                      "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffff08ffffffffffffffff00)
                                                               /*  (AWD009)  */
REM            if fieldnr% = 11% or fieldnr% = 14% or fieldnr% = 17% or     ~
                                           fieldnr% = 20% then goto L41630
            if fieldnr% = 14% or fieldnr% = 17% or     ~
                                           fieldnr% = 20% then goto L41630
               str(pf$(2%),18%,26%) = " " : str(pfkeys$,8%,1%) = hex(ff)
L41630:     return


        REM *************************************************************~
            *               R E P O R T   S C R E E N                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42250,         /* REPORT SELECTION  */   ~
                                L42240,         /* ENTRY (2)         */   ~
                                L42240,         /* ENTRY (3)         */   ~
                                L42240,         /* ENTRY (4)         */   ~
                                L42240,         /* ENTRY (5)         */   ~
                                L42240,         /* ENTRY (6)         */   ~
                                L42240,         /* ENTRY (7)         */   ~
                                L42240,         /* ENTRY (8)         */   ~
                                L42240,         /* ENTRY (9)         */   ~
                                L42240          /* ENTRY (10)        */
              goto L42270

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42240:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L42250:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42270:     accept                                                       ~
               at (01,02),                                               ~
                  "Complaint Tracking - Report Screen - (APCCOMPT)",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Report Selection    :",                      ~
               at (03,25), fac(lfac$(1%)), sel$                 , ch(01),~
               at (03,40), fac(hex(84))  , sel_desc$            , ch(33),~
                                                                         ~
               at (04,02), "(S)ummary, (D)etail :",                      ~
               at (04,25), fac(lfac$(2%)), type$                , ch(01),~
               at (04,30), fac(hex(84))  , type_desc$           , ch(14),~
               at (04,45), "SLMN:",                                      ~
               at (04,51), fac(lfac$(2%)), comp_slmn$           , ch(04),~
         at (04,56), fac(hex(84))  , str(comp_slmn_desc$,1%,23%), ch(23),~
                                                                         ~
               at (05,02), "Customer Code       :",                      ~
               at (05,25), fac(lfac$(3%)), comp_cuscode$        , ch(09),~
               at (05,40), fac(hex(84))  , cust_name$           , ch(32),~
                                                                         ~
               at (06,02), "Complaint Code,(A)ll:",                      ~
               at (06,25), fac(lfac$(4%)), comp_code$           , ch(03),~
               at (06,40), fac(hex(84))  , comp_code_desc$      , ch(32),~
                                                                         ~
               at (07,02), "Initiator Code,(A)ll:",                      ~
               at (07,25), fac(lfac$(5%)), comp_init$           , ch(02),~
               at (07,40), fac(hex(84))  , comp_init_desc$      , ch(32),~
                                                                         ~
               at (08,02), "CSC/PSC Code,  (A)ll:",                      ~
               at (08,25), fac(lfac$(6%)), comp_svcs$           , ch(02),~
               at (08,40), fac(hex(84))  , comp_svcs_desc$      , ch(32),~
                                                                         ~
               at (09,02), "Credit Code,   (A)ll:",                      ~
               at (09,25), fac(lfac$(7%)), comp_crd$            , ch(02),~
               at (09,40), fac(hex(84))  , comp_crd_desc$       , ch(32),~
                                                                         ~
               at (10,02), "Wizard Code,   (A)ll:",                      ~
               at (10,25), fac(lfac$(8%)), comp_wizd$           , ch(02),~
               at (10,40), fac(hex(84))  , comp_wizd_desc$      , ch(32),~
                                                                         ~
               at (11,02), "Beginning Date      :",                      ~
               at (11,25), fac(lfac$(9%)), beg_date$            , ch(10),~
               at (11,40), "Ending Date         :",                      ~
               at (11,65), fac(lfac$(9%)), end_date$            , ch(10),~
                                                                         ~
               at (12,02), "Beginning Model Code:",                      ~
               at (12,25), fac(lfac$(10%)), beg_mod$            , ch(03),~
               at (12,40), "Ending Model Code   :",                      ~
               at (12,65), fac(lfac$(10%)), end_mod$            , ch(03),~
                                                                         ~
               at (13,21), fac(hex(84)),   scr$(1%)             , ch(40),~
               at (14,21), fac(hex(84)),   scr$(2%)             , ch(40),~
                                                                         ~
               at (15,21), fac(hex(84)),   scr$(3%)             , ch(40),~
               at (16,21), fac(hex(84)),   scr$(4%)             , ch(40),~
               at (17,21), fac(hex(84)),   scr$(5%)             , ch(40),~
                                                                         ~
               at (18,21), fac(hex(84)),   scr$(6%)             , ch(40),~
               at (19,21), fac(hex(84)),   scr$(7%)             , ch(40),~
                                                                         ~
               at (20,21), fac(hex(84)),   scr$(8%)             , ch(40),~
               at (21,21), fac(hex(84)),   scr$(9%)             , ch(40),~
                                                                         ~
               at (22,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L43020
                  call "PRNTSCRN"
                  goto L42270

L43020:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L43190     /*  Input Mode             */
            pf$(1%)= "(1)Start Over    (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(2%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L43150
               str(pf$(2%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L43150:     if fieldnr% > 1% then L43170
               str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L43170:     return

L43190: if fieldnr% > 0% then L43260  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over    (14)Generate Report    " &        ~
                     "                       (15)Print Screen"
            pf$(2%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L43260:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return


        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
              gosub'070(1%, fieldnr%)
              gosub set_pf3
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L44320,         /* Complaint Code    */   ~
                                L44320,         /* Complaint Text    */   ~
                                L44320,         /* Column1           */   ~
                                L44320,         /* Column2           */   ~
                                L44320,         /* Column3           */   ~
                                L44320,         /* Column4           */   ~
                                L44320,         /* Column5           */   ~
                                L44320,         /* Column6           */   ~
                                L44320          /* Column7           */   
              goto L44350

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L44320:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L44350:     accept                                                       ~
               at (01,02),                                               ~
                  "Complaint Tracking - Page 2             (APCCOMPT)",  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Complaint Code      :",                      ~
               at (03,25), fac(lfac$(1%)), comp_code$           , ch(03),~
               at (03,40), fac(hex(84)),   comp_code_desc$      , ch(32),~
                                                                         ~
               at (04,02), "Complaint Text      :",                      ~
               at (04,25), fac(lfac$(2%)), comp_init_txt1$      , ch(03),~
                                                                         ~
               at (05,02), "Column 1 - Sub Part :",                      ~
               at (05,25), fac(lfac$(3%)), col1$                , ch(01),~
               at (05,30), fac(hex(84))  , col1_desc$           , ch(50),~
                                                                         ~
               at (06,02), "Column 2 - Part/Err :",                      ~
               at (06,25), fac(lfac$(4%)), col2$                , ch(01),~
               at (06,30), fac(hex(84))  , col2_desc$           , ch(50),~
                                                                         ~
               at (07,02), "Column 3 - Comment  :",                      ~
               at (07,25), fac(lfac$(5%)), col3$                , ch(01),~
               at (07,30), fac(hex(84))  , col3_desc$           , ch(50),~
                                                                         ~
               at (08,02), "Column 4 - N/A      :",                      ~
               at (08,25), fac(lfac$(6%)), col4$                , ch(01),~
               at (08,30), fac(hex(84))  , col4_desc$           , ch(50),~
                                                                         ~
               at (09,02), "Column 5 - N/A      :",                      ~
               at (09,25), fac(lfac$(7%)), col5$                , ch(01),~
               at (09,30), fac(hex(84))  , col5_desc$           , ch(50),~
                                                                         ~
               at (10,02), "Column 6 - N/A      :",                      ~
               at (10,25), fac(lfac$(8%)), col6$                , ch(01),~
               at (10,30), fac(hex(84))  , col6_desc$           , ch(50),~
                                                                         ~
               at (11,02), "Column 7 - N/A      :",                      ~
               at (11,25), fac(lfac$(9%)), col7$                , ch(01),~
               at (11,30), fac(hex(84))  , col7_desc$           , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% = 8% and fieldnr% =  2% then                   ~
                                                  gosub edit_init_text

               if keyhit% <> 15 then goto L45190
                  call "PRNTSCRN"
                  goto L44350

L45190:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
        if edit% = 2% then L45410     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                 (8)Add/Edit Text       " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffff08ffffffffff0e0f1000)
            if fieldnr% = 1% then L45340
               str(pf$(1%),64%) = " " : str(pfkeys$,14%,1%) = hex(ff)
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L45340:     if fieldnr% > 1% then L45360
               str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L45360:     if fieldnr% =  2%  then goto L45390
               str(pf$(3%),18%,26%) = " " : str(pfkeys$,8%,1%) = hex(ff)
L45390:     return

L45410: if fieldnr% > 0% then L45550  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over    (5) Page 1             " &        ~
                     "                                       "
            pf$(2%)= "                 (8)Add/Edit/Display Tex" &        ~
                     "t                      (15)Print Screen"
            pf$(3%)= "                 (12)Delete Complaint   " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffff05ffff08ffffff0cffff0f1000)
            if fieldnr% =  2% then goto L45520
               str(pf$(2%),18%,26%) = " " : str(pfkeys$,8%,1%) = hex(ff)
L45520:     if assign% = 0% and security% = 2% then goto L45530
               str(pf$(3%),18%,26%) = " " : str(pfkeys$,12%,1%) = hex(ff)
L45530:     if security% <> 0% then goto L45540
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L45540:     return                                         
L45550:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                 (8)Add/Edit/Display Tex" &        ~
                     "t                                      "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffff08ffffffffffffffff00)
            if fieldnr% =  2%  then goto L45630
               str(pf$(2%),18%,26%) = " " : str(pfkeys$,8%,1%) = hex(ff)
L45630:     return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
              on fieldnr% gosub L50160,         /* Complaint Code    */   ~
                                L50250,         /* Sales Order No.   */   ~
                                L50350,         /* Sales Order No.   */   ~
                                L50405,         /* S.O. Line Item    */   ~
                                L50455,         /* S.O. Line Item    */   ~
                                L50500,         /* Customer PO No.   */   ~
                                L50540,         /* APC RGA Number    */   ~
                                L50615,         /* Customer Number   */   ~
                                L50715,         /* Part Number       */   ~
                                L50835,         /* Quantity          */   ~
                                L50895,         /* Complaint Code    */   ~
                                L50975,         /* Initiator Code    */   ~
                                L51110,         /* Initiator Text    */   ~
                                L51055,         /* Initiator Date    */   ~
                                L51140,         /* CSC/PSC Err Code  */   ~
                                L51350,         /* Editor ID         */   ~
                                L51380,         /* Date Error Occurred*/  ~
                                L51410,         /* Entry ID          */   ~
                                L51520,         /* Proofer ID        */   ~
                                L51600,         /* Window wizard Code*/   ~
                                L51260,         /* Credit Code       */   ~
                                L51550          /* Cost of Complaint */

            return

L50160: REM COMPLAINT NUMBER                      COMP_NUMBER$
           assign%, ref%, dataload%, comp_rec% = 0%
           if comp_number$ <> " " then goto L50190
/*EWD001*/    if security% <> 0%  then assign% = 1%
              return

L50190:    gosub pack_comp_number           /*  (EWD005)  */

           read #1,key = comp_key$, eod goto L50230
           dataload% = 1%   :   gosub dataload
           fieldnr% = 22%   :   dataload% = 0%
              comp_rec% = 1%                    /* (AWD009) */

              return clear all                  /* (AWD009) */
              goto editpg1                      /* (AWD009) */
REM        return                               /* (AWD009) */
L50230:    errormsg$ = "(Error) - Complaint Number Not on File?"
           init(" ") comp_number$
        return

L50250: REM SALES ORDER                           COMP_SO$
            ref% = 0%
            if str(comp_so$,1%,1%) = "R" then goto L50315
            if str(comp_so$,1%,1%) = "A" then goto L50265
            if str(comp_so$,1%,1%) = "B" then goto L50265
            if comp_so$ <> " " then L50260              /*EWD001*/
              return
L50260:     if comp_so$ <> "?" then goto L50275         /*EWD001*/
              comp_so$ = " "                            /*EWD001*/
              descr$ = hex(06) & "Select Sales Order"   /*EWD001*/
/*EWD001*/    call "PLOWCODE" (#7,comp_so$,descr$,-16%,-.001,f1%(3))
              return
                                              
L50265:     comp_so2$ = str(comp_so$,2,7)             
            convert comp_so2$ to comp_so2%, data goto L50300
            convert comp_so2% to comp_so2$, pic(00000000)

/* ----------- typo???  ------------------ */
REM	    str(comp_so2$,2,7) = comp_so2$
	    str(comp_so$,2,7) = comp_so2$

            gosub lookup_order
              return

L50275:     convert comp_so$ to comp_so%, data goto L50300

            convert comp_so% to comp_so$, pic(00000000)
            gosub lookup_order
        return
L50300:     errormsg$ = "(Error) - Invalid Sales Order Number?"
            init(" ") comp_so$
        return
L50315:     if len(comp_so$) > 1% then goto L50340
               ref% = 1%
               comp_so$ = "R" & "0" & str(next_ref$,2%,6%)
               comp_line$ = "01"
        return
L50340:     comp_key1$ = all(hex(00))
            convert str(comp_so$,2%,7%) to comp_so%, data goto L50300

            convert comp_so% to str(comp_so$,2%,7%), pic(0000000)

            comp_line$ = "01"
            str(comp_key1$,1%,8%) = comp_so$
            str(comp_key1$,9%,2%) = comp_line$
            read #1,key 1% = comp_key1$, eod goto L50300
            gosub dataload
            fieldnr% = 23%
        return

L50350: REM REPLACEMENT SALES ORDER               REPLACE_SO$           
            repl_ref% = 0%
            if str(replace_so$,1%,1%) = "R" then goto L50385
            if str(replace_so$,1%,1%) = "A" then goto L50365
            if str(replace_so$,1%,1%) = "B" then goto L50365
            if replace_so$ <> " " then L50360              /*EWD001*/
              return
L50360:     if replace_so$ <> "?" then goto L50375         /*EWD001*/
              replace_so$ = " "                            /*EWD001*/
              descr$ = hex(06) & "Select Sales Order"   /*EWD001*/
/*EWD001*/    call "PLOWCODE" (#7,replace_so$,descr$,-16%,-.001,f1%(3))
              return
                                              
L50365:     comp_so2$ = str(replace_so$,2,7)             
            convert comp_so2$ to comp_so2%, data goto L50380
            convert comp_so2% to comp_so2$, pic(00000000)
	    str(replace_so$,2,7) = comp_so2$
            gosub lookup_replacement 
              return

L50375:     convert replace_so$ to replace_so%, data goto L50380
            convert replace_so% to replace_so$, pic(00000000)
            gosub lookup_replacement 
        return
L50380:     errormsg$ = "(Error) - Invalid Sales Order Number?"
            init(" ") comp_so$
        return
L50385:     if len(comp_so$) > 1% then goto L50390
               repl_ref% = 1%
               replace_so$ = "R" & "0" & str(next_ref$,2%,6%)
               replace_ln$ = "01"
        return
L50390:     comp_key1$ = all(hex(00))
            convert str(replace_so$,2%,7%) to comp_so%, data goto L50380

            convert comp_so% to str(replace_so$,2%,7%), pic(0000000)

REM         comp_line$ = "01"
REM         str(comp_key1$,1%,8%) = comp_so$
REM         str(comp_key1$,9%,2%) = comp_line$
REM         read #1,key 1% = comp_key1$, eod goto L50380
REM         gosub dataload
REM         fieldnr% = 23%
        return

L50405: REM SALES ORDER LINE ITEM                 COMP_LINE$
            comp_rec% = 0%
            if comp_so$ <> " " then goto L50420
               return
L50420:     if comp_line$ <> " " then goto L50435
L50425:        comp_line$ = "01"

L50435:     convert comp_line$ to comp_line%, data goto L50425

            convert comp_line% to comp_line$, pic(00)
            comp_key1$ = all(hex(00))
            str(comp_key1$,1%,8%) = comp_so$
            str(comp_key1$,9%,2%) = comp_line$
            read #1,key 1% = comp_key1$, eod goto L50445
            gosub dataload
            comp_rec% = 1%
            fieldnr% = 23%
        return
L50445:     gosub lookup_order
        return

L50455: REM SALES ORDER LINE ITEM     REPLACE_LN$             
            comp_rec% = 0%
            if replace_so$ <> " " then goto L50470
               return
L50470:     if replace_ln$ <> " " then goto L50485
L50475:        replace_ln$ = "01"

L50485:     convert replace_ln$ to replace_ln%, data goto L50475
            convert replace_ln% to replace_ln$, pic(00)
            comp_rec% = 1%
REM        fieldnr% = 23%
        return


L50500: REM CUSTOMER PO NUMBER                    COMP_PO$
            comp_rec% = 0%
            if comp_po$ <> " " then goto L50515
               return
L50515:     read #1,key 2% = comp_po$, eod goto L50530
            gosub dataload
            comp_rec% = 1%
            fieldnr% = 23%
L50530: return

L50540: REM APC RGA NUMBER                        COMP_RGA$
            if comp_rga$ <> " " then goto L50570
/*EWD001*/     if comp_number$ = " " and security% = 0% then L50230      
               if comp_so$ <> " " then return
                  fieldnr% = 1%
                  return

L50570:     read #1,key 3% = comp_rga$, eod goto L50590
            gosub dataload
            fieldnr% = 23%
        return
L50590:    if comp_number$ = " " and security% = 0% then L50230      
           if comp_so$ <> " " then return              /*EWD001  ^ */
              fieldnr% = 1%
              return


L50615: REM CUSTOMER NUMBER                       COMP_CUSCODE$
           if str(comp_cuscode$,1%,1%) = "?" then goto L50640
           if comp_cuscode$ <> " " then goto lookup_customer
              goto L50695

L50640:       comp_cuscode$ = " "
              descr$ = hex(06) & "Select Customer Code"
              call "PLOWCODE" (#3,comp_cuscode$, descr$,0%,.30,f1%(3))
              if f1%(3) = 0 then L50695
        lookup_customer
            read #3,key = comp_cuscode$, eod goto L50705
            get #3, using L50675, cust_name$, comp_slmn$
L50675:       FMT POS(10), CH(30), POS(714), CH(04)
          comp_slmn_desc$ = " "
          call "DESCRIBE" (#15,comp_slmn$, comp_slmn_desc$, 0%, f1%(15))
          if f1%(15) <> 0 then return
L50695:      comp_slmn$ = "0000"
             comp_slmn_desc$ = " "
L50705: return

L50715: REM PART NUMBER                           COMP_PART$
            if str(comp_part$,1%,1%) = "?" then goto L50735
            if comp_part$ <> " " then goto L50760
               goto L50820
L50735:      comp_part$ = " "
             comp_part_d$ = hex(06) & "Select a Part for Complaint"
             call "GETCODE" (#11,comp_part$,comp_part_d$,0%,1.32,f1%(11))
             if f1%(11) = 0 then goto L50820

L50760:      if len(comp_part$) <> 3 then goto L50795
                readkey$ = all(hex(00))
                readkey$ = "MODEL    " & str(comp_part$,1%,3%)
                call "DESCRIBE" (#4, readkey$, comp_part_d$, 0%, f1%(4))
                if f1%(4) = 0 then comp_part_d$ = " "
                return

L50795:     call "APCLDSUB" (comp_part$, comp_part_d$, #9, err%)
            if err% <> 0% then goto L50810
        return
L50810:     comp_part_d$ = "Part Number Not in HNYMASTR File"
        return
L50820:     init(" ") comp_part$, comp_part_d$
        return

L50835: REM QUANTITY                              COMP_QUAN$
            if comp_quan$ <> " " then goto L50855
               comp_quan$ = "0001"

L50855:     convert comp_quan$ to comp_quan%, data goto L50875

            convert comp_quan% to comp_quan$, pic(0000)
        return
L50875:     errormsg$ = "(Error) - Invalid Quantity Entered?"
            init(" ") comp_quan$
        return


L50975: REM INITIATOR CODE                        COMP_INIT$
            if comp_init$ <> " " then goto L51010
               readkey$ = all(hex(00))
               readkey$ = "INIT CODE"
               descr$ =hex(06)&"Select a Valid Initiator Code (Required)"
               call "PLOWCODE" (#4, readkey$, descr$, 9%, .30, f1%(4))
               comp_init$ = str(readkey$,10%,2%)
L51010:     readkey$ = all(hex(00))
            readkey$ = "INIT CODE" & comp_init$
            call "DESCRIBE" (#4, readkey$, comp_init_desc$, 0%, f1%(4))
            if f1%(4) = 0 then goto L51035
        return
L51035:   errormsg$ = "(Error) - Invalid Initiator Code? (Required)"
          init(" ") comp_init$, comp_init_desc$
        return

L51055: REM INITIATOR DATE                        COMP_INIT_DTE$
          if comp_init_dte$ <> " " then goto L51070
             comp_init_dte$ = date
L51070:   date% = 0%
          call "DATEOK" (comp_init_dte$, date%, errormsg$)
          if errormsg$ <> " " then comp_init_dte$ = " "
                                           /* DEFAULT REMAINING FIELDS */
REM          comp_svcs$ = "00"
          comp_cost$ = "0.00"
        return


L51140: REM CSC/PSC Error Code                    COMP_SVCS$
            if comp_svcs$ <> " " then goto L51175
               readkey$ = all(hex(00))
               readkey$ = "SVCS CODE"
               descr$ =hex(06)&"Select a Valid PSC/CSC Code (Required)"
               call "PLOWCODE" (#4, readkey$, descr$, 9%, .30, f1%(4))
               comp_svcs$ = str(readkey$,10%,2%)
L51175:     readkey$ = all(hex(00))
            readkey$ = "SVCS CODE" & comp_svcs$
            call "DESCRIBE" (#4, readkey$, comp_svcs_desc$, 0%, f1%(4))
            if f1%(4) = 0 then goto L51200
        return
L51200:   errormsg$ = "(Error) - Invalid CSC/PSC Code? (Required)"
          init(" ") comp_svcs$, comp_svcs_desc$
        return


L51350: REM Editior ID                            COMP_ED_ID$
           readkey$ = all(hex(00))
           readkey$ = comp_ed_id$
           call "DESCRIBE" (#17, readkey$, " ", 0%, f1%(17))
           if f1%(17) = 0 then goto L51370          
        return
L51370:    comp_ed_id$ = "000"
        return

L51380: REM DATE ERROR OCCURRED                   COMP_ERR_DTE$
          if comp_err_dte$ <> " " then goto L51390
                                             /* (EWD006)      */
                                             /* (AWD010)      */
             comp_err_dte$ = date

             if ord_flag% = 1% then comp_err_dte$ = ship_date$

             if ord_flag% = 2% then comp_err_dte$ = ord_date$
 
                                             /* (AWD010)      */ 
L51390:   date% = 0%
                                             /* (EWD008)      */
          if comp_err_sav$ <> " " then comp_err_dte$ = comp_err_sav$

          call "DATEOK" (comp_err_dte$, date%, errormsg$)
          if errormsg$ <> " " then comp_err_dte$ = " "
        return

L51410: REM  Entry ID                             COMP_EN_ID$
           readkey$ = all(hex(00))
           readkey$ = comp_ed_id$
           call "DESCRIBE" (#17, readkey$, " ", 0%, f1%(17))
           if f1%(17) = 0 then goto L51420           
        return
L51420:    comp_en_id$ = "000"
        return

L51520: REM Proofer ID                            COMP_PR_ID$
           readkey$ = all(hex(00))
           readkey$ = comp_ed_id$
           call "DESCRIBE" (#17, readkey$, " ", 0%, f1%(17))
           if f1%(17) = 0 then goto L51530           
        return
L51530:    comp_pr_id$ = "000"
        return

L51600: REM  Window Wizard Code                   COMP_WIZD$   (EWD004)
            if comp_wizd$ <> " " then goto L51620
               readkey$ = all(hex(00))
               readkey$ = "COMP WIZD"
               descr$ =hex(06)&"Select a Valid Window Wizard Code (Required)"
               call "PLOWCODE" (#4, readkey$, descr$, 9%, .30, f1%(4))
               comp_wizd$ = str(readkey$,10%,2%)
L51620:     readkey$ = all(hex(00))
            readkey$ = "COMP WIZD" & comp_wizd$
            call "DESCRIBE" (#4, readkey$, comp_wizd_desc$, 0%, f1%(4))
            if f1%(4) = 0 then goto L51630
        return
L51630:   errormsg$ = "(Error) - Invalid Window Wizard Code? (Required)"
          init(" ") comp_wizd$, comp_wizd_desc$
        return

L51260: REM  CREDIT  CODE                         COMP_CRD$   (EWD004)
            if comp_crd$ <> " " then goto L51270
               readkey$ = all(hex(00))
               readkey$ = "TRCK CODE"
               descr$ =hex(06)&"Select a Valid Credit Code (Required)"
               call "PLOWCODE" (#4, readkey$, descr$, 9%, .30, f1%(4))
               comp_crd$ = str(readkey$,10%,2%)
L51270:     readkey$ = all(hex(00))
            readkey$ = "TRCK CODE" & comp_crd$
            call "DESCRIBE" (#4, readkey$, comp_crd_desc$, 0%, f1%(4))
            if f1%(4) = 0 then goto L51280
        return
L51280:   errormsg$ = "(Error) - Invalid Credit Code? (Required)"
          init(" ") comp_crd$, comp_crd_desc$
        return
        
L51550: REM COST OF COMPLAINT                     COMP_COST$
             comp_cost = 0.0
             convert comp_cost$ to comp_cost, data goto L51590

             comp_cost = round(comp_cost, 2)
             convert comp_cost to comp_cost$, pic(##,###.##-)

        return
L51590:      errormsg$ = "(Error) - Invalid Cost?"
             comp_cost = 0.0 : comp_cost$ = " "
        return


        deffn'152(fieldnr%)
            errormsg$ = " "
              on fieldnr% gosub L51765,         /* Report Selection  */   ~
                                L51830,         /* (S) or (D)        */   ~
                                L51950,         /* Customer Code     */   ~
                                L52005,         /* Complaint Code    */   ~
                                L52055,         /* Initiator Code    */   ~
                                L52105,         /* CSC/PSC Code      */   ~
                                L52155,         /* Credit Code       */   ~
                                L52205,         /* Wizard Code       */   ~
                                L52255,         /* Beg/End Comp. Date*/   ~
                                L52380          /* Beg/End Model Code*/
        return

L51765: REM Report Selection
            if sel$ <> " " then goto L51780
               goto L51810
L51780:     convert sel$ to sel%, data goto L51810

            convert sel% to sel$, pic(#)
            if sel% < 1% or sel% > 7% then goto L51810
               sel_desc$ = str(scr$(sel%+1%),7%,33%)
        return
L51810:     errormsg$ = "(Error) - Invalid Report Selection?"
            init(" ") sel$
        return

L51830: REM Report Type (S or D)
            if type$ <> " " then goto L51845
               type$ = "S"
L51845:     if type$ <> "S" and type$ <> "D" then goto L51865
               if type$ = "S" then type_desc$ = "Summary Report"         ~
                              else type_desc$ = "Detail Report "
        goto L51885
L51865:     errormsg$ = "(Error) - Invalid Report Type?"
            init(" ") type$, type_desc$
        return

L51885: REM SALESMAN CODE
            if str(comp_slmn$,1%,1%) = "A" then goto L51900
            if comp_slmn$ <> " " then goto L51930
L51900:        comp_slmn$ = "ALL"
               comp_slmn_desc$ = " "
               return
            convert comp_slmn$ to comp_slmn%, data goto L51900

            convert comp_slmn% to comp_slmn$, pic(0000)
L51930:     call "DESCRIBE" (#15, comp_slmn$, comp_slmn_desc$,0%,f1%(15))
            if f1%(15) = 0 then goto L51900
        return

L51950: REM Customer Code
            if comp_cuscode$ <> " " then goto L51965
               comp_cuscode$ = "ALL"
L51965:     if len(comp_cuscode$) < 4 and str(comp_cuscode$,1%,1%) = "A" ~
                                              then goto L51985

           if str(comp_cuscode$,1%,1%) = "?" then goto L51970
           if comp_cuscode$ <> " " then goto lookup_customer_rpt
              return
L51970:       comp_cuscode$ = " "
              descr$ = hex(06) & "Select Customer Code"
              call "PLOWCODE" (#3,comp_cuscode$, descr$,0%,.30,f1%(3))
              if f1%(3) = 0 then L50695
        lookup_customer_rpt
            read #3,key = comp_cuscode$, eod goto L51980
            get #3, using L51975, cust_name$
L51975:       FMT POS(10), CH(30)
L51980: return
L51985:     comp_cuscode$ = "ALL"
            cust_name$ = "All Customers"
        return

L52005: REM Complaint Code
            rpt% = 1%
            if len(comp_code$) = 1 and str(comp_code$,1%,1%) = "A"       ~
                                                          then goto L52035
               gosub L50895
               rpt% = 0%
               if f1%(4%) = 0 then goto L52035
        return
L52035:     comp_code$ = "ALL"
            comp_code_desc$ = "All Complaint Codes."
        return

L52055: REM Initiator Code
            if len(comp_init$) = 1 and str(comp_init$,1%,1%) = "A"       ~
                                                          then goto L52085
               gosub L50975
               if f1%(4%) = 0 then goto L52085
        return
L52085:     comp_init$ = "A "
            comp_init_desc$ = "All Initiator Codes."
        return

L52105: REM CSC/PSC Code
            if len(comp_svcs$) = 1 and str(comp_svcs$,1%,1%) = "A"       ~
                                                          then goto L52135
               gosub L51140
               if f1%(4%) = 0 then goto L52135
        return
L52135:     comp_svcs$ = "A "
            comp_svcs_desc$ = "All CSC/PSC Codes."
        return

L52155: REM Credit Code                      /*  (EWD004)  */
            if len(comp_crd$) = 1 and str(comp_crd$,1%,1%) = "A"         ~
                                                          then goto L52185
               gosub L51260
               if f1%(4%) = 0 then goto L52185
        return
L52185:     comp_crd$ = "A "
            comp_crd_desc$ = "All Credit Codes."         
        return

L52205: REM Wizard Code
            if len(comp_wizd$) = 1 and str(comp_wizd$,1%,1%) = "A"       ~
                                                          then goto L52235
               gosub L51600
               if f1%(4%) = 0 then goto L52235
        return
L52235:     comp_wizd$ = "ALL"
            comp_wizd_desc$ = "All Wizard Codes."
        return

L52255: REM Beginning and Ending Date
            if str(beg_date$,1%,1%) = "A" then goto L52270
            if beg_date$ <> " " then goto L52280
L52270:        beg_date$, end_date$ = "ALL" & "     "
               return
L52280:     if end_date$ <> " " then goto L52295
               end_date$ = beg_date$

L52295:     call "DATEOKC" (beg_date$, date%, errormsg$)
            if date% <> 0% then goto L52315
               errormsg$ = "(Error) - Invalid Beginning Date?"
               goto L52365
L52315:     call "DATEOKC" (end_date$, date%, errormsg$)
            if date% <> 0% then goto L52335
               errormsg$ = "(Error) - Invalid Ending Date?"
               goto L52365
L52335:     beg_dte$ = beg_date$
            end_dte$ = end_date$
            call "DATUFMTC" (beg_dte$)
            call "DATUFMTC" (end_dte$)
            if end_dte$ >= beg_dte$ then return
               errormsg$ = "(Error) - Invalid Date Range?"
L52365:        beg_date$, end_date$ = blankdate$
        return

L52380: REM Beginning and Ending Model Code
            if str(beg_mod$,1%,1%) = "A" then goto L52395
            if beg_mod$ <> " " then goto L52405
L52395:        beg_mod$, end_mod$ = "ALL"
               return
L52405:     if end_mod$ <> " " then goto L52420
               end_mod$ = beg_mod$

L52420:     if str(beg_mod$,1%,1%) = "A" then goto L52395
               init(" ") readkey$
               str(readkey$,1%,9%)   = "MODEL    "
               str(readkey$,10%,15%) = beg_mod$
               read #4,key = readkey$, eod goto L52465

               str(readkey$,10%,15%) = end_mod$
               read #4,key = readkey$, eod goto L52465
               if beg_mod$ > end_mod$ then goto L52465
        return
L52465:     errormsg$ = "(Error) - Invalid Beg/End Model Code?"
            init(" ") beg_mod$, end_mod$, readkey$
        return

        lookup_model
            init(" ") readkey$, comp_mod$, comp_mod_d$
            comp_mod_d$ = "MFG Product N/A ?"
            comp_mod$ = str(comp_part$,1%,3%)
            str(readkey$,1%,9%)   = "MODEL    "
            str(readkey$,10%,15%) = comp_mod$
            read #4,key = readkey$, using L52525, comp_mod_d$,            ~
                                                 eod goto L52530
L52525:        FMT POS(25), CH(30)
L52530: return



        deffn'153(fieldnr%)
            errormsg$ = " "
              on fieldnr% gosub L50895,         /* Complaint Code    */   ~
                                L51110,         /* Complaint Text    */   ~
                                L53000,         /* Column 1          */   ~
                                L53100,         /* Column 2          */   ~
                                L53200,         /* Column 3          */   ~
                                L53300,         /* Column 4          */   ~
                                L53400,         /* Column 5          */   ~
                                L53500,         /* Column 6          */   ~
                                L53600          /* Column 7          */   
                   return


L50895: REM COMPLAINT CODE                        COMP_CODE$
            if comp_code$ <> " " then goto L50930
               readkey$ = all(hex(00))
               readkey$ = "COMPLAINT"
               descr$ =hex(06)&"Select a Valid Complaint Code (Required)"
               call "PLOWCODE" (#4, readkey$, descr$, 9%, .30, f1%(4))
               comp_code$ = str(readkey$,10%,3%)
L50930:     readkey$ = all(hex(00))
            readkey$ = "COMPLAINT" & comp_code$
            call "DESCRIBE" (#4, readkey$, comp_code_desc$, 0%, f1%(4))
            if f1%(4) = 0 then goto L50955
                                                /* Check to see if not */
                                                /* to be used          */
            if rpt% = 1% then return
            readkey$ = all(hex(00))
            readkey$ = "COMPNOUSD" & comp_code$
            call "DESCRIBE" (#4, readkey$, " ", 0%, f1%(4))
            if f1%(4) = 1 then goto L50960

REM            if edit% = 2% then gosub check_fields
        return
L50955:   errormsg$ = "(Error) - Invalid Complaint Code? (Required)"
          init(" ") comp_code$, comp_code_desc$
        return
L50960:   errormsg$ = "(Error) - Invalid Complaint Code? This one is not used Anymore!  (Required)"
          init(" ") comp_code$, comp_code_desc$
        return

L51110: REM INITIATOR TEXT FLAG                   COMP_INIT_TXT$
             gosub'099(comp_init_txt$)
             comp_init_txt1$ = "No "
             if txt% = 1% then comp_init_txt1$ = "Yes"
REM            if edit% = 2% then gosub check_fields
        return

L53000: rem  Column 1                                    col1$, col1_desc$
/* (AWD014) */
             if col1$ = " " and comp_code$ <> "117" and  ~
                          comp_code$ <> "133" then col1$ = "Z"
/* <AWD013> */
             col_no$ = "1"
             if comp_code$ = "117" and col1$ = "Z" then goto col_err
             if comp_code$ = "133" and col1$ = "Z" then goto col_err
/* </AWD013> */
             if col1$ <> "Z" and col1$ <> "Y" then goto L53040
                    if col1$ = "Z" then col1$       = "Z"
                    if col1$ = "Z" then col1_desc$  = "NA"
                    if col1$ = "Y" then col1$  = "Y"
                    if col1$ = "Y" then col1_desc$  = "MULTIPLE COMPLAINTS"
                   
                    return
L53040:
             col_no$ = "1"
             f% = 18%
             init(" ") col_key$
             str(col_key$,1%,3%) = comp_code$ 
             str(col_key$,4%,1%) = col1$

             col_len% = 3%
             if col1$ = " " then gosub plow_desc
             if col1$ <> " " then goto L53020
                if f1%(f%) = 0% then goto L53020
                   col1$      = str(col_key$,4%,1%)
                   col1_desc$ = col_desc$
                   return
L53020:
             gosub validate_col
                if col% = 0% then goto col_err
                get #f%, using L53010, col1_desc$
L53010:            FMT POS(05), CH(60)
REM            if edit% = 2% then gosub check_fields
        return

L53100: rem  Column 2                                    col2$, col2_desc$
/* (AWD014) */
             if col2$ = " " and comp_code$ <> "117" and  ~
                          comp_code$ <> "133" then col2$ = "Z"
/* <AWD013> */
             col_no$ = "2"
             if comp_code$ = "117" and col2$ = "Z" then goto col_err
             if comp_code$ = "133" and col2$ = "Z" then goto col_err
/* </AWD013> */
REM             if col2$ <> "Z" and col2$ <> "Y" then goto L53140
             if col2$ <> "Z" then goto L53140
                    if col2$ = "Z" then col2$       = "Z"
                    if col2$ = "Z" then col2_desc$  = "NA"
REM                    if col2$ = "Y" then col2$  = "Y"
REM                    if col2$ = "Y" then col2_desc$  = "MULTIPLE COMPLAINTS"
                    return
L53140:
             col_no$ = "2"
             f% = 19%
             init(" ") col_key$
             str(col_key$,1%,3%) = comp_code$ 
             str(col_key$,4%,1%) = col1$
             str(col_key$,5%,1%) = col2$

             col_len% = 4%
             if col2$ = " " then gosub plow_desc
             if col2$ <> " " then goto L53120
                if f1%(f%) = 0% then goto L53120
                   col2$      = str(col_key$,5%,1%)
                   col2_desc$ = col_desc$
                   return
L53120:
             gosub validate_col
                if col% = 0% then goto col_err
                get #f%, using L53110, col2_desc$
L53110:            FMT POS(6), CH(60)


REM            if edit% = 2% then gosub check_fields
        return

L53200: rem  Column 3                                    col3$, col3_desc$
/* (AWD014) */
             if col3$ = " " and comp_code$ <> "117" and  ~
                          comp_code$ <> "133" then col3$ = "Z"

             if col3$ <> "Z" then goto L53240
                    col3$       = "Z"
                    col3_desc$  = "NA"
                    return
L53240:
             col_no$ = "3"
             f% = 20%
             init(" ") col_key$
             str(col_key$,1%,3%) = comp_code$ 
             str(col_key$,4%,1%) = col1$
             str(col_key$,5%,1%) = col2$
             str(col_key$,6%,1%) = col3$

             col_len% = 5%
             if col3$ = " " then gosub plow_desc
             if col3$ <> " " then goto L53220
                if f1%(f%) = 0% then goto L53220
                   col3$      = str(col_key$,6%,1%)
                   col3_desc$ = col_desc$
                   return
L53220:
             gosub validate_col
                if col% = 0% then goto col_err
                get #f%, using L53210, col3_desc$
L53210:            FMT POS(7), CH(60)


REM            if edit% = 2% then gosub check_fields
        return

L53300: rem  Column 4                                    col4$, col4_desc$
/* (AWD014) */
             if col4$ = " " and comp_code$ <> "117" and  ~
                          comp_code$ <> "133" then col4$ = "Z"
             if col4$ <> "Z" then goto L53310
                    col4$       = "Z"
                    col4_desc$  = "NA"
                    return
L53310:
             col_no$ = "4"
             f% = 21%
             init(" ") col_key$
             str(col_key$,1%,3%) = comp_code$ 
             str(col_key$,4%,1%) = col1$
             str(col_key$,5%,1%) = col2$
             str(col_key$,6%,1%) = col4$


             col_len% = 5%
             if col4$ = " " then gosub plow_desc
             if col4$ <> " " then goto L53320
                if f1%(f%) = 0% then goto L53320
                   col4$      = str(col_key$,6%,1%)
                   col4_desc$ = col_desc$
                   return
L53320:
             gosub validate_col
                if col% = 0% then goto col_err
                get #f%, using L53210, col4_desc$

REM            if edit% = 2% then gosub check_fields

        return


L53400: rem  Column 5                                    col5$, col5_desc$
/* (AWD014) */
             if col5$ = " " and comp_code$ <> "117" and  ~
                          comp_code$ <> "133" then col5$ = "Z"
             if col5$ <> "Z" then goto L53410
                    col5$       = "Z"
                    col5_desc$  = "NA"
                    return
L53410:
             col_no$ = "5"
             f% = 22%
             init(" ") col_key$
             str(col_key$,1%,3%) = comp_code$ 
             str(col_key$,4%,1%) = col1$
             str(col_key$,5%,1%) = col2$
             str(col_key$,6%,1%) = col5$


             col_len% = 5%
             if col5$ = " " then gosub plow_desc
             if col5$ <> " " then goto L53420
                if f1%(f%) = 0% then goto L53420
                   col5$      = str(col_key$,6%,1%)
                   col5_desc$ = col_desc$
                   return
L53420:
             gosub validate_col
                if col% = 0% then goto col_err
                get #f%, using L53210, col5_desc$

REM            if edit% = 2% then gosub check_fields

        return        


L53500: rem  Column 6                                    col6$, col6_desc$
/* (AWD014) */
             if col6$ = " " and comp_code$ <> "117" and  ~
                          comp_code$ <> "133" then col6$ = "Z"
             if col6$ <> "Z" then goto L53510
                    col6$       = "Z"
                    col6_desc$  = "NA"
                    return
L53510:
             col_no$ = "6"
             f% = 23%
             init(" ") col_key$
             str(col_key$,1%,3%) = comp_code$ 
             str(col_key$,4%,1%) = col1$
             str(col_key$,5%,1%) = col2$
             str(col_key$,6%,1%) = col6$


             col_len% = 5%
             if col6$ = " " then gosub plow_desc
             if col6$ <> " " then goto L53520
                if f1%(f%) = 0% then goto L53520
                   col6$      = str(col_key$,6%,1%)
                   col6_desc$ = col_desc$
                   return
L53520:
             gosub validate_col
                if col% = 0% then goto col_err
                get #f%, using L53210, col6_desc$

REM            if edit% = 2% then gosub check_fields

        return        

L53600: rem  Column 7                                    col7$, col7_desc$
/* (AWD014) */
             if col7$ = " " and comp_code$ <> "117" and  ~
                          comp_code$ <> "133" then col7$ = "Z"
             if col7$ <> "Z" then goto L53610
                    col7$       = "Z"
                    col7_desc$  = "NA"
                    return
L53610:
             col_no$ = "7"
             f% = 24%
             init(" ") col_key$
             str(col_key$,1%,3%) = comp_code$ 
             str(col_key$,4%,1%) = col1$
             str(col_key$,5%,1%) = col2$
             str(col_key$,6%,1%) = col7$


             col_len% = 5%
             if col7$ = " " then gosub plow_desc
             if col7$ <> " " then goto L53620
                if f1%(f%) = 0% then goto L53620
                   col7$      = str(col_key$,6%,1%)
                   col7_desc$ = col_desc$
                   return
L53620:
             gosub validate_col
                if col% = 0% then goto col_err
                get #f%, using L53210, col7_desc$

REM            if edit% = 2% then gosub check_fields

        return        

        col_err
             errormsg$ = "(ERROR) - Invaild Value for column " & col_no$
        return

        plow_desc
           call "PLOWCODE" (#f%, col_key$, col_desc$, col_len%, .60, f1%(f%))
        return
        validate_col
             col% = 0%
             read #f%, key = col_key$, eod goto no_col
                  col% = 1%
        no_col
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************
                                                             /* HEADER */
L55040: %  ######## @ ########    ###################################    ~
        ~     Page: ###

L55070: %  Complaint Tracking ##############  Beg: ##########  End: ##########


L55100: %  +-------------------------------------------------------------~
        ~-------------+

L55130: %  Slmn    :#### ##############################

L55150: %  Model    :### ##############################

L55170: %  Cust  :###### ##############################  ################~
        ~######### ####
L55190: %  Complaint:### ##############################  Cost: ##########

L55210: %  Initiator:##  ##############################  Date: ########  ~
        ~Text: ###
L55230: %  CSC/PSC  :##  ##############################  Date: ########
L55250: %  Credit :##  ##############################Entry: ###   Edi~
        ~t: ###
/*L55270: %  Window Wizard :##  ##############################  Proofer: ###*/
L55270: %  Window Wizard :##  ##############################  Mfg Plant: #
L55290: %  Comp No. :######## S.O.:######## LINE:## PO:################ RGA:~
        ~########      

L55320: %  +-------------------------- #################### -------------~
        ~-------------+

L55420: %  Total Qty : ##########

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_header
          if page_no% > pages% then goto generate_done       /* (EWD003) */
          page_no% = page_no% + 1%
          print page
          print using L55040, date$, rpt_time$, print_title$, page_no%
          print using L55070, type_desc$, beg_date$, end_date$
          print using L55100
          lcnt% = 3%
        return

        pt_1
          print using L55170, str(comp_cuscode$,1%,6%), cust_name$,       ~
                                                   comp_part$, comp_quan$
          lcnt% = lcnt% + 1%
          return

        pt_2
          print using L55190, comp_code$, comp_code_desc$, comp_cost$
          lcnt% = lcnt% + 1%
          return

        pt_3
          print using L55210, comp_init$, comp_init_desc$, comp_init_dte$,~
                                                         comp_init_txt1$
          lcnt% = lcnt% + 1%
          return

        pt_4
          print using L55230, comp_svcs$, comp_svcs_desc$, comp_err_dte$
          lcnt% = lcnt% + 1%
          return

        pt_5
          print using L55250, comp_crd$, comp_crd_desc$, comp_en_id$, comp_ed_id$
          lcnt% = lcnt% + 1%
          return

        pt_6
          /* print using L55270, comp_wizd$, comp_wizd_desc$, comp_pr_id$ */
          print using L55270, comp_wizd$, comp_wizd_desc$, comp_mfg_plant$
          lcnt% = lcnt% + 1%
          return

        pt_7
          print using L55290, comp_number$, comp_so$, comp_line$,         ~
                                                      comp_po$, comp_rga$
          lcnt% = lcnt% + 1%
          return

        pt_8
          print using L55130, comp_slmn$, comp_slmn_desc$
          lcnt% = lcnt% + 1%
          return

        pt_9
          print using L55150, comp_mod$, comp_mod_d$
          lcnt% = lcnt% + 1%
          return

        pt_tot
          print using L55420, tot_quan%
          lcnt% = lcnt% + 1%
          return


        pt_line
          print using L55100
          lcnt% = lcnt% + 1%
        return

        print_text
          str(mask$,3%,1%)  = "!"
          str(mask$,78%,1%) = "!"
          if str(comp_init_txt1$,1%,1%) = "N" then return
             txt_1$  = "** Initiator Text **"
             textid$ = comp_init_txt$
             gosub print_text_next
          return
          
REM L60760    if str(comp_svcs_txt1$,1%,1%) = "N" then goto L60800
REM              txt_1$  = "*** Service Text ***"
REM              textid$ = comp_svcs_txt$
REM              gosub print_text_next
REM L60800    if str(comp_trk_txt1$,1%,1%) = "N" then goto L60840
REM              txt_1$  = "** Tracking  Text **"
REM              textid$ = comp_trk_txt$
REM              gosub print_text_next
REM L60840    if str(comp_srce_txt1$,1%,1%) = "N" then return
REM              txt_1$  = "*** Source  Text ***"
REM              textid$ = comp_srce_txt$
REM              gosub print_text_next
REM         return


        print_text_next
            print using L55320, txt_1$
            lcnt% = lcnt% + 1%
REM            status% = 0%         Take out b/c made it look like starting
REM                                 on first page everytime.  (EWD002)   

            call "TXTPRINT" (#16, f2%(16%),      /* TXTFILE            */~
                             80%,                /* PRINTER LINE WIDTH */~
                             textid$,            /* TEXTID TO PRINT    */~
                             " ",                /* PRINT FLAGS TO INCL*/~
                             5%,                 /* CLMN TO PRINT TEXT */~
                             lcnt%,              /* LINE COUNTER       */~
                             58%,                /* LINES PER PAGE     */~
                             " ",                /* DO NOT PRINT HEADER*/~
                             mask$,              /* LINE MASK          */~
                             status%)            /* FINISHED,IN-PROCESS*/
            if status% = 0% then goto L61070
            gosub print_header
            goto print_text_next
L61070: return

        select_printer
            page_no% = 0%
            lcnt%    = 99%
            print_title$ = sel_desc$
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCCOM", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCCOM", " ", 0%, 1%)
        return

        generate_report
            tot_quan% = 0%
            gosub get_pages                          /* (EWD003) */
            call "SHOSTAT" ("Printing Report")
            gosub select_printer      :      dataload% = 0%
            wrk_key1$ = all(hex(00))
            read #10,key 1% > wrk_key1$, using L61330, comp_number$,      ~
                                                   eod goto generate_done
            goto L61340
        generate_next
            read #10, using L61330, comp_number$, eod goto generate_done
L61330:        FMT POS(28), CH(5)
L61340:     str(comp_key$,1%,4%) = str(comp_number$,1%,4%)  /*  (EWD005)  */
            str(comp_key$,5%,1%) = " "
            read #1,key = comp_key$, eod goto generate_next
            dataload% = 1%
            gosub dataload
            dataload% = 0%

            convert comp_quan$ to comp_quan%, data goto bad_qty

bad_qty:

            tot_quan% = tot_quan% + comp_quan%
            if lcnt% > 58% then gosub print_header
            on sel% gosub gen_1, gen_2, gen_3, gen_4, gen_5, gen_6, gen_7
            if type$ = "S" then goto L61400
               status% = 0%           /* Only set when first start TXTPRINT */
               gosub print_text       /* (EWD002) */
L61400:     gosub pt_line
            goto generate_next
        gen_1
            gosub pt_2                                    /* COMPLAINT */
            gosub pt_1                                    /* CUSTOMER  */
            gosub pt_3                                    /* INITIATOR */
            gosub pt_4                                    /* CSC/PSC   */
            gosub pt_5                                    /* Credit (EWD004) */
            gosub pt_6                                    /* Wizard    */
            gosub pt_7                                    /* COMP NO.  */
        return

        gen_2
            gosub pt_1                                    /* CUSTOMER  */
            gosub pt_2                                    /* COMPLAINT */
            gosub pt_3                                    /* INITIATOR */
            gosub pt_4                                    /* CSC/PSC   */
            gosub pt_5                                    /* Credit  (EWD004) */
            gosub pt_6                                    /* Wizard    */
            gosub pt_7                                    /* COMP NO.  */
        return

        gen_3
            gosub pt_6                                    /* SOURCE    */
            gosub pt_2                                    /* COMPLAINT */
            gosub pt_1                                    /* CUSTOMER  */
            gosub pt_3                                    /* INITIATOR */
            gosub pt_4                                    /* CSC/PSC   */
            gosub pt_5                                    /* Credit (EWD004)   */
            gosub pt_7                                    /* COMP NO.  */
        return

        gen_4
            gosub pt_1                                    /* CUSTOMER  */
            gosub pt_6                                    /* SOURCE    */
            gosub pt_2                                    /* COMPLAINT */
            gosub pt_3                                    /* INITIATOR */
            gosub pt_4                                    /* CSC/PSC   */
            gosub pt_5                                    /* Credit (EWD004)   */
            gosub pt_7                                    /* COMP NO.  */
        return

        gen_5
            gosub pt_3                                    /* INITIATOR */
            gosub pt_1                                    /* CUSTOMER  */
            gosub pt_2                                    /* COMPLAINT */
            gosub pt_4                                    /* SERVICE   */
            gosub pt_5                                    /* Credit (EWD004)   */
            gosub pt_6                                    /* Wizard    */
            gosub pt_7                                    /* COMP NO.  */
        return

        gen_6
            gosub pt_8                                    /* Salesman  */
            gosub pt_1                                    /* CUSTOMER  */
            gosub pt_2                                    /* COMPLAINT */
            gosub pt_3                                    /* INITIATOR */
            gosub pt_4                                    /* CSC/PSC   */
            gosub pt_5                                    /* Credit (EWD004)   */
            gosub pt_6                                    /* Wizard    */
            gosub pt_7                                    /* COMP NO.  */
        return

        gen_7
            gosub pt_9                                    /* Model Code*/
            gosub pt_2                                    /* COMPLAINT */
            gosub pt_1                                    /* CUSTOMER  */
            gosub pt_3                                    /* INITIATOR */
            gosub pt_4                                    /* CSC/PSC   */
            gosub pt_5                                    /* Credit (EWD004)   */
            gosub pt_6                                    /* Wizard    */
            gosub pt_7                                    /* COMP NO.  */
        return

        gen_tot
            gosub pt_tot
        return

        generate_done
            gosub gen_tot
            gosub close_printer
        return

        edit_init_text
            gosub'099(comp_init_txt$)
            if txt% = 0% then goto L62230
               call "TXTFUTIL" (#16, f2%(16%), "LOAD", comp_init_txt$)

L62230:     header$ = "Edit Initiator TEXT for " & comp_init_desc$ 
/*EWD001*/  if security% > 0% then                                         ~
            call "TXTINSUB" (#16, f2%(16%), "012", header$, comp_init_txt$,~
                                                         text$() )
/*EWD001*/  str(header$,,4%) = "View"
/*   |  */  if security% = 0% then                                         ~
/*   |  */  call "TXTDSPLY" (#16, f2%(16%), "012", header$, comp_init_txt$,~
/*EWD001*/                                               text$() )
            gosub L51110
            if edit% <> 2% then fieldnr% = fieldnr% + 1%
            if txt% = 0% then return
               call "TXTFUTIL" (#16, f2%(16%), "SAV2", comp_init_txt$)
        return

        deffn'099(txt$)
            txt% = 0%
            if txt$ = hex(00000000) or txt$ = hex(ffffffff)              ~
                                              or txt$ = " " then return
            txt% = 1%
        return

        delete_complaint
* EWD001   if userid$ <> "RHH" and userid$ <> "DKB" and userid$ <> "CDC"  ~
* EWD001     and userid$ <> "DJD"             then gosub check_security
/*EWD001*/ if security% <> 2% then return   /* For safety... */

            call "SHOSTAT" ("DELETING COMPLAINT ("&comp_number$&")")
            gosub pack_comp_number                /*  (EWD005)  */
            read #1,hold,key = comp_key$, eod goto L62920
            delete #1
            call "TXTFUTIL" (#16, f2%(16%), "DELE", comp_init_txt$)
REM         call "TXTFUTIL" (#16, f2%(16%), "DELE", comp_svcs_txt$)
REM         call "TXTFUTIL" (#16, f2%(16%), "DELE", comp_crd_txt$)
REM         call "TXTFUTIL" (#16, f2%(16%), "DELE", comp_srce_txt$)
L62920: return clear all
        goto inputmode

/* <AWD012> */
lookup_replacement:
            bck_key$ = " "
            str(bck_key$,1%,9%) = replace_so$
            if replace_ln$ <> " " then goto L62922
            read #7,key > bck_key$, using L62921, comp_cuscode$,       ~
                          bck_key$,  eod goto L62929
L62921:           FMT CH(9), CH(19)
            if str(bck_key$,1%,9%) <> replace_so$ then goto L62929
            goto L62928

L62922:     convert replace_ln% to str(bck_key$,17%,3%), pic(###)
            read #7,key = bck_key$, using L62921, comp_cuscode$,          ~
                              bck_key$,                                   ~
                              eod goto L62929
L62928:
	    return
L62929:
            replace_so$ = " "
	    replace_ln$ = " "
            errormsg$ = "(Error) - Invalid Replacement Sales Order Number?"
	    return

/* </AWD012> */

        lookup_order
                                                     /* (AWD010)        */
            bck_key$ = " "
            str(bck_key$,1%,9%) = comp_so$
            if comp_line$ <> " " then goto L63030
               read #7,key > bck_key$, using L63010, comp_cuscode$,       ~
                          bck_key$, str(ship_date$,1%,6%), eod goto L63160
L63010:           FMT CH(9), CH(19), POS(212), CH(6)

               goto L63080
L63030:     convert comp_line% to str(bck_key$,17%,3%), pic(###)
                                                /* (EWD006) - Ship Date */ 
            read #7,key = bck_key$, using L63060, comp_cuscode$,          ~
                              bck_key$, comp_part$, str(ship_date$,1%,6%),~
                              eod goto L63160
L63060:        FMT CH(9), CH(19), XX(3), CH(25), POS(212), CH(6)


               so_inv$  = str(bck_key$,1%, 8%)
               item_no$ = str(bck_key$,17%,3%)
               gosub lookup_subpart                /* PAR000 */
               comp_subp$ = str(bcksubpt_rec$,48%,20%)

               gosub L50715                         /* PART DESCRIPTION */
L63080:     

            if str(bck_key$,1%,8%) <> comp_so$ then ship_date$ = " "

            if str(bck_key$,1%,8%) <> comp_so$ then goto L63170
               gosub L50615                         /* CUSTOMER NAME    */
                                                    /* (AWD010)         */
               ord_flag% = 1%
               comp_err_sav$ = ship_date$
                                                    /* (AWD010)         */

            call "DATEFMT" (ship_date$)      /* Set if Header not Found */
            comp_err_dte$ = ship_date$       /* Use Line Item Ship Date */
            comp_err_sav$ = ship_date$       /* (EWD008) Save Date      */
                                                    /* (EWD006)         */
               init(" ") bck_key$
               str(bck_key$,1%,9%)   = comp_cuscode$
               str(bck_key$,10%,16%) = comp_so$ & " "
            read #8,key = bck_key$, using L63150, comp_po$, str(ord_date$,1%,6%),~
                                                  eod goto L63160
L63150:        FMT POS(26), CH(16), POS(806), CH(6)
                                                   /* (AWD010)         */ 
            ord_flag% = ord_flag% + 1%
            comp_err_sav$ = ord_date$
                                                   /* (AWD010)         */

            call "DATEFMT" (ord_date$)
            comp_err_dte$ = ord_date$
            comp_err_sav$ = ord_date$        /* (EWD008) Save Date     */

            return
L63160: 
                                                    /* (EWD007)          */
            gosub lookup_history
 
                                                    /* (EWD007)          */
        return
L63170: 
           gosub lookup_history
           if check% = 1% then return
                                                    /* (EWD007)          */
          init(" ") comp_cuscode$, comp_po$, comp_part$, comp_part_d$

                                                    /* (AWD010)          */
          if ord_flag% = 0% then init(" ") ord_date$, ship_date$,          ~
                                           comp_err_dte$, comp_err_sav$
          if ord_flag% = 1% then comp_err_dte$, comp_err_sav$ = ship_date$
          if ord_flag% = 2% then comp_err_dte$, comp_err_sav$ = ord_date$
                                                   /* (AWD010)           */
        return
                                                   /* (EWD006)           */
/* (EWD003) Get the max number of pages to print */
        get_pages
            pages% = 300        :  init(" ") pages$
            readkey$ = all(hex(00))
            readkey$ = "CSECURITY" & "..."   
            call "DESCRIBE" (#4, readkey$, pages$, 0%, f1%(4))
            if f1%(4%) = 0% then goto L63180

            convert str(pages$,28%,3%) to pages%,               ~
                data goto L63180

L63180:     
        return
/* (EWD003) -End */

        check_security
            comp% = 2%
            hdr$ = "******** Access Denied ********"
            msg$(1) = "Currently 'You' do not have Access To Selection!"
            msg$(2) = "           A c c e s s   D e n i e d            "
            msg$(3) = "       Press <RETURN> To Continue !!!!          "
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return clear all
        goto exit_program

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
        
        open_error                                                            
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
            err% = 0%
        return

        pack_comp_number                    /*  (EWD005)   */
            convert comp_number$ to comp_number%, data goto L63500
L63500:

            put str(comp_number$,1%,4%), using L63510, comp_number%
            str(comp_number$,5%,1%) = " "
            str(comp_key$,1%,5%)    = str(comp_number$,1%,5%)
        return

        unpack_comp_number
            get str(comp_number$,1%,4%), using L63510, comp_number%
L63510:             FMT BI(4)
            convert comp_number% to comp_number$, pic(00000000)

        return                              /*  (EWD005)  */

                                            /* (EWD007)                */
        lookup_history
            check% = 0%
            init(" ") hs_key1$, tm_so$, tm_ln$, tm_part$, tm_cust$,      ~
                      tm_order$, tm_po$, comp_po$, comp_part$, comp_part_d$,~
                      comp_cuscode$
            init(" ") tx_so$, tx_ln$, tx_part$, tx_cust$,      ~
                      tx_order$, tx_po$

            hs_ln% = 1% 
            hs_ln$ = comp_line$
            convert hs_ln$ to hs_ln%, data goto L63550
L63550:
            convert hs_ln% to hs_ln$, pic(###)
            test_fl = 0
            str(hs_key1$,1%,8%) = comp_so$              /* Sales Order */
            str(hs_key1$,9%,3%) = hs_ln$                /* S.O. Line   */
            read #2,key 1% > hs_key1$, using L63600, tm_po$, tm_so$,     ~
                                       tm_ln$, tm_cust$, tm_order$,      ~
                                       tm_part$, eod goto L63602
/* <AWD013> */
REM                                    hs_part$, eod goto L63610
L63600:        FMT POS(26), CH(16), POS(54), CH(8), CH(3), POS(77),      ~
                   CH(9), CH(6), POS(119), CH(25)

REM         if str(hs_key1$,1%,8%) <> hs_so$ then goto L63610
REM         if str(hs_key1$,9%,3%) <> hs_ln$ then goto L63605
            if str(hs_key1$,1%,8%) <> tm_so$ then goto L63602
            if str(hs_key1$,9%,3%) <> tm_ln$ then goto L63601

L63606:
	    hs_po$    = tm_po$
	    hs_so$    = tm_so$
	    hs_ln$    = tm_ln$
	    hs_cust$  = tm_cust$
	    hs_order$ = tm_order$
	    hs_part$  = tm_part$
	    goto L63604
L63601:
	    test_fl = 1

L63602:
            read #42,key 1% > hs_key1$, using L63600, tx_po$, tx_so$,     ~
                                       tx_ln$, tx_cust$, tx_order$,      ~
                                       tx_part$, eod goto L63610

REM            if str(hs_key1$,1%,8%) <> tx_so$ then goto L63604   
       
            if str(hs_key1$,1%,8%) <> tx_so$ then goto L63610
            if str(hs_key1$,9%,3%) <> tx_ln$ then goto L63610

            if str(hs_key1$,9%,3%) = tx_ln$ then goto L63603           
            if test_fl = 1                  then goto L63604
	    goto L63610
L63603:
            test_fl = 0
	    hs_po$    = tx_po$
	    hs_so$    = tx_so$
	    hs_ln$    = tx_ln$
	    hs_cust$  = tx_cust$
	    hs_order$ = tx_order$
	    hs_part$  = tx_part$

L63604:

/* </AWD013> */
            comp_part$    = hs_part$


            gosub L50715                    /* Part Description        */

L63605:
                                                       /* (AWD010)     */
            ord_flag%  = 1%
            ship_date$ = hs_order$
                                                       /* (AWD010)     */
            comp_po$ = hs_po$
            comp_cuscode$ = hs_cust$
            str(ord_date$,1%,6%) = hs_order$

            gosub L50615                    /* Customer Name           */  

            call "DATEFMT" (ord_date$)
            comp_err_dte$ = ord_date$
            comp_err_sav$ = ord_date$       /* (EWD008) save Date      */
 
            check% = 1%
L63610:
        return
                                            /* (EWD007)                */

        lookup_subpart                            /* PAR000  */
            init(" ") bcksubpt_rec$, flds$(), info_flds$()
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
                          err1%)         /* Error Code                 */
           
/* AWD011 */ 
           str(part_info$,1,1) = info_flds$(1)
           str(part_info$,2,1) = info_flds$(2)
           str(part_info$,3,1) = info_flds$(3)
           str(part_info$,4,1) = info_flds$(4)
           str(part_info$,5,1) = info_flds$(5)
           str(part_info$,6,1) = info_flds$(6)
           str(part_info$,7,1) = info_flds$(7)
           str(part_info$,8,2) = info_flds$(8)
            comp_mfg_plant$ = str(part_info$,7,1)
	   /* bytes 10-20 not used yet */
REM        str(part_info$,10,1) = info_flds$(9)
REM        str(part_info$,11,1) = info_flds$(10)
REM        str(part_info$,12,1) = info_flds$(11)
REM        str(part_info$,13,1) = info_flds$(12)
REM        str(part_info$,14,1) = info_flds$(13)
REM        str(part_info$,15,1) = info_flds$(14)
REM        str(part_info$,16,1) = info_flds$(15)
REM        str(part_info$,17,1) = info_flds$(16)
REM        str(part_info$,18,1) = info_flds$(17)
REM        str(part_info$,19,1) = info_flds$(18)
REM        str(part_info$,20,1) = info_flds$(19)

            if err1% <> 0% then                                          ~
                   str(bcksubpt_rec$,48%,20%) = "00000                    "

            return
            if err1% = 0% then return

            errormsg$ = "BCKSUBPT ERR= "&so_inv$ & " Line= " & item_no$  ~
                                      & " Flag= " & flag$


        return                                    /* PAR000 */

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
