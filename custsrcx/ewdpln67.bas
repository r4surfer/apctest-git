        REM *************************************************************~
            *                                                           *~
            *  Note - The Stock Sales Order for (1999) were moved. When *~
            *         1998 History was moved the Stock for 1999 was     *~
            *         moved because we re-initialized Stock S.O. and    *~
            *         reset to S0000001 for starting (2000).            *~
            *                                                           *~
            *  Note (2) - When all History has been moved (Don't move   *~
            *             Warranty Information. Use (EWDPLA67) to move  *~
            *             Warranty Data. After moving Warranty Data     *~
            *             purge prior year data from database.          *~
            *                                                           *~
            *  Program Name      - EWDPLN67                             *~
            *  Creation Date     - 02/15/99                             *~
            *  Last Mod Date     - 03/19/2018                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Last Modified By  - Christie Norman                      *~
            *                                                           *~
            *  Description       - Create History Master Database       *~
            *                      all in One (1) File.                 *~
            *                                                           *~
            *  Special Subs - (create_data)-    Main line History Build *~
            *                 (display_stats)-  Check Progress          *~
            *                 (move_text)-      Move Header/Line Text   *~
            *                 (get_next_id)-    Assign New Text Pointer *~
            *                 (lookup_planning_data)-   Check (APCPLNOR)*~
            *                 (purge_apcplnsc)- Self Explanitory        *~
            *                 (purge_apcplnsd)- Self Explanitory        *~
            *                 (purge_apcplndt)- Self Explanitory        *~
            *                 (dataput)-        Create History Record   *~
            *                >(lookup_warranty)-Check for Warranty Id   *~
            *Use(EWDPLA67)--->(update_warranty_new)- Build for History  *~
            *                >(warranty_purge)- Purge Warranty for S.O. *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/15/99 ! (New) Program                            ! RHH *~
            * 03/29/00 ! (EWD001) Mods and Updates for History    ! RHH *~
            * 03/06/01 ! (EWD002) Mod to fix problem with warranty! RHH *~
            *          !   Info not all being moved to history    !     *~
            *          !                                          !     *~
            * 04/05/05 ! (AWD003) mod to the reclen of APCPLNSD   ! CMG *~
            * 06/09/06 ! (AWD004) Mod for Warranty File Change    ! RHH *~
            * 04/12/07 ! (AWD005) Mod to add subpart to History   ! CMG *~
            *06/26/2007! (AWD006) Mod to split 2005 record        ! DES *~
            *01/11/2008! (AWD007) Add BNKMASTR & BNKLINES         ! DES *~
            *03/22/2013! (AWD008) mod for neg qty - credits       ! CMG *~
            *03/18/2015! (AWD009) mod to roll hist to new files.  ! PWW *~
            *04/01/2017! Mod to always write to ARVHIST files     ! CMN *~
            *03/19/2018! Mod to name history files by year        ! CMN *~
            *05/12/2021! CR2829 New bcklin2 file purge            ! RDB *~
            *02/28/2022! CR3038 Add Ricky to authorization list   ! RDB *~
            *************************************************************

        dim                                                              ~
            filename$8,                  /* Used by EWDOPEN            */~
            bck_key$25, bck_rec$(4%)250, /* (BCKMASTR) Records         */~
            bck_ln_key$19,               /* (BCKLINES) Records         */~
            bck_ln_rec$(3%)100,          /*                            */~
        /* <AWD007> */     ~
            bnk_key$16, bnk_rec$(5%)206, /* (BNKMASTR) Records         */~
            bnk_ln_key$19,               /* (BNKLINES) Records         */~
            bnk_ln_rec$(2%)150,          /*                            */~
            bnk_purge$1,                 /* BCKMASTR/BCKLINES          */~
        /* </AWD007> */     ~
            scr_yr$4, cur_yr$4,          /* S.O. conversion Year       */~
            tst_yr$4,                    /* Test year                  */~
            bck_conv$,  bck_purge$1,     /* BCKMASTR/BCKLINES          */~
            txt_conv$1, txt_purge$1,     /* TXTFILE                    */~
            pln_conv$1, pln_purge$1,     /* APCPLNOR-APCPLNSC-APCPLNSD */~
            stk_conv$1, stk_purge$1,     /* Stock Orders (Inventory)   */~
                                         /* (EWDHIST) Master S.O. Hist */~
            hs_key$15,                   /* Alt Key (1) SO,LN,YYYY     */~
            hs_job$16,                   /* Customer Job Name          */~
            hs_cust$9,                   /* Customer Code              */~
            hs_po$16,                    /* Customer P.O. Number       */~
            hs_ln$3, hs_ln0$3,           /* Sales Order Line Item      */~
            hs_yr$4,                     /* History Data Year (YYYY)   */~
            hs_load$5,                   /* Customer Load No.          */~
            hs_so$8,                     /* Customer Sales Order       */~
                                         /* hs_ln$                     */~
                                         /* hs_yr$                     */~
            hs_hdrt$4,                   /* Header Text Id             */~
            hs_linet$4,                  /* Line Item Text Id          */~
                                         /* hs_cust$                   */~
            hs_order$6,                  /* S.O. Order Date            */~
                                         /* hs_so$                     */~
                                         /* hs_ln$                     */~
            hs_ship$6,                   /* Sales Order Ship Date      */~
            hs_quote$10,                 /* EWD Quote Number           */~
            hs_part$25,                  /* MFG Part Number            */~
                                         /* hs_oqty% - Order Ln Qty    */~
                                         /* hs_sqty% - Shipped Ln Qty  */~
                                         /* hs_hdr_disc                */~
                                         /* hs_ln_disc                 */~
                                         /* hs_uprice - Ln Unit Price  */~
                                         /* hs_eprice - Ln Extnd Price */~
            hs_cat$4,                    /* Ln Item Category Code      */~
            hs_prc$1,                    /* Ln Item Price Code         */~
            hs_rgn$4,                    /* Region Code                */~
            hs_sls$4,                    /* Salesman Code              */~
            hs_inv$8,                    /* EWD Invoice Number         */~
            hs_chk$8,                    /* Customer Check No.         */~
            hs_due$6,                    /* Customer Due Date          */~
/*AWD005*/  hs_subpart$20,               /* Sub part                   */~
/*AWD005*/  hs_infpart$20,               /* Info part                  */~
/*AWD008*/  hs_credit$1,                 /* credit                     */~
/*AWD005*/  hs_filler$1,                 /* Filler Area                */~
            sc_key$10, sc_rec$128,       /* (APCPLNSC)                 */~
            sav_so$8,                    /* Save Sales Order           */~
                                         /*                            */~
            readkey$50,                  /* GENCODES Lookup            */~
            tst_date$10, tt_date$10,     /* Use to Test Year of S.O.   */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$10, userid$3,           /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
/*AWD005*/  bcksubpt_key$11              /* BCKSUBPT Key               */

        dim                              /* PROGRAM VARIABLES          */~
            st_time$8, st_time_d$8,      /* Process Start Time, Decimal*/~
            cr_time$8, cr_time_d$8,      /* Time of Last Analysis, Dec */~
            tt_hr$3,                     /* Total Hours for Process    */~
            tt_mn$3,                     /* Total Minutes for Process  */~
            tt_total$24,                 /* Total Process Time         */~
            cnt%(20%), cnt$(20%)12,      /* Statistics                 */~
            wr_key$15, wr$(32765%)8,     /* Warr Rec Variables (AWD004)*/~
            wr_rec1$128,                 /* (New) Warranty Rec (AWD004)*/~
            wr_key1$15,                  /* Warranty History Record    */~
            wr_rec$32, hs_warranty$8,    /* (New) Warranty Record      */~
            prg_key$60,                  /* Planning Data              */~
            tt$(15%)60,                  /* Display Stats              */~
            textid$4, txt_zero$4,        /* S.O. HEADER TEXT ID        */~
            txtid$4,                     /* Argument                   */~
            text_rec$(3%)70,             /* TEXT RECORD BUFFER         */~
            text_key$11, rhh$10,         /* TEXT KEY                   */~
            text$64,                     /* TEXT DEFINITION AREA       */~
            sav_txt$9                    /* CHECK TEXT KEY             */

        dim awdappor_key1$40,            /* AWDAPPOR readkey #1        */~
            awdappor_so$8,               /* AWDAPPOR SO Number         */~
            awdappor_dte$10,             /* AWDAPPOR Early dispatch dte*/~
            awdappsc_key$10,             /* AWDAPPSC Readkey           */~
            awdappdp_key$07,             /* AWDAPPDP Readkey           */~
            awdappdp_dte$10              /* AWDAPPDP Depart Date       */

        dim f2%(30%),                    /* = 0 if the file is open    */~
            f1%(30%),                    /* = 1 if READ was successful */~
            fs%(30%),                    /* Fs for opencheck           */~
            rslt$(30%)20                 /* Opencheck rslt             */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$32, pname$21
            apc$   = "Move Sales Order Data to History"
            pname$ = "EWDPLN67 - Rev: R7.00"

        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

          REM ***********************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  !          ! <Available>                              *~
            * #2  ! BCKMASTR ! Sales Order Master- Headers              *~
            * #3  ! BCKLINES ! Sales Order Master- Lines                *~
            * #5  ! TXTFILE  ! System Text File                         *~
            *-----!----------! (Above) Live Data Files                  *~
            *-----!----------!------------------------------------------*~
            * Old !          ! <No Longer Used>                         *~
            * #4  ! APCORDHS ! (Old) Planning File Replaced by APCPLNOR *~
            * #6  ! BCKMSTHS ! Sales Order Header History               *~
            * #7  ! BCKLINHS ! Sales Order Line Item History            *~
            * #8  ! TXTHIST  ! Sales Order Text History                 *~
            *-----!----------! (Above) Old History Files                *~
            *-----!----------!------------------------------------------*~
            * #12 ! APCPLNOR ! Planning Master Header File              *~
            * #13 ! APCPLNSC ! Planning Master Line Item File           *~
            * #14 ! APCPLNSD ! Planning Master Schedule File            *~
            * #15 ! APCPLNDT ! Planning Master Detail File              *~
            * #16 ! APCPLNWT ! Master Warranty Pointer File             *~
            *-----!----------! (Above) Current Planning Data (Purge)    *~
            * #17 ! BCKSUBPT !                                          *~
            *-----!----------!------------------------------------------*~
            * #21 ! BNKMASTR ! Sales Order Master- Headers              *~
            * #22 ! BNKLINES ! Sales Order Master- Detail               *~
            * #23 ! AWDAPPOR !                                          *~
            * #24 ! AWDAPPSC !                                          *~
            * #25 ! AWDAPPDP !                                          *~
            * #30 ! BCKLIN2  ! CR2829 new file                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #2,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #3,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #5,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11                      ~



            select #12, "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos = 1,    keylen = 51,                      ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup

            select #13, "APCPLNSC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 24,   keylen = 10,                      ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33

/* (AWD003) - Mod to key and reclen */
            select #14, "APCPLNSD",                                      ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  23

            select #15, "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 24,   keylen = 23,                      ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup
                                                   /* (EWD001) - Begin */
                                                   /* (AWD004)         */
            select #16, "APCPLNWT",                                      ~
                         varc,    indexed,  recsize =  128,              ~
                         keypos =    1, keylen =  8,                     ~
                         alt key 1, keypos =  9, keylen = 10, dup,       ~
                             key 2, keypos =  9, keylen = 18

            select #17, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup


            select #21, "BNKMASTR",                                      ~
                        varc,     indexed,  recsize = 1030,              ~
                        keypos =    1, keylen =   16,                    ~
                        alt key  1, keypos =   18, keylen =  12, dup,    ~
                            key  2, keypos =   31, keylen =  25, dup,    ~
                            key  3, keypos =   17, keylen =  39, dup

            select #22, "BNKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =   19


            select #23, "AWDAPPOR",                                      ~
                        varc,     indexed,  recsize =  384,              ~
                        keypos =   1, keylen =   8,                      ~
                        alt key  1, keypos =    9, keylen =  40,         ~
                            key  2, keypos =   15, keylen =  34,         ~
                            key  3, keypos =   17, keylen =  32


            select #24, "AWDAPPSC",                                      ~
                        varc,     indexed,  recsize =  384,              ~
                        keypos =    1, keylen =  10

            select #25, "AWDAPPDP",                /*(AWD004)*/          ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =   7


            select #26,  "HIST19",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   17, keylen =  32,                     ~
                        alt key  1, keypos =   54, keylen =  15,         ~
                            key  2, keypos =   49, keylen =  20,         ~
                            key  3, keypos =   77, keylen =  26,         ~
                            key  4, keypos =    1, keylen =  25, dup,    ~
                            key  5, keypos =  193, keylen =   8, dup

            select #27, "WARR19",                                        ~
                         varc,    indexed,  recsize =   32,              ~
                         keypos =    1, keylen =  15,                    ~
                         alt key 1, keypos =  16, keylen = 15, dup


            select #28, "TEXTH19",                                       ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen =  11
/* CR2829 */
            select #30, "BCKLIN2",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   10, keylen =  19
                        
                                                   /* (AWD004)         */
            call "SHOSTAT" ("Opening Files, One Moment Please")
                                       /* Current Year's Data       */
            filename$ = "BCKMASTR" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "TXTFILE" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error

REM CALL "OPENCHCK" (#18, FS%(18%), F2%(18%), 10%, RSLT$(18%))
REM CALL "OPENCHCK" (#20, FS%(20%), F2%(20%), 10%, RSLT$(20%))
REM CALL "OPENCHCK" (#26, FS%(26%), F2%(26%), 10%, RSLT$(26%))
REM CALL "OPENCHCK" (#8, FS%(8%), F2%(8%), 10%, RSLT$(8%))
                                           /* Master History Files      */
REM FILENAME$ = "EWDHIST"  CALL "EWDOPEN" (#9, FILENAME$, ERR%)
REM IF ERR% <> 0% THEN GOSUB OPEN_ERROR
REM FILENAME$ = "EWDTEXTH"  CALL "EWDOPEN" (#10, FILENAME$, ERR%)
REM IF ERR% <> 0% THEN GOSUB OPEN_ERROR
REM FILENAME$ = "EWDWARR"  CALL "EWDOPEN" (#11, FILENAME$, ERR%)
REM IF ERR% <> 0% THEN GOSUB OPEN_ERROR



                                       /* Planning master Files     */
            filename$ = "APCPLNOR" : call "EWDOPEN" (#12, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSC" : call "EWDOPEN" (#13, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSD" : call "EWDOPEN" (#14, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDT" : call "EWDOPEN" (#15, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNWT" : call "EWDOPEN" (#16, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#17, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BNKMASTR" : call "EWDOPEN" (#21, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BNKLINES" : call "EWDOPEN" (#22, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDAPPOR" : call "EWDOPEN" (#23, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDAPPSC" : call "EWDOPEN" (#24, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDAPPDP" : call "EWDOPEN" (#25, filename$, err%)
            if err% <> 0% then gosub open_error
/* CR2829 */
            filename$ = "BCKLIN2" : call "EWDOPEN" (#30, filename$, err%)
            if err% <> 0% then gosub open_error
            
            call "OPENCHCK" (#26, fs%(26%), f2%(26%), 100%, rslt$(26%))
            call "OPENCHCK" (#27, fs%(27%), f2%(27%), 100%, rslt$(27%))
            call "OPENCHCK" (#28, fs%(28%), f2%(28%), 100%, rslt$(28%))


            text_n% = 0%
            mat f1% = zer

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
            cur_yr$ = str(date$,7%,4%)     /* Capture the Current Year */
            if userid$ = "PWW" or userid$ = "PWN" then goto L01000
            if userid$ = "CG1" then goto L01000
            if userid$ = "CMG" then goto L01000
            if userid$ = "CGN" then goto L01000
            if userid$ = "RDB" or userid$ = "RBN" then goto L01000  /* CR3038 */
               errormsg$ = "(Error) Not a Valid User for History Create?"
               gosub error_prompt
               goto exit_program


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

L01000: inputmode
            gosub initialize_variables
            for fieldnr% = 1% to  10%
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
                  if keyhit%  = 14% then goto create_data
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 10% then editpg1
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

        create_data
            init(" ") cnt$(), bck_key$, bck_rec$(), st_time$, st_time_d$
            st_time_d$ = time
            call "TIME" (st_time$)

            mat cnt% = zer

            call "SHOSTAT" ("Begin History Build for ["&scr_yr$&"]")
                                                  /* (BCKMASTR)        */
        create_data_header                        /* (EWD001)          */
            read #2,hold,key > bck_key$, using L19000, bck_rec$(),       ~
                                               eod goto create_data_done
L19000:        FMT 4*CH(250)
            cnt%(1%) = cnt%(1%) + 1%              /* Count Sales Orders*/

            if mod(cnt%(1%),500) <> 0 then goto L19005
               finish% = 0%
               gosub display_stats

L19005:     bck_key$ = str(bck_rec$(),1%,25%)     /* Cust and S.O.     */
            if str(bck_key$,10%,8%) = "06753155" then gosub SO_NUMBER
            init(" ") tst_date$, bck_ln_key$, tst_yr$
            tst_date$ = str(bck_rec$(),830%,6%)   /* Check Order Date  */
            call "DATFMTC" (tst_date$)
            rhh% = len(tst_date$)
            if rhh% < 7% then goto create_data_header
                                                   /* Blank Date       */
            tst_yr$ = scr_yr$

REM CALL "SHOSTAT" ("TEST S.O. DATE YEAR ("&TST_DATE$&") "&  ~
REM                 TST_YR$)
REM STOP
                                                  /* Primary Test (A)  */
               if scr_yr$ <> str(tst_date$,7%,4%) then                   ~
                                                  goto create_data_header


                  if bck_purge$ = "N" then goto L19010
                     delete #2                    /* Purge (BCKMASTR)  */
                     cnt%(2%) = cnt%(2%) + 1%
                                                  /* Count Purged S.O. */
                                                  /* Primary Test (B)  */

                                                  /* Load Header Data  */
L19010:        hs_job$  = str(bck_rec$(),619%,16%)/* Customer Job No.  */
               hs_cust$ = str(bck_rec$(),1%,9%)   /* Customer Code     */
               hs_po$   = str(bck_rec$(),26%,16%) /* Customer P.O.     */
               hs_yr$   = tst_yr$                 /* Set Year          */
               hs_hdrt$ = str(bck_rec$(),799%,4%) /* Header Text Id    */
               hs_order$= str(bck_rec$(),806%,6%) /* S.O. Order Date   */
               init(" ") tt_date$
               str(tt_date$,1%,6%) = hs_order$
               gosub date_correct
               if date% = 0% then hs_order$ = str(tt_date$,1%,6%)

               hs_so$   = str(bck_rec$(),10%,8%)  /* S.O. Number       */
               hs_ship$ = str(bck_rec$(),824,6%)  /* S.O. Ship Date    */
               init(" ") tt_date$
               str(tt_date$,1%,6%) = hs_ship$
               gosub date_correct
               if date% = 0% then hs_ship$ = str(tt_date$,1%,6%)

               hs_due$  = str(bck_rec$(),818,6)   /* S.O. Due Date     */
               init(" ") tt_date$
               str(tt_date$,1%,6%) = hs_due$
               gosub date_correct
               if date% = 0% then hs_due$ = str(tt_date$,1%,6%)

               hs_quote$= str(bck_rec$(),599%,10%)/* Customer Quote No */
               hs_rgn$  = str(bck_rec$(),595%,4%) /* Region Code       */
               hs_sls$  = str(bck_rec$(),580%,4%) /* Salesman Code     */
               hs_prc$  = str(bck_rec$(),858%,1%) /* Price Code        */
                                                  /* Disc Pcnt Header  */
               hs_hdr_disc = 0.0
               get str(bck_rec$(),859%,8%), using L19020, hs_hdr_disc
L19020:           FMT PD(15,4)
                                                  /* Convert Hdr TXT   */
               txtid$   = hs_hdrt$
               gosub move_text
               if txt% = 0% then txtid$ = txt_zero$
               hs_hdrt$ = txtid$                  /* New Pointer       */
                                                  /* Invoice No        */
                                                  /* Check Number      */
                                                  /* Load No.          */
               hs_load$  = "99999"                /* Oriented Default  */
               gosub lookup_planning_data         /* May or May Not be */
                                                  /* Applicable        */
        if bck_conv$ = "N" then goto L19030
               init(" ") wr_key$
               str(wr_key$,1%,8%)  = hs_so$

               read #27,key 1% > wr_key$, using L19025, wr_key$,         ~
                                                        eod goto L19028
L19025:           FMT POS(16), CH(15)             /* Check for Skip    */
               if str(wr_key$,1%,8%) = hs_so$ then goto L19030
L19028:    cnt%(10%) = cnt%(10%) + 1%             /* Count (BCKMASTR)  */
                                                  /*                   */

                                                  /* (BCKLINES) or Moved */
L19030: str(bck_ln_key$,1%,8%) = hs_so$           /* Set Sales Order     */

        create_data_line
            read #3,hold,key > bck_ln_key$, using L19040, bck_ln_rec$(), ~
                                              eod goto create_data_header
L19040:        FMT 3*CH(100)
            bck_ln_key$ = str(bck_ln_rec$(),10%,19%)
            if str(bck_ln_key$,1%,8%) <> hs_so$ then                     ~
                                                  goto create_data_header

            init(" ") bcksubpt_key$, hs_subpart$, hs_infpart$
            str(bcksubpt_key$,1,8) = str(bck_ln_rec$(),10,8)
            str(bcksubpt_key$,9,3) = str(bck_ln_rec$(),26,3)

               if bck_purge$ = "N" then L19050
                  delete #3                       /* purge (BCKLINES)  */
                  cnt%(11%) = cnt%(11%) + 1%

L19050:        hs_ln$    = str(bck_ln_rec$(),26%,3%)  /* S.O. Line Item*/
               init(" ") hs_ln0$
               hs_ln% = 0%
               convert hs_ln$ to hs_ln%, data goto L19055
                                                      /* Line Item Zero*/
L19055:        convert hs_ln% to hs_ln0$, pic(000)    /* Filled        */

               hs_linet$ = str(bck_ln_rec$(),242%,4%) /* Line Item Text*/
               hs_part$  = str(bck_ln_rec$(),32%,25%) /* MFG Part No.  */
                                                      /* Order Qty     */
               hs_oqty% = 0%   :   hs_oqty = 0.00
               get str(bck_ln_rec$(),93%,8%), using L19060, hs_oqty
L19060:           FMT PD(14,4)

               hs_credit$ = " "
               hs_oqty% = int(hs_oqty)
               if hs_oqty% < 0% then hs_credit$ = "C"     /* (AWD008) */
               if hs_credit$ = "C" then hs_oqty% = (hs_oqty%) * (-1)

                                                      /* Ship quantity */
               hs_sqty% = 0%   :   hs_sqty = 0.00
               get str(bck_ln_rec$(),101%,8%), using L19060, hs_sqty
               hs_sqty% = int(hs_sqty)
               if hs_credit$ = "C" then hs_sqty% = (hs_sqty%) * (-1)
                                                      /* Unit Price    */
               get str(bck_ln_rec$(),165%,8%), using L19060, hs_uprice
                                                      /* Extended Price*/
                                                      /* using ship Qty*/
               hs_eprice = round(hs_sqty * hs_uprice, 2)
                                                      /* With No Disc. */
                                                      /* 1st Line Item */
               hs_ln_disc = 0.0
               get str(bck_ln_rec$(),173%,8%), using L19020, hs_ln_disc

               dsc_amt   = 0 - round(hs_eprice * hs_ln_disc * .01, 2)
               hs_eprice = round(hs_eprice + dsc_amt, 2)
                                                      /* With Ln Disc  */
                                                      /* 2nd Header    */
               dsc_amt   = 0 - round(hs_eprice * hs_hdr_disc * .01, 2)
               hs_eprice = round(hs_eprice + dsc_amt, 2)
                                                      /* With All Disc */
                                                      /* Category Code */
               hs_cat$   = str(bck_ln_rec$(),89%,4%)
                                                      /* Warranty Id   */
               txtid$ = hs_linet$                     /* Move Line Item*/
               gosub move_text                        /* Text          */
               if txt% = 0% then txtid$ = txt_zero$
               hs_linet$ = txtid$

/*(AWD005)*/
               gosub lookup_bcksubpt
               gosub dataput                          /* Update History*/

               gosub lookup_warranty                  /* Warranty Id   */
               
               if bck_purge$ = "Y" then gosub purge_bcklin2  /* CR2829 */
               goto create_data_line

        create_data_done
            gosub create_data_stock
            gosub purge_bnkfiles
            gosub purge_appfiles

            finish% = 1%
            gosub display_stats
        return clear all
        goto inputmode

        date_correct
            init(" ") rhh$
            date% = 0%
            call "DATFMTC" (tt_date$)                 /* Length (10)   */
            call "DATEOKC" (tt_date$, date%, errormsg$)
            if date% = 0% then goto L20010
            call "DATUFMTC" (tt_date$)
            date% = 0%
        return
L20010:   init(" ") tt_date$
        return
            errormsg$ = "(Error) - Converting date ???"
            gosub error_prompt
            init(" ") tt_date$
            date% = 99%
        return

        create_data_stock
            if stk_conv$ = "N" then return
               call "SHOSTAT" ("Converting Stock")

               rhh% =0%
               init(" ") sc_key$, sc_rec$, sav_so$
               str(sc_key$,1%,1%) ="S"
        create_data_stock_nxt
            read #13,hold,key > sc_key$, using L21000, sc_rec$,        ~
                                        eod goto create_data_stock_done
L21000:        FMT CH(128)

            cnt%(1%) = cnt%(1%) + 1%
            if mod(cnt%(1%),500) <> 0 then goto L21005
               finish% = 0%
               gosub display_stats

L21005:     sc_key$ = str(sc_rec$,24%,10%)
            if str(sc_key$,1%,1%) <> "S" then goto create_data_stock_done
               init(" ") tst_date$, tst_yr$

               tst_date$ = str(sc_rec$,1%,6%)      /* Delivery Date   */
               call "DATFMTC" (tst_date$)
               rhh% = len(tst_date$)
               if rhh% < 7% then goto create_data_stock_nxt

               tst_yr$ = scr_yr$
               if scr_yr$ <> str(tst_date$,7%,4%) then                   ~
                                               goto create_data_stock_nxt

                  if stk_purge$ = "N" then goto L21010
                     delete #13
                     cnt%(7%) = cnt%(7%) + 1%      /* Purge (APCPLNSC) */
                                                   /* Build Header Data*/
L21010:           hs_job$  = "Stock           "    /* Stock Job Number */
                  str(hs_job$,7%,10%) = tst_date$
                  hs_cust$ = str(sc_rec$,59%,9%)   /* (999999)Customer */
                  if sav_so$ <> str(sc_key$,1%,8%) then rhh% = 0%

                  sav_so$ = str(sc_key$,1%,8%)
                  hs_po$   = "StkYYYY xxxxxxxx"   /* Stock N/A        */
                  str(hs_po$,4%,4%) = tst_yr$
                  str(hs_po$,9%,8%) = sav_so$

                  hs_yr$   = tst_yr$               /* Set Year         */
                  hs_hdrt$ = txt_zero$             /* No Text          */
                  hs_order$= str(sc_rec$,1%,6%)    /* Delviery/Prod    */
                  init(" ") tt_date$
                  str(tt_date$,1%,6%) = hs_order$
                  gosub date_correct
                  if date% = 0% then hs_order$ = str(tt_date$,1%,6%)

                  hs_load$ = str(sc_rec$,7%,5%)    /* Load Number      */
                  hs_so$   = str(sc_rec$,24%,8%)
                  hs_ship$ = hs_order$             /* Stock Orders     */
                  hs_due$  = hs_order$             /* Stock Orders     */
                  hs_quote$= "         "
                  hs_rgn$  = "STK "                /* In House         */
                  hs_sls$  = "2100"                /* EWD Coorporate   */
                  hs_prc$  = "A"                   /* List             */
                  hs_hdr_disc = 0.0                /* Discount         */
                                                   /* Build Line Item  */
                  hs_ln% = 0%
                  convert str(sc_rec$,32%,2%) to hs_ln%, data goto L21020
L21020:
                  convert hs_ln% to hs_ln$,  pic(###)
                  convert hs_ln% to hs_ln0$, pic(000)

                  hs_linet$ = txt_zero$            /* No Text          */
                  hs_part$  = str(sc_rec$,34%,25%) /* Stock Part No.   */

                  get str(sc_rec$,68%,2%), using L21030, hs_oqty%
L21030:               FMT BI(2)
                  hs_sqty% = hs_oqty%              /* Order/Ship Qty   */
                  hs_ln_disc = 0.0
                  get str(sc_rec$,76%,8%), using L21040, hs_eprice
L21040:               FMT PD(14,4)
                  hs_uprice = 0.0
                  if hs_oqty% = 0% then goto L21050
                  hs_uprice = round(hs_eprice/hs_oqty%, 2)
L21050:
                  hs_cat$ = str(hs_part$,1%,3%) & "2"

                  gosub dataput_stk
                  cnt%(16%) = cnt%(16%) + 1%

                  gosub lookup_planning_data

                  gosub lookup_warranty

                  goto create_data_stock_nxt
        create_data_stock_done
        return

purge_bnkfiles:
            if bnk_purge$ = "N" then return
               call "SHOSTAT" ("Purgeing BNK files")
               dim pgDte$10, blankDte$6
               init(" ") blankDte$
               pgDte$ = scr_yr$&"1231"

               call "DATEFMT" (pgDte$)
               call "DATUFMTC" (pgDte$)

               rhh% =0%
               init(" ") bnk_key$, bnk_rec$()
purge_bnkmastr_next
              read #21,hold,key > bnk_key$, using BNKMASTR, bnk_rec$(),        ~
                                        eod goto purge_bnkfiles_done
BNKMASTR:         FMT 5*CH(206)

            cnt%(1%) = cnt%(1%) + 1%
            if mod(cnt%(1%),500) <> 0 then goto skip_disp
               finish% = 0%
               gosub display_stats
skip_disp:

               tst_date$ = str(bnk_rec$(),836%,6%)   /* Check Order Date  */
               call "DATFMTC" (tst_date$)
               bnk_key$ = str(bnk_rec$(),1%,16%)
REM RHH% = LEN(TST_DATE$)
REM IF RHH% < 7% THEN GOTO PURGE_BNKMASTR_NEXT
REM TST_YR$ = SCR_YR$
                                                  /* Primary Test (A)  */
               if scr_yr$ <> str(tst_date$,7%,4%) then                   ~
                                               goto purge_bnkmastr_next


REM  IF STR(TST_DATE$,1,6) > STR(PGDTE$,1,6) AND STR(TST_DATE$,1,6)
REM  <> STR(BLANKDTE$,1,6) THEN PURGE_BNKMASTR_NEXT

               delete #21
               cnt%(18%) = cnt%(18%) + 1%
               bnk_ln_key$ = bnk_key$ & "   "
purge_bnklines_next:
               read #22,hold,key > bnk_ln_key$, using BNKLINES, bnk_ln_rec$(), ~
                                        eod goto purge_bnkmastr_next
BNKLINES:      FMT 3*CH(206)
               bnk_ln_key$ = str(bnk_ln_rec$(),1%,19%)
               if str(bnk_ln_key$,1,16) <> bnk_key$ then goto purge_bnkmastr_next

               delete #22
               cnt%(18%) = cnt%(18%) + 1%
               goto purge_bnklines_next

purge_bnkfiles_done
       return

purge_appfiles:
        if bnk_purge$ = "N" then return
        call "SHOSTAT" ("PURGE AWDAPPOR & AWDAPPSC")  
        init(" ") awdappor_key1$, awdappor_dte$, awdappsc_key$
        awdappor_key1$ = all(hex(00))
PURGE_AWDAPPOR_NEXT:
        read #23, hold, key 1% > awdappor_key1$, using AWDAPPOR_FMT1,~
                                     awdappor_so$, awdappor_key1$,   ~
                                            eod goto READ_AWDAPPDP
AWDAPPOR_FMT1:    FMT CH(08), CH(40)


         awdappor_dte$ = str(awdappor_key1$,1%,6)
         call "DATFMTC" (awdappor_dte$)
         if str(awdappor_dte$,7%,4%) > scr_yr$ then                   ~
                                          goto READ_AWDAPPDP
              delete #23

             gosub AWDAPPSC_LINES
         goto PURGE_AWDAPPOR_NEXT


AWDAPPSC_LINES:
        init(" ") awdappsc_key$
        awdappsc_key$ = awdappor_so$
AWDAPPSC_NEXT:
        read #24, hold, key > awdappsc_key$, using AWDAPPSC_FMT,  ~
                                 awdappsc_key$, eod goto AWDAPPSC_DONE
AWDAPPSC_FMT:        FMT CH(10)
          if str(awdappsc_key$,1%,08%) <> awdappor_so$            ~
                                       then goto AWDAPPSC_DONE
             delete #24

             goto AWDAPPSC_NEXT
        AWDAPPSC_DONE
        return

READ_AWDAPPDP:
        init(" ") awdappdp_key$, awdappdp_dte$
        awdappdp_key$ = all(hex(00))
PURGE_AWDAPPDP_NEXT:
        read #25, hold, key > awdappdp_key$, using AWDAPPDP_FMT,         ~
                         awdappdp_key$, awdappdp_dte$, eod goto AWDAPPDP_DONE
AWDAPPDP_FMT:   FMT CH(07), POS(40), CH(06)

             call "DATFMTC" (awdappdp_dte$)      /* Depart Date */
             if str(awdappdp_dte$,7%,4%) > scr_yr$ then                   ~
                                              goto PURGE_AWDAPPDP_NEXT
                  delete #25

                  goto PURGE_AWDAPPDP_NEXT
        AWDAPPDP_DONE
        return

/* CR2829 */
        purge_bcklin2
          read #30, hold, key = bck_ln_key$, eod goto BCKLIN2_DONE
                   delete #30
          
        BCKLIN2_DONE                   
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

        scrn1_msg  :  data                                                ~
         "Enter a Valid Year for S.O. Data Conversion to History (YYYY)?",~
         "Move (BCKMASTR-BCKLINES) Sales Order Data To History    (Y/N)?",~
         "Purge (BCKMASTR-BCKLINES) Sales Order Data From Live    (Y/N)?",~
         "Move (TXTFILE) Sales Order Header/Detail Text to History(Y/N)?",~
         "Purge (TXTFILE) Sales Order Header/Detail Text from Live(Y/N)?",~
         "Move (APCPLNOR-APCPLNSC-APCPLNSD-APCPLNWT) to History   (Y/N)?",~
         "Purge (APCPLNOR-APCPLNSC-APCPLNSD-APCPLNWT) from Live   (Y/N)?",~
         "Move (Stock Sales Orders) to History                    (Y/N)?",~
         "Purge (Stock Sales Orders) from Live                    (Y/N)?",~
         "Purge (BNKMASTR-BNKLINES) Sales Order Data From Live    (Y/N)?"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, bck_key$, bck_rec$(),      ~
                      bck_ln_key$, bck_ln_rec$(),scr_yr$, bck_conv$,     ~
                      bck_purge$, txt_conv$, txt_purge$, pln_conv$,      ~
                      pln_purge$, tt$(), txt_zero$, stk_conv$, stk_purge$

            tt$( 1%) =                                                   ~
            "Customer  [xxxxxxxxx]  S.O.[xxxxxxxx]  P.O[xxxxxxxxxxxxxxxx]"

            tt$( 2%) =                                                   ~
            "The No. of Sales Orders Checked for [xxxx] Equals [xxxxxxxx]"

            tt$( 3%) =                                                   ~
            "Sales Order 'HEADERS'     Moved [xxxxxxxx] Purged [xxxxxxxx]"

            tt$( 4%) =                                                   ~
            "Sales Order 'LINES'       Moved [xxxxxxxx] Purged [xxxxxxxx]"

            tt$( 5%) =                                                   ~
            "Sales Order 'Text Lines'  Moved [xxxxxxxx] Purged [xxxxxxxx]"

            tt$( 6%) =                                                   ~
            "Warranty ID's [xxxxxxxx]  Moved [xxxxxxxx] Purged [xxxxxxxx]"

            tt$( 7%) =                                                   ~
            "(APCPLNOR)                Found [xxxxxxxx] Purged [xxxxxxxx]"

            tt$( 8%) =                                                   ~
            "(APCPLNSC)                Stock [xxxxxxxx] Purged [xxxxxxxx]"

            tt$( 9%) =                                                   ~
            "(APCPLNSD)                                 Purged [xxxxxxxx]"

            tt$(10%) =                                                   ~
            "(APCPLNDT)                                 Purged [xxxxxxxx]"

            tt$(11%) =                                                   ~
            "(BNKMASTR)                                 Purged [xxxxxxxx]"

            tt$(12%) =                                                   ~
            "(BNKLINES)                                 Purged [xxxxxxxx]"

            tt$(13%) = "Information Up To Sales Order Displayed ????"

            put txt_zero$, using L29000, 0%
L29000:       FMT BI(4)
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

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        dataput                          /* (EWDHIST) Master Hist File */
            if bck_conv$ = "N" then return  /* Do not Move to History  */
        dataput_stk
                                         /* stk_conv$ = "N"            */
               fix% = 0%                    /* Correct Dup Po's        */
               init(" ") hs_key$
               str(hs_key$,1%,8%)  = hs_so$
               str(hs_key$,9%,3%)  = hs_ln$ /* (Blank) Filled          */
               str(hs_key$,12%,4%) = hs_yr$
/* AWD006*/
REM FF% = 9%
REM IF HS_YR$ >= "2013" THEN FF% = 18%

               read #26,key 1% = hs_key$, eod goto L31000
                  goto L31010               /* Skip if Exists          */

L31000:     put #26,  using L35040,                                     ~
                     hs_job$,            /* Customer Job Name          */~
                     hs_cust$,           /* Customer Code              */~
                     hs_po$,             /* Customer P.O. Number       */~
                     hs_ln$,             /* Sales Order Line Item      */~
                     hs_yr$,             /* History Data Year (YYYY)   */~
                     hs_load$,           /* EWD Load Number            */~
                     hs_so$,             /* Customer Sales Order       */~
                     hs_ln$,             /* hs_ln$                     */~
                     hs_yr$,             /* hs_yr$                     */~
                     hs_hdrt$,           /* Header Text Id             */~
                     hs_linet$,          /* Line Item Text Id          */~
                     hs_cust$,           /* hs_cust$                   */~
                     hs_order$,          /* S.O. Order Date            */~
                     hs_so$,             /* hs_so$                     */~
                     hs_ln$,             /* hs_ln$                     */~
                     hs_ship$,           /* Sales Order Ship Date      */~
                     hs_quote$,          /* EWD Quote Number           */~
                     hs_part$,           /* MFG Part Number            */~
                     hs_oqty%,           /* hs_oqty% - Order Ln Qty    */~
                     hs_sqty%,           /* hs_sqty% - Shipped Ln Qty  */~
                     hs_hdr_disc,        /* Header Discount Pcnt       */~
                     hs_ln_disc,         /* Line Item Discount Pcnt    */~
                     hs_uprice,          /* hs_uprice - Ln Unit Price  */~
                     hs_eprice,          /* hs_eprice - Ln Extnd Price */~
                     hs_cat$,            /* Ln Item Category Code      */~
                     hs_prc$,            /* Ln Item Price Code         */~
                     hs_rgn$,            /* Region Code                */~
                     hs_sls$,            /* Salesman Code              */~
                     hs_inv$,            /* EWD Invoice Number         */~
                     hs_chk$,            /* Customer Check No.         */~
                     hs_due$,            /* Customer Due Date          */~
                     hs_subpart$,        /* Subpart  AWD005            */~
                     hs_infpart$,        /* Infopart AWD005            */~
                     hs_credit$,         /* Credit   AWD008            */~
                     hs_filler$          /* Filler Area                */
                                         /* (EWDHIST)                  */
            write  #26, eod goto L31020
            cnt%(12%) = cnt%(12%) + 1%   /* Sales Order Line Items     */
                                         /* Moved to History           */
L31010: return

L31020:     if fix% = 1% then goto L31030
               fix% = 1%                 /* Try to fix Dup (Once)      */
               p% = len(hs_po$)
               str(hs_po$,p%+1%,1%)  = "@"
               goto L31000

L31030:     errormsg$ = "(Error) - Update Hist for ( S.O. -- " &       ~
                                    hs_so$ & hs_ln$ & hs_yr$ &")?"
            gosub error_prompt

            errormsg$ = "(Err) - Update Hist ( P.O. -- " & hs_po$ &    ~
                                 hs_ln$ & hs_yr$ & ")?"
            gosub error_prompt

            finish% = 0%
            gosub display_stats
        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

                                       /* (EWDHIST) New Master History */
L35040:     FMT      CH(16),             /* Customer Job Name          */~
                     CH(9),              /* Customer Code              */~
                     CH(16),             /* Customer P.O. Number       */~
                     CH(3),              /* Sales Order Line Item      */~
                     CH(4),              /* History Data Year (YYYY)   */~
                     CH(5),              /* EWD Load Number            */~
                     CH(8),              /* Customer Sales Order       */~
                     CH(3),              /* hs_ln$                     */~
                     CH(4),              /* hs_yr$                     */~
                     CH(4),              /* Header Text Id             */~
                     CH(4),              /* Line Item Text Id          */~
                     CH(9),              /* hs_cust$                   */~
                     CH(6),              /* S.O. Order Date            */~
                     CH(8),              /* hs_so$                     */~
                     CH(3),              /* hs_ln$                     */~
                     CH(6),              /* Sales Order Ship Date      */~
                     CH(10),             /* EWD Quote Number           */~
                     CH(25),             /* MFG Part Number            */~
                     BI(2),              /* hs_oqty% - Order Ln Qty    */~
                     BI(2),              /* hs_sqty% - Shipped Ln Qty  */~
                     PD(15,4),           /* Header Discount Pcnt       */~
                     PD(14,4),           /* Line Item Discount Pcnt    */~
                     PD(14,4),           /* hs_uprice - Ln Unit Price  */~
                     PD(14,4),           /* hs_eprice - Ln Extnd Price */~
                     CH(4),              /* Ln Item Category Code      */~
                     CH(1),              /* Ln Item Price Code         */~
                     CH(4),              /* Region Code                */~
                     CH(4),              /* Salesman Code              */~
                     CH(8),              /* EWD Invoice Number         */~
                     CH(8),              /* Customer Check No.         */~
                     CH(6),              /* S.O. Due Date              */~
                     CH(20),             /* Subpart  AWD005            */~
                     CH(20),             /* Infopart AWD005            */~
                     CH(01),             /* Credit   AWD008            */~
                     CH(01)              /* Filler Area                */
                                         /* (EWDHIST)                  */

                                         /* (EWDWARR) - New File       */
        REM FMT      CH(8),              /* Warranty Id                */~
        REM          CH(3),              /* Line Item                  */~
        REM          CH(4),              /* Warranty Year              */~
        REM          CH(8),              /* Customer Sales Order       */~
        REM          CH(3),              /* Sales Order Line Item      */~
        REM          CH(4),              /* Sales Order Year           */~
        REM          CH(2)               /* Filler                     */

        REM FMT CH(1024)                 /* (EWDTEXTH) New History     */
        REM                              /*    Text Master. Same Format*/
        REM                              /*    asCaelus (TXTFILE)      */


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
              on fieldnr% gosub L40165,          /* Conversion Year    */~
                                L40160,          /* Move S.O. Data     */~
                                L40160,          /* Purge S.O. Data    */~
                                L40160,          /* Move Text Data     */~
                                L40160,          /* Purge Text Data    */~
                                L40160,          /* Move Planning Data */~
                                L40160,          /* Purge Planning Data*/~
                                L40160,          /* Move Stock Data    */~
                                L40160,          /* Purge Stock Data   */~
                                L40160,          /* Move  BNK files    */~
                                L40160           /* Purge BNK files    */

                 goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40165:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,25), fac(hex(a4)), apc$                   , ch(32),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Conversion Year (YYYY)    ?:",               ~
               at (04,32), fac(lfac$(1%)), scr_yr$              , ch(04),~
                                                                         ~
   at (05,02), "Move  (BCKMASTR/BCKLINES) Data to History (Y)es or (N)o?:",~
               at (05,60), fac(lfac$(2%)), bck_conv$            , ch(01),~
                                                                         ~
   at (06,02), "Purge (BCKMASTR/BCKLINES) Data From Live  (Y)es or (N)o?:",~
               at (06,60), fac(lfac$(3%)), bck_purge$           , ch(01),~
                                                                         ~
   at (07,02), "Move  (TXTFILE) Data to History           (Y)es or (N)o?:",~
               at (07,60), fac(lfac$(4%)), txt_conv$            , ch(01),~
                                                                         ~
   at (08,02), "Purge (TXTFILE) Data from Live            (Y)es or (N)o?:",~
               at (08,60), fac(lfac$(5%)), txt_purge$           , ch(01),~
                                                                         ~
   at (09,02), "Move(APCPLNOR/APCPLNSC/APCPLNSD/APCPLNWT) (Y)es or (N)o?:",~
               at (09,60), fac(lfac$(6%)), pln_conv$            , ch(01),~
                                                                         ~
   at (10,02), "Purge(APCPLNOR/APCPLNSC/APCPLNSD/APCPLNWT)(Y)es or (N)o?:",~
               at (10,60), fac(lfac$(7%)), pln_purge$           , ch(01),~
                                                                         ~
   at (11,02), "Move(Stock Sales Orders)                  (Y)es or (N)o?:",~
               at (11,60), fac(lfac$(8%)), stk_conv$            , ch(01),~
                                                                         ~
   at (12,02), "Purge(Stock Sales Orders)                 (Y)es or (N)o?:",~
               at (12,60), fac(lfac$(9%)), stk_purge$           , ch(01),~
                                                                         ~
   at (13,02), "Purge (BNKMASTR/BNKLINES) Data From Live  (Y)es or (N)o?:",~
               at (13,60), fac(lfac$(10%)), bnk_purge$           , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L40420
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
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     if fieldnr% > 1% then L40600
                str(pf$(1),19,19) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40600:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Build Hist  "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
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

        display_stats
            gosub set_pf2

            display                                                      ~
               at (01,02), hex(84), pname$,                              ~
               at (01,64), "Today:",                                     ~
               at (01,71), hex(84), date$,                               ~
               at (01,25), hex(84), apc$,                                ~
                                                                         ~
               at (02,02), "Start Time  :",                              ~
               at (02,17), hex(84), st_time$,                            ~
                                                                         ~
               at (03,02), "Current Time:",                              ~
               at (03,17), hex(84), cr_time$,                            ~
               at (03,58), hex(84), tt_total$,                           ~
                                                                         ~
               at (05,02), hex(84), tt$(1%),                             ~
               at (07,02), hex(84), tt$(2%),                             ~
               at (09,02), hex(84), tt$(3%),                             ~
               at (11,02), hex(84), tt$(4%),                             ~
               at (13,02), hex(84), tt$(5%),                             ~
               at (15,02), hex(84), tt$(6%),                             ~
               at (17,02), hex(84), tt$(7%),                             ~
               at (19,02), hex(84), tt$(8%),                             ~
               at (20,02), hex(84), tt$(9%),                             ~
               at (21,02), hex(84), tt$(10%),                            ~
               at (22,02), hex(84), tt$(11%),                            ~
               at (23,02), hex(84), tt$(12%),                            ~
                                                                         ~
               at (24,02), hex(a4),   tt$(13%)

               call "PAUSE" addr(100%)

            if finish% = 1% then goto L40800

        return

L40800: call "PRNTSCRN"
        call "PRNTSCRN"
        stop                                 /* Make Copy Final   */
        return clear all                     /* of Statistics     */
        goto exit_program

        set_pf2
            init(" ") cnt$()
            for kk% = 1% to 20%
                convert cnt%(kk%) to cnt$(kk%), pic(####,###)
            next kk%

            pf$(1) = "                                        " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)

            str(tt$(1%),12%,9%) = hs_cust$
            str(tt$(1%),29%,8%) = hs_so$
            str(tt$(1%),44%,16%)= hs_po$

            str(tt$(2%),38%,4%) = scr_yr$
            str(tt$(2%),52%,8%) = cnt$(1%)

            str(tt$(3%),34%,8%) = cnt$(10%)
            str(tt$(3%),52%,8%) = cnt$(2%)

            str(tt$(4%),34%,8%) = cnt$(12%)
            str(tt$(4%),52%,8%) = cnt$(11%)

            str(tt$(5%),34%,8%) = cnt$(4%)
            str(tt$(5%),52%,8%) = cnt$(3%)

            str(tt$(6%),16%,8%) = cnt$(13%)
            str(tt$(6%),34%,8%) = cnt$(14%)
            str(tt$(6%),52%,8%) = cnt$(15%)

            str(tt$(7%),34%,8%) = cnt$(5%)
            str(tt$(7%),52%,8%) = cnt$(6%)

            str(tt$(8%),34%,8%) = cnt$(16%)
            str(tt$(8%),52%,8%) = cnt$(7%)

            str(tt$(9%),52%,8%) = cnt$(8%)

            str(tt$(10%),52%,8%) = cnt$(9%)

            str(tt$(11%),52%,8%) = cnt$(17%)

            str(tt$(12%),52%,8%) = cnt$(18%)

            init(" ") cr_time$, cr_time_d$
            cr_time_d$ = time
            call "TIME" (cr_time$)

            convert str(st_time_d$,1%,2%) to tt_hr1%, data goto L48010
L48010:
            convert str(st_time_d$,3%,2%) to tt_mn1%, data goto L48020
L48020:
            if str(st_time_d$,5%,3%) > "30" then tt_mn1% = tt_mn1% + 1%

            convert str(cr_time_d$,1%,2%) to tt_hr2%, data goto L48030
L48030:
            convert str(cr_time_d$,3%,2%) to tt_mn2%, data goto L48040
L48040:
            if str(cr_time_d$,5%,3%) > "30" then tt_mn2% = tt_mn2% + 1%
                                               /* Convert begin and   */
            tt1% = (tt_hr1% * 60%) + tt_mn1%   /* End time to Minutes */
            tt2% = (tt_hr2% * 60%) + tt_mn2%

            if tt1% > tt2% then tt2% = tt2% + 1440% /* Correct with
                                               /* Minutes for (1) day  */
            tt3% = tt2% - tt1%                 /* Total Minutes to     */
                                               /* Complete Conversion  */
            tt_hr3% = int(tt3%/60%)            /* Total Minutes        */
            tt_mn3% = mod(tt3%,60%)

            convert tt_hr3% to tt_hr$, pic(###)/* Total Hours          */

            convert tt_mn3% to tt_mn$, pic(###)/* Total Minutes        */

            tt_total$ = "Total - xxx Hrs/Min xxx"
            str(tt_total$,9%,3%)  = tt_hr$
            str(tt_total$,21%,3%) = tt_mn$

           if finish% = 1% then                                         ~
              tt$(13%) = " F i n i s h e d   W i t h   H i s t o r y   B ~
u i l d   ???"

        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            init(" ") errormsg$
            on fieldnr% gosub L50000,             /* Conversion Year   */~
                              L50100,             /* Move S.O. to Hist */~
                              L50200,             /* Purge S.O. Data   */~
                              L50300,             /* Move S.O. Text    */~
                              L50400,             /* Purge S.O. Text   */~
                              L50500,             /* Move Planning Data*/~
                              L50600,             /* Purge Planning Data*/~
                              L50700,             /* Move Stock Data    */~
                              L50800,             /* Purge Stock Data   */~
                              L51000              /* Purge BNK to Hist */
            return

L50000: REM Conversion Year                       scr_yr$
           if scr_yr$ <> " " then goto L50010
              scr_yr$ = cur_yr$                /* Default Current Year*/
L50010:    if len(scr_yr$) <> 4 then goto L50040

           if scr_yr$ > cur_yr$ then goto L50040
        return
L50040:    errormsg$= "(Error)-Invalid Conversion Year( YYYY )?"
           gosub error_prompt
           init(" ") scr_yr$
        return

L50100: REM Move S.O. Data to History             bck_conv$
           if bck_conv$ <> " " then goto L50110
              bck_conv$ = "N"

L50110:    p% = pos("YN" = bck_conv$)
           if p% = 0% then goto L50120
        return
L50120:    errormsg$= "(Error)-Invalid S.O. Move to History (Y or N)?"
           gosub error_prompt
           init(" ") bck_conv$
        return

L50200: REM Purge Sales Order Data from Live      bck_purge$
           if bck_purge$ <> " " then goto L50210
              bck_purge$ = "N"

L50210:    p% = pos("YN" = bck_purge$)
           if p% = 0% then goto L50220
        return
L50220:    errormsg$= "(Error)-Invalid Purge Live S.O. Data (Y or N)?"
           gosub error_prompt
           init(" ") bck_purge$
        return

L50300: REM Move Text to History                  txt_conv$
           if txt_conv$ <> " " then goto L50310
              txt_conv$ = "N"

L50310:    p% = pos("YN" = txt_conv$)
           if p% = 0% then goto L50320
        return
L50320:    errormsg$= "(Error)-Invalid Text Move to History (Y or N)?"
           gosub error_prompt
           init(" ") txt_conv$
        return

L50400: REM Purge Text from Live Data             txt_purge$
           if txt_purge$ <> " " then goto L50410
              txt_purge$ = "N"

L50410:    p% = pos("YN" = txt_purge$)
           if p% = 0% then goto L50420
        return
L50420:    errormsg$= "(Error)-Invalid Text Data Purge Selection (Y or N)?"
           gosub error_prompt
           init(" ") txt_purge$
        return

L50500: REM Move Planning Data to History         pln_conv$
           if pln_conv$ <> " " then goto L50510
              pln_conv$ = "N"

L50510:    p% = pos("YN" = pln_conv$)
           if p% = 0% then goto L50520
        return
L50520:    errormsg$= "(Error)-Invalid Planning Data Move Selection (Y or N)?"
           gosub error_prompt
           init(" ") pln_conv$
        return

L50600: REM Purge Planning Data in Live Data      pln_purge$
           if pln_purge$ <> " " then goto L50610
              pln_purge$ = "N"

L50610:    p% = pos("YN" = pln_purge$)
           if p% = 0% then goto L50620
        return
L50620:    errormsg$= "(Error)-Invalid Planning Data Purge Selection (Y or N)?"
           gosub error_prompt
           init(" ") pln_purge$
        return

L50700: REM Move Stock Sales Orders to History         stk_conv$
           if stk_conv$ <> " " then goto L50710
              stk_conv$ = "N"

L50710:    p% = pos("YN" = stk_conv$)
           if p% = 0% then goto L50720
        return
L50720:    errormsg$= "(Error)-Invalid Stock Data Move Selection (Y or N)?"
           gosub error_prompt
           init(" ") stk_conv$
        return

L50800: REM Purge Stock Data in Live Data              stk_purge$
           if stk_purge$ <> " " then goto L50810
              stk_purge$ = "N"

L50810:    p% = pos("YN" = stk_purge$)
           if p% = 0% then goto L50820
        return
L50820:    errormsg$= "(Error)-Invalid Stock Data Purge Selection (Y or N)?"
           gosub error_prompt
           init(" ") stk_purge$
        return

L51000: REM Purge Sales Order Data from Live      bck_purge$
           if bnk_purge$ <> " " then goto L51010
              bnk_purge$ = "N"

L51010:    p% = pos("YN" = bnk_purge$)
           if p% = 0% then goto L51020
        return
L51020:    errormsg$= "(Error)-Invalid Purge Live S.O. Data (Y or N)?"
           gosub error_prompt
           init(" ") bnk_purge$
        return


        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        deffn'099(textid$)
            txt% = 0%
            if textid$ = hex(00000000) or textid$ = hex(ffffffff)        ~
                                           or textid$ = " " then return
            txt% = 1%
        return
                                              /* Move Text to New Text */
        move_text                             /* History Database      */
            init(" ") text_key$, sav_txt$, text_rec$(), text$
            gosub'099(txtid$)
            if txt% = 0% then return

            text_key$ = all(hex(00))          /* (TXTFILE)   (EWD001)  */
            str(text_key$,1%,1%) = "M"
            str(text_key$,2%,3%) = "   "
            str(text_key$,5%,4%) = txtid$
            str(text_key$,9%,1%) = "1"
            sav_txt$ = str(text_key$,1%,9%)

            read #5,hold,key > text_key$, using L60000, text_key$,       ~
                                                   eod goto move_txt_done
L60000:        FMT CH(11)
            if sav_txt$ <> str(text_key$,1%,9%) then goto move_txt_done

               get #5, using L60010, text$, text_rec$()
L60010:           FMT CH(64), 3*CH(70)
               if txt_purge$ = "N" then goto L60015
                  delete #5                  /* Purge(TXTFILE/TXTHIST) */
                  cnt%(3%) = cnt%(3%) + 1%       /* Count Deleted Txt  */
                                                 /* Records            */
L60015:        if txt_conv$  = "N" then goto move_txt_done
                  gosub get_next_id              /* Allocate New Id    */

                  str(text$,5%,4%)     = txtid$  /* Set New ID         */
                  str(text_key$,5%,4%) = txtid$
                  read #28,hold,key = text_key$, eod goto L60020
                     goto move_txt_done          /* Skip if Exists     */
                                                 /* Update (EWDTEXTH)  */
L60020:           write #28, using L60010 , text$, text_rec$(),          ~
                                                         eod goto L60025
                  cnt%(4%) = cnt%(4%) + 1%       /* Count Moved Text   */
        move_txt_done                            /*Only 1st three Lines*/

        return
L60025:     errormsg$ = "(Error)-Moving 'TEXT' to History?"
            gosub error_prompt
        return

        get_next_id                               /* Starting Text Id  */
            init(" ") readkey$                    /* 1% - 02/15/1999   */
            readkey$ = "N"

            read #28,hold,key = readkey$, using L60030, readkey$, n%,    ~
                                                       eod goto L60060
L60030:        FMT CH(11), BI(4)
            next_id% = n%
            n%       = next_id% + 1%              /* Increment Pointer */
            delete #28
            write #28, using L60040, "N", n%, " "
L60040:        FMT CH(11), BI(4), CH(49)

            put txtid$, using L60050, next_id%
L60050:        FMT BI(4)
        return
L60060:     text_n% = text_n% + 1%
            if text_n% > 1% then goto text_err_msg
            readkey$ = "N"
            write #28, using L60040, "N", 1%, " ", eod goto text_err_msg
            goto get_next_id
        text_err_msg
            errormsg$ = "(Error) Allocating Next Text Id?"
            gosub error_prompt
        return
                                               /* (APCPLNOR) (EWD001)  */
        lookup_planning_data                   /* Sales Order          */
           hs_inv$   = "99999999"              /* Invoice No.          */
           hs_chk$   = "99999999"              /* Check No.            */

              read #12,hold,key 4% = hs_so$, using L60070, hs_inv$,      ~
                                   hs_chk$, hs_load$, eod goto L60075
L60070:          FMT POS(70), 2*CH(8), POS(94), CH(5)
              cnt%(5%) = cnt%(5%) + 1%         /* Count Sales Orders   */
                                               /* Found in Planning    */
           if pln_purge$ = "N" then goto L60080
              delete #12                       /* Planning Sales Orders*/
              cnt%(6%) = cnt%(6%) + 1%         /* Purged               */
L60075:       if pln_purge$ = "N" then goto L60080
                 gosub purge_apcplnsc
                 gosub purge_apcplnsd
                 gosub purge_apcplndt
L60080: return

        purge_apcplnsc
           init(" ") prg_key$
           str(prg_key$,1%,8%) = hs_so$
L60090:    read #13,hold,key > prg_key$, using L60100, prg_key$,         ~
                                                    eod goto L60110
L60100:       FMT POS(24), CH(10)
           if str(prg_key$,1%,8%) <> hs_so$ then goto L60110
              cnt%(7%) = cnt%(7%) + 1%
              delete #13
              goto L60090
L60110: return

        purge_apcplnsd
           init(" ") prg_key$
           str(prg_key$,1%,8%) = hs_so$
L60120:    read #14,hold,key > prg_key$, using L60130, prg_key$,         ~
                                                     eod goto L60140
L60130:       FMT CH(23)
           if str(prg_key$,1%,8%) <> hs_so$ then goto L60140
              cnt%(8%) = cnt%(8%) + 1%
              delete #14
              goto L60120
L60140: return

        purge_apcplndt
           init(" ") prg_key$
           str(prg_key$,1%,8%) = hs_so$
L60150:    read #15,hold,key > prg_key$, using L60160, prg_key$,         ~
                                                      eod goto L60170
L60160:       FMT POS(24), CH(23)
           if str(prg_key$,1%,8%) <> hs_so$ then goto L60170
              cnt%(9%) = cnt%(9%) + 1%
              delete #15
              goto L60150
L60170: return
                                                /* (APCPLNWT) - File   */
        lookup_warranty                         /* For Each Sales Order*/
                                                /* an Line Item Check  */
           init(" ") wr_key$, wr_rec$, wr$()    /* for Warranty Id.    */
           init(" ") wr_rec1$
           wr% = 1% : rh1% = 1%
           hs_warranty$ = hs_so$
           wr$(wr%)     = hs_warranty$          /* Set Delete Array    */
           str(wr_rec$,1%,8%)  = hs_warranty$
           str(wr_rec$,9%,3%)  = hs_ln$
           str(wr_rec$,12%,4%) = hs_yr$
           str(wr_rec$,16%,8%) = hs_so$
           str(wr_rec$,24%,3%) = hs_ln$
           str(wr_rec$,27%,4%) = hs_yr$
           str(wr_rec$,31%,2%) = "  "

           str(wr_key$,1%,8%) = hs_so$
                                                 /* (Zero) Filled      */

        REM   str(wr_key$,9%,2%) = str(hs_ln0$,2%,2%)
           read #16,key 1% > wr_key$, using L60180, wr_rec1$,            ~
                                                 eod goto warranty_done
L60180:       FMT CH(128)                        /* Alt Key Planning   */
                                                 /* Barcode            */
           goto L60185
        lookup_warranty_next
           read #16, using L60180, wr_rec1$, eod goto warranty_done                ~

L60185:    if str(wr_rec1$,9%,8%) <> hs_so$ then goto warranty_done
                                                    /* (Zero) Filled   */
           if str(wr_rec1$,17%,2%) <> str(hs_ln0$,2%,2%) then            ~
                                             goto lookup_warranty_next
                                                    /* (EWD002)        */
              hs_warranty$ = str(wr_rec1$,1%,8%)
              wr$(wr%)           = hs_warranty$
              cnt%(13%) = cnt%(13%) + 1%     /* Warranty Records Found */
              rh1% = wr%
              wr% = wr% + 1%                 /* Set Up for the Next    */
                                             /* (AWD004) Fix Problem   */
              if wr% > 32765% then rh1%,wr% = 32765%     /* Max Array  */
                                             /* (AWD004)               */
              goto lookup_warranty_next

        warranty_done
           gosub update_warranty_new         /* At Least Write (1)     */

           gosub warranty_purge              /* Check for Purge        */

           rh3% = rh1% - rh2%                /* Skip Dups is Ok        */
           if rh3% < 0% then gosub L60500
        return

        update_warranty_new                    /* (EWDWARR) - New File */
                                               /* If a Record not found*/
                                               /* in (APCPLNWT) at least*/
                                               /* (1) warranty rec will*/
                                               /* be created.          */
          rh2% = 0%
          if bck_conv$ = "N" then goto L60230
             for ww% = 1% to rh1%
                 init(" ") wr_key1$, hs_warranty$
                 hs_warranty$ = wr$(ww%)
                 str(wr_rec$,1%,8%)   = hs_warranty$

                 str(wr_key1$,1%,8%)  = hs_warranty$
                 str(wr_key1$,9%,3%)  = hs_ln$
                 str(wr_key1$,12%,4%) = hs_yr$     /* Exist's Skip         */

REM FF% = 11%
REM IF SCR_YR$ >= "2013" THEN FF% = 20%

              read #27,hold,key = wr_key1$, eod goto L60200
                 goto L60220                    /* Skip when exists     */

L60200:       put #27, using L60210, wr_rec$
L60210:             FMT CH(32)
              write #27, eod goto L60240         /* No Dups Allowed    */
              rh2% = rh2% + 1%
              cnt%(14%) = cnt%(14%) + 1%         /* Count Warranty     */
L60220:    next ww%                              /* Records Created    */

L60230: return
L60240:    errormsg$ = "(Error) Updating Warranty Info for " &           ~
                        str(wr_key1$,1%,15%)
           gosub error_prompt
        goto L60220                              /* Continue Update    */

        warranty_purge                           /* (APCPLNWT) - file  */
           if bck_purge$ = "N" then goto L60310
           for ww% = 1% to rh1%                  /* No. Recs to Purge  */
               read #16,hold,key = wr$(ww%), eod goto L60300
                  delete #16
                  cnt%(15%) = cnt%(15%) + 1%     /* Records Purged     */

L60300:    next ww%
L60310: return

L60500:    errormsg$ = "(Error) EWDWARR-Found = [xxxxxx] Written = [xxxxxx]"
           convert rh1% to str(errormsg$,26%,6%), pic(######)

           convert rh2% to str(errormsg$,45%,6%), pic(######)
           gosub error_prompt
           init(" ") errormsg$
        return

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
        return


/* (AWD005) */
        lookup_bcksubpt
          read #17, hold, key = bcksubpt_key$, eod goto no_bcksubpt

                get #17, using bcksubpt_fmt, hs_subpart$, hs_infpart$
bcksubpt_fmt:          FMT POS(48), CH(20), POS(132), CH(20)

                if bck_purge$ = "N" then no_bcksubpt
                        delete #17

        no_bcksubpt
        return
/* (AWD005/) */

        SO_NUMBER
           call "SHOSTAT"( " SO " & bck_key$) : stop

        return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program

            end

