        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCRGA12                             *~
            *  Creation Date     - 11/29/95                             *~
            *  Last Modified Date- 01/11/06                             *~
            *  Description       - Program to Print RGA forms.          *~
            *                                                           *~
            *  Special Notes     -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/29/95 ! New Program for (APC) - Last Mod Date    ! JBF *~
            * 04/04/96 ! Change Reason Code to 3 bytes (Complaint)! JBF *~
            *          !                                          !     *~
            * 10/31/97 ! Changed Program Version ID To 60403      ! DJD *~
            * 01/11/06 ! (PAR000) CR347 mods for sub part         ! CMG *~
            *          !                                          !     *~
            *************************************************************
        dim                                                              ~
            rga_cuscode$9,               /* RGA Customer No.           */~
            rga_hd_status$2,             /* RGA Item Status Code       */~
            rga_number$4,                /* RGA Number                 */~
            rga_auth_id$3,               /* Authorizing Userid         */~
            rga_dte$8,                   /* RGA Enter Date             */~
            rga_filed_dte$8,             /* RGA Filed Date             */~
            rga_hd_desc_txt$4,           /* RGA Header Text Code       */~
            rga_hd_txt$1,                /* RGA Header Text Flag       */~
            rga_userid$3,                /* Userid of Entry/Mod        */~
            rga_mod_dte$8,               /* RGA Entry/Mod Date         */~
            rga_rg$1,                    /* RGA/RG Flag                */~
            rga_filler$29,               /* APCRGAHD Filler Area       */~
            rga_dt_number$4,             /* RGA Number (Detail)        */~
            rga_item$2,                  /* RGA Item                   */~
            rga_reason$3,                /* RGA Item Reason Code       */~
            rga_part$25,                 /* RGA Item Part No.          */~
            rga_so$8,                    /* RGA S.O. No.               */~
            rga_line$2,                  /* RGA S.O. Line No.          */~
            rga_po$16,                   /* RGA P.O. No.               */~
            rga_salesman$4,              /* RGA Salesman               */~
            dtlkey$6,                    /* APCRGADT File Read Key     */~
            hdrkey1$6,                   /* APCRGAHD File Read (Alt 1) */~
            bck_key$19,                  /* BCKMASTR/BCKLINES Key      */~
            bck_text$4,                  /* BCKLINES Order Text ID     */~
            apc_scr$120,                 /* Screen Description         */~
            apc_prt$60,                  /* Print Description          */~
            apc_sze$20,                  /* Size Long Form             */~
            cus_desc$30,                 /* Customer Description       */~
            cus_route$5,                 /* Customer Route             */~
            salesman_name$30,            /* Salesman Description       */~
            part_desc$32,                /* Part Description           */~
            text_desc$60,                /* Line Item Text             */~
            text_key$11,                 /* Text File Key              */~
            sav_key1$11,                 /* Text File Save Key         */~
            atext$(2)70,                 /* Text (2) Lines             */~
            dt_flag$1,                   /* Line Item Text as Part Desc*/~
            title$45,                    /* Report Title Field         */~
            date$8,                      /* Date for screen display    */~
            userid$3                     /* Current User Id            */

        dim f2%(11%),                    /* = 0 if the file is open    */~
            f1%(11%),                    /* = 1 if READ was successful */~
            fs%(11%),                    /* = 1 if file open, -1 if it */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(11%)20                 /* Text from file opening     */

        dim workdate8$8                  /* standard mm-dd-yy format   */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 10/31/97 RGA Forms Print Program    "
        REM *************************************************************
            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCRGADT ! RGA Detail File                          *~
            * #2  ! APCRGAHD ! RGA Header File                          *~
            * #3  ! CUSTOMER ! Caelus Customer Master File              *~
            * #4  ! SLMMASTR ! Salesman Master File                     *~
            * #5  ! USERLCMS ! Caelus User ID  Master File              *~
            * #8  ! BCKLINES ! S.O. Line Items File                     *~
            * #9  ! HNYMASTR ! Inventory Master File                    *~
            * #10 ! TXTFILE  ! Text File                                *~
            * #11 ! AMTBOMIF ! Inventory Validity Check                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,  "APCRGADT",                                      ~
/*PAR000*/              varc,     indexed,  recsize =  512,              ~
                        keypos =   12, keylen =    6,                    ~
                        alt key  1, keypos =  10, keylen =   8,          ~
                            key  2, keypos =   1, keylen =  17

            select #2,  "APCRGAHD",                                      ~
                        varc,     indexed,  recsize =   80,              ~
                        keypos =   12, keylen =    4,                    ~
                        alt key  1, keypos =  10, keylen =   6,          ~
                            key  2, keypos =   1, keylen =  15

            select #3,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =    1, keylen =    9,                    ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #4,  "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4

            select #5,  "USERLCMS",                                      ~
                        varc,     indexed,  recsize =   400,             ~
                        keypos =    1, keylen =   3,                     ~
                        alt key  1, keypos =    4, keylen =  30, dup

            select #8,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =   19

            select #9,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #10, "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =   11

            select #11, "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1%),  f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2,  fs%(2%),  f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3,  fs%(3%),  f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4,  fs%(4%),  f2%(4%),  0%, rslt$(4%))
            call "OPENOLIB" (#5, "SHARE",   f2%(5%),      rslt$(5%), axd$)
            call "OPENCHCK" (#8,  fs%(8%),  f2%(8%),  0%, rslt$(8%))
            call "OPENCHCK" (#9,  fs%(9%),  f2%(9%),  0%, rslt$(9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 0%, rslt$(11%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)

            date$ = date
            call "DATEFMT" (date$)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        REM INPUTMODE
            gosub begin_process

            goto exit_program

        REM *************************************************************~
            *               P R O C E S S   D A T A                     *~
            *************************************************************
        begin_process
            call "SHOSTAT" ("Creating RGA Forms ")
            gosub select_log_report

            hdrkey1$            =  all(hex(20))
            str(hdrkey1$,1%,2%) = "02"

        next_apcrgahd
            gosub initialize_variables

            read #2,key 1% > hdrkey1$, eod goto L19390

            get  #2, using L35120,                                        ~
                     rga_cuscode$,       /* Customer No.               */~
                     rga_hd_status$,     /* Status of RGA Header       */~
                     rga_number$,        /* RGA Number                 */~
                     rga_auth_id$,       /* Authorizing Userid         */~
                     rga_dte$,           /* RGA Enter Date             */~
                     rga_filed_dte$,     /* RGA Filed Date             */~
                     rga_hd_desc_txt$,   /* RGA Header Text Code       */~
                     rga_hd_txt$,        /* RGA Header Text Flag       */~
                     rga_userid$,        /* Userid of Entry/Mod        */~
                     rga_mod_dte$,       /* RGA Entry/Mod Date         */~
                     rga_rg$,            /* RGA/RG Flag                */~
                     rga_filler$         /* APCRGAHD Filler Area       */

            if rga_hd_status$   > "02"     then L19390
            str(hdrkey1$,1%,2%) =  rga_hd_status$
            str(hdrkey1$,3%,4%) =  rga_number$
/* Y2K */
            workdate8$ = rga_dte$
            call "DATEFMT" (workdate8$)
/* Y2K */
            gosub process_rga_form
            gosub print_footer
            gosub update_apcrgahd

            goto next_apcrgahd
L19390: return

        process_rga_form
            dtlkey$            = all(hex(20))
            str(dtlkey$,1%,4%) = rga_number$

        next_apcrgadt
            read #1,key > dtlkey$, eod goto L19640

            get  #1, using L35030,                                        ~
                     rga_dt_number$,     /* RGA Number (Detail)        */~
                     rga_item$,          /* RGA Item                   */~
                     rga_reason$,        /* RGA Reason Code            */~
                     rga_part$,          /* RGA Part No.               */~
                     rga_so$,            /* RGA S.O. No.               */~
                     rga_line$,          /* RGA S.O. Line No.          */~
                     rga_po$,            /* RGA P.O. No.               */~
                     rga_salesman$       /* RGA Salesman               */

            if rga_dt_number$ <>  rga_number$ then L19640
            str(dtlkey$,1%,4%) =  rga_dt_number$
            str(dtlkey$,5%,2%) =  rga_item$
            gosub print_apcrgadt

            goto next_apcrgadt
L19640: return

        update_apcrgahd
            read   #2,hold,key = rga_number$, eod goto L19920

            delete #2

            rga_hd_status$        = "03"
            rga_userid$           = userid$
            str(rga_mod_dte$,1,6) = date

/* Y2K */
REM            call "DATUNFMT" (rga_dte$)
/* Y2K */
            put    #2, using L35120,                                      ~
                     rga_cuscode$,       /* Customer No.               */~
                     rga_hd_status$,     /* Status of RGA Header       */~
                     rga_number$,        /* RGA Number                 */~
                     rga_auth_id$,       /* Authorizing Userid         */~
                     rga_dte$,           /* RGA Enter Date             */~
                     rga_filed_dte$,     /* RGA Filed Date             */~
                     rga_hd_desc_txt$,   /* RGA Header Text Code       */~
                     rga_hd_txt$,        /* RGA Header Text Flag       */~
                     rga_userid$,        /* Userid of Entry/Mod        */~
                     rga_mod_dte$,       /* RGA Entry/Mod Date         */~
                     rga_rg$,            /* RGA/RG Flag                */~
                     rga_filler$         /* APCRGAHD Filler Area       */

            write  #2, eod goto L19920

L19920: return

        select_log_report
            title$  = "****  Returned Goods Authorization Form  ****"
            pageno% = 0%
            lcnt%   = 99%
            call "TIME"    (xtime$)
            call "SETPRNT" (" ", "RG12", 0%, 0%)

            select printer(134)
        return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") rga_reason$, rga_dt_number$, rga_cuscode$,         ~
                rga_part$, salesman_name$, rga_item$, rga_salesman$,     ~
                rga_so$, rga_po$, part_desc$
            pageno% = 0% : rec% = 0% : lcnt% = 99%
        return

        REM *************************************************************~
            *               F O R M A T    S T A T E M E N T S          *~
            *************************************************************
L35030:     FMT POS(12), CH(04),         /* RGA No.                    */~
                CH(02),                  /* RGA Item No.               */~
                XX(05),  CH(03),         /* RGA Reason Code            */~
                XX(03),  CH(25),         /* RGA Part No.               */~
                CH(08),                  /* RGA S.O. No.               */~
                CH(02),                  /* RGA S.O. Line No.          */~
                XX(08),  CH(16),         /* RGA P.O. No.               */~
                XX(136), CH(04)          /* RGA Salesman               */

L35120:     FMT CH(09),                  /* RGA Customer               */~
                CH(02),                  /* RGA Header Status          */~
                CH(04),                  /* RGA Number                 */~
                CH(03),                  /* Authorizing Userid         */~
                CH(08),                  /* RGA Entry Date             */~
                CH(08),                  /* RGA Filed Date             */~
                CH(04),                  /* RGA Header Text Code       */~
                CH(01),                  /* RGA Header Text Flag       */~
                CH(03),                  /* Userid of Mod/Entry        */~
                CH(08),                  /* RGA Mod/Entry Date         */~
                CH(01),                  /* RGA/RG Flag                */~
                CH(29)                   /* APCRGAHD Filler Area       */

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************
                                          /* RGA Forms Print Columns   */
L50040: %                                                                ~

                                          /* Header Format             */
L50060: % ######## @ ########   ######################################## ~
        ~   Page: ###

L50090: % RGA No.   : ####
L50100: % Issue Date: ########
L50110: % Issued By : ##############################
L50120: % Salesman  : ##############################
L50130: % Customer  : #########  ##############################  Route: #~
        ~####
L50150: % S.O. No.  : ########   P.O. No.: ################
L50160: %!      !                                  !      !        !     ~
        ~           !
L50180: %! Item ! Part Number                      ! Rea. ! Credit ! Note~
        ~ Damages   !
L50200: %! ==== ! ================================ ! ==== ! ====== ! ====~
        ~========== !
L50220: %+_______________________________________________________________~
        ~___________+
                                    /* Detail Format                   */
L50250: %!  ##  ! #########################        ! ###  ! ______ ! ____~
        ~__________ !
L50270: %!      ! ################################ !      !        ! ____~
        ~__________ !
                                    /* Footer Format                   */
L50300: % Customer Signature  : _______________________________  Date: __~
        ~__________
L50320: % APC Driver Signature: _______________________________  Date: __~
        ~__________
L50340: % Pick Up Attempt 1   : Initials - Cust. - ____________  Driver -~
        ~ _________
L50360: % Pick Up Attempt 2   : Initials - Cust. - ____________  Driver -~
        ~ _________
L50380: % Pick Up Attempt 3   : Initials - Cust. - ____________  Driver -~
        ~ _________

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************
        print_apcrgadt
            if  rec% = 1%         then L60100
                rec% = 1%
                gosub lookup_name
                gosub lookup_salesman
                gosub lookup_customer

L60100:     if lcnt% > 49%        then gosub print_header

            if  rga_part$   > " " then L60150
                rga_part$   = "_________________________       "
                part_desc$  = "________________________________"
L60150:     if  rga_part$   > " " then gosub lookup_part

            if  rga_reason$ > " " then L60190
                rga_reason$ = "___"
L60190:     print using L50250, rga_item$, rga_part$, rga_reason$
            print using L50270, part_desc$
            print using L50160
            lcnt% = lcnt% + 3%
        return

        print_header
            if pageno% = 0%    then L60290
                print using L50040
                print using L50220
L60290:     pageno% = pageno% + 1%
            print page
            for l% = 1% to 7%
                print using L50040

            next l%

            print using L50060, date$, xtime$, title$, pageno%
            print using L50040
            print using L50090, rga_number$
            if pageno% > 1%    then L60490
            print using L50100, workdate8$
            print using L50110, auth_desc$
            print using L50120, salesman_name$
            print using L50130, rga_cuscode$, cus_desc$, cus_route$
            if  rga_so$ > " "  then L60460
                rga_so$ = "________"
L60460:     if  rga_po$ > " "  then L60480
                rga_po$ = "________________"
L60480:     print using L50150, rga_so$,      rga_po$
L60490:     print using L50040
            print using L50220
            print using L50180
            print using L50200
            print using L50160
            if pageno% > 1%    then L60570
            lcnt% = 20%
            goto L60580
L60570:     lcnt% = 15%
L60580: return

        print_footer
            if lcnt% > 49%     then L60670
            for l% = 1% to (49% - lcnt%)
                print using L50160

            next l%

L60670:     print using L50220
            print using L50040
            print using L50040
            print using L50300
            print using L50040
            print using L50320
            print using L50040
            print using L50340
            print using L50040
            print using L50360
            print using L50040
            print using L50380
        return

        lookup_customer
            read #3,key = rga_cuscode$, using L60840, cus_desc$,          ~
                cus_route$, eod goto L60870
L60840:         FMT POS(10), CH(30), XX(940), CH(05)

        return
L60870:     init(" ") cus_desc$
        return

        lookup_salesman
            read #4,key = rga_salesman$, using L60930, salesman_name$,    ~
                eod goto L60960
L60930:         FMT POS(05), CH(30)

        return
L60960:     salesman_name$ = "No Salesman Name             "
        return

        lookup_name
            read #5,key = rga_auth_id$, using L61020, auth_desc$,         ~
                eod goto L61050
L61020:         FMT POS(4), CH(30)

        return
L61050:     auth_desc$ = "No Authorizing Name"
        return

        lookup_part                           /* Check HNYMASTR        */
            init(" ") part_desc$, apc_prt$, apc_sze$, apc_scr$, dt_flag$
            read #9,key = rga_part$, using L61120, part_desc$, apc_prt$,  ~
                apc_sze$, eod goto L61150
L61120:         FMT XX(25), CH(32), POS(606), CH(60), CH(20)

            goto L61290
L61150:     err% = 0%
            if len(rga_part$) > 18% then L61240
                part_desc$ = "COMPONENT PART"
                gosub lookup_bcklines
                gosub lookup_text

                if dt_flag$ = "Y"   then part_desc$ = text_desc$
                goto L61290

L61240:     call "APCDESCR" (rga_part$, apc_scr$, apc_prt$, apc_sze$,    ~
                             #11, err%)

            str(part_desc$,1%,16%)  = str(apc_prt$,1%,16%)
            str(part_desc$,17%,16%) = str(apc_sze$,1%,16%)
L61290: return

        lookup_bcklines
            bck_text$            = " "
            bck_key$             = all(hex(20))
            str(bck_key$, 1%,8%) = rga_so$
            convert rga_line$ to xx%, data goto L61360
L61360:
            convert xx%       to str(bck_key$,17%,3%), pic(###)

            read #8,key = bck_key$, using L61410, bck_text$,              ~
                eod goto L61430
L61410:         FMT POS(242), CH(04)

L61430: return

        lookup_text                           /* Look Up Text Id       */
            init(" ") text_desc$, textid$, text_key$, sav_key1$, atext$()
            textid$ = bck_text$
            gosub'099(textid$)

            if txt% = 0% then L61660
            text_key$            = all(hex(00))
            str(text_key$,1%,1%) = "M"
            str(text_key$,2%,3%) = "   "
            str(text_key$,5%,4%) = textid$
            str(text_key$,9%,1%) = "1"
            sav_key1$            = text_key$
            read #10,key > text_key$, eod goto L61660

            get  #10, using L61600, text_key$, atext$()
L61600:         FMT CH(11), POS(64), 2*CH(70)

            if str(sav_key1$,1%,9%) <> str(text_key$,1%,9%) then L61660
            if atext$(1)  <> " " then text_desc$ = str(atext$(1),1%,60%) ~
                                 else text_desc$ = str(atext$(2),1%,60%)
            if text_desc$ <> " " then dt_flag$ = "Y"
L61660: return

        deffn'099(txt$)
            txt% = 0%
            if txt$ = hex(00000000) or txt$ = hex(ffffffff) or txt$ = " "~
                then L61730
            txt% = 1%
L61730: return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*
        exit_program
            call "SHOSTAT" ("One Moment Please")
            close printer

            end
