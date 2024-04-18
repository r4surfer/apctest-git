        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCRG28B  (Like APCRGA8B)            *~
            *  Creation Date     - 01/21/97                             *~
            *  Last Modified Date- 01/11/06                             *~
            *  Description       - Subroutine to print RGA's by         *~
            *                      Status Code Report.                  *~
            *                                                           *~
            *  Special Notes     - Subroutine of APCRGA24               *~
            *                      Uses NEW Planning Files              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/21/97 ! New Program for (APC) - Last Mod Date    ! JBF *~
            * 02/26/98 ! y2k compliant                            ! DJD *~
            * 01/11/06 ! (PAR000) CR347 mods for sub part         ! CMG *~
            *************************************************************

        sub "APCRG28B"  (#1,             /* GENCODES File Channel      */~
                         #2,             /* CUSTOMER File Channel      */~
                         rpt_value$,     /* Status Code or 'ALL'       */~
                         rpt_sum$)       /* Summary  "S" or "D"        */

        dim                                                              ~
            rga_cuscode$9,               /* RGA Customer No.           */~
            rga_hd_status$2,             /* RGA Header Status          */~
            rga_number$4,                /* RGA No.                    */~
            rga_auth_id$3,               /* RGA Authoried by ID        */~
            rga_dte$8,                   /* RGA Date Entered           */~
            rga_dt_status$2,             /* RGA Detail Status          */~
            rga_dt_num$4,                /* RGA No. (Detail)           */~
            rga_item$2,                  /* RGA Item No.               */~
            rga_part$25,                 /* RGA Part No.               */~
            rga_so$8,                    /* RGA Sales Order            */~
            rga_line$2,                  /* RGA S.O. Line              */~
            rga_piece$4,                 /* RGA S.O. Piece Count       */~
            rga_qty$4,                   /* RGA S.O. Order Quantity    */~
            rga_po$16,                   /* RGA P.O. No.               */~
            proc$1,                      /* Process Loop Flag          */~
            hdrkey1$6,                   /* APCRGAHD Read Key (Alt 1)  */~
            dtlkey1$8,                   /* APCRGADT Read Key (Alt 1)  */~
            readkey$25,                  /* GENCODES Read Key          */~
            sc_key$10,                   /* APCPLNSC Read Key          */~
            sc_txt$4,                    /* APCPLNSC Line Text ID      */~
            sav_status$2,                /* Save RGA Status Code       */~
            cus_desc$30,                 /* Customer Description       */~
            status_desc$30,              /* Status Code Description    */~
            status_hd_desc$30,           /* Status Code Description    */~
            part_desc$32,                /* Part Description           */~
            text_desc$60,                /* Line Item Text             */~
            text_key$11,                 /* Text File Key              */~
            sav_key1$11,                 /* Text File Save Key         */~
            atext$(2)70,                 /* Text (2) Lines             */~
            apc_prt$60,                  /* Print Description          */~
            apc_sze$20,                  /* Size Long Form             */~
            apc_scr$120,                 /* Screen Description         */~
            dt_flag$1,                   /* Text as Part Description   */~
            title$54,                    /* Report Title Field         */~
            date$8,                      /* Date for screen display    */~
            userid$3                     /* Current User Id            */

        dim f2%(11%),                    /* = 0 if the file is open    */~
            f1%(11%),                    /* = 1 if READ was successful */~
            fs%(11%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(11%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "01.00.00 11/17/95 RGA by Status Code Report     "
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
            * #1  ! GENCODES ! System Code Table File                   *~
            * #2  ! CUSTOMER ! Customer Master File                     *~
            * #3  ! APCRGAHD ! RGA Header File                          *~
            * #4  ! APCRGADT ! RGA Detail File                          *~
            * #5  ! TXTFILE  ! Text File                                *~
            * #8  ! APCPLNSC ! Planning Master Scheduling File          *~
            * #9  ! HNYMASTR ! Inventory Master File                    *~
            * #11 ! AMTBOMIF ! Inventory Validity Check                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #3,  "APCRGAHD",                                      ~
                        varc,     indexed,  recsize =   80,              ~
                        keypos =   12, keylen =   4,                     ~
                        alt key  1, keypos =  10, keylen =   6,          ~
                            key  2, keypos =   1, keylen =  15

            select #4,  "APCRGADT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   12, keylen =    6,                    ~
                        alt key  1, keypos =  10, keylen =   8,          ~
                            key  2, keypos =   1, keylen =  17

            select #5,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =   11

            select #8,  "APCPLNSC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   24, keylen =   10,                    ~
                        alt key  1, keypos =    7, keylen =  27,         ~
                            key  2, keypos =    1, keylen =  33

            select #9,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #11, "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#3,  fs%(3%),  f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4,  fs%(4%),  f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5,  fs%(5%),  f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#8,  fs%(8%),  f2%(8%),  0%, rslt$(8%))
            call "OPENCHCK" (#9,  fs%(9%),  f2%(9%),  0%, rslt$(9%))
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
            gosub initialize_variables
            gosub select_report
            gosub begin_process

            goto exit_sub

        REM *************************************************************~
            *               P R O C E S S   D A T A                     *~
            *************************************************************
        begin_process
            call "SHOSTAT" ("Creating RGA's by Status Code Report")

            if rpt_sum$ = "D"                then L19100
            if str(rpt_value$,1%,3%) = "ALL" then gosub select_h_all     ~
                                             else gosub select_h_one
            goto L19120
L19100:     if str(rpt_value$,1%,3%) = "ALL" then gosub select_all       ~
                                             else gosub select_one
L19120:     gosub end_report

        return

        select_h_one
            str(hdrkey1$,1%,2%) = str(rpt_value$,1%,2%)

        select_h_all
            read #3,key 1% > hdrkey1$, eod goto L19420

            get  #3, using L35030, rga_cuscode$,    /* RGA Customer     */~
                                  rga_hd_status$,  /* RGA Status       */~
                                  rga_number$,     /* RGA No.          */~
                                  rga_auth_id$,    /* RGA Authorized By*/~
                                  rga_dte$         /* RGA Date Entered */

            if proc$        = "1"             then L19330
                proc$       = "1"
                sav_status$ = rga_hd_status$
                gosub lookup_hd_status

L19330:     if sav_status$  = rga_hd_status$  then L19370
            if str(rpt_value$,1%,3%) <> "ALL" then L19420
                gosub end_selection

L19370:     str(hdrkey1$,1%,2%) = rga_hd_status$
            str(hdrkey1$,3%,4%) = rga_number$
            gosub print_apcrgahd

            goto select_h_all
L19420: return

        select_one
            str(dtlkey1$,1%,2%) = str(rpt_value$,1%,2%)

        select_all
            read #4,key 1% > dtlkey1$, eod goto L19760

            get  #4, using L35090, rga_cuscode$,   /* RGA Customer No.  */~
                                  rga_dt_status$, /* RGA Item Status   */~
                                  rga_dt_num$,    /* RGA No.           */~
                                  rga_item$,      /* RGA Item No.      */~
                                  rga_part$,      /* RGA Part No.      */~
                                  rga_so$,        /* RGA S.O. No.      */~
                                  rga_line$,      /* RGA S.O. Line     */~
                                  rga_piece$,     /* RGA S.O. Piece Cnt*/~
                                  rga_qty$,       /* RGA S.O. Order Qty*/~
                                  rga_po$         /* RGA P.O. No.      */

            if proc$        = "1"             then L19660
                proc$       = "1"
                sav_status$ = rga_dt_status$
                gosub lookup_status

L19660:     if sav_status$  = rga_dt_status$  then L19700
            if str(rpt_value$,1%,3%) <> "ALL" then L19760
                gosub end_selection

L19700:     str(dtlkey1$,1%,2%) = rga_dt_status$
            str(dtlkey1$,3%,4%) = rga_dt_num$
            str(dtlkey1$,7%,2%) = rga_item$
            gosub print_apcrgadt

            goto select_all
L19760: return

        select_report
            if rpt_sum$ = "S" then L19830
            title$  = "APC Building Products - RGA's by Status Code  (Det~
        ~ail)"
            goto L19850
L19830:     title$  = "APC Building Products - RGA's by Status Code  (Sum~
        ~mary)"
L19850:     pageno% = 0%
            lcnt%   = 99%
            call "TIME"    (xtime$)
            call "SETPRNT" (" ", "RGA8", 0%, 0%)

            select printer(134)
        return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") rga_number$, rga_item$, rga_cuscode$, rga_auth_id$,~
                rga_dte$, rga_po$, status_desc$, rga_dt_num$, rga_item$, ~
                rga_so$, rga_line$, rga_piece$, rga_qty$, cus_desc$,     ~
                rga_hd_status$, rga_dt_status$, sav_status$, proc$,      ~
                xtime$, status_hd_desc$, sc_txt$
            hdrkey1$   = all(hex(20))
            dtlkey1$   = all(hex(20))
            rga_total% = 0% : status_total% = 0%
        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
L35030:     FMT CH(09),                  /* RGA Customer               */~
                CH(02),                  /* RGA Header Status          */~
                CH(04),                  /* RGA Number                 */~
                CH(03),                  /* Authorizing Userid         */~
                CH(08)                   /* RGA Entry Date             */

L35090:     FMT CH(09),                  /* RGA Customer No.           */~
                CH(02),                  /* RGA Item Status            */~
                CH(04),                  /* RGA No.                    */~
                CH(02),                  /* RGA Item                   */~
                XX(11),   CH(25),        /* RGA Part No.               */~
                CH(08),                  /* RGA Sales Order No.        */~
                CH(02),                  /* RGA Sales Order Line Item  */~
                CH(04),                  /* RGA Sales Order Piece Ct.  */~
                CH(04),                  /* RGA Sales Order Quantity   */~
                CH(16)                   /* RGA Purchase Order No.     */

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************
                                    /* RGA's by Status Code Columns    */
                                    /* Header Format                   */
L55060: %  ######## @ ########            ###############################~
        ~#######################            Page: ###

                                    /* APCRGAHD Format                 */
L55100: %  Status: ##   ##############################
L55110: %  RGA      Auth. ID     Date Entered     Customer               ~
        ~            Status
L55130: %+ ----  !  --------  !  ------------  !  -----------------------~
        ~-------  !  ------------------------------  +
L55150: %! ####  !    ###     !    ########    !  #######################~
        ~#######  !  ##############################  !

L55180: %  Total RGA's for Status Code ## : ####
L55190: %  Total RGA Items for Status Code ## : ####

                                    /* APCRGADT Format                 */
L55220: %! RGA   Item  Customer                        S.O. No.  Line    ~
        ~ P.O. No.        Part Description
L55240: %+ ---- ! -- ! ----------------------------- ! -------- ! -- ! --~
        ~-------------- ! ------------------------------ +
L55260: %! #### ! ## ! ############################# ! ######## ! ## ! ##~
        ~############## ! ############################## !

L55290: %         *****  End Of RGA's by Status Code Report  (Summary) **~
        ~***
L55310: %         *****  End Of RGA's by Status Code Report  (Detail) ***~
        ~**

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************
        print_apcrgahd
            call "DATEFMT" (rga_dte$)
            gosub lookup_customer

            if lcnt% > 55% then gosub print_header

            print using L55150, rga_number$, rga_auth_id$, rga_dte$,      ~
                cus_desc$, status_hd_desc$
            lcnt%      = lcnt%      + 1%
            rga_total% = rga_total% + 1%
        return

        print_apcrgadt
            gosub lookup_customer
            gosub lookup_part

            if lcnt% > 55% then gosub print_header

            print using L55260, rga_dt_num$, rga_item$, cus_desc$,        ~
                rga_so$, rga_line$, rga_po$, part_desc$
            lcnt%         = lcnt%         + 1%
            status_total% = status_total% + 1%
        return

        print_header
            pageno% = pageno% + 1%
            print page
            print using L55060, date$, xtime$, title$, pageno%
            print
            if rpt_sum$ = "D" then L60350
            print using L55100, rga_hd_status$, status_hd_desc$
            goto L60360
L60350:     print using L55100, rga_dt_status$, status_desc$
L60360:     print
            lcnt% = 6%
            if rpt_sum$ = "D" then L60420
            print using L55110
            print using L55130
            goto L60440
L60420:     print using L55220
            print using L55240
L60440: return

        end_selection
            if rpt_sum$ = "D"  then L60530
            convert rga_total% to rga_total$, pic(####)

            print
            print using L55180, sav_status$, rga_total$
            goto L60570
L60530:     convert status_total% to status_total$, pic(####)

            print
            print using L55190, sav_status$, status_total$
L60570:     if rpt_sum$ = "D"  then gosub lookup_status                  ~
                               else gosub lookup_hd_status

            print
            if rpt_sum$ = "D"  then L60640
            print using L55100, rga_hd_status$, status_hd_desc$
            goto L60650
L60640:     print using L55100, rga_dt_status$, status_desc$
L60650:     print
            lcnt%       = lcnt% + 5%
            rga_total%  = 0% : status_total% = 0%
            if rpt_sum$ = "D"  then sav_status$ = rga_dt_status$         ~
                               else sav_status$ = rga_hd_status$
        return

        end_report
            if rpt_sum$ = "D"  then L60790
            convert rga_total% to rga_total$, pic(####)

            print
            print using L55180, sav_status$, rga_total$
            goto L60830
L60790:     convert status_total% to status_total$, pic(####)

            print
            print using L55190, sav_status$, status_total$
L60830:     print
            print
            if rpt_sum$ = "D"  then print using L55310                    ~
                               else print using L55290
        return

        lookup_status
            readkey$ = all(hex(20))
            str(readkey$,1%,9%)   = "APC  RGA1"
            str(readkey$,10%,15%) = rga_dt_status$
            read #1,key = readkey$, using L60950, status_desc$,           ~
                eod goto L60980
L60950:         FMT POS(25), CH(30)

        return
L60980:     status_desc$ = "No Status Code"
        return

        lookup_hd_status
            readkey$ = all(hex(20))
            str(readkey$,1%,9%)   = "APC  RGA1"
            str(readkey$,10%,15%) = rga_hd_status$
            read #1,key = readkey$, using L61070, status_hd_desc$,        ~
                eod goto L61100
L61070:         FMT POS(25), CH(30)

        return
L61100:     status_hd_desc$ = "No Status Code"
        return

        lookup_customer
            read #2,key = rga_cuscode$, using L61160, cus_desc$,          ~
                eod goto L61190
L61160:         FMT POS(10), CH(30)

        return
L61190:     cus_desc$ = "No Customer Code"
        return

        lookup_part                           /* Check HNYMASTR        */
            init(" ") part_desc$, apc_prt$, apc_sze$, apc_scr$, dt_flag$
            read #9,key = rga_part$,using L61260, part_desc$, apc_prt$,   ~
                apc_sze$, eod goto L61300
L61260:         FMT XX(25), CH(32), POS(606), CH(60), CH(20)

            goto L61440

L61300:     err% = 0%
            if len(rga_part$) > 18% then goto L61390
                part_desc$ = "COMPONENT PART"
                gosub lookup_apcplnsc
                gosub lookup_text

                if dt_flag$ = "Y" then part_desc$ = text_desc$
                goto L61440

L61390:     call "APCDESCR" (rga_part$, apc_scr$, apc_prt$, apc_sze$,    ~
                             #11, err%)

            str(part_desc$,1%,16%)  = str(apc_prt$,1%,16%)
            str(part_desc$,17%,16%) = str(apc_sze$,1%,16%)
L61440: return

        lookup_apcplnsc
            sc_txt$             = " "
            sc_key$             = all(hex(20))
            str(sc_key$, 1%,8%) = rga_so$
            convert rga_line$ to xx%, data goto L61510
L61510:
            convert xx%       to str(sc_key$,9%,2%), pic(##)

            read #8,key = sc_key$, using L61550, sc_txt$, eod goto L61570
L61550:         FMT POS(100), CH(04)

L61570: return

        lookup_text                           /* Look Up Text Id       */
            init(" ") text_desc$, textid$, text_key$, sav_key1$, atext$()
            textid$ = sc_txt$
            gosub'099(textid$)

            if txt% = 0% then L61810
            text_key$            = all(hex(00))
            str(text_key$,1%,1%) = "M"
            str(text_key$,2%,3%) = "   "
            str(text_key$,5%,4%) = textid$
            str(text_key$,9%,1%) = "1"
            sav_key1$            = text_key$
            read #5,key > text_key$, eod goto L61810

            get  #5, using L61750, text_key$, atext$()
L61750:         FMT CH(11), POS(64), 2*CH(70)

            if str(sav_key1$,1%,9%) <> str(text_key$,1%,9%) then L61810
            if atext$(1)  <> " " then text_desc$ = str(atext$(1),1%,60%) ~
                                 else text_desc$ = str(atext$(2),1%,60%)
            if text_desc$ <> " " then dt_flag$ = "Y"
L61810: return

        deffn'099(txt$)
            txt% = 0%
            if txt$ = hex(00000000) or txt$ = hex(ffffffff) or txt$ = " "~
                then L61880
            txt% = 1%
L61880: return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*
        exit_sub
            call "SHOSTAT" ("One Moment Please")

            close #3
            close #4
            close #5
            close #8
            close #9
            close #11
            close printer

            end
