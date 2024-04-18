        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCRG24B                             *~
            *  Creation Date     - 10/24/95                             *~
            *  Last Modified Date- 01/11/06                             *~
            *  Description       - Subroutine to print RGA's by         *~
            *                      Customer Report.                     *~
            *                                                           *~
            *  Special Notes     - Subroutine of APCRGA04               *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/24/95 ! New Program for (APC) - Last Mod Date    ! JBF *~
            * 04/04/96 ! Change Reason Code to 3 bytes (Complaint)! JBF *~
            * 02/20/98 ! Y2K Conversions                          ! DJD *~
            * 01/11/06 ! (PAR000) CR347 mods for sub part         ! CMG *~
            *************************************************************

        sub "APCRG24B"  (#1,             /* GENCODES File Channel      */~
                         #2,             /* CUSTOMER File Channel      */~
                         rpt_value$,     /* Customer No. or 'ALL'      */~
                         rpt_sum$)       /* Summary  "S" or "D"        */

        dim                                                              ~
            rga_hd_rec$80,               /* RGA Header File Record     */~
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
            custkey$15,                  /* APCRGAHD Read Key (Alt 2)  */~
            dtlkey$6,                    /* APCRGADT Read Key          */~
            readkey$25,                  /* GENCODES Read Key          */~
            bck_key$19,                  /* BCKLINES Read Key          */~
            bck_text$4,                  /* BCKLINES Text ID           */~
            sav_cust$9,                  /* Save RGA Customer No.      */~
            cus_desc$30,                 /* Customer Description       */~
            status_desc$30,              /* Status Code Description    */~
            part_desc$32,                /* Part Description           */~
            text_desc$60,                /* Line Item Text             */~
            text_key$11,                 /* Text File Key              */~
            sav_key1$11,                 /* Text File Save Key         */~
            atext$(2)70,                 /* Text (2) Lines             */~
            apc_prt$60,                  /* Print Description          */~
            apc_sze$20,                  /* Size Long Form             */~
            apc_scr$120,                 /* Screen Description         */~
            dt_flag$1,                   /* Text as Part Description   */~
            cust_total$4,                /* Customer Total RGA's       */~
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
            cms2v$ = "01.00.00 10/04/95 RGA by Customer Report     "
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
            * #8  ! BCKLINES ! S.O. Line Items File                     *~
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
/*PAR000*/              varc,     indexed,  recsize =  512,              ~
                        keypos =   12, keylen =    6,                    ~
                        alt key  1, keypos =  10, keylen =   8,          ~
                            key  2, keypos =   1, keylen =  17

            select #5,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =   11

            select #8,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =   19

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
            call "SHOSTAT" ("Creating RGA's by Customer Report")

            if str(rpt_value$,1%,3%) = "ALL" then gosub select_all       ~
                                             else gosub select_one
            gosub end_report

        return

        select_one
            str(custkey$,1%,9%) = rpt_value$
            read #3,key 2% > custkey$, using L19160, rga_hd_rec$,         ~
                eod goto L19500
L19160:         FMT CH(80)

            if str(rga_hd_rec$,1%,9%) <> rpt_value$ then L19500
            rga_cuscode$ = str(rga_hd_rec$,1%,9%)
            sav_cust$    = rga_cuscode$
            gosub lookup_customer

            goto L19330

        select_all
            read #3,key 2% > custkey$, eod goto L19500

            goto L19330

        next_rga
            read #3, eod goto L19500

L19330:     get  #3, using L35030, rga_cuscode$,    /* RGA Customer     */~
                                  rga_hd_status$,  /* RGA Status       */~
                                  rga_number$,     /* RGA No.          */~
                                  rga_auth_id$,    /* RGA Authorized By*/~
                                  rga_dte$         /* RGA Date Entered */

            if sav_cust$ <> " "               then L19430
                sav_cust$ = rga_cuscode$
                gosub lookup_customer

L19430:     if sav_cust$  = rga_cuscode$      then L19470
            if str(rpt_value$,1%,3%) <> "ALL" then L19500
                gosub end_selection

L19470:     if rpt_sum$ = "D"                 then gosub process_apcrgadt~
                                              else gosub print_apcrgahd
            goto next_rga
L19500: return

        process_apcrgadt
            dtlkey$ = all(hex(20))
            str(dtlkey$,1%,4%) = rga_number$

        next_detail
            read #4,key > dtlkey$, eod goto L19750

            get  #4, using L35090, rga_dt_status$, /* RGA Item Status   */~
                                  rga_dt_num$,    /* RGA No.           */~
                                  rga_item$,      /* RGA Item No.      */~
                                  rga_part$,      /* RGA Part No.      */~
                                  rga_so$,        /* RGA S.O. No.      */~
                                  rga_line$,      /* RGA S.O. Line     */~
                                  rga_piece$,     /* RGA S.O. Piece Cnt*/~
                                  rga_qty$,       /* RGA S.O. Order Qty*/~
                                  rga_po$         /* RGA P.O. No.      */

            if rga_dt_num$ <> rga_number$ then L19750
            gosub print_apcrgadt

            str(dtlkey$,1%,4%) = rga_number$
            str(dtlkey$,5%,2%) = rga_item$
            goto next_detail
L19750: return

        select_report
            if rpt_sum$ = "S" then L19820
            title$  = "APC Building Products  -  RGA's by Customer  (Deta~
        ~il)"
            goto L19840
L19820:     title$  = "APC Building Products  -  RGA's by Customer  (Summ~
        ~ary)"
L19840:     pageno% = 0%
            lcnt%   = 99%
            call "TIME"    (xtime$)
            call "SETPRNT" (" ", "RGA4", 0%, 0%)

            select printer(134)
        return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") rga_number$, rga_item$, sav_cust$, rga_cuscode$,   ~
                rga_auth_id$, rga_dte$, rga_po$, cus_desc$, status_desc$,~
                rga_dt_num$, rga_item$, rga_so$, rga_line$, rga_piece$,  ~
                rga_qty$, rga_hd_status$, rga_dt_status$, xtime$,        ~
                bck_text$
            custkey$ = all(hex(20)) : dtlkey$ = all(hex(20))
            cust_total% = 0%
        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
L35030:     FMT CH(09),                  /* RGA Customer               */~
                CH(02),                  /* RGA Header Status          */~
                CH(04),                  /* RGA Number                 */~
                CH(03),                  /* Authorizing Userid         */~
                CH(08)                   /* RGA Entry Date             */

L35090:     FMT POS(10),  CH(02),        /* RGA Item Status            */~
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
                                    /* RGA's by Customer Columns       */
                                    /* Header Format                   */
L55060: %  ######## @ ########            ###############################~
        ~#######################            Page: ###

                                    /* APCRGAHD Format                 */
L55100: %  Customer: #########   Name: ##############################
L55110: %  RGA      Auth. ID     Date Entered     Status
L55120: %+ ----  !  --------  !  ------------  !  -----------------------~
        ~-------  +
L55140: %! ####  !    ###     !    ########    !  #######################~
        ~#######  !

L55170: %  Total RGA's for Customer #########  :  ####

                                    /* APCRGADT Format                 */
L55200: %! RGA   Item  Part Description                  S.O. No.  Line  ~
        ~ P.O. No.           Status
L55220: %+ ---- ! -- ! ------------------------------- ! -------- ! -- ! ~
        ~ ---------------- ! ------------------------------ +
L55240: %! #### ! ## ! ############################### ! ######## ! ## ! ~
        ~ ################ ! ############################## !

L55270: %         *****   End Of RGA's by Customer Report  (Summary)   **~
        ~***
L55290: %         *****   End Of RGA's by Customer Report  (Detail)   ***~
        ~**

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************
        print_apcrgahd
            call "DATEFMT" (rga_dte$)
            gosub lookup_status

            if lcnt% > 55% then gosub print_header

            print using L55140, rga_number$, rga_auth_id$, rga_dte$,      ~
                status_desc$
            lcnt%       = lcnt%       + 1%
            cust_total% = cust_total% + 1%
        return

        print_apcrgadt
            gosub lookup_part
            gosub lookup_status

            if lcnt% > 55% then gosub print_header

            print using L55240, rga_number$, rga_item$, part_desc$,       ~
                rga_so$, rga_line$, rga_po$, status_desc$
            lcnt%       = lcnt%       + 1%
            cust_total% = cust_total% + 1%
        return

        print_header
            pageno% = pageno% + 1%
            print page
            print using L55060, date$, xtime$, title$, pageno%
            print
            print using L55100, rga_cuscode$, cus_desc$
            print
            if rpt_sum$ = "D" then L60380
            print using L55110
            print using L55120
            goto L60400
L60380:     print using L55200
            print using L55220
L60400:     lcnt% = 6%
        return

        end_selection
            convert cust_total% to cust_total$, pic(####)

            print
            print using L55170, sav_cust$, cust_total$
            gosub lookup_customer

            print
            print using L55100, rga_cuscode$, cus_desc$
            print
            lcnt%       = lcnt% + 5%
            cust_total% = 0%
            sav_cust$   = rga_cuscode$
        return

        end_report
            convert cust_total% to cust_total$, pic(####)

            print
            print using L55170, sav_cust$, cust_total$
            print
            print
            if rpt_sum$ = "S" then print using L55270                     ~
                              else print using L55290
        return

        lookup_customer
            read #2,key = rga_cuscode$, using L60720, cus_desc$,          ~
                eod goto L60750
L60720:         FMT POS(10), CH(30)

        return
L60750:     init(" ") cus_desc$
        return

        lookup_status
            readkey$ = all(hex(20))
            str(readkey$,1%,9%)   = "APC  RGA1"
            if rpt_sum$ = "D" then L60840
            str(readkey$,10%,15%) = rga_hd_status$
            goto L60850
L60840:     str(readkey$,10%,15%) = rga_dt_status$
L60850:     read #1,key = readkey$, using L60870, status_desc$,           ~
                eod goto L60900
L60870:         FMT POS(25), CH(30)

        return
L60900:     init(" ") status_desc$
        return

        lookup_part                           /* Check HNYMASTR        */
            init(" ") part_desc$, apc_prt$, apc_sze$, apc_scr$, dt_flag$
            read #9,key = rga_part$,using L60970, part_desc$, apc_prt$,   ~
                apc_sze$, eod goto L61010
L60970:         FMT XX(25), CH(32), POS(606), CH(60), CH(20)

            goto L61150

L61010:     err% = 0%
            if len(rga_part$) > 18% then goto L61100
                part_desc$ = "COMPONENT PART"
                gosub lookup_bcklines
                gosub lookup_text

                if dt_flag$ = "Y" then part_desc$ = text_desc$
                goto L61150

L61100:     call "APCDESCR" (rga_part$, apc_scr$, apc_prt$, apc_sze$,    ~
                             #11, err%)

            str(part_desc$,1%,16%)  = str(apc_prt$,1%,16%)
            str(part_desc$,17%,16%) = str(apc_sze$,1%,16%)
L61150: return

        lookup_bcklines
            bck_text$            = " "
            bck_key$             = all(hex(20))
            str(bck_key$, 1%,8%) = rga_so$
            convert rga_line$ to xx%, data goto L61220
L61220:
            convert xx%       to str(bck_key$,17%,3%), pic(###)

            read #8,key = bck_key$, using L61270, bck_text$,              ~
                eod goto L61290
L61270:         FMT POS(242), CH(04)

L61290: return

        lookup_text                           /* Look Up Text Id       */
            init(" ") text_desc$, textid$, text_key$, sav_key1$, atext$()
            textid$ = bck_text$
            gosub'099(textid$)

            if txt% = 0% then L61520
            text_key$            = all(hex(00))
            str(text_key$,1%,1%) = "M"
            str(text_key$,2%,3%) = "   "
            str(text_key$,5%,4%) = textid$
            str(text_key$,9%,1%) = "1"
            sav_key1$            = text_key$
            read #5,key > text_key$, eod goto L61520

            get  #5, using L61460, text_key$, atext$()
L61460:         FMT CH(11), POS(64), 2*CH(70)

            if str(sav_key1$,1%,9%) <> str(text_key$,1%,9%) then L61520
            if atext$(1)  <> " " then text_desc$ = str(atext$(1),1%,60%) ~
                                 else text_desc$ = str(atext$(2),1%,60%)
            if text_desc$ <> " " then dt_flag$ = "Y"
L61520: return

        deffn'099(txt$)
            txt% = 0%
            if txt$ = hex(00000000) or txt$ = hex(ffffffff) or txt$ = " "~
                then L61590
            txt% = 1%
L61590: return

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
