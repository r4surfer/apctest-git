        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCRGA7B                             *~
            *  Creation Date     - 11/13/95                             *~
            *  Last Modified Date- 01/11/06                             *~
            *  Description       - Subroutine to print RGA's by Prod.   *~
            *                      Date Range Report.                   *~
            *                                                           *~
            *  Special Notes     - Subroutine of APCRGA04               *~
            *                                                           *~
            *                      Call this routine with the dates     *~
            *                      in a packed format.  This will       *~
            *                      allow for century passing with       *~
            *                      the date.                            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/13/95 ! New Program for (APC) - Last Mod Date    ! JBF *~
            * 04/04/96 ! Change Reason Code to 3 bytes (Complaint)! JBF *~
            * 02/26/98 ! Y2K COMPLIANCE MODIFIACTIONS             ! DJD *~
            * 10/18/99 ! (EWD001) Mods to fix Date Problems       ! RHH *~
            *          !    Rewrite routine.                      !     *~  
            * 01/11/06 ! (PAR000) CR347 mods for sub part         ! CMG *~
            *************************************************************

        sub "APCRGA7B"  (#1,             /* GENCODES File Channel      */~
                         #2,             /* CUSTOMER File Channel      */~
                         beg_dte$,       /* Beginning Date             */~
                         end_dte$,       /* Ending    Date             */~
                         rpt_value$,     /* 'ALL' or blank             */~
                         rpt_sum$)       /* Summary  "S" or "D"        */

        dim                                                              ~
            rga_cuscode$9,               /* RGA Customer No.           */~
            rga_hd_status$2,             /* RGA Header Status          */~
            rga_number$4,                /* RGA No.                    */~
            rga_auth_id$3,               /* RGA Authoried by ID        */~
            rga_dte$10,                  /* RGA Date Entered           */~
            rga_dt_status$2,             /* RGA Detail Status          */~
            rga_dt_num$4,                /* RGA No. (Detail)           */~
            rga_item$2,                  /* RGA Item No.               */~
            rga_part$25,                 /* RGA Part No.               */~
            rga_so$8,                    /* RGA Sales Order            */~
            rga_line$2,                  /* RGA S.O. Line              */~
            rga_piece$4,                 /* RGA S.O. Piece Count       */~
            rga_qty$4,                   /* RGA S.O. Order Quantity    */~
            rga_po$16,                   /* RGA P.O. No.               */~
            tst_dte$6,                   /* Test Date          (EWD001)*/~ 
            rga_reason$3, rhh$10,        /* RGA Reason Code    (EWD001)*/~
            rga_prod_dte$8, rga_dt_mod_dte$8, /*               (EWD001)*/~
            errormsg$79, cnt$42,         /*                    (EWD001)*/~
            workkey$14,                  /* APCRGAWK Read Key (Work)   */~
            dtlkey$6,                    /* APCRGADT Read Key          */~
            readkey$25,                  /* GENCODES Read Key          */~
            bck_key$19,                  /* BCKLINES Read Key          */~
            bck_text$4,                  /* BCKLINES Order Text ID     */~
            sav_dte$6, prt_dte$10,       /* Save Production Date       */~
            beg_dte$10, end_dte$10,      /* (EWD001)                   */~ 
            sav_number$4,                /* Save RGA No.               */~
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
            title$54,                    /* Report Title Field         */~
            date$10,                     /* Date for screen display    */~
            userid$3                     /* Current User Id            */

        dim f2%(12%),                    /* = 0 if the file is open    */~
            f1%(12%),                    /* = 1 if READ was successful */~
            fs%(12%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(12%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "01.00.00 11/13/95 RGA by Prod. Date Report    "
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
            * #12 ! APCRGAWK ! Sort Work File                           *~
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

            select #12, "APCRGAWK",                                      ~
                        varc,     indexed,  recsize =   12,              ~
                        keypos =    1, keylen =  12

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#3,  fs%(3%),  f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4,  fs%(4%),  f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5,  fs%(5%),  f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#8,  fs%(8%),  f2%(8%),  0%, rslt$(8%))
            call "OPENCHCK" (#9,  fs%(9%),  f2%(9%),  0%, rslt$(9%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 0%, rslt$(11%))

            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)

            date$ = date
            call "DATFMTC" (date$)

            if str(rpt_value$,1%,3%) = "ALL" then inputmode

	    /* Y2K Changes */                    /* (EWD001)        */
	    /* Y2K Changes */


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        inputmode
            gosub initialize_variables
            gosub select_report
            gosub begin_process

            goto exit_sub

        REM *************************************************************~
            *               P R O C E S S   D A T A                     *~
            *************************************************************
        begin_process
            call "SHOSTAT" ("Building RGA's by Prod. Date Report")
            gosub build_work_file
            gosub select_date
            gosub end_report

        return

        select_date
            call "SHOSTAT" ("Printing RGA's by Prod. Date Report")
            if str(beg_dte$,1%,3%) = "ALL" then goto L19200
               call "DATFMTC" (beg_dte$)
               call "DATFMTC" (end_dte$)
L19200:     workkey$ = all(hex(00))
        next_rga
            read #12,key > workkey$, using L35190, tst_dte$,             ~
                rga_number$, rga_item$, eod goto L19320

            if len(sav_dte$) < 6 then sav_dte$ = tst_dte$
            if sav_dte$ = tst_dte$ then L19250
                gosub end_selection

L19250:     if rpt_sum$ = "D" then gosub process_apcrgadt                ~
                              else gosub process_apcrgahd

            str(workkey$, 1%,6%) = tst_dte$
            str(workkey$, 7%,4%) = rga_number$
            str(workkey$,11%,2%) = rga_item$
            goto next_rga
L19320: return

        process_apcrgahd
            if sav_number$ = rga_number$ then L19480
               sav_number$ = rga_number$

            read #3,key = rga_number$, using L35030,                      ~
                                  rga_cuscode$,    /* RGA Customer     */~
                                  rga_hd_status$,  /* RGA Status       */~
                                  rga_number$,     /* RGA No.          */~
                                  rga_auth_id$,    /* RGA Authorized By*/~
                                  rga_dte$,        /* RGA Date Entered */~
                                  eod goto L19480

            gosub print_apcrgahd

L19480: return

        process_apcrgadt
            str(dtlkey$,1%,4%) = rga_number$
            str(dtlkey$,5%,2%) = rga_item$
            read #4,key = dtlkey$, eod goto L19670

            get  #4, using L35090, rga_dt_status$, /* RGA Item Status   */~
                                  rga_dt_num$,    /* RGA No.           */~
                                  rga_item$,      /* RGA Item No.      */~
                                  rga_part$,      /* RGA Part No.      */~
                                  rga_so$,        /* RGA S.O. No.      */~
                                  rga_line$,      /* RGA S.O. Line     */~
                                  rga_piece$,     /* RGA S.O. Piece Cnt*/~
                                  rga_qty$,       /* RGA S.O. Order Qty*/~
                                  rga_po$         /* RGA P.O. No.      */

            gosub print_apcrgadt

L19670: return

        select_report
            if rpt_sum$ = "S" then L19740
            title$  = "APC Building Products - RGA's by Prod. Date  (Deta~
        ~il)"
            goto L19760
L19740:     title$  = "APC Building Products - RGA's by Prod. Date  (Summ~
        ~ary)"
L19760:     pageno% = 0%
            lcnt%   = 99%
            call "TIME"    (xtime$)
            call "SETPRNT" (" ", "RGA7", 0%, 0%)

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
                rga_so$, rga_line$, rga_piece$, rga_qty$, rga_prod_dte$, ~
                rga_hd_status$, rga_dt_status$, sav_number$, xtime$,     ~
                bck_text$, rga_reason$
            dtlkey$ = all(hex(20)) : workkey$ = all(hex(20))
            rga_total% = 0%
        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        build_work_file
            cnt% = 0% : cnt1% = 0%
            cnt$ = "Records Checked [xxxxxx] Selected [xxxxxx]"

            read #4,key > dtlkey$, eod goto L31220
            goto L31130
        build_next
           
            read #4, eod goto L31220

L31130:     get  #4, using L31140, rga_number$, rga_item$, rga_reason$,~
                                         rga_prod_dte$, rga_dt_mod_dte$ 
L31140:       FMT POS(12),CH(04),CH(02),POS(26),CH(3),POS(167),CH(08), ~
                  POS(236), CH(08)
            cnt% = cnt% + 1%                        /* (EWD001)      */
            if mod(cnt%,50%) <> 0% then goto L31150
               convert cnt% to str(cnt$,18%,6%), pic(######)

               convert cnt1% to str(cnt$,36%,6%), pic(######)

               print at(04,19);hex(84);cnt$;
                
L31150:     init(" ") rhh$                          /* Correct Dates */
            tst_dte$ = str(rga_prod_dte$,1%,6%)
            rhh$     = str(rga_prod_dte$,1%,6%)
            call "DATEOKC" (rhh$, date%, errormsg$)
            if date% = 0% then L31180               /* Bad Production*/
            goto L31185

L31180:     init(" ") rhh$
            tst_dte$ = str(rga_dt_mod_dte$,1%,6%)
            rhh$     = str(rga_dt_mod_dte$,1%,6%)   /* Use Last Mod */ 
            call "DATEOKC" (rhh$, date%, errormsg$)
  
L31185:     if str(rpt_value$,1%,3%) = "ALL" then L31190
               if rga_reason$ <> str(rpt_value$,1%,3%) then            ~
                                                        goto build_next 
    
L31190:     if str(beg_dte$,1%,3%) = "ALL" then goto L31200

            if str(tst_dte$,1%,6%) < str(beg_dte$,1%,6%)               ~
               or str(tst_dte$,1%,6%) > str(end_dte$,1%,6%) then L31210
                                                      /* (EWD001)    */ 
L31200:     gosub dataput_work_file

L31210:     goto build_next

L31220: return

        dataput_work_file
            cnt1% = cnt1% + 1%
            put   #12, using L35190,                                      ~
                       tst_dte$,          /* RGA Production Date        */~
                       rga_number$,       /* RGA No.                    */~
                       rga_item$          /* RGA Item                   */

            write #12, eod goto L31320

L31320: return

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

L35190:     FMT CH(06),                  /* RGA Production Date        */~
                CH(04),                  /* RGA Number                 */~
                CH(02)                   /* RGA Item                   */

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************
                                    /* RGA's by Prod. Date Columns     */
                                    /* Header Format                   */
L55060: %  ########## @ ########          ###############################~
        ~#######################            Page: ###

                                    /* APCRGAHD Format                 */
L55100: %  Production Date Range: ########## thru ##########
L55110: %    RGA Production Date: ##########
L55120: %  RGA      Auth. ID     Date Entered     Status
L55130: %+ ----  !  --------  !  ------------  !  -----------------------~
        ~-------  +
L55150: %! ####  !    ###     !   ##########   !  #######################~
        ~#######  !

L55180: %  Total RGA's for Prod. Date ########## : ####
L55190: %  Total RGA Items for Prod. Date ########## : ####

                                    /* APCRGADT Format                 */
L55220: %! RGA   Item  Part Description                  S.O. No.  Line  ~
        ~ P.O. No.           Status
L55230: %+ ---- ! -- ! ------------------------------- ! -------- ! -- ! ~
        ~ ---------------- ! ------------------------------ +

L55260: %! #### ! ## ! ############################### ! ######## ! ## ! ~
        ~ ################ ! ############################## !

L55290: %         *****  End Of RGA's by Prod. Date Report  (Summary)  **~
        ~***
L55310: %         *****  End Of RGA's by Prod. Date Report  (Detail)  ***~
        ~**

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************
        print_apcrgahd
            call "DATFMTC" (rga_dte$)
            gosub lookup_status

            if lcnt% > 55% then gosub print_header

            print using L55150, rga_number$, rga_auth_id$, rga_dte$,      ~
                status_desc$
            lcnt%      = lcnt%      + 1%
            rga_total% = rga_total% + 1%
        return

        print_apcrgadt
            gosub lookup_part
            gosub lookup_status

            if lcnt% > 55% then gosub print_header

            print using L55260, rga_number$, rga_item$, part_desc$,       ~
                rga_so$, rga_line$, rga_po$, status_desc$
            lcnt%      = lcnt%      + 1%
            rga_total% = rga_total% + 1%
        return

        print_header
            init(" ") prt_dte$
            prt_dte$ = tst_dte$
            call "DATFMTC" (prt_dte$)

            pageno% = pageno% + 1%
            print page
            print using L55060, date$, xtime$, title$, pageno%
            print
            print using L55100, beg_dte$, end_dte$
            print
            print
            print using L55110, prt_dte$
            print
            lcnt% = 9%
            if rpt_sum$ = "D" then L60460
               print using L55120                /* (EWD001) Summary */
               print using L55130
               goto L60480
L60460:     print using L55220                   /* (EWD001) Detail  */ 
            print using L55230
L60480: return

        end_selection
            init(" ") prt_dte$, prt_dte1$
            prt_dte$  = tst_dte$
            prt_dte1$ = sav_dte$
            call "DATFMTC"  (prt_dte$)
            call "DATFMTC"  (prt_dte1$)

            convert rga_total% to rga_total$, pic(####)

            print
            if rpt_sum$ = "D"  then L60600
            print using L55180, prt_dte1$, rga_total$
            goto L60610
L60600:     print using L55190, prt_dte1$, rga_total$
L60610:     print
            print
            print using L55110, prt_dte$
            print
            lcnt%       = lcnt% + 6%
            rga_total%  = 0%
            init(" ") sav_number$
            sav_dte$    = tst_dte$
        return

        end_report
            init(" ") prt_dte1$
            prt_dte1$ = sav_dte$
            call "DATFMTC" (prt_dte1$)
 
            if lcnt% > 55% then gosub print_header
            convert rga_total% to rga_total$, pic(####)

            print
            if rpt_sum$ = "D"  then L60790
            print using L55180, prt_dte1$, rga_total$
            goto L60800
L60790:     print using L55190, prt_dte1$, rga_total$
L60800:     print
            print
            if rpt_sum$ = "D"  then print using L55310                    ~
                               else print using L55290
        return

        lookup_status
            readkey$ = all(hex(20))
            str(readkey$,1%,9%)   = "APC  RGA1"
            if rpt_sum$ = "D" then L60920
            str(readkey$,10%,15%) = rga_hd_status$
            goto L60930
L60920:     str(readkey$,10%,15%) = rga_dt_status$
L60930:     read #1,key = readkey$, using L60950, status_desc$,           ~
                eod goto L60980
L60950:         FMT POS(25), CH(30)

        return
L60980:     init(" ") status_desc$
        return

        lookup_part                           /* Check HNYMASTR        */
            init(" ") part_desc$, apc_prt$, apc_sze$, apc_scr$, dt_flag$
            read #9,key = rga_part$,using L61050, part_desc$, apc_prt$,   ~
                apc_sze$, eod goto L61090
L61050:         FMT XX(25), CH(32), POS(606), CH(60), CH(20)

            goto L61230

L61090:     err% = 0%
            if len(rga_part$) > 18% then goto L61180
                part_desc$ = "COMPONENT PART"
                gosub lookup_bcklines
                gosub lookup_text

                if dt_flag$ = "Y" then part_desc$ = text_desc$
                goto L61230

L61180:     call "APCDESCR" (rga_part$, apc_scr$, apc_prt$, apc_sze$,    ~
                             #11, err%)

            str(part_desc$,1%,16%)  = str(apc_prt$,1%,16%)
            str(part_desc$,17%,16%) = str(apc_sze$,1%,16%)
L61230: return

        lookup_bcklines
            bck_text$            = " "
            bck_key$             = all(hex(20))
            str(bck_key$, 1%,8%) = rga_so$
            convert rga_line$ to xx%, data goto L61300
L61300:
            convert xx%       to str(bck_key$,17%,3%), pic(###)

            read #8,key = bck_key$, using L61350, bck_text$,              ~
                eod goto L61370
L61350:         FMT POS(242), CH(04)

L61370: return

        lookup_text                           /* Look Up Text Id       */
            init(" ") text_desc$, textid$, text_key$, sav_key1$, atext$()
            textid$ = bck_text$
            gosub'099(textid$)

            if txt% = 0% then L61600
            text_key$            = all(hex(00))
            str(text_key$,1%,1%) = "M"
            str(text_key$,2%,3%) = "   "
            str(text_key$,5%,4%) = textid$
            str(text_key$,9%,1%) = "1"
            sav_key1$            = text_key$
            read #5,key > text_key$, eod goto L61600

            get  #5, using L61540, text_key$, atext$()
L61540:         FMT CH(11), POS(64), 2*CH(70)

            if str(sav_key1$,1%,9%) <> str(text_key$,1%,9%) then L61600
            if atext$(1)  <> " " then text_desc$ = str(atext$(1),1%,60%) ~
                                 else text_desc$ = str(atext$(2),1%,60%)
            if text_desc$ <> " " then dt_flag$ = "Y"
L61600: return

        deffn'099(txt$)
            txt% = 0%
            if txt$ = hex(00000000) or txt$ = hex(ffffffff) or txt$ = " "~
                then L61670
            txt% = 1%
L61670: return

        open_work
            if mode% = 1%    then mode$ = "OUTPT"
            if mode% = 2%    then mode$ = "INPUT"
            if mode% = 3%    then mode$ = "SHARE"

            call "WORKOPN2" (#12, mode$, 500%, f2%)
                if f2% <> 0% then L61780

        return
L61780:     call "SHOSTAT" ("ERROR - CANNOT OPEN (APCRGAWK)") : stop

        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*
        exit_sub
            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#12)

            close #3
            close #4
            close #5
            close #8
            close #9
            close #11
            close printer

            end
