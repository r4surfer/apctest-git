        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCRGA02                             *~
            *  Creation Date     - 10/04/95                             *~
            *  Last Modified Date- 01/11/06                             *~
            *  Description       - Program to Create Special Barcode    *~
            *                      labels for use with Scanning         *~
            *                      programs.                            *~
            *                                                           *~
            *  Special Notes     - Values are pulled from the file      *~
            *                      ( APCRGADT ).                        *~
            *                                                           *~
            *                    - Print file - (APCBARRG.DAT)          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *  Subroutines - (APCRGA2B) - The RGA Barcode label print   *~
            *                             routine.                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/04/95 ! New Program for (APC) - Last Mod Date    ! JBF *~
            * 08/05/96 ! Modify Program to support UBI printer    ! JBF *~
            * 01/11/06 ! (PAR000) CR347 mods for sub part         ! CMG *~
            *************************************************************
        dim                                                              ~
            rga_hd_rec$80,               /* RGA Header File Record     */~
/*PAR000*/  rga_dt_rec$(2%)256,          /* RGA Detail File Record     */~
/*PAR000*/  sav_rec$(2%)256,             /* RGA Detail File Save Area  */~
            r_number$6,                  /* RGA No./Item No.           */~
            rga_number$4,                /* RGA No.                    */~
            rga_item$2,                  /* RGA Item No.               */~
            rga_part$25,                 /* RGA Item Part No.          */~
            rga_so$8,                    /* RGA Sales Order No.        */~
            rga_line$2,                  /* RGA Sales Order Line       */~
            r_cuscode$9,                 /* RGA Customer No.           */~
            rga_cuscode$9,               /* RGA Customer No.           */~
            dtlkey$6,                    /* APCRGADT Read Key          */~
            dtlkey1$8,                   /* APCRGADT Read Key (Alt 1)  */~
            bck_key$19,                  /* BCKMASTR/BCKLINES Key      */~
            bck_text$4,                  /* BCKLINES Order Text ID     */~
            apc_scr$120,                 /* Screen Description         */~
            apc_prt$60,                  /* Print Description          */~
            apc_sze$20,                  /* Size Long Form             */~
            sav_number$4,                /* Save RGA No.               */~
            cus_desc$30,                 /* Generic Description        */~
            c_desc$30,                   /* Generic Description        */~
            part_desc$45,                /* Part Description           */~
            text_desc$60,                /* Line Item Text             */~
            text_key$11,                 /* Text File Key              */~
            sav_key1$11,                 /* Text File Save Key         */~
            atext$(2)70,                 /* Text (2) Lines             */~
            dt_flag$1,                   /* Line Item Text as Part Desc*/~
            title$40,                    /* Report Title Field         */~
            date$8,                      /* Date for screen display    */~
            userid$3                     /* Current User Id            */

        dim f2%(11%),                    /* = 0 if the file is open    */~
            f1%(11%),                    /* = 1 if READ was successful */~
            fs%(11%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(11%)20                 /* Text from file opening     */

	dim blankdate$6,		 /* Blank (empty) date         */~
	    workdate10$10,		 /* Century date mm-dd-yyyy    */~
	    workdate8$8			 /* Regular date mm-dd-yy      */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "01.00.00 10/04/95 RGA Scanning Labels        "
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
            * #2  ! CUSTOMER ! Customer Master File                     *~
            * #4  ! APCRGAHD ! RGA Header File                          *~
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

            select #2,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =    1, keylen =    9,                    ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #4,  "APCRGAHD",                                      ~
                        varc,     indexed,  recsize =   80,              ~
                        keypos =   12, keylen =    4,                    ~
                        alt key  1, keypos =  10, keylen =   6,          ~
                            key  2, keypos =   1, keylen =  15

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
            call "OPENCHCK" (#1,  fs%( 1%), f2%( 1%), 0%, rslt$( 1%))
            call "OPENCHCK" (#2,  fs%( 2%), f2%( 2%), 0%, rslt$( 2%))
            call "OPENCHCK" (#4,  fs%( 4%), f2%( 4%), 0%, rslt$( 4%))
            call "OPENCHCK" (#8,  fs%( 8%), f2%( 8%), 0%, rslt$( 8%))
            call "OPENCHCK" (#9,  fs%( 9%), f2%( 9%), 0%, rslt$( 9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 0%, rslt$(11%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            date$ = date
            call "DATEFMT"     (date$)
	    call "DATUFMTC"   (blankdate$)
            call "EXTRACT" addr("ID", userid$)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        REM *************************************************************~
            *               P R O C E S S   D A T A                     *~
            *************************************************************
        REM BEGIN_PROCESS
            call "SHOSTAT" ("Creating Barcode Labels")
            gosub initialize_variables
            gosub select_log_report

            dtlkey1$            =  all(hex(20))
            str(dtlkey1$,1%,2%) = "10"
/*PAR000*/
            read #1,key 1% > dtlkey1$, using L19120, rga_dt_rec$(),         ~
                eod goto process_done
L19120:         FMT 2*CH(256)

            if str(rga_dt_rec$(),10%,2%) <> "10" then process_done
            rec%           =  1%
            goto process_label

        process_next
/*PAR000*/
            dtlkey1$       =  str(sav_rec$(),10%,8%)
            read #1,key 1% > dtlkey1$, using L19120, rga_dt_rec$(),         ~
                eod goto process_done

            if str(rga_dt_rec$(),10%,2%) <> "10" then process_done

        process_label
/*PAR000*/
            r_cuscode$     =  str(rga_dt_rec$(), 1%, 9%)
            rga_cuscode$   =  str(rga_dt_rec$(), 1%, 9%)
            rga_number$    =  str(rga_dt_rec$(),12%, 4%)
            r_number$      =  str(rga_dt_rec$(),12%, 6%)
            rga_item$      =  str(rga_dt_rec$(),16%, 2%)
            rga_part$      =  str(rga_dt_rec$(),28%,25%)
            rga_so$        =  str(rga_dt_rec$(),53%, 8%)
            rga_line$      =  str(rga_dt_rec$(),61%, 2%)
            gosub lookup_customer
            gosub lookup_part
            gosub update_detail
            if sav_number$ <> rga_number$      then gosub update_header
            gosub print_details

            call "APCRGA2B"  (r_number$,                                 ~
                              c_desc$,                                   ~
                              r_cuscode$,                                ~
                              part_desc$)
            goto process_next

        process_done
            r_number$      = "E O F "
            call "APCRGA2B"  (r_number$, " ", " ", " ")
            if rec% = 1%                       then gosub update_header
            print using L55120
            print using L55040
            print using L55130
            goto exit_program

        update_header
            if sav_number$     = " "           then return
            read   #4,     key = sav_number$, using L19590, rga_hd_rec$,  ~
                eod goto L19690
L19590:         FMT CH(80)

            read   #4,hold,key = sav_number$, using L19590, rga_hd_rec$,  ~
                eod goto L19690
            delete #4

            str(rga_hd_rec$,10%,2%) = "02"
            put    #4, using L19590, rga_hd_rec$
            write  #4, eod goto L19690

L19690: return

        update_detail
/*PAR000*/
            sav_rec$()         = rga_dt_rec$()
            str(dtlkey$,1%,4%) = rga_number$
            str(dtlkey$,5%,2%) = rga_item$
            read   #1,hold,key = dtlkey$, eod goto L19820
            delete #1

/*PAR000*/
            str(rga_dt_rec$(),10%,2%) = "11"
            put    #1, using L19120, rga_dt_rec$()
            write  #1, eod goto L19820

L19820: return

        select_log_report
            title$  = "*** APCRGA02 - RGA Barcode Print Log ***"
            pageno% = 0%
            lcnt%   = 99%
            call "TIME"    (xtime$)
            call "SETPRNT" (" ", "RGA2", 0%, 0%)

            select printer(134)
        return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") r_number$,   rga_number$,  rga_item$, dtlkey$,     ~
                      sav_number$, rga_cuscode$, cus_desc$, r_cuscode$,  ~
                      rga_part$,   part_desc$,   rga_so$,   rga_line$
            rec% = 0%
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************
                                    /* RGA Log Report Columns          */
L55040: %                                                                ~

                                    /* Header Format                   */
L55060: %  ######## @ ########   ########################################~
        ~   Page: ###
L55080: %  RGA    Item    Customer    Customer Name
L55090: %+ ---- ! ---- ! --------- ! -------------------------------- +
                                    /* Detail Format                   */
L55110: %! #### !  ##  ! ######### ! ################################ !
L55120: %!======!======!===========!==================================!
L55130: %       ********   RGA Barcode Log Finished   *******

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************
        print_details
            if lcnt%        >  55%         then gosub print_header
            if sav_number$  = " "          then L60120
            if sav_number$ <>  rga_number$ then L60100
                init(" ") rga_number$, rga_cuscode$, cus_desc$, rga_item$
                goto L60130
L60100:     print using L55120
            lcnt%           =  lcnt% + 1%
L60120:     sav_number$     =  rga_number$
L60130:     print using L55110, rga_number$, rga_item$, rga_cuscode$,     ~
                cus_desc$
            lcnt%           =  lcnt% + 1%
        return

        print_header
            pageno%         =  pageno% + 1%
            print page
            print using L55060, date$, xtime$, title$, pageno%
            print using L55040
            print using L55080
            print using L55090
            lcnt%           =  4%
        return

        lookup_customer
            read #2,key = rga_cuscode$, using L60310, cus_desc$,          ~
                eod goto L60340
L60310:         FMT POS(10), CH(30)

            c_desc$ = cus_desc$
        return
L60340:     init(" ") cus_desc$, c_desc$
        return

        lookup_part                           /* Check HNYMASTR        */
            init(" ") part_desc$, apc_prt$, apc_sze$, apc_scr$, dt_flag$
            read #9,key = rga_part$, using L60410, part_desc$, apc_prt$,  ~
                apc_sze$, eod goto L60440
L60410:         FMT XX(25), CH(32), POS(606), CH(60), CH(20)

            goto L60580
L60440:     err% = 0%
            if len(rga_part$) > 18% then L60530
                part_desc$ = "COMPONENT PART"
                gosub lookup_bcklines
                gosub lookup_text

                if dt_flag$ = "Y"   then part_desc$ = text_desc$
                goto L60580

L60530:     call "APCDESCR" (rga_part$, apc_scr$, apc_prt$, apc_sze$,    ~
                             #11, err%)

            str(part_desc$,1%,16%)  = str(apc_prt$,1%,16%)
            str(part_desc$,17%,16%) = str(apc_sze$,1%,16%)
L60580: return

        lookup_bcklines
            bck_text$            = " "
            bck_key$             = all(hex(20))
            str(bck_key$, 1%,8%) = rga_so$
            convert rga_line$ to xx%, data goto L60650
L60650:
            convert xx%       to str(bck_key$,17%,3%), pic(###)

            read #8,key = bck_key$, using L60700, bck_text$,              ~
                eod goto L60720
L60700:         FMT POS(242), CH(04)

L60720: return

        lookup_text                           /* Look Up Text Id       */
            init(" ") text_desc$, textid$, text_key$, sav_key1$, atext$()
            textid$ = bck_text$
            gosub'099(textid$)

            if txt% = 0% then L60950
            text_key$            = all(hex(00))
            str(text_key$,1%,1%) = "M"
            str(text_key$,2%,3%) = "   "
            str(text_key$,5%,4%) = textid$
            str(text_key$,9%,1%) = "1"
            sav_key1$            = text_key$
            read #10,key > text_key$, eod goto L60950

            get  #10, using L60890, text_key$, atext$()
L60890:         FMT CH(11), POS(64), 2*CH(70)

            if str(sav_key1$,1%,9%) <> str(text_key$,1%,9%) then L60950
            if atext$(1)  <> " " then text_desc$ = str(atext$(1),1%,60%) ~
                                 else text_desc$ = str(atext$(2),1%,60%)
            if text_desc$ <> " " then dt_flag$   = "Y"
L60950: return

        deffn'099(txt$)
            txt% = 0%
            if txt$ = hex(00000000) or txt$ = hex(ffffffff) or txt$ = " "~
                then L61020
            txt% = 1%
L61020: return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*
        exit_program
            call "SHOSTAT" ("One Moment Please")
            close printer

            end
