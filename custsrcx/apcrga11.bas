        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCRGA11                             *~
            *  Creation Date     - 11/28/95                             *~
            *  Last Modified Date- 01/11/06                             *~
            *  Description       - Program to Update RGA Header Status  *~
            *                      to Closed.                           *~
            *                                                           *~
            *  Special Notes     -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/28/95 ! New Program for (APC) - Last Mod Date    ! JBF *~
            *          !                                          !     *~
            * 10/31/97 ! Changed Program Version ID To 60403      ! DJD *~
            *          !                                          !     *~
            * 05/05/98 ! Y2K modifications                        ! ERN *~
            * 10/18/01 ! (EWD001) - Mod to close RGA after 45 days! CMG *~
            * 01/11/06 ! (PAR000) CR347 mods for sub part         ! CMG *~
            *************************************************************
        dim                                                              ~
            rga_hd_rec$80,               /* RGA Header File Record     */~
            rga_hd_rec1$80,              /* RGA Header File Record     */~
            rga_number$4,                /* RGA No.                    */~
            rga_cuscode$9,               /* RGA Customer No.           */~
            rga_mod_dte$8,               /* RGA Last Modified Date     */~
            hdrkey$4,                    /* APCRGAHD Read Key          */~
            hdrkey1$6,                   /* APCRGAHD Read Key (Alt 1)  */~
            cus_desc$30,                 /* Customer Description       */~
            title$40,                    /* Report Title Field         */~
            date$8,                      /* Date for screen display    */~
            blank_date$8,                /* Empty date field           */~  
            userid$3                     /* Current User Id            */

        dim f2%(3%),                     /* = 0 if the file is open    */~
            f1%(3%),                     /* = 1 if READ was successful */~
            fs%(3%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(3%)20                  /* Text from file opening     */

        dim workdate8$8                  /* 8 Char Date (mm-dd-yy)     */

        dim                              /*   (EWD001)                 */~
            rga_date$8,                  /*   RGA Enter Date           */~
            rga_fmt_date$8,              /*   RGA Formatted Date       */~
            rga_dte$6,                   /*   DATE routine date        */~
            dte$6,                       /*   DATE routine todays date */~
            lnekey$6,                    /*   APCRGADT Read Key        */~
/*PAR000*/  rga_ln_rec$(2%)256           /*   APCRGADT Record          */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 10/31/97 RGA Closed Status Update   "
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
            * #2  ! APCRGAHD ! RGA Header File                          *~
            * #3  ! CUSTOMER ! Caelus Customer Master File              *~
            * #4  ! APCRGADT ! APC RGA Detail Master File         EWD001*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

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
/*  (EWD001)  */
            select #4,  "APCRGADT",                                      ~
/*PAR000*/              varc,     indexed,  recsize =  512,              ~
                        keypos =   12, keylen =   6,                     ~
                        alt key  1, keypos =  10, keylen =   8,          ~
                            key  2, keypos =   1, keylen =  17

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            
            rga_fmt_date$, dte$ = all(hex(00))             /*  (EWD001)  */
            date$ = date
            rga_fmt_date$ = date                           /*  (EWD001)  */
            dte$     = str(rga_fmt_date$,1%,6%)            /*  (EWD001)  */
            call "DATEFMT" (date$)

            call "DATUNFMT" (blank_date$)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        REM INPUTMODE
            gosub initialize_variables
            gosub begin_process

            if proc% = 0%        then gosub print_header
            print using L55040
            print using L55040
            print using L55150
            call "SETPRNT" (" ", "RG11", 0%, 1%)           /*  (EWD001)  */

            gosub initialize_variables                     /*  (EWD001)  */
            gosub process_old                              /*  (EWD001)  */
            print using L55040
            print using L55040
            print using L55150
            call "SETPRNT" (" ", "RG11", 0%, 1%)           /*  (EWD001)  */

            goto exit_program

        REM *************************************************************~
            *               P R O C E S S   D A T A                     *~
            *************************************************************
        begin_process
            call "SHOSTAT" ("Creating Closed RGA Log")
            gosub select_log_report

            hdrkey1$            =  all(hex(00))
            str(hdrkey1$,1%,2%) = "05"

        next_apcrgahd
            read #2,key 1% > hdrkey1$, using L19130, rga_hd_rec1$,        ~
                eod goto L19230
L19130:         FMT CH(80)

            if str(rga_hd_rec1$,10%,2%) <> "05" then L19230
            if str(rga_hd_rec1$,27%,8%) = blank_date$ then L19210
            if str(rga_hd_rec1$,27%,8%) = " "         then L19210
        REM if str(rga_hd_rec1$,27%,8%)  > " "  then L19190               ~
        REM                                     else L19210

REM L19190:     gosub process_apcrgahd

L19210:     hdrkey1$ = str(rga_hd_rec1$,10%,6%)
            goto next_apcrgahd
L19230: return

        process_apcrgahd
            hdrkey$          = str(rga_hd_rec1$,12%,4%)
            read #2,hold,key = hdrkey$, using L19130, rga_hd_rec$,        ~
                eod goto L19420

            delete #2

            rga_cuscode$ = str(rga_hd_rec$, 1%,9%)
            rga_number$  = str(rga_hd_rec$,12%,4%)
            rga_mod_dte$ = str(rga_hd_rec$,43%,8%)         /*  (EWD001)  */
            if old% = 99% then str(rga_hd_rec$,27%,8%) = rga_fmt_date$

            str(rga_hd_rec$,10%,2%) = "06"
            put   #2, using L19130, rga_hd_rec$

            write #2, eod goto L19420

            gosub print_apcrgahd

L19420: return

        process_old                                 /*  (EWD001)  -  BEGIN  */
            gosub select_log_old_report
            hdrkey1$            =  all(hex(00))
        
        process_old_next
            old% = 0%
            read #2,key 1% > hdrkey1$, using L19130, rga_hd_rec1$,        ~
                eod goto process_old_done

            if str(rga_hd_rec1$,10%,2%) > "04" then process_old_done
            gosub compare_date
            hdrkey1$ = str(rga_hd_rec1$,10%,6%)
            goto process_old_next
        process_old_done
        return

        compare_date
           rga_date$, rga_dte$ = all(hex(00))
           days%, err% = 0%
           rga_date$ = str(rga_hd_rec1$,19%,8%)

           rga_dte$ = str(rga_date$,1%,6%)

           if rga_date$ = blank_date$ then goto close_rga
           if rga_date$ = " " then goto close_rga

           call "DATE" addr("G-", rga_dte$, dte$, days%, err%)

           if days% > 45% then goto close_rga
        return

        close_rga
            init(" ") rga_number$
            rga_number$ = str(rga_hd_rec1$,12%,4%)
            gosub close_rga_lines
            old% = 99%
            gosub process_apcrgahd
        return

        close_rga_lines
            lnekey$ = rga_number$

        close_rga_lines_next
            read #4, hold, key > lnekey$, using L20000, rga_ln_rec$(),  ~
                                          eod goto no_lines
L20000:           FMT 2*CH(256)           /*PAR000*/

            if rga_number$ <> str(rga_ln_rec$(),12%,4%) then goto no_lines

            lnekey$ = str(rga_ln_rec$(),12%,6%)

            delete #4            
            str(rga_ln_rec$(),10%,2%) = "23"

            put #4, using L20000, rga_ln_rec$()

            write #4

            goto close_rga_lines_next
        no_lines
        return

        select_log_old_report
            title$  = "*** APCRGA11 - RGA's Over 45 Days Closed ***"
            pageno% = 0%
            lcnt%   = 99%
            call "TIME"    (xtime$)
            call "SETPRNT" (" ", "RG11", 0%, 0%)

            select printer(134)
        return

                                                       /*  (EWD001)  - END  */

        select_log_report
            title$  = "*** APCRGA11 - RGA Closed Status Log ***"
            pageno% = 0%
            lcnt%   = 99%
            call "TIME"    (xtime$)
            call "SETPRNT" (" ", "RG11", 0%, 0%)

            select printer(134)
        return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") rga_number$, rga_cuscode$, rga_mod_dte$, cus_desc$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************
                                    /* RGA Closed Log Report Columns   */
L55040: %                                                                ~

                                    /* Header Format                   */
L55060: %  ######## @ ########   ########################################~
        ~   Page: ###
L55080: %  RGA    Customer    Customer Name                      Last Mod~
        ~.
L55100: %+ ---- ! --------- ! -------------------------------- ! --------~
        ~- +
                                    /* Detail Format                   */
L55130: %! #### ! ######### ! ################################ ! ########~
        ~  !
L55150: %       ********   RGA Closed Log Finished   *******

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************
/* Y2K */
        print_apcrgahd
            gosub lookup_customer
            str(workdate8$,1%,8%) = rga_mod_dte$
            call "DATEFMT" (workdate8$)

            if lcnt% > 55% then gosub print_header

            print using L55130, rga_number$, rga_cuscode$, cus_desc$,     ~
                workdate8$
            lcnt% = lcnt% + 1%
            if proc% = 0%  then proc% = 1%
        return

        print_header
            pageno% = pageno% + 1%
            print page
            print using L55060, date$, xtime$, title$, pageno%
            print using L55040
            print using L55080
            print using L55100
            lcnt% = 5%
        return

        lookup_customer
            read #3,key = rga_cuscode$, using L60270, cus_desc$,          ~
                eod goto L60300
L60270:         FMT POS(10), CH(30)

        return
L60300:     init(" ") cus_desc$
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            close printer

            end
