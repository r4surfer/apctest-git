        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EWDPLA69                             *~
            *  Creation Date     - 02/26/99                             *~
            *  Last Modified Date- 02/26/99                             *~
            *  Written By        - Brian W. Sanders                     *~
            *                                                           *~
            *  Description       - Write records to a file to print a   *~
            *                      glass remake barcode label.          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDPLA69 - Generates the label format and data to print   *~
            *            glass remake barcode labels. The resulting file*~
            *            is routed to the label printer via a script.   *~
            *                                                           *~
            *          - ERROR% = 0% - Lbl Ok and Sent                  *~
            *                     1% - Could Not Open Lbl Print File    *~
            *                     4% - Shell Script Error               *~
            *                     5% - Print Error Creating Label       *~
            *                     6% - Could Not Reset Table Flag       *~
            *                                                           *~
            * Subroutine - Called by BCKFASTR                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/22/99 ! Original - Copied & Mod (sub) APCFAXSD   ! BWS *~
            *************************************************************

        sub "EWDPLA69" (rec$,            /* Entire EWDPLNGT Record     */~
                        #1,              /* GENCODES Channel           */~
                        error%)          /* Return Code                */

        dim                                                              ~
            a$72, b$72,                  /* Print Lines for Label      */~
            desc$64,                     /* Descriptions for Scripts   */~
            lbl$(20)40,                  /* Label Data Array           */~
            lbl_key$24,                  /* Save File Key              */~
            fs$3,                        /* End of Lbl Records         */~
            file$8,                      /* Lbl Print File             */~
            library$8,                   /* Library Name = APCDATA     */~
            script$8,                    /* Lbl SHELL SCRIPT           */~
            volume$6                     /* DISK VOLUME = CARLOS       */

        dim gt_area$2,                   /* Glass area Location 0=Orig */~
            gt_barcode$9,                /* Glass Barcode              */~
            gt_model$3,                  /* Model Code                 */~
            gt_dept$3,                   /* Department Code            */~
            gt_view$3,                   /* View Top/Bot               */~
            gt_color$6,                  /* Product Color              */~
            gt_grid$7,                   /* Grid Value                 */~
            gt_cut_w$9,                  /* Glass Cut Width            */~
            gt_cut_h$9,                  /* Glass Cut Height           */~
            gt_ty$4,                     /* Glass Type Desc            */~
            gt_seq$5,                    /* Department Production Seq  */~
            gt_so$8,                     /* Customer Sales Order No    */~
            gt_cust$9,                   /* Customer Code              */~
            gt_prod$10,                  /* Production Date Formatted  */~
            gt_num$3,                    /* Glass Remake No            */~
            gt_stat$1,                   /* Status 0=orig,1=area,2=lab */~
            gt_win_w$7,                  /* Actual Window Width        */~
            gt_win_h$6,                  /* Actual Window Height       */~
            gt_shft$2,                   /* Production Shift Code      */~
            gt_contour$1,                /* Flag 'C'                   */~
            gt_text$40,                  /* Glass Text String          */~
            gt_filler$10,                /* Filler Area                */~
            gt_dte$6,                    /* Date Scanned (In) As Remake*/~
            gt_time$8,                   /* Time Sent to Area's (In)   */~
            gt_label$1,                  /* Label Printed (Y/N)        */~
            gt_userid$3,                 /* Prod. User ID Scanned (In) */~
            gt_dte_o$6,                  /* Date (Out) Compltd by Area */~
            gt_time_o$8,                 /* Time (Out) Compltd by Area */~
            gt_shft_o$2,                 /* Shift Code (Out) Cmpltd "  */~
            gt_userid_o$3,               /* User ID (Out) Cmpltd Area  */~
            gt_complete$6                /* Time to Complete HHH:MM    */~
            

        dim f2%(20%),                    /* = 0 if the file is open    */~
            f1%(20%)                     /* = 1 if READ was successful */ 
*           fs%(20%),                    /* = 1 if file open, -1 if it */~
*                                        /*   doesn't exist, or 0 if   */~
*                                        /*   not yet checked (OPENCHCK*/~
*           rslt$(20%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Generate Glass Remake Barcode Lbl "
            pname$ = "EWDPLA69 - Rev: R7.00"

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
            * #1  ! GENCODES ! Master Code Table File                   *~
            *************************************************************~
            * #20 ! LBLGLR?? ! Print File For Glass Remake Labels       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #20, "ROYAL", varc, consec, recsize =   72


            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            error%          =  0%  :  counter% = 0%  :  nbr_lines% = 0%
            library$        = "APCDATA "
            volume$         = "CARLOS"
            fs$ = "^FS"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        REM INPUTMODE
            gosub set_file_name
                if error% <> 0%               then exit_sub
            gosub open_file
            gosub begin_process

            goto exit_sub

        set_file_name
            init(" ") lbl_key$, desc$, file$, script$
            str(lbl_key$,1%,9%)   = "PLAN LB01"
            str(lbl_key$,10%,15%) = "LBLGLR"

        check_next_file
            read #1,hold,key > lbl_key$, using L10210, lbl_key$, desc$,   ~
                eod goto check_next_done
L10210:         FMT CH(24), CH(30)
            if str(lbl_key$,1%,15%) <> "PLAN LB01LBLGLR" then            ~
                    goto check_next_done
            if str(desc$,1%,1%) <> "-"                   then            ~
                    goto check_next_file
            put #1, using L10280, "*"
            rewrite #1
L10280:         FMT POS(25), CH(1)

            file$       = str(lbl_key$,10%,8%)
            script$     = str(desc$,3%,8%)
        return

        check_next_done
            counter%    = counter% + 1%
            if counter% < 4% then goto set_file_name
            error%      = 1%                       /* EXIT TRY LATER */
        return

        open_file
            open nodisplay #20, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return

        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *************************************************************
        begin_process
            get rec$, using L35000,                                      ~
                gt_stat$,                /* Status 0=Open, 1=Clsd(Done)*/~
                gt_area$,                /* Glass area Location 0=Orig */~
                gt_dte$,                 /* Date Scanned (In) As Remake*/~
                gt_time$,                /* Time Sent to Area's (In)   */~
                gt_barcode$,             /* Glass Barcode              */~
                gt_model$,               /* Model Code                 */~
                gt_dept$,                /* Department Code            */~
                gt_view$,                /* View Top/Bot               */~
                gt_color$,               /* Product Color              */~
                gt_grid$,                /* Grid Value                 */~
                gt_cut_w$,               /* Glass Cut Width            */~
                gt_cut_h$,               /* Glass Cut Height           */~
                gt_ty$,                  /* Glass Type Desc            */~
                gt_seq$,                 /* Department Production Seq  */~
                gt_so$,                  /* Customer Sales Order No    */~
                gt_cust$,                /* Customer Code              */~
                gt_prod$,                /* Production Date Formatted  */~
                gt_num$,                 /* Glass Remake No            */~
                gt_label$,               /* Label Printed (Y/N)        */~
                gt_win_w$,               /* Actual Window Width        */~
                gt_win_h$,               /* Actual Window Height       */~
                gt_shft$,                /* Production Shift Code      */~
                gt_userid$,              /* Prod. User ID Scanned (In) */~
                gt_contour$,             /* Flag 'C'                   */~
                gt_text$,                /* Glass Text String          */~
                gt_dte_o$,               /* Date (Out) Compltd by Area */~
                gt_time_o$,              /* Time (Out) Compltd by Area */~
                gt_shft_o$,              /* Shift Code (Out) Cmpltd "  */~
                gt_userid_o$,            /* User ID (Out) Cmpltd Area  */~
                gt_complete$,            /* Time to Complete HHH:MM    */~
                gt_filler$               /* Filler Area                */

        init (" ") lbl$()          
        lbl$(01) = gt_barcode$ & fs$
        lbl$(02) = gt_seq$ & fs$
        lbl$(03) = " " & gt_contour$ & " " & fs$
        lbl$(04) = gt_num$ & fs$
        lbl$(05) = " " & gt_dept$ & " " & fs$
        lbl$(06) = " " & gt_view$ & " " & fs$
        lbl$(07) = gt_grid$ & fs$
        lbl$(08) = gt_ty$ & fs$
        lbl$(09) = gt_text$ & fs$
        lbl$(10) = gt_model$ & fs$
        lbl$(11) = gt_so$ & fs$
        lbl$(12) = gt_prod$ & fs$
        lbl$(13) = gt_cut_h$ & fs$
        lbl$(14) = gt_cut_w$ & fs$
        lbl$(15) = gt_color$ & fs$
        lbl$(16) = gt_shft$ & fs$
        lbl$(17) = str(gt_barcode$,,8%) & ">6" & str(gt_barcode$,9%,1%) & fs$
        lbl$(18) = gt_win_w$ & "X" & gt_win_h$ & fs$

    read_loop
        a$ = " "
        b$ = all(hex(00))
        nbr_lines% = nbr_lines% + 1%
        restore line = lbl_fmt, nbr_lines%
        read a$
        if a$ = " " then return
        a_len% = len(a$)
        str(b$,,a_len%) = str(a$,,a_len%)
        convert str(a$,,2%) to ln%, data goto skip_data
        l_len% = len(lbl$(ln%))
        b_len% = a_len% + l_len%
        str(b$,,b_len%) = str(a$,3%,a_len%-2%)                           ~
            & str(lbl$(ln%),,l_len%)
      skip_data
        if nbr_lines% = 1% then b$ = bin(126,1) & b$
        gosub print_line
        if a$ = "^XZ" then return       /* Last Line */
        goto read_loop


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
L35000:     FMT                                                          ~
                CH(1),                   /* Status 0=Open, 1=Closed (Do*/~
                CH(2),                   /* Glass Area Location 0=Orig.*/~
                CH(6),                   /* Date Scanned (In) as Remake*/~
                CH(8),                   /* Time Sent to Area's (In)   */~
                CH(9),                   /* Glass Barcode              */~
                CH(3),                   /* Model Code                 */~
                CH(3),                   /* Department Code            */~
                CH(3),                   /* View Top/Bot               */~
                CH(6),                   /* Product Color              */~
                CH(7),                   /* Grid Value                 */~
                CH(9),                   /* Glass Cut Width            */~
                CH(9),                   /* Glass Cut Height           */~
                CH(4),                   /* Glass Type Desc            */~
                CH(5),                   /* Department Production Seq  */~
                CH(8),                   /* Customer Sales Order No    */~
                CH(9),                   /* Customer Code              */~
                CH(10),                  /* Production Date Formatted  */~
                CH(3),                   /* Glass Remake No            */~
                CH(1),                   /* Label Printed (Y/N)        */~
                CH(7),                   /* Actual Window Width        */~
                CH(6),                   /* Actual Window Height       */~
                CH(2),                   /* Production Shift Code      */~
                CH(3),                   /* Prod. User ID Scanned (In) */~
                CH(1),                   /* Flag 'C' (Contour)         */~
                CH(40),                  /* Glass Text String          */~
                CH(6),                   /* Date (Out) Compltd by Area */~
                CH(8),                   /* Time (Out) Compltd by Area */~
                CH(2),                   /* Shift Code (Out) Cmpltd "  */~
                CH(3),                   /* User ID (Out) Cmpltd Area  */~
                CH(6),                   /* Time to Complete HHH:MM    */~
                CH(10)                   /* Filler Area                */


        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************
L55030:     FMT CH(72)

        REM *************************************************************~
            *           S P E C I A L   R O U T I N E S                 *~
            *************************************************************

        print_line
            write #20, using L55030, b$, eod goto L61550
        return

L61550:     error% = 5%
        return clear all

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************
        exit_sub
            counter%       = 0% : lb1% = 0% : lb2% = 0%
            if error%      = 1% then end
            close #20
                if error% <> 0% then delete_lbl

            call "LINK" addr(script$, lb1%, lb2%)
                if lb1%    > 0% then error% = 4%

        delete_lbl
            call "FILEBGON" (#20)          /* Scratch Lbl File        */
L65140:     read    #1,hold,key = lbl_key$, eod goto L65190
            put     #1, using L10280, "-"
            rewrite #1

        end
L65190:     counter%       = counter% + 1%
            if counter%    < 4% then L65140
            error%         = 6%
        end


lbl_fmt : DATA                                                           ~
"JO",~
"^XA",~
"^PMN",~
"^MNY",~ 
"^MMT",~ 
"^MTT",~ 
"^MD0",~ 
"^LH0,0",~ 
"^LL254",~
"^PR2",~
"^JMA",~
"^FO569,18^FR^GB30,26,26^FS",~
"^FO423,77^FR^GB53,26,26^FS",~
"^FO345,77^FR^GB63,26,26^FS",~
"01^FO652,51^CI0^ARN,28,15^FR^FD",~
"02^FO715,20^CI0^ARN,28,15^FR^FD",~
"^FO634,18^CI0^ARN,28,15^FR^FDSEQ:^FS",~
"03^FO569,18^CI0^ARN,28,15^FR^FD",~
"04^FO368,116^CI0^ARN,56,30^FR^FD",~
"05^FO423,77^CI0^ARN,28,15^FR^FD",~
"06^FO345,77^CI0^ARN,28,15^FR^FD",~
"07^FO390,18^CI0^ARN,28,15^FR^FD",~
"^FO319,18^CI0^ARN,28,15^FR^FDGRID:^FS",~
"08^FO234,20^CI0^ARN,28,15^FR^FD",~
"09^FO55,183^CI0^ARN,28,15^FR^FD",~
"10^FO102,148^CI0^ARN,28,15^FR^FD",~
"11^FO102,122^CI0^ARN,28,15^FR^FD",~
"12^FO102,98^CI0^ARN,28,15^FR^FD",~
"13^FO102,71^CI0^ARN,28,15^FR^FD",~
"14^FO102,47^CI0^ARN,28,15^FR^FD",~
"15^FO102,20^CI0^ARN,28,15^FR^FD",~
"^FO30,148^CI0^ARN,28,15^FR^FDMOD:^FS",~
"^FO33,122^CI0^ARN,28,15^FR^FDSO.:^FS",~
"^FO33,98^CI0^ARN,28,15^FR^FDDTE:^FS",~
"^FO33,71^CI0^ARN,28,15^FR^FDHGT:^FS",~
"^FO33,47^CI0^ARN,28,15^FR^FDWID:^FS",~
"^FO33,20^CI0^ARN,28,15^FR^FDCLR:^FS",~
"^FO195,148^CI0^ARN,28,15^FR^FDSH:^FS",~
"16^FO244,148^CI0^ARN,28,15^FR^FD",~
"17^FO569,79^BY2,2.0,102^BCN,102,N,N,N^FR^FD>;",~
"18^FO323,47^CI0^ARN,28,15^FR^FD",~
"^PQ1",~
"^XZ"

