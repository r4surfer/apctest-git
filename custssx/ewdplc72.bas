        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EWDPLC72                             *~
            *  Creation Date     - 07/26/99                             *~
            *  Last Modified Date- 04/10/06                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Generate Glass Barcode Label         *~
            *                                                           *~
            *  Process Selection (4) = Special Glass Labels             *~    
            *               Note - PLAN LB01 is used for Production     *~
            *                      Glass. Same as for Re-Make           *~               
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDPLC72 - Generates the label format and data to print   *~
            *            Special Glass Barcode Labels. The resulting    *~
            *            file is routed to the label printer via a      *~
            *            script file.                                   *~
            *                                                           *~
            *          - ERROR% = 0% - Lbl Ok and Sent                  *~
            *                     1% - Could Not Open Lbl Print File    *~
            *                     4% - Shell Script Error               *~
            *                     5% - Print Error Creating Label       *~
            *                     6% - Could Not Reset Table Flag       *~
            *                                                           *~
            * Subroutine - Called by EWDPLN72                           *~
            *                                                   (PAR000)*~
            *            Note - New Sub Part Number is in 'APCPLNGR'    *~
            *                   in position 255% for 20 characters.     *~
            *                   Lookup using <Glass Barcode><Re-Make No>*~
            *                                                           *~ 
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/10/99 ! Original - New Program                   ! RHH *~
            * 12/20/99 ! (EWD001) Mod for new 12 Digit Barcode for! RHH *~
            *          !          glass label                     !     *~ 
            * 01/01/06 ! (PAR000) CR347 Mod for new Sub Part No.  ! RHH *~                    
            * 03/27/06 ! (PAR001) Mod for Printing Labels in the  ! RHH *~
            *          !          North East.                     !     *~                    
            * 04/10/06 ! (PAR002) Mod for NE test Label Script    ! RHH *~
            *          !          not Applicable                  !     *~
            *************************************************************

        sub "EWDPLC72" (gl_rec$,         /* Entire EWDGLSXX Record     */~
                        sel%,            /* Selection Process          */~
                        #3,              /* APCPLNGR           (PAR000)*/~   
                        #1,              /* GENCODES Channel           */~
                        #2,              /* TXTFILE                    */~
                        error%)          /* Return Code                */

        dim                                                              ~
            schema$8,                    /* (PAR001) Schema Switch     */~
            rm_rec$(2%)192,              /* New Remake Glass Rec(PAR000)*/~
            rm_key$12,                   /* Primary Re-Make Key(PAR000)*/~
            sub_part$20,                 /* New Sub Part No.   (PAR000)*/~
            gl_rec$256,                  /* Re-make record             */~
            a$72, b$72,                  /* Print Lines for Label      */~
            desc$64,                     /* Descriptions for Scripts   */~
            lbl$(20%)40,                 /* Label Data Array           */~
            xx$(100%)72,                 /* Label Buffer               */~
            lbl_key$24,                  /* Save File Key              */~
            txt$40,                      /* Detail Text Id             */~
            txt1$40,                     /* Line text (2) Special      */~
            fs$3,                        /* End of Lbl Records         */~
            file$8,                      /* Lbl Print File             */~
            library$8,                   /* Library Name = APCDATA     */~
            script$8,                    /* Lbl SHELL SCRIPT           */~
            volume$6                     /* DISK VOLUME = CARLOS       */

        dim gl_barcode$9,                /* Glass Barcode              */~
            gl_num1$3,                   /* Re-Make Number (EWD001)    */~
            gl_model$3,                  /* Model Code                 */~
            gl_dept$3,                   /* Department Code            */~
            gl_view$3,                   /* View Top/Bot               */~
            gl_color$6,                  /* Product Color              */~
            gl_grid$7,                   /* Grid Value                 */~
            gl_cut_w$10,                 /* Glass Cut Width            */~
            gl_cut_h$10,                 /* Glass Cut Height           */~
            gl_ty$4,                     /* Glass Type Desc            */~
            gl_seq$5,                    /* Department Production Seq  */~
            gl_so$8,                     /* Customer Sales Order No    */~
            gl_prod$10,                  /* Production Date Formatted  */~
            gl_num$3,                    /* Glass Remake No Print Ver  */~
            gl_win_w$7,                  /* Actual Window Width        */~
            gl_win_h$6,                  /* Actual Window Height       */~
            gl_shft$2,                   /* Production Shift Code      */~
            gl_contour$1                 /* Flag 'C'                   */

        dim f2%(2%),                     /* = 0 if the file is open    */~
            f1%(2%)                      /* = 1 if READ was successful */
 
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(AWD)Generate Special Glass Barcode Lable"
            pname$ = "EWDPLC72 - PAR: 01.00"

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
            * #2  ! TXTFILE  ! Master Text File                         *~  
            * #3  ! APCPLNGR ! Master Glass Remake File         (PAR000)*~           
            *************************************************************~
            * #5  ! LBLGLR?? ! Print File For Glass Remake Labels       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #5, "ROYAL", varc, consec, recsize =   72

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            err%    = 0%                 /* (PAR001)                   */
            call "SCHEMA" (schema$,      /* What switch 1-NC 2-NE      */~
                           schema%,      /* Schema                     */~
                           #1,           /* GENCODES                   */~
                           err% )        /* error                      */

            if err% = 0% then goto SS_1
               error% = 9%
               end
SS_1:


            error%          =  0%  :  counter% = 0%  :  nbr_lines% = 0%
            fs$ = "^FS"

            sel% = 4%                                    /* Not Used   */       

            xx$(1%) = "^JO"
            xx$(2%) = "^XA^EG^XZ"
            xx$(3%) = "^XA"
            xx$(4%) = "^PMN"
            xx$(5%) = "^MNY"
            xx$(6%) = "^MMT"
            xx$(7%) = "^MTT"
            xx$(8%) = "^MD0"
            xx$(9%) = "^LH0,0"
            xx$(10%)= "^LL203"
            xx$(11%)= "^PR4"
            xx$(12%)= "^JMA"

            xx$(13%)= "^FO569,10^FR^GB26,24,24^FS"
            xx$(14%)= "^FO392,45^FR^GB57,24,24^FS"
            xx$(15%)= "^FO315,45^FR^GB53,24,24^FS"
                                                        /* Glass Barcode */
            xx$(16%)= "01^FO478,41^BY2,2.0,87^BCN,87,N,N,N^FR^FD>:"

            xx$(17%)= "^FO317,10^CI0^A0N,26,27^FR^FDGRID:^FS"
            xx$(18%)= "^FO30,114^CI0^A0N,26,27^FR^FDSO.:^FS"
            xx$(19%)= "^FO30,89^CI0^A0N,26,28^FR^FDDTE:^FS"
            xx$(20%)= "^FO30,61^CI0^A0N,26,27^FR^FDHGT:^FS"
            xx$(21%)= "^FO30,37^CI0^A0N,26,26^FR^FDWID:^FS"
            xx$(22%)= "^FO30,10^CI0^A0N,26,27^FR^FDCLR:^FS"

                                                  /* Glass Barcode Value */
            xx$(23%)= "01^FO660,12^CI0^A0N,26,26^FR^FD"
                                                  /* Color Code          */
            xx$(24%)= "15^FO102,10^CI0^A0N,26,29^FR^FD"
                                                  /* Glass Type Code     */
            xx$(25%)= "08^FO232,10^CI0^A0N,26,29^FR^FD"
                                                  /* Grid Value          */ 
            xx$(26%)= "07^FO392,10^CI0^A0N,26,28^FR^FD"
                                                  /* Cut Width           */
            xx$(27%)= "14^FO102,37^CI0^A0N,26,26^FR^FD"
                                                  /* Cut Height          */
            xx$(28%)= "13^FO102,61^CI0^A0N,26,28^FR^FD"
                                                  /* Production Date     */
            xx$(29%)= "12^FO102,89^CI0^A0N,26,26^FR^FD"
                                                  /* Sales Order         */
            xx$(30%)= "11^FO102,114^CI0^A0N,26,28^FR^FD"
                                                  /* Contour Grid        */
            xx$(31%)= "03^FO569,10^CI0^A0N,26,24^FR^FD"
                                                  /* Dept code           */
            xx$(32%)= "05^FO392,45^CI0^A0N,26,26^FR^FD"
                                                  /* Top/Bot Descript    */
            xx$(33%)= "06^FO315,45^CI0^A0N,26,24^FR^FD"
                                                  /* Vertical Text       */ 
            xx$(34%)= "09^FO102,142^CI0^A0N,26,28^FR^FD"
                                                  /* Model Descript      */ 
            xx$(35%)= "^FO315,81^CI0^A0N,26,26^FR^FDMOD:^FS"
                                                  /* Model code          */
            xx$(36%)= "10^FO384,81^CI0^A0N,26,28^FR^FD"
                                                  /* Seq. Descript       */
            xx$(37%)= "^FO315,112^CI0^A0N,26,26^FR^FDSEQ:^FS"
                                                  /* Prod Seq No.        */
            xx$(38%)= "02^FO384,112^CI0^A0N,26,28^FR^FD"
                                                  /* Text Descript       */
            xx$(39%)= "^FO35,142^CI0^A0N,26,26^FR^FDVT:^FS"
            xx$(40%)= "^FO35,173^CI0^A0N,26,26^FR^FDHZ:^FS"
                                                  /* Horizontal Text     */
            xx$(41%)= "18^FO102,173^CI0^A0N,26,28^FR^FD"

            xx$(42%)= "^PQ1"
            xx$(43%)= "^XZ"


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
            if schema% = 2% then goto set_file_name1   /* (PAR001)     */

            library$ = "APCDATA"
            volume$  = "CARLOS"
            init(" ") lbl_key$, desc$, file$, script$
            str(lbl_key$,1%,9%)   = "PLAN LB01"
            str(lbl_key$,10%,15%) = "LBLGLR"

        check_next_file
            read #1,hold,key > lbl_key$, using L10210, lbl_key$, desc$,  ~
                eod goto check_next_done
L10210:         FMT CH(24), CH(30)
            if str(lbl_key$,1%,15%) <> "PLAN LB01LBLGLR" then            ~
                    goto check_next_done
            if str(desc$,1%,1%) <> "-"                   then            ~
                    goto check_next_file
            put #1, using L10280, "*"
            rewrite #1
L10280:         FMT POS(25), CH(1)

            file$       = str(lbl_key$,10%,8%)  /* Print Filename  */
                                                /* LBLGLR??        */
            script$     = str(desc$,3%,8%)      /* Script Program  */
                                                /* Name EWDGRS??   */ 
        return
                                                /* (PAR001)        */
        set_file_name1
            library$ = "NEDATA "
            volume$  = "NE    "

            init(" ") lbl_key$, desc$, file$, script$
            str(lbl_key$,1%,9%)   = "PLAN LB08"
            str(lbl_key$,10%,15%) = "LBLNGR"

        check_next_file1
            read #1,hold,key > lbl_key$, using L10210, lbl_key$, desc$,   ~
                eod goto check_next_done
            if str(lbl_key$,1%,15%) <> "PLAN LB08LBLNGR" then            ~
                    goto check_next_done
            if str(desc$,1%,1%) <> "-"                   then            ~
                    goto check_next_file1
            put #1, using L10280, "*"
            rewrite #1

            file$       = str(lbl_key$,10%,8%)  /* Print Filename  */
                                                /* LBLNGR??        */
            script$     = str(desc$,3%,8%)      /* Script Program  */
                                                /* Name NEAGRS??   */ 
        return

        check_next_done
            counter%    = counter% + 1%
            if counter% < 4% then goto set_file_name
            error%      = 1%                       /* EXIT TRY LATER */
        return

        open_file
            open nodisplay #5, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return

        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *************************************************************
        begin_process                                       /* (PAR000) */  
            init (" ") lbl$(), rm_rec$()
            gosub format_special_label
 
    read_loop
        a$ = " "
        b$ = all(hex(00))
        nbr_lines% = nbr_lines% + 1%
        a$ = xx$(nbr_lines%)

        if a$ = " " then return
        a_len% = len(a$)                    /* Calc Length of Data String */
        str(b$,1%,a_len%) = str(a$,1%,a_len%) /* Put into b$ Data from a$   */
        convert str(a$,1%,2%) to ln%, data goto skip_data
                                            /* Look for a Field Number    */ 
        l_len% = len(lbl$(ln%))             /* Find Length of Data Element*/
                                            /* in the Label data array    */
        b_len% = (a_len% - 2%) + l_len%     /* Adjust for 2 digit field No*/

        b$ = all(hex(00))                  /* Initialize Print string    */
                                           /* 1st set Font data for print*/
                                           /* 2nd append Actual Data that*/
                                           /*     will be printed        */    
        str(b$,1%,b_len%) = str(a$,3%,a_len%-2%)                           ~
            & str(lbl$(ln%),1%,l_len%)

      skip_data
        if nbr_lines% = 1% then b$ = bin(126,1) & b$
        gosub print_line
        if a$ = "^XZ" then return       /* Last Line */
        goto read_loop


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

format_special_label                              /* Format (EWDGLSXX) */
                                                  /* (PAR000)          */
        gosub lookup_sub_part
                                                  /* (PAR000)          */

        gl_barcode$ = str(gl_rec$,33%,9%)         /* Glass Barcode     */
        gl_num1$    = str(gl_rec$,42%,3%)         /* (EWD001) Re-Make No*/          
        lbl$(1%) = gl_barcode$ & gl_num1$ & fs$   /* (EWD001) 12 Digit  */
                                                  /* Production Seq No.*/
        gl_seq$ = str(gl_rec$,87%,5%) 
        lbl$(2%) = str(gl_seq$,2%,4%) & fs$       /* Last 4 Digits     */

                                                  /* (PAR000)          */
        gl_contour$ = str(gl_rec$,199%,1%)        /* Contour Grid Flag */
        lbl$(3%) = " " & gl_contour$ & " " & fs$
                                                  /* (PAR000)          */

                                                  /* Glass Remake No   */
        gl_num$ = str(gl_rec$,200%,3%)            /* Print Version     */  
        lbl$(4%) = gl_num$ & fs$
                                                  /* Department Code   */
        gl_dept$ = str(gl_rec$,107%,3%)
        lbl$(5%) = " " & gl_dept$ & " " & fs$
                                                  /* View Top / Bot    */
        gl_view$ = str(gl_rec$,190%,3%)
        lbl$(6%) = " " & gl_view$ & " " & fs$
                                                  /* Grid Value        */
        gl_grid$ = str(gl_rec$,47%,7%)
        lbl$(7%) = gl_grid$ & fs$
                                                  /* Glass Type Code   */
        gl_ty$ = " " & str(gl_rec$,45%,2%) & " "
        lbl$(8%) = gl_ty$ & fs$
                                                  /* Glass Text        */
        txt$ = str(gl_rec$,110%,40%)              /* Line (1) Vertical */
        lbl$(9%) = txt$ & fs$
                                                  /* Model Code        */
        gl_model$ = str(gl_rec$,72%,3%)
        lbl$(10%)= gl_model$ & fs$
                                                  /* Sales Order       */
        gl_so$ = str(gl_rec$,54%,8%)
        lbl$(11%)= gl_so$ & fs$
                                                  /* Production Date   */
        gl_prod$ = str(gl_rec$,2%,6%)
        call "DATFMTC" (gl_prod$)
        lbl$(12%)= gl_prod$ & fs$
                                                  /* Cut Height        */
        gl_cut_h$ = str(gl_rec$,75%,10%)
        lbl$(13%)= gl_cut_h$ & fs$
                                                  /* Cut Width         */
        gl_cut_w$ = str(gl_rec$,62%,10%)  
        lbl$(14%)= gl_cut_w$ & fs$
                                                  /* Color             */
        gl_color$ = str(gl_rec$,193%,6%)
        lbl$(15%)= gl_color$ & fs$
                                                  /* Shift Code        */
        gl_shft$ = str(gl_rec$,85%,2%) 
        lbl$(16%)= gl_shft$ & fs$
                                                  /* Actual Width & Height*/
        gl_win_w$= str(gl_rec$,92%,7%)
        gl_win_h$= str(gl_rec$,101%,6%) 
        lbl$(17%)= gl_win_w$ & "X " & gl_win_h$ & fs$
                                                  /* Glass Text        */
        txt1$ = str(gl_rec$,150%,40%)             /* Line (2) Horizontal*/
        lbl$(18%) = txt1$ & fs$

        return
    
        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************
L55030:     FMT CH(72)

        REM *************************************************************~
            *           S P E C I A L   R O U T I N E S                 *~
            *************************************************************

        print_line
            write #5, using L55030, b$, eod goto L61550
        return

L61550:     error% = 5%
        return clear all
                                                     /* (PAR000)          */
        lookup_sub_part
           init(" ") rm_key$, sub_part$
        return                                       /* Disable for Now   */

           str(rm_key$,1%,9%)  = str(gl_rec$,33%,9%)
           str(rm_key$,10%,3%) = str(gl_rec$,42%,3%)
           read #3,key 0% = rm_key$, using L64000, sub_part$, eod goto L64010
L64000:            FMT POS(257), CH(20)
REM FMT POS(255), CH(20)

           return 
L64010:       sub_part$ = "00000               "
        return
                                                     /* (PAR000)          */
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************
        exit_sub
            counter%       = 0% : lb1% = 0% : lb2% = 0%
            if error%      = 1% then end
            close #5
                if error% <> 0% then delete_lbl

            call "LINK" addr(script$, lb1%, lb2%)
                if lb1%    > 0% then error% = 4%

        delete_lbl
            call "FILEBGON" (#5)          /* Scratch Lbl File        */
L65140:     read    #1,hold,key = lbl_key$, eod goto L65190
            put     #1, using L10280, "-"
            rewrite #1

        end
L65190:     counter%       = counter% + 1%
            if counter%    < 4% then L65140
            error%         = 6%
        end



