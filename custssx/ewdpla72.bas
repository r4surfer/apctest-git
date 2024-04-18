        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EWDPLA72                             *~
            *  Creation Date     - 07/26/99                             *~
            *  Last Modified Date- 04/10/06                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Generate Glass Barcode Label         *~
            *                                                           *~
            *  Process Selection (2) = Glass Production Labels          *~
            *               Note - Print File  (GLASSPRD)               *~
            *                      Script File (GLASSPRD)               *~               
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDPLA72 - Generates the label format and data to print   *~
            *            Production Glass Barcode Labels. The resulting *~
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
            * 04/11/00 ! Major Mods to put all labels in a single ! RHH *~
            *          !   File.                                  !     *~
            * 07/30/03 ! (EWD003) Mod to improve speed            ! RHH *~
            * 01/01/06 ! (PAR000) CR347 Mod for new Sub Part No.  ! RHH *~
            * 03/24/06 ! (PAR001) Mod to print Glass production   ! RHH *~
            *          !          for North East.                 !     *~
            * 04/10/06 ! (PAR002) Mod for NE test Label Script    ! RHH *~
            * 04/02/07 ! (PAR003) Mod for using grid color        ! DES *~
            *03/22/2011! (AWD004) mod for intercepts              ! CMG *~
            *09/03/2014! (AWD005) mod for breathing tubes         ! PWW *~
            *************************************************************

        sub "EWDPLA72" (gl_rec$,         /* Entire EWDGLSXX Record     */~
                        sel%,            /* Selection Process          */~
                        #2,              /* APCPLNGR           (PAR000)*/~   
                        #1,              /* GENCODES Channel           */~
                        error%)          /* Return Code                */

        dim                                                              ~
            schema$8,                    /* (PAR003) Schema Switch     */~
            rm_rec$(2%)192,              /* New Remake Glass Rec(PAR000)*/~
            rm_key$12,                   /* Primary Re-Make Key(PAR000)*/~
            sub_part$20,                 /* New Sub Part No.   (PAR000)*/~
            gl_rec$256,                  /* Re-make record             */~
            a$72, b$72,                  /* Print Lines for Label      */~
            hdr$40,                      /* ASKUSER Header Text        */~
            msg$(3%)79,                  /* ASKUSER Info Text          */~
            lbl$(20%)60,                 /* Label Data Array           */~
            yy$(50%)72,                  /* Label Buffer               */~
            xx$(50%)72,                  /* Label Buffer               */~
            txt$40,                      /* Detail Text Id Line (1)    */~
            txt1$40,                     /* Line text (2) Special      */~
            fs$3,                        /* End of Lbl Records         */~
            file$8,                      /* Lbl Print File             */~
            library$8,                   /* Library Name = APCDATA     */~
            script$8,                    /* Lbl SHELL SCRIPT           */~
            volume$6                     /* DISK VOLUME = CARLOS       */

        dim gl_barcode$9,                /* Glass Barcode              */~
            gl_num1$3,                   /* Glass Barcode      (EWD001)*/~
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
            gl_contour$1,                /* Flag 'C'                   */~
            gl_breatht$1,                /* Flag 'B'     <AWD005>      */~
            gl_ultra$1,                  /* Ultra Flag                 */~
            gl_intercept$2               /* (AWD004) Intercpet         */
            
        dim interdesc$(99)5              /* (AWD004) intercept desc    */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */
 
	    dim gc_key$24, gc_desc$32    /* PAR003 */  
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(AWD)Generate Glass Production Barcode Label"
            pname$ = "EWDPLA72 - PAR: 01.00"

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
            * #2  ! APCPLNGR ! Master Glass Remake File         (PAR000)*~           
            *************************************************************~
            * #5  ! GLASSPRD ! Print File For Glass Production Labels   *~
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
            error%          =  0%  :   nbr_lines% = 0%
            fs$ = "^FS"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        
            init(" ") xx$()
            if beenherebefore% = 1% then goto L01000
               gosub load_label           /* Build Label Format yy$() */ 
               gosub set_file_name        /* Create Label Print File  */
               gosub open_file            /* Open Label Print file    */
               gosub load_interdesc                   /* (AWD004)  */
               beenherebefore% = 1%

L01000:        if sel% = 99% then goto exit_print
                  copy yy$() to xx$()
                  gosub begin_process

                  goto exit_sub

        set_file_name                    /* (EWD003)                   */
            init(" ") file$, script$     
                                         /* (PAR001)                   */
            err%    = 0%
            call "SCHEMA" (schema$,      /* What switch 1-NC 2-NE      */~
                           schema%,      /* Schema                     */~
                           #1,           /* GENCODES                   */~
                           err% )        /* error                      */

            if err% = 0% then goto SS_1
               error% = 9%
               end

SS_1:                                                /* (PAR001)        */
            if schema% <> 1% then goto SS_2
                                                     /* North Caolina   */
               file$    = "GLASSPRD"
               script$  = "GLASSPRD"

               library$ = "APCDATA "
               volume$  = "CARLOS"
               goto SS_3
                                                     /* North East      */
SS_2:
               file$    = "NEGLASS"
               script$  = "NEGLASS"

               library$ = "NEDATA  "
               volume$  = "NE    "

SS_3:
        REM    file$   = "MFGTEST"        /* (RHHTEST)             */ 
        REM    script$ = "MFGTEST"
                                          /* (PAR002)              */
        REM    file$   = "NEATEST"        /* (RHHTEST)             */ 
        REM    script$ = "NEATEST"

        return                             

        open_file
            call "OPENFILE" (#5, "IO   ", f2%(5%), rslt$(5%), axd$ )
            if f2%(5%) <> 0% then goto L01100
               gosub file_exists         
               if comp% <> 16% then goto exit_sub
                  call "FILEBGON" (#5)

L01100:    open nodisplay #5, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return

        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *************************************************************
        begin_process                       /* (PAR000)                 */
            init (" ") lbl$(), rm_rec$()
            gosub format_production_label
 
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

format_production_label                           /* Format (EWDGLSXX) */
                                                  /* (PAR000)          */
        gosub lookup_sub_part
                                                  /* (PAR000)          */
        gl_barcode$ = str(gl_rec$,33%,9%)         /* Glass Barcode     */
        gl_num1$    = str(gl_rec$,42%,3%)         /* (EWD001)          */
        lbl$(1%) = gl_barcode$ & gl_num1$ & fs$   /* (EWD001) 12 Digit */
  
                                                  /* Production Seq No.*/
        gl_seq$ = str(gl_rec$,87%,5%) 
        lbl$(2%) = str(gl_seq$,1%,5%) & fs$       /* Last 5 Digits     */
                                                  /* (PAR000) 'C' or 'W'*/
        gl_contour$ = str(gl_rec$,199%,1%)        /* Contour Grid Flag */
        lbl$(3%) = " " & gl_contour$ & " " & fs$
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
        txt$ = str(gl_rec$,110%,40%)              /* Line (1)          */
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

        lbl$(16%) = " "
/* (AWD004)  */
REM  !        gl_ultra$ = str(gl_rec$,255%,1%)
REM  !        if gl_ultra$ = "1" then lbl$(16%) = "ULTRA" & fs$
        gl_intercept$ = str(gl_rec$,255%,2%)
        convert gl_intercept$ to gl_intercept%, data goto dataerr1
        
dataerr1:

        lbl$(16%) = interdesc$(gl_intercept%) & fs$
/* (\AWD004) */
                                                  /* Actual Width & Height*/
        gl_win_w$= str(gl_rec$,92%,7%)
        gl_win_h$= str(gl_rec$,101%,6%) 
        lbl$(17%)= gl_win_w$ & "X " & gl_win_h$ & fs$
                                                  /* Glass Text        */
        txt1$ = str(gl_rec$,150%,40%)             /* Line (2)          */
        lbl$(18%) = txt1$ & fs$
                                                  /* (AWD005) 'B'      */
        gl_breatht$ = " "                /*<AWD005>  Breath Tube Flag  */
        if str(sub_part$,7%,1%) = "4" then gl_breatht$ = "B"      
/*      gl_breatht$ = "B"                       temp for testing */
        lbl$(19%) = " " & gl_breatht$ & " " & fs$

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

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************
        exit_sub

        end

        exit_print
            lb1% = 0% : lb2% = 0%

            close #5

            call "LINK" addr(script$, lb1%, lb2%)
                if lb1%    > 0% then error% = 4%

            call "FILEBGON" (#5)          /* Scratch 'GLASSPRD'   */
            beenherebefore% = 0%

        end

        load_label
            init(" ") yy$()

            yy$(1%) = "^JO"
            yy$(2%) = "^XA^EG^XZ"

            yy$(3%) = "^XA"
            yy$(4%) = "^PMN"
            yy$(5%) = "^MNY"
            yy$(6%) = "^MMT"                         /* Back Feed Off */
                                                     /* R=Off, T=On   */                   

            yy$(7%) = "^MTT"
            yy$(8%) = "^MD0"
            yy$(9%) = "^LH0,0"
            yy$(10%)= "^LL203"

            yy$(11%)= "^PR6,4,8"                     /* PR Label Print Speed*/
                                                     /* a = 3 Print Speed   */
                                                     /* b = 4 Slew  SPeed   */
                                                     /* c = 8 Back Feed Spee*/ 
            yy$(12%)= "^JMA"

            yy$(13%)= "^COY,362"                     /* 256 + 128 Mem */
                                                     /* (-) 22k Intern*/

/*          yy$(14%)= "^FO569,10^FR^GB26,24,24^FS"         <AWD005> */
            yy$(14%)= "^FO569,10^FR^GB26,24,24^FS^FO532,10^FR^GB26,24,24^FS"
            yy$(15%)= "^FO392,69^FR^GB57,24,24^FS"
            yy$(16%)= "^FO315,69^FR^GB47,24,24^FS"
                                                        /* Glass Barcode */
            yy$(17%)= "01^FO580,85^BY2,2.0,87^BCN,87,N,N,N^FR^FD>;"

            yy$(18%)= "^FO632,10^CI0^A0N,26,30^FR^FDSEQ:^FS"
            yy$(19%)= "^FO317,10^CI0^A0N,26,27^FR^FDGRID:^FS"
            yy$(20%)= "^FO30,140^CI0^A0N,26,27^FR^FDMOD:^FS"
            yy$(21%)= "^FO30,112^CI0^A0N,26,27^FR^FDSO.:^FS"
            yy$(22%)= "^FO30,89^CI0^A0N,26,28^FR^FDDTE:^FS"
            yy$(23%)= "^FO30,61^CI0^A0N,26,27^FR^FDHGT:^FS"
            yy$(24%)= "^FO30,37^CI0^A0N,26,26^FR^FDWID:^FS"
            yy$(25%)= "^FO30,10^CI0^A0N,26,27^FR^FDCLR:^FS"
REM            yy$(26%)= "^FO193,140^CI0^A0N,26,26^FR^FDSH^FS"

            yy$(26%) = "^FO193,140^FR^GB75,24,24^FS"
                                                  /* Glass Barcode Value */
            yy$(27%)= "01^FO595,43^CI0^A0N,26,26^FR^FD"
                                                  /* Color Code          */
            yy$(28%)= "15^FO102,10^CI0^A0N,26,29^FR^FD"
                                                  /* Glass Type Code     */
            yy$(29%)= "08^FO232,10^CI0^A0N,26,29^FR^FD"
                                                  /* Grid Value          */ 
            yy$(30%)= "07^FO392,10^CI0^A0N,26,28^FR^FD"
                                                  /* Cut Width           */
            yy$(31%)= "14^FO102,37^CI0^A0N,26,27^FR^FD"
                                                  /* Cut Height          */
            yy$(32%)= "13^FO102,61^CI0^A0N,26,28^FR^FD"
                                                  /* Production Date     */
            yy$(33%)= "12^FO102,89^CI0^A0N,26,26^FR^FD"
                                                  /* Sales Order         */
            yy$(34%)= "11^FO102,112^CI0^A0N,26,29^FR^FD"
                                                  /* Model code          */
            yy$(35%)= "10^FO102,140^CI0^A0N,26,26^FR^FD"
                                                  /* Shift Code NOW ULTRA */
            yy$(36%)= "16^FO193,140^CI0^A0N,26,28^FR^FD"
                                                  /* Prod Seq No.        */
            yy$(37%)= "02^FO713,10^CI0^A0N,26,29^FR^FD"
                                                  /* Actual Windowb W/H  */
            yy$(38%)= "17^FO317,39^CI0^A0N,26,28^FR^FD"
                                                  /* Contour Grid        */
            yy$(39%)= "03^FO569,10^CI0^A0N,26,25^FR^FD"
                                                  /* Remake Number       */
            yy$(40%)= "04^FO335,106^CI0^A0N,55,56^FR^FD"
                                                  /* Dept code           */
            yy$(41%)= "05^FO392,69^CI0^A0N,26,26^FR^FD"
                                                  /* Top/Bot Descript    */
            yy$(42%)= "06^FO315,69^CI0^A0N,26,25^FR^FD"
                                                  /* Glass Text          */ 
            yy$(43%)= "09^FO53,173^CI0^A0N,26,28^FR^FD"
                                                  /* Breath Tube <AWD005>*/
            yy$(44%)= "19^FO532,10^CI0^A0N,26,25^FR^FD"

            yy$(45%)= "^PQ1"
            yy$(46%)= "^XZ"

        return

        file_exists
          comp% = 2%       
          hdr$ = "*Glass Production File Exists*"
          msg$(1%) = "       The File (GLASSPRD) Already Exists.      "
          msg$(2%) = "  G l a s s   P r o d u c t i o n   L a b e l s  "
          msg$(3%) = "Press <RETURN> To Exit Prog, or PF(16) to Delete."
          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                     /* (PAR000)          */
        lookup_sub_part
           init(" ") rm_key$, sub_part$
/*      return */                                    /* For Now Disable   */

           str(rm_key$,1%,9%)  = str(gl_rec$,33%,9%)
           str(rm_key$,10%,3%) = str(gl_rec$,42%,3%)
           read #2,key 0% = rm_key$, using L64000, sub_part$, eod goto L64010
L64000:          FMT POS(257), CH(20)
REM  FMT POS(255), CH(20)
/* <PAR003> */
	    gc_key$ = "GRDCOLOR                "
            str(gc_key$,10,1) = str(sub_part$,3,1) 
	    read #1,key = gc_key$,using GRDCOLOR, gc_desc$, eod goto L64005
GRDCOLOR:   FMT POS(25), CH(32)
	    p% = 0%
            for l% = 1% to 16%
                if str(gc_desc$,l%,1%) <> "-" then goto L64003
		p% = l% + 2%
		l% = 17%
L64003:
	    next l%
           if p% > 0% then                            ~
                 str(gl_rec$,193%,6%) = str(gc_desc$,p%,6%)
/* </PAR003> */
L64005:    return
L64010:       sub_part$ = "00000               "
        return
                                                    /* (PAR000)           */
                                                    
/* (AWD004) */
        load_interdesc
          init(" ") gc_key$, gc_desc$, interdesc$()
          str(gc_key$,1,9) = "INTERDESC"
        interdesc_next
          read #1, key > gc_key$, using INTERDESC_FMT, gc_key$, gc_desc$, ~
                                              eod goto interdesc_done
INTERDESC_FMT:         FMT  CH(24), CH(32)
                  if str(gc_key$,1,9) <> "INTERDESC" then goto interdesc_done

                  intercept% = 9%
                  convert str(gc_key$,10,2) to intercept%,      ~
                                         data goto interdesc_done

                  if intercept% > 9% then intercept% = 9%

                  interdesc$(intercept%) = str(gc_desc$,1,5)
                  goto interdesc_next

         interdesc_done
         return

/* (\AWD004) */
