        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EWDPLD72                             *~
            *  Creation Date     - 08/20/01                             *~
            *  Last Modified Date- 04/10/06                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Generate Glass Barcode Label         *~
            *                                                           *~
            *  Process Selection (5) = Glass Special Shapes Label       *~
            *               Note - Print File  (GLASSPRD)               *~
            *                      Script File (GLASSPRD)               *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDPLD72 - Generates the label format and data to print   *~
            *            Special Shapes Glass Barcode Labels.           *~
            *            The resulting file is routed to the label      *~
            *            printer via a script file.                     *~
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
            * 08/20/01 ! Original - New Program                   ! RHH *~
            * 02/23/04 ! (AWD001) Mod for new Special Shapes Label! RHH *~
            * 05/20/04 ! (AWD002) Mod to Add Glass Type to Label  ! RHH *~
            * 08/11/04 ! (AWD003) Mod to prict the correct Grid   ! RHH *~
            *          !          Size on the label. 'C'=1 Inch,  !     *~
            *          !          'W'= 3/4 Inch, Blank= 5/8 Inch. !     *~
            *          !          Also correct Glass Barcode to   !     *~
            *          !          12 digits                       !     *~
            * 01/01/06 ! (PAR000) CR347 Mod for new Sub Part No.  ! RHH *~                    
            * 03/24/06 ! (PAR001) Mod to print Glass production   ! RHH *~
            *          !          for North East.                 !     *~
            * 04/10/06 ! (PAR002) Mod for NE test Label Script    ! RHH *~
            * 04/04/07 ! (PAR003) Use grid color from sub part    ! DES *~
            * 05/18/07 ! (PAR004) 18mm fix                        ! DES *~
            * 03/05/08 ! (AWD005) mod for SDL grid                ! CMG *~
            *************************************************************
                                         /* (AWD001)                   */
        sub "EWDPLD72" (gl_rec$,         /* Entire EWDGLSLB Record     */~
                        sel%,            /* Selection Process          */~
                        #2,              /* APCPLNGR           (PAR000)*/~   
                        #1,              /* GENCODES Channel           */~
                        error%)          /* Return Code                */

        dim                                                              ~
            schema$8,                    /* (PAR003) Schema Switch     */~
            rm_rec$(2%)192,              /* New Remake Glass Rec(PAR000)*/~
            rm_key$12,                   /* Primary Re-Make Key(PAR000)*/~
            sub_part$20,                 /* New Sub Part No.   (PAR000)*/~
            readkey$25, desc$30,         /* GENCODES Look-Up Key       */~
            gl_rec$256,                  /* Re-make record             */~
            a$72, b$72,                  /* Print Lines for Label      */~
            hdr$40,                      /* ASKUSER Header Text        */~
            msg$(3%)79,                  /* ASKUSER Info Text          */~
            lbl$(50%)60,                 /* Label Data Array           */~
            yy$(55%)72,                  /* Label Buffer               */~
            xx$(55%)72,                  /* Label Buffer               */~
            txt$40,                      /* Detail Text Id Line (1)    */~
            txt1$40,                     /* Line text (2) Special      */~
            shc$(7%)9,                   /* Calulated Values 1 - 7     *~
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
            gl_ty$2,                     /* Glass Type Desc    (AWD002)*/~
            gl_seq$5,                    /* Department Production Seq  */~
            gl_so$8,                     /* Customer Sales Order No    */~
            gl_prod$10,                  /* Production Date Formatted  */~
            gl_num$3,                    /* Glass Remake No Print Ver  */~
            gl_win_w$7,                  /* Actual Window Width        */~
            gl_win_h$6,                  /* Actual Window Height       */~
            gl_shft$2,                   /* Production Shift Code      */~
            gl_contour$1,                /* Flag 'C'                   */~
            gl_base$9,                   /* Base Size                  */~
            gl_left$9,                   /* Left Size                  */~
            gl_right$9,                  /* Right Size                 */~
            gl_top$9,                    /* Top Size                   */~
            gl_s1$9,                     /* S1 Size                    */~
            gl_config$2,                 /* Special Shape Code         */~
            gl_config_sq$2,              /* Special Shape Code Seq.    */~
            gl_config_d$30,              /* Shape Description          */~ 
            gl_hub$10,                   /* Grid Hub Adjustment        */~
            gl_face$4,                   /* Glass Facing Code          */~
            gl_fields$7,                 /* Four Print Fields          */~
            gl_bridge$7,                 /* Calc Field Y or N          */~
            gl_position$7,               /* Location of Print Data     */~
            sh_type$1,                   /* Shape Grid, 1, 2, 3(AWD003)*/~
            field$8,                     /* Filed Name                 */~
            value$10                     /* Data Value                 */

        dim gc_key$24, gc_desc$32        /* PAR003 */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */
 
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(AWD)Generate Glass Special Shapes Label    "
            pname$ = "EWDPLD72 - PAR: 01.00"

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
            
            nbr_lines% = 0%
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
        begin_process                                       /* (PAR000) */
            init (" ") lbl$(), rm_rec$()
            gosub format_special_shape_label
 
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

format_special_shape_label                        /* Format (EWDGLSXX) */
        gl_barcode$ = str(gl_rec$,33%,9%)         /* Glass Barcode     */
        gl_num1$    = str(gl_rec$,42%,3%)         /* (EWD001)          */
        lbl$(1%) = gl_barcode$ & gl_num1$ & fs$   /* (EWD003) 12 Digits*/
  
                                                  /* Production Seq No.*/
        gl_seq$ = str(gl_rec$,87%,5%) 
        lbl$(2%) = str(gl_seq$,1%,5%) & fs$       /* Last 5 Digits     */
                                                  /* (AWD003)          */
        gosub check_grid_size
        if sh_type$ = "3" then gl_contour$ = " "  /* 5/8 Inch Grid     */
        if sh_type$ = "2" then gl_contour$ = "W"  /* 3/4 Inch Grid     */
        if sh_type$ = "1" then gl_contour$ = "C"  /* 1   Inch Grid     */
	/* <PAR004> */
        if sh_type$ = "4" then gl_contour$ = "E"  /* 18mm grid         */
	/* </PAR004> */
        /* (AWD005) */
        if sh_type$ = "5" then gl_contour$ = "S"      /* SDL */
        /* (AWD005) */
 
        lbl$(3%) = " " & gl_contour$ & " " & fs$
                                                  /* (AWD003) Set Grid */

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
                                                  /* (AWD002)          */
                                                  /* Glass Type Code   */
        gl_ty$ = str(gl_rec$,45%,2%)
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
                                                  /* Actual Width & Height*/
        gl_win_w$= str(gl_rec$,92%,7%)
        gl_win_h$= str(gl_rec$,101%,6%) 
        lbl$(17%)= gl_win_w$ & "X " & gl_win_h$ & fs$

        gosub convert_text
        
                                                  /* Base Size         */
        gl_base$ = str(gl_rec$,204%,9%)
        shc$(1%) = gl_base$
                                                  /* Left Size         */
        gl_left$ = str(gl_rec$,213%,9%)
        shc$(2%) = gl_left$
                                                  /* Right Size        */
        gl_right$ = str(gl_rec$,222%,9%)
        shc$(3%) = gl_right$
                                                  /* Top Size          */
        gl_top$  = str(gl_rec$,231%,9%)
        shc$(4%) = gl_top$
                                                  /* S1 Size           */
        gl_s1$   = str(gl_rec$,240%,9%)
        shc$(5%) = gl_s1$   

        jj% = 1                                   /* First Field       */
        gosub build_field
        lbl$(18%) = field$ & fs$
        lbl$(19%) = value$ & fs$

        jj% = 2%                                  /* Second Field      */
        gosub build_field
        lbl$(20%) = field$ & fs$
        lbl$(21%) = value$ & fs$

        jj% = 3%                                  /* Third Field       */
        gosub build_field
        lbl$(22%) = field$ & fs$
        lbl$(23%) = value$ & fs$

        jj% = 4%                                  /* Fourth Field      */
        gosub build_field
        lbl$(26%) = field$ & fs$
        lbl$(27%) = value$ & fs$ 

                                                  /* Special Shape Code*/
        gl_config$= str(gl_rec$,250%,2%)
        lbl$(24%) = gl_config$ & fs$
                                                  /* Shape Code Seq.   */
        gl_config_sq$ = str(gl_rec$,252%,2%)
        lbl$(25%) = gl_config_sq$ & fs$
                                                  /* Hub Adjustment    */
        lbl$(28%) = "Hub: " & gl_hub$ & fs$

                                                  /* Glass Facing      */ 
        gosub load_face
 
        lbl$(29%) = str(desc$,1%,5%) & fs$

        gosub load_shape_desc
                                                  /* Special Shape Desc */
        lbl$(30%) = str(gl_config_d$,1%,22%) & fs$
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

        convert_text
            init(" ") gl_hub$, gl_face$, gl_fields$, gl_bridge$,   ~
                      gl_position$

            txt1$        = str(gl_rec$,150%,40%)
            gl_hub$      = str(txt1$,1%,10%)
            gl_face$     = str(txt1$,11%,4%)
            gl_fields$   = str(txt1$,15%,7%)
            gl_bridge$   = str(txt1$,22%,7%)
            gl_position$ = str(txt1$,29%,7%) 

        return

        Build_field
            
            init(" ") field$, value$

            if str(gl_position$,jj%,1%) = "0" then goto no_data
               field% = 0%
               convert str(gl_position$,jj%,1%) to field%, data goto no_data

               value$ = shc$(field%)

             if str(gl_fields$,jj%,1%) <> "W" then goto B1
                field$ = "Width   "
                goto no_data

B1:          if str(gl_fields$,jj%,1%) <> "H" then goto B2
                field$ = "Height  "
                goto no_data
 
B2:          if str(gl_fields$,jj%,1%) <> "R" then goto B3
                field$ = "Radius  "
                goto no_data

B3:          if str(gl_fields$,jj%,1%) <> "N" then goto B4
                field$ = "        "
                goto no_data

B4:          if str(gl_fields$,jj%,1%) <> "L" then goto B5
                field$ = "Leg Hght"
                goto no_data

B5:          if str(gl_fields$,jj%,1%) <> "S" then goto B6
                field$ = "LSideLeg"
                goto no_data 

B6:          if str(gl_fields$,jj%,1%) <> "T" then goto B7
                field$ = "Top  Leg"
                goto no_data

B7:          if str(gl_fields$,jj%,1%) <> "X" then goto B8
                field$ = "SlegHght"
                goto no_data     
 
B8:          if str(gl_fields$,jj%,1%) <> "Z" then goto no_data
                field$ = "RSideLeg"   
 
        no_data 
        return 

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


        load_shape_desc                         /* (EWD056)            */
           init(" ") readkey$, gl_config_d$
           str(readkey$,1%,9%)   = "PLNCONFIG"  /* Shape Description   */
           str(readkey$,10%,15%) = gl_config$
           read #1,key = readkey$, using L62000, gl_config_d$,          ~
                                         eod goto load_shape_desc_done
L62000:        FMT POS(25), CH(30)
        load_shape_desc_done
        return

        load_face                               /* (EWD056)            */

           init(" ") readkey$, desc$
           str(readkey$,1%,9%)   = "PLN FACE "  /* Shape Facing        */
           str(readkey$,10%,15%) = gl_face$
           read #1,key = readkey$, using L62000, desc$,                ~
                                         eod goto load_face_done

           return
        load_face_done
           init(" ") desc$
           desc$ = " N/A  "
        return

        check_grid_size                    /* (AWD003)           */
                                           /* (PAR000)           */
           gosub lookup_sub_part
           init(" ") sh_type$
                                           /*  1"  Grid Contour  */ 
           if str(sub_part$,1%,2%) = "23" then sh_type$ = "1"
                                           /* 3/4" Grid          */
           if str(sub_part$,2%,1%) = "2" then sh_type$ = "2"
                                           /* 5/8" Grid          */
           if str(sub_part$,2%,1%) = "1" then sh_type$ = "3"
	/* <PAR004> */
           if str(sub_part$,1%,2%) = "21" then sh_type$ = "4"
           if str(sub_part$,2%,1%) = "5" then sh_type$ = "4"
                                           /* 18mm Grid          */
	/* </PAR004> */

        /* (AWD005) - beg */
           if str(sub_part$,1,1) = "4" then sh_type$ = "5"  

        /* (AWD005) - end SDL grid */

        return                             /* (PAR000)           */
                                           /* (AWD003)           */
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
/* <PAR003> */
/*      return  */                                   /* Disable for Now   */
           str(rm_key$,1%,9%)  = str(gl_rec$,33%,9%)
           str(rm_key$,10%,3%) = str(gl_rec$,42%,3%)
           read #2,key 0% = rm_key$, using L64000, sub_part$, eod goto L64010
L64000:         FMT POS(257), CH(20)
REM  FMT POS(255), CH(20)
	    gc_key$ = "GRDCOLOR                "
            str(gc_key$,10,1) = str(sub_part$,3,1) 
	    read #1,key = gc_key$,using GRDCOLOR, gc_desc$, eod goto L64010
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

           return
L64010:       sub_part$ = "00000               "
        return
                                                     /* (PAR000)          */     
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

            yy$(14%) = "^FO569,10^FR^GB33,24,24^FS"  /* (AWD003)      */
            yy$(15%) = "^FO374,69^FR^GB89,24,24^FS"
            yy$(16%) = "^FO291,69^FR^GB71,24,24^FS"
                                       /* Special Shape Glass Barcode */
        REM    yy$(17%) = "01^FO478,71^BY2,2.0,87^BCN,87,N,N,N^FR^FD>;"
                                       /* (AWD003)                    */
            yy$(17%)= "01^FO580,85^BY2,2.0,87^BCN,87,N,N,N^FR^FD>;"

            yy$(18%) = "^FO632,10^CI0^A0N,26,30^FR^FDSEQ:^FS"
            yy$(19%) = "^FO317,10^CI0^A0N,26,26^FR^FDGRID:^FS"
                                       /* Glass Barcode Text         */
            yy$(20%) = "01^FO595,43^CI0^A0N,26,26^FR^FD"
                                       /* Grid Value                 */
            yy$(21%) = "07^FO392,10^CI0^A0N,26,28^FR^FD"
                                       /* Sequence Number            */
            yy$(22%) = "02^FO713,10^CI0^A0N,26,28^FR^FD"
                                       /* Window Width and Height    */ 
            yy$(23%) = "17^FO317,39^CI0^A0N,26,28^FR^FD"
                                       /* (AWD003)                   */
                                       /* Contour Grid Flag          */
            yy$(24%) = "03^FO569,10^CI0^A0N,26,24^FR^FD"
                                       /* (AWD003)                   */
                                       /* Glass Remake Counter       */
            yy$(25%) = "04^FO335,106^CI0^A0N,55,57^FR^FD"
                                       /* Window Color               */
            yy$(26%) = "15^FO374,69^CI0^A0N,26,26^FR^FD"
                                       /* Glass Facing Code          */
            yy$(27%) = "29^FO291,69^CI0^A0N,26,24^FR^FD"
                                       /* Glass Special Text         */
            yy$(28%) = "09^FO53,173^CI0^A0N,24,24^FR^FD"

            yy$(29%) = "^FO12,132^CI0^A0N,24,24^FR^FDSO:^FS"
            yy$(30%) = "^FO12,152^CI0^A0N,24,24^FR^FDMOD:^FS"
            yy$(31%) = "^FO12,112^CI0^A0N,24,24^FR^FDDTE:^FS"
                                       /* Special Data - Shape Name  */  
            yy$(32%) = "30^FO12,89^CI0^A0N,24,24^FR^FD"
                                       /* Calc Field (4) Name        */
            yy$(33%) = "26^FO12,69^CI0^A0N,24,24^FR^FD"
                                       /* Calc Field (3) Name        */ 
            yy$(34%) = "22^FO12,49^CI0^A0N,24,24^FR^FD"
                                       /* Calc Field (2) Name        */
            yy$(35%) = "20^FO12,26^CI0^A0N,24,24^FR^FD"
                                       /* Calc Field (1) Name        */
            yy$(36%) = "18^FO12,6^CI0^A0N,24,24^FR^FD"
                                       /* Special Shape Code         */
            yy$(37%) = "24^FO280,132^CI0^A0N,22,30^FR^FD"
                                       /* Hub Adjustment             */
            yy$(38%) = "28^FO585,173^CI0^A0N,24,24^FR^FD"
                                       /* Model Code                 */
            yy$(39%) = "10^FO128,152^CI0^A0N,24,24^FR^FD"
                                       /* Sales Order Number         */
            yy$(40%) = "11^FO128,132^CI0^A0N,24,24^FR^FD"
                                       /* Production Date            */
            yy$(41%) = "12^FO128,112^CI0^A0N,24,24^FR^FD"
                                       /* Calc Data (4)              */
            yy$(42%) = "27^FO128,69^CI0^A0N,24,24^FR^FD"
                                       /* Calc Data (3)              */
            yy$(43%) = "23^FO128,49^CI0^A0N,24,24^FR^FD"
                                       /* Calc Data (2)              */
            yy$(44%) = "21^FO128,26^CI0^A0N,24,24^FR^FD"
                                       /* Calc Data (1)              */
            yy$(45%) = "19^FO128,6^CI0^A0N,24,24^FR^FD"
                                       /* (AWD002)                   */
                                       /* Glass Type Code            */
            yy$(46%) = "08^FO526,10^CI0^A0N,26,28^FR^FD"
                                       /* (AWD002)                   */
            yy$(47%) = "^PQ1"
            yy$(48%) = "^XZ"

        return 
