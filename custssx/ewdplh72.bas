        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EWDPLH72                             *~
            *  Creation Date     - 12/08/15                             *~
            *  Last Modified Date-   /  /                               *~
            *  Written By        - Paul W. Williams                     *~
            *                                                           *~
            *  Description       - Generate Glass Barcode Label         *~
            *                                                           *~
            *  Process Selection (5) = Glass Special Shapes Label       *~
            *               Note - Print File  (GLASSPRD)               *~
            *                      Script File (GLASSPRD)               *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDPLH72 - Generates the label format and data to print   *~
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
            * Subroutine - Called by EWDPLP72                           *~
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
            *12/01/2015! SR67154  Cloned from EWDPLD72 for new    ! PWW *~
            *          !          Larger Glass Label stock. 4 1/2 !     *~
            *          !          X 1 1/2 inches. Template is...  !     *~
            *          !          Glass Label H.btw.              !     *~
            *03/14/2016! SR73199  Changed lookup_sub_part to match! PWW *~
            *          !          ewdple72 & ewdplf72.            !     *~
            *03/16/2016! SR73394  Enlarge Barcode.                ! PWW *~
            *          !                                          !     *~
            *08/27/2019! CR2180 Remake Now barcode prefix         ! RDB *~
            *10/25/2019! CR2304 Low-e barcode changes (glchkbc)   ! RDB *~     
            *11/25/2019! CR2352 Move Low-e barcode to new location! RDB *~      
            *10/06/2021! CR2913 Allow 0000 low-e barcode          ! RDB *~            
            *************************************************************
                                         /* (AWD001)                   */
        sub "EWDPLH72" (gl_rec$,         /* Entire EWDGLSLB Record     */~
                        gl_reb$,         /* Expanded CR2304            */~
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
            gl_reb$128,                  /* Re-make record             */~            
            a$72, b$72,                  /* Print Lines for Label      */~
            hdr$40,                      /* ASKUSER Header Text        */~
            msg$(3%)79,                  /* ASKUSER Info Text          */~
            lbl$(50%)60,                 /* Label Data Array           */~
            yy$(60%)72,                  /* Label Buffer               */~
            xx$(60%)72,                  /* Label Buffer               */~
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
            value$10,                    /* Data Value                 */~
            txt_glass$30,                /* Glass Text                 */~
            wnd_color$6,                 /* Window Color               */~
            wnd_color_code$1,            /* Window Color Code          */~
            glass_code$2,                /* Glass Code                 */~
            txt_planbilco$30,            /* PLANBILCO Desc             */~
            txt_inside_outside$7,        /* INSIDE/OUTSIDE Text        */~
            low_e_key$2,                 /* GENCODES LOWE Key          */~
            glchkbc$10,                  /* Glass Check barcode        */~
            rm_part$25                   /* Part Number                */

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
            pname$ = "EWDPLH72 - PAR: 01.00"

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
            err%    = 0%
            call "SCHEMA" (schema$,      /* What switch 1-NC 2-NE      */~
                           schema%,      /* Schema                     */~
                           #1,           /* GENCODES                   */~
                           err% )        /* error                      */
                              
            glchkbc$ = str(gl_reb$,1%,10%)
            if glchkbc$ < "0000" or glchkbc$ > "9999" then glchkbc$ = "0000" 
        
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
            if err% = 0% then goto SS_1
               error% = 9%
               end 

SS_1:                                                /* (PAR001)        */
            if schema% <> 1% then goto SS_2
                                                     /* North Caolina   */
               file$    = "GLASSPRD"                                      
/*             file$    = "GLASSPPW"   pwww t e m p for testing !!!     */
               script$  = "GLASSPRD"                                      
/*             script$  = "GLASSPWW"   pwww t e m p for testing !!!     */

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
        lbl$(36%) = gl_barcode$ & gl_num1$ & fs$   /* (EWD001) 12 Digit */
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
        lbl$(28%) = gl_hub$ & fs$

                                                  /* Glass Facing      */ 
        gosub load_face
 
        lbl$(29%) = str(desc$,1%,5%) & fs$

        gosub load_shape_desc
                                                  /* Special Shape Desc */
        lbl$(30%) = str(gl_config_d$,1%,22%) & fs$
                                                  /* Window Color      */
        wnd_color_code$ = str(gl_rec$,99%,2%)
        gosub lookup_wnd_color
        lbl$(33%)= wnd_color$ & fs$
                                                  /* Inside/Outside    */
/*      glass_code$ = str(gl_rec$,45%,2%)                              */
        txt_inside_outside$ = "N/A"
        gosub lookup_inside_outside
        lbl$(32%)= txt_inside_outside$ & fs$
                                                  /* Glass Description */
/*      glass_code$ = str(gl_rec$,45%,2%)                              */
/*      glass_code$ = str(rm_part$,5%,2%)                              */
        gosub lookup_glass_text
        lbl$(31%) = txt_glass$ & fs$
                                                  /* Glass Code        */
        lbl$(34%) = "(" & glass_code$ & ")" & fs$
                                                  /* New barcodes      */
                                                  /* new bc */  
        if schema% = 2% then goto L55010          /* CR2913 */
        glchkbc$ = str(gl_reb$,1%,10%)
        if glchkbc$ < "0000" or glchkbc$ > "9999" then glchkbc$ = "0000" 
        lbl$(35%) = glchkbc$ & fs$ 
L55010:        
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
           glass_code$ = str(gl_rec$,45%,2%)
           if rm_part$ <> " " then glass_code$ = str(rm_part$,5%,2%)

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
           init(" ") rm_key$, sub_part$, rm_part$
/* <PAR003> */
/*      return  */                                   /* Disable for Now   */
           str(rm_key$,1%,9%)  = str(gl_rec$,33%,9%)
/*         str(rm_key$,10%,3%) = str(gl_rec$,42%,3%)         */
           read #2,key 0% >= rm_key$, using L64000, rm_key$, rm_part$,     ~
/*SR73199 */                sub_part$, eod goto L64010
           if str(gl_rec$,33%,9%) <> str(rm_key$,1%,9%) then goto L64010
           
L64000:          FMT POS(22), CH(9),POS(125), CH(25), POS(257), CH(20)
REM  FMT POS(255), CH(20)
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
              init(" ") rm_key$, sub_part$, rm_part$
              
        return
     
       lookup_wnd_color
           init(" ") readkey$, wnd_color$
           str(readkey$,1%,9%)   = "COLOR    "
           str(readkey$,10%,15%) = wnd_color_code$
           read #1,key = readkey$, using L_CLR, wnd_color$,         ~
                                                eod goto lookup_wnd_color_done
L_CLR:        FMT POS(30), CH(6)
                                        
       lookup_wnd_color_done
           return  

       lookup_glass_text
           init(" ") readkey$, txt_glass$
           str(readkey$,1%,9%)   = "GLASS    "
           str(readkey$,10%,15%) = glass_code$
           read #1,key = readkey$, using L62000, txt_glass$,         ~
                                                eod goto lookup_glass_text_done
                                        
       lookup_glass_text_done
           return  
                                        
       lookup_inside_outside
           init(" ") readkey$
           str(readkey$,1%,9%)   = "PLANBILCO"
           str(readkey$,10%,15%) = glass_code$ & " "
           read #1,key = readkey$, using L62000, txt_planbilco$,         ~
                                                eod goto lookup_again
           goto lookup_low_e
         lookup_again
           str(readkey$,1%,9%)   = "PLANBILCO"
           str(readkey$,10%,15%) = glass_code$ & "B"
           read #1,key = readkey$, using L62000, txt_planbilco$,         ~
                                                eod goto lookup_inside_outside_done
         lookup_low_e
           if str(txt_planbilco$,12%,1%) = "+" then three_codes
         four_codes
              low_e_key$ = str(txt_planbilco$,2%,2%)
              gosub lookup_low_e_g
              low_e_key$ = str(txt_planbilco$,10%,2%)
              gosub lookup_low_e_g
              low_e_key$ = str(txt_planbilco$,16%,2%)
              gosub lookup_low_e_g
              low_e_key$ = str(txt_planbilco$,24%,2%)
              gosub lookup_low_e_g
              goto lookup_inside_outside_done
         three_codes
              low_e_key$ = str(txt_planbilco$,2%,2%)
              gosub lookup_low_e_g
              low_e_key$ = str(txt_planbilco$,10%,2%)
              gosub lookup_low_e_g
              low_e_key$ = str(txt_planbilco$,18%,2%)
              gosub lookup_low_e_g

       lookup_inside_outside_done
           return  
           
       lookup_low_e_g
           init(" ") readkey$
           str(readkey$,1%,9%)   = "LOWE     "
           str(readkey$,10%,15%) = low_e_key$
           read #1,key = readkey$, using L62000, txt_inside_outside$,         ~
                                                eod goto lookup_low_e_g_done
                                        
       lookup_low_e_g_done
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
            yy$(9%) = "^LH0,0"                       /*"^LH32,24"     */
            yy$(10%)= "^LL305^PW928"

            yy$(11%)= "^PR6,4,8"                     /* PR Label Print Speed*/
                                                     /* a = 3 Print Speed   */
                                                     /* b = 4 Slew  SPeed   */
                                                     /* c = 8 Back Feed Spee*/ 
            yy$(12%)= "^JMA"

            yy$(13%)= "^COY,362"                     /* 256 + 128 Mem */
                                                     /* (-) 22k Intern*/

            yy$(14%) = "^CIO"
                                       /* Calc Field (2) Name        */
                                       /* Height: Label or Field #20 */
            yy$(15%) = "20^FT48,62^A0N,25,34^FD"    
                                       /* Calc Field (2)             */
                                       /* Height Field or Field #21 */ 
            yy$(16%) = "21^FT187,62^A0N,25,34^FD"   
            

                                       /* Calc Field (1) Name        */
                                       /* Width: Label or Field #18 */
            yy$(17%) = "18^FT48,34^A0N,25,34^FD"  
            yy$(18%) = "^FT352,59^A0N,25,34^FDGRID:^FS"
                                       /* Grid Value                 */ 
            yy$(19%) = "07^FT441,59^A0N,25,34^FD"
                                       /* Glass Type Code            */
            yy$(20%) = "^FT568,37^A0N,25,34^FS"   /* empty field  */
            yy$(21%) = "^FO593,10^GB19,27,19^FS"
                                       /* Contour Grid Flag          */
            yy$(22%) = "03^FT593,34^A0N,25,34^FR^FD"
            yy$(23%) = "^FT622,41^A0N,34,46^FDSEQ:^FS"
                                       /* Sequence Number            */
            yy$(24%) = "02^FT720,34^A0N,31,46^FD"
                                       /* Calc Field (3) Name        */ 
                                       /* Sideleg Label or Field #22 */ 
            yy$(25%) = "22^FT48,85^A0N,25,34^FD"
                                       /* Calc Field (3)             */ 
                                       /* Sideleg Field or Field #23 */ 
            yy$(26%) = "23^FT187,88^A0N,28,41^FD"
                                       /* Window Width and Height    */ 
            yy$(27%) = "17^FT352,86^A0N,25,34^FD"
                                       /* Calc Field (4) Name        */
                                       /* Sideleg Label or Field #26 */ 
            yy$(28%) = "26^FT48,116^A0N,28,37^FD"
                                       /* Calc Field (4)             */
                                       /* Sideleg Field or Field #27 */ 
            yy$(29%) = "27^FT187,116^A0N,25,34^FD"
            yy$(30%) = "^FO352,94^GB72,27,27^FS"
                                       /* Glass Facing Code          */
            yy$(31%) = "29^FT352,118^A0N,25,34^FR^FD"
            yy$(32%) = "^FO441,94^GB92,27,27^FS"
                                       /* Window Color               */
            yy$(33%) = "15^FT441,118^A0N,25,34^FR^FD"
                                       /* Special Data - Shape Name  */  
            yy$(34%) = "30^FT48,143^A0N,25,34^FD"
            yy$(35%) = "^FT48,169^A0N,25,34^FDDTE:^FS"
                                       /* Production Date            */
            yy$(36%) = "12^FT187,169^A0N,25,34^FD"
            yy$(37%) = "^FT48,198^A0N,25,34^FDSO:^FS"
                                       /* Sales Order Number         */
            yy$(38%) = "11^FT187,198^A0N,25,34^FD"
                                       /* Glass Special Text         */
            yy$(39%) = "09^FT200,249^A0N,25,28^FD"
                                       /* Glass Description          */
            yy$(40%) = "31^FT200,275^A0N,25,29^FD"
                                       /* Glass Remake Counter       */
            yy$(41%) = "04^FT378,196^A0N,51,70^FD"

                                       /* Special Shape Glass Barcode */
REM         yy$(42%) = "01^FO478,71^BY2,2.0,87^BCN,87,N,N,N^FR^FD>;"
                                       /* (AWD003)                    */
REM         yy$(42%) = "01^FO635,132^BY2,2.0,87^BCN,87,N,N,N^FR^FD>;"
/*SR73394   yy$(42%) = "01^FO517,158^BY3^BCN,102,N,Y^FR^FD>;"         */
/* CR2180 */
            if schema% = 2% then   ~
/*SR7 6703*/ yy$(42%)= "36^FO555,114^BY2^BCN,102,N,Y^FR^FD>:.T.>5"  ~
            else    ~                    
             yy$(42%)= "01^FO555,114^BY3^BCN,102,N,Y^FR^FD>;" 
                                       /* Calc Field (1)             */
                                       /* Width: Data or Field #19   */
            yy$(43%) = "19^FT187,34^A0N,25,34^FD"
                                                  /* Inside/Outside    */
            yy$(44%) = "32^FT428,34^A0N,25,34^FD"
            yy$(45%) = "^FT314,34^A0N,25,34^FDLOW E:^FS"
                                                  /* Human Barcode     */
            yy$(46%) = "01^FT606,97^A0N,25,34^FD"
            yy$(47%) = "^FT48,224^A0N,25,34^FDMOD:^FS"
                                       /* Model Code                 */
            yy$(48%) = "10^FT187,224^A0N,25,34^FD"
                                       /* Special Shape Code         */
            yy$(49%) = "24^FT327,211^A0N,25,34^FD"
            yy$(50%) = "^FT657,275^A0N,25,29^FDHub:^FS"
                                       /* Hub Adjustment             */
            yy$(51%) = "28^FT720,275^A0N,25,26^FD"
            yy$(52%) = "^FT352,148^A0N,25,33^FDWND:^FS"
                                                  /* Window Color      */
            yy$(53%) = "33^FT429,148^A0N,25,29^FD"
                                                  /* Glass Code        */
            yy$(54%) = "34^FT784,249^A0N,25,29^FD"
                                                  /* New barcodes      */
            if schema% = 2% then goto nobarcode
            yy$(55%) = "35^FO48,228^BY2^BCN,76,N^FD>;"
            
            yy$(56%) = "^PQ1"
            yy$(57%) = "^XZ"
            
            return 
nobarcode:
            yy$(55%) = "^PQ1"
            yy$(56%) = "^XZ"

        return 
