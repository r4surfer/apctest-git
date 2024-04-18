        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EWDPLF72                             *~
            *  Creation Date     - 12/01/15                             *~
            *  Last Modified Date-   /  /                               *~ 
            *  Written By        - Paul W. Williams                     *~
            *                                                           *~
            *  Description       - Generate Glass Barcode Label         *~
            *                      'Single Label Format'                *~
            *                                                           *~
            *                      'Re-Make Label Format'               *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDPLF72 - Generates the label format and data to print   *~
            *            glass remake barcode label. The resulting file *~
            *            is routed to the label printer via a script.   *~
            *                                                           *~
            *          - ERROR% = 0% - Lbl Ok and Sent                  *~
            *                     1% - Could Not Open Lbl Print File    *~
            *                     4% - Shell Script Error               *~
            *                     5% - Print Error Creating Label       *~
            *                     6% - Could Not Reset Table Flag       *~
            *                     9% - Schema Lookup Error              *~
            *                                                           *~
            *          - sel%   = 1% - Single Label Print (Glass)       *~
            *                     2% - Production                       *~
            *                     3% - Re-Make Glass                    *~
            *                     4% - Special Glass                    *~
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
            * 11/10/99 ! Original - Copied & Mod (sub) EWDPLA69   ! RHH *~
            * 12/20/99 ! (EWD001) Mod for new 12 Digit Barcode for! RHH *~
            *          !          glass label                     !     *~ 
            * 01/01/06 ! (PAR000) CR347 Mod for new Sub Part No.  ! RHH *~
            * 03/27/06 ! (PAR001) Mod for Printing Labels in the  ! RHH *~
            *          !          North East.                     !     *~                    
            * 04/10/06 ! (PAR002) Mod for NE test Label Script    ! RHH *~
            *          !          not Applicable                  !     *~
            * 04/02/07 ! (PAR003) use grid color from sub part    ! DES *~
            * 05/18/07 ! (PAR004) fix 18mm prob. has "C" not "E"  ! DES *~
            * 03/10/08 ! (AWD005) mod for SDL                     ! CMG *~
            *11/18/2009! (AWD006) mod to add Ultra to label       ! CMG *~
            *03/22/2011! (AWD007) mod for intercepts              ! CMG *~
            *09/03/2014! (AWD008) mod for breathing tubes         ! PWW *~
            *12/01/2015! SR67154  Cloned from EWDPLB72 for new    ! PWW *~
            *          !          Larger Glass Label stock. 4 1/2 !     *~
            *          !          X 1 1/2 inches. Template is...  !     *~
            *          !          Glass Label F.btw.              !     *~
            *03/16/2016! SR73394  Enlarge Barcode.                ! PWW *~
            *          !                                          !     *~
            *08/24/2016! SR76703  Move barcode up slightly so as  ! PWW *~
            *          !          to not interfer with glass text.!     *~
            *08/27/2019! CR2180 Remake Now barcode prefix         ! RDB *~ 
            *10/25/2019! CR2304 Low-e barcode changes (glchkbc)   ! RDB *~     
            *11/25/2019! CR2352 Move Low-e barcode to new location! RDB *~              
            *************************************************************

        sub "EWDPLF72" (gl_rec$,         /* Entire EWDPLNGT Record     */~
                        gl_reb$,         /* Expanded CR2304            */~        
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
            gl_reb$128,                  /* Re-make record             */~            
            a$72, b$72,                  /* Print Lines for Label      */~
            desc$64,                     /* Descriptions for Scripts   */~
            lbl$(25)60,                  /* Label Data Array           */~
            xx$(100%)72,                 /* Label Buffer               */~
            lbl_key$24,                  /* Save File Key              */~
            dt_txt$4,text$(2%)70,txt$40, /* Detail Text Id             */~
            fs$3,                        /* End of Lbl Records         */~
            file$8,                      /* Lbl Print File             */~
            library$8,                   /* Library Name = APCDATA     */~
            script$8,                    /* Lbl SHELL SCRIPT           */~
            volume$6                     /* DISK VOLUME = CARLOS       */

        dim gl_barcode$9,                /* Glass Barcode              */~
            gl_num1$3,                   /* Glass Re-Make Number(EWD001)*/~
            gl_model$3,                  /* Model Code                 */~
            gl_dept$3,                   /* Department Code            */~
            gl_view$3,                   /* View Top/Bot               */~
            gl_color$6,                  /* Product Color              */~
            gl_grid$7,                   /* Grid Value                 */~
            gl_cut_w$9,                  /* Glass Cut Width            */~
            gl_cut_h$9,                  /* Glass Cut Height           */~
            gl_ty$4,                     /* Glass Type Desc            */~
            gl_seq$5,                    /* Department Production Seq  */~
            gl_so$8,                     /* Customer Sales Order No    */~
            gl_prod$10,                  /* Production Date Formatted  */~
            gl_num$3,                    /* Glass Remake No            */~
            gl_win_w$7,                  /* Actual Window Width        */~
            gl_win_h$6,                  /* Actual Window Height       */~
            gl_shft$2,                   /* Production Shift Code      */~
            gl_contour$1,                /* Flag 'C'                   */~
            gl_breatht$1,                /* Flag 'B'     <AWD008>      */~
            gl_text$4,                   /* Glass Text String          */~
            gl_ultra$1,                  /* Ultra Flag                 */~
            gl_intercept$2,              /* (AWD004) Intercpet         */~
            txt_glass$30,                /* Glass Text                 */~
            wnd_color$6,                 /* Window Color               */~
            wnd_color_code$1,            /* Window Color Code          */~
            glass_code$2,                /* Glass Code                 */~
            txt_planbilco$30,            /* PLANBILCO Desc             */~
            txt_inside_outside$7,        /* INSIDE/OUTSIDE Text        */~
            low_e_key$2,                 /* GENCODES LOWE Key          */~
            glchkbc$10,                  /* Glass Check barcode        */~
            rm_part$25                   /* Part Number                */

        dim interdesc$(99)5              /* (AWD007) intercept desc    */
	    dim gc_key$24, gc_desc$32        /* (AWD007)                   */ 

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%)                     /* = 1 if READ was successful */ 
*           fs%(10%),                    /* = 1 if file open, -1 if it */~
*                                        /*   doesn't exist, or 0 if   */~
*                                        /*   not yet checked (OPENCHCK*/~
*           rslt$(10%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(AWD) Generate Glass Remake Barcode Lbl "
            pname$ = "EWDPLF72 - PAR: 01.00"

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
                           
            glchkbc$ = str(gl_reb$,1%,10%)
            if glchkbc$ < "0000" or glchkbc$ > "9999" then glchkbc$ = "0000" 
            if err% = 0% then goto SS_1
               error% = 9%
               end
SS_1:
            error%          =  0%  :  counter% = 0%  :  nbr_lines% = 0%

            fs$ = "^FS"

            xx$(1%) = "^JO"
            xx$(2%) = "^XA^EG^XZ"
            xx$(3%) = "^XA"
            xx$(4%) = "^PMN"
            xx$(5%) = "^MNY"
            xx$(6%) = "^MMT"
            xx$(7%) = "^MTT"
            xx$(8%) = "^MD0"
            xx$(9%) = "^LH0,0"                     /*"^LH32,24"       */
            xx$(10%)= "^LL305^PW928"
            xx$(11%)= "^PR2"
            xx$(12%)= "^JMA"

            xx$(13%)= "^CI0"
            xx$(14%)= "^FT48,65^A0N,28,39^FDCLR:^FS"
                                                  /* Grid Color        */
            xx$(15%)= "15^FT137,65^A0N,28,39^FD"
            xx$(16%)= "^FT48,37^A0N,28,39^FDWND:^FS"
            xx$(17%)= "^FT340,65^A0N,28,39^FDGRID:^FS"
                                                  /* Grid Value          */ 
            xx$(18%)= "07^FT441,65^A0N,28,39^FD"
            xx$(19%)= "^FO632,6^GB31,30,30^FS"
                                                  /* Contour Grid        */
            xx$(20%)= "03^FT620,32^A0N,28,39^FR^FD"
            xx$(21%)= "^FO585,6^GB21,30,21^FS"
                                                  /* Breath Tube <AWD008>*/
            xx$(22%)= "19^FT573,32^A0N,28,39^FR^FD"
                                                        /* Glass Barcode */
            xx$(23%)= "^FT670,39^A0N,31,42^FDSEQ:^FS"   /* CR2180 */
                                                  /* Prod Seq No.        */
            xx$(24%)= "02^FT746,37^A0N,34,46^FD"        /* CR2180 */
            xx$(25%)= "^FT48,91^A0N,28,39^FDWID:^FS"
                                                  /* Cut Width           */
            xx$(26%)= "14^FT136,89^A0N,28,41^FD"
                                                  /* Actual Windowb W/H  */
            xx$(27%)= "17^FT340,91^A0N,28,39^FD"
            xx$(28%)= "^FT48,119^A0N,28,39^FDHGT:^FS"
                                                  /* Cut Height          */
            xx$(29%)= "13^FT136,119^A0N,28,39^FD"
            xx$(30%)= "^FO340,93^GB70,30,30^FS"
                                                  /* Top/Bot Descript    */
            xx$(31%)= "06^FT330,119^A0N,28,39^FR^FD"
            xx$(32%)= "^FO441,94^GB86,41,41^FS"
            xx$(33%)= "05^FT425,128^A0N,39,54^FR^FD"
            xx$(34%)= "^FT48,151^A0N,28,39^FDDTE:^FS"
                                                  /* Production Date     */
            xx$(35%)= "12^FT137,151^A0N,28,39^FD"
            xx$(36%)= "^FT48,178^A0N,28,39^FDSO:^FS"
                                                  /* Sales Order         */
            xx$(37%)= "11^FT137,178^A0N,28,39^FD"
            xx$(38%)= "^FT48,207^A0N,28,39^FDMOD:^FS"
                                                  /* Model code          */
            xx$(39%)= "10^FT137,207^A0N,28,39^FD"
            xx$(40%)= "^FO251,181^GB96,30,30^FS"
                                                  /* Shift Code NOW ULTRA */
            xx$(41%)= "16^FT251,207^A0N,28,39^FR^FD"
                                                  /* Glass Text          */ 
            xx$(42%)= "09^FT187,249^A0N,25,34^FD"
                                                  /* Glass Description   */ 
/*          xx$(43%)= "22^FT48,260^A0N,28,39^FD"                         */
/*SR73394*/ xx$(43%)= "22^FT187,278^A0N,28,39^FD"
            xx$(44%)= "04^FT378,183^A0N,51,70^FD"     /* CR2180 */
REM         xx$(45%)= "01^FO581,76^BY3^BCN,102,N,Y^FD"
/*SR73394   xx$(45%)= "01^FO635,132^BY2,2.0,87^BCN,87,N,N,N^FR^FD>;"     */
/*SR73394   xx$(45%)= "01^FO541,148^BY3^BCN,102,N,Y^FR^FD>;"             */
/* CR2180 */
            if schema% = 2% then   ~
/*SR7 6703*/ xx$(45%)= "25^FO568,102^BY2^BCN,102,N,Y^FR^FD>:.T.>5"  ~
            else    ~
             xx$(45%)= "01^FO543,100^BY3^BCN,102,N,Y^FR^FD>;" 
                                                  /* Window Color      */
            xx$(46%)= "20^FT137,37^A0N,28,39^FD"
                                                  /* Inside/Outside    */
            xx$(47%)= "21^FT428,37^A0N,28,39^FD"
            xx$(48%)= "^FT314,37^A0N,28,39^FDLOW E:^FS"
                                                  /* Human Barcode     */
/*          xx$(49%)= "01^FT614,113^A0N,28,39^FD"                      */
/*SR73394   xx$(49%)= "01^FT619,138^A0N,28,39^FD"                      */
/*SR76703*/ xx$(49%)= "01^FT619,085^A0N,28,39^FD"
                                                  /* Glass Code        */
/*          xx$(50%)= "23^FT733,260^A0N,28,39^FD"                      */
/*SR73394*/ xx$(50%)= "23^FT784,278^A0N,28,38^FD"

/* new bc*/
            if schema% = 2% or glchkbc$ = "0000" then goto nobarcode
            xx$(51%)= "24^FO48,216^BY2^BCN,76,N,N^FD>;"
            
            xx$(52%)= "^PQ1"
            xx$(53%)= "^XZ"
            goto barcodtin

nobarcode:
            xx$(51%) = "^PQ1"
            xx$(52%) = "^XZ"
barcodtin:

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        REM INPUTMODE
            gosub load_interdesc                   /* (AWD007)  */
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
/*          script$ = "EWDGRSPW"       pwww T E M P  F O R  T E S T I N G  */ 
/*          file$ = "LBLGLRPW"       pwww T E M P  F O R  T E S T I N G  */ 
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
            error%      = 1%                    /* EXIT TRY LATER  */
        return
                                                /* (PAR001)        */  
        open_file
            open nodisplay #5, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return

        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *************************************************************
        begin_process                       /* (PAR000)                 */
            init (" ") lbl$(), rm_rec$()
            if sel% = 1% then gosub format_single_label                  ~
                         else gosub format_remake_label
 
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

        lookup_text
            txt_line% = 0%
            init(" ") txt$, text$()
            call "APCPLTXT" (#2, dt_txt$, text$(), txt_line%)
            txt$     = str(text$(2%),1%,40%)
        return

       lookup_color
	 /* PAR004 */
/* <PAR003> */
	    readkey$ = "GRDCOLOR                "
            str(readkey$,10,1) = str(sub_part$,3,1) 
	    read #1,key = readkey$,using GRDCOLOR, gc_desc$, eod goto L64005
GRDCOLOR:   FMT POS(25), CH(32)
	    p% = 0%
            for l% = 1% to 16%
                if str(gc_desc$,l%,1%) <> "-" then goto L64003
		p% = l% + 2%
		l% = 17%
L64003:
	    next l%
           if p% = 0% then goto L64005                  
           str(gl_rec$,193%,6%) = str(gc_desc$,p%,6%)
	   gl_color$ = str(gl_rec$,193,6)
	   return
/* </PAR003> */
L64005:    init(" ") readkey$
           str(readkey$,1%,9%)   = "COLOR    "
           str(readkey$,10%,15%) = str(gl_rec$,128%,1%)
           read #1,key = readkey$, using L_CLR, gl_color$,         ~
                                                eod goto L_CLR_DONE
L_CLR:        FMT POS(30), CH(6)
L_CLR_DONE
       return  
     
       lookup_wnd_color
           init(" ") readkey$, wnd_color$
           str(readkey$,1%,9%)   = "COLOR    "
           str(readkey$,10%,15%) = wnd_color_code$
           read #1,key = readkey$, using L_CLR, wnd_color$,         ~
                                                eod goto lookup_wnd_color_done
                                        
       lookup_wnd_color_done
           return  
L62000:        FMT POS(25), CH(30)

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

            REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

format_single_label
                      /* Note _ The format of gl_rec = gl_rec (APCPLNGR)*/
 
        gl_barcode$ = str(gl_rec$,22%,9%)         /* Glass Barcode     */
        gl_num1$    = str(gl_rec$,31%,3%)         /* (EWD001) Re-make No*/         
                                                  /* (PAR000)          */
        gosub lookup_sub_part
        glass_code$ = str(gl_rec$,77%,2%)
        if rm_part$ <> " " then glass_code$ = str(rm_part$,5%,2%)

                                                  /* (PAR000)          */
        lbl$(1%) = gl_barcode$ & gl_num1$ & fs$   /* (EWD001) 12 Digit */
                                                  /* Production Seq No.*/
        lbl$(25%) = gl_barcode$ & gl_num1$ & fs$   /* CR2180 */                                                  
        
        gl_seq$ = str(gl_rec$,242%,5%) 
        lbl$(2%) = gl_seq$ & fs$

        gl_contour$ = " "
                                                  /* (PAR000)          */
        if str(sub_part$,1%,1%) = "2" then gl_contour$ = "C"

        if str(sub_part$,2%,1%) = "2" then gl_contour$ = "W"  /* 3/4"  */
                                                 /* (PAR000)           */
                                                 /* <PAR004>           */
        if str(sub_part$,1%,1%) = "2" and                           ~
           str(sub_part$,2%,1%) = "1" then gl_contour$ = "E"
        if str(sub_part$,2%,1%) = "5" then gl_contour$ = "E"  /* 18mm  */
                                                 /* </PAR000>          */
/* (AWD005)  */
        if str(sub_part$,1,1) = "4" then gl_contour$ = "S"  /*SDL */
/* (AWD005) */
  
        lbl$(3%) = " " & gl_contour$ & " " & fs$
                                                  /* Glass Remake No   */
        gl_num$ = str(gl_rec$,31%,3%)  
        lbl$(4%) = gl_num$ & fs$
                                                  /* Department Code   */
        gl_dept$ = str(gl_rec$,249%,3%)
        lbl$(5%) = " " & gl_dept$ & " " & fs$
                                                  /* View Top / Bot    */
        gl_view$ = "TOP"
        if str(gl_rec$,111%,1%) = "B" then gl_view$ = "BOT"
        lbl$(6%) = " " & gl_view$ & " " & fs$
                                                  /* Grid Value        */
        gl_grid$ = str(gl_rec$,234%,7%)
        lbl$(7%) = gl_grid$ & fs$
                                                  /* Glass Type Code   */
        gl_ty$ = " " & str(gl_rec$,77%,2%) & " "
        lbl$(8%) = gl_ty$ & fs$
                                                  /* Glass Text        */
        gl_text$ = str(gl_rec$,112%,4%)
        gosub lookup_text  
        lbl$(9%) = txt$ & fs$
                                                  /* Model Code        */
        gl_model$ = str(gl_rec$,72%,3%)
        lbl$(10%)= gl_model$ & fs$
                                                  /* Sales Order       */
        gl_so$ = str(gl_rec$,163%,8%)
        lbl$(11%)= gl_so$ & fs$
                                                  /* Production Date   */
        gl_prod$ = str(gl_rec$,1%,6%)
        call "DATFMTC" (gl_prod$)
        lbl$(12%)= gl_prod$ & fs$
                                                  /* Cut Height        */
        gl_cut_h$ = str(gl_rec$,94%,8%) & " "
        lbl$(13%)= gl_cut_h$ & fs$
                                                  /* Cut Width         */
        gl_cut_w$ = str(gl_rec$,85%,9%)  
        lbl$(14%)= gl_cut_w$ & fs$
                                                  /* Grid Color        */
        gosub lookup_color
        lbl$(15%)= gl_color$ & fs$

                                                  /* Window Color      */
        wnd_color_code$ = str(gl_rec$,128%,1%)
        gosub lookup_wnd_color
        lbl$(20%)= wnd_color$ & fs$
                                                  /* Inside/Outside    */
/*      glass_code$ = str(gl_rec$,77%,2%)                              */
        txt_inside_outside$ = "N/A"
        gosub lookup_inside_outside
        lbl$(21%)= txt_inside_outside$ & fs$
        
                                                  /* Glass Description */
/*      glass_code$ = str(gl_rec$,77%,2%)                              */
/*      glass_code$ = str(rm_part$,5%,2%)                              */
        gosub lookup_glass_text
        lbl$(22%) = txt_glass$ & fs$
                                                  /* Glass Code        */
        lbl$(23%) = "(" & glass_code$ & ")" & fs$

/* (AWD006) */

                                                  /* new bc */
        glchkbc$ = str(gl_reb$,1%,10%)
        if glchkbc$ < "0000" or glchkbc$ > "9999" then glchkbc$ = "0000" 
        lbl$(24%) = glchkbc$ & fs$
                                                  /* Shift Code        */
        gl_shft$ = str(gl_rec$,247%,2%) 
        lbl$(16%)= gl_shft$ & fs$
        lbl$(16%) = " "
/*  (AWD007)  */
REM  !        gl_ultra$ = str(gl_rec$,255%,1%)
REM  !        if gl_ultra$ = "1" then lbl$(16%) = "ULTRA" & fs$
/* (/AWD006) */
/* (AWD007) */
        gl_intercept$ = str(gl_rec$,255%,2%)
        convert gl_intercept$ to gl_intercept%, data goto dataerr1

dataerr1:

        lbl$(16%) = interdesc$(gl_intercept%) & fs$
                                                  /* Actual Width & Height*/
        gl_win_w$ = str(gl_rec$,150%,7%)
        gl_win_h$ = str(gl_rec$,157%,6%)
        lbl$(17%)= gl_win_w$ & " X " & gl_win_h$ & fs$
                                                  /* (AWD008) 'B'      */
        gl_breatht$ = " "                /*<AWD008>  Breath Tube Flag  */
        if str(sub_part$,7%,1%) = "4" then gl_breatht$ = "B"      
/*      gl_breatht$ = "B"                       temp for testing */
        lbl$(19%) = " " & gl_breatht$ & " " & fs$

        return

format_remake_label                               /* Format (EWDGLSXX) */
        gl_barcode$ = str(gl_rec$,33%,9%)         /* Glass Barcode     */
        gl_num1$    = str(gl_rec$,42%,3%)         /* Re_make Number(EWD001)*/
        gosub lookup_sub_part
        glass_code$ = str(gl_rec$,45%,2%)
        if rm_part$ <> " " then glass_code$ = str(rm_part$,5%,2%)

        lbl$(1%) = gl_barcode$ & gl_num1$ & fs$   /* (EWD001)          */
                                                  /* Production Seq No.*/
        lbl$(25%) = gl_barcode$ & gl_num1$ & fs$   /* CR2180 */   
        
        gl_seq$ = str(gl_rec$,87%,5%) 
        lbl$(2%) = str(gl_seq$,1%,5%) & fs$       /* Last 5 Digits     */

                                                  /* (PAR000) 'C' or 'W'*/
        gl_contour$ = str(gl_rec$,199%,1%)        /* Contour Grid Flag */
	/* <PAR004> */
        gl_contour$ = str(gl_rec$,199%,1%)        /* Contour Grid Flag */
	/* <PAR004> */

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
                                                  /* Grid Color        */
/*PAR003   gosub lookup_sub_part                                       */
/*PAR003*/ gosub lookup_color
        gl_color$ = str(gl_rec$,193%,6%)
        lbl$(15%)= gl_color$ & fs$
                                                  /* Window Color      */
        wnd_color_code$ = str(gl_rec$,99%,2%)
        gosub lookup_wnd_color
        lbl$(20%)= wnd_color$ & fs$
                                                  /* Inside/Outside    */
/*      glass_code$ = str(gl_rec$,45%,2%)                              */
        txt_inside_outside$ = "N/A"
        gosub lookup_inside_outside
        lbl$(21%)= txt_inside_outside$ & fs$
        
                                                  /* Glass Description */
/*      glass_code$ = str(gl_rec$,45%,2%)                              */
/*      glass_code$ = str(rm_part$,5%,2%)                              */
        gosub lookup_glass_text
        lbl$(22%) = txt_glass$ & fs$
                                                  /* Glass Code        */
        lbl$(23%) = "(" & glass_code$ & ")" & fs$

/* (AWD006) */
             
                                                  /* new bc */
        glchkbc$ = str(gl_reb$,1%,10%)
        if glchkbc$ < "0000" or glchkbc$ > "9999" then glchkbc$ = "0000" 
        lbl$(24%) = glchkbc$ & fs$ 

                                                  /* Shift Code        */
        gl_shft$ = str(gl_rec$,85%,2%) 
        lbl$(16%)= gl_shft$ & fs$

        lbl$(16%) = " "
/*  (AWD007)  */
REM  !        gl_ultra$ = str(gl_rec$,255%,1%)
REM  !        if gl_ultra$ = "1" then lbl$(16%) = "ULTRA" & fs$

/* (/AWD006) */
/* (AWD007) */
        gl_intercept$ = str(gl_rec$,255%,2%)
        convert gl_intercept$ to gl_intercept%, data goto dataerr2

dataerr2:

	lbl$(16%) = interdesc$(gl_intercept%) & fs$
/* (\AWD007) */                                 /* Actual Width & Height*/
        gl_win_w$= str(gl_rec$,92%,7%)
        gl_win_h$= str(gl_rec$,101%,6%) 
        lbl$(17%)= gl_win_w$ & "X " & gl_win_h$ & fs$
                                                  /* Glass Text        */
        txt1$ = str(gl_rec$,150%,40%)             /* Line (2)          */
        lbl$(18%) = txt1$ & fs$
                                                  /* (AWD008) 'B'      */
        gl_breatht$ = " "                /*<AWD008>  Breath Tube Flag  */
        if str(sub_part$,7%,1%) = "4" then gl_breatht$ = "B"      
/*      gl_breatht$ = "B"            pwww       temp for testing */
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

                                                     /* (PAR000)          */
        lookup_sub_part
           init(" ") rm_key$, sub_part$,rm_part$
/*         rm_key$ = "         "     pwww well hell we'll try this!!  */
/*      return   */                                  /* Disable for Now   */
/*         str(rm_key$,1%,9%)  = str(gl_rec$,33%,9%)  this could be wrong */
           str(rm_key$,1%,9%)  = gl_barcode$     
/*         str(rm_key$,10%,3%) = str(gl_rec$,42%,3%) this too             */
/*         str(rm_key$,10%,3%) = gl_num1$  remove so we get a pt-num      */
           read #3,key 0% >= rm_key$, using L64000, rm_key$, rm_part$,    ~
                            sub_part$, eod goto L64010
           if gl_barcode$ <> str(rm_key$,1%,9%) then goto L64010
L64000:          FMT POS(22), CH(09), POS(125), CH(25), POS(257), CH(20)
  REM   FMT POS(255), CH(20)
        
           return
L64010:       sub_part$ = "00000               "
              init(" ") rm_key$, sub_part$,rm_part$
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

/* (AWD007) */
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

/* (\AWD007) */


