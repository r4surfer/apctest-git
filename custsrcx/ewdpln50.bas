        REM *************************************************************~
            *  Program Name      - EWDPLN50                             *~
            *  Creation Date     - 06/27/2022                           *~
            *                                                           *~
            *  Description       - Scan to generate Lowe's stock label  *~
            *                      by barcode.                          *~
            *                                                           *~
            *  Code Tables Used  - PLAN NEWC                            *~
            *                                                           *~
            *  Subroutine Used   - EWDPLO50 (Print Lowe's Label)        *~
            *                                                           *~
            *  Special Comments  -  CR3199                              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *06/27/2022! (New) Program - Copied & Mod EWDPLN48.   ! RDB *~ 
            *************************************************************

        dim                                                              ~
            barcode_scn$18,              /* Barcode Scanned            */~
			prevcode$18,                 /* Last Scanned Barcode       */~
            lb_key$35, lb_key1$23,       /* Record Key from LB         */~
            lb_rec$(4%)256,              /* LB Record Data             */~
            lb_prddate$10,               /* Prod. Date from LB         */~
            lb_dept$3,                   /* Dept. No. from LB          */~
            lb_shift$2,                  /* Shift Code from LB         */~
            lb_load$5,                   /* Load Number from LB        */~
            lb_barcode$18,               /* Production Barcode         */~
            lb_foam$1,                   /* Foam Flag Y,N,0,1          */~
            lb_part$25,                  /* Part number                */~
            lb_seq$5,                    /* Sequence Number            */~
            filename$8,                  /* Used by EWDOPEN            */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            time$8,                      /* System time                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            scrn_title$79,               /* Title of program           */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            inp_text$(3%)79,             /* Input Prompt Text          */~   
            userid$3,                    /* Current User Id            */~
            color$1,                     /* Color code     CR2802      */~
            schema$8,                    /* Schema Switch              */~   
            bck_key$25,                  /* BCKMASTR key  CR2756       */~
            stk_prt$1,                   /* Stock Print Flagged        */~
			scr_info$70,                 /* Window info from print job */~
            sc_sel_p$1,                  /* Printer Number             */~
			sc_nbr_lbl$1,                /* Number of labels to print  */~
            xx$(9%)50,                   /* Screen Display area Text   */~		
			scnr$1,                      /* Scanner screen input       */~
			scr_sku$9,                   /* Screen SKU                 */~
			scr_series$8,                /* Screen Series              */~
			scr_style$ 8,                /* Screen Style               */~
			scr_model$3,                 /* Screen Model               */~
			scr_color$20,                /* Screen Color Description   */~
			scr_grid$10                  /* Screen Grid                */
			
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21                        
			
            apc$   = "Print Lowes Stk Labels"
            pname$ = "EWDPLN50 - Rev: R1.00"

        REM *************************************************************
                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNDT ! Production Master Detail File            *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            * #5  ! EWDPRDLB ! Production Label Data File               *~
            * #6  ! BCKLINES ! Back Log Line Item File                  *~
            * #7  ! AWDSKUXR ! SKU x-ref file                           *~
            * #8  ! APCPCMST ! Passed File                              *~
            * #9  ! BCKMASTR ! Backlog master file                      *~
            * #10 ! NFRCDATA !                                          *~
            * #11 ! NFRCMDL  !                                          *~
            * #12 ! NFRCGLS  !                                          *~			
            * #63 ! BCKSUBPT ! New Sub Prt Number File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  24,  keylen =  23,                     ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #5, "EWDPRDLB",                                       ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =   35,                    ~
                        alt key  1, keypos = 278, keylen =  23

            select #6, "BCKLINES",                                       ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #7,   "AWDSKUXR",                                   ~
                        varc,     indexed,  recsize = 256,              ~
                        keypos =    1, keylen =  16,                    ~
                        alt key  1, keypos =  17, keylen =  20,         ~
                            key  2, keypos =  37, keylen =  45, dup
/*
 sku#       1- 16 ch(16)
 upc#      17- 36 ch(20)
 part      37- 61 ch(25)
 sub part  62- 81 ch(20)
 model     82- 87 ch(06)
 desc      88-157 ch(70)
filler    158-256 ch(99)
*/
            select #8, "AWDPCMST",                                      ~
                        varc,     indexed,  recsize =   128,            ~
                        keypos =    9, keylen = 53,                     ~
                        alt key  1, keypos =   1, keylen =  8

            select #9,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup
            
			select #10, "NFRCDATA",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos = 1,    keylen = 104,                     ~
                        alt key  1, keypos    =   1, keylen = 24, dup,   ~
                            key  2, keypos    = 105, keylen = 30, dup

            select #11, "NFRCMDL",                                       ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 25,    keylen = 20,                     ~
                        alt key 1, keypos = 1, keylen = 44

            select #12, "NFRCGLS",                                       ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,    keylen = 7
						
            select #63, "BCKSUBPT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup

                                                       /* (PAR000)     */


            call "SHOSTAT" ("Opening Files, One Moment Please")
            filename$ = "APCPLNDT" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDPRDLB" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDSKUXR" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDPCMST" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKMASTR" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "NFRCDATA" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "NFRCMDL"  : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "NFRCGLS"  : call "EWDOPEN" (#12, filename$, err%)		
            if err% <> 0% then gosub open_error
			
        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            call "TIME" (time$)
            scrn_title$ = "Lowe's Stock Label for Cardboard Sleeve"  
			
            if str(userid$,2%,1%) = "F" then scnr$ = "Y"
			if userid$ = "LS1" then scnr$ = "Y"
			if scnr$ = "Y" then goto L00005
			
            inp_text$(01%)="Scan Barcode or Manually Enter Barcode Number"
            inp_text$(02%)="Successful Print, Scan next Barcode          "
            inp_text$(03%)="Reprinted Label, Scan next Barcode           "
			goto L00010
L00005:
            inp_text$(01%)="Scan Barcode               "
            inp_text$(02%)="Successful Print, Scan next"
            inp_text$(03%)="Reprinted Label, Scan next "
L00010:   
            init(" ") xx$()
            err%    = 0%
            call "SCHEMA" (schema$,      /* What switch 1-NC 2-NE      */~
                           schema%,      /* Schema                     */~
                           #1,           /* GENCODES                   */~
                           err% )        /* error                      */   

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
		    if scnr$ = "Y" then goto rf_inputm
			
            gosub initialize_variables

            for fieldnr% = 1% to 1%
                gosub'050(1%, fieldnr%)
L10130:         gosub'100(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
					  if keyhit%  =  3% then switchn% = 1%
                      if keyhit% = 16%  then gosub exit_program
                          
                      if keyhit% <> 0% then gosub L10130
                gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then goto L10130
            next fieldnr%
   
            gosub dataload

rf_inputm:
            gosub initialize_variables

            for fieldnr% = 1% to 1%
                gosub'050(1%, fieldnr%)
L10135:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit% =  1% then gosub startover
                      if keyhit% =  4%  then gosub exit_program					  
                      if keyhit% = 16%  then gosub exit_program
                          
                      if keyhit% <> 0% then gosub L10135
                gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then goto L10135
            next fieldnr%
   
            gosub dataload
			
        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Scan Barcode                                                 "
   
        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            switchn% = 0%			
            goto inputmode

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") inpmessage$, barcode_scn$            
                                                      
           lbl% = 0%
        return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload                                        
            call "SHOSTAT" ("Printing Production Labels...")

            been_here% = 0%
            stk_prt$ = " "
   
            lb_key1$ = all(hex(00))
            str(lb_key1$,1%,18) = barcode_scn$

            read #5, key 1% > lb_key1$, using L35040, lb_rec$(),            ~
                                                      eod goto L20001
L35040:     FMT 4*CH(256)               

            lb_key1$ = str(lb_rec$(),278%,23%)
                                                
            lb_barcode$ = str(lb_rec$(),278%,18%)
            if lb_barcode$ <> barcode_scn$ then goto L20002
                                                                 
            lb_prddate$ = str(lb_key$,1%,6%)
            lb_dept$    = str(lb_key$,12%,3%)
            lb_shift$   = str(lb_key$,15%,2%)
            lb_load$    = str(lb_key$,22%,5%)
            lb_seq$     = str(lb_rec$(),311%,5%)     
            lb_foam$    = str(lb_rec$(),601%,1%)
            lb_part$    = str(lb_rec$(),523%,25%)
   
/* White and Black only colors CR2802 */           
            color$ = str(lb_part$,4%,1%)
            if color$ <> "2" and color$ <> "4" then goto L20003
            
               gosub check_repair
               if lb_foam% = 1% then goto L20004  /* Skip       */
            
            gosub check_ecat_sku
            if ecat% = 1% then goto L20005

            err% = 0%

            sc_sel_p$ = "1"      /* only 1 printer */
			sc_nbr_lbl$ = "2"    /* number of labels to print */
			
/* F3 hidden key changes temporarily to 1 */			
			if switchn% = 1% then sc_nbr_lbl$ = "1"   
			
            call "EWDPLO50" (been_here%, lb_rec$(), #4, #1, #6, #63, #7, ~
                 #8, #10, #11, #12, scr_info$, sc_sel_p$, lbl%, scnr$,   ~
				 sc_nbr_lbl$, err%)
            if lbl% = 0% and err% = 0% then err% = 1%
                                            /* Finished - delete ZPL file */
            call "EWDPLO50" (been_here%, lb_rec$(), #4, #1, #6, #63, #7,    ~
                 #8,  #10, #11, #12, scr_info$, sc_sel_p$, lbl%, scnr$,     ~
				 sc_nbr_lbl$, 99%)
            
			if err% <> 0% then gosub print_error

            prevcode$ = barcode_scn$
            gosub set_printed_barcode
			gosub parse_print_info 
			init(" ") errormsg$
			switchn% = 0%
            goto inputmode 

REM *************************************************************~   
    * Validation error messages                                 *~
    *************************************************************
L20001: 
          errormsg$ = "Barcode Not Planned"  
          gosub error_prompt
          goto L20010
L20002: 
          errormsg$ = "Barcode Not Found" 
          gosub error_prompt
          goto L20010
L20003: 
          errormsg$ = "Window Invalid Color" 
          gosub error_prompt
          goto L20010
L20004: 
          errormsg$ = "Foam product is skipped" 
          gosub error_prompt
          goto L20010
L20005:
          errormsg$ = "ECAT order not stock" 
          gosub error_prompt
          goto L20010
L20010:  
          init(" ") barcode_scn$, xx$()
          goto inputmode
		
REM *************************************************************
        check_repair                            
            lb_foam% = 0%
            if lb_foam$ = "Y" or lb_foam$ = "N" then goto L30110

            p% =  pos("AS" = str(lb_load$,1%,1%))

            if p% <> 0% then goto L30110

            lb_foam% = 1%                       /* Skip Label Print     */
L30110: return
        
        REM *************************************************************~
            * New eCat validate check     CR2756                        *~
            *   Should not be needed one APCPLN06 goes into production  *~
            *    already planned orders are completed.                  *~
            *************************************************************
            
        check_ecat_sku
               init(" ") bck_key$
               ecat% = 0%
               
               str(bck_key$,1%,9%)   = str(lb_rec$(),727%,9%)
               str(bck_key$,10%,16%) = str(lb_barcode$,1%,8%)
      
               read #9,key = bck_key$, using L30200, bck_user_entered$, ~
                                                  eod goto L30220
L30200:          FMT POS(836), CH(03)

                  if bck_user_entered$ = "ECT" then ecat% = 1%
L30220:
        return
        

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'100(fieldnr%)
          gosub set_screen
    
          accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), scrn_title$            , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(84)), fld$(1%)               , ch(23),~
               at (05,27), fac(lfac$(1%)), barcode_scn$         , ch(18),~
               at (05,50), fac(lfac$(2%)), wandchar_scn$        , ch(01),~
                                                                         ~
			   at (06,02), fac(hex(84)), fld$(2%)               , ch(25),~
			                                                             ~
               at (06,42), fac(hex(84)), fld$(3%)               , ch(04),~
               at (06,47), fac(hex(84)), prevcode$              , ch(18),~
                                                                         ~
               at (08,16), fac(hex(84)), xx$(1%)                , ch(50),~
               at (09,16), fac(hex(84)), xx$(2%)                , ch(50),~
               at (10,16), fac(hex(84)), xx$(3%)                , ch(50),~
               at (11,16), fac(hex(84)), xx$(4%)                , ch(50),~
               at (12,16), fac(hex(84)), xx$(5%)                , ch(50),~
               at (13,16), fac(hex(84)), xx$(6%)                , ch(50),~
               at (14,16), fac(hex(84)), xx$(7%)                , ch(50),~
               at (15,16), fac(hex(84)), xx$(8%)                , ch(50),~	
			   at (16,16), fac(hex(84)), xx$(9%)                , ch(50),~		
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then L08380
                  gosub startover

L08380:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_screen
          init(" ") dateout$
          inpmessage$ = inp_text$(1%)
          if stk_prt$ = "Y" and errormsg$ = " " then ~
              inpmessage$ = inp_text$(2%)
          if stk_prt$ = "R" and errormsg$ = " " then ~
		      inpmessage$ = inp_text$(3%)
		  if errormsg$ > " "  then init(" " ) xx$() 
          call "TIME" (dateout$)
                                                       /* (AWD028)       */
          fld$(1%)      = "Scan Barcode :"
          fld$(2%)      = "   Print 2 Labels "
		  if switchn% = 1% then fld$(2%)      = "   Print 1 Labels "
          fld$(3%)      = "Prv:"
                                                       /* (AWD028)       */

       
          pf$(1%) = "(1)Startover                            " &       ~
                     "                       (16)Exit Screen "
          pfkeys$ = hex(01ff03ffffffffffffffffffffff0f1000)

             init(" ") barcode_scn$, wandchar$
             lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
             lfac$(3%) = hex(84) : lfac$(4%) = hex(84)

        return

        deffn'101(fieldnr%)
          gosub set_rfscreen
    
          accept                                                       ~
               at (01,02), fac(hex(8c)), apc$                   , ch(22),~	  
               at (02,02), fac(hex(94)), errormsg$              , ch(20),~
                                                                         ~
               at (03,02), fac(hex(84)), fld$(1%)               , ch(20),~
               at (04,03), fac(lfac$(1%)), barcode_scn$         , ch(18),~
               at (04,23), fac(lfac$(2%)), wandchar_scn$        , ch(01),~
                                                                         ~
               at (07,02), fac(hex(a4)),   inpmessage$          , ch(20),~
               at (08,02), fac(hex(8c)),   pf$(1%)              , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then L08381
                  gosub startover

L08381:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_rfscreen
          init(" ") dateout$
          inpmessage$ = inp_text$(1%)
          if stk_prt$ = "Y" and errormsg$ = " " then ~
		        inpmessage$ = inp_text$(2%)
          if stk_prt$ = "R" and errormsg$ = " " then ~
		        inpmessage$ = inp_text$(3%)
		  if errormsg$ > " "  then init(" " ) xx$() 
          call "TIME" (dateout$) 
		  
          fld$(1%)      = "Barcode :"
       
          pf$(1%) = "(1)Strtovr  (4)Exit " 
          pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)

             init(" ") barcode_scn$, wandchar$
             lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
             lfac$(3%) = hex(84) : lfac$(4%) = hex(84)

        return
		
        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50010    /* Production Barcode */

            return
                                                 
L50010: Rem Scan/Enter Production Barcode         
            if str(barcode_scn$,1%,1%) = " " then goto L50230

            lb_barcode$ = barcode_scn$
            gosub check_barcode
            if barcode% = 0% then goto L50230

        return

L50230:     errormsg$ = "Invalid Barcode, or Not on File"
            gosub error_prompt
            init(" ") barcode_scn$, xx$()
        return
                
REM *************************************************************~
    * Validation on barcodes in the print file                  *~
 *************************************************************
        check_barcode
            barcode% = 0%                           
            init(" ") lb_key1$
            str(lb_key1$,1%,18%) = lb_barcode$
            read #5,key 1% > lb_key1$, using L52000, lb_key1$, eod goto L52010
L52000:        FMT POS(278), CH(23)
            if str(lb_key1$,1%,18%) <> lb_barcode$ then goto L52010
               barcode% = 1%
L52010: return                                      

                
REM *************************************************************~
    * Set flag printed                                          *~
 *************************************************************
        set_printed_barcode
                        
            init(" ") lb_key1$
            str(lb_key1$,1%,18%) = lb_barcode$
            read #5, hold, key 1% > lb_key1$, using L52000, lb_key1$, ~
			   eod goto L60010

              if str(lb_key1$,1%,18%) <> lb_barcode$ then goto L60010   

              get #5 using L60020, stk_prt$
L60020:              FMT POS(736), CH(1)
 
              if stk_prt$ = "Y" then stk_prt$ = "R" else stk_prt$ = "Y"
			  
              put #5, using L60020, stk_prt$
              rewrite #5

L60010:       call "ALLFREE"    
        return  
        
REM *************************************************************~
    * Set display from printed label information                *~
    *************************************************************
        parse_print_info
                        
            init(" ") scr_sku$, scr_series$, scr_style$, scr_model$,  ~
			          scr_grid$, xx$()
			
			if scr_info$ = " " then goto L70010
			
			  scr_sku$    = str(scr_info$, 1%, 9%)
			  scr_series$ = str(scr_info$,10%, 8%) 
			  scr_style$  = str(scr_info$,18%, 8%) 
			  scr_model$  = str(scr_info$,26%, 3%) 
			  scr_grid$   = str(scr_info$,29%,10%) 
			  scr_color$  = str(scr_info$,39%,20%) 
			  
              xx$(2%) = "SKU    ==> " & scr_sku$
              xx$(3%) = "SERIES ==> " & scr_series$
              xx$(4%) = "STYLE  ==> " & scr_style$
              xx$(5%) = "MODEL  ==> " & scr_model$
			  xx$(6%) = "COLOR  ==> " & scr_color$
              xx$(7%) = "GRID   ==> " & scr_grid$
			  xx$(8%) = "  REPLACEMENT          "
			  
              init(" ") readkey$, desc$
              str(readkey$,1%,9%)   = "PLAN NEWC"
              str(readkey$,10%,15%) = scr_model$
              read #4,key = readkey$, using L70005, desc$, eod goto L70010
L70005:           FMT POS(25), CH(30)
                 xx$(8%) = "  NEW CONSTRUCTION     "			 
L70010:
REM              xx$(9%) = "Cardboard Location Not Found"
REM              init(" ") readkey$, desc$
REM              str(readkey$,1%,9%)   = "SKUCRDBRD"
REM              str(readkey$,10%,15%) = scr_sku$
REM              read #4,key = readkey$, using L70005, desc$, eod goto L70020

REM                 xx$(9%) = "Cardboard Location " & desc$		 
REM L70020:        
        return
		
        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        error_prompt
		   if scnr$ = "Y" then goto L55000
		   
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
L55000:		   
        return

        open_error                                   
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return

        print_error

          hdr$     = "***** Label Printing Error *****"
          msg$(1%) = "ERROR OCCURRED WHILE PRINTING LABELS!!!"
		  if err% = 1% then msg$(1%) = "0 labels printed                      "
          if err% = 6% then msg$(1%) = "INVALID STYLE/SERIES/COLOR            "
          if err% = 9% then msg$(1%) = "SCHEMA ERROR                          " 
          if err% = 10% then msg$(1%) = "INVALID SKU                          "    
          if err% = 11% then msg$(1%) = "INVALID STOCK SIZE                   "		  
    
          msg$(2%) = "Return Code (EWDPLA50) = "
          msg$(3%) = "Press Any Key To Continue."

		  if scnr$ = "Y" then goto L65000
		   
          convert err% to str(msg$(2%),26%,2%), pic(#0)
          k% = 2%
          call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
		  
L65000:   
          errormsg$ = msg$(1%)	  
		  init(" ") xx$()
          return clear all
          goto inputmode

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program

        end

