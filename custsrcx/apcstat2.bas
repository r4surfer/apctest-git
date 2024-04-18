REM         *************************************************************~
            *  Program Name      - APCSTAT2                             *~
            *  Creation Date     - 07/30/2008                           *~
            *  Last Modified Date- 07/30/2008                           *~
            *  Written By        - David Speight   	                *~
            *                                                           *~
            *  Description       - This program updates the status      *~
            *                      on APCPLNGR with a scan date <=      *~
            *                      the date entered ans a status of     *~
            *                      0 or 1 and changed it to 2.          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *07/30/2008! New program.                             ! DES *~
            *************************************************************

        dim                              /* FILE - APCPLNGR           */ ~
	    rm_date$8,                   /* rm_date$ from a PD(11,1)  */ ~
	    rm_scan$8,                   /* rm_scan$ from a PD(11,1)  */ ~
	    rm_stat$1,                   /* status 0=RMK, 1=Shd, 2=Com*/ ~
	    rm_st_time$8,                /* status time               */ ~
	    rm_bar$9,                    /* remake barcode            */ ~
	    rm_nbr$3,                    /* remake number             */ ~
	    rm_reason$2,                 /* remake glass reason code  */ ~
	    rm_st_dte$8,                 /* date last remake st change*/ ~
	    rm_shift$2,                  /* glass scanned shift code  */ ~
	    rm_time$8,                   /* time scanned as remake    */ ~
	    rm_dte$8,                    /* rm_dte$ from a PD(11,1)   */ ~
	    rm_id$3,                     /* user who requested remake */ ~
	    rm_complete$4,               /* Tot hrs & min to complete */ ~
	    rm_trk$1,                    /* Glass over tracking code  */ ~
	    rm_fil1$1,                   /* fill remake index area    */ ~
	    rm_load$5,                   /* load number               */ ~
	    rm_model$3,                  /* model code                */ ~
            rm_color$2,                  /* rm_cl$ color code         */ ~
	    rm_glass$2,                  /* rm_gls$ glass type code   */ ~
	    rm_lite$6,                   /* rm_lt$ liting description */ ~
	    rm_width1$9,                 /* rm_wd1$ glass width       */ ~
	    rm_height1$8,                /* rm_ht1$ glass height      */ ~
	    rm_clmr$9,                   /* rm_wd2$ glass specified clmr */ ~
            rm_view$1,                   /* glass view (T or B*/ ~
	    rm_txt$4,                    /* glass text id             */ ~
            rm_stock$9,                  /* rm_stk$ glass stock flag  */ ~
	    rm_part$25,                  /* MFG part number           */ ~
	    rm_wd$7,                     /* window width - long       */ ~
	    rm_ht$6,                     /* window height - long      */ ~
	    rm_so$8,                     /* sales order               */ ~
            rm_spacer_desc$10,           /* glass spacer description  */ ~
	    rm_sand$10,                  /* glass sandwich            */ ~
	    rm_wd_d$8,                   /* calc width decimal        */ ~
	    rm_ht_d$8,                   /* calc height decimal       */ ~
            rm_w_adj$6,                  /* glass width adjustment    */ ~
	    rm_h_adj$6,                  /* glass height adjustment   */ ~
            rm_temp$1,                   /* glass tempered flag       */ ~
	    rm_sort$2,                   /* glass GED sort code       */ ~
	    rm_spacer$6,                 /* glass spacer size         */ ~
            rm_tk$6,                     /* glass overall thickness   */ ~
	    rm_muttin$8,                 /* glass muttin              */ ~
	    rm_seq$5,                    /* department sequence number*/ ~
            rm_shft$2,                   /* production shift code     */ ~
	    rm_dept$3,                   /* glass department code     */ ~
	    rm_num$3,                    /* remake number             */ ~
	    rm_subpart$20,               /* sub part number           */ ~
	    rm_fil2$110,                 /* filler area               */ ~
	    rm_key$27,                   /* key 0                     */ ~
	    rm_rec$(2)192                /* lrecl=384                 */






        dim                              /* FILE - (APCPLNW1)          */~
            tmp$23,                      /* temp work area             */~
            wd$7,                        /* Actual Width               */~
            ht$6,                        /* Actual Height              */~
            scr$(10)60,                  /* Actual Height              */~
            txt_flag$1,                  /* Special Text Exists        */~
            report_date$8,               /* Report Date                */~
            rpt_dte$10,                  /* Report Date                */~
            sav_part$25                  /* Save Part Number           */


        dim                              /* (Program Variables)        */~
            wt$3,                        /* Window Type Code           */~
            hdr$40, msg$(3%)79,          /* ASKUSER TEXT               */~
            qty$5,                       /* QUANTITY                   */~
            grid$25,                     /* Description of Grid        */~
            screen_dte$8,                /* Screen Comp Date Formated  */~
            screen_dte1$8,               /* Screen Comp Date Unformated*/~
            sze$(10%)3,                  /* Save Eights                */~
            sze1$(20%)5,                 /* Save Sixteenths            */~
            wd1$9,                       /* Calculated Width           */~
            wd2$9,                       /* CLMR FOR SCREEN            */~
            ht1$8,                       /* Calculated Height          */~
            sav_wd1$9,                   /* Calculated Width     EWD009*/~
            sav_ht1$8,                   /* CLMR FOR SCREEN      EWD009*/~
            readkey$30,                  /* GENCODES Look-Up Key       */~
            descr$30,   stk_so$8,        /* Use for GENCODES Look-Up   */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
            rpt_date$10,                 /* Date for screen display    */~
            rpt_time$8,                  /* Report Time                */~
            title$25,                    /* Report Title               */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3,                    /* Current User Id            */~
/*(EWD002)*/script$8,                    /* Script to send file to Bayfm*/~
/*(EWD005)*/script_rte$8                 /* Script to send file to RteSn*/

        dim f2%(20%),                    /* = 0 if the file is open    */~
            f1%(20%),                    /* = 1 if READ was successful */~
            fs%(20%),                    /* = 1 if file open, -1 if it */~
            rslt$(20%)20                 /* Text from file opening     */

        dim                              /* FILE - (TEXTFILE)          */~
            textid$4                     /* S.O. TEXTID$               */

        dim                              /* FILE - (NEW LABEL VARS)    */~
            model$3,                     /* MODEL CODE                 */~
            cl$1, cl_l$2, color$6,       /* COLOR CODE                 */~
            gl$2,                        /* GLASS CODE                 */~
            lt$2, co_or$8,               /* LITING CODE                */~
            hg$2, hnge$20, hg_l$8, hh$8,  /* HINGE CODE                 */~
            sc$1, sc_l$4, sc_r$20,       /* SCREEN CODES               */~
            lk$1,                        /* LOCK CODES                 */~
            width$4,                     /* WIDTH                      */~
            height$3,                    /* HEIGHT                     */~
            clmr$3,                      /* CENTER LINE MEETING RAIL   */~
            wallw$3                      /* WALL WIDTH                 */

        dim                              /* FILE - Screen Explosion    */~
            cut$(4%)62,                  /* Max (4) Screens            */~
            txt$(3%)50                   /* Screen '102 Header Text    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$41, pname$21
            apc$   = "Update APCPLNGR Status Utility           "
            pname$ = "APCSTAT2 - Rev: 01.00"

        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #3 ! GENCODES ! Master System Table File                  *~
            * #1 ! APCPLNGR ! Screen Audit File (AWD013)                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


            select #1,  "APCPLNGR",                                      ~
                        varc,     indexed,  recsize = 384,               ~
                        keypos =   22, keylen =   12,                    ~
                        alt key  1, keypos =    7, keylen =  27,         ~
                            key  2, keypos  =   1, keylen =  33,         ~
                            key  3, keypos  =  13, keylen =  21           

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#3,  fs%(3%),  f2%(3%), 0%, rslt$(3%))
            call "OPENCHCK" (#1,  fs%(1%),  f2%(1%), 0%, rslt$(1%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
	    time$ = " "
skippy:     call "DATEFMT" (date$)
            call "TIME" (time$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            init(" ") scr$()
REM         scr$( 1%) = "****************************************"
REM         scr$( 2%) = "*        Consolidated Screen           *"
REM         scr$( 3%) = "*                                      *"
REM         scr$( 4%) = "* (1) - Screen Audit Report            *"
REM         scr$( 5%) = "* (2) -                                *"
REM         scr$( 6%) = "* (3) -                                *"
REM         scr$( 7%) = "*                                      *"
REM         scr$( 8%) = "*                                      *"
REM         scr$( 9%) = "****************************************"
        inpmessage$ = "Ready to start.              "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            fieldnr% = 1% 
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
REM                   if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
L10200:               if keyhit% = 16% then exit_program
                      if keyhit% <> 0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            goto editpg1

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  8% then gosub change_status
                  if keyhit% = 16% then exit_program
            goto   editpg1

        REM *************************************************************~
            *             P R O C E S S   D A T A                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

change_status:         
        init(hex(00)) rm_key$
	upd_cnt% = 0%
read_next:
    read #1,key 1% > rm_key$, hold, using APCPLNGR, rm_rec$(), eod goto end_rpt
APCPLNGR: FMT 2*CH(192)

process_record:
        rm_key$   = str(rm_rec$(),7,27)  
	get rm_rec$(), using scan_date, rm_date
	convert rm_date to rm_date$, pic (00000000)
scan_date: FMT POS(7), PD(11,1)
        rm_stat$ = str(rm_rec$(),13,1) 
        if rm_date$ > report_date$ then goto end_rpt   
	if str(rm_rec$(),13,1) <> "1" then goto read_next 
REM	if str(rm_rec$(),13,1) < "0" then goto read_next 
        str(rm_rec$(),13,1) = "2"
        str(rm_rec$(),14,8) = time$ 
        str(rm_rec$(),36,6) = date      
        upd_cnt% = upd_cnt% + 1%
	tmp$ = str(rm_rec$(),1,23)
        rewrite #1, using APCPLNGR, rm_rec$()
        t% = upd_cnt% / 50%
	t% = t% * 50%
	if t% <> upd_cnt% then goto read_next
        inpmessage$ = "Records changed =            "
        convert upd_cnt% to str(inpmessage$,19,7), pic (######0)
        print at(21,02);hex(a4); inpmessage$
	goto read_next

end_rpt:
        return clear all
        inpmessage$ = "Records changed =            "
        convert upd_cnt% to str(inpmessage$,19,7), pic (######0)
        print at(18,02);hex(a4); inpmessage$
        goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return

        deffn'052(fieldnr%)
            enabled% = 1%
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
REM             inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, scr_code$, scr_msg$,       ~
                      rm_rec$, scr_dte$
            txt$(1) =                                                    ~
                "**************************************************"
            txt$(2) =                                                    ~
                "* (Calculate) Screen Size for Manufactured Parts *"
            txt$(3) =                                                    ~
                "**************************************************"
        return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        REM DATALOAD
        REM RETURN

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Update Store Data and Part Data                           *~
            *************************************************************

        REM DATAPUT
        REM RETURN

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              gosub  L40200         /* Production Date   */   

              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40210:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

REM line 6 removed for now, if needed copy from apcpln47.bas
L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(08),~
               at (02,20), fac(hex(a4)), apc$                   , ch(41),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (07,02), "Planned Production Date :",                  ~
               at (07,30), fac(lfac$(1%)), scr_dte$             , ch(08),~
               at (07,42), "MMDDYYYY                 ",                  ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 8% then goto L40600
                  gosub change_status          
                  inpmessage$ = "Records changed =            "
                  convert upd_cnt% to str(inpmessage$,19,7), pic (######0)
                  goto L40230

L40600:        if keyhit% <> 15 then goto L40640
                  call "PRNTSCRN"
                  goto L40230

L40640:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                       (8)Set Status    " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffff08ffffffffff0e0f1000)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            gosub L50340         /* Production Date       */ 
            return

L50340:         /* Production Date  MMDDYYYY     */ 
            if len(scr_dte$) = 7 then scr_dte$ = "0" & scr_dte$
            if len(scr_dte$) <> 8 then goto L50350
            convert str(scr_dte$,1,2) to mm%, data goto L50350
            convert str(scr_dte$,3,2) to dd%, data goto L50350
            convert str(scr_dte$,5,4) to yyyy%, data goto L50350
            if mm% < 01% or mm% > 12% then goto L50350
            if dd% < 01% or dd% > 31% then goto L50350
            if yyyy% < 2005% or yyyy% > 2099% then goto L50350
            if mm% = 2% and dd% > 29% then goto L50350
	    /* need leap year check for feb 29 */
            if mm% = 1% or mm% = 3% or mm% = 5% or mm% = 7% or mm% = 8% ~
	    or mm% =  10% or mm% = 12% then goto L50345
            if dd% > 30% then goto L50350
L50345:
	    report_date$ = "yyyymmdd"
	    convert yyyy% to str(report_date$,1,4), pic (0000)
	    convert mm%   to str(report_date$,5,2), pic (00)
	    convert dd%   to str(report_date$,7,2), pic (00)
	    rpt_dte$ = str(report_date$,5,2) & "/" &                ~
	               str(report_date$,7,2) & "/" &                ~
	               str(report_date$,1,4)

       return
L50350:     errormsg$ = "(Error) - Invalid Date Entered!      "
            gosub error_prompt
            init(" ") scr_code$, scr_msg$
        return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************
        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return


exit_program:

end_program:
        end
        
        
            
