        REM *************************************************************~
            *  Program Name      - SCRLABEL                             *~
            *  Creation Date     - 11/24/2009                           *~
            *                                                           *~
            *  Description       - This Program Creates the Screen      *~
            *                      labels replacing the cyberquery      *~
            *                      label reports.                       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *11/24/2009! New Program                              ! DES *~
            *07/02/2010! AWD001 add dept & new colrs              ! DES *~
            *03/14/2013! (AWD002) mod for NTX Label               ! CMG *~
            *03/26/2013! (AWD003) changes for coastal screen      ! CMG *~
            *************************************************************

        dim                              /* FILE = APCPLNDT            */~
            dt_part$25, dt_wood$3,       /* Prod/Comp Seq. (0) or (1)  */~
            dt_desc$30, des1$20,         /* Prod/Comp Date (0) or (1)  */~
            sub_part$20,                 /* DESCRIPTION                */~
            scr_shft$2,                  /* DESCRIPTION                */~
            scr_shft_d$30                /*                            */

        dim                              /* FILE - (APCPLNW1)          */~
            wrk_key$53,                  /* WRK PRIMARY KEY            */~
            wd$7,                        /* Actual Width               */~
            ht$6                         /* Actual Height              */ 

        dim                              /* (Program Variables)        */~
            hdr$40, msg$(3%)79,          /* ASKUSER TEXT               */~
            screen_dte$8,                /* Screen Comp Date Formated  */~
            screen_dte1$8,               /* Screen Comp Date Unformated*/~
            scr_dte$8,                   /* Screen Completion Date FORM*/~
            scr_dte1$8,                  /* Screen Comp. Date Unform   */~
            scr_code$1,                  /* Screen Report Selection    */~
            scr_dept$3,                  /* Screen Department Code     */~
            dept$3,                      /* Screen Department Code     */~
            scr_from$5,                  /* Screen Department Code     */~
            scr_to$5,                    /* Screen Department Code     */~
            scr_batch_name$20,           /* Screen Department Code     */~
            scr_msg$30,                  /* Screen - Report Selection  */~
            scr_msg1$30, l_txt$25,       /* Screen - Product Line      */~
            scr_load$5, scr_desc$30,     /* Screen - Load Number       */~
            scr_inv$1, scr_pdesc$30,     /* Inv Pull                   */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            message$256,                 /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */ 

        dim f2%(20%),                    /* = 0 if the file is open    */~
            fs%(20%),                    /* = 1 if file open, -1 if it */~
            rslt$(20%)20                 /* Text from file opening     */

        dim                              /* FILE - (NEW LABEL VARS)    */~
            model$3,                     /* MODEL CODE                 */~
            width$10,                    /* WIDTH                      */~
            height$10,                   /* HEIGHT                     */~
            cut$(4%)62                   /* Max (4) Screens            */

       dim  num$3,                       /* Label Day of Week          */~
            scr_pull$1,                  /* Screen Pull or Cut         */~
            colorcd$2, a$256, b$256,                                     ~
            screen$3 

        dim dd$(90%)255, bb$(90%)255

        dim rec$(8)253, xx$(90%)255, yy$(90%)255 ,lbl$(40)252,            ~
            file$8, srt_rec$(9)230,      /* Lbl Print File             */~
            library$8, batch$5,          /* Library Name = APCDATA     */~
            script$8, prev_batch$5,      /* Lbl SHELL SCRIPT           */~
            volume$6, srt_key$46         /* DISK VOLUME = CARLOS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$41, pname$21
            apc$   = "(New)Planning Screen Processing Utility  "
            pname$ = "SCRLABEL - Rev: 01.00"

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
            * #1 ! AWDPLNSR ! Screen Production File                    *~
            * #3 ! GENCODES ! Master System Table File                  *~
            * #5 ! MFGSCR   ! Print File for AES Screen Labels          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "AWDPLNSR",                                     ~
                        varc,     indexed,  recsize = 512,               ~
                        keypos =   42, keylen =   12,                    ~
                        alt key  1, keypos =    7, keylen =  47,         ~
                            key  2, keypos  = 163, keylen =  13,         ~
                            key  3, keypos =   1, keylen =  53,          ~
                            key  4, keypos = 205, keylen =  12, dup 

            select #2,  "SORTLBLS",                                     ~
                        varc,     indexed,  recsize = 2070,              ~
                        keypos = 2025, keylen =   46                      

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #5,  "MFGSCR", varc, consec, recsize = 256



            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1%),  f2%(1%),   0%, rslt$(1%))
            call "OPENCHCK" (#2,  fs%(2%),  f2%(2%), 1000%, rslt$(2%))
                close #2
            call "FILEBGON" (#2)        
            call "OPENCHCK" (#2,  fs%(2%),  f2%(2%), 1000%, rslt$(2%))
            call "OPENCHCK" (#3,  fs%(3%),  f2%(3%),   0%, rslt$(3%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            fs$ = "^FS"
            been_here% = 0%
/* (AWD002) */            
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #3, schema_err%)
/* (\AWD002) */              
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  4%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then end_program
                      if keyhit% <> 0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%
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
                  if keyhit%  = 16% then gosub begin_process
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 4% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120


        REM *************************************************************~
            *             P R O C E S S   D A T A                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        begin_process
            gosub open_file 
            gosub load_label
            gosub print_labels

        return clear all
        goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
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
         "Enter (1) Cut Labels           , (2) Pull Labels             ",~
         "Enter the Planned Production Date Associated with Screens.   ",~
         "Enter the Batch Name to print.                               ",~
         "Enter the Batch Number to print (optional).                  "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28280
                inpmessage$ = edtmessage$
                return

L28280
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, scr_code$, scr_msg$,       ~
                      scr_dte$, scr_dte1$, scr_dept$, scr_msg1$,         ~
                      screen_dte$, screen_dte1$, scr_load$, scr_desc$,   ~
                      dt_part$, dt_desc$, scr_shft$, scr_shft_d$, cut$(),~
                      scr_from$, scr_to$, scr_batch_name$,               ~
                      dt_wood$, scr_pull$, scr_pdesc$, scr_inv$
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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40210,         /* Screen's Selection*/   ~
                                L40200,         /* Production Date   */   ~
                                L40200,         /* Batch Name        */   ~
                                L40210          /* Batch Number      */   

              goto L40230

L40190:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40210:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,20), fac(hex(a4)), apc$                   , ch(41),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Screen Selections (1-2) :",                  ~
               at (06,30), fac(lfac$(1%)), scr_code$            , ch(01),~
               at (06,40), fac(hex(84)), scr_msg$               , ch(30),~
               at (07,02), "Planned Production Date :",                  ~
               at (07,30), fac(lfac$(2%)), scr_dte$             , ch(08),~
               at (08,02), "Batch Name              :",                  ~
               at (08,30), fac(lfac$(3%)), scr_batch_name$      , ch(20),~
               at (09,02), "Batch Number (optional) :",                  ~
               at (09,30), fac(lfac$(4%)), scr_from$            , ch(05),~
               at (09,36), "To",                                         ~
               at (09,39), fac(lfac$(4%)), scr_to$              , ch(05),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40640
                  call "PRNTSCRN"
                  goto L40230

L40640:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
           l_txt$ = "Production Load or Blank:"
           if  scr_code% = 3% then l_txt$ = "Schedule No. for Lineal :"

        if edit% = 2% then L40830     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40790
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40790:     if fieldnr% > 1% then L40810
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40810:     return

L40830: if fieldnr% > 0% then L40920  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Print Labels"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return
L40920:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                 (9)Display Dept's      " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffff09ffffffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50150,         /* Screen's Selection    */ ~
                              L50340,         /* Production Date       */ ~
                              L50350,         /* Batch Name            */ ~
                              L50360          /* Batch Number (optional) */ 
            return

L50150: REM Screen's Selection                    SCR_CODE$
            scr_code% = 0%
            if scr_code$ <> " " then goto L50190
               scr_code$ = "1"
L50190:     convert scr_code$ to scr_code%, data goto L50290

        return
L50290:     errormsg$ = "(Error) - Invalid Screen's Selection?"
            gosub error_prompt
            init(" ") scr_code$, scr_msg$
        return

L50340: REM Planned Production Date               SCR_DTE$, SCR_DTE1$
           date% = 0%
           call "DATEOK" (scr_dte$, date%, errormsg$ )
       wrk_date = date%

        return

L50350          /* Batch Name            */  

        return

L50360          /* Batch Number (optional) */ 
            if scr_from$ = "     " then scr_to$ = "     "
            if scr_from$ = "     " then return
            if scr_to$   = "     " then scr_to$ = scr_from$
            scr_from% = 0%
            scr_to% = 0%
            convert scr_from$ to scr_from%, data goto L50365
            convert scr_to$   to scr_to%,   data goto L50365
            if scr_from% > scr_to% then goto L50367
            convert scr_from% to scr_from$, pic (00000)      
            convert scr_to%   to scr_to$,   pic (00000)      
        return

L50365:     errormsg$ = "(Error) - Invalid Batch Number(s)?   "
            gosub error_prompt
            init(" ") scr_from$, scr_to$, scr_msg$
        return

L50367:     errormsg$ = "(Error) - From Batch must be less then To"
            gosub error_prompt
            init(" ") scr_from$, scr_to$, scr_msg$
        return

/* AWDPLNSR   1 -   6 PD  DATE
              7 -   7 C   PROCESS
              8 -  13 PD  DATE1
             14 -  33 C   BATCH
             34 -  38 C   BATCH #
             39 -  41 C   DEPT
             42 -  50 C   BAR
             51 -  53 C   NUM
             54 -  58 C   SEQ
             59 -  61 C   MODEL
             62 -  62 C   COLOR
             63 -  63 C   SC
             64 -  64 C   TYPE
             65 -  65 C   HF
             66 -  73 PD  WIDTH DEC
             74 -  81 PD  HEIGHT DEC
             82 -  89 PD  CB LEN
             90 -  97 PD  CB LOC
             98 - 107 C   WIDTH FRAC
            108 - 117 C   HEIGHT FRAC
            118 - 127 C   CB LEN FRAC
            128 - 137 C   CB LOC FRAC
            138 - 162 C   PART
            163 - 163 C   ST
            164 - 172 C   BAR1
            173 - 175 C   NUM1
            176 - 176 C   PULL
            177 - 184 C   S.O.
            185 - 194 C   WD
            195 - 204 C   HT     */

print_labels:
        call "SHOSTAT" ("One Moment Please, generating Labels")
        init(hex(00)) wrk_key$
        sc_pull$ = "C"
        put str(wrk_key$,1,6), using get_date, wrk_date
        if scr_code$ = "2" then sc_pull$ = "P"
        read #1, key 3% > wrk_key$, using AWDPLNSR, rec$(), eod goto sorted  
             goto skip_read
read_next:
        read #1, using AWDPLNSR, rec$(), eod goto sorted  
skip_read
AWDPLNSR: FMT 2*CH(256)

SORTFILE: FMT 9*CH(230)
        get str(rec$(),1,6), using get_date, sr_date
        if str(wrk_key$,1,6) <> str(rec$(),1,6) then goto sorted
get_date: FMT PD(11,1)
        if sr_date <> wrk_date then goto read_next
        if str(rec$(),176,1) <> sc_pull$ then goto read_next
/* add batch name & number selection */
        des1$ = str(rec$(),14,20) 
        des2$ = str(rec$(),34,05) 
        if scr_batch_name$ <> str(rec$(),14,20) then goto read_next
        if scr_from$ = "     " then no_filter
        if scr_from$ > str(rec$(),34,5) then read_next
        if scr_to$   < str(rec$(),34,5) then read_next

no_filter:
        colorcd$ = "NA"  
        if str(rec$(),62,1) = "2" then colorcd$ = "WH"
        if str(rec$(),62,1) = "6" then colorcd$ = "AL"
        if str(rec$(),62,1) = "7" then colorcd$ = "CY"
        if str(rec$(),62,1) = "3" then colorcd$ = "BZ"
        if str(rec$(),62,1) = "A" then colorcd$ = "NO"
        if str(rec$(),62,1) = "B" then colorcd$ = "HO"
        if str(rec$(),62,1) = "E" then colorcd$ = "CH"  /* AWD001 */
        if str(rec$(),62,1) = "F" then colorcd$ = "CE"  /* AWD001 */
        init(" ") srt_rec$(), srt_key$
        if scr_code$ = "2" then goto pull_key
/* cut */
        str(srt_key$,01,05) = str(rec$(),34,05)      /* batch */
        type$ = str(rec$(),64,01)      /* type */ 
        str(srt_key$,06,01) = type$                  /* type */ 
        if type$ = "E" then str(srt_key$,6,1) = "2"
        if type$ = "R" then str(srt_key$,6,1) = "1"
        str(srt_key$,07,03) = str(rec$(),39,03)      /* dept */  
        width = 0.00
        height = 0.00
        get str(rec$(),66,8), using real_sz, width  /* width 9's comp */
REM  width = ROUND(width,4%)
        width = (99999999999.9999 - width)
        put str(srt_key$,14,8), using real_sz, width
        get str(rec$(),74,8), using real_sz, height /* height 9's comp */
        put str(srt_key$,22,8), using real_sz, height
        str(srt_key$,30,05) = str(rec$(),54,05)     /* SEQ   */ 
        str(srt_key$,35,12) = str(rec$(),42,12)      
real_sz:  FMT PD(15,4)
        str(srt_rec$(),1,2024) = rec$()
        str(srt_rec$(),2025,45) = srt_key$           

        write #2, using SORTFILE, srt_rec$()
           goto read_next
pull_key:
        str(srt_key$,01,05) = str(rec$(),34,05)     /* batch */ 
        str(srt_key$,06,01) = type$                  /* type */ 
        if type$ = "E" then str(srt_key$,6,1) = "2"
        if type$ = "R" then str(srt_key$,6,1) = "1"
        str(srt_key$,07,02) = colorcd$              /* color */ 
        str(srt_key$,09,20) = str(rec$(),98,20)     /* width & height */ 
        str(srt_key$,29,05) = str(rec$(),54,05)     /* SEQ   */ 
        str(srt_key$,34,12) = str(rec$(),42,12)     /* key 0 */ 
        str(srt_key$,46,01) = " "                 
        str(srt_rec$(),1,2024) = rec$()
        str(srt_rec$(),2025,46) = srt_key$           
        write #2, using SORTFILE, srt_rec$()
          goto read_next

sorted 
        init(hex(00)) wrk_key$
        init(" ") prev_batch$, prev_type$
        gosub print_dummy_labels
        read #2,key > wrk_key$, using SORTFILE, srt_rec$(), eod goto end_label
         goto skip_sorted
read_sorted
        read #2, using SORTFILE, srt_rec$(), eod goto end_label
skip_sorted
        srt_key$ = str(srt_rec$(),2025,41)           
        colorcd$ = "NA"  
        if str(srt_rec$(),62,1) = "2" then colorcd$ = "WH"
        if str(srt_rec$(),62,1) = "6" then colorcd$ = "AL"
        if str(srt_rec$(),62,1) = "7" then colorcd$ = "CY"
        if str(srt_rec$(),62,1) = "3" then colorcd$ = "BZ"
        if str(srt_rec$(),62,1) = "A" then colorcd$ = "NO"
        if str(srt_rec$(),62,1) = "B" then colorcd$ = "HO"
        if str(srt_rec$(),62,1) = "E" then colorcd$ = "CH"  /* AWD001 */
        if str(srt_rec$(),62,1) = "F" then colorcd$ = "CE"  /* AWD001 */

        screen$ = "  "  
        if str(srt_rec$(),63,1) = "1" then screen$ = "HS"
        if str(srt_rec$(),63,1) = "2" then screen$ = "FS"
        if str(srt_rec$(),63,1) = "8" then screen$ = "SCO"
        if str(srt_rec$(),63,1) = "9" then screen$ = "WS"
        if str(srt_rec$(),63,1) = "A" then screen$ = "EFS"
        if str(srt_rec$(),63,1) = "B" then screen$ = "RFS"
        if str(srt_rec$(),63,1) = "C" then screen$ = "RHS"
        if str(srt_rec$(),63,1) = "J" then screen$ = "EHS"

        model$ = str(srt_rec$(),059,3)
        bar$   = str(srt_rec$(),042,9)
        num$   = str(srt_rec$(),051,3)
        seq$   = str(srt_rec$(),054,5)
        width$  = str(srt_rec$(),098,10)
        height$ = str(srt_rec$(),108,10)
        cb_loc$ = str(srt_rec$(),128,10)
        cb_len$ = str(srt_rec$(),118,10)
        wd$     = str(srt_rec$(),185,10)
        ht$     = str(srt_rec$(),195,10)
        type$  = str(srt_rec$(),064,1)
        batch$  = str(srt_rec$(),34,5)

       if batch$ <> prev_batch$ or                                  ~  
           type$  <> prev_type$  then gosub print_break_labels
        prev_batch$ = batch$
        prev_type$ = type$

        gosub print_label

        goto read_sorted

end_label:
        goto exit_program
        return

print_dummy_labels
        xx$() = dd$()
        gosub print_break
        gosub print_break
        gosub print_break
        gosub print_break
        gosub print_break
        gosub print_break
        gosub print_break
        gosub print_break
        gosub print_break
        return

print_break_labels
       str(bb$(14),34,01) = str(type$,1,1) 
       str(bb$(14),39,04) = "PULL" 
       if sc_pull$ = "C" then                                    ~ 
       str(bb$(14),39,04) = "*CUT" 
       str(bb$(14),43,13) = " LABELS " & batch$  
       xx$() = bb$() 
       gosub print_break
       gosub print_break
       gosub print_break
       gosub print_break
     return

load_label:
        /* positioning is 200/inch in both the X & Y axis */
        /* make sure you use font 0 (zero) not O          */

           init(" ") yy$(), dd$(), bb$()
  
           yy$( 1%) = "^JO"                          /* This format is used*/
           yy$( 2%) = "^XA^EG^XZ"
           yy$( 3%) = "^XA"
           yy$( 4%) = "^PMN"
           yy$( 5%) = "^MNY"
           yy$( 6%) = "^MMT"                          /* Back Feed Off */
           yy$( 7%) = "^MTT"
           yy$( 8%) = "^MD0"
           yy$( 9%) = "^LH0,0"
           yy$(10%) = "^LL2400"
           yy$(11%) = "^PR4"                          /* (AWD002)            */
           yy$(12%) = "^JMA"
           yy$(13%) = "01^FO060,020^CI0^A0N,25,35^FR^FD"    
           yy$(14%) = "02^FO060,045^CI0^A0N,25,35^FR^FD"    
           yy$(15%) = "03^FO500,015^BY2,2.5,80^BCN,80,Y,N,N^FR^FD>:"
           yy$(16%) = "04^FO060,088^CI0^A0N,35,35^FR^FD"      
           yy$(17%) = "05^FO510,122^CI0^A0N,35,35^FR^FD"      
           yy$(18%) = "^FO060,127^CI0^A0N,35,35^FR^FD(DO NOT REMOVE)^FS"
           yy$(19%) = "06^FO360,122^CI0^A0N,35,35^FR^FD"      
           yy$(20%) = "07^FO360,077^CI0^A0N,45,45^FR^FD"      
           yy$(21%) = "^PQ1"
           yy$(22%) = "^XZ"

           dd$( 1%) = "^JO"                          /* This format is used*/
           dd$( 2%) = "^XA^EG^XZ"
           dd$( 3%) = "^XA"
           dd$( 4%) = "^PMN"
           dd$( 5%) = "^MNY"
           dd$( 6%) = "^MMT"                          /* Back Feed Off */
           dd$( 7%) = "^MTT"
           dd$( 8%) = "^MD0"
           dd$( 9%) = "^LH0,0"
           dd$(10%) = "^LL2400"
           dd$(11%) = "^PR4"                          /* (AWD002)            */
           dd$(12%) = "^JMA"
  dd$(13%) =  "^FO090,50^CI0^A0N,35,25^FR^FD************ DUMMY ************^FS"
  dd$(14%) = "^FO090,95^CI0^A0N,35,25^FR^FD************ LABEL ************^FS"
           dd$(15%) = "^PQ1"
           dd$(16%) = "^XZ"

           bb$( 1%) = "^JO"                          /* This format is used*/
           bb$( 2%) = "^XA^EG^XZ"
           bb$( 3%) = "^XA"
           bb$( 4%) = "^PMN"
           bb$( 5%) = "^MNY"
           bb$( 6%) = "^MMT"                          /* Back Feed Off */
           bb$( 7%) = "^MTT"
           bb$( 8%) = "^MD0"
           bb$( 9%) = "^LH0,0"
           bb$(10%) = "^LL2400"
           bb$(11%) = "^PR4"                          /* (AWD002)            */
           bb$(12%) = "^JMA"
  bb$(13%) =  "^FO090,50^CI0^A0N,35,25^FR^FD*******************************^FS"
  bb$(14%) = "^FO090,95^CI0^A0N,35,25^FR^FD****X****XXXXXXXXXXXXXXXXX*****^FS"  
           bb$(15%) = "^PQ1"
           bb$(16%) = "^XZ"

           return

print_label    
      init (" ") lbl$()
      lbl$(01) = model$ & "  " & colorcd$ & "  " & screen$ & "      " & ~
                                                    seq$ & fs$  
      lbl$(02) = " CL  " & cb_loc$ & "  C  " & cb_len$ & fs$
      lbl$(03) = bar$ & fs$
      lbl$(04) = width$ & "X" & height$ & fs$ 
      lbl$(05) = " A:  " & wd$ & "X" & ht$ & fs$

      dept$ = str(srt_rec$(),39,03)              
      if dept$ = "007" then dept$ = "07A"
      if dept$ = "049" then dept$ = "49B"
      if dept$ = "005" then dept$ = "05C"
      lbl$(06) = "Dept. " & dept$  & fs$            
      xx$() = yy$()
      sub_part$ = str(srt_rec$(),278,20)              
      lbl$(07) = "    " & fs$            
      if str(sub_part$,15,1) = "2" then lbl$(07) = "WIRE " & fs$            
      if str(dt_part$,11,1) = "D" then lbl$(07) = "LOCK " & fs$ /* AWD002 */
REM Subpart door hardware field      
      if str(sub_part$,4,1) = "3" then lbl$(07) = "CSTL " & fs$ /* AWD002 */
      if str(sub_part$,4,1) = "D" then lbl$(07) = "CSTL " & fs$ /* AWD002 */
      if str(sub_part$,4,1) = "E" then lbl$(07) = "CSTL " & fs$ /* AWD002 */      

    print_break
        nbr_lines% = 0%

    read_loop
        init(" ") a$
        b$ = all(hex(00))
        nbr_lines% = nbr_lines% + 1%
        a$ = xx$(nbr_lines%)
        if a$ = " " then end_process
        a_len% = len(a$)                   /* Calc Length of Data String */
        str(b$,1%,a_len%) = str(a$,1%,a_len%) /* Put into b$ Data from a$ */
        convert str(a$,1%,2%) to ln%, data goto skip_data
                                           /* Look for a Field Number    */ 
        l_len% = len(lbl$(ln%))            /* Find Length of Data Element*/
                                           /* in the Label data array    */
        b_len% = (a_len% - 2%) + l_len%    /* Adjust for 2 digit field No*/

        b$ = all(hex(00))                  /* Initialize Print string    */
                                           /* 1st set Font data for print*/
                                           /* 2nd append Actual Data that*/
                                           /*     will be printed        */    
        str(b$,1%,b_len%) = str(a$,3%,a_len%-2%)                           ~
            & str(lbl$(ln%),1%,l_len%)
      skip_data
                                           /* (AWD002)                  */
        if nbr_lines% = 1% and been_here% = 1% then                        ~
                               b$ = hex(7e) & str(a$,2%,a_len%)

        if nbr_lines% = 1% and been_here% > 1% then                        ~
                               goto read_loop

  
        gosub print_line
                                           /* (AWD002)                 */         
        if a$ = "^XZ" then end_process       /* Last Line */

                                           /* (AWD002)                 */ 
        goto read_loop

      end_process
        been_here% = been_here% + 1%
      return

        print_line
                                                  /* (RHHTEST)          */
            write #5, using L55030, b$, eod goto L55030
L55030: FMT CH(256)
        return

        open_file
            library$        = "APCDATA "
            volume$         = "CARLOS"
            file$           = "MFGSCR"
            script$         = "MFGSCR"
            if schema% = 2% then script$ = "NTXSCR"   /* (AWD001) */
            call "OPENFILE" (#5, "IO   ", f2%(5%), rslt$(5%), axd$ )
            if f2%(5%) <> 0% then goto L01100
               gosub file_exists         
               if comp% <> 16% then goto end_program
                  call "FILEBGON" (#5)

L01100:    open nodisplay #5, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return

        file_exists
          comp% = 2%     
          hdr$ = "***  New AES Barcode Label ***"
          msg$(1%) = "       The File (MFGAES) Already Exists.         "
          msg$(2%) = "     New  AES B A R C O D E   L a b e l s        "
          msg$(3%) = "Press <RETURN> To Exit Prog, or PF(16) to Delete."
          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                    
        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")
            lb1% = 0% : lb2% = 0%

            close #2
            close #5
            call "LINK" addr(script$, lb1%, lb2%)

            call "FILEBGON" (#2)        
            call "FILEBGON" (#5)        
        end_program            
        end
