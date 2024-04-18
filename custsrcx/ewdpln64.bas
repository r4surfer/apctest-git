        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLN64                             *~
            *  Creation Date     - 11/23/98                             *~
            *  Last Mod Date     - 01/01/06                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Special Search Routine for Glass     *~
            *                      Both regular and re-make glass       *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *  Subroutine Used   - (EWDPLA58) Display Data              *~
            *                      (EWDGLSSB) Calculate Glass Cuts      *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/23/98 ! (New) Program                            ! RHH *~
            * 12/03/98 ! (EWD0001) Clean-up mods                  ! RHH *~
            * 01/22/99 ! (EWd002) Mods to (EWDPLA58) add File and ! RHH *~
            *          !   Rack Barcode (EWDPLNRK)                !     *~
            * 11/17/03 ! (EWD003) Mods to Add Tempered Remakes    ! CMG *~
            * 01/01/06 ! (PAR000) CR347 Mod for sub part          ! CMG *~
            *05/26/2015! (IM8022) APCPLNGR & AWDPLNGR mods        ! CMG *~
            *************************************************************

        dim                                                              ~
            sc_code$1, sc_code_d$30,     /* Search Selection Code      */~
            sc_search$9, sc_search_d$32, /* Search Value               */~
            rm_key$12, rm_rec$(2%)256,   /* Re-make key and record     */~
            rm_dept$3,                   /* Department Code            */~
            rm_reason$2,                 /* Re-make reason Code        */~
            rm_model$3,                  /* Model Code                 */~
            rm_seq$4,                    /* Deptment Seq. No.          */~
            rm_so$8,                     /* Re-make Sales Order No.    */~
            rm_ln$2,                     /* Sales Order Line Item      */~
            rm_dte$8,                    /* Last Scann Date            */~
            rm_time$8, rm_time_d$8,      /* Re-make Time Scan Time     */~
            rm_user$3,                   /* Scanned User id            */~
            rm_tot$4, rm_tot1$7,         /* Time Total for Re-make     */~
            h1$9, h2$2, h3$21, h4$2,     /* Display Headers            */~
            h5$3, h6$4, h7$8, h8$8,      /*                            */~
            h9$7, h10$3, title$44,       /*                            */~
            wandchar$1,                  /* Wand Character - Scanner   */~
            sd1$24,                      /* Search text                */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            dsp_msg$79,                  /* Screen Display Message     */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            rk_barcode$9, rk_seq$5,      /* Glass/Rack Barcode and Seq */~ 
            userid$3,                    /* Current User Id            */~
            glsType$20                   /* IM8022  Glass Type         */

        dim dt$(24%)79, tt$(7%)21,       /* Analysis Display    EWD004 */~
            cc$(24%)1                    /* Selection                  */
 
        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(20%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = " Special Glass Search/Look-Up Routine "
            pname$ = "EWDPLN64 - Rev: R7.00"

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
            * #1  ! APCPLNGR ! Glass Re-make Master File                *~
            * #2  ! AMTBOMCD ! Master Equation File                     *~
            * #3  ! APCPLNDT ! Planning Master Detail                   *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            * #5  ! APCPLNOR ! Planning Header Histroy                  *~
            * #6  ! APCPLNSC ! Planning Master Schedule File            *~
            * #7  ! CUSTOMER ! Customer Master File                     *~
            * #8  ! TXTFILE  ! Sales Master Text File                   *~
            * #9  ! AMTBOMIF ! Master VALIDITY FILE                     *~
            * #10 ! BCKLINES ! S.O. Detail                              *~
            * #15 ! EWDPLNRK ! Glass master Rack File (EWD002)          *~
            * #16 ! AWDPLNGR ! Glass Re-make Master File Tempered (EWD003)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
/* IM8022 */
            select #1,  "APCPLNGR",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos = 22,   keylen =  12,                     ~
                        alt key  1, keypos  =     7, keylen = 27,        ~
                            key  2, keypos  =     1, keylen = 33,        ~
                            key  3, keypos  =    13, keylen = 21

            select #2,  "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42

            select #3,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  24,  keylen =  23,                     ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


            select #5,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos = 1,    keylen = 51,                      ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup

            select #6,  "APCPLNSC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 24,   keylen = 10,                      ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33

            select #7,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos = 1,    keylen =  9,                      ~
                        alt key 1, keypos = 10, keylen = 30, dup,        ~
                            key 2, keypos =424, keylen =  9, dup,        ~
                            key 3, keypos =771, keylen =  9, dup,        ~
                            key 4, keypos =780, keylen =  9, dup,        ~
                            key 5, keypos = 1049, keylen = 9, dup

            select #8,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

            select #9, "AMTBOMIF",                                       ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32

            select #10, "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

                                                   /* (EWD002) - Begin */
            select #15, "EWDPLNRK",                                      ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  9,                      ~
                        alt key  1, keypos = 10, keylen =  14 
                                                   /* (EWD002) End     */ 


             call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 100%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),  0%, rslt$(6%))
            call "OPENCHCK" (#7, fs%(7%), f2%(7%),  0%, rslt$(7%))
            call "OPENCHCK" (#8, fs%(8%), f2%(8%),  0%, rslt$(8%))
            call "OPENCHCK" (#9, fs%(9%), f2%(9%),  0%, rslt$(9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%),  0%, rslt$(10%))
                                                    /* (EWD001)        */
            call "OPENCHCK" (#15, fs%(15%), f2%(15%),  0%, rslt$(15%))
                                                    /* (EWD003)        */

            mat f1% = zer

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

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   2%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 2% then editpg1
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
                                                 /* Search for Glass   */ 
        process_data                           
            init(" ") rm_key$
            dt% = 0% 
            str(rm_key$,1%,9%) = sc_search$
            sc_search% = len(sc_search$)
            call "SHOSTAT" ("Searching for Glass Data")
        process_data_next
               gosub find_glass                            /*  (EWD003)  */
               if glass% = 0% then goto process_data_done  /* (EWD003)   */
               gosub load_screen_dt
               goto process_data_next
        process_data_done
            dt_max% = dt%
            gosub display_analysis
        return clear all
        goto inputmode



        find_glass                                   /*  (EWD003)    */
            glass% = 0%
            tempered% = 0%
REM            RM_KEY$ = SC_SEARCH$

            read #1,key > rm_key$, using L19000 , rm_key$, eod goto no_glass
L19000:        FMT POS(22), CH(12)

            if str(rm_key$,1%,sc_search%) > str(sc_search$,1%,sc_search%) ~
                                            then goto no_glass
REM               SV_KEY$ = STR(RM_KEY$,1%,12%)
               glass% = 1%
REM        RETURN
REM        FIND_TEMP_GLASS
REM            INIT(" ") RM_KEY$
REM            STR(RM_KEY$,1%,12%) = SV_KEY$
REM
REM            READ #16,KEY > RM_KEY$, USING L19000 , RM_KEY$, EOD GOTO NO_GLASS
REM
REM            IF STR(RM_KEY$,1%,SC_SEARCH%) > STR(SC_SEARCH$,1%,SC_SEARCH%) ~
REM                                            THEN GOTO NO_GLASS
REM
REM               SV_KEY$ = STR(RM_KEY$,1%,12%)
REM               GLASS% = 1%
REM               TEMPERED% = 1%
        no_glass
        return                                     /*  (EWD003)   */



        load_screen_dt
            gosub dataload                     /* (EWD003) */
            if rm_st$ <> "0" and rm_st$ <> "9" then goto L20000
               k1% = 1%                     /* Scanned in Re-Make    */
               gosub stuff_rm               /* Scanned Re-Make Glass */
               return

L20000:     if rm_st$ <> "1" then goto L20010
               k1% = 5%                     /* Must be (1) Scheduled */
                                            /* Scheduled Glass       */
               if str(rm_num$,2%,2%) <> "00" then k1% = 3%
                                            /* Scheduled Re_Make GLS */
                  gosub stuff_rm
                  return
L20010:     if rm_st$ <> "2" then goto L20040        /*  (EWD004)   */
               k1% = 6%                    /* Completed Glass       */
               if str(rm_num$,2%,2%) <> "00" then k1% = 4%
                                            /* Completed Re-Make     */
               gosub stuff_rm
               return
L20040:     k1% = 7%
            gosub stuff_rm
        return

        stuff_rm                                /* (EWD003) */
           if rm_st$ <> "0" and rm_st$ <> "9" then goto L20100
           if rm_reason$ > "25" and rm_reason$ < "31" then goto L20100
                k1% = 2%                    /* In-House Re-Make Glass  */

L20100:    dt% = dt% + 1%                   /* Update Specified Bucket */
           if dt% > 24% then dt% = 24%      /* Screen Full, No More    */

           str(dt$(dt%),1%,9%)   = str(rm_key$,1%,9%)
                                              /* Remake Barcode        */
           str(dt$(dt%),11%,3%)  = rm_num$    /* Re-Make Number        */
           str(dt$(dt%),15%,21%) = tt$(k1%)   /* Description           */
           str(dt$(dt%),37%,2%)  = rm_reason$ /* Re-Make reason Code   */
           str(dt$(dt%),40%,3%)  = rm_dept$   /* Department Code       */
           str(dt$(dt%),44%,4%)  = rm_seq$    /* Department Seq No.    */
           str(dt$(dt%),49%,8%)  = rm_dte$    /* Last Scan Date        */
           str(dt$(dt%),58%,8%)  = rm_time$   /* Last Scan Time        */
           str(dt$(dt%),67%,7%)  = rm_tot1$   /* Re-Make Total Time    */
           str(dt$(dt%),75%,3%)  = rm_user$   /* Re-Make User Id       */    

        return

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
         "Enter a Valid Search Code? 1 = Warranty Id, 2 = Glass Barcode",~
         "Enter a Valid Search Value?                                  "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sc_code$,  sc_code_d$,     ~
                      sc_search$, dt$(), cc$(), tt$()

            tt$(1%) = "Scanned Re-Make Prod."     /* Text Length = 21   */
            tt$(2%) = "Scanned Re-Make House"
            tt$(3%) = "Scheduled Re-Make Gls"
            tt$(4%) = "Completed Re-Make Gls"
            tt$(5%) = "Scheduled Glass      "
            tt$(6%) = "Completed Glass      "  
            tt$(7%) = "Received  Glass      "          /*  (EWD004)   */

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
            
        dataload    
            get #1, using L30000, rm_rec$()          /* (PAR000) */
L30000:       FMT 2*CH(256)
            init(" ") rm_st$, rm_num$, rm_dept$, rm_reason$, rm_model$,   ~
                      rm_seq$, rm_so$, rm_dte$, rm_time$, rm_tot$,        ~
                      rm_tot1$, rm_user$, rm_time_d$, glsType$

            rm_st$    = str(rm_rec$(),13%,1%)  /* Re-Make Status Code   */
            rm_num$   = str(rm_rec$(),31%,3%)  /* Re-Make Number        */
            rm_dept$  = str(rm_rec$(),249%,3%) /* Production Department */
            rm_reason$= str(rm_rec$(),34%,2%)  /* Remake Reason Code    */
            rm_model$ = str(rm_rec$(),72%,3%)  /* Model Code            */
            rm_seq$   = str(rm_rec$(),243,4%)  /* Sequence Number       */
            rm_so$    = str(rm_rec$(),163,8%)  /* S.O. Number           */
   
            rm_dte$ = str(rm_rec$(),52,6%)     /* Re_make Glass Date    */
                                               /* or Status Date        */
           if len(rm_dte$) < 3 then rm_dte$ = str(rm_rec$(),36%,6%)
           call "DATEFMT" (rm_dte$)            /* Scan/Remake Date     */

           rm_time$ = "xx:xx:xx"               /* Re-make Time         */
           rm_time_d$ = str(rm_rec$(),44%,8%)  /* (EWD001) Clean-up    */   
           if len(rm_time_d$) > 4 then goto L30010
              str(rm_time$,1%,2%) = str(rm_rec$(),14%,2%) /* Hours     */
              str(rm_time$,4%,2%) = str(rm_rec$(),17%,2%) /* Minutes   */
              str(rm_time$,7%,2%) = str(rm_rec$(),20%,2%) /* AM or PM  */
              goto L30020                       /* Time of last Status */     
                                                /* Change              */ 
L30010:    str(rm_time$,1%,2%) = str(rm_rec$(),44%,2%) /* Hours        */
           str(rm_time$,4%,2%) = str(rm_rec$(),46%,2%) /* Minutes      */
           str(rm_time$,7%,2%) = str(rm_rec$(),48%,2%) /* Seconds      */
           
L30020:    rm_user$ = str(rm_rec$(),58%,3%)          /* User Id        */
           rm_tot$  = str(rm_rec$(),61%,4%)
           if len(rm_tot$) < 3% then rm_tot$ = "0000"
           str(rm_tot1$,1%,3%) = str(rm_tot$,1%,2%) & "H"
           str(rm_tot1$,5%,3%) = str(rm_tot$,3%,2%) & "M"
                                                    /* (EWD002)        */
           rk_barcode$ = str(rm_rec$(),22%,9%)
           rk_seq$     = str(rm_rec$(),242%,5%)
                                                    /* (EWD002)        */
/* IM8022 */
           glsType$    = str(rm_rec$(),335%,20%)
           if str(glsType$,1%,8%) = "TEMPERED" then tempered% = 1%                                                    
        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT

        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE


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
              on fieldnr% gosub L40120,          /* Search Selection   */~
                                L40120           /* Search Code        */

              goto L40130

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40120:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40130:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Search Selection Code :",                    ~
               at (03,27), fac(lfac$(1%)), sc_code$             , ch(01),~
               at (03,40), fac(hex(84)),   sc_code_d$           , ch(30),~
                                                                         ~   
               at (04,02), fac(hex(84)),   sd1$                 , ch(24),~
               at (04,27), fac(lfac$(2%)), sc_search$           , ch(09),~
               at (04,38), fac(lfac$(3%)), wandchar$            , ch(01),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40450
                  call "PRNTSCRN"
                  goto L40130

L40450: close ws
        call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf1
           sd1$                        = "                       "
           if sc_code$ = "1" then sd1$ = "Enter Warranty Id.    :"
           if sc_code$ = "2" then sd1$ = "Enter Glass Barcode   :"

           if fieldnr% = 2% then lfac$(3%) = hex(81) 

        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1) = "(1)Start Over     (4)Previous Field     " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(2),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1),19,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over     (4)Previous Field     " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (10)Search Data        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffff0affffff0e0f1000)
            return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       " 
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return


        REM *************************************************************~
            *           D I S P L A Y   S U M M A R Y   S C R E E N     *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_analysis
            k% = 0%
L41000:     gosub set_pf2   
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
                                                                         ~
               at (02,66), fac(hex(84)), pageno$                , ch(14),~
                                                                         ~
               at (03,19), fac(hex(a4)), title$                 , ch(44),~
                                                                         ~
               at (05,04), fac(hex(a4))  , h1$                  , ch(09),~
               at (05,14), fac(hex(a4))  , h2$                  , ch(03),~
               at (05,18), fac(hex(a4))  , h3$                  , ch(21),~
               at (05,40), fac(hex(a4))  , h4$                  , ch(02),~
               at (05,43), fac(hex(a4))  , h5$                  , ch(03),~
               at (05,47), fac(hex(a4))  , h6$                  , ch(04),~
               at (05,52), fac(hex(a4))  , h7$                  , ch(08),~
               at (05,61), fac(hex(a4))  , h8$                  , ch(08),~
               at (05,70), fac(hex(a4))  , h9$                  , ch(07),~
               at (05,78), fac(hex(a4))  , h10$                 , ch(03),~             
                                                                         ~
               at (06,02), fac(hex(81))  , cc$(k% + 1%)         , ch(01),~
               at (06,04), fac(hex(84))  , dt$(k% + 1%)         , ch(77),~
                                                                         ~
               at (07,02), fac(hex(81))  , cc$(k% + 2%)         , ch(01),~
               at (07,04), fac(hex(84))  , dt$(k% + 2%)         , ch(77),~
                                                                         ~
               at (08,02), fac(hex(81))  , cc$(k% + 3%)         , ch(01),~
               at (08,04), fac(hex(84))  , dt$(k% + 3%)         , ch(77),~
                                                                         ~
               at (09,02), fac(hex(81))  , cc$(k% + 4%)         , ch(01),~
               at (09,04), fac(hex(84))  , dt$(k% + 4%)         , ch(77),~
                                                                         ~
               at (10,02), fac(hex(81))  , cc$(k% + 5%)         , ch(01),~
               at (10,04), fac(hex(84))  , dt$(k% + 5%)         , ch(77),~
                                                                         ~
               at (11,02), fac(hex(81))  , cc$(k% + 6%)         , ch(01),~
               at (11,04), fac(hex(84))  , dt$(k% + 6%)         , ch(77),~
                                                                         ~
               at (12,02), fac(hex(81))  , cc$(k% + 7%)         , ch(01),~
               at (12,04), fac(hex(84))  , dt$(k% + 7%)         , ch(77),~
                                                                         ~
               at (13,02), fac(hex(81))  , cc$(k% + 8%)         , ch(01),~
               at (13,04), fac(hex(84))  , dt$(k% + 8%)         , ch(77),~
                                                                         ~
               at (14,02), fac(hex(81))  , cc$(k% + 9%)         , ch(01),~
               at (14,04), fac(hex(84))  , dt$(k% + 9%)         , ch(77),~
                                                                         ~
               at (15,02), fac(hex(81))  , cc$(k% + 10%)        , ch(01),~
               at (15,04), fac(hex(84))  , dt$(k% + 10%)        , ch(77),~
                                                                         ~
               at (16,02), fac(hex(81))  , cc$(k% + 11%)        , ch(01),~
               at (16,04), fac(hex(84))  , dt$(k% + 11%)        , ch(77),~
                                                                         ~
               at (17,02), fac(hex(81))  , cc$(k% + 12%)        , ch(01),~
               at (17,04), fac(hex(84))  , dt$(k% + 12%)        , ch(77),~
                                                                         ~
               at (18,02), fac(hex(81))  , cc$(k% + 13%)        , ch(01),~
               at (18,04), fac(hex(84))  , dt$(k% + 13%)        , ch(77),~
                                                                         ~
               at (19,02), fac(hex(81))  , cc$(k% + 14%)        , ch(01),~
               at (19,04), fac(hex(84))  , dt$(k% + 14%)        , ch(77),~
                                                                         ~
               at (21,02), fac(hex(a4)),   dsp_msg$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~  
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L41040             /* First    */
L41020:           k% = 0%
                  goto L41000

L41040:        if keyhit% <> 3% then goto L41080             /* Last      */
L41060:           x% = int(val_max% / 14%)
                  k% = (x%*14%)
                  if (k% + 1%) > val_max% then k% = k% - 14% 
                  goto L41000

L41080:        if keyhit% <> 4% then goto L41100             /* Previous */
                  if k% < 15% then goto L41020
                  k% = k% - 14%
                  if k% <= 1% then goto L41020
                  goto L41000

L41100:        if keyhit% <> 5% then goto L41115             /* Next     */
                  k% = k% + 14%
                  if k% < val_max% then goto L41000
                  goto L41060

L41115:        if keyhit% <> 0% then goto L41150
                  gosub display_detail
                  goto L41000
        
L41150:        if keyhit% <> 15 then goto L41155
                  call "PRNTSCRN"
                  goto L41000

L41155:        if keyhit% <> 16% then goto L41000

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return clear all
        goto inputmode

        set_pf2
            init(" ") h1$, h2$, h3$, h4$, h5$, h6$, h7$, h8$,h9$, h10$
            dsp_msg$=                                                     ~
             "Use any Character to Display Detail Info.,followed by <Return>?"
            str(dsp_msg$,65%,15%) = "Total [ xxxxx ]"
            convert dt% to str(dsp_msg$,73%,5%), pic(#####)


            title$ = " Display of Glass records for " & str(sd1$,7%,15%)

            pageno$ = "Page: XX of XX"             /* k% = Array Values */
            h1$ = "Barcode  "                      /* (9)               */
            h2$ = "Num"                            /* (3)               */
            h3$ = "<-Glass Description->"          /* (21)              */
            h4$ = "Rs"                             /* (2)               */
            h5$ = "Dpt"                            /* (3)               */
            h6$ = "Seq."                           /* (4)               */
            h7$ = "Scan Dte"                       /* (8)               */
            h8$ = "Scan Tim"                       /* (8)               */
            h9$ = "TotTime"                        /* (7)               */
            h10$= "Usr"                            /* (3)               */
            val_max% = dt_max%
            if val_max% > (24% - 14%) then val_max% = 24% - 14%
                                                        /* Display Max */
            x = val_max%
            yy% = ( val_max% / 14% )
            if mod(x,14) <> 0 then yy% = yy% + 1%

            xx% = (k% / 14%) +1%
            if xx% > yy% then xx% = yy%

            convert xx% to str(pageno$,7%,2%), pic(##) /* Current Page*/

            convert yy% to str(pageno$,13%,2%), pic(##)/* Total Pages */
  

            pf$(1) = "(2)First           (5)Next              " &        ~
                     "                                       "
            pf$(2) = "(3)Last                                 " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(4)Previous                             " &        ~
                     "                       (16)Exit Display" 
            pfkeys$ = hex(ff02030405ffffffffffffffff0f1000)
            gosub check_screen
            return

        check_screen
            if val_max% > 14% then goto L41860
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L41860:      if k% >= 14% then goto L41870
                gosub no_first
                gosub no_prev
L41870:      if (k% + 14%) <= val_max% then goto L41880
                gosub no_last
L41880:      if k% <= (val_max% - 14%) then goto L41900
                gosub no_next
L41900: return
        no_first
            str(pf$(1%),1%, 9%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(1%),20%, 9%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
            str(pf$(2%),1%,9%)   = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(3%),1%,12%)  = " " : str(pfkeys$,4%,1%) = hex(ff)
        return


        REM *************************************************************~
            *           D I S P L A Y   S U M M A R Y   S C R E E N     *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        
       display_detail
            er% = 0%
            for dt% = 1% to dt_max%
                if cc$(dt%) <> " " then goto L42000
            next dt%
            goto L42020                          /* No Selection Found */
L42000:
       read #3,key 4% = str(dt$(dt%),1%,8%), using L42010, rm_so$, rm_ln$,~
                                                   eod goto L42030
L42010:   FMT POS(24), CH(8), CH(2)
 
       call "EWDPLA58" (1%,              /* o%=Info Only, 1%=Info+Glass*/~
                        rm_so$,          /* Sales Order Number         */~
                        rm_ln$,          /* Sales order Line Item      */~
                        rk_barcode$,     /* Glass/Rack Barcode (EWD002)*/~
                        rk_seq$,         /* Production Seq No. (EWD002)*/~
                        " ",             /* place holder               */~
                        " ",             /* place holder               */~
                        " ",             /* place holder               */~
                        " ",             /* place holder               */~
                        " ",             /* place holder               */~
                        " ",             /* place holder               */~
                        " ",             /* Place Holder               */~
                        " ",             /* Place Holder               */~
                        #5,              /* (APCPLNOR) Planning Header */~
                        #6,              /* (APCPLNSC) Planning Ln Item*/~
                        #7,              /* (CUSTOMER) Customer Master */~
                        #4,              /* (GENCODES) Master Tables   */~
                        #9,              /* (AMTBOMIF) Master Validity */~
                        #8,              /* (TXTFILE ) Sales Text File */~
                        #2,              /* (AMTBOMCD) Master Equation */~
                        #10,             /* (BCKLINES) S.O. Dtl(EWD001)*/~
                        #15,             /* (EWDPLNRK) Rack fil(EWD002)*/~  
                        er% )            /* 0% = Ok, Non-Zero = Error  */
L42020: init(" ") cc$()
        return
L42030: init(" ") cc$()
        errormsg$ = "(Error) - Could Not Load Detail Data??"
        gosub error_prompt
        return
       
        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            init(" ") errormsg$
            on fieldnr% gosub L50000,                 /* sc_status$   */  ~
                              L50100                  /* sc_type$     */
            return

L50000: Rem Search Selection Code                      sc_code$
            init(" ") sc_code_d$
            if sc_code$ <> " " then goto L50005
               sc_code$ = "2"
  
L50005:     if sc_code$ = "1" then sc_code_d$ = "Product Warranty Id."                            /* (PLAN SCH1)      */
            if sc_code$ = "2" then sc_code_d$ = "Glass Barcode       "
            if len(sc_code_d$) < 5 then goto L50010
   
        return
L50010:     init(" ") sc_code$, sc_code_d$
            errormsg$ = "(Error) Invalid Search Selection Code?"
            gosub error_prompt
        return

L50100: Rem Search Value                                sc_search$
            if sc_search$ <> " " then goto L50105

             sc_search_d$ = " "
             sc_search_d$ = hex(06) & "Select a Valid Glass Barcode?"
             call "PLOWCODE" (#1,sc_search$, sc_search_d$, 0%, 0.00, f1%(1%))

L50105:     if sc_code$ = "1" and len(sc_search$) < 8 then             ~
                                                goto L50110
            if sc_code$ = "2" and len(sc_search$) < 9 then             ~
                                                goto L50120
            gosub process_data

        return
L50110:     init(" ") sc_search$
            errormsg$ = "(Error) Invalid Warranty Id Code?"
            gosub error_prompt
        return
L50120:     init(" ") sc_search$
            errormsg$ = "(Error) Invalid Glass Barcode?"
            gosub error_prompt
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

     
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            for kk% = 1% to 10%
                close #kk%
            next kk% 
          
        end

