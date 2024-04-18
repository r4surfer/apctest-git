        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLN76                             *~
            *  Creation Date     - 02/04/00                             *~
            *  Last Modified Date- 01/09/08                             *~
            *  Written By        - Christie Gregory                     *~
            *                                                           *~
            *  Description       - Reads APCPLNAD and new file created  *~
            *                      APCPLNUD  on Load and SO             *~
            *                                                           *~
            *                                                           *~
            *  Code Tables Used  - (PLAN DEPT) - Department Codes       *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/04/00 ! New Program                              ! CMG *~
            * 12/16/02 ! (EWD001) Mod to keep status 12.          ! CMG *~
            * 03/12/03 ! (EWD002) Mod to allow numeric or alpha   ! CMG *~
            *          !          load numbers.                   !     *~
            * 01/09/08 ! (AWD003) mod for new dept 054 and 074    ! CMG *~
            *************************************************************

        dim                              /* FILE = APCPLNAD            */~
            ad_rec$64,                   /* Record Format              */~
            ad_key$33,                   /* Primary Key                */~
            ad_bar$18,                   /* Production Bar Code        */~
            ad_dte$6,                    /* Scanning Date              */~
            ad_dept$3,                   /* Scanning Dept Code         */~
            ad_proc$2,                   /* Scanning Process Code      */~
            ad_shift$2,                  /* Production Scanning Shift  */~
            ad_st$2,                     /* Prod./Scan Status code     */~
            ad_time$8,                   /* Time Stamp                 */~
            ad_id$3,                     /* User ID of Scanner         */~
            cnt$30                       /* Screen Counter             */

        dim                              /* FILE = APCPLNUD            */~
            ud_rec$64,                   /* Record Format              */~
            ud_key$48,                   /* Primary Key                */~
            ud_bar$18,                   /* Production Bar Code        */~
            ud_dte$6,                    /* Scanning Date              */~
            ud_dept$3,                   /* Scanning Dept Code         */~
            ud_proc$2,                   /* Scanning Process Code      */~
            ud_shift$2,                  /* Production Scanning Shift  */~
            ud_st$2,                     /* Prod./Scan Status code     */~
            ud_time$8,                   /* Time Stamp                 */~
            ud_id$3,                     /* User ID of Scanner         */~
            ud_so$8,                     /* Sales Order Number         */~
            ud_line$2,                   /* SO Line Item               */~
            ud_load$5                    /* Scheduled Load Number      */

        dim                              /* FILE = APCPLNLD            */~
            ld_load$5,                   /* Load Number                */~
            ld_desc$30                   /* Load No. Desc              */


        dim                              /* FILE = APCPLNSC            */~
            sc_rec$128,                  /* Record Format              */~
            sc_key$33,                   /* Primary key                */~
            sc_cust$9,                   /* Customer Code              */~
            sc_load$5                    /* Sched load Number          */

        dim                              /* FILE = GENCODES            */~
            ge_key$24

        dim                              /* Screen Variables           */~
            bg_dte$10, bg_dte1$10,       /* Begin Scan Date            */~
            ed_dte$10, ed_dte1$10,       /* End Scan Date              */~
            bg_load$5,                   /* Begin Load Number          */~
            ed_load$5,                   /* End Load Number            */~
            bg_desc$30,                  /* Begin Load No. Desc        */~
            ed_desc$30,                  /* End Load No. Desc          */~
            blankdate$8                  /* Blank Unformated Date      */

        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
            rslt$(15%)20                 /* Text from file opening     */

        dim                                                              ~
            hdr$40,                      /* ASKUSER HEADER             */~
            msg$(3%)79,                  /* ASKUSER TEXT               */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$25, pname$21
            apc$   = "APCPLNAD Update Utility"
            pname$ = "EWDPLN76 - Rev: R7.00"

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
            * #1  ! APCPLNAD ! Scanning Audit File                      *~
            * #2  ! APCPLNUD ! Production Audit Update File             *~
            * #3  ! APCPLNSC ! Planning Master Schedule-Old APCLINES    *~
            * #4  ! GENCODES ! Master System Table File                 *~
            * #5  ! APCPLNLD ! Planning Load Master - Old APCMAST       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "APCPLNAD",                                     ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =   19, keylen =   33,                    ~
                        alt key  1, keypos =    1, keylen =  33

            select #2,   "APCPLNUD",                                     ~
                        varc,     indexed,  recsize = 112,               ~
                        keypos =   35, keylen =   48,                    ~
                        alt key  1, keypos =    1, keylen =  49

            select #3, "APCPLNSC",                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   24, keylen =   10,                    ~
                        alt key  1, keypos =    7, keylen =  27,         ~
                            key  2, keypos =    1, keylen =  33

            select #4, "GENCODES",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #5, "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =   128,            ~
                        keypos =   11, keylen =  5,                     ~
                        alt key  1, keypos =    3, keylen =  13,        ~
                            key  2, keypos =    1, keylen =  15


            call "SHOSTAT" ("Opening Files, One Moment Please")

            filename$ = "APCPLNAD" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error

            call "OPENCHCK" (#2, fs%(2%), f2%(2%), 500%, rslt$(2%))

            filename$ = "APCPLNSC" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "APCPLNLD" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error


            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "DATUFMTC" (blankdate$)
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
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
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
                  if keyhit%  = 16% then gosub dataload
                  if keyhit% <>  0% then       editpg1
L11140:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 4% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11190:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11190
                  lastfieldnr% = fieldnr%
            goto L11140

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
         "Enter a Valid Beginning Scann Date or (A)ll.                 ",~
         "Enter a Valid Ending Scan Date or (A)ll.                     ",~
         "Enter a Valid Beginning Load Number or (A)ll.                ",~
         "Enter a Valid Ending Load Number or (A)ll.                   "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, ad_rec$, ad_key$, ad_bar$,  ~
                      ad_dte$, ad_dept$, ad_proc$, ad_shift$, ad_st$,     ~
                      ad_time$, ad_id$, ud_rec$, ud_key$, ud_bar$,        ~
                      ud_dte$, ud_dept$, ud_proc$, ud_shift$, ud_st$,     ~
                      ud_time$, ud_id$, ud_so$, ud_line$, ud_load$,       ~
                      bg_dte$, ed_dte$, bg_load$, ed_load$, bg_desc$,     ~
                      ed_desc$, ld_load$, bg_dte1$, ed_dte1$, ld_desc$
        return

        REM *************************************************************~
            *        L O A D   D A T A   F R O M  F I L E               *~
            *-----------------------------------------------------------*~
            *Loads data from File Record Area into Program Variables    *~
            *************************************************************
       dataload                                  /* (APCPLNAD) - File  */
           ad_key$ = all(hex(00))
           if str(bg_dte$,1%,3%) = "ALL" then goto L30060
           str(ad_key$,1%,6%) = bg_dte$
L30060:    cnt% = 0%
           cnt$ = "RECORDS COUNTED [XXXXXXXX]"
       read_ad_next
           read #1, key > ad_key$, using L30000, ad_key$, eod goto read_ad_done

L30000:    FMT POS(19), CH(33)
         
           get #1, using L30010, ad_bar$,  /* Production Bar Code */~
                  ad_dte$,                /* Scanning Date       */~
                  ad_dept$,               /* Scanning Dept       */~
                  ad_proc$,               /* Scanning Process    */~
                  ad_shift$,              /* Production Shift    */~
                  ad_st$,                 /* Production Status   */~
                  ad_time$,               /* Scanned Time        */~
                  ad_id$                  /* User ID of Scanner  */

           cnt% = cnt% + 1%
           if mod(cnt%,100%) <> 0 then goto L30050
              convert cnt% to str(cnt$,18%,8%), pic(########)
              print at(02,33%);hex(84);cnt$;

L30050:    if str(ad_dte$,1%,6%) > str(ed_dte$,1%,6%) then goto read_ad_done 

           gosub get_customer
           if str(sc_cust$,1%,5%) = "BA111" then goto L30070
           if ad_st$ <> "16" and ad_st$ <> "12" then goto read_ad_next
L30070:
           gosub check_dept

           if ck_dept% = 0% then goto read_ad_next

           if str(bg_dte$,1%,1%) = "A" then goto check_load
              if str(ad_dte$,1%,6%) >= str(bg_dte$,1%,6%) and      ~
                 str(ad_dte$,1%,6%) <= str(ed_dte$,1%,6%) then     ~
                                              goto check_load
           goto read_ad_next
       read_ad_done
       return clear all
       goto inputmode

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode
        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Update Store Data and Part Data                           *~
            *************************************************************

        dataput                               /* (APCPLNUD)-File     */
           init(" ") ud_key$
           str(ud_key$,1%,8%)   = str(ad_bar$,1%,8%)
           str(ud_key$,9%,2%)   = str(ad_bar$,9%,2%)
           str(ud_key$,11%,5%)  = sc_load$
           str(ud_key$,16%,18%) = ad_bar$
           str(ud_key$,34%,6%)  = ad_dte$
           str(ud_key$,40%,3%)  = ad_dept$
           str(ud_key$,43%,2%)  = ad_proc$
           str(ud_key$,45%,2%)  = ad_shift$
           str(ud_key$,47%,2%)  = ad_st$
          
           ud_so$   = str(ad_bar$,1%,8%)
           ud_line$ = str(ad_bar$,9%,2%)

        read #2, hold, key 1% = ud_key$, eod goto L28120
                 delete #2

L28120: put #2, using  L35120, ud_so$,    /* Sales Order            */~
                ud_line$,                 /* SO Line Item           */~
                sc_load$,                 /* Sched load Number      */~
                ad_bar$,                  /* Bar Code               */~
                ad_dte$,                  /* Scanning Date          */~
                ad_dept$,                 /* Scanning Dept Code     */~
                ad_proc$,                 /* Scanning Proc. Code    */~
                ad_shift$,                /* Prod. Scanned Shift Cde*/~
                ad_st$,                   /* Prod/Scan Shift Code   */~
                ud_so$,                   /* Sales Order            */~
                ud_line$,                 /* So Line Item           */~
                sc_load$,                 /* Sched load Number      */~
                ad_bar$,                  /* Bar Code               */~
                ad_time$,                 /* Scanned Time           */~
                ad_id$                    /* User ID of Scan        */

         write #2, eod gosub write_error

        dataput_done
        goto read_ad_next

        write_error
            errormsg$ = "(Write Error) - APCPLNUD = " & ud_key$
            gosub error_prompt
            err% = 0%
        return
        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                          /* (APCPLNAD) - FILE          */
L30010:     FMT CH(18),                   /* Production Bar Code */~
                CH(06),                   /* Scanning Date       */~
                CH(03),                   /* Scanning Dept       */~
                CH(02),                   /* Scanning Process    */~
                CH(02),                   /* Production Shift    */~
                CH(02),                   /* Production Status   */~
                XX(18), CH(08),           /* Scanned Time        */~
                CH(03)                    /* User ID of Scanner  */

L35120:    FMT  CH(08),                   /*  Sales Order           */~
                CH(02),                   /* SO Line Item           */~
                CH(05),                   /* Sched load Number      */~
                CH(18),                   /* Bar Code               */~
                CH(06),                   /* Scanning Date          */~
                CH(03),                   /* Scanning Dept Code     */~
                CH(02),                   /* Scanning Proc. Code    */~
                CH(02),                   /* Prod. Scanned Shift Cde*/~
                CH(02),                   /* Prod/Scan Shift Code   */~
                CH(08),                   /* Sales Order            */~
                CH(02),                   /* So Line Item           */~
                CH(05),                   /* Sched load Number      */~
                CH(18),                   /* Bar Code               */~
                CH(08),                   /* Scanned Time           */~
                CH(03)                    /* User ID of Scan        */

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
              on fieldnr% gosub L40090,         /* Begin  Scan Date  */   ~
                                L40090,         /* End Scan Date     */   ~
                                L40090,         /* Begin  Load No.   */   ~
                                L40090          /* End    Load No.   */

              goto L40105

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40090:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40105:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,35), fac(hex(a4)), apc$                   , ch(25),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (04,02), "Begining Scaning Date :",                    ~
               at (04,30), fac(lfac$(1%)), bg_dte1$             , ch(10),~
                                                                         ~
               at (05,02), "Ending Scaning Date   :",                    ~
               at (05,30), fac(lfac$(2%)), ed_dte1$             , ch(10),~
                                                                         ~
               at (07,02), "Begining Load Number  :",                    ~
               at (07,30), fac(lfac$(3%)), bg_load$             , ch(05),~
               at (07,40), fac(hex(84)), bg_desc$               , ch(30),~
                                                                         ~
               at (08,02), "Begining Load Number  :",                    ~
               at (08,30), fac(lfac$(4%)), ed_load$             , ch(05),~
               at (08,40), fac(hex(84)), ed_desc$               , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L40340
                  call "PRNTSCRN"
                  goto L40105

L40340:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40435     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40415
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40415:     if fieldnr% > 1% then L40425
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40425:     return

L40435: if fieldnr% > 0% then L40480  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                       (16)Update File "
            pfkeys$ = hex(01ffffffffffffffffffffffffffff1000)
            return
L40480:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50000,         /* Beginning Scan Date   */ ~
                              L50100,         /* Ending Scan Date      */ ~
                              L50200,         /* Begining Load Number  */ ~
                              L50300          /* Ending Load Number    */

            return

L50000: REM Beginning Date
           if str(bg_dte1$,1%,1%) = "A" then goto L50010
           if str(bg_dte1$,1%,8%) <> " " then goto L50020
L50010:       bg_dte1$, ed_dte1$, bg_dte$, ed_dte$ = "ALL"
           return
L50020:    call "DATEOKC" (bg_dte1$, date%, errormsg$)
            if date% = 0% then goto L50090
                              
            bg_dte$ = bg_dte1$
            call "DATUFMTC" (bg_dte$)
         return
L50090:  errormsg$ = "(Error) - Invalid Beginning Date?"
         gosub error_prompt
         bg_dte1$, ed_dte1$ = blankdate$
         bg_dte$, ed_dte$ = blankdate$
        return

L50100: REM Ending Date
        if str(ed_dte1$,1%,1%) = "A" then goto L50010

        if str(ed_dte1$,1%,8%) <> " " then goto L50110
               ed_dte1$ = bg_dte$

L50110: call "DATEOKC" (ed_dte1$, date%, errormsg$)
         if date% = 0% then goto L50190
            ed_dte$ = ed_dte1$
            call "DATUFMTC" (ed_dte$)

            if ed_dte$ >= bg_dte$ then return

L50190:     errormsg$ = "(Error) - Invalid Ending Date?"
            gosub error_prompt
            bg_dte1$, ed_dte1$ = blankdate$
            bg_dte$, ed_dte$ = blankdate$
        return

L50200: REM Begining Load Number                    BG_LOAD$
            if str(bg_load$,1%,1%) = "A" then goto L50220
            if bg_load$ <> " " then goto L50210
L50220:        bg_load$, ed_load$ = "ALL  "
            return
L50210:     bg_load% = 0%
                                               /*  (EWD002)  -  BEG  */
               convert bg_load$ to bg_load%, data goto L50280

               convert bg_load% to bg_load$, pic(00000)
                 goto L50270
L50280:        convert str(bg_load$,2%,4%) to bg_load%, data goto L50290

               convert bg_load% to str(bg_load$,2%,4%), pic(0000)
L50270:                                        /*  (EWD002)  -  END  */
            ld_load$ = bg_load$            
            gosub lookup_load
            if apc% = 0% then goto L50290
            bg_desc$ = ld_desc$
        return
L50290:     errormsg$ = "(Error) - Invalid Load Number?"
            gosub error_prompt
            init(" ") bg_load$, ld_load$
        return

L50300: REM Ending Load Number                      ED_LOAD$
            if str(ed_load$,1%,1%) = "A" then goto L50220
            if ed_load$ <> " " then goto L50310
               goto L50220
L50310:     ed_load% = 0%
                                               /*  (EWD002)  -  BEG  */
               convert ed_load$ to ed_load%, data goto L50380

               convert ed_load% to ed_load$, pic(00000)
                   goto L50370
L50380:        convert str(ed_load$,2%,4%) to ed_load%, data goto L50390

               convert ed_load% to str(ed_load$,2%,4%), pic(0000)
L50370:                                         /*  (EWD002)  -  END  */
            ld_load$ = ed_load$            
            gosub lookup_load
            if apc% = 0% then goto L50390
            ed_desc$ = ld_desc$
            if ed_load$ >= bg_load$ then return

L50390:     errormsg$ = "(Error) - Invalid Load Number?"
            gosub error_prompt
            init(" ") ed_load$, ld_load$
        return

        REM *************************************************************~
            *           S p e c i a l   S u b r o u t i n e s           *~
            *************************************************************
        check_load
           init(" ") sc_key$, sc_load$
           str(sc_key$,1%,10%) = str(ad_bar$,1%,10%)
           read #3, key = sc_key$, using L50850, sc_load$,     ~
                                   eod goto check_load_done
L50850:    FMT POS(7), CH(5)

           if str(bg_load$,1%,1%) = "A" then goto dataput
              if str(sc_load$,1%,5%) >= str(bg_load$,1%,5%) and      ~
                 str(sc_load$,1%,5%) <= str(ed_load$,1%,5%) then     ~
                                              goto dataput
              
        check_load_done
        goto read_ad_next

        check_dept
           init(" ") ge_key$
           ck_dept% = 0%
           str(ge_key$,1%,9%) = "PLAN DEPT"
           str(ge_key$,10%,3%) = ad_dept$
           read #4, key = ge_key$, eod goto check_dept_done
           
           ck_dept% = 1%
        check_dept_done
           if ad_dept$ = "044" or ad_dept$ = "001" then ck_dept% = 1%
/*(AWD003) */
           if ad_dept$ = "054" or ad_dept$ = "074" then ck_dept% = 1%
        return

        lookup_load                          /* (APCPLNLD) - Load File */
            apc% = 0%
            read #5,key = ld_load$, using L50860, ld_desc$,     ~
                                    eod goto lookup_load_done
L50860:       FMT POS(16), CH(30)

            apc% = 1%
        lookup_load_done
        return

        open_error                                                            
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
            err% = 0%
        return

        error_prompt
           comp% = 2%
           hdr$ = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        get_customer
           init(" ") sc_key$, sc_cust$
           str(sc_key$,1%,10%) = str(ad_bar$,1%,10%)
           read #3, key = sc_key$, using L50870, sc_cust$,     ~
                                   eod goto check_cust_done
L50870:    FMT POS(59), CH(9)
        check_cust_done
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
