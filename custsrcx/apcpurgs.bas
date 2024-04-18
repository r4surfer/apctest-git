        REM *************************************************************~
            * Note - Special Version Only Deletes Live Data. No History *~
            *                                                           *~
            *   Note - Only valid for Data Up to and including 1996     *~
            *   AAA   PPPP    CCC   PPPP   U   U  RRRR    GGG    SSSS   *~
            *  A   A  P   P  C   C  P   P  U   U  R   R  G   G  S       *~
            *  AAAAA  PPPP   C      PPPP   U   U  RRRR   G        S     *~
            *  A   A  P      C   C  P      U   U  R  R   G  GG     S    *~
            *  A   A  P       CCC   P       UUU   R   R   GGG   SSS     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCPURGS - Purge all Sales Orders after a Specified Date. *~
            *            Note - Uses the 'Pause' Routine for Delayed    *~
            *                   Starting.                               *~
            *                                                           *~
            *          - MOVE BCKMASTR TO HISTORY ( BCKMSTHS )          *~
            *          - MOVE BCKLINES TO HISTORY ( BCKLINHS )          *~
            *          - MOVE TXTFILE  TO HISTORY ( TXTHIST  )          *~
            *          - MOVE APCORDER TO HISTORY ( APCORDHS )          *~
            *          - MOVE APCLINES TO HISTORY ( APCLINHS )          *~
            *          - MOVE APCMAST  TO HISTORY ( APCMSTHS )          *~
            *          - MOVE BCKBUFFR TO HISTORY ( BCKBFFHS )          *~
            *          - MOVE BCKBUF2  TO HISTORY ( BCKBF2HS )          *~
            *                                                           *~
            *          - USE HISTORY VERSION OF BCKFASTR ( BCKHIST  )   *~
            *            TO LOOKUP ORDERS.                              *~
            *                                                           *~
            *          - USE HISTORY VERSION OF APCORDER ( APCORDHS )   *~
            *            TO CHECK THE STATUS OF AN ORDER.               *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/15/93 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 01/30/96 ! Mod to clean-up for the New HP Computer  ! RHH *~
            *          !   System. Add Security check when run.   !     *~
            *          !                                          !     *~
            * 09/23/97 ! Mod for Addition Check before creating   ! RHH *~
            *          !   '95' and '96' History. Special note    !     *~
            *          !   last time old planning files used.     !     *~
            * 11/13/97 ! Mod for upgrade to Release R6.04.03      ! RHH *~
            *          !                                          !     *~
            * 02/02/98 ! Special Version No History Created       ! RHH *~
            *          !   Purge "96" Data.                       !     *~
            *************************************************************

        dim                                                              ~
            hdr$38, msg$(3%)79,          /* Askuser Text Var's         */~
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

        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */

        dim                              /* PROGRAM VARIABLES          */~
            process$1, process_d$25,     /* Purge Process Code/Descr   */~
            master$25,                   /* BCKMASTR PRIMARY KEY       */~
            textid$4,                    /* S.O. HEADER TEXT ID        */~
            m_dte$6, txtid$4,            /* DATE S.O. ENTERED          */~
            bck_rec$(4%)250,             /* BCKMASTR RECORD            */~
            lines$19,                    /* BCKLINES MASTER KEY        */~
            ln_rec$(2%)150,              /* BCKLINES RECORD            */~
            so$16,                       /* SALES ORDER NUMBER         */~
            purge_date$8,                /* FORMATED PURGE DATE        */~
            purge_dte$6,                 /* UNFORMATED PURGE DATE      */~
            x$8, prog$8,                 /* DUMMY DATE                 */~
            text_rec$(3%)70,             /* TEXT RECORD BUFFER         */~
            text_key$11,                 /* TEXT KEY                   */~
            text$64,                     /* TEXT DEFINITION AREA       */~
            sav_txt$9,                   /* CHECK TEXT KEY             */~
            apc_ord$110,                 /* GROUP S.O. RECORD          */~
            apc_load$5,                  /* LOAD NUMBER                */~
            hdr_key$19,                  /* LINES KEY                  */~
            hdr_rec$128,                 /* LOAD S.O. LINES RECORD     */~
            apc_rec$256                  /* APCMAST RECORD             */

        dim                              /* PROGRAM VARIABLES          */~
            bck1$8,                      /* S.O. SCANNED               */~
            bck2$8,                      /* S.O. MOVED                 */~
            ln$8,                        /* S.O. LINE ITEMS MOVED      */~
            txt$8,                       /* S.O. TEXT REC'S MOVED      */~
            grp$8,                       /* GROUP HEADER REC'S MOVED   */~
            apc_ln$8,                    /* GROUP LINES MOVED          */~
            load$8                       /* APC LOADS MOVED            */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Purge S.O. and Planning to History"
            pname$ = "APCPURGS - Rev: R6.04"

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
            * #1  ! BCKMASTR ! MASTER S.O. HEADER FILE                  *~
            * #2  ! BCKLINES ! MASTER S.O. LINE ITEM FILE               *~
            * #3  ! TXTFILE  ! MASTER TEXT FILE                         *~
            * #4  ! APCORDER ! GROUP S.O. HEADER FILE                   *~
            * #5  ! APCLINES ! LOAD S.O. LINE ITEM FILE                 *~
            * #6  ! APCMAST  ! LOAD MASTER FILE                         *~
            ******!**********!*******************************************~
            * #7  ! BCKMSTHS ! MASTER S.O. HEADER FILE (HISTORY)        *~
            * #8  ! BCKLINHS ! MASTER S.O. LINE ITEM FILE (HISTORY)     *~
            * #9  ! TXTHIST  ! MASTER TEXT FILE (HISTORY)               *~
            * #10 ! APCORDHS ! GROUP S.O. HEADER FILE                   *~
            * #11 ! APCLINHS ! LOAD S.O. LINE ITEM FILE                 *~
            * #12 ! APCMSTHS ! LOAD MASTER FILE                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #2,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #3,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

            select #4,   "APCORDER",                                     ~
                        varc,     indexed,  recsize = 110,               ~
                        keypos =    1, keylen =   28,                    ~
                        alt key  1, keypos =   12, keylen =  17,         ~
                            key  2, keypos =   29, keylen =  19, dup,    ~
                            key  3, keypos =   62, keylen =  16, dup,    ~
                            key  4, keypos =   78, keylen =   8, dup,    ~
                            key  5, keypos =   86, keylen =   8, dup

            select #5,  "APCLINES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   11,                    ~
                        alt key  1, keypos =   12, keylen =  19,         ~
                            key  2, keypos =   42, keylen =  36, dup

            select #6,  "APCMAST",                                       ~
                        varc,     indexed,  recsize =   256,             ~
                        keypos =    1, keylen =  5,                      ~
                        alt key  1, keypos =  233, keylen =  13

            select #7,  "BCKMSTHS",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #8,  "BCKLINHS",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #9,  "TXTHIST",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

            select #10,  "APCORDHS",                                     ~
                        varc,     indexed,  recsize = 110,               ~
                        keypos =    1, keylen =   28,                    ~
                        alt key  1, keypos =   12, keylen =  17,         ~
                            key  2, keypos =   29, keylen =  19, dup,    ~
                            key  3, keypos =   62, keylen =  16, dup,    ~
                            key  4, keypos =   78, keylen =   8, dup,    ~
                            key  5, keypos =   86, keylen =   8, dup

            select #11, "APCLINHS",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   11,                    ~
                        alt key  1, keypos =   12, keylen =  19,         ~
                            key  2, keypos =   42, keylen =  36, dup

            select #12, "APCMSTHS",                                      ~
                        varc,     indexed,  recsize =   256,             ~
                        keypos =    1, keylen =  5,                      ~
                        alt key  1, keypos =  233, keylen =  13

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
            gosub check_access
            if userid$ <> "RHH" and userid$ <> "DJD" then                ~
                                                        goto exit_program
            if comp% <> 0% then goto exit_program

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
                  if keyhit%  = 14% then gosub process_data
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
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
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        process_data
            gosub purge_data
        return clear all
        goto exit_program

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
         "Enter a Purge Date ( Purge all S.O. Prior To Specified Date )",~
         "Enter Process Code. ( 0=ALL, 1=S.O. Only, 2=Planning Only )  "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, purge_date$, purge_dte$,   ~
                      x$, so$, lines$, process$, process_d$

        return

        REM *************************************************************~
            *************************************************************

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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        REM DATALOAD
        REM RETURN

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

              on fieldnr% gosub L40180,         /* S.O. Purge Date    */  ~
                                L40180          /* Process Code       */

              goto L40210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Purge To History Date (Inclusive):",         ~
               at (06,38), fac(lfac$(1%)), purge_date$          , ch(08),~
                                                                         ~
               at (07,02), "Process Code. ( 0, 1, or 2 )     :",         ~
               at (07,38), fac(lfac$(2%)), process$             , ch(01),~
               at (07,50), fac(hex(84)), process_d$             , ch(25),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L40460
                  call "PRNTSCRN"
                  goto L40210

L40460:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Move Data   "
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *                  S T A T I S T I C S                      *~
            *************************************************************

        display_stats
              init(" ") bck1$, bck2$, ln$, txt$, grp$, apc_ln$, load$,   ~
                        runtime$
              cnt% = 0%
              convert bck1% to bck1$    , pic(########)
              convert bck2% to bck2$    , pic(########)
              convert ln%   to ln$      , pic(########)
              convert txt_cnt% to txt$  , pic(########)
              convert grp%  to grp$     , pic(########)
              convert apc_ln% to apc_ln$, pic(########)
              convert load% to load$    , pic(########)
              call "TIME" (runtime$)

            display                                                      ~
               at (01,30),                                               ~
                  " S T A T I S T I C S ",                               ~
                                                                         ~
               at (02,26), hex(84), process_d$,                          ~
                                                                         ~
               at (04,02), hex(84), runtime$,                            ~
                                                                         ~
               at (04,30), "Purge Date:",                                ~
               at (04,42), hex(84), purge_date$,                         ~
                                                                         ~
               at (04,56), "Today:",                                     ~
               at (04,63), hex(84), date$,                               ~
                                                                         ~
               at (06,02), "Current Customer    :",                      ~
               at (06,25), hex(84), str(master$,1%,9%),                  ~
                                                                         ~
               at (06,40), "Current S. O. :",                            ~
               at (06,56), hex(84), str(master$,10%,16%),                ~
                                                                         ~
               at (08,02), "Sales Orders Scanned:",                      ~
               at (08,25), hex(84), bck1$,                               ~
                                                                         ~
               at (10,02), "Sales Orders Moved  :",                      ~
               at (10,25), hex(84), bck2$,                               ~
                                                                         ~
               at (12,02), "S.O. Lines Moved    :",                      ~
               at (12,25), hex(84), ln$,                                 ~
                                                                         ~
               at (14,02), "Text Records Moved  :",                      ~
               at (14,25), hex(84), txt$,                                ~
                                                                         ~
               at (16,02), "Group S.O. Moved    :",                      ~
               at (16,25), hex(84), grp$,                                ~
                                                                         ~
               at (18,02), "Group Lines Moved   :",                      ~
               at (18,25), hex(84), apc_ln$,                             ~
                                                                         ~
               at (20,02), "APC Loads Moved     :",                      ~
               at (20,25), hex(84), load$

          call "PAUSE" addr(200%)

        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50120,         /* Purge S.O. Date       */ ~
                              L50310          /* Process Code - 0,1,2  */
            return

L50120: REM Move to History Date                 PURGE_DATE$, PURGE_DTE$
            date% = 0%
            if purge_date$ <> " " then goto L50160
               goto L50240
L50160:     call "DATEOK" (purge_date$, date%, errormsg$)
            if date% = 0% then return                  /* Invalid Date */
            if str(purge_date$,7%,2%) >= str(date$,7%,2%) then           ~
                                                              goto L50270
               x$ = purge_date$
               call "DATUNFMT" (x$)
               purge_dte$ = str(x$,1%,6%)    /* Unformatted Purge Date */
        return
L50240:     errormsg$ ="(Error)-Must Enter a Valid History 'Purge Date'?"
            init(" ") purge_date$, purge_dte$, x$
        return
L50270:     errormsg$ ="(Error)-Cannot Move S.O. from the Current Year"
            init(" ") purge_date$, purge_dte$, x$
        return

L50310: REM Process Code                         PROCESS$
            process% = -1%
            if process$ <> " " then goto L50350
               process$ = "0"
L50350:     p% = pos("012" = process$)
            if p% = 0% then goto L50440
               convert process$ to process%, data goto L50380
L50380:
          if process% = 0% then process_d$ = "Purge S.O. and Planning. "
          if process% = 1% then process_d$ = "Purge S.O. Data Only.    "
          if process% = 2% then process_d$ = "Purge Planning Data Only."
          if process% < 0% or process% > 2% then goto L50440
        return
L50440:     errormsg$ = "(Error) - Invalid Processing Code. (0, 1, 2)? "
            init(" ") process$, process_d$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        purge_data
              prog$ = "APCPURGS"
              call "APCPAUSE" (apc%, prog$)     /* SET START TIME/DATE */
              if apc% = 1% then goto exit_program
                 gosub open_files               /* NOW OPEN ALL FILES  */

           bck1% = 1%
           cnt%     = 0% : keyhit% = 0% : bck2%   = 0% : ln%  = 0%
           txt_cnt% = 0% : grp%    = 0% : apc_ln% = 0% : load% = 0%
           init(" ") master$
        m_next                                  /* (BCKMASTR) - FILE   */
           init(" ") bck_rec$()
           if cnt% < 999% then goto L60180
              gosub display_stats
L60180:    if process% = 2% then goto L60220     /* PURGE PLANNING ONLY */
              read #1,hold,key > master$, using L60230, bck_rec$(),       ~
                                                       eod goto m_done
              goto L60240
L60220:    read #1,key > master$, using L60230, bck_rec$(),eod goto m_done
L60230:       FMT 4*CH(250)
L60240:    master$   = str(bck_rec$(),1%,25%)   /* Set Customer and    */
           bck1% = bck1% + 1%                   /* Sales Order.        */
           cnt% = cnt% + 1%
           m_dte$    = str(bck_rec$(),830%,6%)  /* Test Against the    */
                                                /* Date S.O. Entered   */
           if m_dte$ > purge_dte$ then goto m_next
              if process% = 2% then goto l_done  /* Planning Data Only */
        REM      DELETE #1                       /* Set for 96 History */
                 delete #1                       /* (BCKMASTR) - Delete*/
                                                 /* Load Header Text Id*/
                 txtid$ = str(bck_rec$(),799%,4%)
                 gosub move_txt                  /* Move Header Text  */

                 bck2% = bck2% + 1%              /* Create Header Hist*/
        REM      WRITE #7, USING 60230, BCK_REC$() /*(BCKMSTHS) - File*/

                                        /* Check and Move S.O. Lines  */
           so$ = str(master$,10%,16%)
           init(" ") lines$
           str(lines$,1%,16%) = so$
        l_next                                   /* (BCKLINES) - File  */
           read #2,hold,key > lines$, using L60470, ln_rec$(),            ~
                                                   eod goto l_done
L60470:        FMT 2*CH(150)
           lines$    = str(ln_rec$(),10%,19%)    /* Load S.O. Ln Item  */
           if str(lines$,1%,16%) <> so$ then goto l_done
        REM   DELETE #2                          /* Set for 96 History */
              delete #2                          /* (BCKLINES) - Delete*/
              txtid$ = str(ln_rec$(),242%,4%)    /* Set Ln Item Text Id*/
              gosub move_txt                     /* Move Ln Item Text  */
              ln% = ln% + 1%
        REM   WRITE #8, USING 60470, LN_REC$()   /* (BCKLINHS) - File  */
           goto l_next
        l_done
           if process% = 1% then goto m_next     /* S.O. Data Only    */
              gosub move_apcorder                /* Group S.O. Header */
              gosub move_apcmast                 /* Load Master Record*/
              gosub move_apclines                /* Load S.O. Lines   */
           goto m_next

        m_done
           gosub display_stats
           stop
        return

        move_apcorder                             /* (APCORDER) - File */
        return

            init(" ") apc_load$, apc_ord$
            read #4,hold,key 1% = str(master$,1%,17%), using L60750,      ~
                                           apc_ord$, eod goto L60800
L60750:         FMT CH(110)
            delete #4
            grp% = grp% + 1%                      /* (APCORDHS) - File */
        REM WRITE #10, USING 60730, APC_ORD$, EOD GOTO 60790
            apc_load$ = str(apc_ord$,48%,5%)
L60800: return
            if process% = 2% then return
            errormsg$ =  "(Err - 02)-Updating Customer - " &             ~
                                     str(master$,1%,9%) & "  S.O. - " &  ~
                                     str(master$,10%,16%)
            call "SHOSTAT" (errormsg$) : stop
        return

        move_apclines                             /* (APCLINES) - File */
        return

            init(" ") hdr_key$
            str(hdr_key$,1%,17%) = str(master$,1%,17%)
        apclines_next
            init(" ") hdr_rec$
            read #5,hold,key 1% > hdr_key$, using L60970, hdr_rec$,       ~
                                                  eod goto apclines_done
L60970:         FMT CH(128)
            if str(hdr_rec$,12%,17%) <> str(master$,1%,17%) then return
               hdr_key$ = str(hdr_rec$,12%,19%)
               delete #5
               apc_ln% = apc_ln% + 1%             /* (APCLINHS) - File */
        REM    WRITE #11, USING 60930, HDR_REC$, EOD GOTO 61020
               goto apclines_next
        apclines_done
        return
            errormsg$ =  "(Err - 03)-Updating Customer - " &             ~
                                     str(master$,1%,9%) & "  S.O. - " &  ~
                                     str(master$,10%,16%)
            call "SHOSTAT" (errormsg$) : stop
        return

        move_apcmast                              /* (APCMAST ) - File */
        return

            init(" ") apc_rec$
            read #6,key = apc_load$, eod goto L61240
                                       /* Load and Lock when it Exists */
            read #6,hold,key = apc_load$, using L61200, apc_rec$,         ~
                                                       eod goto L61240
L61200:         FMT CH(256)
            delete #6
            load% = load% + 1%                    /* (APCMSTHS) - File */
        REM WRITE #12, USING 61140, APC_REC$, EOD GOTO 61190
L61240: return
            errormsg$ = "(Err - 04)-Updating Customer - " &              ~
                                     str(master$,1%,9%) & "  S.O. - " &  ~
                                     str(master$,10%,16%)
            call "SHOSTAT" (errormsg$) : stop
        return

        open_files
            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),  0%, rslt$(6%))

            call "OPENCHCK" (#7, fs%(7%), f2%(7%),10000%, rslt$(7%))
            call "OPENCHCK" (#8, fs%(8%), f2%(8%),10000%, rslt$(8%))
            call "OPENCHCK" (#9, fs%(9%), f2%(9%),10000%, rslt$(9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 1000%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 1000%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%), 1000%, rslt$(12%))

            mat f1% = zer
        return

        deffn'099(textid$)
            txt% = 0%
            if textid$ = hex(00000000) or textid$ = hex(ffffffff)        ~
                                           or textid$ = " " then return
            txt% = 1%
        return

        move_txt                              /* To History Text       */
            init(" ") text_key$, sav_txt$, text_rec$(), text$
            gosub'099(txtid$)
            if txt% = 0% then return

            text_key$ = all(hex(00))
            str(text_key$,1%,1%) = "M"
            str(text_key$,2%,3%) = "   "
            str(text_key$,5%,4%) = txtid$
            str(text_key$,9%,1%) = "1"
            sav_txt$ = str(text_key$,1%,9%)
        move_txt_next
            read #3,hold,key > text_key$, using L61710, text_key$,        ~
                                                   eod goto move_txt_done
L61710:        FMT CH(11)
            if sav_txt$ <> str(text_key$,1%,9%) then goto move_txt_done
               get #3, using L61740, text$, text_rec$()
L61740:           FMT CH(64), 3*CH(70)
        REM    DELETE #3                         /* Set for 96 History */
               delete #3

        REM    WRITE #9, USING 61680 , TEXT$, TEXT_REC$(), EOD GOTO 61770
               txt_cnt% = txt_cnt% + 1%
               goto move_txt_next
        move_txt_done
        return
            call "SHOSTAT" ("(Error)-Moving 'TEXT' to History?") : stop
            goto move_txt_next

        check_access
            yr% = 0%
            call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$)
            convert str(date$,7%,2%) to yr%, data goto L61910
L61910:
        REM YR% = YR% - 1%                      /* Set for 96 History */
            yr% = yr% - 2%
            hdr$ = " Create Sales History for (XX)"
            convert yr% to str(hdr$,28%,2%), pic(##)

            comp% = 2%
            msg$(1%)= "****** Special Systems Maintenance Utility ******"
            msg$(2%)= "P u r g e  S a l e s  D a t a  T o  H i s t o r y"
            msg$(3%)= "Press <RETURN> To Continue, Any PF() Key to Exit?"
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")
        end
