        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC    CCC   U    U  TTTTT   SSSSS   CCC  *~
            *  A   A  P   P  C   C  C   C  U    U    T    S       C   C *~
            *  AAAAA  PPPP   C      C      U    U    T      S     C     *~
            *  A   A  P      C   C  C   C  U    U    T        S   C   C *~
            *  A   A  P       CCC    CCC    UUUU     T   SSSSS     CCC  *~
            *                                                           *~
            *            ( M E M O R Y   V E R S I O N )                *~
            *                                                           *~
            * NOTE - ( UPDATE_WORK_OLD ) ROUTINE FOR DISK VERSION       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCCUTSC - Display Screen for MFG Product Saw Cuts        *~
            *                                                           *~
            *      Routines - (APCCUTLD) - Loads all Equation References*~
            *                              for a Specified Product Line *~
            *                                                           *~
            *                 (APCCUTCC) - Calculate all Width and      *~
            *                              Height Cuts                  *~
            *                                                           *~
            *      Table   - (EQUATIONS) - <Prod>-<Eq Type>-<Eq No.>    *~
            *                              ie. 6-1-01                   *~
            *                                                           *~
            *      File    -  (APCCUTEQ) - Saw Equation Cross Reference *~
            *                              File                         *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/15/92 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 02/19/93 ! Mod Add Load Number to Report Selection  ! RHH *~
            *          ! Criteria. (SCR_LOAD$)                    !     *~
            * 08/01/93 ! Mods for Costing, Change the (APCCUTLD)  ! RHH *~
            *          ! and (APCCUTCC) for Costing Cuts Misc/Grid!     *~
            * 03/30/95 ! Mods to Allow for User Work File. Thus   ! RHH *~
            *          !   Allow Multiple Users to run.           !     *~
            *          !                                          !     *~
            * 11/06/97 ! Change revision number to 60403          ! DJD *~
            *          !                                          !     *~
            * 04/01/98 ! Y2K modifications                        ! ERN *~
            *05/19/2014! (CUT001) mod to add dim fields to CUTCC  ! CMG *~
            *************************************************************

        dim                                                              ~
            apc_scr$120, mode$5,         /* Screen Description         */~
            apc_prt$60,                  /* Print Description          */~
            apc_sze$20,                  /* Size Description           */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim                                                              ~
            sa_part$25,                  /* MFG Part Number            */~
            sa_part_desc$30,             /* MFG Part Number Description*/~
            sa_mfg$1,                    /* '0'= MFG Only, or '1'=Parts*/~
            sa_mfg_desc$30,              /* MFG Description            */~
            readkey$24,                  /* Generic Key                */~
            desc$30,                     /* Generic Description        */~
            pd$1,                        /* Product Line               */~
            ct(100%),                    /* Cut Size - DECIMAL         */~
            ct$(100%)9,                  /* Cut Size - WITH FRACTION   */~
            cr$(100%)10,                 /* Cut Raw Material           */~
            cp$(100%)2,                  /* Cut Number of Pieces       */~
            cc$(100%)1,                  /* Linealmate Cut (Y) or (N)  */~
            co$(100%)25,                 /* CUT DESCRIPTIONS           */~
            eq$(100%)8,                  /* Equation Ref Key(s)        */~
            sh$(100%)1,                  /* SASH TYPE ( W, H, N )      */~
            hdr$(2%)60,                  /* CUT HEADER TEXT            */~
            tw$1,                        /* WIDTH PARTS TYPE CODE      */~
            th$1,                        /* HEIGHT PARTS TYPE CODE     */~
            txt$(20)60                   /* CUT DESCRIPTIONS           */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        dim                              /*(APCPIECE) AND REPORT       */~
            ext$(300%)10,                /* EXTRUSIONS ANALYIZED       */~
            units(300%),                 /* TOT UNIT FOR EXTRUSION     */~
            lngth$(300%)5,               /* EXTRUSION LENGTH AND TYPE  */~
            dtl_part$25,                 /* DETAIL MFG PART NUMBER     */~
            sav_part$25,                 /* DETAIL MFG PART NUMBER     */~
            dtl_key1$57,                 /* DETAIL ALTERNATE KEY       */~
            scr_prod$1,                  /* PRODUCT LINE               */~
            scr_dte1$10,                 /* BEGINNING PROD DATE        */~
            scr_dte2$10,                 /* ENDING PROD DATE           */~
            scr_load$5,                  /* Load Number                */~
            scr_desc$30,                 /* Load Description           */~
            dtl_rec$256,                 /* APCPLNDT RECORD            */~
            dte$6,                       /* PRODUCTION DATE            */~
            dte1$6,                      /* BEG PROD DATE              */~
            dte2$6,                      /* END PROD DATE              */~
            c1$(100%)8,                  /* PRIMARY KEY                */~
            c2$(100%)10,                 /* RAW MATERIAL PART NUMBER   */~
            c3(100%),                    /* CUT SIZE DECIMAL           */~
            sa_key$8,                    /* SAW ALT KEY (1)            */~
            wrk_rec$14                   /* WOTK RECORD                */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/06/97 Saw Display Scr and Material Rp"
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
            * #01 ! APCCUTEQ ! Saw Optimization Cross-Reference File    *~
            * #02 ! GENCODES ! Master Code Table File                   *~
            * #03 ! HNYMASTR ! Part Master File                         *~
            * #04 ! AMTBOMCD ! Master Equation File                     *~
            * #05 ! AMTBOMIF ! Master Part Validity File                *~
            * #6  ! APCPLNDT ! APC MASTER DETAIL FILE (APCPIECE) OLD    *~
            * #07 ! HNYQUAN  ! INVENTORY QUANTITIES MASTER              *~
            * #09 ! APCCUTWW ! SORT WORK FILE FOR PRODUCTION MATERIAL   *~
            * #10 ! APCPLNLD ! Load Master File (APCMAST) - OLD         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCCUTEQ",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    2, keylen =   7,                     ~
                        alt key  1, keypos  =     1, keylen =  8

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =  24

            select #3,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1,   keylen =  25,                     ~
                        alt key  1, keypos  =   102, keylen =  9, dup,   ~
                            key  2, keypos  =    90, keylen =  4, dup,   ~
                            key  3, keypos  =    26, keylen = 32, dup

            select #4,  "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42

            select #5,  "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32                      ~

            select #6,   "APCPLNDT",                                     ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen = 57,          ~
                            key  2, keypos  =  53, keylen = 51,          ~
                            key  3, keypos  =   1, keylen = 23, dup,     ~
                            key  4, keypos  =  96, keylen =  8, dup

            select #7,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =   650,             ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #9,   "APCCUTWW",                                     ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos =    1, keylen =   10

            select #10, "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =   11, keylen =   5,                     ~
                        alt key  1, keypos =    3, keylen =  13,         ~
                            key  2, keypos =    1, keylen =  15


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),500%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),  0%, rslt$(6%))
            call "OPENCHCK" (#7, fs%(7%), f2%(7%),  0%, rslt$(7%))
            call "OPENCHCK" (#10, fs%(10), f2%(10),  0%, rslt$(10))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATFMTC" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  1%
L02150:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L02280
L02170:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L02250
L02200:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L02170
                         if fieldnr% = 1% then L02150
                         goto L02200
L02250:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% = 10% then gosub generate_report
                      if keyhit% <> 0% then       L02170
L02280:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L02170
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
                  if keyhit%  = 10% then gosub generate_report
                  if keyhit%  = 14% then gosub calc_cuts
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L02460:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 1% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L02510:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L02510
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L02510
                  lastfieldnr% = fieldnr%
            goto L02460

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        generate_report
           init(" ") scr_dte1$, scr_dte2$, scr_prod$, scrap$

L02680:    gosub'102(1%, 1%)
           if keyhit% = 1% then gosub startover
           if keyhit% = 16% then goto exit_program
              date% = 0%
              call "DATEOKC" (scr_dte1$, date%, errormsg$)
              if date% = 0% then goto L02680

              call "DATEOKC" (scr_dte2$, date%, errormsg$)
              if date% = 0% then goto L02680

              convert scr_prod$ to scr_prod%, data goto L02680

              if scr_prod% > 7% then goto L02680

              convert scrap$ to scrap, data goto L02680

              convert scrap to scrap$, pic(###.##-)

              if scr_load$ <> " " then goto L02890
                 scr_load$ = "ALL  "
                 scr_desc$ = "(ALL) - Loads "
L02890:       if str(scr_load$,1%,3%) = "ALL" then goto L02970
                 read #10,key = scr_load$, using  L02920, scr_desc$,       ~
                                           eod goto L02940
L02920:             FMT POS(16), CH(30)
                 goto L02970
L02940:          errormsg$ = "(Error) - Invalid Load Selection?"
                 goto L02680

L02970:       if keyhit% = 14% then goto L03000
              goto L02680

L03000:   call "DATUFMTC" (scr_dte1$)
          call "DATUFMTC" (scr_dte2$)

          gosub rpt_1

          gosub delete_work
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
            if fieldnr% <> 0% then L03300
                inpmessage$ = edtmessage$
                return

L03300
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Valid Manufactured Product Part Number, Min. = 19 Dg."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sa_part$, sa_part_desc$,   ~
                      sa_mfg$, sa_mfg_desc$, pd$, co$(), ct$(), cr$(),   ~
                      cp$(), cc$(), eq$(), readkey$, desc$, hdr$(),      ~
                      txt$(), apc_scr$, apc_prt$, apc_sze$, sa_key$,     ~
                      sh$(), scr_load$, scr_desc$, axd$

            hdr% = 1%
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
              on fieldnr% gosub L04080           /* Part Number       */

              goto L04100

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L04080:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L04100:     accept                                                       ~
               at (01,02),                                               ~
                  "Saw Cuts For Manufactured Parts (Exploded Out)",      ~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,15), fac(hex(84)), apc_prt$               , ch(60),~
               at (04,02), "Part Number:",                               ~
               at (04,15), fac(lfac$(1%)), sa_part$             , ch(25),~
               at (04,45), fac(hex(84)), sa_part_desc$          , ch(30),~
               at (06,10), fac(hex(84)), hdr$(hdr%)             , ch(60),~
               at (07,10), fac(hex(84)), txt$( 1%)              , ch(60),~
               at (08,10), fac(hex(84)), txt$( 2%)              , ch(60),~
               at (09,10), fac(hex(84)), txt$( 3%)              , ch(60),~
               at (10,10), fac(hex(84)), txt$( 4%)              , ch(60),~
               at (11,10), fac(hex(84)), txt$( 5%)              , ch(60),~
               at (12,10), fac(hex(84)), txt$( 6%)              , ch(60),~
               at (13,10), fac(hex(84)), txt$( 7%)              , ch(60),~
               at (14,10), fac(hex(84)), txt$( 8%)              , ch(60),~
               at (15,10), fac(hex(84)), txt$( 9%)              , ch(60),~
               at (16,10), fac(hex(84)), txt$(10%)              , ch(60),~
               at (17,10), fac(hex(84)), txt$(11%)              , ch(60),~
               at (18,10), fac(hex(84)), txt$(12%)              , ch(60),~
               at (19,10), fac(hex(84)), txt$(13%)              , ch(60),~
               at (20,10), fac(hex(84)), txt$(14%)              , ch(60),~
               at (21,10), fac(hex(84)), txt$(15%)              , ch(60),~
                                                                         ~
               at (22,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 3% then goto L04530
                  if hdr% = 1% then goto L04490
                     hdr% = 1%
                     gosub build_screen
                     goto L04100
L04490:           hdr% = 2%
                  gosub build_screen
                  goto L04100

L04530:        if keyhit% <> 15% then goto L04570
                  call "PRNTSCRN"
                  goto L04100

L04570:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L04760      /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &       ~
                      "(10)Reports            (15)Print Screen"
            pf$(2%) = "(3)Toggle(W/H)   (14)Calc Cuts          " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ff0304ffffffffff0affffff0e0f1000)
            if fieldnr% = 1% then L04700
                str(pf$(2%),64%)   = " " : str(pfkeys$,16%,1%) = hex(ff)
L04700:     if fieldnr% > 1% then L04720
                str(pf$(1%),18%,20%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L04720:     str(pf$(2),1% ,15%)=" "  :  str(pfkeys$, 3%,1%) = hex(ff)
            str(pf$(2),18%,20%)=" "  :  str(pfkeys$,14%,1%) = hex(ff)
            return

L04760: if fieldnr% > 0% then L04830   /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (15)Print Screen"
            pf$(2%) = "(3)Toggle(W/H)   (14)Calc Cuts          " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ff03ffffffffffffffffffff0e0f1000)
            return
L04830:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *               R E P O R T   S C R E E N                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L05060           /* Entries           */

              goto L05090

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L05060:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L05090:     accept                                                       ~
               at (01,02),                                               ~
                  "Materials Requirements Reporting.",                   ~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (04,02), "Beginning Production Date: ",                ~
               at (04,30), fac(hex(81)), scr_dte1$              , ch(10),~
               at (05,02), "Ending Production Date   : ",                ~
               at (05,30), fac(hex(81)), scr_dte2$              , ch(10),~
               at (06,02), "Reporting Product Line   : ",                ~
               at (06,30), fac(hex(81)), scr_prod$              , ch(01),~
               at (07,02), "Scrap Percentage (XX.XX%): ",                ~
               at (07,30), fac(hex(81)), scrap$                 , ch(07),~
               at (08,02), "Load Number or (ALL)     : ",                ~
               at (08,30), fac(hex(81)), scr_load$              , ch(05),~
               at (08,40), fac(hex(84)), scr_desc$              , ch(30),~
                                                                         ~
               at (22,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(2)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L05390
                  call "PRNTSCRN"
                  goto L05090

L05390:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
            inpmessage$ = "Enter the Applicable Data. "
            pf$(1%) = "(1)Start Over    (14)Print Report       " &       ~
                      "                       (15)Print Screen"
            pf$(2%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L05640           /* Part Number           */

            return

L05640: REM MFG Part Number                       SA_PART$

          if sa_part$ <> " " then goto L05720
             sa_part_desc$ = " "
             sa_part_desc$ = hex(06) & "Select a MFG Part Number"
             call "GETCODE" (#3,sa_part$, sa_part_desc$, 0%, 1.32,f1%(3))
             if f1%(3) = 0% then goto L05770

L05720:      gosub lookup_description
             if err% <> 0% then goto L05770
                pd$ = str(sa_part$,1%,1%)
                sa_part_desc$ = apc_sze$
        return
L05770:     errormsg$ = "(Error) - Invalid Part Number."
            sa_part$, sa_part_desc$, pd$ = " "
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        lookup_description
           err% = 0%
           call "APCDESCR" (sa_part$, apc_scr$, apc_prt$, apc_sze$, #5,  ~
                                                                  err% )
        return

        calc_cuts
           call "SHOSTAT" ("Check Saw Equation References")
           tw$ = "1" : th$ = "2"
           call "APCCUTLD" (pd$, cw%, ch%, tw$, th$, #2, err% )
           if err% = 0% then goto L06030
              errormsg$ =                                                ~
                  "(Error) - Saw Cuts not Defined in (EQUATIONS) Table."
              return
L06030:    sa_mfg% = 0%                            /* MFG WINDOWS ONLY */
           call "SHOSTAT" ("Calculating Saw Cuts for Part")
           call "APCCUTCC" ( sa_part$, 0%, 0%, 0%, /* (CUT001) */        ~
               sa_mfg%, cw%, ch%, eq$(), ct$(), cr$(), cp$(), cc$(),     ~
               co$(), ct(), sh$(), tw$, th$, #1, #4, #2, err%)
               
           gosub build_screen
           if kk% <> 0% then return
              errormsg$ =                                                ~
                  "(Error) - No Cut References Defined for Model/Color."
        return

        build_screen
        REM (1) - 8%,21%  (2) - 32%,10%  (3) - 45%,2%  (4) - 50%,8%

            init(" ") hdr$(), txt$()
            hdr$(1%) =                                                   ~
           "Width <-----Description-----> <-Raw Mat.-> <Qt> <--Size-->Ct"

            hdr$(2%) =                                                   ~
           "Height<-----Description-----> <-Raw Mat.-> <Qt> <--Size-->Ct"

                                                             /* WIDTH  */
           if hdr% = 2% then goto L06420
            kk% = 0%
            for i% = 1% to cw%
                if cc$(i%) = " " then goto L06400
                   kk% = kk% + 1%
                   str(txt$(kk%),2%,4%) = "(00)"
                   convert i% to str(txt$(kk%),3%,2%), pic(00)

                   str(txt$(kk%),8%,21%)  = str(co$(i%),1%,21%)
                   gosub check_obsolete

                   str(txt$(kk%),32%,10%) = cr$(i%)
                   str(txt$(kk%),45%,2%)  = cp$(i%)
                   str(txt$(kk%),50%,9%)  = ct$(i%)
                   str(txt$(kk%),60%,1%)  = cc$(i%)
L06400:     next i%
        return
L06420:     j% = cw%                                         /* HEIGHT */
            kk% = 0%
            for i% = 1% to ch%
                if cc$(j% + i%) = " " then goto L06570
                   kk% = kk% + 1%
                   str(txt$(kk%),2%,4%) = "(00)"
                   convert i% to str(txt$(kk%),3%,2%), pic(00)

                   str(txt$(kk%),8%,21%)  = str(co$(j% + i%),1%,21%)
                   gosub check_obsolete

                   str(txt$(kk%),32%,10%) = cr$(j% + i%)
                   str(txt$(kk%),45%,2%)  = cp$(j% + i%)
                   str(txt$(kk%),50%,9%)  = ct$(j% + i%)
                   str(txt$(kk%),60%,1%)  = cc$(j% + i%)
L06570:     next i%
        return


        set_alternate_key
            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work

            k_max%, cw%, ch% = 0%
            tw$ = "1" : th$ = "2"
            call "APCCUTLD" (scr_prod$, cw%, ch%, tw$, th$, #2, err% )
            init(" ") sav_part$, ext$(), lngth$()
            mat units = zer
            dte1$ = str(scr_dte1$,1%,6%)
            dte2$ = str(scr_dte2$,1%,6%)
            dtl_key1$ = all(hex(00))
            str(dtl_key1$,1%,6%) = dte1$                /* BEG PROD DTE*/
        return

        rpt_1                                             /* MATERIALS */
            call "SHOSTAT" ("** Analyzing Production Data **")
            gosub set_alternate_key
            read #6,key 1% > dtl_key1$, using L06850 , dtl_key1$,          ~
                                                       eod goto scan_done
            goto L06860
        scan_next
            read #6, using L06850 , dtl_key1$, eod goto scan_done
L06850:        FMT POS(47), CH(57)
L06860:     dte$ = str(dtl_key1$,1%,6%)
            if dte$ < dte1$ or dte$ > dte2$ then goto scan_done
            get #6, using L06872 , dtl_rec$
L06872:         FMT CH(256)
            if scr_prod$ <> str(dtl_rec$,189%,1%) then goto scan_next
            if str(scr_load$,1%,3%) = "ALL" then goto L06930
               if scr_load$ <> str(dtl_rec$,1%,5%) then goto scan_next

L06930:     dtl_part$ = str(dtl_rec$,189%,25%)
            if len(dtl_part$) < 19 then goto scan_next
               gosub update_work
               goto scan_next
        scan_done

            call "APCCUTMM" (scr_dte1$, scr_dte2$, scr_prod$, scrap,     ~
                          k_max%, ext$(), units(), lngth$(), #2, #3, #7 )
        return

        update_work
          if sav_part$ = dtl_part$ then goto L07060
             sav_part$ = dtl_part$
             gosub get_cuts                              /* FOR REPORT */
L07060:   if j% = 0% then return
          for i% = 1% to j%
              if k_max% = 0% then goto L07140
              for k% = 1% to k_max%
                  if c2$(i%) <> ext$(k%) then goto L07130
                     units(k%) = units(k%) + c3(i%)
                     goto L07210
L07130:       next k%
L07140:       k_max% = k_max% + 1%
              if k_max% > 300% then goto L07260
              read #1,key 1% = c1$(i%), using L07180 , lngth$(k_max%),     ~
                                                            eod goto L07230
L07180:           FMT POS(19), CH(5)                 /* LENGTH AND I/F */
              ext$(k_max%)  = c2$(i%)
              units(k_max%) = c3(i%)
L07210:   next i%
        return
L07230:   call "SHOSTAT" ("(ERROR) READING CROSS REF. ----> "& c1$(i%))
          stop
          goto L07210

L07260:   call "SHOSTAT" ("(ERROR) - MEMORY EXCEEDED (300), CALL 'ROY'?")
          stop
          goto exit_program



        get_cuts                                /* FOR WIDTH AND HEIGHT */
          init(" ") c1$(), c2$()
          j% = 0% : eq% = cw% + ch% : hdr% = 0%
          call "APCCUTCC" (dtl_part$, 0%, 0%, 0%, /* (CUT001) */         ~ 
                      0%, cw%, ch%, eq$(), ct$(), cr$(),                 ~
                      cp$(), cc$(), co$(), ct(), sh$(), tw$, th$,        ~
                                                        #1, #4, #2, err%)
          for i% = 1% to eq%
              if cc$(i%) = " " then goto L07480   /* Skip - No Cross-Ref */
                 j% = j% + 1%                   /*  Defined for Part   */
                 p% = 1%
                 convert cp$(i%) to p%, data goto L07420
L07420:
                 c1$(j%) = eq$(i%)              /* PRIMARY KEY         */
                 gosub check_obsolete           /* REPLACE OBSOLETE    */

                 c2$(j%) = cr$(i%)              /* RAW MATERIAL NUMBER */
                 c3(j%)  = ( ct(i%) * p% )      /* CUT DEC AND PIECES  */
L07480:   next i%
        return

        check_obsolete
          readkey$ = " "
          str(readkey$,1%,9%)   = "APCMATOBS"
          str(readkey$,10%,15%) = cr$(i%)
          if hdr% = 2% then str(readkey$,10%,15%) = cr$(j% + i%)

          read #2,key = readkey$, using L07580, desc$, eod goto L07610
L07580:      FMT POS(25), CH(30)
          cr$(i%) = str(desc$,1%,10%)
          if hdr% = 2% then cr$(j% + i%) = str(desc$,1%,10%)
L07610: return

        update_work_old
          if sav_part$ = dtl_part$ then goto L07680
             sav_part$ = dtl_part$
             gosub get_cuts                              /* FOR REPORT */

L07680:   if j% = 0% then return
          for i% = 1% to j%
              read #9,hold,key = c2$(i%), using L07720 , units,            ~
                                                           eod goto L07780
L07720:          FMT POS(11), PD(14,4)
              units = units + c3(i%)
              put #9, using L07720 , units
              rewrite #9
              goto L07850

L07780:     init(" ") wrk_rec$
            read #1,key 1% = c1$(i%), using L07810 , str(wrk_rec$,1%,5%),  ~
                                                            eod goto L07890
L07810:         FMT POS(19), CH(5)                   /* LENGTH AND I/F */
            put #9, using L07830 , c2$(i%), c3(i%), wrk_rec$
L07830:        FMT CH(10), PD(14,4), CH(14)
            write #9, eod goto L07870
L07850:   next i%
        return
L07870:   call "SHOSTAT" ("(ERROR) UPDATING WORK --> " & c2$(i%))
          stop
          goto L07850
L07890:   call "SHOSTAT" ("(ERROR) READING CROSS-REF ---> " & c1$(i%))
          stop
          goto L07850

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#9,mode$, 500%, f2%)
            if f2% <> 0% then goto L08000
        return
L08000:     call "SHOSTAT" ("ERROR - CANNOT OPEN (APCCUTWW)") : stop
        return
        delete_work
            call "FILEBGON" (#9)
        return


        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
