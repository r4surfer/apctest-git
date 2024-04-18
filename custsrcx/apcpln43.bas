        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLN43                             *~
            *  Creation Date     - 09/25/96                             *~
            *  Last Modified Date- 09/08/99                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Consolidated Cut Sheet for Wegoma    *~
            *                      Saws, and Flat Files for Transfer to *~
            *                      the Plant Network                    *~
            *                                                           *~
            *  Code Tables Used  - (PLAN SHFT) - Shift Codes            *~
            *                      (PLAN DEPT) - Department Codes       *~
            *                      (PLAN PROC) - Planning Process Codes *~
            *                                                           *~
            *  Special Comments  - Subroutine (CHECK_PROFILE) has the   *~
            *                      Equations for the Adjustments to     *~
            *                      Cottage, Oriel, and CLRM             *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/03/95 ! Modification for Passing Data to New     ! RHH *~
            *          ! Saw Optimization Software. Using Routines!     *~
            *          ! ( APCCUTLD - ?00 Series Load Cut Definiti!     *~
            *          ! ( APCCUTCC - ?00 Calculate Cuts for Wind !     *~
            *          ! ( APCCT7SB - (Old) Now Done Internal.    !     *~
            *          !                  Same as (APCCUTSB)      !     *~
            *          ! ( APCPL43A - 700 Series Cut Sheet Wegoma !     *~
            *          !                  Same as (APCCUT7B)      !     *~
            *          ! ( APCWEGOM ) - Special Work File for     ! RHH *~
            *          !                Wegoma Saws.              !     *~
            * 09/26/96 ! Mods to Swith the Old (APCWEGOM) to the  ! RHH *~
            *          !   New Planning System.                   !     *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 09/07/99 ! (EWD001) Mod to change to dept 036 only  ! RHH *~
            *          !   all Products.                          !     *~
            * 04/01/03 ! (EWD002) Mod to change to dept 025 and   ! CMG *~
            *          !   026 as well as 036                     !     *~
            *05/19/2014! (CUT001) mod to add dim fields to CUTCC  ! CMG *~
            *************************************************************

        dim                              /* FILE = AMTBOMCD            */~
            scr_nam$(3%)5, stack$5,      /* Saw File Names (1),(2),(3) */~
            scr_siz$(3%)3, qty$3,        /* Saw Batch Sizes (1),(2),(3)*/~
            srt$5, model$3,              /* Wegoma Sort Code           */~
            mm$2, days$9,                /* Product Line Code of Part  */~
            partno$25

        dim                              /* FILE = APCPLNDT            */~
            save_part$25,                /* Part Number                */~
            dt_rec$256,                  /* Detail Record              */~
            dt_key1$57,                  /* Alt - Production Date      */~
            dt_load$5,                   /* Load Number                */~
            dt_txt$4,                    /* Text ID                    */~
            dt_sort$5,                   /* Production or Stock Sort   */~
            dt_ref$8,                    /* Prod/Pull Reference Number */~
            dt_shft$2,                   /* Shift Code                 */~
            dt_seq$5,                    /* Sequence No.               */~
            dt_dept$3,                   /* Department Code            */~
            dt_sash$1,                   /* Sash Code 0,1,2,3          */~
            dt_part$25                   /* Part Number                */

        dim                              /* File - (APCWEGOM)          */~
            wrk_key$5, wrk$3,            /* Primary Key                */~
            wrk_key1$30,                 /* Alt Key (1)                */~
            wrk_rec$165,                 /* Work Record                */~
            wd$7, s_width$10,            /* Actual Width , and DEC     */~
            ht$6, s_height$10, s_clmr$10 /* Actual Height, and DEC     */

        dim                              /* File-(APCWEGOM) 712 Series */~
            c_o$2,                       /* Cottage/Oriel              */~
            t_t$4,                       /* Twin/Triple                */~
            t_b$3,                       /* TSO, BSO, FSO              */~
            locks$3,                     /* Number of Locks            */~
            fin$3                        /* With Fin (Yes or No)       */

        dim                              /* (Program Variables)        */~
            hdr$40,                      /* Askuser                    */~
            msg$(3%)79,                  /* Askuser                    */~
            sqq$5,                       /* Sequence Number,HIGH TO LOW*/~
            spec$4,                      /* SCREEN - BSO,TSO,FGO       */~
            hnge$20, hng$4,              /* Description of Hinge       */~
            sze$30,                      /* Save Eights                */~
            sz$100,                      /* Save Sixteenths            */~
            opt$1,                       /* Selection (1) Rpt (2) Opt  */~
            opt_desc$30,                 /* Selection Description      */~
            readkey$30,                  /* GENCODES Look-Up Key       */~
            sav_key$50,                  /* Save Key Value             */~
            prod_dte$8,                  /* Screen Completion Date FORM*/~
            scr_dte$8,                   /* Screen PRODUCTION Date FORM*/~
            scr_dte1$8,                  /* Screen PROD. Date Unform   */~
            scr_code$1, scr_shft$2,      /* Screen Report Selection    */~
            scr_prod$1,                  /* Screen Product Line or All */~
            scr_msg$30,                  /* Screen - Report Selection  */~
            scr_msg1$30,                 /* Screen - Product Line      */~
            descr$30,                    /* Use for GENCODES Look-Up   */~
            fld$(11%)4,                  /* Save (11) Field Values     */~
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

        dim f2%(20%),                    /* = 0 if the file is open    */~
            f1%(20%),                    /* = 1 if READ was successful */~
            fs%(20%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(20%)20                 /* Text from file opening     */

        dim desc$32,                     /* GENCODES Description      */ ~
            name$(5%)8,                  /* NEW BATCH FILE NAME       */ ~
            col$(100%)25,                /* Cut Description           */ ~
            eq$(100%)8,                  /* Equation Codes            */ ~
            ct$(100%)9,                  /* Cut Widths and Heights    */ ~
            ct(100%),                    /* Cut Wid/Height Decimal    */ ~
            sh$(100%)1,                  /* Sash Type                 */ ~
            cr$(100%)10,                 /* Raw Material Part Number  */ ~
            cp$(100%)2,                  /* Number of Pieces to Cut   */ ~
            cc$(100%)1,                  /* Cut Piece Yes or No       */ ~
            tw$1,                        /* WIDTH CUT PARTS           */ ~
            th$1                         /* HEIGHT CUT PARTS          */

        dim cl$(15%)2,                   /* COLOR CODES                */~
            sav_key1$12,                 /* PROFILE KEY COPY           */~
            ph$(10%)7, pw$(10%)7,        /* HEIGHT/WIDTH PROFILES      */~
            phr$(10%)10, pwr$(10%)10,    /* HEIGHT/WIDTH RAW MAT'L NO'S*/~
            phs$(10%)2, pws$(10%)2,      /* HEIGHT/WIDTH SAW NUMBERS   */~
            phd$(10%)10,pwd$(10%)10,     /* HEIGHT/WIDTH DESCRIPTION   */~
            php$(10%)2,pwp$(10%)2,       /* HEIGHT/WIDTH PIECES TO CUT */~
            phe$(10%)10,pwe$(10%)10,     /* HEIGHT/WIDTH DESCRIPTION   */~
            phc(10%),pwc(10%),           /* HEIGHT/WIDTH CUT LENGTH DEC*/~
            ofil$(5%)8,                  /* STORE FILE NAMES OF SAWS   */~
            olib$(5%)8,                  /* STORE LIBRARY NAMES        */~
            ovol$(5%)6%,                 /* STORE VOLUME NAMES         */~
            saw$1, bb$2, bb%(5%),        /* SAW NUMBER                 */~
            bcnt%(5%), cnt%(5%),         /* BATCH COUNTERS             */~
            saw_rec$48,                  /* SAW DATA RECORD            */~
            order$10,                    /* CUSTOMER ORDER NUMBER      */~
            prf$5,                       /* PROFILE CODE               */~
            c$2,                         /* ALPHA COLOR CODE           */~
            sss$4,                       /* PRODUCT SEQ. NUMBER        */~
            lll$5                        /* RAW MATERIAL CUT LENGTH    */

        dim sav_name$8, status$2,        /* Used for Report Batch Break*/~
            xtime$8, saw_flags$5,        /* Time for Report            */~
            weg_tit$40,                  /* Batch Report Title         */~
            xdate$8,                     /* Report Date                */~
            prf_mat$10,                  /* Raw Material Number        */~
            err$10                       /* Error Coment               */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(712) Department Wegoma Saw Cut Sheets "
            pname$ = "APCPLN43 - Rev: R6.04"

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
            * #1  ! APCWEGOM ! Work File For Wegoma 700 Series          *~
            * #2  ! APCPLNDT ! (New) Prod Master Detail (APCPIECE)      *~
            * #3  ! GENCODES ! Master System Table File                 *~
            * #6  ! AMTBOMIF ! Master EQUATION FILE                     *~
            * #7  ! HNYMASTR ! MASTER INVENTORY FILE                    *~
            * #9  ! TEXTFILE ! MASTER TEXT FILE                         *~
            * #11 ! APCCUTEQ ! SAW CROSS REF FILE                       *~
            * #12 ! AMTBOMCD ! MASTER EQUATION FILE                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCWEGOM",                                      ~
                        varc,     indexed,  recsize =   200,             ~
                        keypos =    1, keylen =    5,                    ~
                        alt key  1, keypos =    6, keylen = 30

            select #2,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen =  57,         ~
                            key  2, keypos =   53, keylen =  51,         ~
                            key  3, keypos =    1, keylen =  23, dup,    ~
                            key  4, keypos =   96, keylen =   8, dup

            select #3, "GENCODES",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #6,  "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32                      ~


            select #7,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #9,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   1, keylen =  11

            select #11, "APCCUTEQ",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    2, keylen =   7,                     ~
                        alt key  1, keypos  =     1, keylen =  8

            select #12, "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42

            select #15, "@WEGOM1@",                                      ~
                                consec , recsize = 48
            select #16, "@WEGOM2@",                                      ~
                                consec , recsize = 48
            select #17, "@WEGOM3@",                                      ~
                                consec , recsize = 48

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2,  fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3,  fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#6,  fs%(6%), f2%(6%),  0%, rslt$(6%))
            call "OPENCHCK" (#7,  fs%(7%), f2%(7%),  0%, rslt$(7%))
            call "OPENCHCK" (#9,  fs%(9%), f2%(9%),  0%, rslt$(9%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 0%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%), 0%, rslt$(12%))
            mat f1% = zer

            if fs%(1) = -1% then goto L02820
               gosub file_in_use
               goto exit_program

L02820:     if fs%(1) <> 0 then call "FILEBGON" (#1)

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

        REM - NEAREST 8TH INCH
           sze$ = "1/81/43/81/25/83/47/8         "

        REM - NEAREST 16TH OF AN INCH
           sz$ = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
        ~ 3/4 13/16 7/8 15/16     "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  6%
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
                  if keyhit%  = 16% then gosub begin_process
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 6% then editpg1
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
            call "SHOSTAT" ("Creating "& scr_msg$)
            gosub rpt_1

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
         "Enter the Production and Completion Date for Cut Sheets?     ",~
         "Enter the Applicable Shift Code 1,2,3 or (AA) = All?         ",~
         "(1)Rpt,(2)Wegoma Saws?                                       ",~
         "Enter A Valid Batch Name and Size for Saw (1)? ie MON01 100  ",~
         "Enter A Valid Batch Name and Size for Saw (2)? ie MON01 025  ",~
         "Enter A Valid Batch Name and Size for Saw (3)? ie MON01 025  "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, scr_dte$, scr_dte1$,       ~
                      prod_dte$, prod_dte1$, scr_code$, scr_msg$,        ~
                      scr_shft$, scr_msg1$, opt$, opt_desc$, scr_nam$(), ~
                      scr_siz$(), days$

            stack% = 0%
            stack$ = "00000"
            slice% = 200%
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

                                       /* APCPLNDT - Production Detail */
L35050:     FMT CH(256)                /*            Master File       */

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
              on fieldnr% gosub L40210,         /* Prod/Comp Date    */   ~
                                L40210,         /* Production Shift  */   ~
                                L40220,         /* Options 1,2       */   ~
                                L40210,         /* Saw (1) Name/Size */   ~
                                L40210,         /* Saw (2) Name/Size */   ~
                                L40210          /* Saw (3) Name/Size */
              goto L40240

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40210:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40220:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40240:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Cut Sheet Production Date :",                ~
               at (04,30), fac(lfac$(1%)), scr_dte$             , ch(08),~
               at (04,40), "Completion Date :",                          ~
               at (04,60), fac(lfac$(1%)), prod_dte$            , ch(08),~
                                                                         ~
               at (05,02), "Production Shift Code     :",                ~
               at (05,30), fac(lfac$(2%)), scr_shft$            , ch(02),~
               at (05,40), fac(hex(84)), scr_msg1$              , ch(30),~
                                                                         ~
               at (06,02), "(1)Report, (2)Wegoma      :",                ~
               at (06,30), fac(lfac$(3%)), opt$                 , ch(01),~
               at (06,40), fac(hex(84)), opt_desc$              , ch(30),~
                                                                         ~
               at (07,02), "Saw (1) Batch Name        :",                ~
               at (07,30), fac(lfac$(4%)), scr_nam$(1%)         , ch(05),~
               at (07,40), "Size:",                                      ~
               at (07,46), fac(lfac$(4%)), scr_siz$(1%)         , ch(03),~
                                                                         ~
               at (08,02), "Saw (2) Batch Name        :",                ~
               at (08,30), fac(lfac$(5%)), scr_nam$(2%)         , ch(05),~
               at (08,40), "Size:",                                      ~
               at (08,46), fac(lfac$(5%)), scr_siz$(2%)         , ch(03),~
                                                                         ~
               at (09,02), "Saw (3) Batch Name        :",                ~
               at (09,30), fac(lfac$(6%)), scr_nam$(3%)         , ch(05),~
               at (09,40), "Size:",                                      ~
               at (09,46), fac(lfac$(6%)), scr_siz$(3%)         , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L40690
                  call "PRNTSCRN"
                  goto L40240

L40690:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40880     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40840
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L40840:     if fieldnr% > 1% then L40860
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40860:     return

L40880: if fieldnr% > 0% then L40970  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Print Data  "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return
L40970:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
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
            on fieldnr% gosub L50180,         /* Prod/Comp Dates       */ ~
                              L50640,         /* Production Shift Code */ ~
                              L50810,         /* 1-Rpt,2-Opt           */ ~
                              L51030,         /* Saw (1) Name and Size */ ~
                              L51190,         /* Saw (2) Name and Size */ ~
                              L51350          /* Saw (3) Name and Size */

            return

L50180: REM Production/ Completion Date           SCR_DTE$, SCR_DTE1$
           date% = 0%
           call "DATEOK" (scr_dte$, date%, errormsg$ )
           if errormsg$ <> " " then goto L50400
              scr_dte1$ = scr_dte$            /* Check (APCPLNDT) for */
              call "DATUNFMT" (scr_dte1$)     /* Assoc. Data.         */
              dt_key1$ = all(hex(00))
              str(dt_key1$,1%,6%) = str(scr_dte1$,1%,6%)
              read #2,key 1% > dt_key1$,using L35050,dt_rec$,             ~
                                                           eod goto L50400
              if str(dt_rec$,47%,6%) <> str(scr_dte1$,1%,6%) then        ~
                                                        goto L50400
              call "DATE" addr("GD",str(scr_dte1$,1%,6%),days$, date%)
        REM Cut Completion Date                   PROD_DTE$,PROD_DTE1$
           if prod_dte$ <> " " then goto L50340
              prod_dte$ = scr_dte$                /* Equals Production */
L50340:    date% = 0%
           call "DATEOK" (prod_dte$, date%, errormsg$ )
           if errormsg$ <> " " then goto L50400
              prod_dte1$ = prod_dte$
              call "DATUNFMT" (prod_dte1$)
        return
L50400:    errormsg$ = "(Error) - Invalid Prodution/Completion Date?"
           gosub error_prompt
           init(" ") scr_dte$, scr_dte1$, prod_dte$, prod_dte1$
        return

L50640: REM Production Shift Selection
            if scr_shft$ <> " " then goto L50690
L50660:        scr_shft$ = "AA"
               scr_msg1$ = "(AA) = All Shifts"
               return
L50690:     if str(scr_shft$,1%,1%) = "A" then goto L50660
            init(" ") readkey$
            str(readkey$,1%,9%)   = "PLAN SHFT"
            str(readkey$,10%,15%) = scr_shft$
            read #3,key = readkey$, using L50740, scr_msg1$,eod goto L50760
L50740:        FMT POS(25), CH(30)
        return
L50760:     errormsg$ = "(Error) - Invalid Shift Code?"
            gosub error_prompt
            init(" ") scr_shft$, scr_msg1$
        return

L50810: REM Option Selection            /* (1) = Print Report Only     */
                                        /* (2) = Create Saw Optimation */
                                        /*       File For Processing   */
            if opt$ <> " " then goto L50870
               opt$ = "1"

L50870:     if opt$ <> "1" and opt$ <> "2" then goto L50980
            init(" ") opt_desc$, errormsg$
            if opt$ = "1" then opt_desc$ = "(1) Print Applic. Reports"
            if opt$ = "2" then opt_desc$ = "(2) Build Wegoma Saw file"
            if opt$ = "2" then return
               for i% = 1% to 3%
                   scr_nam$(i%) = "N/A00"
                   scr_siz$(i%) = "000"
               next i%
               fieldnr% = 7%
        return
L50980:     errormsg$ = "(Error) - Invalid Option Selection, 1 or 2 ?"
            gosub error_prompt
            init(" ") opt$, opt_desc$
        return

L51030: REM Name ans Size for Saw (1)              SCR_NAM$(),SCR_SIZ$()
           if scr_nam$(1%) <> " " then goto L51070
              scr_nam$(1%) = str(days$,1%,3%) & "00"
              scr_siz$(1%) = "050"
L51070:    if len(scr_nam$(1%)) <> 5 then goto L51140

           convert scr_siz$(1%) to x%,data goto L51140

           convert x% to scr_siz$(1%),pic(000)
           if x% < 1% or x% > 999% then goto L51140
        return
L51140:    errormsg$ = "(Error) - Invalid Name or Size for Saw (1)?"
           gosub error_prompt
           scr_nam$(1%) = " " : scr_siz$(1%) = "025"
        return

L51190: REM Name and Size for Saw (2)              SCR_NAM$(),SCR_SIZ$()
           if scr_nam$(2%) <> " " then goto L51230
              scr_nam$(2%) = str(days$,1%,3%) & "00"
              scr_siz$(2%) = "100"
L51230:    if len(scr_nam$(2%)) <> 5 then goto L51300

           convert scr_siz$(2%) to x%,data goto L51300

           convert x% to scr_siz$(2%),pic(000)
           if x% < 1% or x% > 999% then goto L51300
        return
L51300:    errormsg$ = "(Error) - Invalid Name or Size for Saw (2)?"
           gosub error_prompt
           scr_nam$(2%) = " " : scr_siz$(2%) = "025"
        return

L51350: REM Name and Size for Saw (3)              SCR_NAM$(),SCR_SIZ$()
           if scr_nam$(3%) <> " " then goto L51390
              scr_nam$(3%) = str(days$,1%,3%) & "00"
              scr_siz$(3%) = "025"
L51390:    if len(scr_nam$(3%)) <> 5 then goto L51460

           convert scr_siz$(3%) to x%,data goto L51460

           convert x% to scr_siz$(3%),pic(000)
           if x% < 1% or x% > 999% then goto L51460
        return
L51460:    errormsg$ = "(Error) - Invalid Name or Size for Saw (3)?"
           gosub error_prompt
           scr_nam$(3%) = " " : scr_siz$(3%) = "025"
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        convert_fields
            init(" ") fld$(), wd$, ht$, spec$, t_b$, s_width$, s_height$,~
                      s_clmr$
            save_part$ = dt_part$
            s_width, s_height, s_clmr = 0.0         /* DECIMAL VALUES */
            partno$ = dt_part$
            fld$(1%) = str(partno$,1%,3%)              /* Model Number */
            fld$(2%) = str(partno$,4%,1%)              /* Color        */
            if comp% = 1% then L60245                   /* COMPONENT    */

            fld$(3%) = str(partno$,5%,2%)              /* Glass        */
            fld$(4%) = str(partno$,7%,2%)              /* Liting       */
            fld$(5%) = str(partno$,9%,2%)              /* Hinge        */
            fld$(6%) = str(partno$,11%,1%)             /* Screen       */
            fld$(7%) = str(partno$,12%,1%)             /* Locks        */
            fld$(8%) = str(partno$,13%,4%)             /* Width        */
            fld$(9%) = str(partno$,17%,3%)             /* Height       */
            fld$(10%) = str(partno$,20%,3%)            /* CLMR         */
            fld$(11%) = str(partno$,23%,3%)            /* WALLWIDT     */
            gosub std_wd_ht                     /* ACTUAL WIDTH/HEIGHT */
            gosub lookup_hinge
               a1, a2 = 0.0
               convert str(fld$(8),1%,3%) to a1, data goto L60135
L60135:
               convert str(fld$(8),4%,1%) to a2, data goto L60145
L60145:
               s_width = a1 + (a2/8.0)
               a1, a2 = 0.0
               convert str(fld$(9),1%,2%) to a1, data goto L60165
L60165:
               convert str(fld$(9),3%,1%) to a2, data goto L60175
L60175:
               s_height = a1 + (a2/8.0)
               a1, a2 = 0.0
               convert s_width  to s_width$, pic(####.####-)
               convert s_height to s_height$, pic(####.####-)

            if len(partno$) < 22 then goto L60235
               convert str(fld$(10),1%,2%) to a1, data goto L60235

               convert str(fld$(10),3%,1%) to a2, data goto L60225
L60225:
               s_clmr = a1 + (a2/8.0)
L60235:      convert s_clmr to s_clmr$, pic(####.####-)
        return
L60245:     wd$ = "COMPONE"
            ht$ = "NT PRT"
        return

        check_wegoma                        /* Lookup Wegoma Sort Code */
            check% = 0%
            readkey$ = " "
            str(readkey$,1%,9%)   = "WEGOMA700"
            str(readkey$,10%,15%) = model$
            read #3,key = readkey$,using L60295,descr$,eod goto L60310
L60295:         FMT POS(25), CH(30)
            srt$ = str(descr$,1%,5%)        /* Load Saw Sort Code      */
        check% = 1%
L60310: return

        lookup_hinge                                  /* Look Up Hinge */
            readkey$ = all(hex(00)) : c_o$ = "  "
            hng$ = "    "
            readkey$ = "HINGE    " & fld$(5)
            call "DESCRIBE" (#3, readkey$, descr$, 0%, f1%(3%))
            p% = pos(descr$ = "-")
            hnge$ = str(descr$,p%+2%,4%)
            if str(descr$,1%,2%) = "CO" or str(descr$,1%,2%) = "OR" then ~
                                                c_o$ = str(descr$,1%,2%)
            if str(descr$,5%,3%) = "1/3" then hng$ = "1/3 "
        return

        std_wd_ht              /* CONVERT STANDARD WIDTH AND HEIGHT */
                              /* F0%       - FRACT. NEW PART WIDTH    */
                              /* F1%       - FRACT. NEW PART HEIGHT   */
                              /* WD$   - REPLACEMENT WIDTH & FRACT (7)*/
                              /* HT$   - REPLACEMENT HEIGHT & FRACT(6)*/
           str(wd$,1%,3%)  = str(partno$,13%,3%)    /* WIDTH PART (3) */
           if str(wd$,1%,1%) = "0" then str(wd$,1%,1%) = " "
           str(ht$,1%,2%) = str(partno$,17%,2%)     /* HEIGTH PART (2)*/

           f0%, f1% = 0%                            /* SET FRACTIONS  */
           convert str(partno$,16%,1%) to f0%, data goto L60455 /* WIDTH */

           convert str(partno$,19%,1%) to f1%, data goto L60455 /* HEIGH */

           goto L60460
L60455:      f0%, f1% = 8%
L60460:    if f0% = 0% then f0% = 9%
           if f1% = 0% then f1% = 9%

           str(wd$,4%,1%) = " "          /* Build Width with Fraction */
           str(wd$,5%,3%) = str(sze$,(f0%*3%) - 2%, 3%)

           str(ht$,3%,1%) = " "          /* Build Height with Fraction */
           str(ht$,4%,3%) = str(sze$,(f1%*3%) - 2%, 3%)
        return

        set_alternate_key
            seq% = 0% : wrk_seq% = 0% : wrk% = 0%
            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 0%, rslt$(1%))
            if fs%(1) = -1% then goto L60540
               gosub file_in_use
               goto exit_program
L60540:     call "OPENCHCK" (#1, fs%(1%), f2%(1%),500%, rslt$(1%))
            init(" ") save_part$
            dt_key1$ = all(hex(00))
            str(dt_key1$,1%,6%) = str(scr_dte1$,1%,6%)  /* Prod Date   */
            sav_key$ = dt_key1$
        return

        rpt_1                                 /* Vinyl Prime Wegoma    */
            gosub set_alternate_key           /* Saws.                 */
            read #2,key 1% > dt_key1$, using L35050, dt_rec$,             ~
                                                     eod goto scan_done
            goto L60615
        scan_next
            read #2, using L35050, dt_rec$, eod goto scan_done

L60615:     dt_key1$ = str(dt_rec$,47%,57%)
            if str(dt_key1$,1%,6%) <> str(scr_dte1$,1%,6%) then          ~
                                                            goto scan_done
            dt_part$ = str(dt_rec$,189%,25%)         /* Part Number    */
            model$   = str(dt_part$,1%,3%)           /* Model Code     */
            dt_dept$ = str(dt_rec$,42%,3%)           /* Department Code*/
        REM Special Code for Wegoma Saws
                                                     /* (EWD001)       */ 
                                                     /* (EWD002)  BEG  */ 
REM            if dt_dept$ <> "036" then goto scan_next
               if dt_dept$ = "036" then goto create_data
               if dt_dept$ = "025" then goto create_data
               if dt_dept$ = "026" then goto create_data
                     goto scan_next
        create_data
                                                     /* (EWD002)  END  */ 
              gosub check_wegoma
              if check% = 0% then goto scan_next
                 dt_shft$ = str(dt_rec$,104%,2%)     /* Shift Code     */
                 if scr_shft$ = "AA" then goto L60710
                    if scr_shft$ <> dt_shft$ then goto scan_next
L60710:          comp% = 0%
                 if str(dt_rec$,215%,1%) = "Y" then comp% = 1%
                 dt_load$ = str(dt_rec$,1%,5%)       /* Load Number    */
                 mm$      = str(model$,1%,2%)        /* 1st of Model   */
                 dt_sort$ = str(dt_rec$,106%,5%)     /* Prod Sort Code */
                 dt_txt$  = str(dt_rec$,236%,4%)     /* Text Id.       */
                 dt_seq$  = str(dt_rec$,111%,5%)     /* Seq. No.       */
                 dt_ref$  = str(dt_rec$,96%,8%)      /* Warranty No.   */
                 dt_sash$ = str(dt_rec$,214%,1%)     /* 0,1,2,3        */
                 convert dt_seq$ to seq%, data goto L60760
L60760:                                              /* SEQ%-LOW/HIGH  */
                 sqq% = 99999% - seq%                /* SQQ%-HIGH/LOW  */
                 convert sqq% to sqq$, pic(00000)

                 gosub convert_fields
                 gosub update_rpt_1

            goto scan_next
        scan_done
            if opt$ <> "1" then goto optimize
            gosub print_rpt_1

            call "FILEBGON" (#1)
        return

        optimize                        /* Build Saw Optimization Data */
L60840:     gosub prompt_wegoma         /* Select Reports              */
            if comp% = 0% then return
               status% = comp%
               if status% = 16% then status% = 0%
               if status% > 3% then L60840

            gosub create_wegoma

            call "FILEBGON" (#1)
        return

        update_rpt_1
            init(" ") wrk_key$, wrk_key1$, wrk_rec$
            wrk_seq% = wrk_seq% + 1%                 /* Assign Seq No. */
            convert wrk_seq% to wrk_key$, pic(00000) /* File Being Read*/
            if wrk% > 99998% then wrk% = 0%
            wrk% = wrk% + 1%
            convert wrk% to wrk$, pic(00000)
            if mod(wrk%,slice%) <> 0 then goto L60950
               stack% = stack% + 1%
               convert stack% to stack$,pic(00000)
                                                     /* Date Load Seq. */
L60950:     str(wrk_key1$,1%, 5%) = stack$           /* PROD. LOAD     */
            str(wrk_key1$,6%, 6%) = "000000"         /* FILLER         */
            str(wrk_key1$,12%,5%) = srt$             /* WEGOMA700      */
            str(wrk_key1$,17%,3%) = model$           /* Model Code     */
            str(wrk_key1$,20%,1%) = fld$(2%)         /* Color Code     */
            str(wrk_key1$,21%,5%) = dt_seq$          /* SEQUENCE NUMBER*/
            str(wrk_key1$,26%,5%) = wrk$             /* SEQ NUMBER     */

            gosub get_info                           /* ADDITIONAL INFO*/
            str(wrk_rec$,1%,8%)  = dt_ref$           /* Reference No.  */
            str(wrk_rec$,9%,5%)  = dt_seq$           /* Sequence No.   */
            str(wrk_rec$,14%,2%) = c_o$              /* Cottage/Oriel  */
            str(wrk_rec$,16%,4%) = t_t$              /* Twin/Triple    */
            str(wrk_rec$,20%,3%) = t_b$              /* TSO,BSO,FSO    */
            str(wrk_rec$,23%,3%) = locks$            /* Locks          */
            str(wrk_rec$,26%,3%) = fin$              /* With Fin       */
            str(wrk_rec$,29%,5%) = dt_load$          /* Load Number    */
            str(wrk_rec$,34%,4%) = dt_txt$           /* Text Id        */
            str(wrk_rec$,38%,25%)= dt_part$          /* Part Number    */
            str(wrk_rec$,63%,7%) = wd$               /* WIDTH - WINDOW */
            str(wrk_rec$,70%,7%) = ht$               /* HEIGHT- WINDOW */
            str(wrk_rec$,77%,10%) = s_width$         /* WIDTH WIND DEC */
            str(wrk_rec$,87%,10%) = s_height$        /* HEIGHT WIND DEC*/
            str(wrk_rec$,97%,10%) = s_clmr$          /* CLMR DECIMAL   */
            str(wrk_rec$,107%,59%) = " "             /* Filler         */
                                                     /* Cut Records    */
            put #1,using L61085,wrk_key$,wrk_key1$,wrk_rec$
L61085:        FMT CH(5), CH(30), CH(165)
            write #1, eod goto L61105

        return
L61105:     call "SHOSTAT" ("(Error) Updating Wegoma File? (Duplicate)")
            stop
            call "SHOSTAT" ("(Error) Key --> " & wrk_key1$)
            stop
        return

        get_info
            init(" ") t_t$, t_b$, locks$, fin$
            if comp% = 1% then return
                                               /* LOOKUP_HINGE = C_O$  */
               t_t$ = "SNGL"                   /* Single,Double,Triple */
               if hnge$ = "TWIN" then t_t$ = hnge$
               if hnge$ = "TRPL" then t_t$ = hnge$
               if hng$ = "1/3 " then t_t$  = hng$
                                               /* TSO, BSO, FSO        */
               if dt_sash$ = "1" then t_b$ = "TSO"
               if dt_sash$ = "2" then t_b$ = "BSO"
               if dt_sash$ = "3" then t_b$ = "FSO"
               fin$ = "   "                    /* WITH FIN             */
               if fld$(7%) = "3" or fld$(7%) = "4" or fld$(7%) = "6" then~
                  fin$ = "YES"

               locks$ = "(0)"                   /* LOCKS - O,1,2       */
               if fld$(7%) = "1" or fld$(7%) = "3" then locks$ = "(1%)"
               if fld$(7%) = "2" or fld$(7%) = "4" then locks$ = "(2%)"

        return

        print_rpt_1
            call "APCPL43A" (scr_dte$, prod_dte$, #1, #3, #6, #7, #9,    ~
                                                  #11, #12 )
        return

        file_in_use
           comp% = 2%
           hdr$     = "***** Wegoma Cut Sheets ******"
           msg$(1%) = "      Utility Program Currently Being Used       "
           msg$(2%) = "               C u t   S h e e t s               "
           msg$(3%) = "Press <RETURN> To Exit, Cannot Continue !!!!     "
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        prompt_wegoma
           comp% = 2%
           hdr$     = "***** Wegoma Cut Sheets ******"
           msg$(1%) = " PF(1) = Saw (1), PF(2)  = Saw (2)               "
           msg$(2%) = " PF(3) = Saw (3), PF(16) = Wegoma Network Files  "
           msg$(3%) = " PF(1) thru PF(3) Reports,Press <RETURN> to Exit?"
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        create_wegoma
            end_of_file% = 0%                          /* INIT TO ZERO */
            cw%, ch% = 0%
            ofil$(1%) = "@WEGOM1@" : olib$(1%) = "APCDATA "
            ofil$(2%) = "@WEGOM2@" : olib$(2%) = "APCDATA "
            ofil$(3%) = "@WEGOM3@" : olib$(3%) = "APCDATA "

            ovol$(1%) = "CARLOS" : cl$(1%) = "ML"  : cl$(4%) = "BL"
            ovol$(2%) = "CARLOS" : cl$(2%) = "WH"  : cl$(5%) = "CO"
            ovol$(3%) = "CARLOS" : cl$(3%) = "BZ"  : cl$(6%) = "BG"

            cl$(7%) = "NO"  : cl$(10%) = "BH"
            cl$(8%) = "HO"  : cl$(11%) = "WB"
            cl$(9%) = "BN"  : cl$(12%) = "BB"

            for i% = 1% to 3%
                saw% = i%
                gosub load_info
                gosub open_files
            next i%

            if status% <> 0% then gosub prt_bat_select

        REM BEGIN PROCESS
            if status% = 0% then                                         ~
                         call "SHOSTAT" ("Creating Wegoma Batch Files")  ~
                    else call "SHOSTAT" ("Creating Wegoma Batch Report")

            scr_prod$ = "7"
            save_part$ = " "
            tw$ = "1" : th$ = "2"             /* LOAD CUT DESCRIPTIONS */
            call "APCCUTLD" (scr_prod$, cw%, ch%, tw$, th$, #3, err% )
            if err% <> 0% then goto exit_program
            wrk_key1$ = all(hex(00))
            read #1,key 1% > wrk_key1$,using L61535,wrk_key1$,wrk_rec$,   ~
                                                     eod goto create_done
            goto L61540
        create_next
            read #1,using L61535,wrk_key1$,wrk_rec$,eod goto create_done
L61535:         FMT POS(6), CH(30), CH(165)
L61540:     dt_part$  = str(wrk_rec$,38%,25%)         /* Part Number   */
            dt_seq$   = str(wrk_rec$,9%,5%)           /* Daily Seq. No.*/
            s_width$  = str(wrk_rec$,77%,10%)         /* Decimal Width */
            s_height$ = str(wrk_rec$,87%,10%)         /* Decimal Height*/
            s_clmr$   = str(wrk_rec$,97%,10%)         /* Decimal CLMR  */
            convert dt_seq$ to sss%, data goto L61570
L61570:
            convert sss% to sss$,pic(0000)

            if len(dt_part$) < 19 then goto create_next
               gosub calc_cuts
               gosub update_batches
               goto create_next
        create_done
            end_of_file% = 1%
            for saw% = 1% to 3%
                gosub rename_batch
            next saw%
            if status% = 0% then goto exit_program
               print using L62845
               close printer
        goto exit_program

        calc_cuts
          if save_part$ <> dt_part$ then goto L61675
             return                       /* USE EXISTING PROFILE DATA */

L61675:   save_part$ = dt_part$
          gosub load_prf         /* (CUT001) */
          call "APCCUTCC" (dt_part$, 0%, 0%, 0%, 0%, cw%, ch%, eq$(),    ~
                     ct$(), cr$(), cp$(), cc$(), col$(), ct(), sh$(),    ~
                     tw$, th$, #11, #12, #3, err%)
                     
          eq% = cw% + ch%
          for i% = 1% to eq%
              if cc$(i%) = "N" or cc$(i%) = " " then goto L61740
                 cut% = 0%            /* Skip - No Equation Records */
                 convert str(ct$(i%),1%,3%) to cut%, data goto L61725
L61725:
                 if cut% = 0% then goto L61740
                    gosub check_raw_mat
L61740:   next i%
        return

        file_exists
            comp% = 2%
            hdr$ = "** Optimization File Exists **"
            msg$(1%)= "       The File (@WEGOMA@) Already Exists.       "
            msg$(2%)= "             O P T I M I Z A T I O N             "
            msg$(3%)= "Press <RETURN> To Continue, or PF(16) to Delete. "
            str(msg$(1%),18%,8%) = ofil$(saw%)

            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        check_raw_mat                         /* Check Raw Material    */
            if i% > cw% then goto L61865
               if pw% = 0% then return
               for kk% = 1% to pw%                    /* Check Width   */
                  if cr$(i%) <> pwr$(kk%) then goto L61850
                     pwc(kk%)  = ct(i%)       /* Store Cut Length Dec  */
                     pwp$(kk%) = cp$(i%)      /* Store Number of Pieces*/
                     pwe$(kk%) = "Comp Calc " /* Good Calculation      */
L61850:        next kk%
               return

L61865:     if ph% = 0% then return
            for kk% = 1% to ph%                       /* Check Height  */
                if cr$(i%) <> phr$(kk%) then goto L61895
                   phc(kk%)  = ct(i%)         /* Store Cut Length Dec  */
                   php$(kk%) = cp$(i%)        /* Store Number of Pieces*/
                   phe$(kk%) = "Comp Calc "   /* Good Calculation      */
L61895:     next kk%
        return

        update_batches
            init(" ") saw_flags$
            if ph% = 0% then goto L62000
               for i% = 1% to ph%
                  saw% = 1%
                  convert phs$(i%) to saw%, data goto L61940
L61940:
                  order$ = phd$(i%)
                  prf$   = str(ph$(i%),1%,5%)
                  c$     = str(ph$(i%),6%,2%)
                  prf_mat$ = phr$(i%)
                  err$   = phe$(i%)
                  xx%    = int(100 * phc(i%))
                  convert xx% to lll$,pic(00000)

                  gosub build_rec
                  gosub write_rec
               next i%
L62000:     if pw% = 0% then goto L62080
               for i% = 1% to pw%
                  saw% = 1%
                  convert pws$(i%) to saw%, data goto L62020
L62020:
                  order$ = pwd$(i%)
                  prf$   = str(pw$(i%),1%,5%)
                  c$     = str(pw$(i%),6%,2%)
                  prf_mat$ = pwr$(i%)
                  err$   = pwe$(i%)
                  xx%    = int(100 * pwc(i%))
                  convert xx% to lll$,pic(00000)

                  gosub build_rec
                  gosub write_rec
               next i%
L62080:    for saw% = 1% to 3%                 /* Check Each Saw Batch */
              if cnt%(saw%) >= bcnt%(saw%) then gosub rename_batch
              if str(saw_flags$,saw%,1%) = "*" then                      ~
                 cnt%(saw%) = cnt%(saw%) + 1%
           next saw%
        return

        build_rec
           gosub check_profile
           gosub check_qtys
           saw_rec$ = " "
           str(saw_rec$,1%,11%)  = "K" & order$ /* Raw Material Desc */
           str(saw_rec$,12%,6%)  = "P" & prf$   /* Profile Code      */
           str(saw_rec$,18%,3%)  = "T" & c$     /* Cplor Code        */
           str(saw_rec$,21%,5%)  = "N" & sss$   /* Sequence Number   */
           str(saw_rec$,26%,4%)  = "A" & qty$   /* Quantity to Cut   */
           str(saw_rec$,30%,11%) = "C" & prf_mat$    /* Raw Material */
           str(saw_rec$,41%,6%)  = "L" & lll$   /* Dec Cut Length    */
           str(saw_rec$,47%,2%)  = "F" & "0"    /* Done Flag         */
        return

        check_profile                           /* Special Calc for   */
REM           if prf$ <> "301-B" then return       /* Cottage,Oriel,CLMR */
              if prf$ <> "301-B" and prf$ <> "765-O" then return
              s_height, s_clmr = 0.0
              convert s_height$ to s_height, data goto L62205
L62205:
              convert s_clmr$ to s_clmr, data goto L62215
L62215:
              if s_clmr <> 0 then goto L62250
                 if c_o$ = "OR" then                  /* Oriel Window  */~
                    x = round( (s_height * .600) + .0625, 2)
                 if c_o$ = "CO" then                  /* Cottage Window*/~
                    x = round( (s_height * .400) + .0625, 2)
/* (EWD002)  -  BEG  */
                 if c_o$ = "OR" and str(dt_part$,1%,3%) = "765" then     ~
                           x = round(x + 4,2)
                 if c_o$ = "OR" and str(dt_part$,1%,3%) = "766" then     ~
                           x = round(x + 4,2)
                 if c_o$ = "OR" and str(dt_part$,1%,3%) = "767" then     ~
                           x = round(x + 4,2)
/* (EWD002)  -  BEG  */
                 goto L62255
L62250:       x = round(s_clmr + .125, 2)       /* Center Line Meeting */
L62255:    convert x to prf_mat$,pic(######.##-)      /* Rail          */
        return

        write_rec
           if status% = 0% and str(err$,1%,1%) = "E" then return

           ff% = 14% + saw%
           if status% = 0% then                    /* NETWORK BATCHES */ ~
                                write #ff%,saw_rec$,eod goto L62315       ~
                           else gosub prt_bat_dtl  /* BATCH REPORTS   */
           if str(err$,1%,1%) = "E" then return    /* SKIP ERRORS     */
              str(saw_flags$,saw%,1%) = "*"        /* SET FOR SAW     */
L62315: return

        load_info                       /* NAME$() BATCH FILE NAME     */
                                        /* BCNT%() SIZE OF BATCH FILE  */
            convert scr_siz$(saw%) to bcnt%(saw%), data goto L62340
L62340:
            convert str(scr_nam$(saw%),4%,2%) to bb%(saw%),              ~
                                                          data goto L62355
L62355:
            bb%(saw%) = bb%(saw%) + 1%
            convert bb%(saw%) to bb$,pic(00)
            convert saw% to saw$,pic(0)
            name$(saw%) = "@"&saw$& str(scr_nam$(saw%),1%,3%) &bb$& "@"
            cnt%(saw%) = 1%
        return

        open_files
            if end_of_file% = 1% then return
            ff% = 14% + saw%                  /* SAW%     = 1% SAW (1) */
                                              /*          = 2% SAW (2) */
                                              /*          = 3% SAW (3) */
            if status% <> 0% then return      /* Reports               */
            call "OPENFILE" (#ff%,"IO   ",f2%(ff%),rslt$(ff%),axd$ )
            if f2%(ff%) <> 0% then goto L62470
               gosub file_exists
               if comp% <> 16% then goto L62455
                  call "FILEBGON" (#ff%)      /* Scratch Existing File */
                  goto L62470
L62455:        close #ff%                     /* Append to Existing    */
               call "OPENFILE" (#ff%,"EXTND",f2%(ff%),rslt$(ff%),axd$ )
               goto L62495
L62470:     str(rslt$(ff%),1%,6%)  = "OUTPTP" /* Create a New File     */
            str(rslt$(ff%),7%,8%)  = "00001000"
            str(rslt$(ff%),15%,3%) = "100"
            str(rslt$(ff%),18%,3%) = "100"
            call "OPENFILE" (#ff%,"OUTPT",f2%(ff%),rslt$(ff%), axd$ )
L62495: return

        rename_batch
            ret% = 0%
            ff% = 14% + saw%
            if status% <> 0% then goto L62570     /* Reports            */
               close #ff%
               call "RENAME" addr( "F",          /* Rename a File      */~
                                   ofil$(saw%),  /* Old Filename       */~
                                   olib$(saw%),  /* Old Library Name   */~
                                   ovol$(saw%),  /* Old Volume Name    */~
                                   name$(saw%),  /* New File Name      */~
                                   "B", "L", " ",                        ~
                                   ret% )

L62570:     cnt%(saw%) = 0%                    /* Applicable to Reports*/
            bb%(saw%) = bb%(saw%) + 1%         /* Increment Batch Count*/
            convert bb%(saw%) to bb$,pic(00)
            str(name$(saw%),6%,2%) = bb$       /* Set New Batch Name   */
            gosub open_files                   /* Open Next Batch File */
        return

        load_prf
            ph%,pw%,cl% = 0%
            init(" ") ph$(),phr$(),phs$(),phd$(),pw$(),pwr$(),pws$(),    ~
                      pwd$(), readkey$, c_o$, t_t$, tt$, sav_key1$
            model$ = str(dt_part$,1%,3%)       /* Product Code         */
                                               /* (EWD001)             */
            if str(dt_part$,4%,1%) = "A" then cl% = 7%
            if str(dt_part$,4%,1%) = "B" then cl% = 8%
            if str(dt_part$,4%,1%) = "C" then cl% = 9%
            if str(dt_part$,4%,1%) = "D" then cl% = 10%
            if str(dt_part$,4%,1%) = "E" then cl% = 11%
            if str(dt_part$,4%,1%) = "F" then cl% = 12%

            convert str(dt_part$,4%,1%) to cl%,data goto L62635
L62635:
            c_o$   = str(wrk_rec$,14%,2%)      /* CO-Cottage, OR-Oriel */
            t_t$   = str(wrk_rec$,16%,4%)      /* 'TWIN','TRPL',1/3 '  */
            tt$    = "S"                       /* Standard Window      */
            if c_o$ = "CO" then tt$ = "C"      /* Cottage              */
            if c_o$ = "OR" then tt$ = "O"      /* Oriel                */
            if t_t$ = "1/3 " then tt$ = "3"    /* 1/3,1/3,1/3          */
            str(readkey$,1%,9%)  = "WEGOMAPRF"
            str(readkey$,10%,3%) = model$
            sav_key1$ = str(readkey$,1%,12%)
        load_prf_nxt
            read #3,key > readkey$,using L62700,readkey$,desc$,           ~
                                                   eod goto load_prf_done
L62700:         FMT CH(24),CH(32)
            if str(readkey$,1%,12%) <> sav_key1$ then goto load_prf_done
            if str(readkey$,13%,1%) <> "H" then goto L62765
               if str(readkey$,24%,1%) <> tt$ then goto load_prf_nxt
                  if str(desc$,6%,2%) <> cl$(cl%) then goto load_prf_nxt
                     ph% = ph% + 1%
                     ph$(ph%)  = str(desc$,1%,7%)    /* PROFILE HEIGHT */
                     phr$(ph%) = str(readkey$,14%,10%) /*RAW MAT'L HGT */
                     phs$(ph%) = str(desc$,9%,2%)     /* SAW NUMBER    */
                     phd$(ph%) = str(desc$,12%,10%)   /* DESCRIPTION   */
                     phe$(ph%) = "Error Eq. "        /* Assume Error  */
                     goto load_prf_nxt
                                                     /* CHECK WIDTH    */
L62765:        if str(readkey$,24%,1%) <> tt$ then goto load_prf_nxt
                  if str(desc$,6%,2%) <> cl$(cl%) then goto load_prf_nxt
                     pw% = pw% + 1%
                     pw$(pw%)  = str(desc$,1%,7%)     /* PROFILE WIDTH */
                     pwr$(pw%) = str(readkey$,14%,10%) /*RAW MAT'L WID */
                     pws$(pw%) = str(desc$,9%,2%)     /* SAW NUMBER    */
                     pwd$(pw%) = str(desc$,12%,10%)   /* DESCRIPTION   */
                     pwe$(pw%) = "Error Eq. "         /* Assume Error  */
                     goto load_prf_nxt
        load_prf_done
        return

*       ****************************************************************~
*                       R e p o r t s                                  *~
*       ****************************************************************

L62845: %+---------------------------------------------------------------~
        ~--------------+
L62855: %!---------------------------------------------------------------~
        ~--------------!
L62865: %! ######## @ ########    #######################################~
        ~#   Page: ### !
L62875: %! Production Date: ########                                     ~
        ~Saw Number: ##!
L62885: %!Bat Name!Mod!Order No. ! Prf !Cl!Seq.!Qty!Raw Mat'l !Length!Cou~
        ~nt!  Remarks  !
L62895: %!########!###!##########!#####!##!####!###!##########! #####!###~
        ~##! ##########!
L62905: %!--------!---!----------!-----!--!----!---!----------!------!---~
        ~--!-----------!

        prt_bat_select
           init(" ") xdate$, xtime$, sav_name$, weg_tit$
           pageno% = 0% : lcnt% = 99%
           call "SETPRNT" (" ","WEGM", 0, 0)
           call "TIME" (xtime$)
           xdate$ = date : call "DATEFMT" (xdate$)
           select printer(134)
           weg_tit$ = "***** Wegoma Batch File(s) Report ******"
           convert status% to status$,pic(00)
        return

        prt_bat_hdr
           if lcnt% <> 99% then print using L62845
           pageno% = pageno% + 1%
           print page
           print using L62845
           print using L62865, xdate$, xtime$, weg_tit$, pageno%
           print using L62875, scr_dte$, status$
           print using L62855
           print using L62885
           lcnt% = 4%
        return

        prt_bat_dtl
           if status% <> saw% then return             /* Report By Saw */
           if lcnt% > 57% then gosub prt_bat_hdr
           if sav_name$ = name$(saw%) then goto L63085
              sav_name$ = name$(saw%)
              print using L62905
              print using L62895, name$(saw%),model$,order$,prf$,c$, sss$,~
                                 qty$,prf_mat$,lll$,cnt%(saw%),err$
              goto L63100

L63085:    print using L62905
           print using L62895, "        " ,model$,order$,prf$,c$, sss$,   ~
                              qty$,prf_mat$,lll$,cnt%(saw%),err$
L63100:    lcnt% = lcnt% + 2%
        return

        check_qtys
           qty$ = "001"
           if saw% <> 3% then return
           readkey$ = " "
           str(readkey$,1%,9%) = "WEGOMQTYS"
           str(readkey$,10%,15%) = prf$
           read #3,key = readkey$,using L63150, qty$, eod goto L63155
L63150:       FMT POS(25), CH(3)
L63155: return

        error_prompt
           comp% = 2%
           hdr$ = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

*       ****************************************************************~
*                        E X I T   P R O G R A M                       *~
*       ****************************************************************

        exit_program

        end
