        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   IIIII  N   N  V   V  EEEEE  N   N   *~
            *  A   A  P   P  C   C    I    NN  N  V   V  E      NN  N   *~
            *  AAAAA  PPPP   C        I    N N N  V   V  EEE    N N N   *~
            *  A   A  P      C   C    I    N  NN   VVV   E      N  NN   *~
            *  A   A  P       CCC   IIIII  N   N    V    EEEEE  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCINVEN - Keith Hunter's Inventory Work Sheet Report.    *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF ALUMINUM PRODUCTS CORP., INC., RURAL HALL, *~
            * N.C. AND IS CONFIDENTIAL INFORMATION. UNATHORIZED USE,    *~
            * COPYING, DECOMPILING, TRANSLATING, DISCLOSURE, OR TRANSFER*~
            * OF IT IS PROHIBITED. COPYRIGHT (C) 1991, AN UNPUBLISHED   *~
            * WORK BY ALUMINUM PRODUCTS CORP., INC., RURAL HALL, N.C.   *~
            * ALL RIGHTS RESERVED.                                      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/01/91 ! New Program for (APC) - Last Mod Date    ! RHH *~
            *          !                                          !     *~
            * 11/06/97 ! Revision Update For 60403                ! DJD *~
            * 02/22/99 ! (EWD001) Add Bin Location; Misc. mods.   ! BWS *~
            * 03/30/99 ! (EWD002) Mod for Inactive Qty. records.  ! BWS *~
            * 08/17/00 ! (EWD003) Mod for Adding Vendor Code Sort ! CMG *~            
            *************************************************************

        dim                                                              ~
            hny_key$25,                  /* HNYMASTR - Primary Key     */~
            hny_quan_key$44,             /* HNYQUAN  - Primary Key     */~
            part_no$25,                  /* Part No.                   */~
            part_desc$32,                /* Part Desc                  */~
            stk_unm$4,                   /* Stock Unit of Measure      */~
            stk_desc$30,                 /* Stock Description          */~
            cat$4,                       /* Category                   */~
            cat_desc$30,                 /* Category Desc              */~
            part_type$3,                 /* Part Type                  */~
            type_desc$30,                /* Part Type Desc             */~
            on_hand$12,                  /* Part On Hand Quantity      */~
/*EWD003*/  ven_key$59,                  /* VENPRICE read key          */~            
/*EWD003*/  vendor$9,                    /* Vendor Code                */~
            beg_type$3, end_type$3,      /* Begin and End Part Type    */~
            beg_cat$4, end_cat$4,        /* Begin and End Part Cat     */~
            beg_str$3, end_str$3,        /* Begin and End Store No.    */~
/*EWD003*/  beg_ven$9, end_ven$9,        /* Begin and End Vendor Code  */~            
            descript$30,                 /* Lookup Description         */~
            str_name$30,                 /* Store Name                 */~
            str_no$3,                    /* Store Number               */~
            bin_loc$8,                   /* Primary Bin Location       */~
            readkey$24,                  /* Gencodes Key               */~
            count$5,                     /* Record Count for Work File */~
            company$40,                  /* For Report Company Name    */~
            print_title$40,              /* For Report Title           */~
            wrk_key$35,                  /* For Report Time            */~
/*EWD002*/  vf1$1,                       /* HNYQUAN Variable Field #1  */~
            cursor%(25),                 /* Cursor location for edit   */~
/*EWD001*/  date$10,                     /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(5),                      /* = 0 if the file is open    */~
            f1%(5),                      /* = 1 if READ was successful */~
            fs%(5),                      /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5)20                   /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/06/97 Pre-Release Version            "
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
            * #01 ! HNYMASTR ! Inventory Part Master                    *~
            * #02 ! GENCODES ! SYSTEM MASTER CODE TABLE FILES           *~
            * #03 ! HNYQUAN  ! Inventory Costs and Quantites            *~
            * #04 ! APCINVEN ! Inventory SORT WORK FILE                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #3,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =   650,             ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #4,  "APCINVEN",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  44

/*EWD003*/  select #5,  "VENPRICE",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   10, keylen =  59,                     ~
                        alt key  1, keypos =    1, keylen =  34, dup,    ~
                            key  2, keypos =   35, keylen =  34          ~


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01),  0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02),  0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03),  0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04),  0%, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05),  0%, rslt$(05))            
            if fs%(4) <> 0 then call "FILEBGON" addr(#4)

            f1%(1), f1%(2), f1%(3), f1%(4), f1%(5) = 0%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATFMTC" (date$)      /*EWD001*/
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   4%
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
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then goto exit_program
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
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
            gosub build_work_file
            gosub generate_report
        return clear all
        goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20130,         /* Beg/End Part Type     */ ~
                              L20170,         /* Beg/End Part Category */ ~
                              L20210,         /* Beg/End Store No      */ ~
/* EWD003  */                 L20250          /* Beg/End Vendor Code   */                                
         return

L20130: REM Beginning and Ending Part Type         BEG_TYPE$, END_TYPE$
        REM BEG_TYPE$, END_TYPE$ = "ALL"
         return

L20170: REM Beginning and Ending Part Category     BEG_CAT$, END_CAT$
        REM BEG_CAT$, END_CAT$ = "ALL"
         return

L20210: REM Beginning and Ending Store No.         BEG_STR$, END_STR$
        REM BEG_STR$, END_STR$ = "ALL"
         return

L20250: REM Beginning and Ending Vendor Code       BEG_VEN$, END_VEN$
        REM BEG_VEN$, END_VEN$ = "ALL"
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
         "Enter a Valid Beginning and Ending Part Type or (All).       ",~
         "Enter a Valid Beginning and Ending Part Category or (All).   ",~
         "Enter a Valid Beginning and Ending Store Number or (All).    ",~
         "Enter a Valid Beginning and Ending Vendor Code  or (All).    "         
/* EWD003 */
        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, hny_key$, hny_quan_key$,   ~
                      part_no$, part_desc$, stk_unm$, stk_desc$, cat$,   ~
                      part_type$, descript$, type_desc$, beg_type$,      ~
                      end_type$, beg_cat$, end_cat$, beg_str$, end_str$, ~
                      str_name$, cat_desc$, on_hand$, readkey$, count$,  ~
                      beg_ven$, end_ven$, vendor$   /* EWD003 */
        return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF ALUMINUM PRODUCTS CORP., INC., RURAL HALL, *~
            * N.C. AND IS CONFIDENTIAL INFORMATION. UNATHORIZED USE,    *~
            * COPYING, DECOMPILING, TRANSLATING, DISCLOSURE, OR TRANSFER*~
            * OF IT IS PROHIBITED. COPYRIGHT (C) 1991, AN UNPUBLISHED   *~
            * WORK BY ALUMINUM PRODUCTS CORP., INC., RURAL HALL, N.C.   *~
            * ALL RIGHTS RESERVED.                                      *~
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
        dataload
            get #4, using L30080, part_type$, cat$, part_no$, str_no$,    ~
 /*EWD001*/ /*EWD003*/           vendor$, part_desc$, stk_unm$, on_hand,  ~
                                 bin_loc$
L30080:       FMT CH(3), CH(4), CH(25), CH(3), CH(9), CH(32), CH(4),      ~
 /*EWD001*/         PD(14,4), CH(8)
/*EWD003*/
            convert on_hand to on_hand$,   pic(####,###.##-)

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
              on fieldnr% gosub L40170,         /* BEG AND END TYPE    */ ~
                                L40170,         /* BEG AND END CATEGORY*/ ~
                                L40170,         /* BEG AND END STORE   */ ~
                                L40170          /* BEG AND VENDOR CODE */                                
              goto L40200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02),                                               ~
                  "Inventory Work Sheet Report ",                        ~
/*EWD001*/     at (01,64), "Today:",                                     ~
/*EWD001*/     at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Beginning Part Type:",                       ~
               at (06,25), fac(lfac$( 1)), beg_type$            , ch(03),~
               at (06,40), "Ending Part Type   :",                       ~
               at (06,65), fac(lfac$( 1)), end_type$            , ch(03),~
                                                                         ~
               at (07,02), "Beginning Part Cat.:",                       ~
               at (07,25), fac(lfac$( 2)), beg_cat$             , ch(04),~
               at (07,40), "Ending Part Cat.   :",                       ~
               at (07,65), fac(lfac$( 2)), end_cat$             , ch(04),~
                                                                         ~
               at (08,02), "Beginning Store No.:",                       ~
               at (08,25), fac(lfac$( 3)), beg_str$             , ch(03),~
               at (08,40), "Ending Store No.   :",                       ~
               at (08,65), fac(lfac$( 3)), end_str$             , ch(03),~
                                                                         ~
/*EWD003*/     at (09,02), "Beginning Vendor No:",                       ~
               at (09,25), fac(lfac$( 4)), beg_ven$             , ch(09),~
               at (09,40), "Ending Vendor No.  :",                       ~
/*EWD003*/     at (09,65), fac(lfac$( 4)), end_ven$             , ch(09),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40540
                  call "PRNTSCRN"
                  goto L40200

L40540:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40730     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40690
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40690:     if fieldnr% > 1% then L40710
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40710:     return

L40730: if fieldnr% > 0% then L40820  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40820:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
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
            on fieldnr% gosub L50130,         /* Beg and End Part Type */ ~
                              L50340,         /* Beg and End Part Cat  */ ~
                              L50530,         /* Beg and End Store No. */ ~
                              L50700          /* Beg & End Vendor Code */                              
            return

L50130: REM Beginning and Ending Part Type      BEG_TYPE$, END_TYPE$
            if beg_type$ <> " " then goto L50170
               beg_type$, end_type$ = "ALL"
               return
L50170:     if beg_type$ <> " " and end_type$ = " " then                 ~
                                                    end_type$ = beg_type$

            convert beg_type$ to beg_type%, data goto L50300

            convert end_type$ to end_type%, data goto L50300

            if beg_type% > end_type% then goto L50300
               convert beg_type% to beg_type$, pic(###)
               convert end_type% to end_type$, pic(###)
            if str(beg_type$,1%,1%) <> "2" or str(end_type$,1%,1%) <> "2"~
                                                         then goto L50300
            return
L50300:  errormsg$="Must Enter a Valid Beg/End Part Type (2??) or (All)"
         beg_type$, end_type$ = " "
         return

L50340: REM Beginning and Ending Category       BEG_CAT$, END_CAT$
            if beg_cat$ <> " " then goto L50380
               beg_cat$, end_cat$ = "ALL"
               return
L50380:     if beg_cat$ <> " " and end_cat$ = " " then                   ~
                                                      end_cat$ = beg_cat$

            convert beg_cat$ to beg_cat%, data goto L50490

            convert end_cat$ to end_cat%, data goto L50490

            if beg_cat% > end_cat% then goto L50490
               convert beg_cat% to beg_cat$, pic(0000)
               convert end_cat% to end_cat$, pic(0000)
               return
L50490:  errormsg$ = "Must Enter a Valid Beg and End Part Cat. (All)"
         beg_cat$, end_cat$ = " "
         return

L50530: REM Beginning and Ending Store No.      BEG_STR$, END_STR$
            if beg_str$ <> " " then goto L50570
               beg_str$, end_str$ = "ALL"
               return
L50570:     if beg_str$ <> " " and end_str$ = " " then                   ~
                                                      end_str$ = beg_str$

            convert beg_str$ to beg_str%, data goto L50680

            convert end_str$ to end_str%, data goto L50680

            if beg_str% > end_str% then goto L50680
               convert beg_str% to beg_str$, pic(0##)
               convert end_str% to end_str$, pic(0##)
               return
L50680:  errormsg$ = "Must Enter a Valid Beg and End Store No. (All)"
         beg_str$, end_str$ = " "
         return

L50700: REM Beginning and Ending Vendor Code    BEG_VEN$, END_VEN$
            if beg_ven$ <> " " then goto L50710
               beg_ven$, end_ven$ = "ALL"
               return
L50710:     if beg_ven$ <> " " and end_ven$ = " " then                   ~
                                                      end_ven$ = beg_ven$


            return
REM L50720  errormsg$ = "Must Enter a Valid Beg and End Vendor Code (All)"
         beg_ven$, end_ven$ = " "
         return

         
        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************
REM  Image Stmnts Modified for EWD001 = L55090, L55110, L55170 to L55230.
REM  Image Stmnts Modified for EWD003 = L55120

L55040: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+
                                                   /* COLUMN 1 HEADER */
L55070: %!BEG TYPE: ###         END TYPE: ###    ########################~
        ~################                                    PAGE : ###  !
L55090: %!BEG CAT.: ###         END CAT : ###                            ~
        ~                                                    USER : ###  !
L55110: %!BEG STR : ###         END STR : ###                            ~
        ~                                                                !
L55120: %!BEG VEN : #########   END VEN : #########                      ~
        ~                                               DATE: ########## !
        
L55140: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

L55170: %!PART TYPE!PART CAT! PART NUMBER      !LOCATION!   PART DESCRIPT~
        ~ION             ! UNM!STR NO.! ON HAND QTY! VENDOR CODE ! TOTAL !
                                                           /* EWD003 */
L55200: %!---------!--------!------------------!--------!----------------~
        ~----------------!----!-------!------------!-------------!-------!

L55230: %!   ###   !  ####  ! #################!########!################~
        ~################!####!  ###  !############!  #########  !       !
                                                            /* EWD003 */
        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            page_no% = 0%
            lcnt%    = 99%
/*EWD001*/  print_title$ = "Inventory Work Sheet Report (APCINVEN)"
            call "FMTTITLE" (print_title$, " ", 12%)
            date$ = date  :  call "DATFMTC" (date$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCINV", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCINV", " ", 0%, 1%)
        return

        generate_report
            convert count% to count$, pic(#####)

            call "SHOSTAT" ("Creating Work Sheet-("&count$&") Parts.")
            gosub select_printer
            wrk_key$ = all(hex(00))
            read #4,key > wrk_key$, eod goto generate_done
            goto L60290
        generate_next
            read #4, eod goto generate_done
L60290:       gosub dataload
              gosub print_detail
            goto generate_next
        generate_done
            print using L55040
            gosub close_printer
            call "SHOSTAT" ("Deleting Work File.")
            call "DELETE" (#4, " ", 0%)
            call "FILEBGON" addr(#4)
        return

        print_header
          page_no% = page_no% + 1%
          print page
          print using L55040
          print using L55070,beg_type$,end_type$,print_title$,page_no%
          print using L55090, beg_cat$, end_cat$, userid$   /*EWD001*/
          print using L55110, beg_str$, end_str$       
          print using L55120, beg_ven$, end_ven$, date$     /*EWD003*/
          print using L55140
          print using L55170
          lcnt% = 6%
        return

        print_detail
          if lcnt% < 60% then goto L60570
             if lcnt% <> 99% then print using L55040
             gosub print_header

L60570:   print using L55200
 /*EWD*/  print using L55230, part_type$, cat$, part_no$, bin_loc$,       ~
 /*001*/                      part_desc$, stk_unm$, str_no$, on_hand$,    ~
                              vendor$
          lcnt% = lcnt% + 2%
        return

        build_work_file
            count% = 0%
            call "SHOSTAT" ("Building Work File")
            call "OPENCHCK" (#04, fs%(04), f2%(04),800%, rslt$(04))
            hny_key$ = all(hex(00))
            read #1,key > hny_key$, eod goto hnymastr_done
            goto L60710
        next_hny
            read #1,eod goto hnymastr_done
L60710:       get #1, using L60720, part_type$
L60720:         FMT POS(180), CH(3)
            if str(part_type$,1%,1%) <> "2" then goto next_hny
               if beg_type$ = "ALL" then goto L60780
                  if part_type$ < beg_type$ or part_type$ > end_type$    ~
                                            then goto next_hny

L60780:        get #1, using L60800, part_no$, part_desc$, stk_unm$,      ~
                                    cat$, bin_loc$      /*EWD001*/
L60800:          FMT CH(25), CH(32), XX(16), CH(4), XX(12), CH(4),       ~
                     POS(155), CH(8)                    /*EWD001*/
               convert cat$ to cat%, data goto L60812
L60812:
               convert cat% to cat$, pic(0000)

               if beg_cat$ = "ALL" then goto L60860
                  if cat$ < beg_cat$ or cat$ > end_cat$ then             ~
                                               goto next_hny

L60860:        gosub get_vendor
               if beg_ven$ = "ALL" then goto L60880
                  if vendor$ < beg_ven$ or vendor$ > end_ven$ then       ~
                                                goto next_hny
                                               
L60880:     gosub update_work
            goto next_hny
        hnymastr_done
        return

        update_work
            hny_quan_key$ = all(hex(00))
            str(hny_quan_key$,1%,25%) = part_no$
        update_next
            read #3, key > hny_quan_key$, using L60970, hny_quan_key$,    ~
                       on_hand, vf1$, eod goto update_done  /*EWD002*/
L60970:        FMT POS(17), CH(44), XX(8), PD(14,4), POS(422), CH(1)
           if str(hny_quan_key$,1%,25%) <> part_no$ then goto update_done
              if vf1$ = "Y" then goto update_next           /*EWD002*/
              str_no$ = str(hny_quan_key$,26%,3%)
              if beg_str$ = "ALL" then goto L61040
                 if str_no$ < beg_str$ or str_no$ > end_str$ then        ~
                                                    goto update_next

L61040:    write #4, using L61080, part_type$, cat$, part_no$, str_no$,   ~
                /*EWD001*/        vendor$, part_desc$, stk_unm$, on_hand, ~
                /*EWD001*/        bin_loc$, " ", eod goto L61150

L61080:      FMT CH(3), CH(4), CH(25), CH(3), CH(9), CH(32), CH(4),       ~
                 PD(14,4), CH(8), CH(4)      /*EWD001*/
           count% = count% + 1%
           goto update_next
        update_done
        return

L61150:    stop " DUPLICATE RECORD IN WORK FILE ---> "&part_no$&str_no$
           goto update_next

        get_vendor
          init(" ") ven_key$, vendor$
          str(ven_key$,1%,25%) = part_no$
          read #5, key > ven_key$, using L61200, ven_key$, eod goto vendor_done

L61200:        FMT XX(09), CH(59)
            goto L61210
        get_vendor_next
          read #5, eod goto vendor_done

L61210:   if str(ven_key$,1%,25%) > part_no$ then goto vendor_done
          if str(ven_key$,1%,25%) <> part_no$ then goto get_vendor_next

          vendor$ = str(ven_key$,26%,9%)
          
        vendor_done
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF ALUMINUM PRODUCTS CORP., INC., RURAL HALL, *~
            * N.C. AND IS CONFIDENTIAL INFORMATION. UNATHORIZED USE,    *~
            * COPYING, DECOMPILING, TRANSLATING, DISCLOSURE, OR TRANSFER*~
            * OF IT IS PROHIBITED. COPYRIGHT (C) 1991, AN UNPUBLISHED   *~
            * WORK BY ALUMINUM PRODUCTS CORP., INC., RURAL HALL, N.C.   *~
            * ALL RIGHTS RESERVED.                                      *~
            *-----------------------------------------------------------*~

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
