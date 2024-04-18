        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   U   U  PPPP   DDDD   TTTTT  EEEEE   *~
            *  A   A  P   P  C   C  U   U  P   P  D   D    T    E       *~
            *  AAAAA  PPPP   C      U   U  PPPP   D   D    T    EEE     *~
            *  A   A  P      C   C  U   U  P      D   D    T    E       *~
            *  A   A  P       CCC    UUU   P      DDDD     T    EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCUPDTE - Update Inventory Quantities for Loads where    *~
            *            the MAKE Jobs are Complete.                    *~
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
            * 02/01/91 ! New Program for (APC) - Last Mod Date    ! RHH *~
            *          !                                          !     *~
            * 11/12/97 ! Modified For Revision 60403              ! DJD *~
            *          !                                          !     *~
            *************************************************************

        dim                              /* File - (HNYADDTF)          */~
            jnlid$3,                     /* Journal Id                 */~
            hny_userid$3,                /* User Id                    */~
            hny_seq$3,                   /* Buffer Seq. Number         */~
            hny_part$25,                 /* Part Number                */~
            hny_store$3,                 /* Store Number               */~
            hny_lot$16,                  /* Lot Number (Blank)         */~
            hny_job$8,                   /* Job Number (Blank)         */~
            hny_qty$10,                  /* Quantity to ADD to On-Hand */~
            hny_asset$9,                 /* G/L Asset Account          */~
            hny_source$9,                /* G/L Source Account         */~
            hny_cost(12),                /*                            */~
            hny_cost$96,                 /* 12 Cost Buckets            */~
            hny_vendor$9,                /* Vendor Code (Blank)        */~
            hny_desc$25,                 /* Description of Entry       */~
            hny_po$16,                   /* P.O. Number (Blank)        */~
            hny_filler$67,               /* Filler Area                */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim                              /* FILE = APCJOBSM            */~
            jb_demand$5,                 /* Demand Code/Load Number    */~
            jb_demand_seq$4,             /* D(0000-0999),P(1000-9999)  */~
            jb_qty$12,                   /* Job Quantity               */~
            jb_dte1$6,                   /* Job Completion Date        */~
            jb_sort$5,                   /* PRODUCT/BIN SORT           */~
            jb_part$25,                  /* Part NUmber                */~
            jb_number$9,                 /* Job Number ( DDDDDSSSS )   */~
            jb_status$1,                 /* Job Status ( Y/N )         */~
            jb_flag$1,                   /* Inventory Updated (Y/N)    */~
            jb_key$9,                    /* Job Primary Key            */~
            jb_rec$64                    /* Job Record                 */

        dim                              /* FILE = APCLDCOL            */~
            col_load$5,                  /* Data Collection Load No.   */~
            col_load_ref$5,              /* Data Collection Ref Load   */~
            col_key$11,                  /* Data Collection Primary Key*/~
            jb_load$(100)5               /* Save All Reference Loads   */

        dim f2%(10),                     /* = 0 if the file is open    */~
            f1%(10),                     /* = 1 if READ was successful */~
            fs%(10),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/12/97 Pre-Release Version            "
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
            * #01 ! HNYADDTF ! Additions Buffer for Inventory           *~
            * #02 ! APCJOBSM ! Load/Job Summary File                    *~
            * #03 ! GENCODES ! Master System Code Tables                *~
            * #04 ! HNYMASTR ! Master Inventory File                    *~
            * #05 ! HNYQUAN  ! Inventory Store Quantity File            *~
            * #06 ! SYSFILE2 ! SYSTEM CONTROL FILE                      *~
            * #10 ! APCLDCOL ! DATA COLLECTION TRACKING FILE            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select  #1, "HNYADDTF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 6


            select #02, "APCJOBSM",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   18, keylen =  45

            select #03, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


            select #04, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #05, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select  #6, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #10, "APCLDCOL",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =   12, keylen =  23,         ~
                            key  2, keypos =   35, keylen =  37, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01),100%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02),  0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03),  0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04),  0%, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05),  0%, rslt$(05))
            call "OPENCHCK" (#06, fs%(06), f2%(06),  0%, rslt$(06))
            call "OPENCHCK" (#10, fs%(10), f2%(10),  0%, rslt$(10))

            f1%(1), f1%(2), f1%(3), f1%(4), f1%(5), f1%(6) = 0%

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

            hny_userid$  = userid$
            hny_store$   = "300"
            hny_lot$     = " "
            hny_job$     = " "
            hny_qty$     = " "
            hny_asset$   = "1330-313"
            hny_source$  = "1320-313"
            hny_cost$    = " "
            hny_vendor$  = " "
            hny_desc$    = " "
            hny_po$      = " "
            hny_filler$  = " "

            call "GLUNFMT" (hny_asset$)
            call "GLUNFMT" (hny_source$)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   1%
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
                  if keyhit%  =  9% then gosub update_invevtory
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 1% then editpg1
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
            *             S P E C I A L   P R O C E S S I N G           *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        update_invevtory
            gosub process_update
            jnlid$ = "IAD"
            call "APCPOST" (jnlid$)

        return clear all
        goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20130          /* Load Number           */

         return

L20130: REM Load Number                            LOAD_NO$
        REM LOAD_NO$ = " "
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
         "Enter the Load Number to Update Inventory.                   "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, load_no$,       jb_demand$,~
                      jb_demand_seq$, jb_qty$, jb_dte1$, jb_sort$,       ~
                      jb_part$, jb_number$, jb_status$, jb_flag$,        ~
                      jb_key$, jb_rec$


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

L35040: FMT                 /* FILE: HNYADDTF                          */~
            CH(3),          /* user-id of specific user                */~
            BI(3),          /* seq. no. for additions buffer           */~
            CH(3),          /* item sequence number                    */~
            CH(25),         /* Part Number                             */~
            CH(3),          /* Store or Warehouse Code                 */~
            CH(16),         /* Lot Number                              */~
            CH(8),          /* Job Number                              */~
            PD(14,4),       /* detail to quantity on-hand              */~
            CH(9),          /* Inventory Asset Account                 */~
            CH(9),          /* inventory source account                */~
            CH(96),         /* costs                                   */~
            CH(9),          /* Vendor Code                             */~
            CH(25),         /* description of purpose                  */~
            CH(16),         /* Purchase Order Number                   */~
            CH(67)          /* Filler (Internal, unused space)         */~

                                         /* APCJOBSM - JOB/DEMAND Sum  */
            FMT CH(05),                  /* Demand Code / Load No.     */~
                CH(04),                  /* Demand Seq. No.            */~
                                         /*  (0000-0999) Make          */~
                                         /*  (1000-9999) Pull          */~
                PD(14,4),                /* Demand Quantity            */~
                CH(06),                  /* Completion Date unformatted*/~
                CH(05),                  /* Product/BIN Sort Code      */~
                CH(25),                  /* Part Number        ter     */~
                CH(09),                  /* Demand + Demand Seq. No.   */~
                CH(01),                  /* Job Status Y-Complete,N-No */~
                CH(01)                   /* Inventory Updated (Y/N)    */~

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
              on fieldnr% gosub L40150          /* Load Number       */

              goto L40190

L40150:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02),                                               ~
                  "Inventory Update for Production Loads",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Load Number  :",                             ~
               at (06,20), fac(lfac$( 1)), load_no$             , ch(05),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (9)Update Inventory    " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffff09ffffffffff0f1000)
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
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50120          /* Load Number           */

            return

L50120: REM Load Number                           LOAD_NO$
            if load_no$ <> " " then goto L50150
               goto L50240
L50150:     convert load_no$ to load_no%, data goto L50190

            convert load_no% to load_no$, pic(00000)
            goto L50250
L50190:        convert str(load_no$,2%,4%) to load_no%, data goto L50240

               convert load_no% to str(load_no$,2%,4%), pic(0000)
               goto L50250

L50240:     errormsg$ = "Must Enter a Valid Load Number. "
L50250:  return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        process_update
            gosub find_all_jobs          /* CHECK LOAD FOR ALL REF'S */

            call "SHOSTAT" ("Scanning Load for Parts")
            hny_index%, hny_seq% = 0%
            jb_inc% = 0%

L60100:     jb_inc% = jb_inc% + 1%
            if jb_inc% > jb% then goto L60570   /* FINISHED ALL REF'S */

            jb_key$ = all(hex(00))
            str(jb_key$,1%,5%) = jb_load$(jb_inc%)
        process_next
            read #2,key > jb_key$, using L60180, jb_rec$,                 ~
                                                    eod goto process_done
L60180:        FMT CH(64)

            jb_key$    = str(jb_rec$,1%,9%)
            if str(jb_key$,1%,5%) <> jb_load$(jb_inc%) then              ~
                                                       goto process_done
            if str(jb_key$,6%,1%) <> "0" then goto process_next
            jb_part$   = str(jb_rec$,29%,25%)
            jb_status$ = str(jb_rec$,63%,1%)
            jb_flag$   = str(jb_rec$,64%,1%)
            if jb_status$ = "N" or jb_flag$ = "Y" then goto process_next
            get str(jb_rec$,10%,8%), using L60250, jb_qty
L60250:        FMT PD(14,4)

            gosub check_hnymastr
            if rec% = 0% then goto L60490           /* UPDATE ANYWAY */
            gosub check_cost

            hny_index% = hny_index% + 1%
            hny_seq%   = hny_seq%
            convert hny_seq% to hny_seq$, pic(000)

            hny_part$  = jb_part$
            hny_qty    = jb_qty
            hny_desc$  = "INV ADD'S FOR LOAD-" & jb_load$(jb_inc%)
            hny_qty = round(hny_qty, 2)
            write #1, using L35040,                                       ~
                      hny_userid$, hny_index%, hny_seq$, hny_part$,      ~
                      hny_store$, hny_lot$, hny_job$, hny_qty,           ~
                      hny_asset$, hny_source$, hny_cost$, hny_vendor$,   ~
                      hny_desc$, hny_po$, hny_filler$
                                             /* UPDATE APCJOBSM RECORD */
L60490:     read #2,hold,key = jb_key$, eod goto L60580
               put #2, using L60510, "Y"
L60510:          FMT POS(64), CH(1)
            rewrite #2

            goto process_next
        process_done
            goto L60100                          /* CHECK NEXT LOAD REF */
L60570: return
L60580:     stop "UNABLE TO UPDATE (APCJOBSM) FOR ---> " & jb_key$
            goto process_next

        check_hnymastr
            rec% = 0%
            read #4,key = jb_part$, eod goto L60650
            rec% = 1%
L60650: return

        check_cost
            mat hny_cost = zer : totlcost = 0
            call "STCCOSTS" (jb_part$, " ", #6, 2%, totlcost, hny_cost())
            put hny_cost$ using L60710, hny_cost()
L60710:             FMT 12*PD(14,4)
        return

        find_all_jobs
            init(" ") jb_load$()
            call "SHOSTAT" ("Scanning Load for Reference Jobs")
            jb% = 0%
            col_key$ = all(hex(00))
            str(col_key$,1%,5%) = load_no$
            read #10,key > col_key$, using L60850,col_load$,col_load_ref$,~
                                                       eod goto find_done
            goto L60860
        find_next
            read #10, using L60850, col_load$, col_load_ref$,             ~
                                                       eod goto find_done
L60850:          FMT CH(5), POS(101), CH(5)
L60860:     if col_load$ <> load_no$ then goto find_done
            if jb% <> 0% then goto L60890
               goto L60920
L60890:     for i% = 1% to jb%
               if jb_load$(i%) = col_load_ref$ then goto find_next
            next i%
L60920:     jb% = jb% + 1%
            jb_load$(jb%) = col_load_ref$
            goto find_next
        find_done
            if jb% <> 0% then return
               jb% = 1%
               jb_load$(jb%) = load_no$
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
