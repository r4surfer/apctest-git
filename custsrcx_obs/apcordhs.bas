        REM *************************************************************~
            *              ( Modified - APCORDER )                      *~
            *   AAA   PPPP    CCC    OOO   RRRR   DDDD   H   H   SSSS   *~
            *  A   A  P   P  C   C  O   O  R   R  D   D  H   H  S       *~
            *  AAAAA  PPPP   C      O   O  RRRR   D   D  HHHHH    S     *~
            *  A   A  P      C   C  O   O  R   R  D   D  H   H      S   *~
            *  A   A  P       CCC    OOO   R   R  DDDD   H   H  SSSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCORDHS - Check Status of S.O. in History Database.      *~
            *            Note - No (DATASAVE)                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/15/93 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 09/23/97 ! Check Prior to creating '95' and '96     ! RHH *~
            *          !   History. Note uses Old Planning        !     *~
            * 11/13/97 ! Mod for upgrade to new Release R6.04.03  ! RHH *~
            *          !                                          !     *~
            *************************************************************

        dim                                                              ~
            bck_key$25,                  /* S.O. KEY                   */~
            bck_part$25,                 /* S.O. PART                  */~
            bck_desc$32,                 /* DESCRIPTION - FORECAST     */~
            vinyl_flag$1,                /*                            */~
            bck_ord$16,                  /*                            */~
            ord_tot$12,                  /*                            */~
            bck_rec$200,                 /* PARTIAL ORDER DETAIL REC   */~
            grp_due$6,                   /* Calculated Due Date        */~
            grp_route$5,                 /* Group Route Code           */~
            grp_cuscode$9,               /* Customer Number            */~
            sav_cust$9,                  /* Customer Number            */~
            grp_so$8,                    /* Group Sales Order Number   */~
            grp_dte$6,                   /* Date S.O. Created or Change*/~
            grp_sls$4,                   /* Salesman Code              */~
            grp_load$5,                  /* Group Load Number          */~
            grp_drop$2,                  /* Customer Drop Number       */~
            grp_status$1,                /* Group Status Code          */~
            grp_date$6,                  /* Group Status Date          */~
            grp_po$16,                   /* Group P.O. Number          */~
            grp_inv$8,                   /* Group Invoice Number       */~
            grp_chk$8,                   /* Group Check Number         */~
            grp_howship$10,              /* Special Instructions       */~
            grp_ord_stat$1,              /* (N)ew or (C)hanged S.O.    */~
            grp_filler$4,                /* Filler Area                */~
            grp_key$28,                  /* Primary Key                */~
            grp_key1$17,                 /* Alt Key 1                  */~
            grp_key2$19,                 /* Alt Key 2                  */~
            grp_key3$16,                 /* Alt Key 3                  */~
            grp_key4$8,                  /* Alt Key 4                  */~
            grp_key5$8,                  /* Alt_Key 5                  */~
            grp_rec$110,                 /* Group Record               */~
            mscdescr$30,                 /* Plowcode Description       */~
            sav_due$6,                   /* Save Due Date For Reports  */~
            sav_rte$5,                   /* Save Route Code For Reports*/~
            sav_cus$9,                   /* Save Customer For Reports  */~
            grp_dtee$8,                  /* Formatted Due Date         */~
            grp_dte1$8,                  /* Beginning date             */~
            grp_dte2$8,                  /* Ending date                */~
            grp_cus1$9,                  /* Beginning Customer         */~
            grp_cus2$9,                  /* Ending Customer            */~
            grp_dte3$8,                  /* Formatted Status Date      */~
            status$(15%)10,              /* Group Status Descriptions  */~
            scr$(10%)40,                 /* Status Text                */~
            scr_msg$50,                  /* Special Note Message       */~
            check_status$1,              /* Check Status Flag          */~
            cus_name$30,                 /* Customer Name              */~
            grp_route_desc$30,           /* Route Description          */~
            grp_sls_name$30,             /* Salesman Name              */~
            mod_desc$30,                 /* New or Modified Order      */~
            apc_desc$30,                 /* Load Description           */~
            apc_prod1$8,                 /* Planned Production Date    */~
            apc_comp1$8,                 /* Planned Completion Date    */~
            apc_loaded1$8,               /* Planned Load Date          */~
            grp_stat_desc$30,            /* Status Description         */~
            grp_due_dte1$8,              /* Formatted Group Due Date   */~
            grp_mod_dte$8,               /* Formatted Last Mod Date    */~
            grp_date_dte1$8,             /* Formatted Status Date      */~
            readkey$24,                  /* Gencodes Key               */~
            prt_due$8,                   /* For Report Printing        */~
            prt_date$8,                  /* For Report Printing        */~
            company$60,                  /* For Report Company Name    */~
            print_title$60,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
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

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) S.O. Status Lookup in History     "
            pname$ = "APCORDHS - Rev: R6.04"

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
            * #1  ! APCORDHS ! Group Sales Order Header File  (HISTORY) *~
            * #2  ! CUSTOMER ! Customer Master File                     *~
            * #3  ! BCKMSTHS ! S.O. Master File               (HISTORY) *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            * #5  ! BCKLINHS ! S.O. Line Item Detail File     (HISTORY) *~
            * #6  ! APCMSTHS ! Load Master File               (HISTORY) *~
            * #8  ! SLMMASTR ! Salesman Master File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,   "APCORDHS",                                     ~
                        varc,     indexed,  recsize = 110,               ~
                        keypos =    1, keylen =   28,                    ~
                        alt key  1, keypos =   12, keylen =  17,         ~
                            key  2, keypos =   29, keylen =  19, dup,    ~
                            key  3, keypos =   62, keylen =  16, dup,    ~
                            key  4, keypos =   78, keylen =   8, dup,    ~
                            key  5, keypos =   86, keylen =   8, dup


            select #2,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #3,  "BCKMSTHS",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #5,  "BCKLINHS",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #6,  "APCMSTHS",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =   5,                     ~
                        alt key  1, keypos =  233, keylen =  13

            select #8,  "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),1000%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),   0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),1000%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),   0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),1000%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),1000%, rslt$(6%))
            call "OPENCHCK" (#8, fs%(8%), f2%(8%),   0%, rslt$(8%))

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

            status$( 1) = "Order     "
            status$( 2) = "Assigned  "
            status$( 3) = "Planned   "
            status$( 4) = "Complete  "
            status$( 5) = "Inventory "
            status$( 6) = "Staging   "
            status$( 7) = "Loaded    "
            status$( 8) = "Shipped   "
            status$( 9) = "Invoiced  "
            status$(10) = "Paid      "
            status$(11) = "Invalid   "
            status$(12) = "Invalid   "
            status$(15) = "Invalid   "

            scr$( 1) = "****************************************"
            scr$( 2) = "*          Valid Status Codes          *"
            scr$( 3) = "*          ------------------          *"
            scr$( 4) = "* 0 - Order         6 -  Loaded        *"
            scr$( 5) = "* 1 - Assigned      7 -  Shipped       *"
            scr$( 6) = "* 2 - Planned       8 -  Invoiced      *"
            scr$( 7) = "* 3 - Complete      9 -  Paid          *"
            scr$( 8) = "* 4 - Inventory                        *"
            scr$( 9) = "* 5 - Staged                           *"
            scr$(10) = "****************************************"

            scr_msg$ = "Note: * - Item may be Entered to Retrieve Data?"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   6%
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
L10210:               if keyhit% = 14% then gosub print_report
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
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
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11120:     if cursor%(1%) > 5% then fieldnr% = 2%
            if cursor%(1%) > 10% then fieldnr% = cursor%(1%) - 9%
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
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
            errormsg$ = " "
L19090:     grp_dte1$ = "ALL" : grp_dte2$ = " "
            grp_cus1$ = "ALL" : grp_cus2$ = " "
            check_status% = 10%                         /* SET FOR ALL */
            gosub'102(1%)
               errormsg$ = " "
               if keyhit% = 1% then gosub startover
               if keyhit% = 1% then goto print_report
               if keyhit% = 16% then goto L19550
               if grp_dte1$ = "ALL" then goto L19270
                  if grp_dte2$ = " " then grp_dte2$ = grp_dte1$
                  call "DATEOK" (grp_dte1$, date%, errormsg$)
                  if date% = 0% then goto L19090
                  d1% = date%
                  call "DATEOK" (grp_dte2$, date%, errormsg$)
                  if date% = 0% then goto L19090
                  d2% = date%
                  if d2% >= d1% then goto L19270
                     errormsg$ = "INVALID DATE RANGE ENTERED "
                     goto L19090

L19270:     if grp_cus1$ = "ALL" then goto L19370
                  if grp_cus2$ = " " then grp_cus2$ = grp_cus1$
                  read #2,key = grp_cus1$, eod goto L19340
                  read #2,key = grp_cus2$, eod goto L19340
                  if grp_cus2$ >= grp_cus1$ then goto L19370
                     errormsg$ = "INVALID CUSTOMER RANGE ENTERED "
                     goto L19090
L19340:           errormsg$ = " INVALID CUSTOMER CODE ENTERED "
                  goto L19090

L19370:     if check_status$ <> " " then goto L19390
               check_status$ = "A"
L19390:     check_status% = 0%
            if check_status$ = "A" then goto L19470
               convert check_status$ to check_status%, data goto L19420
L19420:
               if check_status% = 0% or check_status% < 10% then         ~
                                                        goto L19470
                  errormsg$ = "INVALID STATUS CODE ENTERED "
                  goto L19090
L19470:     gosub'102(0%)
            if keyhit% = 1% then gosub startover
            if keyhit% = 1% then goto print_report
            if keyhit% = 16% then goto L19550
            if keyhit% <> 14% then goto L19370

            gosub generate_report

L19550: return clear all
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
         "Enter Customer and S.O,or S.O. No.,P.O. No., Inv No., Chk No.",~
         "Enter a Valid Load Number.    (Should not need to be Changed)",~
         "Enter a Valid Drop Number.    (Should not need to be Changed)",~
         "Enter a Status Code (0 thru 9)(Should not need to be Changed)",~
         "Enter a Status Date.          (Should not need to be Changed)",~
         "Enter Spec. Instr. (xxxxx - ) (Should not need to be Changed)"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, grp_cuscode$, grp_due$,    ~
                      grp_route$, grp_so$, grp_dte$, grp_sls$, grp_load$,~
                      grp_drop$, grp_line$, grp_line_dte$,               ~
                      grp_status$, grp_date$, grp_po$, grp_inv$,         ~
                      grp_chk$, grp_howship$, grp_ord_stat$, grp_filler$,~
                      grp_key$, grp_key1$, grp_key2$, grp_key3$,         ~
                      grp_key4$, grp_key5$,grp_dtee$, grp_dte1$,         ~
                      grp_dte2$, grp_cus1$, grp_cus2$, grp_dte3$,        ~
                      cus_name$, grp_due_dte1$, grp_mod_dte$, apc_prod1$,~
                      apc_comp1$, apc_loaded1$,       grp_date_dte1$,    ~
                      grp_route_desc$, grp_sls_name$, mod_desc$,         ~
                      apc_desc$, grp_stat_desc$, grp_rec$, ord_tot$
            grp_days% = 0%               /* Scheduled Delivery Days    */
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

        get_data

           get #1, using L35040,                                          ~
                                         /* APCORDER - File            */~
            grp_due$,                    /* Calculated Due Date        */~
            grp_route$,                  /* Group Route Code           */~
            grp_cuscode$,                /* Customer Number            */~
            grp_so$,                     /* Group Sales Order Number   */~
            grp_dte$,                    /* Date S.O. Created or Change*/~
            grp_sls$,                    /* Salesman Code              */~
            grp_cuscode$,                /* Customer Number            */~
            grp_load$,                   /* Group Load Number          */~
            grp_drop$,                   /* Customer Drop Number       */~
            grp_status$,                 /* Group Status Code          */~
            grp_date$,                   /* Group Status Date          */~
            grp_po$,                     /* Group P.O. Number          */~
            grp_inv$,                    /* Group Invoice Number       */~
            grp_chk$,                    /* Group Check Number         */~
            grp_howship$,                /* Special Instructions       */~
            grp_ord_stat$,               /* (N)ew or (C)hanged S.O.    */~
            grp_days%,                   /* Scheduled Delivery Days    */~
            grp_filler$                  /* Filler Area                */

            grp_due_dte1$ = grp_due$
            call "DATEFMT" (grp_due_dte1$)

            readkey$ = all(hex(00))
            readkey$ = "ROUTECODE" & grp_route$
            call "DESCRIBE" (#4, readkey$, grp_route_desc$, 0%, f1%(4))
            if f1%(4) = 0% then grp_route$ = "00000"

            call "DESCRIBE" (#8, grp_sls$, grp_sls_name$, 0%, f1%(8))

            grp_mod_dte$ = grp_dte$
            call "DATEFMT" (grp_mod_dte$)

            mod_desc$ = "(New Sales Order)"
         if grp_ord_stat$ = "C" then mod_desc$ = "(Modified Sales Order)"

            init(" ") apc_desc$, apc_prod1$, apc_comp1$, apc_loaded1$
            if grp_load$ = " " then goto L30580
               read #6,key = grp_load$, using L30530, apc_desc$,          ~
                             apc_prod1$, apc_comp1$, apc_loaded1$,       ~
                                         eod goto L30580
L30530:        FMT POS(6),CH(30),XX(6),CH(6),XX(6),CH(6),XX(6),CH(6)
               call "DATEFMT" (apc_prod1$)
               call "DATEFMT" (apc_comp1$)
               call "DATEFMT" (apc_loaded1$)

L30580:     cus_name$ = " "
            call "DESCRIBE" (#2, grp_cuscode$, cus_name$, 0%, f1%(2%))

            grp_date_dte1$ = grp_date$             /* STATUS DATE */
            call "DATEFMT" (grp_date_dte1$)

            status% = 0%
            convert grp_status$ to status%, data goto L30670

L30670:     grp_stat_desc$ = status$(status% + 1%)


        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "SHOSTAT" ("(Error)-Cannot Update History?") : stop
        return clear all
        goto inputmode

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* APCORDER - New File Layout */
L35040:     FMT CH(06),                  /* Due Date                   */~
                CH(05),                  /* Customer Route Code        */~
                CH(09),                  /* Customer Code              */~
                CH(08),                  /* S. O. Number               */~
                CH(06),                  /* Date S.O. Created or Change*/~
                CH(04),                  /* Salesman Code              */~
                CH(09),                  /* Customer Code              */~
                CH(05),                  /* Load Number                */~
                CH(02),                  /* Customer Drop Code         */~
                CH(01),                  /* Status Code                */~
                CH(06),                  /* Status Date                */~
                CH(16),                  /* P. O. Number               */~
                CH(08),                  /* Invoice Number             */~
                CH(08),                  /* Check Number               */~
                CH(10),                  /* Howship - Special Instruct */~
                CH(01),                  /* Order Status (N)ew/(C)hange*/~
                BI(2),                   /* SCHEDULED DELIVER DAYS     */~
                CH(04)                   /* Filler = 110               */

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
              on fieldnr% gosub L40250,         /* Customer Number   */   ~
                                               /* Sales Order No.   */   ~
                                               /* S.O Line Item No. */   ~
                                               /* Customer P.O. No. */   ~
                                               /* Customer Inv. No. */   ~
                                               /* Customer Chk No.  */   ~
                                L40250,         /* Customer Load No. */   ~
                                L40260,         /* Customer Drop No. */   ~
                                L40260,         /* Group Status      */   ~
                                L40250,         /* Group Status Date */   ~
                                L40240          /* Special Instructions */

              goto L40280

L40240:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40250:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40260:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40280:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Creation/Mod Date :",                        ~
               at (03,22), fac(hex(84)),   grp_mod_dte$         , ch(08),~
               at (03,35), fac(hex(84)),   mod_desc$            , ch(20),~
               at (03,57), "S.O. Net:",                                  ~
               at (03,67), fac(hex(94)),   ord_tot$             , ch(12),~
                                                                         ~
               at (04,02), "Production: ",                               ~
               at (04,15), fac(hex(94)), apc_prod1$             , ch(08),~
               at (04,25), "Completion: ",                               ~
               at (04,38), fac(hex(84)), apc_comp1$             , ch(08),~
               at (04,48), "Load      : ",                               ~
               at (04,61), fac(hex(84)), apc_loaded1$           , ch(08),~
                                                                         ~
               at (05,02), fac(hex(84)), scr_msg$               , ch(50),~
                                                                         ~
               at (06,02), "* Customer Number   :",                      ~
               at (06,25), fac(lfac$( 1)), grp_cuscode$         , ch(09),~
               at (06,40), fac(hex(84)), cus_name$              , ch(30),~
               at (07,02), "* Customer S.O. No. :",                      ~
               at (07,25), fac(lfac$( 1)), grp_so$              , ch(08),~
               at (07,35), "Line Item No.:",                             ~
               at (07,50), fac(lfac$( 1)), grp_line$            , ch(02),~
               at (08,02), "* Customer P.O. No. :",                      ~
               at (08,25), fac(lfac$( 1)), grp_po$              , ch(16),~
               at (09,02), "* APC Invoice No.   :",                      ~
               at (09,25), fac(lfac$( 1)), grp_inv$             , ch(08),~
               at (10,02), "* APC Check No.     :",                      ~
               at (10,25), fac(lfac$( 1)), grp_chk$             , ch(08),~
                                                                         ~
               at (11,02), "  Customer Load No. :",                      ~
               at (11,25), fac(lfac$( 2)), grp_load$            , ch(05),~
               at (11,40), fac(hex(84)), apc_desc$              , ch(30),~
                                                                         ~
               at (12,02), "  Customer Drop No. :",                      ~
               at (12,25), fac(lfac$( 3)), grp_drop$            , ch(02),~
                                                                         ~
               at (13,02), "  Current Status    :",                      ~
               at (13,25), fac(lfac$( 4)), grp_status$          , ch(01),~
               at (13,40), fac(hex(94)),   grp_stat_desc$       , ch(30),~
                                                                         ~
               at (14,02), "  Current Status DTE:",                      ~
               at (14,25), fac(lfac$( 5)), grp_date_dte1$       , ch(08),~
                                                                         ~
               at (15,02), "  Spec. Instructions:",                      ~
               at (15,25), fac(lfac$( 6)), grp_howship$         , ch(10),~
                                                                         ~
               at (16,02), "  Group Due Date    :",                      ~
               at (16,25), fac(hex(84)),   grp_due_dte1$        , ch(08),~
               at (17,02), "  Customer Route    :",                      ~
               at (17,25), fac(hex(84)),   grp_route$           , ch(05),~
               at (17,40), fac(hex(84)),   grp_route_desc$      , ch(30),~
               at (18,02), "  Salesman Code     :",                      ~
               at (18,25), fac(hex(84)),   grp_sls$             , ch(04),~
               at (18,40), fac(hex(84)),   grp_sls_name$        , ch(30),~
               at (19,02), "  Made On Date      :",                      ~
               at (19,25), fac(hex(84)),   grp_line_dte$        , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 8 then goto L40940
                  gosub lookup_customer
                  goto L40280

L40940:        if keyhit% <> 15 then goto L40980
                  call "PRNTSCRN"
                  goto L40280

L40980:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41170     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (8)Look-Up Customer    " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffff08ffffffffff0e0f1000)
            if fieldnr% = 1% then L41130
                str(pf$(3),18,22) = " "  :  str(pfkeys$, 8,1) = hex(ff)
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L41130:     if fieldnr% > 1% then L41150
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41150:     return

L41170: if fieldnr% > 0% then L41260  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return
L41260:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Report Selection Screen                                   *~
            *************************************************************

        deffn'102(fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L41500          /* Select Option     */

              goto L41540

L41500:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41540:     accept                                                       ~
               at (01,02),                                               ~
                  "Report Selection (1) Criteria",                       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Starting Due Date or All",                   ~
               at (06,30), fac(lfac$( 1)), grp_dte1$            , ch(08),~
                                                                         ~
               at (06,40), "Ending Due Date : ",                         ~
               at (06,60), fac(lfac$( 1)), grp_dte2$            , ch(08),~
                                                                         ~
               at (07,02), "Starting Customer or All",                   ~
               at (07,30), fac(lfac$( 1)), grp_cus1$            , ch(09),~
                                                                         ~
               at (07,40), "Ending Customer : ",                         ~
               at (07,60), fac(lfac$( 1)), grp_cus2$            , ch(09),~
                                                                         ~
               at (08,02), "Status Code or (A)ll",                       ~
               at (08,30), fac(lfac$( 1)), check_status$        , ch(01),~
                                                                         ~
               at (10,21), fac(hex(84)), scr$( 1)               , ch(40),~
               at (11,21), fac(hex(84)), scr$( 2)               , ch(40),~
               at (12,21), fac(hex(84)), scr$( 3)               , ch(40),~
               at (13,21), fac(hex(84)), scr$( 4)               , ch(40),~
               at (14,21), fac(hex(84)), scr$( 5)               , ch(40),~
               at (15,21), fac(hex(84)), scr$( 6)               , ch(40),~
               at (16,21), fac(hex(84)), scr$( 7)               , ch(40),~
               at (17,21), fac(hex(84)), scr$( 8)               , ch(40),~
               at (18,21), fac(hex(84)), scr$( 9)               , ch(40),~
               at (19,21), fac(hex(84)), scr$(10)               , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L41980
                  call "PRNTSCRN"
                  goto L41540

L41980:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
            inpmessage$ = "Enter Report Criteria: Beg/End Due Date, " &  ~
                          "Customer, or Specific Status"
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            if fieldnr% = 0% then goto L42140
               str(pf$(1),64%) = " "  : str(pfkeys$,14%,1%) = hex(ff)
L42140:     return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50210,         /* Customer Code         */ ~
                                             /* Sales Order No.       */ ~
                                             /* Customer P.O. Number  */ ~
                                             /* Customer Invoice No.  */ ~
                                             /* Customer Check No.    */ ~
                              L51140,         /* Load Number           */ ~
                              L51230,         /* Customer Drop Number  */ ~
                              L51330,         /* Current Status        */ ~
                              L51470,         /* Status Date           */ ~
                              L51530          /* Special Instructions  */

            return

L50210: REM Customer Number                       GRP_CUSCODE$
           cus_name$ = " "
           if grp_cuscode$ = " " then goto L50360
              if grp_cuscode$ <> "?" then goto L50300
                 grp_cuscode$ = " "
                 mscdescr$ = hex(06) & "Select Customer Code"
          call "PLOWCODE" (#2, grp_cuscode$, mscdescr$, 0%, .30, f1%(2))
              if f1%(2) = 0 then goto L50320
              goto L50530
L50300:     read #2,key = grp_cuscode$, eod goto L50320
            goto L50510
L50320:        errormsg$ = "Must Enter a Valid Customer Code "
               grp_cuscode$, grp_so$, grp_po$, grp_inv$, grp_chk$ = " "
            return

L50360: REM Check for S.O. Number
           if grp_so$ = " " then goto L50700
              convert grp_so$ to grp_so%, data goto L50480

              convert grp_so% to grp_so$, pic(00000000)

              readkey$ = all(hex(00))
              str(readkey$,1%,8%) = grp_so$
              if grp_line$ = " " then goto L50440
                 convert grp_line$ to grp_line%, data goto L50480

                 convert grp_line% to grp_line$, pic(00)
                 str(readkey$,9%,2%) = grp_line$

L50440:
              if grp_so$ <> str(readkey$,1%,8%) then goto L50480
                 call "DATEFMT" (grp_line_dte$)
                 if grp_line$ = " " then grp_line_dte$ = " "
                 goto L50530

L50480:    errormsg$ = "Either an Invalid S.O., or (One Not Planned)"
           goto L50670

L50510: REM Customer S. O. Number                 GRP_SO$
           if grp_so$ = " " then goto L50700
L50530:       convert grp_so$ to grp_so%, data goto L50660

              convert grp_so% to grp_so$, pic(00000000)

              grp_key1$ = all(hex(00))
              str(grp_key1$,1%,9%)  = grp_cuscode$
              str(grp_key1$,10%,8%) = grp_so$
              read #1,key 1% = grp_key1$, eod goto L50660
              gosub get_data
              gosub sales_order
              convert ord_tot to ord_tot$, pic($###,###.##-)
              fieldnr% = 10%
        return
L50660:    errormsg$ = "Must Enter a Customer Code and S.O. Number"
L50670:    grp_cuscode$, grp_so$, grp_po$, grp_inv$, grp_chk$ = " "
           grp_line$, grp_line_dte$  = " "
        return

L50700: REM Customer P.O. Number                  GRP_PO$
           if grp_po$ = " " then goto L50830
              grp_key3$ = grp_po$
              read #1,key 3% = grp_key3$, eod goto L50790
              gosub get_data
              gosub sales_order
              convert ord_tot to ord_tot$, pic($###,###.##-)
              fieldnr% = 10%
        return
L50790:    errormsg$ = "Customer P.O. Number Not on File."
           grp_cuscode$, grp_so$, grp_po$, grp_inv$, grp_chk$ = " "
           grp_line$, grp_line_dte$  = " "
        return

L50830: REM Customer Invoice Number               GRP_INV$
           if grp_inv$ = " " then goto L50970
              grp_key4$ = all(hex(00))
              grp_key4$ = grp_inv$
              read #1,key 4% = grp_key4$, eod goto L50930
              gosub get_data
              gosub sales_order
              convert ord_tot to ord_tot$, pic($###,###.##-)
              fieldnr% = 10%
        return
L50930:    errormsg$ = "Customer Invoice Number Not on File."
           grp_cuscode$, grp_so$, grp_po$, grp_inv$, grp_chk$ = " "
           grp_line$, grp_line_dte$  = " "
        return

L50970: REM Customer Check Number                 GRP_CHK$
           if grp_chk$ = " " then goto L51100
              grp_key5$ = all(hex(00))
              grp_key5$ = grp_chk$
              read #1,key 5% = grp_key5$, eod goto L51070
              gosub get_data
              gosub sales_order
              convert ord_tot to ord_tot$, pic($###,###.##-)
              fieldnr% = 10%
        return
L51070:    errormsg$ = "Customer Check Number Not on File."
           grp_cuscode$, grp_so$, grp_po$, grp_inv$, grp_chk$ = " "
           grp_line$, grp_line_dte$  = " "
        return
L51100:    errormsg$ = "Must Enter Valid Selection Data."
           grp_cuscode$, grp_so$, grp_po$, grp_inv$, grp_chk$ = " "
           grp_line$, grp_line_dte$  = " "
        return

L51140: REM Customer S.O. Load Number             GRP_LOAD$
           if grp_load$ = " " then return
               read #6,key = grp_load$, using L51158, apc_desc$,          ~
                             apc_prod1$, apc_comp1$, apc_loaded1$,       ~
                                         eod goto L51190
L51158:        FMT POS(6),CH(30),XX(6),CH(6),XX(6),CH(6),XX(6),CH(6)
               call "DATEFMT" (apc_prod1$)
               call "DATEFMT" (apc_comp1$)
               call "DATEFMT" (apc_loaded1$)

        return
L51190:       errormsg$ = "Invalid Load Number "
        init(" ") grp_load$,apc_desc$,apc_prod1$,apc_comp1$, apc_loaded1$
        return

L51230: REM Customer Drop Number                  GRP_DROP$
              if grp_drop$ = " " then return
              convert grp_drop$ to grp_drop%, data goto L51290

              convert grp_drop% to grp_drop$, pic(00)
        return
L51290:       errormsg$ = "Invalid Drop Number "
              grp_drop$ = " "
        return

L51330: REM Group Status Code                     GRP_STATUS$
            if grp_status$ <> " " then goto L51360
               goto L51440
L51360:     status% = 14%
            convert grp_status$ to status%, data goto L51380
L51380:
            status% = status% + 1%
            if status% < 1% or status% > 10% then goto L51440
               grp_stat_desc$ = status$(status%)
               return

L51440:     errormsg$ = "Must Enter a Valid Status CODE "
        return

L51470: REM Group Status Date                     GRP_DATE$
            if grp_date_dte1$ = " " then return
               date% = 0%
               call "DATEOK" (grp_date_dte1$, date%, errormsg$ )
        return

L51530: REM Special Instructions                  GRP_HOWSHIP$
         return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %######## ########                   ############################~
        ~################################                        APCORDER:

L55080: %USER ID: ###                        ############################~
        ~################################                      PAGE: #####
                                                   /* COLUMN 1 HEADER */
L55110: %DUE DATE ROUTE  CUSTOMER <------- NAME ----------> S. O. NO   S.~
        ~O. TOTAL DOL'S INVOICE  CHECK    STAT DTE STATUS     LOAD  SPECIA~
        ~L
L55140: %-------- ----- --------- ------------------------- -------- - --~
        ~-------------- -------- -------- -------- ---------- ----- ------~
        ~--
                                                   /* DETAIL 1      */
L55180: %######## ##### ######### ######################### ######## # ( ~
        ~$###,###.##- ) ######## ######## ######## ########## ##### ######~
        ~##


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            page_no% = 0%
            lcnt%    = 99%

            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCORD", " ", 0%, 0%)
            select printer (134)
            print_title$ = "Order Group By Due Date"
            call "FMTTITLE" (print_title$, " ", 12%)
        return

        close_printer
            call "SETPRNT" ("APCORD", " ", 0%, 1%)
        return

        generate_report                  /* REPORT FROM PRIMARY KEY */
          call "SHOSTAT" ("Print Group by 'Due Date'")
          gosub select_printer
          grp_key$ = all(hex(00)) : sav_cust$ = " "
          if grp_dte1$ = "ALL" then L60300
             call "DATUNFMT" (grp_dte1$)
             call "DATUNFMT" (grp_dte2$)
             str(grp_key$,1%,6%) = str(grp_dte1$,1%,6%)
L60300:      read #1,key > grp_key$, using L60350,grp_rec$,eod goto done_1
               goto L60360
        read_next
             read #1, using L60350, grp_rec$, eod goto done_1

L60350:          FMT CH(110)
L60360:      grp_due$     = str(grp_rec$,1%,6%)
             grp_cuscode$ = str(grp_rec$,12%,9%)
             grp_status$  = str(grp_rec$,55%,1%)
               status% = 0%
               convert grp_status$ to status%, data goto L60410
L60410:
               if grp_dte1$ <> "ALL" and grp_due$ > str(grp_dte2$,1%,6%) ~
                                                         then goto done_1
               if check_status$ <> "A" and check_status% <> status% then ~
                                                           goto read_next
               if grp_cus1$ = "ALL" then goto L60490
                  if grp_cuscode$ < grp_cus1$ or grp_cuscode$ > grp_cus2$~
                                                      then goto read_next
L60490:        grp_route$ = str(grp_rec$,7%,5%)
               grp_so$    = str(grp_rec$,21%,8%)
               grp_load$  = str(grp_rec$,48%,5%)
               grp_date$  = str(grp_rec$,56%,6%)
               grp_po$    = str(grp_rec$,62%,16%)
               grp_inv$   = str(grp_rec$,78%,8%)
               grp_chk$   = str(grp_rec$,86%,8%)
               grp_howship$ = str(grp_rec$,94%,10%)
               if grp_inv$ = "99999999" then grp_inv$ = " "
               if grp_chk$ = "99999999" then grp_chk$ = " "
               gosub print_detail_1
               goto read_next
        done_1
               gosub close_printer
        return

        print_header_1
          sav_due$, sav_cus$, sav_rte$ = " "
          page_no% = page_no% + 1%
          print page
          print using L55050, date$, rpt_time$, company$
          print using L55080, userid$, print_title$, page_no%
          print
          print using L55110
          print using L55140
          lcnt% = 5%
        return

        print_detail_1
          if lcnt% > 60% then gosub print_header_1
             if sav_cust$ = grp_cuscode$ then goto L60820
             call "DESCRIBE" (#2, grp_cuscode$, cus_name$, 0%, f1%(2%))
             sav_cust$ = grp_cuscode$
L60820:    prt_due$ = " "
           gosub check_print_1

           prt_date$ = grp_date$ & "  "
           call "DATEFMT" (prt_date$)
           gosub lookup_so
           gosub sales_order
           print using L55180, prt_due$, grp_route$, grp_cuscode$,        ~
                 str(cus_name$,1%,25%), grp_so$, vinyl_flag$, ord_tot,   ~
                 grp_inv$, grp_chk$, prt_date$, status$(status%+1%),     ~
                 grp_load$, str(grp_howship$,1%,8%)
          lcnt% = lcnt% + 1%
        return

        check_print_1
          if sav_due$ = grp_due$ then return
             prt_due$ = grp_due$
             sav_due$ = grp_due$
             call "DATEFMT" (prt_due$)
             if lcnt% = 5% then return
                print
                lcnt% = lcnt% + 1%
        return

        lookup_so
             vinyl_flag$ = " "
             bck_key$, readkey$ = all(hex(00))
             str(bck_key$,1%,16%) = grp_so$
             read #5,key > bck_key$, using L61120, bck_part$,             ~
                                                           eod goto L61210
L61120:        FMT XX(31), CH(25)
            if str(bck_part$,1%,1%) <> "3" then goto L61180
               if str(bck_part$,1%,3%) <> "312" then return
                  vinyl_flag$ = "@"
                  return
L61180:     readkey$ = "FORECAST " & str(bck_part$,1%,3%)
            call "DESCRIBE" (#4, readkey$, bck_desc$, 0%, f1%(4%))
            if f1%(4%) = 1% then vinyl_flag$ = str(bck_desc$,1%,1%)
L61210: return

        sales_order
             ord_disc, ln_disc, ord_tot = 0.0
             bck_key$ = " "
             str(bck_key$,1%,9%)  = grp_cuscode$
             str(bck_key$,10%,8%) = grp_so$
             read #3,key = bck_key$, using L61290,ord_disc, eod goto L61300
L61290:         FMT POS(859), PD(14,4)
L61300:
             bck_key$ = all(hex(00))
             str(bck_key$,1%,8%) = grp_so$
             read #5,key > bck_key$, using L61370, bck_rec$,eod goto L61500
             goto L61380
        next_sales_order
             read #5, using L61370, bck_rec$, eod goto L61500
L61370:          FMT CH(200)
L61380:      bck_ord$ = str(bck_rec$,10%,16%)
             get str(bck_rec$,93%,8%) using L61400, unit_qty
L61400:        FMT PD(14,4)
             get str(bck_rec$,165%,16%) using L61420, unit_price, ln_disc
L61420:        FMT 2*PD(14,4)
             if bck_ord$ <> grp_so$ then goto L61500
                total_price =  round(  unit_price * unit_qty, 2)
                                         /* CALCULATE LINE DISCOUNT */
                discamt     =  round( total_price * ln_disc * .01, 2)
                total_price =  round( total_price - discamt, 2)
                ord_tot     = round(ord_tot + total_price, 2)
             goto next_sales_order
L61500:  discamt =  round( ord_tot * ord_disc * .01, 2)
         ord_tot =  round( ord_tot - discamt, 2)
        REM   CONVERT ORD_TOT TO ORD_TOT$, PIC($###,###.##-)
        return

        lookup_customer
          grp_cuscode$, cus_name$ = " "
          mscdescr$ = hex(06) & "Select Customer Code"
          call "PLOWCODE" (#2, grp_cuscode$, mscdescr$, 0%, .30, f1%(2))
          if f1%(2) = 0 then goto L61640

          read #2,key = grp_cuscode$, eod goto L61640
          get #2, using L61630, cus_name$
L61630:      FMT POS(10), CH(30)
L61640: return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
