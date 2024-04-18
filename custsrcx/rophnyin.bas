        REM *************************************************************~
            *                                                           *~
            *  RRRR    OOO   PPPP   H   H  N   N  Y   Y  IIIII  N   N   *~
            *  R   R  O   O  P   P  H   H  NN  N  Y   Y    I    NN  N   *~
            *  RRRR   O   O  PPPP   HHHHH  N N N   YYY     I    N N N   *~
            *  R   R  O   O  P      H   H  N  NN    Y      I    N  NN   *~
            *  R   R   OOO   P      H   H  N   N    Y    IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ROPHNYIN - This program allows manual entry and edit of   *~
            *            the average usage and standard deviation       *~
            *            fields.                                        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/04/87 ! Original                                 ! LKM *~
            * 05/08/89 ! Merge of CMS/CMSI                        ! MJB *~
            *          !  - Replaced GENCODES with ROPCLASS       !     *~
            *          !  - Removed ROP Class from GET & PUT to   !     *~
            *          !     HNYMASTR file.                       !     *~
            *          !  - Changed test for Class 'A' to be test !     *~
            *          !     test for Critical Ration Test Flag.  !     *~
            * 05/11/89 ! Added 2 Report options                   ! MJB *~
            * 04/06/94 ! Additional Mods for APC ROP System       ! RHH *~
            * 11/21/97 ! Mod for Upgrade to new Release R6.04.03  ! RHH *~
            * 06/11/07 ! (AWD001) mod to add min / max            ! CMG *~
            *************************************************************

        dim                                                              ~
            rop_calc$1,                  /* Calc ROP Y or N            */~
            rop_eq$1, rop_eq_d$25,       /* ROP Equation Number        */~
            rop_source$1,rop_source_d$10,/* Source C=Calc, M=Manual    */~
            u_id$3,                      /* User Id Last Change        */~
            clsdescr$30,                 /* For PLOWCODE               */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr_map(26%),              /* For PLOWCODE               */~
            edtmessage$79,               /* Edit screen message        */~
            eoq$10,                      /* Economic Order Qty         */~
            errormsg$79,                 /* Error message              */~
            ess$10,                      /* Econmic Safety Stock       */~
            hdr$(4%)132,                 /* For PLOWCODE               */~
            i$(24%)80,                   /* Screen Image               */~
            incl(2%),                    /* for PLOWCODE               */~
            incl$(2%),                   /* for PLOWCODE               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            part$25,                     /* Part Number                */~
            partdescr$32,                /* Part Description           */~
            pclass$4,                    /* Part Class                 */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            pf12$17,                     /* PF 12 Screen Literal       */~
            pfkeys$16,                   /* PF Keys                    */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rop$10,                      /* Re-order Point             */~
            ropdate$6, rop_date$8,       /* Date ROP last updated      */~
            rptdescr$30,                 /* For PLOWCODE               */~
            stddev$10,                   /* Standard Deviation         */~
            svrop$10,                    /* Saved ROP                  */~
            pp_lead$10, ss_usage$10,     /* HNYMASTR - Part Lead Time  */~
            usage$10, pp_usage$10,       /* STD Usage & SS Usage / Day */~
            scrtime$8,                   /* Screen Time                */~
            userid$3,                    /* Current User Id            */~
            min$10,                      /* Min Quantity     AWD001    */~ 
            max$10                       /* Max Quantity     AWD001    */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) R.O.P. Parameter Maintenance      "
            pname$ = "ROPHNYIN - Rev: R6.04"

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
            * # 1 ! ROPHNY   ! File containing part specific ROP data   *~
            * # 2 ! HNYMASTR ! Inventory Master File                    *~
            * # 3 ! SYSFILE2 ! System File                              *~
            * # 4 ! ROPCLASS ! ROP Class Codes                          *~
            *************************************************************

            select # 1, "ROPHNY",                                        ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =  1,   keylen =  25,                     ~
                        alt key  1, keypos =  104, keylen = 4, dup

            select # 2, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select # 3, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select # 4, "ROPCLASS",                                      ~
                        varc,     indexed,  recsize =  50,               ~
                        keypos = 1,    keylen =  4

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 250%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),   0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),   0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),   0%, rslt$(4%))

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

            str(readkey$,1%,9%) = "ROP PARAM"
            read #3,key = readkey$, eod goto L09150
               goto L09220

L09150:        kh% = 2
               call "ASKUSER" (kh%, "SYSTEM PARAMETER RECORD MISSING",   ~
           "The System Parameter record must be set up through ROPSYSIN",~
               "Press any key to exit this program", " ")
               goto L65000
                                 /* Load Safety Stock Zero Adj Percent */
                                 /* Load ROP Std Deviation Percent     */
L09220:     get #3, using L09230, smco, devratio
L09230:         FMT POS(21), PD(14,7), POS(61), PD(14,4)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            editmode = 0
            pf4$ = " " : pf5$ = " " : pf16$ = "(16)Exit Program"
            pf12$ = " "
            pfkeys$ = hex(0001040cff0f1009ffffff0a0b)
            gosub L29000

            for fieldnr% = 1% to 7%
                if fieldnr% > 1% then pf4$ = "(4)Previous Field"
L10150:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                    if enabled% = 0% then L10410
L10170:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                    if keyhit%  =  1% then gosub startover
                    if keyhit% <>  4% then       L10250
L10200:                 fieldnr% = max(1%, fieldnr% - 1%)
                        gosub'051(fieldnr%)
                        if enabled% = 1% then L10170
                        if fieldnr% = 1% then L10150
                        goto L10200
L10250:             if keyhit% <> 9 then L10370
                        if fieldnr% <> 1% then L10170
                        init (hex(00)) plowkey$ : init(" ") hdr$()
                        partdescr$ = hex(06) & "ROP Part Records"
                        call "PLOWCODE" (#1,part$,partdescr$,8000%,0.32, ~
                           f1%(1%), hdr$(), 0, -1.0026, incl(), incl$(), ~
                            " ", " ", #2%)
                        if f1%(1%) = 1% then L10350
                            errormsg$ = "ROP Part Record Not Found"
                            goto L10170
L10350:                 call "PUTPAREN" (partdescr$)
                        goto L10410
L10370:             if keyhit% = 10% then gosub print_by_part
                    if keyhit% = 11% then gosub print_by_class
                    if keyhit% = 16% and fieldnr% = 1% then exit_program
                    if keyhit% <>  0% then       L10170
L10410:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                    if errormsg$ = " " then L10440
                        if skipfield = 0 then L10170
L10440:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            editmode = 1
            pf4$  = " "
            pf5$  = " "
            if onfile = 1 then pf12$ = "(12)Delete Record"
            pf9$ = " "
            pf16$ = "(16)Save Data"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 12%  and onfile = 1 then delete_record
                  if keyhit%  = 10% then gosub print_by_part
                  if keyhit%  = 11% then gosub print_by_class
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
            fieldnr% = cursor%(1) - 5%
            if fieldnr% < 2% or fieldnr% > 8% then editpg1
            if fieldnr% > 5% then fieldnr% = fieldnr% - 1%
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
                  pf4$, pf5$, pf16$ = " "
L11290:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11290
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ = " " then L11350
                     if skipfield = 0 then L11290
L11350:     goto editpg1

        delete_record
L11380:     keyhit% = 2%
            call "ASKUSER" (keyhit%, "*** DELETE RECORD? ***", "Press Ret~
        ~urn To Delete This Record", "- OR -","Press PF1 To Cancel Delete ~
        ~& Return")
            if keyhit% = 1% then editpg1
            if keyhit% <> 0% then L11380
            delete #1
            goto inputmode

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20170,         /* Part Number        */    ~
                              L20201,         /* Part Class         */    ~
                              L20210,         /* Safety Stock       */    ~
                              L20250,         /* Re-order Point     */    ~
                              L20290,         /* Economic Order Qty */    ~
                              L20370,         /* Standard Deviation */    ~
/*AWD001*/                    L20380          /* Min/Max            */
            return

L20170: REM Def/Enable SystemCode/Part Number      SYS$/PART$
            inpmessage$ = "Enter Part Number"
            return

L20201: REM Def/Enable Part Class                  PCLASS$
            inpmessage$ = "Enter Part ROP Class"
            return

L20210: REM Def/enable Economic Safety Stock       ESS$
            inpmessage$ = "Enter Safety Stock"
            return

L20250: REM Def/Enable Re-order Point              ROP$
            inpmessage$ = "Enter Re-order Point"
            return

L20290: REM Def/Enable Economic Order Qty          EOQ$
            inpmessage$="Enter Economic Order Quantity and Average Usage/~
        ~Day"
            return

L20370: REM Def/Enable Standard Deviation          STDDEV$
            inpmessage$ = "Enter ROP Standard Deviation Percent?"
            return


/*(AWD001)*/
L20380:  REM Min/Max Reorder Quantity              MIN$/MAX$
            inpmessage$ = "Enter Min/Max Quantities?"
            return
/*(\AWD001)*/
L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$,                            ~
                      ess$                   , /* Safety Stock       */  ~
                      eoq$                   , /* Economic Order Qty */  ~
                      pclass$                , /* Part Class         */  ~
                      clsdescr$              , /* Part ROP Cls Desc  */  ~
                      part$                  , /* Part Number        */  ~
                      partdescr$             , /* Part Description   */  ~
                      rop$                   , /* Re-order Point     */  ~
                      stddev$                , /* Standard Deviatio  */  ~
                      usage$                 , /* Average Usage      */  ~
                      pp_lead$               , /* Part Lead time Days*/  ~
                      ss_usage$              , /* SS Part Usage/Day  */  ~
                      pp_usage$              , /* STD Part Usage/Day */  ~
                      rop_calc$              , /* Calc ROP Y or N    */  ~
                      rop_eq$                , /* ROP Eq. Number     */  ~
                      rop_eq_d$              , /* Eq. Description    */  ~
                      rop_date$              , /* Last ROP Calc      */  ~
                      rop_source$            , /* Source C or M      */  ~
                      rop_source_d$          , /* Source Description */  ~
                      u_id$                  , /* Last Change User Id*/  ~
                      min$                   , /* Min Qty AWD001     */  ~ 
                      max$                     /* Max Qty AWD001     */

            onfile = 0
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
            rec% = 0%
            get #1, using L30100, rop, eoq, usage, stddev, ropdate$,      ~
                                      pclass$, rop_source$, u_id$,        ~
                                      min$, max$         /* AWD001 */

L30100:     FMT POS(26), 3*PD(14,4), PD(14,7), POS(98), CH(6), CH(04),   ~
                CH(1), CH(3), POS(117), CH(10), CH(10)

            call "CONVERT" (rop, -2.4, rop$)
            call "CONVERT" (eoq, -2.4, eoq$)
            gosub lookup_part
            gosub lookup_rop_class
            gosub calc_std_usage
            call "CONVERT" (usage, -2.4, usage$)
            call "CONVERT" (stddev, -2.4, stddev$)
            rop_date$ = ropdate$
            call "DATEFMT" (rop_date$)

            if rop_source$ <> "C" then rop_source_d$ = "  Manual  "      ~
                                  else rop_source_d$ = "Calculated"

/* (AWD001) */
            convert min$ to min_qty, data goto bad_min

bad_min

            convert max$ to max_qty, data goto bad_max

bad_max

             convert min_qty to min$, pic(#########0)

             convert max_qty to max$, pic(#########0)


            onfile = 1
            rec% = 1%
            goto editpg1

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            rop_source$ = "M"
            if rec% = 1% then goto L31160
               readkey$ = str(part$)
           write #1, using L31120, part$, rop, eoq, usage, stddev, usage, ~
                                  0, 0, 0, stddev, ropdate$, pclass$,    ~
                                  rop_source$, userid$, min$, max$, " "
L31120:      FMT CH(25), 2*PD(14,4), PD(14,4), PD(14,7), 4*PD(14,4),     ~
                 PD(14,7), CH(6), CH(4), CH(1), CH(3), CH(10), CH(10), CH(125)
            goto L31190

L31160:     put #1, using L30100, rop, eoq, usage, stddev, ropdate$,      ~
                                 pclass$, rop_source$, userid$, min$, max$
            rewrite #1
L31190:     read #2,hold,key = part$, eod goto L31230
               put #2 using L31210, ess
L31210:          FMT POS(318), PD(14,4)
            rewrite #2
L31230:     return

        REM *************************************************************~
            *      P R I N T   R O P   P A R T S   B Y   P A R T        *~
            *-----------------------------------------------------------*~
            * Call PLOWCODE to Print ROP Parts by Part Report           *~
            *************************************************************
        print_by_part
            plowkey$ = " "  :  mat descr_map = zer
            rptdescr$ = hex(06) & "ROP PARTS BY PART NUMBER"
            incl(1%) = 0  :  incl$(1%) = " "
            hdr$(1%) = "                                              "& ~
                      "                               ROP          EO" & ~
                      "Q    Std Usage      Std Dev          ESS"

                     /*xxxxxxxxxxxxxxxxxxxxxxxxx    xxxx    xxxxxxxxx*/
                     /*xxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxx   xxxxxxxxx*/
                     /*x   xxxxxxxxxx   xxxxxxxxxx   xxxxxxxxxx*/

            hdr$(4%) = "Part Number               ROP CLass  Descripti"& ~
                      "on                         Old ROP      Old EO" & ~
                      "Q    Old Usage      Old Dev      Old ESS"

                     /*                                              */
                     /*                        xxxxxxxxxx   xxxxxxxxx*/
                     /*x   xxxxxxxxxx   xxxxxxxxxx   xxxxxxxxxx*/

        /* 1st Detail Line */
            descr_map( 1%) =    1.25 : descr_map( 2%) =   1.0    /*Part*/
            descr_map( 3%) =  104.04 : descr_map( 4%) =  30.0    /*Cls */
            descr_map( 5%) =  -26.32 : descr_map( 6%) =  38.0    /*Desc*/
            descr_map( 7%) =   26.08 : descr_map( 8%) =  71.1044 /*ROP */
            descr_map( 9%) =   34.08 : descr_map(10%) =  84.1044 /*EOQ */
            descr_map(11%) =   42.08 : descr_map(12%) =  97.1044 /*Use */
            descr_map(13%) =   50.08 : descr_map(14%) = 110.1074 /*Dev */
            descr_map(15%) = -318.08 : descr_map(16%) = 123.1044 /* SS */

        /* 2nd Detail Line */
            descr_map(17%) = 66.08 : descr_map(18%) = 1071.1044 /*ROP */
            descr_map(19%) = 74.08 : descr_map(20%) = 1084.1044 /*EOQ */
            descr_map(21%) = 58.08 : descr_map(22%) = 1097.1044 /*Use */
            descr_map(23%) = 90.08 : descr_map(24%) = 1110.1074 /*Dev */
            descr_map(25%) = 82.08 : descr_map(26%) = 1123.1044 /* SS */


            call "PLOWCODE" (#1, plowkey$, rptdescr$, -9000%, 0.3,       ~
                             f1%(1), hdr$(), 0, 0, incl(), incl$(),      ~
                             "r", " ", #2, descr_map())
            return


        REM *************************************************************~
            *      P R I N T   R O P   P A R T S   B Y   C L A S S      *~
            *-----------------------------------------------------------*~
            * Call PLOWCODE to Print ROP Parts by Class Report          *~
            *************************************************************
        print_by_class
            plowkey$ = " "  :  mat descr_map = zer
            rptdescr$ = hex(06) & "ROP PARTS BY ROP PART CLASS"
            incl(1%) = 0  :  incl$(1%) = " "

            hdr$(1%) = "                                              "& ~
                      "                               ROP          EO" & ~
                      "Q    Std Usage      Std Dev     Sfty Stk"

                     /*   xxxx    xxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxx*/
                     /*xxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxx   xxxxxxxxx*/
                     /*x   xxxxxxxxxx   xxxxxxxxxx   xxxxxxxxxx*/

            hdr$(4%) = "ROP Class  Part Number               Descripti"& ~
                      "on                         Old ROP      Old EO" & ~
                      "Q    Old Usage      Old Dev      Old ESS"

                     /*                                              */
                     /*                        xxxxxxxxxx   xxxxxxxxx*/
                     /*x   xxxxxxxxxx   xxxxxxxxxx   xxxxxxxxxx*/

        /* 1st Detail Line */
            descr_map( 1%) =  104.04 : descr_map( 2%) =   4.0    /*Cls */
            descr_map( 3%) =    1.25 : descr_map( 4%) =  12.0    /*Part*/
            descr_map( 5%) =  -26.32 : descr_map( 6%) =  38.0    /*Desc*/
            descr_map( 7%) =   26.08 : descr_map( 8%) =  71.1044 /*ROP */
            descr_map( 9%) =   34.08 : descr_map(10%) =  84.1044 /*EOQ */
            descr_map(11%) =   42.08 : descr_map(12%) =  97.1044 /*Use */
            descr_map(13%) =   50.08 : descr_map(14%) = 110.1074 /*Dev */
            descr_map(15%) = -318.08 : descr_map(16%) = 123.1044 /* SS */

        /* 2nd Detail Line */
            descr_map(17%) = 66.08 : descr_map(18%) = 1071.1044 /*ROP */
            descr_map(19%) = 74.08 : descr_map(20%) = 1084.1044 /*EOQ */
            descr_map(21%) = 58.08 : descr_map(22%) = 1097.1044 /*Use */
            descr_map(23%) = 90.08 : descr_map(24%) = 1110.1074 /*Dev */
            descr_map(25%) = 82.08 : descr_map(26%) = 1123.1044 /* SS */

            call "PLOWCODE" (#1, plowkey$, rptdescr$, -9000%,  1.3,      ~
                             f1%(1), hdr$(), 0, -1.00000, incl(),        ~
                             incl$(), "r", " ", #2, descr_map())
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              scrtime$ = " " : call "TIME" (scrtime$)
              if fieldnr% > 0% then L40130
                 init(hex(86)) lfac$()
                 lfac$(1%) = hex(8c)
                 goto L40190
L40130:       init(hex(8c)) lfac$()
              pf9$ = " "
              str(pfkeys$,8,1) = hex(ff)
              if fieldnr% <> 1% then L40190
                 pf9$ = "(9)See ROP Parts"
                 str(pfkeys$,8%,1%) = hex(09)
L40190:       on fieldnr% gosub L40280,         /* Part Number       */   ~
                                L40280,         /* Part Class        */   ~
                                L40290,         /* Safety Stock      */   ~
                                L40290,         /* Re-order Point    */   ~
                                L40290,         /* Economic Order Qty*/   ~
                                L40290,         /* Standard Deviatio */   ~
                                L40290          /* min max           */
              goto L40310

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40280:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40290:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40310:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Part Number",                                ~
               at (06,23), fac(lfac$(1%)), part$                , ch(25),~
               at (06,49), fac(hex(84)),   partdescr$           , ch(32),~
                                                                         ~
               at (07,02), "Part ROP Class",                             ~
               at (07,23), fac(lfac$(2%)), pclass$              , ch(04),~
               at (07,49), fac(hex(84)),   clsdescr$            , ch(32),~
                                                                         ~
               at (08,02), "Safety Stock",                               ~
               at (08,23), fac(lfac$(3%)), ess$                 , ch(10),~
               at (08,49), "SS Usage/Day   ",                            ~
               at (08,65), fac(hex(84)),   ss_usage$            , ch(10),~
                                                                         ~
               at (09,02), "Re-order Point",                             ~
               at (09,23), fac(lfac$(4%)), rop$                 , ch(10),~
                                                                         ~
               at (10,02), "Economic Order Qty",                         ~
               at (10,23), fac(lfac$(5%)), eoq$                 , ch(10),~
               at (10,49), "Lead Time Days ",                            ~
               at (10,65), fac(hex(84)),   pp_lead$             , ch(10),~
                                                                         ~
               at (11,02), "Average Usage (Calc)",                       ~
               at (11,23), fac(lfac$(5%)), usage$               , ch(10),~
               at (11,49), "STD Usage/Day  ",                            ~
               at (11,65), fac(hex(84)),   pp_usage$            , ch(10),~
                                                                         ~
               at (12,02), "ROP Std. Deviation",                         ~
               at (12,23), fac(lfac$(6%)), stddev$              , ch(10),~
                                                                         ~
               at (13,02), "Mix / Max Planning",                         ~
               at (13,23), fac(lfac$(7%)), min$                 , ch(10),~
               at (13,37), fac(lfac$(7%)), max$                 , ch(10),~
                                                                         ~
               at (15,02), "Calc ROP (Y/N) :",                           ~
               at (15,23), fac(hex(84)), rop_calc$              , ch(01),~
                                                                         ~
               at (16,02), "ROP Equation No:",                           ~
               at (16,23), fac(hex(84)), rop_eq$                , ch(01),~
               at (16,49), fac(hex(84)), rop_eq_d$              , ch(25),~
                                                                         ~
               at (17,02), "Last ROP Change:",                           ~
               at (17,23), fac(hex(84)), rop_date$              , ch(08),~
                                                                         ~
               at (18,02), "ROP Source     :",                           ~
               at (18,23), fac(hex(84)), rop_source_d$          , ch(10),~
                                                                         ~
               at (19,02), "Last Changed By:",                           ~
               at (19,23), fac(hex(84)), u_id$                  , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,18), fac(hex(8c)), pf9$,                           ~
               at (22,37), "(10)Print Report by Part",                   ~
               at (23,18), fac(hex(8c)), pf4$,                           ~
               at (23,37), "(11)Print Report by Class",                  ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,18), fac(hex(8c)), pf12$                  , ch(17),~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys (pfkeys$),                                           ~
               key (keyhit%)

               if keyhit% <> 15 then L41010
                  call "PRNTSCRN"
                  goto L40310

L41010: close ws
        call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            skipfield = 0
            on fieldnr% gosub L50170,         /* Part Number       */     ~
                              L50370,         /* Part Class        */     ~
                              L50470,         /* Safety Stock      */     ~
                              L50520,         /* Re-order Point    */     ~
                              L50590,         /* Economic Order Qty*/     ~
                              L50670,         /* Standard Deviatio */     ~
                              L50680          /* Min / Max (AWD001)*/ 
            return

L50170: REM Check Part_Number
            plowkey$ = str(part$)
            partdescr$ = hex(06) & "Select valid Part Number"
            call "PLOWCODE" (#2, plowkey$, partdescr$, 0%, 0.30, f1%(2%))
            part$ = plowkey$
            if f1%(2%) = 1% then L50250
               errormsg$ = "Part Number not on Inventory Master File"
               return
L50250:     gosub lookup_part

        REM Check ROPHNY File
            readkey$ = str(part$)
            read #1,hold,key = readkey$, eod goto L50320
            goto dataload
        return
L50320:     rop_source$ = "M"
            rop_source_d$ = "  Manual  "
            u_id$ = userid$
        return

L50370: REM Test for Part Class
*       ** PLOWKEY$ = PCLASS$
            clsdescr$ = hex(06) & "Select Part Class"
            call "GETCODE" (#4%, pclass$, clsdescr$, 0%, 0.3, f1%(4%))
            if f1%(4%) = 1% then L50440
               errormsg$ = "Part Class not defined in ROPCLASS file."
               return
L50440:     gosub lookup_rop_class
        return

L50470: REM Test for Economic Safety Stock        ESS$
            call "NUMTEST" (ess$, 0, 9e7, errormsg$, 2.4, ess)
            gosub calc_ss_usage
            return

L50520: REM Test for Re-order Point               ROP$
            call "NUMTEST" (rop$, 0, 9e7, errormsg$, 2.4, rop)
            if svrop$ = rop$ then return
               svrop$ = rop$
               ropdate$ = str(date)
               return

L50590: REM Test for Economic Order Quantity      EOQ$
            call "NUMTEST" (eoq$, 0, 9e7, errormsg$, 2.4, eoq)

        REM Test for Average Usage                USAGE$
            call "NUMTEST" (usage$, 0.01, 9e7, errormsg$, 2.4, usage)
            gosub calc_std_usage
            return

L50670: REM Test for Standard Deviation           STDDEV$
            gosub check_devratio
            call "NUMTEST" (stddev$,0.01,9e7,errormsg$,2.4,stddev)
            return

L50680: REM Test for Min / Max Planning          MIN$, MAX$
             if min$ = " " then min$ = "0"
             if max$ = " " then max$ = "0"
             convert min$ to min_qty, data goto L50695

             convert max$ to max_qty, data goto L50695



             convert min_qty to min$, pic(#########0)

             convert max_qty to max$, pic(#########0)
             

        return
L50695:   errormsg$ = "Bad Min / Max Value ??"
        return


        check_devratio
            stddev = 0.0
            convert stddev$ to stddev, data goto L50750
L50750:
            if stddev = 0.0 then stddev = devratio
               call "CONVERT" (stddev, 2.2, stddev$)
        return

        REM *************************************************************~
            *            S P E C I A L   R O U T I N E S                *~
            *************************************************************

        lookup_part
            read #2,key = part$, using L60070, partdescr$, pp_lead$, ess, ~
                                              pp_pan, eod goto L60160
L60070:       FMT POS(26), CH(30), POS(170), CH(10), POS(318), 2*PD(14,4)
            pp_lead = 1.0
            call "CONVERT" (ess, -2.4, ess$)
            convert pp_lead$ to pp_lead, data goto L60110
L60110: calc_ss_usage
            pp_ess = ess
            if pp_ess < 1.0 then pp_ess = round( smco * pp_pan, 4)
            ss_usage = round( pp_ess / pp_lead, 4)
            call "CONVERT" (ss_usage, 2.4 , ss_usage$)
L60160: return

        lookup_rop_class
            read #4,key = pclass$, using L60210, clsdescr$, rop_calc$,    ~
                                                rop_eq$,   eod goto L60250
L60210:        FMT POS(5), CH(30), XX(1), 2*CH(1)
            if rop_eq$ = "1" then rop_eq_d$ = "Usage & Safety Stock    "
            if rop_eq$ = "2" then rop_eq_d$ = "Safety Stock & Leadtime "
            if rop_eq$ = "3" then rop_eq_d$ = "Usage & On-Hand         "
L60250: return

        calc_std_usage
            if pp_lead = 0.0 then pp_lead = 1.0
            pp_usage = round( eoq / pp_lead, 4)
            call "CONVERT" (pp_usage, 2.4 , pp_usage$)
        return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
