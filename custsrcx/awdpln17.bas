        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN17                             *~
            *  Creation Date     - 07/14/04                             *~
            *  Last Modified Date- 01/02/2020                           *~
            *  Written By        - Christie Gregory                     *~
            *                                                           *~
            *  Description       - This Program Defines the Seven       *~
            *                      Day Capacity for a Specified         *~
            *                      Production Year and Week for,        *~
            *                      Department                           *~
            *                                                           *~
            *  Code Tables Used  - (PLAN DEPT) - Department Codes       *~
            *                                                           *~
            *  Special Comments  - (APCPLN1B) - Subroutine to Lookup    *~
            *                                   and Display Planning    *~
            *                                   Tables.                 *~
            *                      (AWDPLN0B) - Subroutine to Calc      *~
            *                                   Current Planning Dates. *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/14/01 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            *01/02/2020! (CR2371) change to allow year 2020       ! CMN *~
            *************************************************************

        dim                              /* (AWDCAPTY) - FILE          */~
            filename$8,                  /* Used By EWDOPEN            */~
            prd_year$4, prd_wk$2,        /* Production Year and Week   */~
            prd_yr$2,                    /* Binary Production Year     */~
            prd_dept$3, prd_dept_d$30,   /* Planning Dept Cd- PLAN DEPT*/~
            prd_day$1,  prd_day_d$9,     /* Production Start Day       */~
            prd_unts(7%),prd_unts$(7%)10,/* Production Capacity        */~
            prd_date$(7%)10,             /* Production Dates        */~
            prd_dte$6, prd_date$10,      /* Prod Week Start Date       */~
            prd_key$11, sav_key$11,      /*                            */~
            prd_key1$11, cnt$25,         /*                            */~
            current$13,                  /*                            */~
            prd_fill$232                 /* Filler Area                */


        dim                              /* (Program) - Variables      */~
            hdr$40, msg$(3%)79,          /* Askuser - Var's            */~
            cur_yr$2, prv_yr$2,          /* Current and Previous Year  */~
            cur_year$4, prv_year$4,      /* Display versions of above  */~
            cur_wk$2, cur_dy$1,          /* Current Week and Day       */~
            cur_dte$6, cur_date$8,       /* Calc of Prod. Date         */~
            ent_yr$2,                    /* Entry Year                 */~
            ent_year$4,                  /* Entry Year 4 display       */~
            ent_wk$2, ent_dy$1,          /* Entry Week and Day         */~
            ent_dte$6, ent_date$8,       /* Entry Calc of Prod. Date   */~
            bg_dte$(7%)6, ed_dte$(7%)6,  /* Copy Beg/End dates         */~
            day$(14%)9,                  /* Calc Production Day        */~
            scr$(12%)50, days$(7%)9,     /* Screen and Days of Week    */~
            tab$(10%)10,                 /* Save Code Table Names      */~
            desc$30,                     /* Table Value, Description   */~
            code$15,                     /* Use To Look-Up Table Code  */~
            date$10,                     /* Report Title               */~
            datecc$8,                    /* Today's Date as YYYYMMDD   */~
            bg_yr$2, ed_yr$2,            /* Copy From/To Year          */~
            bg_year$4, ed_year$4,        /* as above 4 display         */~
            bg_wk$2, ed_wk$2,            /* Copy From/To Week          */~
            bg_wk_d$8, ed_wk_d$8,        /* Copy From/To Week Date     */~
            bg_dept$3, bg_dept_d$30,     /* Report Beg Department      */~
            ed_dept$3, ed_dept_d$30,     /* Report Ending Department   */~
            fr_yr$2, to_yr$2,            /* Copy From/To Prod Year     */~
            fr_year$4, to_year$4,        /* As above for display       */~
            fr_wk$2, to_wk$2,            /* Copy From/To Prod Week     */~
            prd_rec$256,                  /* Planning Units Capacity Rec*/~
            readkey$50,                  /* Generic Key                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$41, pname$21
            apc$   = " Planning Master Unit Capacity Edit/Rept "
            pname$ = "AWDPLN17 - Rev: R6.04"

        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! AWDCAPTY ! New Effective Units Capacity File        *~
            * #2  ! APCPLNDP ! Planning Master Department File          *~
            * #3  ! GENCODES ! Master Code Tables File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "AWDCAPTY",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 7,    keylen = 10,                      ~
                        alt key 1, keypos =  1, keylen = 16

            select #2,  "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos = 11,   keylen = 12,                      ~
                        alt key 1, keypos =  9, keylen = 14,             ~
                            key 2, keypos =  4, keylen = 12,             ~
                            key 3, keypos =  1, keylen = 15

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24



            call "SHOSTAT" ("Initialization")


REM            filename$ = "AWDCAPTY"  call "EWDOPEN" (#1, filename$, err%)
REM            if err% <> 0% then gosub open_error
            call "OPENCHCK" (#1, fs%(1%), f2%(1%),20%, rslt$(1%))

            filename$ = "APCPLNDP" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error


            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)

            tab$(1%) = "PLAN DEPT" : tab$(2%) = "         "
            tab$(3%) = "MODEL    " : tab$(4%) = "         "
            tab$(5%) = "         " : tab$(6%) = "         "
            tab$(7%) = "PLAN STAT" : tab$(8%) = "         "

            day$( 1%) = "Monday   "
            day$( 2%) = "Tuesday  "
            day$( 3%) = "Wednesday"
            day$( 4%) = "Thursday "
            day$( 5%) = "Friday   "
            day$( 6%) = "Saturday "
            day$( 7%) = "Sunday   "
            day$( 8%) = "Monday   "
            day$( 9%) = "Tuesday  "
            day$(10%) = "Wednesday"
            day$(11%) = "Thursday "
            day$(12%) = "Friday   "
            day$(13%) = "Saturday "
            day$(14%) = "Sunday   "

          days$(1%) = "MONDAY   "
          days$(2%) = "TUESDAY  "
          days$(3%) = "WEDNESDAY"
          days$(4%) = "THURSDAY "
          days$(5%) = "FRIDAY   "
          days$(6%) = "SATURDAY "
          days$(7%) = "SUNDAY   "

          date$ = date                       /* Set The Current Date   */
          call "DATFMTC" (date$,x%,datecc$)                               

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 4%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10240
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
REM                      if keyhit% = 14% and fieldnr% = 1% then report_input
                      if keyhit% = 11% and fieldnr% = 1% then copy_input
                      if keyhit% <> 0% then       L10120
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
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
                  if keyhit%  = 12% then gosub delete_record
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 3%
            zz% = cursor%(1%)           /* Edit Planning Bucket Values */
            if zz% < 11% then goto L11210
               yy% = cursor%(2%)
               if yy% < 47% then fieldnr% = 4%
               if yy% > 45% and yy% < 57% then fieldnr% = 5%
               if yy% > 57% then fieldnr% = 6%

L11210:     if fieldnr% < 1% or fieldnr% > 6% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11250:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11250
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11250
                  lastfieldnr% = fieldnr%
            goto L11130


        REM *************************************************************~
            *      I N P U T   M O D E   C o p y   S c r e e n          *~
            *************************************************************

        copy_input
            gosub initialize_variables

            for fieldnr% = 1% to  6%
L14080:         gosub'071(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L14200
L14100:         gosub'103(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L14180
L14130:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'071(fieldnr%)
                         if enabled% = 1% then L14100
                         if fieldnr% = 1% then L14080
                         goto L14130
L14180:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L14100
L14200:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L14100
            next fieldnr%

        REM *************************************************************~
            *       E D I T   M O D E   C o p y   S c r e e n           *~
            *************************************************************

        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub copy_data
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg3
L15110:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 6% then editpg3
            if fieldnr% = lastfieldnr% then    editpg3
            gosub'071(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg3
L15160:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L15160
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L15160
                  lastfieldnr% = fieldnr%
            goto L15110


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)

        deffn'071(fieldnr%)
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
         "Enter a Valid Planning Production Year or <Return> = Current?",~
         "Enter a Valid Planning Production Week or <Return> = Current?",~
         "Enter a Valid Planning Department Code?                      ",~
         "Enter Planning Capacities for Each Day (1 thru 7) of Week?   ",~
         "Enter No. of Planned Windows for Each Day (1 thru 7) of Week?",~
         "Enter No. of Produced Windows for Each Day (1 thru 7) of Week"

        deffn'070(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28530
                inpmessage$ = edtmessage$
                return

L28530
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn3_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn3_msg  :  data                                               ~
         "Enter the 'Copy From' Production Year ??                     ",~
         "Enter the 'Copy To' Production Year??                        ",~
         "Enter the 'Copy From' Production Week??                      ",~
         "Enter the 'Copy To' Production Week??                        ",~
         "Enter the 'Copy From' Department Code or (All)??             ",~
         "Enter the 'Copy To' Department Code or (All)??               "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, readkey$, prd_year$,         ~
                      prd_wk$, prd_dept$, prd_dte$, prd_date$,             ~
                      prd_dept_d$, prd_unts$(), prd_unta$(), prd_untp$(),  ~
                      bg_dept$, bg_dept_d$, ed_dept$, ed_dept_d$,          ~
                      current$, fr_yr$, fr_year$, to_yr$, to_year$,        ~
                      fr_wk$, to_wk$, prd_rec$, prd_day_d$, bg_yr$,        ~
                      bg_year$, ed_yr$, ed_year$, bg_wk$,                  ~
                      bg_wk_d$, ed_wk$, ed_wk_d$, prd_yr$, bg_dte$(), ed_dte$()
      
            rec%   = 0%
            total% = 0%
            copy%  = 0%
            mat prd_unts  = zer
            mat prd_unta% = zer
            mat prd_untp% = zer
            for i% = 1% to 7%
                prd_unts$(i%) = "     0.00 "
                prd_unta$(i%) = "    0"
                prd_untp$(i%) = "    0"
            next i%
        REM                !-- 6        !-- 19        !-- 33    !-- 43
        REM                v            v             v         v
          scr$( 1%)= "!Production Day !  Capacity  !  Avail  !  Avail  !"
          scr$( 2%)= "!---------------!------------!---------!---------!"
          scr$( 3%)= "!(1) Monday     ! XXXXXXXXXX !  XXXXX  !  XXXXX  !"
          scr$( 4%)= "!(2) Tuesday    !            !         !         !"
          scr$( 5%)= "!(3) Wednesday  !            !         !         !"
          scr$( 6%)= "!(4) Thursday   !            !         !         !"
          scr$( 7%)= "!(5) Friday     !            !         !         !"
          scr$( 8%)= "!(6) Saturday   !            !         !         !"
          scr$( 9%)= "!(7) Sunday     !            !         !         !"
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
          for i% = 1% to 7%
            rec% = 0%
            prd_key$ = " "
            str(prd_key$,1%,4%)  = prd_year$
            str(prd_key$,5%,2%)  = prd_wk$
            str(prd_key$,7%,3%)  = prd_dept$
            convert i% to prd_day$, pic(#)
            str(prd_key$,10%,1%)  = prd_day$     


            read #1,hold,key = prd_key$, eod goto L30420

            get #1, using L35040, prd_date$(i%), prd_year$, prd_wk$, ~
                                 prd_dept$, prd_day$, prd_unts, prd_fill$


            rec% = 1%

                                                
REM            gosub L50155                           /* Production Week   */
REM            gosub L50395                           /* Department Codes  */
            convert prd_unts to prd_unts$(i%),  pic(######.##-)

          next i%

REM         for i% = 1% to 7%
REM             convert prd_unts to prd_unts$(i%),  pic(######.##-)


REM         next i%

L30420: return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        delete_record
            call "SHOSTAT" ("Deleting Dept ("&prd_dept$&") Shift ("&      ~
                                              prd_shft$&")" )
            goto L31130
        dataput
            call "SHOSTAT" ("Updating Dept ("&prd_dept$&") Shift ("&      ~
                                              prd_shft$&")" )
L31130: for i% = 1% to 7%
            prd_key$ = " "
            str(prd_key$,1%,4%)  = prd_year$
            str(prd_key$,5%,2%)  = prd_wk$
            str(prd_key$,7%,3%)  = prd_dept$
            convert i% to prd_day$, pic(#)
            str(prd_key$,10%,1%)  = prd_day$     

            read #1,hold,key = prd_key$, eod goto L31230
               delete #1
               if keyhit% = 12% then goto L31290

L31230:     put #1, using L35040, prd_date$(i%), prd_year$, prd_wk$, ~
                                 prd_dept$, prd_day$, prd_unts(i%), prd_fill$

            write #1, eod goto L31310
L31290: next i%

        return clear all
        goto inputmode
L31310:     errormsg$ = "(Error)-Unable to Update Dept. Capacities? " &  ~
                        prd_key$
        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* File = (APCPLNUC)          */
L35040: FMT CH(06),                      /* Planning Production Date   */~
            CH(04),                      /* Planning Production Year   */~
            CH(02),                      /* Planning Production Week   */~
            CH(03),                      /* Department Code   PLAN DEPT*/~
            CH(01),                      /* Planning Production Day    */~
            PD(14,4),                    /* Plan Sched. Capacity 7 Days*/~
            CH(232)                      /* Filler Area                */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40070:       gosub set_pf1

              gosub'050(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40260,         /* Production Year      */~
                                L40260,         /* Production Week      */~
                                L40260,         /* Department Code      */~
                                L40260,         /* Capacity             */~
                                L40260,         /* Planned              */~
                                L40260          /* Produced             */
              goto L40280

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40260:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40280:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,20), fac(hex(a4)), apc$                   , ch(41),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Planning Production Year  :",                ~
               at (04,30), fac(lfac$(1%)), prd_year$            , ch(04),~
                                                                         ~
               at (05,02), "Planning Production Week  :",                ~
               at (05,30), fac(lfac$(2%)), prd_wk$              , ch(02),~
               at (05,40), fac(hex(84)), prd_date$              , ch(08),~
               at (05,55), fac(hex(84)), current$               , ch(13),~
                                                                         ~
               at (06,02), "Planning Department Code  :",                ~
               at (06,30), fac(lfac$(3%)), prd_dept$            , ch(03),~
               at (06,40), fac(hex(84)), prd_dept_d$            , ch(30),~
                                                                         ~
               at (12,16), fac(hex(84)), scr$( 1%)              , ch(50),~
               at (13,16), fac(hex(84)), scr$( 2%)              , ch(50),~
               at (14,16), fac(hex(84)), scr$( 3%)              , ch(50),~
               at (15,16), fac(hex(84)), scr$( 4%)              , ch(50),~
               at (16,16), fac(hex(84)), scr$( 5%)              , ch(50),~
               at (17,16), fac(hex(84)), scr$( 6%)              , ch(50),~
               at (18,16), fac(hex(84)), scr$( 7%)              , ch(50),~
               at (19,16), fac(hex(84)), scr$( 8%)              , ch(50),~
               at (20,16), fac(hex(84)), scr$( 9%)              , ch(50),~
                                                                         ~
               at (14,34), fac(lfac$(4%)), prd_unts$( 1%)        , ch(10),~
               at (15,34), fac(lfac$(4%)), prd_unts$( 2%)        , ch(10),~
               at (16,34), fac(lfac$(4%)), prd_unts$( 3%)        , ch(10),~
               at (17,34), fac(lfac$(4%)), prd_unts$( 4%)        , ch(10),~
               at (18,34), fac(lfac$(4%)), prd_unts$( 5%)        , ch(10),~
               at (19,34), fac(lfac$(4%)), prd_unts$( 6%)        , ch(10),~
               at (20,34), fac(lfac$(4%)), prd_unts$( 7%)        , ch(10),~
                                                                         ~
               at (14,49), fac(lfac$(5%)), prd_unta$( 1%)        , ch(05),~
               at (15,49), fac(lfac$(5%)), prd_unta$( 2%)        , ch(05),~
               at (16,49), fac(lfac$(5%)), prd_unta$( 3%)        , ch(05),~
               at (17,49), fac(lfac$(5%)), prd_unta$( 4%)        , ch(05),~
               at (18,49), fac(lfac$(5%)), prd_unta$( 5%)        , ch(05),~
               at (19,49), fac(lfac$(5%)), prd_unta$( 6%)        , ch(05),~
               at (20,49), fac(lfac$(5%)), prd_unta$( 7%)        , ch(05),~
                                                                         ~
               at (14,59), fac(lfac$(6%)), prd_untp$( 1%)        , ch(05),~
               at (15,59), fac(lfac$(6%)), prd_untp$( 2%)        , ch(05),~
               at (16,59), fac(lfac$(6%)), prd_untp$( 3%)        , ch(05),~
               at (17,59), fac(lfac$(6%)), prd_untp$( 4%)        , ch(05),~
               at (18,59), fac(lfac$(6%)), prd_untp$( 5%)        , ch(05),~
               at (19,59), fac(lfac$(6%)), prd_untp$( 6%)        , ch(05),~
               at (20,59), fac(lfac$(6%)), prd_untp$( 7%)        , ch(05),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% < 6% or keyhit% > 6% then goto L41090
                  tab% = keyhit% - 5%
                  gosub display_codes
                  goto L40070

L41090:        if keyhit% <> 15 then goto L41170
                  call "PRNTSCRN"
                  goto L40070

L41170:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
            apc$   = " Planning Master Unit Capacity (*Input*) "
            if total% = 0% then goto L41260
               fieldnr% = 0%
               edit% = 2%
               apc$   = " Planning Master Unit Capacity (*Total*) "
L41260:
        if edit% = 2% then L41430     /*  Input Mode             */
            pf$(1) = "(1)Start Over      (6)Department Codes  " &        ~
                     "                                       "
            pf$(2) = "(4)Previous Field                       " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "(11)Copy Data          (16)Exit Program"
            pfkeys$ = hex(01ffff04ff06ffffffff0bffffff0f1000)
            if fieldnr% = 1% then L41390
                str(pf$(1%),64%) = " " : str(pfkeys$,14%,1%) = hex(ff)
                str(pf$(3%),40%) = " " : str(pfkeys$,11%,1%) = hex(ff)
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L41390:     if fieldnr% > 1% then L41410
                str(pf$(2%),1%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L41410: return

L41430: if fieldnr% > 0% then L41570  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over      (6)Department Codes  " &        ~
                     "                       (12)Delete Rec. "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Update Data "
            pfkeys$ = hex(01ffffffff06ffffffffff0cff0e0f1000)
            if rec% = 1% then goto L41530
                str(pf$(1%),64%) = " " : str(pfkeys$,12%,1%) = hex(ff)
L41530:     if total% = 0% then goto L41560
                str(pf$(1%),64%) = " " : str(pfkeys$,12%,1%) = hex(ff)
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L41560: return
L41570:
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
        return



        REM *************************************************************~
            *           D I S P L A Y   C O D E   T A B L E             *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_codes
            call "APCPLN1B" (tab%, #3)
        return

        REM *************************************************************~
            *    C O P Y   P R O D U C T I O N   W E E K   S C R E E N  *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
              gosub set_pf4

              gosub'070(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              gosub L46230                     /* Beg Prod Year        */
                                               /* End Prod Year        */
                                               /* Beg Prod Week        */
                                               /* End Prod Week        */
                                               /* Beg Department       */
                                               /* End Department       */
              goto L46260

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L46230:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L46260:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,57), "( Copy ) Today:",                            ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,20), fac(hex(a4)), apc$                   , ch(41),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Copy From Production Year :",                ~
               at (04,30), fac(lfac$(1%)), bg_year$             , ch(04),~
                                                                         ~
               at (05,02), "Copy To Production Year   :",                ~
               at (05,30), fac(lfac$(2%)), ed_year$             , ch(04),~
                                                                         ~
               at (06,02), "Copy From Production Week :",                ~
               at (06,30), fac(lfac$(3%)), bg_wk$               , ch(02),~
               at (06,40), fac(hex(84)), bg_wk_d$               , ch(08),~
               at (06,55), fac(hex(84)), current$               , ch(13),~
                                                                         ~
               at (07,02), "Copy To Production Week   :",                ~
               at (07,30), fac(lfac$(4%)), ed_wk$               , ch(02),~
               at (07,40), fac(hex(84)), ed_wk_d$               , ch(08),~
                                                                         ~
               at (08,02), "Copy From Department Code :",                ~
               at (08,30), fac(lfac$(5%)), bg_dept$             , ch(03),~
               at (08,40), fac(hex(84)), bg_dept_d$             , ch(30),~
                                                                         ~
               at (09,02), "Copy To Department Code   :",                ~
               at (09,30), fac(lfac$(6%)), ed_dept$             , ch(03),~
               at (09,40), fac(hex(84)), ed_dept_d$             , ch(30),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L46890
                  call "PRNTSCRN"
                  goto L46260

L46890:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf4
            apc$   = " Planning Master Unit Capacity (*Copy*)  "

        if edit% = 2% then L47100     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "(4)Previous                             " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L47060
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L47060:     if fieldnr% > 1% then L43000
                str(pf$(2%),1%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L43000: return

L47100: if fieldnr% > 0% then L47190  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Copy Data   "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffff0cff0e0f1000)
        return
L47190:
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
            on fieldnr% gosub L50100,         /* Planning Prod. Year   */ ~
                              L50155,         /* Planning Prod. Week   */ ~
                              L50395,         /* Department Code       */ ~
                              L50825,         /* Production Capacities */ ~
                              L50895,         /* Production Planned    */ ~
                              L50965          /* Production Produced   */
            return

L50100: REM Planning Production Year
            if prd_year$ <> " " then goto L50115   
               prd_year$ = str(datecc$,1%,4%)      
L50115:     convert prd_year$ to prd_yr%, data goto L50135

REM  IF PRD_YR% < 1994% OR PRD_YR% >= 2020% THEN GOTO L50135
            prd_yr$ = bin(prd_yr%,2)                               
        return
L50135:     errormsg$ = "(Error) - Invalid Production Year??"
            init(" ") prd_year$                                        
        return

L50155: REM Planning Production Week         PRD_WK$, PRD_DTE$, PRD_DATE$
            ent_yr$ = prd_yr$
            ent_wk$ = prd_wk$
            init(" ") ent_dy$, ent_dte$, ent_date$
           call "AWDPLN0B" ( cur_yr$,    /* Current Production Year    */~
                             cur_wk$,    /* Current Production Week    */~
                             cur_dy$,    /* Current Production Day     */~
                             cur_dte$,   /* Current Production Date(6) */~
                             cur_date$,  /* Current Production Date(8) */~
                             ent_yr$,    /* Entry Production Year      */~
                             ent_wk$,    /* Entry Prod Week            */~
                             ent_dy$,    /* Entry Production Day       */~
                             ent_dte$,   /* Entry Production Date (6)  */~
                             ent_date$,  /* Entry Production Date *8)  */~
                             prv_yr$,    /* Previous Year              */~
                             #3,          /* GENCODES                  */~
                             prd_e%    )  /* 0% = No, 1% = Found        */
           
           if prd_e% <> 0% then goto L50375
           convert val(cur_yr$,2) to cur_year$,pic(0000)              
           convert val(ent_yr$,2) to ent_year$,pic(0000)              
           convert val(prv_yr$,2) to prv_year$,pic(0000)              
           prd_wk$   = ent_wk$
           prd_dte$  = ent_dte$
           prd_date$ = ent_date$

           for i% = 1% to 7%
               call "DATE" addr ("G+", prd_dte$, i%-1%, prd_date$(i%), err%)

           next i%
           convert ent_year$ to x%, data goto L50260     /* ENTRY YEAR    */
L50260:
           convert cur_year$ to y%, data goto L50270     /* CURRENT YEAR  */
L50270:
           zz% = (x% - y%) * 52%

           x% = 0% : y% = 0%
           convert ent_wk$ to x%, data goto L50375     /* ENTRY WEEK    */

           convert cur_wk$ to y%, data goto L50305     /* CURRENT WEEK  */
L50305:
           x% = x% + zz%

           current$ = "Current: +XXX"
           if x% <> y% then goto L50340
              str(current$,11%,3%) = "000"
              goto L50370
L50340:    if x% > y% then goto L50360
              str(current$,10%,1%) = "-"                /* Previous Wk */
              convert (y%-x%) to str(current$,11%,3%), pic(000)
              goto L50370                                /* FUTURE WK   */
L50360:    convert (x%-y%) to str(current$,11%,3%), pic(000)

L50370: return
L50375:    errormsg$ = "(Error) - Invalid Production Week (1 thru 53)?"
           init(" ") prd_wk$, prd_dte$, prd_date$
        return

L50395: REM Department Code                       prd_DEPT$, prd_DEPT_D$
            if prd_dept$ <> " " then goto L50415
               prd_dept$ = "000"

L50415:     convert prd_dept$ to x%, data goto L50430

            convert x% to prd_dept$, pic(000)
L50430:     code$ = prd_dept$
            tab%  = 1% : gosub check_code
            if code% = 0% then goto L50455
            prd_dept_d$ = desc$
            gosub dataload
            if rec% = 1% then fieldnr% = 10%
            if rec% = 1% then return

            if edit% = 1% and rec% = 0% then gosub calc_capacity
        return
L50455:     errormsg$ = "(Error) - Invalid Department Code ??"
            init(" ") prd_dept$, prd_dept_d$
        return




L50825: REM Planning Production Capacity          prd_UNTS(), prd_UNTS$()
            mat prd_unts = zer
            for i% = 1% to 7%
                convert prd_unts$(i%) to prd_unts(i%), data goto L50865

                convert prd_unts(i%) to prd_unts$(i%), pic(######.##-)
            next i%
        return
L50865:     errormsg$ = "(Error) - Invalid Capacity Value for - "        ~
                        & days$(i%)
            init(" ") prd_unts$()
            mat prd_unts = zer
        return

L50895: REM Planning Production Planned           prd_UNTA%(), prd_UNTA$()
            mat prd_unta% = zer
            for i% = 1% to 7%
                convert prd_unta$(i%) to prd_unta%(i%), data goto L50935

                convert prd_unta%(i%) to prd_unta$(i%), pic(#####)
            next i%
        return
L50935:     errormsg$ = "(Error) - Invalid Planned Value for - "         ~
                        & days$(i%)
            init(" ") prd_unta$()
            mat prd_unta% = zer
        return

L50965: REM Planning Production Produced          prd_UNTP%(), prd_UNTP$()
            mat prd_untp% = zer
            for i% = 1% to 7%
                convert prd_untp$(i%) to prd_untp%(i%), data goto L51005

                convert prd_untp%(i%) to prd_untp$(i%), pic(#####)
            next i%
        return
L51005:     errormsg$ = "(Error) - Invalid Produced Window Value for -"  ~
                        & days$(i%)
            init(" ") prd_untp$()
            mat prd_untp% = zer
        return

REM     check_dept
            check% = 0%
        REM CHECK% = 1%
            init(" ") sav_key$, prd_key$
            str(prd_key$,1%,3%) = prd_dept$
            str(prd_key$,4%,2%) = "01"
            str(prd_key$,6%,2%) = prd_shft$
            sav_key$ = prd_key$
            read #2,key > prd_key$, using L51080, prd_key$, eod goto L51100
L51080:        FMT POS(11), CH(12)
            if str(sav_key$,1%,7%) <> str(prd_key$,1%,7%) then goto L51100
            check% = 1%
        return
L51100:     init(" ") sav_key$, prd_key$
        return


        calc_capacity                    /* Calculate Capacity Hours   */

            call "SHOSTAT" ("Checking Department Capacity")

            for i% = 0% to 6%
              k% = i% + 1%
              if k% = 6% or k% = 7% then goto L61770
                 prd_unts(k%) = 600.00
                 convert prd_unts(k%) to prd_unts$(k%), pic(######.##-)

L61770:     next i%
        return


        REM *************************************************************~
            *                C o p y   E d i t s                        *~
            *************************************************************

        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L54090,         /* Copy From Prod Year   */ ~
                              L54145,         /* Copy To Prod Year     */ ~
                              L54200,         /* Copy From Prod Week   */ ~
                              L54430,         /* Copy To Production Wk */ ~
                              L52340,         /* Beg Department Code   */ ~
                              L52445          /* End Department Code   */
        return

L54090: REM Copy From Production Year             BG_YR$
            if bg_year$ <> " " then goto L54105                    
               bg_year$ = str(datecc$,1%,4%)                       
L54105:     convert bg_year$ to bg_yr%, data goto L54125           

REM  IF BG_YR% < 1994% OR BG_YR% > 2019% THEN GOTO L54125   /*(CR2371)*/
            bg_yr$ = bin(bg_yr%,2)                                 
        return
L54125:     errormsg$ = "(Error) - Copy From Production Year??"
            init(" ") bg_year$                                     
        return

L54145: REM Copy To Production Year               ED_YR$
            if ed_year$ <> " " then goto L54160                    
               ed_year$ = str(datecc$,1%,4%)                       
L54160:     convert ed_year$ to ed_yr%, data goto L54180           

            if ed_yr% < bg_yr% then goto L54180
            ed_yr$ = bin(ed_yr%,2)                                 
        return
L54180:     errormsg$ = "(Error) - Copy To Production Year??"
            init(" ") ed_year$                                     
        return

L54200: REM Copy From Production Week              BG_WK$, BG_WK_D$
            ent_yr$ = bg_yr$
            ent_wk$ = bg_wk$
            init(" ") ent_dy$, ent_dte$, ent_date$
           call "AWDPLN0B" ( cur_yr$,    /* Current Production Year    */~
                             cur_wk$,    /* Current Production Week    */~
                             cur_dy$,    /* Current Production Day     */~
                             cur_dte$,   /* Current Production Date(6) */~
                             cur_date$,  /* Current Production Date(8) */~
                             ent_yr$,    /* Entry Production Year      */~
                             ent_wk$,    /* Entry Prod Week            */~
                             ent_dy$,    /* Entry Production Day       */~
                             ent_dte$,   /* Entry Production Date (6)  */~
                             ent_date$,  /* Entry Production Date *8)  */~
                             prv_yr$,    /* Previous Year              */~
                             #3,          /* GENCODES                  */~
                             prd_e%    )  /* 0% = No, 1% = Found        */

           if prd_e% <> 0% then goto L54410
           bg_wk$   = ent_wk$
           bg_wk_d$ = ent_date$

           prd_dte$  = ent_dte$

           for i% = 1% to 7%
               call "DATE" addr ("G+", prd_dte$, i%-1%, bg_dte$(i%), err%)

           next i%

           convert ent_wk$ to x%, data goto L54410     /* Entry Week    */

           convert cur_wk$ to y%, data goto L54410     /* Current Week  */
           convert val(cur_yr$,2) to cur_year$,pic(0000)           
           convert val(ent_yr$,2) to ent_year$,pic(0000)           
           convert val(prv_yr$,2) to prv_year$,pic(0000)           
           current$ = "Current: +XX"
           if ent_yr$ = cur_yr$ then goto L54360
              if ent_yr$ > cur_yr$ then goto L54345
                 wk% = ((52% - x%) + y%) * (-1%)
                 goto L54395
L54345:    wk% = ((52% - y%) + x%)                      /* Future Year */
           goto L54395                                   /* Week        */

L54360:    if x% <> y% then goto L54375
              wk% = 0%                                  /* Current Wk  */
              goto L54395
L54375:    if x% > y% then goto L54390
              wk% = (y% - x%) * (-1%)                   /* Previous Wk */
              goto L54395
L54390:    wk% = (x% - y%)
L54395:    convert wk% to str(current$,10%,3%), pic(-00) /*Future Wk*/

        return
L54410:    errormsg$ = "(Error) - Invalid Copy From Production Week??"
           init(" ") bg_wk$, bg_wk_d$, current$
        return

L54430: REM Copy To Production Week                ED_WK$, ED_WK_D$
            ent_yr$ = ed_yr$
            ent_wk$ = ed_wk$
            init(" ") ent_dy$, ent_dte$, ent_date$
           call "AWDPLN0B" ( cur_yr$,    /* Current Production Year    */~
                             cur_wk$,    /* Current Production Week    */~
                             cur_dy$,    /* Current Production Day     */~
                             cur_dte$,   /* Current Production Date(6) */~
                             cur_date$,  /* Current Production Date(8) */~
                             ent_yr$,    /* Entry Production Year      */~
                             ent_wk$,    /* Entry Prod Week            */~
                             ent_dy$,    /* Entry Production Day       */~
                             ent_dte$,   /* Entry Production Date (6)  */~
                             ent_date$,  /* Entry Production Date *8)  */~
                             prv_yr$,    /* Previous Year              */~
                             #3,         /* GENCODES                   */~
                             prd_e%    )  /* 0% = No, 1% = Found        */

           if prd_e% <> 0% then goto L54585
           ed_wk$   = ent_wk$
           ed_wk_d$ = ent_date$
           prd_dte$  = ent_dte$

           for i% = 1% to 7%
               call "DATE" addr ("G+", prd_dte$, i%-1%, ed_dte$(i%), err%)

           next i%


           convert ent_wk$ to x%, data goto L54585     /* Entry Week    */

           convert cur_wk$ to y%, data goto L54585     /* Current Week  */
           convert val(cur_yr$,2) to cur_year$,pic(0000)           
           convert val(ent_yr$,2) to ent_year$,pic(0000)           
           convert val(prv_yr$,2) to prv_year$,pic(0000)           
           if bg_yr$ = ed_yr$ then goto L54570
              if ed_wk$ > bg_wk$ then goto L54585
                 return

L54570:    if ed_wk$ < bg_wk$ then goto L54585
           copy% = 1%
        return
L54585:    errormsg$ = "(Error) - Invalid Copy To Production Week??"
           init(" ") ed_wk$, ed_wk_d$
        return

L52340: REM Beg Department Code                   BG_DEPT$,BG_DEPT_D$
            if bg_dept$ <> " " then goto L52360
               bg_dept$ = "ALL"

L52360:     if str(bg_dept$,1%,1%) <> "A" then goto L52385
               bg_dept$ = "ALL"
               bg_dept_d$ = "(ALL) Planning Departments"
               return

L52385:     convert bg_dept$ to x%, data goto L52425

            convert x% to bg_dept$, pic(000)
            code$ = bg_dept$
            tab%  = 1% : gosub check_code
            if code% = 0% then goto L52425
            bg_dept_d$ = desc$
        return
L52425:     errormsg$ = "(Error) - Invalid Beginning Department Code ??"
            init(" ") bg_dept$, bg_dept_d$
        return

L52445: REM End Department Code                   ED_DEPT$,ED_DEPT_D$
            if ed_dept$ <> " " then goto L52465
               ed_dept$ = bg_dept$

L52465:     if str(bg_dept$,1%,1%) <> "A" then goto L52490
               ed_dept$ = "ALL"
               ed_dept_d$ = "(ALL) Planning Departments"
               return

L52490:     convert ed_dept$ to x%, data goto L52555

            convert x% to ed_dept$, pic(000)
            code$ = ed_dept$
            tab%  = 1% : gosub check_code
            if code% = 0% then goto L52555
            ed_dept_d$ = desc$
            convert bg_dept$ to y%, data goto L52530
L52530:
            if x% < y% then goto L52555
            if copy% = 0% then return
               if x% <> y% then goto L52555
        return
L52555:     errormsg$ = "(Error) - Invalid Ending Department Code ??"
            init(" ") ed_dept$, ed_dept_d$
        return



        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************



        check_code
           code% = 0%
           readkey$ = " "
           str(readkey$,1%,9%)    = tab$(tab%)
           str(readkey$,10%,15%)  = code$
           read #3,key = readkey$, using L60800, desc$,                   ~
                                                           eod goto L60830
L60800:        FMT POS(25), CH(30)
           code% = 1%
        return
L60830:    errormsg$ = "(Error) - Invalid Code Value Entered ??"
        return

        copy_data
            call "SHOSTAT" ("Copy Data From Week ("&bg_wk$&") to Week ("&~
                             ed_wk$&")")
            cnt% = 0%
            cnt$ = "Records Copied = [XXXXX]"
            prd_key$ = " "
            str(prd_key$,1%,4%) = bg_year$
            str(prd_key$,5%,2%) = bg_wk$
            if bg_dept$ = "ALL" then goto L61000
               str(prd_key$,7%,3%) = bg_dept$
                                           /* Skip Beg Process Key    */
L61000:                                    /* Because of Greater Than */
        copy_next
            read #1,hold,key > prd_key$, using L61030, prd_key$,            ~
                                                       eod goto copy_done
L61030:         FMT POS(7), CH(10)
            if str(prd_key$,1%,4%) <> bg_year$ then goto copy_done
            if str(prd_key$,5%,2%) <> bg_wk$ then goto copy_done
            if bg_dept$ = "ALL" then goto L61100
               if str(prd_key$,7%,3%) <> bg_dept$ then goto copy_next

L61100:     
        
            get #1, using L61140, prd_rec$
L61140:           FMT CH(256)

REM   TO DO Finish the copy routine -  Need to get the production dates!!!!
            convert str(prd_rec$,16%,1%) to i%, data goto L61180


L61180:

REM            init(" ") prd_key$
REM            prd_key$ = str(prd_rec$,7%,10%)

            cnt% = cnt% + 1%
            if mod(cnt%,5%) <> 0 then goto L61200
               convert cnt% to str(cnt$,19%,5%), pic(#####)
               print at(03,28); hex(84);cnt$;

L61200:     str(prd_rec$,1%,6%)  = ed_dte$(i%)
            str(prd_rec$,7%,4%)  = ed_year$
            str(prd_rec$,11%,2%) = ed_wk$


            prd_key1$ = str(prd_rec$,7%,10%)



REM               mat prd_unta% = zer
REM               mat prd_untp% = zer
REM               put str(prd_rec$,87%,28%), using L61370, prd_unta%(),        ~
                                                      prd_untp%()
REM L61370           FMT 7*BI(2), 7*BI(2)

            read #1,hold,key = prd_key1$, eod goto L61410
               delete #1
L61410:     put #1, using L61140, prd_rec$
            write #1, eod goto L61480



            goto copy_next
        copy_done

        return clear all
            goto copy_input
L61480:     call "SHOSTAT" ("(Error) - Copying --->  " & prd_key1$ )
            stop : close ws
            goto copy_next




        open_error
           comp% = 2%
           hdr$ = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
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

            end
