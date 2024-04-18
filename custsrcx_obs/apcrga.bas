        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   RRRR    GGG    AAA                  *~
            *  A   A  P   P  C   C  R   R  G   G  A   A                 *~
            *  AAAAA  PPPP   C      RRRRR  G      AAAAA                 *~
            *  A   A  P      C   C  R  R   G  GG  A   A                 *~
            *  A   A  P       CCC   R   R   GGG   A   A                 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCRGA   - Special Utility for entering and Reporting     *~
            *            RGA'S                                          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/18/92 ! New Program for (APC) - Last Mod Date    ! RHH *~
            *          !                                          !     *~
            * 10/31/97 ! Changed Program Version ID to 60403      ! DJD *~
            *          !                                          !     *~
            *************************************************************

        dim                                                              ~
            bg_date$8, bg_dte$6, x$8,    /* Begin Entry Date           */~
            ed_date$8, ed_dte$6,         /* Ending Entry Date          */~
            rga_key$6,                   /* RGA Key                    */~
            rga_no$6,                    /* RGA Number                 */~
            rga_cust$9,                  /* RGA Customer Code          */~
            rga_cust_d$30,               /* RGA Customer Name          */~
            rga_dte_1$8,                 /* RGA Date Entered Formatted */~
            rga_dte1$8,                  /*          Unformatted       */~
            rga_dte_2$8,                 /* RGA Date Completed Formatte*/~
            rga_dte2$8,                  /*          Unformatted       */~
            rga_desc1$30,                /* RGA Description (1)        */~
            rga_desc2$30,                /* RGA Description (2)        */~
            rga_credit$10,               /* RGA Credit Dollars         */~
            rga_entry$3,                 /* ENTRY ID                   */~
            rga_filed$3,                 /* FILED ID                   */~
            rga_fil$1,                   /* RGA Fill Area              */~
            company$60,                  /* For Report Company Name    */~
            print_title$43,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
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

        dim f2%(10),                     /* = 0 if the file is open    */~
            f1%(10),                     /* = 1 if READ was successful */~
            fs%(10),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10)20                  /* Text from file opening     */

        dim blankdate$6,                 /* Check against empty date   */~
            workdate10$10,               /* Full Century work date     */~
            workdate8$8                  /* Work date (mm dd yy) fmt   */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 10/31/97 RGA Tracking Utility           "
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
            * #01 ! APCRGA   ! RGA Master File                          *~
            * #02 ! CUSTOMER ! Customer Master File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,  "APCRGA",                                        ~
                        varc,     indexed,  recsize = 102,               ~
                        keypos =    1, keylen =    6

            select #2,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =  1,   keylen =  9,                      ~
                        alt key  1, keypos  =    10, keylen = 30, dup,   ~
                            key  2, keypos  =   424, keylen =  9, dup,   ~
                            key  3, keypos  =   771, keylen =  9, dup,   ~
                            key  4, keypos  =   780, keylen =  9, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01),100%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02),  0%, rslt$(02))

            f1%(1), f1%(2) = 0%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            str(workdate10$,1%,6%) = date
            call "DATEFMT" (date$)
            call "DATEFMT" (workdate10$)
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

            for fieldnr% = 1% to 7%
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
                      if keyhit% = 14% then gosub begin_process
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
                  if keyhit%  = 12% then gosub delete_rga
                  if keyhit%  = 14% then gosub begin_process
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11140:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 7% then editpg1
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
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        begin_process
           gosub'102(1%)
           errormsg$ = " "
           if rga_no$ <> " " then goto L19120
              str(rga_no$,1%,1%) = "A"

L19120:    if str(rga_no$,1%,1%) = "A" then rga_no$ = "ALL   "
           if bg_date$ <> " " and bg_date$ <> blankdate$ then goto L19150
              bg_date$ = date$
L19150:    date% = 0%
           call "DATEOK" (bg_date$, date%, errormsg$ )
           if date% = 0% then goto begin_process
           if ed_date$ <> " " and ed_date$ <> blankdate$ then goto L19200
              ed_date$ = bg_date$
L19200:    date1% = 0%
           call "DATEOK" (ed_date$, date1%, errormsg$)
           if date1% = 0% then goto begin_process
           if date% <= date1% then goto L19270
              errormsg$ = "(ERROR) - INVALID DATE RANGE ENTERED"
              bg_date$, ed_date$ = " "
              goto begin_process
L19270:    if keyhit% =  1% then gosub startover
           if keyhit% = 14% then goto L19320
           if keyhit% = 16% then goto exit_program
           goto begin_process

L19320:    call "SHOSTAT" ("Creating RGA Report")
           gosub select_printer
           if str(rga_no$,1%,1%) = "A" then rga_no$ = " "
           x$ = bg_date$ : call "DATUNFMT" (x$) : bg_dte$ = str(x$,1%,6%)
           x$ = ed_date$ : call "DATUNFMT" (x$) : ed_dte$ = str(x$,1%,6%)
           rga_key$ = all(hex(00))
           rga_key$ = rga_no$
           read #1,key > rga_key$, using L19440, rga_key$, rga_dte1$,     ~
                                                    eod goto begin_done
              goto L19450
        begin_next
           read #1, using L19440, rga_key$, rga_dte1$,eod goto begin_done
L19440:       FMT CH(6), XX(9), CH(6)
L19450:    if rga_dte1$ < bg_dte$ or rga_dte1$ > ed_dte$ then            ~
                                                          goto begin_next
           rga_no$ = rga_key$
           gosub dataload
           gosub print_detail
           goto begin_next
        begin_done
           if lcnt% <> 99% then print using L55050
           gosub close_printer
        return clear all
        goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20170,         /* RGA Number             */~
                              L20210,         /* RGA Customer Code      */~
                              L20250,         /* RGA Date Entered       */~
                              L20330,         /* RGA Description (1)    */~
                              L20370,         /* Rga Description (2)    */~
                              L20290,         /* RGA Date Completed     */~
                              L20410          /* RGA Credit Dollars     */
         return

L20170: REM RGA Number                             RGA_NO$
        REM RGA_NO$ = " "
         return

L20210: REM RGA Customer Code                      RGA_CUST$
        REM RGA_CUST$ = " "
         return

L20250: REM RGA Date Entered                       RGA_DTE_1$
        REM RGA_DTE_1$ = " "
         return

L20290: REM RGA Date Completed                     RGA_DTE_2$
        REM RGA_DTE_2$ = " "
         return

L20330: REM RGA Description (1)                    RGA_DESC1$
        REM RGA_DESC1$ = " "
         return

L20370: REM RGA Description (2)                    RGA_DESC2$
        REM RGA_DESC2$ = " "
         return

L20410: REM RGA Credit Dollars                     RGA_CREDIT$
        REM RGA_CREDIT$ = " "
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
         "Enter a Valid RGA Number.                          (Required)",~
         "Enter a Valid Customer Code for RGA.               (Required)",~
         "Enter a Valid Entry Date for RGA.                  (Required)",~
         "Enter Description Part (1).                                  ",~
         "Enter Description Part (2).                                  ",~
         "Enter a Valid Completion Date for RGA.                       ",~
         "Enter Credit Dollars ( When Applicable ).                    "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, rga_key$, rga_no$,         ~
                      rga_cust$, rga_cust_d$, rga_dte_1$, rga_dte1$,     ~
                      rga_dte_2$, rga_dte2$, rga_desc1$, rga_desc2$,     ~
                      rga_credit$, rga_fil$, rga_entry$, rga_filed$,     ~
                      bg_date$, bg_dte$, ed_date$, ed_dte$
            rga_credit = 0.0
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
            dataload
              rec% = 0%
              rga_key$ = all(hex(00))
              rga_key$ = rga_no$
              read #1,hold,key = rga_key$, using L35040, rga_no$,         ~
                                 rga_cust$, rga_dte1$, rga_dte2$,        ~
                                 rga_desc1$, rga_desc2$, rga_credit,     ~
                                 rga_entry$, rga_filed$, rga_fil$,       ~
                                 eod goto L30200
              convert rga_credit to rga_credit$, pic($#,###.##-)

              rga_dte_1$ = rga_dte1$
              rga_dte_2$ = rga_dte2$
              gosub L50280                           /* LOOKUP CUSTOMER */
              call "DATEFMT" (rga_dte_1$)
              call "DATEFMT" (rga_dte_2$)
              rec% = 1%
L30200:     return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
            dataput
              rga_key$ = all(hex(00))
              rga_key$ = rga_no$
              read #1,hold,key = rga_key$, eod goto L31100
                 delete #1
L31100:       put #1,using L35040,rga_no$,rga_cust$, rga_dte1$, rga_dte2$,~
                                 rga_desc1$, rga_desc2$, rga_credit,     ~
                                 rga_entry$, rga_filed$, rga_fil$
              write #1, eod goto L31160
            goto L31170
L31160:       stop "(Error) - Could not update RGA file for - "&rga_no$
L31170:     return clear all
            goto inputmode

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35040: FMT CH(06),                        /* (APCRGA) - TRACKING FILE */~
            CH(09),                        /* Customer Code            */~
            CH(06),                        /* RGA Date Entered         */~
            CH(06),                        /* RGA Date Filed           */~
            CH(30),                        /* Description (1)          */~
            CH(30),                        /* Description (2)          */~
            PD(14,4),                      /* Credit Dollars           */~
            CH(3),                         /* Entry Id                 */~
            CH(3),                         /* Filed Id                 */~
            CH(1)                          /* Filler Area              */

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
              on fieldnr% gosub L40100,         /* RGA Number        */   ~
                                L40100,         /* Customer Code     */   ~
                                L40100,         /* Date Entered      */   ~
                                L40095,         /* Description (1)   */   ~
                                L40095,         /* Description (2)   */   ~
                                L40100,         /* Date Filed in/Com */   ~
                                L40105          /* Credit Dollars    */
              goto L40115

L40095:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40100:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40105:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40115:       accept                                                       ~
               at (01,02),                                               ~
                  "RGA Tracking and Reporting ",                         ~
               at (01,66), "Today:",                                     ~
               at (01,69), fac(hex(8c)), workdate10$            , ch(10),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "RGA Number       :",                         ~
               at (06,25), fac(lfac$( 1)), rga_no$              , ch(06),~
                                                                         ~
               at (07,02), "RGA Customer Code:",                         ~
               at (07,25), fac(lfac$( 2)), rga_cust$            , ch(09),~
               at (07,40), fac(hex(84)), rga_cust_d$            , ch(30),~
                                                                         ~
               at (08,02), "RGA Date Entered :",                         ~
               at (08,25), fac(lfac$( 3)), rga_dte_1$           , ch(08),~
               at (08,40), fac(lfac$( 3)), rga_entry$           , ch(03),~
                                                                         ~
               at (09,02), "Description (1)  :",                         ~
               at (09,25), fac(lfac$( 4)), rga_desc1$           , ch(30),~
                                                                         ~
               at (10,02), "Description (2)  :",                         ~
               at (10,25), fac(lfac$( 5)), rga_desc2$           , ch(30),~
                                                                         ~
               at (11,02), "RGA Date Filed   :",                         ~
               at (11,25), fac(lfac$( 6)), rga_dte_2$           , ch(08),~
               at (11,40), fac(lfac$( 6)), rga_filed$           , ch(03),~
                                                                         ~
               at (12,02), "Credit           :",                         ~
               at (12,25), fac(lfac$( 7)), rga_credit$          , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40300
                  call "PRNTSCRN"
                  goto L40115

L40300:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40400     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L40380
                str(pf$(1),64)    = " "  :  str(pfkeys$,14,1) = hex(ff)
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40380:     if fieldnr% > 1% then L40390
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40390:     return

L40400: if fieldnr% > 0% then L40455  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (12)Delete RGA         " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Data Save   "
            pfkeys$ = hex(01ffffffffffffffffffff0cffff0f1000)
            if rec% = 1% then L40450
                str(pf$(2),18,26) = " "  :  str(pfkeys$,12,1) = hex(ff)
L40450:     return
L40455:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return


        REM *************************************************************~
            *               R E P O R T   S C R E E N                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%)
            gosub set_pf2
            inpmessage$ = "Enter Starting RGA No or (A)ll,and Beg/End Ent~
        ~ry Date?"

L41110:     accept                                                       ~
               at (01,02),                                               ~
                  "RGA Tracking and Report ",                            ~
               at (01,66), "Today:",                                     ~
               at (01,69), fac(hex(8c)), workdate10$            , ch(10),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Starting RGA No. :",                         ~
               at (06,25), fac(hex(81)),   rga_no$              , ch(06),~
                                                                         ~
               at (07,02), "Beg Date Entered :",                         ~
               at (07,25), fac(hex(81)),   bg_date$             , ch(08),~
                                                                         ~
               at (08,02), "End Date Entered :",                         ~
               at (08,25), fac(hex(81)),   ed_date$             , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L41380
                  call "PRNTSCRN"
                  goto L41110

L41380:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50170,         /* RGA Number            */ ~
                              L50280,         /* Customer Code         */ ~
                              L50450,         /* RGA Date Entered      */ ~
                              L50700,         /* RGA Description (1)   */ ~
                              L50740,         /* RGA Description (2)   */ ~
                              L50580,         /* RGA Date Complete     */ ~
                              L50780          /* RGA Credit Dollars    */
            return

L50170: REM RGA Number                            RGA_NO$
            if rga_no$ <> " " then goto L50200
               goto L50240
L50200:     gosub dataload
            if rec% = 0% then return
               fieldnr% = 7%
        return
L50240:     errormsg$ = "RGA Number is Required"
            rga_no$ = " "
        return

L50280: REM RGA Customer Code                     RGA_CUST$
           if rga_cust$ <> " " then goto L50340
              rga_cust_d$ = hex(06) & "Select Customer Code"
              call "PLOWCODE" (#2, rga_cust$,rga_cust_d$,0%,.30,f1%(2))
              if f1%(2) = 0 then goto L50410
L50340:       read #2,key = rga_cust$, using L50360, rga_cust_d$,         ~
                                                    eod goto L50380
L50360:       FMT POS(10), CH(30)
        return
L50380:     errormsg$ = "(Error) - Customer Code Not On File."
            rga_cust$, rga_cust_d$ = " "
        return
L50410:     errormsg$ = "RGA Customer Code is Required."
            rga_cust$, rga_cust_d$ = " "
        return

L50450: REM RGA Date Entered                      RGA_DTE_1$
            if rga_dte_1$ <> " " and rga_dte_1$ <> blankdate$ then goto L50480
               goto L50540
L50480:     date% = 0%
            call "DATEOK" (rga_dte_1$, date%, errormsg$ )
            if date% = 0% then return
            rga_dte1$ = rga_dte_1$
            call "DATUNFMT" (rga_dte1$)
        return
L50540:     errormsg$ = "RGA Date Entered is Required."
            rga_dte_1$, rga_dte1$ = " "
        return

L50580: REM RGA Date Completed                    RGA_DTE_2$
            if rga_dte_2$ <> " " and rga_dte_2$ <> blankdate$ then goto L50610
               goto L50670
L50610:     date% = 0%
            call "DATEOK" (rga_dte_2$, date%, errormsg$ )
            if date% = 0% then return
            rga_dte2$ = rga_dte_2$
            call "DATUNFMT" (rga_dte2$)
        return
L50670:     rga_dte_2$, rga_dte2$ = " "
        return

L50700: REM RGA Description (1)                   RGA_DESC1$

        return

L50740: REM RGA Description (2)                   RGA_DESC2$

        return

L50780: REM RGA Credit Dollars                    RGA_CREDIT$
            if rga_credit$ <> " " then goto L50820
               rga_credit$ = "0.0"

L50820:     convert rga_credit$ to rga_credit, data goto L50870

            convert rga_credit to rga_credit$, pic($#,###.##-)

        return
L50870:     errormsg$ = "(Error) - Invalid Credit Entry. "
            rga_credit$ = " " : rga_credit = 0.0
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

L55100: %!########  ########   ##########################################~
        ~#                                                     Page:#### !
                                                   /* Column 1 Header */
L55130: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!
L55150: %!RGA No!Customer !<------- Customer Name ------>!Entered !Id !Fi~
        ~le DTE!Id !<---- Description (1) ------->!Descript(2)!Credit Dol!
L55160: %!------!---------!------------------------------!--------!---!--~
        ~------!---!------------------------------!-----------!----------!
                                                   /* DETAIL 1      */
L55200: %!######!#########!##############################!########!###!##~
        ~######!###!##############################!###########!##########!

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_header
          if lcnt% <> 99% then print using L55050
          page_no% = page_no% + 1%
          print page
          print using L55050
          print using L55100, date$, rpt_time$, print_title$, page_no%
          print using L55130
          print using L55150
          lcnt% = 5%
        return

        print_detail
          if lcnt% > 60% then gosub print_header
          print using L55160
          print using L55200, rga_no$, rga_cust$, rga_cust_d$, rga_dte_1$,~
                             rga_entry$, rga_dte_2$, rga_filed$,         ~
                             rga_desc1$, str(rga_desc2$,1%,11%),         ~
                             rga_credit$
          lcnt% = lcnt% + 2%
          return

        select_printer
            page_no% = 0%
            lcnt%    = 99%

            print_title$ = "Returned Goods Authorization Log Report"
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCRGA", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCRGA", " ", 0%, 1%)
        return

        delete_rga
          call "SHOSTAT" ("Deleting RGA ( "& rga_no$ & " ) " )
          rga_key$ = all(hex(00))
          rga_key$ = rga_no$
          read #1,hold,key = rga_key$, eod goto L60480
             delete #1
        return clear all
        goto inputmode
L60480:      stop "Unable to Delete RGA - "& rga_no$
        return clear all
        goto inputmode

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
