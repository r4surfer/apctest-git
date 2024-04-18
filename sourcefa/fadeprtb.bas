        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  FFFFF   AAA   DDDD   EEEEE  PPPP   RRRR   TTTTT  BBBB    *~
            *  F      A   A  D   D  E      P   P  R   R    T    B   B   *~
            *  FFFF   AAAAA  D   D  EEEE   PPPP   RRRR     T    BBBB    *~
            *  F      A   A  D   D  E      P      R   R    T    B   B   *~
            *  F      A   A  DDDD   EEEEE  P      R   R    T    BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FADEPRTB - Fixed Assets Depreciation tables input.        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/06/88 ! Original                                 ! TLJ *~
            * 09/22/88 ! Mods to allow blank Convention & Period  ! RJM *~
            * 08/06/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            asset$,                      /* Asset                      */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            begdate$10,                  /* Beg date of table          */~
            convention$1,                /* Proration Convention       */~
            convdescr$20,                /* Proration Convention Descr.*/~
            convdescp$20,                /* Proration Convention Descr.*/~
            cnv$(3)1,                    /* Proration Convention Descr.*/~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr$79,                    /* Plow Descriptiom           */~
            enddate$10,                  /* End date of table          */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            group$10,                    /* Group                      */~
            gr$(3)10,                    /* Group for deletion test    */~
            hdr$(3)30,                   /* Plow Headers               */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lf$(3)5,                     /* Life for deletion test     */~
            lfac$(62)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            line3$79,                    /* Header Line for Percents   */~
            line4$79,                    /* Header Line for Percents   */~
            pd$(3)2,                     /* Period for deletion test   */~
            percent$(60)8,               /* Percents to be entered     */~
            percents(60),                /* Percents to be entered     */~
            period$2,                    /* Period Put in Service      */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey2$99,                 /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            recovery$5,                  /* Recovery                   */~
            scrnum$(60)3,                /* Numbers for percent screen */~
            ttype$7,                     /* Property Types             */~
            type$7,                      /* Property Types             */~
            userid$3                     /* Current User Id            */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * #01 ! FAMASTER ! Fixed Assets Master File                 *~
            * #02 ! FATABLE  ! Fixed Assets ACRS Depreciation Tables Fi *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "FAMASTER",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  120, keylen =  10,                     ~
                        alt key  1, keypos =   58, keylen =   1, dup     ~

            select #02, "FATABLE",                                       ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    1, keylen =  18                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 200%, rslt$(02))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "FADEPRTB: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  7%
L10100:         gosub'051(fieldnr%,0%)     /* Default / Enables */
                      if enabled% = 0% then L10280
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10240
                         if fieldnr% = 6% then call "DATUFMTC" (begdate$)
                         if fieldnr% = 7% then call "DATUFMTC" (enddate$)
L10170:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%,0%)
                         if enabled% <> 1% then L10220
                          gosub'151(fieldnr%)
                          goto L10120
L10220:                  if fieldnr% = 1% then L10100
                         goto L10170
L10240:               if keyhit% =  6% then type$ = "RPLAEXO"
                      if keyhit% =  7% then type$ = "       "
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10280:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%

        inputpg2:
            for fieldnr% = 1% to recovery%
L10340:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10410
L10370:                  fieldnr% = max(1%, fieldnr% - 1%)
                         if enabled% = 1% then L10340
                         if fieldnr% = 1% then L10340
                         goto L10370
L10410:               if keyhit% =   9% then goto def_sl
                      if keyhit% =  10% then goto def_db
                      if keyhit% <> 16% then L10470
                                        gosub'152(fieldnr%,2%)
                                        if errormsg$ = " " then datasave
                                        if str(errormsg$,1,6) = "ERROR-" ~
                                        then editpg2 else goto L10340
L10470:               if keyhit% = 32% then exit_program
                      if keyhit% <> 0% then       L10340
                gosub'152(fieldnr%, 1%) /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10340
            next fieldnr%
            gosub'152(fieldnr%,2%)
            if errormsg$ <> " " then editpg2

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  5% then       editpg2
                  if keyhit%  =  8% then gosub delete_table
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 32% then       exit_program
                  if keyhit% <>  0% then       editpg1
L11150:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% then fieldnr% = 1%
            if fieldnr% > 7% then fieldnr% = 7%
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%,1%)      /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11210:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  6% then type$ = "RPLAEXO"
                  if keyhit%  =  7% then type$ = "       "
                  if keyhit% <>  0% then L11210
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11210
                  lastfieldnr% = fieldnr%
            goto L11150

        editpg2
            errormsg$ = " "
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then goto  clear_table
                  if keyhit%  =  4% then       editpg1
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 32% then       exit_program
                  if keyhit% <>  0% then       editpg2
L11370:     fieldnr% = 1%
            if fieldnr% = lastfieldnr% then    editpg2
L11390:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11390
            gosub'152(fieldnr%, 2%)     /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11390
                  lastfieldnr% = fieldnr%
            goto L11370

        REM *************************************************************~
            *         G E N E R A L   F U N C T I O N S                 *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************
        clear_table:
            percent$() = " "
            errormsg$ = " "
            goto inputpg2

        delete_table:
            readkey2$=str(group$,,)& str(recovery$,,) & str(convention$)&~
                                                           str(period$,,)
            call "SHOSTAT" ("Checking FAMASTER")
            found% = 0%
            readkey$ = all(hex(00))
            call "READ102" (#01, readkey$, f1%(1))
L12170:     if f1%(1) = 0% or found%=1% then do_it
              get #01 using L12190, asset$, gr$(), lf$(), cnv$(), pd$()
L12190:           FMT  POS(120), CH(10), POS(270), 3*CH(10), 3*CH(5),    ~
                                 3*CH(1), POS(696), 3*CH(2)
              for i% = 1% to 3%
                 if readkey2$ = str(gr$(i%),,) & str(lf$(i%),,) &        ~
                       str(cnv$(i%),,) & str(pd$(i%),,) then found% = 1% ~
                                                        else next i%
            call "READNEXT" (#01, f1%(1))
            goto L12170

        do_it:
            if found% = 0% then L12360
L12300:       u3% = 2%
              call "ASKUSER" ( u3%, " ", "Cannot DELETE the table.",     ~
              "This table is used by the Asset " & asset$,               ~
              "Press RETURN to Continue")
               if u3% <> 0% then L12300
              return
L12360:     call "SHOSTAT" ("Deleting the TABLE")
            call "DELETE" (#02, readkey2$, 18%)
            return clear all
            goto inputmode

*        Default for straightline
        def_sl:
            fieldnr% = 62%
            accr$ = "4"
L12450:     gosub'102(fieldnr%, 2%)
            if keyhit% <> 1% then L12490
              gosub startover
              goto L12450
L12490:     convert accr$ to accr%, data gosub data_error
            convert recovery$ to recovery
            sl = 100/recovery
            gosub get_mult
            i% = 1%
            total, percents(1) = round(mult*sl,accr%)
            if total + round(sl,accr%) >= 100 then L12590
            for i% = 2% to recovery%-1
               percents(i%) = round(sl,accr%)
               total = total + round(sl,accr%)
               if total + round(sl,accr%) >= 100 then L12590
            next i%
L12590:     percents(i%+1%) = 100 - total
            for i% = i%+2% to recovery% : percents(i%) = 0% : next i%
            for i%=1% to recovery%
              convert percents(i%) to percent$(i%), pic(###.####)
            next i%
            gosub'152(1%, 2%)
            goto editpg2

        def_db:
            fieldnr% = 61%
            if recovery < 15 then dbper$ = "200" else dbper$ = "150"
            accr$ = "4"
L12710:     gosub'102(fieldnr%, 2%)
            if keyhit% <> 1% then L12750
              gosub startover
              goto L12710
L12750:     convert dbper$ to dbper%, data gosub data_error
            convert accr$ to accr%, data gosub data_error
            dbper = dbper%/100
            gosub get_mult        /* Dependent on Convention */
            yrs_rem = recovery + yradj - 1
            rate = dbper / recovery
            if (1/recovery)*mult >= rate                                 ~
              then percents(1) = round((1/recovery)*mult*100, accr%)     ~
              else percents(1) = min(round(rate*mult*100.00, accr%),100)
            total = percents(1)
            unrec = 100 - percents(1)
            for i% = 2% to recovery% - 1
              if yrs_rem > 0 then L12870
                percents(i%) = rate * unrec
                goto L12900
L12870:       if 1/yrs_rem >= rate then percents(i%) = (1/yrs_rem)*unrec ~
                                   else percents(i%) = rate * unrec
              yrs_rem = yrs_rem - 1
L12900:       percents(i%) = round(percents(i%), accr%)
              if total + percents(i%) < 100 then L12910
                i% = i%-1
                goto L12940
L12910:       unrec = unrec - percents(i%)
              total = total + percents(i%)
            next i%
L12940:     percents(i%+1%) = 100-total
            for i%=1% to recovery%
              convert percents(i%) to percent$(i%), pic(###.####)
            next i%
            gosub'152(1%, 2%)
            goto editpg2

        data_error:
L13030:     errormsg$ = "Must Enter Numeric Values."
L13040:     gosub'102(fieldnr%, 2%)
            if keyhit% <> 1% then L13090
              gosub startover
              errormsg$ = " "
              goto L13040
L13090:     convert dbper$ to dbper%, data goto L13030
            convert accr$ to accr%, data goto L13030
            return

        get_mult:
            if period$ <> "  " then L13150
              mult = 1 : yradj =  0 : goto L13240
L13150:     convert period$ to period, data goto shouldnt_happen
            if convention$ <> "1" then L13180
              mult, yradj = .5
L13180:     if convention$ <> "2" then L13210
              mult = (4.5 - period)/4
              yradj = 1 - mult
L13210:     if convention$ <> "3" then L13240
              mult = (12.5 - period)/12
              yradj = 1 - mult
L13240:     return

        shouldnt_happen: /* Convert Error Routine, File Problems??*/
L13270:       u3% = 2%
              call "ASKUSER" ( u3%, " ", "Conversion Error",             ~
              "Possible File Corruption.",                               ~
              "Press RETURN to StartOver.")
               if u3% <> 0% then L13270
               return clear all
               goto inputmode

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "SHOSTAT" ("Saving Table")
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%, edit%)
            enabled% = 1%
            on fieldnr% gosub L20170,         /* Group                  */~
                              L20210,         /* Recovery               */~
                              L20250,         /* Convention             */~
                              L20290,         /* Period                 */~
                              L20390,         /* Beginning date         */~
                              L20450,         /* End date               */~
                              L20480          /* Types                  */
            return

L20170
*        Def/Enable Group                       GROUP$
            if edit% = 1% then enabled% = 0%
            return

L20210
*        Def/Enable Recovery                    RECOVERY$
            if edit% = 1% then enabled% = 0%
            return

L20250
*        Def/Enable Proration Convention        CONVENTION$
            if edit% = 1% then enabled% = 0%
            return

L20290
*        Def/Enable Period Put in Service       PERIOD$
            if edit% = 1% then enabled% = 0%
            if convention$ <> " " then L20340
              period$ = "  "
              enabled% = 0%
              goto L20370
L20340:     if convention$ <> "1" then return
              period$ = " 1"
              enabled% = 0%
L20370:     return

L20390
*        Def/Enable Beginning date                BEGDATE$
            if begdate$ <> " " and begdate$ <> blankdate$ then L20430
              begdate$ = date
              call "DATEOKC" (begdate$, u3%, errormsg$)
L20430:     return

L20450
*        Def/Enable Ending date                   ENDDATE$
            return

L20480
*        Def/Enable Property Type                 TYPE$
            if type$ = " " then type$ = "RPLAEXO"
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
         "Enter Table Group, Leave blank or Use ? to List Existing Groups~
        ~.",                                                              ~
         "Enter Recovery Time in Years (Format ##.##). Leave blank or Use~
        ~ ? to List.",                                                    ~
         "Enter Blank)No Convention, 1)Half-Year, 2)Mid-QTR, 3)Mid-Month.~
        ~ Use ? to List.",                                                ~
         "Enter Period Put in Service, Leave blank or Use ? to List Exist~
        ~ing.",                                                           ~
         "Enter the Beginning Date of the effective date range.        ",~
         "Enter the Ending Date of the effective date range.           ",~
         "R-Real, P-Personal, L-Low Inc., A-Amortized, E-Lease, X-Rental,~
        ~ O-Other."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, convdescr$, convdescp$,    ~
                      convention$, group$, percent$(), period$,          ~
                      recovery$, type$, begdate$, enddate$
            scrnum$()=" 1) 2) 3) 4) 5) 6) 7) 8) 9)10)11)12)13)14)15)16)17~
        ~)18)19)20)21)22)23)24)25)26)27)28)29)30)31)32)33)34)35)36)37)38)3~
        ~9)40)41)42)43)44)45)46)47)48)49)50)51)52)53)54)55)56)57)58)59)60)"

            mat percents = zer
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
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
            plowkey$=str(group$,,)&str(recovery$,,)&str(convention$)&    ~
                                                           str(period$,,)
            call "READ101" (#02, plowkey$, f1%(2))
            if f1%(2) = 0 then return
              call "SHOSTAT" ("Loading Existing Table")
              get #2 using L30110, begdate$,enddate$,percents(),type$
L30110:           FMT    POS(19), CH(8), CH(8), 60*PD(7,4), CH(7)
            for i%=1% to recovery%
              convert percents(i%) to percent$(i%), pic(###.####)
            next i%
            call "DATFMTC" (begdate$)
            call "DATFMTC" (enddate$)
            gosub get_convention_descr
            return

        get_convention_descr:
            if convention$ = "1" then convdescr$ = "Half-Year"
            if convention$ = "2" then convdescr$ = "Mid-Quarter"
            if convention$ = "3" then convdescr$ = "Mid-Month"
            if convention$ = " " then convdescr$ = "No Convention"
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "DATUFMTC" (begdate$)
            call "DATUFMTC" (enddate$)
            plowkey$=str(group$,,)&str(recovery$,,)&str(convention$)&    ~
                                                           str(period$,,)
            call "READ101" (#02, plowkey$, f1%(2))
            put #2 using L31150,   group$, recovery$, convention$,        ~
                                  period$, begdate$, enddate$,           ~
                                  percents(), type$
L31150:           FMT             CH(10), CH(5), CH(1), CH(2), CH(8),    ~
                                  CH(8), 60*PD(7,4), CH(7)
            if f1%(2) = 0% then write #02 else rewrite #02
            return

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
              if edit% = 2% then init(hex(8c)) str(lfac$(),1,4)/* Key */
              on fieldnr% gosub L40220,         /* Group             */   ~
                                L40220,         /* Recovery          */   ~
                                L40220,         /* Convention        */   ~
                                L40220,         /* Period            */   ~
                                L40220,         /* Beg Date          */   ~
                                L40220,         /* End Date          */   ~
                                L40220          /* Types             */
              goto L40250

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40220:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40250:     accept                                                       ~
               at (01,02),                                               ~
                  "Fixed Assets Depreciation Tables",                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Group",                                      ~
               at (06,30), fac(lfac$( 1)), group$               , ch(10),~
                                                                         ~
               at (07,02), "Recovery",                                   ~
               at (07,30), fac(lfac$( 2)), recovery$            , ch(05),~
                                                                         ~
               at (08,02), "Proration Convention",                       ~
               at (08,30), fac(lfac$( 3)), convention$          , ch(01),~
               at (08,40), fac(hex(8c)), convdescp$             , ch(20),~
                                                                         ~
               at (09,02), "Period Put in Service",                      ~
               at (09,30), fac(lfac$( 4)), period$              , ch(02),~
                                                                         ~
               at (10,02), "Beginning Date",                             ~
               at (10,30), fac(lfac$( 5)), begdate$             , ch(10),~
                                                                         ~
               at (11,02), "Ending Date",                                ~
               at (11,30), fac(lfac$( 6)), enddate$             , ch(10),~
                                                                         ~
               at (12,02), "Property Type",                              ~
               at (12,30), fac(lfac$( 7)), type$                , ch(07),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40650
                  call "MANUAL" ("FADEPRTB") : goto L40250

L40650:        if keyhit% <> 15 then L40680
                  call "PRNTSCRN" : goto L40250

L40680:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if convdescr$ <> " " then convdescp$ = "(" & convdescr$ & ")"
        if edit% = 2% then L40960     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (6)Allow All           " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (7)Allow None          " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ff0607ffffffffff0dff0f1000)
            if fieldnr% = 1% then L40840
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40840:     if fieldnr% <> 5% and fieldnr% <> 1% then L40860
                str(pf$(1),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40860:     if fieldnr% = 7% then L40900
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 6,1) = hex(ff)
                str(pf$(3),18,26) = " "  :  str(pfkeys$, 7,1) = hex(ff)
                goto L40940
L40900:     if str(type$,1,7) <> "       " then L40920
                str(pf$(3),18,26) = " "  :  str(pfkeys$, 7,1) = hex(ff)
L40920:     if str(type$,1,7) <> "RPLAEXO" then L40940
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 6,1) = hex(ff)
L40940:     return

L40960: if fieldnr% > 0% then L41050  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (5)Next Screen         " &        ~
                     "  (8)Delete Table      (16/32)Save/Exit"
            pfkeys$ = hex(01ffffff05ffff08ffffffff0dff0f102000)
            return
L41050:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (6)Allow All           " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (7)Allow None          " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffff0607ffffffffff0dff0fff00)
            if fieldnr% = 7% then L41170
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 6,1) = hex(ff)
                str(pf$(3),18,26) = " "  :  str(pfkeys$, 7,1) = hex(ff)
                goto L41220
L41170:     if str(type$,1,7) <> "       " then L41200
                str(pf$(3),18,26) = " "  :  str(pfkeys$, 7,1) = hex(ff)
                goto L41220
L41200:     if str(type$,1,7) <> "RPLAEXO" then L41220
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 6,1) = hex(ff)
L41220:     return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
            inpmessage$ = " "
            gosub set_pf2
            str(scrnum$(), recovery%*3+1,) = " "
            if fieldnr% <> 61% then L41400
              inpmessage$ = "Enter Declining Balance PERCENT and Number"&~
                            " of DECIMAL PLACES."
              init(hex(8c)) lfac$()
              lfac$(61%) = hex(82)
              lfac$(62%) = hex(9c) /* invisable */
              goto L41550
L41400:     if fieldnr% <> 62% then L41450
              inpmessage$ = "Enter Number of DECIMAL PLACES."
              init(hex(8c)) lfac$()
              lfac$(61%) = hex(9c) /* invisable */
              lfac$(62%) = hex(82)
              goto L41550
L41450:     if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            if errormsg$ <> " " and str(errormsg$,1,6) <> "ERROR-"       ~
                                                          then edit% = 1%
            lfac$(61%) = hex(9c)
            lfac$(62%) = hex(9c)
            if fieldnr% = 0% then L41550
            if edit% = 1% then lfac$(fieldnr%) = hex(82) /* Input Mode */~
            else str(lfac$(),1,recovery%) = all(hex(82)) /* Edit Mode  */

L41550:     accept                                                       ~
               at (01,02),                                               ~
                  "Fixed Assets Depreciation Tables",                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (06,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), fac(hex(8c)), line3$                 , ch(79),~
               at (05,02), fac(hex(8c)), line4$                 , ch(79),~
                                                                         ~
               at (07,06), fac(hex(8c)), scrnum$(1)             , ch(03),~
               at (07,10), fac(lfac$( 1)), percent$(1)          , ch(08),~
               at (08,06), fac(hex(8c)), scrnum$(2)             , ch(03),~
               at (08,10), fac(lfac$( 2)), percent$(2)          , ch(08),~
               at (09,06), fac(hex(8c)), scrnum$(3)             , ch(03),~
               at (09,10), fac(lfac$( 3)), percent$(3)          , ch(08),~
               at (10,06), fac(hex(8c)), scrnum$(4)             , ch(03),~
               at (10,10), fac(lfac$( 4)), percent$(4)          , ch(08),~
               at (11,06), fac(hex(8c)), scrnum$(5)             , ch(03),~
               at (11,10), fac(lfac$( 5)), percent$(5)          , ch(08),~
               at (12,06), fac(hex(8c)), scrnum$(6)             , ch(03),~
               at (12,10), fac(lfac$( 6)), percent$(6)          , ch(08),~
               at (13,06), fac(hex(8c)), scrnum$(7)             , ch(03),~
               at (13,10), fac(lfac$( 7)), percent$(7)          , ch(08),~
               at (14,06), fac(hex(8c)), scrnum$(8)             , ch(03),~
               at (14,10), fac(lfac$( 8)), percent$(8)          , ch(08),~
               at (15,06), fac(hex(8c)), scrnum$(9)             , ch(03),~
               at (15,10), fac(lfac$( 9)), percent$(9)          , ch(08),~
               at (16,06), fac(hex(8c)), scrnum$(10)            , ch(03),~
               at (16,10), fac(lfac$(10)), percent$(10)         , ch(08),~
               at (17,06), fac(hex(8c)), scrnum$(11)            , ch(03),~
               at (17,10), fac(lfac$(11)), percent$(11)         , ch(08),~
               at (18,06), fac(hex(8c)), scrnum$(12)            , ch(03),~
               at (18,10), fac(lfac$(12)), percent$(12)         , ch(08),~
               at (07,20), fac(hex(8c)), scrnum$(13)            , ch(03),~
               at (07,24), fac(lfac$(13)), percent$(13)         , ch(08),~
               at (08,20), fac(hex(8c)), scrnum$(14)            , ch(03),~
               at (08,24), fac(lfac$(14)), percent$(14)         , ch(08),~
               at (09,20), fac(hex(8c)), scrnum$(15)            , ch(03),~
               at (09,24), fac(lfac$(15)), percent$(15)         , ch(08),~
               at (10,20), fac(hex(8c)), scrnum$(16)            , ch(03),~
               at (10,24), fac(lfac$(16)), percent$(16)         , ch(08),~
               at (11,20), fac(hex(8c)), scrnum$(17)            , ch(03),~
               at (11,24), fac(lfac$(17)), percent$(17)         , ch(08),~
               at (12,20), fac(hex(8c)), scrnum$(18)            , ch(03),~
               at (12,24), fac(lfac$(18)), percent$(18)         , ch(08),~
               at (13,20), fac(hex(8c)), scrnum$(19)            , ch(03),~
               at (13,24), fac(lfac$(19)), percent$(19)         , ch(08),~
               at (14,20), fac(hex(8c)), scrnum$(20)            , ch(03),~
               at (14,24), fac(lfac$(20)), percent$(20)         , ch(08),~
               at (15,20), fac(hex(8c)), scrnum$(21)            , ch(03),~
               at (15,24), fac(lfac$(21)), percent$(21)         , ch(08),~
               at (16,20), fac(hex(8c)), scrnum$(22)            , ch(03),~
               at (16,24), fac(lfac$(22)), percent$(22)         , ch(08),~
               at (17,20), fac(hex(8c)), scrnum$(23)            , ch(03),~
               at (17,24), fac(lfac$(23)), percent$(23)         , ch(08),~
               at (18,20), fac(hex(8c)), scrnum$(24)            , ch(03),~
               at (18,24), fac(lfac$(24)), percent$(24)         , ch(08),~
               at (07,34), fac(hex(8c)), scrnum$(25)            , ch(03),~
               at (07,38), fac(lfac$(25)), percent$(25)         , ch(08),~
               at (08,34), fac(hex(8c)), scrnum$(26)            , ch(03),~
               at (08,38), fac(lfac$(26)), percent$(26)         , ch(08),~
               at (09,34), fac(hex(8c)), scrnum$(27)            , ch(03),~
               at (09,38), fac(lfac$(27)), percent$(27)         , ch(08),~
               at (10,34), fac(hex(8c)), scrnum$(28)            , ch(03),~
               at (10,38), fac(lfac$(28)), percent$(28)         , ch(08),~
               at (11,34), fac(hex(8c)), scrnum$(29)            , ch(03),~
               at (11,38), fac(lfac$(29)), percent$(29)         , ch(08),~
               at (12,34), fac(hex(8c)), scrnum$(30)            , ch(03),~
               at (12,38), fac(lfac$(30)), percent$(30)         , ch(08),~
               at (13,34), fac(hex(8c)), scrnum$(31)            , ch(03),~
               at (13,38), fac(lfac$(31)), percent$(31)         , ch(08),~
               at (14,34), fac(hex(8c)), scrnum$(32)            , ch(03),~
               at (14,38), fac(lfac$(32)), percent$(32)         , ch(08),~
               at (15,34), fac(hex(8c)), scrnum$(33)            , ch(03),~
               at (15,38), fac(lfac$(33)), percent$(33)         , ch(08),~
               at (16,34), fac(hex(8c)), scrnum$(34)            , ch(03),~
               at (16,38), fac(lfac$(34)), percent$(34)         , ch(08),~
               at (17,34), fac(hex(8c)), scrnum$(35)            , ch(03),~
               at (17,38), fac(lfac$(35)), percent$(35)         , ch(08),~
               at (18,34), fac(hex(8c)), scrnum$(36)            , ch(03),~
               at (18,38), fac(lfac$(36)), percent$(36)         , ch(08),~
               at (07,48), fac(hex(8c)), scrnum$(37)            , ch(03),~
               at (07,52), fac(lfac$(37)), percent$(37)         , ch(08),~
               at (08,48), fac(hex(8c)), scrnum$(38)            , ch(03),~
               at (08,52), fac(lfac$(38)), percent$(38)         , ch(08),~
               at (09,48), fac(hex(8c)), scrnum$(39)            , ch(03),~
               at (09,52), fac(lfac$(39)), percent$(39)         , ch(08),~
               at (10,48), fac(hex(8c)), scrnum$(40)            , ch(03),~
               at (10,52), fac(lfac$(40)), percent$(40)         , ch(08),~
               at (11,48), fac(hex(8c)), scrnum$(41)            , ch(03),~
               at (11,52), fac(lfac$(41)), percent$(41)         , ch(08),~
               at (12,48), fac(hex(8c)), scrnum$(42)            , ch(03),~
               at (12,52), fac(lfac$(42)), percent$(42)         , ch(08),~
               at (13,48), fac(hex(8c)), scrnum$(43)            , ch(03),~
               at (13,52), fac(lfac$(43)), percent$(43)         , ch(08),~
               at (14,48), fac(hex(8c)), scrnum$(44)            , ch(03),~
               at (14,52), fac(lfac$(44)), percent$(44)         , ch(08),~
               at (15,48), fac(hex(8c)), scrnum$(45)            , ch(03),~
               at (15,52), fac(lfac$(45)), percent$(45)         , ch(08),~
               at (16,48), fac(hex(8c)), scrnum$(46)            , ch(03),~
               at (16,52), fac(lfac$(46)), percent$(46)         , ch(08),~
               at (17,48), fac(hex(8c)), scrnum$(47)            , ch(03),~
               at (17,52), fac(lfac$(47)), percent$(47)         , ch(08),~
               at (18,48), fac(hex(8c)), scrnum$(48)            , ch(03),~
               at (18,52), fac(lfac$(48)), percent$(48)         , ch(08),~
               at (07,62), fac(hex(8c)), scrnum$(49)            , ch(03),~
               at (07,66), fac(lfac$(49)), percent$(49)         , ch(08),~
               at (08,62), fac(hex(8c)), scrnum$(50)            , ch(03),~
               at (08,66), fac(lfac$(50)), percent$(50)         , ch(08),~
               at (09,62), fac(hex(8c)), scrnum$(51)            , ch(03),~
               at (09,66), fac(lfac$(51)), percent$(51)         , ch(08),~
               at (10,62), fac(hex(8c)), scrnum$(52)            , ch(03),~
               at (10,66), fac(lfac$(52)), percent$(52)         , ch(08),~
               at (11,62), fac(hex(8c)), scrnum$(53)            , ch(03),~
               at (11,66), fac(lfac$(53)), percent$(53)         , ch(08),~
               at (12,62), fac(hex(8c)), scrnum$(54)            , ch(03),~
               at (12,66), fac(lfac$(54)), percent$(54)         , ch(08),~
               at (13,62), fac(hex(8c)), scrnum$(55)            , ch(03),~
               at (13,66), fac(lfac$(55)), percent$(55)         , ch(08),~
               at (14,62), fac(hex(8c)), scrnum$(56)            , ch(03),~
               at (14,66), fac(lfac$(56)), percent$(56)         , ch(08),~
               at (15,62), fac(hex(8c)), scrnum$(57)            , ch(03),~
               at (15,66), fac(lfac$(57)), percent$(57)         , ch(08),~
               at (16,62), fac(hex(8c)), scrnum$(58)            , ch(03),~
               at (16,66), fac(lfac$(58)), percent$(58)         , ch(08),~
               at (17,62), fac(hex(8c)), scrnum$(59)            , ch(03),~
               at (17,66), fac(lfac$(59)), percent$(59)         , ch(08),~
               at (18,62), fac(hex(8c)), scrnum$(60)            , ch(03),~
               at (18,66), fac(lfac$(60)), percent$(60)         , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               at (23,34), fac(lfac$(62)), accr$                , ch(01),~
                                                                         ~
               at (24,37), fac(lfac$(61)), dbper$               , ch(03),~
               at (24,44), fac(lfac$(61)), accr$                , ch(01),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L43010
                  call "MANUAL" ("FADEPRTB") : goto L41550

L43010:        if keyhit% <> 15 then L43040
                  call "PRNTSCRN" : goto L41550

L43040:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        line3$ = "Group " & group$ & ",  Recovery Period " & recovery$ & ~
                 " years"
        line4$ = "Proration Convention " & convdescr$ &                  ~
                          ",  Period Placed in Service " & period$
        if convention$ = " " then line4$ = "      No Convention"
        call "STRING" addr("CT", line3$, 79%)
        call "STRING" addr("CT", line4$, 79%)

        if edit% = 2% then L43290     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (9)Default S/L         " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (10)Def DB to S/L      " &        ~
                     "                       (16/32)Save/Exit"
            pfkeys$ = hex(01ffff04ffffffff090affff0dff0f102000)
            if fieldnr% > 2% then L43270
                str(pf$(1),18,20) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L43270:     return

L43290: if fieldnr% > 0% and fieldnr% <= 60% then L43490  /* Select Fld */
            pf$(1) = "(1)Start Over    (4)Previous Screen     " &        ~
                     "                       (13)Instructions"
            pf$(2) = "(2)Clear Table   (9)Default S/L         " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (10)Def DB to S/L     %" &        ~
                     ",                      (16/32)Save/Exit"
            pfkeys$ = hex(0102ff04ffffffffffffffff0dff0f102000)
            if fieldnr% > 60% then L43410
              str(pf$(3),17,30) = " "
              str(pf$(2),17,30) = " "
              goto L43480
L43410:     if fieldnr% = 61% then str(pf$(2),17,30) = " "
            if fieldnr% = 62% then str(pf$(3),17,30) = " "
            str(pf$(1),17,30) = " " : str(pfkeys$,4,1) = hex(ff)
            str(pf$(3),64,16) = " "
            str(pfkeys$,16,1) = hex(ff)
            str(pfkeys$,17,1) = hex(ff)
            str(pf$(2),1,16) = " " : str(pfkeys$,2,1) = hex(ff)
L43480:     return
L43490:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50170,         /* Group                  */~
                              L50300,         /* Recovery               */~
                              L50520,         /* Convention             */~
                              L50760,         /* Period                 */~
                              L51030,         /* Beg. Date              */~
                              L51070,         /* End Date               */~
                              L51190          /* Type                   */
            return

L50170
*        Test for Group                        GROUP$
            call "STRING" addr( "LJ", group$, 10% )
            t% = pos(group$ = "?")
            if t% <> 0% or group$ = " " then L50220
              return
L50220:     descr$ = hex(06) & "Select an existing TABLE GROUP or "     &~
                                  "PRESS PF(16) to create the New Table."
            plowkey$ = str(group$,,) & hex(000000)
            call "PLOWCODE" (#02, plowkey$, descr$, -10%, -.001, f1%(2))
            if f1%(2) = 0% then errormsg$ = hex(00)                      ~
                                         else group$ = str(plowkey$,1,10)
            return

L50300
*        Test for Recovery                     RECOVERY$
            t% = pos(recovery$="?")
            if t% <> 0% or recovery$ = " " then L50380
              call "NUMTEST"(recovery$, 1, 60, errormsg$, 2.2,recovery)
              if errormsg$ <> " " then L50500
              convert recovery to recovery$, pic(##.##)
              recovery% = min(round(recovery+1.4,0), 60)
              return
L50380:     descr$ = hex(06) & "Select an existing RECOVERY PERIOD or "  ~
                                 & "PRESS PF(16) to create the New Table"
            plowkey$ = str(group$,,) & str(recovery$,,) & hex(000000)
            call "PLOWCODE" (#02, plowkey$, descr$, 2010%, 0, f1%(2),    ~
                                                               hdr$(), 5)
            if f1%(2) <> 0% then L50460
              errormsg$ = hex(00)
              return
L50460:     convert str(plowkey$,11,5) to recovery,                      ~
                                               data goto shouldnt_happen
            recovery% = min(round(recovery+1.4,0),60)
            recovery$ = str(plowkey$,11,5)
L50500:     return

L50520
*        Test for Proration Convention         CONVENTION$
            if convention$ = "?" then L50570
              if pos("123 " = convention$) = 0% then L50720
              gosub get_convention_descr
              return
L50570:     descr$ = hex(06) & "Select an existing PRORATION CONVENTION" ~
                            & " or PRESS PF(16) to create the New Table."
            if convention$ = "?" then convention$ = hex(00)
            plowkey$=str(group$) & str(recovery$) & str(convention$) &   ~
                                                                "  "
            call "PLOWCODE" (#02, plowkey$, descr$, 2015%, 0, f1%(2),    ~
                                                               hdr$(), 1)
            if f1%(2) = 1% then convention$ = str(plowkey$,16,1)
            if convention$ <> hex(00) then L50690
              convention$ = "?"
              errormsg$ = hex(00)
              return
L50690:     if f1%(2) = 0% and pos("123 " = convention$) = 0% then L50720
            gosub get_convention_descr
            return
L50720:         errormsg$ = "Invalid Convention: MUST be 1, 2, 3 or blank~
        ~.  Please Reenter."
                return

L50760
*        Test for Period Put in Service        PERIOD$
            if convention$ = "1" or convention$ = " " then L50860
            t% = pos(period$="?")
            if t% <> 0% or period$ = "  " then L50880
              if convention$ = "2" then                                  ~
                   call "NUMTEST"(period$, 1, 4, errormsg$, 2.0, period)
              if convention$ = "3" then                                  ~
                   call "NUMTEST"(period$, 1, 12, errormsg$, 2.0, period)
              convert period to period$, pic(##)
              if errormsg$ <> " " then return
L50860:       gosub dataload
              if f1%(2) <> 0% then editpg1 else return
L50880:     descr$ = hex(06) & "Select an existing INSERVICE PERIOD or "&~
                                  "PRESS PF(16) to create the New Table."
            if t% <> 0% then period$ = hex(00)
            plowkey$=str(group$,,)&str(recovery$,,)&str(convention$)&    ~
                                                           str(period$,,)
            call "PLOWCODE" (#02, plowkey$, descr$, 2016%, 0, f1%(2),    ~
                                                               hdr$(), 2)
            if f1%(2) <> 0% then L50990
              if t% <> 0% then period$ = "? "
              errormsg$ = hex(00)
              return
L50990:     period$ = str(plowkey$,17,2)
            gosub dataload
            goto editpg1

L51030
*        Test for Beginning Date               BEGDATE$
            call "DATEOKC" (begdate$, u3%, errormsg$)
            return

L51070
*        Test for Ending Date                  ENDDATE$()
            if enddate$ = " " or enddate$ = blankdate$ then return
            call "DATEOKC" (enddate$, u3%, errormsg$)
            if errormsg$ <> " " then return
            call "DATUFMTC" (begdate$)
            call "DATUFMTC" (enddate$)
            if enddate$ < begdate$ then errormsg$ =                      ~
              "The Ending Date must be prior to the Beginning Date."
            call "DATFMTC" (begdate$)
            call "DATFMTC" (enddate$)
            return

L51190
*        Test for Property Type                TYPE$
            str(ttype$,1,7) = str(type$,1,7)
            type$ = "       "
            if pos(ttype$="R") <> 0% then str(type$,1,1) = "R"
            if pos(ttype$="P") <> 0% then str(type$,2,1) = "P"
            if pos(ttype$="L") <> 0% then str(type$,3,1) = "L"
            if pos(ttype$="A") <> 0% then str(type$,4,1) = "A"
            if pos(ttype$="E") <> 0% then str(type$,5,1) = "E"
            if pos(ttype$="X") <> 0% then str(type$,6,1) = "X"
            if pos(ttype$="O") <> 0% then str(type$,7,1) = "O"
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(fieldnr%, edit%)
            errormsg$ = " "

*        Test for percents                     PERCENT$()
            call "NUMTEST" (percent$(fieldnr%), 0, 100, errormsg$,       ~
                                                       7.4, percents(i%))
            if edit% <> 2% or errormsg$ <> " " then goto L51610
            total = 0
            for i%=1% to recovery%
               call "NUMTEST" (percent$(i%), 0, 100, errormsg$, 7.4,     ~
                                                            percents(i%))
               convert percent$(i%) to percents(i%),                     ~
                                                data goto shouldnt_happen
               convert percents(i%) to percent$(i%), pic(###.####)
               if errormsg$ = " " then L51540
                 fieldnr% = i%
                 return
L51540:        total = total + percents(i%)
            next i%
            total = round(total,4)
            if total = 100 then L51610
              convert total to total$, pic(###.####)
              errormsg$ = "ERROR- Percents total " & total$ &            ~
                                                ".  They must total 100."
L51610:     return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
