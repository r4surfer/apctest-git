        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP   RRRR   JJJJJ  PPPP   IIIII   CCC   K   K  L       *~
            *  P   P  R   R    J    P   P    I    C   C  K  K   L       *~
            *  PPPP   RRRR     J    PPPP     I    C      KKK    L       *~
            *  P      R   R  J J    P        I    C   C  K  K   L       *~
            *  P      R   R   J     P      IIIII   CCC   K   K  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRJPICKL - Generates Pick Lists for Projects              *~
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
            * 12/23/88 ! Original                                 ! MJB *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            columnttl$51,                /* Column titles line         */~
            currproj$8,                  /* Current Project            */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fmproj$8,                    /* Project Number Range       */~
            hiproj$8,                    /* Project Number Range       */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            job_descr$30,                /* Project Description        */~
            jbpart$25,                   /* Project Assembly Number    */~
            jbpartdescr$32,              /* Project Assembly Descr     */~
            jbquant$10,                  /* Project Assembly Quantity  */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            loproj$8,                    /* Project Number Range       */~
            part$25,                     /* Part Number                */~
            partdescr$32,                /* Part Description           */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            pipoutplow$56,               /* Plow Key                   */~
            quantity$10,                 /* Required Quantity          */~
            stdate$8,                    /* Start Date                 */~
            tagnr$19,                    /* PIP Tag Number             */~
            toproj$8,                    /* Project Number Range       */~
            xlines$2,                    /* Extra Lines per Project    */~
            userid$3                     /* Current User Id            */~

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
            cms2v$ = "R5.01.07 09/07/89 Patch Release R5.01.07          "
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
            * #01 ! JOBMASTR ! Project Master File.                     *~
            * #02 ! HNYMASTR ! Inventory Master File                    *~
            * #03 ! PIPOUT   ! Planned inventory use detail rec         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "JOBMASTR",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =   8                      ~

            select #02, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup     ~

            select #03, "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))

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

            str(columnttl$, 1) = "Beginning Code"
            str(columnttl$,27) = "Ending Code"

            str(line2$,62) = "PRJPICKL: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  2%
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
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       generate_report
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  2% then editpg1
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
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Project Numbers        */~
                              L20200          /* Extra Lines            */
            return
L20100: REM Def/Enable Project Number Range        FMPROJ$
            if fmproj$ = " " then fmproj$ = "ALL"
            return

L20200: REM Def/Enable Extra Lines per Project     XLINES$
            if xlines$ = " " then xlines$ = "1"
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Project Number Range                                   ",~
         "Enter Extra Lines per Part to print on Pick List             "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      fmproj$, hiproj$, loproj$, toproj$, xlines$

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
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************
        generate_report
            select printer(134)
            currproj$ = loproj$
            printer% = 0%
            call "SHOSTAT" ("Printing Project Pick Lists")
L30110:     call "PLOWNEXT" (#1, currproj$, 0%, f1%(1))
            if f1%(1) = 0 then exit_program
            tagnr$ = " "
            get #1, using L30160, currproj$, job_descr$,                  ~
                                 stdate$, jbpart$, qty
L30160:         FMT CH(8), CH(30), CH(6), XX(15), CH(25), PD(14,4)

            if currproj$ > hiproj$ then exit_program

            call "CONVERT" (qty, 2.2, jbquant$)
            call "GETCODE" (#2, jbpart$, jbpartdescr$, 0%, 99, f1%(2))
            call "DATEFMT" (stdate$)
            page% = 0%  :  line% = 99%  :  printed% = 0%

            init (hex(00)) pipoutplow$
            str(pipoutplow$,1,19) = "JOB(PROJ): " & str(currproj$,1,8)

L30310:     call "PLOWNEXT" (#3, pipoutplow$, 19%, f1%(3))
            if f1%(3) = 0% then L30520
                printer%, printed% = 1%
                if line% >= 54% then gosub page_head
                get #3, using L30360, part$, qty
L30360:             FMT XX(19), CH(25), XX(12), PD(14,4)
                call "GETCODE" (#2, part$, partdescr$, 0%, 99, f1%(2))
                call "CONVERT" (qty, 2.2, quantity$)
                print using L60300, part$, quantity$
                print using L60320, partdescr$
                if xlines% <= 0% then L30490
                    for i% = 1% to xlines%
                        print using L60340
                        print skip(1)
                        line% = line% + 2%
                    next i%
L30490:         print skip(1)
                line% = line% + 3%
                goto L30310
L30520:     if printed% = 0% then L30110
                print using L60360
                goto L30110

        page_head              /* Page Heading Print Routine */
           print page
           page% = page% + 1%
           line% = 1%
           print using L60150, date$, page%
           print using L60170
           print using L60190, currproj$, job_descr$
           print skip(1)
           print using L60210, jbpart$, jbpartdescr$
           print using L60230, jbquant$
           print skip(1)
           print using L60250
           print using L60270
           print skip(1)
           return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

        FMT                 /* FILE: JOBMASTR                          */~
            CH(8),          /* Job Number                              */~
            CH(30),         /* Job description                         */~
            XX(21),         /* Spacer                                  */~
            CH(25),         /* Part Number                             */~
            PD(14,4)        /* Quantity to be manufactured             */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40080,         /* Project Numbers   */   ~
                                L40085          /* Extra Lines       */
              goto L40095

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40080:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40085:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40095:     accept                                                       ~
               at (01,02),                                               ~
                  "Input Report Selection Criteria",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), "Project Number Range",                       ~
               at (07,30), fac(lfac$( 1)), fmproj$              , ch(08),~
               at (07,56), fac(lfac$( 1)), toproj$              , ch(08),~
                                                                         ~
               at (08,02), "Extra Blank Lines per Part",                 ~
               at (08,30), fac(lfac$( 2)), xlines$              , ch(02),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40240
                  call "MANUAL" ("PRJPICKL") : goto L40095

L40240:        if keyhit% <> 15 then L40255
                  call "PRNTSCRN" : goto L40095

L40255:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40350     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40335
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L40340
L40335:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40340:     return

L40350: if fieldnr% > 0% then L40395  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40395:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* Project Numbers        */~
                              L50200          /* Extra Lines            */
            return
L50100: REM Test for Project Number Range         FMPROJ$
            call "TESTRNGE" (fmproj$,toproj$,loproj$,hiproj$,errormsg$)
            return

L50200: REM Test for Extra Lines per Part         XLINES$
            call "NUMTEST" (xlines$, 0, 3, errormsg$, 0.0, xlines)
            if errormsg$ <> " " then return
            xlines% = xlines
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

L60150: % ########                                                       ~
        ~      Page: ###

L60170: %                  * * * MATERIALS REQUIREMENT LIST * * *        ~


L60190: % Project Number:  ########        Project Desc:  ###############~
        ~###############

L60210: % Part No #########################   ###########################~
        ~#####

L60230: % Quantity To Build  ##########

L60250: % Part Number/Description           Qty Reqired  Qty Issued     W~
        ~arehouse/Lot

L60270: % ================================  ===========  ==========  ====~
        ~===============

L60300: % #########################         ##########   __________  ____~
        ~_____/_________

L60320: %    ################################


L60340: %                                                __________  ____~
        ~_____/_________

L60360: %    * * * END OF MATERIALS LIST * * *                           ~


        REM THISPROGRAMWASGENERATEDBYGENRPPGMAPROPRIETRYPRODUCTOFCAELUS**~
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
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")
            if printer% = 0% then end
            close printer
            end
