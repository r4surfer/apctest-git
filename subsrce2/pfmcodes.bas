        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP   FFFFF  M   M   CCC    OOO   DDDD   EEEEE   SSS    *~
            *  P   P  F      MM MM  C   C  O   O  D   D  E      S       *~
            *  PPPP   FFFF   M M M  C      O   O  D   D  EEEE    SSS    *~
            *  P      F      M   M  C   C  O   O  D   D  E          S   *~
            *  P      F      M   M   CCC    OOO   DDDD   EEEEE   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PFMCODES - This Sub-Routine is to allow the displaying of *~
            *            the Forecast Group Codes and types which are   *~
            *            stored on the GENCODES file without displaying *~
            *            all the other codes on file.                   *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/06/90 ! Original                                 ! RJB *~
            * 06/08/92 ! Modified greatly (re-wrote)              ! JBK *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "PFMCODES" (progmode$,    /* Program Mode:                 */~
                                      /* 'D' = Display Data Only       */~
                                      /* 'V' = Varify & Retrieve       */~
                          cdcntr%,    /* Counter Used to locate the    */~
                                      /* Primary Search Key from the   */~
                                      /* GENCDKEY$ Array               */~
                          errormsg$,  /* Error Message From Caller     */~
                          retvalue$,  /* Code/Type value returned to   */~
                                      /* calling program when PROGMODE */~
                                      /* is equal to 'V'.              */~
                          retdesc$)   /* Code/Type Description returned*/~
                                      /* to calling program when       */~
                                      /* PROGMODE is equal to 'V'.     */

        dim                                                              ~
            codedesc$(60)30,             /* Forecast Code Descrition   */~
            codename$30,                 /* Forecast Code Title        */~
            date$8,                      /* Date for screen display    */~
            gencdkey$(8)9,               /* GENCODES Read/Plow Key     */~
            gencode1$15,                 /* Primary GENCODE Key        */~
            gencode2$15,                 /* Secondary GENCODE Key      */~
            gen_exist$(8)1,              /* GENCODE Existence Flags    */~
            genid$9,                     /* GENCODE File ID            */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            pfmcode$(60)1,               /* Code and Type Values       */~
            plowdescr$50,                /* PLOWCODE Description fld   */~
            plowkey$99,                  /* Miscellaneous Plow Key     */~
            readkey$99,                  /* Miscellaneous Read Key     */~
            retvalue$1,                  /* Returned Value, Mode=V     */~
            retdesc$30,                  /* Returned Descr., Mode=V    */~
            userid$3,                    /* Current User Id            */~
            whichchar$(8)16              /* Code/Type Char. Number     */

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
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "
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
            * #01 ! GENCODES ! System General Codes file.               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            if beenherebefore% = 1% then whichway

            select #01, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24                      ~


            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            str(line2$,62) = "PFMCODES: " & str(cms2v$,,8)

            gencdkey$(1) = "PFMCODE1"
            gencdkey$(2) = "PFMCODE2"
            gencdkey$(3) = "PFMCODE3"
            gencdkey$(4) = "PFMCODE4"
            gencdkey$(5) = "PFMCODE5"
            gencdkey$(6) = "PFMCODE6"
            gencdkey$(7) = "PFMTYPE1"
            gencdkey$(8) = "PFMTYPE2"

            whichchar$(1) = "First  Character"
            whichchar$(2) = "Second Character"
            whichchar$(3) = "Third  Character"
            whichchar$(4) = "Fourth Character"
            whichchar$(5) = "Fifth  Character"
            whichchar$(6) = "Sixth  Character"
            whichchar$(7) = "First  Character"
            whichchar$(8) = "Second Character"

*        Check to see which if any of the Gencodes Exist
            for x% = 1% to 8%
                init(hex(00)) readkey$
                str(readkey$,10) = str(gencdkey$(x%))
                call "READ100" (#1, readkey$, f1%(1))
                if f1%(1) = 1% then gen_exist$(x%) = "X"
            next x%

            beenherebefore% = 1%

        REM *************************************************************~
            *       P R O G R A M   C O N T R O L   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Establishes Primary Key for Secondary GENCODES look-up.   *~
            *************************************************************

        whichway
            if progmode$ = "V" then verify_mode

            if str(gen_exist$()) <> " " then display_mode
                keyhit% = 0%
                call "ASKUSER" (keyhit%, "PFM CODE DISPLAY", "There " &  ~
                     "are NO PFM Group Codes or PFM Type Codes to "   &  ~
                     "Display", " ", "Hit ANY key to Continue...")
                goto exit_routine

        display_mode
            lc% = 0%
            cntr% = cdcntr%
            init (hex(00))  readkey$
            if gen_exist$(cntr%) = " " then next_code
            str(readkey$,10) = str(gencdkey$(cntr%))
            call "READ100" (#1, readkey$, f1%(1))
                if f1%(1) = 0% then next_code

            get #1 using L35060, genid$, gencode1$, codename$

            gosub dataload

L10290:     gosub screen_1
                if keyhit%  =  0% then       next_code
                if keyhit%  =  2% then lc% = 0%
                if keyhit%  =  3% then lc% = max(0,maxlines%-12)
                if keyhit%  =  4% then lc% = max(0,lc%-11)
                if keyhit%  =  5% then lc% = min(lc%+12,max(0,           ~
                                                   maxlines%-12))
                if keyhit%  =  6% then lc% = max(0,lc%-1)
                if keyhit%  =  7% then lc% = min(lc%+1,max(0,            ~
                                              maxlines%-12))
                if keyhit%  =  8% then       previous_code
                if keyhit%  =  9% then       next_code
                if keyhit%  = 16% then       exit_routine
                goto L10290

        previous_code
            cdcntr% = max(1, cdcntr% - 1%)
            if gen_exist$(cdcntr%) = " " then previous_code
            goto display_mode

        next_code
            cdcntr% = cdcntr% + 1%
            if cdcntr% > 8% then cdcntr% = 1%
            goto display_mode

        verify_mode
            init (" ")  plowkey$, plowdescr$
            plowkey$ = str(gencdkey$(cdcntr%)) & retvalue$
            plowdescr$ = hex(06) & "Select " & whichchar$(cdcntr%)
            if cdcntr% < 7% then plowdescr$ = plowdescr$ & " of PFM " &  ~
                                              "Group Code"               ~
                else plowdescr$ = plowdescr$ & " of PFM Type Code"

            call "PLOWCODE" (#1, plowkey$, plowdescr$, 9%, 0.3, f1%(1))

            if f1%(1) = 0% then L15180
                retvalue$ = str(plowkey$,10,1)
                retdesc$  = str(plowdescr$,1,30)
                goto exit_routine

L15180:     errormsg$ = retvalue$ & " is an Invalid Entry for this " &   ~
                                    "Character Position."
            goto exit_routine

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ")  errormsg$, inpmessage$, pfmcode$( ), codedesc$( )
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *************************************************************

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            gosub initialize_variables
            init (hex(00))  plowkey$
            str(plowkey$,1,9) = gencode1$
            codecntr% = 1%
            maxlines% = 0%
L30110:     call "PLOWNEXT" (#1, plowkey$, 9%, f1%(1))
                if f1%(1) = 0 then L30210

            get #1 using L35060, genid$, gencode2$, codedesc$(codecntr%)
                pfmcode$(codecntr%) = str(gencode2$,1,1)
                codecntr% = codecntr% + 1%
                maxlines% = maxlines% + 1%

            goto L30110

L30210:     return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35060: FMT                 /* FILE: GENCODES                          */~
            CH(9),          /* Logical File ID                         */~
            CH(15),         /* CODE                                    */~
            CH(30),         /* Generic for general code descriptions   */~
            CH(2),          /* Maximum allowable length of a Code      */~
            CH(0072)        /* Unused Space                            */~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            *  Display  Forecast Codes and Values                       *~
            *************************************************************

        screen_1
            init(" ")  inpmessage$, errormsg$
            inpmessage$ = "Hit RETURN to see the next code OR one of the ~
        ~Specified PF keys."
            lfac$(1) = hex(8d)
            gosub set_pf1d

L40120:     accept                                                       ~
               at (01,02),                                               ~
                  "Display Forecast Group Codes/Types",                  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Forecast Code Title",                        ~
               at (06,30), fac(hex(85)), codename$              , ch(30),~
               at (07,05), fac(hex(8c)), whichchar$(cntr%)       ,ch(16),~
               at (08,22), "Code Value",                                 ~
               at (08,33), fac(lfac$( 1)), pfmcode$(lc%+1%)      ,ch(01),~
               at (08,38), fac(hex(8c)),   codedesc$(lc%+1%)     ,ch(30),~
               at (09,33), fac(lfac$( 1)), pfmcode$(lc%+2)       ,ch(01),~
               at (09,38), fac(hex(8c)),   codedesc$(lc%+2)      ,ch(30),~
               at (10,33), fac(lfac$( 1)), pfmcode$(lc%+3)       ,ch(01),~
               at (10,38), fac(hex(8c)),   codedesc$(lc%+3)      ,ch(30),~
               at (11,33), fac(lfac$( 1)), pfmcode$(lc%+4)       ,ch(01),~
               at (11,38), fac(hex(8c)),   codedesc$(lc%+4)      ,ch(30),~
               at (12,33), fac(lfac$( 1)), pfmcode$(lc%+5)       ,ch(01),~
               at (12,38), fac(hex(8c)),   codedesc$(lc%+5)      ,ch(30),~
               at (13,33), fac(lfac$( 1)), pfmcode$(lc%+6)       ,ch(01),~
               at (13,38), fac(hex(8c)),   codedesc$(lc%+6)      ,ch(30),~
               at (14,33), fac(lfac$( 1)), pfmcode$(lc%+7)       ,ch(01),~
               at (14,38), fac(hex(8c)),   codedesc$(lc%+7)      ,ch(30),~
               at (15,33), fac(lfac$( 1)), pfmcode$(lc%+8)       ,ch(01),~
               at (15,38), fac(hex(8c)),   codedesc$(lc%+8)      ,ch(30),~
               at (16,33), fac(lfac$( 1)), pfmcode$(lc%+9)       ,ch(01),~
               at (16,38), fac(hex(8c)),   codedesc$(lc%+9)      ,ch(30),~
               at (17,33), fac(lfac$( 1)), pfmcode$(lc%+10)      ,ch(01),~
               at (17,38), fac(hex(8c)),   codedesc$(lc%+10)     ,ch(30),~
               at (18,33), fac(lfac$( 1)), pfmcode$(lc%+11)      ,ch(01),~
               at (18,38), fac(hex(8c)),   codedesc$(lc%+11)     ,ch(30),~
               at (19,33), fac(lfac$( 1)), pfmcode$(lc%+12)      ,ch(01),~
               at (19,38), fac(hex(8c)),   codedesc$(lc%+12)     ,ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                if keyhit% <> 13 then L40590
                   call "MANUAL" ("PFMCODES") : goto L40120

L40590:         if keyhit% <> 15 then L40620
                   call "PRNTSCRN" : goto L40120

L40620:         return

        set_pf1d
            pf$(1) = "                                        " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Return      "
            pfkeys$ = hex(ffffffffffffffffffffffff0dff0f1000)

            if lc% = 0% then L40810
                str(pf$(2), 1,15) = "(2)First"
                str(pfkeys$,2,1)  = hex(02)
                str(pf$(2),16,15) = "(4)Previous"
                str(pfkeys$,4,1)  = hex(04)
                str(pf$(2),32,15) = "(6)Down"
                str(pfkeys$,6,1)  = hex(06)

L40810:     if maxlines% <= lc% + 12% then L40890
                str(pf$(3), 1,15) = "(3)Last"
                str(pfkeys$,3,1)  = hex(03)
                str(pf$(3),16,15) = "(5)Next"
                str(pfkeys$,5,1)  = hex(05)
                str(pf$(3),32,15) = "(7)Up"
                str(pfkeys$,7,1)  = hex(07)

L40890:     if cdcntr% = 1% then L40930
                str(pf$(2),48,16) = "(8)Prev Code"
                str(pfkeys$,8,1)  = hex(08)

L40930:     if cdcntr% = 8% then L40970
                str(pf$(3),48,16) = "(9)Next Code"
                str(pfkeys$,9,1)  = hex(09)

L40970:     return

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
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_routine
            end
*        Return to Calling Program
