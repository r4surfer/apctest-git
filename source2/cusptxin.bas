        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   CCC   U   U   SSS   PPPP   TTTTT  X   X  IIIII  N   N   *~
            *  C   C  U   U  S      P   P    T     X X     I    NN  N   *~
            *  C      U   U   SSS   PPPP     T      X      I    N N N   *~
            *  C   C  U   U      S  P        T     X X     I    N  NN   *~
            *   CCC    UUU    SSS   P        T    X   X  IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CUSPTXIN - Program allows users to create and maintain a  *~
            *            cross reference file of Customer/Manufacturer  *~
            *            part numbers and their corresponding CMS       *~
            *            internal part numbers.                         *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/11/93 ! Original                                 ! MLJ *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            codedescr$30,                /* Code Description           */~
            company$60,                  /* Company Name               */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr$30,                    /* Part Descr.                */~
            descr_map(8),                /* Description Map For PLOW   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            genkey$9,                    /* GENCODES File Key          */~
            header3$36,                  /* Report Sequence Header     */~
            hdr$(3)79,                   /* PLOWCODE Headers           */~
            incl$(1)1,                   /* PLOWCODE Include/Exclude   */~
            incl(1),                     /* PLOWCODE Include/Exclude   */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            intdescr$32,                 /* Internal Part Description  */~
            intpart$25,                  /* Internal Part Number       */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            old_intpart$25,              /* Old Internal Part Number   */~
            pdescr$50,                   /* PLOWCODE Description       */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            refcode$9,                   /* Code                       */~
            refpart$25,                  /* Part No.                   */~
            reftype$1,                   /* Type                       */~
            rptid$6,                     /* Report ID                  */~
            runtime$8,                   /* System Run Time            */~
            typedescr$14,                /* Type Description           */~
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
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
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
            * #01 ! CUSPTXRF ! Customer Part Number Cross Reference Fil *~
            * #02 ! CUSTOMER ! Customer Master File                     *~
            * #03 ! GENCODES ! System General Codes file.               *~
            * #04 ! HNYMASTR ! Inventory Master File                    *~
            * #05 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "CUSPTXRF",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   26, keylen =  35,                     ~
                        alt key  1, keypos =  1, keylen = 60

            select #02, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #03, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #04, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #05, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 100%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),   0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),   0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),   0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),   0%, rslt$(5%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            ret% = 0%
            call "COMPNAME" (12%, company$, ret%)
            rptid$ = "CUS005"
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,1%,34%) = "By Customer or Manufacturer Number"
            str(line2$,62%) = "CUSPTXIN: " & str(cms2v$,,8%)

            genkey$ = "MFG CODES"

        REM Plowcode Screen Mapping...
            hdr$(1%)               = "  TYPE"
            str(hdr$(1%),8%,7%)    = "CUS/MFG"
            str(hdr$(1%),18%,12%)  = "CUS/MFG PART"
            str(hdr$(1%),43%,11%)  = "DESCRIPTION"
                descr_map(1%) = 26.010  :  descr_map(2%) =  1.0
                descr_map(3%) = 27.090  :  descr_map(4%) =  6.0
                descr_map(5%) = 36.250  :  descr_map(6%) = 16.0
                descr_map(7%) = 61.300  :  descr_map(8%) = 41.0

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  3%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
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
                  if keyhit%  = 12% then gosub delete_code
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11130:     if cursor%(1%) < 13% then fieldnr% = 2% else fieldnr% = 3%
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11130

        REM *************************************************************~
            *                P R I N T   R E P O R T                    *~
            * --------------------------------------------------------- *~
            * Print Cross Reference File Report.                        *~
            *************************************************************

        print_report
            page% =   0%
            line% = 999%
            if keyhit% = 11% then L12120
                header3$ = "By Internal Part Number"
                goto L12130
L12120:     header3$ = "By Customer/Manufacturer Code"
L12130:     call "STRING" addr("CT", header3$, 36%)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            select printer
            runtime$ = " "
            call "TIME" (runtime$)
            init(hex(00)) plowkey$
            rpt% = keyhit%
            if keyhit% = 9% then                                         ~
                call "REDALT4" (#1, plowkey$, 1%, f1%(1%)) else          ~
                call "REDALT4" (#1, plowkey$, 0%, f1%(1%))
                    if f1%(1%) = 0% then L12240
                call "SHOSTAT" ("Printing Cross Reference Report")
                goto L12380
L12240:             errormsg$ = "No Records Exist for Printing."
                    return

*        REPORT_LOOP
L12290:     call "READNEXT" (#1, f1%(1%))
            if f1%(1%) <> 0% then L12380
                print skip(2)
                runtime$ = " " :  call "TIME" (runtime$)
                print "***** End Of Report @ " & runtime$ & " *****"
                close printer
                call "SETPRNT" (rptid$, " ", 0%, 1%)
                goto inputmode

L12380:     get #1 using L12400, intpart$, reftype$, refcode$, refpart$,  ~
                   descr$
L12400:         FMT CH(25), CH(1), CH(9), CH(25), CH(30)
            call "DESCRIBE" (#4, str(intpart$), intdescr$, 0%, f1%(4%))
                if f1%(4%) = 0% then intdescr$ = " "
            if line% > 55% then gosub page_heading
            if rpt% = 9%                                                 ~
                then print using L12870, intpart$, intdescr$, reftype$,   ~
                                             refcode$, refpart$, descr$  ~
                else print using L12830, reftype$, refcode$, refpart$,    ~
                                        descr$, intpart$, intdescr$
            line% = line% + 1%
            goto L12290

        page_heading
            print page
            page% = page% + 1% : line% = 7%
            print using L12670, date$,runtime$,company$,"CUSPTXIN",rptid$
            print using L12700, page%
            print using L12730, header3$
            print
            if rpt% = 11% then L12630
                 print using L12910
                 print using L12940
                 return
L12630:     print using L12760
            print using L12790
            return

L12670: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:######

L12700: %                                                  Customer Part ~
        ~Number Cross Reference                                PAGE: ####

L12730: %                                                  ##############~
        ~######################

L12760: %Type  Ref Code  Ref Part Number           Ref Part Description  ~
        ~          Internal Part Number      Internal Part Description

L12790: %----  --------- ------------------------- ----------------------~
        ~--------  ------------------------- -----------------------------~
        ~--

L12830: %#     ######### ######################### ######################~
        ~########  ######################### #############################~
        ~##

L12870: %######################### ################################ #    ~
        ~######### ######################### #############################~
        ~#

L12910: %Internal Part Number      Internal Part Description        Type ~
        ~Ref Code  Reference Part Number     Reference Part Description   ~

L12940: %------------------------- -------------------------------- ---- ~
        ~--------- ------------------------- -----------------------------~
        ~-

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            plowkey$ = str(reftype$) & str(refcode$) & str(refpart$)
            call "READ101" (#1, plowkey$, f1%(1%))
                if intpart$ = old_intpart$ then L19120
                    call "DELETE" (#1, plowkey$, 35%)
                    f1%(1%) = 0%
L19120:     put #1 using L19140, intpart$, reftype$, refcode$, refpart$,  ~
                   descr$, " "
L19140:         FMT CH(25), CH(1), CH(9), CH(25), CH(30), CH(10)
            if f1%(1%) = 1% then rewrite #1 else write #1
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20120,         /* Ref Type, Code, Part   */~
                              L20170,         /* Reference Part Descr   */~
                              L20210          /* Internal Part Number   */
            return
L20120: REM Def/Enable Ref Type,Code,Part...  REFTYPE$, REFCODE$, REFPART$
            inpmessage$ = "Enter Reference Type, Code & Part or Leave B"&~
                          "lank to Search."
            return

L20170: REM Def/Enable Reference Part Descr...     DESCR$
            inpmessage$ = "Enter Reference Part Number Description."
            return

L20210: REM Def/Enable Internal Part Number...     INTPART$
            inpmessage$ = "Enter CMS Internal Part Number or Leave Blan"&~
                          "k to Search."
            return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      reftype$, typedescr$, refcode$, codedescr$,        ~
                      refpart$, descr$, intpart$, intdescr$, pdescr$,    ~
                      old_intpart$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
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
            plowkey$ = str(reftype$) & str(refcode$) & str(refpart$)
            call "READ100" (#1, plowkey$, f1%(1%))
                if f1%(1%) = 0% then return
            get #1 using L30110, intpart$, descr$
L30110:         FMT CH(25), POS(61), CH(30)
            call "DESCRIBE" (#4, str(intpart$), intdescr$, 1%, f1%(4%))
            old_intpart$ = str(intpart$)
            return

        REM *************************************************************~
            *               D E L E T E   R E C O R D                   *~
            *-----------------------------------------------------------*~
            * Delete Currently Displayed Cross Reference Record.        *~
            *************************************************************

        delete_code
            plowkey$ = str(reftype$) & str(refcode$) & str(refpart$)
L32080:     keyhit% = 2%
            call "ASKUSER" (keyhit%, "*** DELETE RECORD ***",            ~
                 "Press RETURN To Delete This Record", "-OR-",           ~
                 "Press PF1 To CANCEL Delete And Return.")
            if keyhit% = 1% then return
                if keyhit% <> 0% then L32080
            call "DELETE" (#1, plowkey$, 35%)
            return clear all
            goto inputmode

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              if fieldnr% = 0% then inpmessage$ = edtmessage$
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              if fieldnr% = 2% then lfac$(fieldnr%) = hex(80)            ~
                               else lfac$(fieldnr%) = hex(81)

L40140:     accept                                                       ~
               at (01,02),                                               ~
                  "Customer Part Cross Reference Management",            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Ref. Type",                                  ~
               at (06,16), fac(lfac$(1%)), reftype$             , ch(01),~
               at (06,43), fac(hex(8c)),   typedescr$           , ch(14),~
                                                                         ~
               at (08,02), "Ref. Code",                                  ~
               at (08,16), fac(lfac$(1%)), refcode$             , ch(09),~
               at (08,43), fac(hex(8c)),   codedescr$           , ch(32),~
                                                                         ~
               at (10,02), "Ref. Part",                                  ~
               at (10,16), fac(lfac$(1%)), refpart$             , ch(25),~
                                                                         ~
               at (12,02), "Part Descr.",                                ~
               at (12,16), fac(lfac$(2%)), descr$               , ch(30),~
                                                                         ~
               at (14,02), "Internal Part",                              ~
               at (14,16), fac(lfac$(3%)), intpart$             , ch(25),~
               at (14,43), fac(hex(8c)),   intdescr$            , ch(34),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9% then L40510
                  gosub print_report
                  goto L40140

L40510:        if keyhit% <> 11% then L40550
                  gosub print_report
                  goto L40140

L40550:        if keyhit% <> 13% then L40590
                  call "MANUAL" ("CUSPTXIN")
                  goto L40140

L40590:        if keyhit% <> 15% then L40630
                  call "PRNTSCRN"
                  goto L40140

L40630:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40820     /*  Input Mode             */
            pf$(1%) = "(1)Start Over            (9)List By Inte" &       ~
                     "rnal Part              (13)Instructions"
            pf$(2%) = "                        (11)List By Cust" &       ~
                     "omer/Manufacturer      (15)Print Screen"
            pf$(3%) = "(4)Previous Field                       " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffff09ff0bff0dff0f1000)
            if fieldnr% = 1% then L40780
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L40780:     if fieldnr% > 2% then L40800
                str(pf$(3%),1%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40800:     return

L40820: if fieldnr% > 0% then L40910  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over            (9)List By Inte" &       ~
                     "rnal Part              (13)Instructions"
            pf$(2%) = "                        (11)List By Cust" &       ~
                     "omer/Manufacturer      (15)Print Screen"
            pf$(3%) = "                        (12)Delete Recor" &       ~
                     "d                      (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffff09ff0b0c0dff0f1000)
            return
L40910:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over            (9)List By Inte" &       ~
                     "rnal Part              (13)Instructions"
            pf$(2%) = "                        (11)List By Cust" &       ~
                     "omer/Manufacturer      (15)Print Screen"
            pf$(3%) = "                        (12)Delete Recor" &       ~
                     "d                                      "
            pfkeys$ = hex(01ffffffffffffff09ff0b0c0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50130,         /* Ref Type, Code, Part   */~
                              L50950,         /* Reference Part Descr   */~
                              L51000          /* Internal Part Number   */
            return

L50130: REM Test for Ref Type, Code & Part...
*        First Test Entire Primary Key (type, code & part)...
          plowkey$ = str(reftype$) & str(refcode$) & str(refpart$)
          if reftype$ = " " and refcode$ = " " and refpart$ = " "        ~
              then L50220                            /* Selection Screen */
          if reftype$ = " " or refcode$ = " " or refpart$ = " "          ~
              then L50430                           /* Edit Individually */
          call "READ100" (#1, plowkey$, f1%(1%))
              if f1%(1%) = 1% then L50270 else L50430
L50220:   pdescr$ = hex(06) & "Select Reference Type, Code & Part"
          call "PLOWCODE" (#1, plowkey$, pdescr$, 9000%, 0.30, f1%(1%),  ~
               hdr$(), 0, 0, incl(), incl$(), "D", " ", #1,              ~
               descr_map())
              if f1%(1%) = 0% then L50430
L50270:   reftype$ = str(plowkey$,1%,1%)
          refcode$ = str(plowkey$,2%,9%)
          refpart$ = str(plowkey$,11%,25%)
          if reftype$ = "M" then L50350
              typedescr$ = "(CUSTOMER)"
              plowkey$ = str(refcode$)
              call "DESCRIBE" (#2, plowkey$, codedescr$, 1%, f1%(2%))
              goto L50380
L50350:   typedescr$ = "(MANUFACTURER)"
          plowkey$ = genkey$ & str(refcode$)
          call "DESCRIBE" (#3, plowkey$, codedescr$, 1%, f1%(3%))
L50380:   gosub dataload
              if f1%(1%) = 0% then return                 /* New Entry */
          return clear all
          goto editpg1

L50430
*        Test Reference Type Code...
            if reftype$ = "?" then reftype$ = " "
                if reftype$ = "C" or reftype$ = "M" then L50490
                    errormsg$ = "Reference Type MUST Be 'C'ustomer Or" & ~
                                " 'M'anufacturer"
                    return
L50490:         if reftype$ = "C" then typedescr$ = "(CUSTOMER)" else    ~
                                       typedescr$ = "(MANUFACTURER)"

*        Test Reference Code...
            if refcode$ = "?" then refcode$ = " "
                if reftype$ = "M" then L50630
            plowkey$ = str(refcode$) & hex(00)
            codedescr$ = hex(06) & "Select Customer Ref Code"
            call "PLOWCODE" (#2, plowkey$, codedescr$, 0%, 0.30, f1%(2%))
                if f1%(2%) = 0% then L50610
                    refcode$ = str(plowkey$,1%,9%)
                    goto L50710
L50610:         errormsg$ = "Ref Code NOT In Customer File"
                return
L50630:     plowkey$ = genkey$ & str(refcode$)
            codedescr$ = hex(06)& hex(84)& "Select Manufacturer Ref"    &~
                         " Code"
            call "PLOWCODE"(#3, plowkey$, codedescr$, 9%, 0.30, f1%(3%))
            if f1%(3%) = 1% then L50700
                errormsg$ = "Ref Code NOT In GENCODES File"
                return
L50700:     refcode$ = str(plowkey$,10%,9%)
L50710:     call "PUTPAREN" (codedescr$)

*        Test Reference Part Number...
            if refpart$ = "?" then refpart$ = " "
            plowkey$ = str(reftype$) & str(refcode$) & str(refpart$)
            if refpart$ = " " then L50800
                call "READ100" (#1, plowkey$, f1%(1%))
                    if f1%(1%) = 1% then L50900
                    return
L50800:     pdescr$ = hex(06) & "Select Reference Part For "
            if reftype$ = "C" then                                       ~
                str(pdescr$,28%,19%) = "Customer: " & refcode$ else      ~
                str(pdescr$,28%,23%) = "Manufacturer: " & refcode$
            call "PLOWCODE" (#1, plowkey$, pdescr$, 10%, 0.0, f1%(1%))
            if f1%(1%) = 1% then L50890
                if refpart$ <> " " then return
                     errormsg$ = "Ref Part Number CANNOT Be Blank"
                     return
L50890:     refpart$ = str(plowkey$,11%,25%)
L50900:     gosub dataload
                if f1%(1%) = 0% then return
            return clear all
            goto editpg1

L50950: REM Test for Reference part Description...     DESCR$
            if descr$ <> " " then return
                errormsg$ = "Reference Part Description CANNOT Be Blank"
                return

L51000: REM Test for Internal Part Number...           INTPART$
            if intpart$ = "?" then intpart$ = " "
            plowkey$ = str(intpart$) & hex(00)
            pdescr$ = hex(06) & "Select Internal Part Number"
            call "GETCODE" (#4,plowkey$, intdescr$, 1%, 0.32, f1%(4%))
                if f1%(4%) = 0% then L51080
                    intpart$ = str(plowkey$,1%,25%)
                    return
L51080:     if intpart$ = " " then                                       ~
                errormsg$ = "Internal Part Number CANNOT Be Blank" else  ~
                errormsg$ = "Internal Part Number Does NOT Exist In HNY"&~
                            "MASTR"
                return

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
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
