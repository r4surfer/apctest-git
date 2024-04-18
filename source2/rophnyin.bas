        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
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
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
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
            * 06/05/91 ! Added a CALL 'READ101' on #1 to decide   ! SID *~
            *          ! whether to do a WRITE or PUT at DATASAVE !     *~
            *          ! Added 'ALLFREE'                          !     *~
            * 07/09/91 ! @50320 s/b "READ101" to a "READ100"      ! SID *~
            * 01/07/92 ! @11370 s/b "DELETE" to a CALL "DELETE"   ! RJH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            clsdescr$30,                 /* For PLOWCODE               */~
            crflag$1,                    /* Critical Ratio Flag        */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr_map(26),               /* For PLOWCODE               */~
            devratio$6,                  /* For display                */~
            edtmessage$79,               /* Edit screen message        */~
            eoq$10,                      /* Economic Order Qty         */~
            errormsg$79,                 /* Error message              */~
            ess$10,                      /* Econmic Safety Stock       */~
            hdr$(4)132,                  /* For PLOWCODE               */~
            i$(24)80,                    /* Screen Image               */~
            incl(2),                     /* for PLOWCODE               */~
            incl$(2),                    /* for PLOWCODE               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
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
            ropdate$6,                   /* Date ROP last updated      */~
            rptdescr$30,                 /* For PLOWCODE               */~
            stddev$10,                   /* Standard Deviation         */~
            svrop$10,                    /* Saved ROP                  */~
            temp$6,                      /* For display                */~
            usage$10,                    /* Average Usage              */~
            userid$3                     /* Current User Id            */~

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.02 01/15/92 CMS Patch Release R6.01.02      "
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
            *************************************************************~
            *       File Selection and Open Calls                       *~
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

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 250%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))

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

            str(readkey$,1,9) = "ROP PARAM"
            call "READ100" (#3, readkey$, f1%(3))
            if f1%(3) = 1 then L09150
               kh% = 2
               call "ASKUSER" (kh%, "SYSTEM PARAMETER RECORD MISSING",   ~
           "The System Parameter record must be set up through ROPSYSIN",~
               "Press any key to exit this program", " ")
               goto L65000
L09150:     get #3, using L09170, devratio
L09170:         FMT POS(53), PD(14,4)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            editmode = 0
            pf4$ = " " : pf5$ = " " : pf16$ = "(16)Exit Program"
            pf12$ = " "
            pfkeys$ = hex(0001040c0d0f1009ffffff0a0b)
            gosub L29000

            for fieldnr% = 1 to 6
                if fieldnr% > 1 then pf4$ = "(4)Previous Field"
L10150:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                    if enabled% = 0 then L10370
L10170:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                    if keyhit%  =  1 then gosub startover
                    if keyhit% <>  4 then       L10250
L10200:                 fieldnr% = max(1%, fieldnr% - 1%)
                        gosub'051(fieldnr%)
                        if enabled% = 1% then L10170
                        if fieldnr% = 1% then L10150
                        goto L10200
L10250:             if keyhit% <> 9 then L10342
                        if fieldnr% <> 1 then L10170
                        init (hex(00)) plowkey$ : init(" ") hdr$()
                        partdescr$ = hex(06) & "ROP Part Records"
                        call "PLOWCODE" (#1,part$,partdescr$,8000%,0.32, ~
                            f1%(1), hdr$(), 0, -1.0026, incl(), incl$(), ~
                            " ", " ", #2%)
                        if f1%(1) = 1 then L10330
                            errormsg$ = "ROP Part Record Not Found"
                            goto L10170
L10330:                 call "PUTPAREN" (partdescr$)
                        goto L10370
L10342:             if keyhit% = 10 then gosub print_by_part
                    if keyhit% = 11 then gosub print_by_class
                    if keyhit% = 16 and fieldnr% = 1 then exit_program
                    if keyhit% <>  0 then       L10170
L10370:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                    if errormsg$ = " " then L10400
                        if skipfield = 0 then L10170
L10400:     next fieldnr%

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
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 12  and onfile = 1 then delete_record
                  if keyhit%  = 10 then gosub print_by_part
                  if keyhit%  = 11 then gosub print_by_class
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editpg1
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 2 or fieldnr% >  7 then editpg1
            if fieldnr% > 5 then fieldnr% = fieldnr% - 1
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
                  pf4$, pf5$, pf16$ = " "
L11220:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11220
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ = " " then L11280
                     if skipfield = 0 then L11220
L11280:     goto editpg1

        delete_record
L11310:     keyhit% = 2%
            call "ASKUSER" (keyhit%, "*** DELETE RECORD? ***", "Press Ret~
        ~urn To Delete This Record", "- OR -","Press PF1 To Cancel Delete ~
        ~& Return")
            if keyhit% = 1% then editpg1
            if keyhit% <> 0% then L11310
            readkey$ = str(part$)
            call  "DELETE" (#1, readkey$, 25%)
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
                              L20370          /* Standard Deviation */
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
            inpmessage$="Enter Economic Order Quantity and Average Usage"
            return

L20370: REM Def/Enable Standard Deviation          STDDEV$
            inpmessage$ = "Enter Standard Deviation"
            return

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
                      usage$                   /* Average Usage      */
            call "ALLFREE"
            onfile = 0
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
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
            get #1, using L30100, rop, eoq, usage, stddev, ropdate$,pclass$
L30100:     FMT POS(26), 3*PD(14,4), PD(14,7), POS(98), CH(6), CH(04)
            call "CONVERT" (rop, -2.4, rop$)
            call "CONVERT" (eoq, -2.4, eoq$)
            call "CONVERT" (usage, -2.4, usage$)
            call "CONVERT" (stddev, -2.4, stddev$)
            onfile = 1
            goto editpg1

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            readkey$ = str(part$)
            call "READ101" (#1, readkey$, f1%(1))
            if f1%(1) = 1 then L31130
            write #1, using L31100, part$, rop, eoq, usage, stddev, usage,~
                                  0, 0, 0, stddev, ropdate$, pclass$, " "
L31100:     FMT CH(25), 2*PD(14,4), PD(14,4), PD(14,7), 4*PD(14,4),      ~
                 PD(14,7), CH(6), CH(4), CH(149)
            goto L31142

L31130:     put #1, using L30100, rop, eoq, usage, stddev, ropdate$,      ~
                                 pclass$
            rewrite #1
L31142:     call "READ101" (#2, str(part$), f1%(2))
            put #2 using L31144, ess
L31144:     FMT POS(318), PD(14,4)
            rewrite #2
            return


        REM *************************************************************~
            *      P R I N T   R O P   P A R T S   B Y   P A R T        *~
            *-----------------------------------------------------------*~
            * Call PLOWCODE to Print ROP Parts by Part Report           *~
            *************************************************************
        print_by_part
            plowkey$ = " "  :  mat descr_map = zer
            rptdescr$ = hex(06) & "ROP PARTS BY PART NUMBER"
            incl(1) = 0  :  incl$(1) = " "
            hdr$(1) = "                                              " & ~
                      "                               ROP          EO" & ~
                      "Q    Std Usage      Std Dev          ESS"

                     /*xxxxxxxxxxxxxxxxxxxxxxxxx    xxxx    xxxxxxxxx*/
                     /*xxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxx   xxxxxxxxx*/
                     /*x   xxxxxxxxxx   xxxxxxxxxx   xxxxxxxxxx*/

            hdr$(4) = "Part Number               ROP CLass  Descripti" & ~
                      "on                         Old ROP      Old EO" & ~
                      "Q    Old Usage      Old Dev      Old ESS"

                     /*                                              */
                     /*                        xxxxxxxxxx   xxxxxxxxx*/
                     /*x   xxxxxxxxxx   xxxxxxxxxx   xxxxxxxxxx*/

        /* 1st Detail Line */
            descr_map( 1) =    1.25 : descr_map( 2) =     1.0    /*Part*/
            descr_map( 3) =  104.04 : descr_map( 4) =    30.0    /*Cls */
            descr_map( 5) =  -26.32 : descr_map( 6) =    38.0    /*Desc*/
            descr_map( 7) =   26.08 : descr_map( 8) =    71.1044 /*ROP */
            descr_map( 9) =   34.08 : descr_map(10) =    84.1044 /*EOQ */
            descr_map(11) =   42.08 : descr_map(12) =    97.1044 /*Use */
            descr_map(13) =   50.08 : descr_map(14) =   110.1074 /*Dev */
            descr_map(15) = -318.08 : descr_map(16) =   123.1044 /* SS */

        /* 2nd Detail Line */
            descr_map(17) =   66.08 : descr_map(18) =  1071.1044 /*ROP */
            descr_map(19) =   74.08 : descr_map(20) =  1084.1044 /*EOQ */
            descr_map(21) =   58.08 : descr_map(22) =  1097.1044 /*Use */
            descr_map(23) =   90.08 : descr_map(24) =  1110.1074 /*Dev */
            descr_map(25) =   82.08 : descr_map(26) =  1123.1044 /* SS */


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
            incl(1) = 0  :  incl$(1) = " "

            hdr$(1) = "                                              " & ~
                      "                               ROP          EO" & ~
                      "Q    Std Usage      Std Dev     Sfty Stk"

                     /*   xxxx    xxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxx*/
                     /*xxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxx   xxxxxxxxx*/
                     /*x   xxxxxxxxxx   xxxxxxxxxx   xxxxxxxxxx*/

            hdr$(4) = "ROP Class  Part Number               Descripti" & ~
                      "on                         Old ROP      Old EO" & ~
                      "Q    Old Usage      Old Dev      Old ESS"

                     /*                                              */
                     /*                        xxxxxxxxxx   xxxxxxxxx*/
                     /*x   xxxxxxxxxx   xxxxxxxxxx   xxxxxxxxxx*/

        /* 1st Detail Line */
            descr_map( 1) =  104.04 : descr_map( 2) =     4.0    /*Cls */
            descr_map( 3) =    1.25 : descr_map( 4) =    12.0    /*Part*/
            descr_map( 5) =  -26.32 : descr_map( 6) =    38.0    /*Desc*/
            descr_map( 7) =   26.08 : descr_map( 8) =    71.1044 /*ROP */
            descr_map( 9) =   34.08 : descr_map(10) =    84.1044 /*EOQ */
            descr_map(11) =   42.08 : descr_map(12) =    97.1044 /*Use */
            descr_map(13) =   50.08 : descr_map(14) =   110.1074 /*Dev */
            descr_map(15) = -318.08 : descr_map(16) =   123.1044 /* SS */

        /* 2nd Detail Line */
            descr_map(17) =   66.08 : descr_map(18) =  1071.1044 /*ROP */
            descr_map(19) =   74.08 : descr_map(20) =  1084.1044 /*EOQ */
            descr_map(21) =   58.08 : descr_map(22) =  1097.1044 /*Use */
            descr_map(23) =   90.08 : descr_map(24) =  1110.1074 /*Dev */
            descr_map(25) =   82.08 : descr_map(26) =  1123.1044 /* SS */

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
              str(line2$,62%) = "ROPHNYIN: " & str(cms2v$,,8%)
              if fieldnr% > 0% then L40120
                 init(hex(86)) lfac$()
                 lfac$(1) = hex(8c)
                 goto L40160
L40120:       init(hex(8c)) lfac$()
              pf9$ = " "
              str(pfkeys$,8,1) = hex(ff)
              if fieldnr% <> 1 then L40160
                 pf9$ = "(9)See ROP Parts"
                 str(pfkeys$,8,1) = hex(09)
L40160:       on fieldnr% gosub L40250,         /* Part Number       */   ~
                                L40250,         /* Part Class        */   ~
                                L40260,         /* Safety Stock      */   ~
                                L40260,         /* Re-order Point    */   ~
                                L40260,         /* Economic Order Qty*/   ~
                                L40260          /* Standard Deviatio */
              goto L40280

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40250:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40260:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40280:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Part Re-order Point Parameters",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Part Number",                                ~
               at (06,23), fac(lfac$( 1)), part$                , ch(25),~
               at (06,49), fac(hex(8c)),   partdescr$           , ch(32),~
                                                                         ~
               at (07,02), "Part ROP Class",                             ~
               at (07,23), fac(lfac$( 2)), pclass$              , ch(04),~
               at (07,49), fac(hex(8c)),   clsdescr$            , ch(32),~
                                                                         ~
               at (08,02), "Safety Stock",                               ~
               at (08,23), fac(lfac$( 3)), ess$                 , ch(10),~
                                                                         ~
               at (09,02), "Re-order Point",                             ~
               at (09,23), fac(lfac$( 4)), rop$                 , ch(10),~
                                                                         ~
               at (10,02), "Economic Order Qty",                         ~
               at (10,23), fac(lfac$( 5)), eoq$                 , ch(10),~
                                                                         ~
               at (11,02), "Average Usage",                              ~
               at (11,23), fac(lfac$( 5)), usage$               , ch(10),~
                                                                         ~
               at (12,02), "Standard Deviation",                         ~
               at (12,23), fac(lfac$( 6)), stddev$              , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,18), fac(hex(8c)), pf9$,                           ~
               at (22,37), "(10)Print Report by Part",                   ~
               at (22,65), "(13)Instructions",                           ~
               at (23,18), fac(hex(8c)), pf4$,                           ~
               at (23,37), "(11)Print Report by Class",                  ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,18), fac(hex(8c)), pf12$                  , ch(17),~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys (pfkeys$),                                           ~
               key (keyhit%)

               if keyhit% <> 13 then L40700
                  call "MANUAL" ("ROPHNYIN")
                  goto L40280

L40700:        if keyhit% <> 15 then L40740
                  call "PRNTSCRN"
                  goto L40280

L40740:           close ws
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
                              L50350,         /* Part Class        */     ~
                              L50450,         /* Safety Stock      */     ~
                              L50490,         /* Re-order Point    */     ~
                              L50530,         /* Economic Order Qty*/     ~
                              L50610          /* Standard Deviatio */
            return

L50170: REM Check Part_Number
            plowkey$ = str(part$)
            partdescr$ = hex(06) & "Select valid Part Number"
            call "PLOWCODE" (#2, plowkey$, partdescr$, 0%, 0.30, f1%(2))
            if f1%(2) = 1 then L50240
               errormsg$ = "Part Number not on Inventory Master File"
               return
L50240:     part$ = str(plowkey$,1,25)
            call "READ100" (#2, str(part$), f1%(2))
            get #2, using L31144, ess
            call "CONVERT" (ess, -2.4, ess$)

        REM Check ROPHNY File
            readkey$ = str(part$)
            call "READ100" (#1, readkey$, f1%(1))
            if f1%(1) = 1 then dataload   else  return

L50350: REM Test for Part Class
*       ** PLOWKEY$ = PCLASS$
            clsdescr$ = hex(06) & "Select Part Class"
            call "GETCODE" (#4%, pclass$, clsdescr$, 0%, 0.3, f1%(4))
            if f1%(4) = 1 then L50395
               errormsg$ = "Part Class not defined in ROPCLASS file."
               return
L50395:     get #4 using L50400, crflag$
L50400:         FMT POS(35), CH(1)
            if crflag$ <> "Y" then return
                if editmode = 0 then return
                    gosub check_devratio
                    return

L50450: REM Test for Economic Safety Stock        ESS$
            call "NUMTEST" (ess$, 0, 9e7, errormsg$, 2.4, ess)
            return

L50490: REM Test for Re-order Point               ROP$
            call "NUMTEST" (rop$, 0, 9e7, errormsg$, 2.4, rop)
            if svrop$ = rop$ then return
               svrop$ = rop$
               ropdate$ = str(date)
               return

L50530: REM Test for Economic Order Quantity      EOQ$
            call "NUMTEST" (eoq$, 0, 9e7, errormsg$, 2.4, eoq)

        REM Test for Average Usage                USAGE$
            call "NUMTEST" (usage$, 0.01, 9e7, errormsg$, 2.4, usage)
            if eoq <= 3 * usage then return
               errormsg$ = "EOQ must not exceed 3 months usage."
            return

L50610: REM Test for Standard Deviation           STDDEV$
            call "NUMTEST" (stddev$,0.01,9e7,errormsg$,2.4,stddev)
            gosub check_devratio
            return

        check_devratio
            temp = stddev / usage
            if temp <= devratio then return
               call "CONVERT" (temp, 2.2, temp$)
               call "CONVERT" (devratio, -2.2, devratio$)
               errormsg$ = "WARNING - Deviation Ratio Exceeds System Limi~
        ~t: " & temp$ & ":" & devratio$
               skipfield = 1
               return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
