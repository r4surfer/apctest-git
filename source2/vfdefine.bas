        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  V   V  FFFFF  DDDD   EEEEE  FFFFF  IIIII  N   N  EEEEE   *~
            *  V   V  F      D   D  E      F        I    NN  N  E       *~
            *  V   V  FFFF   D   D  EEEE   FFFF     I    N N N  EEEE    *~
            *   V V   F      D   D  E      F        I    N  NN  E       *~
            *    V    F      DDDD   EEEEE  F      IIIII  N   N  EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VFDEFINE - Defines characteristics of Variable Fields.    *~
            *----------------------------------------------------------Q*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/22/86 ! ORIGINAL                                 ! ERN *~
            * 02/06/87 ! Added HNYQUAN                            ! JRH *~
            * 04/07/87 ! Added HNYMASTR                           ! ERN *~
            * 08/26/87 ! GENCODES validation capability added.    ! ERN *~
            * 04/21/88 ! Added STORNAME                           ! TLJ *~
            * 03/09/90 ! Added LOCATION file.                     ! MLJ *~
            * 01/30/92 ! Added HNYCCGRP file. (Cycle Counting)    ! SID *~
            * 02/14/92 ! Added HNYCCMST file. (Cycle Counting)    ! RJH *~
            * 04/28/92 ! PRR 12421. User defined # of decimals.   ! JDH *~
            * 10/14/92 ! Added PERMASTR file for SFC/PRL Divorce. ! RJH *~
            * 09/16/93 ! Added MLQMASTR file for MLQuote project. ! JIM *~
            * 01/28/94 ! Added ECRMASTR file for ECT     project. ! LDJ *~
            * 06/10/94 ! Added FORMULA table for FORMCALC project.! LDJ *~
            *          ! Change default field values for new files!     *~
            *          ! from "AU" to "NU".                       !     *~
            * 06/15/94 ! Added HNYACTXF for the OMEGA project     ! ERN *~
            * 06/16/94 ! Added VPCMASTR for the OMEGA project     ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            ffac$(1)1,                   /* File Name Fac              */~
            file$8,                      /* File Name                  */~
            filedescr$32,                /* File Name                  */~
            gencodes$(10)9,              /* GENCODES file names        */~
            gendescr$(10)32,             /*          file descriptions */~
            genkey$50,                   /*          key for reads     */~
            getmsg$30,                   /* Message for GETCODE        */~
            hdr$(5)40, hdr2$(5)40,       /* Column Headings            */~
            hex00$9,                     /* 9 HEX(00)s                 */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            key$20,                      /* SYSFILE2 record key        */~
            line2$79,                    /* Second screen line         */~
            msg$(10)40,                  /* Info Message for VF        */~
            pf16$16, pf8$34,             /* PF Key Prompts             */~
            prompt$(10)20,               /* VF Prompt                  */~
            reqd$(10)1,                  /* Required / Optional        */~
            size%(10),                   /* Field Size (1-20)          */~
            type$(10)2,                  /* Field Type                 */~
            vfac$(10,5)1                 /* Variable Field Facs        */

        dim f2%(04),                     /* = 0 if the file is open    */~
            f1%(04)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus Management System General Informa *~
            * #2  ! GENCODES ! General Codes File                       *~
            * #3  ! WORKAREA ! Work File                                *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "SYSFILE2",                                      ~
                        varc, indexed, recsize = 500,                    ~
                        keypos =    1, keylen =  20

            select #2,  "GENCODES",                                      ~
                        varc, indexed, recsize = 128,                    ~
                        keypos =    1, keylen =  24

            select #3,  "WORKAREA",                                      ~
                        varc, indexed, recsize = 30,                     ~
                        keypos = 1, keylen = 2

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, 0%, f2%(1), 0%, " ")
            call "OPENCHCK" (#2, 0%, f2%(2), 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date : call "DATEFMT" (date$)
            hex00$ = all(hex(00))

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value and Press (RETURN)."

*        Define static screen variables
            str(line2$,62) = "VFDEFINE: " & str(cms2v$,,8)
            hdr$(1) = "Type"
            hdr$(2) = "Size"
            hdr$(3) = "Field Prompt"
            hdr$(4) = "R/O"
            hdr$(5) = "Informational Message"

            hdr2$() = hdr$()
            hdr2$(4) = "File Name"
            hdr2$(5) = "GENCODES File Description    MCL"

*        Write Workfile definitions of Valid Type Codes
            call "WORKOPEN" (#3, "IO   ", 26%, 1%)
            write #3, using L08520, "AU", "Alpha/Numeric Upper Case"
            write #3, using L08520, "AL", "Alpha/Numeric Upper or Lower"
            write #3, using L08520, "N+", "Positive Numeric, 2 decimals"
            write #3, using L08520, "N-", "Any Numeric, 2 decimals"
            write #3, using L08520, "D ", "Date"
            write #3, using L08520, "NU", "Not Used"
            write #3, using L08520, "0+", "Positive Numeric, 0 decimals"
            write #3, using L08520, "1+", "Positive Numeric, 1 decimals"
            write #3, using L08520, "2+", "Positive Numeric, 2 decimals"
            write #3, using L08520, "3+", "Positive Numeric, 3 decimals"
            write #3, using L08520, "4+", "Positive Numeric, 4 decimals"
            write #3, using L08520, "5+", "Positive Numeric, 5 decimals"
            write #3, using L08520, "6+", "Positive Numeric, 6 decimals"
            write #3, using L08520, "7+", "Positive Numeric, 7 decimals"
            write #3, using L08520, "8+", "Positive Numeric, 8 decimals"
            write #3, using L08520, "9+", "Positive Numeric, 9 decimals"
            write #3, using L08520, "0-", "Any Numeric, 0 decimals"
            write #3, using L08520, "1-", "Any Numeric, 1 decimals"
            write #3, using L08520, "2-", "Any Numeric, 2 decimals"
            write #3, using L08520, "3-", "Any Numeric, 3 decimals"
            write #3, using L08520, "4-", "Any Numeric, 4 decimals"
            write #3, using L08520, "5-", "Any Numeric, 5 decimals"
            write #3, using L08520, "6-", "Any Numeric, 6 decimals"
            write #3, using L08520, "7-", "Any Numeric, 7 decimals"
            write #3, using L08520, "8-", "Any Numeric, 8 decimals"
            write #3, using L08520, "9-", "Any Numeric, 9 decimals"
L08520:         FMT CH(2), CH(28)

*        Write out files and defaults to SYSFILE2.
        for f% = 1% to 10%
            type$(f%)   = "NU"
            size%(f%)   = 20%
            reqd$(f%)   = "O"
        next f%

        for f% = 1% to 18%
            on f% goto customer,         /* Customer Master            */~
                       bckmastr,         /* Sales Orders               */~
                       arimastr,         /* Invoice Master File        */~
                       slmmastr,         /* Salesman Master File       */~
                       vbkmastr,         /* PO Master File             */~
                       estmastr,         /* Estimate Master File       */~
                       hnyccgrp,         /* Cycle Count Group File     */~
                       hnyccmst,         /* Cycle Count Part Master Fil*/~
                       hnyquan,          /* Inventory Quantities file  */~
                       hnymastr,         /* Inventory Master File      */~
                       storname,         /* Store Master File          */~
                       location,         /* Location Master File       */~
                       permastr,         /* Employee Master File (SFC) */~
                       mlqmastr,         /* Multi-Line Quote Master fl */~
                       ecrmastr,         /* Eng Chg Requests Master fl */~
                       formula,          /* Attached to HNYMASTR file  */~
                       hnyactxf,         /* Part, Activity X-ref       */~
                       vpcmastr          /* Purchasing Contracts Mastr */

            customer : file$ = "CUSTOMER" : fields% = 10%
                       filedescr$ = "Customer Master File         "
                       goto L09900
            bckmastr : file$ = "BCKMASTR" : fields% = 10%
                       filedescr$ = "Sales Order Master           "
                       goto L09900
            arimastr : file$ = "ARIMASTR" : fields% = 10%
                       filedescr$ = "Invoice Master File          "
                       goto L09900
            slmmastr : file$ = "SLMMASTR" : fields% = 10%
                       filedescr$ = "Salesman Master File         "
                       goto L09900
            vbkmastr : file$ = "VBKMASTR" : fields% = 10%
                       filedescr$ = "Purchase Order Master        "
                       goto L09900
            estmastr : file$ = "ESTMASTR" : fields% = 10%
                       filedescr$ = "Estimate Master              "
                       goto L09900
            hnyccgrp : file$ = "HNYCCGRP" : fields% = 10%
                       filedescr$ = "Cycle Count Group File       "
                       goto L09900
            hnyccmst : file$ = "HNYCCMST" : fields% = 10%
                       filedescr$ = "Cycle Count Part Master File "
                       goto L09900
            hnyquan  : file$ = "HNYQUAN " : fields% = 10%
                       filedescr$ = "Inventory Cost & Quantities  "
                       goto L09900
            hnymastr : file$ = "HNYMASTR" : fields% = 10%
                       filedescr$ = "Inventory Master File        "
                       goto L09900
            storname : file$ = "STORNAME" : fields% = 10%
                       filedescr$ = "Store Master File            "
                       goto L09900
            location:  file$ = "LOCATION" : fields% = 10%
                       filedescr$ = "Location Master File         "
                       goto L09900
            permastr:  file$ = "PERMASTR" : fields% = 10%
                       filedescr$ = "Employee Master File (SFC)   "
                       goto L09900
            mlqmastr:  file$ = "MLQMASTR" : fields% = 10%
                       filedescr$ = "Multi-Line Quote Master File "
                       goto L09900
            ecrmastr:  file$ = "ECRMASTR" : fields% = 10%
                       filedescr$ = "Eng Change Requests Mstr File"
                       goto L09900
            formula :  file$ = "FORMULA " : fields% = 10%
                       filedescr$ = "Formula Fields for HNYMASTR  "
                       goto L09900
            hnyactxf:  file$ = "HNYACTXF" : fields% = 10%
                       filedescr$ = "Part, Activity X-ref File    "
                       goto L09900
            vpcmastr:  file$ = "VPCMASTR" : fields% = 10%
                       filedescr$ = "Purchasing Contracts File    "
                       goto L09900
L09900
*        Write data to file (only if not there)
            newonly% = 1%
            gosub save_data
        next f%
            newonly% = 0%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * Get file name to edit.                                    *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, key$, type$(), prompt$(),  ~
                      reqd$(), msg$(), file$, filedescr$, gencodes$(),   ~
                      gendescr$(), pf8$
            mat size% = zer

            for fieldnr% = 1% to 1%
                gosub'051(fieldnr%)
                      if enabled% = 0% then L10200
L10140:         gosub'101(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% then       L65000
                      if keyhit% <>  0% then       L10140
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10140
L10200:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles editing of variable field definitions.            *~
            *************************************************************
        main_screen
L11060:     gosub'111(0%, 0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  8 then       gencodes_screen
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11060
L11110:     fieldnr% = cursor%(1) - 8%
            if fieldnr% < 1% or fieldnr% > fields% then L11060

L11140:     gosub'111(fieldnr%, err%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11140
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L11140
                     if fieldnr% <> cursor%(1) - 8% then L11110
            goto L11060

        gencodes_screen
L11230:     gosub'112(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  8 then       main_screen
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11230
L11280:     fieldnr% = cursor%(1) - 8%
            if fieldnr% < 1% or fieldnr% > fields% then L11230
            if type$(fieldnr%) = "NU" then L11230
L11310:     gosub'112(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11310
            gosub'153(fieldnr%)
                  if errormsg$ <> " " then L11310
                     if fieldnr% <> cursor%(1) - 8% then L11280
            goto L11230

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub save_data
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * --------------------------------------------------------- *~
            * Sets DEFAULTS and ENABLES fields for the Page 1 of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100          /* File Name        */
                     return

L20100
*        Default/Enable for FILE NAME
            inpmessage$ = "Enter File Name containing Variable Fields."
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            * --------------------------------------------------------- *~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
L29918:     keyhit1% = 2%        /* Put Msg Area at bottom of screen  */
            call "STARTOVR" (keyhit1%)
            if keyhit1%  = 1% then return
            if keyhit1% <> 0% then L29918
                return clear all
                goto inputmode

        REM *************************************************************~
            *                 L O A D   D A T A                         *~
            * --------------------------------------------------------- *~
            * Load up data.  Record '1' is already in buffer.           *~
            *************************************************************
        load_data
            get #1 using L30080, file$, filedescr$, fields%, type$(),     ~
                                size%(), prompt$(), reqd$(), gencodes$()
L30080:         FMT XX(4), CH(8), POS(21), CH(30), BI(1), 10*CH(2),      ~
                    10*BI(1), 10*CH(20), 10*CH(1), 10*CH(9)
            str(key$,3,1) = "2"
            call "READ100" (#1, key$, f1%(1))
            if f1%(1) = 0% then L30240
                get #1 using L30140, msg$()
L30140:              FMT XX(20), 10*CH(40)

            for f% = 1% to fields%
                if gencodes$(f%) = " " then L30250
                     genkey$ = hex00$ & gencodes$(f%)
                     call "READ100" (#2, genkey$, f1%(2))
                     if f1%(2) = 0% then                                 ~
                          gendescr$(f%) = "** Not Defined **"            ~
                                    else                                 ~
                          get #2 using L30240, gendescr$(f%)
L30240:                        FMT POS(25), CH(32)
L30250:     next f%

        return
        REM *************************************************************~
            *               S A V E   D A T A                           *~
            * --------------------------------------------------------- *~
            * Write data to file.  If NEW% = 1% then record is only     *~
            * written if it does not exist.                             *~
            *************************************************************
        save_data
            key$ = "VF1:" & str(file$)
            if newonly% = 1% then L31110
                call "DELETE" (#1, key$, 11%)

L31110:     write #1 using L31140, key$, filedescr$, fields%, type$(),    ~
                                  size%(), prompt$(), reqd$(),           ~
                                  gencodes$(), " ",      eod goto L31200
L31140:         FMT CH(20), CH(30), BI(01), 10*CH(2), 10*BI(1),          ~
                    10*CH(20), 10*CH(1), 10*CH(9), CH(119)
            str(key$,3,1) = "2"
            call "DELETE" (#1, key$, 12%)
            write #1 using L31190, key$, msg$(), " "
L31190:         FMT CH(20), 10*CH(40), CH(80)
L31200: return



        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            * --------------------------------------------------------- *~
            * Inputs Document for the first time.                       *~
            *************************************************************

            deffn'101(fieldnr%)          /* Input File Name            */
                init(hex(81)) ffac$()
                init(hex(9c)) vfac$()
                pf16$ = "(16)Exit Program"
                mode% = 1%               /* Input Mode                 */
                goto L40420

            deffn'111(fieldnr%, err%)    /* Edit VF Characteristics    */
                init(hex(84)) ffac$()
                init(hex(9c)) vfac$()    /* Invisible                  */
                mode% = 0%               /* Edit  Mode                 */
                if fieldnr% = 0% then pf16$ = "(16)Save Data" else       ~
                                      pf16$ = " "
                if fieldnr% = 0% then                                    ~
                       pf8$ = "(8)Edit GENCODES Validation Files"  else  ~
                       pf8$ = " "
                if fieldnr% = 0% then inpmessage$ = edtmessage$ else     ~
                     inpmessage$ = "TYPEs: AU, AL, N+, N-, D, & NU; "  & ~
                                   "R/O: 'R'equired, 'O'ptional, 'S'top."
                for f% = 1% to fields%
                  for f1% = 1% to 5%
                     vfac$(f%, f1%) = hex(84)      /* FIELDNR% = 0     */
*              First get rid of FIELDNR% = 0
                     if fieldnr% = 0% and f1% = 1% then                  ~
                                                 vfac$(f%, f1%) = hex(86)
                     if fieldnr% = 0% then L40390
*              Now set dim and protect as the default if not selected
                     if f% <> fieldnr% then vfac$(f%, f1%) = hex(8c)
                     if f% <> fieldnr% then L40390
*              On the selected field, set for modification
                     if err% = 0% or err% = 1% then vfac$(f%,1%)=hex(81)
                     if err% = 0% or err% = 2% then vfac$(f%,2%)=hex(82)
                     if err% = 0% or err% = 3% then vfac$(f%,3%)=hex(80)
                     if err% = 0% or err% = 4% then vfac$(f%,4%)=hex(81)
                     if err% = 0% or err% = 5% then vfac$(f%,5%)=hex(80)
L40390:         next f1%
                next f%

L40420:   accept                                                         ~
            at (01,02), "Variable Fields Definition",                    ~
            at (01,66), "Today:",                                        ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (06,02), "File Name",                                     ~
            at (06,13), fac(ffac$( 1)), file$                   , ch(08),~
            at (06,24), fac(hex(8c)),   filedescr$              , ch(32),~
                                                                         ~
            at (08,05), fac(hex(ac)), hdr$(1)                   , ch(04),~
            at (08,10), fac(hex(ac)), hdr$(2)                   , ch(04),~
            at (08,15), fac(hex(ac)), hdr$(3)                   , ch(20),~
            at (08,36), fac(hex(ac)), hdr$(4)                   , ch(03),~
            at (08,40), fac(hex(ac)), hdr$(5)                   , ch(40),~
                                                                         ~
            at (09,02), " 1",                                            ~
            at (09,06), fac(vfac$(1%, 1%)), type$  ( 1)         , ch(02),~
            at (09,11), fac(vfac$(1%, 2%)), size%  ( 1)         ,pic(#0),~
            at (09,15), fac(vfac$(1%, 3%)), prompt$( 1)         , ch(20),~
            at (09,37), fac(vfac$(1%, 4%)), reqd$  ( 1)         , ch(01),~
            at (09,40), fac(vfac$(1%, 5%)), msg$   ( 1)         , ch(40),~
                                                                         ~
            at (10,02), " 2",                                            ~
            at (10,06), fac(vfac$(2%, 1%)), type$  ( 2)         , ch(02),~
            at (10,11), fac(vfac$(2%, 2%)), size%  ( 2)         ,pic(#0),~
            at (10,15), fac(vfac$(2%, 3%)), prompt$( 2)         , ch(20),~
            at (10,37), fac(vfac$(2%, 4%)), reqd$  ( 2)         , ch(01),~
            at (10,40), fac(vfac$(2%, 5%)), msg$   ( 2)         , ch(40),~
                                                                         ~
            at (11,02), " 3",                                            ~
            at (11,06), fac(vfac$(3%, 1%)), type$  ( 3)         , ch(02),~
            at (11,11), fac(vfac$(3%, 2%)), size%  ( 3)         ,pic(#0),~
            at (11,15), fac(vfac$(3%, 3%)), prompt$( 3)         , ch(20),~
            at (11,37), fac(vfac$(3%, 4%)), reqd$  ( 3)         , ch(01),~
            at (11,40), fac(vfac$(3%, 5%)), msg$   ( 3)         , ch(40),~
                                                                         ~
            at (12,02), " 4",                                            ~
            at (12,06), fac(vfac$(4%, 1%)), type$  ( 4)         , ch(02),~
            at (12,11), fac(vfac$(4%, 2%)), size%  ( 4)         ,pic(#0),~
            at (12,15), fac(vfac$(4%, 3%)), prompt$( 4)         , ch(20),~
            at (12,37), fac(vfac$(4%, 4%)), reqd$  ( 4)         , ch(01),~
            at (12,40), fac(vfac$(4%, 5%)), msg$   ( 4)         , ch(40),~
                                                                         ~
            at (13,02), " 5",                                            ~
            at (13,06), fac(vfac$(5%, 1%)), type$  ( 5)         , ch(02),~
            at (13,11), fac(vfac$(5%, 2%)), size%  ( 5)         ,pic(#0),~
            at (13,15), fac(vfac$(5%, 3%)), prompt$( 5)         , ch(20),~
            at (13,37), fac(vfac$(5%, 4%)), reqd$  ( 5)         , ch(01),~
            at (13,40), fac(vfac$(5%, 5%)), msg$   ( 5)         , ch(40),~
                                                                         ~
            at (14,02), " 6",                                            ~
            at (14,06), fac(vfac$(6%, 1%)), type$  ( 6)         , ch(02),~
            at (14,11), fac(vfac$(6%, 2%)), size%  ( 6)         ,pic(#0),~
            at (14,15), fac(vfac$(6%, 3%)), prompt$( 6)         , ch(20),~
            at (14,37), fac(vfac$(6%, 4%)), reqd$  ( 6)         , ch(01),~
            at (14,40), fac(vfac$(6%, 5%)), msg$   ( 6)         , ch(40),~
                                                                         ~
            at (15,02), " 7",                                            ~
            at (15,06), fac(vfac$(7%, 1%)), type$  ( 7)         , ch(02),~
            at (15,11), fac(vfac$(7%, 2%)), size%  ( 7)         ,pic(#0),~
            at (15,15), fac(vfac$(7%, 3%)), prompt$( 7)         , ch(20),~
            at (15,37), fac(vfac$(7%, 4%)), reqd$  ( 7)         , ch(01),~
            at (15,40), fac(vfac$(7%, 5%)), msg$   ( 7)         , ch(40),~
                                                                         ~
            at (16,02), " 8",                                            ~
            at (16,06), fac(vfac$(8%, 1%)), type$  ( 8)         , ch(02),~
            at (16,11), fac(vfac$(8%, 2%)), size%  ( 8)         ,pic(#0),~
            at (16,15), fac(vfac$(8%, 3%)), prompt$( 8)         , ch(20),~
            at (16,37), fac(vfac$(8%, 4%)), reqd$  ( 8)         , ch(01),~
            at (16,40), fac(vfac$(8%, 5%)), msg$   ( 8)         , ch(40),~
                                                                         ~
            at (17,02), " 9",                                            ~
            at (17,06), fac(vfac$(9%, 1%)), type$  ( 9)         , ch(02),~
            at (17,11), fac(vfac$(9%, 2%)), size%  ( 9)         ,pic(#0),~
            at (17,15), fac(vfac$(9%, 3%)), prompt$( 9)         , ch(20),~
            at (17,37), fac(vfac$(9%, 4%)), reqd$  ( 9)         , ch(01),~
            at (17,40), fac(vfac$(9%, 5%)), msg$   ( 9)         , ch(40),~
                                                                         ~
            at (18,02), "10",                                            ~
            at (18,06), fac(vfac$(10%, 1%)), type$  (10)        , ch(02),~
            at (18,11), fac(vfac$(10%, 2%)), size%  (10)        ,pic(#0),~
            at (18,15), fac(vfac$(10%, 3%)), prompt$(10)        , ch(20),~
            at (18,37), fac(vfac$(10%, 4%)), reqd$  (10)        , ch(01),~
            at (18,40), fac(vfac$(10%, 5%)), msg$   (10)        , ch(40),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), "(1)Start Over",                                 ~
            at (22,65), "(13)Instructions",                              ~
            at (23,20), fac(hex(8c)), pf8$,                              ~
            at (23,65), "(15)Print Screen",                              ~
            at (24,65), fac(hex(8c)), pf16$,                             ~
                keys(hex(0001080d0f10)),  key (keyhit%)

                if keyhit% <> 13 then L41410
                     call "MANUAL" ("VFDEFINE")
                     goto L40420

L41410:         if keyhit% <> 15 then L41450
                     call "PRNTSCRN"
                     goto L40420

L41450:         if mode% = 1% then return  /* If INPUT mode don't waste*/
                close ws
                call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            * --------------------------------------------------------- *~
            * Inputs Document for the first time.                       *~
            *************************************************************

            deffn'112(fieldnr%)
                init(hex(84)) ffac$()
                init(hex(9c)) vfac$()    /* Invisible                  */
                if fieldnr% = 0% then pf16$ = "(16)Save Data" else       ~
                                      pf16$ = " "
                if fieldnr% = 0% then pf8$ = "(8)Return to Main Screen"  ~
                                 else pf8$ = " "
                if fieldnr% = 0% then inpmessage$ = edtmessage$ else     ~
                     inpmessage$ = "Enter GENCODES File Name -or- '?'" & ~
                                   " to list file names."
                for f% = 1% to fields%
                  for f1% = 1% to 5%
                     vfac$(f%, f1%) = hex(8c)      /* FIELDNR% = 0     */
                     if fieldnr% = 0% then vfac$(f%, 4%) = hex(86)
                     if f% = fieldnr% then vfac$(f%, 4%) = hex(81)
                  next f1%
                next f%

L42420:   accept                                                         ~
            at (01,02), "Variable Fields Definition",                    ~
            at (01,66), "Today:",                                        ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (06,02), "File Name",                                     ~
            at (06,13), fac(ffac$( 1)), file$                   , ch(08),~
            at (06,24), fac(hex(8c)),   filedescr$              , ch(32),~
                                                                         ~
            at (08,05), fac(hex(ac)), hdr2$(1)                  , ch(04),~
            at (08,10), fac(hex(ac)), hdr2$(2)                  , ch(04),~
            at (08,15), fac(hex(ac)), hdr2$(3)                  , ch(20),~
            at (08,36), fac(hex(ac)), hdr2$(4)                  , ch(09),~
            at (08,46), fac(hex(ac)), hdr2$(5)                  , ch(32),~
                                                                         ~
            at (09,02), " 1",                                            ~
            at (09,06), fac(vfac$(1%, 1%)), type$    ( 1)       , ch(02),~
            at (09,11), fac(vfac$(1%, 2%)), size%    ( 1)       ,pic(#0),~
            at (09,15), fac(vfac$(1%, 3%)), prompt$  ( 1)       , ch(20),~
            at (09,36), fac(vfac$(1%, 4%)), gencodes$( 1)       , ch(09),~
            at (09,46), fac(vfac$(1%, 5%)), gendescr$( 1)       , ch(32),~
                                                                         ~
            at (10,02), " 2",                                            ~
            at (10,06), fac(vfac$(2%, 1%)), type$    ( 2)       , ch(02),~
            at (10,11), fac(vfac$(2%, 2%)), size%    ( 2)       ,pic(#0),~
            at (10,15), fac(vfac$(2%, 3%)), prompt$  ( 2)       , ch(20),~
            at (10,36), fac(vfac$(2%, 4%)), gencodes$( 2)       , ch(09),~
            at (10,46), fac(vfac$(2%, 5%)), gendescr$( 2)       , ch(32),~
                                                                         ~
            at (11,02), " 3",                                            ~
            at (11,06), fac(vfac$(3%, 1%)), type$    ( 3)       , ch(02),~
            at (11,11), fac(vfac$(3%, 2%)), size%    ( 3)       ,pic(#0),~
            at (11,15), fac(vfac$(3%, 3%)), prompt$  ( 3)       , ch(20),~
            at (11,36), fac(vfac$(3%, 4%)), gencodes$( 3)       , ch(09),~
            at (11,46), fac(vfac$(3%, 5%)), gendescr$( 3)       , ch(32),~
                                                                         ~
            at (12,02), " 4",                                            ~
            at (12,06), fac(vfac$(4%, 1%)), type$    ( 4)       , ch(02),~
            at (12,11), fac(vfac$(4%, 2%)), size%    ( 4)       ,pic(#0),~
            at (12,15), fac(vfac$(4%, 3%)), prompt$  ( 4)       , ch(20),~
            at (12,36), fac(vfac$(4%, 4%)), gencodes$( 4)       , ch(09),~
            at (12,46), fac(vfac$(4%, 5%)), gendescr$( 4)       , ch(32),~
                                                                         ~
            at (13,02), " 5",                                            ~
            at (13,06), fac(vfac$(5%, 1%)), type$    ( 5)       , ch(02),~
            at (13,11), fac(vfac$(5%, 2%)), size%    ( 5)       ,pic(#0),~
            at (13,15), fac(vfac$(5%, 3%)), prompt$  ( 5)       , ch(20),~
            at (13,36), fac(vfac$(5%, 4%)), gencodes$( 5)       , ch(09),~
            at (13,46), fac(vfac$(5%, 5%)), gendescr$( 5)       , ch(32),~
                                                                         ~
            at (14,02), " 6",                                            ~
            at (14,06), fac(vfac$(6%, 1%)), type$    ( 6)       , ch(02),~
            at (14,11), fac(vfac$(6%, 2%)), size%    ( 6)       ,pic(#0),~
            at (14,15), fac(vfac$(6%, 3%)), prompt$  ( 6)       , ch(20),~
            at (14,36), fac(vfac$(6%, 4%)), gencodes$( 6)       , ch(09),~
            at (14,46), fac(vfac$(6%, 5%)), gendescr$( 6)       , ch(32),~
                                                                         ~
            at (15,02), " 7",                                            ~
            at (15,06), fac(vfac$(7%, 1%)), type$    ( 7)       , ch(02),~
            at (15,11), fac(vfac$(7%, 2%)), size%    ( 7)       ,pic(#0),~
            at (15,15), fac(vfac$(7%, 3%)), prompt$  ( 7)       , ch(20),~
            at (15,36), fac(vfac$(7%, 4%)), gencodes$( 7)       , ch(09),~
            at (15,46), fac(vfac$(7%, 5%)), gendescr$( 7)       , ch(32),~
                                                                         ~
            at (16,02), " 8",                                            ~
            at (16,06), fac(vfac$(8%, 1%)), type$    ( 8)       , ch(02),~
            at (16,11), fac(vfac$(8%, 2%)), size%    ( 8)       ,pic(#0),~
            at (16,15), fac(vfac$(8%, 3%)), prompt$  ( 8)       , ch(20),~
            at (16,36), fac(vfac$(8%, 4%)), gencodes$( 8)       , ch(09),~
            at (16,46), fac(vfac$(8%, 5%)), gendescr$( 8)       , ch(32),~
                                                                         ~
            at (17,02), " 9",                                            ~
            at (17,06), fac(vfac$(9%, 1%)), type$    ( 9)       , ch(02),~
            at (17,11), fac(vfac$(9%, 2%)), size%    ( 9)       ,pic(#0),~
            at (17,15), fac(vfac$(9%, 3%)), prompt$  ( 9)       , ch(20),~
            at (17,36), fac(vfac$(9%, 4%)), gencodes$( 9)       , ch(09),~
            at (17,46), fac(vfac$(9%, 5%)), gendescr$( 9)       , ch(32),~
                                                                         ~
            at (18,02), "10",                                            ~
            at (18,06), fac(vfac$(10%, 1%)), type$    (10)      , ch(02),~
            at (18,11), fac(vfac$(10%, 2%)), size%    (10)      ,pic(#0),~
            at (18,15), fac(vfac$(10%, 3%)), prompt$  (10)      , ch(20),~
            at (18,36), fac(vfac$(10%, 4%)), gencodes$(10)      , ch(09),~
            at (18,46), fac(vfac$(10%, 5%)), gendescr$(10)      , ch(32),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), "(1)Start Over",                                 ~
            at (22,65), "(13)Instructions",                              ~
            at (23,20), fac(hex(8c)), pf8$,                              ~
            at (23,65), "(15)Print Screen",                              ~
            at (24,65), fac(hex(8c)), pf16$,                             ~
                keys(hex(0001080d0f10)),  key (keyhit%)

                if keyhit% <> 13 then L43410
                     call "MANUAL" ("VFDEFINE")
                     goto L42420

L43410:         if keyhit% <> 15 then L43450
                     call "PRNTSCRN"
                     goto L42420

L43450:         close ws
                call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 1.                        *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100          /* File Name        */
                     return

L50100
*        Test Data for FILE NAME
            key$ = "VF1:" & file$
            call "PLOWCODE" (#1, key$, filedescr$, 4%, 0.30, f1%(1))
            if f1%(1) = 1% then L50160
                errormsg$ = "No Variable Fields for File specified."
                return
L50160:     gosub load_data
            return

        REM *************************************************************~
            *            T E S T   F I E L D   D A T A                  *~
            * --------------------------------------------------------- *~
            * Test data for field being entered.                        *~
            *************************************************************~

        deffn'152(fieldnr%)
            f%   = fieldnr%
            err% = 0%
            errormsg$ = " "

*        Test data for TYPE
            getmsg$ = hex(0684) & "Select Field Type Code."
            call "GETCODE" (#3, type$(f%), getmsg$, 0%, .28, f1%(3))
            if f1%(3) = 1% then L51190
                errormsg$ = "Field Type Code NOT valid."
                err%      = 1%
                return

L51190
*        If field type is Not Used then set field and return
            if type$(f%) <> "NU" then L51280
                size%    (f%) = 20%
                prompt$  (f%) = " "
                reqd$    (f%) = "O"
                gencodes$(f%) = " "
                gendescr$(f%) = " "
                return

L51280
*        Test data for SIZE
            if str(type$(f%),,1) < "0" or str(type$(f%),,1) > "9" then   ~
                                                                    L51290
                size%(f%) = 12%
                goto L51380
L51290:     if str(type$(f%),,1) = "N" then size%(f%) = 12%
            if str(type$(f%),,1) = "D" then size%(f%) =  8%
            if str(type$(f%),,1) = "N" or str(type$(f%),,1) = "D"        ~
                                                             then L51380
                if size%(f%) >= 1% and size%(f%) <= 20%      then L51380
                     errormsg$ = "Size must be between 1 and 20."
                     err%      = 2%
                     return

L51380
*        Test data for FIELD PROMPT
            if prompt$(f%) <> " " then L51440
                errormsg$ = "Field Prompt can not be blank."
                err%      = 3%
                return

L51440
*        Test data for REQUIRED/OPTIONAL/STOP
            if reqd$(f%) = "O" or reqd$(f%) = "R" or reqd$(f%) = "S"     ~
                                                             then return
                errormsg$ = "Enter 'R' if required, 'O' if optional," &  ~
                            "S if optional but stop for entry."
                err%      = 4%
                return

        REM *************************************************************~
            *            T E S T   F I E L D   D A T A                  *~
            * --------------------------------------------------------- *~
            * Test data for field being entered.                        *~
            *************************************************************~

        deffn'153(fieldnr%)
            f%   = fieldnr%
            errormsg$ = " "

*        Test data for GENCODES file
            if gencodes$(f%) <> " " then L52150
                gendescr$(f%) = " "
                return
L52150:     if gencodes$(f%) <> "?" then L52220
                genkey$ = hex00$ & hex(00)
                call "PLOWCODE" (#2, genkey$, gendescr$(f%),9%,.32,f1%(2))
                if f1%(2) = 1% then L52270
                     gencodes$(f%), gendescr$(f%) = " "
                     errormsg$ = hex(00)
                     return
L52220:     genkey$ = hex00$ & gencodes$(f%)
            call "READ100" (#2, genkey$, f1%(2))
            if f1%(2) = 1% then L52270
                gendescr$(f%) = "** Not Defined **"
                return
L52270:     get #2 using L52280, gencodes$(f%), gendescr$(f%)
L52280:         FMT XX(9), CH(15), CH(32)
            return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * Closes all the files currently open, and also displays    *~
            * a message (ONLY if in foreground) while linking to the    *~
            * next program.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please")

            end
