        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    CCC   K   K   SSS   TTTTT    A    TTTTT          *~
            *  B   B  C   C  K  K   S        T     A A     T            *~
            *  BBBB   C      KKK     SSS     T    AAAAA    T            *~
            *  B   B  C   C  K  K       S    T   A     A   T            *~
            *  BBBB    CCC   K   K   SSS     T   A     A   T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKSTAT  - A simple driver for SOSTATUS                   *~
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
            * 10/14/87 ! ORIGINAL                                 ! HES *~
            * 12/03/87 ! OPENS FILES THAT DEMSTAT NEEDS AND SENDS !     *~
            *          !            FLAG THAT SAYS THEY ARE OPEN  ! JDH *~
            * 12/10/87 ! Put Screen in for inputs of Cust & SO#   ! JDH *~
            * 12/17/87 ! Put plowcode for SO# into input          ! JDH *~
            * 01/15/88 ! Added History files                      ! JDH *~
            * 04/19/89 ! "Return" now searches current files      ! GGO *~
            * 11/23/92 ! Added PF8 Find by Invoice #.             ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9,                   /* Customer Code              */~
            date$8,                      /* Date for screen display    */~
            dispmsg$13,                  /* Display Message for Screen */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            hd$(3)79,                    /* Screen Header For PLOWCODE */~
            i$(24)80,                    /* Screen Image               */~
            info$79,                     /* Misc Informative text      */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowhdr$(3)79,               /* Plowcode Headers           */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            so$16,                       /* Sales Order Number         */~
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
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
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
            * # 1 ! BCKLINES ! Sales Order Master- Lines                *~
            * # 2 ! SHPLINES ! Shipment Scheduling / Lines              *~
            * # 3 ! SHPHDRS  ! Shipment Scheduling / Pre-Invoicing- Hea *~
            * # 4 ! BCKMASTR ! Sales Order Master - Headers             *~
            * # 5 ! ARIMASTR ! Invoice Master File                      *~
            * # 6 ! ARILINES ! Invoice Line Items File                  *~
            * # 7 ! CUSTOMER ! Customer Master File                     *~
            * # 9 ! BCKHMSTR ! SO History Master File                   *~
            * #10 ! BCKHLNES ! SO History Line Items                    *~
            * #20 ! TXTFILE  ! System Text File                         *~
            * #21 ! PIPCROSS ! hard peg cross reference                 *~
            * #22 ! DEMMASTR ! Demand Master File                       *~
            * #23 ! JBMASTR2 ! Production job master file               *~
            * #24 ! JBSTATUS ! Production job actual structure (RTE) ac *~
            * #25 ! PIPOUT   ! Planned inventory use detail rec         *~
            * #26 ! JBCROSS2 ! Cross reference of RTE & BOM planned for *~
            * #27 ! VBKMASTR ! PURCHASE ORDER HEADER FILE               *~
            * #28 ! VBKLINES ! Purchase Order Line Items File           *~
            * #29 ! JBCREDIT ! Production job credits received detail f *~
            * #30 ! RCVLINES ! Receiver Line Items File  (Purchasing)   *~
            * #31 ! PAYMASTR ! PAYABLES MAIN FILE  (INVOICE DATES)      *~
            * #32 ! PAYLINES ! PAYABLES LINE ITEM FILE                  *~
            * #33 ! HNYMASTR ! Inventory Master File                    *~
            * #34 ! WCMASTR  ! Workcenter Master File                   *~
            * #35 ! RTEMASTR ! Production routing master file           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19                      ~

            select # 2, "SHPLINES",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =   10, keylen =  22                      ~

            select # 3, "SHPHDRS",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  28                      ~

            select # 4, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup     ~

            select # 5, "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17,                     ~
                        alt key  1, keypos =   10, keylen =   8,         ~
                            key  2, keypos =   18, keylen =  16, dup,    ~
                            key  3, keypos =   34, keylen =  16, dup     ~

            select # 6, "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  20                      ~

            select # 7, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup     ~

            select # 9, "BCKHMSTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup     ~

            select #10, "BCKHLNES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19                      ~

            select #20, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11                      ~

            select #21, "PIPCROSS",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  71,                     ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33          ~

            select #22, "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28          ~

            select #23, "JBMASTR2",                                      ~
                        varc,     indexed,  recsize = 1300,              ~
                        keypos =    1, keylen =   8                      ~

            select #24, "JBSTATUS",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  1, keypos =   21, keylen =  44,         ~
                            key  2, keypos =   29, keylen =  36          ~

            select #25, "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select #26, "JBCROSS2",                                      ~
                        varc,     indexed,  recsize =   94,              ~
                        keypos =   29, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  47,         ~
                            key  2, keypos =   48, keylen =  47          ~

            select #27, "VBKMASTR",                                      ~
                        varc,     indexed,  recsize = 1030,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   10, keylen =  16          ~

            select #28, "VBKLINES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28                      ~

            select #29, "JBCREDIT",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  22,                     ~
                        alt key  1, keypos =   23, keylen =  48          ~

            select #30, "RCVLINES",                                      ~
                        varc,     indexed,  recsize =  800,              ~
                        keypos =   26, keylen =  52,                     ~
                        alt key  1, keypos =    1, keylen =  69,         ~
                            key  2, keypos =   42, keylen =  36,         ~
                            key  3, keypos =  128, keylen =  24          ~

            select #31, "PAYMASTR",                                      ~
                        varc,     indexed,  recsize =  350,              ~
                        keypos =    1, keylen =  25                      ~

            select #32, "PAYLINES",                                      ~
                        varc,     indexed,  recsize =  541,              ~
                        keypos =   36, keylen =  28,                     ~
                        alt key  1, keypos =    1, keylen =  63,         ~
                            key  2, keypos =   17, keylen =  47          ~

            select #33, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup     ~

            select #34, "WCMASTR",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6          ~

            select #35, "RTEMASTR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    5, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  35          ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6), 0%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 0%, rslt$( 7))
            call "OPENCHCK" (# 9, fs%( 9), f2%( 9), 0%, rslt$( 9))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
            call "OPENCHCK" (#20, fs%(20), f2%(20), 0%, rslt$(20))
            call "OPENCHCK" (#21, fs%(21), f2%(21), 0%, rslt$(21))
            call "OPENCHCK" (#22, fs%(22), f2%(22), 0%, rslt$(22))
            call "OPENCHCK" (#23, fs%(23), f2%(23), 0%, rslt$(23))
            call "OPENCHCK" (#24, fs%(24), f2%(24), 0%, rslt$(24))
            call "OPENCHCK" (#25, fs%(25), f2%(25), 0%, rslt$(25))
            call "OPENCHCK" (#26, fs%(26), f2%(26), 0%, rslt$(26))
            call "OPENCHCK" (#27, fs%(27), f2%(27), 0%, rslt$(27))
            call "OPENCHCK" (#28, fs%(28), f2%(28), 0%, rslt$(28))
            call "OPENCHCK" (#29, fs%(29), f2%(29), 0%, rslt$(29))
            call "OPENCHCK" (#30, fs%(30), f2%(30), 0%, rslt$(30))
            call "OPENCHCK" (#31, fs%(31), f2%(31), 0%, rslt$(31))
            call "OPENCHCK" (#32, fs%(32), f2%(32), 0%, rslt$(32))
            call "OPENCHCK" (#33, fs%(33), f2%(33), 0%, rslt$(33))
            call "OPENCHCK" (#34, fs%(34), f2%(34), 0%, rslt$(34))
            call "OPENCHCK" (#35, fs%(35), f2%(35), 0%, rslt$(35))

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

            str(line2$,62) = " BCKSTAT: " & str(cms2v$,,8)
            hflag% = 0%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
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
                      if keyhit% = 0% or keyhit% = 11% then L10225
                      if keyhit% = 8% then gosub inv_find
                      if keyhit% <> 0% then       L10130
L10225:             if keyhit% = 0% then hflag% = 0%
                    if keyhit% = 11% then hflag% = 1%
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%
            goto L11000

        inv_find
            info$ = hex(06) & "Select Invoice."
            init (hex(00)) plowkey$
            plowhdr$(1) = "  Customer Invoice    PO              SO"
            break% = 1000%
            if cuscode$ = " " then L10350
                info$ = hex(06) & "Select Invoice for " & cuscode$ & "."
                str(plowkey$,,9) = cuscode$
                plowhdr$(1) = "  Invoice    PO              SO"
            if cuscode$ = " " then break% = 1000% else break% = 1009%
L10350:     call "PLOWCODE" (#5, plowkey$, info$, break%, 0.32, f1%(5%), ~
                                                              plowhdr$())
            if f1%(5%) = 0% then return
                get #5 using L10400, cuscode$, so$
L10400:              FMT CH(9), XX(8), XX(16), CH(16)
                return

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
            goto show_status            /* Skip Edit Mode - */
        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       show_status
                  if keyhit% <>  0% then       editpg1
            if cursor%(1%) < 6% then cursor%(1%) = 6%
            if cursor%(1%) > 7% then cursor%(1%) = 7%
L11120:     fieldnr% = cursor%(1%) - 5%
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
            *            L I N K   T O   S O S T A T U S                *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        show_status

           call "SOSTATUS" (cuscode$,              /* Customer Code    */~
                            so$,                   /* Order #          */~
                            " ",                   /* Blnk for any line*/~
                            0%,                    /* 2% = sales units */~
                                                   /* Else stock units */~
                            #1,                    /* BCKLINES File UFB*/~
                            #2,                    /* SHPLINES File UFB*/~
                            #3,                    /* SHPHDRS  File UFB*/~
                            #4,                    /* BCKMASTR File UFB*/~
                            #5,                    /* ARIMASTR File UFB*/~
                            #6,                    /* ARILINES File UFB*/~
                            #7,                    /* ARILINES File UFB*/~
                            #9,                    /* BCKHMSTR File UFB*/~
                            #10,                   /* BCKHLNES File UFB*/~
                            #20,                   /* TXTFILE  File UFB*/~
                            1%,                    /* 1%= DEMSTAT files*/~
                                                   /*  sent; 0% = not  */~
                            hflag%,                /* 1%=Search History*/~
                            #21,                   /* PIPCROSS File UFB*/~
                            #22,                   /* DEMMASTR File UFB*/~
                            #23,                   /* JBMASTR2 File UFB*/~
                            #24,                   /* JBSTATUS File UFB*/~
                            #25,                   /* PIPOUT   File UFB*/~
                            #26,                   /* JBCROSS2 File UFB*/~
                            #27,                   /* VBKMASTR File UFB*/~
                            #28,                   /* VBKLINES File UFB*/~
                            #29,                   /* JBCREDIT File UFB*/~
                            #30,                   /* RCVLINES File UFB*/~
                            #31,                   /* PAYMASTR File UFB*/~
                            #32,                   /* PAYLINES File UFB*/~
                            #33,                   /* HNYLASTR File UFB*/~
                            #34,                   /* WCMASTR  File UFB*/~
                            #35)                   /* RTEMASTR File UFB*/
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Customer Code          */~
                              L20200          /* Sales Order            */
            return
L20100: REM Def/Enable Customer Code               CUSCODE$
            return

L20200: REM Def/Enable Sales Order Number          SO$
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Customer Code: Leave BLANK if just entering SO number  ",~
         "Enter SO # and Press RETURN to search Current, PF11 to search H~
        ~istory."
        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      cuscode$, so$
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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40080,         /* Customer Code     */   ~
                                L40080          /* Sales Order       */
              goto L40095

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40080:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40095:     accept                                                       ~
               at (01,02),                                               ~
                  "Sales Order Status Inquiry           ",               ~
               at (01,35), fac(hex(8c)), dispmsg$               , ch(13),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Customer Code",                              ~
               at (06,30), fac(lfac$( 1)), cuscode$             , ch(09),~
                                                                         ~
               at (07,02), "Sales Order Number",                         ~
               at (07,30), fac(lfac$( 2)), so$                  , ch(16),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40220
                  call "MANUAL" ("BCKSTAT") : goto L40095

L40220:        if keyhit% <> 15 then L40235
                  call "PRNTSCRN" : goto L40095

L40235:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40330     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field    (8" &        ~
                     ")FIND by Invoice #     (15)Print Screen"
            pf$(3) = "                                      (1" &        ~
                     "1)Search HISTORY       (16)Exit Program"
            pfkeys$ = hex(0001ffff04ffffff08ffff0bff0d0e0f1000)
            if fieldnr% = 1% then L40310
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% = 2% then str(pfkeys$,17,1) = hex(ff)
L40310:     if fieldnr% > 1% then L40320
                str(pf$(2),18,20) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40320:     return

L40330: if fieldnr% > 0% then L40375  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over       (10)Status Excluding" &        ~
                     " History               (13)Instructions"
            pf$(2) = "                    (11)Status Including" &        ~
                     " History               (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffff0a0bff0dff0fff00)
            return
L40375:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* Customer Code          */~
                              L50200          /* Sales Order            */
            return
L50100: REM Test for Customer Code                CUSCODE$
            if cuscode$ = " " then return
            hd$(1) = hex(0684) & "Select Customer, or PF16 To Return"
            call "GETCODE" (#7, cuscode$, hd$(1), 0%, 1.30, f1%(7))
                if f1%(7) = 1% then return
            errormsg$ = "No such customer on file."
            return

L50200: REM Test for Sales Order Number           SO$

            if so$ <> " " then L50250
              errormsg$ = "Sales Order cannot be BLANK"
            return

L50250:   REM 1st check BCKLINES for the SO#
            str(plowkey$,,16) = str(so$,,16)
            str(plowkey$,17) = all(hex(00))
              if hflag% = 1% then L50560
            call "PLOWNEXT" (#1,plowkey$,16%,f1%(1))
                if f1%(1) = 0 then L50300
              get #1, using L50285, cuscode$, so$
L50285:       FMT CH(9), CH(16)
            return

          REM Check here if a CUSCODE was input, if can't find SO above
L50300:       if cuscode$ = " " then L50498
            str(plowkey$,,9) = cuscode$
            str(plowkey$,10,16) = so$
            hd$(1) = hex(0684) & "CURRENT Files - Customer : " & cuscode$
            call "PLOWCODE" (#4,plowkey$,hd$(1),3009%,.30,f1%(4),hd$(1), ~
                               0, 42)
                if f1%(4) = 0 then L50400
            cuscode$ = str(plowkey$,,9)
            so$ = str(plowkey$,10,16)
            return

L50400:     errormsg$ = "Sales Order is not in the CURRENT files."
            return

L50498:   REM Check here if no CUSCODE was entered
            if so$ = "?" then so$ = all(hex(00))
            hd$(1) = hex(0684) & "CURRENT Files - Select SO or PF(16)"
                str(plowkey$,,16) = str(so$,,16)
                str(plowkey$,17,3) = all(hex(00))
            call "PLOWCODE" (#1, plowkey$, hd$(1), -16%, -.001, f1%(1))
                if f1%(1) = 0% then L50400
              get #1, using L50525, cuscode$, so$
L50525:       FMT CH(9), CH(16)
                return
L50560:
            REM Here if Searching the History Files
            call "PLOWNEXT" (#10,plowkey$,16%,f1%(10))
                if f1%(10) = 0 then L50680
              get #10, using L50640, cuscode$, so$
L50640:       FMT CH(9), CH(16)
            return

          REM Check here if a CUSCODE was input, if can't find SO above
L50680:       if cuscode$ = " " then L50820
            str(plowkey$,,9) = cuscode$
            str(plowkey$,10,16) = so$
            hd$(1) = hex(0684) & "HISTORY Files - Customer : " & cuscode$
            call "PLOWCODE" (#9,plowkey$,hd$(1),3009%,.30,f1%(9),hd$(1), ~
                               0, 42)
                if f1%(9) = 0 then L50790
            cuscode$ = str(plowkey$,,9)
            so$ = str(plowkey$,10,16)
            return

L50790:     errormsg$ = "Sales Order is not in the HISTORY files."
            return

L50820:   REM Check here if no CUSCODE was entered
            if so$ = "?" then so$ = all(hex(00))
            hd$(1) = hex(0684) & "HISTORY Files - Select SO or PF(16)"
                str(plowkey$,,16) = str(so$,,16)
                str(plowkey$,17,3) = all(hex(00))
            call "PLOWCODE" (#10, plowkey$, hd$(1), -16%, -.001, f1%(10))
                if f1%(10) = 0% then L50790
              get #10, using L50890, cuscode$, so$
L50890:       FMT CH(9), CH(16)
                return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
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
