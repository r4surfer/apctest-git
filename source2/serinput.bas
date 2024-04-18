        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   EEEEE  RRRR   IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  S      E      R   R    I    NN  N  P   P  U   U    T     *~
            *   SSS   EEEE   RRRR     I    N N N  PPPP   U   U    T     *~
            *      S  E      R   R    I    N  NN  P      U   U    T     *~
            *   SSS   EEEEE  R   R  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SERINPUT - Allows Additions, Modifications and Deletions  *~
            *             from the Serial Number File, SERMASTR.        *~
            *----------------------------------------------------------Q*~
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
            * 02/10/87 ! Original                                 ! MJB *~
            * 06/08/87 ! JBMASTR2 format change                   ! MJB *~
            * 06/02/89 ! Fixed logic flow on return from GETCODE  ! MLJ *~
            *          !   of job number.                         !     *~
            * 03/29/90 ! Now test that serial number exists before! JDH *~
            *          !   testing format for new number.         !     *~
            * 08/21/96 ! Century date conversion                  ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            bomid$3,                     /* Bill of Material ID        */~
            ccyymmdd$8,                  /* ccyymmdd                   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            datetime$7,                  /* Date/Time Stamp            */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fill1$85,                    /* Rest of record             */~
            format$20,                   /* Ser # Format Code          */~
            formatmsg$20,                /* Ser # Format Message       */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            inv_lot$6,                   /* Lot Number                 */~
            job$8,                       /* Job Number                 */~
            jobdescr$30,                 /* Job Number Description     */~
            job_start$8,                 /* Date Job Started           */~
            locinpmsg$(5)79,             /* Input msg for ser # loc    */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            liinfo$30,                   /* PO line not on file msg    */~
            line2$79,                    /* Second Line of Screen Headr*/~
            lotflag$1,                   /* Lot Track Y/N              */~
            max$2,                       /* Max Ser # Length           */~
            min$2,                       /* Min Ser # Length           */~
            nouse$30,                    /* Part Location for not Used */~
            partloc$30,                  /* Part Location              */~
            partnbr$25,                  /* Part Number                */~
            partno$25,                   /* Part Number                */~
            partnodescr$34,              /* Part Description           */~
            partstat$1,                  /* Part Status                */~
            partstatdescr$32,            /* Part Status                */~
            part_add_date$8,             /* Date Part Completed        */~
            pflit$(3)79,                 /* PFKEY Screen Literals      */~
            pf9fac$1,                    /* PF9 FAC                    */~
            pf9lit$26,                   /* PF9 Literal                */~
            pfkeys$32,                   /* Valid PF Key string        */~
            plowdescr$60,                /* Header description Plow    */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            poinfo$30,                   /* PO Not on file msg         */~
            poline$3,                    /* P.O. Line Item Number      */~
            pono$16,                     /* P.O. Number                */~
            readkey$50,                  /* File read key              */~
            rcvrnbr$16,                  /* Receiver Number            */~
            rteid$3,                     /* Routing ID                 */~
            serflag$1,                   /* Serial Number Y/N          */~
            serialno$(1)20,              /* Serial Number              */~
            serialtest$20,               /* Serial Number Test Area    */~
            statmessage$60,              /* Status header line         */~
            statdescr$(3)79,             /* Status Descriptions        */~
            smfac$1,                     /* Fac for status message     */~
            stfac$1,                     /* Fac for status descriptions*/~
            store$3,                     /* Part in Store Nbr          */~
            storedescr$32,               /* Part in Store Nbr          */~
            texta$(196,1)70,             /* Text Array                 */~
            textid$4,                    /* Key to TXTFILE             */~
            textmsg$79,                  /* Message for text routines  */~
            userid$3,                    /* Current User Id            */~
            vencode$9,                   /* Vendor Number              */~
            vencodedescr$32,             /* Vendor Number              */~
            vendlot$16                   /* Vendors Lot Number         */~

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
            * # 1 ! SERMASTR ! Serial Number Tracking Master File       *~
            * # 2 ! HNYMASTR ! Inventory Master File                    *~
            * # 3 ! HNYQUAN  ! Inventory Costs & Quantity Master        *~
            * # 4 ! TXTFILE  ! System Text File                         *~
            * # 5 ! VENDOR   ! Vendor Master Record                     *~
            * # 6 ! VBKLINES ! Purchase order line items file           *~
            * # 7 ! RCVLINES ! Receiver Line Items File                 *~
            * # 8 ! STORNAME ! Store information file                   *~
            * # 9 ! SYSFILE2 ! System Information File                  *~
            * #10 ! JBMASTR2 ! Job Master File                          *~
            * #11 ! VBKMASTR ! P.O. Master File                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76          ~

            select # 2, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select # 3, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            select # 4, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11                      ~

            select # 5, "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup     ~

            select # 6, "VBKLINES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28                      ~

            select # 7, "RCVLINES",                                      ~
                        varc,     indexed,  recsize =  800,              ~
                        keypos =   26, keylen =  52,                     ~
                        alt key  1, keypos =    1, keylen =  69,         ~
                            key  2, keypos =   42, keylen =  36,         ~
                            key  3, keypos =  128, keylen =  24          ~

            select # 8, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3                      ~

            select # 9, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =     1, keylen =  20

            select #10, "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1300,                                  ~
                        keypos = 1, keylen = 8

            select #11, "VBKMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1030,                                 ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  10,  keylen =  16

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6), 0%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 0%, rslt$( 7))
            call "OPENCHCK" (# 8, fs%( 8), f2%( 8), 0%, rslt$( 8))
            call "OPENCHCK" (# 9, fs%( 9), f2%( 9), 0%, rslt$( 9))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$, date%, ccyymmdd$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            statmessage$ = "Status Codes Are as Follows:"
            statdescr$(1) = "'1' - Part Currently Being Built      '4' - ~
        ~Part is Sold or Shipped"
            statdescr$(2) = "'2' - Part is in Inventory            '5' - ~
        ~Part is a Component    "
            statdescr$(3) = "'3' - Part in a Job for another Part  '9' - ~
        ~Serial Number NOT USED"

            pf9lit$ = "Reason Ser # Not Used"

        REM *** Get System Switches ***
            max$ = "6" : min$ = "6" : format$ = all("#")
            readkey$ = "SWITCHS.HNY         "
            call "READ100" (#9, readkey$, f1%(9))
                if f1%(9) = 0% then L09290
            get #9 using L09280, max$, min$, format$

L09280:         FMT POS(42), 2*CH(2), CH(20)
L09290:     max% = 6% : convert max$ to max%, data goto L09300
L09300:     min% = 6% : convert min$ to min%, data goto L09310
L09310:     formatmsg$ = format$
            tran(formatmsg$,"A#N+Y%")replacing

            mat redim serialno$(1)max%
            init (" ") pflit$()
            call "TXTFUTIL" (#4, f2%(4), "INTL", " ")

            locinpmsg$(1) = "Location Text is 'Job Number' as shown, " & ~
                            " you may Accept or Modify              "
            locinpmsg$(2) = "Location Text is 'Store' and 'Lot' (if l" & ~
                            "ot tracking is on), Accept or Modify   "
            locinpmsg$(3) = "Please Enter the Job Number & Parent Par" & ~
                            "t's Serial Number Containing this Part "
            locinpmsg$(4) = "Please Enter the Customer #, Invoice # &" & ~
                            " L.I. # pertaining to the sale         "
            locinpmsg$(5) = "Please enter the Parent Part number and " & ~
                            "Serial Number                          "

        REM *************************************************************~
            *       I N P U T   M O D E   H E A D E R   D A T A         *~
            *-----------------------------------------------------------*~
            * Handles normal input for header screen data.              *~
            *************************************************************

        inputmode
            gosub L29000
            sw% = 1%  :  mod% = 1%
            for fieldnr% = 1 to 6
L10100:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                stfac$, smfac$ = hex(9c)
                      if enabled% = 0 then L10240
L10130:         gosub'101(fieldnr%, sw%)   /* Display & Accept Screen */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then     L10220
                         if fieldnr% <= 2% then L10220
L10170:                  fieldnr% = max(3%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10100
                         goto L10170
L10220:               if keyhit% = 16 and fieldnr% = 1 then exit_program
                      if keyhit% <>  0 then       L10130
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%
            sw% = 2%

        REM *************************************************************~
            *        E D I T   M O D E   P A G E   O N E   ( 1 )        *~
            *-----------------------------------------------------------*

        editpg1
            stfac$, smfac$ = hex(9c)
            if inorout$ <> " " then inpmessage$ = edtmessage$ else       ~
            inpmessage$ = "Press PF4 to Input for a Manufactured Part, "&~
                          "or PF5 for a Purchased Part"
            if partstat% = 9% then inpmessage$ = edtmessage$
            if mod% = 0% then inpmessage$ = "This record is in process an~
        ~d CANNOT be modified at this time!"
            lastfieldnr% = 0%
            gosub'101(0%, sw%)      /* Display Screen - No Entry */
                  if keyhit%  =  1  then gosub startover
                  if keyhit%  =  4  and inorout$ <> " " then editpg2
                  if keyhit%  =  4  and inorout$  = " " then input_manuf
                  if keyhit%  =  5  and inorout$ <> " " then editpg3
                  if keyhit%  =  5  and inorout$  = " " then input_purch
                  if keyhit%  = 12  then gosub delete_it
                  if keyhit%  = 16  and partstat% = 9% then datasave
                  if keyhit%  = 16  and inorout$ <> " " then datasave
                  if keyhit%  = 25  then gosub edit_text
                  if keyhit% <>  0  then editpg1
            fieldnr% = cursor%(1) - 5
L10520:     if cursor%(1) > 6% then fieldnr% = cursor%(1) - 6
            if fieldnr% < 3 or fieldnr% >  6 then editpg1
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
L10570:     gosub'101(fieldnr%, sw%)    /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L10570
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L10570
                  lastfieldnr% = fieldnr%
            goto L10520

        REM *************************************************************~
            *       I N P U T   M O D E   M A N U F   P A R T           *~
            *-----------------------------------------------------------*~
            * Handles normal input for Manufactured Part Data           *~
            *************************************************************

        input_manuf
            inorout$ = "I" : sw% = 1%
            for fieldnr% = 1 to 6
L11090:         gosub'052(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L11200
L11110:         gosub'102(fieldnr%, sw%)    /* Display & Accept Screen */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L11190
L11140:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L11110
                         if fieldnr% = 1% then L11090
                         goto L11140
L11190:               if keyhit% <>  0 then       L11110
L11200:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L11110
            next fieldnr%
            sw% = 2%
            goto editpg1

        REM *************************************************************~
            *        E D I T   M O D E   P A G E   T W O   ( 2 )        *~
            *-----------------------------------------------------------*

        editpg2
            inpmessage$ = edtmessage$
            if mod% = 0% then inpmessage$ = "This record is in process an~
        ~d CANNOT be modified at this time!"
            if inorout$ = "X" then inorout$ = "I"
            lastfieldnr% = 0%
            gosub'102(0%, sw%)          /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  9 then       editpg1
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editpg2
L11370:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 6 then editpg2
            if fieldnr% = lastfieldnr% then editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg2
L11420:     gosub'102(fieldnr%, sw%)    /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11420
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11420
                  lastfieldnr% = fieldnr%
            goto L11370

        REM *************************************************************~
            *       I N P U T   M O D E   P U R C H   P A R T           *~
            *-----------------------------------------------------------*~
            * Handles normal input for Purchased Part Data.             *~
            *************************************************************

        input_purch
            inorout$ = "O" : sw% = 1%
            for fieldnr% = 1 to 7
L12090:         gosub'053(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L12200
L12110:         gosub'103(fieldnr%, sw%) /* Display & Accept Screen   */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L12190
L12140:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'053(fieldnr%)
                         if enabled% = 1% then L12110
                         if fieldnr% = 1% then L12090
                         goto L12140
L12190:               if keyhit% <>  0 then       L12110
L12200:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12110
            next fieldnr%
            sw% = 2%
            goto editpg1

        REM *************************************************************~
            *     E D I T   M O D E   P A G E   T H R E E   ( 3 )       *~
            *-----------------------------------------------------------*

        editpg3
            inpmessage$ = edtmessage$
            if mod% = 0% then inpmessage$ = "This record is in process an~
        ~d CANNOT be modified at this time!"
            lastfieldnr% = 0%
            if inorout$ = "X" then inorout$ = "O"
            gosub'103(0%, sw%)          /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  9 then       editpg1
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editpg3
L12370:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 7 then editpg3
            if fieldnr% = lastfieldnr% then editpg3
            gosub'053(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg3
L12420:     gosub'103(fieldnr%, sw%)    /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12420
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12420
                  lastfieldnr% = fieldnr%
            goto L12370

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
            if mod% = 0% then enabled% = 0%
            on fieldnr% gosub L20170,         /* Part Number        */    ~
                              L20210,         /* Serial Number      */    ~
                              L20250,         /* Part Status        */    ~
                              L20290,         /* Store Number       */    ~
                              L20350,         /* Inventory Lot No   */    ~
                              L20410          /* Not Used Descr     */
            return

L20170: REM Def/Enable Part Number                 PARTNO$
            inpmessage$ = "Enter the Part Number"
            return

L20210: REM Def/Enable Serial Number               SERIALNO$
            inpmessage$ = "Serial Number format is " & formatmsg$ &      ~
                " and from " & min$ & " to " & max$ & " Characters long"
            return

L20250: REM Def/Enable Part Status                 PARTSTAT$
            inpmessage$ = "Enter a Status Code for this Part"
            return

L20290: REM Def/Enable Part in Store Nbr           STORE$
            if partstat% = 2% or partstat% = 5% then L20320
            enabled% = 0% : return
L20320:     inpmessage$ = "Enter Store Number Where Part Located"
            return

L20350: REM Def/Enable Lot Number                  INV_LOT$
            if partstat% = 2% or partstat% = 5% then L20360
            enabled% = 0% : return
L20360:     if store$ = "   " then enabled% = 0%
            if lotflag$ = "N" then enabled% = 0%
            inpmessage$ = "Enter the Inventory Lot Number for this Part"
            return

L20410: REM Def/Enable Special Not Used Input      NOUSE$
            if partstat% <> 9% then enabled% = 0%
            inpmessage$ = "Enter Free Text on Why Not Used"
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            if mod% = 0% then enabled% = 0%
            on fieldnr% gosub L22170,         /* Job Number         */    ~
                              L22210,         /* Job Start Date     */    ~
                              L22250,         /* Job Complete Date  */    ~
                              L22340,         /* BOM ID             */    ~
                              L22380,         /* Routing ID         */    ~
                              L22420          /* Location Text      */
            return
L22170: REM Def/Enable Job Number                  JOB$
            inpmessage$ = "Enter the Production Job Number for This Part"
            return

L22210: REM Def/Enable Date Job Started            JOB_START$
            job_start$ = date
            call "DATEFMT" (job_start$)
            inpmessage$ = "Enter the Date This Job Started"
            return

L22250: REM Def/Enable Date Part Completed         PART_ADD_DATE$
            part_add_date$ = date
            call "DATEFMT" (part_add_date$)
            inpmessage$ = "Enter the Date This Part was Completed"
            return

L22340: REM Def/Enable Bill of Material ID         BOMID$
            inpmessage$ = "Enter the BOM ID That This Part was Built By"
            return

L22380: REM Def/Enable Routing ID                  RTEID$
            inpmessage$ = "Enter the RTE ID That This Part was Built By"
            return

L22420: REM Def/Enable Part Location               PARTLOC$
            gosub build_loc_text
            if partstat% = 0% then return
            inpmessage$ = locinpmsg$(partstat%)
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  3  of Input. *~
            *************************************************************

        deffn'053(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            if mod% = 0% then enabled% = 0%
            on fieldnr% gosub L24340,         /* Vendor Number      */    ~
                              L24380,         /* Date Received      */    ~
                              L24420,         /* P.O. Number        */    ~
                              L24460,         /* P.O. Line Item No  */    ~
                              L24500,         /* Vendors Lot Nbr    */    ~
                              L24540,         /* Receiver Number    */    ~
                              L24630          /* Location Text      */
            return

L24340: REM Def/Enable Vendor Number               VENCODE$
            inpmessage$ = "Enter the Vendor Code from Whom this Part was ~
        ~Received"
            return

L24380: REM Def/Enable Date Received               PART_ADD_DATE$
            part_add_date$ = date
            call "DATEFMT" (part_add_date$)
            inpmessage$ = "Enter the Date the Part Was Received"
            return

L24420: REM Def/Enable P.O. Number                 PONO$
            inpmessage$ = "Enter the Purchase Order Number"
            return

L24460: REM Def/Enable P.O. Line Item Number       POLINE$
            inpmessage$ = "Enter the P.O. Line Item Number"
            return

L24500: REM Def/Enable Vendors Lot Number          VENDLOT$
            inpmessage$ = "Enter the Vendor's Lot Number"
            return

L24540: REM Def/Enable Receiver Number             RCVRNBR$
            inpmessage$ = "Enter the Receiver Number for the Part"
            return

L24630: REM Def/Enable Part Location               PARTLOC$
            gosub build_loc_text
            if partstat% = 0% then return
            inpmessage$ = locinpmsg$(partstat%)
            return

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$,                            ~
                               partnodescr$    , /* Part Number        */~
                      serialno$(1)             , /* Serial Number      */~
                      partloc$, nouse$         , /* Part Location      */~
                      partstat$, partstatdescr$, /* Part Status        */~
                      store$, storedescr$      , /* Part in Store Nbr  */~
                      job$, jobdescr$          , /* Job Number         */~
                      job_start$               , /* Job Start Date     */~
                      part_add_date$           , /* Job Complete Date  */~
                      inv_lot$                 , /* Lot Number         */~
                      bomid$                   , /* BOM ID             */~
                      rteid$                   , /* Routing ID         */~
                      vencode$, vencodedescr$  , /* Vendor Number      */~
                      part_add_date$           , /* Date Received      */~
                      pono$, poinfo$           , /* P.O. Number        */~
                      poline$, liinfo$         , /* P.O. Line Item No  */~
                      vendlot$                 , /* Vendors Lot Nbr    */~
                      rcvrnbr$                 , /* Receiver Number    */~
                      inv_lot$                 , /* Inventory Lot No   */~
                      inorout$

            init(hex(00)) textid$

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
            get #1 using L35030,partstat$,partloc$,serialno$(1), partno$, ~
                   serialno$(1), job$,vencode$, rcvrnbr$, pono$, poline$,~
                   vendlot$, bomid$, rteid$, job_start$, part_add_date$, ~
                   store$, inv_lot$, datetime$, userid$, textid$, fill1$

            call "DATEFMT" (job_start$)
            call "DATEFMT" (part_add_date$)
            inorout$ = "X"  :  sw% = 3%
            if job$ <> " " then inorout$ = "I"
            if vencode$ <> " " then inorout$ = "O"
            call "TXTFUTIL" (#4, f2%(4), "LOAD", textid$)

            convert partstat$ to partstat, data goto L30230
            partstat% = partstat
            if partstat% = 9% then nouse$ = partloc$
            if partstat$ >= hex(30) and partstat$ <= hex(39) then return
L30230:     mod% = 0%
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            if mod% = 0% then return
            gosub build_loc_text
            call "DATUNFMT" (job_start$)
            call "DATUNFMT" (part_add_date$)
            call "GETDTTM" addr(datetime$)
            readkey$ = partno$
            str(readkey$,26,20) = serialno$(1)
            call "READ101" (#1, readkey$, f1%(1))
            put #1 using L35030,partstat$,partloc$,serialno$(1), partno$, ~
                   serialno$(1), job$,vencode$,rcvrnbr$, pono$, poline$, ~
                   vendlot$, bomid$, rteid$, job_start$, part_add_date$, ~
                   store$, inv_lot$, datetime$, userid$, textid$, fill1$

            if f1%(1) = 0% then write #1 else rewrite #1
            call "TXTFUTIL" (#4, f2%(4), "SAV2", " ")
            return

        REM *************************************************************~
            *        E D I T   T E X T   R O U T I N E                  *~
            *-----------------------------------------------------------*

        edit_text
            if mod% = 0% then L34200
            textmsg$ = "Text for Serial Number '" & serialno$(1) & "' on ~
        ~Part Number '" & partno$
            call "TXTINSUB"(#4,f2%(4),"020", textmsg$, textid$, texta$())
            return

L34200:     ask% = 2%
            call "ASKUSER" (ask%, "CANNOT MODIFY", "This part currently h~
        ~as a transaction in process.", "You may NOT MODIFY the text at th~
        ~is time.", "Press PF1 to RETURN")
            if ask% = 1% then return
            goto L34200

        REM *************************************************************~
            *        D E L E T E   M O D E   C O D E                    *~
            *-----------------------------------------------------------*
        delete_it
            if mod% = 0% then L34641
L34340:     ask% = 2%
            call "ASKUSER" (ask%, "DELETE CONFIRMATION", "Press PF28 to D~
        ~elete Serial Number " & serialno$(1), "- or -", "Press PF1 to Can~
        ~cel the DELETE request")
            if ask% = 1% then return
            if ask% <> 28% then L34340

            readkey$ = partno$
            str(readkey$,26,20) = serialno$(1)
            call "READ101" (#1, readkey$, f1%(1))
                if f1%(1) = 0% then exit_program  /* BIG Trouble!!  */
            delete #1
            call "TXTFUTIL" (#4, f2%(4), "DELE", textid$)
            return clear all
            goto inputmode

L34641:     ask% = 2%
            call "ASKUSER" (ask%, "CANNOT DELETE", "This part currently h~
        ~as a transaction in process.", "You may NOT DELETE this part at t~
        ~his time.", "Press PF1 to Cancel The DELETE Request")
            if ask% = 1% then return
            goto L34641

        REM *************************************************************~
            *        B U I L D   L O C A T I O N   T E X T              *~
            *-----------------------------------------------------------*
        build_loc_text
            if partstat% = 1% then partloc$ = job$
            if partstat% = 2% then partloc$ = str(store$) & inv_lot$
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: SERMASTR                          */~
            CH(1),          /* Current Status Of a Serial Numbered Part*/~
            CH(30),         /* Current Location Of a Serial Numbered Pa*/~
            CH(20),         /* Serial Number                           */~
            CH(25),         /* Part code                               */~
            CH(20),         /* Serial Number                           */~
            CH(8),          /* Job Number                              */~
            CH(9),          /* Vendor code                             */~
            CH(16),         /* Receiver Control Number                 */~
            CH(16),         /* Purchase Order Number                   */~
            CH(3),          /* Purchase Line Sequence Number (not ITEM */~
            CH(16),         /* Lot Number                              */~
            CH(3),          /* The specific BOM identifier for a Bill o*/~
            CH(3),          /* The specific routing to use for a Bill. */~
            CH(6),          /* Date production job actually started    */~
            CH(6),          /* Date Job Completed                      */~
            CH(3),          /* Warehouse or Store                      */~
            CH(16),         /* Which lot in inventory - always used wit*/~
            CH(7),          /* The system date and time                */~
            CH(3),          /* user-id of specific user                */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(85)          /* Rest of record, not used here           */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, sw%)
              str(line2$,62%) = "SERINPUT: " & str(cms2v$,,8%)
              str(line2$,,60%) = "Serial Number Header Screen"
              gosub set_pf1
              if partstat% <> 9% then pf9fac$ = hex(9c)                  ~
                                 else pf9fac$ = hex(8c)
              if fieldnr% <> 3% then L40140
              stfac$ = hex(8c)  :  smfac$ = hex(ac)
L40140:       if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40250,         /* Part Number       */   ~
                                L40250,         /* Serial Number     */   ~
                                L40250,         /* Part Status       */   ~
                                L40250,         /* Store Number      */   ~
                                L40250,         /* Lot Number        */   ~
                                L40240          /* Not Used Text     */
              goto L40280

L40240:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40250:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40280:     accept                                                       ~
               at (01,02),                                               ~
                  "Serial Number Management",                            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Part Number",                                ~
               at (06,30), fac(lfac$( 1)), partno$              , ch(25),~
               at (07,32), fac(hex(8c)),   partnodescr$         , ch(34),~
                                                                         ~
               at (08,02), "Serial Number",                              ~
               at (08,30), fac(lfac$( 2)), serialno$(1),                 ~
                                                                         ~
               at (09,02), "Part Status",                                ~
               at (09,30), fac(lfac$( 3)), partstat$            , ch(01),~
               at (09,49), fac(hex(8c)),   partstatdescr$       , ch(32),~
                                                                         ~
               at (10,02), "Part in Store Nbr",                          ~
               at (10,30), fac(lfac$( 4)), store$               , ch(03),~
               at (10,49), fac(hex(8c)),   storedescr$          , ch(32),~
                                                                         ~
               at (11,02), "Lot Number",                                 ~
               at (11,30), fac(lfac$( 5)), inv_lot$             , ch(06),~
                                                                         ~
               at (12,02), fac(pf9fac$),   pf9lit$              , ch(26),~
               at (12,30), fac(lfac$( 6)), nouse$               , ch(30),~
                                                                         ~
               at (15,02), fac(smfac$) ,   statmessage$         , ch(67),~
               at (16,02), fac(stfac$) ,   statdescr$(1)        , ch(79),~
               at (17,02), fac(stfac$) ,   statdescr$(2)        , ch(79),~
               at (18,02), fac(stfac$) ,   statdescr$(3)        , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)), pflit$(1),                      ~
               at (23,02), fac(hex(8c)), pflit$(2),                      ~
               at (24,02), fac(hex(8c)), pflit$(3),                      ~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

            if keyhit% <> 13 then L40730  :  call "MANUAL" ("SERINPUT")
                                            goto L40280

L40730:     if keyhit% <> 15 then L40760  :  call "PRNTSCRN"
                                            goto L40280

L40760:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf1
            if sw% > 1% then L40960  /* This is input mode */
            pflit$(1) = "(1)Start Over                           "  &    ~
                        "                       (13)Instructions"
            pflit$(2) = "                 (4)Previous Field      "  &    ~
                        "                       (15)Print Screen"
            pflit$(3) = "                                        "  &    ~
                        "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40910
            str(pflit$(3),64) = " "    : str(pfkeys$,16,1) = hex(ff)
L40910:     if fieldnr% > 3% then L40930
            str(pflit$(2),18,20) = " " : str(pfkeys$, 4,1) = hex(ff)
L40930:     return

*        This is edit mode - field select, new serial #, input not done
L40960:     if fieldnr% > 0% then L41180
            if inorout$ <> " " then L41290

            pflit$(1) = "(1)Start Over                           "  &    ~
                        "                       (13)Instructions"
            pflit$(2) = "                 (4)Enter Job Data      "  &    ~
                        "                       (15)Print Screen"
            pflit$(3) = "                 (5)Enter Vendor Data   "  &    ~
                        "                                       "

            pfkeys$ = hex(01ffff0405ffffffffffffff0dff0fff00)

            if partstat% <> 9% then L41140
            str(pflit$(2),18,20) = " "  :  str(pflit$(3),18,20) = " "
            str(pflit$(3),64,16) = "(16)Save Data"
            str(pfkeys$,4,2) = hex(ff)  :  str(pfkeys$,16,1) = hex(10)
            return

L41140:     if partstat% <> 1% then return
            str(pflit$(3),18,20) = " "  :  str(pfkeys$,5,1) = hex(ff)  :
            return

L41180
*        Edit mode - field enabled
            pflit$(1) = "(1)Start Over                           "  &    ~
                        "                       (13)Instructions"
            pflit$(2) = "                                        "  &    ~
                        "                       (15)Print Screen"
            pflit$(3) = "                                        "  &    ~
                        "                                       "

            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

L41290
*        This is edit mode, data entry complete
            pflit$(1) = "(1)Start Over                           "  &    ~
                        "(12)Delete Serial No.  (13)Instructions"
            pflit$(2) = "                 (4)Edit Job Data       "  &    ~
                        "(25)Manage Text        (15)Print Screen"
            pflit$(3) = "                 (5)Edit Vendor Data    "  &    ~
                        "                       (16)Save Data   "

            pfkeys$ = hex(01ffff0405ffffffffffff0c0dff0f100019)

            if partstat% <> 9% then L41450
            str(pflit$(2),18,20) = " "  :  str(pflit$(3),18,20) = " "
            str(pfkeys$,4,2) = hex(ff)
            return

            if partstat% = 1% then L41470
L41450:     if inorout$ = "X" then return
            if inorout$ = "O" then L41490
L41470:     str(pflit$(3),18,20) = " "  :  str(pfkeys$,5,1) = hex(ff)
            return
L41490:     str(pflit$(2),18,20) = " "  :  str(pfkeys$,4,1) = hex(ff)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, sw%)
              str(line2$,62%) = "SERINPUT: " & str(cms2v$,,8%)
              str(line2$,,60%) = "Manufactured Part Job Information"
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42210,         /* Job Number        */   ~
                                L42210,         /* Job Start Date    */   ~
                                L42210,         /* Job Complete Date */   ~
                                L42210,         /* BOM ID            */   ~
                                L42210,         /* Routing ID        */   ~
                                L42210          /* Location Text     */
              goto L42240

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42210:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42240:     accept                                                       ~
               at (01,02),                                               ~
                  "Serial Number Management",                            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Job Number",                                 ~
               at (06,30), fac(lfac$( 1)), job$                 , ch(08),~
               at (06,49), fac(hex(8c))  , jobdescr$            ,        ~
                                                                         ~
               at (07,02), "Date Job Started",                           ~
               at (07,30), fac(lfac$( 2)), job_start$           , ch(08),~
                                                                         ~
               at (08,02), "Date Part Completed",                        ~
               at (08,30), fac(lfac$( 3)), part_add_date$       , ch(08),~
                                                                         ~
               at (09,02), "Bill of Material ID",                        ~
               at (09,30), fac(lfac$( 4)), bomid$               , ch(03),~
                                                                         ~
               at (10,02), "Routing ID",                                 ~
               at (10,30), fac(lfac$( 5)), rteid$               , ch(03),~
                                                                         ~
               at (11,02), "Location Text",                              ~
               at (11,30), fac(lfac$( 6)), partloc$             , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)), pflit$(1),                      ~
               at (23,02), fac(hex(8c)), pflit$(2),                      ~
               at (24,02), fac(hex(8c)), pflit$(3),                      ~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

            if keyhit% <> 13 then L42610  :  call "MANUAL" ("SERINPUT")
                                            goto L42240

L42610:     if keyhit% <> 15 then L42640  :  call "PRNTSCRN"
                                            goto L42240

L42640:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf2
            if sw% > 1% then L42830  /* This is input mode */
            pflit$(1) = "(1)Start Over                           "  &    ~
                        "                       (13)Instructions"
            pflit$(2) = "                 (4)Previous Field      "  &    ~
                        "                       (15)Print Screen"
            pflit$(3) = "                                        "  &    ~
                        "                                        "

            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0fff00)
            if fieldnr% > 1% then L42800
            str(pflit$(2),18,20) = " " : str(pfkeys$, 4,1) = hex(ff)
L42800:     return

*        This is edit mode - field select
L42830:     if fieldnr% > 0% then L42940
            pflit$(1) = "(1)Start Over                           "  &    ~
                        "(9)Header Screen       (13)Instructions"
            pflit$(2) = "                                        "  &    ~
                        "                       (15)Print Screen"
            pflit$(3) = "                                        "  &    ~
                        "                       (16)Save Data   "

            pfkeys$ = hex(01ffffffffffffff09ffffff0dff0f1000)
            return

L42940
*        Edit mode - field enabled
            pflit$(1) = "(1)Start Over                           "  &    ~
                        "                       (13)Instructions"
            pflit$(2) = "                                        "  &    ~
                        "                       (15)Print Screen"
            pflit$(3) = "                                        "  &    ~
                        "                                        "

            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%, sw%)
              str(line2$,62%) = "SERINPUT: " & str(cms2v$,,8%)
              str(line2$,,60%) = "Purchased Part & Vendor Information"
              gosub set_pf3
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L44220,         /* Vendor Number     */   ~
                                L44220,         /* Date Received     */   ~
                                L44220,         /* P.O. Number       */   ~
                                L44230,         /* P.O. Line Item No */   ~
                                L44220,         /* Vendors Lot Nbr   */   ~
                                L44220,         /* Receiver Number   */   ~
                                L44220          /* Location Text     */
              goto L44250

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L44220:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L44230:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L44250:     accept                                                       ~
               at (01,02),                                               ~
                  "Serial Number Management",                            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Vendor Number",                              ~
               at (06,30), fac(lfac$( 1)), vencode$             , ch(09),~
               at (06,49), fac(hex(8c)),   vencodedescr$        , ch(32),~
                                                                         ~
               at (07,02), "Date Received",                              ~
               at (07,30), fac(lfac$( 2)), part_add_date$       , ch(08),~
                                                                         ~
               at (08,02), "P.O. Number",                                ~
               at (08,30), fac(lfac$( 3)), pono$                , ch(16),~
               at (08,49), fac(hex(8c)),   poinfo$              , ch(30),~
                                                                         ~
               at (09,02), "P.O. Line Item Number",                      ~
               at (09,30), fac(lfac$( 4)), poline$              , ch(03),~
               at (09,49), fac(hex(8c)),   liinfo$              , ch(30),~
                                                                         ~
               at (10,02), "Vendors Lot Number",                         ~
               at (10,30), fac(lfac$( 5)), vendlot$             , ch(16),~
                                                                         ~
               at (11,02), "Receiver Number",                            ~
               at (11,30), fac(lfac$( 6)), rcvrnbr$             , ch(16),~
                                                                         ~
               at (12,02), "Location Text",                              ~
               at (12,30), fac(lfac$( 7)), partloc$             , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)), pflit$(1),                      ~
               at (23,02), fac(hex(8c)), pflit$(2),                      ~
               at (24,02), fac(hex(8c)), pflit$(3),                      ~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

            if keyhit% <> 13 then L44660  :  call "MANUAL" ("SERINPUT")
                                            goto L44250

L44660:     if keyhit% <> 15 then L44690  :  call "PRNTSCRN"
                                            goto L44250

L44690:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf3
            if sw% > 1% then L44890  /* This is input mode */
            pflit$(1) = "(1)Start Over                           "  &    ~
                        "                       (13)Instructions"
            pflit$(2) = "                 (4)Previous Field      "  &    ~
                        "                       (15)Print Screen"
            pflit$(3) = "                                        "  &    ~
                        "                                        "

            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0fff00)
            if fieldnr% > 1% then L44850
            str(pflit$(2),18,20) = " " : str(pfkeys$, 4,1) = hex(ff)
L44850:     return

*        This is edit mode - field select
            if fieldnr% > 0% then L42940
L44890:     pflit$(1) = "(1)Start Over                           "  &    ~
                        "(9)Header Screen       (13)Instructions"
            pflit$(2) = "                                        "  &    ~
                        "                       (15)Print Screen"
            pflit$(3) = "                                        "  &    ~
                        "                       (16)Save Data   "

            pfkeys$ = hex(01ffffffffffffff09ffffff0dff0f1000)
            return

*        Edit mode - field enabled
            pflit$(1) = "(1)Start Over                           "  &    ~
                        "                       (13)Instructions"
            pflit$(2) = "                                        "  &    ~
                        "                       (15)Print Screen"
            pflit$(3) = "                                        "  &    ~
                        "                                        "

            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50280,         /* Part Number       */     ~
                              L50540,         /* Serial Number     */     ~
                              L51420,         /* Part Status       */     ~
                              L51580,         /* Store Number      */     ~
                              L51710,         /* Lot Number        */     ~
                              L51890          /* Special No Use    */
            return

L50280: REM Test for Part Number                  PARTNO$
            partnodescr$ = hex(06) & "Select the Part Number"
            call "GETCODE" (#2, partno$, partnodescr$, 1%, 0.32, f1%(2))
                if f1%(2) = 1% then L50400
            errormsg$ = "Part Number '" & partno$ & "' is NOT on File "
            return
L50400:     get #2 using L50420, lotflag$, serflag$
L50420:         FMT POS(130), 2*CH(1)
            if serflag$ = "Y" then return
            errormsg$ = "Part Number " & partno$ & " is not defined as a ~
        ~serialized part"
            return

L50540: REM Test for Serial Number                SERIALNO$
*        1st test to see if serial number on file, if not, we'll come
*        back around to test format
            goto L51100
L50560:     serialtest$ = serialno$(1)
            if len(serialtest$) >= min% and len(serialtest$) <= max%     ~
               then L50680
               errormsg$ = "Serial Number Must be Between " & min$ &     ~
                            " and " & max$ & " characters long."
               return
L50680:     for x% = 1% to len(serialtest$)
                if str(format$,x%,1%) = "#" then L51000
                if str(format$,x%,1%) = "+" then L50960
                if x% > 19% then L50840
                if str(format$,x%,2%) <> "%%" then L50840
                   str(serialtest$,x%,2%) = str(ccyymmdd$,3%,2%)
                   x% = x% + 1%
                   goto L51000
L50840:         if str(format$,x%,1%) = str(serialtest$,x%,1%) then L51000
L50860:            errormsg$ = "Sorry, but a Serial Number MUST be in "  ~
                             & "the format " & formatmsg$
                   x% = 99%
                   goto L51000
*        Test for Digits Only
L50960:         if str(serialtest$,x%,1%) < "0" or str(serialtest$,x%,1%)~
                                          > "9" then L50860
L51000:     next x%
            if errormsg$ > " " then return
            serialno$(1) = serialtest$      /* Everything AOK Fine */
            return

*        See if it's already on file
L51100:     init(hex(00)) readkey$
            str(readkey$,1,20) = serialno$(1)
            call "PLOWALTS" (#1, readkey$, 1%, 20%, f1%(1))
                if f1%(1) = 0 then L50560    /* It's a new one! */
            get #1 using L51200, partnbr$
L51200:         FMT  POS(52), CH(25)
            if partnbr$ = partno$ then L51340
            errormsg$ = "Serial Number " & serialno$(1) & " is already on~
        ~ file for Part Number " & partnbr$
            return

*        Found it, so load it and return to edit mode
L51340:     gosub dataload
            return clear all
            goto editpg1

L51420: REM Test for Part Status                  PARTSTAT$
            convert partstat$ to partstat, data goto L51520
            partstat% = partstat
            if partstat% <> 9% then nouse$ = " "
            if (partstat% > 0% and partstat% < 6%) or                    ~
                partstat% = 9% then return
L51520:     errormsg$ = "Status Code must be '1' thru '5' or = '9'"
            return

L51580: REM Test for Part in Store Nbr            STORE$
            if partstat% = 2% or partstat% = 5% then L51600
                return
L51600:     storedescr$ = hex(06) & "Select Store That Part is in"
            call "GETCODE" (#8, store$, storedescr$, 1%, 0.3, f1%(8))
                if f1%(8) = 1% then return
            errormsg$ = "Store '" & store$ & "' Does Not Exist"
            return

L51710: REM Test for Lot Number                   INV_LOT$
            if store$ = " " then return
            errormsg$ = "LOT_CHECK"
            call "LOTVALID" (partno$, store$, inv_lot$, #9, #2, #3,      ~
                             errormsg$)
            if errormsg$ <> " " then return
            plowkey$ = partno$
            str(plowkey$,26,3) = store$
            str(plowkey$,29,16) = inv_lot$
            plowdescr$ = hex(06) & "Shown below are the lots for part '" ~
                         & partno$ & "', in store '" & store$
            call "PLOWCODE"(#3, plowkey$, plowdescr$, 28%, 0.001, f1%(3))
                if f1%(3) = 0% then L51850
            inv_lot$ = str(plowkey$,29,16)
            return
L51850:     errormsg$ = "Lot Number '" & inv_lot$ & "' Not on File for Th~
        ~is Part"
            return

L51890: REM Test for Location Text                PARTLOC$
            partloc$ = nouse$
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L53170,         /* Job Number        */     ~
                              L53290,         /* Job Start Date    */     ~
                              L53330,         /* Job Complete Date */     ~
                              L53510,         /* BOM ID            */     ~
                              L53540,         /* Routing ID        */     ~
                              L53570          /* Location Text     */
            return

L53170: REM Test for Job Number                   JOB$
            jobdescr$ = hex(06) & "Select Job Number that Part is in"
            call "GETCODE" (#10, job$, jobdescr$, 1%, 0.3, f1%(10))
                if f1%(10) = 1% then L53230
*          JOBDESCR$ = "Job '" & JOB$ & "' is Not on File"
            errormsg$ = "Job Number can NOT be blank"
            return
L53230:     get #10 using L53240, partnbr$
L53240:         FMT POS(58), CH(25)
            if partno$ = partnbr$ then return
            errormsg$ = "Job '" & job$ & "' is for part # " & partnbr$ & ~
                        ", NOT part # " & partno$
            return

L53290: REM Test for Date Job Started             JOB_START$
            call "DATEOK" (job_start$, date%, errormsg$)
            return

L53330: REM Test for Date Part Completed          PART_ADD_DATE$
            call "DATEOK" (part_add_date$, date%, errormsg$)
            return

L53510: REM Test for Bill of Material ID          BOMID$
            return

L53540: REM Test for Routing ID                   RTEID$
            return

L53570: REM Test for Location Text                PARTLOC$
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************

        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L55320,         /* Vendor Number     */     ~
                              L55440,         /* Date Received     */     ~
                              L55500,         /* P.O. Number       */     ~
                              L55700,         /* P.O. Line Item No */     ~
                              L55920,         /* Vendors Lot Nbr   */     ~
                              L55960,         /* Receiver Number   */     ~
                              L56000          /* Location Text     */
            return

L55320: REM Test for Vendor Number                VENCODE$
            vencodedescr$ = hex(06) & "Select Part Supplier"
            call "GETCODE" (#5, vencode$, vencodedescr$, 1%, 0.3, f1%(5))
                if f1%(5) = 1% then return
            errormsg$ = "Vendor Number '" & vencode$ & "' Does Not Exist"
            return

L55440: REM Test for Date Received                PART_ADD_DATE$
            call "DATEOK" (part_add_date$, date%, errormsg$)
            return

L55500: REM Test for P.O. Number                  PONO$
            poinfo$ = " "
            plowkey$ = vencode$
            plowdescr$ = hex(06) & "Select the P.O. from " & vencode$
            str(plowkey$,10,16) = pono$
            call "GETCODE" (#11, plowkey$, plowdescr$, 0%, 0.3, f1%(11))
                if f1%(11) = 0% then L55660
            pono$ = str(plowkey$,10,16) : return
L55660:     poinfo$ = "PO " & pono$ & " Not on File"
            return

L55700: REM Test for P.O. Line Item Number        POLINE$
            call "STRING" addr("RJ", poline$, 3%)
            if poinfo$ <> " " then return
            liinfo$ = " "
            plowdescr$ = hex(06) & "Select the P.O. Line Item"
            plowkey$ = vencode$ : str(plowkey$,10,16) = pono$
            str(plowkey$,26,3) = poline$
            call "GETCODE" (#6, plowkey$, plowdescr$, 0%, 0.3, f1%(6))
                if f1%(6) = 0% then L55880
            poline$ = str(plowkey$,26,3) : return
L55880:     liinfo$ = "Line Item #" & poline$ & " Not on PO"
            return

L55920: REM Test for Vendors Lot Number           VENDLOT$
            return

L55960: REM Test for Receiver Number              RCVRNBR$
            return

L56000: REM Test for Part Location                PARTLOC$
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
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
