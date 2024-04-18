        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   RRRR   L      W   W   222   M   M  TTTTT  PPPP    *~
            *  P   P  R   R  L      W   W      2  MM MM    T    P   P   *~
            *  PPPP   RRRR   L      W   W   222   M M M    T    PPPP    *~
            *  P      R  R   L      W W W  2      M   M    T    P       *~
            *  P      R   R  LLLLL   W W   22222  M   M    T    P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLW2MTP - This program allows the user to maintain the   *~
            *            Employer related data for submitting W-2 data  *~
            *            on magnetic tape media. Cloned from PRLMAGIN   *~
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
            * 11/19/87 ! Original (PRLMAGIN)                      ! DAW *~
            * 09/28/88 ! Spruce up. Get rid of hard-coded data.   ! JIM *~
            * 11/09/88 ! Added some Testing for Required Fields   ! MJB *~
            * 12/10/88 ! Made SSA-required mods for 1988 report.  ! JIM *~
            * 01/05/89 ! Much code REM'd out but left in just in  ! JIM *~
            *          !   case it's needed in the future.        ! JIM *~
            * 12/07/90 ! Cloned, Cleaned up & so forth            ! KAB *~
            *          ! Added Other EIN for type 'E' Record      !     *~
            * 12/04/91 ! Added additional mapping for Box 17.     ! JBK *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 12/03/93 ! Changed all internal and external refer  ! JBK *~
            *          !  to Box 17, to Box 13                    !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            address$40,                  /* Street Address             */~
            blocking$2,                  /* Blocking factor            */~
            box13map$(14)1,              /* Box 13 Map                 */~
            city$25,                     /* City                       */~
            computer$8,                  /* Computer Mfr. name         */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            density$2,                   /* Tape density code          */~
            edtmessage$79,               /* Edit screen message        */~
            ein$9,                       /* Transmitter's E.I.N.       */~
            employment$1,                /* Type of Employment         */~
            errormsg$79,                 /* Error message              */~
            est_numbr$4,                 /* Establishment Number       */~
            forgn_a$1, forgn_b$1,        /* Foreign Address indicators */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            label$2,                     /* Internal labeling code     */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            liability$1,                 /* Liability Limitation Flag  */~
            line2$79,                    /* Screen Line #2             */~
            local69$9,                   /* State / Local 69 Number    */~
            name_code$1,                 /* Name Code                  */~
            otherein$9,                  /* Other EIN Number           */~
            pay_year$4,                  /* Payment Year               */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            character$3,                 /* Character Set              */~
            ret_address$35,              /* File Return Address        */~
            ret_city$20,                 /* File Return City           */~
            ret_zip_code$5,              /* Zip/Foreign Postal code    */~
            ret_name$44,                 /* File Return Name           */~
            ret_state$2,                 /* File Return State          */~
            ret_zip_extn$5,              /* Zip/Foreign extension      */~
            state$2,                     /* State                      */~
            tran_name$50,                /* Transmitter Name           */~
            userid$3,                    /* Current User Id            */~
            zip_code$5, zip_extn$5       /* Zip Code & extension       */

        dim rslt$20                      /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.05 12/06/93 1993 Year End Payroll Patch     "

            f2% = 1% /* File is closed */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            *  #1 ! SYSFILE2 ! System data file.                        *~
            *************************************************************

            select #1, "SYSFILE2", varc, indexed, recsize = 500,         ~
                       keypos = 1, keylen = 20


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%, f2%, 0%, rslt$)

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                " to Desired Value & Press (RETURN)."
            str(line2$,62) = "PRLW2MTP: " & str(cms2v$,,8)

            gosub initialize_variables
            gosub dataload
            if hit% <> 0% then goto editpg1

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  9% /* Transmitter "A" record fields */
L10120:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10240
L10140:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10220
L10170:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10140
                         if fieldnr% = 1% then L10120
                         goto L10170
L10220:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10140
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10140
            next fieldnr%

            for fieldnr% = 1% to  11% /* Employer "B" record fields */
L10290:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10410
L10310:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10390
L10340:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10310
                         if fieldnr% = 1% then L10290
                         goto L10340
L10390:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10310
L10410:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10310
            next fieldnr%

            for fieldnr% = 1% to  7% /* Employer "E" record fields */
L10460:         gosub'053(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10580
L10480:         gosub'103(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10560
L10510:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'053(fieldnr%)
                         if enabled% = 1% then L10480
                         if fieldnr% = 1% then L10460
                         goto L10510
L10560:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10480
L10580:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10480
            next fieldnr%

            for fieldnr% = 1% to 1%
L10650:         gosub'054(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10770
L10670:         gosub'104(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10750
L10700:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'054(fieldnr%)
                         if enabled% = 1% then L10670
                         if fieldnr% = 1% then L10650
                         goto L10700
L10750:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10670
L10770:         gosub'154(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10670
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
                  if keyhit%  =  5% then       editpg2
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  9% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
                  lastfieldnr% = fieldnr%
            goto L11130

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then       editpg1
                  if keyhit%  =  5% then       editpg3
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg2
L11340:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 11% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L11390:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11390
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11390
                  lastfieldnr% = fieldnr%
            goto L11340

        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then       editpg2
                  if keyhit%  =  5% then       editpg4
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg3
L11560:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  7% then editpg3
            if fieldnr% = lastfieldnr% then    editpg3
            gosub'053(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg3
L11610:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11610
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11610
                  lastfieldnr% = fieldnr%
            goto L11560

        editpg4
            lastfieldnr% = 0%
            gosub'104(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then       editpg3
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg4
L11780:     fieldnr% = cursor%(1%) - 4% : fieldnr% = 1%
            if fieldnr% < 1% or fieldnr% >  1% then editpg4
            if fieldnr% = lastfieldnr% then    editpg4
            gosub'054(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg4
L11830:     gosub'104(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11830
            gosub'154(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11830
                  lastfieldnr% = fieldnr%
            goto L11780

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto L65000  /* Exit Program */

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20200,         /* Payment Year           */~
                              L20230,         /* Transmitter  EIN       */~
                              L20260,         /* Transmitter Name       */~
                              L20290,         /* Street Address         */~
                              L20320,         /* City                   */~
                              L20350,         /* State                  */~
                              L20380,         /* Zip Code/Foreign Postal*/~
                              L20410,         /* Zip code extension     */~
                              L20432          /* Forgn Address flag     */
            return
L20200: REM Def/Enable Payment Year                PAY_YEAR$
            return

L20230: REM Def/Enable Transmitter's E.I.N.        EIN$
            return

L20260: REM Def/Enable Transmitter Name            TRAN_NAME$
            return

L20290: REM Def/Enable Street Address              ADDRESS$
            return

L20320: REM Def/Enable City                        CITY$
            return

L20350: REM Def/Enable State                       STATE$
            return

L20380: REM Def/Enable Zip Code/Foreign Postal     ZIP_CODE$
            return

L20410: REM Def/Enable Zip code extension          ZIP_EXTN$
            return

L20432: REM Def/Enable Foreign Address flag        FORGN_A$
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L21190,         /* File Return Name       */~
                              L21220,         /* File Return Addr       */~
                              L21250,         /* File Return City       */~
                              L21280,         /* File Return State      */~
                              L21340,         /* File Return Zip/Foreign*/~
                              L21310,         /* File Return Zip exten  */~
                              L21361,         /* Foreign Address flag   */~
                              L21370,         /* Computer               */~
                              L21400,         /* Internal labeling      */~
                              L21430,         /* Density                */~
                              L21460          /* Character set          */
            return
L21190: REM Def/Enable File Return Name            RET_NAME$
            return

L21220: REM Def/Enable File Return Address         RET_ADDRESS$
            return

L21250: REM Def/Enable File Return City            RET_CITY$
            return

L21280: REM Def/Enable File Return State           RET_STATE$
            return

L21310: REM Def/Enable Zip/Foreign Postal code     RET_ZIP_CODE$
            return

L21340: REM Def/Enable File Return Zip extension   RET_ZIP_EXTN$
            return

L21361: REM Def/Enable Foreign Address flag        FORGN_B$
            return

L21370: REM Def/Enable Computer                    COMPUTER$
            return

L21400: REM Def/Enable Internal labeling           LABEL$
            return

L21430: REM Def/Enable Tape density                DENSITY$
            return

L21460: REM Def/Enable Character set               CHARACTER$
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  3  of Input. *~
            *************************************************************

        deffn'053(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L22192,         /* Liability Limitation   */~
                              L22200,         /* State-Local 69 #       */~
                              L22230,         /* Name Code              */~
                              L22260,         /* Establishment Num      */~
                              L22281,         /* Type of Employment     */~
                              L22284,         /* Blocking factor        */~
                              L22287          /* Other EIN              */
            return
L22192: REM Def/Enable Liability Limitation Flag   LIABILITY$
            return

L22200: REM Def/Enable State / Local 69 Number     LOCAL69$
            return

L22230: REM Def/Enable Name Code                   NAME_CODE$
            return

L22260: REM Def/Enable Establishment Number        EST_NUMBR$
            return

L22281: REM Def/Enable Type of Employment          EMPLOYMENT$
            return

L22284: REM Def/Enable Blocking factor             BLOCKING$
            return

L22287: REM Def/Enable Other EIN                   OTHEREIN$
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   4     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  4  of Input. *~
            *************************************************************

        deffn'054(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L23190,         /* BOX 13 MAPPING         */~
                              L23220          /*                        */

            return
L23190: REM Def/Enable
            if str(box13map$()) = " " then                               ~
               str(box13map$()) = "UUIDDDDD****UU"
                                /* ABCDEFGHIJKLMN */
            return

L23220: REM Def/Enable
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
            if scrnr% = 2% then restore line = scrn2_msg, fieldnr%
            if scrnr% = 3% then restore line = scrn3_msg, fieldnr%
            if scrnr% = 4% then restore line = scrn4_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Payment Year (4 digits).                               ",~
         "Enter Transmitter's E.I.N. (9 digits only--no punctuation).  ",~
         "Enter Transmitter Name (50 characters).                      ",~
         "Enter Street Address (40 characters).                        ",~
         "Enter City (25 characters).                                  ",~
         "Enter State (standard 2-character U.S. postal code).         ",~
         "Enter Zip Code (US) or Foreign Postal Code.                  ",~
         "Enter/Continue Zip code or Foreign Postal code extension.    ",~
         "Foreign Address Indicator = 'X' if above address is extra-US."

        scrn2_msg  :  data                                               ~
         "Enter File Return Name (44 characters).                      ",~
         "Enter File Return Address (35 characters).                   ",~
         "Enter File Return City (20 characters).                      ",~
         "Enter File Return State (standard 2-character U.S. postal code)~
        ~.",                                                              ~
         "Enter File Return Zip Code (US) or Foreign Postal Code.      ",~
         "Enter/Continue File Return Zip code or Foreign Postal code exte~
        ~nsion.",                                                         ~
         "Foreign Address Indicator = 'X' if above address is extra-US.",~
         "Enter Computer Manufacturer's name (8 characters).           ",~
         "Enter Internal Labeling (SL, NS, or NL).                     ",~
         "Enter Tape Density (08, 16, or 62).                          ",~
         "Enter Character Set (EBC or ASC).                            "

        scrn3_msg  :  data                                               ~
         "Enter Liability Limitation Flag.                             ",~
         "Enter State / Local 69 Number (9 digits: 69xxxxxxx).         ",~
         "Enter Name Code (S=Surname first; F=First name first).       ",~
         "Enter Establishment Number.                                  ",~
         "A-agriculture, H-household, M-military, Q-mileage, X-railroad o~
        ~r R-regular."                                                   ,~
         "Enter Blocking factor.                                       ",~
         "Enter Other EIN.                                             "

        scrn4_msg  :  data                                               ~
         "Indicate Summation Codes for W-2 Box 13 Codes                ",~
         "                                                             "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                address$, city$, computer$, ein$, density$,              ~
                est_numbr$, zip_extn$, liability$, local69$, name_code$, ~
                pay_year$, character$, label$, tran_name$, zip_code$,    ~
                ret_address$, ret_city$, ret_zip_code$, ret_name$,       ~
                ret_state$, ret_zip_extn$, state$, otherein$
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
            hit% = 0% /* Indicate no record(s) found */
            plowkey$ = "MAGNETIC W-2 REC A"
            call "READ100" (#1, plowkey$, fs%)
                if fs% = 0% then goto L30160
            hit% = 1% /* Indicate record(s) found */
            get #1 using L30140, pay_year$, ein$, forgn_a$, tran_name$,   ~
                                address$, city$, state$, zip_code$,      ~
                                zip_extn$
L30140:         FMT XX(21), CH(4), CH(9), XX(8), CH(1), CH(50), CH(40),  ~
                    CH(25), CH(2), XX(8), CH(5), CH(5)

L30160:     plowkey$ = "MAGNETIC W-2 REC B"
            call "READ100" (#1, plowkey$, fs%)
                if fs% = 0% then goto L30250
            hit% = 1% /* Indicate record(s) found */
            get #1 using L30240, computer$, label$, density$, character$, ~
                                forgn_b$, ret_name$, ret_address$,       ~
                                ret_city$, ret_state$, ret_zip_code$,    ~
                                ret_zip_extn$

L30240:         FMT XX(34), CH(8), CH(2), XX(1), CH(2), CH(3), XX(115),  ~
                    CH(1), CH(44), CH(35), CH(20), CH(2), CH(5), CH(5)

L30250:     plowkey$ = "MAGNETIC W-2 REC E"
            call "READ100" (#1, plowkey$, fs%)
                if fs% = 0% then return
            hit% = 1% /* Indicate record(s) found */
            get #1 using L30330, local69$, name_code$, employment$,       ~
                                blocking$, est_numbr$,  liability$,      ~
                                otherein$

L30330:         FMT XX(34), CH(9), XX(135), CH(1), CH(1), CH(2), CH(4),  ~
                    POS(275), CH(1), XX(1), CH(9)

            str(box13map$()) = "UUIDDDDD****UU"
            plowkey$ = "MAGNETIC W-2 REC *"
            call "READ100" (#1, plowkey$, fs%)
                if fs% = 0% then return
            hit% = 1% /* Indicate record(s) found */
            get #1 using L30400, box13map$()
L30400:         FMT XX(34), 14*CH(1)
*        Check to see if mapping is from 1990
            if str(box13map$()) <> "UI*DDDDD****  " then return

L30430:         keyhit% = 0%
                call "ASKUSER" (keyhit%, "BOX 13 DEFAULTS", "Your Current~
        ~ BOX 13 mapping (Screen 4) appears to match a past year's default~
        ~s.", "PF1 to set the mapping to the current year's defaults -OR-"~
        , "PF16 to leave the mapping as it is.")

                if keyhit%  = 16% then return
                if keyhit% <> 1% then L30430
                str(box13map$()) = "UUIDDDDD****UU"

         return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        dataput
            plowkey$ = "MAGNETIC W-2 REC A"
            call "DELETE" (#1, plowkey$, 16%)
            rec$ = "A"
            put #1 using L35040, plowkey$, rec$, pay_year$, ein$, " ",    ~
                forgn_a$, tran_name$, address$, city$, state$, " ",      ~
                zip_code$, zip_extn$, " ", " "
            write #1

            plowkey$ = "MAGNETIC W-2 REC B"
            rec$ = "B"
            put #1 using L35240, plowkey$, rec$, pay_year$, ein$,         ~
                computer$, label$, " ", density$, character$, " ",       ~
                forgn_b$, ret_name$, ret_address$, ret_city$,            ~
                ret_state$, ret_zip_code$, ret_zip_extn$, " "
            write #1

            plowkey$ = "MAGNETIC W-2 REC E"
            rec$ = "E"
            put #1 using L35440, plowkey$, rec$, pay_year$, ein$,         ~
                local69$, " ", name_code$, employment$, blocking$,       ~
                est_numbr$, " ", liability$, " ", otherein$, " "

            write #1

            plowkey$ = "MAGNETIC W-2 REC *"
            rec$ = "*"
            put #1 using L35840, plowkey$, rec$, pay_year$, ein$,         ~
                box13map$(), " ", " "

            write #1
            return

        REM *************************************************************~
            *     S Y S F I L E 2   R E C O R D   F O R M A T S         *~
            *************************************************************

L35040:     FMT   /* Transmitter record 'A' */                           ~
                CH(20),                  /* SYSFILE2 Record key        */~
                CH(1),                   /* Record type 'A' (begin SSA)*/~
                CH(4),                   /* Payment Year               */~
                CH(9),                   /* Transmitters' EIN          */~
                CH(8),                   /* Blanks                     */~
                CH(1),                   /* Foreign Address indicator  */~
                CH(50),                  /* Transmitter's name         */~
                CH(40),                  /* Transmitter's address      */~
                CH(25),                  /* Transmitter's city         */~
                CH(02),                  /* Transmitter's state        */~
                CH(8),                   /* Blanks                     */~
                CH(05),                  /* Zip/Foreign Postal code    */~
                CH(05),                  /* Zip code extension         */~
                CH(117),                 /* Filler to end of SSA record*/~
                CH(205)                  /* Filler to end of SYSFILE2  */

L35240:     FMT   /* Employer record 'B' */                              ~
                CH(20),                  /* SYSFILE2 Record key        */~
                CH(1),                   /* Record type 'B' (begin SSA)*/~
                CH(4),                   /* Payment Year               */~
                CH(9),                   /* Transmitters' EIN          */~
                CH(8),                   /* Computer Manufacturer      */~
                CH(02),                  /* Internal Labeling          */~
                CH(01),                  /* Blank                      */~
                CH(02),                  /* Tape Density               */~
                CH(03),                  /* Character Set              */~
                CH(115),                 /* Blanks                     */~
                CH(1),                   /* Foreign Address flag       */~
                CH(44),                  /* File Return Name           */~
                CH(35),                  /* File Return Address        */~
                CH(20),                  /* File Return City           */~
                CH(02),                  /* File Return State          */~
                CH(05),                  /* Foreign Postal code        */~
                CH(05),                  /* File Return Zip code       */~
                CH(223)                  /* Filler to end of SYSFILE2  */

L35440:     FMT   /* Employer record 'E' */                              ~
                CH(20),                  /* SYSFILE2 Record key        */~
                CH(1),                   /* Record type 'E' (begin SSA)*/~
                CH(4),                   /* Payment Year               */~
                CH(9),                   /* Transmitters' EIN          */~
                CH(9),                   /* State/Local 69 number      */~
                CH(135),                 /* Blank                      */~
                CH(01),                  /* Name code S or F           */~
                CH(01),                  /* Type of employment code    */~
                CH(02),                  /* Blocking factor            */~
                CH(04),                  /* Establishment number       */~
                CH(88),                  /* Blank                      */~
                CH(1),                   /* Limitation of Liability    */~
                CH(1),                   /* Blank                      */~
                CH(9),                   /* Other EIN                  */~
                CH(215)                  /* Filler to end of SYSFILE2  */

L35840:     FMT   /* Options Record  */                                  ~
                CH(20),                  /* SYSFILE2 Record key        */~
                CH(1),                   /* Record type '*'            */~
                CH(4),                   /* Payment Year               */~
                CH(9),                   /* Transmitters' EIN          */~
                14*CH(1),                /* Box 13 Map                 */~
                CH(202),                 /* Blank                      */~
                CH(250)                  /* Blank                      */

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
              on fieldnr% gosub L40130,         /* Payment Year      */   ~
                                L40130,         /* Transmitter  EIN  */   ~
                                L40125,         /* Transmitter Name  */   ~
                                L40125,         /* Street Address    */   ~
                                L40125,         /* City              */   ~
                                L40125,         /* State             */   ~
                                L40130,         /* Zip Code/Foreign  */   ~
                                L40130,         /* Zip Code extension*/   ~
                                L40125          /* Forgn Address flag*/
              goto L40140

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40125:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40130:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40140:     accept                                                       ~
               at (01,02), "Payroll Magnetic Tape - Transmitter Data - Re~
        ~cord 'A'",                                                       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Payment Year",                               ~
               at (06,30), fac(lfac$( 1)), pay_year$            , ch(04),~
                                                                         ~
               at (07,02), "Transmitter's E.I.N.",                       ~
               at (07,30), fac(lfac$( 2)), ein$                 , ch(09),~
                                                                         ~
               at (08,02), "Transmitter Name",                           ~
               at (08,30), fac(lfac$( 3)), tran_name$           , ch(50),~
                                                                         ~
               at (09,02), "Street Address",                             ~
               at (09,30), fac(lfac$( 4)), address$             , ch(40),~
                                                                         ~
               at (10,02), "City",                                       ~
               at (10,30), fac(lfac$( 5)), city$                , ch(25),~
                                                                         ~
               at (11,02), "State",                                      ~
               at (11,30), fac(lfac$( 6)), state$               , ch(02),~
                                                                         ~
               at (12,02), "ZIP or Foreign Postal Code",                 ~
               at (12,30), fac(lfac$( 7)), zip_code$            , ch(05),~
                                                                         ~
               at (13,02), "ZIP Code Extension",                         ~
               at (13,30), fac(lfac$( 8)), zip_extn$            , ch(05),~
                                                                         ~
               at (14,02), "Foreign Address Ind.",                       ~
               at (14,30), fac(lfac$( 9)), forgn_a$             , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40395
                  call "MANUAL" ("PRLW2MTP") : goto L40140

L40395:        if keyhit% <> 15 then L40410
                  call "PRNTSCRN" : goto L40140

L40410:        if edit% = 1% then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40505     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40485
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40485:     if fieldnr% > 2% then L40495
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40495:     return

L40505: if fieldnr% > 0% then L40550  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (5)Next Screen         " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffff05ffffffffffffff0dff0f1000)
            return
L40550:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'050(2%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L41200,         /* File Return Name  */   ~
                                L41200,         /* File Return Addr  */   ~
                                L41200,         /* File Return City  */   ~
                                L41200,         /* File Return State */   ~
                                L41210,         /* File Return Zip   */   ~
                                L41210,         /* Zip/Foreign exten */   ~
                                L41200,         /* Foreign Addr flag */   ~
                                L41200,         /* Computer          */   ~
                                L41200,         /* Internal labeling */   ~
                                L41210,         /* Density           */   ~
                                L41200          /* Character set     */
              goto L41230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L41200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L41210:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41230:     accept                                                       ~
               at (01,02), "Payroll Magnetic Tape - Employer Data - Recor~
        ~d 'B'",                                                          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "File Return Name",                           ~
               at (06,30), fac(lfac$( 1)), ret_name$            , ch(44),~
                                                                         ~
               at (07,02), "File Return Address",                        ~
               at (07,30), fac(lfac$( 2)), ret_address$         , ch(35),~
                                                                         ~
               at (08,02), "File Return City",                           ~
               at (08,30), fac(lfac$( 3)), ret_city$            , ch(20),~
                                                                         ~
               at (09,02), "File Return State",                          ~
               at (09,30), fac(lfac$( 4)), ret_state$           , ch(02),~
                                                                         ~
               at (10,02), "File Return Zip/Foreign",                    ~
               at (10,30), fac(lfac$( 5)), ret_zip_code$        , ch(05),~
                                                                         ~
               at (11,02), "Zip/Foreign Extension",                      ~
               at (11,30), fac(lfac$( 6)), ret_zip_extn$        , ch(05),~
                                                                         ~
               at (12,02), "Foreign Address Ind.",                       ~
               at (12,30), fac(lfac$( 7)), forgn_b$             , ch(01),~
                                                                         ~
               at (13,02), "Computer Mfr. Name",                         ~
               at (13,30), fac(lfac$( 8)), computer$            , ch(08),~
                                                                         ~
               at (14,02), "Internal Labeling",                          ~
               at (14,30), fac(lfac$( 9)), label$               , ch(02),~
                                                                         ~
               at (15,02), "Tape Density code",                          ~
               at (15,30), fac(lfac$(10)), density$             , ch(02),~
                                                                         ~
               at (16,02), "Character Set",                              ~
               at (16,30), fac(lfac$(11)), character$           , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L41590
                  call "MANUAL" ("PRLW2MTP") : goto L41230

L41590:        if keyhit% <> 15 then L41620
                  call "PRNTSCRN" : goto L41230

L41620:        if edit% = 1% then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L41810     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L41770
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L41770:     if fieldnr% > 2% then L41790
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41790:     return

L41810: if fieldnr% > 0% then L41900  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Screen     " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (5)Next Screen         " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffff0405ffffffffffffff0dff0f1000)
            return
L41900:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
              gosub'050(3%, fieldnr%)
              gosub set_pf3
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42125,         /* Liability Limitatn*/   ~
                                L42130,         /* State-Local 69 #  */   ~
                                L42125,         /* Name Code         */   ~
                                L42125,         /* Establishment Num */   ~
                                L42125,         /* Type of Employment*/   ~
                                L42130,         /* Blocking factor   */   ~
                                L42130          /* Other EIN         */
              goto L42140

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42125:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L42130:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42140:     accept                                                       ~
               at (01,02), "Payroll Magnetic Tape - Employer Data - Recor~
        ~d 'E'",                                                          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Liability Limitation Flag",                  ~
               at (06,30), fac(lfac$( 1)), liability$           , ch(01),~
                                                                         ~
               at (07,02), "State / Local 69 Number",                    ~
               at (07,30), fac(lfac$( 2)), local69$             , ch(09),~
                                                                         ~
               at (08,02), "Name Code",                                  ~
               at (08,30), fac(lfac$( 3)), name_code$           , ch(01),~
                                                                         ~
               at (09,02), "Establishment Number",                       ~
               at (09,30), fac(lfac$( 4)), est_numbr$           , ch(04),~
                                                                         ~
               at (10,02), "Type of Empl (A,H,M,Q,X,R)",                 ~
               at (10,30), fac(lfac$( 5)), employment$          , ch(01),~
                                                                         ~
               at (11,02), "Blocking Factor",                            ~
               at (11,30), fac(lfac$( 6)), blocking$            , ch(02),~
                                                                         ~
               at (12,02), "Other EIN",                                  ~
               at (12,30), fac(lfac$( 7)), otherein$            , ch(09),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L42395
                  call "MANUAL" ("PRLW2MTP") : goto L42140

L42395:        if keyhit% <> 15 then L42410
                  call "PRNTSCRN" : goto L42140

L42410:        if edit% = 1% then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
        if edit% = 2% then L42505     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L42485
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L42485:     if fieldnr% > 2% then L42495
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L42495:     return

L42505: if fieldnr% > 0% then L42550  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Screen     " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (5)Next Screen         " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffff0405ffffffffffffff0dff0f1000)
            return
L42550:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   4                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'104(fieldnr%, edit%)
              gosub'050(4%, fieldnr%)
              gosub set_pf4
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L43160,         /* Box 13 summation  */   ~
                                L43160          /*                   */
              goto L43190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L43160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L43190:     accept                                                       ~
               at (01,02), "Payroll Magnetic Tape - Options Record",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "W-2 Form Box 13 Code Map  'U' = Uncollected F~
        ~ICA, 'I' = Group-term Insurance",                                ~
               at (06,28), "'D' = Deferred Compensation, '*' = Unused",  ~
                                                                         ~
               at (07,02), "A - Uncollected FICA Tax on Tips",           ~
               at (07,50), fac(lfac$( 1)), box13map$( 1)        , ch( 1),~
                                                                         ~
               at (08,02), "B - Uncollected Medicare Tax on Tips",       ~
               at (08,50), fac(lfac$( 1)), box13map$( 2)        , ch( 1),~
                                                                         ~
               at (09,02), "C - Cost of Group-term Life Insurance",      ~
               at (09,50), fac(lfac$( 1)), box13map$( 3)        , ch( 1),~
               at (10,02), "C - Sick Pay Not Included as Income",        ~
                                                                         ~
               at (10,02), "D - Deferred Compensation - 401(k)",         ~
               at (10,50), fac(lfac$( 1)), box13map$( 4)        , ch( 1),~
                                                                         ~
               at (11,02), "E - Deferred Compensation - 403(b)",         ~
               at (11,50), fac(lfac$( 1)), box13map$( 5)        , ch( 1),~
                                                                         ~
               at (12,02), "F - Deferred Compensation - 408(k)(6)",      ~
               at (12,50), fac(lfac$( 1)), box13map$( 6)        , ch( 1),~
                                                                         ~
               at (13,02), "G - Deferred Compensation - 457",            ~
               at (13,50), fac(lfac$( 1)), box13map$( 7)        , ch( 1),~
                                                                         ~
               at (14,02), "H - Deferred Compensation - 501(c)(18)(D)",  ~
               at (14,50), fac(lfac$( 1)), box13map$( 8)        , ch( 1),~
                                                                         ~
               at (15,02), "I - * * * Unused * * *",                     ~
               at (15,50), fac(lfac$( 1)), box13map$( 9)        , ch( 1),~
                                                                         ~
               at (16,02), "J - Sick Pay Not Included as Income",        ~
               at (16,50), fac(lfac$( 1)), box13map$(10)        , ch( 1),~
                                                                         ~
               at (17,02), "K - 20% Excise tax on 'golden parachute'",   ~
               at (17,50), fac(lfac$( 1)), box13map$(11)        , ch( 1),~
                                                                         ~
               at (18,02), "L - Reimbursed Employee Business Expenses",  ~
               at (18,50), fac(lfac$( 1)), box13map$(12)        , ch( 1),~
                                                                         ~
               at (19,02), "M - Uncollected FICA Tax on Group Life Ins", ~
               at (19,50), fac(lfac$( 1)), box13map$(13)        , ch( 1),~
                                                                         ~
               at (20,02), "N - Uncollected Medicare Tax-Group Life Ins",~
               at (20,50), fac(lfac$( 1)), box13map$(14)        , ch( 1),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L43670
                  call "MANUAL" ("PRLW2MTP") : goto L43190

L43670:        if keyhit% <> 15 then L43700
                  call "PRNTSCRN" : goto L43190

L43700:        if edit% = 1% then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf4
        if edit% = 2% then L43900     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L43860
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L43860:     if fieldnr% > 2% then L43880
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L43880:     return

L43900: if fieldnr% > 0% then L43990  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Screen     " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            return
L43990:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50200,         /* Payment Year           */~
                              L50230,         /* Transmitter  EIN       */~
                              L50260,         /* Transmitter Name       */~
                              L50290,         /* Street Address         */~
                              L50320,         /* City                   */~
                              L50350,         /* State                  */~
                              L50380,         /* Forgn Postal Code      */~
                              L50410,         /* Zip Code               */~
                              L50440          /* Forgn Address flag     */
            return
L50200: REM Test for Payment Year                 PAY_YEAR$
            if pay_year$ <> " " then return
                errormsg$ = "Payment Year is Required, Please Enter Year"
                return

L50230: REM Test for Transmitter's E.I.N.         EIN$
            if ein$ <> " " then return
                errormsg$ = "E.I.N. is Required, Please Enter Number"
                return

L50260: REM Test for Transmitter Name             TRAN_NAME$
            return

L50290: REM Test for Street Address               ADDRESS$
            return

L50320: REM Test for City                         CITY$
            return

L50350: REM Test for State                        STATE$
            return

L50380: REM Test for Zip code extension           ZIP_EXTN$
            return

L50410: REM Test for Zip Code                     ZIP_CODE$
            return

L50440: REM Test for Foreign Address flag         FORGN_A$
            if forgn_a$ = " " or forgn_a$ = "X" then return
                errormsg$ = "Indicator must be blank (US address) or 'X"&~
                    "' (outside US)"
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51150,         /* File Return Name       */~
                              L51180,         /* File Return Addr       */~
                              L51210,         /* File Return City       */~
                              L51240,         /* File Return State      */~
                              L51270,         /* Zip/Foreign Postal cd  */~
                              L51300,         /* Zip/Foreign extension  */~
                              L51321,         /* Foreign Addr flag      */~
                              L51330,         /* Computer               */~
                              L51360,         /* Internal labeling      */~
                              L51390,         /* Density                */~
                              L51420          /* Character set          */
            return
L51150: REM Test for File Return Name             RET_NAME$
            return

L51180: REM Test for File Return Address          RET_ADDRESS$
            return

L51210: REM Test for File Return City             RET_CITY$
            return

L51240: REM Test for File Return State            RET_STATE$
            return

L51270: REM Test for Zip/Foreign Postal code      RET_ZIP_CODE$
            return

L51300: REM Test for File Return Zip Extension    RET_ZIP_EXTN$
            return

L51321: REM Test for Foreign Address flag         FORGN_B$
            if forgn_b$ = " " or forgn_b$ = "X" then return
                errormsg$ = "Indicator must be blank (US address) or 'X"&~
                    "' (outside US)"
                return

L51330: REM Test for Computer Mfr.                COMPUTER$
            return

L51360: REM Test for Internal Labeling            LABEL$
            if label$ = "NL" or label$ = "NS" or label$ = "SL"           ~
                        then return
                errormsg$ = "Internal Label Must Be 'NL', 'NS' or 'SL'"
                return

L51390: REM Test for Tape Density                 DENSITY$
            if density$ = "08" or density$ = "16"                        ~
                               or density$ = "62" then return
                errormsg$ = "Tape Density Must Be '08', '16' or '62'"
                return

L51420: REM Test for Character Set                CHARACTER$
            if character$ = "EBC" or character$ = "ASC" then return
                errormsg$ = "Character Set Must Be 'EBC' or 'ASC'"
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************

        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L52212,         /* Liability Limitation   */~
                              L52220,         /* State-Local 69 #       */~
                              L52250,         /* Name Code              */~
                              L52300,         /* Establishment Num      */~
                              L52330,         /* Type of Employment     */~
                              L52360,         /* Blocking factor        */~
                              L52382          /* Other EIN              */
            return
L52212: REM Test for Liability Limitation Flag    LIABILITY$
            return

L52220: REM Test for State / Local 69 Number      LOCAL69$
            return

L52250: REM Test for Name Code                    NAME_CODE$
            if name_code$ = "S" or name_code$ = "F" then return
                errormsg$ = "Enter 'S' for Surname of 'F' for First"
                return

L52300: REM Test for Establishment Number         EST_NUMBR$
            return

L52330: REM Test for Type of Employment           EMPLOYMENT$
            if employment$ = "A" or employment$ = "H" or                 ~
               employment$ = "M" or employment$ = "Q" or                 ~
               employment$ = "X" or employment$ = "R" then return
                errormsg$ = "Must Enter 'A', 'H', 'M', 'Q', 'X' or 'R'"
                return

L52360: REM Test for Blocking factor              BLOCKING$
            return

L52382: REM Test for Other EIN                    OTHEREIN$
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 4.                      *~
            *************************************************************

        deffn'154(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L53110,         /* Box 13 Codes           */~
                              L53200          /*                        */
            return

L53110: REM Test for Box 13 Codes
            for i% = 1% to 14%
                if i% = 9% then box13map$(i%) = "*"
                if box13map$(i%) = " " then box13map$(i%) = "*"
                if pos("*UID" = box13map$(i%)) <> 0% then L53170
                   errormsg$ = "'U' = Uncollected FICA, 'I' = Insurance,"
                   errormsg$ = errormsg$ & " 'D' = Deferred Compensation"
L53170:     next i%
            return

L53200: REM Test for
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
