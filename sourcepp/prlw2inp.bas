        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   RRRR   L      W   W   222   IIIII  N   N  PPPP    *~
            *  P   P  R   R  L      W   W      2    I    NN  N  P   P   *~
            *  PPPP   RRRR   L      W   W   222     I    N N N  PPPP    *~
            *  P      R   R  L      W W W  2        I    N  NN  P       *~
            *  P      R   R  LLLLL   W W   22222  IIIII  N   N  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLW2INP - GETS INFO NEEDED FOR W-2 PRINTING              *~
            *-----------------------------------------------------------*~
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
            * 12/07/84 ! ORIGINAL                                 ! HES *~
            * 11/26/91 ! PRR 12142 - Added changes for Medicare   ! JBK *~
            *          ! tax and wage base for 1991.              !     *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 11/09/92 ! Wage, tax & deduction W-2 mapping now    ! MLJ *~
            *          !  user specified also cleaned up code.    !     *~
            * 12/01/92 ! PRR 11767 - 11/09 mapping modifications. ! MLJ *~
            * 03/23/93 ! PRR 12750 - Fixed deletion of state      ! MLJ *~
            *          !  deduction methods.                      !     *~
            * 11/23/93 ! Changed box number edit checks to match  ! JBK *~
            *          !  changes for the 1993 W-2 form.  Added   !     *~
            *          !  variable to PRLW2MAP to make it appear  !     *~
            *          !  that any new or rewriten type '1'       !     *~
            *          !  record was remapped with 1993 box Nos.  !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            address1$30,                 /* ADDRESS LINE ONE           */~
            address2$30,                 /* ADDRESS LINE TWO           */~
            city$15,                     /* CITY (FOR ADDRESS)         */~
            city$(100)12,                /* CITY FOR LINE ITEMS        */~
            cmthd$(100)6,                /* LINE CITY METHOD OF DEDUCTI*/~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dedcode$(100)6,              /* DEDUCTION CODE ARRAY       */~
            ddescr$(100)30,              /* DEDUCTION CODE DESCR ARRAY */~
            descr$30,                    /* DESCRIPTION                */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            edtmap$80,                   /* TRANSLATION FOR MAP        */~
            edtsta$80,                   /* TRANSLATION FOR STATE      */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            fac$(20,5)1,                 /* FIELD ATTRIBUTE CHARACTERS */~
            fedid$10,                    /* FEDERAL EMPLOYER TAX ID NO.*/~
            fedmthd$6,                   /* FEDERAL INCOME TAX DED CODE*/~
            fedtax$2,                    /* FEDERAL TAX MAP TO BOX     */~
            fedwage$2,                   /* FEDERAL WAGE MAP TO BOX    */~
            ficamthd$6,                  /* F.I.C.A TAX DED CODE -OASDI*/~
            ficatax$2,                   /* F.I.C.A TAX MAP TO BOX     */~
            ficawage$2,                  /* F.I.C.A TAX DED CODE -OASDI*/~
            fix93$4,                     /* 1993 Box Remap Flag        */~
            fixed93$4,                   /* 1993 Box Remap Flag        */~
            high$2,                      /* HIGHEST W-2 BOX # USED     */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            infomsg$79,                  /* INFO MESSAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            line2$79,                    /* SCREEN LINE 2              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            low$2,                       /* LOWEST W-2 BOX # USED      */~
            medicaremthd$6,              /* MEDICARE TAX DED CODE      */~
            medicaretax$2,               /* MEDICARE TAX MAP TO BOX    */~
            medicarewage$2,              /* MEDICARE WAGE MAP TO BOX   */~
            name$30,                     /* EMPLOYER NAME              */~
            pf$(3)79,                    /* PFKEY SCREEN LITERALS      */~
            pfkeys$32,                   /* PFKEY HEX VALUES           */~
            plowkey$99,                  /* MISC PLOW KEY              */~
            readkey$99,                  /* MISC READ KEY              */~
            reccode$1,                   /* PRLW2MAP RECORD CODE       */~
            sdescr$(100)32,              /* STATE METHOD DESCRIPTION   */~
            search%(1),                  /* SEARCH 'TO' RESULT         */~
            state$15,                    /* STATE (FOR ADDRESS)        */~
            state$(100)2,                /* STATES WITH INCOME TAXES   */~
            stateid$(100)10,             /* EMPLOYERS STATE ID NUMBERS */~
            smthd$(100)6,                /* LINE STATE METHOD OF DEDUCT*/~
            tstbox$2,                    /* WORK FIELD FOR BOX         */~
            tstcity$6,                   /* WORK FIELD FOR CITY DED    */~
            tstcode$6,                   /* WORK FIELD FOR DEDUCTION   */~
            tststate$6,                  /* WORK FIELD FOR STATE DED   */~
            temp$6,                      /* MISC WORK VARIABLE         */~
            title$(04,2)79,              /* FUNCTION KEY DESCRIPTIONS  */~
            w2box$(100)2,                /* W-2 BOX NUMBER ARRAY       */~
            w2code$(100)1,               /* W-2 BOX CODE ARRAY (16,17) */~
            w2label$(100)12,             /* W-2 BOX LABEL (18)         */~
            zip$10                       /* ZIP CODE                   */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            fs%(64),                     /*                            */~
            rslt$(64)20                  /* TEXT FROM FILE OPENING     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 08/12/96 Last Wang Release               "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! PRLW2STA ! Payroll W-2 Cross Reference              *~
            * #2  ! PRLW2MAP ! Payroll W-2 Wage, Tax & Deduction Map    *~
            * #3  ! PRLDDT   ! System deductions control file           *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "PRLW2STA",                                      ~
                        varc,  indexed,  recsize =   50,                 ~
                        keypos =  1,   keylen =   12

            select #2,  "PRLW2MAP",                                      ~
                        varc,  indexed,  recsize = 1300,                 ~
                        keypos =  1,   keylen =   11

            select #3,  "PRLDDT",                                        ~
                        varc,  indexed,  recsize =  400,                 ~
                        keypos =    1, keylen =  6


        REM Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 100%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%), 100%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%), 0%, rslt$(3%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            str(line2$,62) = "PRLW2INP: " & str(cms2v$,,8)
            edtmessage$ = "To Modify Displayed Values, Position Cursor "&~
                          "to Desired Value and Press (ENTER)."

        REM State and Map string used for edit computations...
            init(hex(00)) edtsta$, edtmap$
            init(hex(01)) str(edtsta$,  2%), str(edtmap$,  2%)
            init(hex(02)) str(edtsta$,  5%), str(edtmap$, 10%)
            init(hex(03)) str(edtsta$, 16%), str(edtmap$, 56%)
            init(hex(04)) str(edtsta$, 56%), str(edtmap$, 60%)
            init(hex(05)) str(edtsta$, 69%), str(edtmap$, 63%)

        REM Sst W-2 Box Number Low/Hight and Unused Box Limits...
            low%   =    1%                        /* Lowest Box Used  */
            high%  =   14%                        /* Highest Box Used */
                convert low% to low$, pic(##)
                convert high% to high$, pic(##)

            title$(1%,1%) = "(1)Start Over  (2)Col. One  (4)Line Above "&~
                            "(13)Instructions  (16)Edit Mode"
            title$(2%,1%) = "(1)Start Over  (2)First  (4)Prev (6)Down O"&~
                            "NE  (11)Insert Line  (13)Instructions"
            title$(2%,2%) = "(9)Header      (3)Last   (5)Next  (7)Up On"&~
                            "e   (12)Delete Line  (15)Prt (16)Save"
            title$(3%,1%) = "Supply Requested Items and (ENTER) or (1) "&~
                            "Exit Insert Mode"
            title$(3%,2%) = "PF2 for Column One."
            title$(4%,1%) = "Press (ENTER) to Delete Flashing Line or ("&~
                            "1) to Exit Delete."

            fix93$ = "FX93"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * Employer Information and Deductions.                      *~
            *************************************************************

        inputmode
            gosub initialize_variables
            for fieldnr% = 1% to 15%
L10090:         gosub'051(fieldnr%)
                      if enabled% =  0% then L10170
L10110:         gosub'101(fieldnr%, 1%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10130
L10122:                   fieldnr% = max(1%, fieldnr% - 1%)
                          gosub'051(fieldnr%)
                          if enabled% = 1% then L10110
                          if fieldnr% = 1% then L10090
                          goto L10122
L10130:               if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then L10110
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10110
L10170:         next fieldnr%

        REM *************************************************************~
            *              I N P U T   L I N E   I T E M S              *~
            *                                                           *~
            * Deduction W-2 Box Number and Code Mapping.                *~
            *************************************************************

            maxdl%, screenline%, currentline% , line% = 0%

L10270:     screenline%      = screenline%  + 1%
            currentline%, c% = currentline% + 1%
            screen% = 1%
            if currentline% > 100% then L12000
               if screenline% <= 20% then L10350
                  screenline% = 1%
                  line% = line% + 20%

L10350:     inpmessage$ = " "
            for fieldnr% = 1% to 5%
                gosub'052(fieldnr%)
                      if enabled% =  0% then L10490
L10390:         gosub'102(fieldnr%, 1%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  2% then gosub column1_map
                      if keyhit%  =  2% then L10350
                      if keyhit%  =  4% then gosub lineabove_map
                      if keyhit%  =  4% then L10470
                      if keyhit%  = 16% and fieldnr% = 1 then L12000
                      if keyhit% <>  0% then L10390
L10470:         gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10390
L10490:         next fieldnr%

            maxdl% = maxdl% + 1%
            goto L10270

        REM *************************************************************~
            *              I N P U T   L I N E   I T E M S              *~
            *                                                           *~
            * State And Local Deductions.                               *~
            *************************************************************

            maxsl%, screenline%, currentline% , line% = 0%

L10620:     screenline%      = screenline%  + 1%
            currentline%, c% = currentline% + 1%
            screen% = 1%
            if currentline% > 100% then L12500
               if screenline% <= 20% then L10700
                  screenline% = 1%
                  line% = line% + 20%

L10700:     infomsg$ = " "
            for fieldnr% = 1% to 5%
                gosub'053(fieldnr%)
                      if enabled% =  0% then L10840
L10740:         gosub'103(fieldnr%, 1%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  2% then gosub column1_sta
                      if keyhit%  =  2% then L10700
                      if keyhit%  =  4% then gosub lineabove_sta
                      if keyhit%  =  4% then L10820
                      if keyhit%  = 16% and fieldnr% = 1 then L12500
                      if keyhit% <>  0% then L10740
L10820:         gosub'153(fieldnr%)
                      if errormsg$ <> " " then L10740
L10840:         next fieldnr%

            maxsl% = maxsl% + 1%
            goto L10620

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editmode

L11080:     gosub'101(0%, 2%)
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  3% then L12000
                if keyhit%  =  2% then L12500
                if keyhit%  = 12% then gosub deletemap
                if keyhit%  = 16% then datasave
                if keyhit% <>  0% then L11080
            if cursor%(1%)  > 12% then L11170
                fieldnr% = cursor%(1) - 5%
                if fieldnr% = 7% then L11190 else L11210
L11170:     if cursor%(1%)  > 13% then fieldnr% = cursor%(1%) - 1% else  ~
                                     fieldnr% = cursor%(1%) - 3%
L11190:         if cursor%(2%) > 50% then fieldnr% = fieldnr% + 1%
                if cursor%(2%) > 69% then fieldnr% = fieldnr% + 1%
L11210:     if fieldnr% < 1% or fieldnr% > 15% then L11080
            gosub'051(fieldnr%)
                if enabled%   =  0% then editmode
L11240:     gosub'101(fieldnr%, 2%)
                if keyhit%    =  1% then gosub startover
                if keyhit%   <>  0% then L11240
            gosub'151(fieldnr%)
                if errormsg$ <> " " then L11240
            goto L11080

L12000: REM *************************************************************~
            *               E D I T   L I N E   I T E M S               *~
            *                                                           *~
            * Payroll W-2 Deduction Mapping  - Line Items.              *~
            *************************************************************

            line%, currentline%, screenline% = 0%
            infomsg$, errormsg$= " "
L12040:     screen% = 2%
            gosub'102(0%, 2%)
                  if keyhit%  =   1% then gosub startover
                  if keyhit%  =   2% then line% = 0%
                  if keyhit%  =   3% then line% = max(0%, maxdl% - 15%)
                  if keyhit%  =   4% then line% = max(0%, line% - 15%)
                  if keyhit%  =   5% then line% = min(line% + 15%,       ~
                                                  max(0%, maxdl% - 20%))
                  if keyhit%  =   6% then line% = max(0%, line% - 1%)
                  if keyhit%  =   7% then line% = min(line% + 1%,        ~
                                                  max(0%, maxdl% - 20%))
                  if keyhit%  =   9% then editmode
                  if keyhit%  =  11% then gosub insert_mapded
                  if keyhit%  =  12% then gosub delete_mapded
                  if keyhit%  =  16% then       datasave
                  if keyhit%  <>  0% then L12040

        REM Which Field Was Selected...
            screenline% = max(0%, cursor%(1%) - 4%)
            if screenline%  = 0% then L12040
                currentline%, c% = screenline% + line%
            if currentline% > maxdl% then L12040
                fieldnr% = val(str(edtmap$, cursor%(2%)))

            if fieldnr% = 1% or fieldnr% = 3% then L12200
                if fieldnr% = 2% then L12040
            if fieldnr% <> 4% then L12185
                if w2box$(c%) = "11" or w2box$(c%) = "13" then L12200
                    goto L12040
L12185:     if w2box$(c%) = "14" then L12200
                goto L12040

L12200:     gosub'052(fieldnr%)
L12205:         screen% = 2%
            gosub'102(fieldnr%, 2%)
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then L12205
            gosub'152(fieldnr%)
                if errormsg$ <> " " then L12205

        REM Box 11 & 13 Requires an Additional Code, Box 14 Requires an  ~
            Additional Label.  Force Entry if Necessary...

            if fieldnr% <> 3% then L12040
                if temp1% = 11% then L12265
                if temp1% < 13% or temp1% > 14% then L12040
L12265:     if temp1% = 14% then L12285
                if w2code$(c%) <> " " then L12040
                    fieldnr% = fieldnr% + 1%
                    goto L12200
L12285:     if w2label$(c%) <> "  " then L12040
                fieldnr% = fieldnr% + 2%
                    goto L12200

L12500: REM *************************************************************~
            *               E D I T   L I N E   I T E M S               *~
            *                                                           *~
            * PAYROLL W-2 STATE CROSS REFERENCE - LINE ITEMS.           *~
            *************************************************************

            line%, currentline%, screenline% = 0%
            infomsg$, errormsg$= " "
L12580:     screen% = 2%
            gosub'103(0%, 2%)
                  if keyhit%  =   1% then gosub startover
                  if keyhit%  =   2% then line% = 0
                  if keyhit%  =   3% then line% = max(0%, maxsl% - 15%)
                  if keyhit%  =   4% then line% = max(0%, line% - 15%)
                  if keyhit%  =   5% then line% = min(line% + 15%,       ~
                                                  max(0%, maxsl% - 20%))
                  if keyhit%  =   6% then line% = max(0%, line% - 1%)
                  if keyhit%  =   7% then line% = min(line% + 1%,        ~
                                                  max(0%, maxsl% - 20%))
                  if keyhit%  =   9% then       editmode
                  if keyhit%  =  11% then gosub insert_sta
                  if keyhit%  =  12% then gosub delete_staded
                  if keyhit%  =  16% then       datasave
                  if keyhit%  <>  0% then L12580

        REM Which Field Was Selected...
                screenline% = max(0%, cursor%(1%) - 4%)
                if screenline%  = 0% then L12580
                currentline%, c% = screenline% + line%
                if currentline% > maxsl% then L12580
                fieldnr% = val(str(edtsta$, cursor%(2%)))
                if fieldnr% = 0% then L12580
                if fieldnr% = 5% and city$(c%) = " " then L12580

                gosub'053(fieldnr%)
L12850:         screen% = 2%
                gosub'103(fieldnr%, 2%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  0% then L12850
                gosub'153(fieldnr%)
                      if errormsg$ <> " " then L12850
                goto L12580

        REM *************************************************************~
            *        C O L U M N   O N E ,   L I N E   A B O V E        *~
            *                                                           *~
            * COLUMN ONE KEY AND LINE ABOVE KEY FUNCTIONS HANDLED HERE. *~
            *************************************************************

        column1_sta                                      /*  PRLW2STA  */
            c% = currentline%
            init(" ") state$(c%), sdescr$(c%), smthd$(c%), smthd$(c%),   ~
                infomsg$, errormsg$, stateid$(c%)
            return

        column1_map                                      /*  PRLW2MAP  */
            c% = currentline%
            init(" ") dedcode$(c%), ddescr$(c%), w2box$(c%), w2code$(c%),~
                      w2label$(c%)
            return

        lineabove_sta                                    /*  PRLW2STA  */
            if currentline% = 1% then return
            c% = currentline%
            on fieldnr% gosub L13270,     /* STATE                      */~
                              L13280,     /* STATE TAX ID NUMBER        */~
                              L13290,     /* STATE METHOD               */~
                              L13300,     /* CITY                       */~
                              L13310      /* CITY METHOD                */
            return
L13270:                       state$   (c%) = state$   (c%-1%): return
L13280:                       stateid$ (c%) = stateid$ (c%-1%): return
L13290:                       smthd$   (c%) = smthd$   (c%-1%): return
L13300:                       city$    (c%) = city$    (c%-1%): return
L13310:                       cmthd$   (c%) = cmthd$   (c%-1%): return


        lineabove_map                                    /*  PRLW2MAP  */
            if currentline% = 1% then return
            c% = currentline%
            on fieldnr% gosub L13430,     /* DEDUCTION CODE             */~
                              L13440,     /* DEDUCTION DESCRIPTION      */~
                              L13450,     /* W-2 BOX NUMBER             */~
                              L13460,     /* W-2 BOX CODE  (11,13)      */~
                              L13470      /* W-2 BOX LABEL (14)         */
            return
L13430:                       dedcode$ (c%) = dedcode$ (c%-1%): return
L13440:                       ddescr$  (c%) = ddescr$  (c%-1%): return
L13450:                       w2box$   (c%) = w2box$   (c%-1%): return
L13460:                       w2code$  (c%) = w2code$  (c%-1%): return
L13470:                       w2label$ (c%) = w2label$ (c%-1%): return

L14000: REM *************************************************************~
            *              I N S E R T   M O D E   C O D E              *~
            *                                                           *~
            * INSERTION OF A LINE ITEM INTO STA AND MAP                 *~
            *************************************************************

        insert_sta                                     /*  PRLW2STA    */
            if maxsl% = 100% then return               /*  ARRAY FULL  */
            screenline% = max(0%, cursor%(1%) - 4%)
            if line% + screenline% < maxsl% then L14110
               screenline% = maxsl% - line%            /* INS AT END   */
L14110:     if screenline% <> 20% then L14140           /* PAGE BOTTOM  */
               line% = line% + 1%
               screenline% = screenline% - 1%
L14140:     currentline%, c% = screenline% + line%

        REM Copy All Elements Up One Line...
            if c% >= maxsl% then L14270
                for temp% = maxsl% to c% step -1%
                    state$   (temp%+1%) = state$   (temp%)
                    smthd$   (temp%+1%) = smthd$   (temp%)
                    sdescr$  (temp%+1%) = sdescr$  (temp%)
                    city$    (temp%+1%) = city$    (temp%)
                    cmthd$   (temp%+1%) = cmthd$   (temp%)
                    stateid$ (temp%+1%) = stateid$ (temp%)
                next temp%

L14270:     screenline% = screenline% + 1%
            c%, currentline% = currentline% + 1%

            init(" ") state$(c%), sdescr$(c%), smthd$(c%), city$(c%),    ~
                      stateid$(c%), cmthd$(c%)

        REM Input line and Enable Canceling...
L14340:     infomsg$ = " "
            for fieldnr% = 1% to 5%
                gosub'053(fieldnr%)
                      if enabled% =  0% then L14500
L14380:         screen% = 3%
                gosub'103(fieldnr%, 2%)
                      if keyhit%  =  1% then L14530     /* END INSERT*/
                      if keyhit%  =  2% then gosub column1_sta
                      if keyhit%  =  2% then       L14340
                      if keyhit%  =  4% then gosub lineabove_sta
                      if keyhit%  =  4% then L14460
                      if keyhit% <>  0% then L14380
L14460:         gosub'153(fieldnr%)
                      if errormsg$ <> " " then L14380
            next fieldnr%

L14500:     maxsl%  = maxsl% + 1%
            goto L14000

L14530: REM Abort Insert/Delete Function...
            c% = currentline%
            if currentline% <= maxsl% then gosub L14680

            temp% = maxsl% + 1%
            init(" ") state$(temp%), sdescr$(temp%), smthd$(temp%),      ~
                      city$(temp%), stateid$(temp%), cmthd$(temp%),      ~
                      errormsg$, infomsg$

            if currentline% >= maxsl% and screenline% = 20% then         ~
                               line% = max(0%, line% - 1%)
            screen% = 2%
            init(hex(8c)) lfac$()
            return

L14680:     for temp% = currentline% to maxsl%
                state$   (temp%) = state$   (temp%+1%)
                smthd$   (temp%) = smthd$   (temp%+1%)
                sdescr$  (temp%) = sdescr$  (temp%+1%)
                city$    (temp%) = city$    (temp%+1%)
                cmthd$   (temp%) = cmthd$   (temp%+1%)
                stateid$ (temp%) = stateid$ (temp%+1%)
            next temp%
            return

L14780: insert_mapded                                  /*  PRLW2MAP    */
            if maxdl% = 100% then return               /*  ARRAY FULL  */
            screenline% = max(0%, cursor%(1%) - 4%)
            if line% + screenline% < maxdl% then L14830
               screenline% = maxdl% - line%            /* INS AT END   */
L14830:     if screenline% <> 20% then L14860           /* PAGE BOTTOM  */
               line% = line% + 1%
               screenline% = screenline% - 1%
L14860:     currentline%, c% = screenline% + line%

        REM Copy All Elements Up One Line...
            if c% >= maxdl% then L14980
                for temp% = maxdl% to c% step -1%
                    dedcode$ (temp%+1%) = dedcode$ (temp%)
                    ddescr$  (temp%+1%) = ddescr$  (temp%)
                    w2box$   (temp%+1%) = w2box$   (temp%)
                    w2code$  (temp%+1%) = w2code$  (temp%)
                    w2label$ (temp%+1%) = w2label$ (temp%)
                next temp%

L14980:     screenline% = screenline% + 1%
            c%, currentline% = currentline% + 1%

            init(" ") dedcode$(c%), ddescr$(c%), w2box$(c%), w2code$(c%),~
                      w2label$(c%)

        REM Input Line and Enable Canel Option...
L15050:     infomsg$ = " "
            for fieldnr% = 1% to 5%
                gosub'052(fieldnr%)
                      if enabled% =  0% then L15190
L15090:         screen% = 3%
                gosub'102(fieldnr%, 2%)
                      if keyhit%  =  1% then L15240         /* END INSERT*/
                      if keyhit%  =  2% then gosub column1_map
                      if keyhit%  =  2% then L15050
                      if keyhit%  =  4% then gosub lineabove_map
                      if keyhit%  =  4% then L15170
                      if keyhit% <>  0% then L15090
L15170:         gosub'152(fieldnr%)
                      if errormsg$ <> " " then L15090
L15190:     next fieldnr%

            maxdl%  = maxdl% + 1%
            goto L14780

L15240: REM Abort Insert/Delete Function...
            c% = currentline%
            if currentline% <= maxdl% then gosub L15380

            temp% = maxdl% + 1%
            init(" ") dedcode$(temp%), ddescr$(temp%), w2box$(temp%),    ~
                      w2code$(temp%), w2label$(temp%), errormsg$, infomsg$

            if currentline% >= maxdl% and screenline% = 20% then         ~
                               line% = max(0%, line% - 1%)
            screen% = 2%
            init(hex(8c)) lfac$()
            return

L15380:     for temp% = currentline% to maxdl%
                dedcode$ (temp%) = dedcode$ (temp%+1%)
                ddescr$  (temp%) = ddescr$  (temp%+1%)
                w2box$   (temp%) = w2box$   (temp%+1%)
                w2code$  (temp%) = w2code$  (temp%+1%)
                w2label$ (temp%) = w2label$ (temp%+1%)
            next temp%
            return

        REM *************************************************************~
            *              D E L E T E   M O D E   C O D E              *~
            *                                                           *~
            * DELETES A LINE FROM STA AND MAP.                          *~
            *************************************************************

        delete_staded                                   /*  PRLW2STA    */
            if maxsl% = 0% then return
                screenline% = cursor%(1%) - 4%
            if screenline% < 1% then return
                currentline%, c% = screenline% + line%
            if  currentline% > maxsl% then return

L16130:     screen% = 4%
            gosub'103(screenline%, 2%)
                  if keyhit%  =  1% then return
                  if keyhit% <>  0% then L16130

            c% = currentline%
            if currentline% < maxsl% then gosub L14530
            temp% = maxsl%
            init(" ") state$(temp%), sdescr$(temp%), smthd$(temp%),      ~
                      city$(temp%), stateid$(temp%), cmthd$(temp%)
            maxsl% = maxsl% - 1%
            keyhit% = 12%
            return

        delete_mapded                                  /*  PRLW2MAP    */
            if maxdl% = 0% then return
                screenline% = cursor%(1%) - 4%
            if screenline% < 1% then return
                currentline%, c% = screenline% + line%
            if currentline% > maxdl% then return

L16340:     screen% = 4%
            gosub'102(screenline%, 2%)
                  if keyhit%  =  1% then return
                  if keyhit% <>  0% then L16340

            c% = currentline%
            if currentline% < maxdl% then gosub L15240
            temp% = maxdl%
            init(" ") dedcode$(temp%), ddescr$(temp%), w2box$(temp%),    ~
                      w2code$(temp%), w2label$(temp%)
            maxdl% = maxdl% - 1%
            keyhit% = 12%
            return

        deletemap
L17001:     u3% = 2%
            call "ASKUSER" (u3%, "****   W A R N I N G   ****",          ~
                 "Press PF(12) to DELETE " & str(fedid$), "-OR-", "pres"&~
                 "s  PF(1) to RETURN without deleting.")
            if u3% = 12% then L17010
            if u3% = 1% then return
                goto L17001
L17010:     plowkey$ = str(fedid$) & "1"
            call "DELETE" (#2, plowkey$, 10%)
            goto inputmode

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave

        REM Save W-2 Mapping Parameters (PAYW2MAP - Record 1)...
            readkey$ = str(fedid$) & "1"
            call "READ101" (#2, readkey$, f1%(2%))
            put #2 using L30560, fedid$, "1", name$, address1$, address2$,~
                   city$, state$, zip$, fedmthd$, fedtax$, fedwage$,     ~
                   ficamthd$, ficatax$, ficawage$, medicaremthd$,        ~
                   medicaretax$, medicarewage$, dedcode$(), w2box$(),    ~
                   w2code$(), fix93$, " "
            if f1%(2%) = 1% then rewrite #2 else write #2
        REM Save W-2 Mapping Parameters (PAYW2MAP - Record 2)...
            readkey$ = str(fedid$) & "2"
            call "READ101" (#2, readkey$, f1%(2%))
            put #2 using L30790, fedid$, "2", w2label$(), " "
            if f1%(2%) = 1% then rewrite #2 else write #2

        REM Save W-2 State Cross Reference (PRLW2STA)...
            str(readkey$) = all(hex(00))
            call "READ105" (#1, readkey$, f1%(1%))
                if f1%(1%) = 0% then L19240
                    call "DELETE" (#1, readkey$, 0%)
L19240:     if maxsl% = 0% then L19330
            for l% = 1% to maxsl%
                readkey$ = str(smthd$(l%)) & str(cmthd$(l%))
                call "READ101" (#1, readkey$, f1%(1%))
                put #1 using L19300, smthd$(l%), cmthd$(l%), state$(l%),  ~
                       stateid$(l%), city$(l%), " "
L19300:         FMT CH(6), CH(6), CH(2), CH(10), CH(12), CH(14)
                if f1%(1%) = 1% then rewrite #1 else write #1
            next l%
L19330:     goto L65000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20250,         /* FED ID NUMBER    */~
                                    L20300,         /* EMPLOYER NAME    */~
                                    L20340,         /* ADDRESS LINE ONE */~
                                    L20380,         /* ADDRESS LINE TWO */~
                                    L20420,         /* CITY AND STATE   */~
                                    L20460,         /* ZIP CODE         */~
                                    L20500,         /* FED INC DED METHD*/~
                                    L20550,         /* FED TAX BOX #    */~
                                    L20590,         /* FED WAGE BOX #   */~
                                    L20630,         /* FICA DED METHOD  */~
                                    L20680,         /* FICA TAX BOX #   */~
                                    L20720,         /* FICA WAGE BOX #  */~
                                    L20760,         /* MEDICARE DED METH*/~
                                    L20810,         /* MEDICARE TAX BOX */~
                                    L20850          /* MEDICARE WAGE BOX*/
                     return

L20250: REM Default/Enable for Employer Tax ID Number...
            inpmessage$ = "Enter Employer Tax ID to Create OR Press RET"&~
                          "URN to Edit Existing."
            if edit% > 1% then enabled% = 0%
            return

L20300: REM Default/Enable for Employer Name...
            inpmessage$ = "Enter Employer Name."
            return

L20340: REM Default/Enable for Address Line One...
            inpmessage$ = "Enter Address Line One."
            return

L20380: REM Default/Enable for Address Line Two...
            inpmessage$ = "Enter Address Line Two or Leave Blank."
            return

L20420: REM Default/Enable for City and State...
            inpmessage$ = "Enter City and State."
            return

L20460: REM Default/Enable for Zip Code...
            inpmessage$ = "Enter Zip Code."
            return

L20500: REM Default/Enable for Federal Tax Deduction Method...
            inpmessage$ = "Enter Federal Income Tax Deduction Method."
            if fedmthd$ = " " then fedmthd$ = "FIT"
            return

L20550: REM Default/Enable for Federal Tax Amount W-2 Box Number...
            inpmessage$= "Enter W-2 Box Number For Federal Tax Amount."
            return

L20590: REM Default/Enable for Federal Wage Amount W-2 Box Number...
            inpmessage$= "Enter W-2 Box Number For Federal Wage Amount."
            return

L20630: REM Default/Enable for F.I.C.A Deduction Method...
            inpmessage$ = "Enter F.I.C.A. Deduction Method."
            if ficamthd$ = " " then ficamthd$ = "FICAEE"
            return

L20680: REM Default/Enable for F.I.C.A Tax Amount W-2 Box Number...
            inpmessage$= "Enter W-2 Box Number For F.I.C.A. Tax Amount."
            return

L20720: REM Default/Enable for F.I.C.A Wage Amount W-2 Box Number...
            inpmessage$= "Enter W-2 Box Number For F.I.C.A. Wage Amount."
            return

L20760: REM Default/Enable for Medicare Deduction Method...
            inpmessage$= "Enter Medicare Deduction Method."
            if medicaremthd$ = " " then medicaremthd$ = "MEDCRE"
            return

L20810: REM Default/Enable for Medicare Tax Amount W-2 Box Number...
            inpmessage$= "Enter W-2 Box Number For Medicare Tax Amount."
            return

L20850: REM Default/Enable for Medicare Wage Amount W-2 Box Number...
            inpmessage$= "Enter W-2 Box Number For Medicare Wage Amount."
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 2 OF INPUT. *~
            *************************************************************

            deffn'052(fieldnr%)
                enabled% = 1%
                on fieldnr% gosub L22150,           /* DEDUCTION CODE   */~
                                  L22190,           /* DEDUCTION DESCR  */~
                                  L22230,           /* W-2 BOX NUMBER   */~
                                  L22270,           /* W-2 BOX CODE     */~
                                  L22340            /* W-2 BOX LABEL    */
                return

L22150: REM Default/Enable for Deduction Code...
            infomsg$ = "Enter the Deduction Code."
            return

L22190: REM Default/Enable for Deduction Description...
            enabled% = 0%
            return

L22230: REM Default/Enable for Deduction W-2 Box Number...
            infomsg$ = "Enter W-2 Box Number for this deduction."
            return

L22270: REM Default/Enable for Deduction W-2 Box Code...
            if w2box$(c%) = "11" or w2box$(c%) = "13" then L22310
                enabled% = 0%
                return
L22310:     infomsg$ = "Enter W-2 Box Code for this deduction."
            return

L22340: REM Default/Enable for Deduction W-2 Box Label...
            if w2box$(c%) = "14" then L22380
                enabled% = 0%
                return
L22380:     infomsg$ = "Enter W-2 label for this deduction."
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 3 OF INPUT. *~
            *************************************************************

            deffn'053(fieldnr%)
                  enabled% = 1%
                  infomsg$ = " "
                  on fieldnr% gosub L24150,         /* STATE ABBREV     */~
                                    L24200,         /* STATE Tax Id Numb*/~
                                    L24240,         /* STATE METHOD     */~
                                    L24280,         /* CITY NAME        */~
                                    L24330          /* CITY METHOD      */
                     return
L24150: REM Default/Enable for State Abbreviation...
            infomsg$ = "Enter Official State Abbreviation if State Has "&~
                       "an Income Tax."
            return

L24200: REM Default/Enable for Employer State Tax ID Number...
            infomsg$ = "Enter Your  State Tax ID Number."
            return

L24240: REM Default/Enable for State Tax Deduction Method...
            infomsg$ = "Enter the Tax Deduction Method for This State."
            return

L24280: REM Default/Enable for City Name...
            infomsg$ = "Enter City Name, if City has an Income Tax and "&~
                       "is in This State."
            return

L24330: REM Default/Enable for City Tax Deduction Method...
            if city$(c%) <> " " then L24370
                enabled% = 0%
                return
L24370:     infomsg$ = "Enter the Tax Deduction Method for This City."
            return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables

            init(" ") errormsg$, inpmessage$, fedid$, name$, address1$,  ~
                      address2$, city$, state$, zip$, fedtax$, fedwage$, ~
                      fedmthd$, state$(), city$(), cmthd$(), smthd$(),   ~
                      stateid$(), sdescr$(), ficamthd$, ficatax$,        ~
                      ficawage$, medicaremthd$, medicaretax$,            ~
                      medicarewage$, dedcode$(), ddescr$(), w2box$(),    ~
                      w2code$(), w2label$(), tstcode$
            edit% = 0%
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
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
            *         L O A D   O R I G I N A L   D A T A               *~
            *                                                           *~
            * Loads up data.                                            *~
            *************************************************************

        dataload

        REM Load W-2 Mapping Parameters (PRLW2MAP - Record 1)...
            get #2 using L30560, fedid$, reccode$, name$, address1$,      ~
                   address2$, city$, state$, zip$, fedmthd$, fedtax$,    ~
                   fedwage$, ficamthd$, ficatax$, ficawage$,             ~
                   medicaremthd$, medicaretax$, medicarewage$,           ~
                   dedcode$(), w2box$(), w2code$(), fixed93$
        REM Load W-2 Mapping Parameters (PRLW2MAP - Record 2)...
            readkey$ = str(fedid$) & "2"
            call "READ100" (#2, readkey$, f1%(2%))
                if f1%(2%) = 0% then return
            get #2 using L30790, fedid$, reccode$, w2label$()

        REM Store Deduction Descriptions and Count Lines...
            maxdl% = 0%
            for c% = 1% to 100%
                if dedcode$(c%) = " " then L30320
                    readkey$ = str(dedcode$(c%))
                    call "READ100" (#3, readkey$, f1%(3%))
                    get #3 using L30300, ddescr$(c%)
L30300:                 FMT POS(351), CH(30)
                maxdl% = maxdl% + 1%
L30320:     next c%

        REM Load W-2 State Cross Reference (PRLW2STA)...
            maxsl% = 0%
            readkey$ = " "
L30370:     call "PLOWNEXT" (#1, readkey$, 0%, f1%(1%))
                if f1%(1%) = 0% then return
            l%, maxsl% = maxsl% + 1%
            get #1, using L30420, smthd$(l%), cmthd$(l%), state$(l%),     ~
                                 stateid$(l%), city$(l%)
L30420:     FMT CH(6), CH(6), CH(2), CH(10), CH(12)
                plowkey$ = str(smthd$(l%))
                call "READ100" (#3, plowkey$, f1%(3%))
                    get #3 using L30460, sdescr$(l%)
L30460:                 FMT POS(351), CH(30)
            goto L30370

        REM *************************************************************~
            *            F O R M A T    S T A T E M E N T S             *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

L30560: FMT                    /* FILE: PRLW2MAP - Record 1            */~
            CH(10),            /* Federal Tax ID Number                */~
            CH(1),             /* Record Code, Header = "1"            */~
            CH(30),            /* Employer Name                        */~
            CH(30),            /* Employer Address Line 1              */~
            CH(30),            /* Employer Address Line 2              */~
            CH(15),            /* Employer City Name                   */~
            CH(15),            /* Employer State Name                  */~
            CH(10),            /* Employer Zip Code                    */~
            CH(6),             /* Federal Tax Deduction Method         */~
            CH(2),             /* Federal Tax Map To                   */~
            CH(2),             /* Federal Wage Map To                  */~
            CH(6),             /* F.I.C.A. Deduction Method            */~
            CH(2),             /* F.I.C.A. Tax Map To                  */~
            CH(2),             /* F.I.C.A. Wage Map To                 */~
            CH(6),             /* Medicare Tax Deduction Method        */~
            CH(2),             /* Medicare Tax Map To                  */~
            CH(2),             /* Medicare Wage Map To                 */~
            100*CH(6),         /* Deduction Code Array                 */~
            100*CH(2),         /* W-2 Box Number Array                 */~
            100*CH(1),         /* W-2 Box Code Array                   */~
            CH(4),             /* 'FX93' Boxes Re-mapped for 1993      */~
            CH(225)            /* Filler - Unused Space                */

L30790: FMT                    /* FILE: PRLW2MAP - Record 2            */~
            CH(10),            /* Federal Tax ID Number                */~
            CH(1),             /* Record Code, Header = "1"            */~
            100*CH(12),        /* W-2 Box Label Array                  */~
            CH(89)             /* Filler - Unused Space                */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub set_pf1
              if fieldnr% = 0% then inpmessage$ = "Position Cursor and "&~
                 "Press RETURN to Modify."
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              if fieldnr% = 0% then L40150
                  lfac$(fieldnr%) = hex(81)

L40150:     accept                                                       ~
               at (01,02), "Manage Payroll W-2 Parameters"      ,        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Federal Employer Tax ID No."        ,        ~
               at (06,30), fac(lfac$( 1%)), fedid$              , ch(10),~
               at (07,02), "Employer Name"                      ,        ~
               at (07,30), fac(lfac$( 2%)), name$               , ch(30),~
               at (08,02), "Address Line One"                   ,        ~
               at (08,30), fac(lfac$( 3%)), address1$           , ch(30),~
               at (09,02), "Address Line Two"                   ,        ~
               at (09,30), fac(lfac$( 4%)), address2$           , ch(30),~
               at (10,02), "City and State"                     ,        ~
               at (10,30), fac(lfac$( 5%)), city$               , ch(15),~
               at (10,47), fac(lfac$( 5%)), state$              , ch(15),~
               at (11,02), "Zip Code"                           ,        ~
               at (11,30), fac(lfac$( 6%)), zip$                , ch(10),~
                                                                         ~
               at (12,02), "Federal Income Tax Deduction Method",        ~
               at (12,39), fac(lfac$( 7%)), fedmthd$            , ch(06),~
               at (12,47), "Tax Box"                            ,        ~
               at (12,56), fac(lfac$( 8%)), fedtax$             , ch(02),~
               at (12,60), "Wage Box"                           ,        ~
               at (12,70), fac(lfac$( 9%)), fedwage$            , ch(02),~
               at (13,02), "Employee F.I.C.A. Deduction Method" ,        ~
               at (13,39), fac(lfac$(10%)), ficamthd$           , ch(06),~
               at (13,47), "Tax Box"                            ,        ~
               at (13,56), fac(lfac$(11%)), ficatax$            , ch(02),~
               at (13,60), "Wage Box"                           ,        ~
               at (13,70), fac(lfac$(12%)), ficawage$           , ch(02),~
               at (14,02), "Employee Medicare Deduction Method" ,        ~
               at (14,39), fac(lfac$(13%)), medicaremthd$       , ch(06),~
               at (14,47), "Tax Box"                            ,        ~
               at (14,56), fac(lfac$(14%)), medicaretax$        , ch(02),~
               at (14,60), "Wage Box"                           ,        ~
               at (14,70), fac(lfac$(15%)), medicarewage$       , ch(02),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)                               ~

               if keyhit% <> 13 then L40660
                  call "MANUAL" ("PRLW2INP")
                  goto L40150

L40660:        if keyhit% <> 15 then L40700
                  call "PRNTSCRN"
                  goto L40150

L40700:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf1
            if edit% > 1% then L40850
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% > 1% then L40830
                str(pf$(2%),18%,26%) = " " :  str(pfkeys$,4%,1%) = hex(ff)
L40830:     return

L40850:     pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "(2)State Tax                     (12)Del" &        ~
                     "ete                    (15)Print Screen"
            pf$(3) = "(3)Deductions                           " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(010203ffffffffffffffff0c0dff0f1000)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
            if screen% <> 1% then L42100
                pfkeys$ = hex(000102040d0f10ffffffffffffffffffffffffff)
                goto L42290
L42100:     if screen% <> 2% then L42170
                pfkeys$ = hex(0001020304050607090b0c0d0f10ffffffffffff)
                if fieldnr% = 0% then infomsg$ = "To Modify Displayed V"&~
                    "alues, Position Cursor and Press (RETURN)."
                init(hex(86)) fac$()
                if fieldnr% = 0% then L42320
                goto L42290
L42170:     if screen% <> 3% then L42200
                pfkeys$ = hex(0001020fffffffffffffffffffffffffffffffff)
                goto L42290
L42200:     if screen% <> 4% then L42290
                pfkeys$ = hex(00010fffffffffffffffffffffffffffffffffff)
                init(hex(84)) fac$()
                for temp% = 1% to 5%
                    fac$(screenline%, temp%) = hex(94)
                next temp%
                infomsg$=" "
                goto L42320

L42290:     init(hex(84)) fac$()
            fac$(screenline%, fieldnr%) = hex(81)

L42320:     accept                                                       ~
               at (01,02), fac(hex(8c)), title$(screen%,1)      , ch(79),~
               at (02,02), fac(hex(8c)), title$(screen%,2)      , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), fac(hex(a4)), infomsg$               , ch(79),~
                                                                         ~
               at (05,02), fac(fac$( 1%,1%)),dedcode$(line%+ 1%), ch(06),~
               at (06,02), fac(fac$( 2%,1%)),dedcode$(line%+ 2%), ch(06),~
               at (07,02), fac(fac$( 3%,1%)),dedcode$(line%+ 3%), ch(06),~
               at (08,02), fac(fac$( 4%,1%)),dedcode$(line%+ 4%), ch(06),~
               at (09,02), fac(fac$( 5%,1%)),dedcode$(line%+ 5%), ch(06),~
               at (10,02), fac(fac$( 6%,1%)),dedcode$(line%+ 6%), ch(06),~
               at (11,02), fac(fac$( 7%,1%)),dedcode$(line%+ 7%), ch(06),~
               at (12,02), fac(fac$( 8%,1%)),dedcode$(line%+ 8%), ch(06),~
               at (13,02), fac(fac$( 9%,1%)),dedcode$(line%+ 9%), ch(06),~
               at (14,02), fac(fac$(10%,1%)),dedcode$(line%+10%), ch(06),~
               at (15,02), fac(fac$(11%,1%)),dedcode$(line%+11%), ch(06),~
               at (16,02), fac(fac$(12%,1%)),dedcode$(line%+12%), ch(06),~
               at (17,02), fac(fac$(13%,1%)),dedcode$(line%+13%), ch(06),~
               at (18,02), fac(fac$(14%,1%)),dedcode$(line%+14%), ch(06),~
               at (19,02), fac(fac$(15%,1%)),dedcode$(line%+15%), ch(06),~
               at (20,02), fac(fac$(16%,1%)),dedcode$(line%+16%), ch(06),~
               at (21,02), fac(fac$(17%,1%)),dedcode$(line%+17%), ch(06),~
               at (22,02), fac(fac$(18%,1%)),dedcode$(line%+18%), ch(06),~
               at (23,02), fac(fac$(19%,1%)),dedcode$(line%+19%), ch(06),~
               at (24,02), fac(fac$(20%,1%)),dedcode$(line%+20%), ch(06),~
                                                                         ~
               at (05,10), fac(fac$( 1%,2%)), ddescr$(line%+ 1%), ch(30),~
               at (06,10), fac(fac$( 2%,2%)), ddescr$(line%+ 2%), ch(30),~
               at (07,10), fac(fac$( 3%,2%)), ddescr$(line%+ 3%), ch(30),~
               at (08,10), fac(fac$( 4%,2%)), ddescr$(line%+ 4%), ch(30),~
               at (09,10), fac(fac$( 5%,2%)), ddescr$(line%+ 5%), ch(30),~
               at (10,10), fac(fac$( 6%,2%)), ddescr$(line%+ 6%), ch(30),~
               at (11,10), fac(fac$( 7%,2%)), ddescr$(line%+ 7%), ch(30),~
               at (12,10), fac(fac$( 8%,2%)), ddescr$(line%+ 8%), ch(30),~
               at (13,10), fac(fac$( 9%,2%)), ddescr$(line%+ 9%), ch(30),~
               at (14,10), fac(fac$(10%,2%)), ddescr$(line%+10%), ch(30),~
               at (15,10), fac(fac$(11%,2%)), ddescr$(line%+11%), ch(30),~
               at (16,10), fac(fac$(12%,2%)), ddescr$(line%+12%), ch(30),~
               at (17,10), fac(fac$(13%,2%)), ddescr$(line%+13%), ch(30),~
               at (18,10), fac(fac$(14%,2%)), ddescr$(line%+14%), ch(30),~
               at (19,10), fac(fac$(15%,2%)), ddescr$(line%+15%), ch(30),~
               at (20,10), fac(fac$(16%,2%)), ddescr$(line%+16%), ch(30),~
               at (21,10), fac(fac$(17%,2%)), ddescr$(line%+17%), ch(30),~
               at (22,10), fac(fac$(18%,2%)), ddescr$(line%+18%), ch(30),~
               at (23,10), fac(fac$(19%,2%)), ddescr$(line%+19%), ch(30),~
               at (24,10), fac(fac$(20%,2%)), ddescr$(line%+20%), ch(30),~
                                                                         ~
               at (05,56), fac(fac$( 1%,3%)),  w2box$(line%+ 1%), ch(02),~
               at (06,56), fac(fac$( 2%,3%)),  w2box$(line%+ 2%), ch(02),~
               at (07,56), fac(fac$( 3%,3%)),  w2box$(line%+ 3%), ch(02),~
               at (08,56), fac(fac$( 4%,3%)),  w2box$(line%+ 4%), ch(02),~
               at (09,56), fac(fac$( 5%,3%)),  w2box$(line%+ 5%), ch(02),~
               at (10,56), fac(fac$( 6%,3%)),  w2box$(line%+ 6%), ch(02),~
               at (11,56), fac(fac$( 7%,3%)),  w2box$(line%+ 7%), ch(02),~
               at (12,56), fac(fac$( 8%,3%)),  w2box$(line%+ 8%), ch(02),~
               at (13,56), fac(fac$( 9%,3%)),  w2box$(line%+ 9%), ch(02),~
               at (14,56), fac(fac$(10%,3%)),  w2box$(line%+10%), ch(02),~
               at (15,56), fac(fac$(11%,3%)),  w2box$(line%+11%), ch(02),~
               at (16,56), fac(fac$(12%,3%)),  w2box$(line%+12%), ch(02),~
               at (17,56), fac(fac$(13%,3%)),  w2box$(line%+13%), ch(02),~
               at (18,56), fac(fac$(14%,3%)),  w2box$(line%+14%), ch(02),~
               at (19,56), fac(fac$(15%,3%)),  w2box$(line%+15%), ch(02),~
               at (20,56), fac(fac$(16%,3%)),  w2box$(line%+16%), ch(02),~
               at (21,56), fac(fac$(17%,3%)),  w2box$(line%+17%), ch(02),~
               at (22,56), fac(fac$(18%,3%)),  w2box$(line%+18%), ch(02),~
               at (23,56), fac(fac$(19%,3%)),  w2box$(line%+19%), ch(02),~
               at (24,56), fac(fac$(20%,3%)),  w2box$(line%+20%), ch(02),~
                                                                         ~
               at (05,60), fac(fac$( 1%,4%)), w2code$(line%+ 1%), ch(02),~
               at (06,60), fac(fac$( 2%,4%)), w2code$(line%+ 2%), ch(02),~
               at (07,60), fac(fac$( 3%,4%)), w2code$(line%+ 3%), ch(02),~
               at (08,60), fac(fac$( 4%,4%)), w2code$(line%+ 4%), ch(02),~
               at (09,60), fac(fac$( 5%,4%)), w2code$(line%+ 5%), ch(02),~
               at (10,60), fac(fac$( 6%,4%)), w2code$(line%+ 6%), ch(02),~
               at (11,60), fac(fac$( 7%,4%)), w2code$(line%+ 7%), ch(02),~
               at (12,60), fac(fac$( 8%,4%)), w2code$(line%+ 8%), ch(02),~
               at (13,60), fac(fac$( 9%,4%)), w2code$(line%+ 9%), ch(02),~
               at (14,60), fac(fac$(10%,4%)), w2code$(line%+10%), ch(02),~
               at (15,60), fac(fac$(11%,4%)), w2code$(line%+11%), ch(02),~
               at (16,60), fac(fac$(12%,4%)), w2code$(line%+12%), ch(02),~
               at (17,60), fac(fac$(13%,4%)), w2code$(line%+13%), ch(02),~
               at (18,60), fac(fac$(14%,4%)), w2code$(line%+14%), ch(02),~
               at (19,60), fac(fac$(15%,4%)), w2code$(line%+15%), ch(02),~
               at (20,60), fac(fac$(16%,4%)), w2code$(line%+16%), ch(02),~
               at (21,60), fac(fac$(17%,4%)), w2code$(line%+17%), ch(02),~
               at (22,60), fac(fac$(18%,4%)), w2code$(line%+18%), ch(02),~
               at (23,60), fac(fac$(19%,4%)), w2code$(line%+19%), ch(02),~
               at (24,60), fac(fac$(20%,4%)), w2code$(line%+20%), ch(02),~
                                                                         ~
               at (05,63), fac(fac$( 1%,5%)),w2label$(line%+ 1%), ch(12),~
               at (06,63), fac(fac$( 2%,5%)),w2label$(line%+ 2%), ch(12),~
               at (07,63), fac(fac$( 3%,5%)),w2label$(line%+ 3%), ch(12),~
               at (08,63), fac(fac$( 4%,5%)),w2label$(line%+ 4%), ch(12),~
               at (09,63), fac(fac$( 5%,5%)),w2label$(line%+ 5%), ch(12),~
               at (10,63), fac(fac$( 6%,5%)),w2label$(line%+ 6%), ch(12),~
               at (11,63), fac(fac$( 7%,5%)),w2label$(line%+ 7%), ch(12),~
               at (12,63), fac(fac$( 8%,5%)),w2label$(line%+ 8%), ch(12),~
               at (13,63), fac(fac$( 9%,5%)),w2label$(line%+ 9%), ch(12),~
               at (14,63), fac(fac$(10%,5%)),w2label$(line%+10%), ch(12),~
               at (15,63), fac(fac$(11%,5%)),w2label$(line%+11%), ch(12),~
               at (16,63), fac(fac$(12%,5%)),w2label$(line%+12%), ch(12),~
               at (17,63), fac(fac$(13%,5%)),w2label$(line%+13%), ch(12),~
               at (18,63), fac(fac$(14%,5%)),w2label$(line%+14%), ch(12),~
               at (19,63), fac(fac$(15%,5%)),w2label$(line%+15%), ch(12),~
               at (20,63), fac(fac$(16%,5%)),w2label$(line%+16%), ch(12),~
               at (21,63), fac(fac$(17%,5%)),w2label$(line%+17%), ch(12),~
               at (22,63), fac(fac$(18%,5%)),w2label$(line%+18%), ch(12),~
               at (23,63), fac(fac$(19%,5%)),w2label$(line%+19%), ch(12),~
               at (24,63), fac(fac$(20%,5%)),w2label$(line%+20%), ch(12),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if screen% <> 1 then L43490
               if str(infomsg$, 1, 7) <> "To EDIT" then                  ~
                   init(" ") infomsg$

L43490:        if keyhit% <> 13 then L43530
                   call "MANUAL" ("PRLW2INP")
                   goto L42320

L43530:        if keyhit% <> 15 then L43570
                   call "PRNTSCRN"
                   goto L42320

L43570:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
            if screen% <> 1% then L44100
                pfkeys$ = hex(000102040d0f10ffffffffffffffffffffffffff)
                goto L44290
L44100:     if screen% <> 2% then L44170
                pfkeys$ = hex(0001020304050607090b0c0d0f10ffffffffffff)
                if fieldnr% = 0% then infomsg$ = "To Modify Displayed V"&~
                    "alues, Position Cursor and Press (RETURN)."
                init(hex(86)) fac$()
                if fieldnr% = 0% then L44320
                goto L44290
L44170:     if screen% <> 3% then L44200
                pfkeys$ = hex(0001020fffffffffffffffffffffffffffffffff)
                goto L44290
L44200:     if screen% <> 4% then L44290
                pfkeys$ = hex(00010fffffffffffffffffffffffffffffffffff)
                init(hex(84)) fac$()
                for temp% = 1% to 5%
                    fac$(screenline%, temp%) = hex(94)
                next temp%
                infomsg$=" "
                goto L44320

L44290:     init(hex(84)) fac$()
            fac$(screenline%, fieldnr%) = hex(81)

L44320:     accept                                                       ~
               at (01,02), fac(hex(8c)), title$(screen%,1%)     , ch(79),~
               at (02,02), fac(hex(8c)), title$(screen%,2%)     , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), fac(hex(a4)), infomsg$               , ch(79),~
                                                                         ~
               at (05,02), fac(fac$( 1%,1%)), state$ (line%+ 1%), ch(02),~
               at (06,02), fac(fac$( 2%,1%)), state$ (line%+ 2%), ch(02),~
               at (07,02), fac(fac$( 3%,1%)), state$ (line%+ 3%), ch(02),~
               at (08,02), fac(fac$( 4%,1%)), state$ (line%+ 4%), ch(02),~
               at (09,02), fac(fac$( 5%,1%)), state$ (line%+ 5%), ch(02),~
               at (10,02), fac(fac$( 6%,1%)), state$ (line%+ 6%), ch(02),~
               at (11,02), fac(fac$( 7%,1%)), state$ (line%+ 7%), ch(02),~
               at (12,02), fac(fac$( 8%,1%)), state$ (line%+ 8%), ch(02),~
               at (13,02), fac(fac$( 9%,1%)), state$ (line%+ 9%), ch(02),~
               at (14,02), fac(fac$(10%,1%)), state$ (line%+10%), ch(02),~
               at (15,02), fac(fac$(11%,1%)), state$ (line%+11%), ch(02),~
               at (16,02), fac(fac$(12%,1%)), state$ (line%+12%), ch(02),~
               at (17,02), fac(fac$(13%,1%)), state$ (line%+13%), ch(02),~
               at (18,02), fac(fac$(14%,1%)), state$ (line%+14%), ch(02),~
               at (19,02), fac(fac$(15%,1%)), state$ (line%+15%), ch(02),~
               at (20,02), fac(fac$(16%,1%)), state$ (line%+16%), ch(02),~
               at (21,02), fac(fac$(17%,1%)), state$ (line%+17%), ch(02),~
               at (22,02), fac(fac$(18%,1%)), state$ (line%+18%), ch(02),~
               at (23,02), fac(fac$(19%,1%)), state$ (line%+19%), ch(02),~
               at (24,02), fac(fac$(20%,1%)), state$ (line%+20%), ch(02),~
                                                                         ~
               at (05,05), fac(fac$( 1%,2%)),stateid$(line%+ 1%), ch(10),~
               at (06,05), fac(fac$( 2%,2%)),stateid$(line%+ 2%), ch(10),~
               at (07,05), fac(fac$( 3%,2%)),stateid$(line%+ 3%), ch(10),~
               at (08,05), fac(fac$( 4%,2%)),stateid$(line%+ 4%), ch(10),~
               at (09,05), fac(fac$( 5%,2%)),stateid$(line%+ 5%), ch(10),~
               at (10,05), fac(fac$( 6%,2%)),stateid$(line%+ 6%), ch(10),~
               at (11,05), fac(fac$( 7%,2%)),stateid$(line%+ 7%), ch(10),~
               at (12,05), fac(fac$( 8%,2%)),stateid$(line%+ 8%), ch(10),~
               at (13,05), fac(fac$( 9%,2%)),stateid$(line%+ 9%), ch(10),~
               at (14,05), fac(fac$(10%,2%)),stateid$(line%+10%), ch(10),~
               at (15,05), fac(fac$(11%,2%)),stateid$(line%+11%), ch(10),~
               at (16,05), fac(fac$(12%,2%)),stateid$(line%+12%), ch(10),~
               at (17,05), fac(fac$(13%,2%)),stateid$(line%+13%), ch(10),~
               at (18,05), fac(fac$(14%,2%)),stateid$(line%+14%), ch(10),~
               at (19,05), fac(fac$(15%,2%)),stateid$(line%+15%), ch(10),~
               at (20,05), fac(fac$(16%,2%)),stateid$(line%+16%), ch(10),~
               at (21,05), fac(fac$(17%,2%)),stateid$(line%+17%), ch(10),~
               at (22,05), fac(fac$(18%,2%)),stateid$(line%+18%), ch(10),~
               at (23,05), fac(fac$(19%,2%)),stateid$(line%+19%), ch(10),~
               at (24,05), fac(fac$(20%,2%)),stateid$(line%+20%), ch(10),~
                                                                         ~
               at (05,16), fac(fac$( 1%,3%)), smthd$ (line%+ 1%), ch(06),~
               at (06,16), fac(fac$( 2%,3%)), smthd$ (line%+ 2%), ch(06),~
               at (07,16), fac(fac$( 3%,3%)), smthd$ (line%+ 3%), ch(06),~
               at (08,16), fac(fac$( 4%,3%)), smthd$ (line%+ 4%), ch(06),~
               at (09,16), fac(fac$( 5%,3%)), smthd$ (line%+ 5%), ch(06),~
               at (10,16), fac(fac$( 6%,3%)), smthd$ (line%+ 6%), ch(06),~
               at (11,16), fac(fac$( 7%,3%)), smthd$ (line%+ 7%), ch(06),~
               at (12,16), fac(fac$( 8%,3%)), smthd$ (line%+ 8%), ch(06),~
               at (13,16), fac(fac$( 9%,3%)), smthd$ (line%+ 9%), ch(06),~
               at (14,16), fac(fac$(10%,3%)), smthd$ (line%+10%), ch(06),~
               at (15,16), fac(fac$(11%,3%)), smthd$ (line%+11%), ch(06),~
               at (16,16), fac(fac$(12%,3%)), smthd$ (line%+12%), ch(06),~
               at (17,16), fac(fac$(13%,3%)), smthd$ (line%+13%), ch(06),~
               at (18,16), fac(fac$(14%,3%)), smthd$ (line%+14%), ch(06),~
               at (19,16), fac(fac$(15%,3%)), smthd$ (line%+15%), ch(06),~
               at (20,16), fac(fac$(16%,3%)), smthd$ (line%+16%), ch(06),~
               at (21,16), fac(fac$(17%,3%)), smthd$ (line%+17%), ch(06),~
               at (22,16), fac(fac$(18%,3%)), smthd$ (line%+18%), ch(06),~
               at (23,16), fac(fac$(19%,3%)), smthd$ (line%+19%), ch(06),~
               at (24,16), fac(fac$(20%,3%)), smthd$ (line%+20%), ch(06),~
                                                                         ~
               at (05,23), fac(hex(8c)),    sdescr$  (line%+ 1%), ch(32),~
               at (06,23), fac(hex(8c)),    sdescr$  (line%+ 2%), ch(32),~
               at (07,23), fac(hex(8c)),    sdescr$  (line%+ 3%), ch(32),~
               at (08,23), fac(hex(8c)),    sdescr$  (line%+ 4%), ch(32),~
               at (09,23), fac(hex(8c)),    sdescr$  (line%+ 5%), ch(32),~
               at (10,23), fac(hex(8c)),    sdescr$  (line%+ 6%), ch(32),~
               at (11,23), fac(hex(8c)),    sdescr$  (line%+ 7%), ch(32),~
               at (12,23), fac(hex(8c)),    sdescr$  (line%+ 8%), ch(32),~
               at (13,23), fac(hex(8c)),    sdescr$  (line%+ 9%), ch(32),~
               at (14,23), fac(hex(8c)),    sdescr$  (line%+10%), ch(32),~
               at (15,23), fac(hex(8c)),    sdescr$  (line%+11%), ch(32),~
               at (16,23), fac(hex(8c)),    sdescr$  (line%+12%), ch(32),~
               at (17,23), fac(hex(8c)),    sdescr$  (line%+13%), ch(32),~
               at (18,23), fac(hex(8c)),    sdescr$  (line%+14%), ch(32),~
               at (19,23), fac(hex(8c)),    sdescr$  (line%+15%), ch(32),~
               at (20,23), fac(hex(8c)),    sdescr$  (line%+16%), ch(32),~
               at (21,23), fac(hex(8c)),    sdescr$  (line%+17%), ch(32),~
               at (22,23), fac(hex(8c)),    sdescr$  (line%+18%), ch(32),~
               at (23,23), fac(hex(8c)),    sdescr$  (line%+19%), ch(32),~
               at (24,23), fac(hex(8c)),    sdescr$  (line%+20%), ch(32),~
                                                                         ~
               at (05,56), fac(fac$( 1%,4%)), city$  (line%+ 1%), ch(12),~
               at (06,56), fac(fac$( 2%,4%)), city$  (line%+ 2%), ch(12),~
               at (07,56), fac(fac$( 3%,4%)), city$  (line%+ 3%), ch(12),~
               at (08,56), fac(fac$( 4%,4%)), city$  (line%+ 4%), ch(12),~
               at (09,56), fac(fac$( 5%,4%)), city$  (line%+ 5%), ch(12),~
               at (10,56), fac(fac$( 6%,4%)), city$  (line%+ 6%), ch(12),~
               at (11,56), fac(fac$( 7%,4%)), city$  (line%+ 7%), ch(12),~
               at (12,56), fac(fac$( 8%,4%)), city$  (line%+ 8%), ch(12),~
               at (13,56), fac(fac$( 9%,4%)), city$  (line%+ 9%), ch(12),~
               at (14,56), fac(fac$(10%,4%)), city$  (line%+10%), ch(12),~
               at (15,56), fac(fac$(11%,4%)), city$  (line%+11%), ch(12),~
               at (16,56), fac(fac$(12%,4%)), city$  (line%+12%), ch(12),~
               at (17,56), fac(fac$(13%,4%)), city$  (line%+13%), ch(12),~
               at (18,56), fac(fac$(14%,4%)), city$  (line%+14%), ch(12),~
               at (19,56), fac(fac$(15%,4%)), city$  (line%+15%), ch(12),~
               at (20,56), fac(fac$(16%,4%)), city$  (line%+16%), ch(12),~
               at (21,56), fac(fac$(17%,4%)), city$  (line%+17%), ch(12),~
               at (22,56), fac(fac$(18%,4%)), city$  (line%+18%), ch(12),~
               at (23,56), fac(fac$(19%,4%)), city$  (line%+19%), ch(12),~
               at (24,56), fac(fac$(20%,4%)), city$  (line%+20%), ch(12),~
                                                                         ~
               at (05,69), fac(fac$( 1%,5%)), cmthd$ (line%+ 1%), ch(06),~
               at (06,69), fac(fac$( 2%,5%)), cmthd$ (line%+ 2%), ch(06),~
               at (07,69), fac(fac$( 3%,5%)), cmthd$ (line%+ 3%), ch(06),~
               at (08,69), fac(fac$( 4%,5%)), cmthd$ (line%+ 4%), ch(06),~
               at (09,69), fac(fac$( 5%,5%)), cmthd$ (line%+ 5%), ch(06),~
               at (10,69), fac(fac$( 6%,5%)), cmthd$ (line%+ 6%), ch(06),~
               at (11,69), fac(fac$( 7%,5%)), cmthd$ (line%+ 7%), ch(06),~
               at (12,69), fac(fac$( 8%,5%)), cmthd$ (line%+ 8%), ch(06),~
               at (13,69), fac(fac$( 9%,5%)), cmthd$ (line%+ 9%), ch(06),~
               at (14,69), fac(fac$(10%,5%)), cmthd$ (line%+10%), ch(06),~
               at (15,69), fac(fac$(11%,5%)), cmthd$ (line%+11%), ch(06),~
               at (16,69), fac(fac$(12%,5%)), cmthd$ (line%+12%), ch(06),~
               at (17,69), fac(fac$(13%,5%)), cmthd$ (line%+13%), ch(06),~
               at (18,69), fac(fac$(14%,5%)), cmthd$ (line%+14%), ch(06),~
               at (19,69), fac(fac$(15%,5%)), cmthd$ (line%+15%), ch(06),~
               at (20,69), fac(fac$(16%,5%)), cmthd$ (line%+16%), ch(06),~
               at (21,69), fac(fac$(17%,5%)), cmthd$ (line%+17%), ch(06),~
               at (22,69), fac(fac$(18%,5%)), cmthd$ (line%+18%), ch(06),~
               at (23,69), fac(fac$(19%,5%)), cmthd$ (line%+19%), ch(06),~
               at (24,69), fac(fac$(20%,5%)), cmthd$ (line%+20%), ch(06),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

              if screen% <> 1% then L45690
              if str(infomsg$,1%,7%) <> "To EDIT" then init(" ") infomsg$

L45690:        if keyhit% <> 13% then L45730
                  call "MANUAL" ("PRLW2INP")
                  goto L44320

L45730:        if keyhit% <> 15% then L45770
                  call "PRNTSCRN"
                  goto L44320

L45770:        if screen% <> 2% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50250,         /* FED ID NUMBER    */~
                                    L50430,         /* EMPLOYER NAME    */~
                                    L50470,         /* ADDRESS LINE ONE */~
                                    L50520,         /* ADDRESS LINE TWO */~
                                    L50550,         /* CITY AND STATE   */~
                                    L50600,         /* ZIP CODE         */~
                                    L50640,         /* FED INC DED METHD*/~
                                    L50900,         /* FED TAX MAP TO   */~
                                    L51050,         /* FED WAGE MAP TO  */~
                                    L51200,         /* FICA DED METH    */~
                                    L51480,         /* FICA TAX MAP TO  */~
                                    L51620,         /* FICA WAGE MAP TO */~
                                    L51760,         /* MEDICARE DED METH*/~
                                    L52040,         /* MEDICARE TAX MAP */~
                                    L52180          /* MEDICARE WAGE MAP*/
                     return

L50250: REM Test Data for Employer Tax ID Number...
            if new% = 1% then L50380
                if fedid$ = "?" then fedid$ = " "
            init(hex(00)) plowkey$
            call "READ102" (#2, plowkey$, f1%(2%))
                if f1%(2%) = 0% then L50330
                    gosub dataload
                    return clear all
                    goto editmode
L50330:     new% = 1%
            if fedid$ <> " " then L50380
                 errormsg$ = "No Deduction Map exists, please enter Emp"&~
                             "loyer ID to be created"
                 return
L50380:     if str(fedid$,3%,1%) = "-" then return
                 errormsg$ = "Third position of Tax ID MUST be a dash"
                 return

L50430: REM Test Data for Employer Name...
            if name$ = " " then errormsg$ = "Name CANNOT be Blank"
            return

L50470: REM Test Data for Address Line One...
            if address1$ = " " then errormsg$ = "Address Line One CANNO"&~
                           "T be Blank"
            return

L50520: REM Test Data for Address Line Two...
            return

L50550: REM Test Data for City and State...
            if city$ = " " then errormsg$ = "City CANNOT be Blank"
            if state$ = " " then errormsg$ = "State CANNOT be Blank"
            return

L50600: REM Test Data for Zip Code...
            if zip$ = " " then errormsg$ = "Zip Code CANNOT be Blank"
            return

L50640: REM Test Data for Federal Tax Deduction Method...
            descr$ = hex(06) & " "
            call "GETCODE" (#3, fedmthd$, descr$, 0%, .12, f1%(3%))
                if fedmthd$ = " " then errormsg$ = "Federal Income Tax "&~
                              "Deduction Method CANNOT be Blank"
                    if errormsg$ <> " " then return
                if f1%(3%) = 0% then errormsg$ = "Deduction Method NOT "&~
                              "found, please re-enter"
                    if errormsg$ <> " " then return
            get #3, using L50740, temp$
L50740:         FMT XX(18), CH(1)
            if temp$ = "Y" then L50840
                 errormsg$ = "Federal Income Tax Deduction must be an E"&~
                             "MPLOYEE paid deduction"
                 return
L50840:     tstcode$ = str(fedmthd$)
                fedmthd$ = " "
                gosub test_existing_ded
                fedmthd$ = str(tstcode$)
            return

L50900: REM Test Data for Federal Tax Amount W-2 Box Number...
            if fedtax$  <> " " then L50940
                errormsg$ = "Federal Tax Box CANNOT be Blank"
                return
L50940:     convert fedtax$ to temp1%, data goto L50960
            if temp1% >= low% and temp1% <= high% then L50990
L50960:         errormsg$ = "Federal Tax Box MUST be between " & low$   &~
                            " and " & high$
                return
L50990:     tstbox$ =  str(fedtax$)
            gosub test_notused
                if errormsg$ <> " " then return
            call "STRING" addr("RJ", fedtax$, 2%)
            return

L51050: REM Test Data for Federal Wage Amount W-2 Box Number...
            if fedwage$  <> " " then L51090
                errormsg$ = "Federal Wage Box CANNOT be Blank"
                return
L51090:     convert fedwage$ to temp1%, data goto L51110
            if temp1% >= low% and temp1% <= high% then L51140
L51110:         errormsg$ = "Federal Wage Box MUST be between " & low$  &~
                            " and " & high$
                return
L51140:     tstbox$ = str(fedwage$)
            gosub test_notused
                if errormsg$ <> " " then return
            call "STRING" addr("RJ", fedwage$, 2%)
            return

L51200: REM Test Data for F.I.C.A. Deduction Method...
            descr$ = hex(06) & " "
            call "GETCODE" (#3, ficamthd$, descr$, 0%, 0, f1%(3%))
                if ficamthd$ = " " then errormsg$ = "F.I.C.A. Deduction"&~
                               " Method CANNOT be Blank"
                    if errormsg$ <> " " then return
                if f1%(3%) = 0% then errormsg$ = "Deduction Method NOT "&~
                                "found, please re-enter"
                    if errormsg$ <> " " then return
            get #3, using L51300, temp$
L51300:         FMT XX(18), CH(1)
            if temp$ = "Y" then L51350
                 errormsg$ = "F.I.C.A. Deduction Method must be an EMPL"&~
                             "OYEE paid deduction"
                 return
L51350:     gosub fica_code_check
                if errormsg$ <> " " then return
            tstcode$ = str(ficamthd$)
                ficamthd$ = " "
                gosub test_existing_ded
                ficamthd$ = str(tstcode$)
            return

L51480: REM Test Data for F.I.C.A. Tax Amount W-2 Box Number...
            if ficatax$ <> " " then L51510
                errormsg$ = "F.I.C.A. Tax Box To CANNOT be Blank"
L51510:     convert ficatax$ to temp1%, data goto L51530
            if temp1% >= low% and temp1% <= high% then L51560
L51530:         errormsg$ = "F.I.C.A. Tax Box MUST be Between " & low$  &~
                            " and " & high$
                return
L51560:     tstbox$ = str(ficatax$)
            gosub test_notused
                if errormsg$ <> " " then return
            call "STRING" addr("RJ", ficatax$, 2%)
            return

L51620: REM Test Data for F.I.C.A. Wage Amount W-2 Box Number...
            if ficawage$ <> " " then L51650
                errormsg$ = "F.I.C.A. Wage Map Box CANNOT be Blank"
L51650:     convert ficawage$ to temp1%, data goto L51670
            if temp1% >= low% and temp1% <= high% then L51700
L51670:         errormsg$ = "F.I.C.A. Wage Box MUST be Between " & low$ &~
                            " and " & high$
                return
L51700:     tstbox$ = str(ficawage$)
            gosub test_notused
              if errormsg$ <> " " then return
            call "STRING" addr("RJ", ficawage$, 2%)
            return

L51760: REM Test Data For Medicare Deduction Method...
            descr$ = hex(06) & " "
            call "GETCODE" (#3, medicaremthd$, descr$, 0%, 0, f1%(3%))
                if medicaremthd$ = " " then errormsg$ = "Medicare Deduc"&~
                                   "tion Method CANNOT be Blank"
                    if errormsg$ <> " " then return
                if f1%(3%) = 0% then errormsg$ = "Deduction Method NOT "&~
                                "found, please re-enter"
                    if errormsg$ <> " " then return
            get #3, using L51860, temp$
L51860:         FMT XX(18), CH(1)
            if temp$ = "Y" then L51910
                errormsg$ = "Medicare Deduction Method must be an EMPLO"&~
                            "YEE paid deduction"
                return
L51910:     gosub fica_code_check
                if errormsg$ <> " " then return
            tstcode$ = str(medicaremthd$)
                medicaremthd$ = " "
                gosub test_existing_ded
                medicaremthd$ = str(tstcode$)
            return

L52040: REM Test Data For Medicare Tax Amount W-2 Box Number...
            if medicaretax$ <> " " then L52070
                errormsg$ = "Medicare Tax Box CANNOT be Blank"
L52070:     convert medicaretax$ to temp1%, data goto L52090
            if temp1% >= low% and temp1% <= high% then L52120
L52090:         errormsg$ = "Medicare Tax Box MUST be between " & low$  &~
                            " and " &  high$
                return
L52120:     tstbox$ = str(medicaretax$)
            gosub test_notused
                if errormsg$ <> " " then return
            call "STRING" addr("RJ", medicaretax$, 2%)
            return

L52180: REM Test Data For Medicare Wage Amount W-2 Box Number...
            if medicarewage$ <> " " then L52210
                errormsg$ = "Medicare Wage Box CANOT be Blank"
L52210:     convert medicarewage$ to temp1%, data goto L52230
            if temp1% >= low% and temp1% <= high% then L52260
L52230:         errormsg$ = "Medicare Wage Box MUST be between " & low$ &~
                            " and " & high$
                return
L52260:     tstbox$ = str(medicarewage$)
            gosub test_notused
                if errormsg$ <> " " then return
            call "STRING" addr("RJ", medicarewage$, 2%)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                on fieldnr% gosub L53150,           /* DEDUCTION CODE   */~
                                  L53350,           /* DEDUCTION DESCR  */~
                                  L53380,           /* W-2 BOX NUMBER   */~
                                  L53570,           /* W-2 BOX CODE     */~
                                  L53760            /* W-2 Box Label    */
                return

L53150: REM Test Data for Deduction Code...
            if dedcode$(c%) = "?" then dedcode$(c%) = " "
               descr$ = hex(06)
            call "GETCODE" (#3, dedcode$(c%), descr$, 0%, 0, f1%(3%))
                if f1%(3%) = 1% then L53260
            if dedcode$(c%) <> " " then L53230
                errormsg$ = "Deduction Code CANNOT be Blank"
                return
L53230:     errormsg$ = "This is NOT a valid Deduction Code, please re-"&~
                        "enter"
            return
L53260:     tstcode$ = str(dedcode$(c%))
            dedcode$(c%) = " "
            gosub test_existing_ded
                dedcode$(c%) = str(tstcode$)
                if errormsg$ <> " " then return
            get #3 using L53320, ddescr$(c%)
L53320:         FMT POS(351), CH(30)
            return

L53350: REM Test Data for Deduction Description...
            return

L53380: REM Test Data for Deduction Amount W-2 Box Number...
            if w2box$(c%) <> " " then L53430
                errormsg$ = "W-2 Box number for this deduction CANNOT b"&~
                            "e Blank"
                return
L53430:     convert w2box$(c%) to temp1%, data goto L53450
            if temp1% >= low% and temp1% <= high% then L53480
L53450:         errormsg$ = "W-2 Box number MUST be between "  & low$   &~
                            " and " & high$
                return
L53480:     tstbox$ = str(w2box$(c%))
            gosub test_notused
                if errormsg$ <> " " then return
            w2code$(c%) = " "
            if w2box$(c%) <> "14" then w2label$(c%) = " "
            call "STRING" addr("RJ", w2box$(c%), 2%)
            return

L53570: REM Test Data for Deduction W-2 Box Code...
            if w2box$(c%) = "11" or w2box$(c%) = "13" then L53630
                return
            if w2box$(c%) <> " " then L53630
                errormsg$ = "W-2 Box Code CANNOT be Blank"
                return
L53630:     if w2code$(c%) <> "I" then L53660
                errormsg$ = "'I' is NOT a valid W-2 Box Code"
                return
L53660:     if w2box$(c%) = "13" then L53710
                if w2code$(c%) = "N" or w2code$(c%) = " " then return
                    errormsg$ = "W-2 Box Code MUST be BLANK or 'N' for "&~
                                "this deduction"
                    return
L53710:     if w2code$(c%) >= "A" and w2code$(c%) <= "N" then return
                errormsg$ = "W-2 Box Code MUST be between A and N for t"&~
                            "his deduction"
                return

L53760: REM Test Data for Deduction W-2 Box Label...
            if w2box$(c%) <> "14" then return
                if w2box$(c%) <> " " then return
            errormsg$ = "W-2 Box Label CANNOT be Blank"
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 3.                       *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L54150,         /* STATE ABBREV.    */~
                                    L54200,         /* STATE TAX ID. NO.*/~
                                    L54250,         /* STATE METHOD     */~
                                    L54460,         /* CITY NAME        */~
                                    L54490          /* CITY METHOD      */
                     return

L54150: REM Test Data for State Abbreviation...
            if state$(c%) > "AA" and state$(c%) < "ZZ" then return
                errormsg$ = "State Abbreviation MUST be Two Letters"
                return

L54200: REM Test Data for State Tax ID Number...
            if stateid$(c%) <> " " then return
                errormsg$ = "State Tax ID Number CANNOT be Blank"
                return

L54250: REM Test Data for State Tax Deduction Method...
            call "GETCODE" (#3, smthd$(c%), sdescr$(c%), 0%, 0, f1%(3%))
                if smthd$(c%)=" " then errormsg$ = "State Deduction Met"&~
                              "hod CANNOT be Blank"
                    if errormsg$ <> " " then return
                if f1%(3%) = 0% then errormsg$ = "State Deduction Metho"&~
                                "d NOT found, please re-enter"
                    if errormsg$ <> " " then return
            get #3, using L54400, temp$
L54400:     FMT XX(18), CH(1)
            if temp$ = "Y" then return
                 errormsg$ = "State Deduction Method MUST be an Employe"&~
                             "e paid deduction"
                 return

L54460: REM Test Data for City Name...
                return

L54490: REM Test Data for City Tax Deduction Method...
            if city$(c%) = " " then return
            call "GETCODE" (#3, cmthd$(c%), " ", 0%, 0, f1%(3%))
                if cmthd$(c%) = " " then errormsg$ = "City Deduction Me"&~
                                "thod CANNOT be Blank"
                    if errormsg$ <> " " then return
                if f1%(3%) = 0% then errormsg$ = "City Method NOT found"&~
                             ", please re-enter"
                if errormsg$ <> " " then return
            gosub test_existing_statecity
                smthd$(c%) = str(tststate$)
                cmthd$(c%) = str(tstcity$)
                if errormsg$ <> " " then return
            get #3, using L54670, temp$
L54670:         FMT XX(18), CH(1)
            if temp$ = "Y" then return
                 errormsg$ = "City Deduction Method MUST be an Employee"&~
                             " paid deduction"
                 return

        fica_code_check
            if ficamthd$ = " " and medicaremthd$ = " " then return
                if ficamthd$ = medicaremthd$ then errormsg$ = "F.I.C.A "&~
                   "and Medicare Deduction Methods CANNOT be the same"
            return

        test_existing_ded
            if tstcode$ = fedmthd$  or                                   ~
               tstcode$ = ficamthd$ or                                   ~
               tstcode$ = medicaremthd$ then return
            search%(1%) = 0%
            search str(dedcode$()) = str(tstcode$) to search%() step 6%
            if search%(1%) = 0% then return
                errormsg$ = tstcode$ & " already exists as a Deduction "&~
                            "Method, please re-enter"
                return

        test_existing_statecity
            tststate$ = str(smthd$(c%)) :  smthd$(c%) = " "
            tstcity$  = str(cmthd$(c%)) :  cmthd$(c%) = " "
            search str(smthd$()) = str(tststate$) to search%() step 6%
                if search%(1%) = 0% then return
            search str(cmthd$()) = str(tstcity$) to search%() step 6%
                if search%(1%) = 0% then return
            errormsg$ = "State and City Deduction Method Already Exists"
            return

        test_notused
*          SEARCH%(1%) = 0%
*          SEARCH NOBOX$ = STR(TSTBOX$) TO SEARCH%() STEP 2%
*          IF SEARCH%(1%) = 0% THEN RETURN
*              ERRORMSG$ = TSTBOX$ & " is NOT used this year"
            return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
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
