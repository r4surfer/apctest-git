        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  JJJJJ  BBBB   EEEEE  M   M  PPPP   IIIII  N   N  PPPP    *~
            *    J    B   B  E      MM MM  P   P    I    NN  N  P   P   *~
            *    J    BBBB   EEEE   M M M  PPPP     I    N N N  PPPP    *~
            *  J J    B   B  E      M   M  P        I    N  NN  P       *~
            *   J     BBBB   EEEEE  M   M  P      IIIII  N   N  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBEMPINP - MAINTAINS SHOP FLOOR EMPLOYEES FOR TIME CARD   *~
            *            INPUT.  MAINTAINS EMPLOYEES AND EARNINGS CODES *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/15/92 ! ORIGINAL (Largely Cloned from PRLEMPIN)  ! JBK *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        dim                                                              ~
            autopay$1,                   /* AUTOMATIC PAYROLL FLAG     */~
            category$(18)6,              /* CATEGORIES FOR BUILD TABLES*/~
            checkkey$50,                 /* CHECK EARNINGS TYPES       */~
            curjob$16,                   /* Employee's Job Title       */~
            cursor%(2),                  /* CURSOR COLUMN LOCATION     */~
            date$8,                      /* TODAY'S DATE SCREEN DISPLAY*/~
            deductflag$1,                /* FLAG TO DO HIM IN PRLDDUCT */~
            dept$4,                      /* DEPARTMENT CODE THIS GUY   */~
            deptdescr$32,                /* DEPARTMENT DESCRIPTION     */~
            empcode$12,                  /* EMPLOYEE CODE INFO         */~
            empname$32,                  /* FELLA'S NAME               */~
            erncata$(100)4,              /* EARNINGS CATEGORY   CODE   */~
            erntype$(100)12,             /* EARNINGS TYPE (DESCRIPTION)*/~
            ernunits$(100)6,             /* EARNINGS UNITS DESCRIPTION */~
            ernrate$(100)10,             /* EARNINGS RATE              */~
            ernacct$(100)16,             /* EARNINGS ACCOUNT NUMBER    */~
            errormsg$79,                 /* ERROR MESSAGE TEXT INFO    */~
            fac$(15,5)1,                 /* FIELD ATTRIBUTE CHARACTERS */~
            filler$7,                    /* FILLER                     */~
            header$79,                   /* Screen Title               */~
            hdr$(5)30,                   /* Summary Screen Headings    */~
            i$(24)80,                    /* SCREEN IMAGE WORK AREA     */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            inscat$6,                    /* CATEGORY FOR SUPER-INSERT  */~
            jrate$10,                    /* Rate To Post Job At Extrnly*/~
            shift$1,                     /* Normal Shift               */~
            status$1,                    /* Employee Status            */~
            statusdescr$(6)18,           /* Status Descriptions        */~
            lastemp$12,                  /* LAST EMPLOYEE INPUT        */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            message$79,                  /* MESSAGE FOR INPUT          */~
            overacct$16,                 /* OVERHEAD ACCRUAL ACCOUNT   */~
            overacctdescr$32,            /* OVERHEAD ACCOUNT DESCRIPTIN*/~
            perdept$4,                   /* PERMASTR Department Code   */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            readkey$50,                  /* READ KEY FOR FILE INFO     */~
            ssnumber$11,                 /* Employee Social Security # */~
            statusdescr$18,              /* STATUS DESCRIPTION         */~
            tran$80,                     /* CURSOR=>FIELD TRANS TABLE  */~
            userid$3,                    /* USERID OF CURRENT USER     */~
            vf$200,                      /* Variable Fields            */~
            laborclas$4,                 /* LABOR CLASS CODE           */~
            labordescr$32                /* LABOR CLASS CODE DESCRIPTN */

        dim                      /* PERSONNEL STUFF                    */~
            lname$15,            /* Last name of person - part of pers */~
            fname$10,            /* First name of person               */~
            mname$1              /* Middle name of person              */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! System information file (payroll default)*~
            * # 2 ! GLMAIN   ! General ledger main account file.        *~
            * # 3 ! EMPMASTR ! Employee file master records.            *~
            * # 4 ! EMPEARN1 ! Employee earnings details file           *~
            * # 6 ! PRLDEPTF ! Payroll department code file             *~
            * # 8 ! GENCODES ! General Codes File                       *~
            * #12 ! PRLEDFLT ! Payroll earnings default file            *~
            * #14 ! PERMASTR ! Personnel master file                    *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            select #2,  "GLMAIN",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 9

            select #3,  "EMPMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 136,                                  ~
                         keypos = 1, keylen = 12,                        ~
                         alt key  1, keypos = 70, keylen =  1, dup

            select #4,  "EMPEARN1",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 200,                                  ~
                         keypos = 1, keylen = 15,                        ~
                         alt key  1, keypos =  16, keylen = 28

            select #6,  "PRLDEPTF",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 4

            select #8,  "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  128,                                  ~
                        keypos =    1, keylen =   24

            select #12, "PRLEDFLT",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 100,                                  ~
                         keypos = 1, keylen = 7

            select #14, "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 950,                                   ~
                        keypos = 39, keylen = 12,                        ~
                        alt key  1, keypos =  28, keylen = 23,           ~
                            key  2, keypos =   2, keylen = 49,           ~
                            key  3, keypos =   1, keylen = 50

            call "PRLEXTSB" ("SFC", ret%)
                if ret% = 99% then L65000

        call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1%,  0%, f2%(1%),    0%, " ")
            call "OPENCHCK" (#2%,  0%, f2%(2%),    0%, " ")
            call "OPENCHCK" (#3%,  0%, f2%(3%),  100%, " ")
            call "OPENCHCK" (#4%,  0%, f2%(4%),  100%, " ")
            call "OPENCHCK" (#6%,  0%, f2%(6%),    0%, " ")
            call "OPENCHCK" (#8%,  0%, f2%(8%),    0%, " ")
            call "OPENCHCK" (#12%, 0%, f2%(12%),   0%, " ")
            call "OPENCHCK" (#14%, 0%, f2%(14%), 100%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * SETS VALUES OF VARIABLES CRITICAL TO PROGRAM OPERATION.   *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)


            call "EXTRACT" addr ("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            REM Set up first half of TRAN$ for edit earnings table
                init(hex(00)) tran$
                init(hex(01)) str(tran$, 4,  4)
                init(hex(02)) str(tran$,13, 12)
                init(hex(03)) str(tran$,30,  6)
                init(hex(04)) str(tran$,42, 10)
                init(hex(05)) str(tran$,57, 12)

                hdr$(1) = "Category"
                hdr$(2) = "Type"
                hdr$(3) = "Units Descr."
                hdr$(4) = "   Rate"
                hdr$(5) = "Expense Account"

                statusdescr$(1) = "(Current)"
                statusdescr$(2) = "(Previous)"
                statusdescr$(3) = "(Leave of Absence)"
                statusdescr$(4) = "(Military Leave)"
                statusdescr$(5) = "(Terminated)"
                statusdescr$(6) = "(Undefined)"

        REM *************************************************************~
            *            I N P U T   M A I N   P R O G R A M            *~
            *-----------------------------------------------------------*~
            * Inputs header information.                                *~
            *************************************************************

        inputmode
              errormsg$, infomsg$, dept$, deptdescr$, empcode$,          ~
              laborclas$, labordescr$, statusdescr$, autopay$,           ~
              overacctdescr$,  filler$, erncata$(), erntype$(),          ~
              ernacct$(), ernunits$(), ernrate$(), empname$, overacct$,  ~
              shift$, jrate$, status$, lname$, fname$, mname$,           ~
              ssnumber$, curjob$, perdept$, vf$ = " "

            deductflag$, autopay$ = "N"
            change%, editmode% = 0

            call "ALLFREE"

L10340:     for fieldnr% = 1 to 10
                gosub enable_161
                      if enabled% =  0 then L10500
L10370:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then L10460
L10400:                  fieldnr% = fieldnr% - 1%
                         if fieldnr% < 1% then L10340
                         gosub enable_161
                         if enabled% <> 0 then L10370
                         goto L10400
L10460:               if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10370
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10370
L10500:         next fieldnr%

        REM *************************************************************~
            *          I N P U T   E A R N I N G S   T A B L E          *~
            *-----------------------------------------------------------*~
            * Input earnings table.  First, show screen and get the     *~
            * names of the catagories  he gets his earnings from.  Then *~
            * load them into the table and go from there.               *~
            *************************************************************

            REM INPUT EARNINGS TYPES TO BUILD TABLE FROM..
                errormsg$, infomsg$, message$, category$() = " "
L11100:         gosub L45000
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  0 then L11100
                gosub L55000              /* TEST DATA FOR ALL OK       */
                      if errormsg$ <> " " then L11100

            REM Now build earnings table from above.
                c% = 0
                for subscript% = 1 to 18
                    if category$(subscript%) = " " then L11260
                       readkey$ = category$(subscript%)
L11210:                call "PLOWNEXT" (#12, readkey$, 4%, f1%(12))
                            if f1%(12) = 0 then L11260
                       c% = c% + 1
                       gosub L35000       /* load & format earnings ent */
                       goto L11210
L11260:             next subscript%
                maxlines% = c%

            REM Now input mode for the line items.
                line%, screenline% = 0
                infomsg$, errormsg$, message$ = " "
                if maxlines% = 0 then L11590     /* If none in table */

L11340:         screenline% = screenline% + 1
                if screenline% < 15 then L11380
                   line% = line% + 15
                   screenline% = 1
L11380:         currentline%, c% = line% + screenline%
                if currentline% > maxlines% then L11550

                for fieldnr% = 1 to 5
                    gosub'164(fieldnr%)
                          if enabled% =  0 then       L11520
L11450:             gosub'204(screenline%, fieldnr%)
                          if keyhit%  =  0 then       L11500
                          if keyhit%  =  1 then gosub startover
                          if keyhit%  =  4 then gosub elineabove
                          goto L11450
L11500:             gosub'154(fieldnr%)
                          if errormsg$ <> " " then L11450
L11520:             next fieldnr%
                    goto L11340

L11550:     REM Now do regular input mode for more line items.
                screenline% = screenline% - 1
                message$ = "Now You Can Input Additional Earnings Info" &~
                           "rmation."

L11590:         screenline% = screenline% + 1
                if screenline% < 16 then L11630
                   line% = line% + 15
                   screenline% = 1
L11630:         currentline%, c% = line% + screenline%
                if currentline% > 100 then L13000

L11670:         for fieldnr% = 1 to 5
                    gosub'164(fieldnr%)
                          if enabled% =  0 then       L11790
L11700:             gosub'204(screenline%, fieldnr%)
                          if keyhit%  =  0 then       L11770
                          if keyhit%  =  1 then gosub startover
                          if keyhit%  =  2 then gosub ecolumnone
                          if keyhit%  =  4 then gosub elineabove
                          if keyhit%  = 16 and fieldnr% = 1 then L11830
                          goto L11700
L11770:             gosub'154(fieldnr%)
                          if errormsg$ <> " " then L11700
L11790:             next fieldnr%
                    maxlines% = maxlines% + 1
                    goto L11590

L11830:     REM Zap the current field for this guy.
                erncata$(c%) = " "

        REM Do Input for Variable Fields
            call "VFINPSUB" ("PERMASTR", "I", "Manage SFC Employee",     ~
                             str(header$,,60), "NN", vf$, keyhit%)
            if keyhit% = 1% then inputmode

L13000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Edit mode main program edits linear information.          *~
            *************************************************************

        editmode
            init(" ") errormsg$, infomsg$
            editmode% = 1

L13100: REM EDTPG1
            gosub edit_message
            gosub'211(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       edtearnings
                  if keyhit%  =  5 then       edtpg2
                  if keyhit%  = 12 then gosub delete_employee
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L13100
            oldfieldnr% = 0
L13220:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 2 or fieldnr% > 12 then L13100
            if fieldnr% < 4% then L13240
            if fieldnr% < 6% then fieldnr% = 3%
            if fieldnr% > 5% then fieldnr% = fieldnr% - 2%
L13240:     if fieldnr% = oldfieldnr% then L13100
            oldfieldnr% = fieldnr%

            gosub enable_161
                  if enabled% =  0 then L13100
L13290:     gosub'211(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then       L13290
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L13290
            goto L13220

        edit_message
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press RETURN."
            return

        edtpg2                              /* Edit Variable Fields   */
            call "VFINPSUB" ("PERMASTR", "E", "Manage SFC Employee",     ~
                             str(header$,,60), "YY", vf$, keyhit%)
            if keyhit% =  1% then inputmode
            if keyhit% =  4% then editmode
            if keyhit% =  5% then editmode
            if keyhit% = 16% then datasave
                             goto editmode

        REM *************************************************************~
            *           E D I T   E A R N I N G S   T A B L E           *~
            *-----------------------------------------------------------*~
            * Edits earnings table. Also has super-insert code for      *~
            * earnings.  This is like insert except it gets a lot of    *~
            * lines off the disk.                                       *~
            *************************************************************

        edtearnings
            line%, currentline%, screenline% = 0

L14120:     errormsg$ = " "  :  gosub edit_message
L14130:     gosub'214(0%, 0%)
                  if keyhit%  =  0 then       L14330
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then line% = 0
                  if keyhit%  =  3 then line% = max(0,maxlines%-15)
                  if keyhit%  =  4 then line% = max(0,line%-14)
                  if keyhit%  =  5 then line% = min(line%+14,max(0,      ~
                                                maxlines%-15))
                  if keyhit%  =  6 then line% = max(0,line%-1)
                  if keyhit%  =  7 then line% = min(line%+1,max(0,       ~
                                                maxlines%-15))
                  if keyhit%  =  9 then       editmode
                  if keyhit%  = 10 then gosub esuperinsert
                  if keyhit%  = 11 then gosub einsertmode
                  if keyhit%  = 12 then gosub edeletemode
                  if keyhit%  = 16 then       datasave
                  goto L14120

L14330:     REM Now figure out which field he hit.
                screenline% = cursor%(1)-5
                if screenline% < 1 then L14120
                if screenline% > 15 then L14120
                fieldnr% = val(str(tran$,cursor%(2),1))
                if fieldnr% = 0 then L14120
                c%, currentline% = line% + screenline%
                if currentline% > maxlines% then L14120

L14510:         gosub'214(screenline%, fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  0 then L14510
                gosub'154(fieldnr%)
                      if errormsg$ <> " " then L14510
                goto L14130

        REM *************************************************************~
            *        C O L U M N   O N E ,   L I N E   A B O V E        *~
            *-----------------------------------------------------------*~
            * Handles column one and line above logic for both tables.  *~
            *************************************************************

        ecolumnone
            gosub clear_eline
            return clear all
            goto L11670

        elineabove
            if c% = 1 then return
               on fieldnr% gosub L15410,            /* CATEGORY   CODE  */~
                                 L15420,            /* TYPE CODE        */~
                                 L15450,            /* UNITS DESCRIPTION*/~
                                 L15460,            /* RATE PER UNIT    */~
                                 L15470             /* DEBIT ACCOUNT #  */
                  return
L15410:     erncata$   (c%) = erncata$   (c%-1) : return
L15420:     erntype$   (c%) = erntype$   (c%-1) : return
L15450:     ernunits$  (c%) = ernunits$  (c%-1) : return
L15460:     ernrate$   (c%) = ernrate$   (c%-1) : return
L15470:     ernacct$   (c%) = ernacct$   (c%-1) : return

        REM *************************************************************~
            *    I N S E R T / D E L E T E   F O R   E A R N I N G S    *~
            *-----------------------------------------------------------*~
            * Insert and delete mode code for the earnings table is in  *~
            * this section.  It's fairly standard code.                 *~
            *************************************************************

L15930: einsertmode
            if maxlines% >= 100 then return
               screenline% = cursor%(1)-5
               if screenline% < 1% or screenline% > 15% then return
               if line% + screenline% < maxlines% then L15980
                  screenline% = maxlines% - line%         /* AT END */
L15980:        if screenline% <> 15 then L16050     /* BOTTOM OF PAGE   */
                  line% = line% + 1
                  screenline% = screenline% - 1

L16050:     currentline%, c% = screenline% + line%
            gosub L16740                  /* ROLL ARRAYS UP ONE LINE.   */

            REM Now input the line, make so we can cancel out if necessary
                infomsg$ = " "
                for fieldnr% = 1 to 5
                    gosub'164(fieldnr%)
                          if enabled% = 0 then L16180
L16130:             gosub'224(screenline%, fieldnr%)
                          if keyhit%  =  1 then L16240
                          if keyhit% <>  0 then L16130
                    gosub'154(fieldnr%)
                          if errormsg$ <> " " then L16130
L16180:             next fieldnr%

                maxlines% = maxlines% + 1
                goto L15930

L16240:     REM This routine aborts insert mode and destroys SCREENLINE%
                gosub L16980              /* Actually delete @ C%      */

                temp% = c%
                c% = maxlines% + 1
                gosub clear_eline
                c% = temp%

            if currentline% >= maxlines% and screenline% = 15            ~
               then line% = max(0%, maxlines% - 15)
            return

        edeletemode
            REM Figure out where we are on screen and return if invalid
                if maxlines% = 0 then return
                screenline% = cursor%(1)-5
                if screenline% < 1% or screenline% > 15% then return
                c%, currentline% = screenline% + line%
                if currentline% > maxlines% then return
                message$ = "Press RETURN to DELETE the Blinking Line or P~
        ~ress PF(1) to Cancel the Delete."

            REM Now show delete screen with line flashing.
L16560:         gosub'234(screenline%)
                      if keyhit%  =  1 then       return
                      if keyhit% <>  0 then       L16560

            REM Now that he's approved it, delete it.
                c% = currentline%
                if currentline% < maxlines% then gosub L16980
                                         /* Actually delete line @ C%  */
                temp% = c%
                c% = maxlines%
                gosub clear_eline
                c% = temp%

                maxlines% = maxlines% - 1
                if screenline% > maxlines% - 15                          ~
                   then line% = max(0%, maxlines%-15%)
                return

L16740:     REM ***** SUBSIDIARY ROUTINE -- Copy all the elements up one
                if c% >= maxlines% then L16920
                for temp% = maxlines% to c% step -1
                    erncata$   (temp%+1) = erncata$   (temp%)
                    erntype$   (temp%+1) = erntype$   (temp%)
                    ernunits$  (temp%+1) = ernunits$  (temp%)
                    ernrate$   (temp%+1) = ernrate$   (temp%)
                    ernacct$   (temp%+1) = ernacct$   (temp%)
                    next temp%

L16920:         screenline% = screenline% + 1
                c%, currentline% = currentline% + 1
                gosub clear_eline
                errormsg$, infomsg$ = " "
                return

L16980:     REM ***** SUBSIDIARY ROUTINE -- Roll down from C% to end
                for temp% = currentline% to maxlines%
                    erncata$   (temp%) = erncata$   (temp%+1)
                    erntype$   (temp%) = erntype$   (temp%+1)
                    ernunits$  (temp%) = ernunits$  (temp%+1)
                    ernrate$   (temp%) = ernrate$   (temp%+1)
                    ernacct$   (temp%) = ernacct$   (temp%+1)
                    next temp%
                return

        delete_employee
            keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "D E L E T E",                     ~
                            "Enter PF-16 to DELETE Employee", "-OR-",    ~
                            "Press RETURN to CANCEL delete.")
            if keyhit1% <> 16% then return
                call "DELETE" (#4,  empcode$, 12%)  /* EMPEARN1 File   */
                call "DELETE" (#3,  empcode$, 12%)  /* EMPMASTR File   */
                call "DELETE" (#14, empcode$, 12%)  /* PERMASTR File   */
                return clear all
                goto inputmode

        REM *************************************************************~
            *            W R I T E   D A T A   T O   F I L E            *~
            *-----------------------------------------------------------*~
            * Writes data to file.                                      *~
            * Note that we need to have a slight departure from standard*~
            * here in that we cannot delete the old employee record here*~
            * Since we lose hold on the old employee master once we go  *~
            * to delete the line items and we might have someone else   *~
            * out to get the record, in which case we get nuked.  The   *~
            * delete routine is buried in the middle of the write sub.  *~
            *************************************************************

        datasave
            gosub L32000
            lastemp$ = empcode$
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for page 1 of the screen *~
            *************************************************************

            enable_161
                  message$ = " "
                  enabled% = 1
                  on fieldnr% goto  L20220,         /* EMPLOYEE CODE    */~
                                    L20270,         /* Current Status   */~
                                    L20330,         /* Names            */~
                                    L20380,         /* SS Number        */~
                                    L20420,         /* Department       */~
                                    L20500,         /* Job Title        */~
                                    L20600,         /* Applied Overhead */~
                                    L20700,         /* Labor Class      */~
                                    L20800,         /* Rate to Post Job */~
                                    L20900          /* Shift            */

L20220:     REM DEFAULT/ENABLE FOR EMPLOYEE CODE
            message$ = "Enter Employee Code or leave blank to see codes o~
        ~n file."
                return
L20270:     REM DEFAULT/ENABLE FOR CURRENT STATUS
            status$ = "C"
            message$ = "C=Current, P=Previous, L=Leave of Absence, M=Mili~
        ~tary Leave, N=Terminated"
                return
L20330:     REM DEFAULT/ENABLE FOR NAME (LAST, FIRST, MIDDLE)
            message$ = "Enter all three names: Last, First and Middle Ini~
        ~tial"
                return
L20380:     REM DEFAULT/ENABLE FOR SOCIAL SECURITY NUMBER
            message$ = "Enter Social Security Number."
                return
L20420:     REM DEFAULT/ENABLE FOR PRIMARY DEPARTMENT
                message$ = "Enter Employee's Department or leave blank."
                return
L20500:     REM DEFAULT/ENABLE FOR CURRENT JOB TITLE
                message$ = "Enter Employee's Job Title or leave blank."
                return
L20600:     REM DEFAULT/ENABLE FOR APPLIED OVERHEAD ACCOUNT
                message$ = "If Labor Is To Be Posted To A Job, Enter The ~
        ~Default Overhead Accrual Acct."
                return
L20700:     REM DEFAULT/ENABLE FOR LABOR CLASS
                message$ = "The Labor Class Is Used During Job Posting To~
        ~ Figure Overhead."
                return
L20800:     REM DEFAULT/ENABLE FOR TIME CARD PAY RATE (JOB POSTING)
                message$ = "Enter Rate To Charge Job."
                if dept$ > " " then return
                enabled% = 0%
                return
L20900:     REM DEFAULT/ENABLE FOR SHIFT
                message$ = "Enter Normal Shift."
                if dept$ > " " then return
                enabled% = 0%
                return
        REM *************************************************************~
            *   D E F A U L T / E N A B L E   F O R   E A R N I N G S   *~
            *-----------------------------------------------------------*~
            * Default/enable for earnings table- Input Mode only...     *~
            *************************************************************

            deffn'164(fieldnr%)
                  enabled% = 1%
                  on fieldnr% goto  L22380,         /* CATEGORY   CODE  */~
                                    L22420,         /* EARNINGS TYPE    */~
                                    L22510,         /* UNITS DESCRIPTION*/~
                                    L22550,         /* RATE PER UNIT    */~
                                    L22610          /* ACCOUNT NUMBER   */

L22380: REM DEFAULT/ENABLE FOR CATEGORY   CODE
            if erncata$(c%) <> " " then enabled% = 0%
            message$ = "Enter Category Code or Leave Blank."
            return

L22420: REM DEFAULT/ENABLE FOR EARNINGS TYPE
            if erntype$(c%) <> " " then enabled% = 0%
            message$ = "Enter Earnings Type, CANNOT be BLank."
            return

L22510: REM DEFAULT/ENABLE FOR UNITS DESCRIPTION
            if ernunits$(c%) <> " " then enabled% = 0%
            if ernunits$(c%) =  " " then ernunits$(c%) = "HOURS"
            message$ = "Enter Units Description."
            return

L22550: REM DEFAULT/ENABLE FOR UNIT RATE
            if ernrate$(c%) <> " " then enabled% = 0%
            if ernrate$(c%) = " " and c% > 1%                            ~
                                       then ernrate$(c%) = ernrate$(c%-1)
            message$ = "Enter Rate per Unit."
            return

L22610: REM DEFAULT/ENABLE FOR ACCOUNT NUMBER
            if ernacct$(c%) <> " " then enabled% = 0%
            if ernacct$(c%) =  " " and c% > 1%                           ~
                                   then ernacct$(c%) = ernacct$(c%-1)
            message$ = "Enter General Ledger Account Number."
            return

        REM *************************************************************~
            *     S U P E R   I N S E R T   F O R   E A R N I N G S     *~
            *-----------------------------------------------------------*~
            * Handles super insert (insert an entire earnings category) *~
            * for earnings table.  This is similar to insert mode for   *~
            * the earnings table except no screen input.                *~
            *************************************************************

        esuperinsert

            errormsg$, inscat$, message$ = " "
L25110:     gosub L46000                  /* get category to insert     */
                  if keyhit%  =  1 then return
                  if keyhit%  = 16 then L25145
                  if keyhit% <>  0 then L25110
                     goto L25150
L25145:           keyhit% = 1%  :  return  /* Prevent PF(16) Data Save */
L25150:     gosub L57000                  /* test for validity          */
                  if errormsg$ <> " " then L25110

            REM Plow routine for loading all entries in the cat into co
                readkey$ = inscat$
                currentline%, c% = line% + cursor%(1)-5%
                if c% > maxlines% then currentline%, c% = maxlines%
                if cursor%(1) < 5 then currentline%, c% = max(0, line%-1)
                firstline% = currentline%

L25260:         call "PLOWNEXT" (#12, readkey$, 4%, f1%(12))
                     if f1%(12) = 0 then L25340
                        currentline%, c% = currentline% + 1
                        gosub L25690      /* ROLL ARRAYS UP ONE LINE.   */
                        gosub L35000      /* LOAD LINE FROM DISK.       */
                        maxlines% = maxlines% + 1
                     goto L25260

L25340:     REM Now end loading up entries routine.
                lastline% = currentline%

            REM Now begin input mode for the super inserted line items.
                line% = firstline% - mod(firstline%, 15)
                screenline% = firstline% - line%
                infomsg$, errormsg$ = " "
                if maxlines% = 0 then return

L25430:         screenline% = screenline% + 1
                if screenline% < 16 then L25470
                   line% = line% + 15
                   screenline% = 1
L25470:         currentline%, c% = line% + screenline%
                if currentline% > lastline% then L25650

                for fieldnr% = 1 to 5
                    gosub'164(fieldnr%)
                          if enabled% =  0 then       L25620
L25550:             gosub'204(screenline%, fieldnr%)
                          if keyhit%  =  0 then       L25600
                          if keyhit%  =  1 then gosub startover
                          if keyhit%  =  4 then gosub elineabove
                          goto L25550
L25600:             gosub'154(fieldnr%)
                          if errormsg$ <> " " then L25550
L25620:             next fieldnr%
                goto L25430

L25650:     REM Terminate super insert mode.
                line% = min(max(lastline%-15, 0), max(maxlines%-15, 0))
                return

L25690:     REM This routine rolls the current line down.  subset of 17800
                if currentline% > maxlines% then return
                for temp% = maxlines% to c% step -1
                    erncata$   (temp%+1) = erncata$   (temp%)
                    erntype$   (temp%+1) = erntype$   (temp%)
                    ernunits$  (temp%+1) = ernunits$  (temp%)
                    ernrate$   (temp%+1) = ernrate$   (temp%)
                    ernacct$   (temp%+1) = ernacct$   (temp%)
                next temp%

        clear_eline
            erncata$(c%), erntype$(c%), errormsg$, ernunits$(c%),        ~
            ernrate$(c%), ernacct$(c%), infomsg$ = " "
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the user the ability to start over when he wants to *~
            * else return to the menu.  Notice that he has to push 2    *~
            * different buttons to start over--a little harder.         *~
            *************************************************************

        startover
            k% = 2%
            call "STARTOVR" (k%)
            if k% = 1% then return
            if k% <> 0% then startover
            return clear all
            goto inputmode


L30000: REM *************************************************************~
            * L O A D   E M P L O Y E E   M A S T E R   F O R   E D I T *~
            *-----------------------------------------------------------*~
            * Loads the employee master from PERMASTR and EMPMASTR and  *~
            * earnings records.                                         *~
            *************************************************************

            call "READ100" (#14, empcode$, f1%(14))
                 if f1%(14) = 0 then return

            get #14 using L30130, status$, lname$, fname$, mname$,        ~
                                 ssnumber$, curjob$, perdept$, vf$

L30130: FMT                      /* FILE: PERMASTR                     */~
            CH(1),               /* General purpose status indicator   */~
            CH(15),              /* Last name of person                */~
            CH(10),              /* First name of person               */~
            CH(1),               /* Middle name of person              */~
            CH(11),              /* Social security number             */~
            XX(12),              /* employee code                      */~
            XX(250),             /* Filler                             */~
            XX(36),              /* Filler                             */~
            CH(16),              /* Current job title                  */~
            XX(16),              /* Filler                             */~
            CH(4),               /* Current department                 */~
            XX(87),              /* Filler                             */~
            CH(200),             /* Variable Fields                    */~
            XX(250),             /* filler for rest of record or inter */~
            XX(41)               /* filler for rest of record or inter */

            gosub employee_name
            gosub employee_status

        REM Load the EMPMASTR record
            call "READ100" (#3, empcode$, f1%(3))
                 if f1%(3) = 0 then return

            get  #3, using L31070, empcode$, laborclas$, autopay$,        ~
                     deductflag$, dept$, overacct$, jrate, shift

L31070:     FMT CH(12),                  /* Employee code              */~
                CH(4),                   /* Labor class code           */~
                XX(40),                  /* Filler                     */~
                CH(1),                   /* Autopayroll flag = 'N'     */~
                XX(12),                  /* Filler                     */~
                CH(1),                   /* Deduct on run = 'N'        */~
                XX(21),                  /* Filler                     */~
                CH(4),                   /* Department                 */~
                CH(9),                   /* Overhead account           */~
                XX(16),                  /* Filler                     */~
                PD(14,4),                /* Rate for extrbal jb posting*/~
                BI(1),                   /* Shift                      */~
                XX(7)                    /* Filler                     */

        REM Now format INFOMSG$ to say what we're up to.
                infomsg$ = "Loading Employee"
                str(infomsg$,41) = hex(84) & empcode$ & " " & empname$
                call "SHOSTAT" (infomsg$)

            perdept$ = dept$         /* Keep the Departments aligned */

            if jrate > 10000 then jrate = 0
            if shift > 4 then shift = 0
            call "CONVERT" (jrate, -2.4, jrate$)
            convert shift to shift$, pic(#)
            if shift = 0 then shift$ = " "

        REM Format information for employee master record.
                call "DESCRIBE"(#2,overacct$,  overacctdescr$,  1%,f1%(2))
                    call "GLFMT" (overacct$)
                call "DESCRIBE" (#6, dept$, deptdescr$, 1%, f1%(6))
                readkey$ = "LBR CLASS" & laborclas$
                call "DESCRIBE"(#8, readkey$, labordescr$, 1%, f1%(8))

        REM Load the earnings records.
                maxlines%, c% = 0
                readkey$ = empcode$

L31440:         call "PLOWNEXT" (#4, readkey$, 12%, f1%(4))
                     if f1%(4) = 0 then L31700
                maxlines%, c% = c% + 1
                get   #4,  using L31510,                                  ~
                           erntype$(c%), erncata$(c%), ernunits$(c%),    ~
                           ernrate, ernacct$(c%)

L31510:     FMT XX(12),                  /* Employee code              */~
                XX(3),                   /* Sequence number            */~
                XX(12),                  /* Employee code (again)      */~
                CH(12),                  /* Earnings type              */~
                CH(4),                   /* Department code            */~
                XX(2),                   /* Filler                     */~
                CH(6),                   /* Unit description           */~
                PD(14,4),                /* Unit rate                  */~
                CH(9),                   /* Expense account number     */~
                XX(132)                  /* Filler                     */

        REM Format information for earnings records.
            ernrate$(c%) = " "
            if ernrate = 0 then L31670
            call "CONVERT" (ernrate, 2.4, ernrate$(c%))
            call "GLFMT" (ernacct$(c%))
L31670:
            goto L31440

L31700:     return

L32000: REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves employee data on file.  Write the PERMASTR, EMPMASTR*~
            * and the EMPEARN1 files.                                   *~
            *************************************************************

        REM Now format INFOMSG$ to say what we're up to.
                infomsg$ = "Saving Employee"
                str(infomsg$,41) = hex(84) & empcode$ & " " & empname$
                call "SHOSTAT" (infomsg$)

            perdept$ = dept$           /* Keep Departments Aligned */

        REM First take care of the PERMASTR file
            call "DELETE" (#14, empcode$, 12%)

            write #14 using L32200, status$, lname$, fname$, mname$,      ~
                                 ssnumber$, empcode$, " ", " ",          ~
                                 curjob$, " ", perdept$, " ", vf$,       ~
                                 " ", " "

L32200: FMT                      /* FILE: PERMASTR                     */~
            CH(1),               /* General purpose status indicator   */~
            CH(15),              /* Last name of person                */~
            CH(10),              /* First name of person               */~
            CH(1),               /* Middle name of person              */~
            CH(11),              /* Social security number             */~
            CH(12),              /* employee code                      */~
            CH(250),             /* Filler                             */~
            CH(36),              /* Filler                             */~
            CH(16),              /* Current job title                  */~
            CH(16),              /* Filler                             */~
            CH(4),               /* Current department                 */~
            CH(87),              /* Filler                             */~
            CH(200),             /* Variable Fields                    */~
            CH(250),             /* filler for rest of record or inter */~
            CH(41)               /* filler for rest of record or inter */

        REM Now update the EMPMASTR record
            jrate, shift = 0
            convert jrate$ to jrate, data goto L32530
L32530:     convert shift$ to shift, data goto L32540
L32540:     call "GLUNFMT" (overacct$)

            call "READ101"(#3, empcode$, f1%(3))
                if f1%(3) = 0% then L32590
                delete #3
L32590:
            write #3, using L32640, empcode$, laborclas$, " ", autopay$,  ~
                     " ", deductflag$, " ", dept$, overacct$, " ", jrate,~
                     shift, " "

L32640:     FMT CH(12),                  /* Employee code              */~
                CH(4),                   /* Labor class code           */~
                CH(40),                  /* Filler                     */~
                CH(1),                   /* Autopayroll flag = 'N'     */~
                CH(12),                  /* Filler                     */~
                CH(1),                   /* Deduct on run = 'N'        */~
                CH(21),                  /* Filler                     */~
                CH(4),                   /* Department                 */~
                CH(9),                   /* Overhead account           */~
                CH(16),                  /* Filler                     */~
                PD(14,4),                /* Rate for extrbal jb posting*/~
                BI(1),                   /* Shift                      */~
                CH(7)                    /* Filler                     */

            if f1%(3) = 0% then call "CDANPOST" (#3, "A")                ~
                           else call "CDANPOST" (#3, "C")

        REM Now delete the old earnings records.
            call "DELETE" (#4, empcode$, 12%)

        REM Plow routine to write earnings records.
            if maxlines% = 0 then L33250
            for c% = 1% to maxlines%
                if ernrate$(c%)<>" " then convert ernrate$(c%) to ernrate~
                                     else ernrate = 0
                call "GLUNFMT" (ernacct$(c%))
                gosub L36000          /* Check for duplication      */
                write #4,  using L33140, empcode$, c%, empcode$,          ~
                           erntype$(c%), erncata$(c%), " ",              ~
                           ernunits$(c%), ernrate, ernacct$(c%), " "
            next c%

L33140:     FMT CH(12),                  /* Employee code              */~
                PIC(###),                /* Sequence number            */~
                CH(12),                  /* Employee code (again)      */~
                CH(12),                  /* Earnings type              */~
                CH(4),                   /* Department code            */~
                CH(2),                   /* Filler                     */~
                CH(6),                   /* Unit description           */~
                PD(14,4),                /* Unit rate                  */~
                CH(9),                   /* Expense account number     */~
                CH(132)                  /* Filler                     */

L33250: REM NOW RETURN
            return

L35000: REM *************************************************************~
            *      L O A D   A N   E A R N I N G S   D E F A U L T      *~
            *-----------------------------------------------------------*~
            * Loads an earnings default record from the earnings file.  *~
            *************************************************************

            get  #12, using L35170,                                       ~
                      erncata$(c%), erntype$(c%), ernunits$(c%), ernrate,~
                      ernacct$(c%)

            ernrate$(c%) = " "
            if ernrate = 0 then L35130
            call "CONVERT" (ernrate, 2.4, ernrate$(c%))
L35130:     call "GLFMT" (ernacct$(c%))

            return

L35170:     FMT CH(4),                   /* Category   code            */~
                XX(3),                   /* Sequence number            */~
                CH(12),                  /* Earnings type              */~
                XX(2),                   /* Filler                     */~
                CH(6),                   /* Units                      */~
                PD(14,4),                /* Rate per unit              */~
                CH(9),                   /* Account number             */~
                XX(20)                   /* Filler                     */

L36000:     REM Check for earnings type dupilcation
                str(checkkey$,  1) = empcode$
                str(checkkey$, 13) = erntype$(c%)
                str(checkkey$, 25) = erncata$(c%)
                call "REDALT0" (#4, checkkey$, 1%, f1%(4))
                      if f1%(4) = 0 then return
                change% = change% + 1
                convert change% to str(erntype$(c%), 8, 5), pic(#####)
                go to L36000

        employee_name
            empname$ = " "
            empname$ = "(" & lname$
            if fname$ <> " " then empname$ = empname$ & ", " & fname$ &  ~
                             " " & mname$
            empname$ = empname$ & ")"
            return

        employee_status
            statusdescr$ = " "
            p% = pos("CPLMN " = status$)
            if p% <> 0% and p% < 7% then statusdescr$ = statusdescr$(p%)
            return

        REM *************************************************************~
            *     I N P U T / E D I T   P A G E   1   S C R E E N S     *~
            *-----------------------------------------------------------*~
            * Inputs and edits employee master page 1 data.             *~
            *************************************************************

            deffn'201(fieldnr%)
                init(hex(8c)) lfac$()
                header$ = " "
                if lastemp$<>" " then header$="Last Employee: "&lastemp$
                pf$(1) = "(1)Start Over          (4)Previous Field" &    ~
                         "                       (13)Instructions"
                pf$(2) = "                                        " &    ~
                         "                       (15)Print Screen"
                pf$(3) = "                                        " &    ~
                         "                       (16)Exit Program"
                pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
                if fieldnr% > 1 then L40105
                   str(pf$(1),24,18) = " "  :  str(pfkeys$,4,1) = hex(ff)
                   goto L40115
L40105:         header$ = " "
L40115:         goto L40200

            deffn'211(fieldnr%)    /* Editmode logic... */
                init(hex(86)) lfac$() : lfac$(1) = hex(8c)
                header$ = " "
                pf$(1) = "(1)Start Over                           " &    ~
                         "(12)Delete Employee    (13)Instructions"
                pf$(2) = "(2)Earnings            (5)Next Page     " &    ~
                         "                       (15)Print Screen"
                pf$(3) = "                                        " &    ~
                         "                       (16)Save Data   "
                pfkeys$ = hex(0102ffff05ffffffffffff0c0dff0f1000)
                if fieldnr% = 0 then L40200
                   str(pf$(2),,60) = " "
                   str(pfkeys$,2,1) = hex(ff) : str(pfkeys$,2,1) = hex(ff)
                   init(hex(8c)) lfac$()

L40200:           str(header$,62) = "JBEMPINP: " & cms2v$
                  str(pf$(3),63,1) = hex(84)
                  on fieldnr% goto  L40290,         /* Employee code    */~
                                    L40290,         /* Current Status   */~
                                    L40290,         /* Names            */~
                                    L40290,         /* SS Number        */~
                                    L40290,         /* Department       */~
                                    L40290,         /* Job Title        */~
                                    L40290,         /* Overhead Account */~
                                    L40290,         /* Labor Class      */~
                                    L40290,         /* Earnings Rate    */~
                                    L40290          /* Shift            */
                     goto L40345
                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      goto L40345
L40290:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      goto L40345
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      goto L40345


L40345:     accept                                                       ~
               at (01,02), "SFC Employee Master Management",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Employee Code",                              ~
               at (06,33), fac(lfac$( 1)), empcode$             , ch(12),~
               at (06,48), fac(hex(8c)), empname$               , ch(32),~
                                                                         ~
               at (07,02), "Current Status",                             ~
               at (07,33), fac(lfac$( 2)), status$              , ch(01),~
               at (07,48), fac(hex(8c)), statusdescr$           , ch(18),~
                                                                         ~
               at (08,02), "Last Name",                                  ~
               at (08,33), fac(lfac$( 3)), lname$               , ch(15),~
                                                                         ~
               at (09,02), "First Name",                                 ~
               at (09,33), fac(lfac$( 3)), fname$               , ch(10),~
                                                                         ~
               at (10,02), "Middle Initial",                             ~
               at (10,33), fac(lfac$( 3)), mname$               , ch(01),~
                                                                         ~
               at (11,02), "Social Security Number",                     ~
               at (11,33), fac(lfac$( 4)), ssnumber$            , ch(11),~
                                                                         ~
               at (12,02), "Department",                                 ~
               at (12,33), fac(lfac$( 5)), dept$                , ch(04),~
               at (12,48), fac(hex(8c)),   deptdescr$           , ch(32),~
                                                                         ~
               at (13,02), "Current Job Title",                          ~
               at (13,33), fac(lfac$( 6)), curjob$              , ch(16),~
                                                                         ~
               at (14,02), "Applied Overhead Account",                   ~
               at (14,33), fac(lfac$( 7)), overacct$            , ch(12),~
               at (14,48), fac(hex(8c)), overacctdescr$         , ch(32),~
                                                                         ~
               at (15,02), "Labor Class (Overhead Rate)",                ~
               at (15,33), fac(lfac$( 8)), laborclas$           , ch(04),~
               at (15,48), fac(hex(8c)), labordescr$            , ch(32),~
                                                                         ~
               at (16,02), "$/Hr for External Job Posting",              ~
               at (16,33), fac(lfac$( 9)), jrate$               , ch(10),~
                                                                         ~
               at (17,02), "Normal Shift (1-4)",                         ~
               at (17,33), fac(lfac$(10)), shift$               , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L40660
                  call "MANUAL" ("JBEMPINP")
                  goto L40345

L40660:        if keyhit% <> 15 then L40680
                  call "PRNTSCRN"
                  goto L40345

L40680:        if editmode% = 0 then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return


        REM *************************************************************~
            *         E A R N I N G S   T A B L E   S C R E E N         *~
            *-----------------------------------------------------------*~
            * Inputs/edits/inserts/deletes earnings table.              *~
            *************************************************************

            deffn'204(screenline%, fieldnr%)
                  screen% = 1
                  gosub setpf204
                  goto L42340

            deffn'214(screenline%, fieldnr%)
                  screen% = 2
                  gosub setpf214
                  init(hex(86)) fac$()
                  if fieldnr% = 0 then L42350
                  init(hex(8c)) fac$()
                  init(hex(84)) fac$(screenline%, fieldnr%)
                  goto L42350

            deffn'224(screenline%, fieldnr%)
                  screen% = 3
                  gosub setpf224
                  goto L42340

            deffn'234(screenline%)
                  screen% = 4
                  gosub setpf234
                  init(hex(8c)) fac$()
                  for temp% = 1 to 5
                      fac$(screenline%, temp%) = hex(94)
                      next temp%
                  goto L42560

L42340:           init(hex(84)) fac$()
L42350:           on fieldnr% goto  L42500,         /* Category   code  */~
                                    L42500,         /* Earnings type    */~
                                    L42500,         /* Units description*/~
                                    L42530,         /* Rate per unit    */~
                                    L42500          /* Account number   */
                  goto L42560

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      fac$(screenline%, fieldnr%) = hex(80)
                      goto L42560
L42500:           REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(81)
                      goto L42560
L42530:           REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(82)

L42560:     init(" ")  str(header$,,60)
            str(header$,1,60) = "Employee: " & empcode$ & "  " & empname$

            accept                                                       ~
               at (01,02), "SFC Employee Earnings Code Management",      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)), hdr$(1)                , ch(08),~
               at (05,13), fac(hex(ac)), hdr$(2)                , ch(12),~
               at (05,28), fac(hex(ac)), hdr$(3)                , ch(12),~
               at (05,43), fac(hex(ac)), hdr$(4)                , ch(10),~
               at (05,56), fac(hex(ac)), hdr$(5)                , ch(15),~
                                                                         ~
               at (06,04), fac(fac$(1, 1)), erncata$  (line%+ 1), ch(04),~
               at (07,04), fac(fac$(2, 1)), erncata$  (line%+ 2), ch(04),~
               at (08,04), fac(fac$(3, 1)), erncata$  (line%+ 3), ch(04),~
               at (09,04), fac(fac$(4, 1)), erncata$  (line%+ 4), ch(04),~
               at (10,04), fac(fac$(5, 1)), erncata$  (line%+ 5), ch(04),~
               at (11,04), fac(fac$(6, 1)), erncata$  (line%+ 6), ch(04),~
               at (12,04), fac(fac$(7, 1)), erncata$  (line%+ 7), ch(04),~
               at (13,04), fac(fac$(8, 1)), erncata$  (line%+ 8), ch(04),~
               at (14,04), fac(fac$(9, 1)), erncata$  (line%+ 9), ch(04),~
               at (15,04), fac(fac$(10,1)), erncata$  (line%+10), ch(04),~
               at (16,04), fac(fac$(11,1)), erncata$  (line%+11), ch(04),~
               at (17,04), fac(fac$(12,1)), erncata$  (line%+12), ch(04),~
               at (18,04), fac(fac$(13,1)), erncata$  (line%+13), ch(04),~
               at (19,04), fac(fac$(14,1)), erncata$  (line%+14), ch(04),~
               at (20,04), fac(fac$(15,1)), erncata$  (line%+15), ch(04),~
                                                                         ~
               at (06,13), fac(fac$(1, 2)), erntype$  (line%+ 1), ch(12),~
               at (07,13), fac(fac$(2, 2)), erntype$  (line%+ 2), ch(12),~
               at (08,13), fac(fac$(3, 2)), erntype$  (line%+ 3), ch(12),~
               at (09,13), fac(fac$(4, 2)), erntype$  (line%+ 4), ch(12),~
               at (10,13), fac(fac$(5, 2)), erntype$  (line%+ 5), ch(12),~
               at (11,13), fac(fac$(6, 2)), erntype$  (line%+ 6), ch(12),~
               at (12,13), fac(fac$(7, 2)), erntype$  (line%+ 7), ch(12),~
               at (13,13), fac(fac$(8, 2)), erntype$  (line%+ 8), ch(12),~
               at (14,13), fac(fac$(9, 2)), erntype$  (line%+ 9), ch(12),~
               at (15,13), fac(fac$(10,2)), erntype$  (line%+10), ch(12),~
               at (16,13), fac(fac$(11,2)), erntype$  (line%+11), ch(12),~
               at (17,13), fac(fac$(12,2)), erntype$  (line%+12), ch(12),~
               at (18,13), fac(fac$(13,2)), erntype$  (line%+13), ch(12),~
               at (19,13), fac(fac$(14,2)), erntype$  (line%+14), ch(12),~
               at (20,13), fac(fac$(15,2)), erntype$  (line%+15), ch(12),~
                                                                         ~
               at (06,30), fac(fac$(1, 3)), ernunits$ (line%+ 1), ch(06),~
               at (07,30), fac(fac$(2, 3)), ernunits$ (line%+ 2), ch(06),~
               at (08,30), fac(fac$(3, 3)), ernunits$ (line%+ 3), ch(06),~
               at (09,30), fac(fac$(4, 3)), ernunits$ (line%+ 4), ch(06),~
               at (10,30), fac(fac$(5, 3)), ernunits$ (line%+ 5), ch(06),~
               at (11,30), fac(fac$(6, 3)), ernunits$ (line%+ 6), ch(06),~
               at (12,30), fac(fac$(7, 3)), ernunits$ (line%+ 7), ch(06),~
               at (13,30), fac(fac$(8, 3)), ernunits$ (line%+ 8), ch(06),~
               at (14,30), fac(fac$(9, 3)), ernunits$ (line%+ 9), ch(06),~
               at (15,30), fac(fac$(10,3)), ernunits$ (line%+10), ch(06),~
               at (16,30), fac(fac$(11,3)), ernunits$ (line%+11), ch(06),~
               at (17,30), fac(fac$(12,3)), ernunits$ (line%+12), ch(06),~
               at (18,30), fac(fac$(13,3)), ernunits$ (line%+13), ch(06),~
               at (19,30), fac(fac$(14,3)), ernunits$ (line%+14), ch(06),~
               at (20,30), fac(fac$(15,3)), ernunits$ (line%+15), ch(06),~
                                                                         ~
               at (06,43), fac(fac$(1, 4)), ernrate$  (line%+ 1), ch(10),~
               at (07,43), fac(fac$(2, 4)), ernrate$  (line%+ 2), ch(10),~
               at (08,43), fac(fac$(3, 4)), ernrate$  (line%+ 3), ch(10),~
               at (09,43), fac(fac$(4, 4)), ernrate$  (line%+ 4), ch(10),~
               at (10,43), fac(fac$(5, 4)), ernrate$  (line%+ 5), ch(10),~
               at (11,43), fac(fac$(6, 4)), ernrate$  (line%+ 6), ch(10),~
               at (12,43), fac(fac$(7, 4)), ernrate$  (line%+ 7), ch(10),~
               at (13,43), fac(fac$(8, 4)), ernrate$  (line%+ 8), ch(10),~
               at (14,43), fac(fac$(9, 4)), ernrate$  (line%+ 9), ch(10),~
               at (15,43), fac(fac$(10,4)), ernrate$  (line%+10), ch(10),~
               at (16,43), fac(fac$(11,4)), ernrate$  (line%+11), ch(10),~
               at (17,43), fac(fac$(12,4)), ernrate$  (line%+12), ch(10),~
               at (18,43), fac(fac$(13,4)), ernrate$  (line%+13), ch(10),~
               at (19,43), fac(fac$(14,4)), ernrate$  (line%+14), ch(10),~
               at (20,43), fac(fac$(15,4)), ernrate$  (line%+15), ch(10),~
                                                                         ~
               at (06,57), fac(fac$(1, 5)), ernacct$  (line%+ 1), ch(12),~
               at (07,57), fac(fac$(2, 5)), ernacct$  (line%+ 2), ch(12),~
               at (08,57), fac(fac$(3, 5)), ernacct$  (line%+ 3), ch(12),~
               at (09,57), fac(fac$(4, 5)), ernacct$  (line%+ 4), ch(12),~
               at (10,57), fac(fac$(5, 5)), ernacct$  (line%+ 5), ch(12),~
               at (11,57), fac(fac$(6, 5)), ernacct$  (line%+ 6), ch(12),~
               at (12,57), fac(fac$(7, 5)), ernacct$  (line%+ 7), ch(12),~
               at (13,57), fac(fac$(8, 5)), ernacct$  (line%+ 8), ch(12),~
               at (14,57), fac(fac$(9, 5)), ernacct$  (line%+ 9), ch(12),~
               at (15,57), fac(fac$(10,5)), ernacct$  (line%+10), ch(12),~
               at (16,57), fac(fac$(11,5)), ernacct$  (line%+11), ch(12),~
               at (17,57), fac(fac$(12,5)), ernacct$  (line%+12), ch(12),~
               at (18,57), fac(fac$(13,5)), ernacct$  (line%+13), ch(12),~
               at (19,57), fac(fac$(14,5)), ernacct$  (line%+14), ch(12),~
               at (20,57), fac(fac$(15,5)), ernacct$  (line%+15), ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key  (keyhit%)

               if keyhit% <> 13 then L43590
                  call "MANUAL" ("JBEMPINP")
                  goto L42560
L43590:        if keyhit% <> 15 then L43620
                  call "PRNTSCRN"
                  goto L42560
L43620:        if screen% <> 2% and screen% <> 3% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        setpf204
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "(2)Start Line Over     (4)Line Above    " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Edit Mode   "
            pfkeys$ = hex(0102ff04ffffffffffffffff0dff0f1000)
            return

        setpf214
            pf$(1) = "(1)Start Over                  (9)Header" &        ~
                     "       (10)Super Insrt (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "       (11)Insert      (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "       (12)Delete      (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffff090a0b0c0dff0f1000)

            if line% = 0% then L43940
                str(pf$(2), 1,15) = "(2)First"
                str(pfkeys$,2,1)  = hex(02)
                str(pf$(2),16,15) = "(4)Previous"
                str(pfkeys$,4,1)  = hex(04)
                str(pf$(2),32,15) = "(6)Down"
                str(pfkeys$,6,1)  = hex(06)

L43940:         if maxlines% <= line% + 15% then L44020
                str(pf$(3), 1,15) = "(3)Last"
                str(pfkeys$,3,1)  = hex(03)
                str(pf$(3),16,15) = "(5)Next"
                str(pfkeys$,5,1)  = hex(05)
                str(pf$(3),32,15) = "(7)Up"
                str(pfkeys$,7,1)  = hex(07)

L44020:     return

         setpf224
            pf$(1) = "(1)Exit Insert Mode                     " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        setpf234
            pf$(1) = "(1)Cancel Delete                        " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

L45000: REM *************************************************************~
            *   B U I L D   E A R N I N G S   T A B L E   S C R E E N   *~
            *-----------------------------------------------------------*~
            * Screen to input categories for use in building the        *~
            * earnings table.                                           *~
            *************************************************************

            header$ = "Employee: " & empcode$ & "  " & empname$
            str(header$,62) = "JBEMPINP: " & cms2v$
            message$ = "Leave All Fields Blank To Enter Earnings Individu~
        ~ally"

L45050:     accept                                                       ~
               at (01,02), "Build SFC Employee Earnings Table",          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (08,27), "******************************",             ~
               at (09,27), "*    EARNINGS CATEGORIES:    *",             ~
               at (10,27), "*                            *",             ~
               at (11,27), "*                            *",             ~
               at (12,27), "*                            *",             ~
               at (13,27), "*                            *",             ~
               at (14,27), "*                            *",             ~
               at (15,27), "*                            *",             ~
               at (16,27), "*                            *",             ~
               at (17,27), "******************************",             ~
                                                                         ~
               at (11,30), fac(hex(81)), category$( 1)          , ch(04),~
               at (11,40), fac(hex(81)), category$( 2)          , ch(04),~
               at (11,50), fac(hex(81)), category$( 3)          , ch(04),~
               at (12,30), fac(hex(81)), category$( 4)          , ch(04),~
               at (12,40), fac(hex(81)), category$( 5)          , ch(04),~
               at (12,50), fac(hex(81)), category$( 6)          , ch(04),~
               at (13,30), fac(hex(81)), category$( 7)          , ch(04),~
               at (13,40), fac(hex(81)), category$( 8)          , ch(04),~
               at (13,50), fac(hex(81)), category$( 9)          , ch(04),~
               at (14,30), fac(hex(81)), category$(10)          , ch(04),~
               at (14,40), fac(hex(81)), category$(11)          , ch(04),~
               at (14,50), fac(hex(81)), category$(12)          , ch(04),~
               at (15,30), fac(hex(81)), category$(13)          , ch(04),~
               at (15,40), fac(hex(81)), category$(14)          , ch(04),~
               at (15,50), fac(hex(81)), category$(15)          , ch(04),~
               at (16,30), fac(hex(81)), category$(16)          , ch(04),~
               at (16,40), fac(hex(81)), category$(17)          , ch(04),~
               at (16,50), fac(hex(81)), category$(18)          , ch(04),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
                                                                         ~
               keys(hex(00010d0f)),                                      ~
               key (keyhit%)

               if keyhit% <> 13 then L45295
                  call "MANUAL" ("JBEMPINP")
                  goto L45050

L45295:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L45050

L46000: REM *************************************************************~
            *      S U P E R - I N S E R T   E D I T   S C R E E N      *~
            *-----------------------------------------------------------*~
            * Inputs codes of items to be super-inserted.               *~
            *************************************************************

            header$ = "Employee: " & empcode$ & "  " & empname$
            str(header$,62) = "JBEMPINP: " & cms2v$
L46080:     accept                                                       ~
               at (01,02), "SFC Super-Insert Earnings Categories" ,      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Category To Insert",                         ~
               at (06,30), fac(hex(81)), inscat$                , ch(04),~
                                                                         ~
               at (21,02), fac(hex(bc)), message$               , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), "(1)Return To Edit Mode",                     ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Return      ",                           ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L46310
                  call "MANUAL" ("JBEMPINP")
                  goto L46080

L46310:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L46080

        REM *************************************************************~
            *          T E S T   D A T A   F O R   P A G E   1          *~
            *-----------------------------------------------------------*~
            * TEsts data for page 1 of linear input                     *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$, infomsg$ = " "
                  on fieldnr% goto  L50210,         /* Employee code    */~
                                    L50310,         /* Status           */~
                                    L50360,         /* Names            */~
                                    L50410,         /* Social Security #*/~
                                    L50510,         /* Department       */~
                                    L50560,         /* Job Title        */~
                                    L50600,         /* Appl. Overhd acct*/~
                                    L50660,         /* Labor Class      */~
                                    L50750,         /* Rate for Job     */~
                                    L50780          /* Shift            */

L50210:     REM TEST DATA FOR EMPLOYEE CODE
                if empcode$ <> " " then L50240
                      call "GETEMPL" (#14, empcode$, " ", 0%, f1%(14))
                          if f1%(14) <> 0% then L50240
                             errormsg$ = "Employee Code CANNOT be Blank."
                             return
L50240:          gosub L30000
                 if f1%(14) = 1% then L50280
                 return
L50280:         if f1%(3) = 0% then return
                return clear all
                goto editmode
L50310:     REM TEST DATA FOR CURRENT STATUS
            if pos("CPLMN" = status$) = 0 then L50330
                gosub employee_status
                return
L50330:     errormsg$ = "Status must be C, P, L, M or N"
                return
L50360:     REM TEST DATA FOR NAME (LAST, FIRST, MIDDLE)
            if lname$ = " " then L50380
                gosub employee_name
                return
L50380:     errormsg$ = "Last Name CANNOT be Blank."
                return
L50410:     REM TEST DATA FOR SOCIAL SECURITY NUMBER
            if ssnumber$ = " " then L50480
            if str(ssnumber$,4,1) <> "-" or                              ~
               str(ssnumber$,7,1) <> "-" then L50460
            return
L50460:     errormsg$ = "Must be in Format ###-##-####"
            goto L50500
L50480:     errormsg$ = "Social Security Number must be Entered"
L50500:         return
L50510:     REM TEST DATA FOR DEPARTMENT
                if dept$ = " " then return
                call "GETCODE" (#6, dept$, deptdescr$, 1%, 0, f1%(6))
                if f1%(6) = 0% then errormsg$ = hex(00)
                return
L50560: REM Test for Job Title
            return
L50600:     REM TEST DATA FOR OVERHEAD ACCOUNT
                if overacct$ = " " then return
                   call "GETCODE" (#2, overacct$, overacctdescr$,        ~
                                                           1%, 0, f1%(2))
                if f1%(2) = 0% then errormsg$ = hex(00)
                   return
L50660:     REM TEST DATA FOR LABOR CLASS
                labordescr$ = " "
                if laborclas$ = " " then return
                readkey$ = "LBR CLASS" & laborclas$
                call "PLOWCODE"(#8, readkey$,labordescr$, 9%,.30, f1%(8))
                if f1%(8) <> 1% then L50720
                     laborclas$ = str(readkey$,10)
                     labordescr$ = "(" & labordescr$ & ")"
                     return
L50720:        errormsg$ = "Labor Class Code Not On File: " & laborclas$
                      return
L50750:     REM TEST DATA FOR RATE FOR POSTING JOB EXTERNALLY
                call "NUMTEST" (jrate$, 0, 999, errormsg$, 2.4, 0)
                    return
L50780:     REM TEST DATA FOR SHIFT
                if pos(" 1234" = shift$) > 0% then return
                     errormsg$ = "Enter 1, 2, 3, or 4"
                     return

        REM *************************************************************~
            *  T E S T   D A T A   F O R   E A R N I N G S   T A B L E  *~
            *-----------------------------------------------------------*~
            * Tests data for the entries in the earnings table.         *~
            *************************************************************

            deffn'154(fieldnr%)
                  errormsg$, infomsg$ = " "
                  on fieldnr% goto  L52190,         /* Category   code  */~
                                    L52290,         /* Earnings type    */~
                                    L52360,         /* Units description*/~
                                    L52400,         /* Rate per unit    */~
                                    L52600          /* Account number   */

L52190:     REM TEST DATA FOR CATEGORY   CODE
                      return
L52290:     REM TEST DATA FOR EARNINGS TYPE
                if erntype$(c%) = " " then L52322
                   temp$ = erntype$(c%)       /* CHECK */
                   erntype$(c%) = " "     /* FOR DUPLICATE */
                   search str(erntype$()) = str(temp$,,12) to cursor%()  ~
                                                                step 12
                   erntype$(c%) = temp$
                   if cursor%(1) = 0% then return
                errormsg$ = "Can't have the same type set up twice"
                return
L52322:         errormsg$ = "Blank Earnings Type Not Allowed"
                return
L52360:     REM TEST DATA FOR UNITS DESCRIPTION
                if ernunits$(c%) <> " " then return
                   errormsg$ = "Units Description May Not Be Blank"
                   return
L52400:     REM TEST DATA FOR RATE PER UNIT
                call "NUMTEST" (ernrate$(c%), 0, 9e7, errormsg$, -2.4, 0)
                if errormsg$ <> " " then return
            return
L52600:     REM TEST DATA FOR ACCOUNT NUMBER
                   call "GETCODE" (#2, ernacct$(c%), infomsg$, 1%, 0,    ~
                                                   f1%(2))
                   if f1%(2) <> 0% then return
                errormsg$ = "Expense Account Number May Not Be Blank"
                return

L55000: REM *************************************************************~
            *   T E S T   D A T A   O N   B U I L D   E A R N I N G S   *~
            *-----------------------------------------------------------*~
            * Makes sure all data in the build earnings screen is O.K.  *~
            *************************************************************

            errormsg$ = " "
            maxlines% = 0%

            for temp% = 1% to 18%
                if category$(temp%) = " " then L55200
                   count% = 0%
                   readkey$ = category$(temp%)
L55140:            call "PLOWNEXT" (#12, readkey$, 4%, f1%(12))
                        if f1%(12) = 0% then L55180
                           count% = count% + 1%
                   goto L55140
L55180:         if count% = 0% then L55250 /* CATEGORY NOT ON FILE       */
                   maxlines% = maxlines% + count%
L55200:         next temp%
                if maxlines% > 100% then L55290
                return

            REM ERROR MESSAGES FOR VARIOUS CONDITIONS OF BAD INPUT.
L55250:         errormsg$ = "Earnings Category Not On File: " &          ~
                                         category$(temp%)
                return

L55290:         errormsg$ = "There Will Be More Than 100 Entries In The E~
        ~arnings Table.  Please Delete Some"
                return

L57000: REM *************************************************************~
            * T E S T   D A T A   F O R   E A R N I N G S   S U P I N S *~
            *-----------------------------------------------------------*~
            * Tests data for earnings super insert and makes sure that  *~
            * the number of items in the category will not overflow the *~
            * array.                                                    *~
            *************************************************************

            errormsg$ = " "

               count% = 0%
               readkey$ = inscat$
               call "PLOWCODE" (#12, readkey$, " ", -4%, -0.001, f1%(12))
                    if f1%(12) = 0% then L57220
               inscat$ = str(readkey$,,4)
               count% = count% + 1%
L57130:        call "PLOWNEXT" (#12, readkey$, 4%, f1%(12))
                    if f1%(12) = 0 then L57170
                       count% = count% + 1%
               goto L57130
L57170:     if count% = 0% then L57220 /* CATEGORY NOT ON FILE       */
            if maxlines% > 100% then L57260
            return

            REM ERROR MESSAGES FOR VARIOUS CONDITIONS OF BAD INPUT.
L57220:         errormsg$ = "Earnings Category Not On File: " &          ~
                                         inscat$
                return

L57260:         errormsg$ = "There Will Be More Than 100 Entries In The E~
        ~arnings Table.  Please Delete Some"
                return

                errormsg$ = "Blank Category Code Not Permitted"
                return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Closes all the files currently open, and also displays    *~
            * a message (only if in foreground) while linking to the    *~
            * next program.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
