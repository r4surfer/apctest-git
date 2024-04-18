        REM *************************************************************~
            *                                                           *~
            *  H   H  N   N  Y   Y  W   W  DDDD   W   W   AAA   L       *~
            *  H   H  NN  N  Y   Y  W   W  D   D  W   W  A   A  L       *~
            *  HHHHH  N N N   YYY   W   W  D   D  W   W  AAAAA  L       *~
            *  H   H  N  NN    Y    W W W  D   D  W W W  A   A  L       *~
            *  H   H  N   N    Y     W W   DDDD    W W   A   A  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYWDWAL - Enter Withdrawals from Inventory posting them  *~
            *            to the Withdrawals buffer file for later update*~
            *            to the system Master files (see HNYWDWJN).     *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/10/80 ! ORIGINAL (PATTERNED AFTER HNYADDNS)      ! TEM *~
            * 02/04/81 ! NEW HNY LAYOUT                           ! TEM *~
            * 05/19/81 ! ADDED JOB COSTING (EXPANDED WDWBUFFR)    ! TOM *~
            * 06/17/83 ! DISALLOW BLANK DESCR, VALIDATE G/L,      ! JRW *~
            *          ! DISALLOW BLANK PART, MODIF OF L, M, O    ! JRW *~
            * 07/18/83 ! ADDED CALL TO MANUAL                     ! HES *~
            * 06/07/84 ! CORRECTED ARRAY OVERFLOW BUGS            ! LDJ *~
            * 12/06/85 ! Expanded arrays sizes, added buffer save ! ERN *~
            *          ! option. Fixed Lineabove.                 !     *~
            * 01/28/87 ! Changed HNYQUAN Select, added support for! LDJ *~
            *          !   Lot and S/N Tracking, changed WDWBUFFR !     *~
            *          !   Select, added SERTIF, SERMASTR, and    !     *~
            *          !   SERWORK Selects.                       !     *~
            * 03/26/87 ! Added call to HNYQDISP for store/lot     ! LDJ *~
            *          !   inquiry.                               !     *~
            * 05/12/87 ! Expand costs from 3 to 12; Std cost mods ! JIM *~
            *          !   made to files & screen                 !     *~
            * 04/25/88 ! Added Soft Enables                       ! TLJ *~
            * 02/06/89 ! Corrected call to CMSMACHK               ! MJB *~
            * 02/09/90 ! Added PF8 access to HNYLCSUB permitting  ! MLJ *~
            *          !  Location control.                       !     *~
            * 03/09/90 ! Changed LOCATION from 200 to 400.        ! MLJ *~
            * 06/19/91 ! PRR 11893- Moved Call to Location sub.   ! JBK *~
            * 03/31/93 ! PRR 12784 Add LOTVALID & LOTUNQUE calls. ! JIM *~
            * 04/27/93 ! Further Lot Tracking Integrity enhancemts! JIM *~
            * 04/28/93 ! PRRs 10716 & 11937 Honor Destination G/L ! JIM *~
            *          !   acct in SWITCHS.HNY.                   !     *~
            * 04/29/93 ! PRR 12461 A single character Description ! JIM *~
            *          !   drives a Descr lookup in GENCODES.     !     *~
            * 01/31/94 ! PRR 13094 Added UOM to info message.     ! JDH *~
            * 08/11/94 ! PRR 13269 Fixed SERMASTR 'p' if deleted. ! JDH *~
            * 02/09/95 ! PRR 13355 Fixed Lot tracking qty logic.  ! JDH *~
            * 06/25/96 ! Add blank date test var                  ! DER *~
            *************************************************************

        dim                                                              ~
            blank_date$8,                /* Blank date test            */~
            cost(12), cost$(900)96,      /* COST FIELDS                */~
            cursor%(2),                  /* Cursor location for editing*/~
            date$8,                      /* Formatted Date             */~
            datejobclosed$8,             /* Date job closed test       */~
            def_destn$9,                 /* Default Destination acct   */~
            delpart$(900)25,             /* Deleted part               */~
            delstore$(900)3,             /* Deleted store              */~
            dellot$(900)6,               /* Deleted store              */~
            delqty(900),                 /* Deleted quantity           */~
            descr$(900)32,               /* Free text descriptions     */~
            dstacct$(900)16,             /* Destination (HNY) account  */~
            errormsg$79,                 /* Error message text         */~
            fac$(5,20)1,                 /* Field attribute characters */~
            from$(900)16,                /* "where from" free text     */~
            header$(2)80,                /* Plowcode argument          */~
            hnydate$6,                   /* This users inventory date  */~
            hnyhold(900),                /* Currently saved quantity   */~
            i$(24)80,                    /* Screen image--not used     */~
            incl_excl(1),                /* PLOWCODE Argument          */~
            incl_excl$(1)10,             /* PLOWCODE Argument          */~
            index%(900),                 /* Serial #'s Work File Ptrs  */~
            infomsg$79,                  /* Informative message        */~
            jobnr$(900)8,                /* Job number                 */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            location$30,                 /* Serial # Location Key      */~
            lot$(900)16,                 /* Lot number                 */~
            lot_qty_on_hand$10,          /* Edited value               */~
            part$(900)25,                /* Part number this line      */~
            pfkeys$(4)18,                /* Pf keys for tabular screens*/~
            plowkey$99,                  /* Key for plow routines      */~
            qty$(900)10,                 /* Quantity added this line   */~
            readkey$99,                  /* Key for plow routines      */~
            separator$(5)79,             /* Separator lines for tables */~
            scr%(5,10), set%(255),       /* Soft Enable Tables         */~
            seq$(900)3,                  /* Sequence numbers for screen*/~
            srcacct$(900)16,             /* Source account             */~
            status$1,                    /* Serial Number Status Flag  */~
            store$(900)3,                /* Store number this line     */~
            totlcost$(900)10,            /* COST INFORMATION           */~
            tttle$(4,2)79,               /* Titles for tabular screens */~
            tran$(24)80,                 /* For edit computations      */~
            uom$4,                       /* Unit of Measure            */~
            userid$3                     /* Userid of current user     */

        dim f2%(64),                     /* File status flags for      */~
            f1%(64),                     /* Record-on-file flags       */~
            rslt$(64)20,                 /* Return code from "OPENFILE"*/~
            axd$(64)4                    /* Axd pointer from "OPENFILE"*/~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * #1  ! USERINFO ! Default information for this user        *~
            * #2  ! GLMASTR  ! General ledger master file (acct tests)  *~
            * #3  ! HNYMASTR ! Inventory master file                    *~
            * #4  ! HNYQUAN  ! Inventory store quantity file            *~
            * #5  ! HNYWDWTF ! Withdrawal buffer for inventory          *~
            * #6  ! STORNAME ! Store names and addresses                *~
            * #7  ! SYSFILE2 ! System information (months open)         *~
            * #8  ! JOBMASTR ! Wip/jc master file                       *~
            * #9  ! HNYPOOL  ! Inventory LIFO/FIFO Pool records         *~
            *#10  ! SERTIF   ! Additions buffer for inventory S/N's     *~
            *#11  ! SERWORK  ! Temporary Serial #'s Work File           *~
            *#12  ! SERMASTR ! Serial Number Tracking Master File       *~
            *#13  ! HNYLOCNS ! Location Quantity Detail File            *~
            *#14  ! LOCATION ! Location Master File                     *~
            *#20  ! GENCODES ! Control System Codes File                *~
            *************************************************************

            select  #1, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1,  keylen = 3

            select  #2, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select  #3, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 900,                                   ~
                        keypos = 1, keylen = 25,                         ~
                        alternate key 1, keypos = 102, keylen = 9, dup,  ~
                                  key 2, keypos = 90,  keylen = 4, dup

            select  #4, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos =  17, keylen = 44,                       ~
                        alternate key 1, keypos =  1, keylen = 44

            select  #5, "HNYWDWTF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 6

            select  #6, "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 3

            select  #7, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20


            select  #8, "JOBMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 8

            select  #9, "HNYPOOL",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 38

            select #10, "SERTIF",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 62

            select #11, "SERWORK",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  48,                                   ~
                        keypos = 1, keylen = 23

            select #12, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #13, "HNYLOCNS",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =  443, keylen =  42,         ~
                            key  2, keypos =  485, keylen =  42,         ~
                            key  3, keypos =  527, keylen =  42,         ~
                            key  4, keypos =  590, keylen =  42

            select #14, "LOCATION",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =    4, keylen =  11

            select #20, "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos =    1,  keylen = 24

            call "SHOSTAT"  ("Opening Files, One Moment Please...")
            rslt$(1),rslt$(3), rslt$(4), rslt$(6), rslt$(7) = "REQUIRED"
            call "OPENFILE" (# 1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
            call "OPENFILE" (# 2, "SHARE", f2%( 2), rslt$( 2), axd$( 2))
            call "OPENFILE" (# 3, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "OPENFILE" (# 4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))
            call "OPENFILE" (# 5, "SHARE", f2%( 5), rslt$( 5), axd$( 5))
            call "OPENFILE" (# 6, "SHARE", f2%( 6), rslt$( 6), axd$( 6))
            call "OPENFILE" (# 7, "SHARE", f2%( 7), rslt$( 7), axd$( 7))
            call "OPENFILE" (# 8, "SHARE", f2%( 8), rslt$( 8), axd$( 8))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))
            call "OPENFILE" (#13, "SHARE", f2%(13), rslt$(13), axd$(13))
            call "OPENFILE" (#14, "SHARE", f2%(14), rslt$(14), axd$(14))
            call "OPENFILE" (#20, "SHARE", f2%(20), rslt$(20), axd$(20))

            if f2%(1) + f2%(3) + f2%(6) + f2%(7) <> 0% then exit_program
            if f2%(5) = 0 then L03190
               call "OPENFILE" (#5, "OUTPT", f2%(5), rslt$(5), axd$(5))
               close #5
               call "OPENFILE" (#5, "SHARE", f2%(5), rslt$(5), axd$(5))

L03190:     if f2%(13) = 0 then L03240
               call "OPENFILE" (#13, "OUTPT",f2%(13),rslt$(13),axd$(13))
               close #13
               call "OPENFILE" (#13, "SHARE",f2%(13),rslt$(13),axd$(13))

L03240:     if f2%(14) = 0 then L09000
               call "OPENFILE" (#14, "OUTPT",f2%(14),rslt$(14),axd$(14))
               close #14
               call "OPENFILE" (#14, "SHARE",f2%(14),rslt$(14),axd$(14))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Gets userid of this user, sets dates, and tries to read   *~
            * in the entries already in the buffer (so that we can save *~
            * what was out there in case the program bombs)             *~
            *************************************************************
				blank_date$ = " "
				call "DATUFMTC" ( blank_date$ )

        REM See if User is an administator...
            call "CMSMACHK" ("HNY", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then admin% = 1%         ~
                                                else admin% = 0%

*        Get the Default Withdrawals Destination G/L Acct, if any.
            call "READ100" (#7, "SWITCHS.HNY", f1%(7%))    /* SYSFILE2 */
            if f1%(7%) <> 0% then get #7 using L09110, def_destn$
L09110:         FMT POS(130), CH(9)

            gosub init_enables
            ll% = 6%      /* Default max lot length        */
        REM Retrieve inventory date or exit...
                call "EXTRACT" addr ("ID", userid$)
                call "READ100" (#1, userid$, f1%(1))
                      if f1%(1) = 1 then L09220
                call "ASKUSER" (0%, "***ERROR***",                       ~
                     "Unable to locate you Inventory Posting Date!",     ~
                     " ", "Press RETURN to acknowledge and Exit.")
                       go to exit_program
L09220:         get #1, using L09230, hnydate$
L09230:                 FMT XX(27), CH(6)
                date$ = hnydate$
                call "DATEFMT" (date$)
                call "WHICHMON" (#7, hnydate$, thismonth%)
                if thismonth% > 0 and thismonth% < 4 then L09360
                call "ASKUSER" (0%, "INVALID POSTING DATE",              ~
                     "Your Inventory Posting Date is not within the " &  ~
                     "posting window.",                                  ~
                     "Please change your posting date & try again.",     ~
                     "Press RETURN to acknowledge and Exit.")
                      go to exit_program

        REM SET SCREEN TITLES...
L09360:     tttle$(1,2) = "(1)Start Over (2)Col 1 (6)Copy (8)Locations (1~
        ~0)Stores/Lots (16)Exit/Edit Mode"
            tttle$(1,1) = "Enter Withdrawals From Inventory"
            str(tttle$(1,1),61%) = "Post Date: " & date$
            tttle$(2,1) = "(1)Start Over (2)First (3)Last (4)Prev (5)Next~
        ~ (6)Down (7)Up"
            tttle$(2,2) = "(8)Locations (10)Stores/Lots (11)Insert (12)De~
        ~lete (13)Instr (16)Save (32)Updte"
            tttle$(3,1) = "Supply requested items and (RETURN) or (1) to ~
        ~Exit Insert Mode"
            tttle$(4,1) = "Press (RETURN) to delete flashing line or (1) ~
        ~to exit Delete."

            pfkeys$(1) = hex(00010206080a0d0f10ffffffffffffffffff)
            pfkeys$(2) = hex(000102030405060708090a0b0c0d0f10201d)
            pfkeys$(3) = hex(00010a0fffffffffffffffffffffffffffff)
            pfkeys$(4) = hex(00010a0fffffffffffffffffffffffffffff)

            REM Set TRAN$ for edit mode field number computation.
                init(hex(00)) str(tran$(),     1)
                init(hex(01)) str(tran$(6),  1, 35)
                init(hex(02)) str(tran$(6), 36, 10)
                init(hex(03)) str(tran$(6), 46, 12)
                init(hex(04)) str(tran$(6), 57, 17)
                init(hex(06)) str(tran$(7), 43, 17)
                init(hex(07)) str(tran$(7), 63, 15)
                init(hex(08)) str(tran$(8),  1, 22)
                init(hex(09)) str(tran$(8), 26, 38)
                init(hex(0a)) str(tran$(8), 66, 13)

                copy str(tran$(), 321, 1360) to str(tran$(), 641)

        REM *************************************************************~
            *     I N P U T   I N V E N T O R Y   A D D I T I O N S     *~
            *-----------------------------------------------------------*~
            * Handles non-standard tabular input mode (there's no linear*~
            * input in this program).                                   *~
            *************************************************************

        inputmode
            init(" ") seq$(), part$(), descr$(), qty$(), totlcost$(),    ~
                      srcacct$(), dstacct$(),                            ~
                      from$(), store$(), errormsg$, infomsg$, lot$(),    ~
                      jobnr$()
            mat hnyhold = zer : mat cost = zer : init (hex(00)) cost$()
            maxlines%, line%, screenline%, currentline%, index%, edit%,  ~
            delmax% = 0%
            gosub L30000   /* Load records from buffer        */
            if old% = 1% then editmode

L10180:     screenline% = screenline% + 1
            if screenline% < 6 then L10220
               line% = line% + 5
               screenline% = 1
L10220:     currentline% = line% + screenline%
            index% = index% + 1%
            index%(currentline%) = index%
            if currentline% > 899 then L11000
            call "SETSEP" (separator$(), line%, screenline%)

            for fieldnr% = 1 to 10
L10290:         gosub'163(fieldnr%, 1%)  /* Get special defaults.      */
                      if enabled% =  0 then       L10410
L10310:         gosub'203(screenline%, fieldnr%)
                      if keyhit%  =  0 then       L10410
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  2 then gosub columnone
                      if keyhit%  =  6 then gosub lineabove
                      if keyhit%  =  6 then L10410
                      if keyhit%  <> 8 then L10370
                          gosub locations  :  goto L10290
L10370:               if keyhit%  = 16 and fieldnr% = 1                  ~
                                and currentline% = 1 then exit_program
                      if keyhit%  = 16 and fieldnr% = 1 then editmode
                         goto L10290
L10410:         gosub'153(fieldnr%)
                      if errormsg$ <> " " then L10310
            next fieldnr%
            maxlines% = maxlines% + 1
            convert currentline% to seq$(currentline%), pic(###)
            goto L10180

L11000: REM *************************************************************~
            *      E D I T   I N V E N T O R Y   W I T H D R A W A L S  *~
            *-----------------------------------------------------------*~
            * Edits inventory withdraws and also permits insert and     *~
            * delete to occur.                                          *~
            *************************************************************

        editmode
            line%, currentline%, screenline% = 0
            errormsg$, infomsg$ = " "
            call"SETSEP"(separator$(),line%,min(maxlines%-line%,5%))
L11110:     edit% = 1%
L11120:     gosub'213(0%, 0%)
                  if keyhit%  =  0 then       L11320
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then line% = 0
                  if keyhit%  =  3 then line% = max(0,maxlines%-5)
                  if keyhit%  =  4 then line% = max(0,line%-4)
                  if keyhit%  =  5 then line% = min(line%+4,max(0,       ~
                                                maxlines%-5))
                  if keyhit%  =  6 then line% = max(0,line%-1)
                  if keyhit%  =  7 then line% = min(line%+1,max(0,       ~
                                                maxlines%-5))
                  if keyhit%  =  8 then       L11320
                  if keyhit%  =  9 then       editmode
                  if keyhit%  = 11 then gosub insertmode
                  if keyhit%  = 12 then gosub deletemode
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 29 then       L11320
                  if keyhit%  = 32 then       datasave
                  call"SETSEP"(separator$(),line%,min(maxlines%-line%,5%))
                  goto L11110

L11320:     REM Now figure out which field he hit.
                fieldnr% = val(str(tran$(cursor%(1)),cursor%(2)))
                if keyhit% <> 29% then L11390
                   gosub'049(1%, fieldnr%)
                   goto editmode
                if fieldnr% < 4% then L11120 /* Can't chg because of S/N */
                if fieldnr% > 10% then L11120 /* Invalid field selected */
L11390:         screenline% = (cursor%(1)-5)/4+1
                currentline% = line% + screenline%
                if currentline% > maxlines% then L11120
                if keyhit% <> 8 then L11430
                     gosub locations  :  goto L11110

L11430:         gosub'163(fieldnr%, 2%)    /* Test Enables             */
                if enabled% = 0% then editmode
L11450:         gosub'213(screenline%, fieldnr%)
                      if keyhit%  = 1 then gosub startover
                      if keyhit% <> 0 and keyhit% <> 29% then L11450
                gosub'153(fieldnr%)
                      if errormsg$ <> " " then L11450
                goto L11120

        REM *************************************************************~
            *  C O L U M N   O N E ,   L I N E   A B O V E   L O G I C  *~
            *-----------------------------------------------------------*~
            * Contains the code for column one and line above functions.*~
            *************************************************************

        lineabove
            if currentline% = 1 then return
            c% = currentline%
            on fieldnr% gosub L14210,               /* Part number      */~
                              L14220,               /* Store number     */~
                              L14230,               /* Lot number       */~
                              L14240,               /* Quantity added   */~
                              L14250,               /* cost             */~
                              L14300,               /* Source account   */~
                              L14310,               /* Destination acct */~
                              L14320,               /* Where from text  */~
                              L14330,               /* Description      */~
                              L14340                /* Job number       */
            return

L14210:     part$    (c%) = part$    (c%-1): return
L14220:     store$   (c%) = store$   (c%-1): return
L14230:     lot$     (c%) = lot$     (c%-1): return
L14240:     qty$     (c%) = qty$     (c%-1): return
L14250:     totlcost$(c%) = totlcost$(c%-1)
                get cost$(c%-1) using L14280, cost()
                put cost$(c%) using L14280, cost()
L14280:              FMT 12*PD(14,4)
                return
L14300:     srcacct$ (c%) = srcacct$ (c%-1): return
L14310:     dstacct$ (c%) = dstacct$ (c%-1): return
L14320:     from$    (c%) = from$    (c%-1): return
L14330:     descr$   (c%) = descr$   (c%-1): return
L14340:     jobnr$   (c%) = jobnr$   (c%-1): return

        columnone
            c% = currentline%
            if c% <= 0 then return
            init(" ") seq$(c%), part$(c%), descr$(c%), qty$(c%),         ~
                      totlcost$(c%),                                     ~
                      srcacct$(c%), dstacct$(c%), from$(c%), lot$(c%),   ~
                      store$(c%), jobnr$(c%), errormsg$
            init (hex(00)) cost$(c%)
            call "SERSTOVR" (index%(c%), "0", "2",  #12, #11)
            fieldnr% = 1
            return

        REM *************************************************************~
            *              I N S E R T   M O D E   C O D E              *~
            *-----------------------------------------------------------*~
            * Handles insertion of a line item.                         *~
            *************************************************************

        insertmode
L15070:     if maxlines% = 900 then return         /* Array full, can't*/
            REM Otherwise, set currentline%, screenline%, and copy right
                screenline% = max(int((cursor%(1)-1%)/4%),1%)
                if line% + screenline% < maxlines% then L15120
                   screenline% = maxlines% - line% /* To ins at end    */
L15120:         if screenline% <> 5 then L15170     /* Bottom of page   */
                   line% = line% + 1
                   screenline% = screenline% - 1
                   goto L15170

L15170:         call "SETSEP" (separator$(),line%,                       ~
                                      min(5%, maxlines%-line%+1%))
                currentline%, c% = screenline% + line%

            REM Copy all the elements up one
                if c% >= maxlines% then L15410
                for temp% = maxlines% to c% step -1
                    part$    (temp%+1) = part$    (temp%)
                    store$   (temp%+1) = store$   (temp%)
                    lot$     (temp%+1) = lot$     (temp%)
                    descr$   (temp%+1) = descr$   (temp%)
                    qty$     (temp%+1) = qty$     (temp%)
                    hnyhold  (temp%+1) = hnyhold  (temp%)
                    totlcost$(temp%+1) = totlcost$(temp%)
                          get cost$(temp%) using L15330, cost()
                          put cost$(temp%+1) using L15330, cost()
L15330:                        FMT 12*PD(14,4)
                    srcacct$ (temp%+1) = srcacct$ (temp%)
                    dstacct$ (temp%+1) = dstacct$ (temp%)
                    from$    (temp%+1) = from$    (temp%)
                    jobnr$   (temp%+1) = jobnr$   (temp%)
                    index%   (temp%+1) = index%   (temp%)
                next temp%

L15410:         screenline% = screenline% + 1
                c%, currentline% = currentline% + 1
                index% = index% + 1%
                index%(c%) = index%

                init(" ") part$(c%), descr$(c%), qty$(c%), totlcost$(c%),~
                          srcacct$(c%),                                  ~
                          dstacct$(c%), from$(c%), seq$(c%), lot$(c%),   ~
                          store$(c%), jobnr$(c%)
                hnyhold(c%) = 0 : init (hex(00)) cost$(c%)
            REM Now input the line, make so we can cancel out if necc
                infomsg$ = " " : edit% = 0%
                for fieldnr% = 1 to 10
                    gosub'163(fieldnr%, 1%)
                          if enabled% = 0 then L15590
L15560:             gosub'223(screenline%, fieldnr%)
                          if keyhit%  =  1 then L15730
                          if keyhit% <>  0 then L15560
L15590:             gosub'153(fieldnr%)
                          if errormsg$ <> " " then L15560
                next fieldnr%

                maxlines% = maxlines% + 1
                REM RENUMBER ITEM NUMBERS, THEN SET KLUGE CURSOR%(1)
                    if part$(currentline%) = " " then L15700
                       for temp% = 1 to max(maxlines%, currentline%+1)
                           if part$(temp%) = " " then L15690
                              convert temp% to seq$(temp%), pic(###)
L15690:                next temp%
L15700:         cursor%(1) = min(cursor%(1)+4, 24)
                goto L15070

L15730:     REM This routine aborts insert mode and destroys screenline%
                c% = currentline%
                if fieldnr% > 1% then                                    ~
                   call "SERSTOVR" (index%(c%), "0", "2",  #12, #11)
                gosub L15910              /* Actually delete @C%        */

                temp% = maxlines% + 1%
                init(" ") seq$(temp%), part$(temp%), descr$(temp%),      ~
                          qty$(temp%), totlcost$(temp%),                 ~
                          srcacct$(temp%), dstacct$(temp%), from$(temp%),~
                          lot$(temp%), store$(temp%), jobnr$(temp%),     ~
                          errormsg$, infomsg$
                hnyhold(temp%) = 0 : init (hex(00)) cost$(temp%)
                edit% = 1%
            if currentline% >= maxlines% and screenline% = 5%            ~
               then line% = max(0%, line% - 1%)
            return

L15910:     for temp% = currentline% to maxlines%
                part$     (temp%) = part$     (temp%+1)
                store$    (temp%) = store$    (temp%+1)
                lot$      (temp%) = lot$      (temp%+1)
                seq$      (temp%) = seq$      (temp%+1)
                descr$    (temp%) = descr$    (temp%+1)
                qty$      (temp%) = qty$      (temp%+1)
                hnyhold   (temp%) = hnyhold   (temp%+1)
                totlcost$ (temp%) = totlcost$ (temp%+1)
                     get cost$(temp%+1) using L16020, cost()
                     put cost$(temp%) using L16020, cost()
L16020:                   FMT 12*PD(14,4)
                srcacct$  (temp%) = srcacct$  (temp%+1)
                dstacct$  (temp%) = dstacct$  (temp%+1)
                from$     (temp%) = from$     (temp%+1)
                jobnr$    (temp%) = jobnr$    (temp%+1)
                index%    (temp%) = index%    (temp%+1)
            next temp%

            if maxlines% = 0 then return
               for temp% = 1 to max(maxlines%, currentline%)
                   if part$(temp%) = " " then L16140
                      convert temp% to seq$(temp%), pic(###)
L16140:        next temp%
            return

        REM *************************************************************~
            *              D E L E T E   M O D E   C O D E              *~
            *-----------------------------------------------------------*~
            * Deletes a line item from the invoice.  Note that we have  *~
            * to swipe the routine from insert mode that copies all the *~
            * data back down one line, which we will use here to delete *~
            * the actual line we want to.  this makes it easier to add  *~
            * fields to the line items since there's only one delete    *~
            * routine, although it breaks up the structure of the code. *~
            *************************************************************

        deletemode
                if maxlines% = 0 then return
                screenline% = int((cursor%(1)-1)/4)
                if screenline% < 1 then return
                currentline% = screenline% + line%
                if currentline% > maxlines% then return
                if hnyhold(currentline%) = 0 then L17200
                   if delmax% > 899% then return

L17200:         gosub'233(screenline%)
                      if keyhit%  =  1 then       return
                      if keyhit% <>  0 then       L17200

                c% = currentline%
                call "SERSTOVR" (index%(c%), "0", "2",  #12, #11)
                if hnyhold(c%) = 0 then L17340
                call "HNYHOLD" (#4, part$(c%), store$(c%), lot$(c%),     ~
                                    -hnyhold(c%), return%)
                delmax% = delmax% + 1%
                delpart$ (delmax%) = part$ (c%)
                delstore$(delmax%) = store$(c%)
                dellot$  (delmax%) = lot$  (c%)
                delqty   (delmax%) = hnyhold(c%)
L17340:         if currentline% < maxlines% then gosub L15910
                                         /* ACTUALLY DELETE LINE @C%   */
                temp% = maxlines%
                init(" ") seq$(temp%), part$(temp%), descr$(temp%),      ~
                          qty$(temp%), totlcost$(temp%), srcacct$(temp%),~
                          dstacct$(temp%), from$(temp%), lot$(temp%),    ~
                          errormsg$, infomsg$, store$(temp%),            ~
                          jobnr$(temp%)
                hnyhold(temp%) = 0 : init (hex(00)) cost$(temp%)
                maxlines% = maxlines% - 1%
                if currentline% >= maxlines% and screenline% = 5%        ~
                   then line% = max(0%, line% - 1%)
                return

        REM *************************************************************~
            *                      S A V E   D A T A                    *~
            *-----------------------------------------------------------*~
            * Writes data onto buffer, after making sure this user's    *~
            * buffer is empty (delete all old records). Then exit       *~
            * program.                                                  *~
            *************************************************************

        datasave
            call "SHOSTAT" ("Now Saving Entered Data to File...")
            readkey$ = userid$
            call "DELETE" (#5, readkey$, 3%)
            gosub L31000
            if keyhit% = 32% then exit_program else inputmode

        REM *************************************************************~
            * S E T   D E F A U L T S   A N D   E N A B L E   I N P U T *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables input for the line items.       *~
            *************************************************************

            deffn'163(fieldnr%, mode%)
                  call "ENABLSUB" ("SET", "HNYWDWAL", scr%(), set%(), 1%,~
                                   fieldnr%, mode%, enabled%)

                  if mode% = 2% then return
                  c% = currentline%
                  on fieldnr% gosub L20230,         /* Part             */~
                                    L20250,         /* Store number     */~
                                    L20280,         /* Lot number       */~
                                    L20340,         /* Quantity added   */~
                                    L20370,         /* cost             */~
                                    L20450,         /* Source account   */~
                                    L20530,         /* Destination acct */~
                                    L20620,         /* Where from text  */~
                                    L20660,         /* Description      */~
                                    L21000          /* Job number       */
                  return
L20230:     REM Default/enable for part
                return
L20250:     REM Default/enable for store number
                if part$(c%) = " " then return
                return
L20280:     REM Default/enable for lot number
                if part$(c%) = " " then return
                call "LOTENABL" (part$(c%), enabled%, ll%, #7, #3)
                lotenabled% = enabled%
                if enabled% = 0% then lot$(c%) = " "
                if enabled% > 0% then enabled% = 1%
                return
L20340:     REM Default/enable for quantity added
                if part$(c%) = " " then return
                   return
L20370:     REM Default/enable for cost
                if part$(c%) = " " then return
                   readkey$ = str(part$(c%),,25) & str(store$(c%),,3) &  ~
                        lot$(c%)
                   call "READ100" (#4, readkey$, f1%(4))
                   get #4, using L20400, totlcost, cost()
L20400:            FMT POS(117), 13*PD(14,4)
                   call "CONVERT" (totlcost, 2.4, totlcost$(c%))
                   put cost$(c%) using L20430, cost()
L20430:                 FMT 12*PD(14,4)
                   return
L20450:     REM Default/enable for source (inventory) account
                if part$(c%) = " " then return
                   get #4, using L20480, srcacct$(c%)
L20480:                         FMT POS(259), CH(9)
                   if srcacct$(c%) = " " then return
                      call "DESCRIBE"(#2,srcacct$(c%),infomsg$,1%,f1%(2))
                      call "GLFMT" (srcacct$(c%))
                      return
L20530:     REM Default/enable for destination (expense or job) account
                if part$(c%) = " " then return
                   if srcacct$(c%) = " " then return
                   if dstacct$(c%) = " " then dstacct$(c%) = def_destn$
                   if dstacct$(c%) = " " then goto L20560 else goto L20590
L20560:            get #4, using L20570, dstacct$(c%)
L20570:                    FMT POS(250), CH(9)
                   if dstacct$(c%) = " " then return
L20590:               call "DESCRIBE"(#2,dstacct$(c%),infomsg$,1%,f1%(2))
                      call "GLFMT" (dstacct$(c%))
                      return
L20620:     REM Default/enable for where from free text
                if part$(c%) = " " then return
                   return

L20660:     REM Default/enable for description of entry
                if c% > 1% then descr$(c%) = descr$(c%-1%)
                return

L21000:     REM Default enable for job number
                return

        REM *************************************************************~
            *         I N I T I A L I Z E   E N A B L E S               *~
            * --------------------------------------------------------- *~
            * Initialize Soft Enable Settings.                          *~
            *************************************************************
        init_enables
*        Define Screen, Field Cross Ref and Field Enable Settings.
            mat set% = con   : mat set% = (99%) * set%
            mat scr% = zer
            scr%(1, 1) =  1% : set%( 1) = 13%      /* Part Number      */
            scr%(1, 2) =  2% : set%( 2) =  2%      /* Store            */
            scr%(1, 3) =  3% : set%( 3) =  2%      /* Lot              */
            scr%(1, 4) =  4% : set%( 4) =  2%      /* Quantity         */
            scr%(1, 5) =  5% : set%( 5) = 10%      /* Total Cost       */
            scr%(1, 6) =  6% : set%( 6) =  2%      /* Source           */
            scr%(1, 7) =  7% : set%( 7) =  2%      /* Destination      */
            scr%(1, 8) =  8% : set%( 8) =  2%      /* From             */
            scr%(1, 9) =  9% : set%( 9) =  2%      /* Description      */
            scr%(1,10) = 10% : set%(10) =  2%      /* Project          */

            call "ENABLSUB" ("INIT", "HNYWDWAL", scr%(), set%(),         ~
                                                         0%, 0%, 0%, 0%)
            return

        REM *************************************************************~
            *           R E S E T   S O F T   E N A B L E S             *~
            * --------------------------------------------------------- *~
            * Allow User to modify enable settings.                     *~
            *************************************************************
        deffn'049(s%, f%)      /* Screen and Field Numbers             */
            if admin% <> 1% then return            /* Not Authorized   */
            call "ENABLSUB" ("MODIFY", "HNYWDWAL", scr%(), set%(),       ~
                              s%, f%, 0%, 0%)
            return

        REM *************************************************************~
        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the user the ability to start over when he wants to *~
            * else return to the menu.  Notice that he has to push 2    *~
            * different buttons to start over--a little harder.         *~
            *************************************************************

        startover

            keyhit1% = 1%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return
            call "SERSTOVR" (0%, "0", "2",  #12, #11) /* clear all  */
            if delmax% = 0% then L29200
               for i% = 1% to delmax%
                   call "HNYHOLD" (#4, delpart$(i%), delstore$(i%),      ~
                                       dellot$(i%), delqty(i%), return%)
               next i%
L29200:     return clear all
            goto inputmode

L30000: REM *************************************************************~
            *    L O A D   W I T H D R A W S   F R O M   B U F F E R    *~
            *-----------------------------------------------------------*~
            * Loads inventory withdrawal from buffer. If none, then     *~
            * return.                                                   *~
            *************************************************************

            old%,index% = 0%
            readkey$ = userid$ & hex(000000)
            t%, maxlines% = 0%
L30100:     REM GET LINE ITEMS FROM FILE QUICKLY.
                call "PLOWNEXT" (#5, readkey$, 3%, f1%(5))
                     if f1%(5) = 0% then return
                t%, maxlines% = maxlines% + 1
                old% = 1
                REM Get data from the file.
                    get #5, using L30460,index%(t%),part$(t%), store$(t%),~
                            lot$(t%), jobnr$(t%), qty, dstacct$(t%),     ~
                            srcacct$(t%), cost(), from$(t%), descr$(t%)
                    put cost$(t%) using L30200, cost()
L30200:                  FMT 12*PD(14,4)
                totlcost = cost( 1) + cost( 2) + cost( 3) + cost( 4) +   ~
                     cost( 5) + cost( 6) + cost( 7) + cost( 8) +         ~
                     cost( 9) + cost(10) + cost(11) + cost(12)
                convert maxlines% to seq$(maxlines%), pic(###)
                call "CONVERT" (qty,   0.2, qty$     (t%))
                hnyhold(t%) = qty
                call "CONVERT" (totlcost, 2.4, totlcost$(t%))
                call "GLFMT" (dstacct$(t%))
                call "GLFMT" (srcacct$(t%))
                index% = index%(t%)
                call "SERLOAD" (                                         ~
                        index%,          /* Line Item Pointer.         */~
                        "IW",            /* Source Transaction Type    */~
                        readkey$,        /* Source Transaction Key     */~
                        100%,            /* # Trans to Create File for */~
                        " ",             /* If non-blank Load Serial   */~
                        " ",             /* Numbers from this Source & */~
                                         /* Location, Else from TIF.   */~
                        #7,              /* SYSFILE2 UFB               */~
                        #10,             /* SERTIF UFB                 */~
                        #12,             /* SERMASTR UFB               */~
                        #11,             /* SERWORK  UFB               */~
                        u3%)             /* Number of S/N's Loaded     */
                goto L30100

L30460:     FMT XX(3),                   /* Skip over key info         */~
                BI(3),                   /* Index Pointer              */~
                XX(3),                   /* Skip sequence number       */~
                CH(25),                  /* Part number                */~
                CH(3),                   /* Store number               */~
                CH(16),                  /* Lot number                 */~
                CH(8),                   /* Job number                 */~
                PD(14,4),                /* Quantity                   */~
                CH(9),                   /* Destination (inventory) acc*/~
                CH(9),                   /* Source account             */~
                12*PD(14,4),             /* COSTS                      */~
                CH(16),                  /* Where from free text field */~
                CH(32)                   /* Description of purpose     */~

L31000: REM *************************************************************~
            *     W R I T E   W I T H D R A W S   T O   B U F F E R     *~
            *-----------------------------------------------------------*~
            * Writes inventory withdraws to buffer.  Note that old      *~
            * entries must already have been deleted.                   *~
            *************************************************************

            if maxlines% = 0% then clear_deleted_serial_nbrs

            for temp% = 1% to maxlines%
                totlcost, qty = 0
                if part$(temp%) = " " then L31210
                   convert qty$      (temp%) to qty
                   convert totlcost$ (temp%) to totlcost
                   get cost$(temp%) using L31150, cost()
L31150:                 FMT 12*PD(14,4)
                   call "PACKZERO" (cost(), cost$(temp%))
                   qty  = round(qty  ,2)
                   totlcost = round(totlcost, 4)
                   call "GLUNFMT" (srcacct$(temp%))
                   call "GLUNFMT" (dstacct$(temp%))
L31210:         write #5, using L31750,                                   ~
                          userid$,index%(temp%),seq$(temp%),part$(temp%),~
                          store$(temp%), lot$(temp%), jobnr$(temp%),     ~
                          qty, dstacct$(temp%), srcacct$(temp%),         ~
                          cost$(temp%), from$(temp%), descr$(temp%), " "

                call "HNYHOLD" (#4, part$(temp%), store$(temp%),         ~
                             lot$(temp%), qty - hnyhold(temp%), return%)
                hnyhold(temp%) = qty
                if keyhit% = 32% then gosub serial_number_processing
            next temp%

        clear_deleted_serial_nbrs
            REM *** Clear Old, Deleted Entries from SERTIF ***
            readkey$ = "IW" & str(userid$) & hex(000000)
L31360:     call "PLOWNXT1" (#10, readkey$, 5%, f1%(10))
            if f1%(10) = 0% then return
            call "READ100" (#5, str(readkey$,3%,6%), f1%(5))
            if f1%(5) = 1% then L31510
L31400:        get #10 using L31550, str(plowkey$,26%), str(plowkey$,,25%)
               delete #10
               REM *** Reset Status of Unused S/N's ***
               call "READ101" (#12, plowkey$, f1%(12))
               if f1%(12) = 0% then L31490
               status$ = str(key(#12,2),,1%) and hex(3f)
               if status$ <> "7" and status$ <> "0" then L31490
               put #12 using L31540, "2"  /* Back into Inventory        */
               rewrite #12
L31490:        call "PLOWNXT1" (#10, readkey$, 42%, f1%(10))
               if f1%(10) = 1% then L31400
L31510:     str(readkey$,4%,3%) = addc(hex(01))
            str(readkey$,7%) = " "
            goto L31360
L31540:     FMT CH(1)
L31550:     FMT POS(43), CH(20), CH(25)

        serial_number_processing
            readkey$ = str(userid$,,3%) & bin(index%(temp%),3)
            call "SERSAVE" (                                             ~
                        index%(temp%),   /* Line Item Pointer.         */~
                        "IW",            /* Source Transaction Type    */~
                        readkey$,        /* Source Transaction Key     */~
                        maxlines%,       /* # Trans to Create File for */~
                        part$(temp%),    /* Part Code                  */~
                        userid$,         /* Current User ID            */~
                        "0",             /* Change Status to ...       */~
                        "2",             /* Change Status from ...     */~
                        0%,              /* Clear TIF after Save (NO)  */~
                        #7,              /* SYSFILE2 UFB               */~
                        #10,             /* SERTIF UFB                 */~
                        #12,             /* SERMASTR UFB               */~
                        #11)             /* SERWORK  UFB               */
            return

L31750: FMT                 /* FILE: HNYWDWTF                          */~
            CH(3),          /* user-id of specific user                */~
            BI(3),          /* Inventory withdrawals sequence number.  */~
            CH(3),          /* Inventory Withdrawals Item Sequence Numb*/~
            CH(25),         /* Part Number                             */~
            CH(3),          /* Store or Warehouse Code                 */~
            CH(16),         /* Lot Number                              */~
            CH(8),          /* Job Number                              */~
            PD(14,4),       /* detail to quantity on-hand              */~
            CH(9),          /* inventory expense account               */~
            CH(9),          /* Inventory Asset Account                 */~
            CH(96),         /* cost                                    */~
            CH(16),         /* where from text field                   */~
            CH(32),         /* description of entry                    */~
            CH(67)          /* Filler (Internal, unused space)         */~

        rem**************************************************************~
           *   a c c e s s    l o c a t i o n    m a n a g e m e n t    *~
           *------------------------------------------------------------*~
           * call hnylcsub and pass the part, store, lot and quantity   *~
           * so that the user does not have to re enter them.           *~
           **************************************************************

        locations
           if qty$(currentline%) <> " " then L33120
              qty = 0
              goto L33140

L33120:     convert qty$(currentline%) to qty

L33140:     call "HNYLCSUB"   (part$(currentline%),                      ~
                               store$(currentline%),                     ~
                               lot$(currentline%),                       ~
                               qty,                                      ~
                               4%,    /*  WITHDRAWAL MODE              */~
                               #7,    /*  SYSFILE2                     */~
                               #6,    /*  STORNAME                     */~
                               #1,    /*  USERINFO                     */~
                               #3,    /*  HNYMASTR                     */~
                               #13,   /*  HNYLOCNS                     */~
                               #4,    /*  HNYQUAN                      */~
                               #14)   /*  LOCATION                     */
            return

        REM *************************************************************~
            *       T A B L E   O P E R A T I O N S   S C R E E N       *~
            *-----------------------------------------------------------*~
            * This is the screen routine that handles input, edit,      *~
            * insert, and delete modes.                                 *~
            *************************************************************

            deffn'203(screenline%, fieldnr%)
                  screen% = 1
                  goto L40290

            deffn'213(screenline%, fieldnr%)
                  screen% = 2
                  init(hex(86)) fac$()
                  if fieldnr% = 0 then L40300
                     goto L40290

            deffn'223(screenline%, fieldnr%)
                  screen% = 3
                  goto L40290

            deffn'233(screenline%)
                  screen% = 4
                  init(hex(84)) fac$()
                  for temp% = 1 to 12
                      fac$(screenline%, temp%) = hex(94)
                  next temp%
                  goto L40520

L40290:           init(hex(84)) fac$()
L40300:           on fieldnr% gosub L40450,         /* Part             */~
                                    L40450,         /* Store number     */~
                                    L40450,         /* Lot number       */~
                                    L40480,         /* Quantity         */~
                                    L40480,         /* cost             */~
                                    L40450,         /* Source account   */~
                                    L40450,         /* Inventory account*/~
                                    L40450,         /* Where from line  */~
                                    L40420,         /* Description      */~
                                    L40450          /* Job number       */
                  goto L40520

L40420:     REM Set FAC's for upper/lower case input.
                fac$(screenline%, fieldnr%) = hex(80)
                return
L40450:     REM Set FAC's for UPPER case only input
                fac$(screenline%, fieldnr%) = hex(81)
                return
L40480:     REM Set FAC's for numeric input.
                fac$(screenline%, fieldnr%) = hex(82)
                return

L40520:     accept                                                       ~
               at (01,02), fac(hex(8c)),    tttle$(screen%, 1)  , ch(79),~
               at (02,02), fac(hex(ac)),    tttle$(screen%, 2)  , ch(79),~
               at (03,02), fac(hex(94)),    errormsg$           , ch(79),~
               at (04,02), fac(hex(84)),    infomsg$            , ch(79),~
                                                                         ~
               at (05,02), fac(hex(84)),    separator$(1)       , ch(79),~
               at (09,02), fac(hex(84)),    separator$(2)       , ch(79),~
               at (13,02), fac(hex(84)),    separator$(3)       , ch(79),~
               at (17,02), fac(hex(84)),    separator$(4)       , ch(79),~
               at (21,02), fac(hex(84)),    separator$(5)       , ch(79),~
                                                                         ~
               at (06,02), fac(hex(84)),    seq$     (line%+ 1) , ch(03),~
               at (10,02), fac(hex(84)),    seq$     (line%+ 2) , ch(03),~
               at (14,02), fac(hex(84)),    seq$     (line%+ 3) , ch(03),~
               at (18,02), fac(hex(84)),    seq$     (line%+ 4) , ch(03),~
               at (22,02), fac(hex(84)),    seq$     (line%+ 5) , ch(03),~
                                                                         ~
               at (06,06), "P/N"                                        ,~
               at (10,06), "P/N"                                        ,~
               at (14,06), "P/N"                                        ,~
               at (18,06), "P/N"                                        ,~
               at (22,06), "P/N"                                        ,~
                                                                         ~
               at (06,10), fac(fac$(1, 1)), part$    (line%+ 1) , ch(25),~
               at (10,10), fac(fac$(2, 1)), part$    (line%+ 2) , ch(25),~
               at (14,10), fac(fac$(3, 1)), part$    (line%+ 3) , ch(25),~
               at (18,10), fac(fac$(4, 1)), part$    (line%+ 4) , ch(25),~
               at (22,10), fac(fac$(5, 1)), part$    (line%+ 5) , ch(25),~
                                                                         ~
               at (06,37), "Str"                                        ,~
               at (10,37), "Str"                                        ,~
               at (14,37), "Str"                                        ,~
               at (18,37), "Str"                                        ,~
               at (22,37), "Str"                                        ,~
                                                                         ~
               at (06,41), fac(fac$(1, 2)), store$   (line%+ 1) , ch( 3),~
               at (10,41), fac(fac$(2, 2)), store$   (line%+ 2) , ch( 3),~
               at (14,41), fac(fac$(3, 2)), store$   (line%+ 3) , ch( 3),~
               at (18,41), fac(fac$(4, 2)), store$   (line%+ 4) , ch( 3),~
               at (22,41), fac(fac$(5, 2)), store$   (line%+ 5) , ch( 3),~
                                                                         ~
               at (06,46), "Lot"                                        ,~
               at (10,46), "Lot"                                        ,~
               at (14,46), "Lot"                                        ,~
               at (18,46), "Lot"                                        ,~
               at (22,46), "Lot"                                        ,~
                                                                         ~
               at (06,50), fac(fac$(1,3)), str(lot$(line%+1),,ll%),      ~
               at (10,50), fac(fac$(2,3)), str(lot$(line%+2),,ll%),      ~
               at (14,50), fac(fac$(3,3)), str(lot$(line%+3),,ll%),      ~
               at (18,50), fac(fac$(4,3)), str(lot$(line%+4),,ll%),      ~
               at (22,50), fac(fac$(5,3)), str(lot$(line%+5),,ll%),      ~
                                                                         ~
               at (06,58), "Qty"                                        ,~
               at (10,58), "Qty"                                        ,~
               at (14,58), "Qty"                                        ,~
               at (18,58), "Qty"                                        ,~
               at (22,58), "Qty"                                        ,~
                                                                         ~
               at (06,62), fac(fac$(1, 4)), qty$     (line%+ 1) , ch(10),~
               at (10,62), fac(fac$(2, 4)), qty$     (line%+ 2) , ch(10),~
               at (14,62), fac(fac$(3, 4)), qty$     (line%+ 3) , ch(10),~
               at (18,62), fac(fac$(4, 4)), qty$     (line%+ 4) , ch(10),~
               at (22,62), fac(fac$(5, 4)), qty$     (line%+ 5) , ch(10),~
                                                                         ~
               at (07,02), "Total Cost"                                 ,~
               at (11,02), "Total Cost"                                 ,~
               at (15,02), "Total Cost"                                 ,~
               at (19,02), "Total Cost"                                 ,~
               at (23,02), "Total Cost"                                 ,~
                                                                         ~
               at (07,13), fac(fac$(1, 5)), totlcost$(line%+ 1) , ch(10),~
               at (11,13), fac(fac$(2, 5)), totlcost$(line%+ 2) , ch(10),~
               at (15,13), fac(fac$(3, 5)), totlcost$(line%+ 3) , ch(10),~
               at (19,13), fac(fac$(4, 5)), totlcost$(line%+ 4) , ch(10),~
               at (23,13), fac(fac$(5, 5)), totlcost$(line%+ 5) , ch(10),~
                                                                         ~
               at (07,44), "Source"                                     ,~
               at (11,44), "Source"                                     ,~
               at (15,44), "Source"                                     ,~
               at (19,44), "Source"                                     ,~
               at (23,44), "Source"                                     ,~
                                                                         ~
               at (07,51), fac(fac$(1, 6)), srcacct$ (line%+ 1) , ch(12),~
               at (11,51), fac(fac$(2, 6)), srcacct$ (line%+ 2) , ch(12),~
               at (15,51), fac(fac$(3, 6)), srcacct$ (line%+ 3) , ch(12),~
               at (19,51), fac(fac$(4, 6)), srcacct$ (line%+ 4) , ch(12),~
               at (23,51), fac(fac$(5, 6)), srcacct$ (line%+ 5) , ch(12),~
                                                                         ~
               at (07,64), "Dest"                                       ,~
               at (11,64), "Dest"                                       ,~
               at (15,64), "Dest"                                       ,~
               at (19,64), "Dest"                                       ,~
               at (23,64), "Dest"                                       ,~
                                                                         ~
               at (07,69), fac(fac$(1, 7)), dstacct$ (line%+ 1) , ch(12),~
               at (11,69), fac(fac$(2, 7)), dstacct$ (line%+ 2) , ch(12),~
               at (15,69), fac(fac$(3, 7)), dstacct$ (line%+ 3) , ch(12),~
               at (19,69), fac(fac$(4, 7)), dstacct$ (line%+ 4) , ch(12),~
               at (23,69), fac(fac$(5, 7)), dstacct$ (line%+ 5) , ch(12),~
                                                                         ~
               at (08,02), "From"                                       ,~
               at (12,02), "From"                                       ,~
               at (16,02), "From"                                       ,~
               at (20,02), "From"                                       ,~
               at (24,02), "From"                                       ,~
                                                                         ~
               at (08,07), fac(fac$(1, 8)), from$    (line%+ 1) , ch(16),~
               at (12,07), fac(fac$(2, 8)), from$    (line%+ 2) , ch(16),~
               at (16,07), fac(fac$(3, 8)), from$    (line%+ 3) , ch(16),~
               at (20,07), fac(fac$(4, 8)), from$    (line%+ 4) , ch(16),~
               at (24,07), fac(fac$(5, 8)), from$    (line%+ 5) , ch(16),~
                                                                         ~
               at (08,27), "Desc"                                       ,~
               at (12,27), "Desc"                                       ,~
               at (16,27), "Desc"                                       ,~
               at (20,27), "Desc"                                       ,~
               at (24,27), "Desc"                                       ,~
                                                                         ~
               at (08,32), fac(fac$(1, 9)), descr$   (line%+ 1) , ch(32),~
               at (12,32), fac(fac$(2, 9)), descr$   (line%+ 2) , ch(32),~
               at (16,32), fac(fac$(3, 9)), descr$   (line%+ 3) , ch(32),~
               at (20,32), fac(fac$(4, 9)), descr$   (line%+ 4) , ch(32),~
               at (24,32), fac(fac$(5, 9)), descr$   (line%+ 5) , ch(32),~
                                                                         ~
               at (08,66), "Prj"                                        ,~
               at (12,66), "Prj"                                        ,~
               at (16,66), "Prj"                                        ,~
               at (20,66), "Prj"                                        ,~
               at (24,66), "Prj"                                        ,~
                                                                         ~
               at (08,70), fac(fac$(1,10)), jobnr$   (line%+ 1) , ch(08),~
               at (12,70), fac(fac$(2,10)), jobnr$   (line%+ 2) , ch(08),~
               at (16,70), fac(fac$(3,10)), jobnr$   (line%+ 3) , ch(08),~
               at (20,70), fac(fac$(4,10)), jobnr$   (line%+ 4) , ch(08),~
               at (24,70), fac(fac$(5,10)), jobnr$   (line%+ 5) , ch(08),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)

*             IF SCREEN% > 2% THEN 41930
*                IF KEYHIT% <> 8 THEN 41930
*                   GOSUB LOCATIONS
*                   GOTO 40520

               if keyhit% <> 10 then L41990
                  if c% < 1% or c% > dim(part$(),1) then L40520
                  if fieldnr% = 0% then L40520
                  call "HNYQDISP" (part$(c%), #3, #4, #9, #7)
                  goto L40520

L41990:        if keyhit% <> 13 then L42030
                  call "MANUAL" ("HNYWDWAL")
                  goto L40520

L42030:        if keyhit% <> 15 then L42070
                  call "PRNTSCRN"
                  goto L40520

L42070:        if screen% <> 2 then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return
                     u3% = u3%

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Tests data for the line items.                            *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$, infomsg$ = " "
                  c% = currentline%
                  on fieldnr% gosub L50210,         /* Part             */~
                                    L50330,         /* Store number     */~
                                    L50450,         /* Lot number       */~
                                    L50574,         /* Quantity added   */~
                                    L50750,         /* cost             */~
                                    L50780,         /* Source account   */~
                                    L50860,         /* Destination acct */~
                                    L50950,         /* Where from (free)*/~
                                    L50970,         /* Description      */~
                                    L51020          /* Job number       */
                  return

L50210:     REM Test data for part number
                plowkey$ = part$(c%)
                call "PLOWCODE" (#4, plowkey$, infomsg$, -8025%, -.32,   ~
                                 f1%(4), header$(), 0, 0, incl_excl(),   ~
                                 incl_excl$(), "Y", " ", #3)
                if f1%(4) = 0% then L50300
                infomsg$ = "(" & infomsg$ & ")"
                part$(c%) = plowkey$
                if edit% = 0% then return else L50574
L50300:            errormsg$ = "Part not found in the Quantity Master " &~
                               "file: " & part$(c%)
                   return
L50330:     REM Test data for store
                plowkey$ = str(part$(c%)) & store$(c%)
                call "PLOWCODE" (#4, plowkey$, infomsg$,  8025%,  .30,   ~
                                 f1%(4), header$(), 3, 0, incl_excl(),   ~
                                 incl_excl$(), "Y", " ", #6)
                if f1%(4) = 0% then L50420
                   store$(c%) = str(plowkey$,26%)
                   infomsg$ = "(" & infomsg$ & ")"
                if edit% = 0% then return else L50574
L50420:            errormsg$ = "Part not available from this Store."
                   return
L50450:     REM Test lot number
            if lotenabled% <> 2% or lot$(c%) <> " " then goto L50470
                errormsg$ = "You must enter a Lot # for this Part."
                return
L50470:     call "LOTVALID" (part$(c%), store$(c%), lot$(c%), #7, #3, #4,~
                errormsg$)
            if errormsg$ = " " then goto L50495
L50485:         infomsg$ = " "
                return
L50495:     call "LOTUNQUE" (part$(), lot$(), c%, #7, errormsg$)
            if errormsg$ <> " " then goto L50485
            readkey$ = str(part$(c%)) & str(store$(c%)) & lot$(c%)
            call "READ100" (#4, readkey$, f1%(4%))
            if f1%(4%) <> 0% then goto L50535
                errormsg$ = "You must enter an existing Lot # for this Pa~
        ~rt."
                return
L50535:     for_info_msg% = 1% : qty = 0              /* Get avail  */
            convert qty$(c%) to qty, data goto L50538  /*  quantity  */
L50538:     gosub calc_avail : for_info_msg% = 0%     /* as of now. */
            avail = temp1 - temp
            call "CONVERT" (avail, .2, lot_qty_on_hand$)
            call "STRING" addr ("LJ", lot_qty_on_hand$,                  ~
                len(str(lot_qty_on_hand$)))
            uom$ = " "
            call "READ100" (#3, part$(c%), f1%(3%))
                if f1%(3%) = 1% then get #3 using L50560, uom$
L50560:              FMT POS(74), CH(4)
            infomsg$ = "Qty Available in Lot " & lot$(c%) & ": " &       ~
                lot_qty_on_hand$ & " " & uom$
            if edit% = 0% then return

L50574:     REM Test data for quantity
                call "NUMTEST" (qty$(c%),.01, 1e9, errormsg$, 0.2, qty)
                if errormsg$ > " " then return
        calc_avail
                temp = qty - hnyhold(c%)
                   end% = maxlines%                  /* To get it right */
                   if edit% = 0% then end% = end% + 1%  /* for inserts. */
                   for i% = 1% to end%
                       if i% = c% then L50660
                       if part$(i%) <> part$(c%) then L50660
                       if store$(i%) <> store$(c%) then L50660
                       if lot$(i%) <> lot$(c%) then L50660
                          convert qty$(i%) to temp1, data goto L50660
                          temp = temp + temp1 - hnyhold(i%)
L50660:            next i%
                call "HNYAVAIL" (#3, #4, part$(c%), store$(c%), lot$(c%),~
                                         errormsg$, temp, temp1, return%)
                if for_info_msg% = 1% then return  /* Came from lot */
                                                   /* testing above.*/
                if errormsg$ <> " " then return
                if fieldnr% <> 4% then return     /*****/
                call "SERENABL" (part$(c%), enabled%, sl%, #7, #3)
                if enabled% = 1% then gosub enter_serial_numbers
                return
                sl% = sl%
L50750:     REM Test data for total cost -- JIM -- 05/12/87
                call "NUMTEST" (totlcost$(c%), 0, 9e6, errormsg$, 2.4, 0)
                return
L50780:     REM Test data for source (inventory) account
                call "GETCODE" (#2, srcacct$(c%), infomsg$, 1%, 0, f1%(2))
                if f1%(2) = 1 then return
                   errormsg$="Source account not on file :" & srcacct$(c%)
                   return
                REM Gotta zap the other account, too!!
                    dstacct$(c%), srcacct$(c%) = " "
                    return
L50860:     REM Test data for destination (expense) account
                call "GETCODE" (#2, dstacct$(c%), infomsg$, 1%, 0, f1%(2))
                if f1%(2) = 1 then return
                   errormsg$ = "Destination account not on file: "       ~
                                                   & dstacct$(c%)
                   return
                REM Zap other account number too if this one blank.
                    dstacct$(c%), srcacct$(c%) = " "
                    return
L50950:     REM Test data for free text field
                return
L50970:     REM Test data for description of entry
            if descr$(c%) <> " " then goto L51002
                errormsg$ = "May not be blank. Enter a Description, '?', ~
        ~or Code."
                return
L51002:     if len(descr$(c%)) > 1% then return     /* Entered by user */
            if str(descr$(c%),,1%) = "?" then str(descr$(c%),,1%) = " "
            readkey$ = "INVWDWDSC" & str(descr$(c%),,1%)
            call "PLOWCODE" (#20, readkey$, descr$(c%), 9%, .3, f1%(20%))
            if f1%(20%) <> 0% then return
            if str(readkey$,10%,1%) = " " then L51012   /* Want 1-char? */
                descr$(c%) = str(readkey$,10%,1%)      /* 1-char Descr */
                return
L51012:     errormsg$ = "You must enter a Description, Code or '?'."
            return

L51020:     REM Test data for the job number
                if jobnr$(c%) = " " then return
                call "GETCODE" (#8, jobnr$(c%), infomsg$, 1%, 0, f1%(8))
                      if f1%(8) = 1 then L51080
                errormsg$ = "Project not on file: " & jobnr$(c%)
                return
L51080:     REM Now lets see if job is closed and when
                get #8, using L51100, datejobclosed$
L51100:            FMT XX(44), CH(06)
                if datejobclosed$ = " " or ~
						~datejobclosed$ = blank_date$ then return
                if datejobclosed$ > hnydate$ then return
                call "DATEFMT" (datejobclosed$)
                errormsg$ = "Project was closed on  " & datejobclosed$
                return

        enter_serial_numbers
            plowkey$ = str(userid$) & bin(index%(c%),3)
            location$ = str(store$(c%)) & lot$(c%)
            call "SERSELCT" (part$(c%),  /* Part code                  */~
                             location$,  /* S/N Location to Select from*/~
                             qty,        /* Qty to Assign S/N's To     */~
                             index%(c%), /* Pointer For Work File Use  */~
                             100%,       /* Average # Lines per Documnt*/~
                             "IW",       /* Source Transaction Type.   */~
                             plowkey$,   /* Source Transaction Key.    */~
                             "0",        /* Status to Change S/N to.   */~
                             "2",        /* Status to Select/Chg from  */~
                             errormsg$,  /* Returned Error Message     */~
                             #7,         /* SYSFILE2 UFB               */~
                             #3,         /* HNYMASTR UFB               */~
                             #12,        /* SERMASTR UFB               */~
                             #11)        /* SERWORK  UFB               */

            return

        REM *************************************************************~
            *                          E X I T                          *~
            *---------------------------------------------------------- *~
            * Displays foreground message that we're switching, sets    *~
            * return code of zero if there's nothing in the buffer.     *~
            *************************************************************
        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#11)
            REM Set return code for continuing to process.
                readkey$ = str(userid$) & hex(000000)
                call "PLOWNEXT" (#5, readkey$, 3%, f1%(5))

            end f1%(5)
