        REM *************************************************************~
            *                                                           *~
            *  H   H  N   N  Y   Y   AAA   DDDD   DDDD   N   N   SSS    *~
            *  H   H  NN  N  Y   Y  A   A  D   D  D   D  NN  N  S       *~
            *  HHHHH  N N N   YYY   AAAAA  D   D  D   D  N N N   SSS    *~
            *  H   H  N  NN    Y    A   A  D   D  D   D  N  NN      S   *~
            *  H   H  N   N    Y    A   A  DDDD   DDDD   N   N   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYADDNS - Enter additions to inventory and write to the  *~
            *            additions buffer.                              *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/24/80 ! ORIGINAL                                 ! BCW *~
            * 10/08/80 ! REWRITE FOR NEW FILE LAYOUTS             ! TEM *~
            * 02/04/81 ! STILL MORE NEW FILE LAYOUTS              ! TEM *~
            * 05/20/81 ! ADDED WIP/JC (WHAT A DAY)                ! TOM *~
            * 06/17/83 ! DISALLOW BLANK DESCR, VALIDATE G/L,      ! JRW *~
            *          ! DISALLOW BLANK PART                      ! JRW *~
            * 07/18/83 ! ADDED CALL TO MANUAL                     ! HES *~
            * 06/07/84 ! CORRECTED ARRAY OVERFLOW PROBLEMS        ! LDJ *~
            * 10/04/85 ! Changed VENDOR File Format               ! MJB *~
            * 11/25/85 ! Corrected Costs Rounding (from 2 to 4)   ! LDJ *~
            * 12/06/85 ! Fixed line above, added optional save to ! ERN *~
            *          ! buffer, increased array sizes.           !     *~
            * 01/16/87 ! Changed HNYQUAN Select, added support for! LDJ *~
            *          !   Lot and S/N Tracking, changed ADDBUFFR !     *~
            *          !   Select, added SERTIF, SERMASTR, and    !     *~
            *          !   SERWORK Selects.                       !     *~
            * 02/05/87 ! Enhanced Lot Tracking.                   ! ERN *~
            * 05/11/87 ! Expand costs from 3 to 12; add call to   ! JIM *~
            *          !   HNYCDIST; Std cost mods to files/screen!     *~
            * 04/25/88 ! Added Soft Enables                       ! TLJ *~
            * 02/06/89 ! Corrected call to CMSMACHK               ! MJB *~
            * 03/01/89 ! Mod to get standard cost if using cost   ! MJB *~
            *          !  method 'S', 'T' or 'F'                  !     *~
            * 04/03/89 ! Corrected HNYGLGET argument to get asset ! MJB *~
            *          !  acct instead of WIP for Destination     !     *~
            * 06/26/89 ! Modified to 'GET' appropriate Source and ! MLJ *~
            *          !  Destination for Purchased/Mfg parts     !     *~
            * 01/30/89 ! Added PF8 access to HNYLCSUB permitting  ! MLJ *~
            *          !  Location Control.                       !     *~
            * 03/09/90 ! Changed LOCATION from 200 to 400.        ! MLJ *~
            * 04/11/90 ! Added KAB 04/10/90 fix for Source Account! MLJ *~
            *          !  Derivation.                             !     *~
            * 08/01/91 ! If HNYQUAN exists, don't use HNYMASTR    ! JDH *~
            *          !   Cost Method.                           !     *~
            * 03/31/93 ! PRR 12784 Add LOTVALID & LOTUNQUE calls. ! JIM *~
            * 04/27/93 ! Further Lot Tracking Integrity enhancemts! JIM *~
            * 04/28/93 ! PRRs 10716 & 11937 Honor Source G/L acct ! JIM *~
            *          !   in SWITCHS.HNY.                        !     *~
            * 04/29/93 ! PRR 12461 A single character Description ! JIM *~
            *          !   drives a Descr lookup in GENCODES.     !     *~
            *          !   Description length is now 32 bytes.    !     *~
            * 01/31/94 ! PRR 13095 Added UOM to info message.     ! JDH *~
				* 06/24/96 ! Add blank date for datejobclosed test    ! DER *~
            *************************************************************
        dim                                                              ~
	    blank_date$8,                /* Blank date check           */~
            cost(12), cost$(900)96,      /* HNYCDIST COST FIELDS       */~
            cstmthd$1,                   /* Inventory Costing Method   */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDITING*/~
            date$8,                      /* Date for Screen Display    */~
            datejobclosed$8,             /* DATE JOB WAS CLOSED TEST   */~
            defsource$9,                 /* Default Source G/L acct    */~
            descr$(900)32,               /* FREE TEXT DESCRIPTIONS     */~
            dstacct$(900)16,             /* DESTINATION (HNY) ACCOUNT  */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            fac$(5,20)1,                 /* FIELD ATTRIBUTE CHARACTERS */~
            hnydate$6,                   /* THIS USERS INVENTORY DATE  */~
            i$(24)80,                    /* SCREEN IMAGE--NOT USED     */~
            index%(900),                 /* Serial #'s Work File Ptrs  */~
            infomsg$79,                  /* INFORMATIVE MESSAGE        */~
            jobnr$(900)8,                /* JOB NUMBER                 */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lot$(900)16,                 /* LOT NUMBER                 */~
            mode$1,                      /* Mode for HNYCDIST          */~
            part$(900)25, partdescr$34,  /* PART NUMBER THIS LINE      */~
            part_pos$2,                  /* 2 CHAR TEST FIELD          */~
            pfkeys$(4)17,                /* PF KEYS FOR TABULAR SCREENS*/~
            plowkey$99,                  /* Key for plow routines      */~
            ponr$(900)16,                /* PURCHASE ORDER NUMBER      */~
            qty$(900)10,                 /* QUANTITY ADDED THIS LINE   */~
            readkey$99,                  /* KEY FOR PLOW ROUTINES      */~
            scr%(5,11), set%(255),       /* Soft Enable Tables         */~
            separator$(5)79,             /* SEPARATOR LINES FOR TABLES */~
            seq$(900)3,                  /* SEQUENCE NUMBERS FOR SCREEN*/~
            srcacct$(900)16,             /* SOURCE ACCOUNT             */~
            status$1,                    /* Serial Number Status Flag  */~
            store$(900)3,                /* STORE NUMBER THIS LINE     */~
            title$(4,2)79,               /* TITLES FOR TABULAR SCREENS */~
            totlcost$(900)10,            /* COST INFORMATION           */~
            tran$(24)80,                 /* FOR EDIT COMPUTATIONS      */~
            uom$4,                       /* Unit of Measure            */~
            userid$3,                    /* USERID OF CURRENT USER     */~
            vendor$(900)9                /* VENDOR CODE                */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

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
            * #5  ! HNYADDTF ! Additions buffer for inventory           *~
            * #6  ! STORNAME ! Store names and addresses                *~
            * #7  ! SYSFILE2 ! System information (months open)         *~
            * #8  ! VENDOR   ! Vendor master                            *~
            * #9  ! JOBMASTR ! Wip/jc job master file                   *~
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
                                  key 2, keypos = 90, keylen = 4, dup

            select #4,  "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos =  17, keylen = 44,                       ~
                        alternate key 1, keypos =  1, keylen = 44

            select  #5, "HNYADDTF",                                      ~
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

            select  #8, "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos = 1, keylen = 9,                          ~
                        alt key  1, keypos = 10, keylen = 30, dup

            select  #9, "JOBMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 8

            select #10, "SERTIF",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
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
                        alt key 1, keypos =     4, keylen =  11

            select #20, "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos =    1,  keylen = 24

        call "SHOSTAT"  ("Opening Files; One Moment Please.")

            rslt$(1),rslt$(3), rslt$(6), rslt$(7) = "REQUIRED"
            call "OPENFILE" (# 1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
            call "OPENFILE" (# 2, "SHARE", f2%( 2), rslt$( 2), axd$( 2))
            call "OPENFILE" (# 3, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "OPENFILE" (# 4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))
            call "OPENFILE" (# 5, "SHARE", f2%( 5), rslt$( 5), axd$( 5))
            call "OPENFILE" (# 6, "SHARE", f2%( 6), rslt$( 6), axd$( 6))
            call "OPENFILE" (# 7, "SHARE", f2%( 7), rslt$( 7), axd$( 7))
            call "OPENFILE" (# 8, "SHARE", f2%( 8), rslt$( 8), axd$( 8))
            call "OPENFILE" (# 9, "SHARE", f2%( 9), rslt$( 9), axd$( 9))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#13, "SHARE", f2%(13), rslt$(13), axd$(13))
            call "OPENFILE" (#14, "SHARE", f2%(14), rslt$(14), axd$(14))
            call "OPENFILE" (#20, "SHARE", f2%(20), rslt$(20), axd$(20))

            if f2%(1) + f2%(3) + f2%(6) + f2%(7) <> 0% then exit_program
            if f2%(5) = 0 then L03150
               call "OPENFILE" (#5, "OUTPT", f2%(5), rslt$(5), axd$(5))
               close #5
               call "OPENFILE" (#5, "SHARE", f2%(5), rslt$(5), axd$(5))
L03150:     if f2%(13) = 0 then L03190
               call "OPENFILE" (#13, "OUTPT", f2%(13),rslt$(13),axd$(13))
               close #13
               call "OPENFILE" (#13, "SHARE", f2%(13),rslt$(13),axd$(13))
L03190:     if f2%(14) = 0 then L09000
               call "OPENFILE" (#14, "OUTPT", f2%(14),rslt$(14),axd$(14))
               close #14
               call "OPENFILE" (#14, "SHARE", f2%(14),rslt$(14),axd$(14))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Gets userid of this user, sets dates, and tries to read   *~
            * in the entries already in the buffer (so that we can save *~
            * what was out there in case the program bombs).            *~
            *************************************************************
				blank_date$ = " "
				call "DATUFMTC" ( blank_date$ )

*        See if User is an administator
            call "CMSMACHK" ("HNY", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then admin% = 1%         ~
                                                else admin% = 0%
            gosub init_enables

*        Get the Default Additions Source G/L Acct, if any.
            call "READ100" (#7, "SWITCHS.HNY", f1%(7%))    /* SYSFILE2 */
            if f1%(7%) <> 0% then get #7 using L09144, defsource$
L09144:         FMT POS(121), CH(9)

            ll% = 6%      /* Max Lot Length        */
*        Retrieve Inventory Date
            call "EXTRACT" addr ("ID", userid$)
            call "READ100" (#1, userid$, f1%(1))
            if f1%(1) = 1% then L09240
                call "ASKUSER" (0%, "***ERROR***",                       ~
                     "Unable to locate your Inventory Posting Date!",    ~
                     " ", "Press RETURN to acknowledge and Exit.")
                       goto exit_program
L09240:     get #1, using L09250, hnydate$
L09250:         FMT XX(27), CH(6)
            call "WHICHMON" (#7, hnydate$, thismonth%)
            if thismonth% > 0 and thismonth% < 4 then L09350
                call "ASKUSER" (0%, "INVALID POSTING DATE",              ~
                     "Your Inventory Posting Date is not within the " &  ~
                     "posting window.",                                  ~
                     "Please change your posting date & try again.",     ~
                     "Press RETURN to acknowledge and Exit.")
                      goto exit_program

L09350
*        Set Screen Titles and PF Keys
            date$ = hnydate$  :  call "DATEFMT" (date$)
            title$(1,2) = "(1)Start Over (2)Col 1 (6)Line Above (8)Locati~
        ~ons  (13)Instr (16)Exit/Edit Mode"
            title$(1,1) = "Enter Additions To Inventory"
            str(title$(1,1),61%) = "Post Date: " & date$
            title$(2,1) = "(1)Start Over (2)First (3)Last (4)Prev (5)Next~
        ~ (6)Down (7)Up"
            title$(2,2) = "(8)Locations (11)Insert       (12)Delete     (~
        ~13)Instr (16)Save Data (32)Update"
            title$(3,1) = "Supply requested items and (RETURN) or (1) to ~
        ~Exit Insert Mode"
            title$(4,1) = "Press (RETURN) to delete flashing line or (1) ~
        ~to exit Delete."

            pfkeys$(1) = hex(00010206080d0f10ffffffffffffffffff)
            pfkeys$(2) = hex(000102030405060708090b0c0d0f10201d)
            pfkeys$(3) = hex(00010fffffffffffffffffffffffffffff)
            pfkeys$(4) = hex(00010fffffffffffffffffffffffffffff)

*        Set TRAN$ for Edit Mode Field Number Computation.
            init(hex(00)) str(tran$(),   1    )
            init(hex(01)) str(tran$(6),  1, 35)
            init(hex(02)) str(tran$(6), 36, 10)
            init(hex(03)) str(tran$(6), 46, 12)
            init(hex(04)) str(tran$(6), 57, 17)
            init(hex(05)) str(tran$(7),  1, 22)
            init(hex(06)) str(tran$(7), 24, 21)
            init(hex(07)) str(tran$(7), 45, 19)
            init(hex(08)) str(tran$(7), 64, 16)
            init(hex(09)) str(tran$(8),  1, 40)
            init(hex(0a)) str(tran$(8), 46, 19)
            init(hex(0b)) str(tran$(8), 66, 14)

            copy str(tran$(), 321, 1360) to str(tran$(), 641)

        REM *************************************************************~
            *     I N P U T   I N V E N T O R Y   A D D I T I O N S     *~
            *-----------------------------------------------------------*~
            * Handles standard tabular input mode (there's no linear    *~
            * input in this program)                                    *~
            *************************************************************

        inputmode
            init(" ") seq$(), part$(), descr$(), qty$(), totlcost$(),    ~
                      srcacct$(), dstacct$(), lot$(), store$(),          ~
                      errormsg$, infomsg$, vendor$(), ponr$(), jobnr$(), ~
                      part_pos$
            mode$ = "I"
            mat cost = zer : init (hex(00)) cost$()
            maxlines%, line%, screenline%, currentline%, index% = 0%
            gosub dataload
            if old%  = 1% then editmode

L10170:     screenline% = screenline% + 1%
            if screenline% < 6% then L10210
               line% = line% + 5%
               screenline% = 1%
L10210:     currentline% = line% + screenline%
            index% = index% + 1%
            index%(currentline%) = index%
            if currentline% > 899% then L11000
            call "SETSEP" (separator$(), line%, screenline%)

            for fieldnr% = 1% to 11%
                gosub'051(fieldnr%,1%)      /* GET SPECIAL DEFAULTS. */
                      if enabled% =  0 then       L10400
L10300:         gosub'101(screenline%, fieldnr%)
                      if keyhit%  =  0 then       L10400
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  2 then gosub columnone
                      if keyhit%  =  6 then gosub lineabove
                      if keyhit%  =  6 then       L10400
                      if keyhit%  = 16 and fieldnr% = 1                  ~
                                and currentline% = 1 then exit_program
                      if keyhit%  = 16 and fieldnr% = 1 then editmode
                         goto L10300
L10400:         gosub'153(fieldnr%, 1%)
                      if errormsg$ <> " " then L10300
            next fieldnr%
            maxlines% = maxlines% + 1%
            convert currentline% to seq$(currentline%), pic(###)
            goto L10170

L11000: REM *************************************************************~
            *      E D I T   I N V E N T O R Y   A D D I T I O N S      *~
            *-----------------------------------------------------------*~
            * Edits inventory additions and also permits insert and     *~
            * delete to occur.                                          *~
            *************************************************************

        editmode
            line%, currentline%, screenline% = 0
            errormsg$, infomsg$ = " "
            mode$ = "E"
            call"SETSEP"(separator$(),line%,min(maxlines%-line%,5%))

L11130:     gosub'111(0%, 0%)
                  if keyhit%  =  0 then       L11330
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then line% = 0
                  if keyhit%  =  3 then line% = max(0,maxlines%-5)
                  if keyhit%  =  4 then line% = max(0,line%-4)
                  if keyhit%  =  5 then line% = min(line%+4,max(0,       ~
                                                maxlines%-5))
                  if keyhit%  =  6 then line% = max(0,line%-1)
                  if keyhit%  =  7 then line% = min(line%+1,max(0,       ~
                                                maxlines%-5))
                  if keyhit%  =  9 then       editmode
                  if keyhit%  = 11 then gosub insertmode
                  if keyhit%  = 12 then gosub deletemode
                  if keyhit%  = 16 then       datasave   /* Just Save  */
                  if keyhit%  = 29 then       L11330
                  if keyhit%  = 32 then       datasave   /* Save/Update*/
                  call"SETSEP"(separator$(),line%,min(maxlines%-line%,5%))
                  goto L11130

L11330:     REM Now figure out which field he hit.
                fieldnr% = val(str(tran$(cursor%(1)),cursor%(2)))
                if fieldnr% <  2% then L11130 /* NEVER allow chg of Part#*/
                if fieldnr% > 11% then L11130 /* Invalid field selected  */
                screenline% = (cursor%(1)-5)/4+1
                currentline% = line% + screenline%
                if currentline% > maxlines% then L11130
                if keyhit% <> 29% then L11430
                   gosub'049(1%, fieldnr%)
                   goto editmode
L11430:         gosub'051(fieldnr%, 2%)    /* Test Enables             */
                if enabled% = 0% then editmode
L11450:         gosub'111(screenline%, fieldnr%)
                      if keyhit%  = 1 then gosub startover
                      if keyhit% <> 0 then L11450
                gosub'153(fieldnr%, 2%)
                      if errormsg$ <> " " then L11450
                goto L11130

        REM *************************************************************~
            *  C O L U M N   O N E ,   L I N E   A B O V E   L O G I C  *~
            *-----------------------------------------------------------*~
            * Contains the code for column one and line above functions.*~
            *************************************************************

        lineabove
            if currentline% = 1 then return
            c% = currentline%
            on fieldnr% gosub L14220,               /* PART NUMBER      */~
                              L14230,               /* STORE NUMBER     */~
                              L14240,               /* LOT NUMBER       */~
                              L14250,               /* QUANTITY ADDED   */~
                              L14260,               /* COST             */~
                              L14310,               /* SOURCE ACCOUNT   */~
                              L14320,               /* DESTINATION ACCT */~
                              L14330,               /* VENDOR CODE      */~
                              L14340,               /* DESCRIPTION      */~
                              L14350,               /* PURCHASE ORDER NO*/~
                              L14360                /* JOB NUMBER       */
            return

L14220:     part$    (c%) = part$    (c%-1): return
L14230:     store$   (c%) = store$   (c%-1): return
L14240:     lot$     (c%) = lot$     (c%-1): return
L14250:     qty$     (c%) = qty$     (c%-1): return
L14260:     totlcost$(c%) = totlcost$(c%-1)
                get cost$(c%-1) using L14290, cost()
                put cost$(c%) using L14290, cost()
L14290:              FMT 12*PD(14,4)
                return
L14310:     srcacct$ (c%) = srcacct$ (c%-1): return
L14320:     dstacct$ (c%) = dstacct$ (c%-1): return
L14330:     vendor$  (c%) = vendor$  (c%-1): return
L14340:     descr$   (c%) = descr$   (c%-1): return
L14350:     ponr$    (c%) = ponr$    (c%-1): return
L14360:     jobnr$   (c%) = jobnr$   (c%-1): return

        columnone
            c% = currentline%
            if c% <= 0 then return
            init(" ") seq$(c%), part$(c%), descr$(c%), totlcost$(c%),    ~
                      qty$(c%), srcacct$(c%), dstacct$(c%),              ~
                      vendor$(c%), lot$(c%), store$(c%), errormsg$,      ~
                      infomsg$, ponr$(c%), jobnr$(c%)
            init (hex(00)) cost$(c%)
            call "SERSTOVR" (index%(c%), "6", " ",  #12, #11)
            fieldnr% = 1
            return

        REM *************************************************************~
            *              I N S E R T   M O D E   C O D E              *~
            *-----------------------------------------------------------*~
            * Handles insertion of a line item into the array(s).       *~
            *************************************************************

        insertmode
L15070:     if maxlines% >= 900% then return       /* Array full, can't*/
            REM otherwise, set CURRENTLINE%, SCREENLINE%, and copy right
                cursor%(1) = max(5%,cursor%(1))
                screenline% = int((cursor%(1)-1)/4)
                if line% + screenline% < maxlines% then L15130
                   screenline% = maxlines% - line% /* To ins at end    */
L15130:         if screenline% <> 5% then L15180    /* Bottom of page   */
                   line% = line% + 1%
                   screenline% = screenline% - 1%
                   goto L15180

L15180:         call "SETSEP" (separator$(),line%,                       ~
                                      min(5%, maxlines%-line%+1%))
                currentline%, c% = screenline% + line%

            REM Copy all the elements up one
                if c% >= maxlines% then L15420
                for temp% = maxlines% to c% step -1%
                    part$    (temp%+1) = part$    (temp%)
                    store$   (temp%+1) = store$   (temp%)
                    lot$     (temp%+1) = lot$     (temp%)
                    descr$   (temp%+1) = descr$   (temp%)
                    qty$     (temp%+1) = qty$     (temp%)
                    totlcost$(temp%+1) = totlcost$(temp%)
                          get cost$(temp%) using L15330, cost()
                          put cost$(temp%+1) using L15330, cost()
L15330:                        FMT 12*PD(14,4)
                    srcacct$ (temp%+1) = srcacct$ (temp%)
                    dstacct$ (temp%+1) = dstacct$ (temp%)
                    vendor$  (temp%+1) = vendor$  (temp%)
                    ponr$    (temp%+1) = ponr$    (temp%)
                    jobnr$   (temp%+1) = jobnr$   (temp%)
                    index%   (temp%+1) = index%   (temp%)
                next temp%

L15420:         screenline% = screenline% + 1
                c%, currentline% = currentline% + 1
                index% = index% + 1%
                index%(c%) = index%

                init(" ") part$(c%), descr$(c%), qty$(c%), totlcost$(c%),~
                          srcacct$(c%),                                  ~
                          dstacct$(c%), seq$(c%), lot$(c%), vendor$(c%), ~
                          store$(c%), ponr$(c%), jobnr$(c%)
                init (hex(00)) cost$(c%)
            REM Now input the line, make so we can cancel out if necc
                mode$ = "I"
                infomsg$ = " "
                for fieldnr% = 1 to 11
                    gosub'051(fieldnr%,1%)
                          if enabled% = 0 then L15610
L15580:             gosub'123(screenline%, fieldnr%)
                          if keyhit%  =  1 then L15750
                          if keyhit% <>  0 then L15580
L15610:             gosub'153(fieldnr%, 1%)
                          if errormsg$ <> " " then L15580
                next fieldnr%

                maxlines% = maxlines% + 1
                REM RENUMBER ITEM NUMBERS, THEN SET KLUGE CURSOR%(1)
                    if part$(currentline%) = " " then L15720
                       for temp% = 1 to max(maxlines%, currentline%+1)
                           if part$(temp%) = " " then L15710
                              convert temp% to seq$(temp%), pic(###)
L15710:                next temp%
L15720:         cursor%(1) = min(cursor%(1)+4, 24)
                goto L15070

L15750:     REM This routine aborts insert mode and destroys screenline%
                c% = currentline%
                if fieldnr% > 1% then                                    ~
                   call "SERSTOVR" (index%(c%), "6", " ",  #12, #11)
                gosub L15930              /* ACTUALLY DELETE @C%        */

                temp% = maxlines% + 1%
                init(" ") seq$(temp%), part$(temp%), descr$(temp%),      ~
                          qty$(temp%), totlcost$(temp%), ponr$(temp%),   ~
                          srcacct$(temp%), dstacct$(temp%),              ~
                          lot$(temp%), store$(temp%), vendor$(temp%),    ~
                          errormsg$, infomsg$, jobnr$(temp%)
                init (hex(00)) cost$(temp%)
            if currentline% >= maxlines% and screenline% = 5%            ~
               then line% = max(0%, line% - 1%)
            mode$ = "E"
            return

L15930:     for temp% = currentline% to maxlines%
                part$     (temp%) = part$     (temp%+1)
                store$    (temp%) = store$    (temp%+1)
                lot$      (temp%) = lot$      (temp%+1)
                seq$      (temp%) = seq$      (temp%+1)
                descr$    (temp%) = descr$    (temp%+1)
                qty$      (temp%) = qty$      (temp%+1)
                totlcost$ (temp%) = totlcost$ (temp%+1)
                     get cost$(temp%+1) using L16030, cost()
                     put cost$(temp%) using L16030, cost()
L16030:                   FMT 12*PD(14,4)
                srcacct$  (temp%) = srcacct$  (temp%+1)
                dstacct$  (temp%) = dstacct$  (temp%+1)
                vendor$   (temp%) = vendor$   (temp%+1)
                ponr$     (temp%) = ponr$     (temp%+1)
                jobnr$    (temp%) = jobnr$    (temp%+1)
                index%    (temp%) = index%    (temp%+1)
            next temp%

            if maxlines% = 0 then return
               for temp% = 1 to max(maxlines%, currentline%)
                   if part$(temp%) = " " then L16160
                      convert temp% to seq$(temp%), pic(###)
L16160:        next temp%
            return

        REM *************************************************************~
            *              D E L E T E   M O D E   C O D E              *~
            *-----------------------------------------------------------*~
            * Deletes a line item from the invoice.  Note that we have  *~
            * to swipe the routine from insert mode that copies all the *~
            * data back down one line, which we will use here to delete *~
            * the actual line we want to.  This makes it easier to add  *~
            * fields to the line items since there's only one delete    *~
            * routine, although it breaks up the structure of the code. *~
            *************************************************************

        deletemode
                if maxlines% = 0 then return
                screenline% = int((cursor%(1)-1)/4)
                if screenline% < 1 then return
                currentline% = screenline% + line%
                if currentline% > maxlines% then return

L18180:         gosub'133(screenline%)
                      if keyhit%  =  1 then       return
                      if keyhit% <>  0 then       L18180

                c% = currentline%
                call "SERSTOVR" (index%(c%), "6", " ",  #12, #11)
                if currentline% < maxlines% then gosub L15930
                                         /* ACTUALLY DELETE LINE @C%   */
                temp% = maxlines%
                init(" ") seq$(temp%), part$(temp%), descr$(temp%),      ~
                          qty$(temp%), totlcost$(temp%), srcacct$(temp%),~
                          dstacct$(temp%), ponr$(temp%), lot$(temp%),    ~
                          errormsg$, infomsg$, store$(temp%),            ~
                          jobnr$(temp%), vendor$(temp%)
                init (hex(00)) cost$(temp%)
                maxlines% = maxlines% - 1%
                if currentline% >= maxlines% and screenline% = 5%        ~
                   then line% = max(0%, line% - 1%)
                return

        REM *************************************************************~
            *                      S A V E   D A T A                    *~
            *-----------------------------------------------------------*~
            * Writes data onto buffer, after making sure this user's    *~
            * buffer is empty (delete all old records), then exit       *~
            * program.                                                  *~
            *************************************************************

        datasave
            call "SHOSTAT" ("Now Saving Entered Data to File...")
            readkey$ = userid$
            call "DELETE" (#5, readkey$, 3%)
            gosub dataput
            if keyhit% = 32% then exit_program else inputmode

        REM *************************************************************~
            * S E T   D E F A U L T S   A N D   E N A B L E   I N P U T *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables input for the line items.       *~
            *************************************************************

            deffn'051(fieldnr%, mode%)
                  call "ENABLSUB" ("SET", "HNYADDNS", scr%(), set%(), 1%,~
                                   fieldnr%, mode%, enabled%)

                  if mode% = 2% then return
                  c% = currentline%
                  on fieldnr% gosub L20240,         /* PART             */~
                                    L20260,         /* STORE NUMBER     */~
                                    L20290,         /* LOT NUMBER       */~
                                    L20340,         /* QUANTITY ADDED   */~
                                    L20370,         /* COST             */~
                                    L20480,         /* SOURCE ACCOUNT   */~
                                    L20570,         /* DESTINATION ACCT */~
                                    L20700,         /* WHERE FROM TEXT  */~
                                    L20740,         /* DESCRIPTION      */~
                                    L21000,         /* PURCHASE ORDER # */~
                                    L21200          /* JOB NUMBER       */
                  return
L20240:     REM DEFAULT/ENABLE FOR PART
                return
L20260:     REM DEFAULT/ENABLE FOR STORE NUMBER
                if part$(c%) = " " then return
                return
L20290:     REM DEFAULT/ENABLE FOR LOT NUMBER
                if part$(c%) = " " then return
                     call "LOTENABL" (part$(c%), enabled%, ll%, #7, #3)
                     lotenabled% = enabled%
                     if enabled% = 0% then lot$(c%) = " "
                     return
L20340:     REM DEFAULT/ENABLE FOR QUANTITY ADDED
                if part$(c%) = " " then return
                   return
L20370: REM DEFAULT/ENABLE FOR COST
            if part$(c%) = " " then return
                readkey$ = str(part$(c%),,25) & str(store$(c%),,3) &     ~
                        lot$(c%)
                call "READ100" (#4, readkey$, f1%(4))
                if f1%(4) = 0 then L20400
                get #4, using L20390, totlcost, cost(), cstmthd$
L20390:             FMT POS(117), 13*PD(14,4), POS(403), CH(1)
                if cstmthd$ <> "S" and cstmthd$ <> "T" then L20425
                goto L20420

L20400:     get #3 using L20405, cstmthd$
L20405:             FMT POS(307), CH(1)
            if cstmthd$ <> "S" and cstmthd$ <> "T" and                   ~
                                   cstmthd$ <> "F" then L20450
L20420:     call "STCCOSTS" (part$(c%), " ", #7, 2%, totlcost, cost())
L20425:     call "CONVERT" (totlcost, 2.4, totlcost$(c%))
            put cost$(c%) using L20435, cost()
L20435:             FMT 12*PD(14,4)
           return

L20450:    mat cost = zer : totlcost = 0
           goto L20425

L20480:     REM DEFAULT/ENABLE FOR SOURCE ACCOUNT
                if part$(c%) = " " then return
                if srcacct$(c%) = " " then srcacct$(c%) = defsource$
                if srcacct$(c%) = " " then goto L20500 else goto L20540
L20500:         get #3 using L20501, temp$        /* HNYMASTR Part Type */
L20501:             FMT POS(180), CH(3)
                temp% = 100% : convert temp$ to temp%, data goto L20505
L20505:         src% = 1%                       /* Set for Purchased  */
                if temp% = 0%   then src% = 2%  /* WIP                */
                if temp% > 499% then src% = 2%  /* WIP                */
                   call "HNYGLGET" (part$(c%), store$(c%), lot$(c%),     ~
                                    srcacct$(c%), src%, #3, #4)
                if srcacct$(c%) = " " then return
L20540:         call "DESCRIBE"(#2,srcacct$(c%),infomsg$,1%,f1%(2))
                call "GLFMT" (srcacct$(c%))
                return
L20570:     REM DEFAULT/ENABLE FOR DESTINATION (INVENTORY) ACCOUNT
                if part$(c%) = " " then return
                   if srcacct$(c%) = " " then return
                   if f1%(4) <> 0 then L20640
                      call "HNYGLGET" (part$(c%), store$(c%), lot$(c%),  ~
                                       dstacct$(c%), 3%, #3, #4)
                      goto L20660
L20640:            get #4, using L20650, dstacct$(c%)
L20650:                    FMT POS(259), CH(9)
L20660:            if dstacct$(c%) = " " then return
                      call "DESCRIBE"(#2,dstacct$(c%),infomsg$,1%,f1%(2))
                      call "GLFMT" (dstacct$(c%))
                      return
L20700:     REM DEFAULT/ENABLE FOR WHERE FROM FREE TEXT
                if part$(c%) = " " then return
                   return

L20740:     REM DEFAULT/ENABLE FOR DESCRIPTION OF ENTRY
                if c% > 1% then descr$(c%) = descr$(c%-1%)
                return

L21000:     REM DEFAULT/ENABLE FOR PURCHASE ORDER NUMBER
                return

L21200:     REM DEFAULT/ENABLE FOR JOB NUMBER
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
            scr%(1, 5) =  5% : set%( 5) = 11%      /* Total Cost       */
            scr%(1, 6) =  6% : set%( 6) =  2%      /* Source           */
            scr%(1, 7) =  7% : set%( 7) =  2%      /* Destination      */
            scr%(1, 8) =  8% : set%( 8) =  2%      /* Vendor           */
            scr%(1, 9) =  9% : set%( 9) =  2%      /* Description      */
            scr%(1,10) = 10% : set%(10) =  2%      /* Purchase Order # */
            scr%(1,11) = 11% : set%(11) =  2%      /* Job Number       */

            call "ENABLSUB" ("INIT", "HNYADDNS", scr%(), set%(),         ~
                                                         0%, 0%, 0%, 0%)
            return

        REM *************************************************************~
            *           R E S E T   S O F T   E N A B L E S             *~
            * --------------------------------------------------------- *~
            * Allow User to modify enable settings.                     *~
            *************************************************************
        deffn'049(s%, f%)      /* Screen and Field Numbers             */
            if admin% <> 1% then return            /* Not Authorized   */
            call "ENABLSUB" ("MODIFY", "HNYADDNS", scr%(), set%(),       ~
                              s%, f%, 0%, 0%)
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 1%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return
            call "SERSTOVR" (0%, "6", " ",  #12, #11)
            return clear all
            goto inputmode

        REM *************************************************************~
            *    L O A D   A D D I T I O N S   F R O M   B U F F E R    *~
            *-----------------------------------------------------------*~
            * Loads inventory additions from buffer.  If none, then     *~
            * return.                                                   *~
            *************************************************************
        dataload
            old%, index% = 0%
            readkey$ = userid$ & hex(000000)
            t%, maxlines% = 0%
L30100:     REM GET LINE ITEMS FROM FILE QUICKLY.
                call "PLOWNEXT" (#5, readkey$, 3%, f1%(5))
                     if f1%(5) = 0% then return
                t%, maxlines% = maxlines% + 1%
                old% = 1%
                REM GET DATA FROM THE FILE.
                    get #5, using L30460,index%(t%),part$(t%), store$(t%),~
                            lot$(t%), jobnr$(t%), qty, dstacct$(t%),     ~
                            srcacct$(t%), cost(), vendor$(t%),           ~
                            descr$(t%), ponr$(t%)
                    put cost$(t%) using L30210, cost()
L30210:                  FMT 12*PD(14,4)
                totlcost = cost( 1) + cost( 2) + cost( 3) + cost( 4) +   ~
                     cost( 5) + cost( 6) + cost( 7) + cost( 8) +         ~
                     cost( 9) + cost(10) + cost(11) + cost(12)
                convert maxlines% to seq$(maxlines%), pic(###)
                call "GLFMT" (srcacct$(t%))
                call "GLFMT" (dstacct$(t%))
                call "CONVERT" (qty,   0.2, qty$     (t%))
                call "CONVERT" (totlcost, 2.4, totlcost$(t%))
                index% = index%(t%)
                call "SERLOAD" (                                         ~
                        index%,          /* Line Item Pointer.         */~
                        "IA",            /* Source Transaction Type    */~
                        readkey$,        /* Source Transaction Key     */~
                        100%,            /* # Trans to Create File for */~
                        " ",             /* If non-blank Load Serial   */~
                        " ",             /* Numbers from this Source & */~
                                         /* Location, Else from TIF.   */~
                        #7,              /* SYSFILE2 UFB               */~
                        #10,             /* SERTIF   UFB               */~
                        #12,             /* SERMASTR UFB               */~
                        #11, u3%)        /* SERWORK  UFB               */

                goto L30100

L30460: FMT                 /* FILE: ADDBUFFR                          */~
            XX(3),          /* user-id of specific user                */~
            BI(3),          /* seq. no. for additions buffer           */~
            XX(3),          /* item sequence number                    */~
            CH(25),         /* Part Number                             */~
            CH(3),          /* Store or Warehouse Code                 */~
            CH(16),         /* Lot Number                              */~
            CH(8),          /* Job Number                              */~
            PD(14,4),       /* detail to quantity on-hand              */~
            CH(9),          /* Inventory Asset Account                 */~
            CH(9),          /* inventory source account                */~
            12*PD(14,4),    /* Costs                                   */~
            CH(9),          /* Vendor Code                             */~
            CH(32),         /* description of purpose                  */~
            CH(16),         /* Purchase Order Number                   */~
            XX(60)          /* Filler (Internal, unused space)         */~

        REM *************************************************************~
            *     W R I T E   A D D I T I O N S   T O   B U F F E R     *~
            *-----------------------------------------------------------*~
            * Writes inventory additions to buffer.  Note that old      *~
            * entries must already have been deleted.                   *~
            *************************************************************
        dataput
            if maxlines% = 0% then clear_deleted_serial_nbrs

            for temp% = 1% to maxlines%
                totlcost, qty = 0
                if part$(temp%) = " " then L31210
                   convert qty$      (temp%) to qty
                   convert totlcost$ (temp%) to totlcost
                   get cost$(temp%) using L31150, cost()
L31150:                 FMT 12*PD(14,4)
                   call "PACKZERO" (cost(), cost$(temp%))
                   call "GLUNFMT" (dstacct$(temp%))
                   call "GLUNFMT" (srcacct$(temp%))
                   qty   = round(  qty, 2)
                   totlcost = round(totlcost, 4)
L31210:         write #5, using L31680,                                   ~
                          userid$,index%(temp%),seq$(temp%),part$(temp%),~
                          store$(temp%), lot$(temp%), jobnr$(temp%),     ~
                          qty, dstacct$(temp%), srcacct$(temp%),         ~
                          cost$(temp%), vendor$(temp%), descr$(temp%),   ~
                          ponr$(temp%), " "
                gosub serial_number_processing
            next temp%

        clear_deleted_serial_nbrs
            REM *** Clear Old, Deleted Entries from SERTIF ***
            readkey$ = "IA" & str(userid$) & hex(000000)
L31330:     call "PLOWNXT1" (#10, readkey$, 5%, f1%(10))
            if f1%(10) = 0% then return
            call "READ100" (#5, str(readkey$,3%,6%), f1%(5))
            if f1%(5) = 1% then L31450
L31370:        get #10 using L31480, str(plowkey$,26%), str(plowkey$,,25%)
               delete #10
               call "READ101" (#12, plowkey$, f1%(12))
               if f1%(12) = 0% then L31430
               status$ = str(key(#3,2),,1%) and hex(3f)
               delete #12
L31430:        call "PLOWNXT1" (#10, readkey$, 42%, f1%(10))
               if f1%(10) = 1% then L31370
L31450:     str(readkey$,4%,3%) = addc(hex(01))
            str(readkey$,7%) = " "
            goto L31330
L31480:     FMT POS(43), CH(20), CH(25)

        serial_number_processing
            readkey$ = str(userid$,,3%) & bin(index%(temp%),3)
            call "SERSAVE" (                                             ~
                        index%(temp%),   /* Line Item Pointer.         */~
                        "IA",            /* Source Transaction Type    */~
                        readkey$,        /* Source Transaction Key     */~
                        maxlines%,       /* # Trans to Create File for */~
                        part$(temp%),    /* Part Code                  */~
                        userid$,         /* Current User ID            */~
                        "2",             /* Change Status to...        */~
                        "6",             /* Change Status from... (N/A)*/~
                        0%,              /* Clear TIF after Save (NO)  */~
                        #7,              /* SYSFILE2 UFB               */~
                        #10,             /* SERTIF UFB                 */~
                        #12,             /* SERMASTR UFB               */~
                        #11)             /* SERWORK  UFB               */
            return

L31680: FMT                 /* FILE: ADDBUFFR                          */~
            CH(3),          /* user-id of specific user                */~
            BI(3),          /* seq. no. for additions buffer           */~
            CH(3),          /* item sequence number                    */~
            CH(25),         /* Part Number                             */~
            CH(3),          /* Store or Warehouse Code                 */~
            CH(16),         /* Lot Number                              */~
            CH(8),          /* Job Number                              */~
            PD(14,4),       /* detail to quantity on-hand              */~
            CH(9),          /* Inventory Asset Account                 */~
            CH(9),          /* inventory source account                */~
            CH(96),         /* costs                                   */~
            CH(9),          /* Vendor Code                             */~
            CH(32),         /* description of purpose                  */~
            CH(16),         /* Purchase Order Number                   */~
            CH(60)          /* Filler (Internal, unused space)         */

        REM *************************************************************~
            *   A C C E S S    L O C A T I O N    M A N A G E M E N T   *~
            *___________________________________________________________*~
            * Call HNYLCSUB and pass the Part, Store, Lot and Quantity  *~
            * so that the user does not have to re-enter them.          *~
            *************************************************************

         locations
            if qty$(currentline%) <> " " then L33080
               qty = 0
               goto L33100

L33080:     convert qty$(currentline%) to qty

L33100:     call "HNYLCSUB"  (part$(currentline%),                       ~
                              store$(currentline%),                      ~
                              lot$(currentline%),                        ~
                              qty,                                       ~
                              3%,  /*  Additions Mode                  */~
                              #7,  /*  SYSFILE2                        */~
                              #6,  /*  STORNAME                        */~
                              #1,  /*  USERINFO                        */~
                              #3,  /*  HNYMASTR                        */~
                              #13, /*  HNYLOCNS                        */~
                              #4,  /*  HNYQUAN                         */~
                              #14) /*  LOCATION                        */
            return

        REM *************************************************************~
            *       T A B L E   O P E R A T I O N S   S C R E E N       *~
            *-----------------------------------------------------------*~
            * This is the screen routine that handles input, edit,      *~
            * insert, and delete modes.                                 *~
            *************************************************************

            deffn'101(screenline%, fieldnr%)
                  screen% = 1
                  goto L40290

            deffn'111(screenline%, fieldnr%)
                  screen% = 2
                  init(hex(86)) fac$()
                  if fieldnr% = 0 then L40300
                  goto L40290

            deffn'123(screenline%, fieldnr%)
                  screen% = 3
                  goto L40290

            deffn'133(screenline%)
                  screen% = 4
                  init(hex(84)) fac$()
                  for temp% = 1 to 14
                      fac$(screenline%, temp%) = hex(94)
                  next temp%
                  goto L40610

L40290:           init(hex(84)) fac$()
L40300:           on fieldnr% gosub L40470,         /* PART             */~
                                    L40470,         /* STORE NUMBER     */~
                                    L40470,         /* LOT NUMBER       */~
                                    L40500,         /* QUANTITY         */~
                                    L40530,         /* COST             */~
                                    L40470,         /* SOURCE ACCOUNT   */~
                                    L40470,         /* INVENTORY ACCOUNT*/~
                                    L40470,         /* VENDOR CODE      */~
                                    L40440,         /* DESCRIPTION      */~
                                    L40470,         /* PURCHASE ORDER # */~
                                    L40470          /* JOB NUMBER       */
                  if fieldnr% = 5% then return /* No screen for cost */
                  goto L40610

L40440:     REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                fac$(screenline%, fieldnr%) = hex(80)
                return
L40470:     REM SET FAC'S FOR UPPER CASE ONLY INPUT
                fac$(screenline%, fieldnr%) = hex(81)
                return
L40500:     REM SET FAC'S FOR NUMERIC INPUT.
                fac$(screenline%, fieldnr%) = hex(82)
                return
L40530:     REM CALL 'HNYCDIST' FOR INPUT/EDIT OF TOTAL COST FIELD
                c% = currentline% : partdescr$ = " "
                call "DESCRIBE" (#3, part$(c%), partdescr$, 1%, f1%(3))
                call "HNYCDIST" (mode$, part$(c%), partdescr$,           ~
                     "HNYADDNS: Inventory Additions: Cost Distribution", ~
                     #7, cost$(c%), totlcost$(c%), totlcost)
                return

L40610:     accept                                                       ~
               at (01,02), fac(hex(8c)),    title$(screen%, 1)  , ch(79),~
               at (02,02), fac(hex(ac)),    title$(screen%, 2)  , ch(79),~
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
               at (07,02), "Cost Each "                                 ,~
               at (11,02), "Cost Each "                                 ,~
               at (15,02), "Cost Each "                                 ,~
               at (19,02), "Cost Each "                                 ,~
               at (23,02), "Cost Each "                                 ,~
                                                                         ~
               at (07,13), fac(fac$(1, 5)), totlcost$(line%+ 1) , ch(10),~
               at (11,13), fac(fac$(2, 5)), totlcost$(line%+ 2) , ch(10),~
               at (15,13), fac(fac$(3, 5)), totlcost$(line%+ 3) , ch(10),~
               at (19,13), fac(fac$(4, 5)), totlcost$(line%+ 4) , ch(10),~
               at (23,13), fac(fac$(5, 5)), totlcost$(line%+ 5) , ch(10),~
                                                                         ~
               at (07,25), "Source"                                     ,~
               at (11,25), "Source"                                     ,~
               at (15,25), "Source"                                     ,~
               at (19,25), "Source"                                     ,~
               at (23,25), "Source"                                     ,~
                                                                         ~
               at (07,32), fac(fac$(1, 6)), srcacct$ (line%+ 1) , ch(12),~
               at (11,32), fac(fac$(2, 6)), srcacct$ (line%+ 2) , ch(12),~
               at (15,32), fac(fac$(3, 6)), srcacct$ (line%+ 3) , ch(12),~
               at (19,32), fac(fac$(4, 6)), srcacct$ (line%+ 4) , ch(12),~
               at (23,32), fac(fac$(5, 6)), srcacct$ (line%+ 5) , ch(12),~
                                                                         ~
               at (07,46), "Dest"                                       ,~
               at (11,46), "Dest"                                       ,~
               at (15,46), "Dest"                                       ,~
               at (19,46), "Dest"                                       ,~
               at (23,46), "Dest"                                       ,~
                                                                         ~
               at (07,51), fac(fac$(1, 7)), dstacct$ (line%+ 1) , ch(12),~
               at (11,51), fac(fac$(2, 7)), dstacct$ (line%+ 2) , ch(12),~
               at (15,51), fac(fac$(3, 7)), dstacct$ (line%+ 3) , ch(12),~
               at (19,51), fac(fac$(4, 7)), dstacct$ (line%+ 4) , ch(12),~
               at (23,51), fac(fac$(5, 7)), dstacct$ (line%+ 5) , ch(12),~
                                                                         ~
               at (07,65), "Ven"                                        ,~
               at (11,65), "Ven"                                        ,~
               at (15,65), "Ven"                                        ,~
               at (19,65), "Ven"                                        ,~
               at (23,65), "Ven"                                        ,~
                                                                         ~
               at (07,69), fac(fac$(1, 8)), vendor$  (line%+ 1) , ch(09),~
               at (11,69), fac(fac$(2, 8)), vendor$  (line%+ 2) , ch(09),~
               at (15,69), fac(fac$(3, 8)), vendor$  (line%+ 3) , ch(09),~
               at (19,69), fac(fac$(4, 8)), vendor$  (line%+ 4) , ch(09),~
               at (23,69), fac(fac$(5, 8)), vendor$  (line%+ 5) , ch(09),~
                                                                         ~
               at (08,02), "Desc"                                       ,~
               at (12,02), "Desc"                                       ,~
               at (16,02), "Desc"                                       ,~
               at (20,02), "Desc"                                       ,~
               at (24,02), "Desc"                                       ,~
                                                                         ~
               at (08,07), fac(fac$(1, 9)), descr$   (line%+ 1) , ch(32),~
               at (12,07), fac(fac$(2, 9)), descr$   (line%+ 2) , ch(32),~
               at (16,07), fac(fac$(3, 9)), descr$   (line%+ 3) , ch(32),~
               at (20,07), fac(fac$(4, 9)), descr$   (line%+ 4) , ch(32),~
               at (24,07), fac(fac$(5, 9)), descr$   (line%+ 5) , ch(32),~
                                                                         ~
               at (08,47), "PO"                                         ,~
               at (12,47), "PO"                                         ,~
               at (16,47), "PO"                                         ,~
               at (20,47), "PO"                                         ,~
               at (24,47), "PO"                                         ,~
                                                                         ~
               at (08,50), fac(fac$(1,10)), ponr$    (line%+ 1) , ch(16),~
               at (12,50), fac(fac$(2,10)), ponr$    (line%+ 2) , ch(16),~
               at (16,50), fac(fac$(3,10)), ponr$    (line%+ 3) , ch(16),~
               at (20,50), fac(fac$(4,10)), ponr$    (line%+ 4) , ch(16),~
               at (24,50), fac(fac$(5,10)), ponr$    (line%+ 5) , ch(16),~
                                                                         ~
               at (08,67), "Prj"                                        ,~
               at (12,67), "Prj"                                        ,~
               at (16,67), "Prj"                                        ,~
               at (20,67), "Prj"                                        ,~
               at (24,67), "Prj"                                        ,~
                                                                         ~
               at (08,71), fac(fac$(1,11)), jobnr$   (line%+ 1) , ch(08),~
               at (12,71), fac(fac$(2,11)), jobnr$   (line%+ 2) , ch(08),~
               at (16,71), fac(fac$(3,11)), jobnr$   (line%+ 3) , ch(08),~
               at (20,71), fac(fac$(4,11)), jobnr$   (line%+ 4) , ch(08),~
               at (24,71), fac(fac$(5,11)), jobnr$   (line%+ 5) , ch(08),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)

               if screen% > 2% then L42140
                  if keyhit% <> 8% then L42140
                     gosub locations
                     goto L40610

L42140:        if keyhit% <> 13 then L42180
                  call "MANUAL" ("HNYADDNS")
                  goto L40610

L42180:        if keyhit% <> 15 then L42220
                  call "PRNTSCRN"
                  goto L40610

L42220:        if screen% <> 2 then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return
                  u3% = u3%

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Tests data for the line items.                            *~
            *************************************************************

            deffn'153(fieldnr%, edit%)
                  errormsg$, infomsg$ = " "
                  c% = currentline%
                  on fieldnr% gosub L50220,         /* PART             */~
                                    L50270,         /* STORE NUMBER     */~
                                    L50381,         /* LOT NUMBER       */~
                                    L50564,         /* QUANTITY ADDED   */~
                                    L50620,         /* COST             */~
                                    L50650,         /* SOURCE ACCOUNT   */~
                                    L50740,         /* DESTINATION ACCT */~
                                    L50840,         /* VENODR CODE      */~
                                    L50900,         /* DESCRIPTION      */~
                                    L51040,         /* PO NUMBER        */~
                                    L51070          /* JOB NUMBER       */
                   return

L50220:     REM TEST DATA FOR PART NUMBER
                call "GETCODE" (#3, part$(c%), infomsg$, 1%, 0, f1%(3))
                if f1%(3) = 1 then return
                   errormsg$ = "Inventory Part Not on File :" & part$(c%)
                   return
L50270
*        Test data for STORE
            call "GETCODE" (#6, store$(c%), infomsg$, 1%, 0, f1%(6))
            if f1%(6) = 1% then L50320
                errormsg$ = "Store not on file"
                return
L50320:     readkey$ = str(part$(c%),,25) & str(store$(c%),,3)
            init(hex(00))str(readkey$, 29)
            call "PLOWNEXT" (#4, readkey$, 28%, f1%(4))
            if f1%(4) = 1% then L50370
                infomsg$ = "No Quantity Record.  One will be created."
L50370:     if edit% = 1% then return

L50381
*        Test LOT NUMBER
            if lotenabled% <> 2% or lot$(c%) <> " " then goto L50391
                errormsg$ = "You must enter a Lot # for this Part."
                return
L50391:     call "LOTVALID" (part$(c%), store$(c%), lot$(c%), #7, #3, #4,~
                errormsg$)
            if errormsg$ = " " then goto L50396
L50394:         infomsg$ = " "
                return
L50396:     call "LOTUNQUE" (part$(), lot$(), c%, #7, errormsg$)
            if errormsg$ <> " " then goto L50394
            infomsg$ = " "
            readkey$ = str(part$(c%)) & str(store$(c%)) & lot$(c%)
            call "READ100" (#4, readkey$, f1%(4%))
            if f1%(4%) = 1% then L50440
                infomsg$ = "No Quantity Record.  One will be created."
L50440:     uom$ = " "
            call "READ100" (#3, part$(c%), f1%(3%))
                if f1%(3%) = 1% then get #3 using L50444, uom$
L50444:              FMT POS(74), CH(4)
            infomsg$ = infomsg$ & "  (" & uom$ & ")"
            if edit% = 1% then return

L50564:     REM TEST DATA FOR QUANTITY
                call "NUMTEST" (qty$(c%), .01, 1e9, errormsg$, 0.2, q)
                if errormsg$ > " " then return
                call "SERENABL" (part$(c%), enabled%, sl%, #7, #3)
                if enabled% = 1% then gosub enter_serial_numbers
                return
                sl% = sl%
L50620:     REM TEST DATA FOR TOTAL COST -- OBSOLETE -- NOW TAKEN OF BY
            REM THE SUBROUTINE 'HNYCDIST' -- JIM -- 05/11/87
                return
L50650:     REM TEST DATA FOR SOURCE ACCOUNT
        REM     IF SRCACCT$(C%) = " " THEN 50760
                call "GETCODE" (#2, srcacct$(c%), infomsg$, 1%, 0, f1%(2))
                if f1%(2) = 1 then return
                   errormsg$="Source Account not on file: " & srcacct$(c%)
                   return
                REM GOTTA ZAP THE OTHER ACCOUNT, TOO!!
                    dstacct$(c%), srcacct$(c%) = " "
                    return
L50740:     REM TEST DATA FOR INVENTORY (DESTINATION) ACCOUNT
        REM     IF DSTACCT$(C%) = " " THEN 50870
                call "GETCODE" (#2, dstacct$(c%), infomsg$, 1%, 0, f1%(2))
                if f1%(2) = 1 then return
                   errormsg$ = "Destination Account not on file: "       ~
                                                   & dstacct$(c%)
                   return
                REM ZAP OTHER ACCOUNT NUMBER TOO IF THIS ONE BLANK.
                    dstacct$(c%), srcacct$(c%) = " "
                    return
L50840:     REM TEST DATA FOR VENDOR FIELD
                if vendor$(c%) = " " then return
                call "GETCODE" (#8, vendor$(c%), infomsg$, 0%,1.3, f1%(8))
                     if f1%(8) = 1 then return
                     errormsg$ = "Vendor not on file: " & vendor$(c%)
                     return
L50900:     REM TEST DATA FOR DESCRIPTION OF ENTRY
            if descr$(c%) <> " " then goto L50950
                errormsg$ = "May not be blank. Enter a Description, '?', ~
        ~or Code."
                return
L50950:     if len(descr$(c%)) > 1% then return     /* Entered by user */
            if str(descr$(c%),,1%) = "?" then str(descr$(c%),,1%) = " "
            readkey$ = "INVADDDSC" & str(descr$(c%),,1%)
            call "PLOWCODE" (#20, readkey$, descr$(c%), 9%, .3, f1%(20%))
            if f1%(20%) <> 0% then return  /* Got Description for Code */
            if str(readkey$,10%,1%) = " " then L51000   /* Want 1-char? */
                descr$(c%) = str(readkey$,10%,1%)      /* 1-char Descr */
                return
L51000:     errormsg$ = "You must enter a Description, Code or '?'."
            return

L51040:     REM TEST DATA FOR PURCHASE ORDER NUMBER
                return

L51070:     REM TEST DATA FOR THE JOB NUMBER
                if jobnr$(c%) = " " then return
                call "GETCODE" (#9, jobnr$(c%), infomsg$, 0%, 0, f1%(9))
                          if f1%(9) = 1 then L51130
                errormsg$ = "Project not on file: " & jobnr$(c%)
                   return
L51130:    REM NOW LETS SEE IF THE JOB IS CLOSED AND WHEN
                get #9, using L51150, datejobclosed$
L51150:             FMT XX(44), CH(6)
                if datejobclosed$ = " " or ~
					   ~datejobclosed$ = blank_date$ then return
                if datejobclosed$ > hnydate$ then return
					 call "DATEFMT" (datejobclosed$)
                errormsg$ = "Project was closed on " & datejobclosed$
                return

        enter_serial_numbers
            plowkey$ = str(userid$) & bin(index%(c%),3)
            call "SERINSUB" (part$(c%), store$(c%), lot$(c%), q,         ~
                             index%(c%), 100%, "IA", plowkey$, errormsg$,~
                             #7, #3, #12, #11)
            return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Zaps Work File.  Displays foreground message that         *~
            * we're switching; generates return code of zero if         *~
            * there's nothing in the buffer.                            *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#11)
            REM Set return code for continuing to process.
                readkey$ = str(userid$) & hex(000000)
                call "PLOWNEXT" (#5, readkey$, 3%, f1%(5))
            end f1%(5)
