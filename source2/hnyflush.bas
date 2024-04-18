        REM *************************************************************~
            *                                                           *~
            *  H   H  N   N  Y   Y  FFFFF  L      U   U   SSS   H   H   *~
            *  H   H  NN  N  Y   Y  F      L      U   U  S      H   H   *~
            *  HHHHH  N N N   YYY   FFFF   L      U   U   SSS   HHHHH   *~
            *  H   H  N  NN    Y    F      L      U   U      S  H   H   *~
            *  H   H  N   N    Y    F      LLLLL   UUU    SSS   H   H   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYFLUSH - This program will perform a mass-withdraw      *~
            *            inventory for an assembly, to be run by a PROC *~
            *            called PROCFLUH and utilizing HNYWDWJN for     *~
            *            posting and journal printing. (This program is *~
            *            useful when SFC & PLANNING are not being used) *~
            *                                                           *~
            *            W.P. doc. 0222x design document added poten-   *~
            *            tial addition of the parent to inventory, as   *~
            *            well as changing the name of the procedure to  *~
            *            PROCFLSH. BackFlushing became its own creature *~
            *            under this project. See 4/28/93 comment below. *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/09/90 ! Original (Patterned after HNYWDWAL).     ! SID *~
            *          ! I have added the main user input screen  !     *~
            *          ! and allow the user to mass-withdraw from !     *~
            *          ! inventory given a parent part and qty.   !     *~
            * 09/91    ! QC Rework, Enhancements                  ! MLJ *~
            * 03/30/92 ! PRR 12320 Dflt Lot must have valid format! JDH *~
            * 06/22/92 ! PRR 12497 Added call to GLFMT.           ! JDH *~
            *          ! Effective BOM ID defaults in for parent. !     *~
            *          ! Store defaults from JBFLAGS setting.  New!     *~
            *          ! 'P' option for neg inv; honors HNYMASTR. !     *~
            *          ! PRR 12490 Allows lot tracked part withdrl!     *~
            *          ! PRR 12504 Now processes phantom parts.   !     *~
            * 07/10/92 ! Blank lot for non-lot tracked part option! JDH *~
            * 04/28/93 ! PRRs 10716 & 11937 Honor Interim Back-   ! JIM *~
            *          !   Flush/WIP G/L acct in SWITCHS.HNY.     !     *~
            *          !   Also did the enhancements per W.P.     !     *~
            *          !   doc. 0222x for potential inventory     !     *~
            *          !   addition of the Parent Part.           !     *~
            * 04/30/93 ! Added Time of Day to End of report.      ! JIM *~
            * 06/02/93 ! PRR 12947 Show Parent Stocking UOM on    ! JIM *~
            *          !   both the PLOWCODE and main screens.    ! JIM *~
            * 06/02/93 ! Parent Qty LJs for edit; RJs for display.! JIM *~
            * 06/02/93 ! Parent P/# Dim/No Tab in EDITMODE.       ! JIM *~
            * 06/02/93 ! Comps loaded at edit/validation of BOMID,! JIM *~
            *          !   Component Default Store & Lot.         ! JIM *~
            * 07/27/93 ! PRR 12996 Format W/Dwal Source Acct #.   ! JIM *~
            * 03/18/94 ! Chngd exception rpt & component reselect.! JDH *~
            * 06/25/96 ! Add blank date test var                  ! DER *~
            *************************************************************

        dim                                                              ~
            add_parent$1,                /* Add Parent to Inventory?   */~
            asset_acct$9,                /* Inv Asset Account          */~
            asse_pn$25,                  /* Assembly Part number       */~
            blank_date$8,                /* Blank date test            */~
            blank_lot$,                  /* Blank lot flag             */~
            bom$3,                       /* BOM ID from BOMMASTR       */~
            bom_id$3, bom_descr$32,      /* BOM ID for parent part     */~
            bomseq$3,                    /* BOM Seq number BOMMASTR    */~
            comp_pn$25,                  /* Comp part from BOMMASTR    */~
            compdeflot$6,                /* Component Def Lot          */~
            compdefstr$3, compdefstrdesc$32, /* Component Default Store*/~
            costmeth$1, costmethdesc$32, /* Parent Part Cost Method    */~
            avail$10, reqd$10,                                           ~
            cost(12), cost$(00900)96,    /* COST FIELDS                */~
            cst(12),                     /* Cost from HNYQUAN          */~
            cursor%(2),                  /* Cursor location for editing*/~
            date$8,                      /* Formatted Date             */~
            datejobclosed$8,             /* Date job closed test       */~
            defintwip$9,                 /* Default Int B/F/WIP account*/~
            dflt_store$3,                /* Default Store from JBFLAGS */~
            description$32,              /* Component Reason/Descr     */~
            descr$(00900)32,             /* Free text descriptions     */~
            descr_m(16),                 /* Descr Map For PlowCode     */~
            dstacct$(00900)9,            /* Destination (HNY) account  */~
            errormsg$79,                 /* Error message text         */~
            exception$1,                 /* Exceptions Exist Flag      */~
            exdescr$40,                  /* Exception Description      */~
            fac$(5,20)1,                 /* Field attribute characters */~
            fbom$3,                      /* Phantom's explicit Bill    */~
            from$(00900)16,              /* "where from" free text     */~
            header$(3)79,                /* Plowcode argument          */~
            hnydate$6,                   /* This users inventory date  */~
            hnyhold(00900),              /* Currently saved quantity   */~
            i$(24)80,                    /* Screen image--not used     */~
            incl_excl(1),                /* PLOWCODE Argument          */~
            incl_excl$(1)10,             /* PLOWCODE Argument          */~
            infomsg1$79, infomsg2$79,    /* Informative messages       */~
            inpmessage$79,               /* Informational message      */~
            intwipacct$12, intwipdesc$32,/* Interim B/F (WIP) acct     */~
            invassacct$12, invassdesc$32,/* Inventory Asset acct       */~
            jobnr$(00900)8,              /* Job number                 */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            llfac$(20)1,                 /* 1st Screen FAC             */~
            line2$79,                    /* Screen line #2             */~
            lot$16, lotfac$1,            /* Parent Lot # & FAC         */~
            lot$(00900)16,               /* Lot number (Components)    */~
            lot_protected$1,             /* Lot Protection- neg inv    */~
            lot_tracking$1,              /* Lot Trackinq Req'd Flag    */~
            mark$2,                      /* Marker in Bill             */~
            mode$1,                      /* HNYCDIST Mode              */~
            msg1$79,                     /* ASKUSER Message Line       */~
            name$60,                     /* Company Name for Report    */~
            neg_inv$1,                   /* Allowing Neg. Inv. Flag    */~
            parentcost(12), parentcost$12,/* Parent Part's cost stuff  */~
            parentdescr$32,              /* Parent Part # Reason/Descr */~
            parentdist$96,               /* Parent Part's cost stuff   */~
            part$25, partdescr$34,       /* Parent Part # & Descript'n */~
            part$(00900)25,              /* Part number this line      */~
            pf2$13, pf2key$1,                                            ~
            pf10msg$21, pf10key$1, pf10fac$1,/* PF(10)Edit Cost stuff  */~
            pfkeys$(4)17,                /* Pf keys for tabular screens*/~
            pfkeys1$11,                  /* PF key hex values          */~
            phfact(101),                 /* Phantom Factors            */~
            plowkey$99,                  /* Key for plow routines      */~
            plowkey$(100)99,             /* Key for phantoms           */~
            plowhny$99,                  /* Key for plow routines      */~
            qty$10,                      /* Qty of Parent Part         */~
            qty$(00900)10,               /* Quantity added this line   */~
            readkey$99,                  /* Key for plow routines      */~
            runtime$8,                   /* Report Run Time            */~
            separator$(5)79,             /* Separator lines for tables */~
            serial_nbr$1,                /* Serial # Tracking Req'd    */~
            scr%(5,10), set%(255),       /* Soft Enable Tables         */~
            seq$(00900)3,                /* Sequence numbers for screen*/~
            srcacct$(00900)16,           /* Source account             */~
            stkuom$4, stkuomdescr$32,    /* Stocking UOM & Description */~
            store$3,                     /* Parent Part Store #        */~
            store$(00900)3,              /* Component Store #s         */~
            totlcost$(00900)10,          /* COST INFORMATION           */~
            tttle$(4,2)79,               /* Titles for tabular screens */~
            tran$(24)80,                 /* For edit computations      */~
            userid$3                     /* Userid of current user     */

        dim f2%(64),                     /* File status flags for      */~
            f1%(64),                     /* Record-on-file flags       */~
            rslt$(64)20,                 /* Return code from "OPENFILE"*/~
            axd$(64)4                    /* Axd pointer from "OPENFILE"*/

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
            *  #1 ! USERINFO ! Default information for this user        *~
            *  #2 ! GLMAIN   ! General ledger CHart of account file     *~
            *  #3 ! HNYMASTR ! Inventory master file                    *~
            *  #4 ! HNYQUAN  ! Inventory store quantity file            *~
            *  #5 ! HNYFSHTF ! Add/Withdrawal buffer for inventory      *~
            *  #6 ! STORNAME ! Store names and addresses                *~
            *  #7 ! SYSFILE2 ! System information (months open)         *~
            *  #8 ! JOBMASTR ! Wip/jc master file                       *~
            *  #9 ! HNYPOOL  ! Inventory LIFO/FIFO Pool records         *~
            * #10 ! ENGMASTR ! Estimate master file                     *~
            * #14 ! BOMMASTR ! BOM relationship file                    *~
            * #20 ! GENCODES ! Control System Codes File                *~
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

            select  #5, "HNYFSHTF",                                      ~
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

            select #10, "ENGMASTR",                                      ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos =   1, keylen =  29

            select #14, "BOMMASTR",                                      ~
                        varc,     indexed,  recsize  =  150,             ~
                        keypos =  26,  keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56

            select #20, "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos =    1,  keylen = 24

            call "SHOSTAT"  ("Opening Files, One Moment Please...")
            rslt$(1%),rslt$(3%),rslt$(4%),rslt$(6%),rslt$(7%) = "REQUIRED"
            call "OPENFILE" (# 1,"SHARE", f2%( 1%), rslt$( 1%), axd$( 1%))
            call "OPENFILE" (# 2,"SHARE", f2%( 2%), rslt$( 2%), axd$( 2%))
            call "OPENFILE" (# 3,"SHARE", f2%( 3%), rslt$( 3%), axd$( 3%))
            call "OPENFILE" (# 4,"SHARE", f2%( 4%), rslt$( 4%), axd$( 4%))
            call "OPENFILE" (# 5,"SHARE", f2%( 5%), rslt$( 5%), axd$( 5%))
            call "OPENFILE" (# 6,"SHARE", f2%( 6%), rslt$( 6%), axd$( 6%))
            call "OPENFILE" (# 7,"SHARE", f2%( 7%), rslt$( 7%), axd$( 7%))
            call "OPENFILE" (# 8,"SHARE", f2%( 8%), rslt$( 8%), axd$( 8%))
            call "OPENFILE" (# 9,"SHARE", f2%( 9%), rslt$( 9%), axd$( 9%))
            call "OPENFILE" (#10,"SHARE", f2%(10%), rslt$(10%), axd$(10%))
            call "OPENFILE" (#14,"SHARE", f2%(14%), rslt$(14%), axd$(14%))
            call "OPENFILE" (#20, "SHARE", f2%(20), rslt$(20), axd$(20))
            if f2%(1%) + f2%(3%) + f2%(6%) + f2%(7%)<>0% then exit_program
            if f2%(5%) = 0 then L09000
               call "OPENFILE" (#5, "OUTPT", f2%(5%), rslt$(5%), axd$(5%))
               close #5
               call "OPENFILE" (#5, "SHARE", f2%(5%), rslt$(5%), axd$(5%))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *************************************************************
            blank_date$ = " "
            call "DATUFMTC" ( blank_date$ )

            call "EXTRACT" addr ("ID", userid$)
            pf10msg$ = "(10)Edit Parent Cost" & hex(8c)

L09070
*        See if User has any TIF Records that need to be Processed.
            readkey$ = str(userid$) & hex(000000)
            call "PLOWNEXT" (#5, readkey$, 3%, f1%(5%))    /* HNYFSHTF */
            if f1%(5%) = 0% then L09200
                i% = 0%
                call "ASKUSER" (i%, "*** IS THIS A RESTART? ***",        ~
                     "Addition/Withdrawal records exist in HNYFSHTF for"&~
                     " user " & userid$ & ".", "These records must be p"&~
                     "rocessed prior to entering more.", "Press (RETURN"&~
                     ") to continue & process; PF(1) to abort.")
                if i% =  0% then goto absolute_end
                if i% =  1% then f1%(5%) = 0%
                if i% <> 1% then goto L09070
                goto absolute_end

L09200
*        Get default pick store from SYSFILE2 set via JBFLAGS
            call "READ100" (#7, "SWITCHS.SFC", f1%(7%))
            if f1%(7%) <> 0% then get #7 using L09230, dflt_store$
L09230:         FMT POS(37), CH(3)

*        Get default Interim BackFlush/WIP G/L account #, if any.
            call "READ100" (#7, "SWITCHS.HNY", f1%(7%))
            if f1%(7%) <> 0% then get #7 using L09280, defintwip$
L09280:         FMT POS(139), CH(9)

*        See if User is an administator
            call "CMSMACHK" ("HNY", lfac$(1%), lfac$(2%))
            if lfac$(1%) = "Y" or lfac$(2%) = "Y" then admin% = 1%       ~
                                                  else admin% = 0%
            admin% = 1%
            gosub init_enables
            ll% = 6%      /* Default max lot length        */
            REM Retrieve inventory date or exit
                call "READ100" (#1, userid$, f1%(1%))
                      if f1%(1%) = 1 then L09440
                call "ASKUSER" (0%, "***ERROR***",                       ~
                     "Unable to locate your Inventory Posting Date!",    ~
                     " ", "Press (RETURN) to Acknowledge and Exit.")
                       go to exit_program
L09440:         get #1, using L09450, hnydate$
L09450:                 FMT XX(27), CH(6)
                date$ = hnydate$
                call "DATEFMT" (date$)
                call "WHICHMON" (#7, hnydate$, thismonth%)
                if thismonth% > 0 and thismonth% < 4 then L09580
                call "ASKUSER" (0%, "INVALID POSTING DATE",              ~
                     "Your Inventory Posting Date is not within the " &  ~
                     "posting window.",                                  ~
                     "Please change your posting date & try again.",     ~
                     "Press (RETURN) to acknowledge and Exit.")
                      go to exit_program

            REM SET SCREEN TITLES
L09580:     tttle$(1%,2%) = "(2)Col 1   (6)Copy Above   (10)See Stores/Lo~
        ~ts   (16)EXIT/EDIT MODE"
            tttle$(1%,1%) = "BackFlush From &/or To Inventory"
            str(tttle$(1%,1%),61%) = "Post Date: " & date$
            tttle$(2%,1%) = "(2)First     (3)Last     (4)Prev     (5)Next~
        ~  (6)Down     (7)Up    (9)Header"
            tttle$(2%,2%) = "(10)See Stores/Lots        (11)Insert       ~
        ~(12)Delete            (16)Save Data"
            tttle$(3%,1%) = "Supply requested items and (RETURN) or (1) t~
        ~o Exit Insert Mode"
            tttle$(4%,1%) = "Press (RETURN) to delete flashing line or (1~
        ~) to exit Delete."

            pfkeys$(1%) = hex(0002060a0f10ffffffffffffffffffffff)
            pfkeys$(2%) = hex(00020304050607090a0b0c0f10ffffffff)
            pfkeys$(3%) = hex(00010a0fffffffffffffffffffffffffff)
            pfkeys$(4%) = hex(00010a0fffffffffffffffffffffffffff)

            REM Set TRAN$ for edit mode field number computation.
                init(hex(00)) str(tran$(),     1%)
                init(hex(01)) str(tran$(6%),  1%, 35%)
                init(hex(02)) str(tran$(6%), 36%, 10%)
                init(hex(03)) str(tran$(6%), 46%, 12%)
                init(hex(04)) str(tran$(6%), 57%, 17%)
                init(hex(06)) str(tran$(7%), 43%, 17%)
                init(hex(07)) str(tran$(7%), 63%, 15%)
                init(hex(08)) str(tran$(8%),  1%, 22%)
                init(hex(09)) str(tran$(8%), 26%, 38%)
                init(hex(0a)) str(tran$(8%), 66%, 13%)
                copy str(tran$(), 321%, 1360%) to str(tran$(), 641%)

            maxcomps% = dim(part$(), 1) /* Max # of components */
            call "COMPNAME" (12%, name$, ret%)
            ret%, lctr%, pctr%, rpt%, compctr%, eall%  = 0%
            runtime$ = " "  :  call "TIME" (runtime$)

            date$ = date
            call "DATEFMT" (date$)

            str(line2$,62%) = "HNYFLUSH: " & str(cms2v$,,8%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode_1
            gosub initialize_variables
            compctr% = maxlines% :  if rpt% = 1% then print
            if eall% <> 1% then L10120
               readkey$ = userid$
               call "DELETE" (#5, readkey$, 3%) :  eall% = 0%
L10120:     for fieldnr% = 1% to 13%
                if fieldnr% = 1% then pf16$ = "(16)Exit Program"         ~
                     else pf16$ = " "
                gosub'222(0%, fieldnr%) /* EN/DISable PF 2 */
                if fieldnr% <> 1% then L10190
                    pf1$, pf4$ = " "
                    goto L10200
L10190:         pf4$ = "(4)Previous"  :  pf1$ = "(1)Start Over"
L10200:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10370
L10220:         gosub'101(fieldnr%, 0%)/* Display / Accept  */
                      infomsg1$, infomsg2$ = " "
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 10% then gosub cost_distribution
                      if keyhit% <>  2% then       L10290
                          came_from_inputmode% = 1%
                          goto editpg2
L10290:               if keyhit% <>  4% then       L10350
L10300:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10220
                         if fieldnr% = 1% then L10200
                         goto L10300
L10350:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10220
L10370:         gosub'151(fieldnr%, 0%) /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10220
            next fieldnr%
            gosub components                    /* Load the Components */

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            inpmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            pf4$ = " " : pf16$ = "(16)Save Data"
            pfkeys1$ = hex(0001ff040d0f10ffffffff)
            lastfieldnr% = 0%
            gosub'222(1%, 0%) /* EN/DISable PF 2 */
            gosub'101(0%, 1%)           /* Display Screen - No Entry  */
                  infomsg1$, infomsg2$ = " "
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 10% then gosub cost_distribution
                  if keyhit%  =  2% then goto editpg2
                  if keyhit% =  16% then       datasave
L11220:           if keyhit% <>  0% then       editpg1
            fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 2% or fieldnr% > 13% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
            gosub'222(1%, fieldnr%) /* EN/DISable PF 2 */
L11290:     gosub'101(fieldnr%, 1%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11290
            gosub'151(fieldnr%, 1%)     /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11290
                  lastfieldnr% = fieldnr%
            goto L11220

        deffn'222(k%, q%) /* EN/DISable PF 2 */
            q% = 0%
            pf2key$ = hex(ff)
            pf2$ = " "      /* DISABLE it and LEAVE it disabled if ... */
            if k% = 0% and maxlines% = 0% then return
            if maxlines% > maxcomps% then return           /* too many */
            pf2$ = "(2)Line Items"                      /* enable PF 2 */
            pf2key$ = hex(02)
            return

        cost_distribution
            if mode$ = " " then mode$ = "I" else mode$ = "E"
            call "HNYCDIST" (mode$, part$, partdescr$, "BackFlush Paren"&~
                "t Part Cost Distribution", #7, parentdist$, parentcost$,~
                parentcost)
            if mode$ = "E" then goto L51250 else goto cost_distribution

        REM *************************************************************~
            *      E D I T   I N V E N T O R Y   W I T H D R A W A L S  *~
            *-----------------------------------------------------------*~
            * Edits inventory withdraws and also permits insert and     *~
            * delete to occur.                                          *~
            *************************************************************

        editpg2
            line%, currentline%, screenline% = 0%
            errormsg$, infomsg1$, infomsg2$ = " "
            call"SETSEP"(separator$(),line%,min(maxlines%-line%,5%))
L13110:     edit% = 1%
L13120:     gosub'213(0%, 0%)
                  if keyhit%  =  0% then       L13360
                  if keyhit%  =  2% then line% = 0%
                  if keyhit%  =  3% then line% = max(0%,maxlines%-5%)
                  if keyhit%  =  4% then line% = max(0%,line%-5%)
                  if keyhit%  =  5% then line% = min(line%+5%,max(0%,    ~
                                                 maxlines%-5%))
                  if keyhit%  =  6% then line% = max(0%,line%-1%)
                  if keyhit%  =  7% then line% = min(line%+1%,max(0%,    ~
                                                 maxlines%-5%))
                  if keyhit% <>  9% then L13250
                     if came_from_inputmode% = 1% then inputmode_1
                         goto editpg1
L13250:           if keyhit%  = 11% then gosub insertmode
                  if keyhit% <> 12% then L13310
                      gosub deletemode
                      if maxlines% > 0% then L13310
                          eall% = 1%
                          exception$ = " "
L13310:           if keyhit%  = 16% then       datasave
                  if keyhit%  = 29% then       L13360
                  call"SETSEP"(separator$(),line%,min(maxlines%-line%,5%))
                  goto L13110

L13360:     REM Now figure out which field he hit.
                fieldnr% = val(str(tran$(cursor%(1%)),cursor%(2%)))
                if keyhit% <> 29% then L13430
                   gosub'049(1%, fieldnr%)
                   goto editpg2
                if fieldnr% < 4% then L13120 /* Can't chg because of S/N */
                if fieldnr% > 10% then L13120 /* Invalid field selected */
L13430:         screenline% = (cursor%(1%) - 5%) / 4% + 1%
                currentline% = line% + screenline%
                if currentline% > maxlines% then L13120

                gosub'163(fieldnr%, 2%)    /* Test Enables             */
                if enabled% = 0% then editpg2
L13490:         gosub'213(screenline%, fieldnr%)
                      if keyhit% <> 0% and keyhit% <> 29% then L13490
                gosub'153(fieldnr%)
                      if errormsg$ <> " " then L13490
                goto L13120

        REM *************************************************************~
            *              I N S E R T   M O D E   C O D E              *~
            *-----------------------------------------------------------*~
            * Handles insertion of a line item.                         *~
            *************************************************************

        insertmode
L15070:     if maxlines% = maxcomps% then return        /* Array full */
            REM Otherwise, set currentline%, screenline%, and copy right
                screenline% = max(int((cursor%(1%) - 1%) / 4%), 1%)
                if line% + screenline% < maxlines% then L15120
                   screenline% = maxlines% - line% /* To ins at end    */
L15120:         if screenline% <> 5% then L15170     /* Bottom of page   */
                   line% = line% + 1%
                   screenline% = screenline% - 1%
                   goto L15170

L15170:         call "SETSEP" (separator$(),line%,                       ~
                                      min(5%, maxlines%-line%+1%))
                currentline%, c% = screenline% + line%

            REM Copy all the elements up one
                if c% >= maxlines% then L15400
                for temp% = maxlines% to c% step -1%
                    part$    (temp%+1%) = part$    (temp%)
                    store$   (temp%+1%) = store$   (temp%)
                    lot$     (temp%+1%) = lot$     (temp%)
                    descr$   (temp%+1%) = descr$   (temp%)
                    qty$     (temp%+1%) = qty$     (temp%)
                    hnyhold  (temp%+1%) = hnyhold  (temp%)
                    totlcost$(temp%+1%) = totlcost$(temp%)
                          get cost$(temp%) using L15330, cost()
                          put cost$(temp%+1%) using L15330, cost()
L15330:                        FMT 12*PD(14,4)
                    srcacct$ (temp%+1%) = srcacct$ (temp%)
                    dstacct$ (temp%+1%) = dstacct$ (temp%)
                    from$    (temp%+1%) = from$    (temp%)
                    jobnr$   (temp%+1%) = jobnr$   (temp%)
                next temp%

L15400:         screenline% = screenline% + 1%
                c%, currentline% = currentline% + 1%

                init(" ") part$(c%), descr$(c%), qty$(c%), totlcost$(c%),~
                          srcacct$(c%),                                  ~
                          dstacct$(c%), from$(c%), seq$(c%), lot$(c%),   ~
                          store$(c%), jobnr$(c%)
                hnyhold(c%) = 0 : init (hex(00)) cost$(c%)
            REM Now input the line, make so we can cancel out if necc
                infomsg1$, infomsg2$ = " " : edit% = 0%
                for fieldnr% = 1% to 10%
                    gosub'163(fieldnr%, 1%)
                          if enabled% = 0% then L15560
L15530:             gosub'223(screenline%, fieldnr%)
                          if keyhit%  =  1% then L15700
                          if keyhit% <>  0% then L15530
L15560:             gosub'153(fieldnr%)
                          if errormsg$ <> " " then L15530
                next fieldnr%

                maxlines% = maxlines% + 1%
                REM RENUMBER ITEM NUMBERS, THEN SET KLUGE CURSOR%(1)
                    if part$(currentline%) = " " then L15670
                       for temp% = 1% to max(maxlines%, currentline%+1%)
                           if part$(temp%) = " " then L15660
                              convert temp% to seq$(temp%), pic(###)
L15660:                next temp%
L15670:         cursor%(1%) = min(cursor%(1%) + 4%, 24%)
                goto L15070

L15700:     REM This routine aborts insert mode and destroys screenline%
                c% = currentline%
                gosub L15860              /* Actually delete @C%        */

                temp% = maxlines% + 1%
                init(" ") seq$(temp%), part$(temp%), descr$(temp%),      ~
                          qty$(temp%), totlcost$(temp%),                 ~
                          srcacct$(temp%), dstacct$(temp%), from$(temp%),~
                          lot$(temp%), store$(temp%), jobnr$(temp%),     ~
                          errormsg$, infomsg1$, infomsg2$
                hnyhold(temp%) = 0 : init (hex(00)) cost$(temp%)
                edit% = 1%
            if currentline% >= maxlines% and screenline% = 5%            ~
               then line% = max(0%, line% - 1%)
            return

L15860:     for temp% = currentline% to maxlines%
                part$     (temp%) = part$     (temp%+1%)
                store$    (temp%) = store$    (temp%+1%)
                lot$      (temp%) = lot$      (temp%+1%)
                seq$      (temp%) = seq$      (temp%+1%)
                descr$    (temp%) = descr$    (temp%+1%)
                qty$      (temp%) = qty$      (temp%+1%)
                hnyhold   (temp%) = hnyhold   (temp%+1%)
                totlcost$ (temp%) = totlcost$ (temp%+1%)
                cost$     (temp%) = cost$     (temp%+1%)
                srcacct$  (temp%) = srcacct$  (temp%+1%)
                dstacct$  (temp%) = dstacct$  (temp%+1%)
                from$     (temp%) = from$     (temp%+1%)
                jobnr$    (temp%) = jobnr$    (temp%+1%)
            next temp%

            if maxlines% = 0% then return
               for temp% = 1% to max(maxlines%, currentline%)
                   if part$(temp%) = " " then L16060
                      convert temp% to seq$(temp%), pic(##0)
L16060:        next temp%
            return

        REM *************************************************************~
            *              D E L E T E   M O D E   C O D E              *~
            *-----------------------------------------------------------*~
            * Deletes a line item from the array.    Note that we have  *~
            * to swipe the routine from insert mode that copies all the *~
            * data back down one line, which we will use here to delete *~
            * the actual line we want to.  this makes it easier to add  *~
            * fields to the line items since there's only one delete    *~
            * routine, although it breaks up the structure of the code. *~
            *************************************************************

        deletemode
                if maxlines% = 0% then return
                screenline% = int((cursor%(1%) - 1%) / 4%)
                if screenline% < 1% then return
                currentline% = screenline% + line%
                if currentline% > maxlines% then return
                if hnyhold(currentline%) = 0% then L18200
                   if delmax% > 899% then return

L18200:         gosub'233(screenline%)
                      if keyhit%  =  1% then       return
                      if keyhit% <>  0% then       L18200

                c% = currentline%
                if hnyhold(c%) = 0 then L18290
                call "HNYHOLD" (#4, part$(c%), store$(c%), lot$(c%),     ~
                                    -hnyhold(c%), return%)
                delmax% = delmax% + 1%
L18290:         if currentline% < maxlines% then gosub L15860
                                         /* ACTUALLY DELETE LINE @C%   */
                temp% = maxlines%
                init(" ") seq$(temp%), part$(temp%), descr$(temp%),      ~
                          qty$(temp%), totlcost$(temp%), srcacct$(temp%),~
                          dstacct$(temp%), from$(temp%), lot$(temp%),    ~
                          errormsg$, infomsg1$, store$(temp%), infomsg2$,~
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
            if maxlines% = 0% and eall% = 1% then L19310
L19110:
            if exception$ <> "Y" then L19280
                u3% = 0%
                if data5% = 0% then msg1$ =                              ~
                    "All Lines Reported As Exceptions For " & part$      ~
                else msg1$ = "Exceptions Reported For " & part$
                call "ASKUSER" (u3%, "***** EXCEPTIONS *****", msg1$,    ~
                     "Press (RETURN) To Continue - OR - ",               ~
                     "PF16 To Cancel " & part$ & " Processing")
                if u3% <> 0% then L19220
                     goto L19280
L19220:         if u3% = 16% then L19240
                goto L19110
L19240:             print using L35250
                    maxlines% = compctr%

L19280
*        Write the Addition of the Parent (maybe) & Component W/drawals.
            if maxlines% > 0% then gosub L32000
            if add_parent$ = "Y" then gosub L31000
L19310:     goto inputmode_1

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            *                      H E A D E R                          *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20230,         /* Parent Part Number     */~
                              L20280,         /* Parent Part BOM ID     */~
                              L20350,         /* Qty of Parent Part     */~
                              L20400,         /* Add Parent?/Store/Lot  */~
                              L20490,         /* Parent Part Cost Meth. */~
                              L20580,         /* Parent Reason/Descr    */~
                              L20680,         /* Interim B/F (WIP) acct */~
                              L20890,         /* Inventory Asset acct   */~
                              L21010,         /* Component Def Store #  */~
                              L21060,         /* Component Def Lot #    */~
                              L21110,         /* Component Reason/Descr */~
                              L21170,         /* Neg Inventory Flag     */~
                              L21230          /* Blank Lot Flag         */
            return

L20230: REM Def/Enable Parent Part Number          PART$
            inpmessage$ = "Enter a Parent Part Number or '?' to see a lis~
        ~t."
            return

L20280: REM Def/Enable Parent Part BOM ID          BOM_ID$
            if bom_id$ = " " then call "BOMFIND" (part$, hnydate$, #10,  ~
                #7, bom_id$)
            inpmessage$ = "Enter the BOM ID of the Parent or '?' to searc~
        ~h the BOMMASTR file."
            return

L20350: REM Def/Enable Qty of Parent Part          QTY$
            inpmessage$ = "Enter the Quantity of Parent Part to withdraw ~
        ~components for."
            if qty$ <> " " then call "STRING" addr ("LJ", qty$,          ~
                len(str(qty$)))
            return

L20400: REM Def/Enable Add Parent? to Store? Lot?  ADD_PARENT$/STORE$/LOT$
            if add_parent$ = " " then add_parent$ = "Y"
            if add_parent$ <> "Y" then return
            if store$ = " " then store$ = dflt_store$
            inpmessage$ = "Enter 'Y' or 'N' to add Parent to Inventory; i~
        ~f 'Y', enter Store and Lot."
            call "LOTENABL" (part$, lotenabled%, ll%, #7, #3)
            return

L20490: REM Def/Enable Parent Part Cost Method     COSTMETH$
            if add_parent$ = "Y" then goto L20540
                enabled% = 0%
                costmeth$, costmethdesc$ = " "
                return
L20540:     inpmessage$ = "'S'=Standard; 'M'=Manual entry; 'C'=sum of Com~
        ~ponents at journal time."
            return

L20580: REM Def/Enable Parent Reason/Descr         PARENTDESCR$
            if add_parent$ = "Y" then goto L20630
                enabled% = 0%
                parentdescr$ = " "
                return
L20630:     inpmessage$ = "Enter the Parent Part Inventory addition descr~
        ~iption."
            if parentdescr$ <> " " then return
                parentdescr$ = part$ & ": " & bom_id$
                return

L20680: REM Def/Enable Interim B/F (WIP) account   INTWIPACCT$
            if intwipacct$ = " " then intwipacct$ = defintwip$
            if intwipacct$ = " " then goto L20710 else goto L20830
L20710:     call "READ100" (#03, part$, f1%(3%))           /* HNYMASTR */
            if f1%(3%) <> 0% then get #03 using L20730, temp$
L20730:         FMT POS(180), CH(3)              /* HNYMASTR Part Type */
            temp% = 100% : convert temp$ to temp%, data goto L20750
L20750:     src% = 1%                             /* Set for Purchased */
            if temp% = 0% then src% = 2%                        /* WIP */
            if temp% > 499% then src% = 2%                      /* WIP */
            call "HNYGLGET" (part$, store$, lot$, intwipacct$, src%,     ~
                #03, #04)
            if intwipacct$ <> " " then goto L20830
            if f1%(3%) <> 0% then get #03 using L20820, intwipacct$
L20820:         FMT POS(344), CH(9)        /* HNYMASTR Mfg Source Acct */
L20830:     call "DESCRIBE"(#02, intwipacct$, intwipdesc$, 1%, f1%(2%))
            if f1%(2%) = 0% then intwipdesc$ = "(Acct not on file)"
            call "GLFMT" (intwipacct$)
            inpmessage$ = "Enter the Interim BackFlush (WIP) account."
            return

L20890: REM Def/Enable Inventory Asset account     INVASSACCT$
            call "HNYGLGET" (part$, store$, lot$, invassacct$, 3%, #03,  ~
                #04)
            if invassacct$ <> " " then goto L20950
            if f1%(3%) <> 0% then get #03 using L20940, invassacct$
L20940:         FMT POS(353), CH(9)
L20950:     call "DESCRIBE"(#02, invassacct$, invassdesc$, 1%, f1%(2%))
            if f1%(2%) = 0% then invassdesc$ = "(Acct not on file)"
            call "GLFMT" (invassacct$)
            inpmessage$ = "Enter the Inventory Asset account."
            return

L21010: REM Def/Enable Component Default Store #   COMPDEFSTR$
            if compdefstr$ = " " then compdefstr$ = dflt_store$
            inpmessage$ = "Enter the Component Default Store Number."
            return

L21060: REM Def/Enable Component Default Lot #     COMPDEFLOT$
            inpmessage$ = "Enter the Component Default Lot Number (option~
        ~al)."
            return

L21110: REM Def/Enable Component Reason/Descr      DESCRIPTION$
            inpmessage$ = "Enter the Component Inventory withdrawal descr~
        ~iption."
            if description$ <> " " then return
                description$ = part$ & ": " & bom_id$
                return

L21170: REM Def/Enable Neg Inventory Flag (Y/N)  NEG_INV$
            if neg_inv$ = " " then neg_inv$ = "P"
            inpmessage$ = "Enter 'Y' to allow negative inventory; 'N' " &~
                          "to not allow; 'P' to honor Part."
            return

L21230: REM Def/Enable Use Blank Lot Flag (Y/N)  BLANK_LOT$
            if blank_lot$ = " " then blank_lot$ = "Y"
            inpmessage$ = "For Non-Lot Tracked Parts, enter 'Y' to use" &~
                          " BLANK for the withdrawal lot."
            return

        REM *************************************************************~
            * S E T   D E F A U L T S   A N D   E N A B L E   I N P U T *~
            *-----------------------------------------------------------*~
            *                   L I N E   I T E M S                     *~
            *************************************************************

            deffn'163(fieldnr%, mode%)
                  call "ENABLSUB" ("SET", "HNYFLUSH", scr%(), set%(), 1%,~
                                   fieldnr%, mode%, enabled%)

                  if mode% = 2% then return
                  c% = currentline%
                  on fieldnr% gosub L22230,         /* Part             */~
                                    L22250,         /* Store number     */~
                                    L22280,         /* Lot number       */~
                                    L22340,         /* Quantity added   */~
                                    L22360,         /* cost             */~
                                    L22440,         /* Source account   */~
                                    L22530,         /* Destination acct */~
                                    L22560,         /* Where from text  */~
                                    L22590,         /* Description      */~
                                    L22630          /* Job number       */
                  return
L22230:     REM Default/enable for part
                return
L22250:     REM Default/enable for store number
                if part$(c%) = " " then return
                return
L22280:     REM Default/enable for lot number
                if part$(c%) = " " then return
                call "LOTENABL" (part$(c%), enabled%, ll%, #7, #3)
                if enabled% = 0% then lot$(c%) = " "
                if enabled% > 0% then enabled% = 1%
                return
L22340:     REM Default/enable for quantity added
                return
L22360:     REM Default/enable for cost
                if part$(c%) = " " then return
                   get #4, using L22390, totlcost, cost()
L22390:            FMT POS(117), 13*PD(14,4)
                   call "CONVERT" (totlcost, 2.4, totlcost$(c%))
                   put cost$(c%) using L22420, cost()
L22420:                 FMT 12*PD(14,4)
                   return
L22440:     REM Default/enable for source (inventory) account
                if part$(c%) = " " then return
                   get #4, using L22470, srcacct$(c%)
L22470:                         FMT POS(259), CH(9)
                   if srcacct$(c%) = " " then return
                      call "DESCRIBE"(#2, srcacct$(c%), infomsg1$,       ~
                          1%, f1%(2%))
                      call "GLFMT" (srcacct$(c%))
                      return
L22530:     REM Default/enable for destination (expense or job) account
                return

L22560:     REM Default/enable for where from free text
                return

L22590:     REM Default/enable for description of entry
                if c% > 1% then descr$(c%) = descr$(c%-1%)
                return

L22630:     REM Default enable for job number
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
            scr%(1%, 1%) =  1% : set%( 1%) = 13%   /* Part Number      */
            scr%(1%, 2%) =  2% : set%( 2%) =  2%   /* Store            */
            scr%(1%, 3%) =  3% : set%( 3%) =  2%   /* Lot              */
            scr%(1%, 4%) =  4% : set%( 4%) =  2%   /* Quantity         */
            scr%(1%, 5%) =  5% : set%( 5%) = 10%   /* Total Cost       */
            scr%(1%, 6%) =  6% : set%( 6%) =  2%   /* Source           */
            scr%(1%, 7%) =  7% : set%( 7%) =  2%   /* Destination      */
            scr%(1%, 8%) =  8% : set%( 8%) =  2%   /* From             */
            scr%(1%, 9%) =  9% : set%( 9%) =  2%   /* Description      */
            scr%(1%,10%) = 10% : set%(10%) =  2%   /* Project          */

            call "ENABLSUB" ("INIT", "HNYFLUSH", scr%(), set%(),         ~
                                                         0%, 0%, 0%, 0%)
            return

        REM *************************************************************~
            *           R E S E T   S O F T   E N A B L E S             *~
            * --------------------------------------------------------- *~
            * Allow User to modify enable settings.                     *~
            *************************************************************

        deffn'049(s%, f%)      /* Screen and Field Numbers             */
            if admin% <> 1% then return            /* Not Authorized   */
            call "ENABLSUB" ("MODIFY", "HNYFLUSH", scr%(), set%(),       ~
                              s%, f%, 0%, 0%)
            return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            call "ALLFREE"
            init(" ") errormsg$, inpmessage$, add_parent$, costmeth$,    ~
                costmethdesc$, compdefstr$, compdefstrdesc$, compdeflot$,~
                intwipacct$, intwipdesc$, invassacct$, invassdesc$,      ~
                part$, qty$, store$, lot$, description$, bom_id$, mode$, ~
                neg_inv$, partdescr$, header$(), incl_excl$(),           ~
                bom_descr$, exception$, serial_nbr$, lot_tracking$,      ~
                blank_lot$, infomsg1$, infomsg2$, parentcost$,           ~
                parentdescr$, stkuom$, stkuomdescr$
            init(" ") descr$(), dstacct$(), from$(), jobnr$(), lot$(),   ~
                part$(), qty$(), seq$(), srcacct$(), store$(), totlcost$()
            mat hnyhold = zer
            init (hex(00)) parentdist$, cost$()
            lotfac$ = hex(8c)
            pf16$ = "(16)Update"
            mat descr_m = zer : mat incl_excl = zer
            mat parentcost = zer
            maxlines%, came_from_inputmode% = 0%
            assy_cost_tie_brkr% = assy_cost_tie_brkr% + 1%
            pf10key$ = hex(ff)
            pf10fac$ = hex(9c)
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************

        startover
            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return
            maxlines% = compctr%
            return clear all
            goto inputmode_1

L31000: REM *************************************************************~
            *     W R I T E   A D D I T I O N   T O   B U F F E R       *~
            *-----------------------------------------------------------*~
            * Writes inventory addition of the Parent Part to buffer.   *~
            *************************************************************

            qty = 0                                             /* JIC */
            convert qty$ to qty, data /* Whaaaaaaaaaaaaaat? */ goto L31080
L31080:     qty = round(qty, 2)
            call "GLUNFMT" (intwipacct$)
            call "GLUNFMT" (invassacct$)
            if costmeth$ = "C" then goto L31150 /* Compute at Jrnl time */
            if costmeth$ = "M" then goto L31150/* Manually via HNYCDIST */
*        Cost Method must be 'S'tandard -- nuke it for compression.
                call "PACKZERO" (parentcost(), parentdist$)
L31150:     index% = index% + 1%           /* Bump the key tie-breaker */
            write #5, using L32320, userid$, index%, " ", part$, store$,  ~
                lot$, " ", qty, intwipacct$, invassacct$, parentdist$,   ~
                " ", parentdescr$, "P", costmeth$, assy_cost_tie_brkr%,  ~
                " "                                        /* HNYFSHTF */
            return

L32000: REM *************************************************************~
            *  W R I T E   W I T H D R A W A L S   T O   B U F F E R    *~
            *-----------------------------------------------------------*~
            * Writes inventory withdrawals of the components to buffer. *~
            *************************************************************

            for temp% = 1% to maxlines%
                totlcost, qty = 0
                if part$(temp%) = " " then L32180
                   convert qty$      (temp%) to qty
                   convert totlcost$ (temp%) to totlcost
                   get cost$(temp%) using L32120, cost()
L32120:                 FMT 12*PD(14,4)
                   call "PACKZERO" (cost(), cost$(temp%))
                   qty  = round(qty  ,2)
                   totlcost = round(totlcost, 4)
                   call "GLUNFMT" (srcacct$(temp%))
                   call "GLUNFMT" (dstacct$(temp%))
L32180:         index% = index% + 1%       /* Bump the key tie-breaker */
                write #5, using L32320,                                   ~
                          userid$, index%, seq$(temp%), part$(temp%),    ~
                          store$(temp%), lot$(temp%), jobnr$(temp%),     ~
                          qty, dstacct$(temp%), srcacct$(temp%),         ~
                          cost$(temp%), from$(temp%), descr$(temp%),     ~
                          "C", " ", assy_cost_tie_brkr%," "/* HNYFSHTF */
                call "HNYHOLD" (#4, part$(temp%), store$(temp%),         ~
                             lot$(temp%), qty - hnyhold(temp%), return%)
                hnyhold(temp%) = qty
            next temp%
            return

L32320: FMT                 /* FILE #5- HNYFSHTF                       */~
            CH(3),          /* user-id of specific user                */~
            BI(3),          /* Inventory Add/W/D sequence number       */~
            CH(3),          /* Inventory Add/W/D Item Sequence Number  */~
            CH(25),         /* Part Number                             */~
            CH(3),          /* Store or Warehouse Code                 */~
            CH(16),         /* Lot Number                              */~
            CH(8),          /* Job Number                              */~
            PD(14,4),       /* detail to quantity on-hand              */~
            CH(9),          /* inventory expense account               */~
            CH(9),          /* Inventory Asset Account                 */~
            CH(96),         /* Cost breakdown                          */~
            CH(16),         /* where from text field                   */~
            CH(32),         /* description of entry                    */~
            CH(01),         /* 'C'=Component W/D; 'P'=Parent Add'n     */~
            CH(01),         /* Cost Method ('C'=sum comps @ journal tm)*/~
            BI(04),         /* Assy tie-brkr for cost accumulation     */~
            CH(63)          /* Filler (Internal, unused space)         */

        REM *************************************************************~
            *  M I S C E L L A N E O U S   P R I N T   R O U T I N E S  *~
            *************************************************************

        exception_detail
            if rpt% <> 0% then L33070
                gosub printer_setup
L33070:     print using L35190, part$, bom_id$, comp_pn$, store$, lot$,   ~
                               reqd$, exdescr$
            lctr% = lctr% + 1%
            exception$ = "Y"
            if lctr% > 58% then gosub page_heading
            return

        page_heading
            print page
            pctr% = pctr% + 1%
            print using L35040, date$, name$, pctr%
            print using L35080, runtime$
            print skip(2)
            print using L35120
            print using L35160
            lctr% = 5%
            return

        printer_setup
            call "SETPRNT" ("HNY043", " ", 1%, 0%)
            select printer (134)
            rpt% = 1%
            gosub page_heading
            return

        REM *************************************************************~
            *             E X C E P T I O N   R E P O R T               *~
            *************************************************************

L35040: %RUN DATE: ########                   ###########################~
        ~#################################                        PAGE:   ~
        ~###

L35080: %RUN TIME: ########                            BACKFLUSH MASS WIT~
        ~HDRAWAL EXCEPTION REPORT                             HNYFLUSH:HNY~
        ~043

L35120: %Parent Part                BOM  Component Part            Store ~
        ~Lot     Qty Required  Exception Description


L35160: %-------------------------  ---  ------------------------- ----- ~
        ~------  ------------  ----------------------------------------

L35190: %#########################  ###  #########################  ###  ~
        ~######    ##########  ########################################

L35220: %                                          ********** END OF REPO~
        ~RT @ ######## **********

L35250: %****** WITHDRAWALS FOR ABOVE PARENT ABORTED ******
L35260: %***** WITHDRAWALS FOR ABOVE PARENT RESELECTED ****

        REM *************************************************************~
            *           Edit Components for Exceptions                  *~
            *************************************************************

        components
            if mode% <> 1% then L36051
                call "SHOSTAT" ("Clearing Components for reselection...")
                if exception$ <> "Y" then L36051
                     print using L35260 : print
L36051
*        Clear the component arrays.
            init(" ") descr$(), dstacct$(), from$(), jobnr$(), lot$(),   ~
                part$(), qty$(), seq$(), srcacct$(), store$(), totlcost$()
            mat hnyhold = zer
            init (hex(00)) cost$()
            maxlines% = 0%     /* Re-start arrays at the bottom (top?) */
*        Set the key to the BOM header and PLOWNEXT to get components
            call "SHOSTAT" ("Loading Components ... please stand by.")
            plowkey$ = str(part$) & str(bom_id$) & "  0"
            data5% = 0% : phfact(1%), ph% = 1%
        plow_loop
            call "PLOWNEXT" (#14, plowkey$, 28%, f1%(14%))
                if f1%(14%) <> 0% then L36220
            if data5% <> 0% then return
            if keyhit% = 16% then return
L36130:         u3% = 0%
                call "ASKUSER" (0%, "***** NO LINES *****",              ~
                     "No Line Items Available for Edit,",                ~
                     "All Items Have Been Reported as Exceptions.",      ~
                     "Press (RETURN) to Acknowledge and Continue.")
                if u3% <> 0% then L36130
                    return clear all
                    goto inputmode_1

L36220:     get #14 using L36240, comp_pn$, asse_pn$, bom$, bomseq$,      ~
                bomqty, timesused, fixedqty, overage, mark$, fbom$
L36240:         FMT CH(25), CH(25), CH(3), CH(3), 4*PD(14,4), CH(2),     ~
                    XX(1), CH(3)
            ext = bomqty * timesused + overage
            qty_req = round(ext * qty * phfact(ph%) + fixedqty, 2)
            call "CONVERT" (qty_req, 2.2, reqd$)

*        Edit components eliminating exceptions
            if mark$ = "TL" or mark$ = "SP" or mark$ = "RE"              ~
                then goto plow_loop                       /* BYPASS   */
            plowhny$ = str(comp_pn$)
            call "READ100" (#3, plowhny$, f1%(3%))        /* HNYMASTR */
            if f1%(3%) <> 0% then goto L36390
                 exdescr$ = "NON-STOCKED PART"
                 gosub exception_detail
                 goto plow_loop
L36390:     get #3 using L36410, lot_tracking$, serial_nbr$,              ~
                                lot_protected$, temp$, asset_acct$
L36410:         FMT POS(130), CH(1), CH(1), CH(1), POS(180), CH(3),      ~
                    POS(353), CH(9)
            convert temp$ to type%, data goto L36450
            if type% > 199% then L36480
L36450:         exdescr$ = "OPTION PART"
                gosub exception_detail
                goto plow_loop
L36480:     if serial_nbr$ <> "Y" then goto L36520
                exdescr$ = "SERIAL NUMBER TRACKING REQUIRED"
                gosub exception_detail
                goto plow_loop
L36520:     if str(mark$,,1%) = "P" then phantom_processor
            plowhny$ = str(comp_pn$) & str(compdefstr$) & compdeflot$
            if lot_tracking$ = "N" and blank_lot$ = "Y" then             ~
                                              str(plowhny$,29%,16%) = " "
            mat cst = zer
            call "READ100" (#4, plowhny$, f1%(4%))         /* HNYQUAN  */
            if f1%(4%) <> 0% then goto L36620
                exdescr$ = "STORE/LOT NOT VALID FOR PART"
                gosub exception_detail
                goto plow_loop
L36620:     get #4 using L36630, cst(), asset_acct$
L36630:         FMT POS(125), 12*PD(14,4), POS(259), CH(9)

            avail = 0
            if temp$ <> "UU" then goto L36680
                goto L36690
L36680:     if neg_inv$ = "Y" then goto L36840
L36690:         call "HNYAVAIL" (#3, #4, comp_pn$, compdefstr$,          ~
                     compdeflot$, errormsg$, qty_req, avail, u3%)
                if neg_inv$ = "N" then L36750
                    if lot_protected$ = "Y" and errormsg$ <> " "         ~
                                                               then L36760
                    goto L36840
L36750:         if u3% = 0% then goto L36840
L36760:             avail$ = " "
                    call "CONVERT" (avail, -.2, avail$)
                    if temp$ = "UU" then                                 ~
                         exdescr$ = "INSUFFICIENT QTY FOR 'UU' MARKER"   ~
                    else exdescr$ = "INSUFFICIENT QUANTITY AVAILABLE"
                         gosub exception_detail
                         goto plow_loop

L36840:     maxlines% = maxlines% + 1%
            if maxlines% > maxcomps% then goto array_overflow

*        Build array element from component
            convert maxlines% to seq$(maxlines%), pic(##0)
            part$(maxlines%)    = comp_pn$
            store$(maxlines%)   = compdefstr$
            lot$(maxlines%)     = compdeflot$
            if lot_tracking$ = "N" and blank_lot$ = "Y" then             ~
                                                    lot$(maxlines%) = " "
            jobnr$(maxlines%)   = " "
            qty$(maxlines%)     = reqd$
            dstacct$(maxlines%) = intwipacct$
            srcacct$(maxlines%) = asset_acct$
            call "GLFMT" (srcacct$(maxlines%))
            from$(maxlines%)    = " "
            descr$(maxlines%)   = description$
            put cost$(maxlines%) using L37010, cst()
L37010:         FMT 12*PD(14,4)
            totlcost = 0
            for i% = 1% to 12%
                totlcost = totlcost + cst(i%)
            next i%
            call "CONVERT" (totlcost, 2.4, totlcost$(maxlines%))

            data5% = 1%
            goto plow_loop                     /* Get next component */

        array_overflow
            maxlines% = compctr%
L37130:     u3% = 0%
            call "ASKUSER" (u3%, "***** ARRAY OVERFLOW *****",           ~
                "Array size exceeded.  Records must be processed.",      ~
                       part$ & " components will be excluded.",          ~
                "Press PF(16) to Acknowledge and Process.")
            if u3% <> 16% then goto L37130
            goto exit_program

        phantom_processor
            if ph% > 100% then plow_loop
                plowkey$(ph%) = plowkey$
                ph% = ph% + 1%
                phfact(ph%) = bomqty * timesused * phfact(ph%-1%)
                if fbom$ = " " then                                      ~
                      call "BOMFIND" (comp_pn$, hnydate$, #10, #7, fbom$)
                plowkey$ = str(comp_pn$) & str(fbom$) & "  0"
                gosub plow_loop
                ph% = ph% - 1%
                plowkey$ = plowkey$(ph%)
                goto plow_loop

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            pfkeys1$ = hex(000104080d0fffffffffff)
            if fieldnr% = 1% then pfkeys1$ = (hex(00ff0d0f10ffffffffff))
            if fieldnr% = 0% then pfkeys1$ = (hex(0001ffff0d0f10ffffff))
            str(pfkeys1$,10%,1%) = pf2key$
            pf10key$ = hex(ff)          /* PF(10)Edit Cost is DISabled */
            pf10fac$ = hex(9c)
            if edit% = 0% then goto L40160
                if fieldnr% = 0% and costmeth$ = "M" then goto L40170
                goto L40190
L40160:     if fieldnr% < 6% or costmeth$ <> "M" then goto L40190
L40170:         pf10key$ = hex(0a)       /* PF(10)Edit Cost is ENabled */
                pf10fac$ = hex(8c)
L40190:     str(pfkeys1$,11%,1%) = pf10key$
            if fieldnr% > 0% then init(hex(8c)) llfac$(), lotfac$        ~
                             else init(hex(86)) llfac$(), lotfac$
            if fieldnr% = 0% and edit% = 1% then llfac$(1%) = hex(8c)
            on fieldnr% gosub L40360,         /* Parent Part Number     */~
                              L40360,         /* Parent Part BOM ID     */~
                              L40380,         /* Qty of Parent Part     */~
                              L40390,         /* Add Parent?/Store/Lot  */~
                              L40360,         /* Parent Part Cost Meth. */~
                              L40370,         /* Parent Part Description*/~
                              L40360,         /* Interim B/F (WIP) acct */~
                              L40360,         /* Inventory Asset acct   */~
                              L40360,         /* Component Def Store #  */~
                              L40360,         /* Component Def Lot #    */~
                              L40370,         /* Component Description  */~
                              L40360,         /* Neg Inventory Flag     */~
                              L40360          /* Blank Lot Flag         */
            goto L40430
L40360:         llfac$(fieldnr%) = hex(81) : lotfac$ = hex(8c) : return
L40370:         llfac$(fieldnr%) = hex(80) : lotfac$ = hex(8c) : return
L40380:         llfac$(fieldnr%) = hex(82) : lotfac$ = hex(8c) : return
L40390:         llfac$(fieldnr%), lotfac$ = hex(81)
                if lotenabled% = 0% then lotfac$ = hex(8c)
                return

L40430: accept                                                           ~
            at (01,02),   "Component Mass Withdrawal/Potential Addition o~
        ~f Parent Part",                                                  ~
            at (01,66),   "Today:",                                      ~
            at (01,73),   fac(hex(8c)), date$                    ,ch(08),~
            at (02,02),   fac(hex(ac)), line2$                   ,ch(79),~
            at (03,02),   fac(hex(84)), infomsg1$                ,ch(79),~
            at (04,02),   fac(hex(84)), infomsg2$                ,ch(79),~
            at (05,02),   fac(hex(94)), errormsg$                ,ch(79),~
                                                                         ~
            at (06,02),   "Parent Part"                          ,       ~
            at (06,17),   fac(llfac$( 1%)), part$                ,ch(25),~
            at (06,44),   fac(hex(8c)), partdescr$               ,ch(34),~
            at (07,02),   "Parent Part BOM ID"                   ,       ~
            at (07,30),   fac(llfac$( 2%)), bom_id$              ,ch(03),~
            at (07,44),   fac(hex(8c)), bom_descr$               ,ch(32),~
            at (08,02),   "Quantity of Parent Part"              ,       ~
            at (08,30),   fac(llfac$( 3%)), qty$                 ,ch(10),~
            at (08,44),   fac(hex(8c)), stkuomdescr$             ,ch(32),~
            at (09,02),   "Add Parent to Inventory?"             ,       ~
            at (09,30),   fac(llfac$( 4%)), add_parent$          ,ch(01),~
            at (09,44),   "Store"                                ,       ~
            at (09,50),   fac(llfac$( 4%)), store$               ,ch(03),~
            at (09,56),   "Lot"                                  ,       ~
            at (09,60),   fac(lotfac$),     lot$                 ,ch(06),~
            at (10,02),   "Parent Cost Method? (S/M/C)"          ,       ~
            at (10,30),   fac(llfac$( 5%)), costmeth$            ,ch(01),~
            at (10,44),   fac(hex(8c)), costmethdesc$            ,ch(32),~
            at (11,02),   "Parent Description"                   ,       ~
            at (11,30),   fac(llfac$( 6%)), parentdescr$         ,ch(32),~
            at (12,02),   "Interim B/F (WIP) Acct"               ,       ~
            at (12,30),   fac(llfac$( 7%)), intwipacct$          ,ch(12),~
            at (12,44),   fac(hex(8c)), intwipdesc$              ,ch(32),~
            at (13,02),   "Inventory Asset Acct"                 ,       ~
            at (13,30),   fac(llfac$( 8%)), invassacct$          ,ch(12),~
            at (13,44),   fac(hex(8c)), invassdesc$              ,ch(32),~
            at (14,02),   "Component Default Store"              ,       ~
            at (14,30),   fac(llfac$( 9%)), compdefstr$          ,ch(03),~
            at (14,44),   fac(hex(8c)), compdefstrdesc$          ,ch(32),~
            at (15,02),   "Component Default Lot"                ,       ~
            at (15,30),   fac(llfac$(10%)), compdeflot$          ,ch(06),~
            at (16,02),   "Component Description"                ,       ~
            at (16,30),   fac(llfac$(11%)), description$         ,ch(32),~
            at (17,02),   "Allow Negative Inventory?"            ,       ~
            at (17,30),   fac(llfac$(12%)), neg_inv$             ,ch(01),~
            at (18,02),   "Blank Lot for Non-Lot Track"          ,       ~
            at (18,30),   fac(llfac$(13%)), blank_lot$           ,ch(01),~
                                                                         ~
            at (21,02),   fac(hex(a4)), inpmessage$              ,ch(79),~
            at (22,02),   fac(hex(8c)), pf1$                     ,       ~
            at (22,64),   "(13)Instructions"                     ,       ~
            at (23,02),   fac(hex(8c)), pf2$                     ,       ~
            at (23,20),   fac(hex(8c)), pf4$                     ,       ~
            at (23,40),   fac(pf10fac$), pf10msg$                ,       ~
            at (23,64),   "(15)Print Screen"                     ,       ~
            at (24,64),   fac(hex(84)), pf16$                    ,       ~
            keys(pfkeys1$), key(keyhit%)

            if keyhit% <> 13% then L41040
                call "MANUAL" ("HNYFLUSH")
                goto L40430

L41040:     if keyhit% <> 15% then L41080
                call "PRNTSCRN"
                goto L40430

L41080:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        REM *************************************************************~
            *       T A B L E   O P E R A T I O N S   S C R E E N       *~
            *-----------------------------------------------------------*~
            * This is the screen routine that handles input, edit,      *~
            * insert, and delete modes.                                 *~
            *************************************************************

            deffn'213(screenline%, fieldnr%)
                  screen% = 2%
                  init(hex(86)) fac$()
                  if fieldnr% = 0% then L42260
                     goto L42250

            deffn'223(screenline%, fieldnr%)
                  screen% = 3%
                  goto L42250

            deffn'233(screenline%)
                  screen% = 4%
                  init(hex(84)) fac$()
                  for temp% = 1% to 12%
                      fac$(screenline%, temp%) = hex(94)
                  next temp%
                  goto L42480

L42250:           init(hex(84)) fac$()
L42260:           on fieldnr% gosub L42410,         /* Part             */~
                                    L42410,         /* Store number     */~
                                    L42410,         /* Lot number       */~
                                    L42440,         /* Quantity         */~
                                    L42440,         /* cost             */~
                                    L42410,         /* Source account   */~
                                    L42410,         /* Inventory account*/~
                                    L42410,         /* Where from line  */~
                                    L42380,         /* Description      */~
                                    L42410          /* Job number       */
                  goto L42480

L42380:     REM Set FAC's for upper/lower case input.
                fac$(screenline%, fieldnr%) = hex(80)
                return
L42410:     REM Set FAC's for UPPER case only input
                fac$(screenline%, fieldnr%) = hex(81)
                return
L42440:     REM Set FAC's for numeric input.
                fac$(screenline%, fieldnr%) = hex(82)
                return

L42480:     accept                                                       ~
               at (01,02), fac(hex(8c)),    tttle$(screen%, 1%) , ch(79),~
               at (02,02), fac(hex(ac)),    tttle$(screen%, 2%) , ch(79),~
               at (03,02), fac(hex(94)),    errormsg$           , ch(79),~
               at (04,02), fac(hex(84)),    infomsg1$           , ch(79),~
                                                                         ~
               at (05,02), fac(hex(84)),    separator$(1%)      , ch(79),~
               at (09,02), fac(hex(84)),    separator$(2%)      , ch(79),~
               at (13,02), fac(hex(84)),    separator$(3%)      , ch(79),~
               at (17,02), fac(hex(84)),    separator$(4%)      , ch(79),~
               at (21,02), fac(hex(84)),    separator$(5%)      , ch(79),~
                                                                         ~
               at (06,02), fac(hex(84)),    seq$     (line%+ 1%), ch(03),~
               at (10,02), fac(hex(84)),    seq$     (line%+ 2%), ch(03),~
               at (14,02), fac(hex(84)),    seq$     (line%+ 3%), ch(03),~
               at (18,02), fac(hex(84)),    seq$     (line%+ 4%), ch(03),~
               at (22,02), fac(hex(84)),    seq$     (line%+ 5%), ch(03),~
                                                                         ~
               at (06,06), "P/N"                                        ,~
               at (10,06), "P/N"                                        ,~
               at (14,06), "P/N"                                        ,~
               at (18,06), "P/N"                                        ,~
               at (22,06), "P/N"                                        ,~
                                                                         ~
               at (06,10), fac(fac$(1%, 1%)), part$  (line%+ 1%), ch(25),~
               at (10,10), fac(fac$(2%, 1%)), part$  (line%+ 2%), ch(25),~
               at (14,10), fac(fac$(3%, 1%)), part$  (line%+ 3%), ch(25),~
               at (18,10), fac(fac$(4%, 1%)), part$  (line%+ 4%), ch(25),~
               at (22,10), fac(fac$(5%, 1%)), part$  (line%+ 5%), ch(25),~
                                                                         ~
               at (06,37), "Str"                                        ,~
               at (10,37), "Str"                                        ,~
               at (14,37), "Str"                                        ,~
               at (18,37), "Str"                                        ,~
               at (22,37), "Str"                                        ,~
                                                                         ~
               at (06,41), fac(fac$(1%, 2%)), store$ (line%+ 1%), ch( 3),~
               at (10,41), fac(fac$(2%, 2%)), store$ (line%+ 2%), ch( 3),~
               at (14,41), fac(fac$(3%, 2%)), store$ (line%+ 3%), ch( 3),~
               at (18,41), fac(fac$(4%, 2%)), store$ (line%+ 4%), ch( 3),~
               at (22,41), fac(fac$(5%, 2%)), store$ (line%+ 5%), ch( 3),~
                                                                         ~
               at (06,46), "Lot"                                        ,~
               at (10,46), "Lot"                                        ,~
               at (14,46), "Lot"                                        ,~
               at (18,46), "Lot"                                        ,~
               at (22,46), "Lot"                                        ,~
                                                                         ~
               at (06,50), fac(fac$(1%,3%)), str(lot$(line%+1%),,ll%),   ~
               at (10,50), fac(fac$(2%,3%)), str(lot$(line%+2%),,ll%),   ~
               at (14,50), fac(fac$(3%,3%)), str(lot$(line%+3%),,ll%),   ~
               at (18,50), fac(fac$(4%,3%)), str(lot$(line%+4%),,ll%),   ~
               at (22,50), fac(fac$(5%,3%)), str(lot$(line%+5%),,ll%),   ~
                                                                         ~
               at (06,58), "Qty"                                        ,~
               at (10,58), "Qty"                                        ,~
               at (14,58), "Qty"                                        ,~
               at (18,58), "Qty"                                        ,~
               at (22,58), "Qty"                                        ,~
                                                                         ~
               at (06,62), fac(fac$(1%, 4%)), qty$   (line%+ 1%), ch(10),~
               at (10,62), fac(fac$(2%, 4%)), qty$   (line%+ 2%), ch(10),~
               at (14,62), fac(fac$(3%, 4%)), qty$   (line%+ 3%), ch(10),~
               at (18,62), fac(fac$(4%, 4%)), qty$   (line%+ 4%), ch(10),~
               at (22,62), fac(fac$(5%, 4%)), qty$   (line%+ 5%), ch(10),~
                                                                         ~
               at (07,02), "Total Cost"                                 ,~
               at (11,02), "Total Cost"                                 ,~
               at (15,02), "Total Cost"                                 ,~
               at (19,02), "Total Cost"                                 ,~
               at (23,02), "Total Cost"                                 ,~
                                                                         ~
               at (07,13), fac(fac$(1%,5%)), totlcost$(line%+ 1%),ch(10),~
               at (11,13), fac(fac$(2%,5%)), totlcost$(line%+ 2%),ch(10),~
               at (15,13), fac(fac$(3%,5%)), totlcost$(line%+ 3%),ch(10),~
               at (19,13), fac(fac$(4%,5%)), totlcost$(line%+ 4%),ch(10),~
               at (23,13), fac(fac$(5%,5%)), totlcost$(line%+ 5%),ch(10),~
                                                                         ~
               at (07,44), "Source"                                     ,~
               at (11,44), "Source"                                     ,~
               at (15,44), "Source"                                     ,~
               at (19,44), "Source"                                     ,~
               at (23,44), "Source"                                     ,~
                                                                         ~
               at (07,51), fac(fac$(1%,6%)), srcacct$ (line%+ 1%),ch(12),~
               at (11,51), fac(fac$(2%,6%)), srcacct$ (line%+ 2%),ch(12),~
               at (15,51), fac(fac$(3%,6%)), srcacct$ (line%+ 3%),ch(12),~
               at (19,51), fac(fac$(4%,6%)), srcacct$ (line%+ 4%),ch(12),~
               at (23,51), fac(fac$(5%,6%)), srcacct$ (line%+ 5%),ch(12),~
                                                                         ~
               at (07,64), "Dest"                                       ,~
               at (11,64), "Dest"                                       ,~
               at (15,64), "Dest"                                       ,~
               at (19,64), "Dest"                                       ,~
               at (23,64), "Dest"                                       ,~
                                                                         ~
               at (07,69), fac(fac$(1%,7%)), dstacct$ (line%+ 1%),ch(12),~
               at (11,69), fac(fac$(2%,7%)), dstacct$ (line%+ 2%),ch(12),~
               at (15,69), fac(fac$(3%,7%)), dstacct$ (line%+ 3%),ch(12),~
               at (19,69), fac(fac$(4%,7%)), dstacct$ (line%+ 4%),ch(12),~
               at (23,69), fac(fac$(5%,7%)), dstacct$ (line%+ 5%),ch(12),~
                                                                         ~
               at (08,02), "From"                                       ,~
               at (12,02), "From"                                       ,~
               at (16,02), "From"                                       ,~
               at (20,02), "From"                                       ,~
               at (24,02), "From"                                       ,~
                                                                         ~
               at (08,07), fac(fac$(1%,8%)), from$    (line%+ 1%),ch(16),~
               at (12,07), fac(fac$(2%,8%)), from$    (line%+ 2%),ch(16),~
               at (16,07), fac(fac$(3%,8%)), from$    (line%+ 3%),ch(16),~
               at (20,07), fac(fac$(4%,8%)), from$    (line%+ 4%),ch(16),~
               at (24,07), fac(fac$(5%,8%)), from$    (line%+ 5%),ch(16),~
                                                                         ~
               at (08,27), "Desc"                                       ,~
               at (12,27), "Desc"                                       ,~
               at (16,27), "Desc"                                       ,~
               at (20,27), "Desc"                                       ,~
               at (24,27), "Desc"                                       ,~
                                                                         ~
               at (08,32), fac(fac$(1%,9%)), descr$   (line%+ 1%),ch(32),~
               at (12,32), fac(fac$(2%,9%)), descr$   (line%+ 2%),ch(32),~
               at (16,32), fac(fac$(3%,9%)), descr$   (line%+ 3%),ch(32),~
               at (20,32), fac(fac$(4%,9%)), descr$   (line%+ 4%),ch(32),~
               at (24,32), fac(fac$(5%,9%)), descr$   (line%+ 5%),ch(32),~
                                                                         ~
               at (08,66), "Prj"                                        ,~
               at (12,66), "Prj"                                        ,~
               at (16,66), "Prj"                                        ,~
               at (20,66), "Prj"                                        ,~
               at (24,66), "Prj"                                        ,~
                                                                         ~
               at (08,70), fac(fac$(1%,10%)), jobnr$(line%+ 1%) , ch(08),~
               at (12,70), fac(fac$(2%,10%)), jobnr$(line%+ 2%) , ch(08),~
               at (16,70), fac(fac$(3%,10%)), jobnr$(line%+ 3%) , ch(08),~
               at (20,70), fac(fac$(4%,10%)), jobnr$(line%+ 4%) , ch(08),~
               at (24,70), fac(fac$(5%,10%)), jobnr$(line%+ 5%) , ch(08),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)

            errormsg$ = " "
            if keyhit% <> 10% then L44070
                close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                if cursor%(1%) >  4% then goto L43970
L43940:              errormsg$ = "Place cursor at desired line before p"&~
                          "ressing PF(10)"
                     goto L42480
L43970:         c% = 0%
                if cursor%(1%) > 4% and cursor%(1%)<10% then c% = line%+1%
                if cursor%(1%) > 8% and cursor%(1%)<14% then c% = line%+2%
                if cursor%(1%) >12% and cursor%(1%)<18% then c% = line%+3%
                if cursor%(1%) >16% and cursor%(1%)<22% then c% = line%+4%
                if cursor%(1%) >20% and cursor%(1%)<25% then c% = line%+5%
                if c% < 1% or c% > maxlines% then goto L43940
                call "HNYQDISP" (part$(c%), #3, #4, #9, #7)
                goto L42480

L44070:     if keyhit% <> 15% then L44110
                call "PRNTSCRN"
                goto L42480

L44110:     if screen% <> 2% then return
                close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%, mode%)
            errormsg$ = " "
            on fieldnr% gosub L50230,         /* Parent Part Number     */~
                              L50460,         /* Parent Part BOM ID     */~
                              L50700,         /* Qty of Parent Part     */~
                              L50740,         /* Add Parent?/Store/Lot  */~
                              L51090,         /* Parent Part Cost Meth. */~
                              L51380,         /* Parent Part Description*/~
                              L51550,         /* Interim B/F (WIP) acct */~
                              L51640,         /* Inventory Asset acct   */~
                              L51720,         /* Component Def Store #  */~
                              L51820,         /* Component Def Lot #    */~
                              L51880,         /* Component Description  */~
                              L52050,         /* Neg Inventory Flag     */~
                              L52110          /* Blank Lot Flag         */
            return

L50230: REM Test for Part Number                    PART$
            if part$ = "?" then part$ = " "
            mat descr_m = zer
            mat incl_excl = zer
            init(" ") header$(), incl_excl$(), partdescr$, stkuom$,      ~
                stkuomdescr$
            mat incl_excl = zer
            header$(2%) = hex(a4) &" Parent Part                 Descript~
        ~ion"
            header$(3%) = hex(84) &"   Select a Parent Part Number"
            plowkey$ = str(part$) & hex(00)
            descr_m(1%) = -26.32  : descr_m(2%) =   1 /* Part Descrptn */
            descr_m(3%) = -74.04  : descr_m(4%) =  35 /* Part Stkg UOM */
        REM CALL "PLOWCODE" (#14, PLOWKEY$, PARTDESCR$, -8025%, -.32,    ~
                F1%(14%), HEADER$(), 0, .0026, INCL_EXCL(), INCL_EXCL$(),~
                "D", " ", #3)                  /* This is the original */
            call "PLOWCODE" (#14, plowkey$, partdescr$, -9025%, -.32,    ~
                f1%(14%), header$(), 0,     0, incl_excl(), incl_excl$(),~
                "D", " ", #3, descr_m())    /* This one shows Stkg UOM */
            if f1%(14%) <> 0% then goto L50420
                if part$ = " "                                           ~
                     then errormsg$ = "You must select a Parent Part"    ~
                     else errormsg$ = "Part not found in BOMMASTR, plea"&~
                                      "se re-enter"
                return
L50420:     part$ = str(plowkey$,,25%)
            call "PUTPAREN" (partdescr$)
            call "READ100" (#3, part$, f1%(3%))
                if f1%(3%) = 0% then return   /* Part not in HNYMASTR! */
            get #3 using L50436, stkuom$       /* HNYMASTR Stocking UOM */
L50436:         FMT POS(74), CH(4)
            stkuomdescr$ = "(Stocking UOM = " & stkuom$ & ")"
            return

L50460: REM Test for BOM ID number                  BOM_ID$
            if bom_id$ = "?" then bom_id$ = " "
            mat descr_m = zer
            mat incl_excl = zer
            init(" ") header$(), incl_excl$(), bom_descr$
            plowkey$ = str(part$) & str(bom_id$) & "  0"
            header$(3%) = hex(84)&"   Select a BOM ID For Parent Part: " ~
                                 & part$
            header$(1%) = hex(a4) &" BOM ID   Description"
            incl_excl(1%) = 54.03 : incl_excl$(1%) = "  0" /*BOM Headers*/
            descr_m(1%) = 51.03 : descr_m(2%) =   2
            descr_m(3%) = 57.30 : descr_m(4%) =  10
            call "PLOWCODE" (#14, plowkey$, bom_descr$, 9025%, .3,       ~
                f1%(14%), header$(), 0, 0, incl_excl(), incl_excl$(),    ~
                "D", " ", #14, descr_m())
            if f1%(14%) <> 0% then goto L50640
                errormsg$ = "You must select a BOM ID"
                return
L50640:     bom_id$ = str(plowkey$,26%,3%)
            get #14 using L50660, bom_descr$    /* PLOWCODE work-around */
L50660:         FMT POS(57), CH(30)
            call "PUTPAREN" (bom_descr$)
            if mode% = 1% then gosub components     /* Load Components */
            return

L50700: REM Test data for quantity                  QTY$
            call "NUMTEST" (qty$, .01, 1e9, errormsg$, -2.2, qty)
            return

L50740: REM Test for Add Parent? to Store? Lot?  ADD_PARENT$/STORE$/LOT$
            if add_parent$ <> "N" then goto L50780
                store$, lot$, costmeth$, costmethdesc$, parentdescr$ = " "
                return
L50780:     if add_parent$ = "Y" then goto L50810
                errormsg$ = "Enter 'Y' or 'N' for this code."
                return
L50810:     if mode% = 0% then goto L50880
                if costmeth$ <> " " then goto L50880
                     costmeth$ = "S"       /* Default to Standard Cost */
                     gosub L51090
                     parentdescr$ = part$ & ": " & bom_id$
                     infomsg1$ = "Note Cost Method set to 'S'tandard an"&~
                          "d Default Parent Description."
L50880:     if store$ = "?" then store$ = " "
            call "GETCODE" (#6, store$, " ", 0%, 0, f1%(6%))
            if f1%(6%) <> 0% then goto L50940
                errormsg$ = "You must enter a Store Number when adding "&~
                     "to Inventory."
                return
L50940:     if lotenabled% <> 2% or lot$ <> " " then goto L50970
                errormsg$ = "You must enter a Lot # for this Parent Part."
                return
L50970:     call "LOTVALID" (part$, store$, lot$, #7, #3, #4, errormsg$)
            if errormsg$ <> " " then return
                readkey$ = str(part$) & str(store$) & lot$
                call "READ100" (#4, readkey$, f1%(4%))
                if f1%(4%) <> 0% then return
                     if infomsg1$ = " "                                  ~
                          then infomsg1$ = "Note Part / Store / Lot rec"&~
                               "ord will be created in HNYQUAN."         ~
                          else infomsg2$ = "Note Part / Store / Lot rec"&~
                               "ord will be created in HNYQUAN."
                     return

L51090: REM Test data for Parent Part Cost Method       COSTMETH$
            costmethdesc$, parentcost$ = " "
            mat parentcost = zer
            parentcost = 0
            if add_parent$ = "Y" then goto L51160
                costmeth$ = " "
                return
L51160:     on pos("SMC" = costmeth$) goto L51190, L51240, L51280
                errormsg$ = "Enter 'S', 'M' or 'C' for this code."
                return
L51190:     call "STCCOSTS" (part$, " ", #07, 2%, parentcost,            ~
                parentcost())
            costmethdesc$ = "Standard Cost:"
            goto L51310

L51240:     gosub cost_distribution
L51250:     costmethdesc$ = "Manual Entry:"
            goto L51310

L51280:     costmethdesc$ = "Sum of Components at Journal"
            goto L51340

L51310
*        Edit total cost & append to Description.
            call "CONVERT" (parentcost, -2.2, parentcost$)
            costmethdesc$ = costmethdesc$ & " " & parentcost$
L51340
*        Parenthetically ...
            call "PUTPAREN" (costmethdesc$)
            return

L51380: REM Test for Parent Part Description            PARENTDESCR$
            if add_parent$ = "Y" then goto L51390
                parentdescr$ = " "
                return
L51390:     if parentdescr$ <> " " then goto L51430
                errormsg$ = "May not be blank. Enter a Description, '?', ~
        ~or Code."
                return
L51430:     if len(parentdescr$) > 1% then return   /* Entered by user */
            if str(parentdescr$,,1%) = "?" then parentdescr$ = " "
            readkey$ = "INVADDDSC" & str(parentdescr$,,1%)
            call "PLOWCODE" (#20, readkey$, parentdescr$, 9%, .3,        ~
                f1%(20%))
            if f1%(20%) <> 0% then return
            if str(readkey$,10%,1%) = " " then L51520   /* Want 1-char? */
                parentdescr$ = str(readkey$,10%,1%)    /* 1-char Descr */
                return
L51520:     errormsg$ = "You must enter a Description, Code or '?'."
            return

L51550: REM Test data for Interim B/F (WIP) account     INTWIPACCT$
            intwipdesc$ = " "
            if intwipacct$ = "?" then intwipacct$ = " "
            call "GETCODE" (#2, intwipacct$, intwipdesc$, 1%, 0, f1%(2%))
            if f1%(2%) = 1% then return
                errormsg$ = "Interim B/F (WIP) acct not on file. Try agai~
        ~n."
                return

L51640: REM Test data for Inventory Asset account       INVASSACCT$
            invassdesc$ = " "
            if invassacct$ = "?" then invassacct$ = " "
            call "GETCODE" (#2, invassacct$, invassdesc$, 1%, 0, f1%(2%))
            if f1%(2%) = 1% then return
                errormsg$ = "Inventory Asset acct not on file. Try again."
                return

L51720: REM Test data for Component Default Store       COMPDEFSTR$
            compdefstrdesc$ = " "
            if compdefstr$ = " " then return
            if compdefstr$ = "?" then compdefstr$ = " "
            call "GETCODE" (#06, compdefstr$, compdefstrdesc$, 1%, 0,    ~
                f1%(6))
            if f1%(6) <> 0% then goto L51801
                errormsg$ = "Select a valid Store # or enter blanks."
                return
L51801:     if mode% = 1% then gosub components     /* Load Components */
            return

L51820: REM Test data for Component Default Lot         COMPDEFLOT$
            if compdeflot$ = " " then return
            errormsg$ = "LOT-CHECK"
            call "LOTVALID" (" ", " ", lot$, #7, #3, #4, errormsg$)
            if errormsg$ <> " " then return
            if mode% = 1% then gosub components     /* Load Components */
            return

L51880: REM Test for Component Description               DESCRIPTION$
            if description$ <> " " then goto L51930
                errormsg$ = "May not be blank. Enter a Description, '?', ~
        ~or Code."
                return
L51930:     if len(description$) > 1% then return   /* Entered by user */
            if str(description$,,1%) = "?" then description$ = " "
            readkey$ = "INVWDWDSC" & str(description$,,1%)
            call "PLOWCODE" (#20, readkey$, description$, 9%, .3,        ~
                f1%(20%))
            if f1%(20%) <> 0% then return
            if str(readkey$,10%,1%) = " " then L52020   /* Want 1-char? */
                description$ = str(readkey$,10%,1%)    /* 1-char Descr */
                return
L52020:     errormsg$ = "You must enter a Description, Code or '?'."
            return

L52050: REM Test for Neg Inventory Flag (Y/N)
            if neg_inv$ = "Y" or neg_inv$ = "N" or neg_inv$ = "P"        ~
                                                              then return
                errormsg$ = "Please enter 'Y','N', or 'P'"
                return

L52110: REM Test for Blank Lot Flag (Y/N)
            if blank_lot$ = "Y" or blank_lot$ = "N" then return
                errormsg$ = "Please enter 'Y' or 'N'"
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Tests data for the line items.                            *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$, infomsg1$, infomsg2$ = " "
                  init(" ") header$()
                  c% = currentline%
                  on fieldnr% gosub L54220,         /* Part             */~
                                    L54530,         /* Store number     */~
                                    L54670,         /* Lot number       */~
                                    L54800,         /* Quantity added   */~
                                    L54970,         /* cost             */~
                                    L55010,         /* Source account   */~
                                    L55090,         /* Destination acct */~
                                    L55170,         /* Where from (free)*/~
                                    L55200,         /* Description      */~
                                    L55360          /* Job number       */
                  return

L54220:     REM Test data for part number
                plowkey$ = part$(c%)
                init(" ") incl_excl$()
                mat incl_excl = zer
                call "PLOWCODE" (#4, plowkey$, infomsg1$, -8025%, -.32,  ~
                                 f1%(4%), header$(), 0, 0, incl_excl(),  ~
                                 incl_excl$(), "Y", " ", #3)
                if f1%(4%) = 0% then L54330
                    infomsg1$ = "(" & infomsg1$ & ")"
                    part$(c%) = plowkey$
                goto L54360
L54330:            errormsg$ = "Part not found in HNYQUAN, please re-en"&~
                               "ter"
                   return
L54360:         call "READ100" (#3, plowkey$, f1%(3%))
                if f1%(3%) <> 0% then L54410
                    errormsg$ = "Non-stocked parts are not allowed, ple"&~
                                "ase re-enter"
                    return
L54410:         get #3 using L54420, lot_tracking$, serial_nbr$
L54420:             FMT POS(130), CH(1), CH(1)
                if lot_tracking$ <> "Y" then L54470
                    errormsg$ = "Parts requiring Lot Tracking are not a"&~
                                "llowed, please re-enter"
                    return
L54470:         if serial_nbr$ <> "Y" then L54510
                    errormsg$ = "Serial Numbered parts are not allowed,"&~
                                " please re-enter"
                    return
L54510:         if edit% = 0% then return else L54800

L54530:     REM Test data for store
                plowkey$ = str(part$(c%)) & store$(c%)
                init(" ") incl_excl$()
                mat incl_excl = zer
                call "PLOWCODE" (#4, plowkey$, infomsg1$,  8025%,  .30,  ~
                                 f1%(4%), header$(), 3, 0, incl_excl(),  ~
                                 incl_excl$(), "Y", " ", #6)
                if f1%(4%) = 0% then L54640
                   store$(c%) = str(plowkey$,26%)
                   infomsg1$ = "(" & infomsg1$ & ")"
                if edit% = 0% then return else L54800
L54640:            errormsg$ = "Part not available from this store or " &~
                               "qty on hand <= zero"
                   return
L54670:     REM Test lot number
                plowkey$ = str(part$(c%)) & str(store$(c%)) & lot$(c%)
                init(" ") incl_excl$()
                mat incl_excl = zer
                call "PLOWCODE" (#4, plowkey$, infomsg1$,  6028%,  .00,  ~
                                 f1%(4%), header$(), 0, 0, incl_excl(),  ~
                                 incl_excl$(), "Y")
                if f1%(4%) = 0% then L54770
                   lot$(c%) = str(plowkey$,29%)
                if edit% = 0% then return else L54800
L54770:            errormsg$ = "Lot not found or Qty in Lot <= zero"
                   return

L54800:     REM Test data for quantity
                call "NUMTEST" (qty$(c%),.01, 1e9, errormsg$, .2, qty)
                if errormsg$ > " " then return
                temp = qty - hnyhold(c%)
                   for i% = 1% to maxlines%
                       if i% = c% then L54910
                       if part$(i%) <> part$(c%) then L54910
                       if store$(i%) <> store$(c%) then L54910
                       if lot$(i%) <> lot$(c%) then L54910
                          convert qty$(i%) to temp1, data goto L54910
                          temp = temp + temp1 - hnyhold(i%)
L54910:            next i%
                if neg_inv$ = "Y" then L54950
                call "HNYAVAIL" (#3, #4, part$(c%), store$(c%), lot$(c%),~
                                         errormsg$, temp, temp1, return%)
L54950:         return

L54970:     REM Test data for total cost -- JIM -- 05/12/87
                call "NUMTEST" (totlcost$(c%), 0, 9e6, errormsg$, 2.4, 0)
                return

L55010:     REM Test data for source (inventory) account
                call "GETCODE" (#2, srcacct$(c%), infomsg1$, 1%,         ~
                     0, f1%(2%))
                if f1%(2%) = 1% then return
                   errormsg$="Source account not on file, please re-ent"&~
                             "er"
                   return

L55090:     REM Test data for destination (expense) account
                call "GETCODE" (#2, dstacct$(c%), infomsg1$, 1%,         ~
                     0, f1%(2%))
                if f1%(2%) = 1% then return
                   errormsg$ = "Destination account not on file, please"&~
                               " re-enter"
                   return

L55170:     REM Test data for free text field
                return

L55200: REM Test data for description of entry
            if descr$(c%) <> " " then goto L55250
                errormsg$ = "May not be blank. Enter a Description, '?', ~
        ~or Code."
                return
L55250:     if len(descr$(c%)) > 1% then return     /* Entered by user */
            if str(descr$(c%),,1%) = "?" then str(descr$(c%),,1%) = " "
            readkey$ = "INVWDWDSC" & str(descr$(c%),,1%)
            call "PLOWCODE" (#20, readkey$, descr$(c%), 9%, .3, f1%(20%))
            if f1%(20%) <> 0% then return
            if str(readkey$,10%,1%) = " " then L55330   /* Want 1-char? */
                descr$(c%) = str(readkey$,10%,1%)      /* 1-char Descr */
                return
L55330:     errormsg$ = "You must enter a Description, Code or '?'."
            return

L55360:     REM Test data for the job number
                if jobnr$(c%) = " " then return
                call "GETCODE" (#8, jobnr$(c%), infomsg1$, 1%, 0, f1%(8%))
                      if f1%(8%) = 1% then L55440
                errormsg$ = "Project not on file, please re-enter or le"&~
                            "ave blank"
                return

L55440:     REM Now lets see if job is closed and when
                get #8, using L55460, datejobclosed$
L55460:            FMT XX(44), CH(06)
                if datejobclosed$ = " " or ~
						~datejobclosed$ = blank_date$ then return
                if datejobclosed$ > hnydate$ then return
                call "DATEFMT" (datejobclosed$)
                errormsg$ = "Project was closed on  " & datejobclosed$
                return

        REM *************************************************************~
            *                          E X I T                          *~
            *---------------------------------------------------------- *~
            * Displays foreground message that we're switching, sets    *~
            * return code of zero if there's nothing in the buffer.     *~
            *************************************************************

        exit_program
*        Set return code for continuing to process.
            call "SHOSTAT" ("One Moment Please")
            readkey$ = str(userid$) & hex(000000)
            call "PLOWNEXT" (#5, readkey$, 3%, f1%(5%))
            if rpt% = 0% then L65160
                print
                runtime$ = " "  :  call "TIME" (runtime$)
                print using L35220, runtime$
L65160:     close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
        absolute_end
            end f1%(5%)
