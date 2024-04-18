        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   TTTTT   CCC   IIIII  M   M  PPPP   L      EEEEE   *~
            *  S        T    C   C    I    MM MM  P   P  L      E       *~
            *   SSS     T    C        I    M M M  PPPP   L      EEEE    *~
            *      S    T    C   C    I    M   M  P      L      E       *~
            *   SSS     T     CCC   IIIII  M   M  P      LLLLL  EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCIMPLE - Sets a New Standard Cost Set Effective by      *~
            *            Placing Costs in HNYMASTR and Revalues Parts   *~
            *            Flagged for Fixed Standard Cost.               *~
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
            * 04/28/87 ! Original (STCIMPL1- prerelease version)  ! KEN *~
            * 07/22/87 ! Release 5.0 version                      ! ERN *~
            * 03/24/89 ! Added User Option to keep unused Bucket  ! RJM *~
            *          !  Variance Accounts in HNYMASTR & HNYQUAN !     *~
            *          !  If Remaping of the Buckets is required. !     *~
            * 10/03/90 ! Now honoring HNYflag to post revaluation ! JDH *~
            *          !  to Inv Variance Accts or Revaluation Acc!     *~
            *          ! Merge of A/C 1 and GL Export Option.     !     *~
            * 05/21/90 ! No reads or puts if GL Export is off.    ! JDH *~
            * 06/07/90 ! PRR 11769.  Backgnd processing now works.! JDH *~
            * 11/04/91 ! CMS/DEC 'MASK' Project/Added 'ALLFREE'   ! SID *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 03/25/92 ! PRR 12358. DateTime Stamp Delay & EOD.   ! RJH *~
            * 08/18/92 ! CORE STUFF - SEARCH FOR 'CORE'           ! KAB *~
            * 11/11/92 ! Increased Fx% DIMs to 64                 ! KAB *~
            * 03/04/92 ! Added Core Value Coding.                 ! JBK *~
            * 10/06/93 ! Revalue Core Bank, FMT problem posts     ! KAB *~
            *          !    Prebilled Cores to Suspence Acct.     !     *~
            * 12/15/93 ! Restart Logic, if we can                 ! KAB *~
            * 06/09/95 ! Expanded COGS Accts Project.             ! RJH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            accts$(12)9, acct2$(12)9,    /* Variance accounts          */~
            bmap$(12)2,                  /* Bucket Map                 */~
            bmap%(12),                   /* Bucket Map                 */~
            core_swtchs$200,             /* Core Flags                 */~
            core_acct$9,                 /* Core Variance Account      */~
            core_cflg$1,                 /* Core Cost Flag             */~
            core_cogs$9,                 /* Core COGS Account          */~
            core_cdla$9,                 /* Core Dep Liab. Account     */~
            core_cdlf$1,                 /* Core Dep Liab. Flag        */~
            core_unap$9,                 /* Core Unapplied Account     */~
            core_unfl$1,                 /* Core Unapplied Flag        */~
            core_key$20,                 /* Core Record Key            */~
            core_part$25,                /* Core Part Number           */~
            core_wrk_key$50,             /* Core Work File Key         */~
            cfg_acct$9,                  /* Core Finished Goods Acct   */~
            costmethod$1,                /* Inventory Costing Method   */~
            costs$96,                    /* Costs for remapping        */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            glamt(12),                   /* G/L Adjustments            */~
            gltext$100,                  /* General Ledger Text        */~
            header$(2)79,                /* Table Header               */~
            hnyacct$9,                   /* Inventory Asset Account    */~
            hnyqcost(12),                /* Old HNYQUAN Cost  (L,M,O)  */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            method$1,                    /* Post to Inv Var or Revalue */~
            newbktid$(12)10,             /* New Set- Bucket IDs        */~
            newbktdescr$(12)20,          /*          Bucket Descrs     */~
            newbktnr$(12)2,              /*          Bucket Numbers    */~
            newset$8, newsetdescr$32,    /*          Name and Descr    */~
            newsetid$4,                  /*          Internal ID       */~
            oldbktid$(12)10,             /* Old Set- Bucket IDs        */~
            oldbktdescr$(12)20,          /*          Bucket Descrs     */~
            oldset$8, oldsetid$4,        /*          Names             */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            postdate$8,                  /* Posting Date               */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            restart_flags$3,             /* Restart Control Flags      */~
            x$(12)4, f$(12)4, ff$(12)4,  /* Restart Map-ablity test    */~
            a$(12), af$(12), aff$(12),   /*  variables                 */~
            revalu_acct$9,               /* System Revaluation Account */~
            revalu_accts$(200,2)9,       /* Stores & Revaluation Accts */~
            stccost(12), stccost$96,     /* New Std Costs (Packed$)    */~
            stccurr$200,                 /* Current effective Sets     */~
            stcdate$150,                 /* Current effective Dates    */~
            stcint$100,                  /* Current effective Sets     */~
            store$3,                     /* Store from HNYQUAN         */~
            userid$3                     /* Current User Id            */

        dim export_on$1,                 /* G/L Export File processing?*/~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            partcat$4,                   /* Part Category code         */~
            partclass$4,                 /* Part Class code            */~
            partgen$16,                  /* Part Generic code          */~
            parttype$3,                  /* Part Type code             */~
            tran_type$5,                 /* G/L Transaction type       */~
            uom$                         /* Part Unit of measure       */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20,                 /* Text from file opening     */~
            axd$4                        /* A relic from bygone days   */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
            mat f2% = con

*        If running in background call STCIMPFL and End.
            call "EXTRACT" addr("TT", str(temp$,,1))
            if temp$ <> "B" then L02000
               call "STCIMPFL"
               end

L02000: REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 2 ! SYSFILE2 ! System Control File                      *~
            * # 3 ! HNYMASTR ! Inventory Master File                    *~
            * # 4 ! HNYQUAN  ! Inventory Store/Lot File                 *~
            * # 5 ! HNYPOOL  ! Inventory Store/Lot Detail File          *~
            * # 7 ! STCJRNTF ! General Ledger Journal Detail            *~
            * # 8 ! USERINFO ! User Posting Dates                       *~
            * # 9 ! HNYPITKT ! Physical Inventory Ticket File           *~
            * #10 ! STCHNY   ! Standard Cost Set- Inventory Standards   *~
            * #11 ! STCDETAL ! Standard Cost Details                    *~
            * #12 ! STCWCACT ! Standard Cost Work Center / Activities   *~
            * #13 ! STCLABOR ! Standard Costing Labor Standards         *~
            * #14 ! STCMAPNG ! Standard Cost Set Mappings               *~
            * #15 ! STCCHNGS ! Tickler file of changed components       *~
            * #16 ! GLCOGSXR ! GL COGS Accounts via Cost buckets        *~
            * #50 ! CORDRMAS ! Core Debit Master File                   *~
            * #51 ! CORCRMAS ! Core Credit Master File                  *~
            * #52 ! COREXREF ! Core Cross Reference File                *~
            * #53 ! COREWRK  ! Core Work File                           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 2, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select # 3, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select # 4, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            select # 5, "HNYPOOL",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  38                      ~

            select # 7, "STCJRNTF"                                       ~
                        varc,     indexed,  recsize =  645,              ~
                        keypos =    1, keylen =  10                      ~

            select # 8, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            select # 9, "HNYPITKT",                                      ~
                        varc, indexed, recsize = 512,                    ~
                        keypos =    1, keylen =  15,                     ~
                        alt key  1, keypos =   16, keylen =  42, dup,    ~
                            key  2, keypos =  313, keylen =  16

            select #10, "STCHNY",                                        ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos = 1,    keylen = 25

            select #11, "STCDETAL",                                      ~
                        varc,     indexed,  recsize =  340,              ~
                        keypos = 1,    keylen = 28

            select #12, "STCWCACT",                                      ~
                        varc,     indexed,  recsize =  381,              ~
                        keypos =  1,   keylen = 8

            select #13, "STCLABOR",                                      ~
                        varc,     indexed,  recsize =  323,              ~
                        keypos =  1,   keylen = 4

            select #14, "STCMAPNG",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =  1,   keylen =  8

            select #15, "STCCHNGS",                                      ~
                        varc,     indexed,  recsize =   26,              ~
                        keypos =    1, keylen =  26

            select #16, "GLCOGSXR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  9

            select #50, "CORDRMAS",                                      ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   10, keylen =  20,                     ~
                        alt key  1, keypos =    1, keylen =  29,         ~
                            key  2, keypos =   19, keylen =  11, dup,    ~
                            key  3, keypos =   36, keylen =  34, dup,    ~
                            key  4, keypos =   61, keylen =  40, dup,    ~
                            key  5, keypos =  601, keylen =  40, dup     ~

            select #51, "CORCRMAS",                                      ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   10, keylen =  20,                     ~
                        alt key  1, keypos =    1, keylen =  29,         ~
                            key  2, keypos =   19, keylen =  11, dup,    ~
                            key  3, keypos =   36, keylen =  34, dup,    ~
                            key  4, keypos =   61, keylen =  40, dup,    ~
                            key  5, keypos =  601, keylen =  40, dup     ~

            select #52, "COREXREF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =   26, keylen =  50,                     ~
                        alt key  1, keypos =    1, keylen =  50,         ~
                            key  2, keypos =   76, keylen =  25, dup

            select #53, "COREWRK",                                       ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  50

            rslt$(2) = "REQUIRED"
            call "OPENFILE" (# 2, "SHARE", f2%( 2), rslt$( 2), axd$)
            if f2%(2) <> 0% then exit_program_no_impl
                plowkey$ = "STCIMPLE.CONTROL"
                call "READ100" (#2, plowkey$, restart%)
                if restart% = 0% then L03570
                   goto restart_1
L03570:         close #2   /* Just in case they pause & backup next */

L04000: u3% = 2%
        call "ASKUSER" (u3%, "COST SET IMPLEMENTATION",                  ~
                        "Before running your data must be backed up",    ~
                        "and there must be no files opened.",            ~
                        "Press RETURN to Continue -or- PF-16 to Exit.")
            if u3% = 16% then exit_program_no_impl
            if u3% <> 0% then L04000

        restart_resume_1

        call "SHOSTAT" ("Opening Files, One Moment Please")
            rslt$(2) = "REQUIRED"
            call "OPENFILE" (# 2, "SHARE", f2%( 2), rslt$( 2), axd$)
            if f2%(2) <> 0% then exit_program_no_impl

            call "OPENCHCK" (# 9, fs%( 9), f2%( 9),   0%, rslt$( 9))
            if f2%(9) <> 0% then L05240
                init (hex(00)) plowkey$
                call "PLOWNEXT" (#9, plowkey$, 0%, temp%)
                if temp% = 0% then L05240
                     call "ASKUSER" (2%, "COST SET IMPLEMENTATION",      ~
                        "All physical inventory sessions must be closed",~
                        "before you can implement a new cost set.",      ~
                        "Press RETURN to Exit...")
                     goto exit_program_no_impl

L05240:     call "OPENFILE" (# 3, "IO   ", f2%( 3), rslt$( 3), axd$)
            call "OPENFILE" (# 4, "IO   ", f2%( 4), rslt$( 4), axd$)
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 100%, rslt$( 5))
            close #5
            call "OPENFILE" (# 5, "IO   ", f2%( 5), rslt$( 5), axd$)
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 100%, rslt$( 7))
            close #7
            call "OPENFILE" (# 7, "IO   ", f2%( 7), rslt$( 7), axd$)
            call "OPENFILE" (# 8, "IO   ", f2%( 8), rslt$( 8), axd$)
            call "OPENCHCK" (#16, fs%(16), f2%(16),   0%, rslt$(16))
               expandcogs% = fs%(16%) + 1% /*ie. zero if not open */

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(header$(1),  1) = "C U R R E N T   C O S T   S E T"
            str(header$(1), 47) = "    N E W   C O S T   S E T      "
            str(header$(2),  1) = "Bucket ID"
            str(header$(2), 11) = hex(ac) & "Bucket Description  " &     ~
                                                                  hex(8c)
            str(header$(2), 34) = hex(ac) & "Maps To:" & hex(8c)
            str(header$(2), 45) = hex(ac) & "##"
            str(header$(2), 48) = hex(ac) & "Bucket ID"
            str(header$(2), 59) = hex(ac) & "Bucket Description  " &     ~
                                                                  hex(8c)

*        Get Users Posting Date...
            call "READ100" (#8, userid$, f1%(8))
            if f1%(8) <> 0% then L09320
                call "ASKUSER" (keyhit%, "Sorry",                        ~
                  "You're Not Listed As A Valid User In This Data Base", ~
                                          " ", "Press (RETURN) To Exit.")
                goto exit_program_no_impl
L09320:     get #8, using L09330, postdate$
L09330:         FMT POS(22), CH(6)       /* GL POSTING DATE */

*        Validate Users Posting Date...
            call "WHICHMON" (#2, postdate$, temp%)
            if temp% <> 0% then L09430
                call "ASKUSER" (keyhit%, "Sorry",                        ~
                      "Your Posting Date Is Outside The Posting Window", ~
                                          " ", "Press (RETURN) To Exit.")
                goto exit_program_no_impl

L09430
*        Describe Current (old) Cost Set
            oldbkts% = 3%
            call "STCSETID" (oldbkts%, #2, oldset$, oldsetid$,           ~
                             oldbktid$(), oldbktdescr$(), errormsg$)
            errormsg$ = " "
            if oldbkts% <> 0% then L09600
                oldbkts%        = 3%
                oldset$         = "BEFORE 5"
                oldbktid$(1)    = "Material"
                oldbktid$(2)    = "Labor   "
                oldbktid$(3)    = "Overhead"
                oldbktdescr$(1) = "Material Costs"
                oldbktdescr$(2) = "Labor Costs   "
                oldbktdescr$(3) = "Overhead Costs"

L09600
*        Now get Suspense Account
            plowkey$ = "FISCAL DATES"
            call "READ100" (#2, plowkey$, f1%(2%))
                if f1%(2%) = 0% then L09660
                     get #2 using L09640, temp$
L09640:                   FMT POS(417), CH(9)

L09660
*        Now get Revaluation Method & Revaluation Acct
            method$ = "V"  :  plowkey$ = "SWITCHS.HNY"
            call "READ100" (#2, plowkey$, f1%(2%))
                if f1%(2%) = 0% then L09724
                     get #2 using L09710, method$, revalu_acct$
L09710:                   FMT POS(97), CH(1), CH(9)
            if revalu_acct$ = " " then revalu_acct$ = temp$

L09724
*        See if G/L Export is on
            export_on$ = "N"  :  plowkey$ = "SWITCHS.GL"
            call "READ100" (#2, plowkey$, f1%(2%))
            if f1%(2%) = 1% then get #2 using L09732, export_on$
L09732:         FMT POS(22), CH(1)

*        Now build up array of stores and revaluation accounts
            c% = 0% : init (" ") revalu_accts$()
            plowkey$ = "DEFAULTS.STORE." & hex(000000)
L09770:     call "PLOWNEXT" (#2, plowkey$, 15%, f1%(2))
                if f1%(2) = 0% then L09860
            c% = c% + 1%
            get #2 using L09810, revalu_accts$(c%, 1), revalu_accts$(c%, 2)
L09810:         FMT POS(16), CH(3), POS(171), CH(9)
            if revalu_accts$(c%, 2) = " " then                           ~
                                      revalu_accts$(c%, 2) = revalu_acct$
            goto L09770

L09860
*        Check For Core Bank
            plowkey$ = "SWITCHS.COR"
            call "READ100" (#2, plowkey$, core_bank%)
               core_value% = core_bank%
               if core_bank% = 0% then L10000     /* No Core Processing */
            get #2 using L09901, core_swtchs$
L09901:         FMT CH(200)

*        Have found a coreflags record, now must determined the value
*        of two different switch settings to see what will be processed
*        for Cores.  The first switch, "Revalue Cores on Cost Set
*        Implementation", is in the Coreflags record at position 94.  If
*        this flag is on (set to 'Y'), CORE_BANK% will be set to 1 and
*        three file will be opened, #50, #51, #52.  The second switch,
*        "Core Value Postings Active in Inventory", is in the Coreflags
*        record at position 134. If this flag is on (set to 'Y'),
*        CORE_VALUE% will be set to 1 and two file will be opened if
*        necessary, #52 and #53.

            if str(core_swtchs$,114%,1%) <> "Y" then core_bank% = 0%
               if core_bank% = 0% then L09960
            call "OPENFILE" (#50, "IO   ", f2%(50), rslt$(50), axd$)
            call "OPENFILE" (#51, "IO   ", f2%(51), rslt$(51), axd$)
            call "OPENFILE" (#52, "IO   ", f2%(52), rslt$(52), axd$)
L09960:     if str(core_swtchs$,134%,1%) <> "Y" then core_value% = 0%
               if core_value% <> 1% then L10000
            if core_bank% <> 1% then call "OPENFILE" (#52, "IO   ",      ~
                                                f2%(52), rslt$(52), axd$)
            call "WORKOPEN" (#53, "IO", 100%, f2%(53%))

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            on restart% goto revalue2, restart_resume_3
            pf16$ = "(16)Exit Program"
            gosub L29000

            for fieldnr% = 1% to 2%
                gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                     if enabled% = 0% then L10240
L10140:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                     if keyhit%  =  1% then gosub startover
                     if keyhit%  = 16% then exit_program_no_impl
                     if keyhit% <>  0% then L10140
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then L10140
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            pf16$ = "(16)Install Set"
            inpmessage$ = edtmessage$

            gosub'101(0%)               /* Display Screen - No Entry   */
                if keyhit%  =  1% then gosub startover
                if keyhit%  = 16% then install_set
                if keyhit% <>  0% then editpg1

            fieldnr% = 2%

L11170:     gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled% = 0% then editpg1
                pf16$ = " "
L11200:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then L11200
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " " then L11170
            goto editpg1

        REM *************************************************************~
            * Restart Control Section                                   *~
            *************************************************************

        restart_1

            get #2 using L15110, rdate$, userid$,                         ~
                                  bmap%(), restart_flags$,               ~
                                  remap%, remap_keep_flag%,              ~
                                  newset$, newsetid$,                    ~
                                  core_wrk_key$
L15110:         FMT POS(21), CH(6), CH(3), 12*BI(1), CH(3),              ~
                    2*BI(4), CH(8), CH(4), CH(50)
            rdateu$ = rdate$ : call "DATEFMT" (rdate$)
            call "EXTRACT" addr("ID", temp$)
            if userid$ = temp$ then restart_2           /* Same Person */

            call "ASKUSER" (0%, "IMPLEMENTATION IN PROCESS",             ~
                 "An Implementation has been initiated by " & userid$ &  ~
                 " on " & rdate$ & " for set " & newset$,                ~
                 "and must be completed before another implementation" & ~
                 " is possible.",                                        ~
                 "Press RETURN to Exit...")
            goto exit_program_no_impl

        restart_2
            if str(restart_flags$,1%,3%) = "CC " then L15270
            if str(restart_flags$,1%,3%) = "CCC" then L15270
            if remap% <> 0% then restart_3          /* Remap in Effect */
               /* Must be 'IC ' & REMAP% = 0% */
L15270:     ask% = 0%
            call "ASKUSER" (ask%, "IMPLEMENTATION RESTART - NO MAP",     ~
                 "A Cost Set Implementation has been initiated on " &    ~
                  rdate$ & " for set " & newset$,                        ~
                 "and must be completed before another implementation" & ~
                 " is possible.",                                        ~
                 "Press PF8 to Continue Restart, PF16 to Exit.")
            if ask% =  8% then restart_resume_0
            if ask% = 16% then exit_program_no_impl
               goto L15270
        restart_resume_0
            if str(restart_flags$,1%,3%) = "CCC" then restart_resume_last
            if str(restart_flags$,1%,3%) = "CC " then exit_program
            goto restart_resume_1

        restart_3
            if remap% > 1% then gosub restart_background_warning
            if str(restart_flags$,1%,2%) = "C " then L15420
               gosub restart_test_map
L15420:     if str(restart_flags$,1%,3%) <> "IC " then L15450
               remap% = 3%
               goto L15270                      /* No Need for STCIMPFL */
L15450:     ask% = 0%
            call "ASKUSER" (ask%, "IMPLEMENTATION RESTART - FILE MAP",   ~
                 "A Cost Set Implementation has been initiated on " &    ~
                  rdate$ & " for set " & newset$,                        ~
                 "and must be completed before another implementation" & ~
                 " is possible.",                                        ~
                 "Press PF8 to Continue Restart, PF16 to Exit.")
            if ask% =  8% then restart_4
            if ask% = 16% then exit_program_no_impl
               goto L15450

        restart_4
            holdflags$ = str(restart_flags$,1%,3%)
            holdremap% = remap%
            remap% = 1%
            str(restart_flags$,2%,1%) = " "
            plowkey$ = "STCIMPLE.CONTROL"
            call "READ101" (#2, plowkey$, f1%(2))
               if f1%(2) = 0% then exit_program_no_impl
            put #2 using L15587, str(restart_flags$,1%,3%), remap%
L15587:         FMT POS(42), CH(3), BI(4)
            rewrite #2
            if str(restart_flags$,1%,2%) = "C " then restart% = 2%

L15600
*        DO need to remap files.  Do it in background or foreground?
            keyhit% = 2%
            call "ASKUSER" (keyhit%,                                     ~
                  "COST SET IMPLEMENTATION RESTART",                     ~
                  "Press PF-1 to EXIT PROGRAM; - OR -",                  ~
                  "Press PF-16 to CONTINUE (remap files in foreground),",~
                  "Press PF-32 to CONTINUE (remap files in background).")
            if keyhit%  =  1% then restart_abort
            if keyhit%  = 32% then L15730
            if keyhit% <> 16% then L15600
               on restart% goto restart_resume_1, restart_resume_3

*        Queue remapping to occur in background....
L15730:     remap% = 2% : u3% = 0%
            plowkey$ = "STCIMPLE.CONTROL"
            call "READ101" (#2, plowkey$, f1%(2))
               if f1%(2) = 0% then exit_program_no_impl
            put #2 using L15740, remap%
L15740:         FMT POS(45), BI(4)
            rewrite #2

            call "TASKUP" ("ME", u3%)
            if u3% <> 0% then L15780
               if restart% = 2% then exit_program
               goto restart_resume_1

L15780:     remap% = 1%
            plowkey$ = "STCIMPLE.CONTROL"
            call "READ101" (#2, plowkey$, f1%(2))
               if f1%(2) = 0% then exit_program_no_impl
            put #2 using L15816, remap%
L15816:         FMT POS(45), BI(4)
            rewrite #2

L15820
*        DO need to remap files.  Do it in background or foreground?
            keyhit% = 2%
            call "ASKUSER" (keyhit%,                                     ~
                  "BACKGROUND TASK FAILURE - RESTART",                   ~
                  "Press PF-1 to EXIT PROGRAM; - OR -",                  ~
                  "Press PF-16 to CONTINUE (remap files in foreground),",~
                  "Press PF-32 to RETRY    (remap files in background).")
            if keyhit%  =  1% then restart_abort
            if keyhit%  = 32% then L15730
            if keyhit% <> 16% then L15820
               on restart% goto restart_resume_1, restart_resume_3

        restart_abort
            plowkey$ = "STCIMPLE.CONTROL"
            call "READ101" (#2, plowkey$, f1%(2))
               if f1%(2) = 0% then exit_program_no_impl
            put #2 using L15965, holdflags$, holdremap%
L15965:         FMT POS(42), CH(3), BI(4)
            rewrite #2
            goto exit_program_no_impl

        restart_background_warning
            if str(restart_flags$,2%,1%) = "C" then return
L16010:     ask% = 0%
            call "ASKUSER" (ask%, "BACKGROUND WARNING",                  ~
                 "A Cost Set Implementation has been initiated on " &    ~
                  rdate$ & " for set " & newset$,                        ~
                 "Be sure that part of the process is NOT running or " & ~
                 "suspended in BACKGROUND",                              ~
                 "Press PF8 to Continue Restart, PF16 to Exit.")
            if ask% =  8% then return
            if ask% = 16% then exit_program_no_impl
               goto L16010

        restart_test_map

            init (hex(00)) x$(), f$(), ff$()
            init (" ") a$(), af$(), aff$()
            holdremap% = 0%

            for i% = 1% to 12%
                x$(i%) = bin(2%^(i%-1%),4)
                a$(i%) = bin(64% + i%, 1)
            next i%

            for i% = 1% to 12%
                if bmap%(i%) = 0% then L16340
                   f$(bmap%(i%)) = f$(bmap%(i%)) or x$(i%)
L16340:     next i%

            for i% = 1% to 12%
                if bmap%(i%) = 0% then L16390
                   ff$(bmap%(i%)) = ff$(bmap%(i%)) or f$(i%)
L16390:     next i%

            if remap_keep_flag% = 1% then str(af$()) = str(a$())
            for i% = 1% to 12%
                if bmap%(i%) <> 0% then af$(bmap%(i%)) = a$(i%)
            next i%

            if remap_keep_flag% = 1% then str(aff$()) = str(af$())
            for i% = 1% to 12%
                if bmap%(i%) <> 0% then aff$(bmap%(i%)) = af$(i%)
            next i%

            if str(f$()) = str(ff$()) then L16640
            if str(restart_flags$,2%,1%) = "C" then L16780

L16530:     ask% = 0%
            call "ASKUSER" (ask%, "NO RESTART - MAPPING",                ~
                 "A Cost Set Implementation has been initiated on " &    ~
                  rdate$ & " for set " & newset$,                        ~
                 "The process CAN NOT be restarted.  Restoring the" &    ~
                 " data base is required.",                              ~
                 "Press PF16 to Exit....")
            if ask% = 16% then exit_program_no_impl
               goto L16530

L16640:     if str(restart_flags$,1%,1%) = "C" then return
            if str(af$()) = str(aff$()) then return

L16670:     ask% = 0%
            call "ASKUSER" (ask%, "CONDITIONAL RESTART - ACCOUNTS",      ~
                 "A Cost Set Implementation has been initiated on " &    ~
                  rdate$ & " for set " & newset$,                        ~
                 "Restart WILL result in loss of G/L" &                  ~
                 " Accounts in Inventory Records",                       ~
                 "Press PF8 to Continue Restart, PF16 to Exit.")
            if ask% =  8% then return
            if ask% = 16% then exit_program_no_impl
               goto L16670

L16780:     ask% = 0%
            call "ASKUSER" (ask%, "CONDITIONAL RESTART - MAPPING",       ~
                 "A Cost Set Implementation has been initiated on " &    ~
                  rdate$ & " for set " & newset$,                        ~
                 "Restart WILL damage DATA unless Inventory is " &       ~
                 "Valued at FIXED STANDARD",                             ~
                 "Press PF8 to Continue Restart, PF16 to Exit.")
            if ask% =  8% then L16640
            if ask% = 16% then exit_program_no_impl
               goto L16780

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        install_set

*        See if bucket remapping is required...
            remap% = 0% : remap_keep_flag% = 1%
            for b% = 1% to 12%
                if bmap%(b%) = 0% then L19130
                if bmap%(b%) = b% then L19130
                     remap%  = 1%
                     goto L19240
L19130:     next b%

*        DO NOT need to Remap.  Get confirmation to continue...
L19160:     keyhit% = 2%
            call "ASKUSER" (keyhit%,                                     ~
                     "* * * COST SET IMPLEMENTATION * * *",              ~
                     "Press PF-1 to RETURN to EDIT MODE.", "- OR -",     ~
                     "Press PF-16 to CONTINUE with implementation.")
            if keyhit%  =  1% then editpg1
            if keyhit%  = 16% then revalue else L19160

L19240
*        DO need to remap files.  ERASE Unused Bucket Variance Accounts?
L19250:     keyhit% = 2%
            remap_keep_flag% = -1%
            call "ASKUSER" (keyhit%,                                     ~
                  "COST SET IMPLEMENTATION - REMAP OPTION",              ~
                  hex(8c) & "HNYMASTR & HNYQUAN Bucket Variance Accts " &~
                      "may be Erased during Remap",                      ~
                  hex(8c) & "Press" & hex(84) & "PF-1 to ERASE" & hex(8c)~
                    & "Unused Bucket Variance Accounts",                 ~
                  hex(8c) & "Press" & hex(84) & "PF-16 to KEEP" & hex(8c)~
                    & "Unused Bucket Variance Accounts")
            if keyhit%  =  1% then remap_keep_flag% = 0%
            if keyhit%  = 16% then remap_keep_flag% = 1%
            if keyhit%  = 32% then editpg1
            if remap_keep_flag% = -1% then L19250

L19400
*        DO need to remap files.  Do it in background or foreground?
            keyhit% = 2%
            call "ASKUSER" (keyhit%,                                     ~
                  "* * * COST SET IMPLEMENTATION * * *",                 ~
                  "Press PF-1 to RETURN to EDIT MODE; - OR -",           ~
                  "Press PF-16 to CONTINUE (remap files in foreground),",~
                  "Press PF-32 to CONTINUE (remap files in background).")
            if keyhit%  =  1% then editpg1
            if keyhit%  = 32% then L19500
            if keyhit%  = 16% then revalue
               goto L19400

*        Queue remapping to occur in background....
L19500:     remap% = 2% : u3% = 0%
            gosub write_control_record

            call "TASKUP" ("ME", u3%)
            if u3% = 0% then revalue2
                plowkey$ = "STCIMPLE.CONTROL"
                call "READ101" (#2, plowkey$, f1%(2))
                if f1%(2) = 1% then delete #2
                remap% = 1%
L19572
*        DO need to remap files.  Do it in background or foreground?
            keyhit% = 2%
            call "ASKUSER" (keyhit%,                                     ~
                  "* * * BACKGROUND TASK FAILURE * * *",                 ~
                  "Press PF-1 to RETURN to EDIT MODE; - OR -",           ~
                  "Press PF-16 to CONTINUE (remap files in foreground),",~
                  "Press PF-32 to RETRY    (remap files in background).")
            if keyhit%  =  1% then editpg1
            if keyhit%  = 32% then L19500
            if keyhit%  = 16% then revalue
               goto L19572

        revalue
            gosub write_control_record

        revalue2
            call "SHOSTAT" ("Cost Set Implementation in Process.")
            call "STCFOPEN" (newset$, "C", #2, errormsg$,                ~
                                            #10, #11, #12, #13, #14, #15)
            if oldset$ <> newset$ then hnymastr_loop
               if core_value% = 1% then call "FILEBGON" (#53)
               core_value% = 0%

        hnymastr_loop
            call "READNXT1" (#3, f1%(3))
            if f1%(3) = 1% then L19740
                if core_value% = 1% then gosub revalue_cores
                   plowkey$ = "STCIMPLE.CONTROL"
                   call "READ101" (#2, plowkey$, f1%(2))
                      if f1%(2) = 0% then L19715 /* Better never happen */
                init (hex(ff)) core_wrk_key$
                put #2 using L19711, "C", core_wrk_key$
L19711:              FMT POS(42), CH(1), POS(65), CH(50)
                rewrite #2

L19715: restart_resume_3
                if remap% <> 1% then exit_program
                call "SHOSTAT"                                           ~
                     ("Cost Set Implementation Phase 2 in Process.")
                call "STCIMPFL"
                goto exit_program

L19740:     if remap% = 0% then L19810
                get #3 using L19760, accts$() /*HNYMASTR Variance Accts*/
L19760:              FMT POS(389), 12*CH(9)
                gosub remap_accts
                put #3 using L19760, accts$()
                rewrite #3

L19810:     call "STCCOSTS" (key(#3), newset$, #2, 12%, stccost,         ~
                                                               stccost())
            call "PACKZERO" (stccost(), stccost$)

            gosub reset_fixed_standard
            if core_value% = 1% then gosub check_for_reman
            goto  hnymastr_loop


        write_control_record
            restart_flags$ = "IC "
            if remap% = 0% then L19930
            if remap% > 2% then L19930
            str(restart_flags$,2%,1%) = " "
L19930:     write #2 using L19950, "STCIMPLE.CONTROL", date, userid$,     ~
                                  bmap%(), restart_flags$,               ~
                                  remap%, remap_keep_flag%,              ~
                                  newset$, newsetid$,                    ~
                                  core_wrk_key$, 0%, " " , " "
L19950:         FMT CH(20), CH(6), CH(3), 12*BI(1), CH(3),               ~
                    2*BI(4), CH(8), CH(4), CH(50), BI(4),                ~
                    CH(182), CH(200)
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20130,         /* Cost Set           */    ~
                              L20170          /* Bucket Id          */
            return

L20130: REM Def/Enable Cost Set                    NEWSET$
            inpmessage$ = "Enter Cost Set Id.  Current Set = " & oldset$
            return

L20170: REM Def/Enable Bucket Id                   BCKID$
            inpmessage$ = "Enter mapping from Current Set to New Set."
            if oldbkts% <> newbkts% then return
            if str(bmap$()) <> " " then L20250
            mat bmap% = zer
            for i% = 1% to oldbkts%
                bmap%(i%) = i%
                convert bmap%(i%) to bmap$(i%), pic(#0)
            next i%
L20250:     if newset$ = oldset$ then enabled% = 0%
            return

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, newset$, newsetdescr$,     ~
                      newbktid$(), newbktdescr$(), newbktnr$(), bmap$()
            init(hex(00)) core_wrk_key$
            mat bmap% = zer
            call "ALLFREE"
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
            *         R E S E T   F I X E D   S T A N D A R D           *~
            *-----------------------------------------------------------*~
            * Accomplishes two tasks- (1) Remaps HNYQUAN and HNYPOOL    *~
            * if required and (2) revalues str/lot records where cost   *~
            * method has been designated as fixed standard.             *~
            *************************************************************
        reset_fixed_standard

            plowkey$ = key(#3)
            if core_value% = 1% then reman_qty = 0

        hnyquan_loop
            call "PLOWNXT1" (#4, plowkey$, 25%, f1%(4))
            if f1%(4) = 0% then revalue_core_bank

            get #4 using L30170, hnyqty, hnyqcost(), hnyacct$,            ~
                                accts$(), costmethod$
L30170:         FMT POS(69), PD(14,4), POS(125), 12*PD(14,4),            ~
                    POS(259), CH(9), POS(295), 12*CH(9), POS(403), CH(1)

            store$ = str(plowkey$, 26, 3)
            if core_value% = 1% then reman_qty = reman_qty + hnyqty

            if remap% = 0% and costmethod$ <> "F" then hnyquan_loop

            if remap% = 0% then L30340
                put costs$ using cstfmt, hnyqcost()  /* Remapping      */
                     cstfmt: FMT 12*PD(14,4)
                call "STCREMAP" (bmap%(), costs$)
                get costs$ using cstfmt, hnyqcost()
                gosub remap_accts
                put #4 using L30290, costs$, accts$()
L30290:              FMT POS(125), CH(96), POS(295), 12*CH(9)
                if costmethod$ = "F" then L30340
                     rewrite #4
                     goto pools

L30340:     if costmethod$ <> "F" then pools
                put #4 using L30360, stccost, stccost$
L30360:              FMT POS(117), PD(14,4), CH(96)
                rewrite #4
                if hnyqty <> 0 then gosub update_gl

          pools
            init (hex(00)) str(plowkey$,35)
            if costmethod$ = "F" then L30540

          /* Here we just need to remap (not revalue)        */
L30450:     call "PLOWNXT1" (#5, plowkey$, 34%, f1%(5))
            if f1%(5) = 0% then get_out_of_the_pool
                get #5 using L30480, costs$
L30480:              FMT POS(63), CH(96)
                call "STCREMAP" (bmap%(), costs$)
                put #5 using L30480, costs$
                rewrite #5
                goto L30450

L30540:   /* Here we need to revalue                         */
            if hnyqty = 0 then L30610
            call "PLOWNXT1" (#5, plowkey$, 34%, f1%(5))
            if f1%(5) = 0% then build_pool /* How did this happen? */
                put #5, using L30580, hnyqty, stccost, stccost$
L30580:              FMT POS(39), PD(14,4), POS(55), PD(14,4), CH(96)
                rewrite #5

L30610:         call "PLOWNXT1" (#5, plowkey$, 34%, f1%(5))
                if f1%(5) = 0% then get_out_of_the_pool
                     delete #5
                     goto L30610

            build_pool
                write #5 using L30700, str(plowkey$,,34), 9999%, hnyqty,  ~
                          hnyqty, stccost, stccost$, date, hnyacct$, " ",~
                          "CREATED BY STD COST IMPLEMENTATION", " "
L30700:              FMT CH(34), BI(4), 3*PD(14,4), CH(96), CH(6),       ~
                         2*CH(9), CH(40), CH(78)

          get_out_of_the_pool
            init(" ") str(plowkey$,35)
            goto hnyquan_loop


        remap_accts
*        Works on ACCTS$() using ACCT2$() as working string
            if remap% = 0% then return
                init(" ") acct2$()
                if remap_keep_flag% = 1% then                            ~
                     str(acct2$()) = str(accts$())
                for b% = 1% to 12%
                     if bmap%(b%) <> 0% then acct2$(bmap%(b%)) =         ~
                                             accts$(b%)
                next b%
                str(accts$()) = str(acct2$())
                return

        REM *************************************************************~
            *          U P D A T E   G E N E R A L   L E D G E R        *~
            *-----------------------------------------------------------*~
            * Write variance postings to the general journal file.      *~
            * Funny looking code results from the fact that we don't    *~
            * want to write (1) the same transaction to two different   *~
            * journals and (2) the maximum dollar amount must be        *~
            * able to be formatted into a 10 character field.           *~
            *************************************************************
        update_gl
            mat glamt = stccost - hnyqcost
            mat glamt = (hnyqty) * glamt
            temp% = 0% : glamt = 0
            for i% = 1% to 12%
                if glamt(i%) = 0 then L31160
                     temp% = temp% + 2%*(int(abs(glamt(i%))/9000000%)+1%)
                     glamt = glamt + glamt(i%)
L31160:     next i%
            if temp% = 0% then return

*        Do we do this for 12 variance accts or 1 revaluation acct
            if method$ <> "R" then L31200
                saveacct$ = accts$(1) : saveamt  = glamt(1)
                accts$(1) = " "       : glamt(1) = glamt
                for i% = 1% to 200%
                     if store$ <> revalu_accts$(i%, 1) then L31190
                         accts$(1) = revalu_accts$(i%, 2)
                         goto L31192
L31190:         next i%
L31192:         if accts$(1) = " " then accts$(1) = revalu_acct$

L31200:     str(gltext$,31) = str(plowkey$, 1,34)

            if export_on$ = "Y" then gosub load_gl_info
            for i% = 1% to 12%
              if glamt(i%) = 0 then L31425
L31240:         amount = sgn(glamt(i%)) * min(9000000, abs(glamt(i%)))
                debit, credit = abs(amount)
                if amount < 0 then debit = 0 else credit = 0
                if export_on$ = "Y" then                                 ~
                     put gl_post_info$() using L31284, "ISC01", amount,   ~
                                                      hnyqty
L31284:         FMT CH(5), POS(18), 2*PD(15,4)
L31286:         call "GETDTTM" addr(datetime$)
                write #7, using L31330, userid$, datetime$, hnyacct$,     ~
                              gltext$, debit, credit, gl_post_info$(),   ~
                              eod goto L31286

L31330:         FMT CH(3), CH(7), CH(9), CH(100), 2*PD(14,4), 2*CH(255)

                if method$ <> "R" then temp$ = "ISC02" else              ~
                                                          temp$ = "ISC03"
                if export_on$ = "Y" then                                 ~
                     put gl_post_info$() using L31284, temp$, -amount,    ~
                                                      -hnyqty
L31360:         call "GETDTTM" addr(datetime$)
                write #7, using L31330, userid$, datetime$, accts$(i%),   ~
                              gltext$, credit, debit, gl_post_info$(),   ~
                              eod goto L31360

                glamt(i%) = glamt(i%) - amount
                if glamt(i%) <> 0 then L31240
L31425:     if method$ = "R" then L31460
            next i%
            return

L31460:     accts$(1) = saveacct$ : glamt(1) = saveamt
            return

        REM *************************************************************~
            * REVALUE CORE BANK                                         *~
            *************************************************************
        revalue_core_bank
            if core_bank% = 0% then return
            if export_on$ = "Y" then gosub load_gl_info
            plowkey$ = key(#3)
            str(gltext$,31) = str(plowkey$,,25%)

*        GET_CPART_VAR_ACCOUNT
            call "REDALT0" (#52, plowkey$, 1%, f1%(52%))
               if f1%(52%) = 0% then L32060
            get #52 using L32057, core_acct$
L32057:         FMT POS(170), CH(9)
            if core_acct$ <> " " then core_db_loop
L32060:        core_acct$ = str(core_swtchs$,30%,9%)

        core_db_loop
            call "PLOWAL1" (#50, plowkey$, 3%, 25%, f1%(50%))
               if f1%(50%) = 0% then core_cr_loop

L32110:     get #50 using L32120, core_key$, core_qty, temp, core_cost,   ~
                                 core_cogs$, core_cdla$, core_cdlf$,     ~
                                 core_unfl$, core_unap$, core_cflag$

L32120:         FMT POS(10), CH(20), POS(176), 2*PD(14,4), POS(200),     ~
                    PD(14,4), POS(571), 2*CH(9), 2*CH(1), POS(592),      ~
                    CH(9), POS(641), CH(1)

            core_qty = core_qty - temp
              if core_qty  = 0       then L32210
            core_cflg$ = and hex(01)
            if core_cflg$ = hex(01) then L32210
            put #50 using L32180, stccost, stccost$
L32180:         FMT POS(200), PD(14,4), CH(96)
            rewrite #50
            if core_cost = stccost then L32210
            if core_cdlf$ <> "Y" then L32196
               core_cogs$ = core_cdla$
L32196:     if core_unfl$ <> "Y" then L32200
               core_cogs$ = core_unap$
L32200:     gosub update_core_db_gl
L32210:     call "READNXT1" (#50, f1%(50%))
               if f1%(50%) = 0% then core_cr_loop
               if str(key(#50,3%),,25%) <> str(plowkey$,,25%) then       ~
                  core_cr_loop
            goto L32110

        core_cr_loop
            plowkey$ = key(#3)
            call "PLOWAL1" (#51, plowkey$, 3%, 25%, f1%(51%))
               if f1%(51%) = 0% then return
L32300:     get #51 using L32310, core_key$, core_qty, temp, core_cost,   ~
                                 core_cogs$, core_cdla$, core_cdlf$,     ~
                                 core_unfl$, core_unap$, core_cflag$
L32310:         FMT POS(10), CH(20), POS(176), 2*PD(14,4), POS(200),     ~
                    PD(14,4), POS(571), 2*CH(9), 2*CH(1), POS(592),      ~
                    CH(9), POS(641), CH(1)
            core_qty = core_qty - temp
              if core_qty  = 0       then L32410
            core_cflg$ = and hex(01)
            if core_cflg$ = hex(01) then L32410
            put #51 using L32380, stccost, stccost$
L32380:         FMT POS(200), PD(14,4), CH(96)
            rewrite #51
            if core_cost = stccost then L32410
            if core_cdlf$ <> "Y" then L32396
               core_cogs$ = core_cdla$
L32396:     if core_unfl$ <> "Y" then L32400
               core_cogs$ = core_unap$
L32400:     gosub update_core_cr_gl
L32410:     call "READNXT1" (#51, f1%(51%))
               if f1%(51%) = 0% then return
               if str(key(#51,3%),,25%) <> str(plowkey$,,25%) then return
            goto L32300

        update_core_db_gl
             core_amount = stccost - core_cost
             core_amount = core_qty * core_amount
             str(gltext$,,30%) = "Core  Debit:" & core_key$
             goto common_core_gl

        update_core_cr_gl
             core_amount = core_cost - stccost
             core_amount = core_qty * core_amount
             str(gltext$,,30%) = "Core Credit:" & core_key$
*           GOTO COMMON_CORE_GL

        common_core_gl

L32590:         amount = sgn(core_amount) *                              ~
                                    min(9000000, abs(core_amount))
                debit, credit = abs(amount)
                if amount < 0 then debit = 0 else credit = 0
                if export_on$ = "Y" then                                 ~
                     put gl_post_info$() using L32630, "ISC04", amount,   ~
                                                       core_qty
L32630:            FMT CH(5), POS(18), 2*PD(15,4)
L32640:         call "GETDTTM" addr(datetime$)
                write #7, using L32690, userid$, datetime$, core_cogs$,   ~
                              gltext$, debit, credit, gl_post_info$(),   ~
                              eod goto L32640

L32690:         FMT CH(3), CH(7), CH(9), CH(100), 2*PD(14,4), 2*CH(255)

                if export_on$ = "Y" then                                 ~
                     put gl_post_info$() using L32630, "ISC05", -amount,  ~
                                                             -core_qty
L32740:         call "GETDTTM" addr(datetime$)
                write #7, using L32690, userid$, datetime$, core_acct$,   ~
                              gltext$, credit, debit, gl_post_info$(),   ~
                              eod goto L32740

                core_amount = core_amount - amount
                if core_amount <> 0 then L32590
                str(gltext$,,30%) = " "
                return


        REM *************************************************************~
            * Check for Re-manufactured Part with Cores to be Re-valued.*~
            * If Reman Part, Write Work File with Available Data.       *~
            *************************************************************
        check_for_reman
            if reman_qty = 0 then return

            init (hex(00))  readkey$
            readkey$ = key(#3)

            call "PLOWALTS" (#52, readkey$, 0%, 25%, reman%)
                if reman% <> 1% then return        /* Not a Reman Part */
            get #52 using L33020, core_part$, cfg_acct$
L33020:         FMT POS(51), CH(25), POS(137), CH(9)
            if core_part$ = " " then return      /* No Associated Core */
            call "STCCOSTS" (core_part$, newset$, #2, 12%, stccost,      ~
                             stccost())
            write #53 using L33060, str(readkey$,1%,25%), core_part$,     ~
                      cfg_acct$, reman_qty, stccost, stccost()
L33060:         FMT 2*CH(25), CH(9), 14*PD(14,4)
            return


        REM *************************************************************~
            * Process the work file and revalue any cores that are part *~
            * of Reman items.                                           *~
            *************************************************************
        revalue_cores
            init (hex(00))  readkey$
            call "PLOWNEXT" (#53, core_wrk_key$, 0%, f1%(53%))
L33270:         if f1%(53%) = 0% then return
*        Get the Core Part, the Reman Part Core Finished Goods Account
*        and the reman quantity for the reman part
            get #53 using L33300, str(readkey$,1%,25%), core_part$,       ~
                    cfg_acct$, reman_qty, stccost, stccost()
L33300:         FMT 2*CH(25), CH(9), 14*PD(14,4)
            core_wrk_key$ = key(#53)
*        Get both the old (still current standard costs) and the new
*        standard costs for the core part.
*          CALL "STCCOSTS" (CORE_PART$, NEWSET$, #2, 12%, STCCOST,      ~
*                           STCCOST())
            call "STCCOSTS" (core_part$, " ",     #2, 12%, corecost,     ~
                             hnyqcost())
                corecost = corecost

*        Lets see if anything to post before we worry about accounts
            mat glamt = stccost - hnyqcost
            mat glamt = (reman_qty) * glamt
            temp% = 0% : glamt = 0
            for i% = 1% to 12%
                if glamt(i%) = 0 then L33460
                     temp% = temp% + 2%*(int(abs(glamt(i%))/9000000%)+1%)
                     glamt = glamt + glamt(i%)
L33460:     next i%
            if temp% = 0% then L34190

*        Ok, we have amounts to post, now need some account numbers
*        If got core finished goods account from the reman part then
*        we can go on to get other side
            if cfg_acct$ <> " " then L33670
            init (" ")  plowkey$
            plowkey$ = core_part$
            call "REDALT0" (#52, plowkey$, 1%, corepart%)
                if corepart% <> 1% then L33630
            get #52 using L33580, cfg_acct$
L33580:         FMT POS(137), CH(9)
            if cfg_acct$ <> " " then L33670

*        As a last resort take the core finished goods account from
*        the core flags
L33630:     cfg_acct$ = str(core_swtchs$, 84%, 9%)

*        Now decide if system revaluation account or HNYMASTR variances
*        accounts are to be used and get them
L33670:     if method$ = "R" then L33760
            init (" ") accts$()
            call "READ100" (#3, core_part$, f1%(3%))
                if f1%(3%) <> 1% then L33760
            get #3 using L33720, accts$()
L33720:         FMT POS(389), 12*CH(9)

*        Now update the various accounts
*        Do we do this for 12 variance accts or 1 revaluation acct
L33760:     if method$ <> "R" then L33860
                saveacct$  = accts$(1%) : saveamt  = glamt(1%)
                accts$(1%) = " "        : glamt(1%) = glamt
                accts$(1%) = revalu_acct$

L33860:     str(gltext$, 31%)  = core_part$
            str(gltext$,,30%) = "RM: " & str(readkey$,1%,25%)
            init (" ")  plowkey$  :  plowkey$ = core_part$
            if export_on$ = "Y" then gosub load_gl_info
            for i% = 1% to 12%
              if glamt(i%) = 0 then L34130
L33910:         amount = sgn(glamt(i%)) * min(9000000, abs(glamt(i%)))
                debit, credit = abs(amount)
                if amount < 0 then debit = 0 else credit = 0
                if export_on$ = "Y" then                                 ~
                     put gl_post_info$() using L33950, "ISC06", amount,   ~
                                                      reman_qty
L33950:         FMT CH(5), POS(18), 2*PD(15,4)
L33960:         call "GETDTTM" addr(datetime$)
                write #7, using L34010, userid$, datetime$, cfg_acct$,    ~
                              gltext$, debit, credit, gl_post_info$(),   ~
                              eod goto L33960

L34010:         FMT CH(3), CH(7), CH(9), CH(100), 2*PD(14,4), 2*CH(255)

                if method$ <> "R" then temp$ = "ISC07" else              ~
                                                          temp$ = "ISC08"
                if export_on$ = "Y" then                                 ~
                     put gl_post_info$() using L33950, temp$, -amount,    ~
                                                      -reman_qty

L34060:         call "GETDTTM" addr(datetime$)
                write #7, using L34010, userid$, datetime$, accts$(i%),   ~
                              gltext$, credit, debit, gl_post_info$(),   ~
                              eod goto L34060

                glamt(i%) = glamt(i%) - amount
                if glamt(i%) <> 0 then L33910
L34130:     if method$ = "R" then L34170
            next i%
            goto L34190

L34170:     accts$(1) = saveacct$ : glamt(1) = saveamt

L34190:     plowkey$ = "STCIMPLE.CONTROL"
            call "READ101" (#2, plowkey$, f1%(2))
            if f1%(2) = 0% then L34260    /* Better never happen */
                put #2 using L34230, core_wrk_key$
L34230:              FMT POS(65), CH(50)
                rewrite #2

L34260:     call "READNEXT" (#53, f1%(53))
            goto L33270

        load_gl_info

            gosub load_part_info
            put str(gl_post_info$(),,) using L37500,                      ~
                tran_type$,              /* Transaction Type CH(5)     */~
                " ",                     /* Currency code CH(4)        */~
                0,                       /* Transaction Currency amount*/~
                0,                       /* Functional Currency amount */~
                0,                       /* Unit amount                */~
                " ",                     /* Customer code CH(9)        */~
                " ",                     /* Sales Order number CH(16)  */~
                " ",                     /* BOL number CH(3)           */~
                " ",                     /* Customer Type CH(2)        */~
                " ",                     /* State CH(2)                */~
                " ",                     /* Country CH(3)              */~
                " ",                     /* ZIP CH(9)                  */~
                " ",                     /* Sales Region CH(4)         */~
                " ",                     /* Sales Tax code CH(10)      */~
                " ",                     /* Shipping Region CH(4)      */~
                " ",                     /* Salesman code CH(4)        */~
                " ",                     /* Invoice Number CH(8)       */~
                str(plowkey$,,25),       /* Part Number CH(25)         */~
                partcat$,                /* Part Category CH(4)        */~
                partclass$,              /* Part Class CH(4)           */~
                partgen$,                /* Part Generic code CH(16)   */~
                parttype$,               /* Part Type CH(3)            */~
                uom$,                    /* Part UOM CH(4)             */~
                str(plowkey$,26,3),      /* Store Number CH(3)         */~
                " ",                     /* Check Receipt Number CH(8) */~
                " ",                     /* Vendor code CH(9)          */~
                " ",                     /* Vendor type CH(4)          */~
                " ",                     /* Purchase Order CH(16)      */~
                " ",                     /* Receiver Number CH(16)     */~
                " ",                     /* Vendor Invoice CH(16)      */~
                " ",                     /* Check Payment Number CH(8) */~
                " ",                     /* Project code CH(8)         */~
                " ",                     /* Job number CH(8)           */~
                " ",                     /* Work Center CH(4)          */~
                " ",                     /* Activity code CH(4)        */~
                " ",                     /* Employee number CH(12)     */~
                " ",                     /* Department code CH(4)      */~
                " ",                     /* Cost Center CH(4)          */~
                " ",                     /* Earnings Type CH(12)       */~
                " ",                     /* Deduction Type CH(12)      */~
                " ",                     /* P/R Category CH(4)         */~
                " ",                     /* Labor class CH(4)          */~
                " "                      /* Filler                     */

            return

L37500: FMT     CH(5),                   /* Transaction Type CH(5)     */~
                CH(4),                   /* Currency code CH(4)        */~
                PD(14,4),                /* Transaction Currency amount*/~
                PD(14,4),                /* Functional Currency amount */~
                PD(14,4),                /* Unit amount                */~
                CH(9),                   /* Customer code CH(9)        */~
                CH(16),                  /* Sales Order number CH(16)  */~
                CH(3),                   /* BOL number CH(3)           */~
                CH(2),                   /* Customer Type CH(2)        */~
                CH(2),                   /* State CH(2)                */~
                CH(3),                   /* Country CH(3)              */~
                CH(9),                   /* ZIP CH(9)                  */~
                CH(4),                   /* Sales Region CH(4)         */~
                CH(10),                  /* Sales Tax code CH(10)      */~
                CH(4),                   /* Shipping Region CH(4)      */~
                CH(4),                   /* Salesman code CH(4)        */~
                CH(8),                   /* Invoice Number CH(8)       */~
                CH(25),                  /* Part Number CH(25)         */~
                CH(4),                   /* Part Category CH(4)        */~
                CH(4),                   /* Part Class CH(4)           */~
                CH(16),                  /* Part Generic code CH(16)   */~
                CH(3),                   /* Part Type CH(3)            */~
                CH(4),                   /* Part UOM CH(4)             */~
                CH(3),                   /* Store Number CH(3)         */~
                CH(8),                   /* Check Receipt Number CH(8) */~
                CH(9),                   /* Vendor code CH(9)          */~
                CH(4),                   /* Vendor type CH(4)          */~
                CH(16),                  /* Purchase Order CH(16)      */~
                CH(16),                  /* Receiver Number CH(16)     */~
                CH(16),                  /* Vendor Invoice  CH(16)     */~
                CH(8),                   /* Check Payment Number CH(8) */~
                CH(8),                   /* Project code CH(8)         */~
                CH(8),                   /* Job number CH(8)           */~
                CH(4),                   /* Work Center CH(4)          */~
                CH(4),                   /* Activity code CH(4)        */~
                CH(12),                  /* Employee number CH(12)     */~
                CH(4),                   /* Department code CH(4)      */~
                CH(4),                   /* Cost Center CH(4)          */~
                CH(12),                  /* Earnings Type CH(12)       */~
                CH(12),                  /* Deduction Type CH(12)      */~
                CH(4),                   /* P/R Category CH(4)         */~
                CH(4),                   /* Labor class CH(4)          */~
                CH(186)                  /* Filler                     */

        load_part_info

            call "READ100" (#3, str(plowkey$,,25), f1%(3))
                 if f1%(3) = 0% then return /* Shouldn't happen */
            get #3 using L38000, partgen$, uom$, partcat$, partclass$,    ~
                                parttype$
L38000:     FMT POS(58), CH(16), CH(4), POS(90), CH(4), POS(133), CH(4), ~
                POS(180), CH(3)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              str(line2$,62%) = "STCIMPLE: " & str(cms2v$,,8%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40140,         /* Cost Set          */   ~
                                L40150          /* Bucket Id         */
              goto L40180

L40140:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40150:           init(hex(81)) str(lfac$(), 2, oldbkts%) :  return

L40180:     accept                                                       ~
               at (01,02),                                               ~
                  "Implement New Standard Cost Set",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Cost Set to Implement:",                     ~
               at (04,25), fac(lfac$( 1)), newset$              , ch(08),~
               at (04,35), fac(hex(8c)),   newsetdescr$         , ch(32),~
                                                                         ~
               at (06,02), fac(hex(8c)), header$(1)             , ch(79),~
               at (07,02), fac(hex(ac)), header$(2)             , ch(79),~
                                                                         ~
               at (08,02), fac(hex(8c)), oldbktid$( 1)          , ch(10),~
               at (09,02), fac(hex(8c)), oldbktid$( 2)          , ch(10),~
               at (10,02), fac(hex(8c)), oldbktid$( 3)          , ch(10),~
               at (11,02), fac(hex(8c)), oldbktid$( 4)          , ch(10),~
               at (12,02), fac(hex(8c)), oldbktid$( 5)          , ch(10),~
               at (13,02), fac(hex(8c)), oldbktid$( 6)          , ch(10),~
               at (14,02), fac(hex(8c)), oldbktid$( 7)          , ch(10),~
               at (15,02), fac(hex(8c)), oldbktid$( 8)          , ch(10),~
               at (16,02), fac(hex(8c)), oldbktid$( 9)          , ch(10),~
               at (17,02), fac(hex(8c)), oldbktid$(10)          , ch(10),~
               at (18,02), fac(hex(8c)), oldbktid$(11)          , ch(10),~
               at (19,02), fac(hex(8c)), oldbktid$(12)          , ch(10),~
                                                                         ~
               at (08,13), fac(hex(8c)), oldbktdescr$( 1)       , ch(20),~
               at (09,13), fac(hex(8c)), oldbktdescr$( 2)       , ch(20),~
               at (10,13), fac(hex(8c)), oldbktdescr$( 3)       , ch(20),~
               at (11,13), fac(hex(8c)), oldbktdescr$( 4)       , ch(20),~
               at (12,13), fac(hex(8c)), oldbktdescr$( 5)       , ch(20),~
               at (13,13), fac(hex(8c)), oldbktdescr$( 6)       , ch(20),~
               at (14,13), fac(hex(8c)), oldbktdescr$( 7)       , ch(20),~
               at (15,13), fac(hex(8c)), oldbktdescr$( 8)       , ch(20),~
               at (16,13), fac(hex(8c)), oldbktdescr$( 9)       , ch(20),~
               at (17,13), fac(hex(8c)), oldbktdescr$(10)       , ch(20),~
               at (18,13), fac(hex(8c)), oldbktdescr$(11)       , ch(20),~
               at (19,13), fac(hex(8c)), oldbktdescr$(12)       , ch(20),~
                                                                         ~
               at (08,39), fac(lfac$( 2)), bmap$   ( 1)         , ch(02),~
               at (09,39), fac(lfac$( 3)), bmap$   ( 2)         , ch(02),~
               at (10,39), fac(lfac$( 4)), bmap$   ( 3)         , ch(02),~
               at (11,39), fac(lfac$( 5)), bmap$   ( 4)         , ch(02),~
               at (12,39), fac(lfac$( 6)), bmap$   ( 5)         , ch(02),~
               at (13,39), fac(lfac$( 7)), bmap$   ( 6)         , ch(02),~
               at (14,39), fac(lfac$( 8)), bmap$   ( 7)         , ch(02),~
               at (15,39), fac(lfac$( 9)), bmap$   ( 8)         , ch(02),~
               at (16,39), fac(lfac$(10)), bmap$   ( 9)         , ch(02),~
               at (17,39), fac(lfac$(11)), bmap$   (10)         , ch(02),~
               at (18,39), fac(lfac$(12)), bmap$   (11)         , ch(02),~
               at (19,39), fac(lfac$(13)), bmap$   (12)         , ch(02),~
                                                                         ~
               at (08,47), fac(hex(8c)), newbktnr$( 1)          , ch(02),~
               at (09,47), fac(hex(8c)), newbktnr$( 2)          , ch(02),~
               at (10,47), fac(hex(8c)), newbktnr$( 3)          , ch(02),~
               at (11,47), fac(hex(8c)), newbktnr$( 4)          , ch(02),~
               at (12,47), fac(hex(8c)), newbktnr$( 5)          , ch(02),~
               at (13,47), fac(hex(8c)), newbktnr$( 6)          , ch(02),~
               at (14,47), fac(hex(8c)), newbktnr$( 7)          , ch(02),~
               at (15,47), fac(hex(8c)), newbktnr$( 8)          , ch(02),~
               at (16,47), fac(hex(8c)), newbktnr$( 9)          , ch(02),~
               at (17,47), fac(hex(8c)), newbktnr$(10)          , ch(02),~
               at (18,47), fac(hex(8c)), newbktnr$(11)          , ch(02),~
               at (19,47), fac(hex(8c)), newbktnr$(12)          , ch(02),~
                                                                         ~
               at (08,50), fac(hex(8c)), newbktid$( 1)          , ch(10),~
               at (09,50), fac(hex(8c)), newbktid$( 2)          , ch(10),~
               at (10,50), fac(hex(8c)), newbktid$( 3)          , ch(10),~
               at (11,50), fac(hex(8c)), newbktid$( 4)          , ch(10),~
               at (12,50), fac(hex(8c)), newbktid$( 5)          , ch(10),~
               at (13,50), fac(hex(8c)), newbktid$( 6)          , ch(10),~
               at (14,50), fac(hex(8c)), newbktid$( 7)          , ch(10),~
               at (15,50), fac(hex(8c)), newbktid$( 8)          , ch(10),~
               at (16,50), fac(hex(8c)), newbktid$( 9)          , ch(10),~
               at (17,50), fac(hex(8c)), newbktid$(10)          , ch(10),~
               at (18,50), fac(hex(8c)), newbktid$(11)          , ch(10),~
               at (19,50), fac(hex(8c)), newbktid$(12)          , ch(10),~
                                                                         ~
               at (08,61), fac(hex(8c)), newbktdescr$( 1)       , ch(20),~
               at (09,61), fac(hex(8c)), newbktdescr$( 2)       , ch(20),~
               at (10,61), fac(hex(8c)), newbktdescr$( 3)       , ch(20),~
               at (11,61), fac(hex(8c)), newbktdescr$( 4)       , ch(20),~
               at (12,61), fac(hex(8c)), newbktdescr$( 5)       , ch(20),~
               at (13,61), fac(hex(8c)), newbktdescr$( 6)       , ch(20),~
               at (14,61), fac(hex(8c)), newbktdescr$( 7)       , ch(20),~
               at (15,61), fac(hex(8c)), newbktdescr$( 8)       , ch(20),~
               at (16,61), fac(hex(8c)), newbktdescr$( 9)       , ch(20),~
               at (17,61), fac(hex(8c)), newbktdescr$(10)       , ch(20),~
               at (18,61), fac(hex(8c)), newbktdescr$(11)       , ch(20),~
               at (19,61), fac(hex(8c)), newbktdescr$(12)       , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L42010
                  call "MANUAL" ("STCIMPLE")
                  goto L40180

L42010:        if keyhit% <> 15 then L42050
                  call "PRNTSCRN"
                  goto L40180

L42050:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50120,         /* Cost Set          */     ~
                              L50600          /* Bucket Id         */
            return

L50120: REM Test for Cost Set                     NEWSET$
            plowkey$     = "STC.HDR." & newset$
            newsetdescr$ = hex(06) & "Select Cost Set"
            if newset$  = " " then call "PLOWCODE" (#2, plowkey$,        ~
                                                    newsetdescr$,        ~
                                                    8%, 0.30, onfile%)   ~
                          else call "READ100"  (#2, plowkey$, onfile%)
            if onfile% = 1% then L50240
                if newset$  = " " then errormsg$ = hex(00) else          ~
                                       errormsg$ = "Cost Set Not on File."
                newsetdescr$ = " "
                return
L50240:     if newset$ = " " then newset$ = str(plowkey$,9)

            if newset$ <> oldset$ then L50300
L50261:     errormsg$ = "This can only revalue FIXED Standard Inventory."
            if core_value% = 0% then L50265
               errormsg$ = "Only FIXED Standard Inv. will be revalued."
               errormsg$ = errormsg$ & "  Core Fin. Goods will not."
L50265:     ask% = 2%
            call "ASKUSER" (ask%, "SAME COST SET WARNING",               ~
                 "The OLD cost set and the NEW Cost set are the same.",  ~
                 errormsg$,                                              ~
                 "Press PF8 to Continue, RETURN to Re-enter")
            errormsg$ = " "
            if ask% =  8% then L50300
            if ask% =  0% then L50280
               goto L50261

L50280:         newsetdescr$ = " "
                errormsg$ = "New Set cannot be the same as the Current."
                return

L50300:     get #2 using L50310, newsetdescr$, newsetid$, temp$
L50310:         FMT POS(21), CH(30), CH(4), POS(433), CH(9)
            if temp$ <> " " then L50360
               errormsg$ = "This Cost Set Has NOT Been Frozen."
               return

L50360:     get #2 using L50370, newbkts%, newbktid$(), newbktdescr$()
L50370:         FMT POS(59), BI(1), 12*CH(10), 12*CH(20)

            if newbkts% = 12% then L50420
                t% = 10% * newbkts% + 1% : str(newbktid$()   , t%) = " "
                t% = 20% * newbkts% + 1% : str(newbktdescr$(),t%)  = " "
L50420:     if newbkts% = 0% then newbkts% = 12%
            str(newbktnr$()) = " "

            for i% = 1% to newbkts%
                convert i% to newbktnr$(i%), pic(##)
            next i%

            if newbkts% < 12% then init (" ") str(bmap$(), newbkts% + 1%)

            call "STCFOPEN" (newset$, "RRRRRR", #2, errormsg$,           ~
                                            #10, #11, #12, #13, #14, #15)
            if errormsg$ = " " then L50570
               errormsg$ = "Cost Set Unavailable. (Possibly Archived)."
               return

L50570:     if str(bmap$()) = " " then return
               fieldnr% = 2%

L50600: REM Test for Bucket Id
            mat bmap% = zer
            for i% = 1% to oldbkts%
                convert bmap$(i%) to bmap%(i%), data goto L50680
                if bmap%(i%) <= 0% or bmap%(i%) > newbkts% then L50680
                convert bmap%(i%) to bmap$(i%), pic(#0)
            next i%
            return
L50680:         errormsg$ = "Invalid entry for " & oldbktid$(i%)
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

            if core_value% = 1% then call "FILEBGON" (#53)

            if str(restart_flags$,3%,1%) = "C" then restart_resume_last
            plowkey$ = "STC.CONTROL"
            call "READ101" (#2, plowkey$, f1%(2))
            if f1%(2) = 0% then L65235    /* Better never happen */
                get #2, using L65105, stccurr$, stcint$, stcdate$
L65105:              FMT POS(28), CH(192), POS(228), CH(96),             ~
                                                        POS(328), CH(144)
                if str(stccurr$,,8%) <> str(newset$,,8%) then L65115
                if rdateu$ <> str(stcdate$,,6%) then L65115
                   goto restart_resume_last  /* Did It Once All Ready? */

L65115:         put #2, using L65125, userid$, newset$, stccurr$,         ~
                                     newsetid$, stcint$, date, stcdate$
L65125:              FMT POS(25), CH(3), CH(8), CH(192), CH(4), CH(96),  ~
                         CH(6), CH(144)
                rewrite #2

        restart_resume_last
            plowkey$ = "STCIMPLE.CONTROL"
            call "READ101" (#2, plowkey$, f1%(2))
            if f1%(2) = 0% then L65191    /* Better never happen */
                put #2 using L65165, "C"
L65165:              FMT POS(44), CH(1)
                get #2 using L65175, restart_flags$
L65175:              FMT POS(42), CH(3)
                str(restart_flags$,3%,1%) = "C"
                if remap% =  0%  then restart_flags$ = "CCC"
                if remap% >  2%  then restart_flags$ = "CCC"
                if restart_flags$ = "CCC" then delete #2 else rewrite #2
L65191:         call "GETUFBS1" addr(#7, f1%(7))
                if f1%(7) <> 0% then close #7
                if remap% = 0% or expandcogs% = 0% then L65220
                   ask% = 2%
                   call "ASKUSER"(ask%, " ** WARNING **",                ~
                        "Remapping of Cost Buckets May Cause a Conflict",~
                        "With the Expanded COGS Acct File 'GLCOGSXR'",   ~
                        " Press Any PF Key to Acknowledge and Exit ")
L65220:         end 1%

L65235: exit_program_no_impl
            end 0%
