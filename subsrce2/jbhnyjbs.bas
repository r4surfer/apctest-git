        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB   H   H  N   N  Y   Y  JJJJJ  BBBB    SSS    *~
            *    J    B   B  H   H  NN  N  Y   Y    J    B   B  S       *~
            *    J    BBBB   HHHHH  N N N   YYY     J    BBBB    SSS    *~
            *  J J    B   B  H   H  N  NN    Y    J J    B   B      S   *~
            *   J     BBBB   H   H  N   N    Y     J     BBBB    SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBHNYJBS - Allows returning of parts previously reported  *~
            *            completed to a Job.                            *~
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
            * 07/14/87 ! Original                                 ! ERN *~
            * 03/28/89 ! PRR 10776; Fixed bug editing lines when  ! RJM *~
            *          !    more than 1 screen of line items.     !     *~
            *          !    Now prevents fractional qty's on      !     *~
            *          !    Serial Numbered Parts.                !     *~
            * 02/22/90 ! Added PF8 access to HNYLCSUB permitting  ! MLJ *~
            *          !  location control - also added STORNAME, !     *~
            *          !  HNYLOCNS and LOCATION files.            !     *~
            * 03/09/90 ! Changed LOCATION from 200 to 400.        ! MLJ *~
            * 09/04/90 ! G/L Export file modifications.           ! RAC *~
            * 04/14/92 ! PRR 12108. Flag Job as In-use.           ! JDH *~
            * 10/05/92 ! Ensure that in-use is releived when done.! JDH *~
            * 04/05/95 ! PRR 13383. Now uses HNYAVAIL and honors  ! JDH *~
            *          !  'No Negative Lots' flag.                !     *~
            * 08/04/95 ! PRR 13482. Purchase Jobs will no longer  ! JBK *~
            *          !  be allowed to load.  Moved call to      !     *~
            *          !  JNLINFO to delay opening batch until    !     *~
            *          !  Needed.  Conditioned call to JBJNLCLS.  !     *~
            * 07/16/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "JBHNYJBS" (jobin$,               /* Passed Job Number     */~
                        mod$, jnl$,           /* Module, Journal       */~
                        #2 ,                  /* SYSFILE2              */~
                        #3 ,                  /* HNYMASTR              */~
                        #4 ,                  /* HNYQUAN               */~
                        #5 ,                  /* JBMASTR2              */~
                        #6 ,                  /* JBCREDIT              */~
                        #7 ,                  /* USERINFO              */~
                        #10,                  /* SERTIF                */~
                        #11,                  /* SERMASTR              */~
                        #12 )                 /* SERWORK               */

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            closed$8,                    /* Job Closed on Date         */~
            cursor%(2),                  /* Cursor location for edit   */~
            dfac$(20)1,                  /* Display FACs               */~
            dsply$(100)65,               /* Screen Data Display        */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            hdrs$(8)10,                  /* Column Headers             */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            jnl$3, jnl_name$40,          /* G/L Journal ID             */~
            job$8, job_descr$32,         /* Job Number                 */~
            jobin$8,                     /* Passed Job Number          */~
            jobto$8,                     /* Job completion sent to     */~
            left(100),                   /* Qty remaining complete     */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            lot$6,                       /* Lot Number                 */~
            mod$2,                       /* G/L Module                 */~
            part$25, part_descr$34,      /* Part to Build              */~
            pf$(3)79, pfkeys$20,         /* PF Descriptors and Keys    */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            postdate$8, postdatu$6,      /* Posting Date (SFC)         */~
            pj_data$28,                  /* Purchased Job Data         */~
            qty$(100)10,                 /* Qty to be returned         */~
            revkey$(100)22,              /* JBCREDIT key to reverse    */~
            sn_loc$30,                   /* Serial Number Location     */~
            sn_trankey$40,               /* Serial Number Transaction  */~
            str$3,                       /* Store Number               */~
            summary$1,                   /* Posting Summary Option     */~
            userid$3,                    /* Current User Id            */~
            work$56, work1$40            /* JB2TIF arguments           */

        dim f1%(32),                     /* = 1 if READ was successful */~
            f2%(32),                     /* = 0 if file is open        */~
            rslt$(32)20,                 /* Return code from 'OPENFILE'*/~
            axd$(32)4                    /* AXD pointer from 'OPENFILE'*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #2  ! SYSFILE2 ! System Control File                      *~
            * #3  ! HNYMASTR ! Inventory Master File                    *~
            * #4  ! HNYQUAN  ! Inventory Quantity File                  *~
            * #5  ! JBMASTR2 ! Job Master File                          *~
            * #6  ! JBCREDIT ! Job Completion Detail File               *~
            * #7  ! USERINFO ! User's Posting Dates                     *~
            * #10 ! SERTIF   ! Additions buffer for Inventory S/N's     *~
            * #11 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #12 ! SERWORK  ! Temporary Serial Numbers Work File       *~
            * #14 ! STORNAME ! Warehouse File                           *~
            * #15 ! HNYLOCNS ! Location Quantity Detail File            *~
            * #16 ! LOCATION ! Location Master File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #14, "STORNAME",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =   1,  keylen =   3

            select #15, "HNYLOCNS",                                      ~
                         varc,  indexed,  recsize =  700,                ~
                         keypos =  1,  keylen =  42,                     ~
                         alt key 1, keypos =  443, keylen =  42,         ~
                             key 2, keypos =  485, keylen =  42,         ~
                             key 3, keypos =  527, keylen =  42,         ~
                             key 4, keypos =  590, keylen =  42

            select #16, "LOCATION",                                      ~
                         varc,  indexed,  recsize =  400,                ~
                         keypos =  1,  keylen =  11,                     ~
                         alt key 1, keypos =    4, keylen =  11

            call "OPENFILE" (#14, "SHARE", f2%(14), rslt$(14), axd$(14))
            call "OPENFILE" (#15, "SHARE", f2%(15), rslt$(15), axd$(15))
            call "OPENFILE" (#16, "SHARE", f2%(16), rslt$(16), axd$(16))

            if f2%(14) = 0% then L02450
               call "OPENFILE" (#14,"OUTPT",f2%(14),rslt$(14),axd$(14))
               close #14
               call "OPENFILE" (#14,"SHARE",f2%(14),rslt$(14),axd$(14))
L02450:     if f2%(15) = 0% then L02490
               call "OPENFILE" (#15,"OUTPT",f2%(15),rslt$(15),axd$(15))
               close #15
               call "OPENFILE" (#15,"SHARE",f2%(15),rslt$(15),axd$(15))
L02490:     if f2%(16) = 0% then L09000
               call "OPENFILE" (#16,"OUTPT",f2%(16),rslt$(16),axd$(16))
               close #16
               call "OPENFILE" (#16,"SHARE",f2%(16),rslt$(16),axd$(16))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            str(line2$,62) = "JBHNYJBS: " & str(cms2v$,,8)
            edtmessage$  = "To modify Return Quantity, Position Cursor" &~
                           " to Completion Line & Press RETURN."
            job$ = jobin$

            call "READ100" (#7, userid$, f1%(7))   /* USERINFO */
            if f1%(7) = 0% then L09170 else get #7 using L09140, postdatu$
L09140:         FMT XX(33), CH(6)        /* SFC Date */
            call "WHICHMON" (#2, postdatu$, u3%)
            if u3% > 0% then L09220
L09170:         call "ASKUSER" (2%, "INVALID POSTING DATE",              ~
                          "Your Post Date for Shop Floor is invalid",    ~
                          "or not on file.  Please correct.",            ~
                          "Press RETURN to return to menu...")
                goto exit_program
L09220:     postdate$ = postdatu$ : call "DATEFMT" (postdate$)

            hdrs$(1) = "Store"
            hdrs$(2) = "Lot Nr"
            hdrs$(3) = " Posted "
            hdrs$(4) = "Total Cost"
            hdrs$(5) = " Orig. Qty"
            hdrs$(6) = "Prev Retns"
            hdrs$(7) = " Remaining"
            hdrs$(8) = "RETURN QTY"

            postflag% = 0%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub L29000

            for fieldnr% = 1% to 1%     /* Get the Job to be modified */
                gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                     if enabled% =  0% then L10240
L10140:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                     if keyhit%  =  1% then gosub startover
                     if keyhit%  = 16% then exit_program
                     if keyhit% <>  0% then L10140
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then L10140
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edit_mode
            inpmessage$ = edtmessage$
            pf$(1) = "(1)Start Over        (4)Prev            " &        ~
                     " (7)Up                 (13)Instructions"
            pf$(2) = "(2)First             (5)Next            " &        ~
                     " (8)Locations          (15)Print Screen"
            pf$(3) = "(3)Last              (6)Down            " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(0102030405060708ffffffff0dff0f1000)
            if first% > 1% then L11190
                str(pf$(1),22,7), str(pf$(2),1,8), str(pf$(3),22,7) = " "
                str(pfkeys$,2,1), str(pfkeys$,4,1), str(pfkeys$,6,1) =   ~
                                                                  hex(ff)
L11190:     if first% + 12% >= maxlines% then L11200 else L11240
L11200:         str(pf$(1),42,5), str(pf$(2),22,7), str(pf$(3),1,7) = " "
                str(pfkeys$,3,1), str(pfkeys$,5,1), str(pfkeys$,7,1) =   ~
                                                                  hex(ff)

L11240:     lastline% = 0%
            gosub'102(0%)               /* Display Screen - No Entry   */
                errormsg$   = " "
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  2% then first% = 1%
                if keyhit%  =  3% then first% = 9999%
                if keyhit%  =  4% then first% = first% - 12%
                if keyhit%  =  5% then first% = first% + 12%
                if keyhit%  =  6% then first% = first% -  1%
                if keyhit%  =  7% then first% = first% +  1%
                                       first% = min(first%, maxlines%-12%)
                                       first% = max(1%, first%)
                if keyhit%  = 16% then       datasave
                if keyhit% <>  0% then       edit_mode

L11390:     line% = cursor%(1) - 6%
            if line% < 1% or line% > 13% then edit_mode
            if line% = lastline%         then edit_mode
*          C% = LINE% - FIRST% + 1%
            c% = line% + first% - 1%
            if c% < 1% or c% > maxlines% then edit_mode

            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            gosub'052(c%)            /* Check Enables, Set Defaults */
                if enabled%  = 0% then edit_mode
L11540:     gosub'102(line%)         /* Display & Accept Screen     */
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then L11540
            gosub'152(c%)            /* Edit Field for Valid Entry  */
                if errormsg$ <> " " then L11540
                lastline% = line%
                goto L11390

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub data_save
            if jobin$ = " " then inputmode else exit_program

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20100          /* Job Number         */
            return

L20100
*        Def/Enable Job Number                  JOB$
            inpmessage$ = "Enter Job Number."
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(c%)
            inpmessage$ = " "
            enabled% = 1%

*        Def/Enable QUANTITY                    QTY$(C%)
            pending, avail = 0
            call "HNYAVAIL" (#3,#4, part$, str(dsply$(c%),2,3) /*Store*/,~
                             str(dsply$(c%),7,6) /* Lot */, errormsg$,   ~
                             9e9 /* Test for No Neg Lots */, avail, r%)
                             r% = r% /* Compiler thing */
            if errormsg$ = " " then L21400
            errormsg$ = " "  /* Shut off errmsg; rely on inpmsg below */
            for c1% = 1% to maxlines%
                if c1% = c% or str(dsply$(c1%),2,11) <>                  ~
                               str(dsply$(c% ),2,11) then L21250
                     temp = 0
                     convert qty$(c1%) to temp, data goto L21240
L21240:              pending = pending + temp
L21250:         next c1%
            avail = avail - pending
L21270:     avail = min(left(c%), avail)
            if avail > 0 then L21320
                errormsg$ = "There is no available inventory to move."
                enabled%  = 0%
                return
L21320:     call "STRING" addr("LJ", qty$(c%), 10%)
            call "CONVERT" (avail, -0.2, str(inpmessage$,,10))
            inpmessage$ = "Enter quantity to be returned.  Maximum" &    ~
                          " available = " & inpmessage$ & "."
            return

L21400
*        This is not a lot protected part, so the sky's the limit!
            avail = 9e9
            goto L21270

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, job_descr$, part$,         ~
                      part_descr$, dsply$(), revkey$(), qty$(), pj_data$
            first% = 1%
            call "ALLFREE"
            call "JBINUSE" (" ", 1%)  /* Clears In-use Flag for User */
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
                call "SERSTOVR" (0%, "1", "2", #11, #12)
                goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        load_data

*        First, get Job Data and make sure that Job is Open.
            get #5 using L30090, part$, pj_data$, closed$
L30090:         FMT POS(58), CH(25), POS(108), CH(28), POS(153), CH(6)
            if closed$ = " " or closed$ = blankdate$ then L30135
                call "DATEFMT" (closed$)
                errormsg$ = "Job was Closed on " & closed$
                return
L30135:     if pj_data$ = " " then L30145
                errormsg$ = "Job is a Purchased Job and CANNOT be "     &~
                            "Accessed Here."
                return
L30145:     call "GETCODE"  (#3, part$, part_descr$, 1%, 99, f1%(3))
            call "SERENABL" (part$, sn_enable%, u3%, #2, #3)

*        Next load up the Credits (completion Ledger).
            c%, maxlines% = 0%
            plowkey$ = str(job$) & all(hex(00))

          load_loop
            call "PLOWNEXT" (#6, plowkey$, 8%, f1%(6))
            if f1%(6) = 0% then end_load
                get #6 using L30250, orig, cost, retd, jobto$
L30250:              FMT POS(71), 2*PD(14,4), POS(375), PD(14,4), CH(8)
                left = orig - retd
                if left   <= 0   then load_loop
                if jobto$ <> " " then load_loop
                     c%, maxlines% = c% + 1%
                     get #6 using L30350,                                 ~
                          revkey$(c%),             /* Record Key       */~
                          str(dsply$(c%), 2, 3),   /* Store Number     */~
                          str(dsply$(c%), 7, 8),   /* Lot Number       */~
                          str(dsply$(c%),14, 6)    /* Post Date        */
L30350:                        FMT CH(22), POS(48), CH(3), 2*CH(6)
                     call "CONVERT" (cost, 4.4, str(dsply$(c%),23,10))
                     call "CONVERT" (orig, 2.2, str(dsply$(c%),34,10))
                     call "CONVERT" (retd, 2.2, str(dsply$(c%),45,10))
                     call "CONVERT" (left, 2.2, str(dsply$(c%),56,10))
                     call "DATEFMT" (str(dsply$(c%),14,8))
                     left(c%) = left
                     goto load_loop

          end_load
            if maxlines% > 0% then return
                errormsg$ = "Nothing available to return to this Job."
                return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        data_save
            call "SHOSTAT" ("Updating Transaction Image File.")

        for c% = 1% to maxlines%
            convert qty$(c%) to qty, data goto L31490
            if qty <= 0 then L31490

            str$ = str(dsply$(c%),2,3)
            lot$ = str(dsply$(c%),7,6)

            call "HNYHOLD" (#4, part$, str$, lot$, qty, u3%)

            sn_trankey$ = job$
            convert c% to str(sn_trankey$,14,4), pic(0000)
            call "SERSAVE" (c%,              /* Line Item Pointer.     */~
                            "JR",            /* Source Trans Type      */~
                            sn_trankey$,     /* Source Trans Key       */~
                            5%,              /* # Trans                */~
                            part$,           /* Part Code              */~
                            userid$,         /* Current User ID        */~
                            "1", "2",        /* Change Status to, from */~
                            0%,              /* Save to TIF,           */~
                            #2 , #10,        /* SYSFILE2, SERTIF       */~
                            #11, #12)        /* SERMASTR, SERWORK      */

            work1$ = "P"  /* Prorated cost calculation */
            str(work$,9,22) = revkey$(c%)
            if postflag% <> 0% then L31320
                u3% = 0% : f2% = f2%
                call "JNLINFO" (mod$, jnl$, postseq%, summary$,          ~
                                      jnl_name$, postdatu$, #2, f2%, u3%)
                postflag% = 1%

L31320:     call "JB2TIF" ("J2",       /* Send transaction to JBPOST2  */~
                           1%,         /* Wake up task flag            */~
                           0%,         /* Not used if Wake flag = 0    */~
                           1%,         /* Transaction type (1=RPT CMP) */~
                           hex(20),    /* Priority (within 'J2' only)  */~
                           job$,       /* Job number effected          */~
                           mod$,       /* G/L module to post           */~
                           jnl$,       /* G/L journal to post          */~
                           postseq%,   /* G/L posting sequence         */~
                           userid$,    /* Who                          */~
                           postdatu$,  /* Posting Date                 */~
                           part$,      /* Inventory Part Code          */~
                           str$, lot$, /* Store & Lot                  */~
                           -qty,       /* Quantity to process          */~
                           work$,      /* Special Area 1               */~
                           work1$,     /* Special Area 2               */~
                           " ")        /* Costs (not used here)        */
L31490: next c%
        return

        REM *************************************************************~
            *   A C C E S S    L O C A T I O N    M A N A G E M E N T   *~
            *___________________________________________________________*~
            * Call HNYLCSUB and pass the Part, Store, Lot and Quantity  *~
            * so that the user does not have to re-enter them.          *~
            *************************************************************

         locations
            if str(dsply$(c%),56,10) <> " " then L35120
               qty = 0
               goto L35140

L35120:     convert str(dsply$(c%),56,10) to qty

L35140:     call "HNYLCSUB"  (part$,                                     ~
                              str(dsply$(c%),2,3),                       ~
                              str(dsply$(c%),7,6),                       ~
                              qty,                                       ~
                              4%,    /*  Withdrawl Mode                */~
                              #2,    /*  SYSFILE2                      */~
                              #14,   /*  STORNAME                      */~
                              #7,    /*  USERINFO                      */~
                              #3,    /*  HNYMASTR                      */~
                              #15,   /*  HNYLOCNS                      */~
                              #4,    /*  HNYQUAN                       */~
                              #16)   /*  LOCATION                      */
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40140          /* Job Number        */
              goto L40170

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40140:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40170:     accept                                                       ~
               at (01,02),                                               ~
                  "Return Completed Parts to Job",                       ~
               at (01,62), "Post Date:",                                 ~
               at (01,73), fac(hex(8c)), postdate$              , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Job Number",                                 ~
               at (06,20), fac(lfac$( 1)), job$                 , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                     keys(hex(00010d0f10)), key (keyhit%)

            if keyhit% <> 13 then L40440
                call "MANUAL" ("JBHNYJBS")
                goto L40170

L40440:     if keyhit% <> 15 then return
                call "PRNTSCRN"
                goto L40170

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(line%)
          if line% > 0% then L41120
                init(hex(84)) lfac$()                  /* Display Mode */
                init(hex(86)) dfac$()
                goto L41160

L41120:         init(hex(8c)) lfac$(), dfac$()         /* Edit Mode    */
                dfac$(line%) = hex(84)
                lfac$(line%) = hex(82)

L41160:     accept                                                       ~
               at (01,02),                                               ~
                  "Return Completed Parts to Job",                       ~
               at (01,62), "Post Date:",                                 ~
               at (01,73), fac(hex(8c)), postdate$              , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), "Job Number:",                                ~
               at (03,14), fac(hex(84)), job$                   , ch(08),~
               at (03,42), fac(hex(84)), job_descr$             , ch(32),~
               at (04,02), "  To Build:",                                ~
               at (04,14), fac(hex(84)), part$                  , ch(25),~
               at (04,42), fac(hex(84)), part_descr$            , ch(34),~
               at (05,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(ac)),   hdrs$(1)             , ch(05),~
               at (06,08), fac(hex(ac)),   hdrs$(2)             , ch(06),~
               at (06,15), fac(hex(ac)),   hdrs$(3)             , ch(08),~
               at (06,24), fac(hex(ac)),   hdrs$(4)             , ch(10),~
               at (06,35), fac(hex(ac)),   hdrs$(5)             , ch(10),~
               at (06,46), fac(hex(ac)),   hdrs$(6)             , ch(10),~
               at (06,57), fac(hex(ac)),   hdrs$(7)             , ch(10),~
               at (06,70), fac(hex(ac)),   hdrs$(8)             , ch(10),~
                                                                         ~
               at (07,02), fac(dfac$( 1)), dsply$(first% +  0%) , ch(65),~
               at (08,02), fac(dfac$( 2)), dsply$(first% +  1%) , ch(65),~
               at (09,02), fac(dfac$( 3)), dsply$(first% +  2%) , ch(65),~
               at (10,02), fac(dfac$( 4)), dsply$(first% +  3%) , ch(65),~
               at (11,02), fac(dfac$( 5)), dsply$(first% +  4%) , ch(65),~
               at (12,02), fac(dfac$( 6)), dsply$(first% +  5%) , ch(65),~
               at (13,02), fac(dfac$( 7)), dsply$(first% +  6%) , ch(65),~
               at (14,02), fac(dfac$( 8)), dsply$(first% +  7%) , ch(65),~
               at (15,02), fac(dfac$( 9)), dsply$(first% +  8%) , ch(65),~
               at (16,02), fac(dfac$(10)), dsply$(first% +  9%) , ch(65),~
               at (17,02), fac(dfac$(11)), dsply$(first% + 10%) , ch(65),~
               at (18,02), fac(dfac$(12)), dsply$(first% + 11%) , ch(65),~
               at (19,02), fac(dfac$(13)), dsply$(first% + 12%) , ch(65),~
                                                                         ~
               at (07,70), fac(lfac$( 1)), qty$  (first% +  0%) , ch(10),~
               at (08,70), fac(lfac$( 2)), qty$  (first% +  1%) , ch(10),~
               at (09,70), fac(lfac$( 3)), qty$  (first% +  2%) , ch(10),~
               at (10,70), fac(lfac$( 4)), qty$  (first% +  3%) , ch(10),~
               at (11,70), fac(lfac$( 5)), qty$  (first% +  4%) , ch(10),~
               at (12,70), fac(lfac$( 6)), qty$  (first% +  5%) , ch(10),~
               at (13,70), fac(lfac$( 7)), qty$  (first% +  6%) , ch(10),~
               at (14,70), fac(lfac$( 8)), qty$  (first% +  7%) , ch(10),~
               at (15,70), fac(lfac$( 9)), qty$  (first% +  8%) , ch(10),~
               at (16,70), fac(lfac$(10)), qty$  (first% +  9%) , ch(10),~
               at (17,70), fac(lfac$(11)), qty$  (first% + 10%) , ch(10),~
               at (18,70), fac(lfac$(12)), qty$  (first% + 11%) , ch(10),~
               at (19,70), fac(lfac$(13)), qty$  (first% + 12%) , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkeys$),  key(keyhit%)

            if keyhit% <>  8% then L41730
                gosub locations
                goto L41160

L41730:     if keyhit% <> 13% then L41770
                call "MANUAL" ("JBHNYJBS")
                goto L41160

L41770:     if keyhit% <> 15% then L41810
                call "PRNTSCRN"
                goto L41160

L41810:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50110          /* Job Number        */
            return

L50110
*        Test for Job Number                   JOB$
            job_descr$ = hex(06) & "Select Job Number"
            call "GETCODE" (#5, job$, job_descr$, 1%, 0, f1%(5))
            if f1%(5) = 1% then L50160
                errormsg$ = hex(00) : return
L50160:     u3% = 2%   /* Check In-use & Write Flag if Free */
            call "JBINUSE" (job$, u3%)
            if u3% = 0% then L50210
                if jobin$ <> " " then exit_program
                errormsg$ = hex(00)
                return
L50210:     gosub load_data
            if errormsg$ = " " then L50230
                call "JBINUSE" (job$, 1%)  /* Clear job in-use */
                return
L50230:     return clear all
            goto edit_mode
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(c%)
            errormsg$ = " "

*        Test for Quantity                     QTY$(C%)
            if qty$(c%) = " " then qty$(c%) = "0"
            convert qty$(c%) to qty, data goto L51120
L51120:     if qty <> 0 then L51160
                qty$(c%) = " "
                call "SERSTOVR" (c%, "1", "2", #11, #12)
                call "CONVERT" (avail, 2.2, str(dsply$(c%),56,10))
                return
L51160:     if qty  > 0 then L51190
                errormsg$ = "Quantity cannot be negative."
                return
L51190:     if qty <= avail then L51222
                errormsg$ = "Quantity cannot exceed Maximum available" & ~
                            " (see below)."
                return
L51222:     if not ( sn_enable% <> 0% and qty <> int(qty) ) then L51230
                errormsg$ = "Quantity cannot be fractional on Serial" &  ~
                            " numbered Parts."
                return
L51230:     call "CONVERT" (qty, 2.2, qty$(c%))
            call "CONVERT" ((avail - qty), 2.2, str(dsply$(c%),56,10))

*        Now handle serial number CRAP if so required.
            if sn_enable% = 0% then return
                sn_loc$     = str(dsply$(c%),2,3) & str(dsply$(c%),7,6)
                sn_trankey$ = job$
                call "SERSELCT" (part$, sn_loc$, qty, c%, 5%, "JR",      ~
                                 sn_trankey$, "1", "2", errormsg$,       ~
                                 #2, #4, #11, #12)
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
            if postflag% <> 0% then                                      ~
                call "JBJNLCLS" ("J2", userid$, mod$, jnl$, postseq%, 0%)
            call "SHOSTAT" ("One Moment Please")

            end
