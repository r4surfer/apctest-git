        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  W   W   CCC   DDDD   PPPP   U   U  RRRR    GGG   EEEEE   *~
            *  W   W  C   C  D   D  P   P  U   U  R   R  G      E       *~
            *  W W W  C      D   D  PPPP   U   U  RRRR   G      EEEE    *~
            *  WW WW  C   C  D   D  P      U   U  R   R  G GGG  E       *~
            *  W   W   CCC   DDDD   P       UUU   R   R   GGG   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * WCDPURGE - Subroutine purges Work Center capacity details *~
            *            based on the job number passed, and the data   *~
            *            stored in the Work Center Actuals file,        *~
            *            JBSTATUS.                                      *~
            *                                                           *~
            * Flagging Logic involves 2 Variables,                      *~
            *            STATUS%     &     CLR_SETUP%                   *~
            * Below are the 4 capacity purging states;                  *~
            *   STATUS%  CLR_SETUP%   ACTION                            *~
            *   -------  ----------   --------------------------------  *~
            *     1          0        Total Qty moved into step;        *~
            *                           Purge All capacity of all       *~
            *                           previous steps.                 *~
            *     1          1        Some of Total Qty moved into step;*~
            *                           Purge Setup capacity of all     *~
            *                           previous steps.                 *~
            *     2          0        Total Qty moved out of step;      *~
            *                           Purge All capacity of all       *~
            *                           previous steps.                 *~
            *     2          1        Some of Total Qty moved out of    *~
            *                           step;                           *~
            *                           Purge Setup capacity of all     *~
            *                           previous steps.                 *~
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
            * 11/02/87 ! Original                                 ! HES *~
            * 06/24/88 ! Clears Setup Hours & Back Purges skiped  ! RJM *~
            *          !   steps.                                 !     *~
            * 06/27/88 ! Created new flag field in JBSTATUS that  ! RJM *~
            *          !   is used and maintained exclusively in  !     *~
            *          !   this sub.  @ pos 157, it indicates if  !     *~
            *          !   capacity for the step in question has  !     *~
            *          !   already been purged.                   !     *~
            * 10/04/88 ! Modified to speed up I/O and loops       ! RJM *~
            * 04/30/92 ! Clean Up SOME of the less than elegant   ! KAB *~
            *          !   coding practices from another era.     !     *~
            *          !   Helps out Unix Project.                !     *~
            * 06/05/96 ! PRR 13522 - StepComplete Flag in JBSTATUS! RJH *~
            *          !   will force removal of WCOUTs even if   !     *~
            *          !   quantity moved is < total expected.    !     *~
            * 09/05/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

           sub "WCDPURGE" (job$, #1, #2, #3, #4)

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            date$8,                      /* Date for screen display    */~
            date_closed$6,               /* Date Job Closed            */~
            defwc$4,                                                     ~
            fromstep$7,                  /* Step Moved From            */~
            inwc$4,                      /* In Workcenter              */~
            instep$7,                    /*                            */~
            job$8,                       /* Job number to process      */~
            plowkey$50,                  /* Miscellaneous Read/Plow Key*/~
            plowkey2$50,                 /* Miscellaneous Read/Plow Key*/~
            purg_flag$1,                 /* Capacity Purged Flag       */~
            readkey$50,                  /* Miscellaneous Read/Plow Key*/~
            record$(4)245,               /* WCMASTER 490*BI(2) ARRAY   */~
            savewc$4,                    /* Workcenter Code            */~
            seq2$6,                                                      ~
            seqr$6,                      /* WCOUT Sequence Number      */~
            saveseq$6,                                                   ~
            savestep$7,                                                  ~
            step$7,                      /* Job Step Number            */~
            step_cmplt$1,                /* JBSTATUS Step Complete Flag*/~
            wcoutkey$23                  /* Workcenter Details Key     */

        dim f1%(64)                      /* = 1 if READ was successful */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/

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
            * # 1 ! WCOUT    ! Planned work center use detail           *~
            * # 2 ! WCMASTR  ! Workcenter Master File                   *~
            * # 3 ! JBSTATUS ! Production job actual structure (RTE) ac *~
            * # 4 ! JBMASTR2 ! Production job master file               *~
            * # 8 ! WORKAREA ! Temporary Work File                      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #8, "WORKAREA",                                       ~
                        varc,     indexed,  recsize =   48,              ~
                        keypos =    1, keylen =  14

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            f2_8% = 1%
            date$ = date

        REM *************************************************************~
            *  B U I L D   T H E   W O R K F I L E   R E C O R D S      *~
            *-----------------------------------------------------------*~
            * March through WCOUT file, assembling the WCOUTs           *~
            * into a work file keyed by step number.  This allows       *~
            * quick purging of all WCOUTS associated with a step.       *~
            *************************************************************

            REM Get job data to start things out...
            call "READ100" (#4, job$, f1%(4))
              if f1%(4) = 0% then bye_bye  /* Shouldn't happen */
            get #4, using L10120, quantity, date_closed$
L10120:     FMT  POS(83), PD(14,4), POS(153), CH(6)

            plowkey$ = "JOB ORDER: " & str(job$)
            str(plowkey$,20) = all(hex(00))
            cnt%, just_in% = 0%

            if date_closed$ = " " or ~
               date_closed$ = blankdate$ then build_work_file  /* Closed?! */
                call "DELETE" (#1, plowkey$, 19%)       /* Yes */
                goto bye_bye

        build_work_file
            call "PLOWALTS" (#1, plowkey$, 0%, 19%, f1%(1))
                if f1%(1) = 0% then L20000
            get #1, using L10260, savewc$, seq%, step$, ph%
L10260:     FMT CH(4), XX(2), BI(2), POS(40), CH(4), BI(1)
            cnt% = cnt% + 1%
            seq% = seq% / 100%
            convert seq% to seqr$, pic(######)

            if ph% < 1% then L10320
               step$ = step$ & "-"
               convert ph% to str(step$,pos(step$="-")+1), pic(00)

L10320:     if f2_8% <> 0% then call "WORKOPEN" (#8, "IO   ", 500%, f2_8%)
L10330:     write #8, using L10340, seqr$, time, step$, savewc$,          ~
                                   key(#1), eod goto L10330
L10340:     FMT CH(6), CH(8), CH(7), CH(4), CH(23)
            goto build_work_file

L20000: REM *************************************************************~
            *     R E A D   O L D   J B S T A T U S   R E C O R D S     *~
            *-----------------------------------------------------------*~
            * Read each step, check to see if capacity has already been *~
            * purged, if so then accumulated scrap & rework for step.   *~
            *************************************************************

            if cnt% = 0% then bye_bye    /* No WCOUT's left */

            plowkey$ = all(hex(00))
            str(plowkey$,,8) = str(job$)
            tot_thrown = 0

L20105:     call "PLOWALTS" (#3, plowkey$, 0%, 8%, f1%(3))
                if f1%(3) = 0% then L30000
            get #3, using L20130, iqtys, iqtyr, purg_flag$
L20130:         FMT POS(107), 2*PD(14,4), POS(157), CH(1)
            if purg_flag$ <> "P" then L20105
            tot_thrown = tot_thrown + iqtys + iqtyr
            goto L20105

L30000: REM *************************************************************~
            *             R E A D   T H E   W O R K F I L E             *~
            *-----------------------------------------------------------*~
            * Read each step, check to see if capacity details can      *~
            * now be purged...                                          *~
            *************************************************************

            plowkey$ = all(hex(00))

        next_work_record
            call "PLOWALTS" (#8, plowkey$, 0%, 0%, f1%(8))
                if f1%(8) = 0% then check_done
            get #8, using L30130, seqr$, step$, defwc$, wcoutkey$
L30130:         FMT CH(6), XX(8), CH(7), CH(4), CH(23)

            gosub check_deletability
            str(plowkey$,7) = all(hex(ff))
            goto next_work_record

        check_done
            seqr$ = "ZZ"
            step$ = "DONE"
            defwc$ = "xxxx"   /* Don't need to read OUT's from DONE */
            gosub check_deletability
            goto bye_bye


        check_deletability
            inwc$, instep$  = " "
            itot, otot = 0
            clr_setup%, status% = 0%

*        Set to avoid always reading movments from blank W/C (Picking)
*         records in out movement loop below, especially on steps w/
*         no activity & those w/ nothing reported In.
            savewc$ = defwc$

        REM Let's see if the job has been reported into Step yet...
            init(hex(00)) readkey$
            str(readkey$,,8) = str(job$)

L30460:     call "PLOWALTS" (#3, readkey$, 0%, 8%, f1%(3))
              if f1%(3) = 0% then L30620
            get #3, using L30500, inwc$, fromstep$, instep$,              ~
                                 iqtym, iqtys, iqtyr, purg_flag$
L30500:     FMT  POS(13), CH(4), XX(48), CH(7), CH(7),                   ~
                 POS(99), 3*PD(14,4), POS(157), CH(1)
            if instep$ <> step$ then L30460
            if fromstep$ = " " and purg_flag$ <> "P"  then               ~
                               tot_thrown = tot_thrown + iqtys + iqtyr
            itot = itot + iqtym
            savewc$ = inwc$
            goto L30460


        REM What Was reported out of this Step ?
L30620:     str(readkey$,9) = str(savewc$)
            str(readkey$,13) = all(hex(00))
            step_thrown = 0

L30680:     call "PLOWALTS" (#3, readkey$, 1%, 12%, f1%(3))
              if f1%(3) = 0% then L30830
            get #3, using L30720, fromstep$, instep$, oqtym,              ~
                                 oqtys, oqtyr, purg_flag$, step_cmplt$
L30720:       FMT POS(65), CH(7), CH(7), POS(99), 3*PD(14,4),            ~
                  POS(157), CH(1), CH(1)

            if fromstep$ <> step$ then L30680  /* DUP WC CODE */
            if purg_flag$ <> "P" then                                    ~
                                 tot_thrown = tot_thrown + oqtys + oqtyr
            step_thrown = step_thrown + oqtys + oqtyr
            otot = otot + oqtym
            goto L30680

L30830:     if itot > 0 then status% = 1%    /* Some Reported IN  */

            if otot > 0 then status% = 2%    /* Some Reported OUT */
            if step_thrown > 0 then status% = 2%  /* Only scrap out */

            if status% = 0% then return  /* Nothing moved to/from here */

                      /* Job Qty Adjusted by Scrap & ReWork So Far */
            adj_qty = quantity - tot_thrown

                                     /* If True, Not ALL Reported IN  */
            if status% = 1% and round(adj_qty - itot, 2) > 0             ~
                            then clr_setup% = 1%
                                     /* If True, Not ALL Reported OUT */
            if status% = 2% and round(adj_qty - otot, 2) > 0             ~
                            then clr_setup% = 1%

*        Below (if true) is the only odd case where All reported IN
*        and only some are reported OUT and the IN was not reported
*        from the immediately previous step.  This caused a Back Purge
*        of all hours in previus steps not to occur.

            if step_cmplt$ <> "Y" or fromstep$ <> step$  then L31050
                status% = 2%
                clr_setup% = 0%
                goto L31110
L31050:     if status% = 2% and clr_setup% = 1%                          ~
                            and round(adj_qty - itot, 2) <= 0 then L31080
            goto L31110
L31080:         status% = 1%
                clr_setup% = 0%

L31110:         saveseq$ = seqr$
                savestep$ = step$
                gosub back_purge
                return

        REM *************************************************************~
            *             PURGE WORKCENTER ALLOCATION                   *~
            *************************************************************
        zip_it
            if just_in% <> 0% then L31570
               call "SHOSTAT" ("Purging Old Capacity Allocation Details")
               just_in% = 1%
L31570:     call "READ101" (#1, wcoutkey$, f1%(1))
                if f1%(1) = 0% then return  /* Already Purged */
            get #1, using L31600, inwc$, day%, setup%, run%
L31600:     FMT CH(4), POS(28), BI(2), POS(32), 2*BI(4)
            delete #1

            REM Re-align WCMASTR datas...
            call "READ101" (#2, inwc$, f1%(2))
                if f1%(2) = 0% then return  /* !?!? */
            get #2, using L31670, str(record$())
L31670:     FMT POS(1040), CH(980)
            get str(record$(), day%*2% - 1%, 2) using L31690, used%
L31690:         FMT BI(2)
            used% = max(used%-setup%-run%, 0%)
            put str(record$(), day%*2% - 1%, 2) using L31690, used%
            put #2, using L31670, str(record$())
            rewrite #2
            return

        REM *************************************************************~
            *             PURGE WORKCENTER SETUP ALLOCATION             *~
            *************************************************************
        clear_setup

            if just_in% <> 0% then L32080
               call "SHOSTAT" ("Purging Old Capacity Allocation Details")
               just_in% = 1%
L32080:     call "READ101" (#1, wcoutkey$, f1%(1))
                if f1%(1) = 0% then return  /* Already Purged */
            get #1, using L32110, inwc$, day%, setup%, run%
L32110:     FMT CH(4), POS(28), BI(2), POS(32), 2*BI(4)
                               /* Below Purges M/Q Before Record */
            if cnt% = 1% and setup% = 0 and run% = 0 then delete #1
            if setup% = 0 then return
            if run% = 0 then L32200       /* Only Setup this Day ? */
                put #1, using L32150, 0%
L32150:         FMT POS(32), BI(4)
                rewrite #1
                goto L32250

L32200:     delete #1

L32250:     REM  RE-ALIGN WCMASTR DATAS . . .  ( CLEAR OUT SETUP )
            call "READ101" (#2, inwc$, f1%(2))
                if f1%(2) = 0% then return  /* !?!? */
            get #2, using L32290, str(record$())
L32290:     FMT POS(1040), CH(980)
            get str(record$(), day%*2% - 1%, 2) using L31690, used%
            used% = max(used% - setup%, 0%)
            put str(record$(), day%*2% - 1%, 2) using L31690, used%
            put #2, using L32290, str(record$())
            rewrite #2
            return

        REM *************************************************************~
            *          BACK-PURGE WORKCENTER ALLOCATION                 *~
            *************************************************************
        back_purge

            init(hex(00)) readkey$
            instep$, seq2$ = " "
            cnt% = 0%

L33090:     call "PLOWAL1" (#8, readkey$, 0%, 0%, f1%(8))
                if f1%(8) = 0% then return
            get #8, using L33115, seqr$, step$, wcoutkey$
L33115:         FMT CH(6), XX(8), CH(7), XX(4), CH(23)
            if step$ <> instep$ then cnt% = 0%
            instep$ = step$    /* also used in flaging func below   */
                               /* it's ok, only needed here if      */
                               /* CLR_SETUP% = 1 .                  */
            cnt% = cnt% + 1%
            on status% goto L33200, L33220
                return    /* We Should't be Here */

L33200:     if seqr$ >= saveseq$ then return /* Don't do Current step */
                goto L33310
L33220:     if seqr$ >  saveseq$ then return /* Do Current Step Also  */

L33310:     if clr_setup% = 0% then L33350
               gosub clear_setup
               goto L33090

L33350:     delete #8         /* Clean Up Behind */
            gosub zip_it
            if seqr$ <> seq2$ then gosub flag_progress
            seq2$ = seqr$
            goto L33090        /* Next */

        REM *************************************************************~
            *   FLAG JBSTATUS RECORDS THAT CAUSED WCOUT'S TO BE PURGED. *~
            *************************************************************
        flag_progress
                               /* Variable INSTEP$ is               */
                               /* also used in purging func above,  */
                               /* it's ok, only needed here if      */
                               /* CLR_SETUP% = 0 .                  */
            init (hex(00)) plowkey2$
            str(plowkey2$,,8) = str(job$)

L34090:     call "PLOWAL1" (#3, plowkey2$, 0%, 8%, f1%(3))
                if f1%(3) = 0% then return
            get #3, using L34120, fromstep$, instep$
L34120:         FMT POS(65), CH(7), CH(7)
            if step$ <> fromstep$ and step$ <> instep$ then L34090
            put #3, using L34160, "P"
L34160:         FMT POS(157), CH(1)
            rewrite #3
            goto L34090

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

        bye_bye
            call "FILEBGON" (#8)         /* No Dangling Workfiles */
            end  /* Over And Out! */
