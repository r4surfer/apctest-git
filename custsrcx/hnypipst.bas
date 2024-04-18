        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  PPPP   IIIII  PPPP    SSS   TTTTT   *~
            *  H   H  NN  N  Y   Y  P   P    I    P   P  S        T     *~
            *  HHHHH  N N N   YYY   PPPP     I    PPPP    SSS     T     *~
            *  H   H  N  NN    Y    P        I    P          S    T     *~
            *  H   H  N   N    Y    P      IIIII  P       SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYPIPST - Updates Inventory & General Ledger with        *~
            *            Physical Inventory Count Variances.            *~
            *            First asks user for session to post variances  *~
            *            for.  Checks entered session to make sure all  *~
            *            tickets accounted for, etc.  If ok begins      *~
            *            variance posting.  In case of Restart          *~
            *            Ticket records are deleted as we go so at      *~
            *            most only one double posting can occur.        *~
            *----------------------------------------------------------Q*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/04/86 ! Original                                 ! LDJ *~
            * 01/12/86 ! Functionality Change.  If user           ! LDJ *~
            *          !   overrides the costs in HNYPIINP the    !     *~
            *          !   Inventory costs will now be revalued to!     *~
            *          !   the override costs prior to            !     *~
            *          !   performing any variance or adjustment  !     *~
            *          !   postings.  This means the G/L Inventory!     *~
            *          !   Asset & Adjustment accounts will also  !     *~
            *          !   be affected and posted to (IF G/L Post !     *~
            *          !   Flag = Y).  Does not apply to Standard !     *~
            *          !   Cost Methods.                          !     *~
            * 03/11/87 ! HNYQUAN select change and misc...        ! ERN *~
            * 03/23/87 ! File layout changes, no significant      ! LDJ *~
            *          !  functionality changes to this program.  !     *~
            *          !  (just more complex processing code...)  !     *~
            *          !  Includes bug fix also applied to 4.18.06!     *~
            *          !  patch - voided recounts affected the    !     *~
            *          !  count quantity if multiple locations /  !     *~
            *          !  tickets for the same part/store/lot.    !     *~
            * 05/27/87 ! Standard Costing Enhancements            ! MJB *~
            * 10/23/87 ! Corrected call to HNYAPST                ! HES *~
            * 01/03/89 ! Changed to check for HNYLOCNS file       ! MJB *~
            *          !  before attempting to write.             !     *~
            * 02/13/90 ! Added alt. key for HNYLOCNS file         ! JEF *~
            * 09/19/90 ! G/L Export file modifications.           ! RAC *~
            * 05/23/91 ! Conditioned Execution of G/L Export code.! JBK *~
            * 10/30/91 ! CMS/DEC 'MASK' Project/Added 'ALLFREE'   ! SID *~
            * 03/25/92 ! Cycle Count Project/Post to CC Files     ! RJH *~
            * 06/01/92 ! PRR 12468 Fixed channel in HNYPST2 call. ! JDH *~
            *          ! PRR 12389 Fixed var. after HNYAPST call  !     *~
            *          !   and channels in GLPOST2 call.          !     *~
            * 06/24/92 ! PRR 11034 Fixed multiple locations       ! RJH *~
            *          !   processing to work thru all locations. !     *~
            * 09/21/92 ! Changed ABC Class from CH(4) to CH(5)    ! SID *~
            * 09/30/92 ! PRR 12051 Use Location 'Snapshot' File   ! RJH *~
            *          !   when Posting to Location File.         !     *~
            *          ! Write to a Location Audit File if needed.!     *~
            * 10/02/92 ! PRR 11034 Above fix reversed to original.! RJH *~
            * 02/10/93 ! Minor mods for DEC. Short read key prob. ! JDH *~
            * 02/16/93 ! Init stacks for Recap & new Seq each time! JDH *~
            * 02/25/93 ! Added Core Value Tracking Coding.        ! JBK *~
            *          !  Posting from HNYPST2 will be printed    !     *~
            *          !  somewhere.  Posting from HNYPST2 will be!     *~
            *          !  skipped if G/L is not to be updated.    !     *~
            * 05/21/93 ! Write last count qty to HNYCCMST file.   ! JDH *~
            * 07/20/93 ! PRR 12993. Fill end of HNYPOOL rec w/" ".! JDH *~
            *          ! Added Session Number to HNYDETAL Text.   !     *~
            * 08/31/93 ! PRR 13018. Delay call to JNLINFO untill  ! RJH *~
            *          !  start of processing.                    !     *~
            * 09/02/93 ! Cycle Count Part's now use PI count date ! RJH *~
            *          !  to set CC Part's NextCountDate          !     *~
            *          ! Changed conditional for CNTPERIOD Convert!     *~
            *          !  to on DATA GOTO ...                     !     *~
            * 10/05/93 ! Location Snapshot not used when writing a! RJH *~
            *          !  new Location Record.                    !     *~
            * 01/04/93 ! Modified to support LOCAUDIT datetime chg! MLJ *~
            * 01/20/94 ! PRR 13085 - When Part's $ changed via    ! RJH *~
            *          !  HNYPIINP now correctly post to GL.      !     *~
            * 07/01/94 ! Reset CC Files when Ticket is VOID.      ! RJH *~
            * 05/25/94 ! PRR 13428-Don't Dbl formate LOCAUDIT date! RJH *~
            * 08/09/96 ! Changes for the year 2000.               ! DXL *~
            * 08/14/06 ! (AWD001) Mod to add GLORTRAN             ! CMG *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            account$9,                   /* Dummy arg to Stack Routines*/~
            accounted_for$1,             /* All Tickets Accounted For? */~
            actflag$1,                   /* Cycle Count Activity Flag  */~
            binloc$8,                    /* Location                   */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cat$(9,2)4,                  /* Categories to Count (Range)*/~
            cccntby$12,                  /* Cycle Count Counted By ID  */~
            ccenterby$3,                 /* Cycle Count Entered By ID  */~
            ccsesdesc$30,                /* Cycle Count Session Descrip*/~
            ccsesname$12,                /* Cycle Count Session Name   */~
            ccsessionflag$1,             /* Cycle Count Session Flag   */~
            ccsrcflag$1,                 /* Cycle Count Source Flag    */~
            ccsystime$8,                 /* Cycle Count Sys Time Stamp */~
            check_digit$1,               /* Calc & Append Check Digit? */~
            company$60,                  /* Company or Division Name   */~
            core_acct$(10)9,             /* Core G/L Posting Accounts  */~
            core_credit$10,              /* Core Credit Value for Rpt  */~
            core_dbcr(10,2),             /* Core CR & DB Amounts       */~
            core_debit$10,               /* Core Debit Value for Rpt   */~
            costtype$1,                  /* Inventory Costing Method   */~
            cost(12),                    /* Cost Buckets for Update    */~
            cost1(12),                   /* Cost Buckets from HNYPICST */~
            cost2(12),                   /* Cost Buckets from HNYPITKT */~
            cost3(12),                   /* Holding array              */~
            costadj(12),                 /* Cost Variance array        */~
            costs$96,                    /* For PACKZERO               */~
            count_date$8,                /* Date Counted               */~
            countdate$8,                 /* Date Counted (Cycle Count) */~
            cntperiod$3,                 /* Cycle Count Period         */~
            credit$9,                    /* Credit Account             */~
            creditsstk$(500)9,           /* Credits for Rcp#1 Phase    */~
            creditsstk (500),            /* Credit Amounts for Rcp#1   */~
            cur_cost(12),                /* Current Costs in HNYQUAN   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            date1$8,                     /* Date Session Generated     */~
            date2$8,                     /* Date Quantities Captured   */~
            datetime$7,                  /* System DateTime Stamp      */~
            debit$9,                     /* Debit Account              */~
            debitsstk$(250)9,            /* Debit Accts for Rcp#1 Phase*/~
            debitsstk (250),             /* Debit Amounts for Rcp#1    */~
            description$32,              /* Count Session Number       */~
            edtmessage$79,               /* Edit screen message        */~
            enter_date$8,                /* Date Count Entered         */~
            enter_time$8,                /* Time Count Entered         */~
            errormsg$79,                 /* Error message              */~
            extra$1,                     /* Extra/Supplemental Ticket? */~
            fielda$1,                    /* Filler Variable            */~
            filekey$100,                 /* Miscellaneous Plow Key     */~
            firstcntdate$8,              /* First Cycle Count Date     */~
            gvar$1,                      /* Update G/L variances flag  */~
            glacct$9,                    /* G/L Account from Work Files*/~
            gltext$100,                  /* G/L Posting Text           */~
            glvar$1,                     /* Update G/L variances flag  */~
            hyvar$1,                     /* Update HNY variances Flag  */~
            hnyacct$(13)9,               /* HNY Accounts for HNYAPST   */~
            hnyadj$9,                    /* Inventory Asset Account    */~
            hnyasset$9,                  /* Inventory Adjustment Acct  */~
            hnydate$8,                   /* User Inventory Posting Date*/~
            hnydate2$8,                  /* Fmtd Inventory Posting Date*/~
            hnyvar$1,                    /* Update HNY variances Flag  */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            jnlid$3,                     /* Adjustment Journal ID      */~
            lastticket$6,                /* Last Ticket Nbr Processed  */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            loc$8,                       /* Location                   */~
            locauditflag$1,              /* Location Audit Flag (Y/N)  */~
            location$2,                  /* Locator Array for Stacks   */~
            loctranstype$1,              /* Location Transaction Type  */~
            lot$16,                      /* Lot                        */~
            lot_or_loc$1,                /* Tickets Generated by Lot or*/~
            modno$20,                    /* CMS Module Number          */~
            name$20,                     /* Count by name              */~
            neg_only$1,                  /* Count Neg On-Hand Only ?   */~
            nextcntdate$8,               /* Next Cycle Count Date      */~
            override$1,                  /* Cost Override flag         */~
            part$25,                     /* Part Code                  */~
            part$(3,2)25,                /* Parts to Count (Ranges)    */~
            part_req$1,                  /* Part Numbers required ?    */~
            pf16$17,                     /* PF 16 literal              */~
            plowkey$100,                 /* Miscellaneous Plow Key     */~
            pooltext$40,                 /* Text for Pool Record       */~
            prefix$3,                    /* Ticket Number Prefix (Opt) */~
            print$1,                     /* Print tickets or sheets    */~
            rcpacct$(2)16,               /* Account Numbers for Recap  */~
            rcpacctdescr$(2)30,          /* Account Descriptions-recap */~
            rcpamt(2),                   /* Recap Amounts              */~
            rcpline%(2),                 /* Line Pointers for Recap    */~
            rcpptr%(2),                  /* Pointer into D & C Arrays  */~
            readkey$100,                 /* Miscellaneous Read Key     */~
            recdate$8,                   /* Cycle Count Record Start   */~
            rpt_time$8,                  /* Report Generation Time     */~
            seq$1,                       /* Ticket Number Sequence     */~
            session_date$8,              /* Planned Count Date         */~
            session_nbr$2,               /* Count Session Number       */~
            start_ticket$6,              /* Starting Ticket Number     */~
            store$3,                     /* Warehouse                  */~
            summary$1,                   /* Posting Summary Level      */~
            supplement$1,                /* Supplemental Ticket Flag   */~
            text$50,                     /* Posting Text               */~
            text$(8)50,                  /* Text Filler in HNYLOCNS    */~
            ticket$12,                   /* Formatted Ticket Number    */~
            time$8,                      /* Time Quantities Captured   */~
            title$60,                    /* Journal Title              */~
            transtype$2,                 /* Transaction Type (HNYDETAL)*/~
            units$10,                    /* Unit Adjustments           */~
            userid$3,                    /* Current User Id            */~
            user_id$3,                   /* Entered by user id         */~
            variance$10,                 /* Variance Amount            */~
            whse$(9,2)3                  /* Warehouses to Count (Range)*/

        dim                              /* G/L Export Posting Info    */~
            export_on$1,                 /* G/L Export File processing?*/~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            misckey$25,                  /* Miscellaneous Plowkey      */~
            partcat$4,                   /* Part Category code         */~
            partclass$4,                 /* Part Class code            */~
            partgen$16,                  /* Part Generic code          */~
            parttype$3,                  /* Part Type code             */~
            tran_type$5,                 /* G/L Transaction type       */~
            uom$                         /* Part Unit of measure       */

        dim division$3,                  /* division number (AWD001)   */~
            schema$8                     /* schema          (AWD001)   */

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
            * #1  ! HNYPISYS ! Physical Inventory System Session Contro *~
            * #2  ! HNYPITKT ! Physical Inventory Ticket File           *~
            * #3  ! HNYMASTR ! Inventory Master File                    *~
            * #4  ! HNYQUAN  ! Inventory Part / Store / Lot Quantity Fi *~
            * #5  ! HNYLOCNS ! Part Locations Master File               *~
            * #6  ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #7  ! SFCUM2   ! Cumulative sales forecast file           *~
            * #8  ! GLMAIN   ! General Ledger Main File                 *~
            * #9  ! GLDETAIL ! General ledger Detail Transactions Histo *~
            * #10 ! HNYDETAL ! Inventory detail file                    *~
            * #11 ! USERINFO ! Users Default Information File           *~
            * #12 ! HNYADJPF ! HNYPOST Adjustment G/L Journal File      *~
            * #13 ! SYSFILE2 ! Caelus Management System Information     *~
            * #14 ! HNYPOOL  ! Inventory LIFO/FIFO pool records         *~
            * #15 ! HNYPICST ! Physical Inventory Costs Snapshot File   *~
            * #16 ! HNYPSTGL ! HNYPST2 G/L workfile                     *~
            * #17 ! HNYPILOC ! Physical Inventory Location Snapshot File*~
            * #18 ! LOCAUDIT ! Location Audit File                      *~
            * #19 ! GLORTRAN ! GL Oracle transmit file     (AWD001)     *~
            * #20 ! GENCODES ! GENERAL CODES MASTER FILE   (AWD001)     *~
            * #23 ! HNYCCMST ! Cycle Count Master File                  *~
            * #24 ! HNYCCDTL ! Cycle Count Session Detail File          *~
            * #26 ! HNYCCSYS ! Cycle Count Session System File          *~
            * #27 ! COREWRK  ! Core Value Work File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "HNYPISYS",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =    7, keylen =   2,                     ~
                        alt key  1, keypos =    1, keylen =   8          ~

            select #2,  "HNYPITKT",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =    1, keylen =  15,                     ~
                        alt key  1, keypos =   16, keylen =  52, dup,    ~
                            key  2, keypos =  313, keylen =  16

            select #3,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #4,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =  1, keylen =  44

            select #5,  "HNYLOCNS",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =  443, keylen =  42,         ~
                            key  2, keypos =  485, keylen =  42,         ~
                            key  3, keypos =  527, keylen =  42,         ~
                            key  4, keypos =  590, keylen =  42

            select #6,  "PIPMASTR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select #7,  "SFCUM2",                                        ~
                        varc,     indexed,  recsize = 1985,              ~
                        keypos =    1, keylen =  25                      ~

            select #8,  "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9                      ~

            select #9,  "GLDETAIL",                                      ~
                        varc,     indexed,  recsize =  160,              ~
                        keypos =    1, keylen =  26                      ~

            select #10, "HNYDETAL",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =   43, keylen =   6, dup,    ~
                            key  2, keypos =   49, keylen =   2, dup     ~

            select #11, "USERINFO",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =   3                      ~

            select #12, "HNYADJPF",                                      ~
                        varc,     indexed,  recsize =  160,              ~
                        keypos =    1, keylen =  19                      ~

            select #13, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #14, "HNYPOOL",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  38                      ~

            select #15, "HNYPICST",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  46

            select #16, "HNYPSTGL",                                      ~
                        varc,     indexed,  recsize =  160,              ~
                        keypos =    1, keylen =  19                      ~

            select #17, "HNYPILOC",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  44                      ~

            select #18, "LOCAUDIT",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos =    1, keylen =  50,                     ~
                        alt key  1, keypos =   95, keylen =  42, dup     ~



/*(AWD001)*/
            select #19, "GLORTRAN",                                       ~
                        varc,     indexed, recsize = 512,                 ~
                        keypos = 1,    keylen = 33,                       ~
                        alt key 1, keypos = 31, keylen = 47,              ~
                            key 2, keypos = 81, keylen = 26


            select #20, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
/*(AWD001)*/

            select #23, "HNYCCMST",                                      ~
                        varc,     indexed,  recsize = 796,               ~
                        keypos =    1,  keylen = 44,                     ~
                        alt key  1, keypos =   45, keylen =   6, dup,    ~
                            key  2, keypos =   81, keylen =   7, dup,    ~
                            key  3, keypos =   73, keylen =  15, dup     ~

            select #24, "HNYCCDTL",                                      ~
                        varc,     indexed,  recsize =  436,              ~
                        keypos =    1, keylen =  57,                     ~
                        alt key  2, keypos =   14, keylen =  44, dup,    ~
                            key  1, keypos =   13, keylen =  45, dup     ~

            select #26, "HNYCCSYS",                                      ~
                        varc,     indexed,  recsize =  249,              ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  1, keypos =   43, keylen =  13, dup,    ~
                            key  2, keypos =   56, keylen =  10

            select #27, "COREWRK",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 256,                                  ~
                         keypos =  1, keylen = 19                        ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

                rslt$(1 ) = "REQUIRED"
            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))
                if fs%(01) < 1% then exit_program
                rslt$(2 ) = "REQUIRED"
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
                if fs%(02) < 1% then exit_program
                rslt$(3 ) = "REQUIRED"
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
                if fs%(03) < 1% then exit_program
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 0%, rslt$(5 ))
                rslt$(6 ) = "REQUIRED"
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 0%, rslt$(6 ))
                if fs%(06) < 1% then exit_program
            call "OPENCHCK" (#7,  fs%(7 ), f2%(7 ), 0%, rslt$(7 ))
                rslt$(8 ) = "REQUIRED"
            call "OPENCHCK" (#8,  fs%(8 ), f2%(8 ), 0%, rslt$(8 ))
                if fs%(08) < 1% then exit_program
            call "OPENCHCK" (#9,  fs%(9 ), f2%(9 ), 0%, rslt$(9 ))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
                rslt$(11) = "REQUIRED"
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
                if fs%(11) < 1% then exit_program
                rslt$(13) = "REQUIRED"
            call "OPENCHCK" (#13, fs%(13), f2%(13), 0%, rslt$(13))
                if fs%(13) < 1% then exit_program
            call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))
            call "OPENCHCK" (#14, fs%(14), f2%(14), 0%, rslt$(14))
            call "OPENCHCK" (#15, fs%(15), f2%(15), 0%, rslt$(15))
            call "WORKOPEN" (#16, "IO", 10%, f2%(16))
            call "OPENCHCK" (#17, fs%(17%), f2%(17%),   0%, rslt$(17%))
            call "OPENCHCK" (#18, fs%(18%), f2%(18%), 100%, rslt$(18%))

/*(AWD001) */
            call "OPENCHCK" (#19, fs%(19%), f2%(19%), 100%, rslt$(19%))
            call "OPENCHCK" (#20, fs%(20%), f2%(20%),   0%, rslt$(20%))
/*(AWD001)*/

            goto init_program     /* Lets Get Started */

        open_cycle_count_files
            call "OPENCHCK" (#23, fs%(23), f2%(23), 0%, rslt$(23))
            call "OPENCHCK" (#24, fs%(24), f2%(24), 0%, rslt$(24))
            call "OPENCHCK" (#26, fs%(26), f2%(26), 0%, rslt$(26))
        return

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
        init_program


                                                          /* (AWD001)  */

            schema_err%, schema% = 0%
            init(" ") schema$, division$
            call "SCHEMA" (schema$, schema%, #20, schema_err%)


            if schema% = 1% then division$ = "036"   /* NC */
            if schema% = 2% then division$ = "080"   /* NE */
            if schema% = 3% then division$ = "035"   /* AES*/

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (2%, company$, returncode%)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value and Press (RETURN)."

            REM Retrieve Inventory Date for this User
                call "READ100" (#11, userid$, f1%(11))
                      if f1%(11) = 1% then L09230
                call "ASKUSER" (keyhit%, "***INVALID USER OR DATABASE***"~
                   ,"The User Defaults Record was not found for " &      ~
                    userid$ & ".",                                       ~
                    "You will not be allowed to continue until this " &  ~
                    "condition has been corrected.",                     ~
                    "Press RETURN to Acknowlege & Exit.")
                    goto L65000
L09230:         get #11, using L09240, hnydate$
L09240:                 FMT XX(27), CH(6)
                hnydate2$ = hnydate$
                call "DATEFMT" (hnydate2$)
                call "WHICHMON" (#13, hnydate$, whichmonth%)
                     if whichmonth% >= 1% and whichmonth% <= 3% then L09360
                  call "ASKUSER" (keyhit%, "***INVALID POSTING DATE***", ~
                    "Your Inventory Posting Date (" & hnydate2$ & ")",   ~
                    "is Outside of the Posting Window.  You will not " & ~
                    "be allowed to continue until",                      ~
                    "this condition has been corrected.  (Press RETURN" &~
                    " to Acknowlege & Exit.")
                   goto L65000
L09360:      REM Do a GETPARM to find JNLID$
                jnlid$ = "IPA"
                call "GETPARM" addr ("ID", "S", "JNLID   ",  " ", "0001",~
                                     "HNYPIP",                           ~
                                    "Enter the Journal ID to Post Thru ",~
                                      34%, "K", "JNLID   ", jnlid$, 3%,  ~
                                      5%, 32%, "A")
                goto L09600

        call_jnlinfo
                modno$ = "04"
                returncode% = 0%
                call "JNLINFO" (modno$, jnlid$, pstseq%, summary$,       ~
                      title$, hnydate$, #13, f2%(13%), returncode%)

                title$ = title$ & " JOURNAL"
                call "STRING" addr("CT", title$, 60%)
                text$ = "PHYS INV VARIANCE ADJ - SESSION: nn"
                return

L09600
*        See if G/L Export is on
            export_on$ = "N"  :  plowkey$ = "SWITCHS.GL"
            call "READ100" (#13, plowkey$, f1%(13%))
            if f1%(13%) = 1% then get #13 using L09640, export_on$
L09640:         FMT POS(22), CH(1)

*        See if Location Audit is On
            plowkey$ = "SWITCHS.HNY"
            call "READ100" (#13, plowkey$, f1%(13%))
            if f1%(13%) = 1% then get #13 using L09680, locauditflag$
L09680:         FMT POS(120), CH(1)

*        See if Core is on
            plowkey$ = "SWITCHS.COR"
            call "READ100" (#13, plowkey$, core_on%)
                if core_on% <> 1% then L10000
            get #13 using L09733, core_inv_flag$
L09733:         FMT POS(134), CH(1)
            if core_inv_flag$ <> "Y" then core_on% = 0%
            if core_on% <> 1% then L10000
                call "WORKOPEN" (#27, "IO", 10%, f2%(27%))

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            init(" ")errormsg$,inpmessage$,session_nbr$,description$,    ~
                     hnyvar$, glvar$, debitsstk$(), creditsstk$()
            mat debitsstk = zer : mat creditsstk = zer
            debitsptr%, creditsptr% = 0%
            call "ALLFREE"
            pf16$ = "(16)Exit Program"

            for fieldnr% = 1% to  1%
L10100:         gosub'051(fieldnr%)
                      if enabled% = 0 then L10220
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10120
L10220:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

            pf16$ = "(16)Start Posting"
            inpmessage$ = edtmessage$

L11060:     gosub'101(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 12
            if fieldnr% < 1 or fieldnr% >  1 then L11060
            gosub'051(fieldnr%)
                  if enabled% = 0% then       L11060
L11140:     gosub'101(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11140
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11140
            goto L11060

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub call_jnlinfo          /* Open a new batch each time */
            call "SHOSTAT" ("Physical Inventory Variance Posting Now in P~
        ~rogress...")
            call "SETPRNT" ("HNY010","HNYPI   ",5000%, 0%)
            rpt_time$ = " "              /* Grab System Time           */
            call "TIME" (rpt_time$)
            page%, rcppage% = 0%  :  line% = 99%
            select printer(134)
            if ccsessionflag$ <> "C" then L19100
                gosub open_cycle_count_files
                gosub set_ccsession
L19100:     plowkey$ = session_nbr$
            if ccsessionflag$ = "C" then gosub close_cycle_count_session
            gosub plow_tickets
            call "DELETE" (#1, session_nbr$, 2%)
            call "DELETE" (#15, session_nbr$, 2%)
            gosub print_recap
            close printer
            call "SETPRNT" ("HNY010","HNYPI   ", 0%, 1%)
            call "JNLCLOSE" (modno$, jnlid$, pstseq%, returncode%)
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100          /* Count Session Num*/
                     return
L20100:     REM Default/Enable for Count Session Number
                inpmessage$ = "Enter the Count Session Number to "       ~
                            & "Capture Qty's for or RETURN to Find"
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
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

            get #1 using L35040 ,                                         ~
            session_date$,  /* Date count session planned              */~
            session_nbr$,   /* Number corresponding to a Inventory Coun*/~
            description$,   /* Generic for general code descriptions   */~
            whse$(),        /* Warehouse or Stores  (Ranges)           */~
            cat$(),         /* Category codes (Ranges)                 */~
            part$(),        /* Part codes (ranges)                     */~
            neg_only$,      /* Count Negative items only ?             */~
            lot_or_loc$,    /* 1 Ticket per Lot or 1 per Location flag */~
            prefix$,        /* 1 to 3 character prefix to the P.I. Tick*/~
            start_ticket$,  /* Ticket Number to a Physical Inventory Co*/~
            lastticket$,    /* Last Ticket Number used                 */~
            check_digit$,   /* Calculate & append check digit ?        */~
            extra$,         /* Number of extra Physical Inventory Ticke*/~
            seq$,           /* Flag controlling what sequence tickets w*/~
            part_req$,      /* Part Numbers required in Count Entry ?  */~
            print$,         /* Print tickets or Count sheets  Indicator*/~
            hnyvar$,        /* Update inventory variances     Indicator*/~
            glvar$,         /* Update G/L       variances     Indicator*/~
            date1$,         /* Date a session generated                */~
            date2$,         /* Date costs/quantities captured          */~
            time$,          /* Time costs/quantities captured          */~
            accounted_for$, /* All Tickets Accounted For ? (Y or blank)*/~
            ccsessionflag$  /* Cycle Count Session Flag    (C or P)    */

            if ccsessionflag$ = "C" then transtype$ = "IC"               ~
                                    else transtype$ = "IP"
            return

        load_ticket
            get #2 using L35300 ,                                         ~
               session_nbr$,/* Number corresponding to a Inventory     */~
                            /* Count Session                           */~
               ticket$,     /* Ticket Number to a Physical Inventory   */~
                            /* Count Item                              */~
               recount%,    /* A number decremented for each recount   */~
                            /* (99=original                            */~
               part$,       /* Part Number                             */~
               store$,      /* Store or Warehouse Code                 */~
                            /* Warehouse code                          */~
               lot$,        /* Lot Number                              */~
               loc$,        /* bin location                            */~
                            /* Actual Bin Location (from HNYQUAN file  */~
                            /* or HNYLOCNSfile)                        */~
               supplement$, /* Extra (supplemental) Ticket Flag: "X" = */~
                            /* extra, blank = not an extra             */~
               gvar$,       /* Flag or switch which controls posting   */~
                            /* to G/L                                  */~
               hyvar$,      /* Flag or switch which controls posting   */~
                            /* to Inventory                            */~
               override$,   /* Override flag / indicator               */~
                            /* = "Y" if cost(s) are manually           */~
                            /* overridden                              */~
               name$,       /* Name of person who counted something    */~
               user_id$,    /* user-id of specific user                */~
                            /* User Id of Data Entry Operator          */~
               count_date$, /* Date something was counted              */~
               enter_date$, /* Date a transaction was entered          */~
                            /* Date entered into computer              */~
               enter_time$, /* The System Time when a transaction was  */~
                            /* entered                                 */~
               count_qty,   /* Actual Quantity On Hand according to    */~
                            /* the count. If less than 0 then ticket   */~
                            /* is a VOIDED ticket.                     */~
               locqty,      /* Quantity From HNYLOCNS file (if         */~
                            /* applicable)                             */~
               cost2(),     /* 12 Cost Buckets        User             */~
               tot_cost2    /* Total Standard Cost       Over-rides    */

            if count_qty < 0 then return  /* Void, return & Delete It! */

            filekey$ = str(session_nbr$) & str(part$) & str(store$) & lot$
            if filekey$ = key(#15) and fs(#15) = "00" then L30559
            call "READ100" (#15, filekey$, f1%(15))
            if f1%(15) = 1% then L30559
            mat cost1 = zer : qoh = 0 : tot_cost1 = 0 : goto L30600

L30559:     get #15 using L36010,                                         ~
                qoh,        /* quantity on-hand                        */~
                            /* Frozen qty-on-hand from HNYQUAN record. */~
                            /* This is the 'snapshot' qty-on-hand.     */~
                cost1(),    /* 12 Cost Buckets                         */~
                tot_cost1   /* Total Standard Cost                     */~
                            /*         These are the frozen or         */~
                            /*               'snapshot' cost fields    */

L30600:
            if hyvar$ = " " then hyvar$ = hnyvar$
            if gvar$ = " " then gvar$ = glvar$
            locqty = count_qty
            if override$ = "Y" then L30680
               mat cost = cost1
               tot_cost = tot_cost1
               goto L30710
L30680:     tot_cost = tot_cost2
            mat cost = cost2

L30710:     if lot_or_loc$ = "P" then L30880
              /* Loop thru tickets looking for more Locations for   */
              /* current Part/Store/Lot combination to accumulate   */
              /* quantities for HNYQUAN update.                     */
               readkey$ = str(key(#2,1),,44%) & hex(00)
L30730:        call "PLOWALTS" (#2, readkey$, 1%, 44%, f1%(2))
                   if f1%(2) = 0% then L30880
               if key(#2) = plowkey$ then L30730
               get #2 using L30910, binloc$, qty_count, lqty
               if qty_count < 0 then L30830
               count_qty = count_qty + qty_count  : lqty = qty_count
L30830:        filekey$ = str(key(#2),,14%) & hex(00)
               call "DELETE" (#2, filekey$, 14%)
               if qty_count >= 0 then gosub update_locations
               goto L30730

L30880:     var_amt = count_qty - qoh
            return

L30910:     FMT POS(60), CH(8), POS(113), PD(14,4), POS(201), PD(14,4)
            recount% = recount%        /* just to get rid of error     */

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        plow_tickets
            str(plowkey$,15%) = hex(ff)  /* Get Latest Recount Only    */
L31035:     call "PLOWNEXT" (#2, plowkey$, 2%, f1%(2))
            if f1%(2) = 0% then return
            gosub load_ticket
            if count_qty >=0 then L31065            /* Voided Ticket ?  */
               call "DELETE" (#2, plowkey$,15%)
               if ccsessionflag$ = "C" then gosub reset_ccssn_for_void_tkt
               goto L31035
L31065:     if ccsessionflag$ = "C" then gosub get_before_values
            if hyvar$ <> "Y" and gvar$ <> "Y" then L31825
            gltext$ = "TICKET: " & ticket$
            str(gltext$,31%)= str(part$) & str(store$) & lot$
            str(gltext$,69%)= "P.I. VARIANCE ADJ"
            if tot_cost = 0 then gosub try_for_standard
            mat cost3 = cost : tot_cost3 = tot_cost
            if override$ <> "Y" and gvar$ <> "Y" then L31171
            REM *** Get G/L Accounts Like HNYPST2 does ***
            call "HNYGLGET" (part$,      /* Part Code                  */~
                      store$,            /* Store code                 */~
                      lot$,              /* Lot number                 */~
                      hnyasset$,         /* HNY Asset Account          */~
                      3%,                /* Return HNY Asset Account   */~
                      #3,                /* UFB Address of HNYMASTR    */~
                      #4)                /* UFB Address of HNYQUAN     */

            call "HNYGLGET" (part$,      /* Part Code                  */~
                      store$,            /* Store code                 */~
                      lot$,              /* Lot number                 */~
                      hnyadj$,           /* HNY Asset Account          */~
                      6%,                /* Return HNY Adjmt Account   */~
                      #3,                /* UFB Address of HNYMASTR    */~
                      #4)                /* UFB Address of HNYQUAN     */
L31171:     if hyvar$ <> "Y" and var_amt = 0 then L31825
            if hyvar$ <> "Y" then L31565
            if override$ = "Y" then gosub revalue_inventory
            if var_amt = 0 then L31825
            gl1% = 3%
            gl2% = 6%
            hnyasset$, hnyadj$ = " "
            str(text$, 34%) = session_nbr$
            str(gltext$,69%)= "P.I. VARIANCE ADJ"
            if var_amt >= 0 then L31203
            call "HNYHOLD" (#4, part$, store$, lot$, -var_amt,           ~
                                                         returncode%)
L31203:     if export_on$ = "Y" then gosub load_gl_info

            call"HNYPST2"(part$,         /* Part to be updated         */~
                      store$,            /* Store code                 */~
                      lot$,              /* Lot number                 */~
                      var_amt,           /* Quantity + or -            */~
                      0,                 /*  1 = Post to On Hand       */~
                      0,                 /*  2 =         Backordered   */~
                      0,                 /*  3 =         On Order      */~
                      0,                 /*  4 =         Committed     */~
                                         /*  5 =         In Process    */~
                      cost(),            /* 12 Cost buckets            */~
                      tot_cost,          /* Total Cost                 */~
                      0,                 /* Unit Price                 */~
                      0,                 /* Extension of Price         */~
                      hnydate$,          /* Module Date (YYMMDD)       */~
                      transtype$,        /* Transaction Type (HNYDETAL)*/~
                      text$,             /* Reference Text String      */~
                      hnyasset$,         /* HNY Asset Account          */~
                      hnyadj$,           /* Offset Account             */~
                                         /* If blank, will be returned */~
                                         /* via GL1%, GL2%             */~
                      gl1%,              /* 1st Account Level          */~
                      gl2%,              /* 2nd Account Level          */~
                                         /* 1 = SRCE Account,          */~
                                         /* 2 = HNY  Account,          */~
                                         /* 3 = COGS Account,          */~
                                         /* 4 = SALE Account,          */~
                                         /* 5 = VARM Account,          */~
                                         /* 6 = VARL Account,          */~
                                         /* 7 = VARO Account,          */~
                                         /* 8 = ADJ  Account,          */~
                                         /* DEF, GL1%=2, GL2%=1 OR 3   */~
                                         /*                PROC    WDWL*/~
                  modno$,                /* G/L Module Posting Inventor*/~
                  " ",                   /* Journal ID                 */~
                  pstseq%,               /* Posting Sequence No        */~
                  gltext$,               /* G/L Text passed from Progrm*/~
                  userid$,               /* Who did the dirty deed     */~
                      #4,                /* UFB Address of HNYQUAN     */~
                      #10,               /* UFB Address of HNYDETAL    */~
                      #13,               /* UFB Address of SYSFILE2    */~
                      #14,               /* UFB Address of HNYPOOLS    */~
                      #3,                /* UFB Address of HNYMASTR    */~
                      #6,                /* UFB Address of PIPMASTR    */~
                      #7,                /* UFB Address of SFCUM2      */~
                      #8,                /* UFB Address of GLMAIN      */~
                      #9,                /* UFB Address of GLDETAIL    */~
                      #16,               /* UFB Address of Workfile    */~
                      0%,                /* Costing, if a 'Procurement'*/~
                                         /*  and cost is not available,*/~
                                         /*  then use 'Best Guess'  ?  */~
                                         /*  0% = Don't, <> 0% = Do It.*/~
                                         /* Costs are returned         */~
                                         /* as a bonus. If passed as a */~
                                         /* variable will return THIS% */~
                                         /* (Month Posted).            */~
                      returncode%)       /* Error Code Returned;       */

            if core_on% <> 1% or gvar$ <> "Y" then L31494
                call "CORVALSB" (transtype$, part$, store$, lot$,        ~
                                 var_amt, hnydate$, hnyasset$, hnyadj$,  ~
                                 modno$, jnlid$, pstseq%, gltext$,       ~
                                 userid$, #4, #13, #3, #27, u3%)

L31494:     misckey$ = " "
L31496:     call "PLOWNEXT" (#16, misckey$, 0%, f1%(16))
               if f1%(16) = 0% then L31547
               get #16 using L31502, glacct$, gltext$, damt, camt
L31502:        FMT XX(25), CH(9), CH(100), 2*PD(14,4)
               tran_type$ = "IPA01"
               if str(gltext$,67,1) = "H" then L31513
               str(tran_type$,4,2) = "04"
               if str(gltext$,65,1) = "S" then L31513
               str(tran_type$,4,2) = "03"
L31513:        if export_on$ = "Y" then                                  ~
                     put gl_post_info$() using L31544, tran_type$,        ~
                              (damt-camt), 0
               call "GLPOST2" (glacct$, damt, camt, hnydate$, 0%,        ~
                               modno$, gltext$, jnlid$, pstseq% ,        ~
                               userid$, division$, #8, #9, #13, #19,     ~
                               /* (AWD001) */ returncode%, " ",         ~
                               gl_post_info$())

            call "GLPRTSUB" (modno$,     /* Source Module ID           */~
                           jnlid$,       /* Journal ID                 */~
                           pstseq%,      /* Posting Sequence Number    */~
                           userid$,      /* Who did the dirty deed     */~
                           hnydate$,     /* Date of Module Posting Date*/~
                           glacct$,      /* Account to be Updated      */~
                           gltext$,      /* Reference text (100 Chars) */~
                           damt,         /* Debit Amount (0 if credit) */~
                           camt,         /* Credit Amount (0 if debit) */~
                           #12,          /* UFB Address of HNYADJPF    */~
                           f2%(12))      /* File Open Status           */

               call "READ101" (#16, str(misckey$,1%,19%), f1%(16%))
               delete #16
               goto L31496
L31544:        FMT CH(5), POS(18), 2*PD(15,4)

L31547:      call "LOTTRACK"                                             ~
                 ("H",             /* FROM FLAG  H=INVNTORY,V=VEN,C=CUS*/~
                  part$,           /* PART BEING MOVED                 */~
                  lot$,            /* FROM LOT                         */~
                  store$,          /* FROM STORE                       */~
                  " ",             /* FROM WHATEVER                    */~
                  "A",             /* TO FLAG H=INVNTORY, V=VEN, C=CUS */~
                  str(session_date$) & str(session_nbr$) & ticket$,      ~
                  " ",             /* TO LOT                           */~
                  " ",             /* TO STORE                         */~
                  " ",             /* TO WHATEVER                      */~
                  var_amt,         /* QUANTITY MOVED                   */~
                  #3,              /* 'HNYMASTR' FILE                  */~
                  #13)             /* 'SYSFILE2' FILE                  */

L31565:     variance = round(var_amt * tot_cost3 ,2)
            if variance = 0 then L31810
            if gvar$ <> "Y" then L31810

            if export_on$ = "Y" then                                     ~
                put gl_post_info$() using L31544, "IPA01", variance,      ~
                          var_amt
            call"GLPOST2" (hnyasset$,    /* Account to be Updated      */~
                           variance,     /* Debit Amount (0 if credit) */~
                           0,            /* Credit Amount (0 if debit) */~
                           hnydate$,     /* Date of Module Posting Date*/~
                           0%,           /* 0 means restricted to 3open*/~
                           modno$,       /* Source Module ID           */~
                           gltext$,      /* Reference text (100 Chars) */~
                                         /*   1st 30 chars = Reference1*/~
                                         /*   2nd 34 chars = Reference2*/~
                                         /*   3rd  4 chars = Adjust Flg*/~
                                         /*   Lst 32 chars = Descriptn */~
                           jnlid$,       /* Journal ID                 */~
                           pstseq%,      /* Posting Sequence Number    */~
                           userid$,      /* Who did the dirty deed     */~
                           division$,    /* (AWD001) division code     */~
                           #8,           /* UFB Address of GLMAIN file */~
                           #9,           /* UFB Address of GLDETAIL    */~
                           #13,          /* UFB Address of SYSFILE2    */~
                           #19,          /* (AWD001) GLORTRAN          */~
                           returncode%,  /* Error Code Returned        */~
                           " ",          /* Currency document ID       */~
                           gl_post_info$()) /* G/L posting info        */

            if export_on$ = "Y" then                                     ~
                put gl_post_info$() using L31544, "IPA02", -variance,     ~
                           -var_amt

            call"GLPOST2" (hnyadj$,      /* Account to be Updated      */~
                           0,            /* Debit Amount (0 if credit) */~
                           variance,     /* Credit Amount (0 if debit) */~
                           hnydate$,     /* Date of Module Posting Date*/~
                           0%,           /* 0 means restricted to 3open*/~
                           modno$,       /* Source Module ID           */~
                           gltext$,      /* Reference text (100 Chars) */~
                                         /*   1st 30 chars = Reference1*/~
                                         /*   2nd 34 chars = Reference2*/~
                                         /*   3rd  4 chars = Adjust Flg*/~
                                         /*   Lst 32 chars = Descriptn */~
                           jnlid$,       /* Journal ID                 */~
                           pstseq%,      /* Posting Sequence Number    */~
                           userid$,      /* Who did the dirty deed     */~
                           division$,    /* (AWD001) division code     */~
                           #8,           /* UFB Address of GLMAIN file */~
                           #9,           /* UFB Address of GLDETAIL    */~
                           #13,          /* UFB Address of SYSFILE2    */~
                           #19,          /* (AWD001) GLORTRAN          */~
                           returncode%,  /* Error Code Returned        */~
                           " ",          /* Currency document ID       */~
                           gl_post_info$()) /* G/L posting info        */

            if variance < 0 then L31805
               debit$ = hnyasset$ : credit$ = hnyadj$
               goto L31807
L31805:        debit$ = hnyadj$ : credit$ = hnyasset$
L31807:     mat costadj = cost3 - cost
L31810:     tot_costadj = tot_cost3 - tot_cost
            if tot_costadj <> 0 then gosub adjustment_posting

L31825:     readkey$ = str(session_nbr$) & str(ticket$) & hex(00)
            call "DELETE" (#2, readkey$, 14%)
            binloc$ = loc$ : lqty = locqty
            gosub update_last_count_date
            if lot_or_loc$ = "L" then gosub update_locations
            if ccsessionflag$ = "C" then gosub update_cycle_count
            if gvar$ <> "Y" then plow_tickets
            if count_qty < 0 then plow_tickets
            if var_amt = 0 or variance = 0 then plow_tickets
            gosub print_line
            if core_on% = 1% and gvar$ = "Y" then                        ~
                                             gosub process_core_entries
            goto plow_tickets

        try_for_standard
            call "READ100" (#3, part$, f1%(3))
            if f1%(3) = 0% then return
            get #3 using L31920, method$
            if method$ = "S" or method$ = "T" or method$ = "F" then      ~
               call "STCCOSTS" (part$, " ", #13, 2%, tot_cost, cost())
            return

L31915:     FMT POS(573), PD(14,4)
L31920:     FMT POS(307), CH(1)
L31925:     FMT POS(112), CH(6)

        update_last_count_date
            call "READ101" (#3, part$, f1%(3))
            if f1%(3) = 0% then return
            if count_date$ = " " or count_date$ = blankdate$ ~
                               then count_date$ = session_date$
            put #3 using L31925, count_date$
            rewrite #3
            return

        update_locations
            if binloc$ = " " or fs%(5) < 0% then return
            filekey$ = str(session_nbr$) & str(part$) & str(store$) &    ~
                        str(lot$,,6%) & str(binloc$)
            picquan = 0                      /* Snapshot Quantity */
            call "READ100" (#17, filekey$, f1%(17%))
                if f1%(17%) = 0% then L31988
            get #17 using L32108, picquan
L31988:     locquan = 0                      /* Location Quantity */
            filekey$ = str(store$) & str(binloc$) & str(part$) & lot$
            call "READ101" (#5, filekey$, f1%(5%))
            if f1%(5%) = 0% then L32010
            get #5  using L31915, locquan
            lquan = lqty + locquan - picquan /* Adjust Pic to Current */
            put #5 using L31915, lquan
            rewrite #5                       /* File HNYLOCNS  */
            goto L32103
         /* New Location Record */
L32010:     lquan = lqty
            picquan = 0.0
            write #5 using L35580,               /* File HNYLOCNS  */     ~
                store$,     /* Warehouse or Stores                     */~
                binloc$,    /* Stock location                          */~
                part$,      /* Part code                               */~
                lot$,       /* Which lot in inventory - always used wit*/~
                text$(),    /* Text or Descriptive data - no edits, fre*/~
                store$,     /* Warehouse or Stores                     */~
                part$,      /* Part code                               */~
                lot$,       /* Which lot in inventory - always used wit*/~
                binloc$,    /* Stock location                          */~
                store$,     /* Warehouse or Stores                     */~
                part$,      /* Part code                               */~
                binloc$,    /* Stock location                          */~
                lot$,       /* Which lot in inventory - always used wit*/~
                part$,      /* Part code                               */~
                store$,     /* Warehouse or Stores                     */~
                binloc$,    /* Stock location                          */~
                lot$,       /* Which lot in inventory - always used wit*/~
                " ",        /* Stock location skid or pallet           */~
                lquan,      /* Quantity in packed decimal PD(14,4) form*/~
                userid$,    /* Last modified by User ID                */~
                date,       /* Date Record was last modified           */~
                store$,     /* Warehouse or Stores                     */~
                lot$,       /* Which lot in inventory - always used wit*/~
                part$,      /* Part code                               */~
                binloc$,    /* Stock location                          */~
                " "         /* Filler For Rest of Record or Internal Sp*/

L32103:     if lquan - picquan <> 0 then gosub write_location_audit
            return

L32108: FMT POS(45), PD(14,4)        /* Location Snapshot Quantity */

        adjustment_posting
            call"HNYAPST"(part$,         /* Part to be updated         */~
                      store$,            /* Store code                 */~
                      lot$,              /* Lot number                 */~
                      var_amt,           /* Quantity + or -            */~
                      costadj(),         /* 12 cost buckets            */~
                      hnyacct$(),        /* HNY Asset / Adj Account    */~
                      #4,                /* UFB Address of HNYQUAN     */~
                      #13,               /* UFB Address of SYSFILE2    */~
                      #14,               /* UFB Address of HNYPOOLS    */~
                      #3,                /* UFB Address of HNYMASTR    */~
                      returncode%)       /* Error Code Returned;       */

            if returncode% = 0% then return        /* Hunky Dorey !    */
            if gvar$ <> "Y" then return            /* Don't Post G/L   */
            str(gltext$,69)= "P.I. COST DIFFERENTIAL ADJMNT"
            variance2 = round(var_amt * tot_costadj, 2)
            if variance2 = 0 then return

            if export_on$ = "Y" then                                     ~
                put gl_post_info$() using L31544, "IPA01", -variance2, 0
            call"GLPOST2" (hnyasset$,    /* Account to be Updated      */~
                           0,            /* Debit Amount (0 if credit) */~
                           variance2,    /* Credit Amount (0 if debit) */~
                           hnydate$,     /* Date of Module Posting Date*/~
                           0%,           /* 0 means restricted to 3open*/~
                           modno$,       /* Source Module ID           */~
                           gltext$,      /* Reference text (100 Chars) */~
                                         /*   1st 30 chars = Reference1*/~
                                         /*   2nd 34 chars = Reference2*/~
                                         /*   3rd  4 chars = Adjust Flg*/~
                                         /*   Lst 32 chars = Descriptn */~
                           jnlid$,       /* Journal ID                 */~
                           pstseq%,      /* Posting Sequence Number    */~
                           userid$,      /* Who did the dirty deed     */~
                           division$,    /* (AWD001) division code     */~
                           #8,           /* UFB Address of GLMAIN file */~
                           #9,           /* UFB Address of GLDETAIL    */~
                           #13,          /* UFB Address of SYSFILE2    */~
                           #19,          /* (AWD001) GLORTRAN          */~
                           returncode%,  /* Error Code Returned        */~
                           " ",          /* Currency document ID       */~
                           gl_post_info$()) /* GL Posting Info         */


            call "GLPRTSUB" (modno$,     /* Source Module ID           */~
                           jnlid$,       /* Journal ID                 */~
                           pstseq%,      /* Posting Sequence Number    */~
                           userid$,      /* Who did the dirty deed     */~
                           hnydate$,     /* Date of Module Posting Date*/~
                           hnyasset$,    /* Account to be Updated      */~
                           gltext$,      /* Reference text (100 Chars) */~
                           0,            /* Debit Amount (0 if credit) */~
                           variance2,    /* Credit Amount (0 if debit) */~
                           #12,          /* UFB Address of HNYADJPF    */~
                           f2%(12))      /* File Open Status           */

            if export_on$ = "Y" then                                     ~
                put gl_post_info$() using L31544, "IPA02", variance2, 0
            call"GLPOST2" (hnyadj$,      /* Account to be Updated      */~
                           variance2,    /* Debit Amount (0 if credit) */~
                           0,            /* Credit Amount (0 if debit) */~
                           hnydate$,     /* Date of Module Posting Date*/~
                           0%,           /* 0 means restricted to 3open*/~
                           modno$,       /* Source Module ID           */~
                           gltext$,      /* Reference text (100 Chars) */~
                                         /*   1st 30 chars = Reference1*/~
                                         /*   2nd 34 chars = Reference2*/~
                                         /*   3rd  4 chars = Adjust Flg*/~
                                         /*   Lst 32 chars = Descriptn */~
                           jnlid$,       /* Journal ID                 */~
                           pstseq%,      /* Posting Sequence Number    */~
                           userid$,      /* Who did the dirty deed     */~
                           division$,    /* (AWD001) division code     */~
                           #8,           /* UFB Address of GLMAIN file */~
                           #9,           /* UFB Address of GLDETAIL    */~
                           #13,          /* UFB Address of SYSFILE2    */~
                           #19,          /* (AWD001) GLORTRAN          */~
                           returncode%,  /* Error Code Returned        */~
                           " ",          /* Currency document ID       */~
                           gl_post_info$()) /* GL Posting Info         */

            call "GLPRTSUB" (modno$,     /* Source Module ID           */~
                           jnlid$,       /* Journal ID                 */~
                           pstseq%,      /* Posting Sequence Number    */~
                           userid$,      /* Who did the dirty deed     */~
                           hnydate$,     /* Date of Module Posting Date*/~
                           hnyadj$,      /* Account to be Updated      */~
                           gltext$,      /* Reference text (100 Chars) */~
                           variance2,    /* Debit Amount (0 if credit) */~
                           0,            /* Credit Amount (0 if debit) */~
                           #12,          /* UFB Address of HNYADJPF    */~
                           f2%(12))      /* File Open Status           */

            return

        revalue_inventory
            gosub get_cur_qty_and_costs
            if f1%(4) = 0% then return
            if pos("STF " = costtype$) > 0% then return  /* No Standard*/
            if gvar$ <> "Y" then return            /* Don't Post G/L   */
            total9 = tot_c_cost - tot_cost    /* Capture $ - Current $ */
            str(gltext$,69)= "P.I. COST OVERRIDE ADJUSTMENT"
            variance9 = round(ohqty * total9, 2)
            if variance9 = 0 then return

            if export_on$ = "Y" then                                     ~
                put gl_post_info$() using L31544, "IPA01", -variance9, 0
            call"GLPOST2" (hnyasset$,    /* Account to be Updated      */~
                           0,            /* Debit Amount (0 if credit) */~
                           variance9,    /* Credit Amount (0 if debit) */~
                           hnydate$,     /* Date of Module Posting Date*/~
                           0%,           /* 0 means restricted to 3open*/~
                           modno$,       /* Source Module ID           */~
                           gltext$,      /* Reference text (100 Chars) */~
                                         /*   1st 30 chars = Reference1*/~
                                         /*   2nd 34 chars = Reference2*/~
                                         /*   3rd  4 chars = Adjust Flg*/~
                                         /*   Lst 32 chars = Descriptn */~
                           jnlid$,       /* Journal ID                 */~
                           pstseq%,      /* Posting Sequence Number    */~
                           userid$,      /* Who did the dirty deed     */~
                           division$,    /* (AWD001) division code     */~
                           #8,           /* UFB Address of GLMAIN file */~
                           #9,           /* UFB Address of GLDETAIL    */~
                           #13,          /* UFB Address of SYSFILE2    */~
                           #19,          /* (AWD001) GLORTRAN          */~
                           returncode%,  /* Error Code Returned        */~
                           " ",          /* Currency document ID       */~
                           gl_post_info$()) /* GL Posting Info         */

            if export_on$ = "Y" then                                     ~
                put gl_post_info$() using L31544, "IPA02", variance9, 0
            call"GLPOST2" (hnyadj$,      /* Account to be Updated      */~
                           variance9,    /* Debit Amount (0 if credit) */~
                           0,            /* Credit Amount (0 if debit) */~
                           hnydate$,     /* Date of Module Posting Date*/~
                           0%,           /* 0 means restricted to 3open*/~
                           modno$,       /* Source Module ID           */~
                           gltext$,      /* Reference text (100 Chars) */~
                                         /*   1st 30 chars = Reference1*/~
                                         /*   2nd 34 chars = Reference2*/~
                                         /*   3rd  4 chars = Adjust Flg*/~
                                         /*   Lst 32 chars = Descriptn */~
                           jnlid$,       /* Journal ID                 */~
                           pstseq%,      /* Posting Sequence Number    */~
                           userid$,      /* Who did the dirty deed     */~
                           division$,    /* (AWD001) division code     */~
                           #8,           /* UFB Address of GLMAIN file */~
                           #9,           /* UFB Address of GLDETAIL    */~
                           #13,          /* UFB Address of SYSFILE2    */~
                           #19,          /* (AWD001) GLORTRAN          */~
                           returncode%,  /* Error Code Returned        */~
                           " ",          /* Currency document ID       */~
                           gl_post_info$()) /* GL Posting Info         */

            if variance9 > 0 then L32756
               debit$ = hnyasset$ : credit$ = hnyadj$
               goto L32758
L32756:        debit$ = hnyadj$ : credit$ = hnyasset$
L32758:     variance = variance9
            save_amt = var_amt
            var_amt = ohqty
            gosub print_line
            var_amt = save_amt
            return

        get_cur_qty_and_costs
            filekey$ = str(part$) & str(store$) & lot$
            call "READ101" (#4, filekey$, f1%(4))
            if f1%(4) = 0% then return
            get #4 using L32816, ohqty, tot_c_cost, cur_cost(), costtype$
            if pos("STF" = costtype$) > 0% then return /*Standard Cost?*/
            call "PACKZERO" (cost(), costs$)
            put #4 using L32820, tot_cost, costs$
            rewrite #4
            gosub replace_pool_records
            return
L32816:     FMT POS(69), PD(14,4), POS(117),13*PD(14,4), POS(403), CH(1)
L32820:     FMT POS(117), PD(14,4), CH(96)

        replace_pool_records
            str(filekey$,35%) = all(hex(00))
            call "DELETE" (#14, filekey$, 34%)
            if ohqty = 0 then return
            poolseq% = 10000%
            if pos("PTY"=costtype$) > 0% then poolseq% = 1%  /* FIFO */
            pooltext$ = "P.I. OVERRIDE"
L32852:     put #14 using L35860,                                         ~
            part$,          /* Part code                               */~
            store$,         /* Warehouse or Store                      */~
            lot$,           /* Which lot in inventory - always used wit*/~
            poolseq%,       /* Reverse sequence number                 */~
            ohqty,          /* The quantity left in a LIFO/FIFO pool re*/~
            ohqty,          /* The quantity originally in a LIFO/FIFO p*/~
            tot_cost,       /* Total Inventory Cost                    */~
            costs$,         /* 12 cost buckets                         */~
            date,           /* Transaction date                        */~
            hnyasset$,      /* Inventory Asset Account                 */~
            hnyadj$,        /* inventory adjustment gl                 */~
            pooltext$,      /* Posting text                            */~
            " "             /* Filler                                  */
            write #14, eod goto L32914
            return
L32914:     if pos("PTY"=costtype$) > 0% then poolseq% = poolseq% + 1%   ~
                                         else poolseq% = poolseq% - 1%
            goto L32852

        update_cycle_count
            gosub get_after_values
            gosub load_cc_master_record
            cc_var_amt  = cc_afterquan - cc_beforequan
            cc_variance = (cc_afterval * cc_afterquan ) -                ~
                                           (cc_beforeval * cc_beforequan)
            if qoh = 0.0 then varprcnt = 999.9    /*Calc Tol. Hit Cond */~
                         else varprcnt = 100.0 * (cc_var_amt / qoh)
            if abs(varprcnt) <= cnttlernper or                           ~
               abs(cc_var_amt)<= cnttlernqty                             ~
                                    then tolhit = 1.0   else tolhit = 0.0
            cumtolhit = cumtolhit + tolhit
            if cc_var_amt < 0.0                                          ~
                             then cumcntdltam = cumcntdltam + cc_var_amt ~
                             else cumcntdltap = cumcntdltap + cc_var_amt
            cumbohqty = cumbohqty + qoh  :  cntnbr = cntnbr + 1.0
            actflag$ = "C"
            gosub set_nextcntdate

            /* Update Cycle Count Master File */
            put #23  using L36110, count_date$, cntnbr, actflag$,         ~
                                  nextcntdate$, cumtolhit, cumcntdltap,  ~
                                  cumcntdltam, cumbohqty, firstcntdate$, ~
                                  count_qty
            rewrite #23

            readkey$ = str(ccsesname$) & "A" &  str(part$) & str(store$) ~
                       & lot$

            call "READ100" (#24, readkey$, f1%(24)) /* CC Detail File */
            if f1%(24) = 0% then return    /* Shouldn't Happen */
            get #24 using L36390, ccsrcflag$, countdate$, ccenterby$,     ~
                                 count_qty,unitcost,temp     ,temp      ,~
                                 cnttlernper, cnttlernqty,lastcntdate$,  ~
                                 varreason$, cccntby$
            call "DELETE" (#24, readkey$, 57%)
            str(readkey$,13,1) = "C"
            put #24  using L36240, readkey$,                              ~
                                 ccsrcflag$, countdate$, ccenterby$,     ~
                                 count_qty,unitcost,cc_var_amt,          ~
                                 cc_variance, cnttlernper, cnttlernqty,  ~
                                 lastcntdate$, cntperiod$, " ",          ~
                                 varreason$, cccntby$,  " "
            write #24
            return

        write_location_audit
            if locauditflag$ <> "Y" then return        /* Audit Not On */
            datetime$ = " "
            call "GETDTTM" addr(datetime$)
            fielda$ = hex(00)         /* For Use with Report Writers */
            loctranstype$ = "8"       /* '8' = PI Posting Variance */
            multflag$ = " "           /* Not a Multiple Adjustment */

            write #18 using L36480,                  /* File LOCAUDIT   */~
                store$,           /* Warehouse or Stores               */~
                binloc$,          /* Stock location                    */~
                part$,            /* Part code                         */~
                lot$,             /* Which lot in inventory            */~
                fielda$,          /* Filler = HEX(00)                  */~
                datetime$,        /* System DateTime Stamp             */~
                " ",              /* Filler                            */~
                loctranstype$,    /* Audit File Transaction Code       */~
                multflag$,        /* Multiple Transaction Flag         */~
                lquan,            /* New Quantity                      */~
                picquan,          /* Original Quantity                 */~
                binloc$,          /* Stock location        (Original)  */~
                store$,           /* Warehouse or Stores   (Original)  */~
                lot$,             /* Which lot in inventory(Original)  */~
                userid$,          /* User Identificaltion              */~
                part$,            /* Part code                         */~
                store$,           /* Warehouse or Stores               */~
                lot$,             /* Which lot in inventory            */~
                binloc$,          /* Stock location                    */~
                " "               /* Filler                            */

            return
        set_nextcntdate
            cntnbr% = int(cntnbr)
            cntperiod% = int(cntperiod)
            if firstcntdate$ = " " or firstcntdate$ = blankdate$ ~
                                   then firstcntdate$ = date2$
            days% = cntnbr% * cntperiod%
            call "DATE" addr ("G+", firstcntdate$, days%, nxdate1$, err%)
            call "DATE" addr ("G+", date2$,   cntperiod%, nxdate2$, err%)
            if nxdate1$ < nxdate2$ then nextcntdate$ = nxdate2$          ~
                                   else nextcntdate$ = nxdate1$
            return


        close_cycle_count_session
            readkey$ = str(session_nbr$)
            str(readkey$,3,) =  all(hex(00))
            call "REDALT2" (#26, readkey$, 2%, f1%(26))
L33218:     if f1%(26) = 0% then return    /* Shouldn't Happen */
            if str(key(#26, 2%),,2) <> session_nbr$ then L33238
            if str(key(#26, 1%),,1) = "A" then L33223
            call "READNEXT" (#26, f1%(26))
            goto L33218
L33223:     get #26 using L33246, ccsesname$, ccsesdesc$, actflag$,       ~
                    ccsesname$,  session_nbr$, ccsystime$, countdate$

            call "DELETE" (#26, key(#26, 0%), 12%)

            actflag$ = "C"

            put #26 using L33246, ccsesname$, ccsesdesc$, actflag$,       ~
                    ccsesname$,  session_nbr$, ccsystime$, countdate$
            write #26 , eod goto L33240
            return
L33238:     ccsesname$ = " "     /* shouldn't happen */
L33240:     return

            /* HNYCCSYS */
L33246: FMT   CH(12), CH(30), CH(1), CH(12), CH(2), CH(8), CH(6), CH(178)

        reset_ccssn_for_void_tkt
          /* Reset Master Record Activity Flag to Closed */
            readkey$ = str(part$) & str(store$) & lot$
            call "READ101" (#23, readkey$, f1%(23%))
            if f1%(23%) = 0%  then return  /* Shouldn't happen */
            put #23 using L33264, "C"
L33264:    FMT POS(81), CH(1)
            rewrite #23
          /* Delete Record from Session Detail  */
            readkey$ = str(ccsesname$) & "A" &  str(part$) & str(store$) ~
                       & lot$
            call "DELETE" (#24, readkey$, 57%)

            return


        set_ccsession
            readkey$ = str(session_nbr$)
            str(readkey$,3,) =  all(hex(00))
            call "REDALT2" (#26, readkey$, 2%, f1%(26))
L33345:     get #26 using L33375, ccsesname$, actflag$ ,tempses$
            if tempses$ <> session_nbr$ then L33362   /* Big Trouble If */
            if actflag$ <> "A" then L33360            /* Try Another */
                return

L33360:     call "READNEXT" (#26, f1%(26))
            if f1%(26%) <> 0% then L33345    /* Trouble if = 0% */
L33362:     ccsesname$ = " "
            return


L33375:     FMT CH(12), POS(43), CH(1), POS(56), CH(2)

        get_before_values
            cc_beforequan, cc_beforeval = 0.0
            readkey$ = str(part$) & str(store$) & lot$
            call "READ100" (#4, readkey$, f1%(4))
            if f1%(4) = 0 then return  /* Shouldn't happen */
            get #4 using L33480, cc_beforequan, cc_beforeval
            return

        get_after_values
            cc_afterquan, cc_afterval = 0.0
            readkey$ = str(part$) & str(store$) & lot$
            call "READ100" (#4, readkey$, f1%(4))
            if f1%(4) = 0 then return  /* Shouldn't happen */
            get #4 using L33480, cc_afterquan, cc_afterval
            return

L33480:     FMT POS(69), PD(14,4), POS(117), PD(14,4)

        REM *************************************************************~
            *   P R I N T   A   L I N E   O F   T H E   J O U R N A L   *~
            *************************************************************

        print_line

            call "DESCRIBE" (#3, part$, description$, 0%, f1%(3))
                call "CONVERT" (var_amt, 2.2, units$)
                call "CONVERT" (abs(variance), 2.2, variance$)

                if line% > 56% then gosub page_control1
                rcpacct$(1) = debit$
                rcpacct$(2) = credit$
                call "GLFMT" (rcpacct$(1))
                call "GLFMT" (rcpacct$(2))
                print using L33855
                print using L33810,description$,store$,units$,rcpacct$(1),~
                                                              variance$
                print using L33825, part$, lot$, rcpacct$(2), variance$
                line% = line% + 3%
            gosub'162(debit$ , abs(variance))
            gosub'163(credit$, abs(variance))
            return

        REM *************************************************************~
            *       P A G E    C O N T R O L    -    J O U R N A L      *~
            *************************************************************

        page_control1

            if page% = 0% then L33665
            print using L33855
            print using L33840
L33665:     print page
            page% = page% + 1
            print using L33730, date$, rpt_time$, company$, page%
            print using L33745
            print using L33760, title$
            print using L33775, modno$, jnlid$, pstseq%
            print
            print using L33840
            print using L33790
            print using L33840
            line% = 8%
            return

L33730: %######## ########                 ##############################~
        ~##############################                              PAGE:~
        ~###
L33745: %HNYPIPST                                      P H Y S I C A L   ~
        ~I N V E N T O R Y                                        RPT: HNY~
        ~010
L33760: %                                  ##############################~
        ~##############################

L33775: %                             (G/L Module = ##  Journal ID = ### ~
        ~ Posting Sequence Number =########)

L33790: %   !    P A R T    I N F O R M A T I O N      ! L O C A T I O N ~
        ~ ! U N I T S  !  A C C O U N T S  !   D E B I T  !  C R E D I T !


L33810: %   ! PART:  ################################  !   STORE:  ###   ~
        ~ ! ########## !  ################ !  ##########  !              !

L33825: %   ! CODE:  #########################         !   LOT: ######   ~
        ~ !            !  ################ !              !  ##########  !

L33840: %   +------------------------------------------+-----------------~
        ~-+------------+-------------------+--------------+--------------!

L33855: %   !                                          !                 ~
        ~ !            !                   !              !              !

L33858: %   ! PART:  ################################  !   STORE:  ###   ~
        ~ ! ########## !  ################ !  ##########  !  ##########  !

L33861: %   ! CODE:  #########################         !   LOT: ######   ~
        ~ !            !  ################ !  ##########  !  ##########  !

L33864: %   ! ######################################## !                 ~
        ~ !            !  ################ !  ##########  !  ##########  !

        REM *************************************************************~
            *        S T A C K   P U S H I N G   R O U T I N E S        *~
            *                                                           *~
            * PUSHES THE INDICATED INFORMATION ONTO THE DESIRED STACK   *~
            * FOR LATER PROCESSING.                                     *~
            *************************************************************

            deffn'162(account$, amount)  /* RECAP DEBITS ACCUMULATOR   */
                  search str(debitsstk$(),1) = str(account$,,9%)         ~
                               to location$ step 9 /* FIND ACCOUNT #   */
                  if location$ = hex(0000) then L33945  /* PUSH NEW ITEM*/
                     junk% = int(val(location$,2)/9)+1 /* WHICH CELL?  */
                     debitsstk(junk%) = debitsstk(junk%) + amount
                               /* UPDATE AMOUNT FOR EXISTING ACCOUNT   */
                     return
L33945:           REM PUSH NEW ITEM ONTO STACK.
                      debitsptr% = debitsptr% + 1
                      debitsstk$(debitsptr%) = account$
                      debitsstk(debitsptr%) = amount
                      return

            deffn'163(account$, amount)  /* RECAP CREDITS ACCUMULATOR  */
                  search str(creditsstk$(),1) = str(account$,,9%)        ~
                               to location$ step 9
                               /* SCAN ONLY WHAT IS USED OF STACK      */
                  if location$ = hex(0000) then L34020  /* IF NO ==> NEW*/
                     junk% = int(val(location$,2)/9)+1 /* WHICH ELEMENT*/
                     creditsstk(junk%) = creditsstk(junk%) + amount
                                         /* UPDATE AMT FOR THAT ELEMENT*/
                     return
L34020:           REM PUSH NEW ENTRY ONTO SALES ACCOUNT STACK.
                      creditsptr% = creditsptr% + 1
                      creditsstk$(creditsptr%) = account$
                      creditsstk(creditsptr%) = amount
                      return

        REM *************************************************************~
            *         P R I N T   D A I L Y   R E C A P   I N F O       *~
            *                                                           *~
            * TAKES THE CONTENTS OF THE VARIOUS STACKS AND POSTS THEM TO*~
            * THE DAILY RECAP.  SHOWS OFF JUST FOR FUN.  WILL NOT PRINT *~
            * ANYTHING WHERE THE AMOUNT WAS ZERO OR THE STACK WAS EMPTY *~
            *************************************************************

        print_recap

            if debitsptr% = 0 and creditsptr% = 0 then return
               print using L33855
               print using L33840
               totaldebits, totalcredits, colsdone% = 0
               mat rcpline% = con
               mat rcpptr% = zer
               gosub page_control2

L34145:     for column% = 1 to 2
                on column% gosub L34180, L34365
                next column%
                print                    /* FREE UP LINE.              */
            if  colsdone% >= 2 then return         /* DONE W/ REPORT   */
                goto L34145

L34180:     REM HANDLES LEFT (DEBITS) COLUMN FOR REPORT
                on rcpline%(1) gosub L34200, L34275, L34295, L34315, L34335
                return

L34200:         REM PRINT CONTENTS OF DEBITS STACK, IF ANY.
                    if debitsptr% = 0 then L34295
                    rcpptr%(1) = rcpptr%(1) + 1
                    rcpamt(1)=debitsstk(rcpptr%(1))
                    rcpacct$(1)=debitsstk$(rcpptr%(1))
                    REM GET ACCOUNT DESCRIPTION
                        call "DESCRIBE" (#8,rcpacct$(1),rcpacctdescr$(1),~
                                                   0%, f1%(8))
                    call "GLFMT" (rcpacct$(1))
                    print using L34670,rcpacct$(1),rcpacctdescr$(1),      ~
                                         rcpamt(1);
                    totaldebits=totaldebits+debitsstk(rcpptr%(1))
                    if rcpptr%(1) < debitsptr% then return
                       rcpline%(1) = 2
                       return
L34275:         REM PRINTS SEPARATOR LINE.
                    print using L34665, "*--";
                    rcpline%(1) = 3
                    return
L34295:         REM PRINTS TOTAL LINE
                    print using L34675, totaldebits;
                    rcpline%(1) = 4
                    return
L34315:         REM PRINTS STARS
                    print using L34660, "*";
                    rcpline%(1) = 5
                    return
L34335:         REM SETS TO BLANKS AS COLUMN IS DONE.
                    rcpacct$(1), rcpacctdescr$(1) = " "
                    colsdone% = colsdone% + 1
                    rcpline%(1) = 6
                    return

L34365:     REM HANDLES RIGHT HAND COLUMN--CREDITS.
                print tab(70);
                on rcpline%(2) gosub L34385, L34455, L34475, L34495, L34515
                   return
L34385:         REM PRINT THE CREDITS STACK.
                    if creditsptr% = 0 then L34475
                    rcpptr%(2) = rcpptr%(2) + 1
                    rcpamt(2)=creditsstk(rcpptr%(2))
                    rcpacct$(2)=creditsstk$(rcpptr%(2))
                    call "DESCRIBE" (#8, rcpacct$(2), rcpacctdescr$(2),  ~
                                             0%, f1%(8))
                    call "GLFMT" (rcpacct$(2))
                    print using L34670,rcpacct$(2),rcpacctdescr$(2),      ~
                               rcpamt(2);
                    totalcredits=totalcredits+creditsstk(rcpptr%(2))
                    if rcpptr%(2) < creditsptr% then return
                       rcpline%(2) = 2
                       return
L34455:         REM PRINT SEPARATOR LINE
                    print using L34665, "*--";
                    rcpline%(2) = 3
                    return
L34475:         REM PRINT TOTAL CREDITS LINE
                    print using L34680, totalcredits;
                    rcpline%(2) = 4
                    return
L34495:         REM PRINT STARS
                    print using L34660,"*";
                    rcpline%(2) = 5
                    return
L34515:         REM BLANK--PASS...
                    rcpline%(2) = 6
                    colsdone% = colsdone% + 1
                    rcpacct$(2), rcpacctdescr$(2) = " "
                    return

        REM *************************************************************~
            *          P A G E   C O N T R O L   -  R E C A P           *~
            *************************************************************

        page_control2

            REM PAGE CONTROL SUBROUTINE FOR PRINTING DAILY RECAP
                   print page
                   call "STRING" addr("LJ", title$, 60%)
                   title$ = title$ & " SESSION RECAP"
                   call "STRING" addr("CT", title$, 60%)
                   rcppage% = rcppage% + 1%
                   print using L33730, date$, rpt_time$, company$,rcppage%
                   print using L33745
                   print using L33760, title$
                   print using L33775, modno$, jnlid$, pstseq%
                   print
                   print using L34695
                   print
                   print using L34715
                   print using L34735,"#","#"
                   print using L34755
                   return

L34660: %**************************#***********************************
L34665: %#--------------+--------------------------------+------------*
L34670: %* ############ ! ############################## !-#######.## *
L34675: %*              ! TOTAL DEBITS                   !-#######.## *
L34680: %*              ! TOTAL CREDITS                  !-#######.## *


L34695: %=========================D E B I T S==========================  ~
        ~     ===========================C R E D I T S====================~
        ~==

L34715: %**************************************************************  ~
        ~     ************************************************************~
        ~**

L34735: %* ACCOUNT #    !     D E S C R I P T I O N      !   AMOUNT   *  ~
        ~     * ACCOUNT #    !     D E S C R I P T I O N      !   AMOUNT  ~
        ~ *

L34755: %*--------------+--------------------------------+------------*  ~
        ~     *--------------+--------------------------------+-----------~
        ~-*

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35040: FMT                 /* FILE: HNYPISYS                          */~
            CH(6),          /* Date something was counted              */~
            CH(2),          /* Number corresponding to a Inventory Coun*/~
            CH(30),         /* Generic for general code descriptions   */~
            XX(3),          /* Filler                                  */~
            18*CH(3),       /* Warehouse or Stores                     */~
            18*CH(4),       /* category code                           */~
            6*CH(25),       /* Part code                               */~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(3),          /* 1 to 3 character prefix to the P.I. Tick*/~
            CH(06),         /* Ticket Number to a Physical Inventory Co*/~
            CH(06),         /* Last ticket number generated for a Count*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(6),          /* Number of extra Physical Inventory Ticke*/~
            CH(1),          /* Flag controlling what sequence tickets w*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(6),          /* Date a transaction was entered          */~
            CH(6),          /* System Date                             */~
            CH(6),          /* The System Time when a transaction was e*/~
            CH(1),          /* All Parts Accounted for Flag            */~
        /*  CH(5),          /* ABC Category                            */~
            XX(5),          /* Jump Over ABC Catagory                  */~
            CH(1)           /* Cycle Count Session Flag                */~
        /*  CH(141)         /* Filler For Rest of Record or Internal Sp*/

L35300: FMT                 /* FILE: HNYPITKT                          */~
            CH(2),          /* Number corresponding to a Inventory     */~
                            /* Count Session                           */~
            CH(12),         /* Ticket Number to a Physical Inventory   */~
                            /* Count Item                              */~
            BI(1),          /* A number decremented for each recount   */~
                            /* (99=original                            */~
            CH(25),         /* Part Number                             */~
            CH(3),          /* Store or Warehouse Code                 */~
                            /* Warehouse code                          */~
            CH(16),         /* Lot Number                              */~
            CH(8),          /* bin location                            */~
                            /* Actual Bin Location (from HNYQUAN file  */~
                            /* or HNYLOCNSfile)                        */~
            CH(1),          /* Extra (supplemental) Ticket Flag: "X" = */~
                            /* extra, blank = not an extra             */~
            CH(1),          /* Flag or switch which controls posting   */~
                            /* to G/L                                  */~
            CH(1),          /* Flag or switch which controls posting   */~
                            /* to Inventory                            */~
            CH(1),          /* Override flag / indicator               */~
                            /* = "Y" if cost(s) are manually           */~
                            /* overridden                              */~
            CH(20),         /* Name of person who counted something    */~
            CH(3),          /* user-id of specific user                */~
                            /* User Id of Data Entry Operator - 1 for  */~
                            /* each count                              */~
            CH(6),          /* Date something was counted              */~
            CH(6),          /* Date a transaction was entered          */~
                            /* Date entered into computer              */~
            CH(6),          /* The System Time when a transaction was  */~
                            /* entered                                 */~
            PD(14,4),       /* Actual Quantity On Hand according to    */~
                            /* the count. If < 0 then ticket is VOID   */~
            POS(201),       /*                                         */~
            PD(14,4),       /* Quantity From HNYLOCNS file (if         */~
                            /* applicable)                             */~
            13*PD(14,4)     /* 12 Cost Buckets             User        */~
                            /* and Total Standard Cost      Over-rides */

L35580: FMT                 /* FILE: HNYLOCNS                          */~
            CH(3),          /* Warehouse or Stores                     */~
            CH(08),         /* Stock location                          */~
            CH(25),         /* Part code                               */~
            CH(6),          /* Which lot in inventory - always used wit*/~
            8*CH(50),       /* Text or Descriptive data - no edits, fre*/~
            CH(3),          /* Warehouse or Stores                     */~
            CH(25),         /* Part code                               */~
            CH(6),          /* Which lot in inventory - always used wit*/~
            CH(8),          /* Stock location                          */~
            CH(3),          /* Warehouse or Stores                     */~
            CH(25),         /* Part code                               */~
            CH(8),          /* Stock location                          */~
            CH(6),          /* Which lot in inventory - always used wit*/~
            CH(25),         /* Part code                               */~
            CH(3),          /* Warehouse or Stores                     */~
            CH(8),          /* Stock location                          */~
            CH(6),          /* Which lot in inventory - always used wit*/~
            CH(4),          /* Stock location skid or pallet           */~
            PD(14,4),       /* Quantity in packed decimal PD(14,4) form*/~
            CH(3),          /* Last modified by User ID                */~
            CH(6),          /* Date Record was last modified           */~
            CH(3),          /* Warehouse or Stores                     */~
            CH(6),          /* Which lot in inventory - always used wit*/~
            CH(25),         /* Part code                               */~
            CH(8),          /* Stock location                          */~
            CH(69)          /* Filler For Rest of Record or Internal Sp*/

L35860: FMT                 /* FILE: HNYPOOL                           */~
            CH(25),         /* Part code                               */~
            CH(3),          /* Warehouse or Store                      */~
            CH(6),          /* Which lot in inventory - always used wit*/~
            BI(4),          /* Reverse sequence number                 */~
            PD(14,4),       /* The quantity left in a LIFO/FIFO pool re*/~
            PD(14,4),       /* The quantity originally in a L/F pool   */~
            PD(14,4),       /* Total Inventory Cost                    */~
            CH(96),         /* 12 cost buckets                         */~
            CH(6),          /* Transaction date                        */~
            CH(9),          /* Inventory Asset Account                 */~
            CH(9),          /* inventory adjustment gl                 */~
            CH(40),         /* Posting text                            */~
            CH(78)          /* Filler                                  */

L36010: FMT                 /* FILE: HNYPICST                          */~
            POS(47),        /* Position for Field ON-HAND              */~
            PD(14,4),       /* quantity on-hand                        */~
                            /* Frozen qty-on-hand from HNYQUAN record. */~
                            /* This is the 'snapshot' qty-on-hand.     */~
            13*PD(14,4)     /* 12 cost buckets and total cost          */~
                            /*         These are the frozen or         */~
                            /* 'snapshot' cost fields                  */

L36110: FMT                              /* FILE: HNYCCMST             */~
            POS(51),                     /* Position for Date          */~
            CH(6),                       /* Count Date                 */~
            POS(73),                     /* Position for Count Number  */~
            PD(14,4),                    /* Number of Counts           */~
            CH(1),                       /* Activity Flag              */~
            CH(6),                       /* Next Count Date            */~
            POS(94),                     /* Position for Cum Tol. Hits */~
            PD(14,4),                    /* Cumulative Tolerance Hits  */~
            PD(14,4),                    /* Cumulative Count Delta (+) */~
            PD(14,4),                    /* Cumulative Count Delta (-) */~
            PD(14,4),                    /* Cumulative Book Quantity   */~
            POS(196),                    /* Filler                     */~
            CH(6),                       /* First Cycle Count Date     */~
            POS(410), PD(14,4)           /* Last Count Quantity        */
                /* Cycle Count Session detail File */
L36240: FMT                              /* FILE: HNYCCDTL             */~
            CH(57),                      /* Primary Key                */~
            CH(1),                       /* Source Flag                */~
            CH(6),                       /* Count Date                 */~
            CH(3),                       /* Entered By ID              */~
            PD(14,4),                    /* Count Quantity             */~
            PD(14,4),                    /* Part Value (Cost)          */~
            PD(14,4),                    /* Variance Quantity          */~
            PD(14,4),                    /* Variance Value             */~
            PD(14,4),                    /* Count Tolerance Percent    */~
            PD(14,4),                    /* Count Tolerance Quantitiy  */~
            CH(6),                       /* Last Count Date            */~
            CH(3),                       /* Count Period               */~
            CH(6),                       /* Filler                     */~
            CH(6),                       /* Variance Reason Code       */~
            CH(12),                      /* Count by ID                */~
            CH(288)                      /* Filler                     */

L36390: FMT                              /* FILE: HNYCCDTL             */~
            POS(58),                     /* Position for Source Flag   */~
            CH(1),                       /* Source Flag                */~
            CH(6),                       /* Count Date                 */~
            CH(3),                       /* Entered By                 */~
            PD(14,4),                    /* Count Unit Quantity        */~
            PD(14,4),                    /* Count Unit Value           */~
            PD(14,4),                    /* Unit Variance              */~
            PD(14,4),                    /* Value Variance             */~
            PD(14,4),                    /* Count Tolerance Percent    */~
            PD(14,4),                    /* Count Tolerance Quantity   */~
            CH(6),                       /* Last Count Date            */~
            POS(131),                    /* Position for Variance Reasn*/~
            CH(6),                       /* Variance Reason Code       */~
            CH(12)                       /* Counted By                 */

L36480: FMT                                         /* File LOCAUDIT   */~
            CH(3),                /* Warehouse or Stores               */~
            CH(8),                /* Stock location                    */~
            CH(25),               /* Part code                         */~
            CH(6),                /* Which lot in inventory            */~
            CH(1),                /* Filler = HEX(00)                  */~
            CH(7),                /* System DateTime Stamp             */~
            CH(6),                /* Filler                            */~
            CH(1),                /* Audit File Transaction Code       */~
            CH(1),                /* Multiple Transaction Flag         */~
            PD(14,4),             /* New Quantity                      */~
            PD(14,4),             /* Original Quantity                 */~
            CH(8),                /* Stock location        (Original)  */~
            CH(3),                /* Warehouse or Stores   (Original)  */~
            CH(6),                /* Which lot in inventory(Original)  */~
            CH(3),                /* User Identificaltion              */~
            CH(25),               /* Part code                         */~
            CH(3),                /* Warehouse or Stores               */~
            CH(6),                /* Which lot in inventory            */~
            CH(8),                /* Stock location                    */~
            CH(34)                /* Filler                            */

        load_gl_info

            gosub load_part_info
            put str(gl_post_info$(),,) using L37490,                      ~
                tran_type$,              /* Transaction Type CH(5)     */~
                " ",                     /* Currency code CH(4)        */~
                0,                       /* Transaction Currency amount*/~
                0,                       /* Functional Currency amount */~
                var_amt,                 /* Unit amount                */~
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
                part$,                   /* Part Number CH(25)         */~
                partcat$,                /* Part Category CH(4)        */~
                partclass$,              /* Part Class CH(4)           */~
                partgen$,                /* Part Generic code CH(16)   */~
                parttype$,               /* Part Type CH(3)            */~
                uom$,                    /* Part UOM CH(4)             */~
                store$,                  /* Store Number CH(3)         */~
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

L37490: FMT     CH(5),                   /* Transaction Type CH(5)     */~
                CH(4),                   /* Currency code CH(4)        */~
                PD(15,7),                /* Transaction Currency amount*/~
                PD(15,4),                /* Functional Currency amount */~
                PD(15,4),                /* Unit amount                */~
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
                CH(191)                  /* Filler                     */

        load_part_info

            call "READ100" (#3, part$, f1%(3))
                 if f1%(3) = 0% then return /* Shouldn't happen */
            get #3 using L37990, partgen$, uom$, partcat$, partclass$,    ~
                                parttype$
L37990:     FMT POS(58), CH(16), CH(4), POS(90), CH(4), POS(133), CH(4), ~
                POS(180), CH(3)
            return

        load_cc_master_record
            readkey$ = str(part$) & str(store$) & lot$
            call "READ101" (#23, readkey$, f1%(23))
            if f1%(23) = 0%  then return  /* Shouldn't happen */

            get #23  using L38120, countdate$, cnttlernper, cnttlernqty,  ~
                              cntnbr, recdate$, cumtolhit, cumcntdltap,  ~
                              cumcntdltam, cumbohqty, cntperiod$,        ~
                              firstcntdate$

            convert cntperiod$ to cntperiod, data goto L38160

L38120:     FMT  POS(51), CH(6), PD(14,4), PD(14,4), PD(14,4), POS(88),  ~
                 CH(6), PD(14,4), PD(14,4), PD(14,4), PD(14,4), POS(181),~
                 CH(3), POS(196), CH(6)

L38160:     return

        process_core_entries
            init (" ")  core_acct$()
            mat core_dbcr = zer
            core_cntr% = 0%
            misckey$ = " "
L38220:     call "PLOWNEXT" (#27, misckey$, 0%, f1%(27%))
               if f1%(27%) = 0% then L38560
            if gvar$ <> "Y" then L38510
                core_cntr% = core_cntr% + 1%
                get #27 using L38242, glacct$, gltext$, damt, camt
L38242:              FMT XX(25), CH(9), CH(100), 2*PD(14,4)
                if core_cntr% > 1% then L38250
                     get #27, using L38246, part$, store$, lot$, var_amt, ~
                                           tot_cost, variance
L38246:                   FMT POS(157), CH(25), CH(3), CH(16), 3*PD(14,4)
                     store$ = "***"  :  lot$ = "******"
                     if export_on$ = "Y" then gosub load_part_info
L38250:         core_acct$(core_cntr%) = glacct$
                core_dbcr(core_cntr%, 1%) = damt
                core_dbcr(core_cntr%, 2%) = camt
               tran_type$ = "IPA05"
               if str(gltext$,67,1) = "H" then L38310
               str(tran_type$,4,2) = "04"
               if str(gltext$,65,1) = "S" then L38310
               str(tran_type$,4,2) = "06"
L38310:        if export_on$ = "Y" then                                  ~
                     put gl_post_info$() using L38540, tran_type$,        ~
                              (damt-camt), 0
               call "GLPOST2" (glacct$, damt, camt, hnydate$, 0%,        ~
                               modno$, gltext$, jnlid$, pstseq% ,        ~
                               userid$, division$, #8, #9, #13, #19,     ~
                               /* (AWD001)  */ returncode%, " ",         ~
                               gl_post_info$()) 

L38510:        call "READ101" (#27, str(misckey$,1%,19%), f1%(27%))
               delete #27
               goto L38220
L38540:        FMT CH(5), POS(18), 2*PD(15,4)

L38560:     if gvar$ <> "Y" then return
            if core_cntr% = 0% then return

*        Now print the accumulated core entries to the journal
            call "DESCRIBE" (#3, part$, description$, 0%, f1%(3%))
            call "CONVERT" (var_amt, 2.2, units$)
            if line% > 56% then gosub page_control1
            print using L33855
            line% = line% + 1%
            if core_cntr% < 3% then core_cntr% = 3%

            for i% = 1% to core_cntr%
                init (" ")  rcpacct$(1%), core_debit$, core_credit$
                if core_acct$(i%) = " " then L38680
                rcpacct$(1%) = core_acct$(i%)
                call "GLFMT" (rcpacct$(1%))
L38680:         if core_dbcr(i%, 1%) = 0 then L38700
                     call "CONVERT" (core_dbcr(i%, 1%), 2.2, core_debit$)
                     gosub'162 (core_acct$(i%), core_dbcr(i%, 1%))
L38700:         if core_dbcr(i%, 2%) = 0 then L38720
                     call "CONVERT" (core_dbcr(i%, 2%), 2.2, core_credit$)
                     gosub'163 (core_acct$(i%), core_dbcr(i%, 2%))
L38720:         if line% > 56% then gosub page_control1
                if i% = 1% then print using L33858, description$, store$, ~
                                units$, rcpacct$(1%), core_debit$,       ~
                                core_credit$
                if i% = 2% then print using L33861, part$, lot$,          ~
                                rcpacct$(1%), core_debit$, core_credit$
                if i% = 3% then print using L33864, "Core Value:  G/L " & ~
                                "Transaction Only", rcpacct$(1%),        ~
                                core_debit$, core_credit$
                if i% > 3% then print using L33864, " ", rcpacct$(1%),    ~
                                core_debit$, core_credit$
                line% = line% + 1%
                next i%
            return


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document input screen.                                    *~
            *************************************************************

            deffn'101(fieldnr%)
                  line2$ = "and End the Count Session"
                  str(line2$,62%) = "HNYPIPST: " & str(cms2v$,,8%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40160          /* Count Session Num*/
                  goto L40230

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40160:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40230:     accept                                                       ~
               at (01,02),                                               ~
                  "Post Physical Inventory Variances",                   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,26), "INVENTORY POSTING DATE =",                   ~
               at (03,51), fac(hex(84)), hnydate2$              , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02),                                               ~
        "  This program will update, or 'post' the quantity and dollar va~
        ~riances for   ",                                                 ~
               at (06,02),                                               ~
        "  the given inventory session.  Please note that the quantities ~
        ~and costs used",                                                 ~
               at (07,02),                                               ~
        "  to compute variances are those which were captured or 'frozen'~
        ~ earlier, that",                                                 ~
               at (08,02),                                               ~
        "  reflect the book (file) quantities and costs as of the date of~
        ~ the Physical.",                                                 ~
               at (09,02),                                               ~
        "  Please also check the posting date shown above to verify that ~
        ~you will be   ",                                                 ~
               at (10,02),                                               ~
        "  posting to the correct period.                                ~
        ~              ",                                                 ~
               at (11,02),                                               ~
        "  This process will delete the Count Session as it progresses - ~
        ~no turning ",                                                    ~
               at (12,02),                                               ~
        "  back!  An Adjustment Journal report is created when processing~
        ~ is done.",                                                      ~
               at (14,04),                                               ~
                  "Count Session Number       :",                        ~
               at (14,33), fac(lfac$( 1)), session_nbr$         , ch(02),~
               at (14,49), fac(hex(8c)),   description$         , ch(32),~
               at (15,04),                                               ~
                  "Session Level Posting Flags:",                        ~
               at (15,40), "(Flags may only be modified via HNYPIPRM)",  ~
               at (16,04),                                               ~
                  "  Post Inventory Variances?:",                        ~
               at (16,33), fac(lfac$( 2)), hnyvar$              , ch(01),~
               at (17,04),                                               ~
                  "  Post G/L       Variances?:",                        ~
               at (17,33), fac(lfac$( 3)), glvar$               , ch(01),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "PF Key Options:",                            ~
               at (22,64), "(13)Instructions",                           ~
               at (23,04), "(1)Start Over",                              ~
               at (23,64), "(15)Print Screen",                           ~
               at (24,64), fac(hex(84)), pf16$                  , ch(17),~
                                                                         ~
               keys(hex(0001040d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40790
                  call "MANUAL" ("HNYPIPST")
                  goto L40230

L40790:        if keyhit% <> 15 then L40830
                  call "PRNTSCRN"
                  goto L40230

L40830:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100          /* Count Session Num*/
                  return
L50100:     REM Test Data for Count Session Number
                call "GETCODE" (#1, session_nbr$,description$     , 1%,  ~
                                0, f1%(1))
                if f1%(1) = 0% then errormsg$ =                          ~
                   "SESSION UNDEFINED OR NONE ON FILE"
                if f1%(1) = 0% then return
                gosub dataload
                errormsg$ = "Not all the Tickets are Accounted For - Run ~
        ~Ticket Register to Check!"
                if accounted_for$ <> "Y" then return
                errormsg$ = "Qtys & Costs Not Yet Captured! Run the Captu~
        ~re File Quantities program (HNYPICAP) first!"
                if date2$ = " " or date2$ = blankdate$ then return
                errormsg$ = " "
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
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program

            call "FILEBGON" (#16)
*          CALL "JNLCLOSE" (MODNO$, JNLID$, PSTSEQ%, RETURNCODE%)
            plowkey$ = " "
            call "PLOWNEXT" (#2, plowkey$, 0%, f1%(2))
            if f1%(2) = 1% then end
            call "FILEBGON" (#2)
            call "FILEBGON" (#15)
            call "FILEBGON" (#17)
            if core_on% = 1% then call "FILEBGON" (#27)
            end
