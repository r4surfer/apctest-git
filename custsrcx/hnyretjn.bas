        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  RRRR   EEEEE  TTTTT  JJJJJ  N   N   *~
            *  H   H  NN  N  Y   Y  R   R  E        T      J    NN  N   *~
            *  HHHHH  N N N   YYY   RRRR   EEEE     T      J    N N N   *~
            *  H   H  N  NN    Y    R   R  E        T    J J    N  NN   *~
            *  H   H  N   N    Y    R   R  EEEEE    T     J     N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYRETJN - Prints Journal for and Updates transactions    *~
            *            entered via HNYRETIN (or equivalent) "Returns".*~
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
            * 03/02/87 ! Original                                 ! ERN *~
            * 05/13/87 ! File format changes; # costs from 3 to 12! JIM *~
            *          !   HNYPROC dropped.                       !     *~
            * 08/22/88 ! Fixed Mulitiple Line Item posting; was   ! RJM *~
            *          !   Posting cost of last item for every one!     *~
            * 01/03/90 ! Modified code to combine like accounts   ! LAB *~
            *          ! for gl postings - Accountancy Phase I    !     *~
            * 10/12/90 ! Merge GL Export Option & A/C I.          ! JDH *~
            * 05/21/91 ! Stop reads & puts if GL Export is off.   ! JDH *~
            * 01/21/92 ! Added HNYUSESB & get USETYPE$ fr HNYRETTF! JIM *~
            * 07/27/92 ! MPS/PFM Misc mods for release.           ! MLJ *~
            * 02/26/93 ! Added Core Value Tracking Codeing.       ! JBK *~
            *          !  Revised Text Passed to HNYPST2 per JDH. !     *~
            *          !  Removed references to HNYADJPF File as  !     *~
            *          !  Never Used.  Consolidated some code with!     *~
            *          !  Addition of Core Value Code.            !     *~
            * 01/14/94 ! Added to GLCMBSUB argument list.         ! JDH *~
            * 09/11/96 ! Millie date conversion                   ! DER *~
            * 08/14/06 ! (AWD001) Mod to add GLORTRAN             ! CMG *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            acct$12, acct_descr$30,      /* General Purpose Account #  */~
            acctary$(4)9,                /* array for glposting info   */~
            adjacct$16,                  /* adjustment acct            */~
            account$9,                   /* dummy argument to stack    */~
            compname$70,                 /* Company Name               */~
            core_inv_flag$1,             /* Core Value Inv Trans Flag  */~
            costs(12),                   /* Inventory Costs            */~
            creditsstk$(250)9,           /* credit acct for rcp#1 phase*/~
            creditsstk(250),             /* credit amounts for rcp#1   */~
            cr_acct$12,                  /* Credit Account             */~
            cuscode$9,                   /* Customer Code              */~
            descr$32,                    /* Movement Description       */~
            debitsstk$(250)9,            /* debit accts for rcp#1 phase*/~
            debitsstk(250),              /* debit amounts for rcp#1    */~
            dr_acct$12,                  /* Debit Account              */~
            from_str$3, from_lot$6,      /* From Store & Lot           */~
            gl_text$100,                 /* GL Posting Text            */~
            glamount(4),                 /* array for glposting info   */~
            hnyflg$1,                    /* flag to determine var acct */~
            hny_date$8, hny_datu$6,      /* Inventory Posting Date     */~
            hny_text$50,                 /* Inventory Posting Text     */~
            invnr$8,                     /* Invoice Number             */~
            jnl_id$3, jnl_name$70,       /* Journal ID and Title       */~
            location$2,                  /* location array for stack   */~
            part$25, part_descr$32,      /* Part Code                  */~
            passedin_acct$(50)109,       /* Array for glcmbsub         */~
            passedin_dbcr(50,2),         /* Array for glcmbsub         */~
            passedin_type$(50)2,         /* Array for glcmbsub         */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowwrk$99,                  /* Miscellaneous Read/Plow Key*/~
            post$1,                      /* G/L Posting Level          */~
            ret_type$1,                  /* Return Type (I or W)       */~
            rundate$8, runtime$8,        /* Report Run Date and Time   */~
            suspense_acct$16,            /* System Suspense Account    */~
            text$109,                    /* text for glcmbsub          */~
            to_str$3, to_lot$6,          /* To Store & Lot             */~
            usedate$6,                   /* Usage Capture Trans. Date  */~
            userid$3,                    /* Current User Id            */~
            useseq$3,                    /* Usage Capture Line Seg. #  */~
            useso$16,                    /* Usage Capture S/O Number   */~
            usetype$5,                   /* Usage Capture Type Code    */~
            which_pass$1                 /* Which Routine Printing     */

        dim                              /* G/L Export Posting Info    */~
            export_on$1,                 /* G/L Export File processing?*/~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            partcat$4,                   /* Part Category code         */~
            partclass$4,                 /* Part Class code            */~
            partgen$16,                  /* Part Generic code          */~
            parttype$3,                  /* Part Type code             */~
            tran_type$5,                 /* G/L Transaction type       */~
            uom$                         /* Part Unit of measure       */

        dim division$3,                  /* division number (AWD001)   */~
            schema$8                     /* schema          (AWD001)   */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

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
            * # 1 ! USERINFO ! Users Default Information File           *~
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 3 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * # 4 ! HNYMASTR ! Inventory Master File                    *~
            * # 5 ! GLMAIN   ! General Ledger Chart Of Accounts File    *~
            * # 6 ! GLDETAIL ! General Ledger Detailed Trans History    *~
            * # 7 ! SFCUM2   ! Cumulative Sales Forecast File           *~
            * # 9 ! HNYRETTF ! Inventory Returns Transaction File       *~
            * #11 ! HNYQUAN  ! Inventory Costs & Quantity Master        *~
            * #12 ! HNYDETAL ! Inventory Details                        *~
            * #13 ! HNYPOOL  ! Inventory LIFO/FIFO pool records         *~
            * #14 ! GLORTRAN ! GL Oracle transmit file     (AWD001)     *~
            * #15 ! GENCODES ! GENERAL CODES MASTER FILE   (AWD001)     *~
            * #50 ! WRKFILE  ! HNYPST2 work file                        *~
            * #51 ! COREWRK  ! Core Value Work File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "USERINFO",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =   3                      ~

            select # 2, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select # 3, "PIPMASTR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select # 4, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alternate key 1, keypos = 102, keylen = 9, dup,  ~
                                  key 2, keypos =  90, keylen = 4, dup

            select # 5, "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select # 6, "GLDETAIL",                                      ~
                        varc,     indexed,  recsize =  160,              ~
                        keypos =    1, keylen =  26

            select # 7, "SFCUM2",                                        ~
                        varc,     indexed,  recsize = 1985,              ~
                        keypos =    1, keylen =  25

            select # 9, "HNYRETTF",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =  1,   keylen =  13

            select #11, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #12, "HNYDETAL",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =   43, keylen =   6, dup,    ~
                            key  2, keypos =   49, keylen =   2, dup

            select #13, "HNYPOOL",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  38                      ~

/*(AWD001)*/
            select #14, "GLORTRAN",                                       ~
                        varc,     indexed, recsize = 512,                 ~
                        keypos = 1,    keylen = 33,                       ~
                        alt key 1, keypos = 31, keylen = 47,              ~
                            key 2, keypos = 81, keylen = 26

            select #15, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
/*(AWD001)*/


            select #50, "WRKFILE",                                       ~
                        varc,     indexed,  recsize =  160,              ~
                        keypos =    1, keylen =  19

            select #51, "COREWRK",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 256,                                  ~
                         keypos =  1, keylen = 19                        ~

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6), 0%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 0%, rslt$( 7))
            call "OPENCHCK" (# 9, fs%( 9), f2%( 9), 0%, rslt$( 9))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))
            call "OPENCHCK" (#13, fs%(13), f2%(13), 0%, rslt$(13))
/*(AWD001) */
            call "OPENCHCK" (#14, fs%(14%), f2%(14%), 100%, rslt$(14%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%),   0%, rslt$(15%))
/*(AWD001)*/
            call "WORKOPEN" (#50, "IO", 100%, f2%(50))

            if fs%(9) < 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************


                                                          /* (AWD001)  */

            schema_err%, schema% = 0%
            init(" ") schema$, division$
            call "SCHEMA" (schema$, schema%, #15, schema_err%)


            if schema% = 1% then division$ = "036"   /* NC */
            if schema% = 2% then division$ = "080"   /* NE */
            if schema% = 3% then division$ = "035"   /* AES*/

            call "EXTRACT" addr("ID", userid$)
            plowkey$ = str(userid$) & hex(00)
            call "PLOWNEXT" (#9, plowkey$, 3%, f1%(9))
            if f1%(9) = 0% then exit_program

*        See if G/L Export is on
            export_on$ = "N"  :  plowwrk$ = "SWITCHS.GL"
            call "READ100" (#2, plowwrk$, f1%(2))
            if f1%(2) = 1% then get #2 using L09107, export_on$
L09107:         FMT POS(22), CH(1)

            REM GET SYSFILE2 RECORD TO DETERMINE ACCOUNT FOR VARIANCE POST
                plowwrk$ = "SWITCHS.HNY"
                call "READ100" (#2, plowwrk$, f1%(2%))
                      if f1%(2%) = 0% then exit_program
                get #2, using L09116, hnyflg$
L09116:               FMT POS(96), CH(1)

*        Get System Suspense Account
                plowwrk$ = "FISCAL DATES"
                call "READ100" (#2, plowkey$, f1%(2%))
                      if f1%(2%) = 0% then L09135
                get #2, using L09130, suspense_acct$
L09130:              FMT POS(417), CH(16)

L09135
*        See if Core is on
            plowkey$ = "SWITCHS.COR"
            call "READ100" (#2, plowkey$, core_on%)
                if core_on% <> 1% then L09250
            get #2 using L09147, core_inv_flag$
L09147:         FMT POS(134), CH(1)
            if core_inv_flag$ <> "Y" then core_on% = 0%
            if core_on% <> 1% then L09250
                call "WORKOPEN" (#51, "IO", 10%, f2%(51%))

L09250
*        Not restarting.  Get User and Journal Infomation.
            call "READ100" (#1, userid$, f1%(1))
            if f1%(1) = 0% then exit_program
            get #1, using L09290, hny_datu$
L09290:         FMT XX(27), CH(6)
            hny_date$ = hny_datu$  :  call "DATEFMT" (hny_date$)
            call "WHICHMON" (#2, hny_datu$, thismonth%)
            if thismonth% < 1% or thismonth% > 3% then exit_program

L09340:     call "GETPARM" addr ("I ", "R", "JNLID   ",  " ", "0001",    ~
                                 "HNYRET",                               ~
                                 "INPUT THE JOURNAL ID TO POST THRU ",   ~
                                 34%, "K", "JNLID   ", jnl_id$, 3%, 5%,  ~
                                 32%, "A")
            if jnl_id$ = " " then L09340

            u3% = 0%
            call "JNLINFO" ("04", jnl_id$, jnl_seq%, post$, jnl_name$,   ~
                                              hny_datu$, #2, f2%(2), u3%)
            if post$ = "N" then post$ = "D" else post$ = "S"
            call "FMTTITLE" (jnl_name$, "JOURNAL", 12%)


            rundate$ = date  :  call "DATEFMT" (rundate$)
                                call "TIME"    (runtime$)
            call "COMPNAME" (12%, compname$, u3%)
            call "SETPRNT" ("HNY038", " ", 0%, 0%)
            select printer (134)
            summary% = 0%
            line%    = 857%
            tran_type$ = "IRT  "

        REM *************************************************************~
            *                  U P D A T E                              *~
            * --------------------------------------------------------- *~
            * Update Inventory and General Ledger                       *~
            *************************************************************

*        First update inventory.  G/L is updated here if to be posted in
*        detail, else it is updated later on in summary.
            call "SHOSTAT" ("Updating Inventory Returns")
            plowkey$ = str(userid$) & hex(00)

        detail_update_loop
            call "PLOWNEXT" (#9, plowkey$, 3%, f1%(9))
            ret_type$ = str(plowkey$,4,1)
            if f1%(9) = 0% or ret_type$ > "W" then summary_update

            get #9 using L12260, cuscode$, invnr$, part$, from_str$,      ~
                                from_lot$, qty, to_str$, to_lot$, descr$,~
                                dr_acct$, cr_acct$, costs(), usetype$
L12260:         FMT POS(14), CH(9), CH(8), CH(25), CH(3), CH(6),         ~
                    PD(14,4), CH(3), CH(6), CH(32), 2*CH(9), 12*PD(14,4),~
                    CH(5)
            each = 0
            for c% = 1% to 12% : each = each + costs(c%) : next c%
            totalcost = round(qty*each, 2)

            if export_on$ = "Y" then gosub load_part_info
L12308:     init(" ") passedin_acct$(), passedin_type$()
            mat passedin_dbcr = zer
                if hnyflg$ = "V" or hnyflg$ = " " then L12356
                     call "HNYGLGET" (part$, to_str$, to_lot$, adjacct$, ~
                                      6%, #4, #11)
                     if adjacct$ <> " " then L12356
                          adjacct$ = suspense_acct$

L12356:     if ret_type$ <> "I" then L12680

*       *** Update Invoiced Returns
            gl_text$ = " "
            str(gl_text$, 1) = str(cuscode$) & invnr$
            str(gl_text$,31) = str(part$,,25) & str(to_str$,,3) & to_lot$
            str(gl_text$,69) = "INVOICING RETURN"

            passedin_acct$(1) = str(dr_acct$,1,9) & gl_text$
            passedin_acct$(2) = str(cr_acct$,1,9) & gl_text$
            passedin_dbcr(1,1) = totalcost
            passedin_dbcr(1,2) = 0
            passedin_dbcr(2,1) = 0
            passedin_dbcr(2,2) = totalcost
            passedin_type$(1) = "01"
            passedin_type$(2) = "02"
            cntr% = 2

*       ******** Update Usage Files
                gosub usage_capture

            hny_text$ = "Return: " & str(invnr$) & "; Customer: "        ~
                        & cuscode$
            call "HNYPST2"                                               ~
                     (part$, to_str$, to_lot$, qty, 0, 0, 0, 0,          ~
                      costs(), totalcost, 0, 0,                          ~
                      hny_datu$, "RT", hny_text$, dr_acct$, cr_acct$,    ~
                      3%, 4%, "04", "   ", jnl_seq%, gl_text$, userid$,  ~
                      #11, #12, #2, #13, #4, #3, #7, #5, #6, #50, 1%, r%)

            if core_on% <> 1% then L12600
                call "CORVALSB" ("RT", part$, to_str$, to_lot$, qty,     ~
                                 hny_datu$, dr_acct$, cr_acct$, "04",    ~
                                 jn_lid$, jnl_seq%, gl_text$, userid$,   ~
                                 #11, #2, #4, #51, u3%)

L12600:     gosub consolidate_array_and_post

            if from_str$ = " " then L12971
                call "LOTTRACK" ("H", part$, from_lot$, from_str$, " ",  ~
                                 "C", str(cuscode$,,9) & str(invnr$,,8), ~
                                 " ", " ", " ", -qty, #4, #2)
            goto L12971

L12680
*       *** Update Inventory Withdrwal Returns
            gl_text$ = " "
            str(gl_text$, 1) = " "
            str(gl_text$,31) = str(part$,,25) & str(to_str$,,3) & to_lot$
            str(gl_text$,69) = descr$

            passedin_acct$(1) = str(dr_acct$,1,9) & gl_text$
            passedin_acct$(2) = str(cr_acct$,1,9) & gl_text$
            passedin_dbcr(1,1) = totalcost
            passedin_dbcr(1,2) = 0
            passedin_dbcr(2,1) = 0
            passedin_dbcr(2,2) = totalcost
            passedin_type$(1) = "05"
            passedin_type$(2) = "06"
            cntr% = 2

*       ******** Update Usage Files
                gosub usage_capture

            hny_text$ = descr$
            call "HNYPST2"                                               ~
                     (part$, to_str$, to_lot$, qty, 0, 0, 0, 0,          ~
                      costs(), totalcost, 0, 0,                          ~
                      hny_datu$, "IW", hny_text$, dr_acct$, cr_acct$,    ~
                      3%, 0%, "04", "   ", jnl_seq%, gl_text$, userid$,  ~
                      #11, #12, #2, #13, #4, #3, #7, #5, #6, #50, 1%, r%)

            if core_on% <> 1% then L12910
                call "CORVALSB" ("IW", part$, to_str$, to_lot$, qty,     ~
                                 hny_datu$, dr_acct$, cr_acct$, "04",    ~
                                 jn_lid$, jnl_seq%, gl_text$, userid$,   ~
                                 #11, #2, #4, #51, u3%)

L12910:     gosub consolidate_array_and_post

            if from_str$ = " " then L12971
                call "LOTTRACK" ("H", part$, from_lot$, from_str$, " ",  ~
                                 "D", str(descr$,,25), str(descr$,26,6), ~
                                 str(descr$,32,1), " ", -qty, #4, #2)

L12971:     if post$ = "S" then L12985
            call "DELETE" (#9, plowkey$, 13%)
L12985:     if core_on% = 1% then gosub process_core_entries
            goto detail_update_loop


        summary_update
            print using L16480
            print using L16510, rpt1, rpt2
            goto summary_report_loop

        end_update   /* Clear rest of transaction records from TIF     */
            plowkey$ = userid$
            call "DELETE" (#9, plowkey$, 3%)
            goto exit_program


        REM *************************************************************~
            * CALL TO CONSOLIDATE                                       *~
            *************************************************************

        consolidate_array_and_post
                first% = 1%  :  which_pass$ = "H"
                new_part% = 1%  :  wf% = 50%
                plowwrk$ = all (hex(00))
L13340:         gosub read_work_file
                     if f1%(50) = 0 and first% = 1% then L19050
                     if f1%(50) = 0 then L13500
                     first% = 0%
                if hnyflg$ = "V" or hnyflg$ = " " then L13430
                chkacct$ = str(text$,1,9)
                if chkacct$ = dr_acct$ or chkacct$ = cr_acct$ then       ~
                     L13430
                     text$ = str(adjacct$,1,9) & str(text$,10,100)
L13430:         cntr% = cntr% + 1%
                gosub load_gl_arrays
                if ret_type$ = "I" then passedin_type$(cntr%) = "01"     ~
                                   else passedin_type$(cntr%) = "05"
                if str(text$,76,1) = "H" then L13470
                if hnyflg$ = "A" then L13468
                if ret_type$ = "I" then passedin_type$(cntr%) = "04"     ~
                                   else passedin_type$(cntr%) = "08"
                if str(text$,74,1) = "S" then L13470
L13468:         if ret_type$ = "I" then passedin_type$(cntr%) = "03"     ~
                                   else passedin_type$(cntr%) = "07"
L13470:         delete #50
                goto L13340

L13500:         call "GLCMBSUB" (passedin_acct$(), passedin_dbcr(),      ~
                                 passedin_type$(), #2, cntr%)
                gosub gl_post_loop
                return

        REM *************************************************************~
            *         C O M M O N   G / L   P O S T   L O G I C         *~
            *                                                           *~
            * ACCT$(), GL_TEXT$, and GLAMOUNT must be set prior to this.*~
            *************************************************************

        post_gl

            if export_on$ = "Y" then                                     ~
                put gl_post_info$() using L13716, tran_type$, (glamount(1)~
                                                           - glamount(2))
L13716:         FMT CH(5), POS(18), PD(15,4)

            REM Account in ACCT$(1) is debited...

            if glamount(1) = 0 then L13950

            call "GLPOST2" (acctary$(1), /* ACCOUNT TO BE UPDATED      */~
                      glamount(1),       /* DEBIT AMOUNT (0 IF CREDIT) */~
                      0,                 /* CREDIT AMOUNT (0 IF DEBIT) */~
                      hny_datu$,         /* DATE OF MODULE             */~
                      0%,                /*                            */~
                      "04",              /* TYPE CODE OF TRANSACTION   */~
                      gl_text$,          /* REFERENCE TEXT (100 CHARS) */~
                      "IRT",             /* JOURNAL ID                 */~
                      jnl_seq%,          /* POSTING SEQUENCE NUMBER    */~
                      userid$,           /* WHO                        */~
                      division$,         /* (AWD001) division code     */~
                      #5,                /* UFB ADDRESS OF G/L MAIN    */~
                      #6,                /* UFB ADDRESS OF G/L DETAILS */~
                      #2,                /* UFB ADDRESS OF SYSFILE2    */~
                      #14,               /* (AWD001) GLORTRAN          */~
                      r%,                /* ERROR RETURN FROM SUBROUTIN*/~
                      " ", gl_post_info$())


L13950:     REM Account in ACCT$(2), ...  is credited...
            for credit% = 2% to 4%
               if acctary$(credit%) = " " then L14190
               if glamount(credit%) = 0 then L14190

            call "GLPOST2" (acctary$(credit%),/* ACCOUNT TO BE UPDATED */~
                      0,                 /* DEBIT AMOUNT (0 IF CREDIT) */~
                      glamount(credit%), /* CREDIT AMOUNT (0 IF DEBIT) */~
                      hny_datu$,         /* DATE OF MODULE             */~
                      0%,                /*                            */~
                      "04",              /* TYPE CODE OF TRANSACTION   */~
                      gl_text$,          /* REFERENCE TEXT (100 CHARS) */~
                      "IRT",             /* JOURNAL ID                 */~
                      jnl_seq%,          /* POSTING SEQUENCE NUMBER    */~
                      userid$,           /* WHO                        */~
                      division$,         /* (AWD001) division code     */~
                      #5,                /* UFB ADDRESS OF G/L MAIN    */~
                      #6,                /* UFB ADDRESS OF G/L DETAILS */~
                      #2,                /* UFB ADDRESS OF SYSFILE2    */~
                      #14,               /* (AWD001) GLORTRAN          */~
                      r%,                /* ERROR RETURN FROM SUBROUTIN*/~
                      " ", gl_post_info$())


L14190:     next credit%

        return


        REM *************************************************************~
            *             P R I N T    R E P O R T                      *~
            *-----------------------------------------------------------*~
            * Print report and summarize by account number.             *~
            *************************************************************
        detail_report_loop
            gosub'162(acctary$(1%), glamount(1%))
            gosub'163(acctary$(2%), glamount(2%))
            rpt1 = rpt1 + glamount(1%)
            rpt2 = rpt2 + glamount(2%)
            call "GLFMT" (acctary$(1%))
            call "GLFMT" (acctary$(2%))
            if new_part% = 1% then                                       ~
                call "DESCRIBE" (#4, part$, part_descr$, 0%, f1%(4%))

            if line% > 55% then gosub page_heading
            if new_part% <> 1% then L15220
            print using L16390, part$, part_descr$, cuscode$, from_str$,  ~
                               to_str$
            if which_pass$ = "H" then                                    ~
                print using L16410, descr$, invnr$, from_lot$, to_lot$,   ~
                          qty, acctary$(1), glamount(1), glamount(2)
            if which_pass$ = "C" then                                    ~
                print using L16504, "Core Value-G/L Trans Only", descr$,  ~
                                   invnr$, from_lot$, to_lot$, qty,      ~
                                   acctary$(1), glamount(1), glamount(2)
            line% = line% + 2%
            goto L15240
L15220:
            print using L16450, acctary$(1), glamount(1), glamount(2)
            line% = line% + 1%
L15240:     return

        REM *************************************************************~
            *        S T A C K   P U S H I N G   R O U T I N E S        *~
            *                                                           *~
            * PUSHES THE INDICATED INFORMATION ONTO THE DESIRED STACK   *~
            * FOR LATER PROCESSING.                                     *~
            *************************************************************

            deffn'162(account$, amount)  /* RECAP DEBITS ACCUMULATOR   */
                  search str(debitsstk$(),1) = str(account$)             ~
                               to location$ step 9 /* FIND ACCOUNT #   */
                  if location$ = hex(0000) then L15410  /* PUSH NEW ITEM*/
                     junk% = int(val(location$,2)/9)+1 /* WHICH CELL?  */
                     debitsstk(junk%) = debitsstk(junk%) + amount
                               /* UPDATE AMOUNT FOR EXISTING ACCOUNT   */
                     return
L15410:           REM PUSH NEW ITEM ONTO STACK.
                      debitsptr% = debitsptr% + 1
                      debitsstk$(debitsptr%) = account$
                      debitsstk(debitsptr%) = amount
                      return

            deffn'163(account$, amount)  /* RECAP CREDITS ACCUMULATOR  */
                  search str(creditsstk$(),1) = str(account$)            ~
                               to location$ step 9
                               /* SCAN ONLY WHAT IS USED OF STACK      */
                  if location$ = hex(0000) then L15560  /* IF NO ==> NEW*/
                     junk% = int(val(location$,2)/9)+1 /* WHICH ELEMENT*/
                     creditsstk(junk%) = creditsstk(junk%) + amount
                                         /* UPDATE AMT FOR THAT ELEMENT*/
                     return
L15560:           REM PUSH NEW ENTRY ONTO SALES ACCOUNT STACK.
                      creditsptr% = creditsptr% + 1
                      creditsstk$(creditsptr%) = account$
                      creditsstk(creditsptr%) = amount
                      return

        summary_report_loop
            summary% = 1%
            line% = 857%
            if debitsptr% = 0% and creditsptr% = 0% then end_update
            if debitsptr% > creditsptr% then goto L15690
                loop% = creditsptr%
                goto L15700
L15690:         loop% = debitsptr%
L15700:     for x% = 1% to loop%
            acct$ = debitsstk$(x%)
            dr = debitsstk(x%)
            cr = creditsstk(x%)
            call "DESCRIBE" (#5, acct$, acct_descr$, 0%, f1%(5))
            call "GLFMT" (acct$)
            if post$ <> "S" then L15820
            gl_text$ = " "
            str(gl_text$,69) = "INVENTORY RETURNS: SUMMARY POSTING"
            put gl_post_info$() using L13716, tran_type$, (dr-cr)
            call "GLPOST2" (acct$, dr, cr,  hny_datu$, 0%, "04",         ~
                            gl_text$, "IRT", jnl_seq%, userid$,          ~
                            division$,        /* (AWD001) */  ~
                            #5, #6, #2, #14, r%, " ", gl_post_info$())
L15820:     if line% > 54% then gosub page_heading
            if dr = 0 then goto L15860
            print using L16610, acct$, acct_descr$, dr, 0
            line% = line% + 1%
            if cr = 0 then goto L15880
L15860:     print using L16610, acct$, acct_descr$, 0, cr
            line% = line% + 1%
L15880:     next x%
                print using L16630
                print using L16610, " ", "** TOTALS **", rpt1, rpt2
                print  :  print "** END OF REPORT **"
                call "SETPRNT" ("HNY038", " ", 0%, 1%)
                close printer
                goto end_update


        page_heading
            page% = page% + 1%  :  line% = 7%
            print page
            print using L16190, rundate$, runtime$, compname$
            print using L16220, hny_date$, jnl_name$, page%
            print using L16250, jnl_id$, jnl_seq%
            print
            if summary% > 0% then L16110
                print using L16270
                print using L16300
                print using L16330
                print using L16360
                return

L16110:         print using L16550
                print
                print using L16570
                print using L16590
                return



L16190: % RUN DATE: ######## ########       #############################~
        ~#########################################            HNYRETJN:HNY~
        ~038
L16220: %POST DATE: ########                #############################~
        ~#########################################                PAGE: ##~
        ~###
L16250: %JOURNAL ###  SEQ: ##########

L16270: %                                                           * ISS~
        ~UED FROM *   *** RETURNED ***

L16300: %                          PART DESCRIPTION    /            CUSTO~
        ~MER/ STR/    STR/

L16330: %PART RETURNED             RETURNS DESCRIPTION               INVO~
        ~ICE  LOT NR LOT NR   QUANTITY   ACCOUNT       DB AMT       CR AMT

L16360: %------------------------- -------------------------------- -----~
        ~---- ------ ------ ---------- ------------  -----------  --------~
        ~---
L16390: %######################### ################################ #####~
        ~#### ###    ###
L16410: %                          ################################  ####~
        ~#### ###### ###### #######.## ############ #########.## #########~
        ~.##

L16450: %                                                                ~
        ~                              ############ #########.## #########~
        ~.##
L16480: %                                                                ~
        ~                                           ------------ ---------~
        ~---
L16504: %######################### ################################ #####~
        ~#### ###### ###### #######.## ############ #########.## #########~
        ~.##
L16510: %                                                                ~
        ~                         ** TOTAL **       -####,###.## -########~
        ~.##

L16550: %                                            R E C A P   B Y   G ~
        ~/ L   A C O U N T    C O D E
L16570: %                             ACCOUNT CODE  ACCOUNT DESCRIPTION  ~
        ~                 DEBIT AMOUNT     CREDIT AMOUNT
L16590: %                             ------------  ---------------------~
        ~---------      --------------    --------------
L16610: %                             ############  #####################~
        ~#########     -###,###,###.##   -###,###,###.##
L16630: %                                                                ~
        ~               --------------    --------------


        load_gl_info

            put str(gl_post_info$(),,) using L17490,                      ~
                " ",                     /* Transaction Type CH(5)     */~
                " ",                     /* Currency code CH(4)        */~
                0,                       /* Transaction Currency amount*/~
                0,                       /* Functional Currency amount */~
                qty,                     /* Unit amount                */~
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
                to_str$,                 /* Store Number CH(3)         */~
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

L17490: FMT     CH(5),                   /* Transaction Type CH(5)     */~
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

        usage_capture             /* Post Quantity Used to Usage Files */
            useqty  = -qty
            usedate$ = hny_datu$
            call "HNYUSESB" (useso$, useseq$, to_str$, part$, "A",       ~
                usedate$, usetype$, useqty)
            return

        load_part_info
            call "READ100" (#4, part$, f1%(4%))
                 if f1%(4%) = 0% then L12308  /* Shouldn't happen */
            get #4 using L18150, partgen$, uom$, partcat$, partclass$,    ~
                                parttype$
L18150:     FMT POS(58), CH(16), CH(4), POS(90), CH(4), POS(133), CH(4), ~
                POS(180), CH(3)
            return


        REM *************************************************************~
            * CALL TO PROCESS CORE ENTRIES, IF ANY                      *~
            *************************************************************

        process_core_entries
                cntr%, first% = 0%  :  new_part% = 1%
                wf% = 51%  :  which_pass$ = "C"
                init (" ") passedin_acct$(), passedin_type$()
                mat passedin_dbcr = zer
                plowwrk$  = all (hex(00))
L18400:         gosub read_work_file
                     if f1%(51%) = 0% and first% = 0% then return
                     if f1%(51%) = 0% then L18670
                     first% = 1%
                if cntr% > 0% then L18490
                     get #51, using L18470, part$, to_str$, to_lot$, qty, ~
                                           each, totalcost
L18470:                   FMT POS(157), CH(25), CH(3), CH(16), 3*PD(14,4)
                     if export_on$ = "Y" then gosub load_part_info
L18490:         cntr% = cntr% + 1%
                gosub load_gl_arrays
                if cntr% > 2% then L18570
                     if ret_type$ = "I" then passedin_type$(cntr%) = "09"~
                                        else passedin_type$(cntr%) = "13"
                     if str(text$,76%,1%) = "H" then L18640
                     if ret_type$ = "I" then passedin_type$(cntr%) = "10"~
                                        else passedin_type$(cntr%) = "14"
                     goto L18640

L18570:         if ret_type$ = "I" then passedin_type$(cntr%) = "09"     ~
                                   else passedin_type$(cntr%) = "13"
                if str(text$,76%,1%) = "H" then L18640
                if hnyflg$ = "A" then L18610
                    if ret_type$ = "I" then passedin_type$(cntr%) = "12" ~
                                       else passedin_type$(cntr%) = "16"
                    if str(text$,74%,1%) = "S" then L18640
L18610:             if ret_type$ = "I" then passedin_type$(cntr%) = "11" ~
                                       else passedin_type$(cntr%) = "15"

L18640:         delete #51
                goto L18400

L18670:         call "GLCMBSUB" (passedin_acct$(), passedin_dbcr(),      ~
                                 passedin_type$(), #2, cntr%)

                gosub gl_post_loop
                return

        REM *************************************************************~
            * MISCELLANEOUS SUBROUTINES                                 *~
            *************************************************************

        read_work_file
            call "PLOWNXT1" (#wf%, plowwrk$, 0%, f1%(wf%))
                     if f1%(wf%) = 0% then return

            get #wf%, using L18820, text$, dbamt, cramt
L18820:         FMT XX(25), CH(109), 2*PD(14,4)
            return

        load_gl_arrays
            passedin_acct$(cntr%) = text$
            passedin_dbcr(cntr%,1%) = dbamt
            passedin_dbcr(cntr%,2%) = cramt
            return


        REM *************************************************************~
            *  Prepare the Accumulated Arrays for G/l Posting           *~
            *************************************************************
        gl_post_loop
L19050:     if export_on$ = "Y" then gosub load_gl_info
            if which_pass$ = "C" then init ("*")  from_str$, to_str$,    ~
                                                  from_lot$, to_lot$
            for x% = 1% to 50%
                if passedin_acct$(x%) = " " then goto L19200
                acctary$(1%) = str(passedin_acct$(x%),,9%)
                acctary$(2%) = acctary$(1%)
                glamount(1%) = passedin_dbcr(x%,1%)
                glamount(2%) = passedin_dbcr(x%,2%)
                gl_text$ = str(passedin_acct$(x%),10%)
                str(tran_type$,4%,2%) = passedin_type$(x%)
                if post$ = "S" then L19160
                gosub post_gl
L19160:         gosub detail_report_loop
                new_part% = 0%
            next x%

L19200:     return

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
            call "JNLCLOSE" ("04", "IRT", jnl_seq%, r%)
            call "FILEBGON" (#50)
            if core_on% = 1% then call "FILEBGON" (#51)
            call "SHOSTAT" ("One Moment Please")

            end
