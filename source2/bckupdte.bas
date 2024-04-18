        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    CCC   K   K  U   U  PPPP   DDDD   TTTTT  EEEEE   *~
            *  B   B  C   C  K  K   U   U  P   P  D   D    T    E       *~
            *  BBBB   C      KKK    U   U  PPPP   D   D    T    EEEE    *~
            *  B   B  C   C  K  K   U   U  P      D   D    T    E       *~
            *  BBBB    CCC   K   K   UUU   P      DDDD     T    EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKUPDTE - Move Sales Order from the buffers to the master*~
            *            files.  Update Demands and HNYQUAN records for *~
            *            the backorder quantities.                      *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/21/86 ! Original                                 ! ERN *~
            * 02/04/87 ! Changed HNYQUAN Format                   ! KAB *~
            * 04/16/87 ! Remove any bring down records upon exit  ! ERN *~
            * 05/18/87 ! Changed HNYMASTR Record length           ! JIM *~
            * 02/02/88 ! Added cancelled SOs  - BKNGTYPE$ = "4"   ! JDH *~
            *          !                  and RECORDTYPE$ = "5"   ! JDH *~
            * 06/17/88 ! Added Alt Key 3 on DEMMASTR              ! MJB *~
            * 08/10/89 ! Allocation type 'C' now stays a 'C' even ! JDH *~
            *          !   though the open qty is zero.           !     *~
            * 11/27/89 ! Expanded size of SADETAIL file.          ! JEF *~
            * 09/10/90 ! SA treats cancelled the same as deleted. ! JDH *~
            * 06/03/91 ! PRRs 11651 & 11902.  BCKPRIDX deletes &  ! JDH *~
            *          !   writes modified.                       !     *~
            * 09/17/91 ! Resurrected orders treated as new for SA.! JDH *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 07/23/92 ! PRR 12531 Added Allocation Type 'P'.     ! JDH *~
            * 07/24/92 ! ALLOC for type 'A' no longer gives up    ! JDH *~
            *          !   previously allocated quantity.         !     *~
            * 08/13/92 ! MPS/PFM - Added Coding for calls to      ! JBK *~
            *          !   Usage Capture Subroutine (HNYUSESB).   !     *~
            * 09/08/92 ! Chngd call to PLANALLC for more info out.! JDH *~
            * 03/09/93 ! PRR 11696,12416 Rpt ID for unplan report.! JDH *~
            * 03/11/93 ! Integrated Kenny's PORT/MESSAGE subs.    ! JDH *~
            * 07/20/93 ! Init OLDALLOC$ on new line to 'blank'.   ! JDH *~
            * 09/27/93 ! Added Multi-Line Quote cutover logic.    ! JIM *~
            * 11/18/93 ! BUFFER plowkey now primed to get records ! MLJ *~
            *          !  with a date > than day 1 ignoring all   !     *~
            *          !  orders currently in edit.               !     *~
            * 03/08/94 ! Changed record length for BOMSPEC file   ! WPH *~
            * 10/17/94 ! PRR 13285 - Calculate the Stocking Price ! RJH *~
            *          !   instead of using the less accurate file!     *~
            *          !   value.                                 !     *~
            * 03/10/95 ! PRR 13187 - Delete Part Xref Shadow Recrd! RJH *~
            * 03/10/95 ! PMetal - Delete Part Scharge Shadow Recrd! RJH *~
            * 03/21/95 ! PRR 13143. Added flag for Acknowledgment?! JDH *~
            * 07/31/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            adjrsn$9,                    /* Adjustment Reason Code     */~
            alloc$1,                     /* Allocation Flag            */~
            bkngtype$1,                  /* Bkng Rpt 1=New 2=Adj 3=Canc*/~
            bufferkey$10,                /* BCKBUFFR Primary Key       */~
            crhold$1,                    /* Credit Hold Status (b/H)   */~
            currency$4,                  /* Transaction Currency       */~
            convdate$6,                  /* Convertion eff. date       */~
            cuscode$9,                   /* Customer Code              */~
            datetime$7,                  /* Date Time Stamp            */~
            firstship$6,                 /* Earliest Req'd Ship Date   */~
            linekey$19,                  /* Line Item Key              */~
            message$10,                  /* Messaging Work Variable    */~
            mlqyorn$1, mlqxref$1,        /* MLQ processing params      */~
            mlquote_seq$11,              /* Quote & Seq LI derived from*/~
            newest$8,                    /* Buffer Estimate Number     */~
            newdemtype$1,                /* Buffer Demand Type         */~
            newhdr$(5)200,               /* Buffer Header Record       */~
            newline$(2)150,              /* Buffer Line Item Record    */~
            newlot$6,                    /* Buffer Lot Number          */~
            neworderdate$,               /* New Order Date             */~
            neworigdue$6,                /* Buffer Original Due Date   */~
            newsa$(2)150,                /* New Sales Analysis Data    */~
            newstore$3,                  /* Buffer Store Code          */~
            newusagedate$6,              /* Buffer Usage Capture Date  */~
            oldalloc$1,                  /* Allocation Flag            */~
            oldcrhold$1,                 /* Previously Cancelled Order */~
            olddemtype$1,                /* Master Line Demand Type    */~
            oldest$8,                    /* Master Estimate Number     */~
            oldhdr$(5)200,               /* Master Header Record       */~
            oldline$(2)150,              /* Master Line Item Record    */~
            oldlot$6,                    /* Master Lot Number          */~
            oldorderdate$,               /* Old Order Date             */~
            oldorigdue$6,                /* Old Original Due Date      */~
            oldreqdship$6,               /* Old Required Ship Date     */~
            oldsa$(2)150,                /* New Sales Analysis Data    */~
            oldstore$3,                  /* Master Store Code          */~
            oldusagedate$6,              /* Old Usage Capture Date     */~
            part$25,                     /* Part Number                */~
            planflags$(25)20,            /* Planning System Flags      */~
            port$4,                      /* Messaging Port             */~
            print$1,                     /* Pick List/BOL Print Flag   */~
            priority$1,                  /* Demand Priority            */~
            recordtype$1,                /* Record type for SADETAIL   */~
            readkey$50,                  /* Multi-use read key         */~
            reqdship$6,                  /* Required Ship Date         */~
            rptid$6,                     /* Report ID                  */~
            seq$3,                       /* Line Sequence Number       */~
            shiptoname$30,               /* Ship-to Name from Buffer   */~
            so$16,                       /* Sales Order Number         */~
            stat$4,                      /* Statutory Currency         */~
            tdate$8,                     /* Temporary Date Variable    */~
            type$1,                      /* Demand Type                */~
            usage_date$1,                /* Usage Capture Date Flag    */~
            usage_on$1,                  /* Usage Capture ON-OFF Flag  */~
            usage_store$1,               /* Usage Capture Store Flag   */~
            usagetype$5,                 /* MPS/PFM Usage Type         */~
            userid$3                     /* User ID                    */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64)                      /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/


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
            * #1  ! SYSFILE2 ! Caelus Management System General Informa *~
            * #2  ! DEMMASTR ! Demand Master File                       *~
            * #3  ! HNYQUAN  ! Inventory Part / Store / Lot Quantity Fi *~
            * #4  ! HNYMASTR ! Inventory Master File                    *~
            * #5  ! BCKMASTR ! Backlog master file                      *~
            * #6  ! BCKLINES ! Back Log Line Item File                  *~
            * #7  ! BCKBKGRF ! SO Bookings Report file                  *~
            * #8  ! BCKPRIDX ! BCK Print Index File                     *~
            * #9  ! BCKBUFFR ! Backlog buffer for SO headers            *~
            * #10 ! BCKBUF2  ! Buffer for line items                    *~
            * #12 ! JBCROSS2 ! Cross reference of RTE & BOM planned for *~
            * #13 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #14 ! WCMASTR  ! Workcenter Master File                   *~
            * #15 ! WCOUT    ! Planned work center use detail record    *~
            * #16 ! PIPIN    ! Planned inventory additions detail       *~
            * #17 ! PIPOUT   ! Planned inventory use detail record      *~
            * #18 ! SFMASTR2 ! Sales forecast master file               *~
            * #19 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #20 ! PIPCROSS ! hard peg cross reference                 *~
            * #21 ! BOMSPEC  ! options selected file                    *~
            * #22 ! JBPIPXRF ! option part harder peg                   *~
            * #23 ! ESTMASTR ! Estimate master file                     *~
            * #24 ! BCKLNCUR ! Transaction Currency File                *~
            * #30 ! MLQMASTR ! Quotation Master file                    *~
            * #33 ! MLQCROSS ! Quotation-to-S.O. Cross Reference file   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #2,  "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28,         ~
                            key  3, keypos =   29, keylen =  25, dup

            select #3,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #4,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #5,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #6,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #7,  "BCKBKGRF",                                      ~
                        varc,     indexed,  recsize =  88,               ~
                        keypos =  1,    keylen = 33

            select #8,  "BCKPRIDX",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =  11,   keylen =  29,                    ~
                        alt key  1, keypos =    1, keylen =  39

            select #9,  "BCKBUFFR",                                      ~
                        varc,     indexed,  recsize = 1020,              ~
                        keypos =    1, keylen =  10,                     ~
                        alt key  1, keypos =    4, keylen =   7, dup,    ~
                            key  2, keypos =   30, keylen =  16

            select #10, "BCKBUF2",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #12, "JBCROSS2",                                      ~
                        varc,     indexed,  recsize =  94,               ~
                        keypos =  29,  keylen = 19,                      ~
                        alt key  1, keypos =    1, keylen =  47,         ~
                            key  2, keypos =   48, keylen =  47

            select #13, "PIPMASTR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26

            select #14, "WCMASTR",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6

            select #15, "WCOUT",                                         ~
                        varc,     indexed,  recsize =   68,              ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =    1, keylen =  27

            select #16, "PIPIN",                                         ~
                        varc,     indexed,  recsize =   60,              ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48

            select #17, "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37

            select #18, "SFMASTR2",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  25

            select #19, "SFCUM2",                                        ~
                        varc,     indexed,  recsize = 1985,              ~
                        keypos =    1, keylen =  25

            select #20, "PIPCROSS",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =  1,   keylen = 71,                      ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33

            select #21, "BOMSPEC",                                       ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  54,                     ~
                        alt key  1, keypos =   57, keylen =  23

            select #22, "JBPIPXRF",                                      ~
                        varc,     indexed,  recsize =  63,               ~
                        keypos = 1,    keylen =  63,                     ~
                        alt key  1, keypos =   45, keylen =  19

            select #23, "ESTMASTR",                                      ~
                        varc,     indexed,  recsize =  2000,             ~
                        keypos =   10, keylen =  8,                      ~
                        alt key  1, keypos =  18, keylen =  30, dup,     ~
                            key  2, keypos =  48, keylen =  25, dup,     ~
                            key  3, keypos =   1, keylen =  17,          ~
                            key  4, keypos = 416, keylen =  16, dup

            select #24, "BCKLNCUR",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos = 5,    keylen =  19,                     ~
                        alt key  1, keypos =   1, keylen =  23

            select #30,  "MLQMASTR",                                     ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =   10, keylen =   8,                     ~
                        alt key  1, keypos =    1, keylen =  17

            select #33,  "MLQCROSS",                                     ~
                        varc,     indexed,  recsize =    41,             ~
                        keypos =    1, keylen =  33,                     ~
                        alt key  1, keypos =    1, keylen =   8, dup,    ~
                            key  2, keypos =    9, keylen =  25, dup,    ~
                            key  3, keypos =   18, keylen =  16, dup,    ~
                            key  4, keypos =    9, keylen =   9, dup

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ),   0%, " ")
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 100%, " ")
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ),   0%, " ")
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ),   0%, " ")
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 100%, " ")
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 100%, " ")
            call "OPENCHCK" (#7,  fs%(7 ), f2%(7 ), 100%, " ")
            call "OPENCHCK" (#8,  fs%(8 ), f2%(8 ), 100%, " ")
            call "OPENCHCK" (#9,  fs%(9 ), f2%(9 ),   0%, " ")
            call "OPENCHCK" (#10, fs%(10), f2%(10),   0%, " ")
            call "OPENCHCK" (#12, fs%(12), f2%(12), 100%, " ")
            call "OPENCHCK" (#13, fs%(13), f2%(13),   0%, " ")
            call "OPENCHCK" (#14, fs%(14), f2%(14), 100%, " ")
            call "OPENCHCK" (#15, fs%(15), f2%(15), 100%, " ")
            call "OPENCHCK" (#16, fs%(16), f2%(16), 100%, " ")
            call "OPENCHCK" (#17, fs%(17), f2%(17), 100%, " ")
            call "OPENCHCK" (#18, fs%(18), f2%(18), 100%, " ")
            call "OPENCHCK" (#19, fs%(19), f2%(19), 100%, " ")
            call "OPENCHCK" (#20, fs%(20), f2%(20), 100%, " ")
            call "OPENCHCK" (#21, fs%(21), f2%(21),   0%, " ")
            call "OPENCHCK" (#22, fs%(22), f2%(22), 100%, " ")
            call "OPENCHCK" (#23, fs%(23), f2%(23),   0%, " ")
            call "OPENCHCK" (#24, fs%(24), f2%(24),   0%, " ")

            select printer  /* In an attempt to print unplan report */
                            /* where the users wants it.            */

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            rptid$ = "BCK014"

*        Dig Up Port Id For Inter-Task Messaging...
            call "MSGCREAT" (#1, port$, "PORT.ID.BCKUPDTE", return%)
               if return% <> 0% then end

*        Remove any left over 'Kill me' tranactions.
            bufferkey$ = " "
            call "DELETE" (#9, bufferkey$, 3%)

            call "BCKSWTCH" ("BCK", "IDLETIME", message$, idle, u3%)
            if idle < 10 then idle = 10
            message$ = " "
            inactive% = -1% /* Force First Shostat */

*        Load in Planning System Flags for Allocations.  Use Default
*        Values if record is not on file.
            call "PIPINDEX" (#1, " ", today%, ret%)
            call "READ100" (#1, "PLANNING SYSTEM FLAG", f1%(1))
            if f1%(1) = 1% then L09330
                str(planflags$(), 1,1) = "N"       /* Display Option   */
                str(planflags$(),34,6) = hex(030000000100) /* SS Intru */
                str(planflags$(),82,6) = hex(000000000100) /* Ign ATC  */
                goto L09360
L09330:     get #1, using L09340, str(planflags$(),1,480)
L09340:         FMT XX(20), CH(480)

L09360:     call "READ100" (#1, "SWITCHS.CUR", f1%(1))
               if f1%(1) = 0% then L09500
            get #1, using L09390, stat$
L09390:         FMT POS(22), CH(4)

L09500
*        Load HNYFLAGS values for MPS/PFM Usage Capture
            usage_on$ = "N"
            call "READ100" (#1, "SWITCHS.HNY", f1%(1%))
                if f1%(1%) = 0% then L09570
            get #1, using L09550, usage_store$, usage_on$, usage_date$
L09550:         FMT POS(109), 2*CH(1), POS(119), CH(1)

L09570
*        Get Multi-Line Quotation processing values, if present.
            mlqxref$, mlqyorn$ = "N"/* Default = Negatory on MLQ front */
            call "READ100" (#01, "SWITCHS.MLQ", f1%(1%))   /* SYSFILE2 */
            if f1%(1%) <> 0% then get #01 using L09610, mlqyorn$, mlqxref$
L09610:         FMT POS(32), CH(1), POS(41), CH(1)
            if mlqyorn$ <> "Y" then goto L10000    /* No MLQ processing */
                call "OPENCHCK" (#30, fs%(30%), f2%(30%), 0%, " ")
                if mlqxref$ = "Y" then /* OPEN MLQCROSS Cross-Ref file?*/~
                     call "OPENCHCK" (#33, fs%(33%), f2%(33%), 100%, " ")

L10000: REM *************************************************************~
            *           G E T   N E X T   T R A N S C A T I O N         *~
            * --------------------------------------------------------- *~
            * Wait for the next Sales Order to appear in the buffer or  *~
            * for a time out / shutdown instruction.                    *~
            *************************************************************
        next_trans        /* Looking into buffer for a PO to update.   */
            bufferkey$ = all(hex(00))
            str(bufferkey$,3%,1%) = hex(01)
            call "PLOWALTS" (#9, bufferkey$, 1%, 0%, f1%(9))
            if f1%(9) = 1% then check_out_trans
                inactive% = max(inactive%, 1%)
                if message$  = "CANCEL" then exit_program
                if inactive% > idle     then exit_program
                call "MSGBFGND" (" ",                                    ~
                                 port$, message$, 10%, inactive%, u3%)
                if u3% = 16 then exit_program    /* Something's Whacko */
                     goto next_trans

        check_out_trans   /* See if what we've found is to be updated  */
            if inactive% <> 0% then call "SHOSTAT" ("Processing")
            inactive% = 0%     /* Reset Idle Time Control    */
            get #9 using L10210, userid$
L10210:         FMT CH(3)
            if userid$ = " " then exit_program     /* Kill Update Task */

        REM *************************************************************~
            *            P R O C E S S   T R A N S A C T I O N          *~
            * --------------------------------------------------------- *~
            * Control loading and processing of Sales Order.  Toss      *~
            * lines as we go to facilitate restarting (BCKINPUT always  *~
            * writes lines found in Master to the buffer; therefore if  *~
            * not found in buffer the line has already been processed). *~
            *************************************************************

        gosub initialize

        gosub load_headers


*       *** Process LINES ITEMS ****************************************
*        Get next line from buffer and, if any, the corresponding line *
*        from the master.  Update the backorder quantity and Sales     *
*        Analysis.  Then toss the line into the Master file.  Lastly,  *
*        update the Planning System Demand and PIPs.                   *
*       ****************************************************************

        linekey$ = str(so$) & hex(00)

        line_loop
          call "PLOWNEXT" (#10, linekey$, 16%, f1%(10))
          if f1%(10) = 0% then header_update

          gosub load_lines
          if str(newline$()) = str(oldline$()) and newstore$ = oldstore$ ~
                                                               then L10689

*        Adjust HNYQUAN (Quantity On Order)
            if oldline% = 0% or oldopenqty = 0 then L10630
                qty = -oldopenqty
                call "HNYPST1" (part$, oldstore$, oldlot$,               ~
                                0, qty,        0, 0, 0,                  ~
                                #3, #4, #1, f2%(3),f2%(4),f2%(1),0%,u3%)

L10630:     if newopenqty = 0 then L10664
                call "HNYPST1" (part$, newstore$, newlot$,               ~
                                0, newopenqty, 0, 0, 0,                  ~
                                #3, #4, #1, f2%(3),f2%(4),f2%(1),0%,u3%)

L10664
*        Check for MPS/PFM Usage Capture
            if pos("DSB" = usage_on$) > 0% then gosub usage_capture

*        Update Estimate Master File...
            if oldline% = 0% or oldest$ = " " then L10679
            call "READ101" (#23, oldest$, f1%(23))
                if f1%(23) = 0 then L10679
            put #23, using L10677, " ", " "
            rewrite #23
L10677:     FMT POS(1467), CH(16), CH(3)

L10679:     if newest$ = " " then L10689
            call "READ101" (#23, newest$, f1%(23))
                if f1%(23) = 0 then L10689
            put #23, using L10677, so$, str(newline$(),26,3)
            rewrite #23

L10689
*        Update Sales Analysis
            get str(newhdr$()) using L10710, neworderdate$
            get str(oldhdr$()) using L10710, oldorderdate$
L10710:         FMT POS(806), CH(6)

            if oldhdr% = 0% or oldline% = 0% then L11020
            if resurrected% = 1% then L11020  /* Resurrected do as new */
                oldsagross = round(oldorderqty * oldprice, 2)
                temp       = round(oldsagross  * olddiscpct * .01, 2)
                oldsanet   = oldsagross - temp
                oldsagross = -oldsagross
                oldsanet   = -oldsanet
                recordtype$ = "2"
                if crhold$ = "C" then recordtype$ = "5"
                put oldsa$() using L11430,                                ~
                     recordtype$,                  /* Adj. or Cancelled*/~
                     " ",                          /* Date/ Time Stamp */~
                     oldorderdate$,                /* SA Post Date     */~
                     so$, " ", " ",                /* SO, BOL, Invoice */~
                     str(newline$(), 26, 3),       /* Sequence Number  */~
                     " ",                          /* Date Shipped     */~
                     str(newhdr$(), 848, 9),       /* Adj Reason Code  */~
                     -oldorderqty,                 /* Open Units       */~
                     oldsagross,                   /* Gross Value      */~
                     oldsanet,                     /* Net Value        */~
                     0,                            /* Cost             */~
                     " ",                          /* Stocked Item?    */~
                     part$,                        /* Part Code        */~
                     str(oldline$(), 89, 4),       /* Category Code    */~
                     str(oldhdr$(), 884, 9),       /* Account Xref     */~
                     cuscode$,                     /* Customer Code    */~
                     str(oldhdr$(), 882, 2),       /* Customer Type    */~
                     oldstore$,                    /* Store Code       */~
                     str(oldhdr$(), 595, 4),       /* Region Code      */~
                     str(oldhdr$(), 580, 4),       /* Salesman         */~
                     -1,                           /* Inventory Cost   */~
                     currency$,          /* Currency                   */~
                     convdate%,          /* Convertion Eff. Date       */~
                     conveqv,            /* Convertion Factor          */~
                     convunt,            /* Convertion Factor          */~
                     cprice1,            /* Trans. Price (Stcking)     */~
                     cprice2,            /* Trans. Price (Prcing)      */~
                     " "                           /* Filler           */

L11020:         newsagross = round(neworderqty * newprice, 2)
                temp       = round(newsagross  * newdiscpct * .01, 2)
                newsanet   = newsagross - temp
                put newsa$() using L11430,                                ~
                     "1",                          /* New Bookng Trans */~
                     " ",                          /* Date/ Time Stamp */~
                     neworderdate$,                /* SA Post Date     */~
                     so$, " ", " ",                /* SO, BOL, Invoice */~
                     str(newline$(), 26, 3),       /* Sequence Number  */~
                     " ",                          /* Date Shipped     */~
                     str(newhdr$(), 848, 9),       /* Adj Reason Code  */~
                     neworderqty,                  /* Open Units       */~
                     newsagross,                   /* Gross Value      */~
                     newsanet,                     /* Net Value        */~
                     0,                            /* Cost             */~
                     " ",                          /* Stocked Item?    */~
                     part$,                        /* Part Code        */~
                     str(newline$(), 89, 4),       /* Category Code    */~
                     str(newhdr$(), 884, 9),       /* Account Xref     */~
                     cuscode$,                     /* Customer Code    */~
                     str(newhdr$(), 882, 2),       /* Customer Type    */~
                     newstore$,                    /* Store Code       */~
                     str(newhdr$(), 595, 4),       /* Region Code      */~
                     str(newhdr$(), 580, 4),       /* Salesman         */~
                     -1,                           /* Inventory Cost   */~
                     currency$,          /* Currency                   */~
                     convdate%,          /* Convertion Eff. Date       */~
                     conveqv,            /* Convertion Factor          */~
                     convunt,            /* Convertion Factor          */~
                     cprice1,            /* Trans. Price (Stcking)     */~
                     cprice2,            /* Trans. Price (Prcing)      */~
                     " "                           /* Filler           */

*        Now see if there's a need to Update Sales Analysis Files
          if oldhdr% = 0% or oldline% = 0% then donew /* No Old Line   */
          if resurrected% = 1% then donew /* Resurrected treated as new */
            if crhold$ = "C"                    then doold /* Cancelled*/
            if neworderdate$ <> oldorderdate$   then doold /* Perd Chg */
            if str(oldsa$(),93) <> str(newsa$(),93)                      ~
                then doold                                 /* Code Chg */
            if neworderqty - oldorderqty <> 0   then doold /* Qty Chg  */
            if newsagross + oldsagross <> 0     then doold /* $$$ Chg  */
            if newsanet   + oldsanet   <> 0     then doold /* $$$ Chg  */
            goto L11480  /* Same New Line as the Old- Don't do a thing  */
          doold /* Need to Do OLD (implies adjusting transactions)     */
                str(newsa$(),,1) = "2"
                  if crhold$ = "C" then str(newsa$(),,1) = "5"
                call "SAPOSTSB" (oldsa$(), #1, #4)
          donew /* Need to do NEW IF the line hasn't been deleted or   */
            if str(newline$(),57,1) = hex(ff) then L11480
            if crhold$ = "C" then L11480                  /* cancelled. */
                call "SAPOSTSB" (newsa$(), #1, #4)

L11430:         FMT CH(1), CH(7), CH(6), CH(16), CH(3), CH(8), CH(3),    ~
                    CH(6), CH(9), 4*PD(14,4), CH(1), CH(25), CH(4),      ~
                    CH(9), CH(9), CH(2), CH(3), CH(4), CH(4), PD(14,4),  ~
                    CH(4), BI(4), 2*PD(14,7), 2*PD(14,4), CH(100)

L11480
*        Toss Line into Master
            call "DELETE" (#6, linekey$, 19%)
            if str(newline$(), 57, 1) = hex(ff) then L11540 /* Deleted  */
                put #6 using L11520, str(newline$())
L11520:              FMT CH(300)
                write #6
L11540:     call "DELETE" (#10, linekey$, 19%)

*        Remove Shadow files
            if str(newline$(), 57, 1) <> hex(ff) then L11570 /*No Deleted*/
            /* Remove Cross Reference Shadow Record */
            call "PTUSEDSB" ("D", "BCK ", str(linekey$,,16%),            ~
                             str(linekey$,17%,3%), " ", " ", " ", ret%)

            /* Remove Precious Metal Surcharge Shadow File */
            call "PMCALSUB" (" ", " ", 0, 0, 1, "R", " ", 0, "S",        ~
                             str(linekey$,,19%), " ", " ", 0%)

L11570
*        And lastly, take care of Planning Interface for this line item
            if str(newline$()) = str(oldline$()) and                     ~
                   newstore$   = oldstore$       then L11795

            call "SETPRNT" (rptid$, " ", 0%, 0%) /* For unplan rpt */
            call "BCKPIPSB" (so$,             /* Sales Order Number    */~
                             str(newline$(),26,3), /* Seq Number       */~
                             newstore$,       /* For new transaction   */~
                             crhold$,         /* "H" = on Hold         */~
                             #6 ,             /* BCKLINES              */~
                             #2 ,             /* DEMMASTR              */~
                             #1 ,             /* SYSFILE2              */~
                             #13,             /* PIPMASTR              */~
                             #16,             /* PIPIN                 */~
                             #17,             /* PIPOUT                */~
                             #21,             /* BOMSPEC               */~
                             #14,             /* WCMASTR               */~
                             #15,             /* WCOUT                 */~
                             #18,             /* SFMASTR2              */~
                             #19,             /* SFCUM2                */~
                             #12,             /* JBCROSS2              */~
                             #20,             /* PIPCROSS              */~
                             #22 )            /* JBPIPXRF              */
            call "SETPRNT" (rptid$, " ", 0%, 1%) /* For unplan rpt */

L11795
*        Here are the final operations for the line items.
            gosub process_line_for_mlq
            goto line_loop


*       ** HEADER UPDATING *********************************************
*        Update the report and print files as required and then toss   *
*        the new header into the master if there are any lines left    *
*       ****************************************************************
        header_update

*        Check to see if there are any lines left on the order
            linekey$ = str(so$) & hex(00)
            call "PLOWNEXT" (#6, linekey$, 16%, linesleft%)

*        Update the Bookings Report File
            netgross = newgross - oldgross
            netdisc  = newdisc  - olddisc
            bkngtype$ = "1"
            if oldhdr% = 1% and linesleft% = 1% then bkngtype$ = "2"
            if oldhdr% = 1% and linesleft% = 0% then bkngtype$ = "3"
            if oldhdr% = 1% and crhold$ = "C" then bkngtype$ = "4"
            call "GETDTTM" addr(datetime$)
            write #7 using L12050, bkngtype$, cuscode$, so$,              ~
                                  str(bufferkey$,4,7), /* 1 per trans! */~
                                  shiptoname$, adjrsn$, netgross,        ~
                                  netdisc, eod goto L12080
L12050:         FMT CH(1), CH(9), CH(16), CH(7), CH(30), CH(9), 2*PD(14,4)


L12080
*        Update BCK Print Index Files as requested.
          if linesleft% = 0% then L12310  /* No order left  */

*         Order Acknowledgement Record First
            if print$ <> "1" and print$ <> "4" and                       ~
               print$ <> "6" and print$ <> "9" then L12140
            readkey$ = "A" & str(cuscode$) & str(so$)
            call "DELETE" (#8, readkey$, 26%)
            write #8 using L12280, "A", newstore$, " ", "A", cuscode$,    ~
                                      so$, " ", date

L12140
*         Pick List
            if print$ <> "3" and print$ <> "4" and                       ~
               print$ <> "8" and print$ <> "9" then L12210
                readkey$ = "P" & str(cuscode$) & str(so$)
                call "DELETE" (#8, readkey$, 26%)
                write #8 using L12280, "P", newstore$, firstship$, "P",   ~
                                      cuscode$, so$, " ", date

L12210
*         Bill of Lading Record
            if print$ <> "5" and print$ <> "6" and                       ~
               print$ <> "8" and print$ <> "9" then L12310
                readkey$ = "B" & str(cuscode$) & str(so$)
                call "DELETE" (#8, readkey$, 26%)
                write #8 using L12280, "B", newstore$, firstship$, "B",   ~
                                      cuscode$, so$, " ", date

L12280:         FMT CH(1), CH(3), CH(6), CH(1), CH(9), CH(16), CH(3),    ~
                    CH(6)

L12310
*        Now toss the Header around a bit
            readkey$ = str(cuscode$) & so$
            call "DELETE" (#5, readkey$, 25%)   /* Remove Old Header   */
            if linesleft% = 0% then L12380        /* Order Deleted      */
                put #5 using L12360, str(newhdr$())
L12360:              FMT CH(1000)
                write #5
L12380:     call "DELETE" (#9, bufferkey$, 10%)  /* Kill Buffer Hdr    */
            goto next_trans

        REM *************************************************************~
            *             I N I T I A L I Z A T I O N                   *~
            * --------------------------------------------------------- *~
            * Clear variables prior to processing transaction           *~
            *************************************************************
        initialize
            init (" ") newhdr$(), oldhdr$(), newstore$, oldstore$,       ~
                       crhold$, shiptoname$, adjrsn$, oldcrhold$
            firstship$ = all(hex(ff))
            resurrected% = 0%
            newgross, newdisc, oldgross, olddisc = 0
            newsagross, newsanet, oldsagross, oldsanet, mlqtotal = 0
            return

        REM *************************************************************~
            *            L O A D   D A T A   R O U T I N E S            *~
            * --------------------------------------------------------- *~
            * Routines for loading data from files.                     *~
            *************************************************************

        load_headers
            get #9 using L30080, bufferkey$, print$, str(newhdr$())
L30080:         FMT CH(10), CH(1), XX(9), CH(1000)

            get str(newhdr$()) using L30120, cuscode$, so$, shiptoname$,  ~
                     newstore$, adjrsn$, newdisc, newgross, crhold$
L30120:         FMT CH(9), CH(16), XX(16), CH(30), POS(803), CH(3),      ~
                    POS(848), CH(9), XX(2), 2*PD(14,4), CH(1)
            newdisc = round(newgross * newdisc * .01, 2)

            call "READ100" (#5, str(newhdr$(),,25), oldhdr%)
            if oldhdr% = 0% then return
                get #5 using L30190, str(oldhdr$())
L30190:              FMT CH(1000)
                get str(oldhdr$()) using L30220,                          ~
                          oldstore$, olddisc, oldgross, oldcrhold$
L30220:              FMT POS(803), CH(3), POS(859), 2*PD(14,4), CH(1)
                olddisc = round(oldgross * olddisc * .01, 2)
                if oldcrhold$ = "C" and crhold$ <> "C" then              ~
                                                        resurrected% = 1%
                return


        load_lines   /* Load new and old line (dummy up old line if it */
                     /* doesn't exist).  Also take care of allocation. */
            mlq_line, oldsanet, newsanet = 0
*        Load in new line item
            get #10 using L30310, str(newline$())
L30310:         FMT CH(300)
            get str(newline$()) using L30340, seq$, part$, neworderqty,   ~
                    newopenqty,  newprice,                               ~
                                conv_fact, temp, newdiscpct, neworigdue$,~
                    reqdship$, newlot$, newdemtype$, newest$,            ~
                    mlquote_seq$

L30340:         FMT XX(25), CH(3), XX(3), CH(25), POS(93), PD(14,4),     ~
                    XX(8), PD(14,4), POS(141), PD(14,4), POS(157),       ~
                                                               PD(14,7), ~
                    2*PD(14,4), POS(200), CH(6), POS(212), CH(6), CH(6), ~
                    POS(240), CH(1), POS(255), CH(8), POS(266), CH(11)

            newprice = temp / conv_fact       /* More accurate value */

            if reqdship$ < firstship$ then firstship$ = reqdship$

            currency$ = stat$:conveqv, convunt = 1:convdate% = 0%
            cprice1 = newprice:cprice2 = temp:convdate$ = " "
            call "READ100" (#24, key(#10), f1%(24))
               if f1%(24) = 0% then L30400
            get #24 using L30379, currency$, cprice1, cprice2, convdate$, ~
                                 conveqv, convunt
L30379:        FMT CH(4), POS(24), 2*PD(14,4), POS(40), CH(6), 2*PD(14,7)
            tdate$ = convdate$
            call "DATEFMT" (tdate$, convdate%)

*        Take care of Allocation
L30400:     gosub L30740  /* I need the old line, so load it up first */
            get str(newline$()) using L30410, alloc, preinv, alloc$
L30410:         FMT POS(125), 2*PD(14,4), POS(246), CH(1)
            get str(oldline$()) using L30414, oldalloc$
L30414:         FMT POS(246), CH(1)
            if oldline% = 0% then oldalloc$ = " " /* New line item */
            if newopenqty = 0 and alloc$ <> "C" then alloc$ = "Z"
            if pos("ACNZP" = alloc$) = 0% then alloc$ = "N"
            on pos("ACNZP" = alloc$) goto L30450, L30620, L30650, L30680,L30650
L30450:     /* Allocate to ATC */
            /* If a line is being reallocated 'A' then this logic will */
            /* not give up any previously allocated quantity.  If the  */
            /* user want to reallocate from scratch, then the line     */
            /* must be save using 'Z' first and then save using 'A'.   */
                get str(newline$()) using L30470, type$, priority$
L30470:              FMT POS(240), 2*CH(1)
                alloc_adj = 0
                call "PIPINDEX" (#1, reqdship$, reqdship%, ret%)
                if oldalloc$ <> "C" then L30496
                   alloc_adj = alloc
                   alloc = 0
                   call "PIPINDEX" (#1, oldreqdship$, oldreqdship%, ret%)
                   ret% = oldreqdship%
L30496:         req_qty = min(newopenqty - preinv, newopenqty - alloc)
                call "PLANALLC"                                          ~
                       (part$,           /* Part to Allocate           */~
                        req_qty,         /* Quantity Requested         */~
                        alloc_adj,       /* Quantity Allocatable (IN)  */~
                                         /* Old Alloc Complete Qty(OUT)*/~
                        type$,           /* Demand Type                */~
                        priority$,       /* Priority                   */~
                        reqdship%,       /* Date Required              */~
                        today%,          /* Today's Date               */~
                        planflags$(),    /* Planning System Flags      */~
                        #13, #19,        /* PIPMASTR, SFCUM2           */~
                        ret%)            /* RETURN CODE (IN)           */
                                         /* Old shipdate (OUT)         */
                alloc$ = "P"   /* Prevents redoing unless told to */
                alloc  = min(newopenqty, alloc + alloc_adj)
                goto L30710
L30620:     /* Allocate Complete */
                alloc = newopenqty
                goto L30710
L30650:     /* No Allocation (or Preciously Allocated) */
            /* Difference is 'N' will have ALLOC = 0   */
                alloc = min(newopenqty, alloc)
                goto L30710
L30680:     /* Zero Allocation  */
                alloc = 0
                alloc$ = "N"
L30710:     /* Put allocation results back into NEWLINE      */
                put str(newline$()) using L30722, alloc, alloc$
L30722:             FMT POS(125), PD(14,4), POS(246), CH(1)
            return

L30740
*        Load up (or dummy up) Old Line Item
            str(oldline$()) = " "
            call "READ100" (#6, linekey$, oldline%)
            if oldline% = 0% then put #6 using L30810, 0, 0, 0, 1, 0, 0,  ~
                               " ", " ", " ", " ", " "
            get #6 using L30310, str(oldline$())
            get str(oldline$()) using L30810, oldorderqty, oldopenqty,    ~
                                   oldprice, conv_fact, unitprice,       ~
                                             olddiscpct, oldorigdue$,    ~
                                   oldreqdship$, oldlot$, olddemtype$,   ~
                                   oldest$
L30810:         FMT POS( 93), PD(14,4), XX(8)   , PD(14,4),              ~
                    POS(141), PD(14,4), POS(157), PD(14,7), PD(14,4),    ~
                                        POS(173), PD(14,4),              ~
                    POS(200), CH(6), POS(212), CH(6), CH(6),             ~
                    POS(240), CH(1), POS(255), CH(8)
            if conv_fact <> 0 then                                       ~
                oldprice = unitprice / conv_fact /* More accurate value */

            return

        usage_capture

*        If the MPS/PFM Usage Capture flag is turned on, then check to
*        see if the item is a planned item, if not don't update usage
*        files.  If an old lines record truely exists then check to see
*        if its previously captured usage should be removed so that the
*        usage of the buffer line can be added.

            call "READ100" (#13, part$, f1%(13%))
                if f1%(13%) <>  1% then return

*       Determine which dates to use and set them for 'requested' usage
            oldusagedate$ = oldreqdship$  :  newusagedate$ = reqdship$
            if usage_date$ <> "O" then L32160
                oldusagedate$ = oldorigdue$ : newusagedate$ = neworigdue$

L32160
*        Check if old line needs usage modified
            if oldline% = 0% or oldorderqty = 0 then usage_add
            if oldcrhold$ = "C" then usage_add
            if crhold$ = "C" then usage_remove
            if str(newline$(),57%,1%) = hex(ff) then usage_remove
            if oldorderqty <> neworderqty then usage_remove
            if oldusagedate$ <> newusagedate$ then usage_remove
            if usage_store$ = "Y" and oldstore$ <> newstore$ then        ~
                usage_remove
            if olddemtype$ <> newdemtype$ then usage_remove
            goto L32400

        usage_remove
            usageqty = -oldorderqty
            usagetype$ = "UNKN"
            if olddemtype$ = "1" then usagetype$ = "FCST"
            if olddemtype$ = "2" then usagetype$ = "NFCT"
            call "HNYUSESB" (so$, seq$, oldstore$, part$, "R",           ~
                             oldusagedate$, usagetype$, usageqty)

        usage_add
            if crhold$ = "C" then L32400
            if str(newline$(),57%,1%) = hex(ff) then L32400
            usagetype$ = "UNKN"
            if newdemtype$ = "1" then usagetype$ = "FCST"
            if newdemtype$ = "2" then usagetype$ = "NFCT"

            usageqty = neworderqty
            call "HNYUSESB" (so$, seq$, newstore$, part$, "R",           ~
                             newusagedate$, usagetype$, usageqty)

L32400:     return

        process_line_for_mlq        /* Multi-Line Quotation Line Items */
            if mlqyorn$ <> "Y" then return/* Nothing to do? Do nothing */
            if mlquote_seq$ = " " then return  /* No Quote? Do nothing */
            mlq_line = newsanet + oldsanet /* Line Item net +/- change */
*        Begin the MLQ update process. First update the Cross-Ref file.
            if mlqxref$ <> "Y" then goto L32520/* No X-Ref? Next action */
                mlqtotal = 0
                readkey$ = str(mlquote_seq$,,8%) & str(cuscode$) & so$
                call "READ101" (#33, readkey$, f1%(33%))   /* MLQCROSS */
                if f1%(33%) <> 0% then get #33 using L32465, mlqtotal
L32465:              FMT POS(34), PD(14,4)
                mlqtotal = mlqtotal + mlq_line
                put #33 using L32490, str(mlquote_seq$,,8%), cuscode$,    ~
                     so$, mlqtotal                         /* MLQCROSS */
L32490:              FMT CH(8), CH(9), CH(16), PD(14,4)
                if f1%(33%) = 0% then write #33 else rewrite #33

L32520
*        Next, update the MLQMASTR file. This indicates 'Quote awarded'.
            readkey$ = str(mlquote_seq$,,8%)
            call "READ101" (#30, readkey$, f1%(30%))       /* MLQMASTR */
            if f1%(30%) = 0% then goto L32589 /* Not there? Next action */
                get #30 using L32550, mlqtotal  /* Get prev Award Total */
L32550:              FMT POS(1092), PD(14,4)
                mlqtotal = mlqtotal + mlq_line               /* Adjust */
                put #30 using L32565, cuscode$, so$, date, mlqtotal
L32565:              FMT POS(1061), CH(9), CH(16), CH(6), PD(14,4)
                rewrite #30                                /* MLQMASTR */

L32589
*        MLQ Line Item processing continues.
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
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
*        Remove any left over 'Kill me' tranactions.
            bufferkey$ = " "
            call "DELETE" (#9, bufferkey$, 3%)
            call "MESSAGE" addr("DE", port$, return%)
            end
