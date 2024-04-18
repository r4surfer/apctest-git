        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   TTTTT   CCC   RRRR    OOO   L      U   U  PPPP    *~
            *  S        T    C   C  R   R  O   O  L      U   U  P   P   *~
            *   SSS     T    C      RRRR   O   O  L      U   U  PPPP    *~
            *      S    T    C   C  R   R  O   O  L      U   U  P       *~
            *   SSS     T     CCC   R   R   OOO   LLLLL   UUU   P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCROLUP - Standard Cost Roll Up                          *~
            *----------------------------------------------------------Q*~
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
            * 04/03/87 ! Original                                 ! KEN *~
            * 12/10/87 ! Don't exit if error hit...               ! HES *~
            * 04/03/89 ! Don't STOP either,  Added Error List for ! MJB *~
            *          !  errors during roll up.  Required a      !     *~
            *          !  second return code to be added to the   !     *~
            *          !  calling arg list for sub STCSPROL.      !     *~
            * 09/20/89 ! Added ASKUSER to inform user if Error    ! JDH *~
            *          !   List was created.                      !     *~
            * 11/14/90 ! Added Management DMC calculation/storage.! JDH *~
            * 06/14/91 ! Externalized Management DMC calculation. ! KAB *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 09/22/93 ! PACKed before MATed costs.               ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "STCROLUP" (set$,               /* Cost Set Being Rolled  */ ~
                        rollup$,            /* Roll-up,Flag           */ ~
                        #2,                 /* SYSFILE2               */ ~
                        #5,                 /* BOMMASTR               */ ~
                        #10,                /* STCHNY                 */ ~
                        #12,                /* STCWCACT               */ ~
                        #13,                /* STCLABOR               */ ~
                        #16,                /* RTEMASTR               */ ~
                        #6,                 /* WCMASTR                */ ~
                        #15,                /* STCCHNGS               */ ~
                        #4,                 /* HNYMASTR               */ ~
                        #17)                /* MGTFCTR1               */

        dim                                                              ~
            assybom$3,                   /* Assembly BOM               */~
            assyrte$3,                   /* Assembly Route             */~
            bomcost(12),                 /* Bill of Material Costs     */~
            bomcost$96,                  /* Bill of Material Costs     */~
            company$60,                  /* Company Name for Header    */~
            date$8,                      /* Current Date               */~
            level%(100),                 /* Low Level Number           */~
            msccost(12),                 /* Misc. Costs                */~
            p$(100)56,                   /* Low Level Plowkey          */~
            part$25,                     /* Part Code                  */~
            plowkey$100,                 /* Miscellaneous Read/Plow Key*/~
            rtecost(12),                 /* Route Costs                */~
            rtecost$96,                  /* Route Costs                */~
            totals(12),                  /* Total Costs (work)         */~
            total(1),                    /* Total Costs (Reciever)     */~
            userid$3,                    /* Current User Id            */~
            work(1,12)                   /* Work Array                 */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
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
            * # 2 ! SYSFILE2 ! System Control File                      *~
            * # 4 ! HNYMASTR ! Inventory Master File                    *~
            * # 5 ! BOMMASTR ! BOM relationship file                    *~
            * #10 ! STCHNY   ! Inventory Master File                    *~
            * #12 ! STCWCACT ! Work Center Activity Costs               *~
            * #13 ! STCLABOR ! Labor Costs                              *~
            * #16 ! RTEMASTR ! Route Master File                        *~
            * #17 ! MGTFCTR1 ! Management Factors - Billing & Transfer  *~
            * #22 ! WRKFILE1 ! BOM keys                                 *~
            * #23 ! WRKFILE2 ! Low Level Tag File                       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #22, "WRKFILE1",                                      ~
                        varc,     indexed,  recsize = 56,                ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56          ~

            select #23, "STCLLTG3",                                      ~
                        varc,     indexed,  recsize = 29,                ~
                        keypos = 1,    keylen = 29,                      ~
                        alt key  1, keypos =    5, keylen =  25          ~

L04000:     ask% = 2%
            call "ASKUSER" (ask%, "* * * ROLL-UP ABOUT TO BEGIN * * *",  ~
                 "Press PF-1 to EXIT without Roll-Up.",                  ~
                 "Press PF-16 or PF-32 to Continue.",                    ~
                 "PF-32 will OPEN the Bill, Route and Other Common Files ~
        ~for EXCLUSIVE use.")

            if ask% = 1% then end
            if ask% = 16% then L05000
            if ask% = 32% then L05000
               goto L04000

L05000:     call "SHOSTAT" ("Opening Files, One Moment Please")


            call "GETUFBS1" addr(#5, bommastr%)
            records% = 0%
            if bommastr% = 0% then L05120
              call "GETNAMES" addr(#5, file$, library$, volume$)
              call "READFDR"  addr(file$, library$, volume$, 0%, "RC",   ~
                                   records%, return%)
              if return% <> 0% then records% = 100%

L05120:     records% = max(records%, 100%)
            call "WORKOPEN" (#22, "OUTPT", records%, f2%(22))

            if ask% = 16% then L09000

            call "GETUFBS1" addr(#16, rtemastr%)
            call "GETUFBS1" addr(#6, wcmastr%)

            if bommastr% = 0% then L05200
               close #5
               call "OPENFILE" (#5, "INPUT", f2%, " ", " ")
L05200:     if rtemastr% = 0% then L05230
               close #16
               call "OPENFILE" (#16, "INPUT", f2%, " ", " ")
L05230:     if wcmastr%  = 0% then L09000
               close #6
               call "OPENFILE" (#6, "INPUT", f2%, " ", " ")

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            init (hex(00)) plowkey$

            call "SHOSTAT" ("Costing Purchased Parts.")
            erte%, ebom%, records% = 0%

            call "COMPNAME" (12%, company$, erte%)
            date$ = date
            call "DATEFMT" (date$)
            select printer(85)
            err%, pcntr% = 0%  :  lcntr% = 999%

        REM *************************************************************~
            * FIND NON-ASSEMBLY PARTS, COST THEM.                       *~
            *-----------------------------------------------------------*~
            * BUILD WORKFILE FOR LOW LEVEL CODES.                       *~
            *************************************************************

            call "PLOWNEXT" (#10, plowkey$, 0%, f1%(10))
                goto L10090
L10080:     call "READNEXT" (#10, f1%(10))
L10090:         if f1%(10) = 0% then L10250
            get #10, using L10110, str(plowkey$,,25), str(plowkey$,26)
L10110:         FMT CH(25), POS(38), CH(3)
            if str(plowkey$,26,3) <> " " then L10170
L10130:        gosub cost_part_purchased
               goto L10080

L10170:     str(plowkey$,29,3) = "  0"
            call "PLOWALTS" (#5, plowkey$, 0%, 28%, f1%(5))
                if f1%(5) = 0% then L10130
            records% = records% + 1%
            goto L10210

L10190:     call "PLOWALTS" (#5, plowkey$, 0%, 28%, f1%(5))
                if f1%(5) = 0% then L10080
L10210:     write #22, using L10220, key(#5,1%)
L10220:           FMT CH(56)
            goto L10190

L10250:     close #22
            if records% = 0% then L65000

            call "WORKOPN2" (#22, "INPUT", 100%, f2%(22))
            records% = max(records%, 100%)
            call "WORKOPEN" (#23, "IO", records%, f2%(23))

            call "SHOSTAT" ("Setting Low Level Codes.")

        REM *************************************************************~
            * SET LOW LEVEL CODES                                       *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

            init (hex(00)) plowkey$

L11080:     call "PLOWALTS" (#22, plowkey$, 0%, 0%, f1%(22))
               if f1%(22) = 0% then L11370
            call "REDALT0" (#23, plowkey$, 1%, f1%(23))
               if f1%(23) <> 0% then L11140
            part$ = plowkey$:l% = 0%
            gosub L11170
L11140:     init (hex(ff)) str(plowkey$,26)
            goto L11080

L11170:     l% = l% + 1%
            p$(l%) = part$
            level%(l%) = 99999%

L11210:     call "PLOWALTS" (#22, p$(l%), 1%, 25%, f1%(22))
                if f1%(22) = 0% then L11320
            part$ = str(p$(l%),26,25)
            call "REDALT0" (#23, part$, 1%, f1%(23))
                if f1%(23) <> 0% then L11270
            gosub L11170
L11270:     get #23, using L11280, temp%
L11280:         FMT BI(4)
            level%(l%) = min(level%(l%), temp% - 1%)
            goto L11210

L11320:     write #23, using L11330, level%(l%), p$(l%)
L11330:         FMT BI(4), CH(25)
            l% = l% - 1%
            return

L11370:     call "SHOSTAT" ("Costing Assembled Parts.")

        REM *************************************************************~
            * SET COSTS, LEVEL BY LEVEL                                 *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

            init (hex(00)) plowkey$

            call "PLOWNEXT" (#23, plowkey$, 0%, f1%(23))
               goto L12120

L12110:     call "READNEXT" (#23, f1%(23))
L12120:        if f1%(23) = 0% then L65000

            get #23, using L12150, plowkey$
L12150:         FMT POS(5), CH(25)
            gosub cost_part
            goto L12110

        REM *************************************************************~
            * COST PARTS, USE MISC. AS IS.  DERIVE RTE AND BOM COSTS.   *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        cost_part

            call "READ100" (#10, plowkey$, f1%(10))
               if f1%(10) = 0% then return

        cost_part_purchased

            get #10, using L13110, assybom$, assyrte$, srq, msccost()
L13110:         FMT POS(38), 2*CH(3), PD(14,4), POS(252), 12*PD(14,4)

            call "STCSPROL" (plowkey$, assybom$, assyrte$, srq,          ~
                             bomcost(), rtecost(),                       ~
                             #5,          /* BOMMASTR                  */~
                             #10,         /* STCHNY                    */~
                             #12,         /* STCWCACT                  */~
                             #13,         /* STCLABR                   */~
                             #16,         /* RTEMASTR                  */~
                             #6,          /* WCMASTR                   */~
                             erte%,       /* Ret Code for RTE Error    */~
                             ebom%)       /* Ret Code for BOM Error    */

            if erte% <> 0% or ebom% <> 0% then gosub error_print

            call "PACKZERO" (bomcost(), bomcost$)
            call "PACKZERO" (rtecost(), rtecost$)
            mat totals = bomcost + rtecost
            mat totals = totals  + msccost
            mat work = con : mat total = work * totals
            total(1%) = round(total(1%),4)

            if mdmc = -1 then L13221
            call "STCMDMC" (str(plowkey$,,25), set$, totals(), mdmc,     ~
                            #4, #17, #2)

L13221:     call "READ101" (#10, plowkey$, f1%(10))
               if f1%(10) = 0% then return
            put #10, using L13250, total(), bomcost$, rtecost$,           ~
                                 assybom$, assyrte$, srq, mdmc
L13250:         FMT POS(52), PD(14,4), CH(96), CH(96), POS(348), 2*CH(3),~
                    PD(14,4), POS(364), PD(14,4)
            rewrite #10

            return

        error_print
            if err% = 0% then err% = 1%
            if lcntr% > 56% then gosub page_head
            if erte% <> 0% then print using L60150, str(plowkey$,,25),    ~
                                "RTE ID", assyrte$
            if ebom% <> 0% then print using L60150, str(plowkey$,,25),    ~
                                "BOM ID", assybom$
            lcntr% = lcntr% + 1%
            return

        page_head
            if pcntr% <> 0% then print page
            pcntr% = pcntr% + 1%
            print using L60030, company$
            print skip(1)
            print using L60060, date$, pcntr%
            print skip(2)
            print using L60090
            print using L60120
            lcntr% = 6%
            return

        REM *************************************************************~
            *   I M A G E   S T A T E M E N T S   F O R   E R R O R S   *~
            *************************************************************
L60030: %            ####################################################~
        ~########

L60060: %Run: ########                COST ROLL UP ERROR LOG             ~
        ~       Page: ###

L60090: %Part Number                Error Message

L60120: %-------------------------  -----------------------------------

L60150: %#########################  Problem Encountered with ###### ###

L60170: %                     * * * * * E N D   O F   L O G * * * * *


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
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            plowkey$ = "STC.HDR." & set$
            call "READ101" (#2, plowkey$, f1%(2))
               if f1%(2) = 0% then L65240
            put #2 using L65200, " ", date
L65200:         FMT POS(420), CH(1), POS(427), CH(6)
            if err% <> 0% then put #2 using L65200, "Y", date
            rewrite #2
            if err% <> 0% then L65240

            init (hex(00)) plowkey$
            call "DELETE" (#15, plowkey$, 0%)

L65240:     if pcntr% = 0% then L65255
                print skip(3)
                print using L60170
L65255:     call "FILEBGON" (#22)
            call "FILEBGON" (#23)

            close printer

            if err% = 0% then L65285
                log% = 0%
                call "ASKUSER" (log%, "* * *  ERROR LOG  * * *",         ~
                          "There have been problems with the roll-up.",  ~
                          "Use PRINTQ to examine the Error Log.",        ~
                          "Press  -Any PF Key-  to acknowledge.")

L65285:     if ask% = 16% then end

            if bommastr% = 0% then L65320
               close #5
               call "OPENCHCK" (#5, f2%, 0%, 0%, " ")
L65320:     if rtemastr% = 0% then L65350
               close #16
               call "OPENCHCK" (#16, f2%, 0%, 0%, " ")
L65350:     if wcmastr%  = 0% then L65390
               close #6
               call "OPENCHCK" (#6, f2%, 0%, 0%, " ")

L65390:     end
