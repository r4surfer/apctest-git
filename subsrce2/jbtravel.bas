        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  JJJJJ  BBBB   TTTTT  RRRR    AAA   V   V  EEEEE  L       *~
            *    J    B   B    T    R   R  A   A  V   V  E      L       *~
            *    J    BBBB     T    RRRR   AAAAA  V   V  EEEE   L       *~
            *  J J    B   B    T    R   R  A   A   V V   E      L       *~
            *   J     BBBB     T    R   R  A   A    V    EEEEE  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBTRAVEL - Prints a job order traveler.                   *~
            *            Shows where and when the job is supposed to be *~
            *            based on the RTEMASTR and WCOUT files.  Room   *~
            *            is allowed for user to record the actual dates,*~
            *            quantities, etc.                               *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/04/83 ! ORIGINAL                                 ! ECR *~
            * 11/20/85 ! Added TXTFILE for Text printing          ! MJB *~
            * 10/20/86 ! Basicaly WCOUT driven now.               ! HES *~
            * 02/20/87 ! Print Parent/Job serial #s               ! JRH *~
            * 02/19/88 ! Fix text print logic, added new flag     ! HES *~
            * 02/22/88 ! Added Drawing Data To Header             ! HES *~
            * 04/25/88 ! Added Bar Code Printing Capability       ! MDE *~
            * 05/06/88 ! Added Job Descrip to Header, Both types  ! RJM *~
            * 09/09/88 ! Chngd to work w/C9301-P Font Cartridge   ! JDH *~
            * 02/14/89 ! Added Control Number & corrected length  ! MJB *~
            *          !  of Job Description when printed.        !     *~
            *          !  Also increased space between Step & WC  !     *~
            *          !  Bar Codes.                              !     *~
            * 07/20/89 !  Corrected Test Printing                 ! LKM *~
            * 08/09/89 !  Misc clean up.                          ! JDH *~
            * 01/11/94 ! Explicitly show Move Queue Before &      ! KAB *~
            *          !  After to avoid confusion.               !     *~
            * 06/28/94 ! Added Job Text (killed PRR 13084) and    ! LDJ *~
            *          !  HNYACTXF Text as text sources for print.!     *~
            * 08/10/94 ! PRR 13174 - When printing a Summary Trvlr! RJH *~
            *          !  use a consolidated WCOUT array which    !     *~
            *          !  combines duplicate (Overlaped) steps.   !     *~
            * 10/07/94 ! Added SO Line Text for printing.         ! HES *~
            *          ! Implement new barcoding scheme via sub.  !     *~
            *          ! Re-arrange summry travel not to chop off !     *~
            *          ! descriptions.                            !     *~
            *          ! Corrrect problem with no text printing   !     *~
            *          ! here if pick list printed at same time.  !     *~
            * 06/18/96 ! Determine Plan Rte Seq from Rte array not! RJH *~
            *          !  WCOUT Seq #.                            !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "JBTRAVEL" (part$,           /* PART NUMBER                */~
                        job$,            /* JOB CODE                   */~
                        qtymake,         /* QTY OF THIS PART TO MAKE   */~
                        #1,              /* RTEMASTR FILE              */~
                        #2,              /* WCMASTR  FILE              */~
                        #3,              /* WCOUT FILE                 */~
                        #4,              /* JBCROSS2 FILE              */~
                        #5,              /* HNYMASTR FILE              */~
                        #7,              /* BOMMASTR FILE              */~
                        #8,              /* ENGMASTR FILE              */~
                        #9,              /* SYSFILE2 FILE              */~
                        #10)             /* JBMASTR2 FILE              */

        dim                                                              ~
            acode$4,                     /* Activity Code              */~
            act_textid$4,                /* HNYACTXF Text ID           */~
            bar_code$1,                  /* Bar Codes Active; 0% = No  */~
            activity$30,                 /* WC ACTIVITY                */~
            basedate$6,                  /* Planning Calendar Base Date*/~
            bomused$3,                   /* EFFECTIVE BOM              */~
            ctlnbr$19,                   /* Shop Control Number        */~
            date$(2)8,                   /* Formatted Dates For Print  */~
            descr$34,                    /* part description           */~
            hddate$45,                   /* HEADER DATE                */~
            hdr_textid$4,                /* HEADER TEXT ID             */~
            header$80,                   /* HEADER FOR REPORT          */~
            jobdescr$32,                 /* JOB DESCRIPTION            */~
            job$8,                       /* JOB ORDER CODE             */~
            jobtxtid$4,                  /* Job Master Rec Text ID     */~
            part$25,                     /* PART CODE                  */~
            partdescr$77,                /* PART DESCRIPTION           */~
            plowkey$99,                  /* Misc PLOW variable         */~
            print$(2)11,                 /* SET UP TIME FOR WC         */~
            readkey$99,                  /* Misc PLOW variable         */~
            record$(256)67,              /* FORMATTED WCOUT DATA       */~
            record_packed$(256)67,       /* Consolidated WCOUT DATA    */~
            rslt$20,                     /* OPENCHCK variable          */~
            rte$(255)200,                /* ROUTING ARRAY              */~
            rteused$3,                   /* EFFECTIVE RTE              */~
            serline$80,                  /* PRINT LINE- SERIAL NUMBERS */~
            sotextid$4,                  /* Sales Order Line Text Id   */~
            step$7,                      /* STEP LABEL                 */~
            temp_step$7,                 /* STEP LABEL                 */~
            test_date1$6,                /* Consolidateion Test Date   */~
            test_date2$6,                /* Consolidateion Test Date   */~
            test_step$7,                 /* Consolidateion Test Step   */~
            textid$4,                    /* Text ID                    */~
            ttype$1,                     /* TRAVELER TYPE TO PRINT     */~
            userid$3,                    /* Mister Big                 */~
            wc$(4)4,                     /* WORK CENTERS AND CONCURENTS*/~
            wcdescr$32,                  /* WORK CENTER DESCRIPTION    */~
            work$50                      /* WCMASTR PLOW KEY           */

        dim f1%(64)                      /* RECORD-ON-FILE FLAGS       */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 08/12/96 Last Wang Release               "
        REM *************************************************************

            select #6, "TXTFILE",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos =  1, keylen =  11

            select #11, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #12, "HNYACTXF",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =    1, keylen =  29,                     ~
                        alt key  1, keypos =   26, keylen =   4, dup

            select #13, "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            if ttype$ <> " " then L09170

            REM Following Code Is Executed Only Once...
                call "OPENCHCK" (#6,  0%, 0%, 0%, rslt$)
                call "OPENCHCK" (#11, fs%, 0%, 0%, rslt$)
                call "OPENCHCK" (#12, fs12%, 0%, 0%, rslt$)
                call "OPENCHCK" (#13, 0%, 0%, 0%, rslt$)
                call "READ100" (#9, "MONTHS OPEN", f1%(9%))
                    if f1%(9%)=0% then end
                get #9, using L09050, basedate$
L09050:         FMT XX(32), CH(6)
                call "READ100" (#9, "SWITCHS.SFC", f1%(9%))
                    if f1%(9) <> 0 then get #9, using L09090,ttype$,tform$,~
                     bar_code$
L09090:         FMT XX(27), CH(1), XX(3), CH(1), XX(1), CH(1)
             if ttype$ = " " then ttype$ = "B"
             if tform$ < "A" then tform$ = "S"
             call "EXTRACT" addr("ID", userid$)
             pge% = 56%
             call "BARCODES" ("I", bar_code$, " ", " ", " ", " ", " ",   ~
                              " ", 134%, barsize%)

L09170: REM *************************************************************~
            *              I N I T I A L I Z A T I O N                  *~
            *                                                           *~
            * Do prepritory work for printing of traveler               *~
            *************************************************************

            line% = 1000%
            ser%, page%, report% = 0

            REM Call Subroutine to loadup the datas.
            call "PLNTRVLR" ("JOB ORDER: " & str(job$),                  ~
                        #1,              /* RTEMASTR FILE              */~
                        #3,              /* WCOUT FILE                 */~
                        #4,              /* JBCROSS2 FILE              */~
                        #7,              /* BOMMASTR FILE              */~
                        #8,              /* ENGMASTR FILE              */~
                        #9,              /* SYSFILE2 FILE              */~
                        #10,             /* PIPIN or JBMASTR2          */~
                        rteused$,        /* Route For Tag (Returned)   */~
                        bomused$,        /* Bill For Tag (Returned)    */~
                        rte$(),          /* Phantomized Routing        */~
                        rtes%,           /* Number Of Elements in Array*/~
                        record$(),       /* Traveler Array             */~
                        ptr%)            /* Number Of Elements in Array*/

            if rtes% <= 0% and ptr% <= 0% then exit_program
            call "CONVERT" (qtymake, -0.4, qtymake$)
            dref$ = " "
            call "GETCODE" (#5, part$, descr$, 1%, 99, f1%(5))
                if f1%(5) <> 0 then L10310
                descr$ = "** NOT ON FILE!! **"
                goto L10330
L10310:     get #5, using L10320, dref$
L10320:     FMT POS(139), CH(16)
L10330:     partdescr$ = part$ & " " & descr$

            sotextid$ = " "
            call "DESCRIBE" (#10, job$, jobdescr$, 1%, f1%(10%))
                if f1%(10%) = 0% then jobtxtid$ = all(hex(ff))
                if f1%(10%) = 0% or jobdescr$ = "()" then jobdescr$ = " "
                if f1%(10%) <> 0% then                                   ~
                     get #10 using L10400, jobtxtid$, ctlnbr$
L10400:              FMT POS(228), CH(4), POS(1120), CH(19)
                     call "READ100" (#13, ctlnbr$, f1%(13%))
                     if f1%(13%) = 0% then L10430
                        get #13, using L10422, sotextid$
L10422:                 FMT POS(242), CH(4)
L10430:     if jobdescr$ = descr$ then jobdescr$ = " "
            header$ = "JOB: " & job$ & " " & jobdescr$
            if ctlnbr$ = " " then L10470
                header$ = header$ & " " & "Ctl # " & ctlnbr$
L10470:     call "STRING" addr ("CT", header$, 80%)
            select printer(134)
            REM Note - SETPRNT could cause trouble in this routine...

        REM *************************************************************~
            *          P R I N T   S U M M A R Y   R O U T E            *~
            *                                                           *~
            * Prints the short traveler, if no WCOUTS exist             *~
            *************************************************************

            plowkey$ = str(part$) & str(rteused$) & hex(00)
            call "PLOWNEXT" (#1,plowkey$,28%,f1%(1))
            if f1%(1) <> 1 then L11080
               get #1 using L11064, hdr_textid$
L11064:        FMT POS(127), CH(4)
               textid$ = hdr_textid$

L11080:     REM This print only happens in case where no WCOUTS, but     ~
                a route is linked to job...
            if ptr% > 0% then L12000    /* Process Normal Codition */

            REM *ONLY* If No Wcouts...
            if rtes% = 0% then exit_program
            gosub header_text
            gosub new_page
            for c% = 1 to rtes%
                get rte$(c%), using L11190, wc$(1), step$, activity$,     ~
                                                              textid$
L11190:         FMT POS(32), CH(4), POS(88), CH(5), POS(98), CH(30),     ~
                                                          XX(4), CH(4)
                temp% = val(str(step$,5,1),1)
                str(step$,5) = " "
                if temp% = 0 then L11290
                     step$ = step$ & "-"
                     convert temp% to str(step$,pos(step$="-")+1),pic(00)
L11290:         templine% = line% : line% = line% + 1% + barsize%
                gosub new_page : line% = min(templine%, line%)
                call "GETCODE" (#2, wc$(1), wcdescr$, 0%, 99, f1%(2))
                     if f1%(2) = 0 then wcdescr$ = "  ** NOT ON FILE **"

                print using L60370,step$, wc$(1), wcdescr$
                print using L60340, activity$
                line% = line% + 2%
                call "BARCODES" ("P", step$, "1", wc$(1), "20", " ", " ",~
                                 " ", 0%, line%)

L11430:         call "TXTPRINT" (#6, 0%, 134%, textid$, "JB0004", 6%,    ~
                                           line%, pge%, "N", " ", stat%)
                if stat% = 0% then L11480
                   gosub new_page
                   goto L11430
L11480:         print  :  line% = line% + 1%
L11481:         call "TXTPRINT" (#6, 0%, 134%, act_textid$, "JB0004", 6%,~
                                             line%,pge%, "N", " ", stat%)
                if stat% = 0% then L11486
                   gosub new_page
                   goto L11481
L11486:         print  :  line% = line% + 1%
            next c%
            goto exit_program

L12000: REM *************************************************************~
            *          P R I N T   S U M M A R Y   R O U T E            *~
            *                                                           *~
            * Prints the normal short traveler, if user wants it.       *~
            *************************************************************

            if ptr% = 0% then exit_program
            if ttype$ = "D" then L13000
            gosub consolidate_travler_array /*Pack for Overlapping WC's*/
            doing_summary% = 1%
            gosub header_text
            gosub new_page

            for c% = 1 to ptr_packed%
                gosub get_record
                templine% = line% : line% = line% + 1% + barsize%
                gosub new_page : line% = min(templine%, line%)
                call "GETCODE" (#2, wc$(1), wcdescr$, 0%, 99, f1%(2))
                     if f1%(2) = 0 then wcdescr$ = "  ** NOT ON FILE **"
                work$ =  str(date$(1)) & "-" & date$(2)

                print using L60320, step$, wc$(1), wcdescr$, work$
                print using L60340, activity$
                line% = line% + 2%
                call "BARCODES" ("P", step$, "1", wc$(1), "30", " ", " ",~
                                 " ", 0%, line%)
                if ttype$ <> "S" then L12328
L12270:         call "TXTPRINT" (#6, 0%, 134%, textid$, "JB0004", 6%,    ~
                                            line%, pge%, "N", " ", stat%)
                if stat% = 0% then L12312
                   gosub new_page
                   goto L12270
L12312:         print  :  line% = line% + 1%
L12321:         call "TXTPRINT" (#6, 0%, 134%, act_textid$, "JB0004", 6%,~
                                             line%,pge%, "N", " ", stat%)
                if stat% = 0% then L12328
                   gosub new_page
                   goto L12321
L12328:         print  :  line% = line% + 1%
            next c%

            doing_summary% = 0%  /* End of summary */
            line% = 1000%      /* End of summary.  Ensure new page. */

L13000: REM *************************************************************~
            *         P R I N T   D E T A I L E D   R O U T E           *~
            *                                                           *~
            * Prints the traveler work sheet, if user wants it.         *~
            *************************************************************

            if ttype$ = "S" then exit_program  /* DONT WANT DETAILED */
            report% = 1%                       /* WC REPORT FORM   */
            textid$ = hdr_textid$
            chdrsw% = 0%
            gosub header_text
            if chdrsw% = 0% then gosub new_page
        REM GOSUB NEW_PAGE
            for c% = 1% to ptr%
                gosub get_record
                print$(), work$ = " "
                if alt% > 0 then work$ = "(Alternate)"
                call "CONVERT" (totbmq, -0.2, print$(1))
                if print$(1) = "0" then L13200
                if print$(1) = "1" then print$(1) = print$(1) &" Day"    ~
                                   else print$(1) = print$(1) &" Days"
L13200:         call "CONVERT" (parts_in, -0.2, print$(2))
                call "PUTPAREN" (print$(1))
                call "PUTPAREN" (print$(2))
                print$(1) = print$(1) & "__________"
                print$(2) = print$(2) & "__________"
                templine% = line% : line% = line% + 13% + (barsize% * 2%)
                gosub new_page : line% = min(line%, templine%)
                call "GETCODE" (#2, wc$(1), wcdescr$, 0%, 99, f1%(2))
                print using L60520, wc$(1), work$, date$(1), date$(2),    ~
                                   print$(1), print$(2)
                call "BARCODES" ("P", wc$(1), "1", " ", " ", " ", " ",   ~
                                 " ", 0%, line%)
                print using L60650, wcdescr$
                line% = line% + 2%

                if wc$(1) = "VEND" then L13360
                   call "WCUN2HRS" (#2, wc$(1), 0, totsu, " ")
L13360:         call "CONVERT" (totsu, -0.2, print$(1))
                call "CONVERT" (parts_out, -0.2, print$(2))
                call "PUTPAREN" (print$(1))
                call "PUTPAREN" (print$(2))
                print$(1) = print$(1) & "__________"
                print$(2) = print$(2) & "__________"
                print using L60580, step$, print$(1), print$(2)
                call "BARCODES" ("P", step$, "12", " ", " ", " ", " ",   ~
                                 " ", 0%, line%)
                print using L60670, activity$
                line% = line% + 2%

                if wc$(1) = "VEND" then L13490
                   call "WCUN2HRS" (#2, wc$(1), 0, totrun, " ")
L13490:         call "CONVERT" (totrun, -0.2, print$(1))
                call "PUTPAREN" (print$(1))
                print$(1) = print$(1) & "__________"
                print using L60680, print$(1)
                line% = line% + 1
                if wc$(2)=" " and wc$(3)=" " and wc$(4)=" " then L13620
                   print using L60710, wc$(2), wc$(3), wc$(4)
                   line% = line% + 1
                   goto L13621
L13620:         print skip(1) : line% = line% + 1%
L13621:         call "CONVERT" (totamq, -0.2, print$(1))
                if print$(1) = "0" then L13625
                if print$(1) = "1" then print$(1) = print$(1) &" Day"    ~
                                   else print$(1) = print$(1) &" Days"
L13625:         call "PUTPAREN" (print$(1))
                print$(1) = print$(1) & "__________"
                print using L60730, print$(1)
                line% = line% + 1%                     /* MQ Aft,rework */
                print skip(1) : line% = line% + 1%
                print using L60745 : line% = line% + 1% /* other         */
                print skip(1) : line% = line% + 1%
                print using L60940 : line% = line% + 1% /* total         */
                print skip(1) : line% = line% + 1%
                print using L60920 : line% = line% + 1% /* operator      */
                if textid$ = hex(00000000) then textid$ = " "
                if textid$ = hex(ffffffff) then textid$ = " "
                if textid$ = " " then L13751
L13680:         call "TXTPRINT" (#6, 0%, 134%, textid$, "JB0004", 6%,    ~
                                             line%,pge%, "N", " ", stat%)
                if stat% = 0% then L13730
                   gosub new_page
                   goto L13680
L13730:         print
                line% = line% + 1%
L13751:         call "TXTPRINT" (#6, 0%, 134%, act_textid$, "JB0004", 6%,~
                                             line%,pge%, "N", " ", stat%)
                if stat% = 0% then L13756
                   gosub new_page
                   goto L13751
L13756:         print skip(2)
                line% = line% + 2
                if tform$ <> "O" then L13840
                   REM Force Top Of Form...
                   print skip(1)
                   if c% = ptr% then L13830
                      nextwc$ = str(record$(c%+1),10,4)
                      line% = 1000%
                      print using L60760, c%, ptr%, nextwc$
                      goto L13840
L13830:            print using L60770, ptr%
L13840:     next c%
            goto exit_program     /* ALL DONE! */

        header_text
           if jobtxtid$ = hex(ffffffff) then jobtxtid$ = " "
           if jobtxtid$ = hex(00000000) then jobtxtid$ = " "
           if textid$   = hex(ffffffff) then textid$ = " "
           if textid$   = hex(00000000) then textid$ = " "
           if sotextid$ = hex(ffffffff) then sotextid$ = " "
           if sotextid$ = hex(00000000) then sotextid$ = " "
           if jobtxtid$=" " and textid$=" " and sotextid$=" " then return
           gosub common_header
           gosub serial_line_tester
           if jobtxtid$ = " " then L14049
L14044:        call "TXTPRINT" (#6,0%,134%,jobtxtid$,"JB0004", 5%, line%,~
                                                  pge%, "Y", " ", stat%)
               if stat% = 0% then L14049
                  gosub common_header
                  goto L14044
L14049:    if textid$ = " " then L14104
L14080:        call "TXTPRINT" (#6,0%,134%,textid$,"JB0004",  5%,  line%,~
                                                  pge%, "Y", " ", stat%)
               if stat% = 0% then L14104
                  gosub common_header
                  goto L14080
L14104:    if sotextid$ = " " then L14112
L14106:        call "TXTPRINT" (#6,0%,134%,sotextid$,"JB0004", 5%, line%,~
                                                  pge%, "Y", " ", stat%)
               if stat% = 0% then L14112
                  gosub common_header
                  goto L14106
L14112:    print skip(1)
           line% = line% + 1%
           gosub new_page_2
           return

        print_serial_line
            line% = line% + 1%
            if line% >= pge% then gosub common_header
            print using L60170, serline$
            serline$ = " " : serlth% = 10%
            return

        serial_line_tester
            if ser% <> 0% then return
                ser% = 1%
            if fs% < 0% then return
            length%, enabled% = 0%
            call "SERENABL" (part$, enabled%, length%, #9, #5)
            if enabled% = 0% then return
            serline$ = "SER. #S:" : serlth% = 10%
            plowkey$ = "1" & job$
L14300:     call "PLOWALTS" (#11, plowkey$, 2%, 31%, f1%(11))
            if f1%(11) = 0 then goto L14370
            if 80% - serlth% < length% then gosub print_serial_line
            get #11 using L14340, str(serline$, serlth%)
L14340:         FMT  POS(32), CH(20)
            serlth% = serlth% + length% + 1%
            goto L14300
L14370:     if serlth% > 10% then gosub print_serial_line
            line% = line% + 1% : print
            return

        REM *************************************************************~
            *             H E A D I N G S   R O U T I N E S             *~
            *                                                           *~
            * Prints report headings when nessasary                     *~
        REM **************************************************************

        new_page:
           if line% < pge% then return
L14720:    gosub common_header
           gosub serial_line_tester
        new_page_2
           if line% + 4% >= pge% then L14720
           if report% = 1% then L14830
              print using L60230          /* WORK CENTER LIST */
              print                      /*  Summary Header  */
              print using L60260
              print using L60290
              line% = line% + 4%
              return

L14830:    print using L60430             /* WORK CENTER FORM */
           print                         /*  Detail Header   */
           print using L60460
           print using L60490
           line% = line% + 4%
           return

        common_header
*         SELECT PRINTER (134)
           chdrsw% = 1%
           print page
           page% = page%+1
           call "DATE" addr("HD", hddate$)
           call "SPCESMSH" (hddate$, 2%)
           call "STRING" addr("CT", hddate$, 45%)
           print using L60080, userid$, page%
           print using L60060, hddate$
           print using L60170, header$
           line% = 3%
           call "BARCODES" ("P", job$, "22", " ", " ", " ", " ", " ", 0%,~
                            line%)
           print
           print using L60140, partdescr$, rteused$
           line% = line% + 2%  : if page% > 1% then L15040
           call "BARCODES" ("P", part$, "7", " ", " ", " ", " ", " ", 0%,~
                            line%)
L15040:    print using L60200, qtymake$, "#", dref$, bomused$
           print
           line% = line% + 2%
           return

        get_record
            if doing_summary% = 1% then L30035
            get record$(c%), using L30025, step$, rteseq%, wc$(1), wc$(2),~
                wc$(3), wc$(4), totbmq, totsu, totrun, totamq,           ~
                acode$, date$(1), date$(2), parts_in, parts_out, alt%
L30025:         FMT CH(7), BI(2), 4*CH(4), 4*BI(2), CH(4), 2*CH(6),      ~
                                                        2*PD(14,4), BI(2)
             goto L30120
L30035:     get record_packed$(c%), using L30050, step$, rteseq%,         ~
                                      wc$(1%), wc$(2%), wc$(3%), wc$(4%),~
                                      acode$, date$(1%), date$(2%)
L30050:     FMT CH(7), BI(2), 4*CH(4), POS(34), CH(4), 2*CH(6)

L30120:     call "DATEFMT" (date$(1))
            call "DATEFMT" (date$(2))
            textid$ = all(hex(ff))
            act_textid$ = all(hex(ff))
            activity$ = "Off Planned Route: " & acode$
*          IF RTESEQ% < 1 OR RTESEQ% > RTES% THEN 30182
            gosub get_plan_rte_seq
            if success% <> 1% then L30182
            get rte$(pln_rteseq%), using L30180, acode$, activity$, textid$
L30180:     FMT POS(94), CH(4), CH(30), XX(4), CH(4)

L30182
*          Get HNYACTXF Text ID using Job Part and Step Activity Code
            if fs12% = 0% then return    /* No FILE          */
            if acode$ = " " then return
            readkey$ = str(part$) & acode$
            call "READ100" (#12, readkey$, f1%(12%))
            if f1%(12%) = 0% then return
            get #12 using L30189, act_textid$
L30189:     FMT POS(266), CH(4)
        return

         consolidate_travler_array /*Pack for Overlapping WC's*/
            ptr_packed% = 1%
            init (" ") record_packed$()

            for i% = 1% to ptr%

            hit% = 0%
            get record$(i%), using L31060, step$, rteseq%, wc$(1%),       ~
                                        wc$(2%), wc$(3%), wc$(4%),       ~
                                        acode$, date$(1%), date$(2%)
L31060:     FMT CH(7), BI(2), 4*CH(4), POS(34), CH(4), 2*CH(6)
          /* Loop thru packed record looking for Steps existence */
            for j% = 1% to ptr_packed%
                get record_packed$(j%), using L31110, test_step$,         ~
                                                  test_date1$, test_date2$
L31110:         FMT CH(7), POS(38), 2*CH(6)
                if step$ <> test_step$ then L31200   /* Next J */
                    /* Test Dates & reset for greatest use window */
                    if date$(1%) > test_date1$ then date$(1%)= test_date1$
                    if date$(2%) < test_date2$ then date$(2%)= test_date2$
                    hit% = j%
                    j% = 999%    /* Drop out of Loop */
L31200:     next j%

            if hit% > 0% then pos% = hit% else pos% = ptr_packed%

            put record_packed$(pos%), using L31260, step$, rteseq%,       ~
                                      wc$(1%), wc$(2%), wc$(3%), wc$(4%),~
                                      acode$, date$(1%), date$(2%)
L31260:     FMT CH(7), BI(2), 4*CH(4), POS(34), CH(4), 2*CH(6)

            if hit% = 0% then  ptr_packed% = ptr_packed% + 1%

            next i%

            ptr_packed% = ptr_packed% - 1%  /* Set to true end */

            return

        get_plan_rte_seq
          /* Loop thru Plan Route for Step # match to determine pln Seq */
            success%, pln_rteseq% = 0%
            if rtes% < 1% then return
            for k% = 1% to rtes%
                get rte$(k%) using L31410, temp_step$
L31410:            FMT POS(88), CH(5)
                temp% = val(str(temp_step$,5%,1%),1%)
                str(temp_step$,5%) = " "
                if temp% = 0 then L31460
                   temp_step$ = temp_step$ & "-"
                   convert temp%  to                                     ~
                           str(temp_step$,pos(temp_step$="-")+1), pic(00)
L31460:         if step$ <> temp_step$ then L31500   /* Next k% */
                    pln_rteseq% = k%
                    k% = rtes% + 1%     /* Force exit from K loop */
                    success% = 1%
L31500:     next k%

            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *                                                           *~
            * Image statements                                          *~
            *************************************************************

L60060: %                #############################################

L60080: %RUN BY:###           * * * *   T R A V E L E R   * * * *        ~
        ~        PAGE ###

        %JOB ORDER NUMBER: ########               #################
        %          ################################

        %################################

L60140: %PART: ##########################################################~
        ~#  Routing: ###

L60170: %################################################################~
        ~################

L60200: %QUANTITY TO BUILD: ###########           Drawing Ref#:##########~
        ~###### BOM: ###

L60230: %WORK CENTERS PLANNED TO BE EMPLOYED ----------------------------~
        ~----------------

L60260: %STEP    W/C  WORK CENTER/ACTIVITY DESCRIPTIION   FROM  -   TO
L60290: %======= ==== ================================= ======== ========
L60320: %####### #### ##############################    #################
L60340: %             ##############################

L60370: %####### #### ##############################    ________-________

L60430: %WORK CENTER REPORT FORM ----------------------------------------~
        ~----------------

L60460: %WORK CENTER/DESCRIPTION  DATE IN   DATE OUT HOURS SCHED  ACTL  M~
        ~TL   SCHED  ACTL

L60490: %=======================  ========  ======== ===== =====  ====  =~
        ~==   =====  ====

L60520: %#### ###########         ########  ######## M/Q   ###########  I~
        ~N    ###########

        %      ####  ########   ######## M/Q   ##########   IN    #######~
        ~#########
        % ########  ######## M/Q   ##########   IN    ################
L60580: %     STEP: #######       __/__/__  __/__/__ SETUP ###########  O~
        ~UT   ###########
        %          #################################

        %          __/__/__  __/__/__ SETUP ###########  OUT   ##########~
        ~###
        %W/C: ####################
L60650: %######################################
        %       STEP: ###################
L60670: %       ######################################
L60680: %                                            RUN   ###########  S~
        ~CRAP ___________

L60710: %CONCURRENTS: #### #### ####
        %CONCURRENTS: ############# ############# #############
L60730: %                                            M/Q   ###########  R~
        ~WK   ___________
L60745: %                                            OTHER ___________

L60760: %This Step Is Line ### In a ### Line Routing.  Next WC is ####
L60770: %This Step Is The Last Step In a ### Line Routing.
L60780: %                         * * * END OF TRAVELER * * * ###


        %###
        %####
        %######

        %PART : #########################             ########

        %       ##################################
        %ROUTING: #####   BOM: ###                #######################~
        ~###########

L60920: %                                            OPERATOR:___________~
        ~__________
L60940: %                                            TOTAL _____________


        %DRAWING REF#: #########                    #####################~
        ~###
        %QUANTITY TO BUILD: ##########              #####################~
        ~####

        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC
        exit_program
            if page% = 0% then L65180
            print
            print using L60780
            call "BARCODES" ("E"," "," "," "," "," "," "," ",0%,0%)
L65180:
            end
