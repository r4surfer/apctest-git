        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  DDDD   EEEEE  M    M  SSS   TTTTT   AAA   TTTTT          *~
            *  D   D  E      MM  MM S        T    A   A    T            *~
            *  D   D  EEE    M MM M  SSS     T    AAAAA    T            *~
            *  D   D  E      M    M     S    T    A   A    T            *~
            *  DDDD   EEEEE  M    M  SSS     T    A   A    T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DEMSTAT  - Subroutine to display the jobs, POs and advices*~
            *            created to fill the demand/line passed in      *~
            *            or selected and the status of each procurement.*~
            *            Calls JBSEEACT to see job status and POSTATUS  *~
            *            to see purchase order receipt details.         *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/17/87 ! ORIGINAL                                 ! WPH *~
            * 11/24/87 ! REWORK                                   ! JDH *~
            * 04/13/88 ! Fix call to POSTATUS & JBSEEACT (fieldnr)! JDH *~
            *          ! & also fixed bug for loop on cancelled WO!     *~
            * 06/06/88 ! Misc cleanup for R5.01; Date, Version,.RJ! JDH *~
            * 06/10/91 ! (PRR 11144) Added code to handle the     ! RJB *~
            *          !     Rescheduled Tags same as Cancelled.  !     *~
            *          !     Added call to 'ALLFREE' (new stand.) !     *~
            * 06/29/92 ! Fixed trace of non-SO demands.           ! JDH *~
            * 07/02/92 ! PRR 12513- Now selects SYSFILE2, & gets  ! WPH *~
            *          ! PIPIN in calling arg. so now can get qty !     *~
            *          ! & date info from PIPIN, not PIPCROSS.    !     *~
            * 05/24/94 ! PRR 13044- Now uses Planned Dates from   ! RJH *~
            *          !  VBKLINES(PO's) or JBMASTR2(JB's) if     !     *~
            *          !  PIPIN does not exist.                   !     *~
            * 07/15/96 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "DEMSTAT" (demstring$, #1,     /* PIPCROSS FILE  */          ~
                                 #2,     /* DEMMASTR FILE  */            ~
                                 #3,     /* JBMASTR2 FILE  */            ~
                                 #4,     /* JBSTATUS FILE  */            ~
                                 #5,     /* PIPOUT   FILE  */            ~
                                 #6,     /* JBCROSS2 FILE  */            ~
                                 #7,     /* VBKMASTR FILE  */            ~
                                 #8,     /* VBKLINES FILE  */            ~
                                 #9,     /* JBCREDIT FILE  */            ~
                                 #10,    /* RCVLINES FILE  */            ~
                                 #11,    /* PAYMASTR FILE  */            ~
                                 #12,    /* PAYLINES FILE  */            ~
                                 #13,    /* HNYMASTR FILE  */            ~
                                 #14,    /* WCMASTR  FILE  */            ~
                                 #15,    /* RTEMASTR FILE  */            ~
                                 #16,    /* TXTFILE  FILE  */            ~
                                 #17,    /* PIPIN    FILE  */            ~
                                 return%, driver%)

         REM   IF RETURN% = 1 Successful end of check, found pegging     ~
                            2 Demand passed in was not valid             ~
                            3 Status checking cancelled by user          ~
                            4 Could not find PIPINS or pegging linkage   ~
                            5 The demand passed/selected is not planned

         REM   AFTER THE DISPLAY, IF DRIVER% = 0% THEN GOTO TO INPUT     ~
                                             = 1% THEN RETURN TO DRIVER

        dim                                                              ~
            astart$(350)8,               /* ACTUAL START  DATE FOR JOB */~
            aend$(350)8,                 /* ACTUAL  END DATE FOR JOB   */~
            blankdate$8,                 /* Blank date for comparison. */~
            bline1$79,                   /* COLUMN HEADING  UNDERLINE  */~
            bline2$79,                   /* COLUMN HEADING  UNDERLINE  */~
            cancelled$(100)19,           /* PIPOUTS for Canceled PIPINS*/~
            date$8,                      /* TODAY'S DATE               */~
            demand$19,                   /* DEMAND                     */~
            demcust$9,                   /* CUSTOMER IF SO DEMAND      */~
            demcode$16,                  /* DEMAND CODE                */~
            demline$3,                   /* DEMAND LINE                */~
            demquan$10,                  /* DEMAND QUANTITY            */~
            dempart$25,                  /* DEMAND PART NUMBER         */~
            dempcd$8,                    /* DEMAND PLANNED COMPL. DATE */~
            demstring$19,                /* DEMAND PASSED IN           */~
            demtype$1,                   /* DEMAND TYPE                */~
            due_date$6,                  /* PO Due Date                */~
            el$(350)6,                   /* FAC FOR ENDING LATE        */~
            enddif$(350)2,               /* PLANNED END - ACTUAL END   */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED     */~
            infomsg$59,                  /* WHY FAILED OR TYPE OF DEM  */~
            info2msg$79,                 /* LIMIT & NO DETAIL MESSAGE  */~
            inpmessage$79,               /* Input Message              */~
            job$8,                       /* JOB NUMBER FOR PASS        */~
            jobnumb$(350)19,             /* JOB NUMBER                 */~
            kit$(350)3,                  /* JOB KITTED COMPLETE Y OR N */~
            level$(350)2,                /* LEVEL IN BILL OF MATERIALS */~
            line2$79,                    /* SECOND LINE OF SCREEN HDR  */~
            lrdate$6,                    /* PO Last Recieved Date      */~
            mquant$(350)10,              /* QUANTITY TO MAKE  FOR JOB  */~
            cquant$(350)10,              /* QUANTITY COMPLETE FOR JOB  */~
            partno$(350)25,              /* PART TO BUILD FOR JOB      */~
            pipin$(350)19,               /* PIPIN (SOURCE) FROM PIPCRS */~
            pend$(350)8,                 /* PLANNED END DATE FOR JOB   */~
            plbase$6,                    /* Planning calendar base date*/~
            plowkey$75,                  /*                            */~
            postdate$6,                  /* POSTING DATE IN JBCREDIT   */~
            plowvar$75,                                                  ~
            pstart$(350)8,               /* PLANNED START DATE FOR JOB */~
            readkey$75,                  /*                            */~
            resched$(100)19,             /* PIPOUTS for Resched. PIPINS*/~
            rkey$75,                     /* READ KEY FOR PIPCROSS      */~
            sl$(350)6,                   /* FAC FOR STARTING LATE      */~
            startdif$(350)2,             /* PLANNED START - ACTUAL ST  */~
            tend$6,                      /* Temp Planned End Date      */~
            tstart$6,                    /* Temp Planned Start Date    */~
            vendor$(350)9,                                               ~
            lfac$(20)1

        dim cquant(350),                 /* COMPLETED QUANTITY         */~
            mquant(350),                 /* QUANTITY TO MAKE           */~
            level%(350),                 /* LEVEL IN BILL OF MATERIALS */~
            c%(350),                     /* COMPONENTS IN BILL LEVEL   */~
            startdif%(350),              /* STARTING DIFFERENCE IN DAYS*/~
            enddif%(350),                /* ENDING DIFFERENCE IN DAYS  */~
            cursor%(2)

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *  Initialization Section                                   *~
            *************************************************************

          blankdate$ = " "
          call "DATUFMTC" (blankdate$)

          init(hex(84)) lfac$()


        REM *************************************************************~
            * Check the demand/line passed in and load data if valid    *~
            *************************************************************
           date$ = date
           call "DATEFMT" (date$)

              demand$ = demstring$

*        Obtain Planning Base Date

            if plbase$ <> " " and plbase$ <> blankdate$ then L09400

            select #60, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            call "OPENCHCK" (#60, fs%, 0%, 0%, " ")
               if fs% < 0% then L65000

            call "READ100" (#60, "MONTHS OPEN", f1%(60))
                if f1%(60) = 0 then L65000
            get #60 using L09395  , plbase$
L09395:         FMT XX(32), CH(6)
               close #60

L09400:    if demand$ = "  " then inputmode
           call "SHOSTAT" ("Checking Order Status")

              goto get_info

L09450:    readkey$ = demand$
           call "REDALT0" (#2, readkey$, 1%, f1%(2))
                 if f1%(2) <> 0 then L09510
                 return% = 2
                 goto inputmode

L09510:      get #2, using L09530 , demtype$, duedate$, dempart$, demquan$,~
                                  dempcd$, demcust$
L09530:      FMT XX(1), CH(1), XX(1), CH(6), XX(19), CH(25),             ~
                        CH(10), XX(19), CH(6), CH(9)
             /* To Insure Consistant Format Lets Do This */
             convert demquan$ to demquan
             call "CONVERT" (demquan, 2.2, demquan$)

           if dempcd$ <> " " and dempcd$ <> blankdate$ then  L09600
                 return% = 5
                 infomsg$ = "This Demand/Sales Order Is Not Yet Planned"

L09600:    call "DATEFMT" (dempcd$)
           call "DATEFMT" (duedate$)
           goto start_trace



        REM *************************************************************~
            *  Prompt for input or selection of the demand/line.        *~
            *************************************************************

        inputmode

           call "ALLFREE"
           init(" ") errormsg$, inpmessage$, demcode$, demline$


            pf16$="(16)Exit Program"

          goto L10240
            if return% = 0 then L10240

            if return% = 2 then                                          ~
                errormsg$ = "Not A Valid Demand/Sales Order"
            if return% = 3 then                                          ~
                errormsg$ = "Checking Canceled By User"
            if return% = 4 then                                          ~
                errormsg$ = "Could Not Find Pipouts Or Pegging"
            if return% = 5 then                                          ~
                errormsg$ = " Demand/Sales Order Not Yet Planned"

L10240:     for fieldnr% = 1 to  1
                gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10390
L10270:         gosub'101(fieldnr%)      /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <> 16 then       L10380
                         return% = 3%
                         goto exit_program
L10380:               if keyhit% <>  0 then       L10270
L10390:         gosub'150(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10270
            next fieldnr%

            goto get_info



        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20110          /* Demand Code/Line */

                     return
L20110:     REM Default/Enable for Demand Code
             inpmessage$ = "Enter the Demand Code and Line.  Leave blank ~
        ~to see list for selection."
                return



        rem**************************************************************~
           *      initialize input for display                          *~
           **************************************************************
        get_info

           call "SHOSTAT" ("Checking Order Status")

          str(demcode$,1,16) = str(demand$,1,16)
          str(demline$, 1,3) = str(demand$,17,3)

          init(" ") errormsg$, vendor$(),  demtype$, jobnumb$(),         ~
                      partno$(), mquant$(), pipin$(), kit$(), infomsg$,  ~
                      dempart$, demquan$, demcust$, dempcd$ , cquant$(), ~
                      pstart$(), astart$(), pend$(), aend$(), level$(),  ~
                      startdif$(), enddif$(), cancelled$()

          init(hex(8c)) el$(), sl$()
          mat startdif% = zer
          mat enddif% = zer
          mat mquant   = zer
          mat cquant   = zer

          line%, maxline%, mline%, return%, can%  = 0

            goto L09450



        start_trace

        REM *************************************************************~
            *        BEGIN THE TRACE DOWNWARD USING PEGGING LINKAGE     *~
            *************************************************************

           line% = 1         /* LINE%  = DISPLAY SEQUENCE              */
           l% = 0            /* L%     = LEVELS DOWN IN THE BILL       */
           mat c% = zer      /* C%(L%) = COMPONENTS IN GIVEN BILL LEVEL*/
           p% = 0            /* P%     = MAX. LINE%. IN PREV. LEVEL    */
           nl% = 0           /* NL%    = NEXT LEVEL DOWN IN BILL       */
           mat level% = zer  /* LEVEL% = LEVEL IN BILL                 */
           can% = 0%

              if return% = 5% then display_path

           plowkey$ = demand$

           if demtype$   = "1" then L30860
           if demtype$   = "2" then L30860


          /* NO PIPOUT WILL EXIST FOR TYPES > 2 SO FIND TOP LEVEL PIPIN*/
L30710:    call "PLOWNEXT"(#1, plowkey$, 19%, f1%(1))
            if f1%(1) <> 0 then L30744
            infomsg$ = "Order was filled from stock or no pegging exists"
            goto display_path

L30744:    if str(plowkey$, 39, 19) = " " or str(plowkey$, 39, 19) =     ~
              str(plowkey$,  1, 19) then L30750
                goto L30710
L30750:    str(pipin$(line%),1,19) = str(plowkey$, 20, 19)
           gosub load_pipin
           infomsg$ = "The Demand is not a sales order; no PIPOUT exists"
           if partno$(line%)  <> dempart$ then L30710

           goto L30960   /* FROM HERE IT'S JUST LIKE SALES ORDER DEMAND */

L30860:    /* FOR SALES ORDER DEMANDS WE PLOW FOR TOP LEVEL PIPIN        ~
              USING THE DEMAND AS THE PIPOUT */

           call "PLOWALTS" (#1,plowkey$ , 2%, 19% , f1%(1))
                 if f1%(1) =  0 then L31100   /* NO PIPINS FOR TOP LEVEL */
                                             /* LEVEL DEMAND.           */


           /* YOU GET HERE ONLY IF YOU FOUND A TOP LEVEL PIPIN */
             get #1 using L30951, pipin$(line%)  /* Load Array */
L30951:      FMT XX(19),CH(19)

L30960:    l% = l% + 1             /* WE'VE FOUND ANOTHER BILL LEVEL */
           c%(l%) = c%(l%) + 1     /* COUNT IT AS A COMPONENT        */
           level%(line%) = l%


        REM  ************************************************************~
             *     TEST TO SEE IF WE ARE FINISHED TRACING DOWNWARD      *~
             ************************************************************

L31100:    if l% <> 0 then L31160
              return% = 4
              infomsg$ = "The demand was filled from stock or no pegging ~
        ~exists"

              goto load_data             /* Done: Get Data For Display */


L31160:    if c%(l%) <> 0 then L31260     /* NO PIPINS FOR THIS LEVEL */
              return% = 1
              goto load_data             /* Done: Get Data For Display */

        REM   At this point we find that we are not finished tracing so  ~
              we plow with each PIPIN as if it was a PIPOUT, count the   ~
              PIPINs we find for each and end only when we can find no   ~
              more PIPINs pegged to the demand.




L31260:    nl% = l% + 1            /* DETERMINE NEXT LEVEL NUMBER     */

           p% = line% - c%(l%)     /* WE USE P% TO FIND FIRST PIPIN   */
                                   /* OF THE LEVEL AND GO FROM THERE  */

           for i% = 1 to c%(l%)
              plowvar$ = pipin$(p% + i%)
L31330:       call "PLOWALTS" (#1,plowvar$ , 2%, 19% , f1%(1))
                if f1%(1) =  0 then L31470   /* NO MORE PIPINS */
                if str(plowvar$,,12) = "WO CANCELLED" then L31470
                if str(plowvar$,,14) = "WO RESCHEDULED" then L31470
               line% = line% + 1
               if line% > 350 then goto load_data    /* Array Full */
               c%(nl%) = c%(nl%) + 1  /* COUNT COMPONENTS IN NEXT LEVEL */
                 get #1 using L31385, pipin$(line%)   /* Load Array */
L31385:          FMT XX(19), CH(19)
               if str(pipin$(line%),4,9) <> "CANCELLED" then L31405
                can% = can% + 1%      /*  Counter - # of Cancelled WOs  */
                cancelled$(can%) = pipin$(p%+i%) /* PIPOUT for Can PIPIN*/
                goto L31420
L31405:        if str(pipin$(line%),4,11) <> "RESCHEDULED" then L31420
                resh% = resh% + 1%      /* # of Recheduled Jobs */
                resched$(resh%) = pipin$(p%+i%) /* PIPOUT for Rsh PIPIN*/
L31420:        level%(line%) = nl%

             goto L31330               /* Try next PIPIN for this PIPOUT */

L31470:    next i%    /* Move on to the next one and process its PIPINs */

           l% = nl%         /* At this point we are done with the level */

           goto L31160    /* Go see if we are finished or if we have     */
                         /* another level of PIPINs to process          */

        load_data                           /* Load Goodies For Display */
            mline% = line%
            maxline% = line%
            resh%, can% = 0%
            for line% = 1 to mline%
               gosub load_pipin
               gosub load_job
               gosub load_po
               gosub do_convert
            next line%
        goto display_path

        load_pipin

        REM *************************************************************~
            *  Get the data on the PIPIN from the PIPCROSS File         *~
            *************************************************************
            no_pipin% = 0%                /* PIPIN Exists */
            if nl% = 0% then L34105
            if str(pipin$(line%),4,9) <> "CANCELLED" then L34070
                can% = can% + 1%
                str(rkey$,,19) = str(pipin$(line%),,19)
                str(rkey$,20,19) = cancelled$(can%)
                goto L34090
L34070:     if str(pipin$(line%),4,11) <> "RESCHEDULED" then L34105
                resh% = resh% + 1%
                str(rkey$,,19) = str(pipin$(line%),,19)
                str(rkey$,20,19) = resched$(resh%)
L34090:       call "PLOWALTS" (#1, rkey$, 1%, 38%, f1%(1))
            goto L34115

L34105:    rkey$ = pipin$(line%)
           call "PLOWALTS" (#1, rkey$, 1%, 19%, f1%(1))
L34115:      if f1%(1) = 0 then return

           get #1, using L34155,          pipin$(line%),                  ~
                                         partno$(line%)

L34155:    FMT  XX(19), CH(19), XX(19), XX(15), CH(25)

           call "READ100" (#17, pipin$(line%), f1%(17%))
           if f1%(17%) = 1% then L34169
               no_pipin% = 1%
               mquant(line%) = 0
               pstart$(line%), pend$(line%) = " "
               return
L34169:    get #17, using L34171, end%, mquant(line%), start%
L34171:      FMT  POS(26), BI(4), POS(49), PD(14,4), BI(4)

           call "DATE" addr("G+", plbase$, end%-1%, pend$(line%), ret%)
           call "DATE" addr("G+", plbase$, start%-1%, pstart$(line%),ret%)

         return




        load_po

        REM *************************************************************~
            * SEE IF PIPIN IS A RELEASED PO, IF SO LOAD CURRENT STATUS  *~
            *************************************************************

           origquan, outquan = 0
           lrdate$ = " "
           readkey$ = " "

           if str(pipin$(line%),1,2) <> "PO"   then return
           init (hex(20)) readkey$
           str(readkey$,1,14) = str(pipin$(line%), 3, 14)
           call "REDALT0" (#7 , readkey$, 1%,   f1%(7))
               if f1%(7) = 0 then return
             get #7  , using L34430, vendor$(line%), astart$ (line%)
L34430:      FMT CH(9), POS(451), CH(6)

           init (hex(20)) plowkey$
           str(plowkey$,1,23)  = str(vendor$(line%),1,9) &               ~
                       str(pipin$(line%),3,14)
           str(plowkey$,26,3)  = str(pipin$(line%),17,3)
           call "READ100"(#8, plowkey$, f1%(8))
               if f1%(8) = 0 then return
             get #8, using  L34515, origquan, outquan, due_date$, lrdate$
L34515:      FMT XX(92), PD(14,4), XX(8),  PD(14,4), XX(25), CH(6), CH(6)
             if no_pipin% = 0%    then L34600        /* PIPIN Exists */
                 pstart$(line%) = astart$(line%)
                 pend$(line%)   = due_date$
                 mquant(line%)  = origquan

        REM   Since a PO line can provide enough quantity to fill the    ~
              the needs of several buy order advices, we set it such that~
              the entire PO line quantity must be received before the    ~
              actual end date for the purchase is set.  This will        ~
              present 'worst case' information in this status display


L34600:     cquant(line%) = origquan - outquan
            if outquan > 0  then return
            aend$(line%) = lrdate$

            return



        load_job

        REM  ************************************************************~
             * SEE IF PIPIN IS A RELEASED JOB, IF SO LOAD STATUS        *~
             ************************************************************

           if str(pipin$(line%),1,9) <> "JOB ORDER" then return
           readkey$ = str(pipin$(line%), 12, 8)
           call "READ100" (#3 ,readkey$, f1%(3))
             get #3  , using L35340, mquant (line%),  /* quan to make*/   ~
                                    cquant (line%),  /* quan complete*/  ~
                                    astart$(line%),                      ~
                                    aend$(line%),                        ~
                                    tstart$, tend$

L35340:     FMT  XX(82),                                                 ~
                 PD(14,4),                                               ~
                 PD(14,4),                                               ~
                 XX(48),                                                 ~
                 CH(6),                                                  ~
                 CH(6),                                                  ~
                 POS(168), CH(6), CH(6)

           /* Is there still a PIPIN                        */
           if no_pipin% = 0% then L35420           /* PIPIN Exists */
                pstart$(line%) = tstart$
                pend$(line%)   = tend$

           /* FIGURE OUT IF IT HAS BEEN KITTED COMPLETE YET */
L35420:    call "READ100" (#5, pipin$(line%), f1%(5))
              if f1%(5) <> 0  then L35460
           kit$ (line%)  = "YES"
           goto L35540
L35460:    kit$ (line%) = "NO "

           /* IF JOB IS COMPLETED (QUANT. TO MAKE = QUANT. COMPLETE) */
           /* THEN FIND THE LAST DATE ANY WAS REPORTED COMPLETE AND  */
           /* WE'LL CALL THAT THE COMPLETION DATE FOR THE JOB        */
           /* OTHERWISE, WE HAVE TO WAIT TILL THE JOB IS CLOSED TO   */
           /* SEE THE END DATE GET SET IN THE JBMASTR2 FILE          */

L35540:    if cquant(line%) < mquant(line%) then return
           postdate$ = " ": str(readkey$, 9%) = all(hex(00))
           call "DATUNFMT" (postdate$)
L35560:    call "PLOWNEXT"(#9, readkey$, 8%,   f1%(9))
             if f1%(9) = 0 then L35610
           get #9, using L35590, postdate$
L35590:      FMT XX(8), CH(6)
           goto L35560  /* KEEP PLOWING TILL WE GET THE LAST POST DATE*/
L35610:    aend$(line%) = postdate$
           return



        do_convert

        REM  ************************************************************~
             * GET EVERYTHING ALL CONVERTED AND FORMATED FOR DISPLAY    *~
             ************************************************************

           convert level%(line%) to level$(line%), pic(##)
           convert mquant(line%) to mquant$(line%), pic (##########)
           convert cquant(line%) to cquant$(line%), pic (##########)

           if pstart$(line%) <> " " and pstart$(line%) <> blankdate$ then L35760
               startdif%(line%) = 0%
               goto L35782
L35760:    call"DATE" addr("G-", astart$(line%), pstart$(line%),         ~
                         startdif%(line%), r%)

L35782:    if pend$(line%) <> " " and pend$(line%) <> blankdate$ then L35790
               enddif%(line%) = 0%
               goto L35860
L35790:    call"DATE" addr("G-", aend$(line%), pend$(line%),             ~
                         enddif%(line%), r%)


           /* THIS IS WHERE TO SET THE FAC VARIABLE FOR LINE%.  LETS  */
           /* SHOW THE 'DIF' AS BRIGHT IF THE JOB IS BEHIND SCHEDULE  */

L35860:    if startdif%(line%) <  0 then sl$(line%) = hex(84)
           if enddif%(line%) <  0 then el$(line%) = hex(84)
           startdif%(line%) =  abs(startdif%(line%))
           enddif%(line%) =  abs(enddif%(line%))
           convert startdif%(line%) to startdif$(line%), pic (##)
           convert enddif%(line%) to enddif$(line%), pic (##)

           call "DATEFMT" (pstart$(line%))
           call "DATEFMT" (astart$(line%))

           call "DATEFMT" (pend$(line%))
           call "DATEFMT" (aend$(line%))


           pstart$(line%) = str(pstart$(line%),1,5)
           astart$(line%) = str(astart$(line%),1,5)
           pend$(line%) = str(pend$(line%),1,5)
           aend$(line%) = str(aend$(line%),1,5)

           return






        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  str(line2$,63) = "DEMSTAT:" & str(cms2v$,,8)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40190,         /* Demand Code      */~
                                    L40190          /* Demand line      */
                  goto L40250

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40190:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return
L40250:
L40260:     accept                                                       ~
               at (01,02),                                               ~
                  "Select Demand for Status Reporting",                  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Demand Code",                                         ~
               at (06,30), fac(lfac$( 1)), demcode$             , ch(16),~
               at (07,02),                                               ~
                  "Demand line",                                         ~
               at (07,30), fac(lfac$( 1)), demline$             , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(000104050d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L40550
                  call "MANUAL" ("DEMSTAT")
                  goto L40260

L40550:        if keyhit% <> 15 then L40590
                  call "PRNTSCRN"
                  goto L40260

L40590:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return


        display_path


        REM *************************************************************~
            *   DISPLAY COMPARISON OF PLANNED AND ACTUAL DATES FOR JOBS *~
            *************************************************************

          line% = 0
            info2msg$ = " "
L40730:     bline1$ = "LV   (PIPIN Tag Number)    Part Procured          ~
        ~Pln   Act  Dif Pln   Act  Dif"
            bline2$ = "Position Cursor to line, press RETURN to see detai~
        ~l status of Job or P.O.      "


        accept                                                           ~
               at (01,02), "Demand Code (SO #):",                        ~
               at (01,22), fac(hex(84)), demcode$               , ch(16),~
               at (02,26), "Cust:",                                      ~
               at (02,32), fac(hex(84)), demcust$               , ch( 9),~
               at (02,02), "Demand Line (LN #):",                        ~
               at (02,22),  fac(hex(84)), demline$              , ch(03),~
               at (03,02),  fac(hex(94)), infomsg$              , ch(59),~
               at (03,66),  "Due:",                                      ~
               at (03,71), fac(hex(84)), duedate$               , ch(8) ,~
               at (01,42), "Demand Part:",                               ~
               at (01,56),  fac(hex(84)), dempart$              , ch(25),~
               at (02,42), "Demand Quan:",                               ~
               at (02,56),  fac(hex(84)), demquan$              , ch(10),~
               at (02,66), "PCD:",                                       ~
               at (02,71), fac(hex(84)), dempcd$                , ch(8) ,~
               at (05,02), fac(hex(ac)), bline1$                , ch(79),~
               at (04,02), "BOM  Job or P.O. Number                      ~
        ~   --Start Dates--  --End Dates--",                              ~
               at (06,02), fac(hex(84)), level$ (line%+ 1)      , ch( 2),~
               at (07,02), fac(hex(84)), level$ (line%+ 2)      , ch( 2),~
               at (08,02), fac(hex(84)), level$ (line%+ 3)      , ch( 2),~
               at (09,02), fac(hex(84)), level$ (line%+ 4)      , ch( 2),~
               at (10,02), fac(hex(84)), level$ (line%+ 5)      , ch( 2),~
               at (11,02), fac(hex(84)), level$ (line%+ 6)      , ch( 2),~
               at (12,02), fac(hex(84)), level$ (line%+ 7)      , ch( 2),~
               at (13,02), fac(hex(84)), level$ (line%+ 8)      , ch( 2),~
               at (14,02), fac(hex(84)), level$ (line%+ 9)      , ch( 2),~
               at (15,02), fac(hex(84)), level$ (line%+10)      , ch( 2),~
               at (16,02), fac(hex(84)), level$ (line%+11)      , ch( 2),~
               at (17,02), fac(hex(84)), level$ (line%+12)      , ch( 2),~
               at (18,02), fac(hex(84)), level$ (line%+13)      , ch( 2),~
               at (19,02), fac(hex(84)), level$ (line%+14)      , ch( 2),~
                                                                         ~
                                                                         ~
               at (06,05), fac(hex(84)), pipin$  (line%+ 1)     , ch(19),~
               at (07,05), fac(hex(84)), pipin$  (line%+ 2)     , ch(19),~
               at (08,05), fac(hex(84)), pipin$  (line%+ 3)     , ch(19),~
               at (09,05), fac(hex(84)), pipin$  (line%+ 4)     , ch(19),~
               at (10,05), fac(hex(84)), pipin$  (line%+ 5)     , ch(19),~
               at (11,05), fac(hex(84)), pipin$  (line%+ 6)     , ch(19),~
               at (12,05), fac(hex(84)), pipin$  (line%+ 7)     , ch(19),~
               at (13,05), fac(hex(84)), pipin$  (line%+ 8)     , ch(19),~
               at (14,05), fac(hex(84)), pipin$  (line%+ 9)     , ch(19),~
               at (15,05), fac(hex(84)), pipin$  (line%+10)     , ch(19),~
               at (16,05), fac(hex(84)), pipin$  (line%+11)     , ch(19),~
               at (17,05), fac(hex(84)), pipin$  (line%+12)     , ch(19),~
               at (18,05), fac(hex(84)), pipin$  (line%+13)     , ch(19),~
               at (19,05), fac(hex(84)), pipin$  (line%+14)     , ch(19),~
                                                                         ~
               at (06,25), fac(hex(84)), partno$ (line%+ 1)     , ch(25),~
               at (07,25), fac(hex(84)), partno$ (line%+ 2)     , ch(25),~
               at (08,25), fac(hex(84)), partno$ (line%+ 3)     , ch(25),~
               at (09,25), fac(hex(84)), partno$ (line%+ 4)     , ch(25),~
               at (10,25), fac(hex(84)), partno$ (line%+ 5)     , ch(25),~
               at (11,25), fac(hex(84)), partno$ (line%+ 6)     , ch(25),~
               at (12,25), fac(hex(84)), partno$ (line%+ 7)     , ch(25),~
               at (13,25), fac(hex(84)), partno$ (line%+ 8)     , ch(25),~
               at (14,25), fac(hex(84)), partno$ (line%+ 9)     , ch(25),~
               at (15,25), fac(hex(84)), partno$ (line%+10)     , ch(25),~
               at (16,25), fac(hex(84)), partno$ (line%+11)     , ch(25),~
               at (17,25), fac(hex(84)), partno$ (line%+12)     , ch(25),~
               at (18,25), fac(hex(84)), partno$ (line%+13)     , ch(25),~
               at (19,25), fac(hex(84)), partno$ (line%+14)     , ch(25),~
                                                                         ~
               at (06,51), fac(hex(84)), pstart$ (line%+ 1)     , ch( 5),~
               at (07,51), fac(hex(84)), pstart$ (line%+ 2)     , ch( 5),~
               at (08,51), fac(hex(84)), pstart$ (line%+ 3)     , ch( 5),~
               at (09,51), fac(hex(84)), pstart$ (line%+ 4)     , ch( 5),~
               at (10,51), fac(hex(84)), pstart$ (line%+ 5)     , ch( 5),~
               at (11,51), fac(hex(84)), pstart$ (line%+ 6)     , ch( 5),~
               at (12,51), fac(hex(84)), pstart$ (line%+ 7)     , ch( 5),~
               at (13,51), fac(hex(84)), pstart$ (line%+ 8)     , ch( 5),~
               at (14,51), fac(hex(84)), pstart$ (line%+ 9)     , ch( 5),~
               at (15,51), fac(hex(84)), pstart$ (line%+10)     , ch( 5),~
               at (16,51), fac(hex(84)), pstart$ (line%+11)     , ch( 5),~
               at (17,51), fac(hex(84)), pstart$ (line%+12)     , ch( 5),~
               at (18,51), fac(hex(84)), pstart$ (line%+13)     , ch( 5),~
               at (19,51), fac(hex(84)), pstart$ (line%+14)     , ch( 5),~
                                                                         ~
               at (06,57), fac(hex(84)), astart$ (line%+ 1)     , ch( 5),~
               at (07,57), fac(hex(84)), astart$ (line%+ 2)     , ch( 5),~
               at (08,57), fac(hex(84)), astart$ (line%+ 3)     , ch( 5),~
               at (09,57), fac(hex(84)), astart$ (line%+ 4)     , ch( 5),~
               at (10,57), fac(hex(84)), astart$ (line%+ 5)     , ch( 5),~
               at (11,57), fac(hex(84)), astart$ (line%+ 6)     , ch( 5),~
               at (12,57), fac(hex(84)), astart$ (line%+ 7)     , ch( 5),~
               at (13,57), fac(hex(84)), astart$ (line%+ 8)     , ch( 5),~
               at (14,57), fac(hex(84)), astart$ (line%+ 9)     , ch( 5),~
               at (15,57), fac(hex(84)), astart$ (line%+10)     , ch( 5),~
               at (16,57), fac(hex(84)), astart$ (line%+11)     , ch( 5),~
               at (17,57), fac(hex(84)), astart$ (line%+12)     , ch( 5),~
               at (18,57), fac(hex(84)), astart$ (line%+13)     , ch( 5),~
               at (19,57), fac(hex(84)), astart$ (line%+14)     , ch( 5),~
                                                                         ~
               at (06,63), fac(sl$(line%+ 1)), startdif$(line%+ 1),ch(2),~
               at (07,63), fac(sl$(line%+ 2)), startdif$(line%+ 2),ch(2),~
               at (08,63), fac(sl$(line%+ 3)), startdif$(line%+ 3),ch(2),~
               at (09,63), fac(sl$(line%+ 4)), startdif$(line%+ 4),ch(2),~
               at (10,63), fac(sl$(line%+ 5)), startdif$(line%+ 5),ch(2),~
               at (11,63), fac(sl$(line%+ 6)), startdif$(line%+ 6),ch(2),~
               at (12,63), fac(sl$(line%+ 7)), startdif$(line%+ 7),ch(2),~
               at (13,63), fac(sl$(line%+ 8)), startdif$(line%+ 8),ch(2),~
               at (14,63), fac(sl$(line%+ 9)), startdif$(line%+ 9),ch(2),~
               at (15,63), fac(sl$(line%+10)), startdif$(line%+10),ch(2),~
               at (16,63), fac(sl$(line%+11)), startdif$(line%+11),ch(2),~
               at (17,63), fac(sl$(line%+12)), startdif$(line%+12),ch(2),~
               at (18,63), fac(sl$(line%+13)), startdif$(line%+13),ch(2),~
               at (19,63), fac(sl$(line%+14)), startdif$(line%+14),ch(2),~
                                                                         ~
               at (06,66), fac(hex(84)), pend$   (line%+ 1)     , ch( 5),~
               at (07,66), fac(hex(84)), pend$   (line%+ 2)     , ch( 5),~
               at (08,66), fac(hex(84)), pend$   (line%+ 3)     , ch( 5),~
               at (09,66), fac(hex(84)), pend$   (line%+ 4)     , ch( 5),~
               at (10,66), fac(hex(84)), pend$   (line%+ 5)     , ch( 5),~
               at (11,66), fac(hex(84)), pend$   (line%+ 6)     , ch( 5),~
               at (12,66), fac(hex(84)), pend$   (line%+ 7)     , ch( 5),~
               at (13,66), fac(hex(84)), pend$   (line%+ 8)     , ch( 5),~
               at (14,66), fac(hex(84)), pend$   (line%+ 9)     , ch( 5),~
               at (15,66), fac(hex(84)), pend$   (line%+10)     , ch( 5),~
               at (16,66), fac(hex(84)), pend$   (line%+11)     , ch( 5),~
               at (17,66), fac(hex(84)), pend$   (line%+12)     , ch( 5),~
               at (18,66), fac(hex(84)), pend$   (line%+13)     , ch( 5),~
               at (19,66), fac(hex(84)), pend$   (line%+14)     , ch( 5),~
                                                                         ~
               at (06,72), fac(hex(84)), aend$   (line%+ 1)     , ch( 5),~
               at (07,72), fac(hex(84)), aend$   (line%+ 2)     , ch( 5),~
               at (08,72), fac(hex(84)), aend$   (line%+ 3)     , ch( 5),~
               at (09,72), fac(hex(84)), aend$   (line%+ 4)     , ch( 5),~
               at (10,72), fac(hex(84)), aend$   (line%+ 5)     , ch( 5),~
               at (11,72), fac(hex(84)), aend$   (line%+ 6)     , ch( 5),~
               at (12,72), fac(hex(84)), aend$   (line%+ 7)     , ch( 5),~
               at (13,72), fac(hex(84)), aend$   (line%+ 8)     , ch( 5),~
               at (14,72), fac(hex(84)), aend$   (line%+ 9)     , ch( 5),~
               at (15,72), fac(hex(84)), aend$   (line%+10)     , ch( 5),~
               at (16,72), fac(hex(84)), aend$   (line%+11)     , ch( 5),~
               at (17,72), fac(hex(84)), aend$   (line%+12)     , ch( 5),~
               at (18,72), fac(hex(84)), aend$   (line%+13)     , ch( 5),~
               at (19,72), fac(hex(84)), aend$   (line%+14)     , ch( 5),~
                                                                         ~
               at (06,78), fac(el$(line%+ 1)), enddif$(line%+ 1), ch( 2),~
               at (07,78), fac(el$(line%+ 2)), enddif$(line%+ 2), ch( 2),~
               at (08,78), fac(el$(line%+ 3)), enddif$(line%+ 3), ch( 2),~
               at (09,78), fac(el$(line%+ 4)), enddif$(line%+ 4), ch( 2),~
               at (10,78), fac(el$(line%+ 5)), enddif$(line%+ 5), ch( 2),~
               at (11,78), fac(el$(line%+ 6)), enddif$(line%+ 6), ch( 2),~
               at (12,78), fac(el$(line%+ 7)), enddif$(line%+ 7), ch( 2),~
               at (13,78), fac(el$(line%+ 8)), enddif$(line%+ 8), ch( 2),~
               at (14,78), fac(el$(line%+ 9)), enddif$(line%+ 9), ch( 2),~
               at (15,78), fac(el$(line%+10)), enddif$(line%+10), ch( 2),~
               at (16,78), fac(el$(line%+11)), enddif$(line%+11), ch( 2),~
               at (17,78), fac(el$(line%+12)), enddif$(line%+12), ch( 2),~
               at (18,78), fac(el$(line%+13)), enddif$(line%+13), ch( 2),~
               at (19,78), fac(el$(line%+14)), enddif$(line%+14), ch( 2),~
               at (20,02), "Bright Dif Means BEHIND Schedule"           ,~
               at (21,02), fac(hex(94)), info2msg$              , ch(79),~
               at (22,02), fac(hex(ac)), bline2$                , ch(79),~
               at (23,02),                                               ~
           "(1)Start Over               (4)Prev       (8)Show Quantity   ~
        ~    (15)Prnt Scr",                                               ~
               at (24,02),                                               ~
           "(2)First                    (5)Next       (9)Indent by BOM Le~
        ~vel (16)Return  ",                                               ~
               keys (hex(000102040508090f1020)),                         ~
               key  (keyhit%)

           if keyhit% <> 15 then L42480
               call "PRNTSCRN"
               goto L40730

L42480:     if keyhit% <> 9 then L42520
                call "DEMEXPLS" (demand$, #13, #1)
                goto L40730

L42520:    if keyhit%  =  2 then line% = 0
           if keyhit%  =  4 then line% = max(0, line% - 14)
           if keyhit%  =  5 then line% = min(line% + 14,                 ~
                                             max(0, maxline% - 14))

           if line% < 336% then L42555
           info2msg$ = "You Have Reached The Limit of 350 PIPINS!"
           goto L42560
L42555:    info2msg$ = " "

L42560:    if keyhit%  =  8 then  L42820

           if keyhit% <> 1 then L42610
               gosub startover

L42610:    if keyhit% <> 16 then L42645
               if driver% = 1% then exit_program
               goto inputmode

L42645:    close ws
           call "SCREEN"  addr ("C", u3%, "I", i$(), cursor%())

           if keyhit% <> 0 then L42720
           if cursor%(1) < 6% then L42720
              fieldnr% = cursor%(1) - 5
              gosub load_detail

           fieldnr% = 0
L42720:    goto L40730




        REM *************************************************************~
            *      DISPLAY QUAN TO MAKE COMPARED WITH QUAN COMPLETE     *~
            *************************************************************
          line% = 0
            info2msg$ = " "

L42820:     bline1$ = "LV   (PIPIN Tag Number)    Part Procured          ~
        ~to Procure  Comp/Rec   Compl?"
            bline2$ = "Position Cursor to line, press RETURN to see detai~
        ~l status of Job or P.O.      "
L42860: accept                                                           ~
               at (01,02), "Demand Code (SO #):",                        ~
               at (01,22), fac(hex(84)), demcode$               , ch(16),~
               at (02,26), "Cust:",                                      ~
               at (02,32), fac(hex(84)), demcust$               , ch( 9),~
               at (02,02), "Demand Line (LN #):",                        ~
               at (02,22),  fac(hex(84)), demline$              , ch(03),~
               at (03,02),  fac(hex(94)), infomsg$              , ch(59),~
               at (03,66),  "Due:",                                      ~
               at (03,71), fac(hex(84)), duedate$               , ch(8) ,~
               at (01,42), "Demand Part:",                               ~
               at (01,56),  fac(hex(84)), dempart$              , ch(25),~
               at (02,42), "Demand Quan:",                               ~
               at (02,56),  fac(hex(84)), demquan$              , ch(10),~
               at (02,66), "PCD:",                                       ~
               at (02,71), fac(hex(84)), dempcd$                , ch(8) ,~
               at (05,02), fac(hex(ac)), bline1$                , ch(79),~
               at (04,02), "BOM  Job or P.O. Number                      ~
        ~      Quantity   Quantity  Job Kit",                             ~
               at (06,02), fac(hex(84)), level$ (line%+ 1)      , ch( 2),~
               at (07,02), fac(hex(84)), level$ (line%+ 2)      , ch( 2),~
               at (08,02), fac(hex(84)), level$ (line%+ 3)      , ch( 2),~
               at (09,02), fac(hex(84)), level$ (line%+ 4)      , ch( 2),~
               at (10,02), fac(hex(84)), level$ (line%+ 5)      , ch( 2),~
               at (11,02), fac(hex(84)), level$ (line%+ 6)      , ch( 2),~
               at (12,02), fac(hex(84)), level$ (line%+ 7)      , ch( 2),~
               at (13,02), fac(hex(84)), level$ (line%+ 8)      , ch( 2),~
               at (14,02), fac(hex(84)), level$ (line%+ 9)      , ch( 2),~
               at (15,02), fac(hex(84)), level$ (line%+10)      , ch( 2),~
               at (16,02), fac(hex(84)), level$ (line%+11)      , ch( 2),~
               at (17,02), fac(hex(84)), level$ (line%+12)      , ch( 2),~
               at (18,02), fac(hex(84)), level$ (line%+13)      , ch( 2),~
               at (19,02), fac(hex(84)), level$ (line%+14)      , ch( 2),~
                                                                         ~
                                                                         ~
               at (06,05), fac(hex(84)), pipin$  (line%+ 1)     , ch(19),~
               at (07,05), fac(hex(84)), pipin$  (line%+ 2)     , ch(19),~
               at (08,05), fac(hex(84)), pipin$  (line%+ 3)     , ch(19),~
               at (09,05), fac(hex(84)), pipin$  (line%+ 4)     , ch(19),~
               at (10,05), fac(hex(84)), pipin$  (line%+ 5)     , ch(19),~
               at (11,05), fac(hex(84)), pipin$  (line%+ 6)     , ch(19),~
               at (12,05), fac(hex(84)), pipin$  (line%+ 7)     , ch(19),~
               at (13,05), fac(hex(84)), pipin$  (line%+ 8)     , ch(19),~
               at (14,05), fac(hex(84)), pipin$  (line%+ 9)     , ch(19),~
               at (15,05), fac(hex(84)), pipin$  (line%+10)     , ch(19),~
               at (16,05), fac(hex(84)), pipin$  (line%+11)     , ch(19),~
               at (17,05), fac(hex(84)), pipin$  (line%+12)     , ch(19),~
               at (18,05), fac(hex(84)), pipin$  (line%+13)     , ch(19),~
               at (19,05), fac(hex(84)), pipin$  (line%+14)     , ch(19),~
                                                                         ~
               at (06,25), fac(hex(84)), partno$ (line%+ 1)     , ch(25),~
               at (07,25), fac(hex(84)), partno$ (line%+ 2)     , ch(25),~
               at (08,25), fac(hex(84)), partno$ (line%+ 3)     , ch(25),~
               at (09,25), fac(hex(84)), partno$ (line%+ 4)     , ch(25),~
               at (10,25), fac(hex(84)), partno$ (line%+ 5)     , ch(25),~
               at (11,25), fac(hex(84)), partno$ (line%+ 6)     , ch(25),~
               at (12,25), fac(hex(84)), partno$ (line%+ 7)     , ch(25),~
               at (13,25), fac(hex(84)), partno$ (line%+ 8)     , ch(25),~
               at (14,25), fac(hex(84)), partno$ (line%+ 9)     , ch(25),~
               at (15,25), fac(hex(84)), partno$ (line%+10)     , ch(25),~
               at (16,25), fac(hex(84)), partno$ (line%+11)     , ch(25),~
               at (17,25), fac(hex(84)), partno$ (line%+12)     , ch(25),~
               at (18,25), fac(hex(84)), partno$ (line%+13)     , ch(25),~
               at (19,25), fac(hex(84)), partno$ (line%+14)     , ch(25),~
                                                                         ~
               at (06,51), fac(hex(84)), mquant$ (line%+ 1)     , ch(10),~
               at (07,51), fac(hex(84)), mquant$ (line%+ 2)     , ch(10),~
               at (08,51), fac(hex(84)), mquant$ (line%+ 3)     , ch(10),~
               at (09,51), fac(hex(84)), mquant$ (line%+ 4)     , ch(10),~
               at (10,51), fac(hex(84)), mquant$ (line%+ 5)     , ch(10),~
               at (11,51), fac(hex(84)), mquant$ (line%+ 6)     , ch(10),~
               at (12,51), fac(hex(84)), mquant$ (line%+ 7)     , ch(10),~
               at (13,51), fac(hex(84)), mquant$ (line%+ 8)     , ch(10),~
               at (14,51), fac(hex(84)), mquant$ (line%+ 9)     , ch(10),~
               at (15,51), fac(hex(84)), mquant$ (line%+10)     , ch(10),~
               at (16,51), fac(hex(84)), mquant$ (line%+11)     , ch(10),~
               at (17,51), fac(hex(84)), mquant$ (line%+12)     , ch(10),~
               at (18,51), fac(hex(84)), mquant$ (line%+13)     , ch(10),~
               at (19,51), fac(hex(84)), mquant$ (line%+14)     , ch(10),~
                                                                         ~
               at (06,62), fac(hex(84)), cquant$ (line%+ 1)     , ch(10),~
               at (07,62), fac(hex(84)), cquant$ (line%+ 2)     , ch(10),~
               at (08,62), fac(hex(84)), cquant$ (line%+ 3)     , ch(10),~
               at (09,62), fac(hex(84)), cquant$ (line%+ 4)     , ch(10),~
               at (10,62), fac(hex(84)), cquant$ (line%+ 5)     , ch(10),~
               at (11,62), fac(hex(84)), cquant$ (line%+ 6)     , ch(10),~
               at (12,62), fac(hex(84)), cquant$ (line%+ 7)     , ch(10),~
               at (13,62), fac(hex(84)), cquant$ (line%+ 8)     , ch(10),~
               at (14,62), fac(hex(84)), cquant$ (line%+ 9)     , ch(10),~
               at (15,62), fac(hex(84)), cquant$ (line%+10)     , ch(10),~
               at (16,62), fac(hex(84)), cquant$ (line%+11)     , ch(10),~
               at (17,62), fac(hex(84)), cquant$ (line%+12)     , ch(10),~
               at (18,62), fac(hex(84)), cquant$ (line%+13)     , ch(10),~
               at (19,62), fac(hex(84)), cquant$ (line%+14)     , ch(10),~
                                                                         ~
               at (06,76), fac(hex(84)), kit$   (line%+ 1)      , ch( 3),~
               at (07,76), fac(hex(84)), kit$   (line%+ 2)      , ch( 3),~
               at (08,76), fac(hex(84)), kit$   (line%+ 3)      , ch( 3),~
               at (09,76), fac(hex(84)), kit$   (line%+ 4)      , ch( 3),~
               at (10,76), fac(hex(84)), kit$   (line%+ 5)      , ch( 3),~
               at (11,76), fac(hex(84)), kit$   (line%+ 6)      , ch( 3),~
               at (12,76), fac(hex(84)), kit$   (line%+ 7)      , ch( 3),~
               at (13,76), fac(hex(84)), kit$   (line%+ 8)      , ch( 3),~
               at (14,76), fac(hex(84)), kit$   (line%+ 9)      , ch( 3),~
               at (15,76), fac(hex(84)), kit$   (line%+10)      , ch( 3),~
               at (16,76), fac(hex(84)), kit$   (line%+11)      , ch( 3),~
               at (17,76), fac(hex(84)), kit$   (line%+12)      , ch( 3),~
               at (18,76), fac(hex(84)), kit$   (line%+13)      , ch( 3),~
               at (19,76), fac(hex(84)), kit$   (line%+14)      , ch( 3),~
               at (20,02), "Bright Dif Means BEHIND Schedule"           ,~
               at (21,02), fac(hex(94)), info2msg$              , ch(79),~
               at (22,02), fac(hex(ac)), bline2$                , ch(79),~
               at (23,02),                                               ~
           "(1)Start Over               (4)Prev       (8)Show Dates      ~
        ~    (15)Prnt Scr",                                               ~
               at (24,02),                                               ~
           "(2)First                    (5)Next                          ~
        ~    (16)Return  ",                                               ~
               keys (hex(0001020405080f1020)),                           ~
               key  (keyhit%)

           if keyhit% <> 15 then L44100
               call "PRNTSCRN"
               goto L42860

L44100:    if keyhit%  =  2 then line% = 0
           if keyhit%  =  4 then line% = max(0, line% - 14)
           if keyhit%  =  5 then line% = min(line% + 14,                 ~
                                                max(0, maxline% - 14))

           if line% < 336% then L44135
           info2msg$ = "YOU HAVE REACHED THE LIMIT OF 350 PIPINS!"
           goto L44140
L44135:    info2msg$ = " "

L44140:    if keyhit%  =  8 then  L40730

           if keyhit% <> 1 then L44190
               gosub startover

L44190:    if keyhit% <> 16 then L44225
               if driver% = 1% then exit_program
               goto inputmode

L44225:    close ws
           call "SCREEN"  addr ("C", u3%, "I", i$(), cursor%())

           if keyhit% <> 0 then L44310
           if cursor%(1) < 6% then L44310
              fieldnr% = cursor%(1) - 5
              gosub load_detail

           fieldnr% = 0

L44310:    goto L42860



        load_detail

          info2msg$ = " "

          if str(pipin$(line% + fieldnr%),1,2) <> "PO" then  L44430
            goto display_po_status

L44430:   if str(pipin$(line% + fieldnr%),1,9) <> "JOB ORDER" then  L44460
            goto display_jb_status

L44460:   info2msg$ = "No detail yet for the tag number you selected"
          return

        REM *************************************************************~
            *           D I S P L A Y    P O   H I S T O R Y            *~
            *                                                           *~
            * Call  POSTATUS to display PO history of receipts          *~
            *************************************************************
        display_po_status


          str(po$,1,16) = str(pipin$(line% + fieldnr%),3,16)
          str(pol$,1,3) = str(pipin$(line% + fieldnr%),17,3)
          mode% = 1%
           call "POSTATUS" (vendor$(line% + fieldnr%),/* Vendor Number */~
                            po$,                   /* Purchase Order # */~
                            pol$,                  /* Blnk for any line*/~
                            mode%,                 /* 2% = ven units   */~
                                                   /*   Else our units */~
                            #8,                    /* VBKLINES File UFB*/~
                            #10,                   /* RCVLINES File UFB*/~
                            #7,                    /* VBKMASTR File UFB*/~
                            #12,                   /* PAYLINES File UFB*/~
                            #11,                   /* PAYMASTR File UFB*/~
                            #16)                   /* TXTFILE  File UFB*/

         return


         display_jb_status

           str(job$, 1, 8) = str(pipin$(line% + fieldnr%), 12, 8)

           call "JBSEEACT" ( job$, #4, #14, #13, #3, #15, #6)
           return



        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'150(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50110          /* Demand Code/line */

                  return
L50110:     REM Test Data for Demand Code

            if demline$ = " " then goto L50140
                call "STRING" addr("RJ", demline$, 3%)
L50140:     demand$ = str(demcode$) & str(demline$) & hex(00)
          descr$ = hex(06) & "DEMAND CODE       LINE         PART        ~
        ~ QUAN"
          call "PLOWCODE" (#2, demand$, descr$ ,  0%, 1.48 , f1%(2))
           if f1%(2) = 1 then goto L50240
            errormsg$ = "Demand not on file: " & demand$
                return
L50240:    return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

L65000: exit_program
        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            end


