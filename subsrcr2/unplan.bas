        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  U   U  N   N  PPPP   L       AAA   N   N                 *~
            *  U   U  NN  N  P   P  L      A   A  NN  N                 *~
            *  U   U  N N N  PPPP   L      AAAAA  N N N                 *~
            *  U   U  N  NN  P      L      A   A  N  NN                 *~
            *   UUU   N   N  P      LLLLL  A   A  N   N                 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * UNPLAN   - ROUTINE TO UNPLAN DEMANDS FOR LEVEL-2 CMS.     *~
            *            SO & SF ARE HANDLED IN 'SECTION 1', PROCUREMENT*~
            *            AND PM'S IN 'SECTION 2'.   THE CALLING         *~
            *            PROGRAM MUST HAVE OPENED THE NEEDED FILES.     *~
            *            AN IMPORTANT FUNCTION IS TO PROPERLY UPDATE    *~
            *            THE FORECAST FILES FOR THE 9 DEMAND TYPES-     *~
            *    1 = SALES ORDER TO BE NETTED AGAINST FORECASTS         *~
            *    2 = SALES ORDER NOT TO BE NETTED AGAINST FORECASTS     *~
            *    3 = REQUISITION - LIKE A TYPE 2 SALES FORECAST         *~
            *    4 = REGULAR SALES FORECAST- ASSUMED TO BE PART OF      *~
            *           NETTABLE SALES ENTERED BEFORE OR AFTER THIS FCST*~
            *           WILL ONLY PLAN FOR MORE IF CUMF IS > 0          *~
            *    5 = TRUE NET NEW SALES FORECASTS- WILL ADD TO CUMF &   *~
            *           WILL PLAN FOR THE QUANTITY SPECIFIED NO MATTER  *~
            *           WHAT CUMF IS NOW.                               *~
            *    6 = USER SET - NOW DOES NOTHING                        *~
            *    7 = USER SET - NOW DOES NOTHING                        *~
            *    8 = PROCUREMENT DEMAND (NO JUMP, NO PIPOUT FOR PART    *~
            *    9 = PREVENTITIVE MAINTENANCE DEMAND                    *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/06/83 ! ORIGINAL                                 ! GLW *~
            * 02/23/84 ! Return part to unplan correctly upon exit! ECR *~
            * 01/27/86 ! FIXED #11 BUG STILL READING "U" (4.15.07)! KAB *~
            * 09/15/86 ! SUPPORT TYPE 7 DEMANDS                   ! KAB *~
            * 08/19/93 ! Purchase Job Support - BW's              ! KAB *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "UNPLAN"  (dc$,              /* DEMAND CODE                */~
                     dl$,                /* DEMAND LINE                */~
                     dt$,                /* DEMAND TYPE                */~
                     ed%,                /* DATE PLANNED FOR SO/SF ONLY*/~
                     dd%,                /* DEMAND DUE DATE            */~
                     dpart$,             /* PART TO UNPLAN             */~
                     quantity,           /* QUANTITY TO UNPLAN         */~
                     today%,                                             ~
                     unplanopt$,         /* UNPLANNING OPTION          */~
                     unplanrpt$,         /* UNPLANNING REPORT          */~
                     #8,                 /* JBCROSS2                   */~
                     #2,                 /* PIPMASTR                   */~
                     #11,                /* WCMASTR                    */~
                     #23,                /* WCOUT                      */~
                     #33,                /* PIPIN                      */~
                     #34,                /* PIPOUT    REFERENCES       */~
                     #40,                /* SFMASTR2  SALES FORECASTS  */~
                     #41,                /* SFCUM2  CUMULATIVE FCSTS   */~
                     #35,                /* PIPCROSS                   */~
                     #36)                /* JBPIPXRF                   */

        dim cumf%(490),                                                  ~
           date$8,                                                       ~
           dpart$25,                                                     ~
           part$25,                                                      ~
           ptagnr$19,                                                    ~
           wcoutkey$35,                                                  ~
           wc$4,                                                         ~
           pipoutkey$56,                                                 ~
           pipplow$100,                  /*                            */~
           dc$16, dl$3                   /* DEMAND CODE & LINE         */

           dim a1$19, a2$19, p$25, i$8


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************
           part$ = dpart$                /* Don't stomp on DPART$      */
           line% = 999%
           page% = 0%
           date$=date
           call "DATEFMT" (date$)
           if dt$ = "1" then goto L02500
           if dt$ = "2" then goto L03000
           if dt$ = "3" then goto L03500
           if dt$ = "4" then goto L01700
           if dt$ = "5" then goto L01700
           if dt$ = "6" then end
           if dt$ = "7" then goto L03500
           if dt$ = "8" then goto L03500
           if dt$ = "9" then gosub L04500
           end

L01700: REM *************************************************************~
           *  SECTION 1 - SALES FORECASTS                               *~
           **************************************************************

           call "READ101" (#40, part$, f1%)
                     if f1% <> 1 then goto L01820
           get #40, using L01770   , part$, cumf%()
L01770:    FMT CH(25), 490*BI(4)

           cumf%(ed%) = cumf%(ed%) - quantity
           rewrite #40, using L01770, part$, cumf%()

L01820:    call "READ101" (#41, part$, f1%)
                     if f1% <> 1 then goto L01920
           get #41, using L01850   , part$, cumf%()
L01850:    FMT CH(25), 490*BI(4)

           for i = ed% to 490%
           cumf%(i) = cumf%(i) - quantity
           next i
           rewrite #41, using L01850, part$, cumf%()

L01920: call "PIPFLAGS"                                                  ~
                    (part$,              /* THE PART TO CHECK          */~
                     today%,                                             ~
                     490%,               /* DAY QUANTITY ADDED         */~
                     0,                  /* QUANTITY TO ADD            */~
                     #2,                 /* PIPMASTR                   */~
                     #41)                /* SFCUM2                     */

           if unplanrpt$="Y" then gosub print_netted_down
           goto L03500

L02500: REM *************************************************************~
           *  SECTION 2 - SALES ORDERS  TYPE 1                          *~
           **************************************************************

           call "READ101" (#41, part$, f1%)
                     if f1% <> 1 then goto L03000
           get #41, using L02570   , part$, cumf%()
L02570:    FMT CH(25), 490*BI(4)

           for i = ed% to 490%
           cumf%(i) = cumf%(i) + quantity
           next i
           rewrite #41, using L02570, part$, cumf%()
           if unplanrpt$="Y" then gosub print_netted_up

L03000: REM *************************************************************~
           * SALES ORDERS TYPE 1 & 2 (CONTINUATION OF TYPE 1) DEMANDS   *~
           **************************************************************
           dqu=0
           init (hex(00)) pipplow$
           str(pipplow$,1,19) = str(dc$,1,16) & str(dl$,1,3)
L03060:    call "PLOWNXT1" (#34, pipplow$, 19%, f1%)
                     if f1% = 0% then goto L03250

           get   #34, using L03110 , part$, day%, qu
           delete #34
L03110:    FMT XX(19), CH(25), BI(4), XX(8), PD(14,4)
           dqu=dqu+qu
        call "PIPFLAGS"                                                  ~
                    (part$,              /* THE PART TO CHECK          */~
                     today%,                                             ~
                     day%,               /* DAY QUANTITY ADDED         */~
                     qu,                 /* QUANTITY TO ADD            */~
                     #2,                 /* PIPMASTR                   */~
                     #41)                /* SFCUM2                     */

            goto L03060

L03250:     if abs(dqu) < .00001 then L03310

            write #34, using L03290, str(pipplow$,1,19),part$,dd%,time,dqu
            call "PIPFLAGS" (part$,today%,dd%,-dqu,#2,#41)
L03290:         FMT CH(19), CH(25), BI(4), CH(8), PD(14,4)

L03310:     call "READ101" (#33, pipplow$, f1%)
                 if f1% = 0 then L03380
            get #33, using L03360, part$, day%, qu
            delete #33
            call "PIPFLAGS" (part$,today%,day%,-qu,#2,#41)
L03360:         FMT CH(25), BI(4), XX(19), PD(14,4)

L03380:     if abs(dqu) < .00001 then L03440
            write #33, using L03420, part$,dd%,str(pipplow$,1,19),dqu,dd%
            call "PIPFLAGS" (part$,today%,dd%,dqu,#2,#41)

L03420:         FMT CH(25),BI(4),CH(19),PD(14,4),BI(4)

L03440:    if unplanrpt$="Y" then gosub print_so_reset

L03500: REM *************************************************************~
           *       NOW FOLLOW HARD PEG ROUTE                            *~
           **************************************************************

            init (hex(00)) pipplow$
            str(pipplow$,1,19) = str(dc$,1,16) & str(dl$,1,3)
              if unplanopt$ = "N" or unplanopt$ = "P" then L04100
L03570:     call "PLOWNEXT" (#35,pipplow$,19%,f1%)
            if f1%=0 then L04100
            get #35, using L03600, ptagnr$, part$, qp, qu
L03600:         FMT XX(19), CH(19), XX(33), XX(1), CH(25), 2*PD(14,4)
            if unplanopt$ = "Y" then L03650
            if unplanopt$ <> "T" then L03640
               if part$ = dpart$ then L03650
L03640:     if qp - qu > .00001 then L03665
L03650:     if str(ptagnr$,1,2)="BO" then L04000
            if str(ptagnr$,1,2)="WO" then L03700
            if str(ptagnr$,1,2)="BW" then L03700
L03665:    if unplanrpt$="Y" then gosub print_not_purged
           goto L03570

L03700: REM PIPOUT LOGIC FOR WO'S

            call "READ101" (#8,ptagnr$,f1%)
            if f1%<>0 then delete #8
            call "DELETE" (#36,ptagnr$,19%)
            call "REDALT1" (#36, ptagnr$,1%, f1%)
            if f1% <> 0 then delete #36

            init (hex(00)) pipoutkey$
            str(pipoutkey$,1,19)=ptagnr$
L03770:     call "PLOWNXT1" (#34,pipoutkey$,19%,f1%)
            if f1%=0 then L03900
            get #34, using L03800, part$,day%,qu
L03800:         FMT XX(19),CH(25),BI(4),XX(8),PD(14,4)
            delete #34
            call "PIPFLAGS" (part$,today%,day%,qu,#2,#41)
            goto L03770

L03900:     init (hex(00)) wcoutkey$
            str(wcoutkey$,1,19)=ptagnr$
            gosub L04590

L04000: REM PIPIN LOGIC IN ANY CASE
            call "READ101" (#33, ptagnr$, f1%)
            if f1%=0 then L04065
            get #33, using L04040, part$,day%,qu
L04040:         FMT CH(25),BI(4),XX(19),PD(14,4)
            delete #33
            call "PIPFLAGS" (part$,today%,day%,-qu,#2,#41)
L04065:    if unplanrpt$="Y" then gosub print_purged
            goto L03570

L04100:     call "DELETE" (#35,pipplow$,19%)
            if unplanrpt$ ="Y" and line% > 900% then gosub print_header
            close printer
            end

L04500: REM *************************************************************~
           *  SECTION 2 - PREVENTITIVE MAINTENANCE                      *~
           **************************************************************

           REM NOW ELIMINATE THE WC USEAGE

           init(hex(00)) wcoutkey$
           str(wcoutkey$,1,19) = str(dc$,1,16) & str(dl$,1,3)
           str(wcoutkey$,1,2) = "WO"
L04590:    call "PLOWNXT1" (#23, wcoutkey$,  19%, f1%)
                if f1% <> 1 then return
           get #23, using L04630, wc$, day1%, su%, run%
           delete #23
           ot% = su% + run%
L04630:    FMT CH(4), BI(2), XX(25), 2*BI(4)
           call "READ101" (#11, str(wc$,1,4) & " ", f1%)
                  if f1% <> 1 then goto L04590
           get #11, using L04670, cumf%()
L04670:       FMT XX(1039), 490*BI(2)

           cumf%(day1%) = cumf%(day1%) - ot%
           put #11, using L04715, cumf%()
L04715:       FMT POS(1040), 490*BI(2)
           rewrite #11
           goto L04590

        print_header
            if line% < 60% then return
            page% = page% + 1%
            select printer (134)
            print page
            print using L05100, date$, dc$, dl$, page%
            print skip (1)
            print using L05110, dt$, round(quantity,2), dpart$, unplanopt$
            print skip (2)
            line% = 0%
            return
L05100: % ########     UNPLANNING DEMAND ################ LINE ###     PA~
        ~GE ####
L05110: % DEMAND TYPE # FOR -########.## OF #########################  OP~
        ~TION  #

        print_netted_down
            gosub print_header
            print using L05190, round(quantity,2)
            print skip (1)
            line% = line% + 2%
            return
L05190: %FORECASTS ADJUSTED DOWN BY -########.##

        print_netted_up
            gosub print_header
            print using L05240, round(quantity,2)
            print skip (1)
            line% = line% + 2%
            return
L05240: %FORECASTS ADJUSTED UP BY -########.##

        print_not_purged
            gosub print_header
            get #35, using L05280, a1$, a2$, p$, p, u, i$
L05280:      FMT XX(19),CH(19),CH(19),XX(15),CH(25),2*PD(14,4),XX(8),CH(6)
            if a2$=" " then a2$="'STOCK'"
            call "DATEFMT" (i$)
            print using L05300, a1$, round(p,2), p$, round(u,2), i$, a2$
L05300: % ################### FOR -########.## ######################### ~
        ~-########.## NEEDED BY ######## FOR ################### NOT PURGED
            line% = line% + 1%
            return

        print_purged
            gosub print_header
            get #35, using L05380, a1$, a2$, p$, p, u, i$
L05380:      FMT XX(19),CH(19),CH(19),XX(15),CH(25),2*PD(14,4),XX(8),CH(6)
            if a2$=" " then a2$="'STOCK'"
            call "DATEFMT" (i$)
            print using L05400, a1$, round(p,2), p$, round(u,2), i$, a2$
L05400: % ################### FOR -########.## ######################### ~
        ~-########.## NEEDED BY ######## FOR ###################  PURGED***
            line% = line% + 1%
            return

        print_so_reset
            gosub print_header
            print using L05500, round(dqu,2)
            print skip(1)
            line% = line% + 1%
            return
L05500: % SALES ORDER RESET TO BACKORDER QUANTITY -########.##

