        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   CCC   L      RRRR   RRRR   SSSSS   CCC   H   H  DDDD    *~
            *  C   C  L      R   R  R   R  S      C   C  H   H  D   D   *~
            *  C      L      RRRR   RRRR    SSS   C      HHHHH  D   D   *~
            *  C   C  L      R   R  R   R      S  C   C  H   H  D   D   *~
            *   CCC   LLLLL  R   R  R   R  SSSSS   CCC   H   H  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CLRRSCHD - SUBROUTINE USED TO CLEAR AN ENTRY IN THE PIPS  *~
            *            AND WORK CENTER STUFF BACK TO A CERTAIN STEP   *~
            *            OR POINT IN TIME. ONLY WORKS ON JO'S OR WO'S.  *~
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
            * 07/05/83 ! ORIGINAL                                 ! ECR *~
            * 09/25/85 ! CLONED FROM CLRPIPIN                     ! KAB *~
            * 10/19/93 ! Purchase Jobs (Not in Process, BW's)     ! KAB *~
            * 07/09/96 ! Corrected potential DUP error.           ! JDH *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "CLRRSCHD" (                                                 ~
                     ptagnr$,            /* TAG NO. OF PIPIN TO CLEAR  */~
                     today%,             /* FOR PIPFLAGS               */~
                     day%,               /* CLEAR PIPOUTS >  THIS DAY  */~
                     step%,              /* CLEAR WC STEPS > THIS STEP */~
                     clear%,             /* CLEAR WORKFILE? (ALSO RET.)*/~
                     #8,                 /* JBCROSS2                   */~
                     #2,                 /* PIPMASTR                   */~
                     #11,                /* WCMASTR                    */~
                     #23,                /* WCOUT                      */~
                     #33,                /* PIPIN                      */~
                     #34,                /* PIPOUT                     */~
                     #41,                /* SFCUM2                     */~
                     #35,                /* PIPCROSS                   */~
                     #36,                /* JBPIPXRF                   */~
                     #61,                /* WORKFILE TO STORE CLEARED  */~
                     ret%)               /* RECORDS FOR RESTORE        */

        dim part$25, ptagnr$19, record$200, readkey$200, wc%(490),       ~
            workfile$8, worklib$8, workvol$6

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 08/12/96 Last Wang Release               "
        REM *************************************************************
            if clear% <> 0% then L01120

            call "GETNAMES" addr(#61, workfile$, worklib$, workvol$)
               close #61
            call "SCRATCH" addr("F", workfile$, worklib$, workvol$,      ~
                                    "B", " ", f2%)

            f2%=1%: call "WORKOPEN" (#61, "IO   ", 1000%, f2%)

L01120: REM CLEAR IT IF IT IS A JOB OR WORK ORDER

           ret% = 99%
           if str(ptagnr$,1,2) = "WO" then L02000
           if str(ptagnr$,1,2) = "JO" then L02000
           if str(ptagnr$,1,2) = "BW" then L02000
              goto L65000

L02000: REM FIRST DEAL WITH THE PIPOUTS
           ret% = ret% - 1%
           init(hex(00)) readkey$
           str(readkey$,1,19) = str(ptagnr$,1,19)

L02050:    call "PLOWNXT1" (#34, readkey$,  19%, f1%)
              if f1% = 0% then L03000
           get #34, using L02080, record$
L02080:        FMT CH(64)
           get str(record$,1,64) using L02100, part$, day1%, pipqty
L02100:        FMT XX(19), CH(25), BI(4), XX(8), PD(14,4)
           if day1% <= day% then L02050
           delete #34
L02130:    write #61, using L02140, ptagnr$, "O", date, time, record$,     ~
                                                            eod goto L02130
L02140:          FMT CH(19), CH(1), CH(6), CH(8), CH(64)
           if abs(pipqty) < .0001 then L02050

           call "PIPFLAGS"                                               ~
                    (part$,              /* THE PART TO CHECK          */~
                     today%,                                             ~
                     day1%,              /* DAY QUANTITY ADDED         */~
                     pipqty,             /* QUANTITY TO ADD            */~
                     #2,                 /* PIPMASTR                   */~
                     #41)                /* SFCUM2                     */


           goto L02050

L03000: REM NOW ELIMINATE THE WC USEAGE
           ret% = ret% - 1%
           init(hex(00)) readkey$
           str(readkey$,1,19) = str(ptagnr$,1,19)

L03050:    call "PLOWNXT1" (#23, readkey$,  19%, f1%)
              if f1% = 0% then L04000
           get #23, using L03080, record$
L03080:        FMT CH(68)
           get str(record$,,68) using L03100, part$, day1%, step1%,su%, rn%
L03100:        FMT CH(4), 2*BI(2), XX(23), 2*BI(4)
           pipqty = su% + rn%
           step1% = step1%/100%
           if step1% <= step% then L03050
           delete #23
L03130:    write #61, using L03140, ptagnr$, "W", date, time, record$,     ~
                                                            eod goto L03130
L03140:          FMT CH(19), CH(1), CH(6), CH(8), CH(68)
           if abs(pipqty) < .0001 then L03050

           call "READ101" (#11, str(part$,1,4) & " ", f1%)
                  if f1% = 0% then L03050
           get #11, using L03200, wc%()
L03200:    FMT XX(1039), 490*BI(2)

           wc%(day1%) = wc%(day1%) - pipqty
           put #11, using L03240, wc%()
L03240:        FMT POS(1040), 490*BI(2)
           rewrite #11
           goto L03050

L04000: REM NOW FOR THE PIPINS
           ret% = ret% - 1%

           call "READ101" (#33, ptagnr$, f1%)
              if f1% = 0% then L05000
           get #33, using L04060, record$
L04060:        FMT CH(60)
           get str(record$,1,60) using L04080, part$, day1%, pipqty
L04080:        FMT CH(25), BI(4), XX(19), PD(14,4)
           delete #33
L04100:    write #61, using L04110, ptagnr$, "I", date, time, record$,     ~
                                                            eod goto L04100
L04110:          FMT CH(19), CH(1), CH(6), CH(8), CH(60)
           if abs(pipqty) < .0001 then L05000

           call "PIPFLAGS"                                               ~
                    (part$,              /* THE PART TO CHECK          */~
                     today%,                                             ~
                     day1%,              /* DAY QUANTITY ADDED         */~
                     -pipqty,            /* QUANTITY TO ADD            */~
                     #2,                 /* PIPMASTR                   */~
                     #41)                /* SFCUM2                     */

L05000: REM JBPIPXRF
           ret% = ret% - 1%

           call "REDALT1" (#36, ptagnr$, 1%, f1%)
              if f1%=0 then L05500
           get #36, using L05060, record$
L05060:        FMT CH(63)
           delete #36
L05080:    write #61, using L05090, ptagnr$, date, time, "X", record$,     ~
                                                            eod goto L05080
L05090:          FMT CH(19), CH(6), CH(8), CH(1), CH(63)

L05500:    ret% = ret% - 1%

           init(hex(00)) readkey$
           str(readkey$,1,19) = str(ptagnr$,1,19)
L05540:    call "PLOWNXT1" (#36, readkey$,  19%, f1%)
              if f1% = 0% then L06000
           get #36, using L05570, record$
L05570:        FMT CH(63)
           delete #36
L05590:    write #61, using L05600, ptagnr$, date, time, "X", record$,     ~
                                                            eod goto L05590
L05600:          FMT CH(19), CH(6), CH(8), CH(1), CH(63)
           goto L05540

L06000: REM JBCROSS2
           ret% = ret% -1%

           call "READ101" (#8, ptagnr$, f1%)
              if f1% = 0% then L07000
           get #8, using L06060, record$
L06060:        FMT CH(94)
           delete #8
L06080:    write #61, using L06090, ptagnr$, "J", date, time, record$,     ~
                                                            eod goto L06080
L06090:          FMT CH(19), CH(1), CH(6), CH(8), CH(94)

L07000: REM PIPCROSS
           ret% = ret% - 1%
           init (hex(00)) readkey$
           str(readkey$,1,19) = str(ptagnr$,1,19)

L07050:    call "PLOWAL1" (#35, readkey$, 1%, 19%, f1%)
              if f1% = 0 then L07500
           get #35, using L07080, record$
L07080:        FMT CH(150)
           delete #35
L07100:    write #61, using L07110, ptagnr$, "S", date, time, record$,     ~
                                                            eod goto L07100
L07110:          FMT CH(19), CH(1), CH(6), CH(8), CH(150)
           put #35, using L07130 , " RESCHEDULED     "
L07130:        FMT POS(22), CH(17)
           write #35
           goto L07050

L07500:    ret% = ret% - 1%
           init (hex(00)) readkey$
           str(readkey$,1,19) = str(ptagnr$,1,19)

L07540:    call "PLOWAL1" (#35, readkey$, 2%, 19%, f1%)
              if f1% = 0 then L08000
           get #35, using L07570, record$
L07570:        FMT CH(150)
           delete #35
L07590:    write #61, using L07600, ptagnr$, "U", date, time, record$,     ~
                                                            eod goto L07590
L07600:          FMT CH(19), CH(1), CH(6), CH(8), CH(150)
           put #35, using L07620 , " RESCHEDULED     "
L07620:         FMT POS(41), CH(17)
           write #35
           goto L07540

L08000: REM OK, ALL DONE
            ret% = 0%

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
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
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
