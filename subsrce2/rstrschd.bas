        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  RRRR    SSS   TTTTT  RRRR    SSS    CCC   H   H  DDDD    *~
            *  R   R  S        T    R   R  S      C   C  H   H  D   D   *~
            *  RRRR    SSS     T    RRRR    SSS   C      HHHHH  D   D   *~
            *  R   R      S    T    R   R      S  C   C  H   H  D   D   *~
            *  R   R   SSS     T    R   R   SSS    CCC   H   H  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RSTRSCHD - RESTORE DETAILS 'SAVED' BY CLRRSCHD            *~
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
            * 09/26/85 ! ORIGINAL                                 ! KEN *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "RSTRSCHD" (                                                 ~
                     ptagnr$,            /* TAG NO. OF PIPIN TO CLEAR  */~
                     today%,             /* FOR PIPFLAGS               */~
                     #8,                 /* JBCROSS2                   */~
                     #2,                 /* PIPMASTR                   */~
                     #11,                /* WCMASTR                    */~
                     #23,                /* WCOUT                      */~
                     #33,                /* PIPIN                      */~
                     #34,                /* PIPOUT                     */~
                     #41,                /* SFCUM2                     */~
                     #35,                /* PIPCROSS                   */~
                     #36,                /* JBPIPXRF                   */~
                     #61)                /* WORKFILE TO STORE CLEARED  */~
                                         /* RECORDS FOR RESTORE        */

        dim part$25, ptagnr$19, record$200, readkey$200, type$1, wc%(490)

        REM SET UP AND PLOW IT
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.17.01 11/20/86 Order process & planning #2     "
        REM *************************************************************
            break% = 19%
            if ptagnr$ = " " then break% = 0%
            init (hex(00)) readkey$
            str(readkey$,1,19) = str(ptagnr$,1,19)

L01080:     call "PLOWNEXT" (#61, readkey$, break%, f1%)
                if f1% = 0% then L08000
            get #61, using L01110, type$
L01110:         FMT XX(19), CH(1)
            on pos("OWIXJSU" = type$) gosub  L02000,  /* PIPOUT    */      ~
                                             L03000,  /* WCOUT     */      ~
                                             L04000,  /* PIPIN     */      ~
                                             L05000,  /* JBPIPXRF  */      ~
                                             L06000,  /* JBCROSS2  */      ~
                                             L07000,  /* PIPCROSS 'S' */   ~
                                             L07500   /* PIPCROSS 'U' */
            goto L01080

L02000: REM DEAL WITH THE PIPOUTS

           get #61, using L02030, record$
L02030:        FMT XX(34), CH(64)
           get str(record$,1,64) using L02050, part$, day1%, pipqty
L02050:        FMT XX(19), CH(25), BI(4), XX(8), PD(14,4)
           write #34, using L02070, record$, eod goto L02190
L02070:          FMT CH(64)
           if abs(pipqty) < .0001 then return

           call "PIPFLAGS"                                               ~
                    (part$,              /* THE PART TO CHECK          */~
                     today%,                                             ~
                     day1%,              /* DAY QUANTITY ADDED         */~
                     -pipqty,            /* QUANTITY TO ADD            */~
                     #2,                 /* PIPMASTR                   */~
                     #41)                /* SFCUM2                     */


L02190:    return

L03000: REM NOW THE WC USEAGE & MASTER FILE RECORD

           get #61, using L03030, record$
L03030:        FMT XX(34), CH(68)
           get str(record$,1,68) using L03050, part$, day1%, su%, rn%
L03050:        FMT CH(4), BI(2), XX(25), 2*BI(4)
           pipqty = su% + rn%
           write #23, using L03070, record$, eod goto L03190
L03070:          FMT CH(68)
           if abs(pipqty) < .0001 then return

           call "READ101" (#11, str(part$,1,4) & " ", f1%)
                  if f1% = 0% then L03190
           get #11, using L03130, wc%()
L03130:    FMT XX(1039), 490*BI(2)

           wc%(day1%) = wc%(day1%) + pipqty
           put #11, using L03170, wc%()
L03170:        FMT POS(1040), 490*BI(2)
           rewrite #11
L03190:    return

L04000: REM NOW FOR THE PIPINS

           get #61, using L04030, record$
L04030:        FMT XX(34), CH(60)
           get str(record$,1,60) using L04050, part$, day1%, pipqty
L04050:        FMT CH(25), BI(4), XX(19), PD(14,4)
           write #33, using L04070, record$, eod goto L04180
L04070:          FMT CH(60)
           if abs(pipqty) < .0001 then return

           call "PIPFLAGS"                                               ~
                    (part$,              /* THE PART TO CHECK          */~
                     today%,                                             ~
                     day1%,              /* DAY QUANTITY ADDED         */~
                     pipqty,             /* QUANTITY TO ADD            */~
                     #2,                 /* PIPMASTR                   */~
                     #41)                /* SFCUM2                     */

L04180:    return


L05000: REM JBPIPXRF

           get #61, using L05030, record$
L05030:        FMT XX(34), CH(63)
           write #36, using L05050, record$, eod goto L05060
L05050:          FMT CH(63)
L05060:    return

L06000: REM JBCROSS2

           get #61, using L06030, record$
L06030:        FMT XX(34), CH(94)
           write #8, using L06050, record$, eod goto L06060
L06050:          FMT CH(94)
L06060:    return

L07000: REM PIPCROSS 'S'

           get #61, using L07030, record$
L07030:        FMT XX(34), CH(150)
           write #35, using L07050, record$, eod goto L07060
L07050:          FMT CH(150)
L07060:    str(record$,22,17) = " RESCHEDULED     "
           call "READ101" (#35, record$, f1%)
              if f1% <> 0% then delete #35
           return

L07500: REM PIPCROSS 'U'
           get #61, using L07520, record$
L07520:        FMT XX(34), CH(150)
           write #35, using L07540, record$, eod goto L07550
L07540:          FMT CH(150)
L07550:    str(record$,41,17) = " RESCHEDULED     "
           call "READ101" (#35, record$, f1%)
              if f1% <> 0% then delete #35
           return

L08000: REM OK, ALL DONE
            init (hex(00)) readkey$
            str(readkey$,1,19) = str(ptagnr$,1,19)
            call "DELETE" (#61, readkey$, break%)

        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
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
