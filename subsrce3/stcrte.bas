        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS   TTTTT   CCC   RRRR   TTTTT  EEEEE                 *~
            *  S        T    C   C  R   R    T    E                     *~
            *   SSS     T    C      RRRR     T    EEEE                  *~
            *      S    T    C   C  R   R    T    E                     *~
            *   SSS     T     CCC   R   R    T    EEEEE                 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCRTE   - CLONED FROM PLANRTE.                           *~
            *          - PLANRTE  - LOADS A ROUTE FOR A COMPONENT.      *~
            *----------------------------------------------------------Q*~
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
            * 10/15/86 ! ORIGINAL                                 ! KAB *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "STCRTE"                     /*                            */~
                     (assypart$,         /* PART NEEDED                */~
                      assyrte$,          /* THE WC ROUTE TO USE        */~
                      assybom$,          /* WHICH BOM TO USE           */~
                      rtestep$(),        /* THE DERIVED ROUTE          */~
                      pbs$(),            /* PICK-BY-STEPS              */~
                      x%,                /* # OF ELEMENTS              */~
                      #15,               /* BOMMASTR                   */~
                      #7,                /* RTEMASTR                   */~
                      #10)               /* STCHNY                     */~

        dim oldcompplowkey$(100)31,      /* FOR PHANTOM LOGIC          */~
            loc%(1),                     /*                            */~
            phfact(101),                                                 ~
            palevel%(100),                                               ~
            assypart$25,                                                 ~
            assyrte$3,                   /* WHICH ROUTE TO USE         */~
            assybom$3,                   /* WHICH BOM TO USE           */~
            comppart$25,                                                 ~
            comprte$3,                   /* WHICH ROUTE TO USE         */~
            compbom$3,                   /* WHICH BOM TO USE           */~
            rtestep$(255)160,            /* THE STEP AS A STRING       */~
            pbs$(255)5,                  /* BOM PICK BY STEP           */~
            pbs$5,                       /* BOM PICK BY STEP           */~
            phflag$1,                    /* PHANTOM ASSY. PHLAG        */~
            plowkey$31,                  /* PLOW ON RTEMASTR           */~
            compplowkey$31,              /* PLOW BOMMASTR              */~
            mkr$2                        /* BOM MARKER FOR PHANTOMS    */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.00 09/08/87 Standard cost to 12             "

        REM *************************************************************

            lwcarray% = dim(rtestep$(),1)
            ti% = 1% : phfact(ti%) = 1
            palevel%(ti%), palevel%, x% = 0%: xx% = lwcarray%
            plowkey$ = str(assypart$,1,25) & str(assyrte$,1,3) & " "
            compplowkey$ = str(assypart$,1,25) &                         ~
                                                str(assybom$,1,3) & "  0"

            gosub L08000
              if x% < 0% then L65000

           call "READ100" (#15, compplowkey$, f1%)
              if f1% = 0% then L65000
           get #15, using L01930, phflag$
L01930:        FMT POS(106), CH(1)
           if phflag$ <> "Y" then L65000
           gosub L09000
           goto L12000

L08000: rem**************************************************************~
           *                      m f g d a t e                         *~
           **************************************************************

L08040:    call "PLOWNEXT" (#7, plowkey$, 28%, f1%)
                if f1% = 0% then return
           if x% < xx% then goto L08110
              x% = -99%
              return

L08110:    x% = x% + 1%
               get #7 using L08150, str(rtestep$(x%),1,91),                ~
                      str(rtestep$(x%),93,59)

L08150:               FMT CH(91), CH(59)

            put str(rtestep$(x%)) using L08200,                            ~
                                             palevel%(ti%), phfact(ti%)
L08200:         FMT POS(92), BI(1), POS(152), PD(14,4)
            pbs$(x%) = str(rtestep$(x%),88,5)

            goto L08040

L09000: REM *************************************************************~
            * NOW TEST FOR PHANTOM ASSEMBLIES                           *~
            *************************************************************

           call "PLOWNEXT" (#15, compplowkey$, 28%, f1%)
                if f1% =  0% then return
           compbom$, comprte$ = " "
           get #15, using L09170, comppart$, qu, tu, mkr$, pbs$

L09170:       FMT CH(25), XX(31), 2*PD(14,4), XX(16), CH(2), XX(8), CH(4)

           if mkr$ <> "PA" then L09000

           put str(pbs$,5) using L09225, palevel%(ti%)
L09225:        FMT BI(1)

        REM * * * PHANTOM LOOP LOGIC * * *

             if palevel% < 99% then L09310
              x% = -98%
              goto L09712
L09310:      oldcompplowkey$(ti%) = compplowkey$
             ti% = ti% + 1%
             phfact(ti%)= phfact(ti%-1%)*qu*tu
                     call "READ100" (#10, str(comppart$,1,25), f1%)
                         if f1% = 0% then L09770
                          get #10, using L09420, compbom$, comprte$
L09420:                       FMT POS(38), 2 * CH(3)
                         if compbom$ = " " then L09770

             compplowkey$ = str(comppart$,1,25) &                        ~
                                            str(compbom$,1,3) & "  0"

             call "READ100" (#15, compplowkey$, f1%)
                if f1% =  1% then goto L09730
                     x% = -98%
L09712:              return clear all
                     goto L65000
L09730:       get #15, using L09740, phflag$
L09740:           FMT POS(106), CH(1)

             plowkey$ = str(comppart$,1,25) & str(comprte$,1,3) & " "
             gosub L10000
             palevel%(ti%), palevel% = palevel% + 1%
             gosub L08000
               if x% < 0% then L09712
             gosub L11000
             if phflag$ <> "Y" then goto L09770
             gosub L09000

L09770:      ti% = ti% - 1%
             compplowkey$ = oldcompplowkey$(ti%)
             goto L09000

L10000: REM *************************************************************~
            * PUSH ROUTE DOWN                                           *~
            *************************************************************

            loc%(1%) = 1%:range% = -1%
            if x% = 0% then return

            if str(pbs$,,4) = " " then L10180 /* FIRST */
            if x% = 1% then L10180            /* DITTO */

            search str(pbs$(),,5%*x%) = str(pbs$,1,5) to loc%() step 5
               loc%(1%) = int((5% + loc%(1%))/5%)

L10180:     range% = x% - loc%(1%)
            for i% = 0% to range%
                rtestep$(255% - i%) = rtestep$(x% - i%)
            next i%

            x% = x% - range% - 1%
            xx% = lwcarray% - range% - 1%

            return

L11000: REM *************************************************************~
            * PULL ROUTE UP                                             *~
            *************************************************************

            if range% < 0% then return

            x% = x% + 1%

            for i% = 0% to range%
                rtestep$(x% + range% - i%) = rtestep$(255% - i%)
                pbs$    (x% + range% - i%) =                             ~
                                    str(rtestep$(x% + range% - i%),88,5)
            next i%

            x% = x% + range%

            return

L12000: REM *************************************************************~
            * ADJUST YIELD FACTORS                                      *~
            *************************************************************

            if x% = 1% then L65000

            get str(rtestep$(x%),79, 1) using L12070, yield
L12070:         FMT BI(1)
            put str(rtestep$(x%),80, 8) using L12090, yield
L12090:         FMT PD(14,7)

            for i% = x% - 1% to 1% step -1%
                get str(rtestep$(i%),79, 1) using L12070, yield
                get str(rtestep$(i%+1%),80, 8) using L12090, yield1
                    yield1 = yield1 * yield / 100
                put str(rtestep$(i%),80, 8) using L12090, yield1
            next i%

L65000: REM *************************************************************~
            * EXIT SUBROUTINE                                           *~
            *************************************************************

            end
