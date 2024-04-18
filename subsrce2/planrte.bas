        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   L       AAA   N   N  RRRR   TTTTT  EEEEE          *~
            *  P   P  L      A   A  NN  N  R   R    T    E              *~
            *  PPPP   L      AAAAA  N N N  RRRR     T    EEEE           *~
            *  P      L      A   A  N  NN  R   R    T    E              *~
            *  P      LLLLL  A   A  N   N  R   R    T    EEEEE          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLANRTE  - LOADS A ROUTE FOR A COMPONENT.                 *~
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
            * 10/15/86 ! ORIGINAL                                 ! KAB *~
            * 08/19/88 ! ADDED CONCURRENT AND SETUP ACTIVITY CODES! RJM *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "PLANRTE"                    /*                            */~
                     (assypart$,         /* PART NEEDED                */~
                      assyrte$,          /* THE WC ROUTE TO USE        */~
                      assybom$,          /* WHICH BOM TO USE           */~
                      errormsg$,         /* THE RETURN MESSAGE         */~
                      rtestep$(),        /* THE DERIVED ROUTE          */~
                      ed%,               /* EFFECTIVE DATE             */~
                      x%,                /* # OF ELEMENTS              */~
                      #15,               /* BOMMASTR                   */~
                      #7,                /* RTEMASTR                   */~
                      #24)               /* ENGMASTR                   */~

        dim eff$(490)3,                  /* EFF, PLT WORK ARRAY        */~
            oldcompplowkey$(100)31,      /* FOR PHANTOM LOGIC          */~
            loc%(1),                     /*                            */~
            phfact(101),                                                 ~
            palevel%(100),                                               ~
            assypart$25,                                                 ~
            assyrte$3,                   /* WHICH ROUTE TO USE         */~
            assybom$3,                   /* WHICH BOM TO USE           */~
            comppart$25,                                                 ~
            comprte$3,                   /* WHICH ROUTE TO USE         */~
            compbom$3,                   /* WHICH BOM TO USE           */~
            rtestep$(255)200,            /* THE STEP AS A STRING       */~
            pbs$(255)5,                  /* BOM PICK BY STEP           */~
            pbs$5,                       /* BOM PICK BY STEP           */~
            testbom$60,                  /* G.P. PLOWKEY               */~
            phflag$1,                    /* PHANTOM ASSY. PHLAG        */~
            plowkey$31,                  /* PLOW ON RTEMASTR           */~
            errormsg$79,                 /*                            */~
            compplowkey$31,              /* PLOW BOMMASTR              */~
            mkr$2                        /* BOM MARKER FOR PHANTOMS    */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R5.01.03 11/15/88 Patch Release                   "

        REM *************************************************************

            lwcarray% = dim(rtestep$(),1)
            ti% = 1% : phfact(ti%) = 1
            palevel%(ti%), palevel%, x% = 0%: xx% = lwcarray%
            plowkey$ = str(assypart$,1,25) & str(assyrte$,1,3) & " "
            compplowkey$ = str(assypart$,1,25) &                         ~
                                                str(assybom$,1,3) & "  0"

            gosub L08000
              if errormsg$ <> " " then L65000

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
              errormsg$="TOO MANY WORK CENTER STEPS, PART " & assypart$  ~
                          & " RTE " & assyrte$
              return

L08110:    x% = x% + 1%
               get #7 using L08160, str(rtestep$(x%),32,4),                ~
                      str(rtestep$(x%),1,31), str(rtestep$(x%),36,56),   ~
                      str(rtestep$(x%),93,43), str(rtestep$(x%),144,33), ~
                      str(rtestep$(x%),177,16)

L08160:            FMT CH(4), CH(31), CH(56), CH(43), POS(151), CH(33),  ~
                       POS(135), CH(16)

            put str(rtestep$(x%)) using L08200,                            ~
                                             palevel%(ti%), phfact(ti%)
L08200:         FMT POS(92), BI(1), POS(136), PD(14,4)
            pbs$(x%) = str(rtestep$(x%),88,5)

            goto L08040

L09000: REM *************************************************************~
            * NOW TEST FOR PHANTOM ASSEMBLIES                           *~
            *************************************************************

           call "PLOWNEXT" (#15, compplowkey$, 28%, f1%)
                if f1% =  0% then return
           compbom$, comprte$ = " "
           get #15, using L09170, comppart$, qu, tu, mkr$,                 ~
                                compbom$, pbs$
L09170:       FMT CH(25), XX(31), 2*PD(14,4), XX(16), CH(2), XX(1),      ~
                  CH(3), XX(4), CH(4)

           if mkr$ <> "PA" then L09000

           put str(pbs$,5) using L09225, palevel%(ti%)
L09225:        FMT BI(1)

        REM * * * PHANTOM LOOP LOGIC * * *

             if palevel% < 99% then L09310
              errormsg$="TOO MANY PHANTOMS FOR THIS ASSEMBLY " & assypart$
              goto L09712
L09310:      oldcompplowkey$(ti%) = compplowkey$
             ti% = ti% + 1%
             phfact(ti%)= phfact(ti%-1%)*qu*tu
                  if compbom$ <> " " then L09440
                     testbom$ = str(comppart$,1,25) & "1" & hex(000000)
                         call "PLOWNEXT" (#24, testbom$, 26%, f1%)
                         if f1% = 1% then L09410
                                   errormsg$ = "PART: " & comppart$ &    ~
                                               " HAS NO EFFECTIVE BOM."
                                   goto L09712
L09410:                   get #24, using L09420 , eff$()
L09420:                       FMT XX(29), 490 * CH(3)
                     compbom$ = eff$(ed%)
L09440:      compplowkey$ = str(comppart$,1,25) &                        ~
                                            str(compbom$,1,3) & "  0"

           call "READ100" (#15, compplowkey$, f1%)
                if f1% =  1% then goto L09730
                     errormsg$ = "PART: " & comppart$ & " BOM: " &       ~
                                 compbom$ & " NOT FOUND."
L09712:              return clear all
                     goto L65000
L09730:    get #15, using L09740, comprte$, phflag$
L09740:         FMT POS(87), CH(3), POS(106), CH(1)

             plowkey$ = str(comppart$,1,25) & str(comprte$,1,3) & " "
             gosub L10000
             palevel%(ti%), palevel% = palevel% + 1%
             gosub L08000
               if errormsg$ <> " " then L09712
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
