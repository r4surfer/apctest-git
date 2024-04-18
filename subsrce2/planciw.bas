        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   L       AAA   N   N   CCC   IIIII  W   W          *~
            *  P   P  L      A   A  NN  N  C   C    I    W   W          *~
            *  PPPP   L      AAAAA  N N N  C        I    W   W          *~
            *  P      L      A   A  N  NN  C   C    I    W W W          *~
            *  P      LLLLL  A   A  N   N   CCC   IIIII   W W           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLANCIW  - CALCIW SECTION EXTRACTED FROM PLANSUB.         *~
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
            * 09/10/86 ! ORIGINAL                                 ! KAB *~
            * 03/25/87 ! POTENTIAL BUG WITH PIP(491)?             ! KAB *~
            * 04/20/87 ! SS INTRUSION FLAG INVERTED (OOPS)        ! KAB *~
            * 06/21/93 ! CODE FOR PRESCAN TO ALLOCATE PH FROM STK ! KAB *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "PLANCIW" (dt$,              /* DEMAND TYPE                */~
                       testdemd$,        /* SWITCH MASK                */~
                       errormsg$,        /* THE RETURN MESSAGE         */~
                       qtydd,            /* QTY ON DELIVERY DAY        */~
                       cd%,              /* COMPLETION DAY OFFSET      */~
                       ml%,              /* MATERIALS ARRAY LOCATION   */~
                       todaya%,          /* ACTUAL TODAY               */~
                       workfile%,        /* WORKFILE STATUS            */~
                       atcf%,            /* WARNING FLAG               */~
                       prescan%,         /* PRELIMINARY SCAN  0 = real */~
                                         /*                   1 = test */~
                       #2,               /* PIPMASTR                   */~
                       #41,              /* SFCUM2  CUMULATIVE FCSTS   */~
                       #62)              /* MATERIALS WORKFILE         */~

        rem************** explanation of variables **********************~
           *  arrays = 1000 are for each line in the plan               *~
           *  arrays = 100 are for each work center step for any line   *~
           *  *do not change com block.  it is used several places!!!!* *~
           **************************************************************

        com planflags$(25)20,                                            ~
            yymmdd$(490)6,                                               ~
            eff$(490)3,                  /* EFF, PLT WORK ARRAY        */~
            oldcompplowkey$(100)31,      /* FOR PHANTOM LOGIC          */~
            pip%(490),                                                   ~
            cumf%(490),                                                  ~
            awca%(490),                  /* CONCURRENT WC AVAILABILITY */~
            awcu1%(490),                 /* 1ST CONCURRENT USED        */~
            awcu2%(490),                 /* 2ND CONCURRENT USED        */~
            awcu3%(490),                 /* 3RD CONCURRENT USED        */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            phfact(101),                                                 ~
                               /* THE ABOVE ELEMENTS CANNOT BE CHANGED */~
            part$    (1000)25,                                           ~
            intagnr$ (1000)19,                                           ~
            outtagnr$(1000)19,                                           ~
            rte$     (1000)3,            /* WHICH ROUTE TO USE         */~
            bom$     (1000)3,            /* WHICH BOM TO USE           */~
            ed%      (1000),                                             ~
            sd%      (1000),                                             ~
            parline% (1000),             /* PARENT LINE NUMBER         */~
            action%  (1000),             /* ACTION TO TAKE             */~
            lt%      (1000),             /* LEADTIME ARRAY             */~
            moq%     (1000),             /* MOQ                        */~
            type%    (1000),             /* PART TYPE                  */~
            qtyu     (1000),             /* QTY USED (NEEDED)          */~
            qtyp     (1000),             /* QTY TO PROCURE             */~
                               /* THE ABOVE ELEMENTS ARE THE MATERIALS */~
            wc$(2000)4,                  /* WORK CENTER                */~
            wl$(2000)1,                  /* JUST FOR SUMMARY REPORT    */~
            wa$(2000)1,                  /* IN CASE OF ARRAY OVERFLOW  */~
            ws$(2000)5,                  /* WC STEP #                  */~
            wl%(2000),                   /* LINE STACK                 */~
            du%(2000),                   /* DATE USED ARRAY STACK      */~
            au%(2000),                   /* AMOUNT USED ARRAY STACK    */~
            su%(2000),                   /* SET UP TIME TODAY          */~
            wa%(2000),                   /* WC ALTERNATE SEQ NO.       */~
                               /* THESE ARE THE WORK CENTER ARRAYS     */~
            rtestep$(255)150,            /* THE STEP AS A STRING       */~
            step$   (255)5,              /* STEP                       */~
            yld     (255),               /* STEP YIELD                 */~
            pd%     (255),               /* WC STEP START DATE(SPL PCK)*/~
            mmx%    (255),               /* START OF RTE STEP          */~
            xsd%    (255)                /* START OF RTE STEP          */~
                               /* THESE ARE FOR ROUTE STEPS            */~

        dim                                                              ~
           errormsg$79,                  /*                            */~
           matkey1$50,                   /*                            */~
           testbyte$6,                   /*                            */~
           testdemd$6,                   /*                            */~
           fwdpct%(8),                   /*                            */~
           fwdfxd%(8)                    /*                            */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "

        REM *************************************************************

            l% = ml%
            if workfile% = 0% then L01830
               l% = 2%
                  if prescan% <> 0% then l% = 3%

L01830:     if ml% <> 1% then L07000
            get str(planflags$(),209) using L01850, fwdpct%(), fwdfxd%()
L01850:         FMT 8*BI(4), 8*BI(4)

L07000: rem**************************************************************~
           *                   c a l c i w                              *~
           *                                                            *~
           *  this routine calculates the amount that can be withdrawn  *~
           *  from planned inventory as of ed%(l%) and sets qtyp(l%)    *~
           **************************************************************

           errormsg$ = " "

           mat cumf% = zer : qtyp(l%) = 0 : atcf% = 0%

           call "READ100"(#2, part$(l%), f1%(2))
              if f1%(2%) <> 0% then goto L07160
                 errormsg$ = "PART: " & part$(l%) &" NOT FOUND IN THE PIP"
                 goto L65000

           if qtyu(l%) < 0 then L65000

L07160:    get #2, using L07170 , pip%(), ss, pz, atch%
L07170:         FMT XX(26), 490*BI(4), XX(8), PD(14,4), XX(8), PD(14,4), ~
                    XX(4), BI(2)

           atch% = max(0%, mod(atch%, 1000%))
           atc% = 0%

        REM TEST TO ALLOW VARIALBE ATC HORIZON
         str(testdemd$,1,1)  = bin(2^(int(type%(l%)/100)-2%))
         str(testbyte$,1,6)=str(testdemd$,1,6) and str(planflags$(),130,6)
         if str(testbyte$,1,6)<>hex(000000000000) then L07240
            atch% = 999%

L07240:    if type%(l%)=0% then L07270
           if l% > 1% then L07310
           if dt$ < "7" then L07430
L07270:       qtyp(l%) = qtyu(l%)
              goto L65000

        REM TEST FOR FORCED IW
L07310:   str(testdemd$,1,1)  = bin(2^(int(type%(l%)/100)-2%))
          str(testbyte$,1,6)=str(testdemd$,1,6) and str(planflags$(),46,6)
          if str(testbyte$,1,6)<>hex(000000000000) then L65000

        REM TEST FOR FORCED PROCUREMENT
          str(testdemd$,1,1)  = bin(2^(int(type%(l%)/100)-2%))
          str(testbyte$,1,6)=str(testdemd$,1,6) and str(planflags$(),76,6)
          if str(testbyte$,1,6)=hex(000000000000) then L07430
              qtyp(l%) = qtyu(l%)
              goto L08250       /* TEST MOQ AND PANSIZE */

        REM ONLY COMMIT THE SAFETY STOCK WHEN FLAGGED
L07430:   ss% = int(ss)
          str(testdemd$,1,1)  = bin(2^(int(type%(l%)/100)-2%))
          str(testbyte$,1,6)=str(testdemd$,1,6) and str(planflags$(),34,6)
          if str(testbyte$,1,6)=hex(000000000000) then L07500
                ss% = 0%

        REM IGNORE NEGATIVE PIP?
L07500:   negpip% = -999999999%
          str(testdemd$,1,1)  = bin(2^(int(type%(l%)/100)-2%))
          str(testbyte$,1,6)=str(testdemd$,1,6) and str(planflags$(),40,6)
          if str(testbyte$,1,6)=hex(000000000000) then L07580
             negpip% = 0%

        REM ADJUST PIP FOR PRIOR (UNRECORDED) USE IN THIS PLAN

L07580:    if ml% < 2% then L07870
           if workfile% = 0% then L07760

                matkey1$ = str(part$(l%),1,25) & bin(10000% - ml%, 4)
L07620:         call "PLOWALTS" (#62, matkey1$, 1%, 25%, f1%(62))
                     if f1%(62) = 0 then L07870
                     get #62, using L07650, tempa%, edt%, temp, temp1
L07650:                  FMT XX(29),BI(1),XX(12),BI(4),XX(19),2*PD(14,4)

                     if tempa% > 0% then L07670

                        qtyp(l%) = -999999  /* SEQUENCE BLOCK */
                        if prescan% <> 0% then L65000
                        errormsg$ = "MATERIALS SEQUENCE ERROR "  &       ~
                                    part$(l%)
                        goto L65000

L07670:                   pipadj = temp1 - temp
                          pipadj% = sgn(pipadj)*int(abs(round(pipadj,2%)))
                          if pipadj% = 0% then L07620
                          for j% = edt% to 490%
                               pip%(j%) = pip%(j%) + pipadj%
                          next j%

                 goto L07620

L07760:         for i% = 1% to l%-1%
                     if part$(i%)<>part$(l%) then L07840
                     if action%(i%) > 0% then L07780

                        qtyp(l%) = -999999  /* SEQUENCE BLOCK */
                        if prescan% <> 0% then L65000
                        errormsg$ = "MATERIALS SEQUENCE ERROR "  &       ~
                                    part$(l%)
                        goto L65000

L07780:                   pipadj = qtyp(i%) - qtyu(i%)
                          pipadj% = sgn(pipadj)*int(abs(round(pipadj,2%)))
                          if pipadj% = 0% then L07840
                          for j% = ed%(i%) to 490%
                               pip%(j%) = pip%(j%) + pipadj%
                          next j%
L07840:          next i%

        REM TEST FOR IGNORE ATC, USE PIP
L07870:   atc% = pip%(ed%(l%))
          if ed%(l%) > todaya% + atch% then L08070
          str(testdemd$,1,1)  = bin(2^(int(type%(l%)/100)-2%))
          str(testbyte$,1,6)=str(testdemd$,1,6) and str(planflags$(),82,6)
          if str(testbyte$,1,6)<>hex(000000000000) then L08090

           if l% > 1% then L07950
              if dt$ = "1" then L08000
L07950:    call "READ100" (#41, part$(l%), f1%(41))
              if f1%(41%) <> 1% then L08000
           get #41, using L07980, cumf%()
L07980:       FMT XX(25), 490*BI(4)

L08000: REM CALCULATE THE AVAILABLE TO COMMIT
           atc% = 999999999%
           for i% = min(490%, todaya% + atch%)                           ~
                                to max(ed%(l%), todaya%) step -1%
             pip%(i%), atc% = max(negpip%, min(atc%,                     ~
                                    pip%(i%) - ss% - max(0, cumf%(i%))))
           next i%

L08070:      if atc% < 0% then atcf% = 1%

L08090:    atc% = max(atc%, negpip%)
           if qtyu(l%) <= min(atc%, pip%(ed%(l%))) then L08190
           if ed%(l%) >= 490% then L08190
           atch% = (fwdpct%(int(type%(l%)/100)-1%)*lt%(l%))/100%
           if atch% <> 0% then L08172
           atch% = fwdfxd%(int(type%(l%)/100)-1%)
           if atch% = 0% then L08190

L08172:    for i% = ed%(l%)+1% to min(ed%(l%)+atch%, 490%)
               if pip%(i%) < 0% then L08176
               atc% = max(pip%(i%), atc%)
               if qtyu(l%) <= atc% then L08190
L08176:    next i%

        REM CALCULATE QTYP
L08190:    if l% > 1% or ed%(1%)<>cd% then L08210
              qtydd = max(0, min(atc%, qtyu(l%)))
L08210:    qtyp (l%)   = max(0, qtyu(l%)-atc% )
           if abs(qtyp(l%)) < .00001 then L65000

        REM DISREGARD PANSIZE AND MOQ?
L08250:   if prescan% <> 0% then L65000
          str(testdemd$,1,1)  = bin(2^(int(type%(l%)/100)-2%))
          str(testbyte$,1,6)=str(testdemd$,1,6) and str(planflags$(),52,6)
          if str(testbyte$,1,6)<>hex(000000000000) then L65000

           qtyp (l%)   = max(qtyp(l%), moq%(l%))
           if pz < .0001 then L65000
           if abs(mod(qtyp(l%)-moq%(l%),pz)) < .00001 then L65000
           qtyp(l%)=round(qtyp(l%)+pz-mod(qtyp(l%)-moq%(l%),pz),2)

L65000:    end
