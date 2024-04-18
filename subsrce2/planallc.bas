        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   L       AAA   N   N   AAA   L      L       CCC    *~
            *  P   P  L      A   A  NN  N  A   A  L      L      C   C   *~
            *  PPPP   L      AAAAA  N N N  AAAAA  L      L      C       *~
            *  P      L      A   A  N  NN  A   A  L      L      C   C   *~
            *  P      LLLLL  A   A  N   N  A   A  LLLLL  LLLLL   CCC    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLANALLC - EMULATE PALNNING  SYSTEM LOGIC TO DETERMINE    *~
            *            QUANTITY AVAILABLE FOR ALLOCATION AS OF DUE    *~
            *            DATE.  REACTION COULD BE SIMPLE DISPLAY OR     *~
            *            ACTUALLY ALLOCATION OF INVENTORY (IN THE CASE  *~
            *            OF POSTING SALES ORDERS).                      *~
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
            * 05/14/85 ! ORIGINAL                                 ! KEN *~
            * 09/08/92 ! Considers Previously Allocated Complete  ! JDH *~
            *          !   Quantity.                              !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "PLANALLC" (part$,           /* PART TO ALLOCATE           */~
                        qtyreq,          /* QUANTITY REQUESTED         */~
                        qtyall,          /* QUANTITY ALLOCATABLE (RET) */~
                                         /* Qty Prev Alloc Complete(IN)*/~
                                         /*   Only non-zero if line was*/~
                                         /*   previously saved as      */~
                                         /*   Allocate Complete 'C'.   */~
                        type$,           /* DEMAND TYPE                */~
                        priority$,       /* PRIORITY                   */~
                        req%,            /* DATE REQUIRED              */~
                        today%,          /* TODAY'S DATE               */~
                        flags$(),        /* PLANNING SYSTEM FLAGS      */~
                        #2,              /* PIPMASTR                   */~
                        #3,              /* SFCUM2                     */~
                        ret%)            /* RETURN CODE                */~

        dim part$25,                     /*                            */~
            priority$1,                  /*                            */~
            type$1,                      /*                            */~
            flags$(25)20,                /*                            */~
            testbyte$6,                  /*                            */~
            testdemd$6,                  /*                            */~
            cumf%(490),                  /*                            */~
            pip%(490)                    /*                            */~

        REM *************************************************************~
            *                                                           *~
            * BEGIN PROCESSING                                          *~
            *                                                           *~
            *************************************************************

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "
        REM *************************************************************
           prev_alloc = qtyall /* Non-zero if previously Alloc Complete */
           prev_ship% = ret%   /* Only set if previously Alloc Complete */
           qtyall = 0
           ret% = 99%                    /* 99 =  FLAGS TEST           */
           if str(flags$(),1,1) = " " then L65000

           str(testdemd$,1,6)=hex(00)                                    ~
                             & bin(2^(val(type$,1)-49%),1)               ~
                             & bin(2^(val(priority$,1)-64%),4)

        rem**************************************************************~
           *  calculate allocation amount                               *~
           *                                                            *~
           *  this routine calculates the amount that can be withdrawn  *~
           *  from planned inventory as of req%                         *~
           **************************************************************


           mat cumf% = zer : ss = 0

           ret% = ret% - 1%              /* 95 = PIP ERROR             */
           call "READ100"(#2, part$, f1%)
              if f1% = 0% then L65000

           get #2, using L12150, pip%(), ss, type%, atch%
L12150:         FMT XX(26), 490*BI(4), XX(8), PD(14,4), XX(16),          ~
                    BI(2), XX(2), BI(2)
           atch% = max(0%, mod(atch%, 1000%))

        REM Add previously allocated complete quantity, if necessary
           if prev_alloc = 0 then L12170
           for i% = prev_ship% to 490%
                pip%(i%) = pip%(i%) + prev_alloc
           next i%

L12170:    atc% = 0%
           if type% = 0% then L12450
           if type$ = "8"  then L12450

        REM TEST FOR IGNORE ATC, USE PIP
          atc% = pip%(req%)
          if req% > today% + atch% then L12390
          str(testdemd$,1,1)  = bin(2^(int(type%/100)-2%))
          str(testbyte$,1,6)=str(testdemd$,1,6) and str(flags$(),82,6)
          if str(testbyte$,1,6)<>hex(000000000000) then L12390

          if type$ = "1" then goto L12330
              call "READ100" (#3, part$, f1%)
                 if f1% = 0% then goto L12330
                     get #3, using L12310, cumf%()
L12310:                   FMT XX(25), 490*BI(4)

L12330: REM CALCULATE THE AVAILABLE TO COMMIT

           for i% = max(today%, req%) to min(490%, today%+atch%)
                atc% = min(atc%, (pip%(i%) - max(0, cumf%(i%))))
           next i%

L12390: REM ONLY COMMIT THE SAFETY STOCK WHEN FLAGGED
          str(testdemd$,1,1)  = bin(2^(int(type%/100)-2%))
          str(testbyte$,1,6)=str(testdemd$,1,6) and str(flags$(),34,6)
          if str(testbyte$,1,6)<>hex(000000000000) then L12450
                atc% = atc% - int(ss)

L12450: REM CALCULATE QTYALL

           atc%=max(atc%,0%)

           qtyall = max(0, min(atc%, qtyreq))
           ret% = 0%                    /* 0 = ALL OK                 */

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
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end

