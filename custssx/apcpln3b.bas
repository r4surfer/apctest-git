        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLN3B                             *~
            *  Creation Date     - 03/18/96                             *~
            *  Last Modified Date- 11/14/97                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Calculate the Customer S.O. Due Date/*~
            *                      Delivery Date using the Set-Up Codes *~
            *                      Defined for Customer. Cut-Off Day    *~
            *                      always from Customer. Date may be    *~
            *                      Overroad by Puting in a Delivery     *~
            *                      Code. The Delivery Code Used for the *~
            *                      Date Calc is returned in DELIVERY$.  *~
            *                                                           *~
            *  Code Tables Used  - (PLAN CUTO) - Customer Cut-Off Day   *~
            *                                    and Time Primary (A).  *~
            *                    - (PLAN DELV) - Customer Delivery Days *~
            *                                    Primary (A).           *~
            *                                                           *~
            *  Special Comments  -  Hours and Minutes are used to Check *~
            *                       the Cut-Off Time (Convert to 24 Clk)*~
            *                                                           *~
            *                    -  When (DELIVERY$) Blank Use Customer *~
            *                       or Default Value, Otherwise use the *~
            *                       Value as an Override.               *~
            *                                                           *~
            *                    -  If (DFLTDUE$) is not Blank then     *~
            *                       routine is Exited and No Due Date   *~
            *                       is Calculated, and (DELIVERY$) is   *~
            *                       no changed.                         *~
            *                                                           *~
            *  Used By Programs  -  (BCKFASTR) - Sales Order Entry      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/18/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 05/29/97 ! Special Mod for Delivery Codes Greater   ! RHH *~
            *          ! than (80). Set INC% to the Number of Days!     *~
            *          ! Defined in Code.                         !     *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 03/29/98 ! Y2K                                      ! LDJ *~
            *************************************************************

            sub "APCPLN3B" (dfltdue$,    /* Calculated Delivery/Due DTE*/~
                            cuscode$,    /* Ship To Customer Code      */~
                            delivery$,   /* (In)-Use,(Out)-What Used   */~
                            #1,          /* (CUSTOMER) - Master File   */~
                            #2 )         /* (GENCODES) - Table File    */

        dim days$(7%)9,                  /* Days of the Week           */~
            day$9,                       /* Day of the Week            */~
            cuscode$9,                   /* Ship To Customer Code      */~
            readkey$24, desc$30,         /* Code Table Key and Descript*/~
            co$(2%)20, delivery$20,      /* Cut-Off Date Codes         */~
            cd$(2%)20, fob$(2%)20,       /* Customer Delivery Codes    */~
            ct%(2%), x$8,                /* Cut-Off Time 24 Hour Clock */~
            co%(2%),                     /* Customer Cut-Off Day Code  */~
            dd%(2%),                     /* Delivery Days Add-On       */~
            def_dte$6, dfltdue$8,        /* Calculated Due Date        */~
            td$6,                        /* Today's Date               */~
            jtd$7, errormsg$79           /* Julian Date - (YYDDD)               (Y2K, LDJ) */

            if str(delivery$,1%,2%) <> "00" then goto L00730
               call "DATEOK" (dfltdue$, er%, errormsg$)
               if er% <> 0% then goto exit_sub
                  init(" ") dfltdue$
                  dfltdue$ = date
                  call "DATEFMT" (dfltdue$)
                  goto exit_sub             /* Customer Service Will   */
                                            /* Assign the Due Date     */
L00730:     days$(1%) = "MONDAY   " : days$(5%) = "FRIDAY   "
            days$(2%) = "TUESDAY  " : days$(6%) = "SATURDAY "
            days$(3%) = "WEDNESDAY" : days$(7%) = "SUNDAY   "
            days$(4%) = "THURSDAY "
            delv% = 0%

            gosub load_data         /* Load Customer Cutoff/Delivery    */

            call "DATEOK" (dfltdue$, er%, errormsg$)
            if er% = 0% then goto L00860
               if str(delivery$,1%,1%) = " " then delivery$ = fob$(1%)
               goto exit_sub                /* Date already entered or */
                                            /* Previously Calculated   */
L00860:     gosub calc_today        /* Calc Today and Julian Adjustment*/
            convert str(delivery$,1%,2%) to delv%, data goto L00880
L00880:
            if delv% < 80% then goto L00920    /* 80 or Greater *Special* */
               convert str(delivery$,17%,2%) to inc%, data goto L00910
L00910:
L00920:     gosub calc_due_date     /* Convert Julian to New Delivery  */

            goto exit_sub

        load_data
            init(" ") readkey$, desc$, co$(), cd$(), errormsg$,          ~
                      fob$(), td$, x$, jtd$, def_dte$
            mat co% = zer : mat ct% = zer : mat dd% = zer
            co%(1%) = 4%                        /* Default Thursday    */
            ct%(1%) = 1730%                     /* Default 05:35 PM    */
            dd%(1%) = 18%                       /* Default Two Weeks   */
                                                /* Monday Delivery     */
            read #1,key = cuscode$, using L01060 , co$(1%), cd$(1%),       ~
                                    co$(2%), cd$(2%), eod goto L01390
L01060:        FMT POS(860), 4*CH(20)
            if len(co$(1%)) > 1 then goto L01110
               co$(1%) = "04"                    /* Default Thursday   */
               cd$(1%) = "10"                    /* Two Week (Fri)     */

L01110:     if len(delivery$) < 2 then goto L01150
               cd$(1%) = str(delivery$,1%,2%)    /* Use What Customer  */
                                                 /* Service Entered    */
               cd$(2%) = str(delivery$,1%,2%)    /* Primary/Secondary  */
L01150:     for i% = 1% to 2%
                str(readkey$,1%,9%)   = "PLAN CUTO"
                str(readkey$,10%,15%) = str(co$(i%),1%,2%)
                read #2,key = readkey$, using L01190 , desc$,eod goto L01390
L01190:            FMT POS(25), CH(30)
                                                /* Cut-Off Day Code (A)*/
                convert str(desc$,1%,1%) to co%(i%), data goto L01390
                                                /* Cut-Off Time Hour   */
                convert str(desc$,9%,2%) to h%, data goto L01390
                                                /* Cut-Off Time Minute */
                convert str(desc$,12%,2%) to m%, data goto L01390

                if str(desc$,15%,2%) = "AM" then goto L01300
                   if h% < 12% then h% = h% + 12%

L01300:         ct%(i%) = (100% * h%) + m%      /* HHMM% = Hrs and Mins*/
                str(readkey$,1%,9%)   = "PLAN DELV"
                str(readkey$,10%,15%) = str(cd$(i%),1%,2%)
                read #2,key = readkey$, using L01190 , desc$,eod goto L01390
                                                /* Add-On No. of Days-A*/
                convert str(desc$,14%,2%) to dd%(i%), data goto L01390
                                         /* Valid Primary Cut-Off Codes*/
                fob$(i%) = str(cd$(i%),1%,2%) & "/" & desc$
            next i%
L01390: return

        calc_today
            call "TIME" (x$)                       /* Set for Primary  */
            typ% = 1%                              /* Cut-Off First    */
            convert str(x$,1%,2%) to h%, data goto L01450
L01450:
            convert str(x$,4%,2%) to m%, data goto L01470
L01470:
            if str(x$,7%,2%) = "AM" then goto L01500
               if h% < 12% then h% = h% + 12%
L01500:     time% = (100% * h%) + m%
            td$ = date
            call "DATE" addr("GJ",td$, str(jtd$,,5%), er%)                          /* (Y2K, LDJ) */
            call "DATJULCV" (jtd$)                /* Todays Julian DTE*/            /* (Y2K, LDJ) */    
            call "DATE" addr("GD",td$, day$, er%)  /* Find Day of Week */
            for i% = 1% to 7%
              if str(days$(i%)) = str(day$) then goto L01580
            next i%
              i% = 1%
L01580:     td% = i%                          /* Current Day Week      */
        REM - 1st Check Primary Cut-Off       /* Check (Today) against */
          for typ% = 1% to 2%                 /* TYP% = 1% Primary     */
                                              /* TYP% = 2% Secondary   */
            if td% > co%(typ%) then goto L01690     /* Cut-Off Day for Wk*/
               if td% < co%(typ%) then goto L01670  /* Made the Cut-Off  */
                  if time% <= ct%(typ%) then goto L01670   /* " " " "    */
                     goto L01690                    /* Missed Cut-Off    */

L01670:        inc% = (co%(typ%) - td%) + dd%(typ%) /* Calc the Julian */
               return                               /* Date Adjustment */
L01690:   next typ%                                 /* Cutt-Off        */
        REM - After Cut-Off Date               /* Missed Prime or Sec. */
            inc% = (7% - td%) + co%(1%) + dd%(1%) /* go back to Prime  */
            typ% = 1%
        return

        calc_due_date
            ddd% = 365% : jyr% = 0% : jdd% = 0%
            convert str(jtd$,1%,4%) to jyr%, data goto L01780                       /* (Y2K, LDJ) */
L01780:
            convert str(jtd$,5%,3%) to jdd%, data goto L01800                       /* (Y2K, LDJ) */
L01800:
            if mod(jyr%,4%) = 0% then ddd% = 366%  /* Check for Leap Yr*/
            jdd% = jdd% + inc%                     /* Add Julian Adj.  */
            if jdd% <= ddd% then goto L01860
               jyr% = jyr% + 1%                    /* Delivery Next Yr */
               jdd% = jdd% - ddd%                  /* Day for Next Year*/
L01860:     convert jyr% to str(jtd$,1%,4%), pic(0000)                              /* (Y2K, LDJ) */

            convert jdd% to str(jtd$,5%,3%), pic(000)                               /* (Y2K, LDJ) */
            call "DATJULCV" (jtd$)                                                  /* (Y2K, LDJ) */
            call "DATE" addr("JG", str(jtd$,,5%), def_dte$, er%)                    /* (Y2K, LDJ) */
            dfltdue$ = def_dte$                    /* Calculated       */
            call "DATEFMT" (dfltdue$)              /* Delivery Date    */
            delivery$ = fob$(typ%)
        return

        exit_sub
        end
