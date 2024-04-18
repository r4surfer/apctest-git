        REM *************************************************************~
            * APCHNYSB - Find 1st S.0 For 12 Monthly Buckets.           *~
            *                                                           *~
            *                                                           *~
            * RECORD LAYOUT FOR - (SYSFILE2)                            *~
            *   1. CH(20)      -  '*STARTING S. O. NO.*' (Primary Key)  *~
            *   2. CH(2)       -  Base Year For Buckets                 *~
            *   3. 12*CH(08)   -  Starting S.O. For Each Month          *~
            *                                                           *~
            *************************************************************~
            * 03/31/98  ERN  Y2K Modifications                          *~
            *************************************************************

            sub "APCHNYSB" (so$(),       /* 12 Monthly Buckets S.O. NO*/ ~
                            dt$(),       /* 12 DATES                  */ ~
                               #2,       /* BCKLINES File             */ ~
                               #3)       /* BCKMASTR File             */

        dim                                                              ~
            apc_so$8,                    /* Beginning S.O. Number      */~
            so$(12)8,                    /* Starting S.O. for each MON */~
            dt$(12)6,                    /* 12 Month Date Table        */~
            bck_key$19,                  /* BCKLINES - Primary Key     */~
            bck_cust$9,                  /* Customer Number            */~
            dte$6,                       /* Date S.O. Entered          */~
            yr$2,                        /* Current Year               */~
            date$6,                      /* Todays Date                */~
            base_yr$2,                   /* Base Year For Buckets      */~
            i$2,                         /* Month Counter              */~
            sys_key$20,                  /* SYSFILE2 Key               */~
            hdr_key$25,                  /* BCKMASTR Key               */~
            temp$10, temp2$10            /* Temporary variables        */

        dim f2%(3),                      /* = 0 if the file is open    */~
            f1%(3),                      /* = 1 if READ was successful */~
            fs%(3),                      /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(3)20                   /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "01.00.00 04/05/91 Pre-Release Version            "
        REM *************************************************************

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! SYSFILE2 ! SYSTEM MASTER FILE                       *~
            * #02 ! BCKLINES ! S.O. DETAIL LINES FILE                   *~
            * #03 ! BCKMASTR ! S.O. MASTER HEADER FILE                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            call "OPENCHCK" (#01, fs%(01), f2%(01),   0%, rslt$(01))

            f1%(1), f1%(2), f1%(3) = 0

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

        REM *************************************************************~
            *             C H E C K   O R D E R   S T A T U S           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************
                                         /* 1ST FIND LAST SALES ORDER */

            date$     = date
            temp1$    = date
            call "DATEOKC" (temp1$, temp%, temp2$)
            yr$       = str(temp2$,3%,2%)  /* SAVE CURRENT YEAR         */
            convert str(temp2$,5%,2%) to curr_mon%, data goto L10100
L10100:
            base_yr$  = "91"              /* SET BASE YEAR             */
            init(" ") so$(), dt$(), apc_so$
            sys_key$ = "*STARTING S. O. NO.*"
            read #1,key = sys_key$, using L10160,sys_key$,base_yr$, so$(),~
                                                           eod goto L10200
L10160:        FMT CH(20), CH(2), 12*CH(08)
            if base_yr$ = yr$ then goto L10200
               base_yr$ = yr$             /* RESET BUCKETS FOR NEXT YR */
               init(" ") so$()
L10200:     for i% = 1% to 12%
                convert i% to i$, pic(00)

                dt$(i%) = base_yr$ & i$ & "01" /* BUILD DATE TABLE     */
                if so$(i%) <> " " then apc_so$ = so$(i%)
            next i%

            call "SHOSTAT" ("Scanning Sales Orders From - "& apc_so$)
            bck_key$ = all(hex(00))
            if apc_so$ <> " " then str(bck_key$,1%,8%) = apc_so$
                                                   /* BCKLINES - START */
        read_orders
            read #2,key > bck_key$, using L10340, bck_cust$, bck_key$,    ~
                                                  eod goto orders_done
L10340:        FMT CH(9), CH(19)
            hdr_key$ = " " : mon% = 1%
            str(hdr_key$,1%,9%)   = bck_cust$
            str(hdr_key$,10%,16%) = str(bck_key$,1%,16%)
            read #3,key = hdr_key$, using L10390, dte$, eod goto L10480
L10390:        FMT POS(830), CH(6)
            temp1$ = dte$
            call "DATEOK" (temp1$, temp%, temp2$)
            dte$ = str(temp2$,3)    /* dte$ = YYMMDD */
            convert str(dte$,5%,2%) to mon%, data goto L10410
L10410:
            if so$(mon%) <> " " then goto L10480
               if str(dte$,1%,2%) <> str(dt$(mon%),1%,2%) then goto L10480
               if mon% = 1% then goto L10470
                  if so$(mon% - 1%) > str(bck_key$,1%,8%) then goto L10480

L10470:        so$(mon%) = str(bck_key$,1%,8%)
L10480:     str(bck_key$,17%,3%) = "999"
            if so$(mon%) <> " " and curr_mon% = mon% then                ~
                                                     goto orders_done
            goto read_orders
        orders_done
            sys_key$ = "*STARTING S. O. NO.*"
            read #1,hold,key = sys_key$,using L10560 ,sys_key$,           ~
                                                          eod goto L10580
L10560:          FMT CH(20)
            delete #1
L10580:     put #1, using L10160, sys_key$, base_yr$, so$()
            write #1

        goto exit_program

        exit_program

        end
