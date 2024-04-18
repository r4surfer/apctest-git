        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *   CCC   U   U   SSS    CCC   H   H  EEEEE   CCC   K   K   *~
            *  C   C  U   U  S      C   C  H   H  E      C   C  K  K    *~
            *  C      U   U   SSS   C      HHHHH  EEEE   C      KKK     *~
            *  C   C  U   U      S  C   C  H   H  E      C   C  K  K    *~
            *   CCC    UUU    SSS    CCC   H   H  EEEEE   CCC   K   K   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CUSCHECK - THIS SUBROUTINE CHECKS THE CUSTOMER STATUS IN  *~
            *            THE CUSTOMER FILE AND THE CURRENT CREDIT STATUS*~
            *            BASED ON CREDIT EXPOSURE.  INFO MESSAGES ARE   *~
            *            DISPLAYED IF THERE IS A PROBLEM.               *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/15/92 ! ORIGINAL                                 ! JDH *~
            * 03/23/94 ! PRR 12840. Honors switch for only using  ! JDH *~
            *          !   A/R Balances in credit limit checking. !     *~
            * 04/04/94 ! PRR 13006,13055. Corrected spelling.     ! JDH *~
            * 01/13/95 ! Consider SO Auto-hold flag whether to    ! JDH *~
            *          !   display credit messages. PRR 13345.    !     *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        sub "CUSCHECK" (customer$,       /* Customer Code              */~
                        cr_ar_only$,     /* Only use A/R Balances?     */~
                        #01,             /* #01 - CUSTOMER             */~
                        keyhit%)         /* 0%  = Cancel on return     */
                                         /* 16% = Continue on return   */

        dim ar(2), oo(2),                /* Bill-to & Family amounts   */~
            billto$9,                    /* Bill-to of Customer        */~
            crflag$1,                    /* Credit Hold Module Flag    */~
            cr_ar_only$1,                /* Only use A/R Balances?     */~
            cstat_msg$(5)8,              /* Status messages            */~
            cstat_msg2$(3)38,            /* Who's status message       */~
            cstat_msg3$(3)24,            /* Who's not there            */~
            cstatus$1,                   /* Customer Status - CUSTOMER */~
            customer$9,                  /* Customer Passed In         */~
            msg$(3)80,                   /* Status messages - ASKUSER  */~
            parent$9,                    /* Credit Parent of Customer  */~
            readkey$99                   /* General purpose read var.  */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            msg$() = " "

            cstat_msg$(1) = "ACTIVE"
            cstat_msg$(2) = "UNKNOWN"
            cstat_msg$(3) = "INACTIVE"
            cstat_msg$(4) = "ON HOLD"
            cstat_msg$(5) = "DELETE"

            cstat_msg2$(1) = "Customer's status is '"
            cstat_msg2$(2) = "Customer's Bill-to's status is '"
            cstat_msg2$(3) = "Customer's Credit Parent's status is '"

            cstat_msg3$(1) = "Customer"
            cstat_msg3$(2) = "Customer's Bill-to"
            cstat_msg3$(3) = "Customer's Credit Parent"

            call "BCKSWTCH" ("BCK", "CRHOLD  ", crflag$, temp, u3%)
                temp = u3% /* Just to stop the compile error */

        REM *************************************************************~
            *       S T A R T   C H E C K I N G   H E R E               *~
            *-----------------------------------------------------------*~
            * 1st) We'll check the status in the customer master file.  *~
            *      This will be reported on line 1 of the ASKUSER.      *~
            * 2nd) We'll check the credit exposure for the customer.    *~
            *      This will be reported on line 2 of the ASKUSER.      *~
            * 3rd) We'll check for late payments for the customer.      *~
            *      This will be reported on line 3 of the ASKUSER.      *~
            *                                                           *~
            * If the customer is active and there are no intrusions on  *~
            * their credit limit or late payment limit, then no user    *~
            * warning will occur.                                       *~
            *************************************************************

*        Customer status
            readkey$ = customer$
            loop% = 1%
        read_here
            call "READ100" (#01, readkey$, hit%)
            if hit% = 1% then L10140
                msg$(2) = cstat_msg3$(loop%) & " (" & readkey$ &         ~
                          ") is NOT on file!!!"    /* This couldn't     */
                msg$(3) = "Press RETURN to Cancel" /* happen, could it? */
                goto call_askuser_now
L10140:     get #01 using L10150, cstatus$, parent$
L10150:         FMT POS(793), CH(1), POS(1049), CH(9)
            if loop% = 1% then get #01 using L10170, billto$
L10170:         FMT POS(780), CH(9)
            if pos("AIHD"=cstatus$) = 0 then cstatus$ = "X" /* UNKNOWN */
            if pos("AXIHD"=cstatus$) < 2 then L10340    /* VALID STATUS */
                msg$(1) = cstat_msg2$(loop%) &                           ~
                          cstat_msg$(pos("AXIHD"=cstatus$)) & "'."
                goto check_credit_limit

L10340
*        Now test Customer's Bill-to and Credit Parent
L10350:     loop% = loop% + 1% : cstatus$ = " "
            if loop% > 2% then L10420
                if customer$ = billto$ then L10350
                readkey$ = billto$
                goto read_here

L10420:     if loop% > 3% then check_credit_limit
                if parent$ = " " then L10350
                readkey$ = parent$
                goto read_here

*        Now check for exceeding credit limit
        check_credit_limit
          if crflag$ = "N" then call_askuser_now
            par% = 0%
            call "BCKCRDSB" (billto$, oo(), ar(), crlimit, par%)
            if cr_ar_only$ = "Y" then oo(2%) = 0
            if oo(2%) + ar(2%) <= crlimit then check_late_limit
                msg$(2) = "Customer or its Credit Family currently"  &   ~
                          " over the Monetary Credit Limit."
                goto check_late_limit

*        Now check for exceeding late payment limit
        check_late_limit
            call "BCKDAYSB" (billto$, late%)
            if late% = 0% then call_askuser_now
                msg$(3) = "Customer or its Credit Family currently"  &   ~
                          " has outstanding Late Payments due."
                goto call_askuser_now

*        Display warning message now
        call_askuser_now
            if msg$() = " " then no_problem
            keyhit% = 2%   /* OPEN WINDOW AT BOTTOM */
            call "ASKUSER" (keyhit%,                                     ~
                            "*** Press PF(16) to Continue or RETURN " &  ~
                            "to Cancel ***",                             ~
                            msg$(1), msg$(2), msg$(3))
            if msg$(3) = "Press RETURN to Cancel" then keyhit% = 0%
            if keyhit% = 16 then get_out
            if keyhit% <> 0 then call_askuser_now
                goto get_out

*        No problem exists for this customer so set KEYHIT% and GET out
        no_problem
            keyhit% = 16%
            goto get_out

*        All done, now read customer again for future GETs by caller
        get_out
            readkey$ = customer$
            call "READ100" (#01, readkey$, hit%)
            goto L65000

L65000: REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

            end
