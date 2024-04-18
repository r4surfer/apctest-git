        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   PPPP   RRRR   U   U  PPPP   DDDD    SSS    BBBB   *~
            *  C      P   P  R   R  U   U  P   P  D   D  S       B   B  *~
            *  C      PPPP   RRRR   U   U  PPPP   D   D   SSS    BBBB   *~
            *  C      P      R  R   U   U  P      D   D      S   B   B  *~
            *   CCC   P      R   R   UUU   P      DDDD    SSS    BBBB   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CPRUPDSB - Handles almost all reads of the CPRPRICE and   *~
            *            CPRCURCY files. It updates to the future price *~
            *            figures if the future price date so indicates. *~
            *            This lends transparency to the implementation  *~
            *            of new price sets. Updates only the 'C'-type   *~
            *            records; ignores 'S'pecials.                   *~
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
            * 01/30/89 ! ORIGINAL (Proj 7880714)                  ! JIM *~
            * 04/14/89 ! Fixed Read101 to use key passed in       ! RJM *~
            * 04/10/90 ! Mod so that file is not read on hold and ! JDH *~
            *          !  than returned to caller with record held!     *~
            *          !  Also, removed unnessesary code & gosubs.!     *~
            * 06/28/96 ! Add blank date for test                  ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "CPRUPDSB"   (#01,           /* CPRPRICE or CPRCURCY UFB   */~
                                         /* Caller must open it, etc.  */~
                          file%,         /* 0%= CPRPRICE; 1%= CPRCURCY */~
                          type$,         /* Type of READ to perform    */~
                          key$,          /* Key passed by the caller   */~
                          kl%,           /* Key length (PLOWs only)    */~
                          hit%)          /* READ result is passed back */

        dim                                                              ~
            blank_date$8,                /* Blank date                 */~
            c_s$1,                       /* Record type 'C' or 'S'     */~
            fut_date$6,                  /* Future price set date      */~
            fut_data$201,                /* Future price set prices    */~
            key$51,                      /* Key passed from caller     */~
            minus1(16),                  /* 16 FP -1s                  */~
            neg9s(8),                    /* 8 FP -999.99s              */~
            nulls$207,                   /* Null values (-1, etc.)     */~
            today$6,                     /* Today's date (YYMMDD)      */~
            type$2                       /* Customer Type Code         */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! CPRxxxxx ! xxxxx depends on caller (PRICE or CURCY) *~
            *************************************************************

        REM *************************************************************~
            *       D E T E R M I N E   W H A T   T O   D O             *~
            *************************************************************
				blank_date$ = " " : call "DATUNFMT" (blank_date$)

            if beenherebefore% <> 0% then goto L09140
                beenherebefore% = 1%
                today$ = date
                mat minus1 = con : mat minus1 = (-1) * minus1
                for i% = 1% to 8%
                     neg9s(i%) = -999.99
                next i%
                put nulls$ using L09120, " ", minus1(), neg9s()
L09120:             FMT CH(15), 16*PD(14,4), 8*PD(14,4)

L09140:     hit% = 0% /* In case Invalid TYPE$ */
            update% = 0% /* Indicate 'no update done' */
            if type$ = "00" then goto read100_routine
            if type$ = "PN" then goto plownext_routine
            if type$ = "RN" then goto readnext_routine
            end  /* Invalid TYPE$ */

        REM *************************************************************~
            *        V A R I O U S   R E A D   R O U T I N E S          *~
            *************************************************************

        read100_routine /* Returns record = KEY$, UNheld */
            call "READ101" (#01, key$, hit%)
            if hit% = 0% then end
                gosub perform_future_price_update
                if update% = 0% then gosub call_strtrlse
                end

        plownext_routine /* Returns next record, UNheld */
            call "PLOWNXT1" (#01, key$, kl%, hit%)
            if hit% = 0% then end
                gosub perform_future_price_update
                if update% = 0% then gosub call_strtrlse
                end

        readnext_routine /* Returns next record, unHELD */
            call "READNXT1" (#01, hit%)
            if hit% = 0% then end
                gosub perform_future_price_update
                if update% = 0% then gosub call_strtrlse
                end

        REM *************************************************************~
            * H E R E ' S  T H E  P R I C E  U P D A T E  R O U T I N E *~
            *************************************************************

        perform_future_price_update /* All records are HELD coming in */
            get #01 using L15060, c_s$, fut_date$, fut_data$
L15060:         FMT CH(1), POS(494), CH(6), CH(201)
            if c_s$ <> "C" then return
            if fut_date$ = blank_date$ then return
            if fut_date$ > today$ then return

*        The new price set must now be put into effect.
            update% = 1% /* Indicate 'record updated' */
            if file% = 0% /* Update depends on which file it is */       ~
                then put #01 using L15190, fut_data$, nulls$              ~
                else put #01 using L15200, fut_data$, nulls$
            rewrite #01
            return

L15190:     FMT POS(48), CH(201), POS(494), CH(207) /* CPRPRICE */
L15200:     FMT POS(52), CH(201), POS(494), CH(207) /* CPRCURCY */

        call_strtrlse
            call "STRTRLSE" addr (#01)
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN
