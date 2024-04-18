        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   RRRR    CCC   RRRR    GGG    SSS   TTTTT  RRRR    *~
            *  C   C  R   R  C   C  R   R  G      S        T    R   R   *~
            *  C      RRRR   C      RRRR   G       SSS     T    RRRR    *~
            *  C   C  R   R  C   C  R  R   E          S    T    R  R    *~
            *   CCC   R   R   CCC   R   R  EEEEE   SSS     T    R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CRCRGSTR - Prints Cash Receipts Register.                 *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/15/86 ! Original                                 ! ERN *~
            * 02/09/90 ! Rounded check amount to 2 decimals.      ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            billto$9,                    /* Bill-to Customer Code      */~
            check$8,                     /* Check Number               */~
            check_amt$13,                /* Check Amount               */~
            check_date$8,                /* Check Date                 */~
            compname$60,                 /* Company Name               */~
            date$8,                      /* Run Date                   */~
            name$30,                     /* Bill-to Name               */~
            plowkey$50,                  /* Plow Key                   */~
            postdate$8,                  /* Session Post Date          */~
            runtime$8,                   /* Report Run Time            */~
            session$6,                   /* Session ID                 */~
            srcedoc$8,                   /* Source Document Applied To */~
            stlmnt$14,                   /* Settlement Number          */~
            userid$3                     /* Current User Id            */~

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.01 04/13/90 Patch Release w/ Auto Replace   "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! CRCRGSRF ! Cash Receipts Register Report File       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "CRCRGSRF",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  30

            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "SHOSTAT" ("Printing Cash Receipts Register")
            call "EXTRACT" addr("ID", userid$)
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (runtime$)
            call "COMPNAME" (12%, compname$, f1%(2))
            call "SETPRNT" ("CRC003", " ", 0%, 0%)
            select printer (134)

        REM *************************************************************~
            *                P R I N T    R E P O R T                   *~
            *-----------------------------------------------------------*~
            * Print a report for each session on file for this User.    *~
            *************************************************************


        session_loop
            plowkey$ = str(userid$) & hex(00)
            call "PLOWNEXT" (#1, plowkey$, 3%, f1%(1))
            if f1%(1) = 0% then exit_program

            get #1 using L10130, session$, postdate$
L10130:         FMT POS(4), CH(6), POS(40), CH(6)
            call "DATEFMT" (postdate$)
            sdisca, sdiscu, snet, scheck = 0
            page%   = 0%
            checks% = 0%
            gosub page_heading


        header_loop
            call "PLOWNEXT" (#1, plowkey$, 9%, f1%(1))
            if f1%(1) = 1% then L10290
                gosub session_totals
                plowkey$ = str(userid$) & session$
                call "DELETE" (#1, plowkey$, 9%)
                goto session_loop

L10290:     get #1 using L10310, billto$, check$, name$, check_date$,     ~
                                check_amt
L10310:         FMT XX(9), CH(9), CH(8), POS(31), CH(30), CH(6), PD(14,4)
            call "DATEFMT" (check_date$)
            check_amt = round(check_amt, 2)
            convert check_amt to check_amt$, pic(-#####,###.00)
            hdrprint% = 1%
            lines% = 0%
            cdisca, cdiscu, cnet = 0

            line_loop
                call "PLOWNEXT" (#1, plowkey$, 26%, f1%(1))
                if f1%(1) = 1% then L10430
                     gosub check_totals
                     goto  header_loop
L10430:         get #1 using L10440, srcedoc$, stlmnt$, disca, discu, net
L10440:              FMT POS(31), CH(8), CH(12), 3*PD(14,4)
                disca = round(disca, 2)
                discu = round(discu, 2)
                net   = round(net,   2)
                if stlmnt$ <> " " then stlmnt$ = str(stlmnt$,1,8) & "-" &~
                                                 str(stlmnt$,9,2) & "-" &~
                                                 str(stlmnt$,11)
                if line% > 54% then gosub page_heading
                if hdrprint% > 0% then                                   ~
                     print using L11210, billto$, name$, check$,          ~
                                        check_date$, check_amt$,         ~
                                        srcedoc$, stlmnt$, disca, discu, ~
                                        net                              ~
                               else                                      ~
                     print using L11210, " ", " ", " ", " ", " ",         ~
                                        srcedoc$, stlmnt$, disca, discu, ~
                                        net
                hdrprint% = 0%
                line%  = line%  + 1%
                lines% = lines% + 1%
                cdisca = cdisca + disca
                cdiscu = cdiscu + discu
                cnet   = cnet   + net
                goto line_loop

            check_totals
                print using L11240
                print using L11270, check$, check_amt, lines%,            ~
                                   cdisca, cdiscu, cnet
                print
                line%   = line%   + 3%
                sdisca  = sdisca  + cdisca
                sdiscu  = sdiscu  + cdiscu
                snet    = snet    + cnet
                scheck  = scheck  + check_amt
                checks% = checks% + 1%
                lines%  = 0%
                cdisca, cdiscu, cnet = 0
                return


            session_totals
                print using L11300, checks%, scheck, sdisca, sdiscu, snet
                print "** END OF REPORT **"
                return


            page_heading
                page% = page% + 1%
                line% = 6%
                print page
                print using L11030, date$, runtime$, compname$
                print using L11060, postdate$, session$, page%
                print
                print using L11120
                print using L11150
                print using L11180
                return




L11030: %RUN DATE: ######## ########                 ####################~
        ~########################################             CRCRGSTR-CRC~
        ~003
L11060: %    POST: ######## (######)                         C A S H   R ~
        ~E C E I P T S   R E G I S T E R                          PAGE: ##~
        ~##
        %                               #################################~
        ~###########################

L11120: %  BILL-TO                                  CHECK    CHECK       ~
        ~          SOURCE DOC    PAYMENT       ALLOWED   UNALLOWED    N E ~
        ~T
L11150: % CUSTOMER CUSTOMER NAME                   NUMBER    DATE    CHEC~
        ~K AMOUNT  APPLIED TO SETTLEMENT NO.  DISCOUNTS  DISCOUNTS   PAYME~
        ~NT
L11180: %--------- ------------------------------ -------- --------  ----~
        ~--------  ---------- --------------  ---------  ---------  ------~
        ~---
L11210: %######### ############################## ######## ######## #####~
        ~########   ########  ############## -######.## -######.## -######~
        ~.##
L11240: %                                                            ----~
        ~--------                             ---------  ---------  ------~
        ~---
L11270: %                               * CHECK ######## TOTALS     -####~
        ~#,###.##    (#### LINES)            -######.## -######.## -######~
        ~.##
L11300: %                              ** TOTALS (##### CHECKS)     -####~
        ~#,###.##                            -######.## -######.## -######~
        ~.##


        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "SETPRNT" ("CRC003", " ", 0%, 1%)
            end
