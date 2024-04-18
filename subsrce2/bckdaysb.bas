        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  BBBB    CCC   K   K  DDDD    AAA   Y   Y   SSS   BBBB    *~
            *  B   B  C   C  K  K   D   D  A   A  Y   Y  S      B   B   *~
            *  BBBB   C      KKK    D   D  AAAAA   YYY    SSS   BBBB    *~
            *  B   B  C   C  K  K   D   D  A   A    Y        S  B   B   *~
            *  BBBB    CCC   K   K  DDDD   A   A    Y     SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKDAYSB - This subroutine determines if an S. O. should  *~
            *            be placed on credit hold based on the number of*~
            *            days contained in the CUSTOMER file vs. the #  *~
            *            of days old the unpaid invoices are.           *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/22/88 ! Original                                 ! JIM *~
            * 04/20/89 ! Changed POS of Credit # of days          ! JDH *~
            * 04/06/90 ! Continue reading file to see if paid.    ! JDH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "BCKDAYSB" (billto$,         /* Bill-to Customer code      */~
                       ret%)             /* zero=OK; 1=past due/cr hold*/

        dim                                                              ~
            billto$9,                    /* Bill-to Customer code      */~
            date$6,                      /* Today's date unformatted   */~
            netdate$6,                   /* ARMTRIAL Document Date     */~
            plowkey$21                   /* Key to ARMTRIAL            */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

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
            * #01 ! CUSTOMER ! Customer Master File                     *~
            * #02 ! ARMTRIAL ! Accounts Receivable Trial Balance        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #02, "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  21

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            if beenherebefore% <> 0% then goto L10000
                beenherebefore% = 1%
                call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
                call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
                date$ = date

L10000: REM *************************************************************~
            *         M A I N   P R O G R A M   L O G I C               *~
            *************************************************************

            ret% = 0% /* Initialize the return code to 'OK' */
        REM If there's no Bill-to customer, put S.O. on credit hold.
            call "READ100" (#01, billto$, f1%(1))
                if f1%(1) = 0% then goto error_out /* Shouldn't happen */
            get #01 using L10084, nbrdays% /* Cr # days this Bill-to */
L10084:         FMT POS(1058), BI(2)
            if nbrdays% = 0% then goto L65000

            billto_total = 0
            plowkey$ = str(billto$) & hex(00)
              /* Find Bill-to's invoices */
L10120:     call "PLOWNEXT" (#02, plowkey$, 9%, f1%(2))
                if f1%(2) = 0% then L65000 /* All done - no Cr hold */
            get #02 using L10142, netdate$, tranamt
L10142:         FMT POS(37), CH(6), POS(68), PD(14, 4)
            goto L10200

L10150:     call "PLOWNEXT" (#02, plowkey$, 19%, f1%(2))
                if f1%(2) = 0% then L10220 /* Done with this settlement */
            get #02 using L10190, tranamt
L10190:         FMT POS(68), PD(14, 4)
L10200:     billto_total = billto_total + tranamt
            goto L10150 /* Go get another one */

L10220:     if billto_total > .01 then goto L15000
            billto_total = 0
            goto L10120

L15000: REM We now have an unpaid invoice for this Bill-to Customer.
            call "DATE" addr ("G-", netdate$, date$, diff%, misc%)
            if misc% <> 0% then goto error_out /* Bad date(s)- error */
            billto_total = 0
            if diff% <= nbrdays% then goto L10120

        error_out /* S.O. will be on credit hold ... some error or ... */
            ret% = 1% /* Invoice is older than permitted this customer */

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

            end
