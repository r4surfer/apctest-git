        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  BBBB    CCC   K   K   CCC   RRRR   DDDD    SSS   BBBB    *~
            *  B   B  C   C  K  K   C   C  R   R  D   D  S      B   B   *~
            *  BBBB   C      KKK    C      RRRR   D   D   SSS   BBBB    *~
            *  B   B  C   C  K  K   C   C  R   R  D   D      S  B   B   *~
            *  BBBB    CCC   K   K   CCC   R   R  DDDD    SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKCRDSB - This subroutine receives the requisite infor-  *~
            *            mation from the caller and computes the total  *~
            *            A/R exposure for the customer, i.e., the total *~
            *            exposure for the customer's "credit parent" and*~
            *            all of the parent's children.                  *~
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
            * 09/12/88 ! Original                                 ! JIM *~
            * 11/11/88 ! Now returns Flag if Billto is a Cr Parent! JDH *~
            * 11/12/92 ! Cust Credit- Dynamic fields from CCRMASTR! JIM *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "BCKCRDSB" (bill_to$,        /* Bill-to Customer code      */~
                        oo(),            /* On Order amounts           */~
                        ar(),            /* Total A/R exposures        */~
                        cl,              /* Credit Limit of family     */~
                        par%)            /* Is this a Credit Parent    */

        dim                                                              ~
            ar(2), oo(2),                /* Bill-to & Family amounts   */~
            bill_to$9,                   /* Bill-to Customer code      */~
            parent$9,                    /* Customer's CR Parent code  */~
            temp_cus$9,                                                  ~
            temp_par$9

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! CUSTOMER ! Customer Master File                     *~
            * #02 ! CCRMASTR ! Customer Credit Master file              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01,  "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #02, "CCRMASTR",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =   9

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            if been_here_before% <> 0% then goto L10000
                f2%, been_here_before% = 1%
                call "OPENCHCK" (#01, 0%, f2%, f1%, " ")
                call "OPENCHCK" (#02, 0%, xx%, xx%, " ")

L10000: REM *************************************************************~
            *       M A I N   S U B R O U T I N E   L O O P             *~
            *************************************************************

            cl = 0 : mat oo = zer : mat ar = zer

*        First, read the Bill-to customer passed in to us. CUSTOMER 1st.
            call "READ100" (#01, bill_to$, f1%)
                if f1% = 0% then goto L65000            /* Customer NIF */
            get #01 using L10100, parent$
L10100:         FMT POS(1049), CH(9)
*        CCRMASTR 2nd.
            oo(1%), ar(1%) = 0                                  /* JIC */
            call "READ100" (#02, bill_to$, xx%)
            if xx% <> 0% then get #02 using L10150, oo(1%), ar(1%)
L10150:         FMT POS(114), PD(14,4), XX(8), PD(14,4)
            if parent$ <> " " then goto L10220

*        The passed customer has no credit parent -- he's his own parent.
                parent$ = bill_to$
                par% = 1%           /* Could be a Credit Parent */

L10220
*        Passed customer has a credit parent -- look up the parent AND
*        all associated children.
            call "READ100" (#01, parent$, f1%)
                if f1% = 0% then goto L65000 /* OOPS! Parent NIF */
            gosub get_credit_limit /* Else get parent's credit limit */
            gosub get_oo_and_ar /* & begin exposure accumulation */

*        Now PLOW through the children and accumulate OO & AR.
            call "REDALT0" (#01, parent$, 5%, f1%) /* Read 1st child */
                if f1% = 0% then par% = 0%  /* If no child; no parent*/
            goto L10370

L10340
*        All READs but the first are done here.
            call "READNEXT" (#01, f1%) /* Next guy on file */

L10370
*        The children-reading loop continues here.
            if f1% = 0% then goto L65000 /* Have we found a child? */
            get #01 using L10400, temp_cus$, temp_par$ /* Same parent? */
L10400:         FMT CH(9), POS(1049), CH(9)
            if temp_par$ <> parent$ then goto L65000 /* No? we're done */
            if temp_cus$ =  parent$ then goto L10340 /* Ignore parent */
            gosub get_oo_and_ar /* Else continue exposure accumulation */
            goto L10340

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

        get_credit_limit /* Get Credit Limit for caller */
            get #01 using L15060, cl
L15060:         FMT POS(526), PD(14,4)
            return

        get_oo_and_ar  /* Accumulate Bill-to Open Order & A/R Exposure */
            temp1, temp2 = 0                                    /* JIC */
            call "READ100" (#02, key(#01), xx%)            /* CCRMASTR */
            if xx% <> 0% then get #02 using L15130, temp1, temp2
L15130:         FMT POS(114), PD(14,4), XX(8), PD(14,4)
            oo(2) = oo(2) + temp1 /* Compute family Open Order */
            ar(2) = ar(2) + temp2 /* Compute family A/R exposure */
            return

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
