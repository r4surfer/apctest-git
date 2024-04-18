        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  BBBB    CCC   K   K  M   M  IIIII  N   N   SSS   BBBB    *~
            *  B   B  C   C  K  K   MM MM    I    NN  N  S      B   B   *~
            *  BBBB   C      KKK    M M M    I    N N N   SSS   BBBB    *~
            *  B   B  C   C  K  K   M   M    I    N  NN      S  B   B   *~
            *  BBBB    CCC   K   K  M   M  IIIII  N   N   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKMINSB - This subroutine validates the order quantity   *~
            *            entered by the user against the Minimum Sales  *~
            *            Order Quantity and Minimum Sales Order Incre-  *~
            *            ment from the HNYMASTR file. It calls ASK-     *~
            *            USER for verification if there are violations. *~
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
            * 02/17/89 ! Original                                 ! JIM *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "BCKMINSB" (orderqty, minsoqty, minsoinc, return%)

        dim                                                              ~
            msg$79,                                                      ~
            temp1$10, temp2$10

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.00 01/19/90 CMS2 / CMS-I Merge              "
        REM *************************************************************

        REM *************************************************************~
            *       M A I N   S U B R O U T I N E   L O G I C           *~
            *************************************************************

            return% = 16% /* Set Return Code to 'continue' */
*        Test for uninitialized (blank) HNYMASTR Minimum fields.
            if minsoqty > 10e9 or minsoinc > 10e9 then goto L65000
            msg$ = "Order Qty ="
            init (" ") temp1$, temp2$ /* 'No problem' */
            temp1, temp2 = 0
            if orderqty = 0 then goto L65000 /* No validations */
            if minsoqty = 0 then goto L10220 /* No Min SO Quantity */
            if orderqty >= minsoqty then goto L10220
                if minsoinc = 0 then goto L10170 /* Order Qty < Min Qty */
                    temp2 = max(minsoqty, max(1, int(minsoqty /          ~
                        minsoinc)) * minsoinc)
                    goto L10280
L10170:         call "CONVERT" (minsoqty, -0.4, temp1$)
                msg$ = msg$ & " " & temp1$ & " or more to satisfy minim"&~
                    "um."
                goto askuser_out

L10220
*        Order Qty => Min Qty; test for acceptable increment values.
            if minsoinc = 0 then goto L65000 /* No Min SO Increment */
            if mod(orderqty, minsoinc) = 0 then goto L65000  /* OK */
            temp1 = int(orderqty / minsoinc) * minsoinc /* Not an incr */
            temp2 = temp1 + minsoinc

L10280:     if temp1 >= minsoqty and temp1 <> 0                          ~
                then call "CONVERT" (temp1, -0.4, temp1$)
            call "CONVERT" (temp2, -0.4, temp2$)
            if temp1$ <> " "                                             ~
                then msg$ = msg$ & " " & temp1$ & " or " & temp2$        ~
                else msg$ = msg$ & " " & temp2$
            msg$ = msg$ & " to satisfy minimum and increment."

        askuser_out
            call "CONVERT" (minsoqty, -0.4, temp1$)
            call "CONVERT" (minsoinc, -0.4, temp2$)
L10390:     return% = 2% /* Window at bottom */
            call "ASKUSER" (return%, "*** ORDER QUANTITY WARNING ***",   ~
                "Part Minimum Quantity = " & temp1$ & "; Increment = " & ~
                temp2$ & ".", msg$, "Press PF(1) to reenter Order Quant"&~
                "ity; PF(16) to accept Quantity as is.")
            if return%  =  1% then goto L65000
            if return% <> 16% then goto L10390

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
