        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   U   U   SSS   TTTTT   CCC    CCC   RRRR           *~
            *  C   C  U   U  S        T    C   C  C   C  R   R          *~
            *  C      U   U   SSS     T    C      C      R R R          *~
            *  C   C  U   U      S    T    C   C  C   C  R  R           *~
            *   CCC    UUU    SSS     T     CCC    CCC   R   R          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CUSTCCR  - Return CCRMASTR Information For A Customer.    *~
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
            * 12/11/03 ! Original                                 ! CMG *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "CUSTCCR"    (acct$,         /* Customer Code - IN         */~
                          openar$,       /* Customer Open AR - OUT     */~
                          openat$)       /* Customer Open Bill To - OUT*/ 
                                         /* Return Code                */
                                       


        dim                                                              ~
            acct$9,                      /* Acct # for test/descr rtn  */~
            date$8,                      /* Date for screen display    */~
            openar$14,                   /* Customer Open AR Amount    */~
            openat$14                    /* Customer Open Bill To Amt  */

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
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
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
            * #1  ! CCRMASTR ! Customer Credit Master file              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1 , "CCRMASTR",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =   9

            call "OPENCHCK" (#1,  fs%( 1), f2%( 1), 0%, rslt$( 1))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date

            openbill, opensh, openar, openhigh = 0.00

            call "READ100" (#1, acct$, f1%(1))
               if f1%(1) = 0% then goto exit_sub
            get #1 using L09240, openbill, opensh, openar, openhigh
L09240:         FMT POS(114), 4*PD(15,4)

            convert openbill to openat$, pic(-########.####)

            convert openar to openar$,   pic(-########.####)

            exit_sub
            end




