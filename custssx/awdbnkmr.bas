        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   W   W  DDDD   BBBB   N   N  K    K M   M  RRRR    *~
            *  A   A  W   W  D   D  B   B  NN  N  K   K  MM MM  R   R   *~
            *  AAAAA  W W W  D   D  BBBB   N N N  KKKK   M M M  RRRR    *~
            *  A   A  WW WW  D   D  B   B  N  NN  K   K  M   M  R   R   *~
            *  A   A  W   W  DDDD   BBBB   N   N  K    K M   M  R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * AWDBNKMR - Sales Order Booking Master Data                *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/11/03 ! Original                                 ! CMG *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "AWDBNKMR"   (master$(),     /* Master Transaction Record  */~
                          adjrsn$,       /* Adjustment Reason Flag     */~
                          trans_number$, /* Transaction Number         */~
                          date$,         /* Transaction Date           */~
                          time$,         /* Transaction Time           */~
                          #1 , #2,       /* GENCODES, BNKMASTR         */~
                          bck_err%  )    /* Error Number               */

        dim                                                              ~
            master$(5)200,               /* BCKMASTR Record and Input  */~
            adjrsn$9,                    /* Adjustment Reason Flag     */~
            readkey$100,                 /* Misc Use Read Key          */~
            trans_number$16,             /* Transaction Number         */~
            date$6,                      /* Current Date               */~
            time$6,                      /* Current Time               */~
            record_type$1                /* Record Type                */

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
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * #1  ! GENCODES ! Master Code Table Files                  *~
            * #2  ! BNKMASTR ! New Sales Booking Master File            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            
            gosub initialize_variables
            gosub update_master


            goto exit_program



            initialize_variables
                init(" ") readkey$
                bck_err% = 0%
            return


            update_master
                if adjrsn$ = "N" then record_type$ = "1"
                if adjrsn$ = "R" then record_type$ = "2"
                if adjrsn$ = "D" then record_type$ = "3"


                if adjrsn$ = "N" then gosub update_new                   ~
                   else gosub update_resubmit

            return

            update_new

                init(" ") readkey$
                str(readkey$,1%,16%) = trans_number$

                read #2, hold, key = readkey$, eod goto no_new_trans
                      delete #2
            no_new_trans

                put #2, using L00200, trans_number$,                      ~
                                      record_type$,                       ~
                                      date$,                              ~
                                      time$,                              ~
                                      " ",                                ~
                                      master$()

                write #2


L00200:               FMT CH(16), CH(1), CH(6), CH(6), CH(1), 5*CH(200)
            return


            update_resubmit

                init(" ") readkey$
                str(readkey$,1%,16%) = trans_number$

                read #2, hold, key = readkey$, eod goto no_res_trans
                      delete #2
            no_res_trans
                gosub change_qty
                put #2, using L00200, trans_number$,                      ~
                                      record_type$,                       ~
                                      date$,                              ~
                                      time$,                              ~
                                      " ",                                ~
                                      master$()

                write #2

            return


            change_qty

                grossopn = 0.00
                get str(master$()) using L00500, grossopn

L00500:         FMT POS(867), PD(14,4)

                grossopn = -grossopn
                put str(master$()) using L00500, grossopn


            return


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

            end
