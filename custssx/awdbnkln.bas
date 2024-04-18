        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   W   W  DDDD   BBBB   N   N  K    K L      N   N   *~
            *  A   A  W   W  D   D  B   B  NN  N  K   K  L      NN  N   *~
            *  AAAAA  W W W  D   D  BBBB   N N N  KKKK   L      N N N   *~
            *  A   A  WW WW  D   D  B   B  N  NN  K   K  L      N  NN   *~
            *  A   A  W   W  DDDD   BBBB   N   N  K    K LLLLL  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * AWDBNKLN - Sales Order Booking Detail Data                *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/11/03 ! Original                                 ! CMG *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "AWDBNKLN"   (detail$(),     /* Detail Transaction Record  */~
                          adjrsn$,       /* Adjustment Reason Flag     */~
                          trans_number$, /* Transaction Number         */~
                          date$,         /* Transaction Date           */~
                          time$,         /* Transaction Time           */~
                          #1 , #2,       /* GENCODES, BNKLINES         */~
                          bck_err%  )    /* Error Number               */

        dim                                                              ~
            detail$(2)150,               /* BCKLINES Record and Input  */~
            adjrsn$9,                    /* Adjustment Reason Flag     */~
            readkey$100,                 /* Misc Use Read Key          */~
            desc$30,                     /* Description Field          */~
            trans_number$16,              /* Transaction Number         */~
            date$6,                      /* Current Date               */~
            time$6                       /* Current Time               */

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
            * #2  ! BNKLINES ! New Sales Booking Detail File            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

*      Only Do Once!!!!!!
            if date$ = " " then date$ = date
            if time$ = " " then time$ = time
            if trans_number$ = " " then gosub get_next_trans

            gosub initialize_variables
            gosub update_detail


            goto exit_program



            initialize_variables
                init(" ") readkey$, desc$
                bck_err% = 0%
            return


            update_detail

                if adjrsn$ = "N" then gosub update_new                   ~
                   else gosub update_resubmit

            return


            get_next_trans
                init(" ") readkey$
                trans_number% = 0%
                str(readkey$,1%,9%) = "BCKBNKS"
                str(readkey$,10%,3%) = "NNN"

                read #1, hold, key = readkey$, eod goto no_number
                       get #1, using L00100, desc$

L00100:              FMT POS(25), CH(30)
                convert str(desc$,1%,16%) to trans_number%, data goto bad_number

                trans_number% = trans_number% + 1%                     
                convert trans_number% to trans_number$, pic(0000000000000000)

                       put #1, using L00100, trans_number$
                       rewrite #1
                 
            return
            no_number
               bck_err% = 1%
            return
            bad_number
               bck_err% = 2%
            return

            update_new

                init(" ") readkey$
                str(readkey$,1%,16%) = trans_number$
                str(readkey$,17%,3%) = str(detail$(),26%,3%)

                read #2, hold, key = readkey$, eod goto no_new_trans
                      delete #2
            no_new_trans

                put #2, using L00200, trans_number$,                      ~
                                      str(detail$(),26%,3%),              ~
                                      date$,                              ~
                                      time$,                              ~
                                      " ",                                ~
                                      detail$()

                write #2

L00200:               FMT CH(16), CH(3), CH(6), CH(6), CH(5), 2*CH(150)
            return


            update_resubmit

                init(" ") readkey$
                str(readkey$,1%,16%) = trans_number$
                str(readkey$,17%,3%) = str(detail$(),26%,3%)

                read #2, hold, key = readkey$, eod goto no_res_trans
                      delete #2
            no_res_trans
                gosub change_qty

                put #2, using L00200, trans_number$,                      ~
                                      str(detail$(),26%,3%),              ~
                                      date$,                              ~
                                      time$,                              ~
                                      " ",                                ~
                                      detail$()

                write #2

            return

            change_qty

                orderqty = 0.00
                get str(detail$()) using L00500, orderqty

L00500:         FMT POS( 93), PD(14,4)

                orderqty = -orderqty
                put str(detail$()) using L00500, orderqty     


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
