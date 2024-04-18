        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   W   W  DDDD    OOO   RRRR    AAA    SSS    BBBB   *~
            *  A   A  W   W  D   D  O   O  R   R  A   A  S       B   B  *~
            *  AAAAA  W W W  D   D  O   O  RRRR   AAAAA   SSS    BBBB   *~
            *  A   A  WW WW  D   D  O   O  R   R  A   A      S   B   B  *~
            *  A   A  W   W  DDDD    OOO   R   R  A   A   SSS    BBBB   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * AWDORASB - Oracel Description File by Sales Order         *~
            *   Error  - 0      Okay                                    *~
            *            1      Can not update File                     *~
            *            2      Can not find data                       *~
            *            3      Can not update Invoice                  *~
            *            99     Starting Error Number                   *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *06/25/2015! (SR66200)Original                        ! CMG *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "AWDORASB"   (pgm$,          /* Calling Program 0=BCKUPDTE */~
                                         /* 1=Any Other 2=Delete       */~
                                         /* 3=Invoice                  */~
                          so_inv$,       /* SO or Invoice Num to lookup*/~
                          item_no$,      /* Item Number                */~
                          oradescr_rec$(), /* Record If BCKUPDTE then    */~
                                         /* pass in else pass out      */~
                          #1,            /* ORADESC2 File              */~
                          err%)          /* Error Code                 */

        dim                                                              ~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            oradescr_key$11,             /* ORADESC2 Read Key          */~
            oradescr_rec$(4%)256         /* ORADESC2 Record            */~
             


        dim f2%(32%),                    /* = 0 if the file is open    */~
            f1%(32%),                    /* = 1 if READ was successful */~
            fs%(32%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32%)20                 /* Text from file opening     */

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
            * #1  ! ORADESC2 ! Oracle Description File by Sale Order    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************


            gosub initialize_variables
            if pgm$ = "0" or pgm$ = "2" then gosub create_oradescr

            if pgm$ = "1" then gosub lookup_oradescr

            goto exit_program



            initialize_variables
               init(" ") oradescr_key$
               str(oradescr_key$,1%,8%) = so_inv$
               str(oradescr_key$,9%,3%) = item_no$

               err% = 99% 
               key% = 0%

               if pgm$  = "0" then return   /* Returns if add */

               init(" ") oradescr_rec$()
            return


            create_oradescr
               gosub check_exists
               if pgm$ = "2" then return

               write #1, using L00100, oradescr_rec$(), eod goto update_err

L00100:           FMT 4*CH(256)

               err% = 0%
            return
            update_err
                  err% = 1%
            return 



            check_exists


               read #1, hold, key = oradescr_key$, eod goto no_oradescr

                    delete #1                           

            no_oradescr
            return



            lookup_oradescr


               read #1, key key% = oradescr_key$, eod goto no_oradescr_lookup
                
                  get #1, using L00100, oradescr_rec$()

                err% = 0%
            return
            no_oradescr_lookup
               err% = 2%
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




