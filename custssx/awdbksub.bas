        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   W   W  DDDD   BBBB   K    K  SSS   U   U   BBBB   *~
            *  A   A  W   W  D   D  B   B  K   K  S      U   U   B   B  *~
            *  AAAAA  W W W  D   D  BBBB   KKKK   SSSS   U   U   BBBB   *~
            *  A   A  WW WW  D   D  B   B  K   K      S  U   U   B   B  *~
            *  A   A  W   W  DDDD   BBBB   K    K  SSS    UUU    BBBB   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * AWDBKSUB - New Sub Part File By Sales Order               *~
            *   Error  - 0      Okay                                    *~
            *            1      Can not update File                     *~
            *            2      Can not find data                       *~
            *            3      Can not update Invoice                  *~
            *            99     Starting Error Number                   *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/12/05 ! Original                                 ! CMG *~
            * 04/24/06 ! (AWD001) Mod to autofill with Zero       ! CMG *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "AWDBKSUB"   (flag$,         /* Flag 0=SalesOrder 1=Invoice*/~
                          pgm$,          /* Calling Program 0=BCKUPDTE */~
                                         /* 1=Any Other 2=Delete       */~
                                         /* 3=Invoice                  */~
                          so_inv$,       /* SO or Invoice Num to lookup*/~
                          item_no$,      /* Item Number                */~
                          bcksubpt_rec$, /* Record If BCKUPDTE then    */~
                                         /* pass in else pass out      */~
                          flds$(),       /* Part Number Fields         */~
                          info_flds$(),  /* Information Fields         */~
                          #1,            /* BCKSUBPT File              */~
                          err%)          /* Error Code                 */

        dim                                                              ~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_key$11,             /* BCKSUBPT Read Key          */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4             /* Additional Info Fields     */
             


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
            * #1  ! BCKSUBPT ! New Sub Part Number File                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************


            gosub initialize_variables
            if pgm$ = "0" or pgm$ = "2" then gosub create_bcksubpt

            if pgm$ = "1" then gosub lookup_bcksubpt
            if pgm$ = "1" then gosub lookup_fields
 

            goto exit_program



            initialize_variables
                init(" ") bcksubpt_key$
                str(bcksubpt_key$,1%,8%) = so_inv$
                str(bcksubpt_key$,9%,3%) = item_no$

                err% = 99% 
                key% = 0%

                if flag$ = "1" then key% = 1%
                if pgm$  = "0" then return

                init(" ") bcksubpt_rec$, flds$(), info_flds$()
            return


            create_bcksubpt
                gosub check_exists
                if pgm$ = "2" then return

                write #1, using L00100, bcksubpt_rec$, eod goto update_err

L00100:             FMT CH(256)

                err% = 0%
            return
            update_err
                  err% = 1%
            return 



            check_exists


                read #1, hold, key key% = bcksubpt_key$, eod goto no_bcksubpt

                              delete #1                           

            no_bcksubpt
            return



            lookup_bcksubpt


                read #1, key key% = bcksubpt_key$, eod goto no_bcksubpt_lookup
                
                     get #1, using L00100, bcksubpt_rec$

                err% = 0%
            return
            no_bcksubpt_lookup
               err% = 2%
            return

            lookup_fields
                  flds$(1%)  = str(bcksubpt_rec$,23%,3%)   /* Model  */
                  flds$(2%)  = str(bcksubpt_rec$,26%,1%)   /* Color  */
                  flds$(3%)  = str(bcksubpt_rec$,27%,2%)   /* Glass  */
                  flds$(4%)  = str(bcksubpt_rec$,29%,2%)   /* Liting */
                  flds$(5%)  = str(bcksubpt_rec$,31%,2%)   /* Hinge  */
                  flds$(6%)  = str(bcksubpt_rec$,33%,1%)   /* Screen */
                  flds$(7%)  = str(bcksubpt_rec$,34%,1%)   /* Locks  */
                  flds$(8%)  = str(bcksubpt_rec$,35%,4%)   /* Width  */
                  flds$(9%)  = str(bcksubpt_rec$,39%,3%)   /* Height */
                  flds$(10%) = str(bcksubpt_rec$,42%,3%)   /* CLMR   */
                  flds$(11%) = str(bcksubpt_rec$,45%,3%)   /* WallDe */


                  flds$(12%) = str(bcksubpt_rec$,48%,1%)   /* GRDTYPE*/
                  flds$(13%) = str(bcksubpt_rec$,49%,1%)   /* GRDSIZE*/
                  flds$(14%) = str(bcksubpt_rec$,50%,1%)   /* GRDCOLO*/
                  flds$(15%) = str(bcksubpt_rec$,51%,1%)   /* HARDWAR*/
                  flds$(16%) = str(bcksubpt_rec$,52%,1%)   /* FOAM   */
                  flds$(17%) = str(bcksubpt_rec$,53%,1%)   /* Casing */

                  flds$(18%) = str(bcksubpt_rec$,54%,1%)   /* AVAIL  */
                  flds$(19%) = str(bcksubpt_rec$,55%,1%)   /* AVAIL  */
                  flds$(20%) = str(bcksubpt_rec$,56%,1%)   /* AVAIL  */
                  flds$(21%) = str(bcksubpt_rec$,57%,1%)   /* AVAIL  */
                  flds$(22%) = str(bcksubpt_rec$,58%,1%)   /* AVAIL  */
                  flds$(23%) = str(bcksubpt_rec$,59%,1%)   /* AVAIL  */
                  flds$(24%) = str(bcksubpt_rec$,60%,1%)   /* AVAIL  */
                  flds$(25%) = str(bcksubpt_rec$,61%,1%)   /* AVAIL  */
                  flds$(26%) = str(bcksubpt_rec$,62%,1%)   /* AVAIL  */
                  flds$(27%) = str(bcksubpt_rec$,63%,1%)   /* AVAIL  */
                  flds$(28%) = str(bcksubpt_rec$,64%,1%)   /* AVAIL  */
                  flds$(29%) = str(bcksubpt_rec$,65%,1%)   /* AVAIL  */
                  flds$(30%) = str(bcksubpt_rec$,66%,1%)   /* AVAIL  */
                  flds$(31%) = str(bcksubpt_rec$,67%,1%)   /* AVAIL  */

                  info_flds$(1%)  = str(bcksubpt_rec$,132%,1%)   /* INTCOL */
                  info_flds$(2%)  = str(bcksubpt_rec$,133%,1%)   /* EXTCOL */
                  info_flds$(3%)  = str(bcksubpt_rec$,134%,1%)   /* SEAT   */
                  info_flds$(4%)  = str(bcksubpt_rec$,135%,1%)   /* FIN    */
                  info_flds$(5%)  = str(bcksubpt_rec$,136%,1%)   /* PlntInv*/
                  info_flds$(6%)  = str(bcksubpt_rec$,137%,1%)   /* PlntDef*/
                  info_flds$(7%)  = str(bcksubpt_rec$,138%,1%)   /* PlntAct*/

                  info_flds$(8%)  = str(bcksubpt_rec$,139%,2%)   /* Brand  */

                  info_flds$(9%)  = str(bcksubpt_rec$,141%,1%)   /* Avail  */ 
                  info_flds$(10%) = str(bcksubpt_rec$,142%,1%)   /* Avail  */ 
                  info_flds$(11%) = str(bcksubpt_rec$,143%,1%)   /* Avail  */
                  info_flds$(12%) = str(bcksubpt_rec$,144%,1%)   /* Avail  */
                  info_flds$(13%) = str(bcksubpt_rec$,145%,1%)   /* Avail  */
                  info_flds$(14%) = str(bcksubpt_rec$,146%,1%)   /* Avail  */
                  info_flds$(15%) = str(bcksubpt_rec$,147%,1%)   /* Avail  */
                  info_flds$(16%) = str(bcksubpt_rec$,148%,1%)   /* Avail  */
                  info_flds$(17%) = str(bcksubpt_rec$,149%,1%)   /* Avail  */
                  info_flds$(18%) = str(bcksubpt_rec$,150%,1%)   /* Avail  */
                  info_flds$(19%) = str(bcksubpt_rec$,151%,1%)   /* Avail  */

                  gosub auto_fill                  /* (AWD001) */
            return


/* (AWD001) */
            auto_fill

                  for i% = 1% to 20%
                     if flds$(i%) =  " " then             ~ 
                           flds$(i%) = "0"
                     if info_flds$(i%) =  " " then        ~ 
                           info_flds$(i%) = "0"

                     if str(bcksubpt_rec$,(47%+i%),1%) = " " then ~
                           str(bcksubpt_rec$,(47%+i%),1%) = "0"

                     if str(bcksubpt_rec$,(131%+i%),1%) = " " then ~
                           str(bcksubpt_rec$,(131%+i%),1%) = "0"

                  next i%

            return
/* (AWD001) */

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




