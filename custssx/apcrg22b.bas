        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCRG22B (Like APCRGA2B)             *~
            *  Creation Date     - 01/21/97                             *~
            *  Last Modified Date- 11/18/97 DJD                         *~
            *  Written By        - J. Browning Fields                   *~
            *                                                           *~
            *  Description       - Print RGA Barcode Labels.            *~
            *                      Uses NEW Planning Files              *~
            *                                                           *~
            *                    - 'STUFF%' is Set to the Number of     *~
            *                               Dummy Labels needed at the  *~
            *                               Beginning and End of the    *~
            *                               Label Print run.            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/21/97 ! New Program for (APC) - Last Mod Date    ! JBF *~
            *          !                                          !     *~
            * 11/18/97 ! Revision For 60403 Update                ! DJD *~
            *          !                                          !     *~
            * 02/20/98 ! Y2K Conversions                          ! DJD *~
            *          !                                          !     *~
            * 12/20/18 ! CR1829 cust 6 to 9 char for Dallas       ! DES *~
            *************************************************************
        sub "APCRG22B" (r_number$, cus_desc$, r_cuscode$, part_desc$)

        dim                                                              ~
            r_number$9,                  /* RGA No./Item No.           */~
            cus_desc$30,                 /* Customer Description       */~
            r_cuscode$9,                 /* Customer Code              */~
            part_desc$45,                /* Part Description           */~
            rslt$24,                     /* Control File Definition    */~
            a1$9, a2$30, a3$9, a4$45, a5$9

            str(rslt$, 1%,6%) = "OUTPUT"   /* Set So File Cannot Exist */
            str(rslt$, 7%,8%) = "00005000" /* Create for ? No. Records */
            str(rslt$,15%,3%) = "100"      /* Dpack Percentage         */
            str(rslt$,18%,3%) = "100"      /* Ipack Percentage         */

        REM *************************************************************~
            *                 I N I T I A L I Z E                       *~
            *************************************************************
        init(" ") a1$, a2$, a3$, a4$, a5$
            stuff%          = 1%
        if beenherebefore%  = 1%         then create_label

        REM CREATE_OPEN                  /* Only do the 1st Time      */
            select #1, "APCBARRG", consec, recsize = 112
            call "OPENFILE" (#1, "OUTPT", 0%, rslt$, " " )

            beenherebefore% = 1%
            gosub stuff_label            /* Create Dummy Label Begin   */

        REM *************************************************************~
            *           P R O C E S S   D A T A                         *~
            *************************************************************
        create_label
            a1$             =  r_number$
            a2$             =  cus_desc$
            a3$             =  r_number$
            a4$             =  part_desc$
            a5$             =  r_cuscode$
            if r_number$   <> "E O F "   then create_print
            gosub stuff_label            /* Create Dummy Label End     */
            close #1

            beenherebefore% =  0%
            goto exit_sub

        create_print
            gosub print_label

            goto exit_sub

        stuff_label
            for kk% =  1% to stuff%
                a1$ = "999999999"                       /* RGA No./Item*/
                a2$ = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"  /* Cust. Name  */
                a3$ = "999999999"                       /* Barcode     */
                a4$ = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"  /* Part Descr. */
                a5$ = "XXXXXXXXX"                       /* Cust. Code  */
                gosub  print_label

            next kk%

        return

        REM *************************************************************~
            *             L A B E L   S U B R O U T I N E S             *~
            *************************************************************
        print_label
            put #1,using L35030,     "{1``",        /* Label Default( 4)*/~
                               a1$, "`",           /* RGA No./Item ( 7)*/~
                               a2$, "`",           /* Customer Name(31)*/~
                               a3$, "`",           /* Barcode      ( 7)*/~
                               a4$, "`",           /* Part Descr.  (46)*/~
                               a5$, "`",           /* Customer Code( 7)*/~
                                    "}"            /* End of Label ( 1)*/

            write #1, eod goto error_sub

        return

        error_sub
           call "SHOSTAT" ("Error-With Barcode ("&r_number$&")")
           stop
        return

        REM *************************************************************~
            *             F O R M A T   S T A T E M E N T               *~
            *************************************************************
L35030:     FMT CH(04),                            /* Label Default( 4)*/~
                CH(09), CH(1),                     /* RGA No./Item ( 7)*/~
                CH(30), CH(1),                     /* Cust. Name   (31)*/~
                CH(09), CH(1),                     /* Barcode      ( 7)*/~
                CH(45), CH(1),                     /* Part Descr.  (46)*/~
                CH(09), CH(1),                     /* Cust. Code   ( 7)*/~
                CH(01)                             /* End of Label ( 1)*/~

        REM *************************************************************~
            *             E X I T   S U B R O U T I N E                 *~
            *************************************************************
        exit_sub
        end
