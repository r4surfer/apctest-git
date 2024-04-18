        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCVENLB                             *~
            *  Creation Date     - 03/06/97                             *~
            *  Last Modified Date- 11/13/97  djd                        *~
            *  Written By        - J. Browning Fields                   *~
            *                                                           *~
            *  Description       - Print New Vendor Mailling Labels.    *~
            *                                                           *~
            *                    - 'STUFF%' is Set to the Number of     *~
            *                               Dummy Labels needed at the  *~
            *                               Beginning and End of the    *~
            *                               Label Print run.            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/06/97 ! New Program for (APC) - Last Mod Date    ! JBF *~
            *          !                                          !     *~
            * 11/13/97 ! Changed Program Version ID To 60403      ! DJD *~
            *          !                                          !     *~
            *************************************************************

        sub "APCVENLB" (vendor_no$, vendor_name$, vendor_desc$,          ~
                        vendor_add$())

        dim                                                              ~
            vendor_no$9,                 /* Vendor No.                 */~
            vendor_name$30,              /* Vendor Name                */~
            vendor_desc$30,              /* Vendor Description         */~
            vendor_add$(5%)30,           /* Vendor Billing Address(5)  */~
            vtarr2$9,                                                    ~
            vtarr3$30,                                                   ~
            vtarr4$30,                                                   ~
            vtarr5$30,                                                   ~
            vtarr6$30,                                                   ~
            vtarr7$30,                                                   ~
            vtarr8$30,                                                   ~
            vtarr9$30,                                                   ~
            rslt$24                      /* Control File Definition    */

            str(rslt$, 1%,6%) = "OUTPUT"   /* Set So File Cannot Exist */
            str(rslt$, 7%,8%) = "00005000" /* Create for ? No. Records */
            str(rslt$,15%,3%) = "100"      /* Dpack Percentage         */
            str(rslt$,18%,3%) = "100"      /* Ipack Percentage         */

        REM *************************************************************~
            *                 I N I T I A L I Z E                       *~
            *************************************************************
        init(" ") vtarr2$, vtarr3$, vtarr4$, vtarr5$, vtarr6$, vtarr7$,  ~
                  vtarr8$, vtarr9$
        if beenherebefore%  = 1%         then create_label

        REM CREATE_OPEN                  /* Only do the 1st Time      */
            select #1, "APCVENLB", consec, recsize = 240
            call "OPENFILE" (#1, "OUTPT", 0%, rslt$, " " )

            beenherebefore% = 1%
            stuff%          = 1%
            gosub stuff_label            /* Create Dummy Label Begin   */

        REM *************************************************************~
            *           P R O C E S S   D A T A                         *~
            *************************************************************
        create_label
            vtarr2$         = vendor_no$
            vtarr3$         = vendor_name$
            vtarr4$         = vendor_desc$
            vtarr5$         = vendor_add$(1%)
            vtarr6$         = vendor_add$(2%)
            vtarr7$         = vendor_add$(3%)
            vtarr8$         = vendor_add$(4%)
            vtarr9$         = vendor_add$(5%)
            if vendor_no$  <> "END LABEL"  then create_print
            stuff%          = 1%
            gosub stuff_label            /* Create Dummy Label End     */

            close #1

            beenherebefore% = 0%
            goto exit_sub

        create_print
            gosub print_label

            goto exit_sub

        stuff_label
            for kk% = 1% to stuff%
                vtarr2$ = "000000000"                   /* Vendor No.  */
                vtarr3$ = "XXXXXXXXXXXXXXXXXXXX"        /* Vendor Name */
                vtarr4$ = "XXXXXXXXXXXXXXXXXXXX"        /* Vendor Desc */
                vtarr5$ = "XXXXXXXXXXXXXXXXXXXX"        /* Vendor Add1 */
                vtarr6$ = "XXXXXXXXXXXXXXXXXXXX"        /* Vendor Add2 */
                vtarr7$ = "XXXXXXXXXXXXXXXXXXXX"        /* Vendor Add3 */
                vtarr8$ = "XXXXXXXXXXXXXXXXXXXX"        /* Vendor Add4 */
                vtarr9$ = "XXXXXXXXXXXXXXXXXXXX"        /* Vendor Add5 */
                gosub print_label

            next kk%

        return

        REM *************************************************************~
            *             L A B E L   S U B R O U T I N E S             *~
            *************************************************************
        print_label
            put #1,using L35030,     "{1``",        /* Label Default( 4)*/~
                   vtarr2$,         "`",           /* Vendor No.   (10)*/~
                   vtarr3$,         "`",           /* Vendor Name  (31)*/~
                   vtarr4$,         "`",           /* Vendor Desc  (31)*/~
                   vtarr5$,         "`",           /* Vendor Addr1 (31)*/~
                   vtarr6$,         "`",           /* Vendor Addr2 (31)*/~
                   vtarr7$,         "`",           /* Vendor Addr3 (31)*/~
                   vtarr8$,         "`",           /* Vendor Addr4 (31)*/~
                   vtarr9$,         "`",           /* Vendor Addr5 (31)*/~
                                    "}"            /* End of Label ( 1)*/

            write #1, eod goto error_sub

        return

        error_sub
           call "SHOSTAT" ("Error With Label  ("&vtarr2$&")")
           stop
        return

        REM *************************************************************~
            *             F O R M A T   S T A T E M E N T               *~
            *************************************************************
L35030:     FMT CH(04),                            /* Label Default( 4)*/~
                CH(09), CH(1),                     /* Vendor No.   (10)*/~
                CH(30), CH(1),                     /* Vendor Name  (31)*/~
                CH(30), CH(1),                     /* Vendor Desc  (31)*/~
                CH(30), CH(1),                     /* Vendor Addr1 (31)*/~
                CH(30), CH(1),                     /* Vendor Addr2 (31)*/~
                CH(30), CH(1),                     /* Vendor Addr3 (31)*/~
                CH(30), CH(1),                     /* Vendor Addr4 (31)*/~
                CH(30), CH(1),                     /* Vendor Addr5 (31)*/~
                CH(01)                             /* End of Label ( 1)*/~

        REM *************************************************************~
            *             E X I T   S U B R O U T I N E                 *~
            *************************************************************
        exit_sub
        end
