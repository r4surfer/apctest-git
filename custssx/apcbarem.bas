        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCBAREM                             *~
            *  Creation Date     - 08/02/96                             *~
            *  Last Modified Date- 11/17/97                             *~
            *  Written By        - Roy H. Hoffman                       *~
            *                                                           *~
            *  Description       - Print New Employee Barcode Labels.   *~
            *                                                           *~
            *                    - 'STUFF%' is Set to the Number of     *~
            *                               Dummy Labels needed at the  *~
            *                               Beginning and End of the    *~
            *                               Label Print run.            *~
            *        Note        - Use by (APCEMPMN) to Print Employee  *~
            *                      Barcode Labels.                      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/02/96 ! New Program for (APC) - Last Mod Date    ! JBF *~
            * 11/17/97 ! Mof for Upgrade to R6.04.03              ! RHH *~
            *************************************************************

        sub "APCBAREM" (empno$,e_lname$, e_fname$, e_init$)

        dim                                                              ~
            empno$5,                     /* Employee Number/Barcode    */~
            e_lname$15,                  /* Employee Last  Name        */~
            e_fname$10,                  /* Employee First Name        */~
            e_init$1,                    /* Employee Middle Initial    */~
            rslt$24,                     /* Control File Definition    */~
            a1$5, a2$15, a3$10, a4$1, a5$5

            str(rslt$, 1%,6%) = "OUTPUT"   /* Set So File Cannot Exist */
            str(rslt$, 7%,8%) = "00005000" /* Create for ? No. Records */
            str(rslt$,15%,3%) = "100"      /* Dpack Percentage         */
            str(rslt$,18%,3%) = "100"      /* Ipack Percentage         */

        REM *************************************************************~
            *                 I N I T I A L I Z E                       *~
            *************************************************************
        init(" ") a1$, a2$, a3$, a4$, a5$
        if beenherebefore%  = 1%         then create_label

        REM CREATE_OPEN                  /* Only do the 1st Time      */
            select #1, "APCBAREM", consec, recsize = 46
            call "OPENFILE" (#1, "OUTPT", 0%, rslt$, " " )

            beenherebefore% = 1%
            stuff%          = 1%
            gosub stuff_label            /* Create Dummy Label Begin   */

        REM *************************************************************~
            *           P R O C E S S   D A T A                         *~
            *************************************************************
        create_label
            a1$             = empno$
            a2$             = e_lname$
            a3$             = e_fname$
            a4$             = e_init$
            a5$             = empno$
            if empno$      <> "E O F"    then create_print
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
                a1$ = "A0000"                           /* Employee No.*/
                a2$ = "XXXXXXXXXXXXXXX"                 /* Last  Name  */
                a3$ = "XXXXXXXXXX"                      /* First Name  */
                a4$ = "X"                               /* Mid. Initial*/
                a5$ = "A0000"                           /* Barcode     */
                gosub print_label

            next kk%

        return

        REM *************************************************************~
            *             L A B E L   S U B R O U T I N E S             *~
            *************************************************************
        print_label
            put #1,using L01120 ,     "{1``",        /* Label Default( 4)*/~
                               a1$, "`",           /* Employee No. ( 6)*/~
                               a3$, "`",           /* First Name   (11)*/~
                               a4$, "`",           /* Mid. Initial ( 2)*/~
                               a2$, "`",           /* Last  Name   (16)*/~
                               a5$, "`",           /* Barcode      ( 6)*/~
                                    "}"            /* End of Label ( 1)*/

            write #1, eod goto error_sub

        return

        error_sub
           call "SHOSTAT" ("Error-With Barcode ("&empno$&")")
           stop
        return

        REM *************************************************************~
            *             F O R M A T   S T A T E M E N T               *~
            *************************************************************
L01120:     FMT CH(04),                            /* Label Default( 4)*/~
                CH(05), CH(1),                     /* Employee No. ( 6)*/~
                CH(10), CH(1),                     /* First Name   (11)*/~
                CH(01), CH(1),                     /* Mid. Initial ( 2)*/~
                CH(15), CH(1),                     /* Last  Name   (16)*/~
                CH(05), CH(1),                     /* Barcode      ( 6)*/~
                CH(01)                             /* End of Label ( 1)*/~

        REM *************************************************************~
            *             E X I T   S U B R O U T I N E                 *~
            *************************************************************
        exit_sub
        end
