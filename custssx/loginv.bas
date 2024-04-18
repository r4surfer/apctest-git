        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPR2SB                             *~
            *  Creation Date     - 10/20/2009                           *~
            *  Last Modified Date-                                      *~
            *  Description       - Subroutine to create file log of     *~
            *                      processed invoices                   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *10/20/2009! New Program                              ! CMG *~
	    *************************************************************

            sub "LOGINV"   ( cuscode$,  /* Customer Code               */~
                             invnr$,    /* Invoice Number              */~
                             postdate$) /* PostDate                    */


       dim cuscode$9,                      /* CUSTOMER CODE            */~
           invnr$8,                        /* INVOICE NUMBER           */~
           postdate$6                      /* POST DATE                */



        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************

        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! ORAINV   ! Log to send data to DTS_ARIUPDTE         *~
            * # 2 ! SALESLOG ! Log to run EWDSLS00 create daily         *~
            *************************************************************~

            select #1,  "ORAINV",                                        ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    7, keylen =  18,                     ~
                       alt key   1, keypos =    1, keylen =  24       

            select #2,  "SALESLOG",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    7, keylen =  18,                     ~
                       alt key   1, keypos =    1, keylen =  24       


            if been_here_before% = 1% then L09052
                call "OPENCHCK" (#1, 0%, f2%, 100%, " ")
                call "OPENCHCK" (#2, 0%, f2%, 100%, " ")
                been_here_before% = 1%



L09052: REM *************************************************************~
            *                CREATE DATA                                *~
            *                                                           *~
            *************************************************************




           put #1, using orainv_fmt,              ~
                    date,         /* SYSTEM DATE */~
                    "S",          /* Tran type   */~
                    cuscode$,     /* CusCode     */~
                    invnr$,       /* Inv Number  */~
                    postdate$,    /* PostDate    */~
                    " "           /* Filler      */
 
           write #1, eod goto no_orainv

        no_orainv


           put #2, using orainv_fmt,              ~
                    date,         /* SYSTEM DATE */~
                    "S",          /* Tran type   */~
                    cuscode$,     /* CusCode     */~
                    invnr$,       /* Inv Number  */~
                    postdate$,    /* PostDate    */~
                    " "           /* Filler      */
 
           write #2, eod goto no_salelog

        no_salelog

REM End Subroutine

        end                  





orainv_fmt:          FMT CH(06), CH(01), CH(09), CH(08), CH(06), CH(218)





