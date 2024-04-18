        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPL3AB                             *~
            *  Creation Date     - 06/20/96                             *~
            *  Last Modified Date- 11/14/97                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Build Display Script for the Status  *~
            *                      Code Tracking Information.           *~
            *                                                           *~
            *  Code Tables Used  - (PLAN STAT) - Status Tracking Codes  *~
            *                                                           *~
            *  Programs Used By  -  BCKFASTR - Sales Order Entry        *~
            *                       APCPLN05 - Sales Order Lookup       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/20/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            *          !                                          !     *~
            *          !                                          !     *~
            *************************************************************

            sub "APCPL3AB" (code$,       /* Table Code             -In */~
                            code_val%,   /* Convert Code Value     -Out*/~
                            table$,      /* Table Name - Gen Codes -In */~
                            d1$,         /* Short Description Left -Out*/~
                            d2$,         /* Short Description Right-Out*/~
                            d3$,         /* Long Description  (All)-Out*/~
                            #1 )         /* (GENCODES) - Table File    */

        dim code$4,                           /* Table Code Value      */~
            table$9,                          /* GENCODES - Table Name */~
            d1$15,                            /* Short Description Left*/~
            d2$15,                            /* Short Description Righ*/~
            d3$30,                            /* Long Description      */~
            gen_key$24                        /* GENCODES - Primary Key*/
            code_val% = 999%                         /* Invalid Value  */
            convert code$ to code_val%, data goto L00410
L00410:
            str(gen_key$,1%,9%)   = table$
            str(gen_key$,10%,15%) = code$
            read #1,key = gen_key$, using L00450  , d3$, eod goto exit_sub
L00450:        FMT POS(25), CH(30)
            p%  = pos(d3$ = "-")                     /* Find Location  */
            d1$ = str(d3$,1%,p%-1%)                  /* Left Side      */
            d2$ = str(d3$,p%+1%)                     /* Right Side     */
            if code_val% > 1% and table$ = "PLAN STAT" then              ~
               d1$ = "**" & str(d3$,1%,p%-1%)        /* Left Side S.O  */
        exit_sub
        end
