        REM *************************************************************~
            *                       As of 11/06/98                      *~
            *                 (Sales Orders that are Not Planned)       *~
            * APCPRYSB - 1.Look-Up Private label code in (BCKLINES) 1st *~
            *              if none found then obtain from customer 2nd  *~  
            *            2.Look-Up Customer and find out what Private   *~
            *              Label Code to Use. (CUSTOMER)                *~
            *            3.Using the Model (Input) and Private Label    *~
            *              Code Lookup Series name for Model.(ELLISON05)*~
            *              <Series> <Type> for Specified Model          *~
            *            4.Using the Private Label Code s_1$ find the   *~
            *               Private label name (ELLISON01)              *~
            *            5.Argument List       -  Input/Output          *~
            *              Customer Code       =  cussode$- Input       *~
            *              New Model Code      =  S_23M$  - Input       *~
            *              Sale Order          =  s_so$   - Input       *~
            *              Sales Order Line    =  s_ln$   - Input       *~
            *                                                           *~ 
            *              Private Label Name  =  s_prv$  - Output      *~
            *              private Label Code  =  s_1$    - Output      *~ 
            *              New Series Code     =  S_23$   - Output      *~
            *              Series Length       =  S_23%   - Output      *~
            *              Return Code         =  0% = Ok               *~
            *                                  =  1% = Series N/A       *~
            *************************************************************~
            * Programs Using Sub   =    EWDPLN58 - (P) Specials/Tempered*~
            *                           EWDPLA58 - (S) Barcode Display  *~
            *                           APCFAXSD - (S)                  *~
            *************************************************************

        sub "APCPRYSB" (cuscode$,        /* Customer Code      Input   */~
                        s_23m$,          /* Model Code         Input   */~
                        s_so$,           /* Sales Order Number Input   */~
                        s_ln$,           /* Sales Order Ln ItemInput   */~
                        s_prv$,          /* Private Label Name Output  */~
                        s_1$,            /* Private Label Code Output  */~   
                        s_23$,           /* Series Name        Output  */~
                        s_23%,           /* Length of Name     Output  */~
                        #1,              /* (CUSTOMER) - Customer Maste*/~
                        #2,              /* (GENCODES) - Code Tables   */~
                        #3,              /* (BCKLINES) - S.O. Detail   */~
                        x_er% )          /* Return Code                */

        dim                                                              ~
            readkey$24,                  /* GENCODES Key               */~
            cuscode$9,                   /* Customer Code              */~
            s_so$8,                      /* Sales Order Number         */~
            s_ln$3,                      /* Sales Order Line Item      */~
            s_prv$30,                    /* Private Label Name         */~ 
            s_1$2,                       /* Private Label Code         */~
            s_23$8, s_23m$3,             /* Series Name/Model Code     */~
            s_verify$5                   /* Use to Auto Verify         */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! CUSTOMER ! Customer Master File                     *~
            * #2  ! GENCODES ! Caelus Table Master File                 *~
            * #3  ! APCPLNDT ! Master Planning File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            x_er% = 0% : s_23% = 0% : s_ln% = 0%
            init(" ") readkey$, s_1$, s_23$, s_verify$, s_prv$
            if len(s_so$) < 5 then goto L00510    /* Use Customer Data */
               convert s_ln$ to s_ln%, data goto L00400
L00400:
               convert s_ln% to s_ln$, pic(###)   /* Zero Fill         */  
               str(readkey$,1%,16%) = s_so$       /* Sales Order       */
               str(readkey$,17%,3%) = s_ln$       /* Line Item         */
               read #3, key = readkey$, using L00500, readkey$, s_1$,    ~
                                                      eod goto L00510
L00500:           FMT POS(10), CH(19), POS(282), CH(2)
               if s_so$ <> str(readkey$,1%,8%) then goto L00510 
               if s_ln$ <> str(readkey$,17%,3%) then goto L00510 
                  s_1% = 0%
                  convert s_1$ to s_1%, data goto L00510
                                                  /* Found in BCKLINES */
                  goto L00530                  
 
L00510:     init(" ") readkey$, s_1$
            read #1,key = cuscode$, using L00520  , s_1$, eod goto L00600
L00520:        FMT POS(960), CH(2)
                                                  /* Found in Customer */
L00530:     str(s_verify$,1%,3%) = s_23m$         /* Model Code        */
            str(s_verify$,4%,2%) = s_1$           /* private Label Code*/ 
            str(readkey$,1%,9%)   = "ELLISON05"
            str(readkey$,10%,15%) = s_verify$
            read #2,key = readkey$, using L00540  , s_23$, eod goto L00600
L00540:        FMT POS(25), CH(8)
            s_23% = len(s_23$)

            init(" ") readkey$                    /* Find Private Label*/
            str(readkey$,1%,9%)   = "ELLISON01"   /* Name              */
            str(readkey$,10%,15%) = s_1$
            read #2,key = readkey$, using L00550, s_prv$,eod goto L00600
L00550:        FMT POS(25), CH(30)
 
            goto exit_sub
L00600: x_er% = 1%                                 /* Series N/A       */
        init(" ") s_23$, s_prv$, s_1$

        exit_sub
        end

