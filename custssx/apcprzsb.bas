        REM *************************************************************~
            *                       As of 04/14/00                      *~
            * APCPRZSB - 1.Look_Up Private Label code in either         *~
            *              (APCPLNDT-PRV% = 0%) or (BCKLINES-PRV% = 1%) *~
            *              if none found then obtain from CUSTOMER 2nd, *~
            *            2.If Private Label Code is Passed in as a      *~
            *              non-blank (s_1$) then go to ( Step-4 )       *~
            *            3.Look-Up Customer and find out what Private   *~
            *              Label Code to Use. (CUSTOMER)                *~
            *            4.Using the Model (Input) and Private Label    *~
            *              Code Lookup Series name for Model.(ELLISON05)*~
            *              <Series> <Type> for Specified Model          *~
            *            5.Using the Private Label Code s_1$ find the   *~
            *               Private label name (ELLISON01)              *~
            *            6.Argument List       -  Input/Output          *~
            *              Look_up Code        =  PRV% = 0% or 1%       *~
            *              Private Label Code  =  s_1$    - Input/Output*~
            *              Customer Code       =  cuscode$- Input       *~
            *              New Model Code      =  S_23M$  - Input       *~
            *              Sale Order          =  s_so$   - Input       *~
            *              Sales Order Line    =  s_ln$   - Input       *~
            *                                                           *~ 
            *              Private Label Name  =  s_prv$  - Output      *~
            *              New Series Code     =  S_23$   - Output      *~
            *              Series Length       =  S_23%   - Output      *~
            *              Return Code         =  0% = Ok               *~
            *                                  =  1% = Series N/A       *~
            *************************************************************~
            * Programs Using Sub   =    APCPRCQT - Price Quote Utility  *~
            *     (APCPLNDT)            BCKFASTR - S.O. Entry (Dead)    *~
            *                           APCPL41A - (P) PLANNING REPORTS *~
            *                           APCBLSUB - (S) SHPDOCPR  - BOL  *~
            *                           APCEDIPO - (S) APCEDI02  - S.O  *~
            *                           APCPLA44 - (P) Shipping  - DLV  *~
            *                                                           *~
            * Programs Using Sub   =    EWDPLN58 - (P) Specials/Tempered*~
            *     (BCKLINES)            EWDPLA58 - (S) Barcode Display  *~
            *                           APCFAXSD - (P) BCKUPDTE         *~
            *                           BCKACKS  - (P) S.O. Acknowledge *~
            *                           ARIPRSUB - (S) ARIPRINT Invoice *~
            *                           APCSLS00 - (P) APCSLSDT Sales   *~
            *************************************************************

        sub "APCPRZSB" (prv%,            /* 0%=APCPLNDT, 1%=BCKLINES   */~
                        s_1$,            /* Priv Label CodeInput/Output*/~
                        cuscode$,        /* Customer Code      Input   */~
                        s_23m$,          /* Model Code         Input   */~
                        s_so$,           /* Sales Order Number Input   */~
                        s_ln$,           /* Sales Order Ln ItemInput   */~
                        s_prv$,          /* Private Label Name Output  */~
                        s_23$,           /* Series Name        Output  */~
                        s_23%,           /* Length of Name     Output  */~
                        #1,              /* (CUSTOMER) - Customer Maste*/~
                        #2,              /* (GENCODES) - Code Tables   */~
                        #3,              /* (APCPLNDT) - Planning DTL  */~
                        #4,              /* (BCKLINES) - S.O. Detail   */~
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
            * #4  ! BCKLINES ! Sales Order Detail                       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            x_er% = 0% : s_23% = 0% : s_ln% = 0%
            init(" ") readkey$, s_23$, s_verify$, s_prv$
            if len(s_1$) = 2 then goto L00530     /* Value Passed In   */

            if len(s_so$) < 5 then goto L00510    /* Use Customer Data */
               convert s_ln$ to s_ln%, data goto L00500
L00500:
               if prv% = 0% then gosub lookup_apcplndt                   ~
                            else gosub lookup_bcklines
               if s_1% <> 0% then goto L00530     /* Private Code Found*/


L00510:     init(" ") readkey$, s_1$
            read #1,key = cuscode$, using L00520  , s_1$, eod goto L00600
L00520:        FMT POS(960), CH(2)
                                                  /* Found in Customer */
L00530:     init(" ") readkey$                    /* Find Private Label*/
            str(readkey$,1%,9%)   = "ELLISON01"   /* Name              */
            str(readkey$,10%,15%) = s_1$
            read #2,key = readkey$, using L00535, s_prv$,eod goto L00600
L00535:        FMT POS(25), CH(30)

            init(" ") readkey$
            str(s_verify$,1%,3%)  = s_23m$        /* Model Code        */
            str(s_verify$,4%,2%)  = s_1$          /* private Label Code*/ 
            str(readkey$,1%,9%)   = "ELLISON05"
            str(readkey$,10%,15%) = s_verify$
            read #2,key = readkey$, using L00540, s_23$, eod goto L00600
L00540:        FMT POS(25), CH(8)
            s_23% = len(s_23$)

            goto exit_sub
L00600: x_er% = 1%                                 /* Series N/A       */
        init(" ") s_23$

        exit_sub
        end

        lookup_apcplndt
           s_1% = 0%
           convert s_ln% to s_ln$, pic(000)   /* Zero Fill         */  
           str(readkey$,1%,8%) = s_so$        /* Sales Order       */
           str(readkey$,9%,2%) = str(s_ln$,2%,2%) /* Line Item     */
           read #3,key > readkey$,using L01000, readkey$, s_1$,       ~
                                                      eod goto L01010
L01000:       FMT POS(24), CH(23), POS(243), CH(2)
           if s_so$ <> str(readkey$,1%,8%) then goto L01010 
           if str(s_ln$,2%,2%) <> str(readkey$,9%,2%) then goto L01010 
              convert s_1$ to s_1%, data goto L01010
                                              /* Found in APCPLNDT */
L01010: return                                /* s_1% not Zero     */
 
        lookup_bcklines
           s_1% = 0%
           convert s_ln% to s_ln$, pic(###)   /* Blank Fill        */  
           str(readkey$,1%,16%) = s_so$       /* Sales Order       */
           str(readkey$,17%,3%) = s_ln$       /* Line Item         */
           read #4,key = readkey$,using L02000, readkey$, s_1$,       ~
                                                      eod goto L02010
L02000:       FMT POS(10), CH(19), POS(282), CH(2)
           if s_so$ <> str(readkey$,1%,8%) then goto L02010 
           if str(s_ln$,2%,2%) <> str(readkey$,18%,2%) then goto L02010 
              convert s_1$ to s_1%, data goto L02010
                                              /* Found in BCKLINES */
L02010: return                                /* s_1% not Zero     */ 
