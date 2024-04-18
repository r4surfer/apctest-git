        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCLDSUB                             *~
            *  Creation Date     - 08/19/91                             *~
            *  Last Modified Date- 05/26/98                             *~
            *  Written By        - Roy H. Hoffman                       *~
            *  Description       - Build Special Options Descriptions   *~
            *                      for Production Reporting, Skip N/A   *~
            *                                                           *~
            *     - PARTNO$  - Manufactured Part Number          ( In  )*~
            *                  (Must be Greater Than 16)                *~
            *     - APC_SOD$ - Special Options Description (40)  ( Out )*~
            *                                                           *~
            *     - #1       - Channel for (AMTBOMIF) File       ( In  )*~
            *                                                           *~
            *     - ERR%     - Error Code                        ( Out )*~
            *                  (0%) All Ok                              *~
            *                  (1%) No Description                      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/19/91 ! New Subroutine for (APC) - LAST MOD DATE ! RHH *~
            * 11/17/97 ! Mof for Upgrade to R6.04.03              ! RHH *~
            * 05/26/98 ! (EWD001) Wood Surround Factory Mull      ! RHH *~
            *          !   Code for Description                   !     *~    
            *************************************************************

        sub "APCLDSUB"   (partno$,       /* MFG Part No > 18           */~
                          apc_sod$,      /* Special Option Description */~
                          #1,            /* AMTBOMIF File              */~
                          err% )         /* Error Code 0 = Ok, 1 = err */

        dim                                                              ~
            partno$25,                   /* Part Number (Manufactured) */~
            apc_sod$40,                  /* Special Option Description */~
            prt$30,                      /* Printer Display Description*/~
            i$24,                        /* Field Number Array         */~
            readkey$50,                  /* GENCODES Key               */~
            fld_val$(11%)4               /* Field Values               */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Build Report Descriptions for Prin"
            pname$ = "APCLDSUB - Rev: R6.04"

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! AMTBOMIF ! Master Validity File                     *~
            *************************************************************~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************
            err% = 0%                                /* Set Error Flag */
            init(" ") apc_sod$
            fld_val$(1%) = str(partno$,1%,3%)          /* Model Number */
            fld_val$(2%) = str(partno$,4%,1%)          /* Color        */
            fld_val$(3%) = str(partno$,5%,2%)          /* Glass        */
            fld_val$(4%) = str(partno$,7%,2%)          /* Liting       */
            fld_val$(5%) = str(partno$,9%,2%)          /* Hinge        */
            fld_val$(6%) = str(partno$,11%,1%)         /* Screen       */
            fld_val$(7%) = str(partno$,12%,1%)         /* Locks        */
            fld_val$(8%) = str(partno$,13%,4%)         /* Width        */
            fld_val$(9%) = str(partno$,17%,3%)         /* Height       */
            fld_val$(10%)= str(partno$,20%,3%)         /* CLMR         */
            fld_val$(11%)= str(partno$,23%,3%)         /* WALLWIDT     */

            i$ = "XX0102030405060708091011"

        REM *************************************************************~
            *             B U I L D   D E S C R I P T I O N S           *~
            *                                                           *~
            *************************************************************

               max% = 9%
               if len(partno$) = 22% then max% = 10%
               if len(partno$) = 25% then max% = 11%
               readkey$ = all(hex(00))
               str(readkey$,1%,15%) = fld_val$(1%)
               for i% = 3% to max%
                 if i% = 8% or i% = 9% then goto L01050
                    init(" ") prt$   : x% = 0%
                    str(readkey$,16%,2%)  = str(i$,i%*2%+1%,2%)
                    str(readkey$,18%,15%) = fld_val$(i%)
                    if max% < 10% then goto L00930      /* (EWD001)     */ 
                     convert str(fld_val$(i%),1%,1%) to x%, data goto L01030 

L00930:             read #1,key = readkey$, using L00940  , prt$,           ~
                                                           eod goto L00970
L00940:                FMT XX(33), XX(20), CH(30)
                    if str(prt$,1%,3%) = "N/A" then goto L01050  else      ~
                                                              goto L01000
L00970:                   prt$ = "ERR(" & str(i$,i%*2%+1%,2%) & ")"
                          err% = 1%

L01000:             if i% <> 3% then goto L01040
                       apc_sod$ = prt$
                       goto L01050

L01030:             prt$ = fld_val$(i%)                /* (EWD001)   */

L01040:             apc_sod$ = apc_sod$ & " " & prt$
L01050:        next i%

        REM  EXIT_PROGRAM

        end
