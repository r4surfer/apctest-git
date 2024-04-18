        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPR4SB                             *~
            *  Creation Date     - 12/09/93                             *~
            *  Last Modified Date- 10/31/97                             *~
            *  Description       - This Subroutine obtains the Price    *~
            *                      of Product when found in in the      *~
            *                      SKU# Master File                     *~
            *                                                           *~
            *  Special Comments  - Note that the Part Number (PART$)    *~
            *                      is 'Exact Size'. Also the Specified  *~
            *                      Customer Must have a SKU Code        *~
            *                      Assigned in the Customer Master File.*~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/04/94 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 10/31/97 ! Check for Upgrade to R6.04.03            ! RHH *~
            *          !                                          !     *~
            *************************************************************

            sub "APCPR4SB" ( part$,      /* MFG Part Number(Exact Size)*/~
                             cuscode$,   /* Customer Code              */~
                             pc_p,       /* Price of MFG               */~
                             #1,         /* CUSTOMER - File            */~
                             #2,         /* APCSKUNO - File            */~
                             pc_p% )     /* 0% = No Price              */

        dim                              /* (GENCODES) Code Tables   . */~
            part$25,                     /* MFG Part Number            */~
            cuscode$9,                   /* Customer Account Code      */~
            sku_code$3,                  /* SKU Code from Customer     */~
            sku_key$28                   /* Sku Key for Part Number    */

                                           /* Get Value from Code Desc */
            pc_p = 0.0 : pc_p% = 0% : sku_code$ = " "
            read #1,key = cuscode$, using   L00380, sku_code$,              ~
                                                    eod goto exit_program
L00380:         FMT POS(1000), CH(3)
            sku_key$ = " "
            str(sku_key$,1%,3%)  = sku_code$
            str(sku_key$,4%,25%) = part$
            read #2,key 1% = sku_key$, using   L00440, pc_p,                ~
                                                    eod goto exit_program
L00440:        FMT POS(57), PD(14,4)
            pc_p% = 1%                        /* SKU NO. Price on File */
        exit_program
        end

