********************************************************************************
* AWD Procedure Control Program: PROCINVA - Direct Inventory Additions Procedur*
********************************************************************************
* Mod Date ! By  ! Description                                                 *
*----------+-----!-------------------------------------------------------------*
* 10/08/80 ! EM  ! Original                                                    *
* 02/29/87 ! LDJ ! Changed name from PROC42                                    *
* 04/21/97 ! LDJ ! Convert to BASIC Program for NT.                            *
********************************************************************************
* 01/30/06 ! RHH ! (PAR000) CR347 New Part Number. Create New Proc from        *
*          !     !    PROCHNYA.                                                *
********************************************************************************

********************************************************************************
* Standard Declares - should not need to modify                                *
********************************************************************************
    dim rlib$8,                 /* Run library for link call                 */~
        rvol$6,                 /* Run volume for link call                  */~
        run$8                   /* Program to Run                            */~

********************************************************************************
* Below Section is where you need to add your code and/or changes              *
********************************************************************************
        rem *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "REV:01.00 01/30/06 New Part Number Inventory ADD "
        rem *************************************************************
        call "SHOSTAT" ("Preparing to enter Inventory Additions Transactions")
*STEP01:
        run$ = "INVADDNS"                                 /* (PAR000)        */
        gosub Run_Program
        if return% = 0% then Normal_Exit

        call "SHOSTAT" ("Preparing to Post Inventory Additions Entries")

*STEP02:
        call "PUTPARM" addr("E",/* Enter or Display                          */~
            "JNLID   ",         /* PRNAME                                    */~
            1%,                 /* Number of Fields this PRNAME              */~
                "JNLID   ",     /* 1st Field Name                            */~
                "IAD",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
                                /* Repeat fields & PRNAMES as needed         */~
            return%)            /* Placeholder                               */

        run$ = "INVADDJN"                                 /* (PAR000)        */
        gosub Run_Program

        goto Normal_Exit

********************************************************************************
*       Below Code Should not need to be Changed                               *
********************************************************************************
Run_Program:
        rlib$, rvol$ = " "
        call "PROCLINK" (run$, rlib$, rvol$, return%, comp%)
        if comp% = 0% then return
        end comp%

Normal_Exit:
        end
