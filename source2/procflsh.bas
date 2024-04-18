********************************************************************************
* CMS Procedure Control Program: PROCFLSH - Mass withdraw of Inventory to Job  *
********************************************************************************
* Mod Date ! By  ! Description                                                 *
*----------+-----!-------------------------------------------------------------*
* 07/08/91 ! SID ! Original                                                    *
* 03/11/93 ! JRH ! Rename to PROCFLSH                                          *
* 04/18/97 ! LDJ ! Convert to BASIC Program for NT.                            *
********************************************************************************

********************************************************************************
* Standard Declares - should not need to modify                                *
********************************************************************************
    dim rlib$8,                  /* Run library for link call                */~
        rvol$6,                  /* Run volume for link call                 */~
        run$8                    /* Program to Run                           */~

********************************************************************************
* Below Section is where you need to add your code and/or changes              *
********************************************************************************

        rem *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        rem *************************************************************
        call "SHOSTAT" ("Enter Inventory Backflush Transactions")

*STEP01:
        run$ = "HNYFLUSH"
        gosub Run_Program
        if return% = 0% then Normal_Exit

        call "SHOSTAT" ("Preparing to Post Transaction Data")

*STEP02:
        call "PUTPARM" addr("E",/* Enter or Display                          */~
            "JNLID   ",         /* PRNAME                                    */~
            1%,                 /* Number of Fields this PRNAME              */~
                "JNLID   ",     /* 1st Field Name                            */~
                "MFL",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
                                /* Repeat fields & PRNAMES as needed         */~
            return%)            /* Placeholder                               */

        run$ = "HNYFSHJN"
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
