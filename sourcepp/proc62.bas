********************************************************************************
* CMS Procedure Control Program: PROC62 -                                     *
********************************************************************************
* Mod Date ! By  ! Description                                                 *
*----------+-----!-------------------------------------------------------------*
* 06/02/81 ! EM  ! Original                                                    *
* 04/21/97 ! LDJ ! Convert to BASIC Program for NT.                            *
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

*STEP01:
        rem *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        rem *************************************************************
        run$ = "PRLRANGE"
        gosub Run_Program
        if return% = 1% or return% = 99% then Normal_Exit

*STEP02:
        run$ = "PRLPRDIN"
        gosub Run_Program

*STEP03:
        run$ = "PRLDDUCT"
        gosub Run_Program
        if return% = 99% then Normal_Exit

*STEP04:
        call "PUTPARM" addr("E",/* Enter or Display                          */~
            "PRLJURN0",         /* PRNAME                                    */~
            1%,                 /* Number of Fields this PRNAME              */~
                "PROOFME ",     /* 1st Field Name                            */~
                "YES",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
                                /* Repeat fields & PRNAMES as needed         */~
            return%)            /* Placeholder                               */

        run$ = "PRLJURN0"
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
