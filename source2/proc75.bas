********************************************************************************
* CMS Procedure Control Program: PROC75 -                                     *
********************************************************************************
* Mod Date ! By  ! Description                                                 *
*----------+-----!-------------------------------------------------------------*
* 10/22/84 ! HT  ! Original                                                    *
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
        run$ = "PAYADJIN"
        gosub Run_Program
        if return% = 0% then Normal_Exit

*STEP02:
        call "PUTPARM" addr("E",/* Enter or Display                          */~
            "UPDATE  ",         /* PRNAME                                    */~
            1%,                 /* Number of Fields this PRNAME              */~
                "UPDATE  ",     /* 1st Field Name                            */~
                "NO ",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
            return%)            /* Placeholder                               */

        call "PUTPARM" addr("E",/* Enter or Display                          */~
            "JNLID   ",         /* PRNAME                                    */~
            1%,                 /* Number of Fields this PRNAME              */~
                "JNLID   ",     /* 1st Field Name                            */~
                "VAJ",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
                                /* Repeat fields & PRNAMES as needed         */~
            return%)            /* Placeholder                               */

        run$ = "PAYUPDTE"
        gosub Run_Program

*STEP03:
        call "PUTPARM" addr("E",/* Enter or Display                          */~
            "UPDCTL  ",         /* PRNAME                                    */~
            2%,                 /* Number of Fields this PRNAME              */~
                "REPORT  ",     /* 1st Field Name                            */~
                "FUL",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
                "POSTME  ",     /* 2ND Field Name                            */~
                "YES",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
            return%)            /* Placeholder                               */

        call "PUTPARM" addr("E",/* Enter or Display                          */~
            "JNLID   ",         /* PRNAME                                    */~
            1%,                 /* Number of Fields this PRNAME              */~
                "JNLID   ",     /* 1st Field Name                            */~
                "VAJ",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
                                /* Repeat fields & PRNAMES as needed         */~
            return%)            /* Placeholder                               */

        run$ = "PAYJURNL"
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
