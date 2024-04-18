********************************************************************************
* CMS Procedure Control Program: PROCE7 - G/L Monthend Roll Procedure          *
********************************************************************************
* Mod Date ! By  ! Description                                                 *
*----------+-----!-------------------------------------------------------------*
* 08/04/81 ! EM  ! Original                                                    *
* 11/09/88 ! JDH ! Added Local Authority Books, took out Print Records         *
* 04/21/97 ! LDJ ! Convert to BASIC Program for NT.                            *
********************************************************************************

********************************************************************************
* Standard Declares - should not need to modify                                *
********************************************************************************
    dim rlib$8,                 /* Run library for link call                 */~
        rvol$6,                 /* Run volume for link call                  */~
        run$8                   /* Program to Run                            */

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
        run$ = "GLSTART"
        gosub Run_Program
        if return% = 0% then Normal_Exit

*STEP02:
        call "PUTPARM" addr("E",/* Enter or Display                          */~
            "MONTHEND",         /* PRNAME                                    */~
            1%,                 /* Number of Fields this PRNAME              */~
                "MONTHEND",     /* 1st Field Name                            */~
                "YES",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
                                /* Repeat fields & PRNAMES as needed         */~
            return%)            /* Placeholder                               */

        run$ = "GNLEDGER"
        gosub Run_Program

*STEP03:
        call "PUTPARM" addr("E",/* Enter or Display                          */~
            "MONTHEND",         /* PRNAME                                    */~
            2%,                 /* Number of Fields this PRNAME              */~
                "MONTHEND",     /* 1st Field Name                            */~
                "YES",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
                "LOCLAUTH",     /* 1st Field Name                            */~
                "NO ",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
                                /* Repeat fields & PRNAMES as needed         */~
            return%)            /* Placeholder                               */

        run$ = "GLTRIAL"
        gosub Run_Program
        if return% = 0% then STEP05

*STEP04:
        call "PUTPARM" addr("E",/* Enter or Display                          */~
            "MONTHEND",         /* PRNAME                                    */~
            2%,                 /* Number of Fields this PRNAME              */~
                "MONTHEND",     /* 1st Field Name                            */~
                "YES",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
                "LOCLAUTH",     /* 1st Field Name                            */~
                "YES",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
                                /* Repeat fields & PRNAMES as needed         */~
            return%)            /* Placeholder                               */

        run$ = "GLTRIAL"
        gosub Run_Program

STEP05:
        call "PUTPARM" addr("E",/* Enter or Display                          */~
            "MONTHEND",         /* PRNAME                                    */~
            1%,                 /* Number of Fields this PRNAME              */~
                "MONTHEND",     /* 1st Field Name                            */~
                "YES",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
                                /* Repeat fields & PRNAMES as needed         */~
            return%)            /* Placeholder                               */

        run$ = "GLJURN99"
        gosub Run_Program

*STEP06:
        run$ = "GLROLL"
        gosub Run_Program
        if return% = 99% then Normal_Exit

*STEP07:
        run$ = "STJPASS1"
        gosub Run_Program

*STEP08:
        call "PUTPARM" addr("E",/* Enter or Display                          */~
            "MONTHEND",         /* PRNAME                                    */~
            1%,                 /* Number of Fields this PRNAME              */~
                "MONTHEND",     /* 1st Field Name                            */~
                "YES",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
            return%)            /* Placeholder                               */

        call "PUTPARM" addr("E",/* Enter or Display                          */~
            "JNLID   ",         /* PRNAME                                    */~
            1%,                 /* Number of Fields this PRNAME              */~
                "JNLID   ",     /* 1st Field Name                            */~
                "STJ",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
                                /* Repeat fields & PRNAMES as needed         */~
            return%)            /* Placeholder                               */

        run$ = "STJJURN1"
        gosub Run_Program

*STEP09:
        run$ = "STJFILE2"
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
