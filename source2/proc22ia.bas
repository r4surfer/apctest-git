********************************************************************************
* CMS Procedure Control Program: PROC22IA - Enter A/P Invoices and submit      *
*                                           Background Task for Processing.    *
********************************************************************************
* Mod Date ! By  ! Description                                                 *
*----------+-----!-------------------------------------------------------------*
* 01/30/84 ! ??? ! Original  (copy of PROC22AA)                                *
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
        call "PUTPARM" addr("E",/* Enter or Display                          */~
            "RUNMODE ",         /* PRNAME                                    */~
            1%,                 /* Number of Fields this PRNAME              */~
                "INVOICES",     /* 1st Field Name                            */~
                "   ",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
                                /* Repeat fields & PRNAMES as needed         */~
            return%)            /* Placeholder                               */
        run$ = "PAYINPUT"
        gosub Run_Program
        if return% = 0% then Normal_Exit

*        call "SHOSTAT" ("Preparing to Post Transaction Data")

*STEP02:
        call "PUTPARM" addr("E",/* Enter or Display                          */~
            "TSKBKG01",         /* PRNAME                                    */~
            3%,                 /* Number of Fields this PRNAME              */~
                "TASKID  ",     /* 1st Field Name                            */~
                "ZZ",           /* Field Value                               */~
                2%,             /* Length of Field Value                     */~
                "FUNCTION",     /* 2nd Field Name                            */~
                "S",            /* Field Value                               */~
                1%,             /* Length of Field Value                     */~
                "PROGRAM ",     /* 3rd Field Name                            */~
                "PROC22IB",     /* Field Value                               */~
                8%,             /* Length of Field Value                     */~
                                /* Repeat fields & PRNAMES as needed         */~
            return%)            /* Placeholder                               */

        run$ = "TASKBKGD"
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
