********************************************************************************
* CMS Procedure Control Program: PROCGC20 - Currency Conversion table type     *
********************************************************************************
* Mod Date ! By  ! Description                                                 *
*----------+-----!-------------------------------------------------------------*
* 01/22/88 ! MJB ! Original                                                    *
* 04/21/97 ! DER ! Converted to BASIC Program.                                 *
********************************************************************************

********************************************************************************
* Standard Declares - should not need to modify                                *
********************************************************************************
    dim rlib$8,                  /* Run library for link call                */~
        rvol$6,                  /* Run volume for link call                 */~
        run$8                    /* Program to Run                           */

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
        call "PUTPARM" addr("E",    /* Enter or Display                     */~
            "GENCDSIN",             /* PRNAME                               */~
            3%,                     /* Number of Fields this PRNAME         */~
            "CODEFILE",             /* 1st Field Name                       */~
            "CURR TYPE",            /* Field Value                          */~
            9%,                     /* Length of Field Value                */~
            "FILEDESC", ~
            "Currency Conversion Table Type",~
            30%, ~
            "CDLENGTH",~
            " 1",~
            2%,~
                                    /* Repeat fields & PRNAMES as needed    */~
            return%)                /* Placeholder                          */

        run$ = "GENCDSIN"
        gosub Run_Program
        goto Normal_Exit

********************************************************************************
*       Below Code Should not need changing                                    *
********************************************************************************
Run_Program:
        rlib$, rvol$ = " "
        call "PROCLINK" (run$, rlib$, rvol$, return%, comp%)
        if comp% = 0% then return
        end comp%

Normal_Exit:
        end
