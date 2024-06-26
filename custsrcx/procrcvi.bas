********************************************************************************
* CMS Procedure Control Program: PROCRCVI -                                    *
********************************************************************************
* Mod Date ! By  ! Description                                                 *
*----------+-----!-------------------------------------------------------------*
*     ?    ! KAB ! Original                                                    *
* 04/21/97 ! LDJ ! Convert to BASIC Program for NT.                            *
* 04/06/99 ! BWS ! Mods to write active program to the EWDXOLOG file. (EWD001) *
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
        run$ = "RCVINPUT"
        gosub Run_Program
        if return% = 0% then Normal_Exit

*STEP02:
        run$ = "RCVUPDTE"
        gosub Run_Program

*STEP03:
        call "PUTPARM" addr("E",/* Enter or Display                          */~
            "RUNMODE ",         /* PRNAME                                    */~
            1%,                 /* Number of Fields this PRNAME              */~
                "RUNMODE ",     /* 1st Field Name                            */~
                "ALL",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
                                /* Repeat fields & PRNAMES as needed         */~
            return%)            /* Placeholder                               */

        run$ = "RCVHNYRT"
        gosub Run_Program

*STEP04:
        call "PUTPARM" addr("E",/* Enter or Display                          */~
            "POSTME  ",         /* PRNAME                                    */~
            1%,                 /* Number of Fields this PRNAME              */~
                "POSTME  ",     /* 2nd Field Name                            */~
                "YES",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
            return%)            /* Placeholder                               */

        call "PUTPARM" addr("E",/* Enter or Display                          */~
            "JNLID   ",         /* PRNAME                                    */~
            1%,                 /* Number of Fields this PRNAME              */~
                "JNLID   ",     /* 1st Field Name                            */~
                "PRC",          /* Field Value                               */~
                3%,             /* Length of Field Value                     */~
                                /* Repeat fields & PRNAMES as needed         */~
            return%)            /* Placeholder                               */

        run$ = "RCVJURNL"
        gosub Run_Program

        goto Normal_Exit

********************************************************************************
*       Below Code Should not need to be Changed                               *
********************************************************************************
Run_Program:
        rlib$, rvol$ = " "
        call "EWDLOGUP" (run$)        /* Log Active Program */  /*EWD001*/
        call "PROCLINK" (run$, rlib$, rvol$, return%, comp%)
        call "EWDLOGUP" (" ")         /*  Reset Active Pgm  */  /*EWD001*/
        if comp% = 0% then return
        end comp%

Normal_Exit:
        end
