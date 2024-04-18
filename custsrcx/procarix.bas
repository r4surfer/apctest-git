********************************************************************************
* CMS Procedure Control Program: PROCARIU - Process A/R Invoice Sessions       *
********************************************************************************
* Mod Date ! By  ! Description                                                 *
*----------+-----!-------------------------------------------------------------*
* 09/15/86 ! ERN ! Original                                                    *
* 10/26/92 ! JDH ! Added return code check to ARIUPDTE (step 2)                *
* 04/21/97 ! LDJ ! Converted to BASIC Program.                                 *
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

*STEP00:
        rem *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        rem *************************************************************
        call "PUTPARM" addr("E",/* Enter or Display                     */~
            "PROCID  ",         /* PRNAME                               */~
            1%,                 /* Number of Fields this PRNAME         */~
                "PROCID  ",     /* 1st Field Name                       */~
                "PROCARIU",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                                /* Repeat fields & PRNAMES as needed    */~
            return%)            /* Placeholder                          */

        run$ = "UPDCHECK"
        gosub Run_Program
        if return% = 2% then Normal_Exit

*CHECK1:
        call "PUTPARM" addr("E",/* Enter or Display                     */~
            "GETPGMS ",         /* PRNAME                               */~
            2%,                 /* Number of Fields this PRNAME         */~
                "PREVPGM ",     /* 1st Field Name                       */~
                "UPDCHECK",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                "NEXTPGM ",     /* 2nd Field Name                       */~
                "UPDSELCX",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                                /* Repeat fields & PRNAMES as needed    */~
            return%)            /* Placeholder                          */

        run$ = "UPDNEXT"
        gosub Run_Program
        if return% = 1% then CHECK2

*STEP01:
        call "PUTPARM" addr("E",/* Enter or Display                     */~
            "UPDSELCX",         /* PRNAME                               */~
            1%,                 /* Number of Fields this PRNAME         */~
                "UPDATE  ",     /* 1st Field Name                       */~
                "ARIUPDTE",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                                /* Repeat fields & PRNAMES as needed    */~
            return%)            /* Placeholder                          */

        run$ = "UPDSELCX"
        gosub Run_Program
        if return% = 0% then DONE

CHECK2:
        call "PUTPARM" addr("E",/* Enter or Display                     */~
            "GETPGMS ",         /* PRNAME                               */~
            2%,                 /* Number of Fields this PRNAME         */~
                "PREVPGM ",     /* 1st Field Name                       */~
                "UPDSELCX",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                "NEXTPGM ",     /* 2nd Field Name                       */~
                "ARIUPDTE",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                                /* Repeat fields & PRNAMES as needed    */~
            return%)            /* Placeholder                          */

        run$ = "UPDNEXT"
        gosub Run_Program
        if return% = 1% then CHECK3

*STEP02:
        run$ = "ARIUPDTE"
        gosub Run_Program
        if return% = 1% then Normal_Exit

CHECK3:
        call "PUTPARM" addr("E",/* Enter or Display                     */~
            "GETPGMS ",         /* PRNAME                               */~
            2%,                 /* Number of Fields this PRNAME         */~
                "PREVPGM ",     /* 1st Field Name                       */~
                "ARIUPDTE",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                "NEXTPGM ",     /* 2nd Field Name                       */~
                "ARIRGSTR",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                                /* Repeat fields & PRNAMES as needed    */~
            return%)            /* Placeholder                          */

        run$ = "UPDNEXT"
        gosub Run_Program
        if return% = 1% then CHECK4

*STEP03:
        run$ = "ARIRGSTR"
        gosub Run_Program

CHECK4:
        call "PUTPARM" addr("E",/* Enter or Display                     */~
            "GETPGMS ",         /* PRNAME                               */~
            2%,                 /* Number of Fields this PRNAME         */~
                "PREVPGM ",     /* 1st Field Name                       */~
                "ARIRGSTR",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                "NEXTPGM ",     /* 2nd Field Name                       */~
                "ARIJURNL",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                                /* Repeat fields & PRNAMES as needed    */~
            return%)            /* Placeholder                          */

        run$ = "UPDNEXT"
        gosub Run_Program
        if return% = 1% then CHECK5

*STEP04:
        run$ = "ARIJURNL"
        gosub Run_Program

CHECK5:
        call "PUTPARM" addr("E",/* Enter or Display                     */~
            "GETPGMS ",         /* PRNAME                               */~
            2%,                 /* Number of Fields this PRNAME         */~
                "PREVPGM ",     /* 1st Field Name                       */~
                "ARIJURNL",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                "NEXTPGM ",     /* 2nd Field Name                       */~
                "ARIPRINT",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                                /* Repeat fields & PRNAMES as needed    */~
            return%)            /* Placeholder                          */

        run$ = "UPDNEXT"
        gosub Run_Program
        if return% = 1% then CHECK6

*STEP05:
        run$ = "ARIPRINT"
        gosub Run_Program

CHECK6:
        call "PUTPARM" addr("E",/* Enter or Display                     */~
            "GETPGMS ",         /* PRNAME                               */~
            2%,                 /* Number of Fields this PRNAME         */~
                "PREVPGM ",     /* 1st Field Name                       */~
                "ARIPRINT",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                "NEXTPGM ",     /* 2nd Field Name                       */~
                "CORARUPD",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                                /* Repeat fields & PRNAMES as needed    */~
            return%)            /* Placeholder                          */

        run$ = "UPDNEXT"
        gosub Run_Program
        if return% = 1% then CHECK7

STEP06:
        run$ = "CORARUPD"
        gosub Run_Program
        if return% <> 0% then Normal_Exit

CHECK7:
DONE:   run$ = "UPDDONE"
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
