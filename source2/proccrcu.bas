********************************************************************************
* CMS Procedure Control Program: PROCCRCU - Process Cash Receipts Sessions     *
********************************************************************************
* Mod Date ! By  ! Description                                                 *
*----------+-----!-------------------------------------------------------------*
* 12/15/86 ! ERN ! Original                                                    *
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
                "PROCCRCU",     /* Field Value                          */~
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
                "UPDSELCT",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                                /* Repeat fields & PRNAMES as needed    */~
            return%)            /* Placeholder                          */

        run$ = "UPDNEXT"
        gosub Run_Program
        if return% = 1% then CHECK2

*STEP01:
        call "PUTPARM" addr("E",/* Enter or Display                     */~
            "UPDSELCT",         /* PRNAME                               */~
            1%,                 /* Number of Fields this PRNAME         */~
                "UPDATE  ",     /* 1st Field Name                       */~
                "CRCUPDTE",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                                /* Repeat fields & PRNAMES as needed    */~
            return%)            /* Placeholder                          */

        run$ = "UPDSELCT"
        gosub Run_Program
        if return% = 0% then DONE

CHECK2:
        call "PUTPARM" addr("E",/* Enter or Display                     */~
            "GETPGMS ",         /* PRNAME                               */~
            2%,                 /* Number of Fields this PRNAME         */~
                "PREVPGM ",     /* 1st Field Name                       */~
                "UPDSELCT",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                "NEXTPGM ",     /* 2nd Field Name                       */~
                "CRCUPDTE",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                                /* Repeat fields & PRNAMES as needed    */~
            return%)            /* Placeholder                          */

        run$ = "UPDNEXT"
        gosub Run_Program
        if return% = 1% then CHECK3

*STEP02:
        run$ = "CRCUPDTE"
        gosub Run_Program

CHECK3:
        call "PUTPARM" addr("E",/* Enter or Display                     */~
            "GETPGMS ",         /* PRNAME                               */~
            2%,                 /* Number of Fields this PRNAME         */~
                "PREVPGM ",     /* 1st Field Name                       */~
                "CRCUPDTE",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                "NEXTPGM ",     /* 2nd Field Name                       */~
                "CRCJURNL",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                                /* Repeat fields & PRNAMES as needed    */~
            return%)            /* Placeholder                          */

        run$ = "UPDNEXT"
        gosub Run_Program
        if return% = 1% then CHECK4

*STEP03:
        run$ = "CRCJURNL"
        gosub Run_Program

CHECK4:
        call "PUTPARM" addr("E",/* Enter or Display                     */~
            "GETPGMS ",         /* PRNAME                               */~
            2%,                 /* Number of Fields this PRNAME         */~
                "PREVPGM ",     /* 1st Field Name                       */~
                "CRCJURNL",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                "NEXTPGM ",     /* 2nd Field Name                       */~
                "CRCRGSTR",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                                /* Repeat fields & PRNAMES as needed    */~
            return%)            /* Placeholder                          */

        run$ = "UPDNEXT"
        gosub Run_Program
        if return% = 1% then CHECK5

*STEP04:
        run$ = "CRCRGSTR"
        gosub Run_Program

CHECK5:
        call "PUTPARM" addr("E",/* Enter or Display                     */~
            "GETPGMS ",         /* PRNAME                               */~
            2%,                 /* Number of Fields this PRNAME         */~
                "PREVPGM ",     /* 1st Field Name                       */~
                "CRCRGSTR",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                "NEXTPGM ",     /* 2nd Field Name                       */~
                "CRCDEPST",     /* Field Value                          */~
                8%,             /* Length of Field Value                */~
                                /* Repeat fields & PRNAMES as needed    */~
            return%)            /* Placeholder                          */

        run$ = "UPDNEXT"
        gosub Run_Program
        if return% = 1% then CHECK6

*STEP05:
        run$ = "CRCDEPST"
        gosub Run_Program

CHECK6:

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
