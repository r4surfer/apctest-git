********************************************************************************
* CMS Procedure Control Program: PROCCAL - Roll Production Calendar.           *
********************************************************************************
* Mod Date ! By  ! Description                                                 *
*----------+-----!-------------------------------------------------------------*
* 07/19/83 ! HES ! Original                                                    *
* 04/25/85 ! LDJ ! Eliminated references to RUNLIB and RUNVOL, renamed         *
* 08/14/86 ! MJB ! Corrected spelling errors                                   *
* 06/07/89 ! MLJ ! Changed Label GHNG3 to FILE3                                *
* 06/15/89 ! MJB ! Added escape route from CALINPUT                            *
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


*STEP01:
        rem *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        rem *************************************************************
        run$ = "CALINPUT"
        gosub Run_Program
        if return% = 1% then FILE3
        if return% = 2% then DOBCK
        if return% = 16% then RESTORE1
        goto Normal_Exit

DOBCK:
* a Forced Backup could be placed here.  It would be required only
* if there is an existing calendar that needs to be changed.

*FILE1:
        run$ = "PIPOUTOP"
        gosub Run_Program

*CHNG1:
        run$ = "CALCHNG1"
        gosub Run_Program
        if return% = 999% then RESTORE1
        run$ = "PIPOUTSR"
        gosub Run_Program

*FILE2:
        run$ = "PIPINOP"
        gosub Run_Program

*CHNG2:
        run$ = "CALCHNG2"
        gosub Run_Program
        if return% = 999% then RESTORE1
        run$ = "PIPINSR"
        gosub Run_Program

FILE3:  run$ = "WCOUTOP"
        gosub Run_Program

*CHNG3:
        run$ = "CALCHNG3"
        gosub Run_Program
        if return% = 999% then RESTORE1
        if return% <> 0% then STEP4

*STEP1:
        run$ = "WCOUTSR"
        gosub Run_Program
        goto Normal_Exit

STEP4:
        run$ = "WCOUTSR"
        gosub Run_Program
        end 999%

RESTORE1:
* A Forced restore of the Database could be placed here to ensure data integrity
* The Programs set the Return Code if Forced to Abort
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
