********************************************************************************
* CMS Procedure LINK Routine: PROCLINK - Simplified LINK for Procedure Programs*
********************************************************************************
* Mod Date ! By  ! Description                                                 *
*----------+-----!-------------------------------------------------------------*
* 04/21/97 ! LDJ ! Original                                                    *
********************************************************************************
sub "PROCLINK" (run$,           /* Name of Program to Run                    */~
                rlib$,          /* Run Library (normally leave blank)        */~
                                /*   outputs library ran from.               */~
                rvol$,          /* Run Volume (normally leave blank)         */~
                                /*   outputs volume ran from.                */~
                return%,        /* Return code from program ran              */~
                comp%)          /* Program Completion Code                   */
                                /*   =  0 if run Successfully                */
                                /*   =  8 if not found                       */
                                /*   = 16 if Abnormal Termination / Cancelled*/

********************************************************************************
* Standard Declares                                                            *
********************************************************************************
    dim rlib$8,                  /* Run library for link call                */~
        rvol$6,                  /* Run volume for link call                 */~
        caller$16,               /* CMSLINK Argument                         */~
        run$8,                   /* Program to Run                           */~
        pf16$24,                 /* CMSLINK argument                         */~
        userid$3                 /* argument for CMSLINK                     */
********************************************************************************

        rem *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto cms_start
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        rem *************************************************************
cms_start
select #3,"USERLCMS",indexed,recsize=400,keypos=1,keylen=3 /* Req'd by CMSLINK*/

********************************************************************************
*       Below Code Is what does the Job                                        *
********************************************************************************
Run_Program:
        call "CMSLINK" addr(#3, userid$,"R",run$, rlib$,             ~
                     rvol$," ","N", pf16$, caller$, "N", comp%, return%)
        if comp% = 0% then Exit_Routine
        if comp% = 16% then AbEnd
        call "ASKGUI" (21%,"Program Not Found!","Cannot find " & ~
             run$ & " in your run list. To correct and Retry, place the program"~
             & " into your run library/volume and select Retry,"~
             & " otherwise CANCEL",return%)
        if return% = 4% then call "SHOSTAT" ("Looking for " & run$ & " ...")
        if return% = 4% then Run_Program
        goto Exit_Routine

AbEnd:  call "ASKGUI" (16%,"Program Aborted!",run$ & ~
             " terminated abnormally!  This procedure will now end.",~
             return%)

Exit_Routine:
        end
