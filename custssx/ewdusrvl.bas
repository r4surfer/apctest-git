REM         *-----------------------------------------------------------*~
            * EWDUSRVL - CHECKS TO SEE IF THE USER IS IN THE DATA BASE  *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/28/98 ! ORIGINAL - Copied & Mod CMSDBADM.        ! BWS *~
            * 02/10/99 ! EWD001 - Mod to extract SYSLIB & SYSVOL. ! BWS *~
            *************************************************************

            sub "EWDUSRVL" (useridin$, yesno$)

        dim                                                              ~
            file$8,                      /* File Name                  */~
            syslib$8,                    /* SYSTEM LIBRARY             */~
            sysvol$6,                    /* System Volume              */~
            userid$3,                    /* UserId This User           */~
            useridin$3,                  /* UserId Passed-In           */~
            yesno$1                      /* True Believer or Imposter? */~

        REM *************************************************************~
            *                   S E L E C T   F I L E S                 *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! DESCRIPTION                              *~
            *-----+----------+------------------------------------------*~
            * #1  ! USERLCMS ! Program Access Control User Info file    *~
            *************************************************************

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            select #1,  "USERLCMS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos =    1, keylen =   3,                     ~
                        alt key 1, keypos =  4, keylen = 30, dup

            f2% = -1%
            yesno$ = "N"
/*EWD001*/  call "EXTRACT" addr("ID",userid$,"XL",syslib$,"XV",sysvol$)
            if useridin$ = " " then useridin$ = userid$
            if useridin$ = userid$ then L02500  

            call "READFDR" addr("USERLCMS", syslib$, sysvol$, 0%, f2%)
                if f2% <> 0% then L65000
            file$ = "USERLCMS"
            call "PUTNAMES" addr(#1, file$, syslib$, sysvol$)
            call "WORKOPN2" (#1, "SHARE", 0%, f1%)

            REM NOW TRY AND GET DESIRED INFO.
            call "READ100" (#1, useridin$, f1%)
                if f1% = 0% then L65000
L02500:     yesno$ = "Y"

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            if f2% = 0% then close #1
            end


