REM         *-----------------------------------------------------------*~
            * EWDLOGOU - Deletes a rec from EWDXOLOG when user logs out.*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/10/99 ! ORIGINAL - Copied & Mod Sub EWDLOGIN.    ! BWS *~
            *************************************************************

        dim                                                              ~
            file$8,                      /* File Name                  */~
            syslib$8,                    /* SYSTEM LIBRARY             */~
            sysvol$6,                    /* System Volume              */~
            userid$3,                    /* UserId This User           */~
            readkey$50,                  /* Key to read EWDXOLOG       */~
            lastdt$6,                    /* Last Successful Logon Date */~
            lasttm$6                     /* Last Successful Logon Time */~

        REM *************************************************************~
            *                   S E L E C T   F I L E S                 *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! DESCRIPTION                              *~
            *-----+----------+------------------------------------------*~
            * #1  ! USERLCMS ! Program Access Control User Info file    *~
            * #2  ! EWDXOLOG ! EWD User Login/Logout Log File           *~
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

            select #2,  "EWDXOLOG",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  32,                                   ~
                        keypos =    1, keylen =  15                      

            call "EXTRACT" addr("ID",userid$,"XL",syslib$,"XV",sysvol$)
            call "READFDR" addr("USERLCMS", syslib$, sysvol$, 0%, f2%)
                if f2% <> 0% then L65000
            file$ = "USERLCMS"
            call "PUTNAMES" addr(#1, file$, syslib$, sysvol$)
            call "WORKOPN2" (#1, "SHARE", 0%, f1%)

            call "OPENCHCK" (#2, f1%, f2%, 500%, " ")

            REM NOW TRY AND GET DESIRED INFO.
            call "READ100" (#1, userid$, f1%)
                if f1% = 0% then L65000
            get #1, using L00500, lasttm$, lastdt$
L00500:         fmt pos(39), ch(6), pos(402), ch(6)
            readkey$ = str(userid$) & str(lastdt$) & str(lasttm$)
            read #2, hold, key = readkey$, eod goto L65000
            delete #2

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            end


