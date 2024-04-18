        REM *************************************************************~
            *                                                           *~
            *   CCC   M   M   SSS   DDDD   BBBB     A    DDDD   M   M   *~
            *  C   C  MM MM  S      D   D  B   B   A A   D   D  MM MM   *~
            *  C      M M M   SSS   D   D  BBBB   AAAAA  D   D  M M M   *~
            *  C   C  M   M      S  D   D  B   B  A   A  D   D  M   M   *~
            *   CCC   M   M   SSS   DDDD   BBBB   A   A  DDDD   M   M   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CMSDBADM - CHECKS TO SEE IF THIS USER IS AN ADMINISTRATOR *~
            *            FOR THE DATA BASE                              *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/16/85 ! ORIGINAL                                 ! HES *~
            * 08/13/90 ! Basic 4.3 & SSL Compatibility            ! KAB *~
            * 09/09/96 ! Millie date conversion - UserLCms to 600 ! DER *~
            *************************************************************

            sub "CMSDBADM" (adm$, free$)

        dim                                                              ~
            file$8,                      /* File Name                  */~
            free$16,                     /* For future use             */~
            syslib$8,                    /* SYSTEM LIBRARY             */~
            sysvol$6,                    /* System Volume              */~
            user_mask$(3)4,              /* User access bit masks      */~
            prog_mask$(3)4,              /* Prog access bit masks      */~
            userid$3                     /* USERID THIS USER.          */

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

            adm$ = " "
            call "EXTRACT" addr("ID", userid$,"XL",syslib$,"XV",sysvol$, ~
                                "ME", prog_mask$(1), "MR", prog_mask$(2),~
                                "MW", prog_mask$(3), "UE", user_mask$(1),~
                                "UR", user_mask$(2), "UW", user_mask$(3))

            if pos(user_mask$() <> hex(ff)) = 0 then L02250
            if pos(prog_mask$() <> hex(ff)) <> 0 then L02270
L02250:     adm$ = "Y" : goto L65000

L02270:     call "READFDR" addr("USERLCMS", syslib$, sysvol$, 0%, f2%)
                if f2% <> 0% then L65000
            file$ = "USERLCMS"
            call "PUTNAMES" addr(#1, file$, syslib$, sysvol$)
            call "WORKOPN2" (#1, "SHARE", 0%, f1%)
        REM OPEN NOGETPARM, #1, SHARED, FILE = FILE$,                    ~
                                  LIBRARY = SYSLIB$, VOLUME = SYSVOL$

            REM NOW TRY AND GET DESIRED INFO.
            call "READ100" (#1, userid$, f1%)
                if f1% = 0% then L65000
            get #1, using L02360, adm$
L02360:     FMT XX(359), CH(1)
            close #1

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            end
