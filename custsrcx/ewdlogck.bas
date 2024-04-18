REM         *-----------------------------------------------------------*~
            * EWDLOGCK - Compares EWDXOLOG recs to USERLCMS recs.       *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/10/99 ! ORIGINAL - Copied & Mod Sub EWDLOGOU.    ! BWS *~
            * 03/23/99 ! (EWD001) Add ability to delete log recs. ! BWS *~
            * 04/05/99 ! (EWD002) Mods to display active pgm name.! BWS *~
            *          !  Also to display all recs dated B4 today.!     *~
            *************************************************************

        dim                                                              ~
            file$8,                      /* File Name                  */~
            syslib$8,                    /* SYSTEM LIBRARY             */~
            sysvol$6,                    /* System Volume              */~
            userid$3,                    /* UserId This User           */~
            readkey$50,                  /* Key to read EWDXOLOG       */~
            lastdt$10,                   /* Last Logon Date - Log File */~
            lasttm$8,                    /* Last Logon Time - Log File */~
            usrldt$6,                    /* Last Logon Date - USERLCMS */~
            usrltm$6,                    /* Last Logon Time - USERLCMS */~
            prog$8,                      /* Active Program Nm/' '=None */~
/*EWD002*/  date$10                      /* Current Date (Fmtd/Unfmtd) */~

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

            date$ = date                /*EWD002*/
            call "DATFMTC"  (date$)     /*EWD002*/
            call "DATUFMTC" (date$)     /*EWD002*/
            call "EXTRACT" addr("XL",syslib$,"XV",sysvol$)
            call "READFDR" addr("USERLCMS", syslib$, sysvol$, 0%, f2%)
                if f2% <> 0% then L65000
            file$ = "USERLCMS"
            call "PUTNAMES" addr(#1, file$, syslib$, sysvol$)
            call "WORKOPN2" (#1, "SHARE", 0%, f1%)

            call "OPENCHCK" (#2, f1%, f2%, 500%, " ")

            REM NOW TRY AND GET DESIRED INFO.
            readkey$ = all(hex(00))
next_rec:   read #2, hold, key > readkey$, using L00400, readkey$, prog$,~
                eod goto L65000             /*  ^ EWD001 ^ ... EWD002 ^ */
L00400:             fmt ch(15), ch(8)                       /* EWD002 */
            userid$ = str(readkey$,,3%)
            lastdt$ = str(readkey$,4%,6%)
            lasttm$ = str(readkey$,10%,6%)
            call "READ100" (#1, userid$, f1%)
                if f1% = 0% then next_rec
            get #1, using L00500, usrltm$, usrldt$
L00500:         fmt pos(39), ch(6), pos(402), ch(6)
            if lastdt$ <> date$ then goto L00510            /*EWD002*/
            if usrltm$ = lasttm$ and usrldt$ = lastdt$ then next_rec
L00510:     call "DATFMTC" (lastdt$)
            lasttm$ = str(lasttm$,,4%) & "  "
            call "TIME" (lasttm$)

L00550:     k% = 2%                         /* EWD001 - Begin */
            call "ASKUSER" (k%,"EWDLOGCK",                               ~
                "User ID: "& userid$ &", Date: "& lastdt$ &", Time: " &  ~
/*EWD002*/      lasttm$, "Active Program (' '= None/UnTracked): "& prog$,~
                "Press <ENTER> to Continue, <PF1> to Delete Log Record")  
            if k% = 1% then delete #2
            if k% > 1% then goto L00550     /* EWD001 - End */
            goto next_rec

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            end


