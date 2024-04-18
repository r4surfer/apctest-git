        REM *************************************************************~
            *                                                           *~
            *  DDDD   EEEEE  L      EEEEE  TTTTT  EEEEE                 *~
            *  D   D  E      L      E        T    E                     *~
            *  D   D  EEEE   L      EEEE     T    EEEE                  *~
            *  D   D  E      L      E        T    E                     *~
            *  DDDD   EEEEE  LLLLL  EEEEE    T    EEEEE                 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DELETE -- DELETES A SINGLE RECORD OR RANGE OF RECORDS     *~
            *           STARTING FROM A CONTROL BREAK (PRIMARY KEY).    *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/14/80 ! ORIGINAL                                 ! BCW *~
            * 09/21/80 ! "GETUFBWA" SUB INSTEAD OF BASIC "GET"    ! BCW *~
            * 03/11/81 ! UPDATE FOR OFFSET KEY LENGTH             ! BCW *~
            * 07/11/83 ! CHANGE KEY LEN TO 100                    ! GLW *~
            * 01/03/85 ! PICK UP BI(4), CHECK KEYLEN, RD SEQ LOOP ! KAB *~
            *************************************************************

            sub "DELETE" (#1, key$, keylen%)

                dim key$255,             /* STARTING KEY TO DELETE     */~
                    oldreadkey$255,      /* KEY FOR PLOW ROUTINE TO DEL*/~
                    newreadkey$255       /* AND KEY FOR DELETE THING.  */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.14.11 01/11/85 Openfile routines              "
        REM *************************************************************
            if keylen% < 0% then end
            if keylen% > 255% then end

            oldreadkey$ = key$

            filekeylen% = keylen%
            if filekeylen% = 0% then filekeylen% = len(key$)
            if filekeylen% = 255% then L00380
            init(hex(00)) str(oldreadkey$, filekeylen% + 1)

L00380:     REM GET KEY LENGTH FROM UFB AND DECODE IT.
                call "GETUFBKL" addr (#1, keylen$)
                filekeylen% = val (keylen$)

            REM IN CASE KEYLEN = FILEKEYLEN, READ = THEN DELETE.
                if keylen% = filekeylen% then L00570

            REM NOW THAT EVERYTHING'S SET UP, START DELETING.
                call"READ105"(#1,str(oldreadkey$,1,filekeylen%),f1%)
L00470:              if f1% = 0 then end
                newreadkey$ = key(# 1)
                if keylen% = 0% then L00520
                if str(oldreadkey$, 1, keylen%) <>                       ~
                     str(newreadkey$, 1, keylen%) then L00630
L00520:         delete #1
                oldreadkey$ = str(newreadkey$, 1, filekeylen%)
                call "READNXT1" (#1, f1%)
                goto L00470

L00570:    REM HANDLES CASE WHERE WE'RE DELETING ONE (KEYLEN=FILEKEYLEN)
               call"READ101"(#1, str(oldreadkey$,1,filekeylen%), f1%)
                    if f1% = 0 then end
               delete #1
               end

L00630:    REM RELEASE RECORD
               call "STRTRLSE" addr(#1)
               end

