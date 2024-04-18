        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EMPPOLL                              *~
            *  Creation Date     - 07/24/2017                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie Norman                      *~
            *                                                           *~
            *  Description       - Poll APD Oracle transaction table &  *~
            *                      update Caelus Employee & Hours       *~
            *-----------------------------------------------------------*~
            *-----------------------------------------------------------*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *07/24/2017! Original                                 ! CMN *~
            *01/31/2019! CR-1821 ADP Increase Emp ID field length ! DES *~
            *03/01/2019! CR-1894 Increate EMP DEPT to 3 bytes     ! DES *~
            *************************************************************


        dim                              /*                            */~
            errormsg$79,                 /* Error message              */~
            hdr$40, msg$(3%)79,          /* Shostat Error Messages     */~
            printmsg$256,                /* Print Message              */~
            server$25,                   /* Connection String          */~
            user$25,                     /* User Name to Connect       */~
            pass$25,                     /* Password to Connect        */~
            error$256,                   /* Error String               */~
            fields$(100)64,              /* Query Return Field         */~
            field$64,                    /* Query Return Field         */~
            stmt1$250,                   /* First Query String         */~
            stmt2$250                    /* Second Query String        */

        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
            rslt$(40%)20,                /* Text from file opening     */~
            filename$8,                  /* File Name                  */~
            volume$6,                    /* DISK VOLUME = CARLOS       */~
            library$8,                   /* Library Name = APCDATA     */~
            vtoc$22                      /* Destination of Return Entry*/

        dim jobid$10,                    /* APD Job ID                 */~
            trans$10,                    /* Transaction Number         */~
            action$1,                    /* Data Action                */~
            file$256,                    /* File to Process            */~
            division$3,                  /* Division Number            */~
            state$2,                     /* State NC or TX             */~
            location$3,                  /* Location Number            */~
            department$4,                /* Corporate Department       */~
            mfgDept$3,                   /* Mfg Department             */~
            badgeID$5,                   /* Badge ID                   */~
            keyval$256,                  /* Key Value                  */~
            colname$256,                 /* Colume Name                */~
            colval$256                   /* Colume Value               */

         dim blankdte$6,                 /* Blank Date                 */~
             dte$6,                      /* System Date                */~
             purgekey$6,                 /* Purge Read Key             */~
             purgeDte$6,                 /* APCEMPDT Purge Date        */~
             rec$128                     /* Purge Rec                  */

        dim logMessage$256, date$, time$8


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            mat f2% = con
            init(" ") rslt$()

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCEMPLY !                                          *~
            * #2  ! APCEMPMT !                                          *~
            * #3  ! APCEMPDT !                                          *~
            * #4  ! GENCODES !                                          *~
            * #5  ! SYSFILE2 ! Caelus Management System General Informa *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCEMPLY",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos = 7,    keylen =  5,                      ~
                        alt key 1, keypos =  1, keylen = 11, dup,        ~
                            key 2, keypos = 12, keylen = 26, dup

            select #2,  "APCEMPMT",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,    keylen =  13,                     ~
                        alt key 1, keypos =  4, keylen = 10, dup,        ~
			    key 2, keypos = 82, keylen =  6, dup

            select #3,  "APCEMPDT",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,    keylen =  18,                     ~
                        alt key 1, keypos = 114, keylen =  6, dup

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,    keylen =  24

            select #5,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #6,  "ADPEMPMT",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 7,    keylen =  23,                     ~
                        alt key 1, keypos = 10, keylen = 23,             ~
                            key 2, keypos =  1, keylen = 29,             ~
                            key 3, keypos = 90, keylen =  6, dup


            select #11, "APCEMPLY",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos = 7,    keylen =  5,                      ~
                        alt key 1, keypos =  1, keylen = 11, dup,        ~
                            key 2, keypos = 12, keylen = 26, dup

            select #12, "APCEMPMT",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,    keylen =  13,                     ~
                        alt key 1, keypos =  4, keylen = 10, dup,    ~
			    key 2, keypos = 82, keylen =  6, dup

            select #13, "APCEMPDT",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,    keylen =  18,                     ~
                        alt key 1, keypos = 114, keylen =  6, dup

            select #14, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,    keylen =  24

            select #16, "ADPEMPMT",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 7,    keylen =  23,                     ~
                        alt key 1, keypos = 10, keylen = 23,             ~
                            key 2, keypos =  1, keylen = 29,             ~
                            key 3, keypos = 90, keylen =  6, dup

        call "OPENCHCK" (#5, fs%(5%), f2%(5%), 25%, rslt$(5%))

        debug% = 0%

        init(" ") errormsg$, hdr$, msg$(), printmsg$, server$, user$, ~
                  pass$, error$, fields$(), field$, stmt1$, stmt2$,   ~
                  volume$, library$, vtoc$, jobid$, trans$, action$,  ~
                  file$, division$, location$, department$, mfgDept$, ~
                  keyval$, colname$, colval$, blankdte$, state$


        chno%, chno1%, chno2% = 0%
        purgemt% = 0%
        dte$ = date
        call "DATE" addr("G+",dte$,-15%,purgeDte$,err%)

        call "DATUFMTC" (blankdte$)
        date$ = dte$
        call "DATEFMT" (date$)
        call "TIME" (time$)

        if debug% = 1% then call "SHOSTAT" ("OPEN FILES")
        gosub openFiles
        if debug% = 1% then call "SHOSTAT" ("ORACLE CONNECT")
        gosub oracle_connect
 
        init(" ") logMessage$
        logMessage$ = "RECORD BEGIN" & date$ & " " & time$
        if debug% = 1 then gosub logIt
REM ==============================
REM  try_again -> read next record
REM ===============================

try_again:

        init(" ") stmt1$, stmt2$
REM                  ....+....1....+....2....+....3....+....4....+....5....+.
          stmt1$  = "SELECT * FROM MSSQL.OE_ADPBUF WHERE FILENAME = ~
                    ~'APCEMPDT' ORDER BY TRANSNBR"

REM             stmt1$  = "SELECT * FROM MSSQL.OE_ADPBUF WHERE FILENAME = ~
REM                       ~'APCEMPMT' ORDER BY TRANSNBR"

        gosub select_data
REM FETCH_NEXT
        gosub fetch_data
          if oci_err% <> 0% and oci_err% <> 100% then FINI
          if oci_err% = 100% then FINI

        init(" ") jobid$, trans$, action$, file$, keyval$, colname$, colval$
        for field_num% = 1%  to no_fields%
          gosub oracle_getfield
          if field_num% = 1%  then jobid$      = field$
          if field_num% = 2%  then trans$      = field$
          if field_num% = 4%  then action$     = field$
          if field_num% = 5%  then file$       = field$
          if field_num% = 6%  then division$   = field$
          if field_num% = 7%  then location$   = field$
          if field_num% = 8%  then department$ = field$
          if field_num% = 9%  then mfgDept$    = field$
          l% = field_num% - 9%
          if (l% > 0%) then fields$(l%) = field$
        next field_num%

        if debug% = 1% then call "SHOSTAT" ("RECORD " & file$ & " " & trans$)
REM ========================
REM  Set state$ NC or TX
REM ========================
        state$ = "TX"
        if division$ = "350" then state$ = "NC"
        if division$ = "360" then state$ = "NC"

        convert mfgDept$ to des%, data goto skip_cnv
        convert des% to mfgDept$, pic(000) 
skip_cnv:
        init(" ") logMessage$
        logMessage$ = "RECORD " & date$ & " " & file$ & " " & trans$ & " " & ~
                 ~ jobid$ & " " & state$ & " " & mfgDept$ & " " & fields$(01%)
        if debug% = 1 then gosub logIt
REM ========================
REM  APCEMPLY
REM ========================
REM  !! ADD IN AT GO LIVE !!
REM    if division$ <> "640" then goto deleteTransaction
REM ========================

        err% = 0%
        if file$ <> "APCEMPLY" then goto NotEmpMstr
          chno% = 1%
          if state$ = "TX" then chno% = 11%
          
REM  IF DIVISION$ = "640" THEN CHNO% = 11%
REM  IF DIVISION$ = "360" THEN GOTO DELETETRANSACTION
REM  IF DIVISION$ = "350" THEN GOTO DELETETRANSACTION
REM =======================================================
REM    Comment out to let in NC Transactions 11/27/2017
REM =======================================================
REM IF STATE$ = "NC" THEN GOTO DELETETRANSACTION

          init(" ") badgeID$
          badgeID$ = fields$(01%)
          gosub checkBadgeID
          if badgeID$ = "00000" then goto deleteTransaction

          init(" ") logMessage$
          logMessage$ = "RECORD APCEMPLY" & date$ & " " & file$ & " " & trans$ ~
            ~ & " " & jobid$ & " " & state$ & " " & mfgDept$ & " " & fields$(01%)
          if debug% = 1 then gosub logIt

          call "ORAEMPLY" (#chno%, jobid$, trans$, action$, file$, division$,~
                           location$, department$, mfgDept$, fields$(), err%)

         if err% <> 0% then goto try_again
        goto deleteTransaction

NotEmpMstr:
REM ========================
REM  APCEMPMT
REM ========================
        err% = 0%
        if file$ <> "APCEMPMT" then goto NotTimeDetl
REM =====================================
REM  Purge Last 14 Days
REM =====================================
        if purgeMT% <> 0% then goto alreadyMTPurge
         err%, pass% = 0%                       /* More than 14 Days Old */
REM Temporary only until NC goes live
REM =======================================================
REM    Comment out to let in NC Transactions 11/27/2017
REM =======================================================
REM PASS% = 1%
         gosub purgeMTRecords
         purgeMT% = 1%

alreadyMTPurge:
          chno% = 2%
          chno1% = 4%
          chno2% = 6%
          if state$ = "TX" then chno%  = 12%
          if state$ = "TX" then chno1% = 14%
          if state$ = "TX" then chno2% = 16%
          
REM IF DIVISION$ = "640" THEN CHNO% = 12%
REM IF DIVISION$ = "640" THEN CHNO1% = 14%
REM IF STATE$ = "NC" THEN GOTO DELETETRANSACTION

          init(" ") badgeID$
          badgeID$ = fields$(01%)
          gosub checkBadgeID
          if badgeID$ = "00000" then goto deleteTransaction
          process% = 1%
          gosub checkField07
          if process% = 1% then goto deleteTransaction
          
          init(" ") logMessage$
          logMessage$ = "RECORD APCEMPMT" & date$ & " " & file$ & " " & trans$ ~
            ~ & " " & jobid$ & " " & state$ & " " & mfgDept$ & " " & fields$(01%)
          if debug% = 1 then gosub logIt          

          call "ORAEMPMT" (#chno%, #chno1%, #chno2%, jobid$, trans$, action$, ~
                           file$, division$, location$, department$, mfgDept$,~
                           fields$(), err%)

          if err% <> 0% then goto try_again
        goto deleteTransaction

NotTimeDetl:
REM ========================
REM  APCEMPDT
REM ========================
        err% = 0%
        if file$ <> "APCEMPDT" then goto NotPuchDetl
REM =====================================
REM  Purge Last 14 Days
REM =====================================
        if purgeDT% <> 0% then goto alreadyDTPurge
         err%, pass% = 0%                       /* More than 14 Days Old */
REM Temporary only until NC goes live
REM =======================================================
REM    Comment out to let in NC Transactions 11/27/2017
REM =======================================================
REM PASS% = 1%
         gosub purgeDTRecords
         purgeDT% = 1%

alreadyDTPurge:
        chno% = 3%
        chno1% = 4%
REM  IF FIELDS$(01%) <= " " THEN FIELDS$(01%) = "00000"
          init(" ") badgeID$
          badgeID$ = fields$(01%)
          gosub checkBadgeID
          if badgeID$ = "00000" then goto deleteTransaction
          if state$ = "TX" then chno%  = 13%
          if state$ = "TX" then chno1% = 14%
          
REM IF STATE$ = "NC" THEN GOTO DELETETRANSACTION
REM IF DIVISION$ = "640" THEN CHNO% = 13%
REM IF DIVISION$ = "640" THEN CHNO1% = 14%
REM IF DIVISION$ = "360" THEN GOTO DELETETRANSACTION
REM IF DIVISION$ = "350" THEN GOTO DELETETRANSACTION

          init(" ") logMessage$
          logMessage$ = "RECORD APCEMPMT" & date$ & " " & file$ & " " & trans$ ~
             ~ & " " & jobid$ & " " & state$ & " " & mfgDept$ & " " & fields$(01%)
          if debug% = 1 then gosub logIt

          call "ORAEMPDT" (#chno%, #chno1%, jobid$, trans$, action$, file$,~
                           division$, location$, department$, mfgDept$,     ~
                           fields$(), err%)

          if err% <> 0% then goto try_again
        goto deleteTransaction
NotPuchDetl:


deleteTransaction:
        init(" ") stmt1$, stmt2$
REM                 ....+....1....+....2....+....3....+....4....+....5....+.
          stmt1$ = "DELETE FROM MSSQL.OE_ADPBUF WHERE TRANSNBR = " & trans$
          gosub oracle_exec
          stmt1$  = "COMMIT "
          gosub oracle_exec

        goto try_again

        oracle_connect
          init(" ") user$, pass$, server$
REM USER$   = "MSSQL"
REM PASS$   = "MSSQL"
REM SERVER$ = "WWDEV"
           call "READ100" (#5, "ORACLE PASSWORD", f1%(5%))   /* SYSFILE2 */
           if f1%(5%) <> 0% then get #5 using ORCL_PSWD, user$, pass$
ORCL_PSWD:       FMT POS(21), CH(50), POS(50)

           oci_err% = 0%
           call "CONNECT" (user$, pass$, server$, oci_err%)

           if oci_err% >= 0% then goto exit_con
           errormsg$ = "YOU ARE NOT CONNECTED TO ORACLE, CONTACT SYSTEMS!!!!"
           gosub error_prompt
exit_con:
        return

        error_prompt
REM COMP% = 2%
          hdr$ = "******* (Error) (Error) (Error)  *******"
          msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
          msg$(2%) = errormsg$
          msg$(3%) = "Press Any Key To Continue."
          print errormsg$
REM CALL "ASKUSER" (COMP%, HDR$, MSG$(1%), MSG$(2%), MSG$(3%))
          goto FINI
        return

        select_data
          gosub oracle_flush
          gosub oracle_query
        return

        fetch_data
          gosub oracle_fetch
        return

        oracle_query
          oci_err% = 0%
          call "QUERY" (stmt1$, stmt2$, oci_err%)
        return

        oracle_exec
          gosub oracle_flush
          oci_err% = 0%
          call "EXEC" (stmt1$, stmt2$, oci_err%)
        return

        oracle_flush
          oci_err% = 0%
          call "FLUSH" (oci_err%)
        return

        oracle_fetch
          oci_err% = 0%
          no_fields% = 0%
          call "FETCH" (no_fields%, oci_err%)
        return

        oracle_getfield
          oci_err% = 0%
          field_len% = 0%
          init(" ") field$, name$
          call "FIELDINF" (field_num%, field$, name$, field_len%, oci_err%)
        return

REM ************************
REM ========================
REM  ROUTINES
REM ========================
REM ************************


REM ========================
REM  checkBadgeID
REM ========================
        checkBadgeID
          empid% = 0%
          convert badgeID$ to empid%, data goto badEmpID

badEmpID:
          convert empid% to fields$(01%), pic(00000)
          badgeID$ = fields$(01%)
        return

REM ========================
REM  checkPayCode - FIELD07
REM ========================
        checkField07
           if fields$(06%) = "*rg" then process% = 0%
           if fields$(06%) = "*OT" then process% = 0%
        return

REM ========================
REM  purgeRecords
REM ========================
        purgeDTRecords
          if debug% = 1% then call "SHOSTAT" ("DT PURGE")
          chno% = 3%
          if pass% = 1% then chno% = 13%
          
          init(" ") logMessage$
          logMessage$ = "RECORD DTPURGE" & date$ & " " & file$ & " " & trans$ ~
            ~ & " " & jobid$ & " " & state$ & " " & mfgDept$ & " " & fields$(01%) 
          if debug% = 1 then gosub logIt
          
          purgekey$ = all(hex(00))
          str(purgekey$,1%,6%) = purgeDte$
          read #chno%, hold, key 1% >= purgekey$, using PURGEDT_FMT, rec$,  ~
                                                        eod goto purgeDTDone
PURGEDT_FMT:      FMT CH(128)
           goto purgeDTFirst
        purgeDTNxt:
          read #chno%, hold, using PURGEDT_FMT, rec$, eod goto purgeDTDone

        purgeDTFirst:
          purgekey$ = str(rec$,114%,6%)    /*Don't purge if not applied Date*/
          if purgekey$ = " " then goto purgeDTNxt
          if blankdte$ = purgekey$ then goto purgeDTNxt
          if purgeDte$ > purgekey$ then goto purgeDTNxt

             delete #chno%

          goto purgeDTNxt
        purgeDTDone
          if pass% >= 1% then return
          pass% = pass% + 1%
          goto purgeDTRecords
          if debug% = 1% then call "SHOSTAT" ("DT PURGE DONE")
        return


        purgeMTRecords
          if debug% = 1% then call "SHOSTAT" ("MT PURGE")
          chno% = 6%
          if pass% = 1% then chno% = 16%
          
          init(" ") logMessage$
          logMessage$ = "RECORD DTPURGE" & date$ & " " & file$ & " " & trans$ ~
             ~ & " " & jobid$ & " " & state$ & " " & mfgDept$ & " " & fields$(01%) 
                        
          purgekey$ = all(hex(00))
          str(purgekey$,1%,6%) = purgeDte$
          read #chno%, hold, key 3% >= purgekey$, using PURGEMT_FMT, rec$,  ~
                                                        eod goto purgeMTDone
PURGEMT_FMT:      FMT CH(128)
           goto purgeMTFirst
        purgeMTNxt:
          read #chno%, hold, using PURGEMT_FMT, rec$, eod goto purgeMTDone
    
        purgeMTFirst:
          purgekey$ = str(rec$,90%,6%)    /*Don't purge if not applied Date*/
          if purgekey$ = " " then goto purgeMTNxt
          if blankdte$ = purgekey$ then goto purgeMTNxt
          if purgeDte$ >= purgekey$ then goto purgeMTNxt

             delete #chno%

          goto purgeMTNxt
        purgeMTDone
          if pass% >= 1% then return
          pass% = pass% + 1%
          goto purgeMTRecords
          if debug% = 1% then call "SHOSTAT" ("MT PURGE DONE")
        return

REM ========================
REM  debug Log File
REM ========================
        logIt
          call "ADPLOG" (logMessage$)
        return
        
REM ========================
REM  openFiles NC and TX
REM ========================
        openFiles
          init(" ") filename$, library$, volume$, vtoc$
          filename$ = "APCEMPLY"
          library$  = "APCDATA"
          chno% = 1%
          gosub openSpecial

          init(" ") filename$, library$, volume$, vtoc$
          filename$ = "APCEMPMT"
          library$  = "APCDATA"
          chno% = 2%
          gosub openSpecial

          init(" ") filename$, library$, volume$, vtoc$
          filename$ = "APCEMPDT"
          library$  = "APCDATA"
          chno% = 3%
          gosub openSpecial

          init(" ") filename$, library$, volume$, vtoc$
          filename$ = "GENCODES"
          library$  = "APCDATA"
          chno% = 4%
          gosub openSpecial

          init(" ") filename$, library$, volume$, vtoc$
          filenADP$ = "ADPEMPMT"
          library$  = "APCDATA"
          chno% = 6%
          gosub openSpecial

REM =======================================================
REM        TX FILES                                          
REM =======================================================
          init(" ") filename$, library$, volume$, vtoc$
          filename$ = "APCEMPLY"
          library$  = "NEDATA"
          chno% = 11%
          gosub openSpecial

          init(" ") filename$, library$, volume$, vtoc$
          filename$ = "APCEMPMT"
          library$  = "NEDATA"
          chno% = 12%
          gosub openSpecial

          init(" ") filename$, library$, volume$, vtoc$
          filename$ = "APCEMPDT"
          library$  = "NEDATA"
          chno% = 13%
          gosub openSpecial

          init(" ") filename$, library$, volume$, vtoc$
          filename$ = "GENCODES"
          library$  = "NEDATA"
          chno% = 14%
          gosub openSpecial

          init(" ") filename$, library$, volume$, vtoc$
          filename$ = "ADPEMPMT"
          library$  = "NEDATA"
          chno% = 16%
          gosub openSpecial
        return

REM ========================
REM  openSpecial
REM ========================
        openSpecial
          x% = 1%
          volume$   = "?"
          call "FIND" addr(filename$, library$, volume$,1%,x%,vtoc$)
          if vtoc$ = " " then goto fileError
            volume$ = vtoc$
            open nogetparm #chno%,                                   ~
                 shared,                                             ~
                 file    = filename$,                                ~
                 library = library$,                                 ~
                 volume  = volume$
        return


fileError:
FINI:
          end











