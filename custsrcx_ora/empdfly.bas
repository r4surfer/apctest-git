        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EMPDFLY                              *~
            *  Creation Date     - 06/20/2020                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie Norman                      *~
            *                                                           *~
            *  Description       - Poll APD Oracle transaction table &  *~
            *                      update Caelus Employee & Hours       *~
            *        NC                                                 *~
            *        chno% = 2%     APCEMPMT                            *~
            *        chno1% = 4%    GENCODES                            *~
            *        chno2% = 6%	DFEMPMT                             *~
            *                                                           *~
            *        TX                                                 *~
            *        chno% = 12%	APCEMPMT                            *~
            *        chno1% = 14%	GENCODES                            *~
            *        chno2% = 16%	DFEMPMT                             *~
            *-----------------------------------------------------------*~
            *-----------------------------------------------------------*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *06/20/2020! Original                                 ! CMN *~
            *************************************************************


        dim                              /*                            */~
            errormsg$79,                 /* Error message              */~
            hdr$40, msg$(3%)79,          /* Shostat Error Messages     */~
            server$25,                   /* Connection String          */~
            user$25,                     /* User Name to Connect       */~
            pass$25,                     /* Password to Connect        */~
            error$256,                   /* Error String               */~
            fields$(100)64,              /* Query Return Field         */~
            field$64,                    /* Query Return Field         */~
            stmt1$250,                   /* First Query String         */~
            stmt2$250,                   /* Second Query String        */~
            oci_err$10                   /* String OCI Error Message   */

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
            location$6,                  /* Location Number            */~
            department$6,                /* Corporate Department       */~
            mfgDept$3,                   /* Mfg Department             */~
            badgeID$8,                   /* Badge ID                   */~
            keyval$256,                  /* Key Value                  */~
            colname$256,                 /* Colume Name                */~
            colval$256                   /* Colume Value               */

         dim blankdte$6,                 /* Blank Date                 */~
             dte$6,                      /* System Date                */~
             purgekey$6,                 /* Purge Read Key             */~
             purgeDte$6,                 /* Purge Date                 */~
             purgeDate$8,                /* Formated Purge Date        */~
             rec$128,                    /* Purge Rec                  */~
             mindte$10                   /* Oldest Date in Table       */
             

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
                        alt key 1, keypos =  4, keylen = 10, dup,        ~     
                            key 2, keypos = 82, keylen =  6, dup

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

        init(" ") errormsg$, hdr$, msg$(), server$, user$,            ~
                  pass$, error$, fields$(), field$, stmt1$, stmt2$,   ~
                  volume$, library$, vtoc$, jobid$, trans$, action$,  ~
                  file$, division$, location$, department$, mfgDept$, ~
                  keyval$, colname$, colval$, blankdte$, state$


        chno%, chno1%, chno2% = 0%
        purgemt% = 0%
        
        gosub initialize
REM ==============================
REM  try_again -> read next record
REM ===============================

try_again:

        init(" ") stmt1$, stmt2$
REM                  ....+....1....+....2....+....3....+....4....+....5....+.
REM       stmt1$  = "SELECT * FROM MSSQL.OE_DFBUF WHERE FILENAME <> ~
                    ~'APCEMPDT' ORDER BY TRANSNBR"

          stmt1$  = "SELECT * FROM MSSQL.OE_DFBUF WHERE FILENAME =  ~
                    ~'APCEMPLY' ORDER BY TRANSNBR"

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
     state$ = " "
     if division$ = "350" then state$ = "NC"         /*NC Extrusion     */
     if division$ = "360" then state$ = "NC"         /*NC Welcome       */
     if division$ = "340" then state$ = "TX"         /*TX AES GreenVille*/
     if division$ = "640" then state$ = "TX"         /*TX Dallas        */
     if state$ = " " then goto deleteTransaction   
     
        if department$ = "Histor" then department$ = "9999"
        if mfgDept$ = " " then mfgDept$ = "999"        

        dept% = 0%
        convert mfgDept$ to dept%, data goto skip_cnv

        convert dept% to mfgDept$, pic(000)
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

REM !! remove     
          fields$(21%) = "01-1st Shift"   /* Default Shift */

          init(" ") badgeID$
          badgeID$ = fields$(01%)
          gosub checkBadgeID
          if badgeID$ = "00000000" then goto deleteTransaction

          init(" ") logMessage$
          logMessage$ = "RECORD APCEMPLY" & date$ & " " & file$ & " " & trans$ ~
            ~ & " " & jobid$ & " " & state$ & " " & mfgDept$ & " " & fields$(01%)
          if debug% = 1 then gosub logIt


          call "ORADFLY" (#chno%, jobid$, trans$, action$, file$, division$,~
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
REM CALL "SHOSTAT" ("PURGE MT ")  STOP
         gosub purgeMTRecords
         err%, pass% = 0%    /*More than 14 Days Old;Reset pass for DFEMPMT*/         
         gosub purgeMTDFRecords         
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

                                                   /* Dave McBride AES TX */
          if state$ = "TX" and mfgDept$ <> "018" then goto deleteTransaction 

          init(" ") badgeID$
          badgeID$ = fields$(01%)
          gosub checkBadgeID
          if badgeID$ = "00000000" then goto deleteTransaction
          process% = 1%
          gosub checkField06            /* hourscode */
          if process% = 1% then goto deleteTransaction
          
          init(" ") logMessage$
          logMessage$ = "RECORD APCEMPMT" & date$ & " " & file$ & " " & trans$ ~
            ~ & " " & jobid$ & " " & state$ & " " & mfgDept$ & " " & fields$(01%)
          if debug% = 1 then gosub logIt          

          call "ORADFMT" (#chno%, #chno1%, #chno2%, jobid$, trans$, action$, ~
                           file$, division$, location$, department$, mfgDept$,~
                           fields$(), err%)

          if err% <> 0% then goto try_again
        goto deleteTransaction

NotTimeDetl:

deleteTransaction:
        init(" ") stmt1$, stmt2$
REM                 ....+....1....+....2....+....3....+....4....+....5....+.
          stmt1$ = "DELETE FROM MSSQL.OE_DFBUF WHERE TRANSNBR = " & trans$
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

        
        

REM ************************
REM ========================
REM  ROUTINES
REM ========================
REM ************************
        
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
          gosub oracle_flush        
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

        oracle_error
        return
           call "ERROR" (error$)
           call "SHOSTAT" ("ERROR ERROR RETURN VALUE --> " & ERROR$)
           stop       
        return


REM ************************
REM ========================
REM   INITIALIZE
REM ************************
REM ========================
    initialize
       if debug% = 1% then call "SHOSTAT" ("OPEN FILES")
       gosub openFiles    
    
REM       dte$ = date
REM       call "DATE" addr("G+",dte$,-14%,purgeDte$,err%)
REM       call "DATE" addr("G+",dte$,-14%,purgeDte$,err%)

       call "DATUFMTC" (blankdte$)
       date$ = dte$
       call "DATEFMT" (date$)
       call "DATFMTC" (purgeDate$)
       call "TIME" (time$)
        
       if debug% = 1% then call "SHOSTAT" ("ORACLE CONNECT")
       gosub oracle_connect
 
       init(" ") logMessage$
       logMessage$ = "RECORD BEGIN" & date$ & " " & time$
       if debug% = 1 then gosub logIt

       oci_err$ = "0"
       init(" ") stmt1$
       stmt1$ = "CALL MSSQL.PROC_DF_APCEMPLY()"
       gosub oracle_exec       

       if oci_err% <> 0% then gosub oracle_error
   
       stmt1$ = "COMMIT"
       gosub oracle_exec
       
REM    oci_err$ = "0"       
REM    init(" ") stmt1$
REM    stmt1$ = "CALL MSSQL.PROC_DF_APCEMPMT()"
REM    gosub oracle_exec 

REM    if oci_err% <> 0% then gosub oracle_error

REM    stmt1$ = "COMMIT"
REM    gosub oracle_exec       
       
REM    gosub getMinDate
REM    mindte$ = field$
REM    call "DATUFMTC" (mindte$)
REM    purgeDte$ = str(mindte$,1%,6%)       

    return
REM ========================
REM   MIN DATE
REM ========================
    getMinDate
        init(" ") stmt1$, stmt2$
REM                  ....+....1....+....2....+....3....+....4....+....5....+.
          stmt1$  = "select min(field7) as mindate from mssql.oe_dfbuf ~
                    ~where filename <> 'APCEMPDT'"

        gosub select_data
REM FETCH_NEXT
        gosub fetch_data
          if oci_err% <> 0% and oci_err% <> 100% then FINI
          if oci_err% = 100% then FINI    
        field_num% = 1%  
        gosub oracle_getfield
    return
REM ========================
REM  checkBadgeID
REM ========================
    checkBadgeID
      empid% = 0%
      convert badgeID$ to empid%, data goto badEmpID

badEmpID:
      convert empid% to fields$(01%), pic(00000000)
      badgeID$ = fields$(01%)
    return

REM ========================
REM  checkPayCode - FIELD06
REM ========================
    checkField06
       if str(fields$(06%),1%,3%) = "REG" then fields$(06%) = "*rg"
                                     
       if str(fields$(06%),1%,5%) = "OT1.5" then fields$(06%) = "*OT"
       
       if fields$(06%) = "*rg" then process% = 0%
       if fields$(06%) = "*OT" then process% = 0%
    return

REM ************************
REM ========================    
REM  purgeRecords
REM ========================
REM ************************
    
REM ========================    
REM  MT Purge
REM ========================
    
    purgeMTRecords
      if debug% = 1% then call "SHOSTAT" ("MT PURGE")
      chno% = 2%
      if pass% = 1% then chno% = 12%
      
      init(" ") logMessage$
      logMessage$ = "RECORD MTPURGE" & date$ & " " & file$ & " " & trans$ ~
         ~ & " " & jobid$ & " " & state$ & " " & mfgDept$ & " " & fields$(01%) 
                    
      purgekey$ = all(hex(00))
      str(purgekey$,1%,6%) = purgeDte$
      
      read #chno%, hold, key 2% >= purgekey$, using PURGEMT_FMT, rec$,  ~
                                                    eod goto purgeMTDone
PURGEMT_FMT:      FMT CH(128)

           goto purgeMTFirst
    purgeMTNxt:
      read #chno%, hold, using PURGEMT_FMT, rec$, eod goto purgeMTDone

    purgeMTFirst:
      purgekey$ = str(rec$,82%,6%)    /*Don't purge if not applied Date*/
      if purgekey$ = " " then goto purgeMTNxt
      if blankdte$ = purgekey$ then goto purgeMTNxt
      if purgeDte$ > purgekey$ then goto purgeMTNxt

         delete #chno%

      goto purgeMTNxt
    purgeMTDone
      if pass% >= 1% then return
      pass% = pass% + 1%
      goto purgeMTRecords
      if debug% = 1% then call "SHOSTAT" ("MT PURGE DONE")
    return    

REM ========================    
REM  MT DF Purge
REM ========================

    purgeMTDFRecords
      if debug% = 1% then call "SHOSTAT" ("MT PURGE")
      chno% = 6%
      if pass% = 1% then chno% = 16%
      
      init(" ") logMessage$
      logMessage$ = "RECORD MTDFPURGE" & date$ & " " & file$ & " " & trans$ ~
         ~ & " " & jobid$ & " " & state$ & " " & mfgDept$ & " " & fields$(01%) 
                    
      purgekey$ = all(hex(00))
      str(purgekey$,1%,6%) = purgeDte$
      read #chno%, hold, key 3% >= purgekey$, using PURGEDFMT_FMT, rec$,  ~
                                                    eod goto purgeDFMTDone
PURGEDFMT_FMT:      FMT CH(128)
           goto purgeDFMTFirst
    purgeDFMTNxt:
      read #chno%, hold, using PURGEMT_FMT, rec$, eod goto purgeDFMTDone

    purgeDFMTFirst:
      purgekey$ = str(rec$,90%,6%)    /*Don't purge if not applied Date*/
      if purgekey$ = " " then goto purgeDFMTNxt
      if blankdte$ = purgekey$ then goto purgeDFMTNxt
      if purgeDte$ > purgekey$ then goto purgeDFMTNxt

         delete #chno%

      goto purgeDFMTNxt
    purgeDFMTDone
      if pass% >= 1% then return
      pass% = pass% + 1%
      goto purgeMTDFRecords
      if debug% = 1% then call "SHOSTAT" ("MT DF PURGE DONE")
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
      filename$ = "GENCODES"
      library$  = "APCDATA"
      chno% = 4%
      gosub openSpecial

      init(" ") filename$, library$, volume$, vtoc$
      filename$ = "ADPEMPMT"
      library$  = "APCDATA"
      chno% = 6%
      gosub openSpecial


REM =======================================================
REM    TX FILES Dave McBride AES
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







