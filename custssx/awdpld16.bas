
        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - AWDPLD16                             *~
            *  Creation Date     - 04/05/2018                           *~
            *  Last Modified Date- 03/17/2011                           *~
            *  Written By        - Christie Norman                      *~
            *  Modifications By  -                                      *~
            *                                                           *~
            *  Description       - Create Glass Batch Files             *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            * Tables Used                                               *~
            *             GED SCHED                                     *~
            *             GED SCHE1                                     *~
            *             GED SCHE2 - Pre-cut1                          *~
            *             GED SCHE3 - Pre-cut2                          *~
            * This Subroutine is used to TB Sort Options                *~
            *   FileID in GED SCHED1 should begin with 1                *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *04/05/2018! Original -                               ! CMN *~
            *************************************************************

        sub "AWDPLD16" (#1, #2, pre%, ds_merge%, schema%)
                                           /* Return Code                */


        dim                              /*                            */~
            hdr$40,                      /* Askuser Header             */~
            msg$(3%)79,                  /* Askuser Messages           */~
            filename$8,                  /* File Name for Open         */~
            library$8,                   /* Library Name = 'APCDATA'   */~
            volume$6                     /* Volume Name = 'CARLOS'     */

        dim f2%(99%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(99%)20                 /* Text from file opening     */

        dim readkey$24,                  /* GENCODES Readkey           */~
            desc$30                      /* GENCODES Description       */

        dim name$9,                      /* Batch File Names           */~
            batch1%(3%)                  /* Batch Sizes                */

        dim gls_read$(2%)192,            /* GLS GED Read key           */~
            gls_sort$11,                 /* GED SCHE1 Readkey          */~
            neu_key$70,                  /* New Key                    */~
            cnt$6,                       /* GLS GED Count              */~
            sav_read$(2%)192,            /* GLS GED Read key           */~
            gls_hdr$(2%)192              /* GLS Header Record          */

        dim intercept$2,                 /* intercept                  */~
            file$3                       /* File Name                  */



        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! @GLSGED@ ! Glass Batch File for GED Glass System    *~
            * #2  ! GENCODES ! Master Code Tables File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #3, "GLSSRT",                                        ~
                        varc,     indexed,  recsize =   512,            ~
                        keypos =    1, keylen =   70

            select #04, "CHRISTIE", consec, recsize = 384
            select #05, "CHRISTIE", consec, recsize = 384
            select #06, "CHRISTIE", consec, recsize = 384
            select #07, "CHRISTIE", consec, recsize = 384
            select #08, "CHRISTIE", consec, recsize = 384
            select #09, "CHRISTIE", consec, recsize = 384
            select #10, "CHRISTIE", consec, recsize = 384
            select #11, "CHRISTIE", consec, recsize = 384
            select #12, "CHRISTIE", consec, recsize = 384
            select #13, "CHRISTIE", consec, recsize = 384
            select #14, "CHRISTIE", consec, recsize = 384
            select #15, "CHRISTIE", consec, recsize = 384
            select #16, "CHRISTIE", consec, recsize = 384
            select #17, "CHRISTIE", consec, recsize = 384
            select #18, "CHRISTIE", consec, recsize = 384
            select #19, "CHRISTIE", consec, recsize = 384
            select #20, "CHRISTIE", consec, recsize = 384
            select #21, "CHRISTIE", consec, recsize = 384
            select #22, "CHRISTIE", consec, recsize = 384
            select #23, "CHRISTIE", consec, recsize = 384
            select #24, "CHRISTIE", consec, recsize = 384
            select #25, "CHRISTIE", consec, recsize = 384
            select #26, "CHRISTIE", consec, recsize = 384
            select #27, "CHRISTIE", consec, recsize = 384
            select #28, "CHRISTIE", consec, recsize = 384
            select #29, "CHRISTIE", consec, recsize = 384
            select #30, "CHRISTIE", consec, recsize = 384

            select #31, "CHRISTIE", consec, recsize = 384
            select #32, "CHRISTIE", consec, recsize = 384
            select #33, "CHRISTIE", consec, recsize = 384
            select #34, "CHRISTIE", consec, recsize = 384
            select #35, "CHRISTIE", consec, recsize = 384
            select #36, "CHRISTIE", consec, recsize = 384
            select #37, "CHRISTIE", consec, recsize = 384
            select #38, "CHRISTIE", consec, recsize = 384
            select #39, "CHRISTIE", consec, recsize = 384
            select #40, "CHRISTIE", consec, recsize = 384

            select #41, "CHRISTIE", consec, recsize = 384
            select #42, "CHRISTIE", consec, recsize = 384
            select #43, "CHRISTIE", consec, recsize = 384
            select #44, "CHRISTIE", consec, recsize = 384
            select #45, "CHRISTIE", consec, recsize = 384
            select #46, "CHRISTIE", consec, recsize = 384
            select #47, "CHRISTIE", consec, recsize = 384
            select #48, "CHRISTIE", consec, recsize = 384
            select #49, "CHRISTIE", consec, recsize = 384
            select #50, "CHRISTIE", consec, recsize = 384
            select #51, "CHRISTIE", consec, recsize = 384
            select #52, "CHRISTIE", consec, recsize = 384
            select #53, "CHRISTIE", consec, recsize = 384
            select #54, "CHRISTIE", consec, recsize = 384
            select #55, "CHRISTIE", consec, recsize = 384
            select #56, "CHRISTIE", consec, recsize = 384
            select #57, "CHRISTIE", consec, recsize = 384
            select #58, "CHRISTIE", consec, recsize = 384
            select #59, "CHRISTIE", consec, recsize = 384
            select #60, "CHRISTIE", consec, recsize = 384
            select #61, "CHRISTIE", consec, recsize = 384

            gosub openFile
            pre%, ds_merge%, err%, srtNme% = 0% 

            gosub read_glsged
REM            call "SHOSTAT" ("READ GLSGED DONE" )  stop
            if cnt% <= 0% then goto exit_sub  
            gosub read_work_file
            goto exit_sub            
            
            
REM=================================================================
REM   SORT GLASS TO WORKFILE
REM=================================================================
        read_glsged                         /* Create Work File */
          call "SHOSTAT" (" Sorting Glass Data ")
REM          mode% = 1%
REM          gosub OPEN_WORK
REM          mode% = 3%
REM          gosub OPEN_WORK

          init(" ") gls_read$(), sav_read$(), neu_key$
          cnt%, temp% = 0%
        gls_nxt
          read #1, using L00500, gls_read$(), eod goto gls_done

L00500:                  FMT 2*CH(192)



REM READNXT SETS -> SAV_READ$() = GLS_READ$() & UPDATES GLS_READ$() &
REM SETS NEXT% = 1%
          next% = 0%
          if str(gls_read$(),1%,1%) = "#" then goto gls_nxt        
          if str(gls_read$(),1%,1%) = "<" then gosub readNxt
          if temp% = 1% then goto gls_nxt
          
            gosub addCntr
            gosub setGLSKey
            gosub writeWork
            goto gls_nxt
        gls_done
REM CALL "SHOSTAT" ("SORT DONE") 
REM STOP
        return

        writeWork
          if next% = 0% then goto writeCurWrk
          put #3, using WORKFILE_FMT, neu_key$, sav_read$(), eod goto wrte_done
          
          str(neu_key$,04%,03%) = str(gls_read$(),110%,03%) /*GED SCHED  */ 
          next% = 0%
          init(" ") sav_read$()
          gosub writeRec
          
          
writeCurWrk:          

          put #3, using WORKFILE_FMT, neu_key$, gls_read$(), eod goto wrte_done

WORKFILE_FMT:   FMT CH(70), 2*CH(192)

writeRec:
          write #3

        wrte_done
        return

          
        readNxt
          sav_read$() = gls_read$()
          read #1, using L00500, gls_read$(), eod goto read_nxtDone

                    
          if str(gls_read$(),81%,04%) = "Temp" or  ~
             str(gls_read$(),81%,04%) = "TEMP" then temp% = 1%
          next% = 1%
        read_nxtDone
        return
        setGLSKey
          if str(gls_read$(),1%,1%) = "*" then gosub setHdr
        

            hdrcnt% = hdrcnt% + 1%
REM 06 -> Record Counter
          str(neu_key$,57%,06%) = cnt$
          str(neu_key$,64%,01%) = " "
        return
        addCntr
          cnt% = cnt% + 1%
          convert cnt% to cnt$, pic(000000)
        return            
        setHdr
REM INIT (" ") NEU_KEY$
REM 01 -> 01,03,04 
            spacer% = 99%
            convert str(gls_read$(),89%,02%) to spacer%, data goto badSpacer
            
badSpacer:            
REM STR(NEU_KEY$,01%,02%) = STR(GLS_READ$(),89%,02%)  /*SPACER    */
            convert spacer% to str(neu_key$,01%,02%), pic(00) /*Spacer    */
            str(neu_key$,03%,01%) = " "
REM 02 -> File Number from GED SCHED 000, 304, 120, 202
            filesort% = 999%
            convert str(gls_read$(),110%,03%) to filesort%, data goto badFileSort
            
badFileSort:            
REM STR(NEU_KEY$,04%,03%) = STR(GLS_READ$(),110%,03%) /*GED SCHED  */ 
            convert filesort% to  str(neu_key$,04%,03%), pic(000) /*GED SCHED*/ 
            if str(sav_read$(),1%,1%) = "<" then                 ~
                str(neu_key$,04%,03%) = "000" /*GED SCHED  */ 
            str(neu_key$,07%,01%) = " "            
REM 03 -> 000001, 010001, 020001 00 DS, 01 TB, 02 TT/BB          /* APCPLA45   */
            str(neu_key$,08%,06%) = str(gls_read$(),71%,06%)  /*Strenth &Cnt*/
            str(neu_key$,14%,01%) = " "
REM 04 -> Header Count
            convert hdrcnt% to str(neu_key$,15%,04%), pic(0000) /*hdr cnt   */
            str(neu_key$,19%,01%) = " "
REM 05 -> APCPLA45 batch count IE 001 - 305
            str(neu_key$,20%,04%) = str(gls_read$(),51%,04%)  /* batch cnt */
            str(neu_key$,24%,01%) = " "     
REM 06 -> APCPLA45 comments
            str(neu_key$,25%,31%) = str(gls_read$(),78%,31%)  /* comments  */ 
            str(neu_key$,56%,01%) = " "            

            hdrcnt% = hdrcnt% + 1%
            
        return      


        
REM=================================================================
REM   READ GLASS WORKFILE TO CREATE GLASS BATCH FILES
REM=================================================================
        read_work_file
        init(" ") neu_key$, gls_read$(), gls_sort$, file$, name$, ~
                  intercept$
        mat batch1% = zer
        file%, ff%, batch% = 0%
        work_file_next
        read #3, key > neu_key$, using WORKFILE_FMT, neu_key$, gls_read$(),~
                            eod goto readWrkFlDne
                            
          if str(gls_read$(),01%,01%) <> "<" then goto notFileStart
            gls_hdr$() = gls_read$()
          goto work_file_next
          
notFileStart:
          if str(gls_read$(),01%,01%) <> "*" then goto notOrderStart
             gls_sort$  = str(neu_key$,25%,11%)
             intercept$ = str(neu_key$,01%,02%)
             gosub loadSortName
             if newfile% = 0% then goto NotNewFile /* Same File Name */
             if file% <> 0% and ff% <> 0% then gosub writeEnding     
REM BATCH% = 1%             
             gosub loadFileName
             gosub openGlsFile  
/* Write File Header */
NotNewFile:
             gosub writeGlsRec
          goto work_file_next

notOrderStart:         
          gosub writeGlsRec
          goto work_file_next
        readWrkFlDne
        return

        loadSortName
          init(" ") readkey$, desc$
          file%, filenbr%, newfile% = 0% 
          prvfile% = -1%
          str(readkey$,01%,09%) = "GED SCHED"
REM  str(readkey$,01%,09%) = "GED SCHES"
          str(readkey$,10%,14%) = gls_sort$ 
          read #2, key = readkey$, using L00100, desc$, eod goto srtNmeDne
L00100:          FMT XX(24), CH(30)        
                                        
REM CALL "SHOSTAT" (" CHECK FILE ") STOP
            if str(desc$,1%,3%) <> file$ then goto diffFile
               if batch1%(batch%) = 999 then return  /* All in same file */
               if batch1%(batch%) > 0 then return
                 batch% = batch% + 1%
                 gosub getFileInfo
               return

diffFile:
            mat batch1% = zer
            flecnt% = 1%
            batch% = 1%
            gosub getFileInfo            
        return

        srtNmeDne
REM !!##ERROR##!! No File Name Found for gls_sort$ 
          srtNme% = srtNme% + 1%
REM FILE% = 299%                                 /* DEFAULT TO TT/BB */
REM IF STR(GLS_SORT$,10%,02%) = "TB" THEN FILE% = 199%
REM IF STR(GLS_SORT$,04%,02%) = "DS" THEN FILE% = 399%
REM IF INTERCEPT$ = " " THEN INTERCEPT$ = "99"
          file% = 999%
          str(name$,01%,05%) = "ERROR"          
          convert srtNme% to str(name$,06%,03%), pic(000)
          filenbr% = 57%   /* ff% = ff% + 4% Only 61 file select stmts */
          newfile% = 1%
        return

        getFileInfo
          newfile% = 1%
          
          convert str(desc$,1%,3%) to file%, data goto L00140
            
L00140:            
          convert str(desc$,2%,2%) to filenbr%, data goto L00150

L00150:

          convert file% to file$, pic(000)

            
          prvfile% = file%        
        return
        
        loadFileName
REM IF SRTNME% > 0% THEN RETURN
          if file% = 999% then return
          init(" ") readkey$, desc$, name$
          str(readkey$,01%,09%) = "GED SCHE1"
          str(readkey$,10%,14%) = file$
          read #2, key = readkey$, using L00100, desc$, eod goto loadFleNmDne   

            name$ = intercept$ & str(desc$,1%,9%)   
            
            if newfile% = 1% and batch1%(batch%) > 0% then return
REM IF NOOPEN% <> 0% THEN RETURN   
            if batch% > 3% then batch% = 0%
REM IF BATCH% > 1% THEN RETURN 
            
                                               /* Batch Counters          */
REM BATCH% = 0%
            convert str(desc$,11%,3%) to batch1%(1%), data goto L00200
             
L00200:
            convert str(desc$,15%,3%) to batch1%(2%), data goto L00250
             
L00250:
            convert str(desc$,19%,3%) to batch1%(3%), data goto L00300
                 
L00300:
        loadFleNmDne
          if name$ = " " then gosub srtNmeDne
        return
        
        openGlsFile 
          newfile% = 0%        
          library$ = "SCHEDUL"
          volume$  = "CARLOS"
          if schema% = 2% then volume$ = "NE"
          ff% = filenbr%
          ff% = ff% + 4%    /* ff% needs to start at least at 4 */          
          p% = 0%
          p% = pos(name$ = "#" )
          if p% <> 0% then gosub updateFileNumber
             call "SHOSTAT" ("Creating Glass Batch File - "& name$ )
             flerr% = 0%
             call "EWDOPEN" (#ff%, name$, flerr%)
                if flerr% = 1% then goto L00350   /* Doesn't Exist        */
             str(msg$(1%),19%,8%) = name$
             gosub file_exists
               if comp% <> 16% then goto L00400
               call "FILEBGON" addr(#ff%)
             goto L00350
                                            /* Append Mode      */
L00400:        close #ff%

               call "OPENFILE" (#ff%,"EXTND",f2%(fy%),rslt$(fy%), axd$ )
             goto L00360

L00350:
             open nodisplay, #ff%, output, space = 300%,               ~
             dpack   = 100%, ipack = 100%, file = name$,    ~
             library = library$, volume = volume$, blocks = 5%
/* Write File Header */
             str(gls_hdr$(1%),21%,40%) = name$ & bin(34%,1)  
             write #ff%, using L10200, gls_hdr$(1%), gls_hdr$(2%)
L10200:        FMT CH(192), CH(192)

L00360:
        return   
        
        updateFileNumber
          convert fleCnt% to str(name$,p%,1%),pic(#)
          fleCnt% = fleCnt% + 1%
        return
        
        writeEnding
          write #ff%, using L10200, "#", " "        
          
          gosub closeFile
        return
        closeFile
         close #ff%
        return
        
        writeGlsRec
          glsged% = 0%
          convert str(gls_read$(),01%,03%) to glsged%, data goto notIG
              if batch1%(batch%) = 999% then goto notIG
              if batch1%(batch%) > 0 then                ~
                         batch1%(batch%) = batch1%(batch%) - 1%
notIG          
          write #ff%, using L10200, gls_read$(1%), gls_read$(2%)
        return
        
        file_exists
         comp% = 2%
         hdr$ = "**** Glass Batch File " & name$ & "***"
                                                 /* msg$(1%) Pre-Set   */
         msg$(2) = "       G L A S S   B A T C H   F I L E S         "
         msg$(3) = "Press <RETURN> To Continue, or PF(16) to Delete. "
         call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        open_error
         errormsg$ = "(Open Error) - File = " & filename$
         gosub error_prompt
         err% = 0%
        return

        error_prompt
         comp% = 2%
         hdr$ = "***** (Error) (Error) (Error)  *****"
         msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
         msg$(2%) = errormsg$
         msg$(3%) = "Press Any Key To Continue."
         call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
        
        OPEN_WORK
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#3,mode$, 500%, f2%)
            if f2% <> 0% then goto OPEN_WORK_ERR

        return
OPEN_WORK_ERR:
        errormsg$ = "Can Not Open AWDPLNWK"
        gosub error_prompt

        return

openFile:
        call "OPENFILE" (#3%, "IO   ", f2%(3%), rslt$(3%), axd$ )

        if f2%(3%) <> 0% then goto L01200
REM GOSUB FILE_EXISTS
REM IF COMP% <> 16% THEN GOTO L01160
              call "FILEBGON" (#3%)
              goto L01200
REM L01160
           close #3%                 /* Re-Open in Append Mode    */
           call "OPENFILE" (#3%,"EXTND",f2%(3%),rslt$(3%),axd$)
           return

L01200: str(rslt$(3%),1%,6%)  = "OUTPTP"
        str(rslt$(3%),7%,8%)  = "00001000"
        str(rslt$(3%),15%,3%) = "100"
        str(rslt$(3%),18%,3%) = "100"
        call "OPENFILE" (#3%,"OUTPT",f2%(3%),rslt$(3%),axd$ )
return
        

        exit_sub
            call "SHOSTAT" ("END ")
          end



