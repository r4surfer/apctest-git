        REM *************************************************************~
            *  Program Name      - ATSTADPU                             *~
            *  Creation Date     - 06/07/2021                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Ricky Beane                          *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description - Purge Atlas Status audit file.  Set to     *~
            *  purge system write date older than 100 days.             *~
            *-----------------------------------------------------------*~
			*04/21/22 ! CR3078 Update displays                     ! RDB*~
            *************************************************************

        dim pdate$6,                     /* Purge date                 */~
            currdate$6,                  /* Current date               */~
            datechk$6,                   /* File system date  CR3078   */~
			btime$8,                     /* Current time begin  CR3078 */~
            etime$8,                     /* Current time endb          */~
            dcnt$30, rcnt$30,            /* Screen counter message     */~
            at_rec$255,                  /* Record length              */~
            readkey$106                  /* Key of tracking file       */

        dim f2%(45%),                    /* = 0 if the file is open    */~
            fs%(45%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(45%)20                 /* Text from file opening     */

            mat f2% = con
            mat fs% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "ATSTTSAD",                                       ~
                        varc,     indexed,  recsize =  255,              ~
                        keypos =    1, keylen =  106   
                        
            call "OPENCHCK" (#1, fs%(1%), f2%(1%),500%, rslt$(1%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
REM    initialize
         init(" ") currdate$, pdate$, dcnt$, rcnt$
         dcnt% = 0% : rcnt% = 0%
         currdate$ = date 
		 call "TIME" (btime$)    /* CR3078 */
         
    /* More than 100 Days Old  */
         call "DATE" addr("G+",currdate$, -100%,pdate$,err%)
           
REM------------------------------------------------------------------------
REM             M A I N                                                   -
REM------------------------------------------------------------------------
REM    main
      
       gosub purge_recs
       
       convert dcnt% to dcnt$,  pic(########)
       convert rcnt% to rcnt$,  pic(########)
/* CR3078 */	   
	   call "TIME" (etime$)
	   call "SHOSTAT" ("Started Time " & btime$ & " Ended " & etime$         & ~
	                   "  Enter to see results")
	   STOP
	   
       call "SHOSTAT" ("Finished Purging older than 100 days               " &  ~
                       "Reads " & rcnt$ & "  " & "Deletes " & dcnt$)
       STOP 
       
       goto exit_program
       
REM------------------------------------------------------------------------
REM       Process Records for Purging                                     -
REM------------------------------------------------------------------------     
       purge_recs
          init(" ") readkey$, at_rec$
          dcnt% = 0%
          
       purge_nxt_rec  
          
          if MOD(rcnt%,1000) <> 0 then goto L05000
          convert dcnt% to dcnt$,  pic(########)
          convert rcnt% to rcnt$,  pic(########)
          call "SHOSTAT" ("Purge older than 100 days               " &  ~
                          "Reads " & rcnt$ & "  " & "Deletes " & dcnt$)
          
L05000:          
          read #1, hold, key > readkey$, using L05200, at_rec$, eod goto L05275
                    
L05200:      FMT CH(255)

          readkey$ = str(at_rec$,1%,106%)
          datechk$  = str(at_rec$,58%,6%)
          rcnt% = rcnt% + 1%
          
          if datechk$ >= pdate$ then goto L05250
          
          delete #1
          dcnt% = dcnt% + 1%

L05250:   goto purge_nxt_rec 
          
L05275:   return

REM------------------------------------------------------------------------
REM       E X I T    P R O G R A M                                        -
REM------------------------------------------------------------------------

        exit_program
            call "SHOSTAT" ("One Moment Please")
            close #1
        end






















