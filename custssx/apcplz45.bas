/*
REM       +---------------------------------------------------------------+
REM       | APCPLZ42: Calculate hub values.                               |
REM       |           Uses HUBDTAFL to get formulas, using                |
REM       |           vinyl duduction (left, right, center),              |
REM       |           mull, and Rspecial ld.                              |
REM       +----------+------------------------------------------------+---+
REM       |11/21/2006| AWD001 - Initial program write                 |DES|
REM       +----------+------------------------------------------------+---+
 */
        sub "APCPLZ45" (key$,     /* hubdatfl key  */          ~
                        ww$,      /* window width  */          ~
                        hub1,                                  ~
			hub2,                                  ~
			hub3,                                  ~
                        #1,       /* hubdatfl  */              ~             
			hubm,                                  ~
                        err%)



        dim rec$(2)256, key$40, hub1$64, hub2$64, hub3$64, hubs$(9)             
	dim vdl$8, vdr$8, cr$8, mull$8, ld$8, tmp$64, gw$9, lite$9
        dim stck(6), stck1$9, stck2$9, stck3$9, stck4$9
        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
            logmsg$256,                  /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            axd$(32)4,                                                   ~
            rslt$(32)20                  /* Text from file opening     */

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! HUBDATFL ! HUB FORMULA FILE                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

/*          select #1,  "HUBCALFL",                                      ~
                        varc,     indexed,  recsize = 512,               ~
                        keypos =    1, keylen =  29 */

 
REM        call "SHOSTAT" ("Opening Files, One Moment Please")
REM	    call "OPENFILE" (#1, "INPUT", f2%(1), rslt$(1), axd$(1))
	    stck(1) = 0.0
	    stck(2) = 0.0
	    stck(3) = 0.0
	    stck(4) = 0.0
	    stck(5) = 0.0
	    stck(6) = 0.0
	    err% = 0


LOOP:
            read #1, key = key$, using F100, key$, hub1$, hub2$, hub3$,   ~
                           vdl$, vdr$,  cr$, mull$, ld$, eod goto NOT_FOUND 
Fx00:       FMT CH(29), 3*CH(64), 5*CH(08) 
F100:       FMT CH(29), 3*CH(64), CH(08), 4*CH(08)  

            if vdl$ = "        " then vdl$ = "0.0000"
            if vdr$ = "        " then vdr$ = "0.0000"
            if cr$  = "        " then cr$  = "0.0000"
            if ld$  = "        " then ld$  = "0.0000"
            if mull$ = "        " then mull$ = "0.0000"
            convert vdl$ to vdl
            convert vdr$ to vdr
            convert ww$  to ww 
            convert cr$  to cr
            gw = ww - vdl - vdr     
REM         gw = ww - vdl - vdr - cr
REM	    gw = gw / 2.0  /* ??? */
            convert gw to gw$, pic(####.####)
REM	    lite = gw / 3.00
	    lite = gw / (hubm + 1)
	    convert lite to lite$, pic(####.####)
	    hubm = hubm + 1
	    convert hubm to hubs$, pic(####.####)
	    logmsg$ = "<> " & gw$ & "/" & lite$ & "/" & vdl$ & ":" & vdr$ & ~
		      ":" & ww$
REM         call "LOGFILE" (logmsg$)
	    hub1 = 0.00
	    hub2 = 0.00
	    hub3 = 0.00
            tmp$ = hub1$
            gosub S200
            hub1 = stck(1)
            tmp$ = hub2$
	    gosub S200
            hub2 = stck(1)
            tmp$ = hub3$
	    gosub S200
            hub3 = stck(1)
       
            goto FINISH


S200:       x%=0%
	    y%=0%
	    z%=0%
	    cnt% = 0%
	    init(" ") var$
	    stck(1) = 0.00
	    stck(2) = 0.00
	    stck(3) = 0.00
	    stck(4) = 0.00

	    if tmp$ = " " then return 
            for x% = 63% to 1% step -1%
              if str(tmp$,x%,1%) = " " then goto  S205
              if str(tmp$,x%,1%) = "=" then goto S206
              if str(tmp$,x%,1%) = "+" then goto S206
              if str(tmp$,x%,1%) = "-" then goto S206
              if str(tmp$,x%,1%) = "*" then goto S206
              if str(tmp$,x%,1%) = "/" then goto S206
              if str(tmp$,x%,1%) = "^" then goto S206
              if str(tmp$,x%,1%) = "%" then goto S206
              str(tmp$,x%+1,1%) = "="
              goto S206
S205:       next x%
S206:	    type% = 0%  /* 1 = variable, 2 = number */
	    y%=0%
            for x%=1% to 64%
            if str(tmp$,x%,1%) <= " " and cnt% > 0% then goto S350 
            if str(tmp$,x%,1%) <= " " and cnt% = 0% then goto S295 
               if str(tmp$,x%,1%) < "A" or str(tmp$,x%,1%) > "Z" then goto S210 
	       type% = 1%
               goto S250

S210:          if str(tmp$,x%,1%) = "." then goto S220 
               if str(tmp$,x%,1%) < "0" or str(tmp$,x%,1%) > "9" then goto S295 
	       if type% = 1% then goto S250
S220:	       type% = 2%
               goto S250

S250:          y% = y% + 1%
	       str(var$,y%,1%) = str(tmp$,x%,1%)
 
S290:          /* push var/num on stack */
               if type% = 1 then goto S292
               /* push variable */


               goto S350
S292:

               goto S350

S295:          if type% = 2% then goto S297        
               if type% = 0% then goto S298        
               /* fetch variable */
	       stck(6) = stck(5)
	       stck(5) = stck(4)
	       stck(4) = stck(3)
	       stck(3) = stck(2)
	       stck(2) = stck(1)
	       if var$ = "LITE" then var$ = lite$
	       if var$ = "VDL" then var$ = vdl$
	       if var$ = "VDR" then var$ = vdr$
	       if var$ = "MULL" then var$ = mull$
	       if var$ = "CR" then var$ = cr$
	       if var$ = "LD" then var$ = ld$
	       if var$ = "GW" then var$ = gw$
	       if var$ = "WW" then var$ = ww$
	       if var$ = "HUB1" then convert hub1 to var$, pic(####.####)
	       if var$ = "HUB2" then convert hub2 to var$, pic(####.####)
	       if var$ = "HUBM" then var$  = hubs$                        
               convert var$ to stck(1)
	       var$ = "         "               
	       y% = 0%
               type% = 0% /* massage stck[0] */ 
               goto S298

S297:          /* massage number */
	       stck(6) = stck(5)
	       stck(5) = stck(4)
	       stck(4) = stck(3)
	       stck(3) = stck(2)
	       stck(2) = stck(1)
               convert var$ to stck(1)
               type% = 0% /* massage stck[0] */ 
	       var$ = "        "               
	       y% = 0%
               goto S298

S298:          cnt% = cnt% + 1%
               init(" ") var$
               if str(tmp$,x%,1%) <> "+" then S300
               stck(1) = stck(2) + stck(1)
	       stck(2) = stck(3)
	       stck(3) = stck(4)
	       stck(4) = stck(5)
	       stck(5) = stck(6)
	       stck(6) = 0.0
	       goto S350

S300:          if str(tmp$,x%,1%) <> "-" then S310
               stck(1) = stck(2) - stck(1)
	       stck(2) = stck(3)
	       stck(3) = stck(4)
	       stck(4) = stck(5)
	       stck(5) = stck(6)
	       stck(6) = 0.0
	       goto S350


S310:          if str(tmp$,x%,1%) <> "*" then S320
               stck(1) = stck(2) * stck(1)
	       stck(2) = stck(3)
	       stck(3) = stck(4)
	       stck(4) = stck(5)
	       stck(5) = stck(6)
	       stck(6) = 0.0
	       goto S350


S320:          if str(tmp$,x%,1%) <> "/" then S330
               stck(1) = stck(2) / stck(1)
	       stck(2) = stck(3)
	       stck(3) = stck(4)
	       stck(4) = stck(5)
	       stck(5) = stck(6)
	       stck(6) = 0.0
	       goto S350


S330:          if str(tmp$,x%,1%) <> "^" then S340
               stck(1) = stck(2) ** stck(1)
	       stck(2) = stck(3)
	       stck(3) = stck(4)
	       stck(4) = stck(5)
	       stck(5) = stck(6)
	       stck(6) = 0.0
	       goto S350


S340:          if str(tmp$,x%,1%) <> "%" then S345 /* remainder */
               tmp = stck(2) / stck(1)
               tmp = tmp * stck(1)
               stck(1) = stck(2) - tmp     
	       stck(2) = stck(3)
	       stck(3) = stck(4)
	       stck(4) = stck(5)
	       stck(5) = stck(6)
	       stck(6) = 0.0
	       goto S350

S345:          if str(tmp$,x%,1%) <> "=" then S350
           /*  stck(6) = stck(5)
	       stck(5) = stck(4)
	       stck(4) = stck(3)
	       stck(3) = stck(2)
	       stck(2) = stck(1)
	       stck(1) = 0.0  */

S350:
            if str(tmp$,x%,1%) <= " " then x% = 65%
            next x%
            return

NOT_FOUND: err% = -1%
FINISH:    end      



