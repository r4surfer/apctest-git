        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *-----------------------------------------------------------*~
            * AWDHUBED - Provides for the updating of the hub info file *~      
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN-----+--------------WHAT----------------------+-WHO-*~
            * 11/21/2006 | original                               | DES *~
            *------------+----------------------------------------+-----*

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79, msg$79,         /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            pfd$(3)79, pf$(20)1,         /* PF Descriptors and Keys    */~
            summary$(100)79,             /* Lines for Summary Screen   */~
            CustKey$9,                   /* CUSTOMET FILE KEY          */~
            CustYard$6,                  /* CUSTOMET FILE KEY          */~
            CustName$42,                 /* CUSTOMET FILE KEY          */~
            SkuDesc$42,                  /* CUSTOMET FILE KEY          */~
	    Addr1$30, CustSku$3, OldCustSku$3, PageNbr%,                 ~
	    Addr2$30, CustCode$9, CustDist$3,                            ~
	    key$29,key2$4,key3$15,key4$10,                               ~
	    hub1$64,hub2$64,hub3$64,tmp$256,tmp2$256,                    ~
	    FStat$,  add$, rec$(2)256,                                   ~
            userid$3                     /* Current User Id            */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R1.00.00  6/07/2006                               "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! HUBCALFL ! EDI account/district xref                *~
            *************************************************************~

            select #1, "HUBCALFL",                                      ~
                       varc,     indexed,  recsize = 512,               ~
                       keypos =    1, keylen =  29                      ~

            call "OPENCHCK" (#1,  fs%( 1), f2%( 1), 0%, rslt$( 1))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            init(" ") line2$

REM         +---------------------------------------------------------------+
REM         |             Main Screen                                       |
REM         +---------------------------------------------------------------+
        main_screen
	    PageNbr% = 1%
            init(" ") errormsg$, inpmessage$
            gosub initialize_variables 
	    for fieldnr% = 1% to 11%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
	        if enabled% = 0% then L10240
L10130:         gosub'101(fieldnr%)    /* Display / Accept  */
                if keyhit%  =  1% then gosub startover
	        if keyhit% <>  5% then       L10220
L10160: /*&     fieldnr% = max(1%, fieldnr% - 1%) */
                gosub'051(fieldnr%)
		if enabled% = 1% then L10130
		if fieldnr% = 1% then L10110
		goto L10160
L10220:   
                if keyhit% = 16% then exit_program
                if keyhit% <> 7% then goto L10230
                gosub save_data
	        if u3% = 1% then return
	        return clear all
                goto main_screen
L10230:         if keyhit% <>  6% then goto L10235       
                gosub delete_data
	        if u3% = 1% then return
	        return clear all
                goto main_screen
L10235:         if keyhit% <> 0% then       L10130
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                if errormsg$ <> " " then L10130
	    next fieldnr%
 
	 editpg1
	    lastfieldnr% = 0%
	    gosub'101(0%, 2%) 
L11130:     if keyhit%  =  1% then gosub startover
            if keyhit% <> 7% then goto L11120
	    gosub save_data
	    if u3% = 1% then return
	    return clear all
	    goto main_screen
L11120:     if keyhit% <>  6% then goto L11125
            gosub delete_data
            if u3% = 1% then return
            return clear all
            goto main_screen
L11125:     if keyhit% <>  0% then       editpg1
            if fieldnr% < 1% or fieldnr% > 8% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
	    gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
	    if enabled% =  0% then       editpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
            if keyhit%  =  1% then gosub startover
	    if keyhit% <>  0% then L11180
	    gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
	    if errormsg$ <> " " then L11180
	    lastfieldnr% = fieldnr%
            goto L11130

initialize_variables
        init(" ") rec$(), key2$, key3$, key4$, key$
	init(" ") hub1$, hub2$, hub3$, vdl$, vdr$, cr$, mull$, ld$
        init(" ") CustCode$, CustSku$, CustDist$, Addr1$, Addr2$
        init(" ") City$, State$, Zip$, SkuDesc$, CustName$, CustYard$
        init(hex(8C)) lfac$()
        return

        deffn'051(fieldnr%)
            gosub setpf_acct    
	    enabled% = 1%
REM	    if fieldnr% > 4% then enabled% = 0%
	return

        deffn'101(fieldnr%)
	    if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
            else init(hex(86)) lfac$()
	    if fieldnr% > 0% then                                      ~
	              lfac$(fieldnr%) = hex(81)
	    if inpmessage$ = " " then inpmessage$ = edtmessage$
L44140:     accept                                                       ~
               at (01,02), "Hub Calculation File Maintenance  ",         ~
	       at (01,59), "Today's Date:",                              ~
	       at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
	       at (05,02), "Key:    ",                                   ~
	       at (05,10), fac(lfac$( 1)), key2$                , ch(04),~
	       at (05,15), fac(lfac$( 2)), key3$                , ch(15),~
	       at (05,31), fac(lfac$( 3)), key4$                , ch(10),~
	       at (06,09), "Model",                                      ~
	       at (06,15), "Type      ",                                 ~
	       at (06,31), "Verticles & Hubs (I.e. 2V3H)",               ~
	       at (08,02), "Hub1: ",                                     ~
	       at (08,10), fac(lfac$( 4)), hub1$                , ch(64),~
	       at (09,02), "Hub2: ",                                     ~
	       at (09,10), fac(lfac$( 5)), hub2$                , ch(64),~
	       at (10,02), "Hub3: ",                                     ~
	       at (10,10), fac(lfac$( 6)), hub3$                , ch(64),~
	       at (11,02), "Hub calculations are in RPN, enter with",    ~
	       at (11,42), "a leading '=' for regular notation.",      ~
	       at (12,02), "Formulas entered in regular notation ",      ~
	       at (12,39), "will be converted to RPN.",                  ~
	       at (13,02), "I.e. LITE=2.5+3/ for (LITE+2.5)/3 or ",      ~
	       at (13,39), "WW=WW=5.9-3/-.7+ for WW-((WW-5.9)/3)+.7",   ~
	       at (14,02), "WW=Window Width, GW=WW-VDL-VDR (windo",      ~
	       at (14,39), "w width), LITE=GW/3               ",      ~
	       at (15,02), "Vinyl Deduction Left:",                      ~
	       at (15,25), fac(lfac$( 7)), vdl$                , ch(08), ~
	       at (15,39), "(VDL)                ",                      ~
	       at (16,02), "Vinyl Deduction Right:",                     ~
	       at (16,25), fac(lfac$( 8)), vdr$                , ch(08), ~
	       at (16,39), "(VDR)                ",                      ~
	       at (17,02), "Center Rail:",                               ~
	       at (17,25), fac(lfac$( 9)), cr$                 , ch(08), ~
	       at (17,39), "(CR)                 ",                      ~
	       at (18,02), "MULL: ",                                     ~
	       at (18,25), fac(lfac$(10)), mull$               , ch(08), ~
	       at (19,02), "LD:   ",                                     ~
	       at (19,25), fac(lfac$(11)), ld$                 , ch(08), ~
	       at (21,02), fac(hex(8c)),   errormsg$            , ch(79),~
	       at (22,02), fac(hex(8c)),   pfd$(1)              , ch(79),~
	       at (23,02), fac(hex(8c)),   pfd$(2)              , ch(79),~
	       at (24,02), fac(hex(8c)),   pfd$(3)              , ch(79),~
               keys(str(pf$())),                                         ~
	       key (keyhit%)

            if keyhit% <> 13 then L44540
	    call "MANUAL" ("UPDARIIN")
	    goto L44140
L44540:     if keyhit% <> 15 then L44580
            call "PRNTSCRN"
            goto L44140
L44580:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

       deffn'151(fieldnr%)
	   errormsg$ = " "
           on fieldnr% gosub L50120,         /* Key2                  */ ~
                             L50130,         /* Key3                  */ ~
                             L50140,         /* key4                  */ ~
	                     L50150,         /* Hub1                  */ ~
                             L50160,         /* Hub2                  */ ~
                             L50170,         /* Hub3                  */ ~
                             L50180,         /* vdl                   */ ~
                             L50190,         /* vdr                   */ ~
                             L50200,         /* cr                    */ ~
                             L50210,         /* mull                  */ ~
			     L50220          /* ld                    */
           return

L50110: return
L50120: return
L50130: return
L50140:                          
        str(key$,01,04) = key2$ 
        str(key$,05,15) = key3$ 
        str(key$,20,10) = key4$ 
	init(" ") rec$()
        read #1, key = key$, using F0100, rec$(), eod goto L50145
F0100:  FMT 2*CH(256)
        key$  = str(rec$(),1,29)
        hub1$ = str(rec$(),030,64)
        hub2$ = str(rec$(),094,64)
        hub3$ = str(rec$(),158,64)
        vdl$  = str(rec$(),222,8)
        vdr$  = str(rec$(),230,8)
        cr$   = str(rec$(),238,8)
        mull$ = str(rec$(),246,8)
        ld$   = str(rec$(),254,8)
L50145: return

L50150: tmp$ = hub1$
	gosub test_postfix
        hub1$ = tmp$
	if err% <> 0% then errormsg$ = "*** Invalid calculation syntax ***"
        return

L50160: tmp$ = hub2$
	gosub test_postfix
        hub2$ = tmp$
	if err% <> 0% then errormsg$ = "*** Invalid calculation syntax ***"
        return

L50170: tmp$ = hub3$
	gosub test_postfix
        hub3$ = tmp$
	if err% <> 0% then errormsg$ = "*** Invalid calculation syntax ***"
        return

L50180: return
L50190: return
L50200: return
L50210: return
L50220: return

test_postfix:
        goto infix
REM     if str(tmp$,1,1) <> "=" then goto infix               
REM	tmp2$ = str(tmp$,2,63)
REM	tmp$ = tmp2$
REM     call "RPNCVTSB" (tmp$,err%)   
REM	tmp2$ = tmp$

infix:
        err% = 0%  /* test 1, check for valid characters */
	x%=0%
	y%=0%
	z%=0%
	for l% = 1% to 64%
        if str(tmp$,l%,1%) = " " then goto loop        
        if str(tmp$,l%,1%) = "=" then goto func_label   
        if str(tmp$,l%,1%) = "+" then goto func_label 
        if str(tmp$,l%,1%) = "-" then goto func_label 
        if str(tmp$,l%,1%) = "/" then goto func_label 
        if str(tmp$,l%,1%) = "*" then goto func_label 
        if str(tmp$,l%,1%) = "^" then goto func_label 
        if str(tmp$,l%,1%) = "%" then goto func_label 
        if str(tmp$,l%,1%) = "." then goto var_label 
        if str(tmp$,l%,1%) >= "0" and                      ~
	   str(tmp$,l%,1%) <= "9" then goto var_label
        if str(tmp$,l%,1%) >= "A" and                      ~
	   str(tmp$,l%,1%) <= "Z" then goto var_label
        err% = -1%
        l% = 65%
	return

var_label:
       y% = 1%
       goto loop

func_label:
       x% = x% + 1%
       if str(tmp$,l%,1%) = "=" and y% <> 1 then goto loop_err     
       if str(tmp$,l%,1%) = "=" then x% = x% - 1%      
       if y% = 1% then z% = z% + 1%
       y% = 0%

loop:  next l%
       y% = z% - x%
       if z% = 0% and x% = 0% then goto test_exit
       if y% = 1% then goto test_exit

loop_err:  msg$ = "*** ERROR ***"
           err% = -1%

teST_EXit:          
REM        if err% = 0% then call "RPNCVTSB" (tmp$, err%)
	   return

save_data: 
        init(" ") rec$()
	found% = 0%
        str(rec$(),1,40)   = key$
        read #1, key = key$, hold, using F0100, rec$(), eod goto build_rec
	found% = 1%

build_rec:
        str(rec$(),030,64) = hub1$
        str(rec$(),094,64) = hub2$
        str(rec$(),158,64) = hub3$
        str(rec$(),222,8)  = vdl$
        str(rec$(),230,8)  = vdr$
        str(rec$(),238,8)  = cr$
        str(rec$(),246,8)  = mull$
        str(rec$(),254,8)  = ld$

        if found% = 0% then goto found_rec             
        rewrite #1, using F0100, rec$() 
        init(" ") rec$()
	return

found_rec:                              
        write #1, using F0100, rec$() 
        init(" ") rec$()
        return

delete_data: 
        init(" ") rec$()
        str(rec$(),1,40)   = key$
        read #1, hold, key = key$, using F0100, rec$(), eod goto no_rec   
	delete #1
no_rec:
	return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        load_account
            init(" ") summary$()

          return

setpf_acct
         pfd$(1) = "(1)Start Over                          "   &        ~
                   "                      (13)Instructions"
         pfd$(2) = "                               (6)Delet"   &        ~
                   "e                     (15)Print Screen"
         pfd$(3) = "                               (7)Save "   &        ~
                   "                      (16)Exit Program "
         str(pf$()) = hex(01ffffffff0607ffffffffff0d0e0f101700ffffff)
         return

startover:
         call "STARTOVR" (u3%)
	 if u3% = 1% then return
	    return clear all
         goto main_screen

exit_program:
            end
