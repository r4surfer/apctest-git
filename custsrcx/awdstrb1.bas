        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *-----------------------------------------------------------*~
            * AWDSTRB1 - Provides for the updating of the district bill *~      
            *            Vendor Invoice Entry (with the exception of    *~
            *            releasing for update).                         *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN-----+--------------WHAT----------------------+-WHO-*~
            * 06/07/2006 | original                               | DES *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
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
	    City$18,                                                     ~
	    State$2,                                                     ~
	    Zip$10,                                                      ~
	    FStat$,  add$,                                               ~
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
            * #1  ! EDISTRBR ! EDI account/district xref                *~
            * #2  ! DISSTRBR ! District Bill to information             *~
            * #3  ! CUSTOMER ! Customer information file                *~
            * #4  ! GENCODES ! Codes File                               *~
            *************************************************************~

            select #1, "EDISTRBR",                                      ~
                       varc,     indexed,  recsize = 32,                ~
                       keypos =    1, keylen =  9                       ~

            select #2, "DISSTRBR",                                      ~
                       varc,     indexed,  recsize = 128,               ~
                       keypos =    1, keylen =  3                       ~

            select #3, "CUSTOMER",                                      ~
                        varc, indexed, recsize = 1200,                  ~
                        keypos = 1, keylen =   9,                       ~
                        alt key  1, keypos =  10, keylen =  30, dup,    ~
                            key  2, keypos = 424, keylen =   9, dup,    ~
                            key  3, keypos = 771, keylen =   9, dup,    ~
                            key  4, keypos = 780, keylen =   9, dup

            select #4,  "GENCODES",                                     ~
                        varc,     indexed,  recsize =  128,             ~
                        keypos =    1, keylen =  24

            call "OPENCHCK" (#1,  fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (#2,  fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (#3,  fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (#4,  fs%( 4), f2%( 4), 0%, rslt$( 4))

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

REM         +---------------------------------------------------------------+
REM         |             Main Screen                                       |
REM         +---------------------------------------------------------------+
        main_screen
	    PageNbr% = 1%
            init(" ") errormsg$, inpmessage$
            gosub initialize_variables 
	    for fieldnr% = 1% to 8%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
	        if enabled% = 0% then L10240
L10130:         gosub'101(fieldnr%)    /* Display / Accept  */
                if keyhit%  =  1% then gosub startover
	        if keyhit% <>  5% then       L10220
L10160:         fieldnr% = max(1%, fieldnr% - 1%)
                gosub'051(fieldnr%)
		if enabled% = 1% then L10130
		if fieldnr% = 1% then L10110
		goto L10160
L10220:         if keyhit% =  3% and fieldnr% = 1% then goto dist_screen
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
            gosub setpf_edit    
 
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
            fieldnr% = 0%                              
            if cursor%(1%) =  6% then fieldnr% = 1%
            if cursor%(1%) =  8% then fieldnr% = 2%
            if cursor%(1%) = 10% then fieldnr% = 3%
            if cursor%(1%) = 12% then fieldnr% = 4%
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

REM         +---------------------------------------------------------------+
REM         |             District Screen                                   |
REM         +---------------------------------------------------------------+
        dist_screen
            CustSku$ = "040"
	    PageNbr% = 2%
            init(" ") errormsg$, inpmessage$
            gosub initialize_variables
	    for fieldnr% = 1% to 6%
L20110:         gosub'052(fieldnr%)        /* Default / Enables */
	        if enabled% = 0% then L20240
L20130:         gosub'102(fieldnr%)    /* Display / Accept  */
                if keyhit%  =  1% then gosub startover
	        if keyhit% <>  4% then       L20220
L20160:         fieldnr% = max(1%, fieldnr% - 1%)
                gosub'052(fieldnr%)
		if enabled% = 1% then L20130
		if fieldnr% = 1% then L20110
		goto L20160
L20220:         if keyhit% =  3% and fieldnr% = 1% then goto main_screen
                if keyhit% = 16% then exit_program
                if keyhit% <> 7% then goto L20230
                gosub save_dist_data
	        if u3% = 1% then return
	        return clear all
                goto dist_screen
L20230:         if keyhit% <>  6% then goto L20235       
                gosub delete_dist
	        if u3% = 1% then return
	        return clear all
                goto dist_screen
L20235:         if keyhit% <> 0% then       L20130
L20240:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                if errormsg$ <> " " then L20130
	    next fieldnr%
            gosub setpf_edit    
 
	 editpg2
	    lastfieldnr% = 0%
	    gosub'102(0%, 2%)    
L21125:     if keyhit%  =  1% then gosub startover
            if keyhit% <> 7% then goto L21120
	    gosub save_dist_data
	    if u3% = 1% then return
	    return clear all
	    goto dist_screen
L21120:     if keyhit% <>  6% then goto L21130
            gosub delete_dist 
            if u3% = 1% then return
            return clear all
            goto dist_screen
L21130:     if keyhit% <>  0% then       editpg1
            fieldnr% = 0%                              
            if cursor%(1%) = 11% then fieldnr% = 1%
            if cursor%(1%) = 13% then fieldnr% = 2%
            if cursor%(1%) = 14% then fieldnr% = 3%
            if cursor%(1%) <> 15% then goto L21135  
            fieldnr% = 4%
            if cursor%(2%) > 43% then fieldnr% = 5%
            if cursor%(2%) > 46% then fieldnr% = 6%
L21135:     if fieldnr% < 1% or fieldnr% > 8% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
	    gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
	    if enabled% =  0% then       editpg2
L21180:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
            if keyhit%  =  1% then gosub startover
	    if keyhit% <>  0% then L21180
	    gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
	    if errormsg$ <> " " then L21180
	    lastfieldnr% = fieldnr%
            goto L21125

initialize_variables
            init(" ") CustCode$, CustSku$, CustDist$, Addr1$, Addr2$
            init(" ") City$, State$, Zip$, SkuDesc$, CustName$, CustYard$
            init(hex(8C)) lfac$()
            return

        deffn'051(fieldnr%)
            gosub setpf_acct    
	    enabled% = 1%
	    if fieldnr% > 1% then gosub setpf_save
	    if fieldnr% > 4% then enabled% = 0%
	return

        deffn'101(fieldnr%)
	    if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
            else init(hex(86)) lfac$()
	    if fieldnr% > 0% then                                      ~
	              lfac$(fieldnr%) = hex(81)
	    if inpmessage$ = " " then inpmessage$ = edtmessage$
L44140:     accept                                                       ~
               at (01,02), "Customer District Cross Reference ",        ~
	       at (01,59), "Today's Date:",                              ~
	       at (01,73), fac(hex(8c)), date$                  , ch(08),~
	       at (02,12), fac(hex(ac)), line2$                 , ch(79),~
	       at (06,02), "Customer Code    ",                          ~
	       at (06,25), fac(lfac$( 1)), CustCode$            , ch(09),~
	       at (06,35), fac(hex(8C)),   CustName$            , ch(42),~
	       at (08,02), "Customer Sku# Code",                         ~
	       at (08,25), fac(lfac$( 2)), CustSku$             , ch(03),~
	       at (08,35), fac(hex(8C)),   SkuDesc$             , ch(42),~
	       at (10,02), "Customer Yard Nbr.",                         ~
	       at (10,25), fac(lfac$( 3)), CustYard$            , ch(06),~
	       at (12,02), "Customer District ",                         ~
	       at (12,25), fac(lfac$( 4)), CustDist$            , ch(03),~
	       at (14,02), "Bill-to Address   ",                         ~
               at (14,25), fac(lfac$( 5)), Addr1$                 , ch(30),~
	       at (15,25), fac(lfac$( 6)), Addr2$                 , ch(30),~
	       at (16,25), fac(lfac$( 7)), City$                  , ch(18),~
	       at (16,44), fac(lfac$( 8)), State$                 , ch(02),~
	       at (16,47), fac(lfac$( 9)), Zip$                   , ch(10),~
	       at (21,02), fac(hex(8c)),   inpmessage$          , ch(79),~
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
           on fieldnr% gosub L50120,         /* Customer Code         */ ~
                             L50130,         /* Customer SKU          */ ~
                             L50132,         /* Customer Yard         */ ~
                             L50140,         /* Customer District     */ ~
                             L50150,         /* Address line 1        */ ~
                             L50160,         /* Address Line 2        */ ~
                             L50170,         /* City                  */ ~
                             L50180,         /* State                 */ ~
			     L50190          /* Zip Code              */
           return

        deffn'052(fieldnr%)
            gosub setpf_dist    
	    enabled% = 1%
	    if fieldnr% > 1% then gosub setpf_save
	return

        deffn'102(fieldnr%)
	    if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
            else init(hex(86)) lfac$()
	    if fieldnr% > 0% then                                      ~
	              lfac$(fieldnr%) = hex(81)
	    if inpmessage$ = " " then inpmessage$ = edtmessage$
            accept                                                       ~
               at (01,02), "Customer District Cross Reference ",        ~
	       at (01,59), "Today's Date:",                              ~
	       at (01,73), fac(hex(8c)), date$                  , ch(08),~
	       at (02,12), fac(hex(ac)), line2$                 , ch(79),~
	       at (12,02), "Customer District ",                         ~
	       at (12,25), fac(lfac$( 1)), CustDist$            , ch(03),~
	       at (14,02), "Bill-to Address   ",                         ~
               at (14,25), fac(lfac$( 2)), Addr1$                 , ch(30),~
	       at (15,25), fac(lfac$( 3)), Addr2$                 , ch(30),~
	       at (16,25), fac(lfac$( 4)), City$                  , ch(18),~
	       at (16,44), fac(lfac$( 5)), State$                 , ch(02),~
	       at (16,47), fac(lfac$( 6)), Zip$                   , ch(10),~
	       at (21,02), fac(hex(8c)),   inpmessage$          , ch(79),~
	       at (22,02), fac(hex(8c)),   pfd$(1)              , ch(79),~
	       at (23,02), fac(hex(8c)),   pfd$(2)              , ch(79),~
	       at (24,02), fac(hex(8c)),   pfd$(3)              , ch(79),~
               keys(str(pf$())),                                         ~
	       key (keyhit%)

            if keyhit% <> 13 then L45540
	    call "MANUAL" ("UPDARIIN")
	    goto L44140
L45540:     if keyhit% <> 15 then L45580
            call "PRNTSCRN"
            goto L44140
L45580:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

       deffn'152(fieldnr%)
	   errormsg$ = " "
           on fieldnr% gosub L50135,         /* Customer District     */ ~
                             L50150,         /* Address line 1        */ ~
                             L50160,         /* Address Line 2        */ ~
                             L50170,         /* City                  */ ~
                             L50180,         /* State                 */ ~
			     L50190          /* Zip Code              */
           return

L50120:   gosub load_account
          return

L50130:             
          str(cde_key$,1%,9%)   = "EDI TRADE"
          str(cde_key$,10%,15%) = CustSku$
	  call "PLOWCODE" (#4, cde_key$, SkuDesc$, 9%, .30, f1%(4))
          CustSku$ = str(cde_key$,10%,15%)
          return

L50132:                 
          return

L50135:   gosub load_district
          return

L50140:                 
          if CustSku$ <> "040" then goto L50145
          call "GETCODE" (#2, CustDist$, " ", 0%, 0, f1%(2))
	  read #2, key = CustDist$,                       ~
			       eod goto L50145
          get #2,  using L50145,                                ~
		 Addr1$, Addr2$, City$, State$, Zip$
L50145:    FMT POS(4), CH(35), CH(35), CH(25), CH(02), CH(10)
	  return
	     
L50150:                 
          return
	     
L50160:                 
          return
	     
L50170:                 
          return
	     
L50180:                 
          return
	     
L50190:                 
          return
	     
        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        load_account
            init(" ") summary$()

           add$ = "Y"  
	   call "PLOWCODE" (#3, CustCode$,CustName$, 0%, .30, f1%(3))
	   if CustCode$ = "      " then goto load_account
           read #1, hold, key = CustCode$,                             ~
                                      eod goto L30130              
           get #1, using L30130, CustCode$, CustDist$, CustYard$,      ~
                                      eod goto L30130              
           add$ = "N"  
L30130:    FMT CH(09), CH(3), ch(6)

           CustKey$ = CustCode$
           read #3, key = CustKey$, using  L01390, CustName$, CustSku$, ~
		   eod goto L01390
           OldCustSku$ = CustSku$
L01390:    FMT POS(10), CH(30), POS(1000), CH(3)

          SkuDesc$ = "Have to add account using CUSINPUT"
          str(cde_key$,1%,9%)   = "EDI TRADE"
          str(cde_key$,10%,15%) = CustSku$
          read #4,key = cde_key$, using  L01570, SkuDesc$, eod goto L01570
L01570:   FMT POS(25), CH(30)

          if CustSku$ = "040" then goto L01580
          SkuDesc$ = "District 'Bill To' Not Used"
          return                                 

L01580:    /* call "GETCODE" (#2, CustDist$, " ", 0%, 0, f1%(2)) */
           read #2, key = CustDist$,  using L01585,              ~
                    Addr1$, Addr2$, City$, State$, Zip$,         ~
                                      eod goto L01585              
L01585:    FMT POS(4), CH(35), CH(35), CH(25), CH(02), CH(10)
          return

load_district
          if CustSku$ <> "040" and PageNbr% = 1% then return            
           add$ = "Y"  
	   if PageNbr% = 1% then                                 ~
	   call "GETCODE" (#2, CustDist$, " ", 0%, 0, f1%(2))
           read #2, hold, key = CustDist$,                       ~
                                      eod goto L30135              
           get #2,  using L30135,                                ~
                    Addr1$, Addr2$, City$, State$, Zip$             
           add$ = "N"  

L30135:    FMT POS(4), CH(35), CH(35), CH(25), CH(02), CH(10)
           return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
save_data
	   put #1, using L33310,  CustCode$, CustDist$, CustYard$ 
L33310:    FMT CH(09), CH(03), CH(06)
           if add$ = "Y" then goto L33315
           rewrite #1
	   fieldnr% = 0%
           go to L33320

L33315:    write #1
	   fieldnr% = 0%

L33320:    if OldCustSku$ = CustSku$ then goto L33322
REM----->  update CUSTOMER file, Sku field
           read #3, hold, key = CustKey$, eod goto L33322                      
           put #3, using L33322, CustSku$
           rewrite #3
L33322:    FMT POS(1000), CH(3)
           return

save_dist_data
           put #2, using L33410,        ~
                   CustDist$, Addr1$, Addr2$, City$, State$, Zip$

L33410:    FMT CH(4), CH(35), CH(35), CH(25), CH(02), CH(10)
           if add$ = "Y" then goto L33415
           rewrite #2
	   fieldnr% = 0%
           go to L33420

L33415:    write #2
	   fieldnr% = 0%
L33420:    return

delete_data
           if add$ = "Y" then return        
	   delete #1
	   return

delete_dist
           if add$ = "Y" then return        
	   delete #2
	   return

setpf_acct
         pfd$(1) = "(1)Start Over                           "   &        ~
                   "                       (13)Instructions"
         pfd$(2) = "(3)Edit District                (6)Delet"   &        ~
                   "e                      (15)Print Screen"
         pfd$(3) = "                                (7)Save "   &        ~
                   "                       (16)Exit Program "
         str(pf$()) = hex(010203ffff0607ffffffffff0dff0f101700ffffff)
         return

setpf_dist
         pfd$(1) = "(1)Start Over                           "   &        ~
                   "                       (13)Instructions"
         pfd$(2) = "(3)Edit Account                 (6)Delet"   &        ~
                   "e                      (15)Print Screen"
         pfd$(3) = "                                (7)Save "   &        ~
                   "                       (16)Exit Program "
         str(pf$()) = hex(010203ffff0607ffffffffff0dff0f101700ffffff)
         return

setpf_save
         pfd$(1) = "(1)Start Over                           "   &        ~
                   "                       (13)Instructions"
         pfd$(2) = "                                (6)Delet"   &        ~
                   "e                      (15)Print Screen"
         pfd$(3) = "(4)Previous Field               (7)Save "   &        ~
                   "                       (16)Exit Program "
         str(pf$()) = hex(01ffff04ff0607ffffffffff0dff0f101700ffffff)
         return

setpf_edit
         pfd$(1) = "(1)Start Over                           "   &        ~
                   "                                       "
         pfd$(2) = "                                (6)Delet"   &        ~
                   "e                                      "
         pfd$(3) = "                                (7)Save "   &        ~
                   "                                        "
         str(pf$()) = hex(01ffffffff0607ffffffffffffffffff1700ffffff)
         return

startover:
         call "STARTOVR" (u3%)
	 if u3% = 1% then return
	    return clear all
         if PageNbr% = 2% then goto dist_screen
         goto main_screen

exit_program:
            end
