REM         +---------------------------------------------------------------+
REM         | load kanban master file (old style labels)                    |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = CUSTOMER            */~
            hold_key$9,                  /* Detail Record              */~
            hold_type$4                    

        dim f2%(60%),                    /* = 0 if the file is open    */~
            f1%(60%),                    /* = 1 if READ was successful */~
            fs%(60%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(60%)20                 /* Text from file opening     */ 
            
            dim csv_rec$256, kan_rec$144, q$1, qq$2     
            dim comma(50), func$10, u_m$3, sub_inv$11 
            dim v1$8, v2$8, v3$8, message$256
	    dim fields$(50)256, init_rec$256, wrk_rec$256
            dim key0$9, status$1, xref$9, sort_name$30, filler$11         
	    dim seq_nbr$6, part_nbr$25, part_desc$64, pour$16, qty$6

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$41, pname$32, apc1$41                   /* (EWD055) */
                                                            /* (EWD060) */
                                                            /* (EWD066) */
                                                            /* (EWD068) */
                                                            /* (EWD072) */
                                                            /* (AWD077) */
                                                            /* (AWD082) */ 
        REM *************************************************************

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
            * #1  ! KANBANMS ! Kanban master file                       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,   "KANOLDMS",                                     ~
                        varc,     indexed,  recsize = 144,              ~
                        keypos =   26, keylen =   6,                    ~
                        alt key  1, keypos =   1, keylen =  31
/*
 part# 1-25  ch(25)
 seq#  26-31 ch(06)
 desc  32-95 ch(64)
 pour  96-111 ch(16)
 qty   112-117 ch(06)
 UofM  118 - 120 ch(3)
 SubInv 121 - 131 ch(11)   
filler 132 - 144 ch(13)
*/
            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 500%, rslt$(1%))

            mat f1% = zer

	    /* Open file from Oracle */
	    csv_rec$ = "test.csv"          
	    func$ = "open"
	    err% = 0%
             call "SEQREAD" (func$,csv_rec$,err%)
	     if err% <> 0% then goto END_JOB

	     /* initialise variables */
            v1% = 0%
	    v2% = 0%
	    v3% = 0%
             sold_sw$ = "0"
	     mult_bill$ = "0"
             mult_rec$ = "0"
	    init(" ") init_rec$, hold_key$
	    q$ = hex(22)
	    qq$ = q$ & q$

L01000:      /* main loop */
	    func$ = "read"
	    csv_rec$ = init_rec$
	    err% = 0%
	    /* Read record */
             call "SEQREAD" (func$,csv_rec$,err%)
	     if err% <> 0% then goto END_JOB
             l = 0
	     v1% = v1% + 1%
             wrk_rec$ = csv_rec$            
            /* append '|' to record */
	     for y = 1 to 255        
		x = 256 - y
		if str(wrk_rec$,x,1) <= " " then goto skip_char 
                   str(wrk_rec$,x+1,1) = ","
		   max_col = x + 1
		   y = 255
skip_char:
	     next y
	     comma(1) = 0
	     comma(2) = 0
	     comma(3) = 0
	     comma(4) = 0
	     comma(5) = 0
	     comma(6) = 0
	     comma(7) = 0

            /* find ',' field delimiters and table */
	    quoteSw = 1
             l = 0
	     for x = 1 to max_col
	       if str(wrk_rec$,x,1) = q$ then quoteSw = quoteSw * -1
               if quoteSw < 0 or str(wrk_rec$,x,1) <> "," then go to skip_it
                     l = l + 1
                     comma(l) = x
skip_it:
	     next x

            /* break out fields and store in 'fields$' table */
	     fields$(1) = str(wrk_rec$,1,(comma(1)-1)) & "         "
             strlen = comma(1) -1
	     fields$(1) = " "    
	     p = 1
	     for y=1 to strlen
	         if str(wrk_rec$,(y),1) <> q$ then               ~
	             str(fields$(1),p,1) = str(wrk_rec$,(y),1)
	         if str(wrk_rec$,(y),1) <> q$ then               ~
	             p = p + 1
	         if str(wrk_rec$,y,2) = qq$ then               ~
	             str(fields$(1),p,1) = q$                   
	         if str(wrk_rec$,y,2) = qq$ then               ~
	             p = p + 1
	         if str(wrk_rec$,y,2) = qq$ then               ~
	             y = y + 1
	     next y                

             for x = 2 to 7
		 strlen = comma(x) - comma(x - 1) -1
		 fields$(x) = " "    
		 p = 1
		 z = comma(x - 1) 
		 for y=1 to strlen
		   if str(wrk_rec$,(z+y),1) <> q$ then               ~
	              str(fields$(x),p,1) = str(wrk_rec$,(z+y),1)
		   if str(wrk_rec$,(z+y),1) <> q$ then               ~
		      p = p + 1
		   if str(wrk_rec$,z+y,2) = qq$ then               ~
	              str(fields$(x),p,1) = q$                   
		   if str(wrk_rec$,z+y,2) = qq$ then               ~
		      p = p + 1
		   if str(wrk_rec$,z+y,2) = qq$ then               ~
		      y = y + 1
		 /* handle quotes */
		 next y                
    	     next x
/*
 part# 1-25  ch(25)
 seq#  26-31 ch(06)
 desc  32-95 ch(64)
 pour  96-111 ch(16)
 qty   112-117 ch(06)
 UofM  118 - 128 ch(11)
 SubInv 129 - 132 ch(3)   
filler 133 - 145 ch(13)
*/

REM  dim seq_nbr$6, part_nbr$25, part_desc$64, pour$16, qty$6
   	     seq_nbr$ = str(fields$(7),1,6) & "        "           
   	     read #1,hold,key = seq_nbr$, USING KANOLDFMT,                  ~  
	           part_nbr$,seq_nbr$, part_desc$, pour$, qty$,             ~ 
	           u_m$,sub_inv$, filler$,    ~ 
		   eod goto L02000
             delete #1
KANOLDFMT: FMT CH(25), CH(06), CH(64), CH(16), CH(06), CH(3), CH(11), CH(13)                       
/*
 part# 1-25  ch(25)
 seq#  26-31 ch(06)
 desc  32-95 ch(64)
 pour  96-111 ch(16)
 qty   112-117 ch(06)
 UofM  118 - 120 ch(3)
 SubInv 121 - 131 ch(11)   
filler 132 - 144 ch(13)
*/

REM          goto L01000

L02000:  /* NOT FOUND */
	    fields$(1) = fields$(1) & "                              "
            part_nbr$  = str(fields$(1),1,25)
	    fields$(7) = fields$(7) & "          "                 
   	    seq_nbr$   = str(fields$(7),1,6)            
	    fields$(2) = fields$(2) & "                                 "
	    fields$(2) = fields$(2) & "                                 "
            part_desc$ = str(fields$(2),1,64)
	    fields$(6) = fields$(6) & "                           "
            pour$      = str(fields$(6),1,16)
	    fields$(5) = fields$(5) & "                           "
            qty$       = str(fields$(5),1,6)
	    fields$(3) = fields$(3) & "   "
            u_m$       = str(fields$(3),1,3)
	    fields$(4) = fields$(4) & "                      "
            sub_inv$   = str(fields$(4),1,11)
            filler$    = "               "
	    message$ = "desc = " & part_desc$
REM          call "LOGFILE" (message$)
            init(" ") kan_rec$
	    str(kan_rec$,01,25) = part_nbr$
	    str(kan_rec$,26,06) = seq_nbr$
	    str(kan_rec$,32,64) = part_desc$
	    str(kan_rec$,96,16) = pour$        
	    str(kan_rec$,112,6) = qty$         
	    str(kan_rec$,118,3) = u_m$         
	    str(kan_rec$,121,11) = sub_inv$     
	    str(kan_rec$,132,13) = "             "
   	     write #1, USING KANOLDFM2, kan_rec$                            
KANOLDFM2:   FMT CH(144)
             goto L01000
	      
END_JOB:    /* END OF JOB */
            convert v1% to v1$, pic (########)
            convert v2% to v2$, pic (########)
            convert v3% to v3$, pic (########)
            end
