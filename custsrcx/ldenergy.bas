REM         +---------------------------------------------------------------+
REM         | load ewdplnes file                                            |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = EWDPLNES            */~
            hold_key$9,                  /* Detail Record              */~
            hold_type$4                    

        dim f2%(60%),                    /* = 0 if the file is open    */~
            f1%(60%),                    /* = 1 if READ was successful */~
            fs%(60%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(60%)20                 /* Text from file opening     */ 
            
	    dim message$256
            dim csv_rec$256, rec$32, key$6     
            dim comma(50), func$10, temp$64 
            dim v1$8, v2$8, v3$8               
	    dim fields$(50)128, init_rec$256, wrk_rec$256
            dim key0$9, status$1         
	    dim model$3, group$3, dp$2, filler$8
	    dim group1$3, group2$3, group3$3

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
            * #1  ! EWDPLNESMS ! Kanban master file                       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,  "EWDPLNES",                                      ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos =  1,   keylen =   6
/*
 1-3    model$ CH(3)  
 4-6    group$ CH(3)
 7-14   res    PD(14,4)
 15-22  nores  PD(14,4)
 23-24  dp$    CH(2) 
 25-32  filler$ CH(8)  
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
	    init(" ") init_rec$, hold_key$
	    for l = 1 to 256
	    str(init_rec$,l,1) = " "
	    next l

L01000:      /* main loop */
	    func$ = "read      "
	    csv_rec$ = init_rec$
	    err% = 0%
	    for l = 1 to 50
	       comma(l) = l + 1
	       fields$(l) = "               "
            next l

	    /* Read record */
             call "SEQREAD" (func$,csv_rec$,err%)
	     if err% <> 0% then goto END_JOB

             l = 0
	     v1% = v1% + 1%
             wrk_rec$ = csv_rec$            
	     str(wrk_rec$,256,1) = ","
            /* append ',' to record */
	     for y = 1 to 255        
		x = 256 - y
		if str(wrk_rec$,x,1) <= " " then goto skip_char 
                   str(wrk_rec$,x+1,1) = ","
   		   y = 255
skip_char:
	     next y

            /* find '|' field delimiters and table */
	    l = 0
	     for x = 1 to 128
               if l = 7 and str(wrk_rec$,x,1) < " " then str(wrk_rec$,x,1) = ","
		 if str(wrk_rec$,x,1) <> "," then goto skip_it
                     l = l + 1
                     comma(l) = x
		     if l > 6 then goto exit_loop
skip_it:
	     next x
exit_loop:

            /* break out fields and store in 'fields$' table */
	     fields$(1) = str(wrk_rec$,1,(comma(1)-1)) & "         "
             for x = 2 to 7
		 strlen = (comma(x) - comma(x - 1)) - 1
	         fields$(x) = str(wrk_rec$,(comma(x-1)+1),strlen) & "         "
    	     next x

/*
 1-3    model$ CH(3)  
 4-6    group$ CH(3)
 7-14   res    PD(14,4)
 15-22  nores  PD(14,4)
 23-24  dp$    CH(2) 
 25-32  filler$ CH(8)  
*/

L02000:  /* NOT FOUND */
            res1  = 0.0000
            res2  = 0.0000
            res3  = 0.0000
            nores = 0.0000

	    fields$(1) = fields$(1) & "     "
            model$     = str(fields$(1),1,3)
	    fields$(2) = fields$(2) & "     "                 
    	    group1$    = str(fields$(2),1,3)            

	    fields$(3) = fields$(3) & "                       "
            temp$      = str(fields$(3),1,16)

REM     convert temp$ to res1, data goto L02100
            get temp$, using strfmt, res1
strfmt: FMT PIC(#.##)
L02100:
            fields$(4) = fields$(4) & "                           "
            group2$    = str(fields$(4),1,16)
            fields$(5) = fields$(5) & "     "
            temp$      = str(fields$(5),1,16)
REM	    convert temp$ to res2, data goto L02200
            get temp$, using strfmt, res2
L02200:     fields$(6) = fields$(6) & "                           "
            group3$    = str(fields$(6),1,16)
            fields$(7) = fields$(7) & "     "
            temp$      = str(fields$(7),1,16)
REM	    convert temp$ to res3, data goto L02300
            get temp$, using strfmt, res3
L02300:     dp$        = "  "
            filler$    = "             "
	    nores = 0.0000
	    res = res1
	    group$ = group1$
	    key$ = model$ & group$
	    read #1, hold, key = key$, using EWDPLNESFM2, rec$, eod goto L02400
	    delete #1  
L02400:	                                                             
	    write #1, USING EWDPLNESFMT,                                  ~
	           model$, group$, res, nores, dp$, filler$             
L02450:	    res = res2
	    group$ = group2$
	    key$ = model$ & group$
	    read #1, hold, key = key$, using EWDPLNESFM2, rec$, eod goto L02500
	    delete #1  
L02500:	                                                          
	    write #1, USING EWDPLNESFMT,                                  ~
	           model$, group$, res, nores, dp$, filler$             
L02550:	    res = res3
	    group$ = group3$
	    key$ = model$ & group$
	    read #1, hold, key = key$, using EWDPLNESFM2, rec$, eod goto L02600
	    delete #1  
L02600:	                                                          
	    write #1, USING EWDPLNESFMT,                                  ~
	           model$, group$, res, nores, dp$, filler$             
EWDPLNESFMT:   FMT CH(3), CH(3), PD(14,4), PD(14,4), CH(2), CH(8)
EWDPLNESFM2:   FMT CH(32)
L02650:      goto L01000
	      
END_JOB:    /* END OF JOB */
            end
