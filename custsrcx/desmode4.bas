
        dim                              /* FILE = APCPLNDT            */~
            pc_rec$(3)256      /* Detail Record              */
        dim                              /* FILE = APCPLNDT            */~
           tmp_rec$(3)256      /* Detail Record              */

        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
            pc_key$9,                    /*   doesn't exist, or 0 if   */~
            sv_key$9,                    /*   doesn't exist, or 0 if   */~
            rslt$(40%)20                 /* Text from file opening     */ 
            
        dim tbl$(10,5)3            

        dim                              /* (AWD083)                   */~
            Model$3,                     /* Calling Program Flag       */~
            message$256,                 /* Calling Program Flag       */~
            cat_tbl$(85)4                                                     
        dim mdl_tbl$(132)3                                                     

       dim logmsg$256

       dim new$3 
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
            * #1  ! APCPCMSD ! Pricing Master Definition File (New)     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "APCPCMSD",                                      ~
                        varc,     indexed,  recsize =   768,            ~
                        keypos =    1, keylen = 9                        


            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))


            tbl$(01,1) = "9A2"
            tbl$(01,2) = "9A1"
            tbl$(01,3) = "9A1"
            tbl$(01,4) = "9A3"
            tbl$(01,5) = "9A3"
            tbl$(02,1) = "9A6"
            tbl$(02,2) = "9A5"
            tbl$(02,3) = "9A5"
            tbl$(02,4) = "9A7"
            tbl$(02,5) = "9A7"
            tbl$(03,1) = "9B0"
            tbl$(03,2) = "9A9"
            tbl$(03,3) = "9A9"
            tbl$(03,4) = "9B1"
            tbl$(03,5) = "9B1"
            tbl$(04,1) = "9B4"
            tbl$(04,2) = "9B3"
            tbl$(04,3) = "9B3"
            tbl$(04,4) = "9B5"
            tbl$(05,5) = "9B5"
            tbl$(05,1) = "9B8"
            tbl$(05,2) = "9B7"
            tbl$(05,3) = "9B7"
            tbl$(05,4) = "9B9"
            tbl$(05,5) = "9B9"
            tbl$(06,1) = "928"
            tbl$(06,2) = "928"
            tbl$(06,3) = "927"
            tbl$(06,4) = "9C0"
            tbl$(06,5) = "9C0"
            tbl$(07,1) = "929"
            tbl$(07,2) = "932"
            tbl$(07,3) = "931"
            tbl$(07,4) = "9C2"
            tbl$(07,5) = "9C2"
            tbl$(08,1) = "933"
            tbl$(08,2) = "936"
            tbl$(08,3) = "935"
            tbl$(08,4) = "9C4"
            tbl$(08,5) = "9C4"
            tbl$(09,1) = "937"
            tbl$(09,2) = "940"
            tbl$(09,3) = "939"
            tbl$(09,4) = "9C6"
            tbl$(09,5) = "9C6"
            tbl$(10,1) = "941"
            tbl$(10,2) = "944"
            tbl$(10,3) = "943"
            tbl$(10,4) = "9C8"
            tbl$(10,5) = "9C8"
            mat f1% = zer
            upd% = 0%
            add% = 0%
            tst% = 0%
	    cnt% = 0%           

L01000:      /* main loop */
            l = 1                  
	    pc_key$ = "000000926"
            goto L01100

L01050:
                str(pc_key$,7,3) = "ZZZ"                
		goto L01100

L01075:
                str(pc_key$,7,3) = "926"                

L01100:
	    cnt% = cnt% + 1
                read #1, key > pc_key$, using L00020, pc_rec$(),        ~
                                                   eod goto FINI   
                pc_key$ = str(pc_rec$(),1,9)
                if str(pc_key$,1,4) <> "0169" then goto L01150
                sv_key$ = pc_key$

L01150:
                if str(pc_key$,7,3) < "926"  then goto L01075
                if str(pc_key$,7,3) > "9C8"  then goto L01050

REM         if tbl$(1-10,2-5) = model$, roll to tbl$(1-10,1)
            Model$ = str(pc_key$,7,3)
            for x% = 1% to 10%
	    y% = x%
	    if Model$ = tbl$(x%,2) then gosub process 
	    if Model$ = tbl$(x%,3) then gosub process 
	    if Model$ = tbl$(x%,4) then gosub process 
	    if Model$ = tbl$(x%,5) then gosub process 
	    next x%
            goto L01100

process:         
            sv_key$ = pc_key$
            str(pc_key$,7,3) = tbl$(y%,1%)        
            str(pc_rec$(1),7,3) = tbl$(y%,1%)        
            tmp_rec$(1) = pc_rec$(1)
            tmp_rec$(2) = pc_rec$(2)
            tmp_rec$(3) = pc_rec$(3)
            read #1,hold, key = pc_key$, using L00020, pc_rec$(),     ~
                                                   eod goto L01500
REM         rewrite #1, using L00020, pc_rec$()
REM	    upd% = upd% + 1%
            pc_key$ = sv_key$
            return           

L01500:
            write #1, using L00020, tmp_rec$()
            pc_key$ = sv_key$
            add% = add% + 1%
            return

L00010:      FMT CH(04)
L00020:      FMT 3*CH(256)	
L00030:      FMT CH(09)  
FINI:       print "         records read    = ", cnt%
            print "         records added   = ", add%
            print "         records updated = ", upd%
             end
	      
