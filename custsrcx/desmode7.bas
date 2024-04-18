
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


            mat f1% = zer
            upd% = 0%
            add% = 0%
            tst% = 0%
	    cnt% = 0%           

L01000:      /* main loop */
            l = 1                  
	    pc_key$ = "000000312"
            goto L01100

L01050:
                str(pc_key$,7,3) = "ZZZ"                
		goto L01100

L01075:
                str(pc_key$,7,3) = "312"                

L01100:
	    cnt% = cnt% + 1
                read #1, key > pc_key$, using L00020, pc_rec$(),        ~
                                                   eod goto FINI   
                pc_key$ = str(pc_rec$(),1,9)
REM             if str(pc_key$,1,4) = "0000" then goto L01050
                if str(pc_key$,7,3) < "312"  then goto L01075
                if str(pc_key$,7,3) > "333"  then goto L01050
                if str(pc_key$,7,3) = "313"  then goto L01150
                if str(pc_key$,7,3) = "333"  then goto L01150
            goto L01100

L01150:         sv_key$ = pc_key$
                if str(pc_key$,7,3) = "313"  then model$ = "373"
                if str(pc_key$,7,3) = "333"  then model$ = "383"
                str(sv_key$,7,3) = model$             
                str(pc_rec$(),7,3) = model$              
		gosub update_rec
L01200:
            goto L01100

update_rec:
            tmp_rec$() = pc_rec$()
            read #1,hold, key = sv_key$, using L00020, pc_rec$(),     ~
                                                   eod goto L01500
            rewrite #1, using L00020, pc_rec$()
   	    upd% = upd% + 1%
            return           

L01500:
            write #1, using L00020, tmp_rec$()
            add% = add% + 1%
            return

L00010:      FMT CH(04)
L00020:      FMT 3*CH(256)	
L00030:      FMT CH(09)  
FINI:       print "         records read    = ", cnt%
            print "         records added   = ", add%
            print "         records updated = ", upd%
             end
	      
