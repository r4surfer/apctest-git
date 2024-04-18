REM         +---------------------------------------------------------------+
REM         | copy model 849 with ref 27/28 to 819                          |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            pc_rec$(3)256,apckey$9          /* Detail Record              */

        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),tmp_rec$(3)256,        /* = 1 if READ was successful */~
            fs%(40%),apckey2$9,          /* = 1 if file open, -1 if it */~
            pc_ref$8, readkey$50,        /*   doesn't exist, or 0 if   */~
            r_rec$30, message$256,       /*   not yet checked (OPENCHCK*/~
            rslt$(40%)20                 /* Text from file opening     */ 
            
            

        dim                              /* (AWD083)                   */~
            Ref$2,                       /* Calling Program Flag       */~
            Model$3,                     /* Calling Program Flag       */~
            No_Price$(458)4,                                             ~
            wrkkey$7,                                                    ~
            wrkrec$9,                                                    ~
            work_rec$9,                                                    ~
            Calc$3                                                        


       dim Price_3$(178)3, Price_1$(22)3, PI_Catalog$(39)4, mdl$(11)3

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
            * #2  ! WORKFILE !                                          *~
            * #4  ! WORKFIL2 !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "APCPCMSD",                                      ~
                        varc,     indexed,  recsize =   768,            ~
                        keypos =    1, keylen = 9                        

            select #2, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =  9,              ~
                        keypos =    1, keylen =   9

            select #4, "WORKFIL2",                                      ~
                        varc,     indexed,  recsize =   768,            ~
                        keypos =    1, keylen = 9                        

            call "FILEBGON" (#2)
            call "FILEBGON" (#4)
            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),500%, rslt$(2%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),500%, rslt$(4%))

            mat f1% = zer
            upd% = 0%
            tst% = 0%
	    cnt% = 0%           
	    init(hex(00)) apckey$

/* models to copy from cat 000 to cats with the model present */
            mdl$(01) = "413"
            mdl$(02) = "416"
            mdl$(03) = "420"
            mdl$(04) = "260"
            mdl$(05) = "269"
            mdl$(06) = "261"
            mdl$(07) = "262"
            mdl$(08) = "263"
            mdl$(09) = "264"
            mdl$(10) = "378"
            mdl$(11) = "388"


L01000:      /* main loop */
 	     init(" ") apckey$
	     str(apckey$,01,04) = "0000"         /* catalog */
	     str(apckey$,05,02) = "  "           /* catalog methode */
	     str(apckey$,07,03) = "   "          /* model code */

L01200:
            read #1, key > apckey$, using L50760, pc_rec$(),          ~
                                                      eod goto L01500
             apckey$ = str(pc_rec$(),1,9)
	     if str(apckey$,01,04) = "0000" then L01250                     
             for l% = 1% to 11%
                 if str(apckey$,07,03) = mdl$(l%) then L01300        
   	     next l%
             goto L01200

L01250:
             for l% = 1% to 11%
                 if str(apckey$,07,03) = mdl$(l%) then L01275        
   	     next l%
             goto L01200

L01275:          /* write to workfil2 */

             write #4, using L50760, pc_rec$()

             goto L01200

L01300:
            /* write model/catelog */
	    work_rec$ = str(apckey$,7,3) & str(apckey$,1,6)
            call "LOGFILE" (work_rec$) 
            write #2, using L50770, work_rec$
	    goto L01200

L01500:
 	    init(" ") apckey$
	    message$ = "start part 2"
            call "LOGFILE" (message$)  
L01550:
            read #4, key > apckey$, using L50760, pc_rec$(),          ~
                                                      eod goto L01700
            apckey$ = str(pc_rec$(),1,9)
 	    init(" ") wrkkey$
            wrkkey$ = str(pc_rec$(),7,3) & "    "
L01600:
            read #2, key > wrkkey$, using L50760, wrkrec$,          ~
                                                      eod goto L01550
            wrkkey$ = str(wrkrec$,1,9)
            model$  = str(wrkrec$,1,3)
            cat$    = str(wrkrec$,4,6)
   	    if str(wrkkey$,1,3) <> str(pc_rec$(),7,3) then L01600 
            str(pc_rec$(),1,6) = cat$        
REM write #1
            apckey2$ = str(pc_rec$(),1,9)
            read #1, key = apckey2$, hold, using L50760, tmp_rec$(),  ~
				    eod goto L01650
            delete #1
            goto L01650

            rewrite #1, using L50760, pc_rec$()
            goto L01600

L01650:
            write #1, using L50760, pc_rec$()
            goto L01600


L01700:



L50760:     FMT 3*CH(256)
L50770:     FMT CH(9)
L56890:     print "         records read    = ", cnt%
            print "         records test    = ", tst%
            print "         records updated = ", upd%
REM         call "FILEBGON" (#2)
REM         call "FILEBGON" (#4)
             end
	      
