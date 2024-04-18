         REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCCTFIX                             *~
            *  Creation Date     - 10/09/97                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - David J. Dulong                      *~
            *                                                           *~
            *  Description       - Program to extract the complaint     *~
            *                      text records from the text file      *~
            *                      (txtfile.dat) and move them into     *~
            *                      thier own text file.                 *~
            *                                                           *~
            *                      Note - the text IDs are NOT being    *~
            *                      changed.  They are being left        *~
            *                      as they were and a new N records     *~
            *                      is being created in the text file    *~
            *                      reflecting the next text ID to use.  *~
            *                                                           *~
            *                      This new number will be 100 higher   *~
            *                      than the highest number read in.     *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/09/97 ! New Program for (APC) - Last Mod Date    ! DJD *~
            *          !                                          !     *~
            *************************************************************

	dim comp_key$5,  		 /* Complaint code key         */~
     	    comp_init_txt$4,             /* Complaint init text id     */~
     	    comp_svcs_txt$4,             /* Complaint service text id  */~
     	    comp_trk_txt$4,              /* Complaint tracking text    */~
     	    comp_srce_txt$4,             /* Complaint srouce text id   */~
            working_msg$20,              /* processing message         */~
	    high_text_id%,               /* highest text ID (converted)*/~
            totextid$4,			 /* text id to copy to         */~
            txtid%,                      /* text id from record        */~
            error%,                      /* error flag                 */~
            reccnt$10,                   /* record counter string      */~
            reccnt%,                     /* record counter             */~
 	    low_text_id%,	         /* save lowest text id        */~
  	    compid$5,	  	         /* complaint id               */~
	    dupid%,		         /* new id to assign to dups   */~
	    origtxt%,  	                 /* original text id           */~
            filler$47,   		 /* filler used in text record */~
	    notxtmsg$70,	         /* no text found              */~
            filler2$140	 	         /* more filler                */


        dim text_rec$(3%)70,             /* TEXT RECORD BUFFER         */~
            text_key$11,                 /* TEXT KEY                   */~
            text$64,                     /* TEXT DEFINITION AREA       */~
            sav_txt$9,                   /* CHECK TEXT KEY             */~
            txt%                         /* Actual Text In Integer     */

        dim f2%(3%),                     /* = 0 if the file is open    */~
            f1%(3%),                     /* = 1 if READ was successful */~
            fs%(3%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(3%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "01.00.00 10/09/96 Complaint Text Fix Utility     "
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
            * #1  ! APCCOMPT ! COMPLAINT TRACKING MASTER FILE           *~
            * #2  ! TXTFILE  ! MASTER SYSTEM TEXT FILE                  *~
            * #3  ! TXTCOMPL ! NEW COMPLAINT TEXT FILE                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "APCCOMPT",                                     ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos =    1, keylen =    5,                    ~
                        alt key  1, keypos =    6, keylen =  10,         ~
                            key  2, keypos =   16, keylen =  16, dup,    ~
                            key  3, keypos =   32, keylen =   8, dup,    ~
                            key  4, keypos =   40, keylen =  13, dup

            select #2,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

            select #3,  "TXTCOMPL",                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11


	/* Open The Files */		

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1%), f2%(1%), 0%, rslt$(1%))
	    call "OPENCHCK" (#2,  fs%(2%), f2%(2%), 0%, rslt$(2%))
            call "OPENCHCK" (#3,  fs%(3%), f2%(3%), 0%, rslt$(3%))
	    call "OPENCHCK" (#4,  fs%(4%), f2%(4%), 0%, rslt$(4%))
STOP

	mat f1% = zer

        if fs%(1%) <> 0 and fs%(2%) <> 0 and fs%(3%) <> 0 then   ~
               goto Initializing




Initializing:
        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

	    high_text_id% = 0
	    low_text_id%  = 999999999


	    INIT(" ") comp_init_txt$, comp_svcs_txt$, comp_trk_txt$,     ~
                      comp_srce_txt$, working_msg$,                      ~
                      comp_key$

	    dupid% = 0

          REM *************************************************************~
             *                P R O C E S S I N G                        *~
             *-----------------------------------------------------------*~
             * Main Processing Loop                                      *~
             *************************************************************

           display " "
 
	  comp_key$ = all(hex(00))
           read #1, hold, key > comp_key$, using FMTAPCCOMPT, comp_init_txt$,   ~
                         comp_svcs_txt$, comp_trk_txt$, comp_srce_txt$,         ~
                  eod goto normalexit

           gosub handle_move_text

read_next:
           reccnt% = reccnt% + 1


           convert reccnt% to reccnt$, pic(##########)
           working_msg$ = "Processing:" & reccnt$

           print at(1,1), working_msg$

           read #1, hold, using FMTAPCCOMPT, comp_init_txt$,                   ~
                  comp_svcs_txt$, comp_trk_txt$, comp_srce_txt$,         ~
                  eod goto normalexit

	   gosub handle_move_text
           goto read_next

REM =====================================================================
REM                         END OF MAIN MODULE CODE 
REM =====================================================================



         REM *************************************************************~
             *           L O C A L   F U N C T I O N S                   *~
             *-----------------------------------------------------------*~
             * deffn functions go here!                                  *~
             *************************************************************

        deffn'099(textid$)
            txt% = 0%
            if textid$ = hex(00000000) or textid$ = hex(ffffffff)        ~
                                           or textid$ = " " then return
            txt% = 1%
        return

         REM *************************************************************~
             *           L O C A L   S U B R O U T I N E S               *~
             *-----------------------------------------------------------*~
             * "GOSUB" Sub Routines                                      *~
             *************************************************************

handle_move_text:
	totextid$ = comp_init_txt$      /* Re-Write the Initiator Text */   
	gosub move_txt
        if error% <> 0 then goto programexit

	totextid$ = comp_svcs_txt$      /* Re-Write the service Text   */   
        gosub move_txt
        if error% <> 0 then goto programexit

	totextid$ = comp_trk_txt$       /* Re-Write the tracking Text   */   
        gosub move_txt
        if error% <> 0 then goto programexit

	totextid$ = comp_srce_txt$       /* Re-Write the source Text   */   
        gosub move_txt
        if error% <> 0 then goto programexit

        return

move_txt:                             /* To History Text       */

            init(" ") text_key$, sav_txt$, text_rec$(), text$
            error% = 0 
            gosub'099(totextid$)

   	    get totextid$, using L61640, txtid%
L61640:        FMT BI(4)

            if txtid% = -1 then txt% = 0

            /* no text id assigned so get out! */
            if txt% = 0% then goto move_txt_done

	    origtxt% = txt%

            if txtid% > low_text_id% then goto CheckHigh

	    low_text_id% = txtid%
     
	    convert low_text_id% to working_msg$, pic(##########)
	    print at(2,1), "Low Text ID: " & working_msg$


CheckHigh:
            if txtid% < high_text_id% then goto StartMove

	    high_text_id% = txtid%

	    convert high_text_id% to working_msg$, pic(##########)
	    print at(3,1), "High Text ID: " & working_msg$

StartMove:
            text_key$ = all(hex(00))
            str(text_key$,1%,1%) = "M"
            str(text_key$,2%,3%) = "   "
            str(text_key$,5%,4%) = totextid$
            str(text_key$,9%,1%) = "1"
            sav_txt$ = str(text_key$,1%,9%)

move_txt_next:
            read #2, hold,key > text_key$, using L61650, text_key$,        ~
                                                   eod goto move_txt_done
L61650:        FMT CH(11)

            if sav_txt$ <> str(text_key$,1%,9%) then goto move_txt_done

               get #2, using L61680, text$, text_rec$()
L61680:           FMT CH(64), 3*CH(70)

               REM  delete #3

               write #3, using L61680 , text$, text_rec$(), eod goto L61770
               delete #2
               txt_cnt% = txt_cnt% + 1%
               goto move_txt_next

	    REM =================================================
	    REM =   HANDLE DUPLICATE TEXT ID'S                  =
            REM =================================================

L61770:     
            REM convert txtid% to working_msg$, pic(##########)
            REM call "SHOSTAT" ("(Error)-Moving 'TEXT'?" & working_msg$) 
            REM STOP
            REM error% = 1

	    get #1, using L61771, compid$
L61771:        FMT CH(5) 

            if dupid% = 0 then dupid% = low_text_id% - 10001
            error% = 1

	    txtid% = dupid% + 1
            dupid% = txtid%
            
            convert txtid% to reccnt$, pic(##########)
            print at(4,1), "Problem Complaint ID: " & compid$ & " using " & reccnt$
            REM print at(5,1), "* Press ENTER * "
            REM print at(6,1), " "
            REM Stop
            print at(5,1), "                    "
            print at(6,1), "                    "


           read #1, hold, key = compid$, using FMTAPCCOMPT, comp_init_txt$,   ~
                         comp_svcs_txt$, comp_trk_txt$, comp_srce_txt$,         ~
                  eod goto rewrite_problem

            /* ======================================================= */
            /* now re-write the compalint record with the new text id! */
            /* ======================================================= */


	   /* INITIATOR TEXT */
	   if totextid$ <> comp_init_txt$ then goto checkservice

                put totextid$ using LFIX01, txtid%
LFIX01:              FMT BI(4)
                
                put #1 using L61775, txtid%
L61775:              FMT POS(93), BI(4)
                rewrite #1
		goto create_new


checkservice:
	    /* SERVICE TEXT */
	    if totextid$ <> comp_svcs_txt$ then goto checktracking

                put totextid$ using LFIX02, txtid%
LFIX02:              FMT BI(4)

                put #1 using L61776, txtid%
L61776:              FMT POS(105), BI(4)
                rewrite #1
		goto create_new


checktracking:
            /* TRACKING TEXT */
            if totextid$ <> comp_trk_txt$ then goto checksource

                put totextid$ using LFIX03, txtid%
LFIX03:              FMT BI(4)

                put #1 using L61777, txtid%
L61777:              FMT POS(117), BI(4)
                rewrite #1
		goto create_new

checksource: 
            /* SOURCE TEXT */
            if totextid$ <> comp_srce_txt$ then goto move_txt_done

                put totextid$ using LFIX04, txtid%
LFIX04:              FMT BI(4)

		put #1 using L61778, txtid%
L61778:              FMT POS(123), BI(4)
                rewrite #1
		goto create_new

rewrite_problem:  error% = 1
		goto move_txt_done

create_new: 	init(" ") filler$, filler2$, notxtmsg$
                
                notxtmsg$ = "Duplicate text ID found during coversion!  Text Lost! "

       		write #3 using textrecord, "M", txtid%, "1", 1%, "012", 1%, ~
                             70%, filler$, notxtmsg$, filler2$
textrecord:    FMT CH(1), POS(5), BI(4), CH(1), BI(2), CH(3), BI(2),BI(1),CH(47),   ~
                   CH(70),CH(140)
               error% = 0
               stop       

move_txt_done:
        return


   	    rem put txtid%, using L61780, totextid$
L61780:        FMT CH(4)





          REM *************************************************************~
              *               F O R M A T   S T A T E M E N T             *~
              *-----------------------------------------------------------*~
              * This is where all the program format statements are       *~
              *************************************************************	  

	      /* ====================================================== */
	      /* Format for reading in the text ID's from the complaint */
              /* record  (ACPCOMPT)					*/
	      /* ====================================================== */

FMTAPCCOMPT:  FMT   POS(93),CH(04),POS(105),CH(04),POS(117),CH(04),        ~
                    POS(123),CH(04)


	      /* ====================================================== */
	      /* Format for reading in the text files key fields        */
              /* (TXTFILE)                                              */
	      /* ====================================================== */
textfile_key: FMT   CH(11)                      

	      /* ====================================================== */
	      /* Format for converting bi numbers to ch                 */
	      /* ====================================================== */
convertbi4:    FMT CH(4)

	      /* ====================================================== */
	      /* Format for converting bi numbers to ch                 */
	      /* ====================================================== */
convertch4:    FMT BI(4)



normalexit: REM *************************************************************~
                *           N O R M A L   T E R M I N A T I O N             *~
                *-----------------------------------------------------------*~
                * This is where the normal program termination is           *~
                *************************************************************

          /* Now - re-write the N record with the new HIGH text ID */	  
	    call "SHOSTAT" ("Re-Write Next Text")
	    stop
            text_key$ = all(hex(00))
            str(text_key$,1%,1%)  = "N"
            str(text_key$,2%,10%) = "          "

            read #3, hold,key > text_key$, using textfile_key, text_key$,        ~
                                                   eod goto finish_write

               put #3, using L61690, high_text_id% + 200000
L61690:           FMT POS(05), BI(4)
               rewrite #3
               goto programexit

finish_write
          stop
	  goto programexit


programexit:
          REM *************************************************************~
              *               P R O G R A M   E X I T                     *~
              *-----------------------------------------------------------*~
              * Here is where the program exits back to the menu          *~
              *************************************************************

	call "SHOSTAT" ("Exiting, One Moment Please")
	end

