REM         +---------------------------------------------------------------+
REM         | Add date to AMTBOMIF.                                         |
REM         +---------------------------------------------------------------+

  dim                              /* (AMTBOMIF) - File                */~
            des_model$15,                /* Model Number               */~
            val_model$15,                /* Model Number               */~
            val_field$2,                 /* Field Number               */~
            val_value$15,                /* Field Value                */~
            val_desig$1,                 /* Field Type I,E,U,L         */~
            val_scr$20,                  /* Screen Description         */~
            val_prt$30,                  /* Printed Description        */~
            val_phantom$25,              /* Phantom Part Number        */~
            val_bom$3,                                                   ~
            val_filler$9                 /* BOM ID for Phantom         */  

  DIM                             /* New validation record data vars   */~
            neu_field$2,                 /* New Field Number           */~
            neu_value$15,                /* New Field Value            */~
            desc$30,                     /* description work area      */~ 
            neu_src$20,                  /* New Screen Description     */~
            neu_prt$30                   /* New Printed Description    */ 

  DIM       options02A$4,                /* Possible vals for 02A      */~
            options02$8,                 /* Possible vals for 02       */~
            options06$4,                 /* Possible vals for 06       */~
            options07$27                 /* Possible vals for 07       */

  DIM       sKey$32,                     /* Key for AMTBOMIF data file */~
            sMainKey$32,                 /* Key for AMTBOMIF data      */~
            sGenKey$24,                  /* Key for GENCODES data      */~
            sDoors$51,                   /* All door models            */~
            fDoors(10%),                 /* Search for model           */~
            sValPart$8                   /* Descr of Partnum portion   */

  DIM       bSuccess$1                   /* Bool for pass/fail         */
                                                                                 
 DIM        sCount$8,                    /* Displays Process Rec Count */~
            sTest$10                     /* Test Var - remove later!!! */ 

REM   ** UNDECLARED VARS **		 !!!                             ~											        
        iCount%   = record counter
       

REM         +-----+---------------------------------------------------------+
REM         |     |                                                         |
REM         +-----+---------------------------------------------------------+
REM         |     |                                                         |
REM         |     |                                                         |
REM         |     |                                                         |
REM         |     |                                                         |
REM         |     |                                                         |
REM         |     |                                                         |
REM         +-----+---------------------------------------------------------+

   select #1,  "AMTBOMIF", varc,     indexed,  recsize = 120,            ~
                        keypos = 1,    keylen =  32                     
                                  
   select #3,  "GENCODES", varc,     indexed,  recsize = 128,            ~
                        keypos = 1,    keylen = 24
      
   select #4,  "AMTBOMPM", varc,     indexed,  recsize = 250,            ~
                        keypos = 1,    keylen = 2                        
  
   call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 200%, rslt$(01))           
            call "OPENCHCK" (#03, fs%(03), f2%(03), 200%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 200%, rslt$(04))

            f1%(1%), f1%(2%), f1%(3%), f1%(4%) = 0%

REM --------------------------- INIT VARS ------------------------------

  /* need a counter? !!!*/
  iCount% = 0  /* used to count num of recs processed */
  A% = 0
  B% = 0
  bDupe% = 0
  bWriteOK% = 0
 
  init(" ") sMainKey$, sCount$     

  options02A$ = "26AB"
  options02$ = "CDEFGHIJ"
  options06$ = "ABCD"
  options07$ = "ABCDEFGKLMNOQRSTUZ789IPVWXY"

  sDoors$ = "311,312,313,314,315,316,321,326,332,333,334,335,336" 
           /*too long?!!!*/  
  mat fDoors = zer

        REM *************************************************************~
            *              F O R M A T   S T A T E M E N T S            *~
            *************************************************************

L10100: FMT                 /* FILE: AMTBOMIF                          */~
            CH(15),         /* Model Number                            */~
            CH(2),          /* Definition for elements whose Type = FIE*/~
            CH(15),         /* Value of the field of the Model         */~
            CH(1),          /* Include / Exclude the Value             */~
            CH(20),         /* Verbage to appear on the Screen Displays*/~
            CH(30),         /* Verbage to print on the Hardcopy        */~
            CH(25),         /* Phantom Assembly Part Number            */~
            CH(03),         /* Bom Id Associated with Phantom          */~
            CH(09)          /* Filler Area                             */


REM ---------------------------- GET DATA ------------------------------
           des_cnt% = 0%     
	   des_model$ = "             "
 READ_NEXT:
            /* Next record loaded into vars */       
            read #1, key > sMainKey$, eod goto END_RECS

            /* Record data loaded into main vars */
            get #1, using L10100, val_model$, val_field$, val_value$,    ~
                                  val_desig$, val_scr$, val_prt$,        ~
                                  val_phantom$, val_bom$, val_filler$    
                                   

            /* Set Main Loop Key data */    
REM            if str(sMainKey$,1%,3%) <> val_model$ then gosub NEW_MODEL

            str(sMainKey$,1%,15%)  = val_model$
            str(sMainKey$,16%,2%)  = val_field$
            str(sMainKey$,18%,15%) = val_value$
            if des_model$ = val_model$ then goto READ_NEXT
            des_model$ = val_model$
            des_cnt% = des_cnt% + 1%
            if val_desig$ = " " then val_desig$ = "I"
            model_fnd$ = "0"
            gosub check_gencodes             
            if model_fnd$ = "0" then                                     ~
	        gosub PROCESS_RECORD
            goto READ_NEXT

check_gencodes:
            sGenKey$ = "PLANPARTS         "                               
            str(sGenKey$,10,15) = val_model$
            read #3,key = sGenKey$, using L60170, desc$,                 ~
                              eod goto check_next
            model_fnd$ = "1"
            return 
check_next:
            sGenKey$ = "SCREENONL         "                               
            str(sGenKey$,10,15) = val_model$
            read #3,key = sGenKey$, using L60170, desc$,                 ~
                              eod goto not_found 
            model_fnd$ = "1"
not_found:  return 





 PROCESS_RECORD:                                                      
            /*-------------- !!! TESTS FOR UPDATE MATCH -------------*/  
            /* Test for match to update!!! */     
            /*Set bool back to false!!!*/        
            bSuccess$ = "F"     
            neu_field$ = "18" : neu_value$ = "0"
            gosub FIND_DESCRIPTION                              
            gosub CREATE_VALIDATION      

            neu_field$ = "18" : neu_value$ = "1"
            gosub FIND_DESCRIPTION                              
            gosub CREATE_VALIDATION      

            neu_field$ = "18" : neu_value$ = "2"
            gosub FIND_DESCRIPTION                              
            gosub CREATE_VALIDATION      

            neu_field$ = "18" : neu_value$ = "3"
            gosub FIND_DESCRIPTION                              
            gosub CREATE_VALIDATION      

            return           

END_RECS:
      /* close data files and display total rec count */
      call "SHOSTAT" ("End of Records ") : STOP /*!!!*/     
      close #1 
REM   convert iCount% to sCount$, pic(00000)
      convert des_cnt% to sCount$, pic(00000)
      call "SHOSTAT"                                                     ~
         ("Transfer complete. Num of Recs Processed: " & sCount$) : STOP
      gosub EXIT_PROGRAM

        REM *************************************************************~
            *                                                           *~
            *           S p e c i a l   S u b r o u t i n e s           *~
            *                                                           *~          
            *************************************************************

  FIND_DESCRIPTION /*****************************************************/
rem call "SHOSTAT" ("Find Description ")  STOP /*!!!*/
            read #4,key = neu_field$, eod goto L60190
            get #4, using L60160, sValPart$
L60160:         FMT POS(03), CH(08)  
            /*We have the description of this portion of partnum (ex    */
            /*  'COLOR') and can now lookup text in GENCODES.           */
            sGenKey$ = sValPart$
            str(sGenKey$,10,15) = neu_value$
            read #3,key = sGenKey$, using L60170, desc$,                 ~
                                                           eod goto L60190
L60170:         FMT POS(25), CH(30)
	    sw% = 0%
	    s2% = 0%
            for l% = 1% to 30%
	      if sw% > 1% and s2% = 0% and str(desc$,l%,1%) <> " " then s2% = l%
              if str(desc$,l%,1%) = "-" then sw% = l% - 1% 
            next l%                                                  
            neu_src$ = str(desc$,1%,sw%)          
            neu_prt$ = str(desc$,s2%,30%)          
            goto L60200  /*success, exit*/
L60190:     init("?") neu_src$, neu_prt$

L60200:  return /********************************************************/

  CREATE_VALIDATION /****************************************************/  
            /* Write out new validation record */ 
rem call "SHOSTAT" ("Create Rec - " )  STOP /*!!!*/          
            bWriteOK% = 0% /*use error checking? !!!*/
            str(sKey$,1%,15%)  = val_model$
            str(sKey$,16%,2%)  = neu_field$   
            str(sKey$,18%,15%) = neu_value$
            read #1, key = sKey$, hold, using keyfmt, sKey$, eod goto notfound  
keyfmt: FMT CH(32)
            delete #1          
notfound:
            put #1, using L10100, val_model$, neu_field$, neu_value$,    ~                                                       
                                "I", neu_src$, neu_prt$, " ", " ", " "
            write #1, eod goto L60250
                    
            bWriteOK% = 1%

L60250:  return /********************************************************/    

  ERROR_EXIT /***********************************************************/
        call "SHOSTAT"("Error ! " & sMainKey$) :  STOP /*!!!*/  

L60500: goto END_RECS /**************************************************/

EXIT_PROGRAM:
      call "SHOSTAT" ("Closing Program; One Moment Please") :STOP
      end

  
