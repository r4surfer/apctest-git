        REM *-----------------------------------------------------------*~
            *                                                           *~
            *   A    W    W DDDD   GGGG  EEEEE TTTTTT   0000      11    *~
            *  A  A  W    W D   D G      E       T     0    0    1 1    *~
            * AAAAAA W WW W D   D GF GG  EEE     T     0    0   1  1    *~
            * A    A WW  WW D   D G    G E       T     0    0      1    *~
            * A    A W    W DDDD   GGGG  EEEEE   T      0000    11111   *~
            *                                                           *~            
            *-----------------------------------------------------------*~
            * AWDGET01 - Create Flat File To Send To Atrium.            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/01/06 ! Original                                 ! CMG *~
            *-----------------------------------------------------------*

        dim getpadpy_key$26,              /* GETPADPY Readkey          */~
            getpadpy_rec$(2%)256,         /* GETPADPY Record           */~
            arcash_rec$218,               /* ARCASH Record             */~
            format_date$8                 /* Formatted Date 8          */


        dim schema$8,                     /* SCHEMA                    */~
            volume$8                      /* VOLUME to create file     */
            
      
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! GETPADPY ! GetPaid Payment File                     *~
            * #20 ! ARPAYMNT ! Payment Transfer file                    *~ 
            *************************************************************~
            *              File Selection and Open Calls                *~
            *************************************************************

            select #1,  "GETPADPY",                                      ~
                        varc,     indexed,  recsize = 512,               ~
                        keypos =    7, keylen =   26,                    ~
                        alt key  1, keypos =    1, keylen =  32,         ~
                            key  2, keypos =    8, keylen =  25, dup

            select #3, "GENCODES",                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   1 , keylen =  24
                        


            select #20, "ARCASH",                                        ~
                        varc,       consec,          recsize = 218                                                  


            call "SETPRNT" ("AWDG", "AWDG", 0%, 0%)
            select printer (134)




            call "OPENCHCK" (#1, fs%(1%), f2%(1%),    0%, rslt$(1%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),    0%, rslt$(3%))


* (AWD002) Begin
            call "SHOSTAT" ("Find Volume ...")
            schema_err%, schema% = 0%
            init(" ") schema$, volume$
            call "SCHEMA" (schema$, schema%, #3, schema_err%)


            if schema% = 1% then volume$ = "CARLO2"
            if schema% = 2% then volume$ = "NE2"

            call "SHOSTAT" ("Opening EDI Files...")
            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 1000%, rslt$(1%))


            open nodisplay #20, output, space = 100%,                   ~
                dpack = 100%, ipack = 100%, file = "ARCASH",             ~
                library = "FTPGETPD", volume = volume$, blocks = 5%


            
        REM *************************************************************~
            *       M A I N  P R O C E S S  S E C T I O N                ~
            *************************************************************~

        call "SHOSTAT" ("Creating EDI CASH Transmit File...")

        gosub initialize_data        /* Initialize Data Variable */
        gosub build_payment_data     /* Build ARCASH             */


        goto  exit_program           /* Exit Program */

        initialize_data
            init (" ") getpadpy_key$, getpadpy_rec$(), format_date$, ~
                       arcash_rec$
                             

        return


        build_payment_data
                            /* Start File At First Invoice Send Record */
           init(" ") getpadpy_key$, getpadpy_rec$()
        
           read #1, hold, key > getpadpy_key$, using GETPADPY_FMT1,        ~
                                     getpadpy_rec$(), eod goto payment_finished
           
GETPADPY_FMT1:              FMT 2*CH(256)                                  
                   goto payment_first

        read_payment_next
                 read #1, hold, using GETPADPY_FMT1,        ~
                                     getpadpy_rec$(), eod goto payment_finished

payment_first:
           
                 getpadpy_key$ = str(getpadpy_rec$(),7,26)
                 if str(getpadpy_key$,1%,1%) <> "S" then goto payment_finished

                 gosub create_record

                 gosub write_record


                     delete #1

                 str(getpadpy_rec$(),7,1) = "T"
                 
                 write #1, using GETPADPY_FMT1, getpadpy_rec$()

                 
              
                   goto read_payment_next  /* Get Next GEDPADPY Record */

        payment_finished
        return
       



        create_record
                                                       /* Customer */
                 str(arcash_rec$,1,20)  = str(getpadpy_rec$(),8,9)  
		 p% = 0%
		 p% = pos(str(arcash_rec$,1%,20%) = " ")
		 if p% <> 0% then str(arcash_rec$,p%,4%) = ".036"
                                                        /* invoice  */
                 str(arcash_rec$,21,20) = str(getpadpy_rec$(),17,8)  
                                                        /* bank ind */
                 str(arcash_rec$,41,30) = str(getpadpy_rec$(),34,30)  
                                                       /* Batch No  */
                 str(arcash_rec$,71,20) = str(getpadpy_rec$(),64,20)  
                                                       /* Currency Cd*/
                 str(arcash_rec$,91,10) = str(getpadpy_rec$(),84,20)
                                                       /* Receip No */
                 str(arcash_rec$,101,20)= str(getpadpy_rec$(),94,20)
REM                 call "SHOSTAT"(" Amount 1 " ) stop  
                                                       /* Recipt Amt*/
                 amount = 0.00
                 get str(getpadpy_rec$(),114,8)                      ~
                             using GETPADPY_FMT2, amount
GETPADPY_FMT2:          FMT PD(14,4)

REM                 convert amount to str(arcash_rec$,121,20),            ~
                                             pic(################.##-)

                 convert amount to str(arcash_rec$,121,20),            ~
					    pic(-00000000.00) 
                                                       /* Deposit Date */
                 init(" ") format_date$
                 format_date$ = str(getpadpy_rec$(),122,6)
                 gosub format_date
                 str(arcash_rec$,141,8) = format_date$

                                                       /* Deposit Diff */
                 init(" ") format_date$
                 format_date$ = str(getpadpy_rec$(),128,6)
                 gosub format_date
                 str(arcash_rec$,149,10) = format_date$     /* date & time */
                 str(arcash_rec$,159,6) = str(getpadpy_rec$(),135,6)
                 str(arcash_rec$,165,4) = " " & str(getpadpy_rec$(),143,3)

                 
REM                 call "SHOSTAT"(" Amount 2 " ) stop
                                                       /* Application Amt */
                 amount = 0.00

                 get str(getpadpy_rec$(),148,8)                      ~
                             using GETPADPY_FMT2, amount	

REM                 convert amount to str(arcash_rec$,169,20),            ~
                                             pic(################.##-)   

                                                        /* Application Date */

		 convert amount to str(arcash_rec$,169,20),            ~
					    pic(-00000000.00) 

                 init(" ") format_date$ 
                 format_date$ = str(getpadpy_rec$(),156,6)
                 gosub format_date
                 str(arcash_rec$,189,8) = format_date$

                                                        /* Operation */
                 str(arcash_rec$,197,2) = str(getpadpy_rec$(),162,2)
                                                    
                                                        /* Reason Code */
                 str(arcash_rec$,199,10) = str(getpadpy_rec$(),164,10)
 
                                                        /* Jurnal Ident*/
                 str(arcash_rec$,209,10) = str(getpadpy_rec$(),174,10)



         return

        format_date
             call "DATEFMT" (format_date$)
         return


        write_record

           write #20, using ARCASH_FMT1, arcash_rec$

ARCASH_FMT1:   FMT CH(218)

         return

   
        exit_program
         
             close #20                               /* Close ARCASH File */
        end




