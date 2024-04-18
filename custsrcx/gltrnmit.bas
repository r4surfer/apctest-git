        REM *-----------------------------------------------------------*~
            *                                                           *~
            *  GGGG   L     TTTTT  RRRR  N   N M    M  IIIIII  TTTTTT   *~
            * G       L       T    R   R NN  N MM  MM     I       T     *~
            * G  GGG  L       T    RRRR  N N N M MM M     I       T     *~
            * G    G  L       T    R R   N  NN M    M     I       T     *~
            *  GGGG   LLLLL   T    R  R  N   N M    M  IIIIII     T     *~
            *-----------------------------------------------------------*~
            * GLTRNMIT - Program to transmit file to Corporte for GL    *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/02/06 ! Original                                 ! CMG *~
            *-----------------------------------------------------------*


        dim glortran_key$33,             /* read key                   */~
            glortran_rec$(2%)256,        /* glortan record             */~
            gldata_rec$(2%)130,          /* gldata  record             */~
            wrkdate$10,                  /* work date                  */~
            date$,                       /* todays date                */~
            transmit$1,                  /* Transmit flag              */~
            module_id$2,                 /* Module ID                  */~
            journal_id$3,                /* Journal ID                 */~
            db$14,                       /* Debit amount               */~ 
            cr$14,                       /* Credit amount              */~
            gl_seq$12,                   /* GL Sequence                */~
            post_seq$12,                 /* Posting Sequence           */~
            gl_ref1$25,                  /* GL Reference 1             */~
            delimiter$1,                 /* File Delimiter             */~
            journal_category$20,         /* GL Category name           */~
            account$16                   /* GL account number          */
            

        dim gl_date$6,                   /* GL Test date               */~
            run_date$10,                 /* Run Date                   */~
            end_date$10,                 /* End Date                   */~
            trns_date$6                  /* Transmit Date              */


        dim file$8,                      /* IPNet Print File           */~
            library$8,                   /* Library Name = APCDATA     */~
            volume$6                     /* DISK VOLUME = CARLO2       */


            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! GLORTRAN ! GL Oracle Transmit File                  *~
            * #4  ! GENCODES ! GENCODES Table File                      *~
            * #5  ! GLDATA   ! GL data to send (actual file sent)       *~
            *************************************************************~
            *              File Selection and Open Calls                *~
            *************************************************************

            select #1,  "GLORTRAN",                                       ~
                        varc,     indexed, recsize = 512,                 ~
                        keypos = 1,    keylen = 33,                       ~
                        alt key 1, keypos = 31, keylen = 47,              ~
                            key 2, keypos = 81, keylen = 26

            select #4, "GENCODES",                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   1 , keylen =  24

            select #5, "GLDATA  ",                                       ~
                        varc,     consec,   recsize = 260



            call "SHOSTAT" ("Opening EDI Files...")

            filename$ = "GLORTRAN" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error


            init(" ") file$, library$, volume$
            file$     = "GLDATA  "
            library$  = "FTPCORP"
            volume$   = "CARLO2"
            gosub open_file

        REM *************************************************************~
            *       M A I N  P R O C E S S  S E C T I O N                ~
            *************************************************************~

        call "SHOSTAT" ("Creating EDI Transmit File...")
        gosub initialize_data               /* Initialize Data Variable */
        gosub build_gldata       /* Write GLDATA  Recs */                 

        goto  exit_program                              /* Exit Program */

        initialize_data
            init (" ") glortran_key$, glortran_rec$(), gldata_rec$(),   ~
                       wrkdate$, date$, transmit$, module_id$,          ~
                       journal_id$, journal_category$


            gl_date$, run_date$, end_date$, trns_date$ = all(hex(00))

            date$ = date

REM            date$ = "04/23/2007"
REM            call "DATUFMTC" (date$)

            call "DATE" addr("G+",date$, -1%,run_date$,err%)
            call "DATE" addr("G+",date$, -1%,end_date$,err%)



REM            call "DATEFMT" (date$)


            delimiter$ = "|"

REM         run_date$ = "20061221"
REM            end_date$ = "20061208"
            call "DATFMTC" (run_date$)
            call "DATUFMTC" (run_date$)

REM            call "DATFMTC" (end_date$)
REM            call "DATUFMTC" (end_date$)

        return



        build_gldata
                            /* Start File At First GL Record           */
           str(glortran_key$,1%,1%) = "S" 

        next_gldata
           read #1,hold,key > glortran_key$,using  GLORTRAN_FMT,         ~
                                glortran_rec$(), eod goto glortran_done

GLORTRAN_FMT:           FMT   2*CH(256)


           str(glortran_key$,1%,33%) = str(glortran_rec$(),1%,33%)
           transmit$ = str(glortran_rec$(),1%,1%)

           if transmit$ <> "S" then return

           if str(glortran_rec$(),18%,2%) <> "02" then goto next_gldata	

REM        if str(glortran_rec$(),18%,2%) = "01" then goto next_gldata
REM        if str(glortran_rec$(),18%,2%) = "99" then goto next_gldata


           account$    = str(glortran_rec$(),2%,16%)
           gl_date$ = str(glortran_rec$(),97%,6%)
           trns_date$ = str(glortran_rec$(),223%,6%)

REM        if str(trns_date$,1%,6%) <> str(run_date$,1%,6%) then ~
                                          goto next_gldata
 
 
REM           if str(gl_date$,1%,6%) < str(run_date$,1%,6%)        ~
                  or str(gl_date$,1%,6%) > str(end_date$,1%,6%) ~
                                             then goto next_gldata

           if str(gl_date$,1%,6%) > str(run_date$,1%,6%) ~
                                             then goto next_gldata


REM           if trns_date$ <> str(date$,1%,6%) ~
                                             then goto next_gldata

           module_id$  = str(glortran_rec$(),18%,2%)
           journal_id$ = str(glortran_rec$(),20%,3%)



REM Do not include manual entries


           if journal_id$ = "GAJ" then goto next_gldata
           if journal_id$ = "GNJ" then goto next_gldata

           if str(account$,1%,3%) = "999" then goto next_gldata

           init(" ") journal_category$

           gl_ref1$    = str(glortran_rec$(),123%,25%)

                                            /* Accounts Receivable */
           if module_id$ = "01" then gosub accounts_receivable
                                            /* Accounts Payable    */
           if module_id$ = "02" then gosub accounts_payable
                                            /* Inventory control   */
           if module_id$ = "04" then gosub inventory       
                                            /* Purchasing          */
           if module_id$ = "05" then gosub purchasing       
                                            /* General Ledger      */
           if module_id$ = "06" then gosub general_ledger  

           if module_id$ = "99" then gosub year_end 


           gosub reset_transmit_flag


                  write #5, using GLDATA_FMT, gldata_rec$()

GLDATA_FMT:             FMT 2*CH(130)

              goto next_gldata            /* Get Next GLORTRAN Record */

        glortran_done
        return



       
                                           

        reset_transmit_flag
              delete #1
              str(glortran_rec$(),1%,1%)  = "T"
              str(glortran_rec$(),34%,1%) = "T"
              str(glortran_rec$(),223%,6%) = date
              str(glortran_rec$(),229%,4%) = time
 
                                          /* Reset Transmit Flag To 'T' */
              put #1, using GLORTRAN_FMT, glortran_rec$()

              write #1
        return


                                     
        open_file
            open nodisplay #5, output, space = 100%,                     ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return

        accounts_receivable
              journal_category$ = "Legacy Receivables"
              gosub create_gldata_rec
              p% = 0%
              p% = pos(gl_ref1$ = " ")

              if p% = 0% then return

              pos% = 222%
 
                                                 /* Invoice */
              str(gldata_rec$(),pos%,15%) = str(gl_ref1$,p%+1%,15%)
              pos% = pos% + 15%
              str(gldata_rec$(),pos%,1%)  = delimiter$
              pos% = pos% + 1%
                                                 /* Customer */
              str(gldata_rec$(),pos%,p%) = str(gl_ref1$,1%,p%)
              pos% = pos% + p%
              str(gldata_rec$(),pos%,1%)  = delimiter$
              pos% = pos% + 1%

                                                /* Transaction Type */
              str(gldata_rec$(),pos%,1%) = " "
              pos% = pos% + 1%
              str(gldata_rec$(),pos%,1%)  = delimiter$
              pos% = pos% + 1%

                                                /* Supplier */
              str(gldata_rec$(),pos%,1%) = " "
              pos% = pos% + 1%
              str(gldata_rec$(),pos%,1%)  = delimiter$
              pos% = pos% + 1%

                                                /* Payable Invoice */
              str(gldata_rec$(),pos%,1%) = " "
              pos% = pos% + 1%
              str(gldata_rec$(),pos%,1%)  = delimiter$
              pos% = pos% + 1%




              
        return
 

        accounts_payable
              journal_category$ = "Legacy Payables   "
              gosub create_gldata_rec

              p% = 0%
              p% = pos(gl_ref1$ = " ")

              if p% = 0% then return

              pos% = 222%
 
                                                 /* Invoice */
              str(gldata_rec$(),pos%,1%) = " "
              pos% = pos% + 1%
              str(gldata_rec$(),pos%,1%)  = delimiter$
              pos% = pos% + 1%

                                                 /* Customer */
              str(gldata_rec$(),pos%,1%) = " "
              pos% = pos% + 1%
              str(gldata_rec$(),pos%,1%)  = delimiter$
              pos% = pos% + 1%

                                                /* Transaction Type */
              str(gldata_rec$(),pos%,1%) = " "
              pos% = pos% + 1%
              str(gldata_rec$(),pos%,1%)  = delimiter$
              pos% = pos% + 1%

                                                /* Supplier */
              str(gldata_rec$(),pos%,15%) = str(gl_ref1$,p%+1%,15%)
              pos% = pos% + 15%
              str(gldata_rec$(),pos%,1%)  = delimiter$
              pos% = pos% + 1%

                                                /* Payable Invoice */
              str(gldata_rec$(),pos%,p%) = str(gl_ref1$,1%,p%)
              pos% = pos% + p%
              str(gldata_rec$(),pos%,1%)  = delimiter$
              pos% = pos% + 1%



        return  


        inventory
              journal_category$ = "Legacy Inventory  "
              gosub create_gldata_rec
                                                 /* BLANK */
              str(gldata_rec$(),222%,1%) = " "
              str(gldata_rec$(),223%,1%)  = delimiter$
                                                 /* BLANK */
              str(gldata_rec$(),224%,1%) = " "
              str(gldata_rec$(),225%,1%)  = delimiter$
                                                 /* BLANK */
              str(gldata_rec$(),226%,1%) = " "
              str(gldata_rec$(),227%,1%)  = delimiter$
                                                 /* BLANK */
              str(gldata_rec$(),228%,1%) = " "
              str(gldata_rec$(),229%,1%)  = delimiter$
                                                 /* BLANK */
              str(gldata_rec$(),230%,1%) = " "
              str(gldata_rec$(),231%,1%)  = delimiter$

        return

        purchasing
              journal_category$ = "Legacy Payables   "
              gosub create_gldata_rec

                                                 /* BLANK */
              str(gldata_rec$(),222%,1%) = " "
              str(gldata_rec$(),223%,1%)  = delimiter$
                                                 /* BLANK */
              str(gldata_rec$(),224%,1%) = " "
              str(gldata_rec$(),225%,1%)  = delimiter$
                                                 /* BLANK */
              str(gldata_rec$(),226%,1%) = " "
              str(gldata_rec$(),227%,1%)  = delimiter$
                                                 /* BLANK */
              str(gldata_rec$(),228%,1%) = " "
              str(gldata_rec$(),229%,1%)  = delimiter$
                                                 /* BLANK */
              str(gldata_rec$(),230%,1%) = " "
              str(gldata_rec$(),231%,1%)  = delimiter$

        return


        general_ledger
              journal_category$ = "GL Entries        "
              gosub create_gldata_rec
                                                 /* BLANK */
              str(gldata_rec$(),222%,1%) = " "
              str(gldata_rec$(),223%,1%)  = delimiter$
                                                 /* BLANK */
              str(gldata_rec$(),224%,1%) = " "
              str(gldata_rec$(),225%,1%)  = delimiter$
                                                 /* BLANK */
              str(gldata_rec$(),226%,1%) = " "
              str(gldata_rec$(),227%,1%)  = delimiter$
                                                 /* BLANK */
              str(gldata_rec$(),228%,1%) = " "
              str(gldata_rec$(),229%,1%)  = delimiter$
                                                 /* BLANK */
              str(gldata_rec$(),230%,1%) = " "
              str(gldata_rec$(),231%,1%)  = delimiter$

        return

        year_end
              journal_category$ = "Year End Entries  "
              gosub create_gldata_rec
                                                 /* BLANK */
              str(gldata_rec$(),222%,1%) = " "
              str(gldata_rec$(),223%,1%)  = delimiter$
                                                 /* BLANK */
              str(gldata_rec$(),224%,1%) = " "
              str(gldata_rec$(),225%,1%)  = delimiter$
                                                 /* BLANK */
              str(gldata_rec$(),226%,1%) = " "
              str(gldata_rec$(),227%,1%)  = delimiter$
                                                 /* BLANK */
              str(gldata_rec$(),228%,1%) = " "
              str(gldata_rec$(),229%,1%)  = delimiter$
                                                 /* BLANK */
              str(gldata_rec$(),230%,1%) = " "
              str(gldata_rec$(),231%,1%)  = delimiter$

        return

        create_gldata_rec
          init(" ") gldata_rec$()
                                                 /* Division */
          str(gldata_rec$(),1%,3%) = str(glortran_rec$(),31%,3%)
          str(gldata_rec$(),4%,1%) = delimiter$
                                                 /* Module ID */
          str(gldata_rec$(),5%,20%) = journal_category$
REM             str(gldata_rec$(),5%,20%) = module_id$
          str(gldata_rec$(),25%,1%) = delimiter$
                                                 /* "CAELUS"  */
          str(gldata_rec$(),26%,6%) = "CAELUS"
          str(gldata_rec$(),32%,1%) = delimiter$
                                                 /* GL ACCOUNT NO*/
          str(gldata_rec$(),33%,16%) = str(glortran_rec$(),2%,16%)
          str(gldata_rec$(),49%,1%) = delimiter$
                                                 /* Division */
          str(gldata_rec$(),50%,3%) = str(glortran_rec$(),31%,3%)
          str(gldata_rec$(),53%,1%) = delimiter$
                                                 /* Blank    */
          str(gldata_rec$(),54%,1%) = " "
          str(gldata_rec$(),55%,1%) = delimiter$
                                                 /* Blank    */
          str(gldata_rec$(),56%,1%) = " "
          str(gldata_rec$(),57%,1%) = delimiter$
 
          gosub unpack_amount
                                                 /* Debit   */
          str(gldata_rec$(),58%,14%) = db$
          str(gldata_rec$(),72%,1%) = delimiter$
                                                 /* Credit   */
          str(gldata_rec$(),73%,14%) = cr$
          str(gldata_rec$(),87%,1%) = delimiter$
                                                 /* GL module date*/
          gosub unpack_date
          str(gldata_rec$(),88%,10%) = wrkdate$
          str(gldata_rec$(),98%,1%)  = delimiter$
          gosub unpack_bi
                                                 /* Posting Sequence Number */
                                                 /* Batch Number            */
          str(gldata_rec$(),99%,12%) = post_seq$ 
          str(gldata_rec$(),111%,1%) = "-" 
          str(gldata_rec$(),112%,12%) = gl_seq$ 
          str(gldata_rec$(),124%,1%)  = delimiter$
                                                 /* Batch Description */
          str(gldata_rec$(),125%,4%) = str(glortran_rec$(),20%,3%) & " "
          str(gldata_rec$(),129%,32%) = str(glortran_rec$(),177%,32%)
          str(gldata_rec$(),161%,1%)  = delimiter$
                                                 /* Journal Entry Name*/
          str(gldata_rec$(),162%,25%) = str(glortran_rec$(),123%,25%)
          str(gldata_rec$(),187%,1%)  = delimiter$
                                                 /* Journal Entry Descr*/
          str(gldata_rec$(),188%,25%) = str(glortran_rec$(),148%,25%)
          str(gldata_rec$(),213%,1%)  = delimiter$
                                                 /* Month*/
          str(gldata_rec$(),214%,2%) = str(wrkdate$,1%,2%)
          str(gldata_rec$(),216%,1%)  = delimiter$
                                                 /* Year*/
          str(gldata_rec$(),217%,4%) = str(wrkdate$,7%,4%)
          str(gldata_rec$(),221%,1%)  = delimiter$


        return

        unpack_date
           init(" ") wrkdate$
           wrkdate$ = str(glortran_rec$(),97%,6%)
           call "DATFMTC" (wrkdate$)

        return

        unpack_amount
            init(" ") db$, cr$
            db, cr = 0.00


            get str(glortran_rec$(),107%,8%) using PD_FMT, db
PD_FMT:            FMT PD(14,4)

            get str(glortran_rec$(),115%,8%) using PD_FMT, cr

            convert db to db$, pic(###########.##)

            convert cr to cr$, pic(###########.##)


        return


        unpack_bi

REM            call "SHOSTAT" ("UNPACK BI" )  stop
            init(" ") post_seq$
            post_seq% = 0%


           post_seq%, gl_seq% = 0%

           get str(glortran_rec$(),23%,4%) using BI_FMT, gl_seq%
BI_FMT:            FMT BI(04)

           get str(glortran_rec$(),27%,4%) using BI_FMT, post_seq%


           convert gl_seq% to gl_seq$,     pic(############)
            
           convert post_seq% to post_seq$, pic(############)

             

        return


        open_error
               goto exit_program



        exit_program

         end


