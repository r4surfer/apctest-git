        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN14                             *~
            *  Creation Date     - 08/25/03                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New Program to Build | delimited     *~
            *                      files to import into Oracle.         *~
            *                                                           *~
            *                      This program only creates the sales  *~
            *                      detail for (EWDSLSDT) or the         *~
            *                      AWDSLSDT file to import!!!!          *~
            *                                                           *~
            *                      This program only creates files      *~
            *                      need for Shane                       *~
            *                      AWDSLSDT - EWDSLSDT                  *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Spec. Comm (Screen 1) -                                  *~
            *                         PF(10) Create Delimited File.     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/25/03 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            * 12/20/18 ! CR1828 EWDSLSDT conversion for Dallas    ! DES *~
            *************************************************************

        dim                              /* (CATEGORY) - FILE          */~
            readkey$100,                 /* Readkey                    */~
            rec$256,                     /* Read Record                */~
            ewdslsdt$20,                 /* EWDSLSDT Key               */~
            fields$(500%)256,            /* Generic Fields             */~
            num_fields(500%)            /* Generic Fields             */ 
            
            
        dim                              /* (Program) - Variables      */~
            filename$8,                  /* Used By EWDOPEN            */~
            cnt$28,                      /* Screen Display             */~
            hdr$45, msg$(3%)79,          /* Askuser - Var's            */~
            date$8,                      /* REPORT TITLE               */~
            userid$3,                    /* Current User Id            */~
            readdate$10                  /* Read date                  */

        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */



        dim file_to_import$(20)1,        /* File to Import On Screen   */~
            file_selection%(20)          /* Which File Did User Select */

        dim comma$1,                     /* Comma for EWDAPPSN File    */~
            file$8,                      /* File Name                  */~
            library$8,                   /* Library Name = APCDATA     */~
            volume$6                     /* DISK VOLUME = CARLOS       */
            
       dim schema$8                     /* Schema                     */            



        REM *************************************************************

            mat f2% = con
            mat fs% = zer
            rslt$() = " "

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #8  ! EWDSLSDT ! APC SALES ANALYSIS DETAIL FILE           *~            
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            
            
            select #1,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
   
            select #8,  "EWDSLSDT",                                      ~
                        varc,     indexed,  recsize =  409,              ~
                        keypos =   30, keylen =   20,                    ~
                        alt key 1,keypos =  79, keylen = 45,             ~
                            key 2,keypos =  88, keylen = 36,             ~
                            key 3,keypos =  97, keylen = 27,             ~
                            key 4,keypos =   1, keylen = 49,             ~
                            key 5,keypos =  50, keylen = 29

            select #9,  "SALESLOG",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    7, keylen =  18,                     ~
                       alt key   1, keypos =    1, keylen =  24    
                                                    

            select #27, "AWDSLSDT",                                      ~
                        varc,     indexed, recsize = 700,                ~
                        keypos = 38,   keylen = 23                                             
REM                     keypos = 35,   keylen = 23                                             

REM            call "SHOSTAT" ("Initialization")

            filename$ = "GENCODES" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            
            filename$ = "EWDSLSDT" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error            
            filename$ = "SALESLOG" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error            
            
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date

            call "DATEFMT" (date$)
            mat num_fields     = zer
            
            
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #1, schema_err%)              

        gosub initialize_variables
        gosub files_analysis
        goto exit_program

REM   END OF AUTOMATION

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        files_analysis
            comma$ = "|"

            gosub create_sales

        return




        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") readkey$, fields$(), filename$, cnt$, hdr$, msg$(),~
                      date$, library$, volume$, readdate$

        return

        REM *************************************************************~
            *************************************************************

        create_sales
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDSLSDT"
             ff% = 27%  
             gosub open_file
      
             gosub read_sales
        return


        read_sales
            init(" ") readkey$, fields$()
            mat num_fields = zer
            rec% = 0%
            str(readkey$,1,1) = "O"
        sales_nxt
            init(" ") fields$()
            mat num_fields = zer      
            read #9, key > readkey$, using salesFmt, readkey$,   ~
                                          eod goto read_sales_done
salesFmt:      FMT POS(07), CH(18)

             cnt% = cnt% + 1%

             if str(readkey$,1,1) <> "O" then read_sales_done

             init(" ") ewdslsdt$
             str(ewdslsdt$,1,17) = str(readkey$,2,17)
             gosub loadInvoice
             gosub transFlag

             goto sales_nxt

        read_sales_done
        return

        transFlag
             read #9, hold key = readkey$, using transFmt, rec$,        ~
                          eod goto transFlagDone

transFmt:            FMT CH(256)

                    delete #9

                 str(rec$,1,6) = date
                 str(rec$,7,1) = "T"


                 put #9, using transFmt, rec$

                 write #9, eod goto transFlagDone

        transFlagDone
        return


        loadInvoice
            read #8, key > ewdslsdt$, using ewdslsdtFmt, ewdslsdt$,    ~
                                 eod goto loadInvoiceDone

ewdslsdtFmt:        FMT POS(30), CH(20)
            if str(ewdslsdt$,1,17) <> str(readkey$,2,17) then ~
                             goto loadInvoiceDone

             get #8, using L69110, fields$(1%), fields$(2%),  ~
                                   fields$(3%), fields$(4%),  ~
                                   fields$(5%), fields$(6%),  ~
                                   fields$(7%), fields$(8%),~
                                   fields$(9%), fields$(10%),~
                                   fields$(11%), fields$(12%),~
                                   fields$(13%), fields$(14%),~
                                   fields$(15%), fields$(16%),~
                                   fields$(17%), fields$(18%),~
                                   fields$(19%), fields$(20%),~
                                   fields$(21%), fields$(22%),~
                                   fields$(23%), fields$(24%),~
                                   fields$(25%), fields$(26%),~
                                   num_fields(27%), num_fields(28%),~
                                   num_fields(29%), num_fields(30%),~
                                   num_fields(31%), num_fields(32%),~
                                   fields$(33%), num_fields(34%),~
                                   num_fields(35%), num_fields(36%),~
                                   num_fields(37%), num_fields(38%),~
                                   num_fields(39%), num_fields(40%),~
                                   num_fields(41%), num_fields(42%),~
                                   fields$(43%), fields$(44%)           

REM      I do not know if field 5 will work b/c it is number
REM          FMT   CH(06), CH(02), CH(06), CH(04), CH(04), CH(04), 
L69110:      FMT   CH(06), CH(02), CH(09), CH(04), CH(04), CH(04), ~
                   CH(09), CH(08), CH(03), CH(06), CH(01), CH(02), ~
                   CH(09), CH(11), CH(09), CH(09), CH(04), CH(09), ~
                   CH(03), CH(11), CH(03), CH(16), CH(03), CH(03), ~
                   CH(16), CH(25), PD(14,4), PD(14,4), PD(14,4),   ~
                   PD(14,4), PD(14,4), PD(14,4), CH(09), PD(14,4), ~
                   PD(14,4), PD(14,4), PD(14,4), PD(14,4),         ~
                   PD(14,4), PD(14,4), PD(14,4), PD(14,4),         ~
                   CH(20), CH(74)   
                      
                      
            
            convert num_fields(27%) to fields$(27%), pic(-#######0.00##)
            convert num_fields(28%) to fields$(28%), pic(-#######0.00##)
            convert num_fields(29%) to fields$(29%), pic(-#######0.00##)
            convert num_fields(30%) to fields$(30%), pic(-#######0.00##)
            convert num_fields(31%) to fields$(31%), pic(-#######0.00##)
            convert num_fields(32%) to fields$(32%), pic(-#######0.00##)
            convert num_fields(34%) to fields$(34%), pic(-#######0.00##)
            convert num_fields(35%) to fields$(35%), pic(-#######0.00##)
            convert num_fields(36%) to fields$(36%), pic(-#######0.00##)
            convert num_fields(37%) to fields$(37%), pic(-#######0.00##)
            convert num_fields(38%) to fields$(38%), pic(-#######0.00##)
            convert num_fields(39%) to fields$(39%), pic(-#######0.00##)            
            convert num_fields(40%) to fields$(40%), pic(-#######0.00##)
            convert num_fields(41%) to fields$(41%), pic(-#######0.00##)
            convert num_fields(42%) to fields$(42%), pic(-#######0.00##)            
            
            call "DATFMTC" (fields$(1%), date%, fields$(1%))
            call "DATFMTC" (fields$(10%), date%, fields$(10%))
            

            gosub write_upload_sales

            goto loadInvoice
        loadInvoiceDone
        return




        write_upload_sales
            write #27, using L69400, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$, fields$(6%),  ~
                              comma$, fields$(7%), comma$,       ~
                              fields$(8%), comma$, fields$(9%),  ~
                              comma$, fields$(10%), comma$,      ~
                              fields$(11%), comma$, fields$(12%),~
                              comma$, fields$(13%), comma$,      ~
                              fields$(14%), comma$, fields$(15%),~
                              comma$, fields$(16%), comma$,      ~
                              fields$(17%), comma$, fields$(18%),~
                              comma$, fields$(19%), comma$,      ~
                              fields$(20%), comma$, fields$(21%),~
                              comma$, fields$(22%), comma$,      ~ 
                              fields$(23%), comma$, fields$(24%),~
                              comma$, fields$(25%), comma$,      ~
                              fields$(26%), comma$, fields$(27%),~
                              comma$, fields$(28%), comma$,      ~
                              fields$(29%), comma$, fields$(30%),~
                              comma$, fields$(31%), comma$,      ~
                              fields$(32%), comma$, fields$(33%),~
                              comma$, fields$(34%), comma$,      ~
                              fields$(35%), comma$, fields$(36%),~
                              comma$, fields$(37%), comma$,      ~
                              fields$(38%), comma$, fields$(39%),~
                              comma$, fields$(40%), comma$,      ~
                              fields$(41%), comma$, fields$(42%),~
                              comma$, fields$(43%), comma$,      ~
                              fields$(44%), comma$

REM                   FMT CH(08), CH(01), CH(02), CH(01), CH(06), CH(01), 
L69400:               FMT CH(08), CH(01), CH(02), CH(01), CH(09), CH(01), ~
                          CH(04), CH(01), CH(04), CH(01), CH(04), CH(01), ~
                          CH(09), CH(01), CH(08), CH(01), CH(03), CH(01), ~
                          CH(08), CH(01), CH(01), CH(01), CH(02), CH(01), ~
                          CH(09), CH(01), CH(11), CH(01), CH(09), CH(01), ~
                          CH(09), CH(01), CH(04), CH(01), CH(09), CH(01), ~
                          CH(03), CH(01), CH(11), CH(01), CH(03), CH(01), ~
                          CH(16), CH(01), CH(03), CH(01), CH(03), CH(01), ~
                          CH(16), CH(01), CH(25), CH(01), CH(14), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(09), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                          CH(94), CH(01), CH(20), CH(01)
        return


        open_error
           comp% = 2%
           hdr$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return



        open_file
            init(" ") library$, volume$
            library$        = "ORAFILES"
REM            volume$         = "CARLO2"

            if schema% = 1% then volume$ = "CARLO2"
            if schema% = 2% then volume$ = "NE2"   

             open nodisplay #ff%, output, space = 100%,                  ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return
       



        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
REM            call "SHOSTAT" ("One Moment Please")

            end
