        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN31                             *~
            *  Creation Date     - 06/02/06                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New Program to Build | delimited     *~
            *                      files to import into Oracle.         *~
            *                                                           *~
            *                      This program only creates the plan   *~
            *                      header or APCPLNOR file to import!!  *~
            *                                                           *~
            *                      This program only creates files      *~
            *                      need for WW import                   *~
            *                      AWDVENOR - APCVENOR                  *~
            *                      AWDVENBF - VENDORBF                  *~
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
            * 06/02/06 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            *************************************************************

        dim                              /* (vendor  ) - FILE          */~
            readkey$100,                 /* Readkey                    */~
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


        dim comma$1,                     /* Comma for EWDAPPSN File    */~
            file$8,                      /* File Name                  */~
            library$8,                   /* Library Name = APCDATA     */~
            volume$6                     /* DISK VOLUME = CARLOS       */


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
            * # 1 ! VENDOR   ! Vendor Master File                       *~
            * # 2 ! VENDORBF ! Vendor Buy From file                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select # 1, "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos = 1, keylen =  9,                         ~
                        alt key  1, keypos = 10, keylen = 30, dup

            select # 2, "VENDORBF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 15


            select #4, "GENCODES",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #21, "AWDVENDR",                                      ~
                        varc,     indexed, recsize = 700,                ~
                        keypos = 1,    keylen = 10


            select #22, "AWDVENBF",                                      ~
                        varc,     indexed, recsize = 600,                ~
                        keypos = 1,    keylen = 17

REM            call "SHOSTAT" ("Initialization")

            filename$ = "VENDOR"   : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "VENDORBF" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error



            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date

                                                          /* (AWD004)  */

            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #4, schema_err%)


            call "DATEFMT" (date$)
            mat num_fields     = zer

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

            gosub create_vendor   
            gosub create_vendorbf 

        return





        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") readkey$, fields$(), filename$, cnt$, hdr$, msg$(),~
                      date$,  library$, volume$, readdate$

        return

        REM *************************************************************~
            *************************************************************


        create_vendor   
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDVENDR"
             ff% = 21%  
             gosub open_file
      
             gosub read_vendor  
                   goto L61030
        create_vendor_nxt 
             gosub read_vendor_nxt
             if rec% <> 1% then goto vendor_done
L61030:

               gosub write_upload_vendor
               goto create_vendor_nxt
        return
        vendor_done
        return

        read_vendor
            init(" ") fields$(), readkey$
            
            mat num_fields = zer
            rec% = 0%
            read #1, key > readkey$, eod goto read_vendor_done
                goto L61300
        read_vendor_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #1, eod goto read_vendor_done

L61300:         cnt% = cnt% + 1%
            goto L63155
            if mod(cnt%,50%) <> 0% then goto L63155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L63155:
                get #1, using L61140, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), fields$(6%),  ~
                                      fields$(7%), fields$(8%),  ~
                                      fields$(9%), fields$(10%), ~
                                      fields$(11%), fields$(12%),~
                                      fields$(13%), fields$(14%),~
                                      fields$(15%), fields$(16%),~
                                      num_fields(17%), num_fields(18%),~   
                                      fields$(19%), fields$(20%),~
                                      fields$(21%), fields$(22%),~
                                      fields$(23%), fields$(24%),~
                                      fields$(25%), fields$(26%),~
                                      fields$(27%), fields$(28%),~
                                      fields$(29%), fields$(30%),~
                                      fields$(31%), fields$(32%),~
                                      fields$(33%), fields$(34%),~
                                      fields$(35%)


L61140:           FMT CH(09), CH(30), CH(30), CH(30), CH(30), CH(30), ~
                      CH(30), CH(30), CH(20), CH(10), CH(09), CH(09), ~
                      CH(09), CH(09), CH(08), CH(08), PD(15,4),       ~
                      PD(15,4), CH(09), CH(87), CH(01), CH(01),       ~
                      CH(30), CH(01), CH(30), CH(04), CH(09), CH(04), ~ 
                      CH(09), CH(12), CH(04), CH(09), CH(04), CH(07),~
                      CH(62)



REM         call "DATFMTC" (fields$(15%), date%, fields$(15%))
REM         call "DATFMTC" (fields$(16%), date%, fields$(16%))


            convert num_fields(15%) to fields$(15%), pic(-#######0.0000)
            convert num_fields(16%) to fields$(16%), pic(-#######0.0000)

            convert num_fields(17%) to fields$(17%), pic(-#######0.0000)
            convert num_fields(18%) to fields$(18%), pic(-#######0.0000)


            convert fields$(28%) to num_fields%(28%), data goto L61170

L61170:     convert num_fields%(28%) to fields$(28%), pic(##########)

            rec% = 1%            
        read_vendor_done
        return


        write_upload_vendor
            write #21, using L65400, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$, fields$(6%),  ~
                              comma$, fields$(7%), comma$,       ~
                              fields$(8%), comma$, fields$(9%),  ~
                              comma$, fields$(10%), comma$,      ~
                              fields$(11%), comma$, fields$(12%),~
                              comma$, fields$(13%), comma$,      ~
                              fields$(14%), comma$,              ~
                                                                 ~
                                                                 ~
                              fields$(15%),                      ~
                              comma$,                            ~
                                                                 ~
                              fields$(16%),                      ~
                              comma$,                            ~
                                                                 ~
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
                              fields$(35%), comma$

L65400:               FMT CH(09), CH(01), CH(30), CH(01), CH(30),     ~
                      CH(01), CH(30), CH(01), CH(30), CH(01),         ~
                      CH(30), CH(01), CH(30), CH(01), CH(30), CH(01), ~
                      CH(20), CH(01), CH(10), CH(01), CH(09), CH(01), ~
                      CH(09), CH(01), CH(09), CH(01), CH(09), CH(01), ~
                                                                      ~
                      CH(14), CH(01), ~
                      CH(14), CH(01), ~
                                                                      ~
                      CH(14), CH(01), CH(14), CH(01), CH(09), CH(01), ~
                      CH(87), CH(01), CH(01), CH(01), CH(01), CH(01), ~
                      CH(30), CH(01), CH(01), CH(01), CH(30), CH(01), ~
                      CH(04), CH(01), CH(09), CH(01), CH(04), CH(01), ~
                      CH(09), CH(01), CH(12), CH(01), CH(04), CH(01), ~
                      CH(09), CH(01), CH(04), CH(01), CH(07), CH(01), ~
                      CH(62), CH(01)

        return



        create_vendorbf 
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDVENBF"
             ff% = 22%  
             gosub open_file
      
             gosub read_vendorbf
                   goto L62030
        create_vendorbf_nxt 
             gosub read_vendorbf_nxt
             if rec% <> 1% then goto vendorbf_done
L62030:

               gosub write_upload_vendorbf
               goto create_vendorbf_nxt
        return
        vendorbf_done
        return

        read_vendorbf
            init(" ") fields$(), readkey$
            
            mat num_fields = zer
            rec% = 0%
            read #2, key > readkey$, eod goto read_vendorbf_done
                goto L61400
        read_vendorbf_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #2, eod goto read_vendorbf_done

L61400:         cnt% = cnt% + 1%
            goto L64155
            if mod(cnt%,50%) <> 0% then goto L64155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L64155:
                get #2, using L61440, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), fields$(6%),  ~
                                      fields$(7%), fields$(8%),  ~
                                      fields$(9%), fields$(10%), ~
                                      fields$(11%), fields$(12%),~
                                      fields$(13%), fields$(14%),~
                                      fields$(15%), fields$(16%),~
                                      fields$(17%), fields$(18%),~   
                                      fields$(19%), fields$(20%),~
                                      fields$(21%)


L61440:           FMT CH(09), CH(06), CH(30), CH(30), CH(30), CH(30), ~
                      CH(30), CH(30), CH(30), CH(20), CH(10), CH(01), ~
                      CH(01), CH(30), CH(30), CH(01), CH(09), CH(09), ~
                      CH(04), CH(10), CH(150)


            convert fields$(19%) to num_fields%(19%), data goto L61180

L61180:     convert num_fields%(19%) to fields$(19%), pic(##########)


            rec% = 1%            
        read_vendorbf_done
        return


        write_upload_vendorbf
            write #22, using L65500, fields$(1%), comma$,        ~
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
                              comma$

L65500:               FMT CH(09), CH(01), CH(06), CH(01), CH(30),     ~
                      CH(01), CH(30), CH(01), CH(30), CH(01), CH(30), ~
                      CH(01), CH(30), CH(01), CH(30), CH(01), CH(30), ~
                      CH(01), CH(20), CH(01), CH(10), CH(01), CH(01), ~
                      CH(01), CH(01), CH(01), CH(30), CH(01), CH(30), ~
                      CH(01), CH(01), CH(01), CH(09), CH(01), CH(09), ~
                      CH(01), CH(10), CH(01), CH(10), CH(01), CH(150),~
                      CH(01)

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
            if schema% = 1% then volume$         = "CARLO2"
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
