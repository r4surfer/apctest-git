        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN10                             *~
            *  Creation Date     - 08/25/03                             *~
            *  Last Modified Date- 10/26/2012                           *~
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
            *                      AWDPLNOR - APCPLNOR                  *~
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
            *10/26/2012! (AWD001) mod for carrier code ensenda    ! CMG *~
            *03/24/2015! (AWD002) mod for appian arrival date     ! PWW *~
			*11/02/2022! (CR3085) Add prefix sales order to file  ! RDB *~
            *************************************************************

        dim                              /*                            */~
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

        dim appor_so$08,                 /* (AWD001) AWDAPPOR readkey  */~
            carrier$02,                  /* (AWD001) Carrier Code      */~
            arr_dte$6                    /* (AWD002) Arrival date      */
            
            
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
            * #3  ! APCPLNOR ! (NEW) S.O. Header Histroy                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************



            select #3,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos = 1,    keylen = 51,                      ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup


            select #4, "GENCODES",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

/* (AWD001) */                                                
            select #5,  "AWDAPPOR",                                      ~
                        varc,     indexed,  recsize =  384,              ~
                        keypos =   1, keylen =   8,                      ~
                        alt key  1, keypos =    9, keylen =  40,         ~
                            key  2, keypos =   15, keylen =  34,         ~
                            key  3, keypos =   17, keylen =  32
                        

            select #22, "AWDPLNOR",                                      ~
                        varc,     indexed, recsize = 500,                ~
                        keypos = 1,    keylen = 70

REM            call "SHOSTAT" ("Initialization")

            filename$ = "APCPLNOR" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDAPPOR" : call "EWDOPEN" (#5, filename$, err%)
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

            gosub create_planning

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


        create_planning
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDPLNOR"
             ff% = 22%  
             gosub open_file
      
             gosub read_plan    
                   goto L61030
        create_plan_nxt
             gosub read_plan_nxt
             if rec% <> 1% then goto plan_done
L61030:
               init(" ") appor_so$, carrier$, arr_dte$
               appor_so$ = fields$(8%)
               
               gosub lookup_carrier
               gosub write_upload_plan
               goto create_plan_nxt
        return
        plan_done
        return

        read_plan
            init(" ") fields$()
            readdate$ = "20030101"
            call "DATFMTC" (readdate$)
            call "DATUFMTC" (readdate$)
            readkey$ = all(hex(00))
            str(readkey$,1%,6%) = readdate$
            
            mat num_fields = zer
            rec% = 0%
            read #3, key > readkey$, eod goto read_plan_done
                goto L61300
        read_plan_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #3, eod goto read_plan_done

L61300:         cnt% = cnt% + 1%
            goto L63155    
            if mod(cnt%,1000%) <> 0% then goto L63155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L63155:
                get #3, using L61140, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), fields$(6%),  ~
                                      fields$(7%), fields$(8%),  ~
                                      fields$(9%), fields$(10%), ~
                                      fields$(11%), fields$(12%),~
                                      fields$(13%), fields$(14%),~
                                      fields$(15%), fields$(16%),~
                                      num_fields(17%), num_fields(18%),~
                                      num_fields(19%), num_fields(20%),~
                                      num_fields(21%), fields$(22%),~
                                      fields$(23%), fields$(24%),~
                                      fields$(25%), num_fields(26%),~
                                      num_fields(27%), fields$(28%),~
                                      fields$(29%)

REM      I do not know if field 5 will work b/c it is number
L61140:           FMT CH(08), CH(02), CH(05), CH(09), CH(02), CH(09), ~
                      CH(16), CH(08), CH(02), CH(08), CH(08), CH(08), ~
                      CH(04), CH(02), CH(02), CH(05), PD(14,4),       ~
                      PD(14,4), PD(14,4), BI(2), BI(2), CH(08),       ~
                      CH(08), CH(03), CH(08), BI(04), BI(2), CH(10),  ~ 
                      CH(01)


            call "DATFMTC" (fields$(1%), date%, fields$(1%))
            call "DATFMTC" (fields$(10%), date%, fields$(10%))
            call "DATFMTC" (fields$(22%), date%, fields$(22%))
            call "DATFMTC" (fields$(23%), date%, fields$(23%))
            call "DATFMTC" (fields$(25%), date%, fields$(25%))

            convert num_fields(17%) to fields$(17%), pic(-#######0.0000)
            convert num_fields(18%) to fields$(18%), pic(-#######0.0000)
            convert num_fields(19%) to fields$(19%), pic(-#######0.0000)

            convert num_fields(20%) to fields$(20%), pic(########00000)
            convert num_fields(21%) to fields$(21%), pic(########00000)
            convert num_fields(26%) to fields$(26%), pic(########00000)
            convert num_fields(27%) to fields$(27%), pic(########00000)

            rec% = 1%            
        read_plan_done
        return


        write_upload_plan
            write #22, using L65400, str(fields$(1%),1%,4%), "-", ~
                              str(fields$(1%),5%,2%), "-",        ~
                              str(fields$(1%),7%,2%), comma$,     ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$, fields$(6%),  ~
                              comma$, fields$(7%), comma$,       ~
                              fields$(8%), comma$, fields$(9%),  ~
                              comma$, str(fields$(10%),1%,4%),   ~
                              "-", str(fields$(10%),5%,2%), "-", ~
                              str(fields$(10%),7%,2%),  comma$,  ~
                              fields$(11%), comma$, fields$(12%),~
                              comma$, fields$(13%), comma$,      ~
                              fields$(14%), comma$, fields$(15%),~
                              comma$, fields$(16%), comma$,      ~
                              fields$(17%), comma$, fields$(18%),~
                              comma$, fields$(19%), comma$,      ~
                              fields$(20%), comma$, fields$(21%),~
                              comma$, str(fields$(22%),1%,4%),   ~
                              "-", str(fields$(22%),5%,2%), "-", ~
                              str(fields$(22%),7%,2%),  comma$,  ~ 
                              str(fields$(23%),1%,4%), "-",      ~
                              str(fields$(23%),5%,2%), "-",      ~
                              str(fields$(23%),7%,2%),  comma$,  ~
                              fields$(24%), comma$,              ~
                              str(fields$(25%),1%,4%), "-",      ~
                              str(fields$(25%),5%,2%), "-",      ~
                              str(fields$(25%),7%,2%), comma$,   ~
                              fields$(26%), comma$, fields$(27%),~
                              comma$, fields$(28%), comma$,      ~
                              carrier$, comma$, fields$(30%),    ~
							  comma$, fields$(29%), comma$ 

/* (AWD001) replace filler with carrier and delimiter */                                                            
REM                              FIELDS$(29%), COMMA$

L65400:               FMT CH(04), CH(01), CH(02), CH(01), CH(02),     ~
                      CH(01), CH(02), CH(01), CH(05), CH(01),         ~
                      CH(09), CH(01), CH(02), CH(01), CH(06), CH(01), ~
                      CH(16), CH(01), CH(08), CH(01), CH(02), CH(01), ~
                      CH(04), CH(01), CH(02), CH(01), CH(02), CH(01), ~
                      CH(08), CH(01), CH(08), CH(01), CH(04), CH(01), ~
                      CH(02), CH(01), CH(02), CH(01), CH(05), CH(01), ~
                      CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                      CH(14), CH(01), CH(14), CH(01), CH(04), CH(01), ~
                      CH(02), CH(01), CH(02), CH(01), CH(04), CH(01), ~
                      CH(02), CH(01), CH(02), CH(01), CH(03), CH(01), ~
                      CH(04), CH(01), CH(02), CH(01), CH(02), CH(01), ~
                      CH(14), CH(01), CH(14), CH(01), CH(10), CH(01), ~
                      CH(02), CH(01), CH(08), CH(01), CH(01), CH(01)
					  
/* CR3085 added prefix sales order */					  

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

             open nodisplay #ff%, output, space = 200%,                  ~
                dpack   = 200%, ipack = 200%, file = file$,              ~
                library = library$, volume = volume$, blocks = 50%
        return

/* (AWD001) */
        lookup_carrier
          read #5, key = appor_so$, using FMT_CARRIER, arr_dte$, carrier$,~
                                            eod goto carrier_done
          fields$(30%) = arr_dte$
          call "DATFMTC" (fields$(30%), date%, fields$(30%))  /*(AWD002) */
          
FMT_CARRIER:     FMT POS(49), CH(06), POS(238), CH(02)
        
        carrier_done  
        return



        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
REM            call "SHOSTAT" ("One Moment Please")

            end
