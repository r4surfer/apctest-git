        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN20                             *~
            *  Creation Date     - 09/10/04                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New Program to Build | delimited     *~
            *                      files to import into Oracle.         *~
            *                                                           *~
            *                                                           *~
            *                      This program only creates the booking*~
            *                      master and lines data to import!!    *~
            *                                                           *~
            *                      This program only creates files      *~
            *                      need for Shanes Reports              *~
            *                      AWDVENPR - VENPRICE                  *~
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
            * 09/10/04 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            *************************************************************

        dim                              /* (        ) - FILE          */~
            readkey$100,                 /* Readkey                    */~
            fields$(500%)256,            /* Generic Fields             */~
            num_fields(500%)             /* Generic Fields             */
            
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
            * #2  ! VENPRICE ! Vendor Price Catalogue File              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #2,  "VENPRICE",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 256,                                  ~
                         keypos = 10, keylen = 59,                       ~
                         alternate key 1, keypos = 1, keylen = 34, dup,  ~
                                   key 2, keypos =35, keylen = 34





            select #21, "AWDVENPR",                                      ~
                        varc,     indexed, recsize = 336,                ~
                        keypos =    11, keylen =    62


REM            call "SHOSTAT" ("Initialization")

            filename$ = "VENPRICE" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
REM            filename$ = "ROPHNY"    call "EWDOPEN" (#3, filename$, err%)

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************


            call "EXTRACT" addr("ID", userid$)
            date$ = date

            call "DATEFMT" (date$)
            mat num_fields      = zer


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

            gosub create_prc

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

        create_prc
REM             call "SHOSTAT" (" Create PRC " ) 
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDVENPR"
             ff% = 21%  
             gosub open_file
      
             gosub read_prc     
                   goto L61020
        create_prc_nxt
             gosub read_prc_nxt
             if rec% <> 1% then goto prc_done
L61020:

               gosub write_upload_prc
               goto create_prc_nxt
        return
        prc_done
        return

        read_prc
            init(" ") readkey$, fields$()
            mat num_fields = zer
            rec% = 0%
            read #2, key > readkey$, eod goto read_prc_done
                goto L61200
        read_prc_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #2, eod goto read_prc_done

L61200:         cnt% = cnt% + 1%
            goto L62155
            if mod(cnt%,50%) <> 0% then goto L62155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L62155:
                get #2, using L61130, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      num_fields(5%), fields$(6%),~
                                      fields$(7%), fields$(8%),  ~
                                      fields$(9%), num_fields(10%),~
                                      num_fields(11%), fields$(12%),~
                                      fields$(13%), fields$(14%), ~
                                      fields$(15%), fields$(16%), ~
                                      num_fields(17%), num_fields(18%),~
                                      num_fields(19%), fields$(20%),~
                                      fields$(21%), fields$(22%)


L61130:           FMT CH(09), CH(25), CH(09), CH(25), PD(15,5), CH(06),  ~
                      CH(03), CH(03), CH(04), PD(15,4), PD(15,4), CH(06),~
                      CH(06), CH(06), CH(03), CH(04), PD(15,4), PD(15,4),~ 
                      PD(15,4), CH(04), CH(01), CH(94)

            convert num_fields(5%) to fields$(5%), pic(-#######0.000##)
            convert num_fields(10%) to fields$(10%), pic(-#######0.000#)
            convert num_fields(11%) to fields$(11%), pic(-#######0.000#)
REM            convert num_fields(17%) to fields$(17%), pic(-#######0.000#)
REM            convert num_fields(18%) to fields$(18%), pic(-#######0.000#)
REm            convert num_fields(19%) to fields$(19%), pic(-#######0.000#)


            call "DATFMTC" (fields$(6%), date%, fields$(6%))
            call "DATFMTC" (fields$(12%), date%, fields$(12%))
            call "DATFMTC" (fields$(13%), date%, fields$(13%))
            call "DATFMTC" (fields$(14%), date%, fields$(14%))

            rec% = 1%            
        read_prc_done
        return


        write_upload_prc

            write #21, using L64400, fields$(1%), comma$,        ~
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
                              comma$, fields$(22%), comma$

L64400:               FMT CH(09), CH(1), CH(25), CH(1), CH(09),  ~
                          CH(1), CH(25), CH(1),                  ~
                          CH(14), CH(1),     /* VENPPU  */       ~
                          CH(08), CH(1),     /* PRICEDATE */     ~
                          CH(03), CH(1), CH(03), CH(1), CH(04),  ~
                          CH(1),                                 ~
                          CH(14), CH(1),     /* QTY-UOM */       ~
                          CH(14), CH(1),     /* CONTRACTPRICE*/  ~
                          CH(08), CH(1),     /* EFFECTIVEDATE*/  ~
                          CH(08), CH(1),     /* EXPIREDDATE  */  ~
                          CH(08), CH(1),     /* LASTMODIFED  */  ~
                          CH(03), CH(1), CH(04), CH(1),          ~
                          CH(14), CH(1),     /* PRICE        */  ~
                          CH(14), CH(1),     /* DISCPCNT     */  ~
                          CH(14), CH(1),     /* DISCAMOUNT   */  ~
                          CH(04), CH(1), CH(01), CH(1), CH(94), CH(1)
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
            volume$         = "CARLO2"


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
