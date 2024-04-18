        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN19                             *~
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
            *                      AWDDETAL - HNYDETAL                  *~
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
            * #1  ! HNYDETAL ! Inventory Details                        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "HNYDETAL",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =   43, keylen =   6, dup,    ~
                            key  2, keypos =   49, keylen =   2, dup     






            select #20, "AWDDETAL",                                      ~
                        varc,     indexed, recsize = 256,                ~
                        keypos = 1,    keylen = 68


                        

REM            call "SHOSTAT" ("Initialization")

            filename$ = "HNYDETAL" : call "EWDOPEN" (#1, filename$, err%)
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

            gosub create_hnydetal

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
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        create_hnydetal
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDDETAL"
             ff% = 20%  
             gosub open_file
      
             gosub read_hnydetal
                   goto L61010
        create_hnydetal_nxt
             gosub read_hnydetal_nxt
             if rec% <> 1% then goto hnydetal_done
L61010:
REM             gosub build_file
               gosub write_upload_hnydetal
               goto create_hnydetal_nxt
        return
        hnydetal_done
        return

        read_hnydetal
            init(" ") readkey$, readdate$, fields$()
            readdate$ = "20040101"
            call "DATFMTC" (readdate$)
            call "DATUFMTC" (readdate$)
            readkey$ = all(hex(00))
            str(readkey$,1%,6%) = readdate$
            mat num_fields = zer
            rec% = 0%
            read #1, key 1% > readkey$, eod goto read_hnydetal_done
                goto L61100
        read_hnydetal_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #1, eod goto read_hnydetal_done

L61100:         cnt% = cnt% + 1%

            goto L61155
            if mod(cnt%,50%) <> 0% then goto L61155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L61155:
                get #1, using L61110, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      num_fields(5%), num_fields(6%),~
                                      fields$(7%), fields$(8%),  ~
                                      num_fields(9%), num_fields(10%),~
                                      num_fields(11%), num_fields(12%),~
                                      fields$(13%), fields$(14%),  ~
                                      fields$(15%), num_fields(16%),  ~
                                      num_fields(17%), fields$(18%)


REM      I do not know if field 5 will work b/c it is number
L61110:           FMT CH(25), CH(03), CH(06), CH(01), BI(03), BI(04), ~
                      CH(06), CH(02), PD(15,4), PD(15,4), PD(15,4),   ~
                      PD(15,4), CH(40), CH(03), CH(06), PD(15,4),     ~
                      PD(15,4), CH(03)

            convert num_fields(5%) to fields$(5%), pic(-#######0)
            convert num_fields(6%) to fields$(6%), pic(-#######0)

            convert num_fields(9%) to fields$(9%), pic(-#######0.000#)
            convert num_fields(10%) to fields$(10%), pic(-#######0.000#)
            convert num_fields(11%) to fields$(11%), pic(-#######0.000#)
            convert num_fields(12%) to fields$(12%), pic(-#######0.000#)
            convert num_fields(16%) to fields$(16%), pic(-#######0.000#)
            convert num_fields(17%) to fields$(17%), pic(-#######0.000#)


            call "DATFMTC" (fields$(7%), date%, fields$(7%))
            call "DATFMTC" (fields$(15%), date%, fields$(15%))

            rec% = 1%            
        read_hnydetal_done
        return


        write_upload_hnydetal
            write #20, using L63400, fields$(1%), comma$,        ~
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
                              comma$

L63400:               FMT CH(25), CH(1), CH(03), CH(1), CH(06),   ~
                          CH(1), CH(01), CH(1),                   ~
                          CH(09), CH(1), /* POST DATE */          ~
                          CH(09), CH(1), /* SYS TIME  */          ~
                          CH(08), CH(1), /* HNY DATE  */          ~
                          CH(02), CH(1),                          ~
                          CH(14), CH(1), /* DET ON HAND */        ~
                          CH(14), CH(1), /* DET TOT COST */       ~
                          CH(14), CH(1), /* DET PRICE   */        ~
                          CH(14), CH(1), /* DET EXT     */        ~
                          CH(40), CH(1), CH(03), CH(1),           ~
                          CH(08), CH(1), /* SYSTEM DATE */        ~
                          CH(14), CH(1), /* SUMAS       */        ~
                          CH(14), CH(1), /* MDMC        */        ~
                          CH(03), CH(1)
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
