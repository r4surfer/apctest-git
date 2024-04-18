        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN21                             *~
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
            *                      AWDROPHY - ROPHNY                    *~
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
            * #3  ! ROPHNY   ! File containing part specific ROP data   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #3,  "ROPHNY",                                        ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  104, keylen = 4, dup






            select #22, "AWDROPHY",                                      ~
                        varc,     indexed, recsize = 336,                ~
                        keypos = 1,    keylen = 26

REM            call "SHOSTAT" ("Initialization")

            filename$ = "ROPHNY"  :  call "EWDOPEN" (#3, filename$, err%)
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

            gosub create_rophny


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


        create_rophny
REM             call "SHOSTAT" (" CREATE ROPHNY " )
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDROPHY"
             ff% = 22%  
             gosub open_file
      
             gosub read_rophny
                   goto L61030
        create_rophny_nxt
             gosub read_rophny_nxt
             if rec% <> 1% then goto rophny_done
L61030:

               gosub write_upload_rophny
               goto create_rophny_nxt
        return
        rophny_done
        return

        read_rophny
            init(" ") fields$(), readkey$
            date% = 0%
            mat num_fields = zer
            rec% = 0%
            read #3, key > readkey$, eod goto read_rophny_done
                goto L61300
        read_rophny_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #3, eod goto read_rophny_done

L61300:         cnt% = cnt% + 1%
            goto L63155
            if mod(cnt%,50%) <> 0% then goto L63155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L63155:
                get #3, using L61140, fields$(1%), num_fields(2%), ~
                                      num_fields(3%), num_fields(4%),~
                                      num_fields(5%), num_fields(6%),~
                                      num_fields(7%), num_fields(8%),~
                                      num_fields(9%), num_fields(10%),~
                                      fields$(11%), fields$(12%),  ~
                                      fields$(13%)


L61140:           FMT CH(25), PD(15,4), PD(15,4), PD(15,4), PD(15,7),  ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,7),~
                      CH(06), CH(4), CH(149)





            convert num_fields(2%) to fields$(2%), pic(-#######0.000#)
            convert num_fields(3%) to fields$(3%), pic(-#######0.000#)
            convert num_fields(4%) to fields$(4%), pic(-#######0.000#)
            convert num_fields(5%) to fields$(5%), pic(-####0.000####)
            convert num_fields(6%) to fields$(6%), pic(-#######0.000#)
            convert num_fields(7%) to fields$(7%), pic(-#######0.000#)
            convert num_fields(8%) to fields$(8%), pic(-#######0.000#)
            convert num_fields(9%) to fields$(9%), pic(-#######0.000#)
            convert num_fields(10%) to fields$(10%), pic(-####0.000#000)


            call "DATFMTC" (fields$(11%), date%, fields$(11%))

            rec% = 1%            
        read_rophny_done
        return


        write_upload_rophny
            write #22, using L65400, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$, fields$(6%),  ~
                              comma$, fields$(7%), comma$,       ~
                              fields$(8%), comma$, fields$(9%),  ~
                              comma$, fields$(10%), comma$,      ~
                              fields$(11%), comma$, fields$(12%),~
                              comma$, fields$(13%), comma$

L65400:               FMT CH(25), CH(1),                         ~
                          CH(14), CH(1),   /* ROP        */      ~
                          CH(14), CH(1),   /* EOQ        */      ~
                          CH(14), CH(1),   /* AVG USAGE  */      ~
                          CH(14), CH(1),   /* STD DEV    */      ~
                          CH(14), CH(1),   /* SAVE AVG US*/      ~
                          CH(14), CH(1),   /* SAVE ROP   */      ~
                          CH(14), CH(1),   /* SAVE EOQ   */      ~
                          CH(14), CH(1),   /* SAVE ESS   */      ~
                          CH(14), CH(1),   /* SAVE STD DE*/      ~
                          CH(08), CH(1),   /* DATE CHANGE*/      ~
                          CH(04), CH(1), CH(149), CH(1)

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
