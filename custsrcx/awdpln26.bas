        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN26                             *~
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
            *                      AWDEFFCY - EWDEFFCY                  *~
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
            num_fields(500%),            /* Generic Fields             */~
            eff_mdl$(100)3,              /* Efficiency Models          */~
            eff_model$(100)4,            /* Efficiency Models          */~
            eff_unit%(100),              /* Efficiency Units           */~
            eff_unit$(100)6,             /* Efficiency Units           */~
            eff_unit_s%(100),            /* Efficiency Units           */~
            eff_unit_s$(100)6,           /* Efficiency Units           */~
            eff_unit_ss%(100),           /* Efficiency Units           */~
            eff_unit_ss$(100)6,          /* Efficiency Units           */~
            eff_unit_p%(100),            /* Efficiency Units           */~
            eff_unit_p$(100)6            /* Efficiency Units           */
            
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
            * #8  ! EWDEFFCY ! New Efficiency Master Table File         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #8,   "EWDEFFCY",                                     ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    7, keylen =    13,                   ~
                        alt key  1, keypos =    1, keylen =  19 





                        
            select #27, "AWDEFFCY",                                      ~
                        varc,     indexed, recsize = 2020,               ~
                        keypos = 8,    keylen = 19
                        

REM            call "SHOSTAT" ("Initialization")


            filename$ = "EWDEFFCY" : call "EWDOPEN" (#8, filename$, err%)
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

            gosub create_eff


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


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        create_eff

             for k% = 1% to 100%
                 str(eff_model$(k%),4%,1%)   = "|"
                 str(eff_unit$(k%),6%,1%)    = "|"
                 str(eff_unit_s$(k%),6%,1%)  = "|"
                 str(eff_unit_ss$(k%),6%,1%) = "|"
                 str(eff_unit_p$(k%),6%,1%)  = "|"
             next k%


             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDEFFCY"
             ff% = 27%  
             gosub open_file
      
             gosub read_eff
                   goto L70010
        create_eff_nxt
             gosub read_eff_nxt
             if rec% <> 1% then goto eff_done
L70010:
REM             gosub build_file
               gosub write_upload_eff
               goto create_eff_nxt
        return
        eff_done
        return                
        
        read_eff
            init(" ") readkey$, fields$()
            date% = 0%
            mat num_fields = zer
            rec% = 0%
            str(readkey$,1%,5%) = "P2003"
            read #8, key > readkey$, eod goto read_eff_done

                goto L70100
        read_eff_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #8, eod goto read_eff_done

L70100:         cnt% = cnt% + 1%
            goto L70155
            if mod(cnt%,50%) <> 0% then goto L70155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L70155:
                get #8, using L70110, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), fields$(6%),  ~
                                      fields$(7%), eff_mdl$(),   ~
                                      eff_unit%(), eff_unit_s%(),~ 
                                      eff_unit_ss%(), eff_unit_p%()

REM      I do not know if field 5 will work b/c it is number
L70110:           FMT CH(06), CH(01), CH(04), CH(02), CH(01), CH(03), CH(02),  ~
                      100*CH(03), 100*BI(02), 100*BI(02), 100*BI(02), 100*BI(02)
            
            if str(fields$(3%),1%,4%) < "2003" then goto read_eff_nxt

            call "DATFMTC" (fields$(1%), date%, fields$(1%))

            for k% = 1% to 100%
                str(eff_model$(k%),1%,3%) = str(eff_mdl$(k%),1%,3%)
                convert eff_unit%(k%) to str(eff_unit$(k%),1%,5%), pic(#####)
                convert eff_unit_s%(k%) to str(eff_unit_s$(k%),1%,5%), pic(#####)
                convert eff_unit_ss%(k%) to str(eff_unit_ss$(k%),1%,5%), pic(#####)
                convert eff_unit_p%(k%) to str(eff_unit_p$(k%),1%,5%), pic(#####)
            next k%

            rec% = 1%            
        read_eff_done
        return


        write_upload_eff

            write #27, using L70400, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$, fields$(6%),  ~
                              comma$, fields$(7%), comma$,       ~
                              eff_model$(), eff_unit$(),         ~
                              eff_unit_s$(), eff_unit_ss$(),     ~
                              eff_unit_p$()

L70400:           FMT CH(08), CH(1), CH(01), CH(1), CH(04), CH(1), CH(02),    ~
                      CH(1), CH(01), CH(1), CH(03), CH(1), CH(02), CH(1),     ~
                      100*CH(4), 100*CH(6), 100*CH(6), 100*CH(6), 100*CH(6)
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
