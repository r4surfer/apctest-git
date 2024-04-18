        REM *************************************************************~
            *                                                           *~
            *  Program Name      - ORCLPOEXT                            *~
            *  Creation Date     - 01/03/2012                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New Program to Build | delimited     *~
            *                      files to import into Oracle.         *~
            *                                                           *~
            *                                                           *~
            *                      This program only creates the Oracle *~
            *                      PO data to import!!                  *~
            *                                                           *~
            *                      This program only creates files      *~
            *                      ORLPOEXT - ORACLEPO                  *~
            *                                                           *~
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
            *01/03/2012! New Program for (AWD) - Last Mod Date    ! CMG *~
            *************************************************************

        dim                                                              ~
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
            
        dim oraKey$48,                   /* ORACLEPO readkey           */~
            oraRec$(8)253                /* ORACLEPO Record            */
            
            
        dim strength$1,                                                  ~
            oraclePart$40,                                               ~
            sandwich$20,                                                 ~
            warrantyseq$1,                                               ~
            sqFeet$8,                                                    ~
            genKey$15,                                                   ~
            desc$30
            
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
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #6,  "ORACLEPO",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  48,                     ~
                        alt key  1, keypos =   39, keylen =  10


            select #25, "ORLPOEXT",                                      ~
                        varc,     indexed, recsize = 256,               ~
                        keypos = 1,    keylen = 58
                        

                        


            filename$ = "GENCODES" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ORACLEPO" : call "EWDOPEN" (#6, filename$, err%)
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
            
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #1, schema_err%)   
                        
            mat num_fields     = zer

            file$   = "ORLPOEXT"
            ff% = 25%
            gosub open_file

            gosub initialize_variables
REM            gosub check_file
            if err% <> 0% then goto exit_program
            gosub files_analysis_daily
               goto exit_program

REM   END OF AUTOMATION


        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        files_analysis_daily
            comma$ = "|"

            init(" ") readkey$, fields$()
            readkey$ = all(hex(00))

        file_analysis_daily_next
            gosub read_oraclepo
               if rec% = 0% then goto daily_done
            gosub create_orlpoext

            gosub update_oraclepo
              goto file_analysis_daily_next

daily_done
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


        
        create_orlpoext
REM             init(" ") readkey$, file$
          rec% = 0%

          get #6, using mst_fmt, fields$(1%), fields$(2%), fields$(3%), ~
                                 fields$(4%), fields$(5%), fields$(6%), ~
                                 fields$(7%), fields$(8%), fields$(9%), ~
                                 fields$(10%), fields$(11%), fields$(12%), ~
                                 fields$(13%), fields$(14%), fields$(15%), ~
                                 num_fields(16%), num_fields(17%),         ~
                                 num_fields(18%), fields$(19%), fields$(20%)
          
            convert num_fields(16%) to fields$(16%), pic(-#######0.00##)
            convert num_fields(17%) to fields$(17%), pic(-#######0.00##)
            convert num_fields(18%) to fields$(18%), pic(-#######0.00##)

            call "DATFMTC" (fields$(2%), 0%, fields$(2%))


            gosub write_upload
        return
mst_fmt:     FMT CH(01), CH(06), CH(01), CH(05), CH(05), CH(20), CH(09), ~
                 CH(01), CH(20), CH(40), CH(20), CH(08), CH(02), CH(10), ~
                 CH(10), PD(14,4), PD(14,4), PD(14,4), CH(01), CH(16)
                 
        read_oraclepo
           rec% = 0%
           oraKey$ = all(hex(00))
           str(oraKey$,1%,1%) = "S"
           read #6, key > oraKey$, using orapo_fmt, oraRec$(),  ~
                                                      eod goto no_orapo

orapo_fmt:    FMT CH(2024)

              oraKey$ = str(oraRec$(),1,48)
              if str(oraKey$,1,1) <> "S" then goto no_orapo

              rec% = 1%
        no_orapo
        return                

        update_oraclepo
          read #6, hold, key = oraKey$, using orapo_fmt,     ~
                                  oraRec$(), eod goto no_updte_orapo


             str(oraRec$(),1,1)  = "T"
             str(oraRec$(),2,6) = date

             delete #6

             write #6, using orapo_fmt1, oraRec$(),  eod goto no_updte_orapo
                                             
orapo_fmt1:         FMT 8*CH(253)

        no_updte_orapo
        return                

        write_upload
           write #25, using L67400, fields$(1%), comma$,        ~
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
                             fields$(20%), comma$
 
L67400:   FMT CH(01), CH(01), CH(08), CH(01), CH(01), CH(01), CH(05), CH(01), ~
              CH(05), CH(01), CH(20), CH(01), CH(09), CH(01), CH(01), CH(01), ~
              CH(20), CH(01), CH(40), CH(01), CH(20), CH(01), CH(08), CH(01), ~
              CH(02), CH(01), CH(10), CH(01), CH(10), CH(01), CH(14), CH(01), ~
              CH(14), CH(01), CH(14), CH(01), CH(01), CH(01), CH(16), CH(01)

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
            if schema% = 1% then volume$ = "CARLO2"
            if schema% = 2% then volume$ = "NE2"            

             open nodisplay #ff%, output, space = 100%,                  ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return
        
        
        check_file
           init(" ") strength$, oraclePart$, sandwich$, warrantyseq$,  ~
                     sqFeet$, genKey$, desc$
           err% = 0%
           sqFeet = 0.00
           oraKey$ = all(hex(00))
           str(oraKey$,1%,1%) = "S"
           
        checkFileNxt
           read #6, hold,  key > oraKey$, using orapo_fmt, oraRec$(),  ~
                                                      eod goto nocheckFile


              oraKey$ = str(oraRec$(),1,48)
              if str(oraKey$,1,1) <> "S" then goto nocheckFile
              
              strength$    = str(oraRec$(),183%,1%)
              oraclePart$  = str(oraRec$(),69%,40%)
              sandwich$    = str(oraRec$(),109%,20%)
              warrantyseq$ = str(oraRec$(),48%,1%)
              
              if oraclePart$ <> "NoOraclePart" then goto checkFileNxt

              sqFeet  = 0.00
              get str(oraRec$()) using pd14_4FMT, sqFeet
pd14_4FMT:         FMT POS(175), PD(14,4)

              if strength$ <> " " and strength$ > " " then goto noSqFeet
                 strength$ = "4"
                 if sqFeet > 25 then strength$ = "5"
                 p%,p1% = 0%
                 p% = pos(sandwich$ = "OT")
                 p% = pos(sandwich$ = "OB")
                 if strength$ = "5" and p%  <> 0% then strength$ = "6"
                 if strength$ = "5" and p1% <> 0% then strength$ = "6"
                 
noSqFeet:

                gosub lookupPart
                if oraclePart% = 0% then err% = 1%
                if oraclePart% = 0% then goto checkFileNxt
                
                rewrite #6, using oraclpoRewriteFmt, oraclePart$, strength$,~
                      eod goto setErr

oraclpoRewriteFmt:   FMT POS(69), CH(40), POS(183), CH(1)

               goto checkFileNxt
         nocheckFile
         return
           call "ALLFREE"
         setErr
          err% = 2%
         return
         
         lookupPart
           oraclePart% = 0%
           init(" ") genKey$, desc$
           str(genKey$,1%,9%)  = "ORACLPART"
           str(genKey$,10%,1%) = strength$
           str(genKey$,11%,2%) = str(sandwich$,4%,2%)
           if warrantyseq$ = "2" then                ~ 
              str(genKey$,11%,2%) = str(sandwich$,7%,2%)
           
           read #1, key = genKey$, using genFmt, desc$, eod goto noOraclePart
genFmt:             FMT POS(25), CH(30)
           
                oraclePart$ = str(desc$,1%,10%)
                oraclePart% = 1%
         noOraclePart
         return


        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            close #25

REM            call "SHOSTAT" ("One Moment Please")  stop
 
            end



