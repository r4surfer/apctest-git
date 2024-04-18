        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN24                             *~
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
            *                      AWDRCVMS - RCVMASTR                  *~
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
            * #6  ! RCVMASTR ! Receiver Master File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************



            select #6,  "RCVMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos= 1, keylen = 16





                        
            select #25, "AWDRCVMS",                                      ~
                        varc,     indexed, recsize = 2020,               ~
                        keypos = 1,    keylen = 17
                        


REM            call "SHOSTAT" ("Initialization")


            filename$ = "RCVMASTR" : call "EWDOPEN" (#6, filename$, err%)
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

            gosub create_rcvms

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



        create_rcvms
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDRCVMS"
             ff% = 25%  
             gosub open_file
      
             gosub read_rcvms
                   goto L68010
        create_rcvms_nxt
             gosub read_rcvms_nxt
             if rec% <> 1% then goto rcvms_done
L68010:
REM             gosub build_file
               gosub write_upload_rcvms
               goto create_rcvms_nxt
        return
        rcvms_done
        return                
        
        read_rcvms
            init(" ") readkey$, fields$()
            date% = 0%
            mat num_fields = zer
            rec% = 0%
            read #6, key > readkey$, eod goto read_rcvms_done

                goto L68100
        read_rcvms_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #6, eod goto read_rcvms_done

L68100:         cnt% = cnt% + 1%
            goto L68155
            if mod(cnt%,50%) <> 0% then goto L68155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L68155:
                get #6, using L68110, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), fields$(6%),  ~
                                      fields$(7%), fields$(8%),  ~
                                      fields$(9%), fields$(10%)


REM      I do not know if field 5 will work b/c it is number
L68110:           FMT CH(16), CH(09), CH(30), CH(30), CH(06), CH(01),          ~
                      CH(04), CH(08), CH(30), CH(16)

            
            

            call "DATFMTC" (fields$(5%), date%, fields$(5%))            

            rec% = 1%            
        read_rcvms_done
        return


        write_upload_rcvms

            write #25, using L68400, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$, fields$(6%),  ~
                              comma$, fields$(7%), comma$,       ~
                              fields$(8%), comma$, fields$(9%),  ~
                              comma$, fields$(10%), comma$

L68400:           FMT CH(16), CH(1), CH(09), CH(1), CH(30), CH(1), CH(30),    ~
                      CH(1),                                                  ~
                      CH(08), CH(1),                  /* Date Received */     ~
                      CH(01), CH(1), CH(04), CH(1), CH(08), CH(1), CH(30),    ~
                      CH(1), CH(16), CH(1)
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
