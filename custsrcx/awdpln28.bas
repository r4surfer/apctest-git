        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN28                             *~
            *  Creation Date     - 02/11/05                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New Program to Build | delimited     *~
            *                      files to import into Oracle.         *~
            *                                                           *~
            *                                                           *~
            *                      This program only creates the        *~
            *                        Complaint file data to import!!    *~
            *                                                           *~
            *                      This program only creates files      *~
            *                      need for Shanes Reports              *~
            *                      AWDCOMPT - APCCOMPT                  *~
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
            * 02/14/07 ! AWD001 Add new fields to Oracle file     ! DES *~
            *************************************************************

        dim                              /* (        ) - FILE          */~
            readkey$100,                 /* Readkey                    */~
            blankfield$1,                /* Blank Field End Comp Num   */~
            fields$(500%)256,            /* Generic Fields             */~
            num_fields(500%)             /* Generic Fields             */
            
        dim                              /* (Program) - Variables      */~
            filename$8,                  /* Used By EWDOPEN            */~
            cnt$28,                      /* Screen Display             */~
            hdr$45, msg$(3%)79,          /* Askuser - Var's            */~
            date$8,                      /* REPORT TITLE               */~
            userid$3                     /* Current User Id            */

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
            mat fs% = con
            init(" ") rslt$()

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCCOMPT ! COMPLAINT TRACKING MASTER FILE           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "APCCOMPT",                                     ~
/*PAR000*/              varc,     indexed,  recsize = 256,               ~
                        keypos =    1, keylen =    5,                    ~
                        alt key  1, keypos =    6, keylen =  10,         ~
                            key  2, keypos =   16, keylen =  16, dup,    ~
                            key  3, keypos =   32, keylen =   8, dup,    ~
                            key  4, keypos =   40, keylen =  13, dup


            select #20, "AWDCOMPL",                                      ~
/*PAR000*/              varc,     indexed, recsize = 384,                ~
                        keypos = 1,    keylen = 12 


REM            call "SHOSTAT" ("Initialization")

            filename$ = "APCCOMPT" : call "EWDOPEN" (#1, filename$, err%)
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

            gosub create_complaint

        return



        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") readkey$, fields$(), filename$, cnt$, hdr$, msg$(),~
                      date$, library$, volume$, blankfield$

        return

        REM *************************************************************~
            *************************************************************


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        create_complaint
             init(" ") readkey$, file$, blankfield$
             rec%, cnt% = 0%
             file$   = "AWDCOMPT"
             ff% = 20%  
             gosub open_file
      
             gosub read_complaint
                   goto L61010
        create_complaint_nxt
             gosub read_complaint_nxt
             if rec% <> 1% then goto complaint_done
L61010:
REM             gosub build_file
               gosub write_upload_complaint
               goto create_complaint_nxt
        return
        complaint_done
        return

        read_complaint
            init(" ") readkey$, fields$(), blankfield$, comp_number$
            readkey$ = all(hex(00))
         goto read_file
            comp_number$ = "00001077"

            convert comp_number$ to comp_number%, data goto L60010
L60010:

            put str(comp_number$,1%,4%), using L61120, comp_number%

            str(comp_number$,5%,1%) = " "
            str(readkey$,1%,5%)     = str(comp_number$,1%,5%)
read_file:
            mat num_fields = zer
            rec% = 0%
            comp_number% = 0%
            read #1, hold, key > readkey$, eod goto read_complaint_done
                goto L61100
        read_complaint_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #1, hold, eod goto read_complaint_done

L61100:         cnt% = cnt% + 1%

            goto L61155
            if mod(cnt%,50%) <> 0% then goto L61155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L61155:
                get #1, using L61110, str(fields$(1%),1%,4%),         ~
                                      blankfield$,                    ~
                                      fields$(2%), fields$(3%),       ~
                                      fields$(4%), fields$(5%),       ~
                                      fields$(6%), fields$(7%),       ~
                                      fields$(8%), fields$(9%),       ~
                                      fields$(10%), fields$(11%),     ~
                                      fields$(12%), num_fields(13%),  ~
                                      fields$(14%), fields$(15%),     ~
                                      fields$(16%),  fields$(17%),    ~
                                      fields$(18%), fields$(19%),     ~
                                      fields$(20%), fields$(21%),     ~
                                      num_fields(22%), fields$(23%),  ~
                                      fields$(24%), fields$(25%),     ~
                                      fields$(26%), fields$(27%),     ~
                                      fields$(28%), fields$(29%),     ~
                                      fields$(30%), fields$(31%),     ~
/*PAR000*/                            fields$(32%),                   ~
/*AWD001*/                            fields$(34%), fields$(35%),     ~
/*AWD001*/                            fields$(36%)


REM      I do not know if field 5 will work b/c it is number
L61110:           FMT CH(04), CH(01), CH(08), CH(02), CH(16), CH(08),   ~
                      CH(04), CH(09), CH(25), CH(04), CH(03), CH(02),   ~
                      CH(06), BI(04), CH(02), CH(06), CH(04), CH(02),   ~
                      CH(06), CH(04), CH(02), CH(04), PD(14,4), CH(03), ~
                      CH(06), CH(01), CH(01), CH(01), CH(01), CH(01),   ~
/* AWD001 */          CH(01), CH(01), CH(20), CH(20), CH(08), CH(02)            
REM                   CH(01), CH(01), CH(20) 

            get str(fields$(1%),1%,4%), using L61120, comp_number%
L61120:             FMT BI(4)
            convert comp_number% to fields$(1%), pic(00000000)
            convert num_fields(22%) to fields$(22%), pic(-#######0.000#)
            convert num_fields(13%) to fields$(13%), pic(########0)

            init(" ") x$
            x$ = fields$(12%)
            call "DATEOKC" (x$, date%, " ")
            if date% <> 0% then                      ~
                 call "DATFMTC" (fields$(12%), date%, fields$(12%))
            if date% = 0% then init(" ") fields$(12%)
            init(" ") x$
            x$ = fields$(15%)
            call "DATEOKC" (x$, date%, " ")
            if date% <> 0% then                      ~
                 call "DATFMTC" (fields$(15%), date%, fields$(15%))
            if date% = 0% then init(" ") fields$(15%)
            init(" ") x$
            x$ = fields$(18%)
            call "DATEOKC" (x$, date%, " ")
            if date% <> 0% then                      ~
                 call "DATFMTC" (fields$(18%), date%, fields$(18%))
            if date% = 0% then init(" ") fields$(18%)
            init(" ") x$
            x$ = fields$(24%)
            call "DATEOKC" (x$, date%, " ")
            if date% <> 0% then                      ~
                 call "DATFMTC" (fields$(24%), date%, fields$(24%))
            if date% = 0% then init(" ") fields$(24%)

            
REM            if fields$(13%) <> " " then num_fields(13%) = VAL(fields$(13%),4)                                

            
REM            convert num_fields(13%) to fields$(13%), pic(########0)
REM            convert num_fields(32%) to fields$(32%), pic(########0)
REM            convert num_fields(33%) to fields$(33%), pic(########0)
REM            convert num_fields(45%) to fields$(45%), pic(########0)
REM            convert num_fields(61%) to fields$(61%), pic(########0)
REM            convert num_fields(62%) to fields$(62%), pic(########0)
   
            if str(fields$(13%),1%,4%) = hex(ffffffff) then fields$(13%) = " "
            if str(fields$(13%),1%,4%) = all(hex(ff)) then fields$(13%) = " "
            if str(fields$(13%),1%,4%) < " " then fields$(13%) = " "

            if str(fields$(34%),1,256) < " " then str(fields$(34%),1,256) = " "
            if str(fields$(35%),1,256) < " " then str(fields$(35%),1,256) = " "
            if str(fields$(36%),1,256) < " " then str(fields$(36%),1,256) = " "

REM This code is for bad values in the fields
            if str(fields$(16%),1%,4%) = hex(ffffffff) then fields$(16%) = " "
            if str(fields$(16%),1%,4%) = all(hex(ff)) then fields$(16%) = " "
            if str(fields$(16%),1%,4%) < " " then fields$(16%) = " "

            if str(fields$(19%),1%,4%) = hex(ffffffff) then fields$(19%) = " "
            if str(fields$(19%),1%,4%) = all(hex(ff)) then fields$(19%) = " "
            if str(fields$(19%),1%,4%) < " " then fields$(19%) = " "


            if str(fields$(21%),1%,4%) = hex(ffffffff) then fields$(21%) = " "
            if str(fields$(21%),1%,4%) = all(hex(ff)) then fields$(21%) = " "
            if str(fields$(21%),1%,4%) < " " then fields$(21%) = " "


            p% = 0%
            p% = pos(fields$(4%) = "|")
            if p% <> 0% then str(fields$(4%),p%,1%) = " "

            rec% = 1%            
        read_complaint_done
        return


        write_upload_complaint
            write #20, using L63400, fields$(1%), comma$,        ~
                              blankfield$, comma$,               ~
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
/*PAR000*/                    fields$(32%), comma$,              ~ 
/*AWD001*/		      fields$(34%), comma$, fields$(35%), comma$, ~
/*AWD001*/		      fields$(36%), comma$

L63400:      FMT      CH(08), CH(1),                               ~
                      CH(01), CH(1),                               ~
                      CH(08), CH(1), CH(02), CH(1),                ~
                      CH(16), CH(1), CH(08), CH(1), CH(04), CH(1), ~
                      CH(09), CH(1), CH(25), CH(1), CH(04), CH(1), ~
                      CH(03), CH(1), CH(02), CH(1),                ~
                      CH(08), CH(1),  /* Complaint Init Date */    ~
                      CH(09), CH(1),  /* Complaint Text ID   */    ~
                      CH(02), CH(1),                               ~
                      CH(08), CH(1),  /* Complaint Service Date */ ~
                      CH(04), CH(1), CH(02), CH(1),                ~
                      CH(08), CH(1),  /* Complaint error Date */   ~
                      CH(04), CH(1), CH(02), CH(1), CH(04), CH(1), ~
                      CH(14), CH(1),  /* Complaint Cost  */        ~
                      CH(03), CH(1),                               ~
                      CH(08), CH(1),  /* Date Last Modified */     ~
                      CH(01), CH(1),                               ~ 
                      CH(01), CH(1), CH(01), CH(1), CH(01), CH(1), ~
                      CH(01), CH(1), CH(01), CH(1), CH(01), CH(1), ~
                      CH(20), CH(1),  /* Subpart PAR000 */         ~
/*AWD001*/            CH(20), CH(1),                               ~ 
/*AWD001*/            CH(08), CH(1),                               ~ 
/*AWD001*/            CH(02), CH(1)                                
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
