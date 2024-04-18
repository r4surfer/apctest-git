        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN25                             *~
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
            *                      AWDRCVLS - RCVLINES                  *~
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
            * #7  ! RCVLINES ! Receiver Line Items                      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #7,  "RCVLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 800,                                   ~
                        keypos= 26, keylen = 52,                         ~
                        alt key 1, keypos =  1, keylen = 69,             ~
                            key 2, keypos = 42, keylen = 36,             ~
                            key 3, keypos =128, keylen = 24 

            select #26, "AWDRCVLS",                                      ~
                        varc,     indexed, recsize = 2020,               ~
                        keypos = 27,    keylen = 57
                        

REM            call "SHOSTAT" ("Initialization")
            filename$ = "RCVLINES" : call "EWDOPEN" (#7, filename$, err%)
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

            gosub create_rcvls


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

        
        create_rcvls
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDRCVLS"
             ff% = 26%  
             gosub open_file
      
             gosub read_rcvls
                   goto L69010
        create_rcvls_nxt
             gosub read_rcvls_nxt
             if rec% <> 1% then goto rcvls_done
L69010:
REM             gosub build_file
               gosub write_upload_rcvls
               goto create_rcvls_nxt
        return
        rcvls_done
        return                
        
        read_rcvls
            init(" ") readkey$, fields$()
            mat num_fields = zer
            rec% = 0%
            read #7, key > readkey$, eod goto read_rcvls_done

                goto L69100
        read_rcvls_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #7, eod goto read_rcvls_done

L69100:         cnt% = cnt% + 1%
            goto L69155
            if mod(cnt%,50%) <> 0% then goto L69155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L69155:
                get #7, using L69110, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), num_fields(6%),  ~
                                      fields$(7%), fields$(8%),  ~
                                      fields$(9%), fields$(10%), ~
                                      fields$(11%), num_fields(12%),~
                                      fields$(13%), num_fields(14%),~
                                      num_fields(15%), num_fields(16%),~
                                      num_fields(17%), num_fields(18%),~
                                      num_fields(19%), num_fields(20%),~
                                      num_fields(21%), num_fields(22%),~
                                      num_fields(23%), num_fields(24%),~
                                      fields$(25%), num_fields(26%),~
                                      num_fields(27%), num_fields(28%),~
                                      num_fields(29%), num_fields(30%),~
                                      num_fields(31%), fields$(32%),~
                                      fields$(33%), fields$(34%),~
                                      fields$(35%), fields$(36%),~
                                      fields$(37%), fields$(38%),~
                                      fields$(39%), fields$(40%),~
                                      num_fields(41%), num_fields(42%),~
                                      num_fields(43%), num_fields(44%),~
                                      fields$(45%), fields$(46%),~
                                      num_fields(47%), fields$(48%),~
                                      num_fields(49%), num_fields(50%),~
                                      num_fields(51%), num_fields(52%),~
                                      num_fields(53%), num_fields(54%),~
                                      num_fields(55%), num_fields(56%),~
                                      num_fields(57%), num_fields(58%), ~
                                      num_fields(59%), num_fields(60%), ~
                                      num_fields(61%), num_fields(62%), ~
                                      num_fields(63%), num_fields(64%), ~
                                      num_fields(65%), num_fields(66%), ~
                                      num_fields(67%), num_fields(68%), ~
                                      num_fields(69%), num_fields(70%), ~
                                      num_fields(71%), num_fields(72%), ~
                                      num_fields(73%), num_fields(74%), ~
                                      fields$(75%), num_fields(76%), ~
                                      fields$(77%), fields$(78%)


REM      I do not know if field 5 will work b/c it is number
L69110:           FMT CH(25), CH(16), CH(09), CH(16), CH(03), BI(04), POS(78), CH(32),~
                      CH(06), CH(06), CH(06), CH(16), BI(04), POS(152), CH(04),        ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),      ~
                      PD(15,4), PD(15,4), PD(15,7), PD(15,7), PD(15,7),      ~
                      PD(15,4), CH(04), PD(15,4), PD(15,4), PD(15,4),        ~
                      PD(15,4), PD(15,4), PD(15,4), CH(09), CH(09), CH(09),  ~
                      CH(09), CH(03), CH(06), CH(03), CH(06), CH(06),        ~
                      PD(15,4), PD(15,7), PD(15,4), PD(15,4), CH(04), CH(04),~
                      BI(04), CH(08), PD(15,4), PD(15,4), PD(15,4), PD(15,4),~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),      ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),      ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),      ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),      ~
                      PD(15,4), PD(15,4), CH(22), PD(15,4), CH(06), CH(149)

            convert num_fields(6%) to fields$(6%), pic(#######0)   
            convert num_fields(12%) to fields$(12%), pic(#######0) 
            convert num_fields(47%) to fields$(47%), pic(########0)   

            convert num_fields(14%) to fields$(14%), pic(-#######0.00##)   
            convert num_fields(15%) to fields$(15%), pic(-#######0.00##)   
            convert num_fields(16%) to fields$(16%), pic(-#######0.00##)   
            convert num_fields(17%) to fields$(17%), pic(-#######0.00##)   
            convert num_fields(18%) to fields$(18%), pic(-#######0.00##)   
            convert num_fields(19%) to fields$(19%), pic(-#######0.00##)   
            convert num_fields(20%) to fields$(20%), pic(-#######0.00##)   

            convert num_fields(21%) to fields$(21%), pic(-####0.00#####)
            convert num_fields(22%) to fields$(22%), pic(-####0.00#####)
            convert num_fields(23%) to fields$(23%), pic(-####0.00#####)

            convert num_fields(24%) to fields$(24%), pic(-#######0.00##)   
            convert num_fields(26%) to fields$(26%), pic(-#######0.00##)   
            convert num_fields(27%) to fields$(27%), pic(-#######0.00##)   
            convert num_fields(28%) to fields$(28%), pic(-#######0.00##)   
            convert num_fields(29%) to fields$(29%), pic(-#######0.00##)   
            convert num_fields(30%) to fields$(30%), pic(-#######0.00##)   
            convert num_fields(31%) to fields$(31%), pic(-#######0.00##)   
            convert num_fields(41%) to fields$(41%), pic(-#######0.00##)   
            convert num_fields(42%) to fields$(42%), pic(-####0.00#####)
            convert num_fields(43%) to fields$(43%), pic(-#######0.00##)   
            convert num_fields(44%) to fields$(44%), pic(-#######0.00##)   
            convert num_fields(49%) to fields$(49%), pic(-#######0.00##)
            convert num_fields(50%) to fields$(50%), pic(-#######0.00##)
            convert num_fields(51%) to fields$(51%), pic(-#######0.00##)
            convert num_fields(52%) to fields$(52%), pic(-#######0.00##)
            convert num_fields(53%) to fields$(53%), pic(-#######0.00##)
            convert num_fields(54%) to fields$(54%), pic(-#######0.00##)
            convert num_fields(55%) to fields$(55%), pic(-#######0.00##)
            convert num_fields(56%) to fields$(56%), pic(-#######0.00##)
            convert num_fields(57%) to fields$(57%), pic(-#######0.00##)
            convert num_fields(58%) to fields$(58%), pic(-#######0.00##)
            convert num_fields(59%) to fields$(59%), pic(-#######0.00##)
            convert num_fields(60%) to fields$(60%), pic(-#######0.00##)
            convert num_fields(61%) to fields$(61%), pic(-#######0.00##)
            convert num_fields(62%) to fields$(62%), pic(-#######0.00##)
            convert num_fields(63%) to fields$(63%), pic(-#######0.00##)
            convert num_fields(64%) to fields$(64%), pic(-#######0.00##)
            convert num_fields(65%) to fields$(65%), pic(-#######0.00##)
            convert num_fields(66%) to fields$(66%), pic(-#######0.00##)
            convert num_fields(67%) to fields$(67%), pic(-#######0.00##)
            convert num_fields(68%) to fields$(68%), pic(-#######0.00##)
            convert num_fields(69%) to fields$(69%), pic(-#######0.00##)
            convert num_fields(70%) to fields$(70%), pic(-#######0.00##)
            convert num_fields(71%) to fields$(71%), pic(-#######0.00##)
            convert num_fields(72%) to fields$(72%), pic(-#######0.00##)
            convert num_fields(73%) to fields$(73%), pic(-#######0.00##)
            convert num_fields(74%) to fields$(74%), pic(-#######0.00##)
            convert num_fields(76%) to fields$(76%), pic(-#######0.00##)   

            call "DATFMTC" (fields$(8%), date%, fields$(8%))            
            call "DATFMTC" (fields$(9%), date%, fields$(9%))
            call "DATFMTC" (fields$(10%), date%, fields$(10%))
            call "DATFMTC" (fields$(77%), date%, fields$(77%))

            rec% = 1%            
        read_rcvls_done
        return


        write_upload_rcvls

            write #26, using L69400, fields$(1%), comma$,        ~
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
                              fields$(44%), comma$, fields$(45%),~
                              comma$, fields$(46%), comma$,      ~
                              fields$(47%), comma$, fields$(48%),~
                              comma$, fields$(49%), comma$,      ~
                              fields$(50%), comma$, fields$(51%),~
                              comma$, fields$(52%), comma$,      ~
                              fields$(53%), comma$, fields$(54%),~
                              comma$, fields$(55%), comma$,      ~
                              fields$(56%), comma$, fields$(57%),~
                              comma$, fields$(58%), comma$,      ~
                              fields$(59%), comma$, fields$(60%),~
                              comma$, fields$(61%), comma$,      ~
                              fields$(62%), comma$, fields$(63%),~
                              comma$, fields$(64%), comma$,      ~
                              fields$(65%), comma$, fields$(66%),~
                              comma$, fields$(67%), comma$,      ~
                              fields$(68%), comma$, fields$(69%),~
                              comma$, fields$(70%), comma$,      ~
                              fields$(71%), comma$, fields$(72%),~
                              comma$, fields$(73%), comma$,      ~
                              fields$(74%), comma$, fields$(75%),~
                              comma$, fields$(76%), comma$,      ~
                              fields$(77%), comma$, fields$(78%),~
                              comma$

L69400:           FMT CH(25), CH(1), CH(16), CH(1), CH(09), CH(1), CH(16),   ~
                      CH(1), CH(03), CH(1), CH(08), CH(1), CH(32), CH(1),    ~
                      CH(08), CH(1),                    /* Date Ordered */   ~
                      CH(08), CH(1),                    /* Date Due     */   ~ 
                      CH(08), CH(1),                    /* Date Received*/   ~
                      CH(16), CH(1), CH(08), CH(1), CH(04), CH(1),           ~
                      CH(14), CH(1),                    /* Total Received*/  ~
                      CH(14), CH(1),                    /* Total Rec Hold*/  ~
                      CH(14), CH(1),                    /* Total In QC   */  ~
                      CH(14), CH(1),                    /* Total Rejected*/  ~
                      CH(14), CH(1),                    /* Total Returned*/  ~
                      CH(14), CH(1),                    /* Total On Hand */  ~
                      CH(14), CH(1),                    /* Purchase Price*/  ~
                      CH(14), CH(1),                    /* Unused        */  ~
                      CH(14), CH(1),                    /* Unused        */  ~
                      CH(14), CH(1),                    /* # Of Stocking */  ~
                      CH(14), CH(1),                    /* Unit Of Measur*/  ~
                      CH(04), CH(1),                                         ~
                      CH(14), CH(1),                    /* Extension     */  ~
                      CH(14), CH(1),                    /* Not Invoiced  */  ~
                      CH(14), CH(1),                    /* Amt Invoiced  */  ~
                      CH(14), CH(1),                    /* AP Adjust Amt */  ~
                      CH(14), CH(1),                    /* Ret of Ven Amt*/  ~
                      CH(14), CH(1),                    /* Unused        */  ~
                      CH(09), CH(1), CH(09), CH(1), CH(09),  CH(1),          ~
                      CH(09), CH(1), CH(03), CH(1), CH(06), CH(1), CH(03),   ~
                      CH(1), CH(06), CH(1), CH(06), CH(1),                   ~
                      CH(14), CH(1),                    /* Qty to Buy    */  ~
                      CH(14), CH(1),                    /* AP Price      */  ~
                      CH(14), CH(1),                    /* Unused        */  ~
                      CH(14), CH(1),                    /* Unused        */  ~
                      CH(04), CH(1), CH(04), CH(1),                          ~
                      CH(09), CH(1),                    /* Serial Index  */  ~
                      CH(08), CH(1),                                         ~
                      CH(14), CH(1),                    /* Total Cost PO */  ~
                      CH(14), CH(1),                    /* Inv Cost1     */  ~
                      CH(14), CH(1),                    /* Inv Cost2     */  ~
                      CH(14), CH(1),                    /* Inv Cost3     */  ~
                      CH(14), CH(1),                    /* Inv Cost4     */  ~
                      CH(14), CH(1),                    /* Inv Cost5     */  ~
                      CH(14), CH(1),                    /* Inv Cost6     */  ~
                      CH(14), CH(1),                    /* Inv Cost7     */  ~
                      CH(14), CH(1),                    /* Inv Cost8     */  ~
                      CH(14), CH(1),                    /* Inv Cost9     */  ~
                      CH(14), CH(1),                    /* Inv Cost10    */  ~
                      CH(14), CH(1),                    /* Inv Cost11    */  ~
                      CH(14), CH(1),                    /* Inv Cost12    */  ~
                      CH(14), CH(1),                    /* Total Cost AP */  ~
                      CH(14), CH(1),                    /* Inv Cost1     */  ~
                      CH(14), CH(1),                    /* Inv Cost2     */  ~
                      CH(14), CH(1),                    /* Inv Cost3     */  ~
                      CH(14), CH(1),                    /* Inv Cost4     */  ~
                      CH(14), CH(1),                    /* Inv Cost5     */  ~
                      CH(14), CH(1),                    /* Inv Cost6     */  ~
                      CH(14), CH(1),                    /* Inv Cost7     */  ~
                      CH(14), CH(1),                    /* Inv Cost8     */  ~
                      CH(14), CH(1),                    /* Inv Cost9     */  ~
                      CH(14), CH(1),                    /* Inv Cost10    */  ~
                      CH(14), CH(1),                    /* Inv Cost11    */  ~
                      CH(14), CH(1),                    /* Inv Cost12    */  ~
                      CH(22), CH(1),                                         ~
                      CH(14), CH(1),                    /* Qty Failed    */  ~
                      CH(08), CH(1),                    /* Org Due Date  */  ~
                      CH(149), CH(1)

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
