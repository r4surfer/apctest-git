*       ****************************************************************~
*                       NEW -( R e p o r t s )                         *~
*                                                                      *~
*                                                                      *~
*        EWDPLA59 -  Data file listing report of records in APCPLNWC   *~
*                    (Tube Windings & Coil Data).  Prints specific     *~
*                    Model No. or ALL as requested by the user.        *~
*                                                                      *~
*----------------------------------------------------------------------*
*Mod Date! Description                                             !By *
*--------+---------------------------------------------------------+---*
*08/17/98! New Program                                             !BWS*
*04/27/99! (EWD001) Add Lookup Selection & Title for Laminate.     !BWS*
************************************************************************

        sub "EWDPLA59" (modelin$,        /* Specific Model or ALL      */~
/*EWD001*/              luin$,           /* Specific Lookup or ALL(" ")*/~
                        #1,              /* (APCPLNWC) Winds & Coils   */~
                        #3)              /* (GENCODES) General Codes   */ 

        dim wrk_key1$15,                 /* Winds/Coils Work Key       */~
            wrk_rec$49,                  /* Winds/Coils Work Record    */~
            readkey$50,                  /* Code Table Key             */~
            date$8,                      /* Date for screen display    */~
            descr$32,                    /* Code Table Description     */~
            scr_msg$40,                  /* Header Text                */~
            sav_mod$3,                   /* Save Model Code            */~
            sav_width$5,                 /* Save Width                 */~
            sav_modlu$5,                 /* Save Model & Lkup Diameter */~
            model$3,                     /* Model Code                 */~
            modelin$3,                   /* Passed-in Model Code       */~
            modellu$5,                   /* Model & Lkup Diameter      */~
            windcl$(6)6,                 /* Winds/Coil Data            */~
            tubedi$(6)2,                 /* Tube Diameter Codes        */~
            lu$2,                        /* Lookup Diameter Code       */~
            luin$2,                      /* Passed-in Lookup Diam Code */~
            width$5,                     /* Width                      */~
            height$5                     /* Height                     */~


            scr_msg$ = "  Tube Windings & Coils Detail Report   "
            call "SHOSTAT" ("Printing Report for Model(s) " & modelin$)

            x%, pageno% = 0% : lcnt% = 99%
            call "SETPRNT" ("APCC", " ", 0%, 0%)
            select printer (134)
            date$ = date
            call "DATEFMT" (date$)
            init(" ") wrk_rec$, sav_mod$, sav_width$, sav_modlu$
            wrk_key1$ = all(hex(00))
            if modelin$ <> "ALL" then str(wrk_key1$,,3) = str(modelin$)
            read #1, key > wrk_key1$, using L00940, wrk_key1$, wrk_rec$, ~
                                                    eod goto exit_program
            goto L00950
        print_next
            read #1, using L00940  , wrk_key1$, wrk_rec$,                ~
                                                    eod goto exit_program
L00940:       FMT CH(15), CH(49)
L00950:     if modelin$ <> "ALL" and str(wrk_key1$,,3) > modelin$        ~
                then goto exit_program
/*EWD001*/  if luin$ <>  " " and str(wrk_key1$,4%,2%) <> luin$           ~
/*EWD001*/      then print_next
            model$    = str(wrk_key1$,,3%)          /* MODEL       */
            lu$       = str(wrk_key1$,4%,2%)        /* Lookup Diam */
            width$    = str(wrk_key1$,6%,5%)        /* Width       */
            height$   = str(wrk_key1$,11%,5%)       /* Height      */ 

            windcl$() = str(wrk_rec$,1%,36%)        /* Winds/Coils */
            tubedi$() = str(wrk_rec$,37%,12%)       /* Tube Diams  */
            if sav_mod$ <> model$ then gosub lookup_model
            modellu$ = str(model$) & str(lu$)
 
            gosub print_detail

            sav_mod$ = model$
            sav_width$ = width$
            sav_modlu$ = modellu$
            goto print_next

        lookup_model                                  /* Look Up Model */
            readkey$ = all(hex(00))
            readkey$ = "MODEL    " & model$
            call "DESCRIBE" (#3, readkey$, descr$, 0%, x%)
        return


* EWD001 - Added Lamint to Line L01800 Below.
L01800: %! Width: #####  To Height: #####  Normal: ###### Double: ###### ~
        ~ Lamint: ######           ######         ######        ######   ! 

 
L01920: %!                          #####          ######         ###### ~
        ~         ######           ######         ######        ######   !

L01930: %!               Tube Diam:                    ##             ## ~
        ~             ##               ##             ##            ##   !

L01940: %!                                             ##             ## ~
        ~             ##               ##             ##            ##   !

        print_detail
             if lcnt% > 57% then gosub header
             if modellu$ <> sav_modlu$ and lcnt% > 3% then gosub header
             if lcnt% = 3% or width$ <> sav_width$ then                  ~
             print using L01800, width$, height$, windcl$(1), windcl$(2),~
                    windcl$(3), windcl$(4), windcl$(5), windcl$(6)       ~
             else                                                        ~
             print using L01920, height$, windcl$(1), windcl$(2),        ~
                    windcl$(3), windcl$(4), windcl$(5), windcl$(6)       
             if lcnt% = 3% or width$ <> sav_width$ then                  ~
             print using L01930, tubedi$(1), tubedi$(2), tubedi$(3),     ~
                    tubedi$(4), tubedi$(5), tubedi$(6)                   ~
             else                                                        ~
             print using L01940, tubedi$(1), tubedi$(2), tubedi$(3),     ~
                    tubedi$(4), tubedi$(5), tubedi$(6)                   ~

             lcnt% = lcnt% + 2%
       return

L02340: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

L02370: %                    Model No.: ###    ##########################~
        ~####        Lookup Diameter Code: ##
                                                                                
L02400: %  Date: ########                         #######################~
        ~#################                                    Page: ####  

        header
          if lcnt% <> 99% then print using L02340
          pageno% = pageno% + 1%
          print page
          print using L02400, date$, scr_msg$, pageno%
          print using L02370, model$, descr$, lu$
          print using L02340
          lcnt% = 3%
        return


        exit_program
            print using L02340
            call "SETPRNT" ("APCC", " ", 0%, 1%)
        end

