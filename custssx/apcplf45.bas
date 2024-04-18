        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLF45 - Program (APCPLA45)        *~
            *  Creation Date     - 08/20/01 -                           *~
            *  Last Modified Date- 01/01/06                            *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Create BILCO Glass Bridge File with  *~
            *                      Batches for processing Special       *~
            *                      glass entered by hand.               *~
            *                                                           *~
            * x$ = bin(35%,1)      STUFF Pound symbol into X$           *~
            *                                                           *~
            *  MOD - One (1) Title and (1) Header Record per batch      *~
            *        Identification Field. Use 'MODEL' as the           *~
            *        Identifier instead of 'LOAD'.                      *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *      - Bridge File Names                                  *~
            *        (@SHAPE2@) - Batch Created Manually        (AWD001)*~
            *                                                           *~                                  
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/20/01 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/18/03 ! (AWD001) Mods to turn on all Shape Eq.   ! RHH *~
            * 06/07/04 ! (AWD002) Mod for Remake Glass            ! RHH *~
            * 01/01/06 ! (PAR000) New Sub Part No-No Changes      ! RHH *~
            *************************************************************

                                          /* ( Bilco Glass Cutter )    */
        sub "APCPLF45" (size%,            /* Specified Batch Size      */~
                        scr_sel$,         /* Screen Selection          */~
                        scr_dte$,         /* Planned Production Date   */~
                        file$,            /* Name of Optimized File    */~
                        bat$,             /* Number of Batches Created */~
                        bil$(),           /* Window Cut Instructions   */~
                        sh_max%,          /* Number of Windows         */~ 
                        #1,               /* (GENCODES) TABLES         */~
                        #2)               /* (@SHAPE2@)In House(AWD001)*/
                                          /* (EWD031) -                */

        dim scr_sel$1,                   /* Screen Selection          */ ~
            scr_dte$8, bat$3,            /* Glass Production Date     */ ~
            bat_rec$165, rhh$3,          /* Batch Record, ROUTE CODE  */ ~
            seq$3,                       /* Item Numbers              */ ~
            x$1,                         /* Store Pound sysbol(EWD010)*/ ~    
            model$3,                     /* Model Product Code        */ ~
            file$20,                     /* Batch File Name           */ ~
            bil$(300%,30%)20,            /* Batch File Data   (AWD001)*/ ~ 
            inc$12,                      /* Batch File Identifier     */ ~
            hdr$40,                      /* ASKUSER Header Text       */ ~
            msg$(3%)79,                  /* ASKUSER Info Text         */ ~
            errormsg$79,                 /* Error Message Text        */ ~
            short_date$8, short_date1$8  /* Force to short date       */


        dim f2%(10%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! GENCODES ! Master System Table File                 *~
            * #2  ! @SHAPE2@ ! Special Shape Bridge File        (AWD001)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            x$ = bin(35%,1)                      /* Store Pound Symbol */

            size% = 0%

            short_date$ = scr_dte$
            if len(short_date$) = 10% then                          ~
               call "DATUFMTC" (short_date$)                        ~
            else                                                    ~
               call "DATUNFMT" (short_date$) 


            call "DATEFMT" (short_date$)
             
            short_date1$ = short_date$
            
                                   
            ff$ = "@SHAPE2@" : ff% = 2%        /* (AWD001)              */  
                                               /* Create Glass Batches  */
            call "OPENFILE" (#ff%, "IO   ", f2%(ff%), rslt$(ff%), axd$ )
            if f2%(ff%) <> 0% then goto L01300
               gosub file_exists
               if comp% <> 16% then goto L01260
                  call "FILEBGON" addr(#ff%)
                  goto L01300

L01260:        close #ff%
               call "OPENFILE" (#ff%,"EXTND",f2%(ff%),rslt$(ff%),axd$ )
               goto L01360

L01300:     str(rslt$(ff%),1%,6%)  = "OUTPTP"
            str(rslt$(ff%),7%,8%)  = "00001000"
            str(rslt$(ff%),15%,3%) = "100"
            str(rslt$(ff%),18%,3%) = "100"
            call "OPENFILE" (#ff%,"OUTPT",f2%(ff%), rslt$(ff%), axd$ )

L01360:     
            
            bat_no% = 1%

            call "SHOSTAT" ("Creating Batch(es)-"& file$ )

            gosub build_title

            gosub build_header

            for kk% = 1% to sh_max%

               gosub build_detail

            next kk%

            gosub build_end
            close #ff%
            convert (bat_no% - 1%) to bat$, pic(000)

        goto exit_program       


        build_title                            /* New Title each Batch */
          init(" ") bat_rec$
          errormsg$="(Error)-(Title) Rec in Batch- "& file$
          inc$ = " BATCH (XXX)"
          convert bat_no% to str(inc$,9%,3%), pic(000)

          str(bat_rec$,1%,2%) = "NT"                      /* New Title */
          str(bat_rec$,3%,32%)= file$ & inc$
          str(bat_rec$,35%,131%) = " "                    /* Line Feed */
          write #ff%, bat_rec$, eod goto L03020
        return

        build_header                           /* New Header each Load */
          init(" ") bat_rec$
          errormsg$="(Error)-(Header) Rec in Batch- "& file$
          str(bat_rec$,1%,2%)   = "NH"                 /* New Header   */
                                                       /* Model Code   */
          str(bat_rec$,3%,10%)  = "Spec Shape"
          str(bat_rec$,13%,18%) = " "
          str(bat_rec$,31%,8%)  = short_date1$         /* Prod Date    */
          str(bat_rec$,39%,8%)  = Short_date1$         /* Gls Prod Date*/
          str(bat_rec$,47%,119%)= " "                  /* Line Feed    */
          write #ff%, bat_rec$, eod goto L03020
        return

        build_detail
          init(" ") bat_rec$, seq$
          errormsg$ ="(Error)-(Item) Rec in Batch- "& file$

          convert kk% to seq$, pic(000)

          str(bat_rec$,1%,2%)   = "NI"                    /* New Detail*/
          model$ = str(bil$(kk%,1%),1%,3%)                /* Model Code*/
          str(bat_rec$,3%,10%)  = "Spec Shape"            /* Same as Hdr*/
          str(bat_rec$,13%,3%)  = seq$                    /* Seq Number*/
          str(bat_rec$,16%,4%)  = str(bil$(kk%,5%),1%,4%) /* Unit Qty  */
          str(bat_rec$,20%,9%)  = str(bil$(kk%,7%),1%,9%) /* Base      */
                                                          /*(AWD001)   */
          if str(bil$(kk%,27%),1%,1%) = "N" then                         ~
             str(bat_rec$,20%,9%) = "         " 

          str(bat_rec$,29%,9%)  = str(bil$(kk%,9%),1%,9%) /* Left      */
                                                          /*(AWD001)   */
          if str(bil$(kk%,27%),2%,1%) = "N" then                         ~
             str(bat_rec$,29%,9%) = "         " 
                                                          /* (AWD002)  */
         rhh% = 0%
         convert str(bil$(kk%,2%),1%,3%) to rhh%, data goto RH_1
RH_1:
         convert rhh% to rhh$, pic(###)

        REM  str(bat_rec$,38%,3%)  = str(bil$(kk%,2%),1%,3%) /* Shape Code*/

        str(bat_rec$,38%,3%)  = rhh$                      /* Shape Code*/
                                                          /* (AWD002)  */
          str(bat_rec$,41%,9%)  = str(bil$(kk%,11%),1%,9%)/* Right     */
                                                          /*(AWD001)   */
          if str(bil$(kk%,27%),3%,1%) = "N" then                         ~
             str(bat_rec$,41%,9%) = "         " 

          str(bat_rec$,50%,9%)  = str(bil$(kk%,13%),1%,9%)/* Top       */
                                                          /*(AWD001)   */
          if str(bil$(kk%,27%),4%,1%) = "N" then                         ~
             str(bat_rec$,50%,9%) = "         " 

          str(bat_rec$,59%,9%)  = str(bil$(kk%,15%),1%,9%)/* S1        */
                                                          /*(AWD001)   */
          if str(bil$(kk%,27%),5%,1%) = "N" then                         ~
             str(bat_rec$,59%,9%) = "         " 

          str(bat_rec$,68%,9%)  = str(bil$(kk%,17%),1%,9%)/* S2        */
                                                          /*(AWD001)   */
          if str(bil$(kk%,27%),6%,1%) = "N" then                         ~
             str(bat_rec$,68%,9%) = "         " 

          str(bat_rec$,77%,9%)  = "         "
          str(bat_rec$,86%,9%)  = "         "   
          str(bat_rec$,95%,18%) = "@" & seq$ & "              "                 
                                                          /* Label Info*/
          str(bat_rec$,113%,4%) = "****"                  /* Reserved  */
                                                          /* Set to N  */
          str(bat_rec$,117%,8%) = "        "              /* Edgework  */ 

REM          str(bat_rec$,125%,11%)= str(bil$(kk%,21%),1%,11%)/* Sandwich */
REM          str(bat_rec$,136%,30%) = "  "
          str(bat_rec$,125%,20%)= str(bil$(kk%,21%),1%,20%)/* Sandwich */
          str(bat_rec$,145%,21%) = "  "

          write #ff%, bat_rec$, eod goto L03020
        return

        build_end
          init(" ") bat_rec$
          errormsg$ = "(Error)-(End) Rec in Batch- "& file$
          str(bat_rec$,1%,3%) = "END"                /* END OF BATCH    */
          str(bat_rec$,4%,162%) = " "                /* LINE FEED       */
          write #ff%, bat_rec$, eod goto L03020
          bat_no% = bat_no% + 1%
        return
L03020:   gosub error_prompt
        return

        file_exists
            comp% = 2%
            hdr$ = "** Optimization File Exists **"
            msg$(1%) = "The File (XXXXXXXX) Already Exists. "
            str(msg$(1%),11%,8%) = ff$
            msg$(2%) = "             O P T I M I Z A T I O N             "
            msg$(3%) = "Press <RETURN> To Continue, or PF(16) to Delete. "
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        exit_program

        end

                              
