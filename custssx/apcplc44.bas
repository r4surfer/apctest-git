        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLC44                             *~
            *  Creation Date     - 10/04/96                             *~
            *  Last Modified Date- 11/14/97                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Staging, Loading, Update Utility     *~
            *                                                           *~
            *                                                           *~
            *  Code Tables Used  - (PLAN SHFT) - Shift Codes            *~
            *                      (PLAN DEPT) - Department Codes       *~
            *                      (PLAN PROC) - Planning Process Codes *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/04/96 ! Mods to Switch (APCTRACK) to New Planning! RHH *~
            *          !   System.                                !     *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            *          !                                          !     *~
            *          !                                          !     *~
            *************************************************************

            sub "APCPLC44" (load$, #3, #4, #5, #6)

        dim                              /* FILE = APCPLNOR            */~
            or_rec$170,                  /* Record Format              */~
            sc_key$10,                   /* Primary Key - APCPLNSC     */~
            sc_rec$128,                  /* Record Format - APCPLNSC   */~
            dt_key3$23,                  /* Alt Key - APCPLNDT         */~
            dt_rec$256,                  /* Record Format - APCPLNDT   */~
            dt_st$2, dt_dept$3,          /* Scanning Status Code       */~
            save_code$8,                 /* SALES ORDER                */~
            save_code1$10                /* SALES ORDER AND LINE ITEM  */

        dim                              /* (Program Variables)        */~
            load$5, readkey$24,          /* Load Number to Process     */~
            date$8,                      /* Date for screen display    */~
            userid$3                     /* Current User Id            */

        dim f2%(10%)                     /* = 0 if the file is open    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Update Staging and Loading Data   "
            pname$ = "APCPLC44 - Rev: R6.04"

        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #3  ! APCPLNOR ! Replace (APCORDER)                       *~
            * #4  ! APCPLNSC ! Replace (APCLINES)                       *~
            * #5  ! GENCODES !                                          *~
            * #6  ! APCPLNDT ! Replace (APCDATAC)                       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************


            scr_code% = 1%                          /* Update Staging  */
            dt_st$ = "14"
            gosub update_load_header

            scr_code% = 2%                          /* Update Loading  */
            dt_st$ = "16"
            gosub update_load_header
            goto exit_program

        update_load_header                         /* Scan Data Detail */
            init(" ") save_code$, save_code1$, dt_key3$
            check% = 0% : check1% = 0%             /* APCPLNDT - Driver*/
            str(dt_key3$,1%,5%) = load$            /* (14)- staging    */
                                                   /* (16)- Loading    */
            read #6,key 3% > dt_key3$, using L01120 , dt_rec$,             ~
                                                     eod goto header_done
               save_code$   = str(dt_rec$,24%,8%)        /* Sale Order */
               save_code1$  = str(dt_rec$,24%,10%)       /* S.O./Line  */
            goto L01130
        next_header
            read #6, using L01120 , dt_rec$, eod goto header_done
L01120:        FMT CH(256)
L01130:     if load$ <> str(dt_rec$,1%,5%) then goto header_done
               dt_dept$ = str(dt_rec$,42%,3%)
               gosub check_support
               if supp% = 1% then goto next_header
               if save_code$ = str(dt_rec$,24%,8%) then goto L01210
                  if check% = 0% then gosub update_apcplnor
                     check%       = 0%
                     save_code$   = str(dt_rec$,24%,8%)
L01210:        if save_code1$ = str(dt_rec$,24%,10%) then goto L01250
                  if check1% = 0% then gosub update_apcplnsc
                  check1%     = 0%
                  save_code1$ = str(dt_rec$,24%,10%)
L01250:     if scr_code% <> 1% then goto L01270
               if str(dt_rec$,64%,2%) < "14" then check%, check1% = 1%
L01270:     if scr_code% <> 2% and scr_code% <> 3% then goto L01290
               if str(dt_rec$,64%,2%) < "16" then check%, check1% = 1%
L01290:     goto next_header
        header_done
            if check%  = 0% then gosub update_apcplnor   /* Sales Order*/
            if check1% = 0% then gosub update_apcplnsc   /* Line Items */
        return

        update_apcplnsc                             /* Pass - (2A)      */
           if str(load$,1%,1%) = "S" then return   /* SKIP STOCK LOAD*/
           init(" ") sc_key$, sc_rec$              /* (APCPLNSC) Lines */
           sc_key$ = save_code1$                   /* S.O. / Line Item */
           read #4,hold,key  = sc_key$, using L01410 , sc_rec$,            ~
                                                           eod goto L01470
L01410:       FMT CH(128)
           if str(sc_rec$,110%,2%) >= dt_st$ then goto L01470
              str(sc_rec$,110%,2%) = dt_st$
              str(sc_rec$,112%,6%) = date
              put #4, using L01410 , sc_rec$
              rewrite #4
L01470: return

        update_apcplnor
           if str(load$,1%,1%) = "S" then return     /* SKIP STOCK LOAD*/
           read #3,hold,key = save_code$, using L01530 , or_rec$,          ~
                                                       eod goto L01580
L01530:       FMT CH(170)
           if str(or_rec$,60%,2%) >= dt_st$ then goto L01590
              str(or_rec$,60%,2%) = dt_st$
              str(or_rec$,62%,8%) = date
              put #3, using L01530 , or_rec$
L01580:       rewrite #3
L01590: return

        check_support
           supp% = 0%
           init(" ") readkey$
           if dt_dept$ = "102" or dt_dept$ = "104" then                  ~
                                                  goto check_support_done
           str(readkey$,1%,9%)   = "PLAN SUPP"
           str(readkey$,10%,15%) = dt_dept$
           read #5,key = readkey$, eod goto check_support_done
           supp% = 1%
        check_support_done
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
