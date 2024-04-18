        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLB44                             *~
            *  Creation Date     - 10/04/96                             *~
            *  Last Modified Date- 02/08/06                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Inventory Update Utility             *~
            *                                                           *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/06/96 ! Mods to Switch (APCTRACK) to New Planning! RHH *~
            *          !   System.                                !     *~
            * 01/17/97 ! Additional Mods for New Planning         ! RHH *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 02/08/06 ! (PAR000) mod for subpart CR347           ! CMG *~
            *************************************************************
* PAR000 add arguments so can update by load or salesorder

            sub "APCPLB44" (caller$, load_no$, sales_order$,  #2)

        dim                              /* File - (INVADDTF)          */~
            jnlid$3,                     /* Journal Id                 */~
            inv_userid$3,                /* User Id                    */~
            inv_seq$3,                   /* Buffer Seq. Number         */~
/*PAR000*/  inv_part$45,                 /* Part Number                */~
            inv_store$3,                 /* Store Number               */~
            inv_lot$16,                  /* Lot Number (Blank)         */~
            inv_job$8,                   /* Job Number (Blank)         */~
            inv_qty$10,                  /* Quantity to ADD to On-Hand */~
            inv_asset$9,                 /* G/L Asset Account          */~
            inv_source$9,                /* G/L Source Account         */~
            inv_cost(12%),               /*                            */~
            a_bom(12%),                  /*                            */~
            a_dtl(12%),                  /*                            */~ 
            a_rte(12%),                  /*                            */~ 
            inv_cost$96,                 /* 12 Cost Buckets            */~
            inv_vendor$9,                /* Vendor Code (Blank)        */~
            inv_desc$32,                 /* Description of Entry       */~
            inv_po$16,                   /* P.O. Number (Blank)        */~
            inv_filler$40,               /* Filler Area                */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            userid$3                     /* Current User Id            */

        dim                              /* FILE = APCLINES            */~
/*PAR000*/  jb_part$45                   /* Part NUmber                */

        dim                              /* FILE = APCPLNSC            */~
            sc_key1$27, sc_inv$1,        /* PRIMARY KEY - APCPLNSC     */~
            sc_rec$128,                  /* RECORD FORMAT              */~
/*PAR000*/  sc_part$45,                  /* PART NUMBER                */~
/*PAR000*/  pp$(900%)45,                 /* MFG Part Numbers           */~
            pp_qty%(900%)                /* Save Quantities            */

        dim f2%(64%),                    /* = 0 if the file is open    */~
            f1%(64%),                    /* = 1 if READ was successful */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64%)20                 /* Text from file opening     */

        dim caller$1,                    /* PAR000 by load or so?      */~
            sales_order$,                /* PAR000                     */~
            sc_key$10                    /* PAR000 sc key 0            */

        dim                              /* PAR000                     */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4             /* Additional Info Fields     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 03/10/98 - Current release            "
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
            * #1  ! INVADDTF ! Additions Buffer for Inventory     PAR000*~
            * #2  ! APCPLNSC ! Replace (APCLINES)                       *~
            * #3  ! GENCODES ! Master System Code Tables                *~
            * #4  ! INVMASTR ! Master Inventory File     PAR000         *~
            * #5  ! INVQUAN  ! Inventory Store Quantity File   PAR000   *~
            * #6  ! SYSFILE2 ! SYSTEM CONTROL FILE                      *~
            * #63 ! BCKSUBPT ! Sub part number file     PAR000          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select  #1, "INVADDTF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 6


            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

* PAR000
            select #4,  "INVMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 1024,                                 ~
                         keypos = 1, keylen = 45,                        ~
                         alternate key 1, keypos = 122, keylen = 9, dup, ~
                                   key 2, keypos = 110, keylen = 4, dup, ~
                                   key 3, keypos = 46, keylen = 32, dup

            select #5,  "INVQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   768,                                 ~
                        keypos =   17, keylen =  64,                     ~
                        alt key  1, keypos =    1, keylen =  64

            select  #6, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20


            select #63, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~ 
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup 

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),100%, rslt$(1%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),  0%, rslt$(6%))
            call "OPENCHCK" (#63, fs%(63%), f2%(63%),  0%, rslt$(63%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            inv_userid$  = userid$
            inv_store$   = "300"
            inv_lot$     = " "
            inv_job$     = " "
            inv_qty$     = " "
            inv_asset$   = "1330-313"
            inv_source$  = "1320-313"
            inv_cost$    = " "
            inv_vendor$  = " "
            inv_desc$    = " "
            inv_po$      = " "
            inv_filler$  = " "

            call "GLUNFMT" (inv_asset$)
            call "GLUNFMT" (inv_source$)

        REM *************************************************************~
            *             S P E C I A L   P R O C E S S I N G           *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

            gosub process_update
            jnlid$ = "IAD"
REM            call "APCPOST" (jnlid$)
* PAR000
            call "PUTPARM" addr("E",/* Enter or Display               */~
                "JNLID   ",         /* PRNAME                         */~
                1%,                 /* Number of Fields this PRNAME   */~
                    "JNLID   ",     /* 1st Field Name                 */~
                    "IAD",          /* Field Value                    */~
                    3%,             /* Length of Field Value          */~
                                    /* Repeat fields & PRNAMES as need*/~
                return%)            /* Placeholder                    */

            return%, comp% = 0%

            call "PROCLINK" ("INVADDJN", " ", " ", return%, comp%)



            goto exit_program

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        REM DATALOAD
        REM RETURN

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT

        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35040: FMT                 /* FILE: INVADDTF                          */~
            CH(3),          /* user-id of specific user                */~
            BI(3),          /* seq. no. for additions buffer           */~
            CH(3),          /* item sequence number                    */~
/*PAR000*/  CH(45),         /* Part Number                             */~
            CH(3),          /* Store or Warehouse Code                 */~
            CH(16),         /* Lot Number                              */~
            CH(8),          /* Job Number                              */~
            PD(14,4),       /* detail to quantity on-hand              */~
            CH(9),          /* Inventory Asset Account                 */~
            CH(9),          /* inventory source account                */~
            CH(96),         /* costs                                   */~
            CH(9),          /* Vendor Code                             */~
            CH(32),         /* description of purpose                  */~
            CH(16),         /* Purchase Order Number                   */~
            CH(40)          /* Filler (Internal, unused space)         */~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        process_update
* PAR000
            if caller$ = "1" then gosub find_sales_orders
            if caller$ = "2" then gosub lookup_sales_orders


            call "SHOSTAT" ("Processing Manufactured Parts")
            inv_index%, inv_seq% = 0%
            if pp_max% = 0% then goto process_done
            for i% = 1% to pp_max%
               jb_part$   = pp$(i%)
               jb_qty     = pp_qty%(i%)
               gosub check_invmastr
               if rec% = 0% then goto L60290               /* SKIP PART */
               gosub check_cost
               inv_index% = inv_index% + 1%
               inv_seq%   = inv_seq%
               convert inv_seq% to inv_seq$, pic(000)

               inv_part$  = jb_part$
               inv_qty    = jb_qty
               inv_desc$  = "INV ADD'S FOR LOAD-" & load_no$
               inv_qty = round(inv_qty, 2)
               write #1, using L35040,                                    ~
                      inv_userid$, inv_index%, inv_seq$, inv_part$,      ~
                      inv_store$, inv_lot$, inv_job$, inv_qty,           ~
                      inv_asset$, inv_source$, inv_cost$, inv_vendor$,   ~
                      inv_desc$, inv_po$, inv_filler$, eod goto L60290

L60290:     next i%
        process_done
        return

        check_invmastr
            rec% = 0%
REM            call "SHOSTAT" (" JB PART -->   " & jb_part$)  stop


            read #4,key = jb_part$, eod goto L60370
            rec% = 1%
L60370: return

        check_cost
            mat inv_cost = zer : totlcost = 0
            mat a_bom    = zer
            mat a_rte    = zer
            mat a_dtl    = zer
* PAR000
            call "STCCOSTS" (str(jb_part$,1%,25%), " ", #6, 2%, totlcost,  ~
                          inv_cost(), a_bom(), a_rte(), a_dtl() )
            put inv_cost$ using L60430, inv_cost()
L60430:             FMT 12*PD(14,4)
        return

        find_sales_orders
            init(" ") pp$(), sc_key1$, sc_inv$
            mat pp_qty% = zer
            pp_max% = 0%
            call "SHOSTAT" ("Scanning Load for Sales Orders")

            str(sc_key1$,1%,5%) = load_no$
        find_next
            read #2,hold,key 1% > sc_key1$, using L60560, sc_rec$,        ~
                                                       eod goto find_done
L60560:          FMT CH(128)
            sc_key1$ = str(sc_rec$,7%,27%)
            if str(sc_key1$,1%,5%) <> load_no$ then goto find_done
               if str(sc_rec$,104%,1%) = "Y" then goto find_next
                  get #2, using L60690, sc_part$, sc_mqty%, sc_inv$
L60690:             FMT POS(34), CH(25), POS(70), BI(2), POS(104), CH(1)
                  if sc_inv$ = "Y" then goto find_next
                  if sc_mqty% = 0% then goto L60800

* PAR000 go get subpart     
                  init(" ") so_inv$, item_no$              
                  so_inv$  = str(sc_rec$,24%,8%)
                  item_no$ = str(sc_rec$,32%,2%)
                  gosub lookup_subpart
                  str(sc_part$,26%,20%) = str(bcksubpt_rec$,48%,20%)


                  if pp_max% = 0% then goto L60750

                  for i% = 1% to pp_max%
                      if pp$(i%) = sc_part$ then goto L60780
                  next i%
L60750:           pp_max% = pp_max% + 1%
                  i% = pp_max%
                  pp$(i%) = sc_part$ 
L60780:           pp_qty%(i%) = pp_qty%(i%) + sc_mqty%

L60800:           put #2, using L60810, "Y"
L60810:             FMT POS(104), CH(1)
                  rewrite #2
               goto find_next
        find_done
        return

* PAR000
        lookup_sales_orders
            init(" ") pp$(), sc_key$, sc_inv$
            mat pp_qty% = zer
            pp_max% = 0%
            call "SHOSTAT" ("Scanning  Sales Order")

            str(sc_key$,1%,8%) = sales_order$
        lookup_next
            read #2,hold,key > sc_key$, using L60560, sc_rec$,        ~
                                                       eod goto lookup_done

            sc_key$ = str(sc_rec$,24%,10%)
            if str(sc_key$,1%,8%) <> sales_order$ then goto lookup_done
               if str(sc_rec$,104%,1%) = "Y" then goto lookup_next

                  get #2, using L60690, sc_part$, sc_mqty%, sc_inv$

                  if sc_inv$ = "Y" then goto lookup_next
                  if sc_mqty% = 0% then goto L60805

* PAR000 go get subpart   
                  init(" ") so_inv$, item_no$                
                  so_inv$  = str(sc_rec$,24%,8%)
                  item_no$ = str(sc_rec$,32%,2%)
                  gosub lookup_subpart
                  str(sc_part$,26%,20%) = str(bcksubpt_rec$,48%,20%)


                  if pp_max% = 0% then goto L60755

                  for i% = 1% to pp_max%
                      if pp$(i%) = sc_part$ then goto L60785
                  next i%
L60755:           pp_max% = pp_max% + 1%
                  i% = pp_max%
                  pp$(i%) = sc_part$
L60785:           pp_qty%(i%) = pp_qty%(i%) + sc_mqty%

L60805:           put #2, using L60810, "Y"

                  rewrite #2
               goto lookup_next
        lookup_done
        return



        lookup_subpart                            /* PAR000  */
            init(" ") bcksubpt_rec$, flds$(), info_flds$()
            flag$ = "0"                  /* Sales Order Info         */
            pgm$  = "1" 


            convert so_inv$ to so_inv%, data goto convert_alpha

            convert so_inv% to so_inv$, pic(00000000)
         
            goto order_converted

convert_alpha:          
            convert str(so_inv$,2%,7%) to so_inv%, data goto sub_part1
sub_part1:
            convert so_inv% to str(so_inv$,2%,7%), pic(0000000)


order_converted:
            convert item_no$ to item_no%, data goto sub_part2 
sub_part2:

            convert item_no% to item_no$, pic(###)   

        call "AWDBKSUB"   (flag$,        /* Flag 0=SalesOrder 1=Invoice*/~
                          pgm$,          /* Calling Program 0=BCKUPDTE */~
                                         /* 1=Any Other 2=Delete       */~
                                         /* 3=Invoice                  */~
                          so_inv$,       /* SO or Invoice Num to lookup*/~
                          item_no$,      /* Item Number                */~
                          bcksubpt_rec$, /* Record If BCKUPDTE then    */~
                                         /* pass in else pass out      */~
                          flds$(),       /* Part Number Fields         */~
                          info_flds$(),  /* Information Fields         */~
                          #63,           /* BCKSUBPT File              */~
                          err1%)         /* Error Code                 */
           
            if err1% <> 0% then                                          ~
                   str(bcksubpt_rec$,48%,20%) = "00000000000000000000"

        return                                    /* PAR000 */

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*

        exit_program

            end


