        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - AWDPLA05                             *~
            *  Creation Date     - 04/03/03                             *~
            *  Last Modified Date- 10/19/2016                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Modifications By  - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Write records to a file to print a   *~
            *                      new shipping label.                  *~
            *                                                           *~
            *                      Print File  = MFGSTAG,MFGSHIP,MFGTEST*~
            *                      Script File = MFGSTAG,MFGSHIP,MFGTEST*~
            *                                   (Print File and Script) *~
            *                      switch% = 1%    MFGSTAG NESTAG   'A' *~
            *                      switch% = 2%    MFGSHIP NESHIP   'B' *~
            *                      switch% = 3%    MFGTEST NETEST   'T' *~
            *                      switch% = 4%    MFGRGA  NERGA    'C' *~
            *                      switch% = 5%    MFGUPS  NEUPS    'D' *~
            *                      switch% = 6%    MFGNEA  MFGNEA   'Z' *~
            *                      switch% = 7%    MFGFLB  NEFLB    'E' *~
            *                      switch% = 8%    MFGUPS2          'F' *~
            *                      switch% = 9%    NECROSD          'G' *~
            *                      switch% = 10%   MFGDCZA          'H' *~
            *                      switch% = 11%   MFGDCZB          'I' *~
            *                      switch% = 12%   MFGDCZC          'J' *~
            *                      switch% = 13%   NESTGK           'K' *~
            *                      switch% = 14%   NESTGL           'L' *~
            *                      switch% = 15%   NESTGM           'M' *~
            *                      switch% = 16%   NESTGN           'N' *~
            *                      switch% = 17%   NESTGP           'P' *~
            *                      switch% = 18%   NESTGQ           'Q' *~
            *                      switch% = 19%   NESTGR           'R' *~
            *                      switch% = 20%   NESTGS           'S' *~
            *                      switch% = 21%   NESTGU           'U' *~
            *                      switch% = 22%   MFG300V          'V' *~
            *        CR2493        switch% = 23%   MFPAINT          'W' *~
            *-----------------------------------------------------------*~
            * AWDPLA05 - Generates the label format and data to print   *~
            *            Shipping labels. The resulting file is routed  *~
            *            to the label printer via a script.             *~
            *                                                           *~
            *          - ERROR% = 0% - Lbl Ok and Sent                  *~
            *                     1% - Could Not Open Lbl Print File    *~
            *                     4% - Shell Script Error               *~
            *                     5% - Print Error Creating Label       *~
            *                     6% - Could Not Reset Table Flag       *~
            *                     7% - Label Data Format Error          *~
            *                     8% - No Data for Label                *~
            *                     9% - Schema Lookup Erro               *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/16/03 ! Original - Copied & Mod (sub) AWDPLA04.  ! RHH *~
            * 06/30/03 ! (AWD001) Label Modifications             ! RHH *~
            * 08/20/03 ! (AWD002) Mods for Non Appian Labels      ! RHH *~
            * 06/22/04 ! (AWD003) Mod to Change the Size and the  ! RHH *~
            *          !    location of the production Seq. on the!     *~
            *          !    label. White on Black, lower left side!     *~
            *          !    in Box.                               !     *~
            * 06/30/04 ! (AWD004) Mod to add switch flag = 3% for ! RHH *~
            *          !    testing shipping labels.              !     *~
            * 07/16/04 ! (AWD005) Mo to increase the size of the  ! RHH *~
            *          !    drop number.                          !     *~
            * 11/18/04 ! (AWD006) Correct the printing of the     ! RHH *~
            *          !    Opening Size. Was not printing Fract. !     *~
            * 04/04/05 ! (AWD007) Change Job Nmae from 10 to 16   ! RHH *~
            * 04/04/05 ! (AWD008) Change to High Lit P.O. Number  ! RHH *~
            * 10/27/05 ! (AWD009) Add S,L,B,U to Loading dock Door! RHH *~
            * 12/28/05 ! (AWD010) Mod for two new printers in     ! RHH *~
            *          !    Shipping.                             !     *~
            * 01/01/06 ! (PAR000) CR347 Sub Part No.              ! RHH *~
            * 03/03/06 ! (PAR001) Mod for New Sub Part Number     ! RHH *~
            *          !    in the Production Label File.         !     *~
            * 03/20/06 ! (PAR002) Mod for printing Shipp labels at! RHH *~
            *          !    North East.                           !     *~
            * 04/20/06 ! (PAR003) Mod to add new Printer code 'Z' ! RHH *~
            *          !    for the North East But Print Here at  !     *~
            *          !    Atrium.                               !     *~
            * 09/25/06 ! (PAR004) Mod for New Job Name and Room   ! RHH *~
            *          !    Location size change to 16 and added  !     *~
            *          !    to 'AWDAPPLS' as single fields.       !     *~
            * 04/20/07 ! (PAR005) Mod to handling longer Rm lbl(68! DES *~
            *08/08/2008! (AWD011) Mod for breaking up drop numbers! DES *~
            *10/29/2009! (AWD012) Mod to add new Printer code 'E' ! DES *~
            *03/23/2015! (AWD013) Mod to add new Printer code 'F' ! PWW *~
            *06/26/2015! SR66608  Mod to move piece of qty and    ! PWW *~
            *          !          Drop Number.                    !     *~
            *10/26/2015! SR70079  Mod to make shipping lane       ! PWW *~
            *          !          Bolder & Bigger.                !     *~
            *10/28/2015! SR70180  Mod to add new CrossDock printer! PWW *~
            *05/03/2016! SR74525  Mod to add run date & time.     ! PWW *~
            *          !                                          !     *~
            *10/19/2016! (CR456) mod for DC Center                ! CMG *~
            *05/15/2017! (CR932) mod to add BP back on label      ! CMN *~  
            *07/10/2017! CR1014 New printers for staging in NTX   ! RDB *~  
            *10/18/2017! CR1174 Add new shipment block            ! RDB *~   
            *10/26/2017! SR82456 Fix position of # of total field ! RDB *~   
            *06/18/2018! CR1567 Convert DC scripts to H, I, J     ! RDB *~  
            *12/10/2018! CR1810 Add printers Q,R,S,U for TX eol   ! RDB *~  
            *02/14/2019! CR1918 Use PlyGem PO on label when found ! RDB *~    
            *07/19/2019! CR2130 Add new building 300 shipping prt ! RDB *~ 
            *04/07/2020! CR2493 Add new paint receive printer     ! RDB *~   
            *04/12/2021! CR2815 Move date & time up on label      ! RDB *~     
            *10/14/2021! CR2930 Get JOB field from Bckmastr       ! RDB *~            
            *************************************************************
      
        sub "AWDPLA05" (switch%,         /* 0% thru 23                 */~
                        been_here%,      /* Zero (Only 1st Time)       */~
                        rec$(),          /* Production Label Data      */~
                        #1,              /* GENCODES                   */~
                        #2,              /* EWDPRDLB           (AWD001)*/~
                        #3,              /* BCKSUBPT           (PAR000)*/~
                        #4,              /* BCKMASTR           (CR1918)*/~
                        error%)          /* Return Code                */
     
        dim                                                              ~
            schema$8,                    /* (PAR002) Schema Switch     */~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            a$90, b$90,                  /* Print Lines for Label      */~
            lbl$(45%)60,                 /* Label Data Array           */~
            hdr$40,                      /* ASKUSER Header Text        */~
            msg$(3%)79,                  /* ASKUSER Info Text          */~
            fs$3,                        /* End of Lbl Records         */~
            file$8,                      /* Lbl Print File             */~
            library$8,                   /* Library Name = APCDATA     */~
            script$8,                    /* Lbl SHELL SCRIPT           */~
            volume$6,                    /* DISK VOLUME = CARLOS       */~
            rec$(4%)256                  /* Shipping Label Rec (PAR004)*/

        dim lb_wood_part$1,              /* (W) or (P)                 */~
            yy$(150%)90,                 /* Buffer                     */~
            xx$(150%)90,                 /* Buffer                     */~
            lb_wood$6,                   /* Wood Surr. Fab. Code       */~
            lb_part$25,                  /* MFG Part Number            */~
            lb_sub_part$20,              /* New Sub Part       (PAR000)*/~
            lb_sub_info$10,              /* New Info Fields    (PAR001)*/~
            lb_sub_descr$40,             /* Ne Descrip Sub Part(PAR001)*/~
            how1$10,                     /* Product type               */~
            how2$10,                     /* Product type               */~
            how3$10,                     /* Product Type               */~
            lb_var$250,                  /* Product Description        */~
            lb_var1$30,                  /* Description                */~
            lb_var2$55,                  /* Description                */~
            lb_var3$55,                  /* Description                */~
            lb_var4$55,                  /* Description                */~
            lb_var5$55,                  /* Description                */~
            ap_howship$2,                /* Save How Ship Code         */~
            lb_zip$10,                   /* Formated Zip Code  (AWD001)*/~
            lb_contract$16,              /* Contractor Name    (AWD001)*/~
            lb_job$10, lb_job1$6,        /* Customer Job Name (AWD007)*/~
            lb_room$6,                   /* Room Location      (AWD001)*/~
            lb_job_new$16,               /* New Job Name       (PAR004)*/~
            lb_room_new$16,              /* New Room Location  (PAR004)*/~
            lb_nominal$6,                /* Nominal Window Size(AWD001)*/~
            lb_line$2,                   /* Caelus Line No.    (AWD001)*/~
            lb_bar$18,                   /* Bar Code           (AWD001)*/~
            lb_qty_of$10,                /* S.O. Piece Qty     (AWD001)*/~
            lb_key1$23,                  /* Production Label Ky(AWD001)*/~
            lb_itmn$3,                   /* S.O Item Piece     (AWD001)*/~
            lb_itmtot$3,                 /* S.O. Piece Total   (AWD001)*/~
            lb_open$19,                  /* Opening Size       (AWD001)*/~
            lb_exact$19,                 /* Exact Size         (AWD001)*/~
            lb_label$1,                  /* Appian Label(YorN) (AWD002)*/~
            lb_shp_blk$3,                /* Shipment Block CR1174      */~
            work_var2$10,                /* Use for date conversion    */~
            s_key$25,                    /* Bckmastr key               */~
            bck_job$20                   /* Bckmastr job field CR2930  */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            axd$4,                       /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        dim bck_key$11, bck_rec$256,     /* BCKSUBT for AWD011 */        ~
/*SR74525*/ date$8,                      /* Date for screen display    */~
/*SR74525*/ runtime$8,                   /* Run Time for Report        */~
            pg_po$20                     /* PlyGem PO number   CR1918  */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(AWD) Generate Shipping Labels          "
            pname$ = "AWDPLA05 - Rev: R8.00"

        REM *************************************************************
            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #5  ! MFGSHIP  ! Print File For Production Labels         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #5, "ROYAL", varc, consec, recsize =   90

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
          
            sel% = error%
            error%     = 0%
            nbr_lines% = 0%
            fs$ = "^FS"
/*SR74525 + */
            date$ = date
            call "DATEFMT" (date$)
/*SR74525 - */


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
            init(" ") xx$()
            if been_here% > 0% then goto L01000
               d1 = 2
               d2 = 1
               gosub load_label           /* Build Label Format yy$() */
               gosub set_file_name        /* Create Label Print File  */
               gosub open_file            /* Open Label Print file    */

L01000:        if sel% = 99% then goto exit_print
                  been_here% = been_here% + 1%
                  copy yy$() to xx$()
                  gosub begin_process

                  goto exit_sub

        set_file_name                    /* (AWD004)                     */
            init(" ") file$, script$
                                         /* (PAR002)                     */
            err%    = 0%
            call "SCHEMA" (schema$,      /* What switch 1-NC 2-NE        */~
                           schema%,      /* Schema                       */~
                           #1,           /* GENCODES                     */~
                           err% )        /* error                        */

            if err% = 0% then goto SS_1
               error% = 9% 
               end

SS_1:                                    /* (PAR002)                     */
                                         /* (PAR003)                     */
            on switch% gosub L01010,     /* 'MFGSTAG' Staging Printer 'A'*/~
                             L01020,     /* 'MFGSHIP' Staging Printer 'B'*/~
                             L01030,     /* 'MFGTEST' Label Test Print'T'*/~
                             L01040,     /* 'MFGRGA'  Staging Printer 'C'*/~
                             L01050,     /* 'MFGUPS'  Staging Printer 'D'*/~
                             L01060,     /* 'MFGNEA'  Staging Prt NE  'Z'*/~
                             L01070,     /* 'MFGNEA'  Staging Prt NE  'Z'*/~
/*AWD013*/                   L01080,     /* 'MFGUPS2' UPS Prt NC      'F'*/~
/*SR70180*/                  L01090,     /* 'MFGNECD' Cross Dock      'G'*/~
/*(CR456)*/                  L01150,     /* 'MFGDCZH' Cross Dock      'H'*/~
/*(CR456)*/                  L01170,     /* 'MFGDCZI' Cross Dock      'I'*/~
/*(CR456)*/                  L01250,     /* 'MFGDCZJ' Cross Dock      'J'*/~
/*(CR1014)*/                 L01260,     /* 'NESTGK' NE Staging Prt   'K'*/~
/*(CR1014)*/                 L01261,     /* 'NESTGL' NE Staging Prt   'L'*/~
/*(CR1014)*/                 L01262,     /* 'NESTGM' NE Staging Prt   'M'*/~
/*(CR1014)*/                 L01263,     /* 'NESTGN' NE Staging Prt   'N'*/~
/*(CR1014)*/                 L01264,     /* 'NESTGP' NE Staging Prt   'P'*/~
/*(CR1810)*/                 L01265,     /* 'NESTGQ' NE Staging Prt   'Q'*/~
/*(CR1810)*/                 L01266,     /* 'NESTGR' NE Staging Prt   'R'*/~
/*(CR1810)*/                 L01267,     /* 'NESTGS' NE Staging Prt   'S'*/~
/*(CR1810)*/                 L01268,     /* 'NESTGU' NE Staging Prt   'U'*/~
/* CR2130 */                 L01269,     /* 'MFG300V NC Building 300  'V'*/~
/* CR2493 */                 L01310      /* 'MFPAINT' Paint Recv Ptr  'W'*/
       
                                         /* (PAR003)                     */
        return

L01010:                                         /* 'MFGSTAG'-1%*/
        if schema% <> 1% then goto L01015
            file$   = "MFGSTAG"
            script$ = "MFGSTAG"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return
L01015:
            file$   = "NESTAG"
            script$ = "NESTAG"
            library$= "NEDATA  "
            volume$ = "NE    "
        return

L01020:                                         /* 'MFGSHIP'-2%*/
        if schema% <> 1% then goto L01025
            file$   = "MFGSHIP"
            script$ = "MFGSHIP"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return
L01025:
            file$   = "NESHIP"
            script$ = "NESHIP"
            library$= "NEDATA  "
            volume$ = "NE    "

        return

L01030:                                         /* 'MFGTEST'-3%*/
        if schema% <> 1% then goto L01035
            file$   = "MFGTEST"
            script$ = "MFGTEST"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return 
L01035:
            file$   = "NETEST"
            script$ = "NETEST"
            library$= "NEDATA  "
            volume$ = "NE    "
        return
L01040:                                         /* 'MFGSHIP'-4%*/
        if schema% <> 1% then goto L01045
            file$   = "MFGRGA"
            script$ = "MFGRGA"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return
L01045:
            file$   = "NERGA"
            script$ = "NERGA"
            library$= "NEDATA  "
            volume$ = "NE    "
        return

L01050:                                         /* 'MFGUPS' -5%*/
        if schema% <> 1% then goto L01055
            file$   = "MFGUPS"
            script$ = "MFGUPS"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return
L01055:
            file$   = "NEUPS"
            script$ = "NEUPS"
            library$= "NEDATA  "
            volume$ = "NE    "
        return
                                                /* (PAR003)   */
L01060:                                         /* 'MFGNEA'-6%*/
        if schema% <> 1% then goto L01065
            file$   = "NEASTAG"
            script$ = "NEASTAG"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return
L01065:
            file$   = "MFGNEA"                  /* NE but Print at Atrium */
            script$ = "MFGNEA"
            library$= "NEDATA  "
            volume$ = "NE    "
        return

L01070:                                         /* 'MFGNEA'-6%*/
        if schema% <> 1% then goto L01075
            file$   = "MFGFLB"
            script$ = "MFGFLB"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return
L01075:
            file$   = "NEFLB"                  /* NE but Print at Atrium */
            script$ = "NEFLB"
            library$= "NEDATA  "
            volume$ = "NE    "
        return

L01080:                /* AWD013                   'MFGUPS2'-8%*/
REM     if schema% <> 1% then goto L01075
            file$   = "MFGUPS2"
            script$ = "MFGUPS2"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return

L01090:                /* SR70180                  'MFGNECD'-9%*/
REM     if schema% <> 1% then goto L01075
            file$   = "NECROSD"
            script$ = "NECROSD"
            library$= "NEDATA  "
            volume$ = "NE    "
        return
/* (CR456) + */
L01150:
        if schema% <> 1% then return           /* switch% = 10% */
            file$   = "MFGDCZH"
            script$ = "MFGDCZH"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return


L01170:
        if schema% <> 1% then return           /* switch% = 11% */
            file$   = "MFGDCZI"
            script$ = "MFGDCZI"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return

L01250:
        if schema% <> 1% then return           /* switch% = 12% */
            file$   = "MFGDCZJ"
            script$ = "MFGDCZJ"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return
/* (CR456) - */
/* CR1014    */
               
L01260:
        if schema% <> 2% then return           /* switch% = 13% */
            file$   = "NESTGK"                 
            script$ = "NESTGK"
            library$= "NEDATA  "
            volume$ = "NE    "
        return
               
L01261:
        if schema% <> 2% then return           /* switch% = 14% */
            file$   = "NESTGL"                 
            script$ = "NESTGL"
            library$= "NEDATA  "
            volume$ = "NE    "
        return
               
L01262:
        if schema% <> 2% then return           /* switch% = 15% */
            file$   = "NESTGM"                 
            script$ = "NESTGM"
            library$= "NEDATA  "
            volume$ = "NE    "
        return
               
L01263:
        if schema% <> 2% then return           /* switch% = 16% */
            file$   = "NESTGN"                 
            script$ = "NESTGN"
            library$= "NEDATA  "
            volume$ = "NE    "
        return
               
L01264:
        if schema% <> 2% then return           /* switch% = 17% */
            file$   = "NESTGP"                 
            script$ = "NESTGP"
            library$= "NEDATA  "
            volume$ = "NE    "
        return        
        
/* CR1014    */        
/* CR1810    */
L01265:
        if schema% <> 2% then return           /* switch% = 18% */
            file$   = "NESTGQ"                 
            script$ = "NESTGQ"
            library$= "NEDATA  "
            volume$ = "NE    "
        return        
  
L01266:
        if schema% <> 2% then return           /* switch% = 19% */
            file$   = "NESTGR"                 
            script$ = "NESTGR"
            library$= "NEDATA  "
            volume$ = "NE    "
        return  
        
L01267:
        if schema% <> 2% then return           /* switch% = 20% */
            file$   = "NESTGS"                 
            script$ = "NESTGS"
            library$= "NEDATA  "
            volume$ = "NE    "
        return  
L01268:
        if schema% <> 2% then return           /* switch% = 21% */
            file$   = "NESTGU"                 
            script$ = "NESTGU"
            library$= "NEDATA  "
            volume$ = "NE    "
        return  
L01269:
        if schema% <> 1% then return           /* switch% = 22% */
            file$   = "MFG300V"
            script$ = "MFG300V"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return          
/* CR1810    */   
/* CR2493 */
L01310:
        if schema% <> 1% then return           /* switch% = 23% */
            file$   = "MFPAINT"
            script$ = "MFPAINT"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return 
                                                /* (PAR003)    */
                                                /* (PAR001)    */
                                                /* (AWD010)    */
                                                /* (AWD004)    */
        open_file
                                                /* (PAR002)    */
            call "OPENFILE" (#5, "IO   ", f2%(5%), rslt$(5%), axd$ )
            if f2%(5%) <> 0% then goto L01100
               gosub file_exists
               if comp% <> 16% then goto exit_sub
                  call "FILEBGON" (#5)

L01100:    open nodisplay #5, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return


        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *************************************************************
        begin_process
                                          /* Load data from (AWDAPPLB) */
            init (" ") lbl$(), work_var1$, work_var2$, lb_var$, lb_var1$, ~
                       lb_var2$, lb_var3$, lb_var4$, lb_var5$, lb_zip$,   ~
                       lb_line$, lb_qty_of$, lb_job$, lb_job1$, lb_room$
                                          /* Complete Tests            */
            gosub check_wood_part
            gosub check_howship
                                          /* (W) or (P)                */
            lbl$(01%) = lb_wood_part$         & fs$
                                          /* Appian Load No.           */
            lbl$(02%) = str(rec$(),23%,5%)    & fs$
                                   /* Appian Drop No.           */
/*          lbl$(39%) = str(rec$(),1016%,8%)  & fs$        SR66608 */
/*SR66608   lbl$(39%) = str(rec$(),1022%,1%)  & fs$      SR82456 */
            lbl$(03%) = str(rec$(),19%,2%)    & fs$
                                          /* Customer P.O. Number      */
/* </AWD011> */
            lbl$(04%) = str(rec$(),39%,16%)   & fs$
/* CR1918 */
            pg_po$ = " "
            gosub lookup_pg_po            
            if pg_po$ > " " then lbl$(04%) = pg_po$ & fs$
            
                                          /* Customer Sales Order No.  */
            lbl$(05%) = str(rec$(),56%,8%)    & fs$
                                          /* (AWD003)                  */
                                          /* Production Seq. No.       */
            lbl$(06%) = str(rec$(),74%,5%)    & fs$
                                          /* (AWD003)                  */

                                          /* Ship Date                 */
            work_var1$ = str(rec$(),512%,6%)
            call "DATFMTC" (work_var1$)
            lbl$(07%) = work_var1$            & fs$
                                          /* WW Config Code            */
            lbl$(08%) = str(rec$(),85%,3%)    & fs$
                                          /* Appian Arrival Date       */
            work_var2$ = str(rec$(),88%,6%)
            call "DATFMTC" (work_var2$)
            lbl$(09%) = work_var2$            & fs$
                                          /* Customer Code             */
            lbl$(10%) = str(rec$(),30%,9%)    & fs$
                                          /* Customer Name             */
            lbl$(11%) = str(rec$(),94%,30%)   & fs$
                                          /* Address (1)               */
            lbl$(12%) = str(rec$(),124%,30%)  & fs$
                                          /* Address (2)               */
            lbl$(13%) = str(rec$(),154%,30%)  & fs$
                                          /* Customer City             */
            lbl$(14%) = Str(rec$(),184%,18%)  & fs$
                                          /* Customer State            */
            lbl$(15%) = str(rec$(),202%,2%)   & fs$
                                          /* Customer Zip              */
                                          /* (AWD001)                  */
            lb_zip$ = str(rec$(),204%,5%) & "-" & str(rec$(),209%,4%)
            lbl$(16%) = lb_zip$               & fs$
                                          /* MFG Part Number           */
            lbl$(17%) = lb_part$              & fs$
                                          /* (PAR000)                  */
            lb_sub_part$ = " "
                                          /* (PAR000)                  */

                                          /* Part Long Description     */
            lb_var$  = str(rec$(),238%,250%)
            lb_var1$ = str(lb_var$,1%,30%)
            lb_var2$ = str(lb_var$,31%,55%)
            lb_var3$ = str(lb_var$,86%,55%)
            lb_var4$ = str(lb_var$,141%,55%)
            lb_var5$ = str(lb_var$,196%,55%)
            call "SPCESMSH" (lb_var1$,1%)
            call "SPCESMSH" (lb_var2$,1%)
            call "SPCESMSH" (lb_var3$,1%)
            call "SPCESMSH" (lb_var4$,1%)
            call "SPCESMSH" (lb_var5$,1%)

            lbl$(18%) = "Production Description: " & lb_var1$ & fs$

            lbl$(19%) = lb_var2$ & fs$

            lbl$(20%) = lb_var3$ & fs$

            lbl$(21%) = lb_var4$ & fs$

            lbl$(22%) = lb_var5$ & fs$
                                          /* Production Barcode        */
            lb_bar$   = str(rec$(),1%,18%)
            lbl$(23%) = lb_bar$ & str(rec$(),19%,2%) & fs$
                                          /* Check product Howship     */
                                          /* Use for Sample or Disp    */
            lbl$(24%) = how1$                 & fs$
                                          /* Use for Customer Pick Up  */
            lbl$(25%) = how2$                 & fs$
                                          /* Use for Tech Pick Up      */
            lbl$(26%) = how3$                 & fs$
                                          /* Caelus Load No            */
            lbl$(27%) = str(rec$(),490%,5%)   & fs$
                                          /* Caelus Drop No.           */
            lbl$(28%) = str(rec$(),495%,2%)   & fs$
                                          /* (AWD002)                  */
                                          /* Non Appain Product Labels */
            
            if lb_label$ = "N" then                                     ~
               lbl$(27%) = "NonAP" & fs$
            if lb_label$ = "N" then                                     ~
               lbl$(28%) = "NN"    & fs$
                                          /* (AWD002)                  */
                                          /* Product Series and Style  */
            lbl$(29%) = str(rec$(),66%,8%)    & fs$
                                          /* Shipping Region Code      */
                                          /* (AWD009) Add S,L,B        */
                                          /* Loading Dock Door         */
/* (CR932) */ 
            if str(rec$(),596%,1%) = "B" then                ~
              lbl$(30%) = str(rec$(),577%,2%) & "/BP" & FS$  ~
            else                                             ~
              lbl$(30%) = "  " & str(rec$(),577%,2%) & FS$
/* (CR932) - */               
REM  LBL$(30%) = STR(REC$(),577%,2%) & STR(REC$(),596%,1%)  & FS$
                                          /* (AWD009)                  */

                                          /* (AWD001) - 6/27/2003      */
                                          /* Contractor Name           */
                                          /* (AWD007) - 04/04/05       */
                                          /* (PAR004) - 09/25/06       */
            init(" ") lb_contract$, lb_job_new$, lb_room_new$, lb_nominal$

                      lb_contract$ = str(rec$(),539%,16%)

            lbl$(31%) = lb_contract$          & fs$
                                          /* Customer Job Nme          */
                                          /* (AWD007)                  */
                                          /* (PAR004)                  */
            lb_job_new$ = str(rec$(),597%,16%)

REM   LB_JOB1$ = STR(REC$(),590%,6%)
REM   LB_JOB$  = STR(REC$(),555%,10%) & LB_JOB1$

REM CR2930            lbl$(32%) = lb_job_new$           & fs$
REM    LBL$(32%) = LB_JOB$               & FS$
/* CR2930 */
       gosub lookup_job_bckm
       lbl$(32%) = bck_job$ & fs$
                                          /* (PAR004)                  */

                                          /* Customer Room Location    */
                                          /* (PAR004)                  */
                      lb_room_new$ = str(rec$(),613%,16%)

REM    LB_ROOM$ = STR(REC$(),565%,6%)

            lbl$(33%) = lb_room_new$          & fs$
REM    LBL$(33%) = LB_ROOM$              & FS$
                                          /* (PAR004)                  */
                                          /* Window Nominal Size       */
                      lb_nominal$ = str(rec$(),571%,6%)

            lbl$(34%) = lb_nominal$           & fs$
                                          /* (AWD001) - 06/27/2003     */
                                          /* Caelus Line No.           */
            lb_line$  = str(rec$(),9%,2%)
            lbl$(35%) = lb_line$              & fs$
                                          /* Opening and Exact Size    */
            gosub lookup_sizes
                                          /* S.O. Piece and Qty        */
            lb_qty_of$ = lb_itmn$ & " OF " & lb_itmtot$
            lbl$(36%) = lb_qty_of$            & fs$
                                          /* Opening and Exact Size    */
                                          /* Opening                   */
                                          /* (AWD006) - 11/18/2004     */
            lbl$(37%) = str(lb_open$,1%,16%)   & fs$
                                          /* Exact Size                */
            lbl$(38%) = str(lb_exact$,1%,16%) & fs$
                                         /* (AWD001) = 06/27/2003      */
/*SR74525 + */
            lbl$(40%) = date$ & fs$
            runtime$ = " "
            call "TIME" (runtime$)
            lbl$(41%) = runtime$ & fs$
/*SR74525 - */

/*CR1174  + */
            lbl$(42%) = lb_shp_blk$ & fs$
/*CR1174  - */

    read_loop
        init(" ") a$
        b$ = all(hex(00))
        nbr_lines% = nbr_lines% + 1%
        a$ = xx$(nbr_lines%)
        if a$ = " " then end_process
        a_len% = len(a$)                   /* Calc Length of Data String */
        str(b$,1%,a_len%) = str(a$,1%,a_len%) /* Put into b$ Data from a$ */
        convert str(a$,1%,2%) to ln%, data goto skip_data
                                           /* Look for a Field Number    */
        l_len% = len(lbl$(ln%))            /* Find Length of Data Element*/
                                           /* in the Label data array    */
        b_len% = (a_len% - 2%) + l_len%    /* Adjust for 2 digit field No*/

        b$ = all(hex(00))                  /* Initialize Print string    */
                                           /* 1st set Font data for print*/
                                           /* 2nd append Actual Data that*/
                                           /*     will be printed        */
        str(b$,1%,b_len%) = str(a$,3%,a_len%-2%)                           ~
            & str(lbl$(ln%),1%,l_len%)
      skip_data
                                           /* (AWD002)                  */
        if nbr_lines% = 1% and been_here% = 1% then                        ~
                               b$ = hex(7e) & str(a$,2%,a_len%)

        if nbr_lines% = 1% and been_here% > 1% then                        ~
                               goto read_loop


        gosub print_line
        if a$ = "^XZ" then end_process       /* Last Line */
        goto read_loop

    end_process
        if nbr_lines% = 0% then error% = 8%
        return

        check_samples
            ss% = 0%
            if str(lb_part$,1%,1%) = "9" then return

            if len(lb_part$) < 20 then goto LS2      /* Quick Test      */
            convert str(lb_part$,20%,3%) to ss%, data goto LS1

            if ss% < 1% or ss% > 80% then goto LS1   /* Not Samp/Disp   */

            if str(lb_part$,7%,2%) > "99" then goto LS1

        return                                       /* Code Found      */
LS1:        convert str(lb_part$,23%,3%) to ss%, data goto LS2

            if ss% < 1% or ss% > 80% then goto LS2
                                                     /* Code Found      */
        return
LS2:        ss% = 0%
        return

        check_howship
            ap_howship$ = str(rec$(),488%,2%)
            if ap_howship$ = "06" then how2$ = "SampPickup"
            if ap_howship$ = "09" then how2$ = "T/S/P/  Up"
            if ap_howship$ = "10" then how2$ = "CustPickup"
            if ap_howship$ = "24" then how2$ = "Parts     "
        return

        check_wood_part
            init(" ") lb_part$, lb_wood$, lb_sample$, lb_wood_part$,    ~
                      how1$, how2$, how3$

            lb_part$   = str(rec$(),213%,25%)
            lb_wood$   = str(rec$(),532%,6%)
            lb_sample$ = str(rec$(),538%,1%)
                                                     /* Wood Surround */
            if str(lb_wood$,1%,3%) <> "N/A" then lb_wood_part$ = "W"
            if lb_wood_part$ = "W" then goto LS3     /* Not a Part    */

                                                     /* Parts         */
            gosub check_parts
            if part% = 1% then lb_wood_part$ = "P"
            if part% = 1% then how3$ = "Parts     "
            if part% = 1% then return                /* Not a Sample  */
LS3:
            gosub check_samples
            if ss% > 11% and ss% < 29% then lb_wood_part$ = "P"

            if lb_sample$ = "1" then how1$ = " SAMPLE   "
            if lb_sample$ = "2" then how1$ = " DISPLAY  "


        return

        check_parts                                   /* Check Model  */
            part% = 0%
            if len(lb_part$) < 19% then goto L10005
            p% = pos("456" = str(lb_part$,11%,1%))
            if p% <> 0% then goto L10005

            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PLANPARTS"
            str(readkey$,10%,15%) = str(lb_part$,1%,3%)
            read #1,key = readkey$, using L10000, desc$, eod goto L10010
L10000:        FMT POS(25), CH(30)
L10005:     part% = 1%
L10010: return
    
        lookup_sizes                                   /* (PAR001)      */
            init(" ") lb_key1$, lb_itmn$, lb_itmtot$, lb_open$, lb_exact$,~
                      lb_sub_part$, lb_sub_info$, lb_sub_descr$, lb_shp_blk$
  
            str(lb_key1$,1%,18%) = lb_bar$
/*CR1174 add shipment block */                         /* (PAR001)      */
            read #2, key 1% > lb_key1$, using L11000, lb_key1$, lb_itmn$, ~
                              lb_itmtot$, lb_open$, lb_exact$,            ~
                              lb_sub_part$, lb_sub_info$, lb_sub_descr$,  ~
                              lb_shp_blk$,     eod goto L11200
                                                       /* (PAR001)      */
L11000:        FMT POS(278), CH(23), POS(351), CH(03), CH(03), POS(424),  ~
                   CH(19), CH(19), POS(603), CH(20), POS(637), CH(10), CH(40), ~
                   POS(724), CH(03)
                                                       /* (PAR001)      */
            if lb_bar$ <> str(lb_key1$,1%,18%) then goto L11300

L11200: return
L11300:
            init(" ") lb_open$, lb_exact$, lb_itmn$, lb_itmtot$

        return
                                                       /* (PAR001)      */

/* CR1918 */                                                    
        lookup_pg_po
              init(" ") s_key$, job_name$
              str(s_key$,1%,9%)   = dtl_cuscode$
              str(s_key$,10%,16%) = dtl_so$
              read #4,key = s_key$, using L12100, pg_po$,   ~
                                                           eod goto L12199
L12100:          FMT POS(619), CH(20), POS(950), CH(20)  
L12199: return

/* CR2930 get JOB */  
        lookup_job_bckm
              init(" ") s_key$, bck_job$
              str(s_key$,1%,9%)   = str(rec$(),30%,9%)
              str(s_key$,10%,16%) = str(rec$(),56%,8%)
              read #4,key = s_key$, using L12200, bck_job$,   ~
                                                           eod goto L12299
L12200:          FMT POS(619), CH(20)
L12299: return
        
        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                                       /* (EWD001)     */

        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************
L55030:     FMT CH(90)

        REM *************************************************************~
            *           S P E C I A L   R O U T I N E S                 *~
            *************************************************************

        print_line
            write #5, using L55030, b$, eod goto L61550
        return

L61550:     error% = 5%
        return clear all

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************
        exit_sub


        end

        exit_print
            lb1% = 0% : lb2% = 0%

            close #5

            call "LINK" addr(script$, lb1%, lb2%)
            if lb1%    > 0% then error% = 4%

            call "FILEBGON" (#5)          /* Scratch 'MFGSHIP'        */
        end

        file_exists
          comp% = 2%
          hdr$ = "***   New Shipping Label   ***"
          msg$(1%) = "       The File (MFGSHIP) Already Exists.       "
          msg$(2%) = "       New  S h i p p i n g   L a b e l s        "
          msg$(3%) = "Press <RETURN> To Exit Prog, or PF(16) to Delete."
          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        load_label
        init(" ") yy$()


        yy$(  1%) = "^JO"
        yy$(  2%) = "^XA^EG^XZ"

        yy$(  3%) = "^XA"
        yy$(  4%) = "^PMN"
        yy$(  5%) = "^MNY"
        yy$(  6%) = "^MMT"                             /* Back Feed Off */
                                                       /* R=Off, T=On   */
        yy$(  7%) = "^MTT"
        yy$(  8%) = "^MD0"
        yy$(  9%) = "^LH0,0"
        yy$( 10%) = "^LL813"
        yy$( 11%) = "^PR3,4,8"                         /* PR Label Print Speed*/
                                                       /* a = 3 Print Speed   */
                                                       /* b = 4 Slew  SPeed   */
                                                       /* c = 8 Back Feed Spee*/
        yy$( 12%) = "^JMA"

        yy$( 13%) = "^COY,362"                         /* 256 + 128 Mem */
                                                       /* (-) 22k Intern*/

        yy$( 14%) = "^SZ2^JMA"
        yy$( 15%) = "^MCY^PMN"
        yy$( 16%) = "^PW1234~JSN"
        yy$( 17%) = "^JZY"
        yy$( 18%) = "^LH0,0^LRN"
        yy$( 19%) = "^XA"
        yy$( 20%) = "^FT1137,51"
        yy$( 21%) = "^CI0"
        yy$( 22%) = "01^A0R,45,62^FD"           /* 01 - (W) Wood Surr, Part */
        yy$( 23%) = "^FT1112,127"
        yy$( 24%) = "02^A0R,116,99^FD"          /* 02 - Appian Load Number  */
        yy$( 25%) = "^FT1049,38"
        yy$( 26%) = "^A0R,68,46^FDPO:^FS"
        yy$( 27%) = "^FO1044,115"                 /* CR1918 */
        yy$( 28%) = "^GB56,446,56^FS"
        yy$( 29%) = "^FT1052,115"
        yy$( 30%) = "04^A0R,54,42^FR^FD"        /* 04 - Customer P.O. No.   */
        yy$( 31%) = "^FT1110,362"
        yy$( 32%) = "03^A0R,116,90^FD-"         /* 03 - Appian Drop Number  */
/*      YY$( 33%) = "^FT1141,576"                      SR66608  */
        yy$( 33%) = "^FT1141,564"                    /*SR66608  */  /* SR82456*/
        yy$( 34%) = "36^A0R,56,49^FD"        /* 36 - SO Item Piece an Total */
        yy$( 35%) = "^FO426,57"
        yy$( 36%) = "23^BY2^BCI,89,N,N^FD>:"    /* 23 - Production Bar Code */
        yy$( 37%) = "^FT830,34"
        yy$( 38%) = "23^A0I,23,32^FD"           /* 23 - Production Bar Code */
        yy$( 39%) = "^FT985,38"
        yy$( 40%) = "^A0R,56,46^FDSO:^FS"
        yy$( 41%) = "^FT980,115"
        yy$( 42%) = "05^A0R,65,46^FD"           /* 05 - Customer S.O.       */
        yy$( 43%) = "^FO1188,39"
        yy$( 44%) = "^GB0,75,3^FS"
        yy$( 45%) = "^FO1113,37"
        yy$( 46%) = "^GB75,0,3^FS"
        yy$( 47%) = "^FO1113,113"
        yy$( 48%) = "^GB75,0,3^FS"
        yy$( 49%) = "^FO1112,39"
        yy$( 50%) = "^GD0,76,3,B,L^FS"
        yy$( 51%) = "^FT938,430"
        yy$( 52%) = "29^A0R,56,36^FDSeries Style: "    /* 29 - Series Style */
        yy$( 53%) = "^FT883,430"
        yy$( 54%) = "08^A0R,45,37^FDLine No: "    /* 08 - WW Config Line No */
        yy$( 55%) = "^FT832,430"
        yy$( 56%) = "31^A0R,45,37^FDCust: "       /* 31 - Contractor Name   */
        yy$( 57%) = "^FT788,430"
        yy$( 58%) = "32^A0R,35,31^FDJob: "        /* 32 - Job Name  CR2930  */
        yy$( 59%) = "^FT742,435"
        yy$( 60%) = "33^A0R,39,33^FDRoom: "     /* 33 - Room Location       */
/*      YY$( 61%) = "^FO649,574"                       SR66608  */
        yy$( 61%) = "^FO1,1"                         /*SR66608 do nothing */
/*      YY$( 62%) = "^GB70,229,70^FS"                  SR66608 */
        yy$( 62%) = "^FO1,1"                         /*SR66608 do nothing */
/*      YY$( 63%) = "^FT661,574"                       SR66608  */
        yy$( 63%) = "^FT1110,609"
/*      YY$( 64%) = "39^A0R,68,54^FR^FD"             39 - LOAD DROP PO      */
        yy$( 64%) = "39^A0R,116,90^FR^FD"       /*(SR66608) 39-Load Drop PO */
                       /* SR82456 this has been dropped from label */
        yy$( 65%) = "^FO109,13"
        yy$( 66%) = "^GB0,203,3^FS"
        yy$( 67%) = "^FO114,622"
        yy$( 68%) = "^GB0,203,3^FS"
        yy$( 69%) = "^FO0,213"
        yy$( 70%) = "^GB107,0,3^FS"
        yy$( 71%) = "^FO28,64"
        yy$( 72%) = "^GB70,130,70^FS"
        yy$( 73%) = "^FT40,64"
        yy$( 74%) = "06^A0R,68,54^FR^FD"         /* 06 - Production Seq No. */
        yy$( 75%) = "^FT297,28"
        yy$( 76%) = "18^A0R,39,28^FD"            /* 18 - Desc Line (1)*/
        yy$( 77%) = "^FT345,51"
        yy$( 78%) = "17^A0R,39,41^FDPart No:  "  /* 17 - MFG Part Number    */
        yy$( 79%) = "^FO378,26"
        yy$( 80%) = "^GB0,711,3^FS"
        yy$( 81%) = "^FO327,26"
        yy$( 82%) = "^GB0,711,3^FS"
        yy$( 83%) = "^FO327,735"
        yy$( 84%) = "^GB54,0,3^FS"
        yy$( 85%) = "^FT807,165"
        yy$( 86%) = "34^A0R,45,29^FDNom: "       /* 34 - Window Nominal Size*/
        yy$( 87%) = "^FT756,165"
        yy$( 88%) = "37^A0R,45,29^FDOpen:"       /* 37 - Window Opening Size*/
        yy$( 89%) = "^FT706,165"
        yy$( 90%) = "38^A0R,45,29^FDExact:"      /* 38 - Window Exact Size  */
        yy$( 91%) = "^FT629,168"
        yy$( 92%) = "10^A0R,56,36^FDCust: "      /* 10 - Customer Code      */
        yy$( 93%) = "^FT571,152"
        yy$( 94%) = "11^A0R,56,36^FDName: "      /* 11 - Customer Name      */
        yy$( 95%) = "^FT509,168"
        yy$( 96%) = "12^A0R,56,36^FDAddr: "      /* 12 - Address Line (1)   */
        yy$( 97%) = "^FO442,255"
        yy$( 98%) = "^GB53,319,53^FS"
        yy$( 99%) = "^FT450,255"
        yy$(100%) = "14^A0R,51,24^FR^FD"         /* 14 - Customer City      */
        yy$(101%) = "^FO442,585"
        yy$(102%) = "^GB53,35,35^FS"
        yy$(103%) = "^FT450,585"
        yy$(104%) = "15^A0R,51,33^FR^FD"         /* 15 - State Code         */
        yy$(105%) = "^FO442,629"
        yy$(106%) = "^GB53,174,53^FS"
        yy$(107%) = "^FT450,629"
        yy$(108%) = "16^A0R,51,33^FR^FD"         /* 16 - Zip Code           */
/*      yy$(109%) = "^FT30,338"                                             */
/*        YY$(109%) = "^FT32,275"                 (SR70079)                 */
        yy$(109%) = "^FT32,211"                 /*(SR70079)                 */
/*      yy$(110%) = "30^A0R,93,135^FD"             30 - Lane/Loading Dock D */
REM  YY$(110%) = "30^A0R,102,291^FD"    /*(SR70079)30-LANE/LOADING DOCK D*/
/*      yy$(110%) = "30^A0R,93,88^FD"     (CR932)  30-Lane/Loading Dock D   */
        yy$(110%) = "30^A0R,127,175^FD"  /* (CR932)  30-Lane/Loading Dock D*/
        yy$(111%) = "^FT48,647"             /* CR2815 */
        yy$(112%) = "27^A0R,28,30^FD"       /* 27 - Caelus PLoad No.     */
        yy$(113%) = "^FT29,647"             /* CR2815 */
        yy$(114%) = "28^A0R,28,30^FD"       /* 28 - Caelus PDrop No.     */
        yy$(115%) = "^FO327,26"
        yy$(116%) = "^GD53,0,3,B,L^FS"
        yy$(117%) = "^FT254,28"
        yy$(118%) = "19^A0R,39,26^FD"       /* 19 - Description Line (2)*/
        yy$(119%) = "^FT212,28"
        yy$(120%) = "20^A0R,39,26^FD"       /* 20 - Description Line (3)*/
        yy$(121%) = "^FT169,28"
        yy$(122%) = "21^A0R,39,26^FD"       /* 21 - Description Line (4)*/
        yy$(123%) = "^FT126,28"
        yy$(124%) = "22^A0R,39,26^FD"       /* 22 - Description Line (5)*/
        yy$(125%) = "^FT980,296"
        yy$(126%) = "35^A0R,65,46^FD-"      /* 35 - Caelus Line No.     */
        yy$(127%) = "^FO0,622"
        yy$(128%) = "^GD114,0,3,B,L^FS"
/*SR74525  CR2815  + */
        yy$(129%) = "^FT83,647"
        yy$(130%) = "40^A0R,20,21^FD"
        yy$(131%) = "^FT83,728"
        yy$(132%) = "41^A0R,20,21^FD"
/*SR74525 - */
/*CR1174 +  */
        yy$(133%) = "^FO1019,581^GB94,223,94^FS"
        yy$(134%) = "^FT1037,581"
        yy$(135%) = "42^A0R,93,126^FR^FD"
/*CR1174 - */
        yy$(136%) = "^PQ1,0,1,Y"
        yy$(137%) = "^XZ"
        yy$(138%) = " "

        return

                                                           /* (AWD001)     */
REM     LOAD_LABEL
REM     INIT(" ") YY$()
REM
REM        YY$( 1%) = "^JO"
REM        YY$( 2%) = "^XA^EG^XZ"
REM
REM        YY$( 3%) = "^XA"
REM        YY$( 4%) = "^PMN"
REM        YY$( 5%) = "^MNY"
REM        YY$( 6%) = "^MMT"                          /* BACK FEED OFF */
REM                                                   /* R=OFF, T=ON   */
REM        YY$( 7%) = "^MTT"
REM        YY$( 8%) = "^MD0"
REM        YY$( 9%) = "^LH0,0"
REM        YY$(10%) = "^LL813"
REM        YY$(11%) = "^PR3,4,8"                      /* PR LABEL PRINT SPEED*/
REM                                                   /* A = 3 PRINT SPEED   */
REM                                                   /* B = 4 SLEW  SPEED   */
REM                                                   /* C = 8 BACK FEED SPEE*/
REM        YY$(12%) = "^JMA"
REM
REM        YY$(13%) = "^COY,362"                      /* 256 + 128 MEM */
REM                                                   /* (-) 22K INTERN*/
REM
REM        YY$(14%) = "^FO488,20^FR^GB69,4,4^FS"
REM        YY$(15%) = "^FO488,784^FR^GB69,4,4^FS"
REM        YY$(16%) = "^FO488,24^FR^GB4,760,4^FS"
REM        YY$(17%) = "^FO553,24^FR^GB4,760,4^FS"
REM        YY$(18%) = "^FO12,14^FR^GB140,4,4^FS"
REM        YY$(19%) = "^FO12,303^FR^GB140,4,4^FS"
REM        YY$(20%) = "^FO12,18^FR^GB4,285,4^FS"
REM        YY$(21%) = "^FO148,18^FR^GB4,285,4^FS"
REM        YY$(22%) = "^FO20,565^FR^GB116,4,4^FS"
REM        YY$(23%) = "^FO20,768^FR^GB116,4,4^FS"
REM        YY$(24%) = "^FO20,569^FR^GB4,199,4^FS"
REM        YY$(25%) = "^FO132,569^FR^GB4,199,4^FS"
REM                                      /* (AWD008) - NEW LINE */
REM        YY$(26%) = "^FO1042,144^FR^GB63,526,63^FS"
REM                                      /* (AWD008) - NEW LINE */
REM        YY$(27%) = "^FO1116,26^FR^GB91,4,4^FS"
REM        YY$(28%) = "^FO1116,112^FR^GB91,4,4^FS"
REM        YY$(29%) = "^FO1116,30^FR^GB4,82,4^FS"
REM        YY$(30%) = "^FO1203,30^FR^GB4,82,4^FS"
REM        YY$(31%) = "^FO559,591^FR^GB51,190,51^FS"
REM        YY$(32%) = "^FO559,516^FR^GB51,57,51^FS"
REM        YY$(33%) = "^FO1042,693^FR^GB63,61,61^FS"
REM        YY$(34%) = "^FO559,201^FR^GB51,287,51^FS"
REM                                     /* (AWD003) - NEW LINE */
REM                                     /* (AWD009) NO CHANGE  */
REM        YY$(35%) = "^FO26,75^FR^GB69,154,69^FS"
REM                                     /* (AWD003) - NEW LINE */
REM                                     /* (AWD009)            */
REM
REM                                     /* (W) WOOD SURR, PART */
REM        YY$(36%) = "01^FO1128,49^CI0^A0R,61,81^FR^FD"
REM                                     /* MFG PART NUMBER     */
REM        YY$(37%) = "17^FO494,211^CI0^A0R,51,41^FR^FD"
REM                                     /* APPIAN LOAD NUMBER  */
REM        YY$(38%) = "02^FO1097,134^CI0^A0R,102,102^FR^FD"
REM                                     /* (AWD005) INCREASE SZ*/
REM                                     /* APPIAN DROP NUMBER  */
        REM   YY$(39%) = "03^FO1124,713^CI0^A0R,61,51^FR^FD"
REM
REM        YY$(39%) = "03^FO1093,679^CI0^A0R,102,102^FR^FD"
REM                                     /* (AWD005)            */
REM                                     /* (AWD008) 65,65 TO 63,65 */
REM                                     /* CUSTOMER P.O. NO.   */
REM        YY$(40%) = "04^FO1042,144^CI0^A0R,63,65^FR^FD"
REM                                     /* LABEL TEXT          */
REM        YY$(41%) = "^FO925,26^CI0^A0R,61,51^FR^FDSEQ ^FS"   /* REMOVED COLON AFTER SEQ */
REM        YY$(42%) = "^FO443,51^CI0^A0R,37,26^FR^FDPRODUCTION DESCRIPTION ^FS" /* REMOVED COLON AFTER DESC */
REM                                     /* ADDRESS LINE (1)    */
REM        YY$(43%) = "12^FO606,203^CI0^A0R,51,35^FR^FD"
REM                                     /* DESCRIPTION LINE (2)*/
REM        YY$(44%) = "19^FO404,53^CI0^A0R,41,28^FR^FD"
REM                                     /* DESCRIPTION LINE (3)*/
REM        YY$(45%) = "20^FO368,53^CI0^A0R,41,28^FR^FD"
REM                                     /* DESCRIPTION LINE (4)*/
REM        YY$(46%) = "21^FO331,53^CI0^A0R,41,28^FR^FD"
REM                                     /* DESCRIPTION LINE (5)*/
REM        YY$(47%) = "22^FO289,53^CI0^A0R,41,28^FR^FD"
REM                                     /* PRODUCTION BAR CODE */
REM        YY$(48%) = "23^FO185,148^BY2,2.0,98^BCR,98,Y,N,N^FR^FD>" /* REMOVED COLON AFTER > */
REM                                     /* LABEL TEXT          */
REM        YY$(49%) = "^FO494,39^CI0^A0R,51,41^FR^FDPART NO ^FS"    /* REMOVED COLON AFTER NO */
REM                                     /* (AWD003)            */
REM                                     /* PRODUCTION SEQ NO.  */
REM        YY$(50%) = "06^FO925,142^CI0^A0R,61,51^FR^FD"
REM                                     /* DESCRIPTION LINE (1)*/
REM        YY$(51%) = "18^FO443,354^CI0^A0R,41,28^FR^FD"
REM                                     /* HOW (1)             */
REM        YY$(52%) = "24^FO98,24^CI0^A0R,45,35^FR^FD"
REM                                     /* (AWD003) REMOVE (2) */
REM
REM                                     /* HOW (2)             */
        REM   YY$(52%) = "25^FO59,24^CI0^A0R,45,35^FR^FD"
REM                                     /* HOW (3)             */
        REM   YY$(53%) = "26^FO20,24^CI0^A0R,45,35^FR^FD"
REM
REM                                     /* (AWD003) REMOVE (2) */
REM                                     /* CAELUS LOAD NO.     */
REM        YY$(53%) = "27^FO85,579^CI0^A0R,45,35^FR^FD"
REM                                     /* CAELUS DROP NO.     */
REM        YY$(54%) = "28^FO30,579^CI0^A0R,45,35^FR^FD"
REM                                     /* LABEL TEXT          */
REM        YY$(55%) = "^FO1032,26^CI0^A0R,77,77^FR^FDPO ^FS"        /* REMOVED COLON AFTER PO */
REM        YY$(56%) = "^FO606,26^CI0^A0R,51,35^FR^FDCUST ADDR ^FS"  /* REMOVED COLON AFTER ADDR */
REM                                     /* ZIP CODE            */
REM        YY$(57%) = "16^FO559,591^CI0^A0R,51,35^FR^FD"
REM                                     /* STATE CODE          */
REM        YY$(58%) = "15^FO559,516^CI0^A0R,51,35^FR^FD"
REM                                     /* WW CONFIG LINE NO   */
REM        YY$(59%) = "08^FO935,569^CI0^A0R,51,41^FR^FD"
REM                                     /* (AWD009)            */
REM                                     /* LOADING DOCK DOOR   */
REM        YY$(60%) = "30^FO26,360^CI0^A0R,102,102^FR^FD"
REM                                     /* (AWD009)            */
REM
REM                                     /* CONTRACTOR NAME     */
REM        YY$(61%) = "31^FO890,498^CI0^A0R,51,41^FR^FD"
REM                                     /* JOB NAME            */
REM                                     /* (PAR004)            */
REM        YY$(62%) = "32^FO839,498^CI0^A0R,51,41^FR^FD"
REM                                     /* LABEL TEXT          */
REM        YY$(63%) = "^FO890,396^CI0^A0R,51,41^FR^FDCUST ^FS"    /* REMOVED COLON AFTER CUST */
REM        YY$(64%) = "^FO935,396^CI0^A0R,51,41^FR^FDLINE NO ^FS" /* REMOVED COLON AFTER NO */
REM                                     /* CUSTOMER CODE       */
REM        YY$(65%) = "10^FO703,203^CI0^A0R,51,35^FR^FD"
REM                                     /* LABEL TEXT          */
REM        YY$(66%) = "^FO703,26^CI0^A0R,51,35^FR^FDCUST CODE ^FS" /* REMOVED COLON AFTER CODE */
REM                                     /* ROOM LOCATION       */
REM                                     /* (PAR004)            */
/*         YY$(67%) = "33^FO790,498^CI0^A0R,51,41^FR^FD" */
/*PAR005*/ YY$(67%) = "33^FO790,470^CI0^A0R,51,41^FR^FD"
REM                                     /* LABEL TEXT          */
REM        YY$(68%) = "^FO790,396^CI0^A0R,51,41^FR^FDRM ^FS"      /* REMOVED COLON AFTER RM */
REM                                     /* CAELUS LINE NO.     */
REM        YY$(69%) = "35^FO1042,693^CI0^A0R,65,65^FR^FD"
REM                                     /* S.O. ITEM PIECE AN TOTAL */
REM        YY$(70%) = "36^FO1124,455^CI0^A0R,69,47^FR^FD"
REM                                     /* CUSTOMER NAME       */
REM        YY$(71%) = "11^FO652,203^CI0^A0R,51,35^FR^FD"
REM                                     /* LABEL TEXT          */
REM        YY$(72%) = "^FO652,26^CI0^A0R,51,35^FR^FDCUST NAME ^FS" /* REMOVED COLON AFTER NAME */
REM                                     /* CUSTOMER CITY       */
REM        YY$(73%) = "14^FO559,201^CI0^A0R,51,35^FR^FD"
REM                                     /* SERIES STYLE        */
REM        YY$(74%) = "29^FO988,614^CI0^A0R,51,41^FR^FD"
REM                                     /* LABEL TEXT          */
REM        YY$(75%) = "^FO988,396^CI0^A0R,51,41^FR^FDSERIES STYLE ^FS" /* REMOVED COLON AFTER STYLE */
REM                                     /* CUSTOMER S.O.       */
REM        YY$(76%) = "05^FO977,124^CI0^A0R,61,51^FR^FD"
REM                                     /* LABEL TEXT          */
REM        YY$(77%) = "^FO977,26^CI0^A0R,61,51^FR^FDSO ^FS"    /* REMOVED COLON AFTER SO */
REM        YY$(78%) = "^FO839,396^CI0^A0R,51,41^FR^FDJOB ^FS"  /* REMOVED COLON AFTER JOB */
REM        YY$(79%) = "^FO750,26^CI0^A0R,51,41^FR^FDEXACT ^FS" /* REMOVED COLON AFTER EXACT */
REM        YY$(80%) = "^FO801,26^CI0^A0R,51,41^FR^FDOPEN ^FS"  /* REMOVED COLON AFTER OPEN */
REM        YY$(81%) = "^FO849,26^CI0^A0R,51,41^FR^FDNOM ^FS"   /* REMOVED COLON AFTER NOM */
REM                                     /* WINDOW EXACT SIZE   */
REM        YY$(82%) = "38^FO750,134^CI0^A0R,51,41^FR^FD"
REM                                     /* WINDOW OPENING SIZE */
REM                                     /* (AWD006) CHANGE SIZE*/
REM                                     /* OF FONT FROM 41 TO  */
REM                                     /* (36)    CALC VAL 32 */
REM        YY$(83%) = "37^FO801,134^CI0^A0R,51,36^FR^FD"
REM                                     /* WINDOW NOMINAL SIZE */
REM        YY$(84%) = "34^FO849,134^CI0^A0R,51,41^FR^FD"
REM                                     /* (AWD003) - NEW LINE */
REM                                     /* PRODUCTION SEQ. NO. */
REM        YY$(85%) = "06^FO26,75^CI0^A0R,71,61^FR^FD"
REM                                     /* (AWD003) - NEW LINE */
REM  /* NEW DROP SUB NUMBER */
REM        YY$(86%) = "^FO705,500^FR^GB81,285,81^FS"
REM        YY$(87%) = "39^FO710,500^CI0^A0R,71,61^FR^FD"
REM        YY$(88%) = "^PQ1"
REM        YY$(89%) = "^XZ"


        return


