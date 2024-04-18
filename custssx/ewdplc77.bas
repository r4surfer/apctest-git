        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EWDPLC77 - Energy Star               *~
            *  Creation Date     - 03/13/00                             *~ 
            *  Last Modified Date- 05/31/2012                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Modifications By  - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Write records to a file to print the *~
            *                      new Energy Star Labels.              *~
            *                                                           *~
            *                      Print File  = MFENERGY               *~
            *                      Script File = MFENERGY               *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDPLC77 - Generates the label format and data to print   *~
            *            Energy Star Labels. The resulting file is      *~
            *            routed to the label printer via a script.      *~
            *                                                           *~
            *          - ERROR% = 0% - Lbl Ok and Sent                  *~
            *                     1% - Could Not Open Lbl Print File    *~
            *                     4% - Shell Script Error               *~
            *                     5% - Print Error Creating Label       *~
            *                     6% - Could Not Reset Table Flag       *~
            *                     7% - Label Data Format Error          *~
            *                     8% - No Data for Label                *~
            *                                                           *~
            *          - Special Data File with label format            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/11/00 ! Original - Copied & Mod (sub) EWDPLA71.  ! RHH *~
            * 08/31/00 ! (EWD001) - Mods for forms change.        ! RHH *~
            * 11/14/00 ! (EWD002) - Mods Visible Transmittance    ! RHH *~
            * 12/07/00 ! (EWD003) - Mods for form change (Mistake)! RHH *~
            * 01/22/01 ! (EWD004) - Mods to labels for pre-printed! RHH *~
            *          !              Data.                       !     *~
            * 07/27/01 ! (EWD005) - Mod to put Text on lower part ! RHH *~
            *          !            of label with size and DP     !     *~
            *          !            Value.                        !     *~
            * 06/24/02 ! (EWD006) - Mod for Wind Zone Design      ! RHH *~
            *          !            pressure.                     !     *~
            * 12/12/02 ! (EWD007) - New Energy Start Label        ! RHH *~
            * 02/28/03 ! (EWD009) - Mode for 5/8 and 1 Inch Grid  ! RHH *~
            *          !            also Multiple labels for      !     *~
            *          !            Continuous Head and Seal Prod !     *~
            * 06/17/03 ! (EWD010) - Mods to improve speed         ! RHH *~
            *          !            PR6,4,8 for New Faster Printer!     *~
            *          !            PR3,4,8 for Pld Slower Printers!    *~
            *          !                                          !     *~
            *          ! (Special Note) lb_nonresvisible contains !     *~
            *          !   the Value for Air Leakage              !     *~
            * 09/04/03 ! (EWD011) - Mod for New Energy Star Map   ! RHH *~
            *          !            Also the New Energy Star Logo !     *~
            * 07/16/04 ! (EWD012) - Mod to put printable version  ! RHH *~
            *          !            of the Barcode on the bottom  !     *~
            *          !            of the Label                  *     *~
            * 08/12/05 ! (EWD013) - Mod to 6 fields for NFRC      * RHH *~
            *          !            label format change.          *     *~
            * 10/31/05 ! (AWD014) - Mod for Label format change.  * RHH *~
            *          !            Have the Air Leakage text     *     *~
            *          !            removed fro the label. Alos,  *     *~
            *          !            Put leading Zero's in front of*     *~
            *          !            three ratings. Except for N/A *     *~
            * 01/01/06 ! (PAR000) - CR347 New Sub Part Number     * RHH *~
            * 03/03/06 ! (PAR001) - Mod for Sub Part Number No Chg* RHH *~
            * 08/26/08 ! (AWD015) - mod to replace NA with '-'    * CMG *~
            *12/09/2008! (AWD016) - mod for Lowe's energy star    * DES *~
            *01/20/2009! (AWD017) - mod for Florida Windows       * DES *~
            *04/01/2009! (AWD018) - lower or all region map       * CMG *~
            *11/05/2009! (AWD019) - use diff region maps          * DES *~
            *05/08/2012! (AWD020) - mod for triple pane           * CMG *~
            *05/31/2012! (AWD021) - mod for CPD Number            * CMG *~
            *11/15/2012! (AWD022) mod for door map                ! CMG *~
            *04/05/2013! (AWD023) TDI Number                      ! CMG *~
            *09/08/2014! (AWD024) Added Acoustical Performance    ! PWW *~
            *          !          Ratings (STS & OITC)            !     *~
            *11/25/2014! (AWD025) NFRC Reg Changes.               ! PWW *~
            *04/03/2015! (IM8005) Use bcksubpt Series & Style.    ! PWW *~
            *04/15/2015! (IM8020) Change Air Leakage syntax.      ! PWW *~
            *04/28/2015! (IM8020) Fix Series & Style issues.      ! PWW *~
            *12/10/2015! SR67154  2016 NFRC Reg Changes.          ! PWW *~
            *07/18/2016! CR00532  Increase lb_series$ + 1 char.   ! PWW *~
            *08/01/2017! CR1051   Suppress backfeed added         ! RDB *~ 
            *01/15/2018! CR1251   Increase lb_series$ for AmerClas! RDB *~
            *01/29/2109! CR1906   Window Nation                   ! RDB *~
            *05/16/2021! CR2847   Copy ewdplb77 with map changes  ! RDB *~
			*03/31/2023! CR3285 NFRC print map 0 fix              ! RDB *~
            *************************************************************

        sub "EWDPLC77" (been_here%,      /* Zero (Only 1st Time)       */~
                        lb_brand$,       /* Brand Name                 */~
                        lb_series$,      /* Series Number              */~
                        lb_style$,       /* Vinyl Window Style         */~
                        lb_style2$,      /* Vinyl Window Style         */~
                        lb_glass_op$,    /* Glass Option               */~
                        lb_glass_op1$,   /* Glass Option 2             */~
                        lb_resu$,        /* Resident U-Factor          */~
                        lb_nonresu$,     /* Non-Resident U-Factor      */~
                        lb_resheat$,     /* Resident Heat Coeff        */~
                        lb_nonresheat$,  /* Non-Resident Heat Coeff    */~
                        lb_resvisible$,  /* Resident Visible Trans(EWD002)*/~
                        lb_nonresvisible$,/* Non-Resident Visible Trans*/~
                        lb_dp$,          /* Design Pressure            */~
                        lb_width$,       /* Window Width               */~
                        lb_height$,      /* Window Height              */~
                        lb_seq$,         /* Sequence Number            */~
                        lb_dep_so$,      /* Department Sales Order     */~
                        lb_ld_mod$,      /* Load Model Number          */~
                        lb_pd_dte$,      /* Production Date            */~
                        lb_zone$,        /* Wind Zone            EWD006*/~
                        lb_pressure1$,   /* Positive Pressure    EWD006*/~
                        lb_pressure2$,   /* Negitive Pressure    EWD006*/~
                        lb_txt1$,        /* No Grid,5/8,3/4,1,3/8PAR000*/~
                        lb_txt2$,        /* Dual Glazed          EWD009*/~
                        lb_barcode$,     /* Prod. Barcode        EWD012*/~
                        lb_thick$,       /* Glass Thickness      EWD013*/~
                        part$,           /* Part                       */~
                        sub_part$,       /* Part                       */~
                        lb_cpdnum$,      /* (AWD021) CPD Number        */~
                        #5,              /* (MFENERGY)                 */~
                        #2,              /* (AWDSKUXR)           AWD016*/~
                        #4,              /* (GENCODES)           AWD017*/~
                        lb_fl$,          /* Florida Approval Code      */~
                        lb_tdi$,         /* (AWD022) TDI Number     */~
                        energy_star%,    /* (AWD018) which region      */~
                        door%,           /* (AWD022) mod for door map  */~
                        stc$,            /* (AWD024) mod for door map  */~
                        ctrlflag$,       /* CR9999 control backfeed    */~
                        error%)          /* Return Code                */

REM         a$80, b$80,                  /* Print Lines for Label      */
        dim                                                              ~
            a$100, b$100,                  /* Print Lines for Label      */~
            lbl$(60%)65,                 /* Label Data Array           */~
            sku_key$45,                  /* Label Data Array           */~
            fs$3                         /* End of Lbl Records         */

        dim lb_brand$10,                 /* Brand Name                 */~
/*          lb_series$12,                   Series Number              */~
/*CR1251*/  lb_series$24,                /* Series Number              */~
            lb_style$24,                 /* Vinyl Window Style         */~
            lb_style2$24,                /* Vinyl Window Style         */~
            lb_glass_op$24,              /* Glass Option               */~
            lb_glass_op1$24,             /* Glass Option 2             */~
            lb_resu$4,                   /* Resident U-Factor          */~
            lb_nonresu$4,                /* Non-Resident U-Factor      */~
            lb_resheat$4,                /* Resident Heat Coeff        */~
            lb_nonresheat$4,             /* Non-Resident Heat Coeff    */~
            lb_resvisible$4,             /* Resident Visible (EWD002)  */~
            lb_nonresvisible$5,          /* Non-Resident Visible(EWD002)*/~
            lb_dp$2,                     /* Design Pressure            */~
            lb_width$7, lb_x$4,          /* Window Width               */~
            lb_height$6,                 /* Window Height              */~
            lb_seq$16,                   /* Sequence Number            */~
            lb_dep_so$24,                /* Department Sales Order     */~
            lb_ld_mod$24,                /* Load Model Number          */~
            lb_pd_dte$24,                /* Production Date            */~
            lb_zone$3,                   /* Wind Zone          (EWD006)*/~
            lb_pressure1$6,              /* Positive Pressure  (EWD006)*/~
            lb_pressure2$6,              /* Negitive Pressure  (EWD006)*/~
            lb_txt1$10, lb_txt2$10,      /* Grid or Dual Glazed(EWD009)*/~
            lb_barcode$18,               /* Prod. Barcode      (EWD012)*/~
            prt_barcode$21,              /* Formatted Barcode  (EWD012)*/~
            lb_thick$5,                  /* Glass Thickness    (EWD013)*/~
            low$(950%)100,               /* Lowe's Label format        */~
            yy$(950%)100,                /* Label format               */~
            xx$(950%)100,                /* Buffer                     */~
            l0$(830%)100,                /* map 2  nada                */~
            l1$(830%)100,                /* map 2  N, NC, SC & S       */~
            l2$(830%)100,                /* map 2  N                   */~
            l3$(830%)100,                /* map 3  N & NC              */~
            l4$(830%)100,                /* map 4  NC                  */~
            l5$(830%)100,                /* map 5  N, NC & SC          */~
            l6$(830%)100,                /* map 6  NC & SC             */~
            l7$(830%)100,                /* map 7  SC                  */~
            l8$(830%)100,                /* map 8  nc, sc & s          */~
            l9$(830%)100,                /* map 9  SC & S              */~
            l10$(830%)100                /* map 10 S                   */
            
            
        dim lb_fl$12,                    /* Florida Approval Code      */~
            lb_tdi$12                    /* TDI Number                 */    

        dim part$25, sub_part$20
        dim gen_key$24, gen_data$30
        dim stc$3, oitc$2                /*<AWD024>                    */
        dim ctrlflag$1                   /* CR99999                    */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            axd$4,                       /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21                            /* (PAR001) */
            apc$   = "(AWD)Create Energy Star Labels 03/03/06"
            pname$ = "EWDPLC77 - Rev: R7.00"

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
            * #5  ! MFENERGY ! Print File For Production Labels         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            init(" ") axd$, rslt$()
            nbr_lines% = 0%
            fs$ = "^FS"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

            init(" ") xx$()
            if been_here% > 0% then goto L01000     /* (EWD010)         */
               gosub load_label

L01000:
/* (AWD018)  240 lower region glass */
            if energy_star% <> 3% then goto check_lowes

            if ctrlflag$ = "N" then goto skipctrl
             low$(002%) = "^XA^EG^XB^XZ"             /* CR1051 suppress start */
             low$(679%) = "^LL813^XB"  
skipctrl:
            copy low$() to xx$()
        lowes% = 1%
            been_here% = been_here% + 1%
            gosub begin_process
            goto exit_sub

check_lowes:
             /* if Lowe's use low$ else yy$  */
/*  #2  AWD016
 sku#       1- 16 ch(16) = key0
 upc#      17- 36 ch(20) = key1
 part      37- 61 ch(25) = key2
 sub part  62- 81 ch(20)
 model     82- 87 ch(06)
 desc      88-122 ch(35)
filler    123-160 ch(38)
*/
          init(" ") sku_key$, low_sku$
          str(sku_key$,01,25) = part$
REM         str(sku_key$,26,20) = sub_part$
          str(sku_key$,26,11) = sub_part$
          read #2,key 2 = sku_key$, using AWDSKUXR, low_sku$, eod goto copy_reg
AWDSKUXR:   FMT POS(05), CH(11)
            if ctrlflag$ = "N" then goto skipctr2
             low$(002%) = "^XA^EG^XB^XZ"             /* CR1051 suppress start */
             low$(410%) = "^LL813^XB"  
skipctr2:
            copy low$() to xx$()
            lowes% = 1%
            been_here% = been_here% + 1%
            gosub begin_process
            goto exit_sub

copy_reg:
            if ctrlflag$ = "N" then goto skipctr3
             yy$(002%) = "^XA^EG^XB^XZ"             /* CR1051 suppress start */
             yy$(411%) = "^LL813^XB"    /* chgit */
skipctr3:
            copy yy$() to xx$()
            lowes% = 0%
            been_here% = been_here% + 1%            /* (EWD010)         */
            gosub begin_process
            goto exit_sub

        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *************************************************************
        begin_process

            init(" ") l2$(), l3$(), l4$(), l5$(), l6$(), l7$()
        init(" ") l8$(), l9$(), l10$(), l0$(), l1$()

/* CR3285 fix mapping layout to allow NFRC label to print */        
        l0$(001) = "^JO"
        l0$(002) = "^XA^EG^XZ" 
        if ctrlflag$ = "Y" then ~
            l0$(002) = "^XA^EG^XB^XZ"             /* CR1051 suppress start */
        l0$(003) = hex(7e) & "DGR:DGLABEL0.GRF,11866,34,"
        l0$(004) = ","
        l0$(005) = ","
        l0$(006) = ","
        l0$(007) = ","
        l0$(008) = ","
        l0$(009) = ","
        l0$(010) = ","
        l0$(011) = ","
        l0$(012) = ","
        l0$(013) = ","
        l0$(014) = ","
        l0$(015) = ","
        l0$(016) = ","
        l0$(017) = ","
        l0$(018) = ","
        l0$(019) = ","
        l0$(020) = ","
        l0$(021) = ","
        l0$(022) = ","
        l0$(023) = ","
        l0$(024) = ","
        l0$(025) = ","
        l0$(026) = ","
        l0$(027) = ","
        l0$(028) = ","
        l0$(029) = ","
        l0$(030) = ","
        l0$(031) = ","
        l0$(032) = ","
        l0$(033) = ","
        l0$(034) = ","
        l0$(035) = ","
        l0$(036) = ","
        l0$(037) = ","
        l0$(038) = ","
        l0$(039) = ","
        l0$(040) = ","
        l0$(041) = ","
        l0$(042) = ","
        l0$(043) = ","
        l0$(044) = ","
        l0$(045) = ","
        l0$(046) = ","
        l0$(047) = ","
        l0$(048) = ","
        l0$(049) = ","
        l0$(050) = ","
        l0$(051) = ","
        l0$(052) = ","
        l0$(053) = ","
        l0$(054) = ","
        l0$(055) = ","
        l0$(056) = ","
        l0$(057) = ","
        l0$(058) = ","
        l0$(059) = ","
        l0$(060) = ","
        l0$(061) = ","
        l0$(062) = ","
        l0$(063) = ","
        l0$(064) = ","
        l0$(065) = ","
        l0$(066) = ","
        l0$(067) = ","
        l0$(068) = ","
        l0$(069) = ","
        l0$(070) = ","
        l0$(071) = ","
        l0$(072) = ","
        l0$(073) = ","
        l0$(074) = ","
        l0$(075) = ","
        l0$(076) = ","
        l0$(077) = ","
        l0$(078) = ","
        l0$(079) = ","
        l0$(080) = ","
        l0$(081) = ","
        l0$(082) = ","
        l0$(083) = ","
        l0$(084) = ","
        l0$(085) = ","
        l0$(086) = ","
        l0$(087) = ","
        l0$(088) = ","
        l0$(089) = ","
        l0$(090) = ","
        l0$(091) = ","
        l0$(092) = ","
        l0$(093) = ","
        l0$(094) = ","
        l0$(095) = ","
        l0$(096) = ","
        l0$(097) = ","
        l0$(098) = ","
        l0$(099) = ","
        l0$(100) = ","
        l0$(101) = ","
        l0$(102) = ","
        l0$(103) = ","
        l0$(104) = ","
        l0$(105) = ","
        l0$(106) = ","
        l0$(107) = ","
        l0$(108) = ","
        l0$(109) = ","
        l0$(110) = ","
        l0$(111) = ","
        l0$(112) = ","
        l0$(113) = ","
        l0$(114) = ","
        l0$(115) = ","
        l0$(116) = ","
        l0$(117) = ","
        l0$(118) = ","
        l0$(119) = ","
        l0$(120) = ","
        l0$(121) = ","
        l0$(122) = ","
        l0$(123) = ","
        l0$(124) = ","
        l0$(125) = ","
        l0$(126) = ","
        l0$(127) = ","
        l0$(128) = ","
        l0$(129) = ","
        l0$(130) = ","
        l0$(131) = ","
        l0$(132) = ","
        l0$(133) = ","
        l0$(134) = ","
        l0$(135) = ","
        l0$(136) = ","
        l0$(137) = ","
        l0$(138) = ","
        l0$(139) = ","
        l0$(140) = ","
        l0$(141) = ","
        l0$(142) = ","
        l0$(143) = ","
        l0$(144) = ","
        l0$(145) = ","
        l0$(146) = ","
        l0$(147) = ","
        l0$(148) = ","
        l0$(149) = ","
        l0$(150) = ","
        l0$(151) = ","
        l0$(152) = ","
        l0$(153) = ","
        l0$(154) = ","
        l0$(155) = ","
        l0$(156) = ","
        l0$(157) = ","
        l0$(158) = ","
        l0$(159) = ","
        l0$(160) = ","
        l0$(161) = ","
        l0$(162) = ","
        l0$(163) = ","
        l0$(164) = ","
        l0$(165) = ","
        l0$(166) = ","
        l0$(167) = ","
        l0$(168) = ","
        l0$(169) = ","
        l0$(170) = ","
        l0$(171) = ","
        l0$(172) = ","
        l0$(173) = ","
        l0$(174) = ","
        l0$(175) = ","
        l0$(176) = ","
        l0$(177) = ","
        l0$(178) = ","
        l0$(179) = ","
        l0$(180) = ","
        l0$(181) = ","
        l0$(182) = ","
        l0$(183) = ","
        l0$(184) = ","
        l0$(185) = ","
        l0$(186) = ","
        l0$(187) = ","
        l0$(188) = ","
        l0$(189) = ","
        l0$(190) = ","

        l1$(001) = "^JO"
        l1$(002) = "^XA^EG^XZ"
        if ctrlflag$ = "Y" then ~
           l1$(002) = "^XA^EG^XB^XZ"             /* CR1051 suppress start */
        l1$(003) = hex(7e) & "DGLABEL1.GRF,12768,32,"
        l1$(004) = "S04,S08,,::::R020,,::R020,R010,::R018,R010,"
        l1$(005) = ",:S08,S04,:S02,S02L010,S01L040,S018I0606080,T0CI0E0B080,T040023418E0,T"
        l1$(006) = "060031C8070,T030038EC630,V018C4EB1C,T0381C63118C,T014163188C6,U0A1318C"
        l1$(007) = "462,U05998C6230,U0218C63118,U018C63188D,V046318C46,V02318C622,V0198C63"
        l1$(008) = "12,X0C63188,X06318C6,W07B18C60,W0598C630,X08C6318,W02C6318C,X0E318C4,X"
        l1$(009) = "0918C60,X088C634,W01C46318,W02623188,W023118C0,W03188C60,W038C4630,W01"
        l1$(010) = "CE7330,W02,:,::::::hK05,hH0BC1C80,:hG0318C640,h0598C6318,h0IC6318F20,g"
        l1$(011) = "X09FC66318C739F0,gV0398FE3318C6198C4,gU019CDEF198C630DC63,gT018CC6398C"
        l1$(012) = "C6318763180,gU0C66318C66318E3318E0,gR060C63319C63318C7398C78,gP0108E63"
        l1$(013) = "198D63198C61ACC631,gP030C6318CC7318CC630E66318C0,gP0206318C66318C66318"
        l1$(014) = "63318C68,gO08I318C63318C63318E7198C633,gP02318C63198C63198C758CC6318F0"
        l1$(015) = ",X06S08C6318CC6318CC61CC66318C6F80,X02R0842318C66318C6630C63318C63FFCE"
        l1$(016) = "7,gQ0C2118C63318C63318E3198C631DDC62,gQ07108C63198C63198EB18CC6318CC63"
        l1$(017) = "0,gM0180638C46318CC6318CC798C66318CE6318,X018N01870C62318C66318C6618C6"
        l1$(018) = "3318DF33188,X03CN0101863118C63318C6331C63198C63198C0,Y0CO038C3188C6319"
        l1$(019) = "FC631996318CC6318FC70,Y04O02C718C46318EC7318CF318C66318C7E30,X01P0C638"
        l1$(020) = "C62318CE63F8C6718C6331CC63BB0,gN01630C63118C73318FE378C63198E631FE7,gN"
        l1$(021) = "0331863188C67198C671EC6318CC6318IF,gL01F198C318C46398CC631FE6318C66718"
        l1$(022) = "C6FFC0gL0118CC718C62338C66318CF318C633D8C637BC0gL018C6638C6311CC63318C"
        l1$(023) = "6998C63198C63198C0gM0C6330C63189C63198C638CC6318CC6318CC,gL02631986318"
        l1$(024) = "C66318CC631866318C6E318C66,gL02318CC318C6E318C66319E3318C63B18C63380gL"
        l1$(025) = "0318C6718C63118C63318D7198C63198C631980gL038C6338C63788C63198C718CC631"
        l1$(026) = "8CC6318C,gM0C6318C6318C46318CC630C66318CE6318C7,W04O06318C631AC62318C6"
        l1$(027) = "631863318C6B318C63,W08N02318C7318C63118C6331AE3198C63998C631,W0DN0318C"
        l1$(028) = "6398F63188C63198E718CC6318CC6318,W0EN018C630CC6318C46318CC618C66318C66"
        l1$(029) = "318C,V014N04C631863B18C62318C6630C63318CE3318C6,W08N066318C3318C63118C"
        l1$(030) = "6335863198C6B198C62,gL07318C71C8C63188C6319CE318CC6398CC630,gK01598C63"
        l1$(031) = "9C46318C46318CC718C66318C6631C,gK03DEC630E62318C62318C6618C63318C63318"
        l1$(032) = "C,gL0F76318E3118C63118C6378C63198C63198C0,V02N0463F18F3188C63188C6319F"
        l1$(033) = "E318CC7318CC60,V02N0E3198FF18C46318C46318CE3F8C66318C6638,U022N0318CFE"
        l1$(034) = "FFC62318C62318CE718CE3318C63318,U026M0118C6630C63118C63118C6318C73198C"
        l1$(035) = "631980,U066M038C6331863188C63188C6318C6318CE6318CC0,V02N0C63198C3D8C46"
        l1$(036) = "318C46318E6318E66318C670,T0138M046318CC71CC62318C62319C7318C7B3D8C6330"
        l1$(037) = ",T0118M06318C6638FE3118C63118D6198C6379CE63190,U06N0318C6330C63F88C631"
        l1$(038) = "88C630CC631BCC7F18C0,U0CM0198C631986319FC6318C4631866318C6631FC60,U06M"
        l1$(039) = "01CC6318CC318C67B18C62338E3318C63318C7E0,U02M0266318C6718CE319CC6311AC"
        l1$(040) = "7198C63198C6320,gI063318C6338C73188EF3188E618CC6318CC63180,R07P07198C6"
        l1$(041) = "318C6318C463F8C4630C66318C66318C0,P01318O098CC6318C6318C62318FE2318633"
        l1$(042) = "18C6331CC40,P0118CO08C66318C7319C63118C63F58E3198C6319FFE40,P038C6N01C"
        l1$(043) = "63318C6398E63188C6318CC718CC6318DE63C0,Q0C62O063198C630CC7318C46318C46"
        l1$(044) = "18C66318C663180,Q0E3O02318CC631862318C62318C6630C63318C7F31880,Q0318N0"
        l1$(045) = "318C66318C3138C63118C6311863199EE7198C,Q058CN018C63318C718AC63188C6318"
        l1$(046) = "8E318CD77D8CC6,:R0CO04C63198C638C66318C46318C4718C67B18C663,R068N04631"
        l1$(047) = "8CC630C62318C62318CE218C63398C6331,Q022O06318C6631863318C63118C6B10C63"
        l1$(048) = "798C63198,Q01P0318C63318C31C8C63188C631886318CC6318CC,R08O098C63198C71"
        l1$(049) = "8C46318C46318C631FC66318C67,gH0CC6318CC638C62318C62319C6118C63318C633,"
        l1$(050) = "gH0E6318C6630C67118C63118D6308C63198C6318,gH03318C6331863588C63188C731"
        l1$(051) = "846718CC6318C,gH0198C63198C318C46318C46318E2358C66318C6,gH08CC6318CC71"
        l1$(052) = "8C62318C62318C711CC63318C62,gH0C66318C6638CE3118C63318C6188C63198C632,"
        l1$(053) = "gH063318C6330C6B188C631BCE630C46318CC6318,gG017F98C631986398C46318E6FF"
        l1$(054) = "1862718C66318C,gG0118FC6318CC318C62318C6239FE31F8C63318C4,gG018C67F18C"
        l1$(055) = "6718C63118C67118C7F8EC63198C60,gH0C6331DEE338C63188C63788C619FC6318CC6"
        l1$(056) = "30,gH063198C63F8C7318C4631CC4630C63B18C6631C,gG01318CC6318FF318C62318C"
        l1$(057) = "6231863198C63318C,gH038C66318C73FCC63118C63118E3198C63198C0,gI0C63318C"
        l1$(058) = "639DE7F188C67188C718DC6318CC60,gI063198C630CF631FCC6398C4618C72318C663"
        l1$(059) = "8,gH02318CC631863318C6FB18C6230C63118C63318,gH0318C66318C3398C6319FC63"
        l1$(060) = "11863188C631988,gH018C63318C719CC63188CE3188E319C46318CC0,gI0C63198C63"
        l1$(061) = "8E66318C46B18C4718C62318C660,gI06318CC630C63318C62398C6218C63118C6330,"
        l1$(062) = "gH06318C6631863198C63118C6310C63188C63190,gH0318C63318C338CC63188C6318"
        l1$(063) = "86338C46318C0,gH078C63198C71AC66318C46318C631AC62318C60,gH04C6318CC638"
        l1$(064) = "C63318C63318C6118E63118C630,gG01E6318C6630C63198C63118C6308C63188C6310"
        l1$(065) = ",gG023318C633186318CC63198C631846318C463180,gG07398C63198C358C66318D46"
        l1$(066) = "318E2318C62318C0,gG09ACC6318CC71CC63318C72318C711CC63118C60,g018E66318"
        l1$(067) = "C6638C63198C63118C6188C63188C620,g04C63318C6330C6318CC63188C630C46318C"
        l1$(068) = "463,g0C63198C631986718C66319C4631862718C623180,Y016338CC6318CC358C6331"
        l1$(069) = "8C62318E3158C63118E0,Y0331AC66318C671CC63198C63118C718CC63188C40,Y0918"
        l1$(070) = "C63318C6338C6318CC63188C618C46318C46,W01B88C63198C6318C6318C66338C4630"
        l1$(071) = "C66318C623,W058C46318CC6318C6B18C6331AC6231863118C631180,X0C62358C6631"
        l1$(072) = "8C7398C63198E63118E3188C63188C0,V01C6311CC63318C639CC6318CC63188C718C4"
        l1$(073) = "6318C440,W063188C63198C630CE6318C66318C4618CE2318C62,V02318C46318CC631"
        l1$(074) = "86B318C6I38C6230C6B118C631,V0318C62718C66318C3998C6319CC6311863988C631"
        l1$(075) = "880,V058C63158C63318C718CC6318CC63188E318C46318C,U018C6318CC63198C638C"
        l1$(076) = "66318C66318C4718C62318C6,U01C6318C46318CC630CE3318C63718C6218CE3118C63"
        l1$(077) = ",U01E318C66318C6631873198C631D8C6310C73188C63180,V0718C63118C63318C318"
        l1$(078) = "CC6318CC631886318C4631880,V018C631FCC63198C718C66318C7E318C6318C62318C"
        l1$(079) = "80,W046318C67FB8CC639C63318C673BFC6119C63118C6,W02318C62318IF30D63198C"
        l1$(080) = "63598C77FCD63188C6380,W0318C63118C633BF7318CC6318CC631867F38C463180,W0"
        l1$(081) = "28C63188C63198F318C66318C66358E231IF631880,X046318C46318CC718C63318C63"
        l1$(082) = "31CC7118C633FFC,W022318C62318C6639C63198C63198C6188C63188E7,W03118C631"
        l1$(083) = "18C6330D6318CC6398CC630C46318C463,W0588C63188C6319C6318C66318C66718623"
        l1$(084) = "18C6231,W04C46318C46318CF318C63318C63358E311AC63118,W0C62318C62318C671"
        l1$(085) = "FFE7198CE319CC7188E63188C,V0163118C63118C6378C631FFC6B18CC618C46318C47"
        l1$(086) = ",V023188C63188C631CE6318C6F398C6630C62318C623,V0718C46318C46318C6318C6"
        l1$(087) = "3318C6331863138C6311,:U0398C62318C62318C7318C63198C63198E318AC63188,U0"
        l1$(088) = "B8C63118C63118C67BCC6318CC6318CC718C66318C4,T031C63188C63188C634EE6318"
        l1$(089) = "CE6318CE618C62318C63,S01886318C46318C4631C63318C73318C6F30C63318C630,S"
        l1$(090) = "02C4318C62318C62318C3198C63198C6399863188C6318,S0C6218C63118C63118C738"
        l1$(091) = "CC6318CC6318CE318C46318C,R016318C63188C63188C67AC66319C66318C6718C6231"
        l1$(092) = "8C6,R02319C6318CC6318CC634E66318D66318CE618C67318C62,Q01718C6318C66318"
        l1$(093) = "CE7FF863318C73318C6B30C63398C630,Q0318C6318C63318C72318C3198C63198C639"
        l1$(094) = "986318CC6318,P0718C6398C63198C63118C738CC6318CC6318CE318C66318C,P098C6"
        l1$(095) = "31CC6318CC63188C63AC66319C66318C6718C63318C6,O01CC631866318C66319C4630"
        l1$(096) = "C63318D63318CE318C67198C62,P066318C3318C63318C6231863198C73198C6B18C63"
        l1$(097) = "58CC630,O023318C6198C63198C63118C318CC6318CC6398E631CC66318,O03198C638"
        l1$(098) = "CC6318CC67188C738C66318C66318C7318C63318E,O058CC631C66318C663D8C463CC6"
        l1$(099) = "3319C63318C6198C63198C6,P0C6631863318C63318C6230C63198C63198CE30CC6718"
        l1$(100) = "CC62,O0463318C3198C63198C631186318CC6318CC731866358C6630,O063198C618CC"
        l1$(101) = "6318CCE3188C318C66318C66318E331CC63318,O0B18CC638C66318C67B18C4718C633"
        l1$(102) = "18C63318C7198C63198E,N0198C6631C63318C63318C623CC63198C6319AC618CC6318"
        l1$(103) = "CC6,N018FE331863198C63198C6310C6318CE6318CE630C66718C662,N01IFB98C318C"
        l1$(104) = "C6318FC631886318C66318C66318633D8C6330,O0D007CC618C66318C66318C4718C63"
        l1$(105) = "318C63318E319CC63198,R01E638C63318C67318C6358C63198C631B8C718CC6318CC,"
        l1$(106) = "S0F31C63198C63798C630CC6318CC6318EC618C66318C64,S07986318CC631BCC63184"
        l1$(107) = "2318C66318C6630C63318C630,S03CC318C66318E66318C6118C63318C6731863198C6"
        l1$(108) = "7FC,S01E618C63318C63318C7508C63398C63598E318CC67D8C,S01F38C63198C67198"
        l1$(109) = "C63CC4631BCC631IC718FE7F98C4,T0F9C6318CC6358CC630C62318E66319IF18CEFB1"
        l1$(110) = "8C60,T04C6318C6631CC6631863118C63319C6338C73198C630,T046318C63318E6331"
        l1$(111) = "8C7188C63199F631986318CC6318,T02398C63198C63198C758C46338CF6319CE318C6"
        l1$(112) = "6318C,T031CC6318CC6318CC63CC6231ADE6318CE718C63318C4,T01866318C66338C6"
        l1$(113) = "630C63118E73318C7318C63198C60,U0C3318C6331AC6331863188C7B198C6318C6318"
        l1$(114) = "CC630,U0E198C63198E63198C718C46318CC6318E6318C66318,U078CC6318CC6318CC"
        l1$(115) = "758C62359C66318C7318C63318C80,U03F66318C66718C663CC631DCD63318C6198C63"
        l1$(116) = "198C6,U01E3318C633D8C6330C6319CC73198C730CC6318CC63,V07198C6319CC633BL"
        l1$(117) = "F46318CC631866318C6630,V038CC6318CKFCC318C62318C66318E3318C63310,V02C6"
        l1$(118) = "6318C76318C6F18C63119C63318C7198C631990,W063318C63318C63B8C63188D63198"
        l1$(119) = "C618CC6318CC0,W03198JF98C6318C6318C47318CC730C66318C660,V0198CCE318CC6"
        l1$(120) = "318C6318C62318C6631863318C63,V01FE67319C66318CF318C63118C63318E3198C63"
        l1$(121) = "180,W0C7FF18D63318C6B98C63189C63198C718CC6318C0,W063198C73198C638CC631"
        l1$(122) = "8C56318CC618C66318C60,W0318CC6318CC631862318C63318C6730C63318C620,W018"
        l1$(123) = "C66318C66318C3118C63118C633987F798C6320,V018C63319C63318CF188C63198C63"
        l1$(124) = "198E31IC631C0,W0C63198D63198C6B8C46318DC6318CC758C6631880,W06318CC7318"
        l1$(125) = "CC638C62318C62318C661CC63318C,W0318C66318C6631863118C63118C6330C63198C"
        l1$(126) = "680,V0118C63318C63318C3188C63188C6319F6318C463,V038C63199C63198CF18C46"
        l1$(127) = "319C46318CF318C6230,V01C6318CD6318CC6B8C62318CE2318CEF18C63118,V01E318"
        l1$(128) = "C67318C6638C63118C73118C6B98C63188D,V03B18C63318C6331863188C77188C63F8"
        l1$(129) = "C6318C47,V0398EE3198C63198C318C47B18C463F8E6318DC23,V01CC7798DC6318CCF"
        l1$(130) = "18C62318EE2358C7318C7E11,V026631CC77F18C6638C63138C7311CC6198C62608,U0"
        l1$(131) = "1E3318FEIF8C6338C6318CC63189C630CC63200C,:U01F198C7F99DC631986318FC631"
        l1$(132) = "9C763186631F006,U0198CE6338CC6318CC318C62318CE7B18E3318C006,U038C66318"
        l1$(133) = "C663F8C6718C63118C63998C7198CE002,U03C63318C6331FC6738C73F88C63188C618"
        l1$(134) = "CC6B002,U03E3198C63198DE338C63D8C46318CC630C6631C,U03F18DC6318CC6F99C6"
        l1$(135) = "37CC62318C6A31863318C06,U0398C76318C6639FC731CC63118C63918E3199C602,U0"
        l1$(136) = "3BC63318C63318DE398C63188C63188C718CF6303,U01E63198C63198C63FCF6318C46"
        l1$(137) = "318C4618C66318180,V06319CC6318CCE31F72318C62318CE230C63319C080,V07F8D6"
        l1$(138) = "6318C66318FF118C63118C6B118631B8DE0,V07FC73318C63398C7388C63188C63988E"
        l1$(139) = "318EC620,U018F7F198C63198C678C46318C46318C4718CE6220,U0247B18CC6318CC6"
        l1$(140) = "34DE2318C62318C6219C6F3020,U03D038C66318C6631C77118C63118C6710D6F99C,U"
        l1$(141) = "018C6C63318C63318C3388C63188C6F09E6198C8,X0263198C631D8C719C46318C46K0"
        l1$(142) = "F4CC70,X0631BFC6318CC638D7E318C623L058E30,X03FCC6739FFE630C63318CEFFEL"
        l1$(143) = "01CF10,X038C63318C67318E338IF318CM02590,X06C63198C63598CB1BC66318C6M01"
        l1$(144) = "ED0,X046318CC631IC798E62318C62N0468,X03318C66318C6638C63118C631I031J03"
        l1$(145) = "0,X0318C63318C6330C63188C6319C00B0D80118,X03EC63198C67198E338C46318D70"
        l1$(146) = "31866818C,X03CE318CC6358CCB1EC62318C72398E3105C4,X016B18C66318C6798FE3"
        l1$(147) = "118C63118C7198EE0,3FFET01B98C63318C6338C6F188C63188C618F40B0,319ET0198"
        l1$(148) = "C63198C6318C6398C46319C4630C6201A,F88ET01CC6318CC6318C6318C62318CE2318"
        l1$(149) = "631C88,EC46T016E318C66338C7318E63118C63118E319BC0,E622U0BB18C6331AC639"
        l1$(150) = "CC73188C63188C718C7F0,E312U0998C63198E630CE63F8C46318C4618C66B8,318ET0"
        l1$(151) = "18CC6318CC63186331AC62319C6230C630B8,F8C6T01C66318C67F18C7198E6331FF63"
        l1$(152) = "1186318C0,2C62U067318C637B8C758CC77FFCC73188E3188C0,E632T033598C6379AC"
        l1$(153) = "63CC663B8C46318C4718C0C0,E31AT031IC6378CE630C6331AC62318C6218C60C0,F18"
        l1$(154) = "ET078C66378C6631863198E63118C6310C630,FDDES01CC63318C63318C718CCE3188C"
        l1$(155) = "E318E2340,1BFCS03C67FBEC63198C758C67B18C46B18C7,W026378CC6318DC63CC633"
        l1$(156) = "98C62398C6180,W03318C66318C7630C63198C6311AC630C0,W0319C63318C63318631"
        l1$(157) = "8DC631I86318,W018D63198C63198C318C76318C70318E,X0C7318CC6319CC738C6331"
        l1$(158) = "8C63808C0,X06318C66318DE63AC63198C631004,07EU0318C63318C7330F6318CC631"
        l1$(159) = "80,1E78T019C63198C673987B18DE6318C0,300CT0256318CC6378EC3D8C63318C70,3"
        l1$(160) = "004T023318C66319C773FC7F198C630,2004T07118C63318CE3BAF6738CC6310,2004T"
        l1$(161) = "0998C63198D671CE6B99E663188,300CS018CC6318CC6378C73D8C73318C4,1C1CT0C6"
        l1$(162) = "2318C67B1AC731FC63D98C62,0C1T0463118C63318E639CE631FCC631,V01E3188C631"
        l1$(163) = "B8C630DE6318E66318,0F8T0318C46318EC631873B18C73318C80,3BCS0118CE2318CE"
        l1$(164) = "6338C31D8C63B98C680,326P01EFD8C6B118C7B318C718CC631FCCFF40,I2P07F18C63"
        l1$(165) = "988C63198E639C66318CFF31A0,326P0FD84633CC46358CC630CE3318C73318D0,3BCO"
        l1$(166) = "0318C2318E6231CC6631863398C63198C70,0B8O0D8C6118C6311FC6I38C319CC6338C"
        l1$(167) = "C638,Q03CC6388CF3F88C6319EC718DE6319C6633C,Q07C631C7F3FFCFE318CC638C6F"
        l1$(168) = "318C6331AE08,:3FEN066318663B00FF318C6E30CE31D8C73198E6B0,006N0F318C311"
        l1$(169) = "J0FD8C633186B18FC6718CC6318,006J02003118C618CJ03CC63198C398C66318C6631"
        l1$(170) = "8E,002J0200588C638DK01C6318CC718C63B19C63318C7,002J03004C4631C6L0F318C"
        l1$(171) = "6638C631D8D63198C61,1FF8I0100C6231868L0318C6730C6318DC7318CF630,3FFCI0"
        l1$(172) = "181E3118DFM018C635986718C6E318C67318,202L0F3188C7CM01C6318CC358C63738C"
        l1$(173) = "63318F,N01F18C478O06319C671CC6319FC63198C7,3FE4K098C622P0318F6338C6318"
        l1$(174) = "CEE318CC61,3FE4J058C631CP01CC6318C6318C67B18C6630,N03C631ER0E6718C6318"
        l1$(175) = "C63B98C63B19,N01E318CS0398C7338C631F8C63198E80,006L0E58ET058C639EC6318"
        l1$(176) = "CC6318CC741,3FFCgI03C630CE6318CE6318CE61A780,0064gI01631863318C73718C6"
        l1$(177) = "730D640,0024gI01318C3198C7B1D8C631986310,gM0118C719CC6378CC631CCE3188,"
        l1$(178) = "gM01EC638D663F8C66318C6718C6,3FE4gJ01630C7671ECE6318C6618C63,gO0B18633"
        l1$(179) = "5AIF318C6330C631,gO078C339DEFBFB8C63398631880,0F8gL03E71ACEJFCC631ACE3"
        l1$(180) = "18C80,1FCgL01679CIF3ECF6318C6718C6,326gL017FDEB80FC7FB18DE318C63,I2gL0"
        l1$(181) = "1JF901EF3F99E6318C63180,I2gL01E43797FE338CE7F18E6318C0,3BEgM0E737C1FF9"
        l1$(182) = "CC66358C7318C40,1B8gM023F7001DE963B1CC6198C640,gP010DCI07F13198C630CC6"
        l1$(183) = "380,gQ0818K0318CCE31867F980,1FCgN042M0CC7FB18E3318E0,306gW07E3F18C7798"
        l1$(184) = "C60,202gX070BCFF18CC620,202h0DE670C663,306h076358E33180,3FFCgY0731CEF3"
        l1$(185) = "98C0,hI0398C7FCFE60,hI038C618C6678,hI011630C633DE,hJ0D33863198E,hJ0E9A"
        l1$(186) = "E718CFF,hJ06CE758C7F3C0,hJ02FE1CC6F319F80,hK0730E6F198C634,hK03F87318C"
        l1$(187) = "C631880,hK03CF378C66318C60,hK01E7E3C63318C660,hK01F62073198C63,hK01580"
        l1$(188) = "0198CC631C0,hL0CJ0EC66318E0,hK016J0663318C60,hL068I023198C620,hL038I03"
        l1$(189) = "18CC6320,hR07C6631C0,hR0763318,hR0F31980,hR0498D80,hR068C6,hS0C66,hS06"
        l1$(190) = "38,hS07,hS03C0,hS01C0,"

        l2$(001) = "^JO"
        l2$(002) = "^XA^EG^XZ" 
        if ctrlflag$ = "Y" then ~
           l2$(002) = "^XA^EG^XB^XZ"             /* CR1051 suppress start */
        l2$(003) = hex(7e) & "DGLABEL2.GRF,12183,31,"
        l2$(004) = "hR018,R010,,::R020,,R040,R060,,:R060,:R020,:"
        l2$(005) = "R030,R020,R030,,R010,R018,S08,S04L040,S06L0C0,S06K01A1,S03I01C3A280,S0"
        l2$(006) = "1800FB2E680,T0C019D27F80,T0C01CF319E0,T0603E7B98F0,T070733BEC70,T07071"
        l2$(007) = "9EE630,T03858CE7318,T01IC673988,T01E66339CC8,U067319CE64,U07398CE73C,U"
        l2$(008) = "019CC6739C,V0CE6339C8,V077319CE8,V01B98CE78,V03FCC6738,V03EE63390,V01F"
        l2$(009) = "7319C0,W0F398CE0,W0F9CC670,W03CE6330,W02E73190,W027398D0,W0739CC60,W0B"
        l2$(010) = "9CE620,W09CE7320,W0CE739A0,W0E739CE0,V01KFC0,V018,W08,,::::::hJ038,gY0"
        l2$(011) = "807F1C4,h0180E03,gY03EJ01C0,gY07L033FA,gW07F8M01FF80,gU07C87EM01CCE0,g"
        l2$(012) = "T01878F6M01E630,gT0EJ04M01F33C,gS03K0CM03399F,gP063DEK08M039CCFE0,gP0C"
        l2$(013) = "43L0CM03CE63FC,gP0C4M08M02E731BF80,gP0CV067398CFE0,gN020C8U0739CC63F8,"
        l2$(014) = "gP09V079CE6319FE0,W018Q09V04CE7318CIFB3E0W018Q03V0C67398C67KF8gN010CV0"
        l2$(015) = "E339CC633DFC630gP08V0B19CE6319CF6360X04N0601W098CE7319DE731C0X0EN0461V"
        l2$(016) = "018C67398FF739880X0AO04EV01C6339CC7F3BCD80W011P08P0FCJ016319CE6719FE78"
        l2$(017) = "0X01P08O03FFCI03318CE7378CFF3,X0FO03P0739FC00398C6739EC67FF30gN04O01F9"
        l2$(018) = "CCFC03CC6339CE633FF70gM018O039CE63FC2E6319CE6319IFCgL07EP0ECE7318FE731"
        l2$(019) = "8CE7718CIFCgL08P03C67398C6F398C673D8C67FFCgL08P076339CC6379CC6339CC633"
        l2$(020) = "9CCgL08O01E319CE631DCE6319CE6319CECgL08O03318CE7318CE7318CEF318CE7CgL0"
        l2$(021) = "8O0F98C67398CE7398C67B98C6738gL08N019CC6339CC6B39CC6339CC63398gL08N07C"
        l2$(022) = "E6319CE6399CE6319CE6319D8gU0EE7318CE7318CE7318CE7318CF0V038V03E7398C67"
        l2$(023) = "399C67398C6F398C670V068M01N07339CC6339CD6339CC63B9CC6330V024M01M01F19C"
        l2$(024) = "E6319CE7319CE631DCE631B0V02CM01M0778CE7318CE7318CE7318CE7318E0V07N01M0"
        l2$(025) = "C6C67398C673B8C67398CE7398C60V06N01L03866339CC6339EC6339CC6F39CC660gK0"
        l2$(026) = "5L0607319CE6319CE6319CE6399CE6360gK0FK01C0798CE7318CE7318CE7318CE731C0"
        l2$(027) = "gK0978I0300DCC67398C67398C67398C67398C0gJ011CCI0C00CE6339CC633FEC6339C"
        l2$(028) = "DE339IC0U018M01007C01800E7319CE6319CFE319CE7319CE6C0U018M02I061FI0F398"
        l2$(029) = "CE7318CE73F8CE7318CE7380U058M04I02F3FE0B9CC67398C67399EE7398C673980T01"
        l2$(030) = "D8M04I03800319CE6339CC63F9CC7BB9FC6339D80U0D8M08M031CE7319CE631DCE631D"
        l2$(031) = "CE6319CF80U018M08M061E7398CE7318CE7318EE7318CE780T066M01N061F39CC67398"
        l2$(032) = "CE7398C7F7F8C673,T06AM02N03FF9CE6339CC7B39CC63FDFFE339,T01CM02O03FCE73"
        l2$(033) = "19CE6399CE631BCE7F99F,T024M04P04FF398CE7318CE7318CE731FCF,T02N04M0C004"
        l2$(034) = "F7FCC67399C67398C67398CFE,T024M08L03F00C79FFE339CD6339CC6339CC63E,T028"
        l2$(035) = "L01M06F808DCE7FF9CE7319CE6319CE631E,Q01E018L01M0E7808CE739FEE7318CE731"
        l2$(036) = "8CE7318E,P03E36N02L01F3C09E739CC7FBB8C67398C6739FC6,P0401AN02L0339F1F3"
        l2$(037) = "39CE633FEC6339CC6339IF4,P04002N04L079CIF19CE7319CE6319CE6319FE63C,P040"
        l2$(038) = "04N08L0CCE7398CE7398CE7318CE7318CE731C,P02004N08K01C6739CC6739CC67398C"
        l2$(039) = "67398C7F398C,P01008N08K016339DE6339CE633FCC6339DFE739IC,Q0808M01L03319"
        l2$(040) = "CF7319CE7319CE6319CF7FF9CE68,Q081N01L0718CE7398CE7398CE7318CE7B38CE738"
        l2$(041) = ",Q083N01L0D8C6739CC6739CC67398C67798C67398,Q044N01K01CC631BCE6339CE637"
        l2$(042) = "9CC637FCC6339D8,Q058N02K03E6318EE7319CE731DCE631FCE6319CF8,Q03O02K0673"
        l2$(043) = "18C67398CE7398CE731FCE7318CE70,gG02K0F398C6339CC6739CCE7398E67398C6730"
        l2$(044) = ",gG02J01B9CC6339CE6339CE6B39CC6339CC63390,gG02J019CE631ECE7319CE7399CE"
        l2$(045) = "6719CE6319F0,gG02J03CE7318E67398CE7398CE73D8CE7318CF0,gL0667398C6339CC"
        l2$(046) = "6739CC6739CC67398C660,gG04J0E339CC6719CE6339CFE339CC6339CC6320,gG04I01"
        l2$(047) = "B19CE6358CE7319DFF319CEE319CE631A0,gG07F80318CE731CC67398CF7FF8CE7F18C"
        l2$(048) = "E7318E0,gG040IF8C67398C6339CC6739FFE73F8C67398C60,gG04001FFE339CEE319C"
        l2$(049) = "E6339CE7FF9EC6339CC660,gG04I0E7FF9CE7F18CE731BCE7319FE6319CE6340,gJ01F"
        l2$(050) = "31FFE7398C67398EE7398CE7B18CE731C0,gG06001398C67FDCC6339CC6739CC67398C"
        l2$(051) = "67398C0,gH08019CC6339FFE319CE6739CE6339CC6339IC0,gH0801CE7719CEIF8CE73"
        l2$(052) = "39CE7319DE6319CE6C0,gH0801E77F8CE7B9CFF739ECE7398CF7318CE73C0,gH0800F3"
        l2$(053) = "C3C6739CC63FFCE6739CC67398C673980,gH08007F01E339CE6319FFE339CE6339CC63"
        l2$(054) = "39C80,gN01B19DE7318CE7719CE7319CE6319CE80,gG01L0198CF7398C673D8CE7398F"
        l2$(055) = "E7318CE780,gG01M08C6739CC6339CC6739CC67398C67380,gG01M0C6319CE6319CE63"
        l2$(056) = "39CE6339CC633980,gG01M0E319CE7318CEF319CE7339CE6319D,gG01CK01B18F67398"
        l2$(057) = "C67F98CE739ACE7318CF,gG01CK0198C7339CC6339CC6739CE67398C67,gG06CK01CC6"
        l2$(058) = "319CE6319CE6339CE6339CC633,gG088K01E6338CE7318CE7319CE7719CE6319,g0108"
        l2$(059) = "K03731AC67398C6F398CE73F8CE7318F,g0608J0FF398E6339CC63B9CC6739CC67398C"
        l2$(060) = "7,g0C08J0F39CC6319CE6319CE6339CE6339CC63,Y01008I0199CE6318CE7319CE7319"
        l2$(061) = "CE7319CE632,Y02008I018CE7338C67398DE7398CE7798CE731A,Y04008I01C6739EC6"
        l2$(062) = "339CC7339CC673DCC67398E,Y0C008J0E339CE6319CE6319CE6339CE6339CC6,X07001"
        l2$(063) = "8J0719CE7318CE7318CE7319CE7319CE66,W0E80018J038CE7798C67399C67398CEF39"
        l2$(064) = "8CE736,V01J01L0C673DCC6339CD6339CC67B9CC6739E,V03J01L0E339CE6319CE7319"
        l2$(065) = "CE6339CE6339CC,V04J01L0719CE7318CE7318CE7319CE7319CE4,V04J01L058CE7398"
        l2$(066) = "C67398C67398CE7398CE74,V08J01L04C67FDCC6339FC6339CC6F39CC673C,U01K01L0"
        l2$(067) = "4633C6E6319CE6319CE63B9CE6339C,U01K01L0631BC27318CE7318CE7319CE7319CC,"
        l2$(068) = "U02K01L0718E43398C67398C67399CE7398CEC,U04K01L078DCC19CC633BCC6339CDE7"
        l2$(069) = "39CC67C,U02K03L03FF0C1CE6319EE6319CE7B39CE633C,U018J03M0FC081E7318CE73"
        l2$(070) = "18CE7399CE7319C,V06J03FEN081F398C67FF8C67398CE7398C8,V01L07FFCK08139CC"
        l2$(071) = "6339JF39DC6739CC68,W08N0IFE00839CE631BCE67JF6339CE638,W08Q07FF86CE7318"
        l2$(072) = "EE7338CJF9CE7318,W08S098C67398C6739BC6739JF3998,W08S088E339CC6739CE633"
        l2$(073) = "9CF677FFD8,W08S088B19CE6379CE6319CE7339CEF8,V018S08898CE731CCE7318CE73"
        l2$(074) = "19CE738,V01S01I8C67398C673B8C673B8CE7398,V01S0198C6339CC6339EC6339EC67"
        l2$(075) = "39D8,V03S011JF9CE6719CE6319CE6339CF8,V06S0118F1KFD8CE7318CE7319CE70,U0"
        l2$(076) = "1T01106C633DCC633FCC633BCC67390,U02T011026319CE6319EE6319EE6339D0,T018"
        l2$(077) = "T01103318CE7318CE7318CE7319CF0,T0CU0110398C6F398C67398C67398CE70,S03V0"
        l2$(078) = "1101CC63B9CC6379CC6339CC6730,S04V01101E6319CE631DCE631BCE63390,R018V01"
        l2$(079) = "100F318CE7318CE7318EE7319D0,R02W01100F98CE7398C67398C67398CF0,R04S01F0"
        l2$(080) = "0110058C6F398C67398C67398C670,Q0F8S037IF1006C63B9CC6379CC6379CC6330,P0"
        l2$(081) = "3U06J010066319CE631DCE631DCE631B0,O01CU04J01003318CE7318CE7318CE7318F0"
        l2$(082) = ",O03V04J0100398CE7398C67398C67398C70,O02V0CJ01001CC6B39CC6739CC6739CC6"
        l2$(083) = "20,O06U018J01001E6399CE63D9CE6359CE6320,O04U01K01I0F318CE731CCE731CCE7"
        l2$(084) = "31A0,O04U03K03I0F98C67398C67398C67398E0,O08U03K03I07CCE339CC6339CC6339"
        l2$(085) = "CC60,gK02K03I02E7B19CE6719CE6719CE620,N01V03K03I037398CE73D8CE7358CE73"
        l2$(086) = "20,N01V03K02I01B98C6739CC6739CC6739A0,N01V0EK02J0DDC6339CC6339CC6339CE"
        l2$(087) = "0,N03V0CK02J06FE319CF6319CE6319CE60,N023FCS0CK02J027318CE7318CE7718CE7"
        l2$(088) = "20,N03IFER0CK02J03398C67398C673D8C673A0,N03F01ER08K02J019CC6339CC6339C"
        l2$(089) = "C6339E0,R07Q018K02K0DE6319DE6319CE6319CE0,R018P018K02K077318CF7318CE73"
        l2$(090) = "18CE60,S0EQ0CK02K07398C67398C67798C6760,S06Q08K02K039CC6339CC633DCC67F"
        l2$(091) = "E0,S078O018K02K03CE631FCE631FCE77F9E0,S038O018K02K03E7318IF318FEIF8CE0"
        l2$(092) = ",S018O03L02K037398DNFB98C660,T08O018K02K0339CCFB3DCEF339CC6320,T0CP08K"
        l2$(093) = "02K039CEFB19CE6319CE631A0,T04P08K02K03CEFB18CE7318CE7318E0,T04P08K02K0"
        l2$(094) = "37FF98C6F398C67398C60,T02P0CK02K037B9CC63B9CC6339CC660,T02P08K02K07F9C"
        l2$(095) = "E6319CE6319CE6360,T01CO0CK02K078CE7318CE7318CE731E0,U0CO0CK02J01FCE739"
        l2$(096) = "8CE7398C67398FC,U07CM018K02J07E6B39CC6F39CC6339CC6C,U03EM03L02J0F6399C"
        l2$(097) = "E6399CE6319CE7F8,U01CM03J03LFB318CE7318CE7318CE73C0,U018M07KF2K0398C67"
        l2$(098) = "398C67398C6739,V08M06K02K03CCE339CCE339CC6339D,V08M06K02K02E6B19CE6F19"
        l2$(099) = "CE6319CF,V04J0IFCK02K027398CE7398CE7318CE7,V07I07804L02K03398C6739CC67"
        l2$(100) = "398C672,V05FC1E004L02K039CC6339CC6339CC633B,V0247FI04L02K03CEE319CEE31"
        l2$(101) = "9CE6319F,V0201CI04L02K02E7B18CE7B18CE7318CF,gI04L02K027398C673D8C67398"
        l2$(102) = "C67,V04L04L02K0339CC6339CC7FF9CC636,V04L04L02K039CE6319CE67BDCE631E,V0"
        l2$(103) = "4L04L02K02CEF318CEF378CEF319C,V04L04L02K0267B98C67F9CC67F98CC,V08L04L0"
        l2$(104) = "2K0I39CC633DCC6339CC78,V08L04L02K0319CE6319CFE319DE638,V08L04L02K038CE"
        l2$(105) = "7318CE7318CFF330,V0EL04L02K02C6F398C67798C67B9B0,V0EL04L02K0267B9CC63F"
        l2$(106) = "DCC633DCF0,V0EL04L02K077F9CE633FCE6319C678,V0E00FI04L02K07F9CE731FCE73"
        l2$(107) = "18FE338,V0600FE004L02K0DCC77B9BC67398C7E390,V0800878047K02J01DE63F9CE6"
        l2$(108) = "339CC6361D0,U03800C1E7FFEJ02J01E7319CEE319CE63320F0,U03I0C07F11F8I02J0"
        l2$(109) = "3F398CE7F18CE731F00F0,U07I0C018003CI03J0339CC673D8C67398D8070,U0EI0CL0"
        l2$(110) = "F8003J019CE633FCC6339CCE8060,U0EI04L01C033I09FEE7319CE6319CE7BC030,U07"
        l2$(111) = "I04M0743F001E277398CE7318CE739C0,U07I04M03F36003601B9CC67798C67398E060"
        l2$(112) = ",U06I04M023FI02I0DCE633DCC6339DC7060,U07E004M0207C004I0CE7319CE6319CF6"
        l2$(113) = "3830,U038004M0203F8FCI0E7398CE7318CE731818,U018C04M0200198J0F39CC67398"
        l2$(114) = "C6739FC08,U01FE04M02001F8J0B9CE6379CC633BCFE,U03FE04M020017CI019CE731D"
        l2$(115) = "CE6319EE6E,U0FBFFCM0300104I01CE7398CE7318CE726,U079F8N030010CI036739CC"
        l2$(116) = "6739BC7F3E2,U07418N030010FI0I39CE673DCIFB9C0,U02398N010010DI0719CE73JF"
        l2$(117) = "7FF9D80,V01D8N0100101F0078CE73FF80BIFCF,W01807IFC003001C0BF04C6739EJ01"
        l2$(118) = "FBE7,W01FFE003JFI0C0138I7IFL03CF3,W01CN01I04018JF9CFM06FB,W01EN01I0401"
        l2$(119) = "83798CE7M01EF80,W017N01I0401805CC6738M0E780,W01EN01I0401004E6339C003F8"
        l2$(120) = "0073C0,X086M01I04018067319CF80IFD011C0,X08EM01I0403007398CEIFCCEF838C0"
        l2$(121) = ",X0F2M01I04030039CC67BBCC67395C60,X0C2M01I0403E03CE6339CE633FCFE60,IFC"
        l2$(122) = "T062M01800400602E7319CE7319FE6F60,E73CT062M018004004027398CE7398CFFC9E"
        l2$(123) = "0,F39CT042N0800400C0339CC6F39CC673F9A0,D9CCT042N08006007039CE63F9CE633"
        l2$(124) = "9FD80,CCE4T062N08006001C3CE731DCE7319CFF80,C674T062N08006I07167398CE73"
        l2$(125) = "98CE7BC0,E33CT063N08002I011339CEE739CC676F80,F19CT063M078002I01I9JF39C"
        l2$(126) = "E633CE,D8CCT081L03C8002J0IFE73D9CE73198C,CC64T0C1K03E08002J0FE6739CCE7"
        l2$(127) = "398D8C,C634S01C1J03E008002J01E339CC673FCC78C,E31CS01C1I03EI08002J03B19"
        l2$(128) = "CE6339EE6F80,IFCS0701070EJ0C002J0798CE7719CEIF,W0C01IF8J04002J0D8C673D"
        l2$(129) = "8CE79F4,W0803802K04002J0DC6339FC673C,W0C06N04002J096319CFE339C,W0C04N0"
        l2$(130) = "4002I019318CEDF19CC,W0604N04003J0998C67C1CCF8,W0304N07003J0BCC633E067E"
        l2$(131) = "0,W0184N078018I0BE6319C07E,1FEU084N048018I0F7318D80,3FF8T084M016F81CI0"
        l2$(132) = "F398C780,E018T084M01E3816001B9CC6380,E01CT084M0260E1381F3CE631C0,C00CS"
        l2$(133) = "0104M0630310831BE7318C0,E01CS0204M06303184E0FF398CE0,F038S0C04L0181831"
        l2$(134) = "87C01F9CC670,3878S0806L030080881801FCE6310,182S03006L0400808838003E731"
        l2$(135) = "98,V06006L0C0080E86I01F398CC,3F8S06002K01800803847C01F9CC64,F7CQ041C00"
        l2$(136) = "2K03I080082E600FCE63E,E4CP07FFI02K06I080082F301LF,C4CO01F8J06K0CI0C008"
        l2$(137) = "3B9CF67FD8F80,E4CO03F8J0F8I01J0C00839CFE3F9CC780,F78O041K068I03J0400C1"
        l2$(138) = "CE6319CE63C0,17O018M08001EJ0C00C0E7319CE733E0,Q078K03FFC01J0180040F398"
        l2$(139) = "DE7399F0,FFCN0FCJ0KFEFJ03I0407DCC7B39CF78E0,FFCN0CJ0183C0BFCJ02I04007E"
        l2$(140) = "6399CE73FE0,00CJ04003AL0EI07FJ02I04003F3F8CE731FF0,00CJ0400EL018I01FJ0"
        l2$(141) = "3I04I0F9CC67398C70,004J04008L0EK078I01I04I07CEE339DC630,00CJ06018K018K"
        l2$(142) = "03CI01I04I07E7F19CFE310,IF8I0301L0AM0CI01I06I01F3D8CE7B190,IF8I0303K03"
        l2$(143) = "CM0FI01I02J0F9CC67398D0,E0CJ031EK0F8M07I02I02J03FC6339CC70,M0138J03AN0"
        l2$(144) = "3I06I02J01FE319CE630,FFD8I013K0EO01800CI02K03F18CEF310,FFD8J0FJ038P070"
        l2$(145) = "08I02K06F8C67F998,N07J0EQ0381J03K05EC633DCD8,004K0380018R063J01K076631"
        l2$(146) = "9CE7C,IFK01F81ES016J01K063318CE73E70,IF8K02EV0CJ01K0E398C6739BFC,00DCg"
        l2$(147) = "I04J01K0C3CC6379CFFE,004CgI04J01I01D83E631BCE63780,gM04J018007F83F318E"
        l2$(148) = "E7318C0,FFD8gI078J08207801B98C67398C60,FFD8gJ0CJ082CF67198C67398C670,g"
        l2$(149) = "N028I082DIF9CC6339CC6338,gN018I083B7FFDE631BCE63198,3FgM0CD81CMF318FE7"
        l2$(150) = "318C8,FFCgL0598E5FF9B9FBD8C67398C6C,E4CgL05F8C5E87BBFIC6F39CC63C,C4CgL"
        l2$(151) = "07FCE498FFE7C7F399CE631C,E4CgL07F8F6BFE61C673F8CE7318C,F7CgL0393BF1FE6"
        l2$(152) = "2C379BC67398C4,37gN0DBFC00F7481DCE6339CC6E,gP066FI07FB807E6319CE7FE,0E"
        l2$(153) = "gN023CK01803FF18CE7FDA,3F8gM01FM0601FD8C67798E,F1CgV03D9FEC7FFDCC7,C0C"
        l2$(154) = "gW07F7JFDCE63,E0CgY05FB79CE733,IF8gX06F9ECE7399,IF8gX02FCE6FF9CD,hH035"
        l2$(155) = "E63FDFE780,hH015F318CE77E0,hI0ABB8C673FF0,hI0EFDCE339CF0,hI07CF6719CFF"
        l2$(156) = "8,hI03E7358CFF3F,hI017F9CC7F39BFF80,hJ079CE7F39CCE7F0,hJ03FE7F99CE631B"
        l2$(157) = "E,hJ03E7FF8CE7318CF,hJ01F3FFE67398C66,hK0FF60FF39CC63C,hK0FE003F9CE631"
        l2$(158) = "E,hK0F8001FCE7318E,hK0F6I0F67398C7,hK03EI07339CC63,hK03CI07F9CE632,hK0"
        l2$(159) = "38I03FCE731E,hQ0FE739FC,hQ0FF39FC0,hQ07D9FE,hQ07CCE0,hQ01E6E0,hR0E3E0,"
        l2$(160) = "hR073,hR03F,hR01E,"
        l2$(161) = ","
        l2$(162) = ","
        l2$(163) = ","
        l2$(164) = ","
        l2$(165) = ","
        l2$(166) = ","
        l2$(167) = ","
        l2$(168) = ","
        l2$(169) = ","
        l2$(170) = ","
        l2$(171) = ","
        l2$(172) = ","
        l2$(173) = ","
        l2$(174) = ","
        l2$(175) = ","
        l2$(176) = ","
        l2$(177) = ","
        l2$(178) = ","
        l2$(179) = ","
        l2$(180) = ","
        l2$(181) = ","
        l2$(182) = ","
        l2$(183) = ","
        l2$(184) = ","
        l2$(185) = ","
        l2$(186) = ","
        l2$(187) = ","
        l2$(188) = ","
        l2$(189) = ","
        l2$(190) = ","
   

        l3$(001) = "^JO"
        l3$(002) = "^XA^EG^XZ" 
        if ctrlflag$ = "Y" then ~
           l3$(002) = "^XA^EG^XB^XZ"             /* CR1051 suppress start */
        l3$(003) = hex(7e) & "DGLABEL3.GRF,12193,31,"
        l3$(004) = "S08,R010,,::::R040,,::R040,R020,::R030,R020"
        l3$(005) = ",,:R010,S08,:S04,S04L040,S02K01,S03I018182,S01800382C2,T08008D06380,T0"
        l3$(006) = "C00C7201C0,T0600E3B18C0,V06313AC70,T070718C4630,T02858C62318,T0144C631"
        l3$(007) = "188,U0A663188C0,U046318C460,U02318C6234,U0118C63118,V08C631I8,V066318C"
        l3$(008) = "48,W0318C620,W018C6318,V01EC63180,V0166318C0,W02318C60,W0B18C630,W038C"
        l3$(009) = "6310,W02463180,W022318D0,W07118C60,W0988C620,W08C463,W0C623180,W0E3118"
        l3$(010) = "C0,W0739IC0,W08,:,::::::hJ020,hG046084,h01J02,gY0280,gY04L01270,gW0438"
        l3$(011) = "N09F80,gV0C85CN08CE0,gV020C6M01C630,gT04J04M016338,gY04M01318F,gQ030CK"
        l3$(012) = "08M0198C7C0,gP044M08M03CC63B8,gP04V0266319E,gP08V023318C7C0,gN020C8U07"
        l3$(013) = "198C6378,gP08V058CC6319F20,W01R01V04C66318C7FF01E0X08Q02V0463318C63JF7"
        l3$(014) = "0hM0E3198C631DDC630hM0B18CC6319CC6360gM0601W098C66318DE631C0gN06W018C6"
        l3$(015) = "3318FF331880gN04W01C63198C631B8C80X01P08P0F8J016318CC6718FE7,gO08O0167"
        l3$(016) = "8I01318C66358C7F3,gN02P0731F800318C6331CC63FF,gN04O01998C78038C63198E6"
        l3$(017) = "31FF70gN08O038CC63382C6318CC6319IF0gL07Q04C66318FE6318C66718CIFCgW01C6"
        l3$(018) = "3318C67318C633D8C63FFCgW0663198C63598C63198C6319CCgW0E318CC631IC6318CC"
        l3$(019) = "6318CC4gV03318C66318C66318C6E318C668gV0718C63318CE3318C63B18C6338gU018"
        l3$(020) = "8C63198C6B198C63198C63198gU03C46318CC6398CC6318CC6318D0gU0C62318C66318"
        l3$(021) = "C66318CE6318C70V01W0363118C63318C63318C6B318C630W08V063188C63198D63198"
        l3$(022) = "C63998C6330V024V0B18C46318CC7318CC6318CC631A0V028U0278C62318C66318C663"
        l3$(023) = "18C66318E0V04V0C4C63118C63318C63318CE3318C60gR010463188C6319AC63198C6B"
        l3$(024) = "198C620gR0606318C46318CE6318CC6398CC6340gK01L080718C62318C66318C66318C"
        l3$(025) = "6631C0gK0978I030058C63118C63318C63318C63318C0gK01C4I0400CC63188C631BCC"
        l3$(026) = "63198C63198C40V08P03801800C6318C46318CFE318CC7318CC680gJ02I040EI0E318C"
        l3$(027) = "62318C663F8C66318C66380V08Q0273FE0918C63118C67318CE3318C633180U04R018J"
        l3$(028) = "088C63188C63198C73198C6319880U0DN08N01C46318C46318CC6318CE6318CD,V08U0"
        l3$(029) = "21E2318C62318C66318E66318C67,T046M01N0613118C63118CE3318C7B3D8C633,U0A"
        l3$(030) = "V01DF88C63188C6B198C6379CE6319,U04X0FC46318C46318CC631BCC7F18D,U04M04P"
        l3$(031) = "04FE318C62318C66318C6631FC6,gQ0C00473D8C63119C63318C63318C7E,U04M08L03"
        l3$(032) = "FI058CE63188D63198C63198C636,gP06F0084C47798C47318CC6318CC631A,R0801U0"
        l3$(033) = "E7808C6231FC62318C66318C66318E,P01802N02L01B3C09E3118C7F118C63318C6331"
        l3$(034) = "CC4,gO0319F0F3188C631FAC63198C6319FFE4,P04Q04L078CFFB18C46318C66318CC6"
        l3$(035) = "318DE63C,P04004U0CC66398C62318C62318C66318C6631C,gO0C63118C63118C63318"
        l3$(036) = "C63318C7F3188,P01W0163189C63188C63188C63199EE7198C8,Q08V02318C56318C46"
        l3$(037) = "318C46318CD77D8CC68,gG01L0718C63318C62318C62318C67B18C6638,gN0D8C63118"
        l3$(038) = "C63118C67118C63398C63318,R04T018C63198C63188C63588C63798C631980,gM03C6"
        l3$(039) = "318E46318C46318C46318CC6318CD0,Q03U066318C62318C62318C6231FC66318C670,"
        l3$(040) = "gM07318C63118C63118CE3118C63318C6330,gM0998C63388C63188C6B188C63198C63"
        l3$(041) = "190,gL018CC631AC46318C46398C46718CC6318D0,gL03C66318C62318C62318C62358"
        l3$(042) = "C66318C60,gL0463318C63118C63118C6311CC63318C620,gL0E3198C67188C63198C6"
        l3$(043) = "3188C63198C6320,gK01B18CC6358C46318DE7318C46318CC631A0,gG05F00118C6631"
        l3$(044) = "CC62318C737F8C62718C66318E0,gI03E38C63318C63118C6311CFE31F8C63318C40,g"
        l3$(045) = "K0FC63198C63188C63388C63F8EC63198C640,gK0C77B8CC6318C4631AC46319FC6318"
        l3$(046) = "CC6340,gJ01E318FE6398C62318E62318C63B18C6631C0,gG04001318C63F18C63118C"
        l3$(047) = "63118C63198C63318C0,gH080198C6319FE63188C63188C63198C63198C40,gJ01CC63"
        l3$(048) = "18CEF3F8C46338C46318DC6318CC6,gJ01E67F8C67B18FE631CC62318C72318C66380,"
        l3$(049) = "gK0F3C3C63198C637D8C63118C63118C633180,gK03E016319CC6318CFE3188C63188C"
        l3$(050) = "6319880,gO0B18CE6318C46718C46319C46318CC80,gO098C73318C62358C62318C623"
        l3$(051) = "18C6780,gO08C63198C6311CC63118C63118C63380,gO0C6318CC63188C63188C63188"
        l3$(052) = "C6319,gO0E319C66318C46318C46338C46318D,gG018K01B18D63318C62318C6231AC6"
        l3$(053) = "2318C7,gG01M098C63198C63198C63118E63118C63,gG04L018C6318CC63188C63188C"
        l3$(054) = "63188C631,gN01C6318C66318CC6318C46318C46319,g0108K03631AC63318C6A318C6"
        l3$(055) = "2318C62318F,g0208J07F318E63198C63918C6311CC63118C6,gH08J0B198C6318CC63"
        l3$(056) = "188C63188C63188C62,Y01008I0198CC6318C66318C46318C46318C4632,Y02008I018"
        l3$(057) = "C66338C63318CE2318C62718C6231A,Y04008J0C6331AC63198C63118C63158C63118E"
        l3$(058) = ",gM0E3198E6318CC63188C6318CC63188C6,X02N0718CC6318C66318C46318C46318C4"
        l3$(059) = "62,W04O018C66318C63319C62318C66318C6230,V01Q0C63358C63198D63118C63118C"
        l3$(060) = "6311C,gG01L0E319CC6318CC73188C63188C63188C,V04J01L0718CE6318C66318C463"
        l3$(061) = "18C46318C44,gG01L058C67318C63318C62318CE2318C624,V08J01L04C63FD8C63199"
        l3$(062) = "C63118C6B118C6314,gG01L04631C6C6318CE63188C63988C6318C,gG01L0631B42631"
        l3$(063) = "8C66318C46318C46318C4,U02K01L0718E01318C63318C62318C62318C64,gN038D801"
        l3$(064) = "98C631B8C63118CE3118C630,U02R01FF001CC6318EC63188C73188C6318,U01S03800"
        l3$(065) = "1E6318C66318C46318C463188,V04J01FO0813318C63F18C62318C62318C8,V01M0FEL"
        l3$(066) = "081198C6339DFE3119C63118C68,gL01FFI0838CC631ACC63BFCD63188C638,W08R0F8"
        l3$(067) = "86C66318C66318C67F38C46318,W08S088463318C6331AC6231IF63188,gQ088E3198C"
        l3$(068) = "63198E63118C633FFC8,gR08B18CC6318CC63188C63188E78,gR0898C6631CC66318C4"
        l3$(069) = "6318C4638,V01V08C63318C6I38C62318C62310,V01U08C63198C6319AC6311AC63118"
        l3$(070) = "0,V02U0BFF38CC6718CE63188E63188D0,V04S0100718FFE358C66318C46318C470,U0"
        l3$(071) = "1T01006C63198C63198C63138C63110,U02T011026318CC6318CC6318AC631890,T01U"
        l3$(072) = "01103318C66318C66318C66318C50,T04U0110118C67318C67318C62318C630,S02V01"
        l3$(073) = "1018C63998C63798C63318C6310,S04V01101C6318CC631IC63188C63190,S08V01100"
        l3$(074) = "E318C66318C66318C46318D0,R02W01100F18CE3318C63318C62318C70,R04W0110058"
        l3$(075) = "C6B318C67318C67318C620,Q028S0203FE1004C63998C63598C63398C6320,gL04J010"
        l3$(076) = "026318CC631IC6318CC63180,P08U04J01002318C66318C66318C66318C0,gL04J0100"
        l3$(077) = "118CE3318C63318C63318C60,O02V04M018C6B198C67198C67198C620,gK01O0C6398C"
        l3$(078) = "C6358CC6358CC6320,O04U01O0E318C6631CC6631CC6631A0,gU0718C63318C63318C6"
        l3$(079) = "3318E0,O08U01O078CE3198C63198C63198C60,gK02O02C6318CC6718CC6718CC620,g"
        l3$(080) = "K02O016318C66398C66358C66320,gK02O01B18C63318C6331CC6331A0,N01V06P098C"
        l3$(081) = "63198C63198C63198E0,gK08P04C6318CD6318CC6318CC60,N02178S04P027318C6731"
        l3$(082) = "8C66718C6620,N02IF2R0CK02J01318C63318C633D8C63320,N01A00ER08K02K098C63"
        l3$(083) = "198C6319CC6319E0,R03R08K02K0CC6318DC6318CC6318CE0,R018P018K02K066318C7"
        l3$(084) = "6318C66318C660,S0CQ0CK02K03318C63318C63318C6320,S06Q08K02K0398C63398C6"
        l3$(085) = "3198C67FE0,S02Q08K02K03CC631ACC6318CC67D8E0,S03P01L02K03E6318E66318FE7"
        l3$(086) = "F98C60,S018O01L02K013318CIF98CEFB18C620,T08O01L02K01198CE319CC73198C63"
        l3$(087) = "20,T08P08K02K038CCFB18CC6318CC631A0,T04P08K02K03C67B18CE6318C66318E0,T"
        l3$(088) = "04P08K02K036F318C67318C63318C60,T02P08K02K037998C63998C63198C6,gK08K02"
        l3$(089) = "K07D8CC6318CC6318CC63,U08O08K02K058C66318C66318C6631E0,U0CO08K02J01ECE"
        l3$(090) = "3318C63318C63318FC,U07CM018K02J03E6B198C63198C63198C6C,U028M01L02J0D63"
        l3$(091) = "98CC6398CC6318CC6F8,U018M02K0LF9318C66318C66318C663,V08M03JFC2K0318C63"
        l3$(092) = "318C63318C6331,V08M04K02K028CE3198C63198C63199,gJ04K02K0246B18CC6318CC"
        l3$(093) = "6318CF,gG0IFCK02K022398C66398C66318C66,V06I01004L02K03118C63318C63318C"
        l3$(094) = "632,V05B80C004L02K0388C63198C63198C631A,X07FI04L02K02C4E318CC6318CC631"
        l3$(095) = "8D,gI04L02K0262B18C66318C66318C7,gI04L02K023198C63398C63318C63,gI04L02"
        l3$(096) = "K03188C63198C7F798C636,gI04L02K038CC6318CC631IC631C,gI04L02K02C6E318C6"
        l3$(097) = "6358C66319C,gI04L02K0263118C6331CC63318CC,gI04L02K023188C63198C63198C7"
        l3$(098) = "8,gI04L02K0318C46318CF6318D4630,V08L04L02K038CE2318C67318C72330,V06L04"
        l3$(099) = "L02K02C67118C67718C6391A0,V06L04L02K0263988C635D8C631C8F0,V0EL04L02K06"
        l3$(100) = "3B8C4631FCC6318C478,V0A002I04L02K07D8C6231FC66318FC238,V04005E004L02K0"
        l3$(101) = "98C7711AC63318C7E110,V08I030046P019C63988E63198C636190,U028J0E3FFCO01E"
        l3$(102) = "6318C4E318CC63300F0,U03K07E007O01E318CE3B18C6631F0070,U02L080018N0I18C"
        l3$(103) = "673D8C63318C8060,U06P078M0188C631IC63198CE8020,U06Q0C01J09FC46318C4631"
        l3$(104) = "8CC6BC020,U06Q0600A001E032318C66318C6631C0,U07I04M03E04003600918C63518"
        l3$(105) = "C63318E040,U06I04M0238I02I0C8C631C8C63199C6020,U04C004M0207I04I0446318"
        l3$(106) = "C46318CF63030,U038004M0200F078I062318C62318C6631818,U018C04M0200198J0F"
        l3$(107) = "118C67118C6331FC08,U01FC04P01F8J0988C63588C631B8FE,U01FE04P0104I018C46"
        l3$(108) = "31CC46318EC62,U023DFCP0104I01C62318C62318CE622,U051ER0108I0363118C6311"
        l3$(109) = "BC6F362,U07408Q010AI023188C63388FEF99C0,U02318Q0101I0718C463IFE6FD8D80"
        l3$(110) = ",X08N0100100C0058C6233I01FFEC7,W01800FCJ01I080BE04C6311CK0F9E3,X0BFK03"
        l3$(111) = "FFI0401184677FFL01CF3,X08N01I040107FF98C7M02FB,W018N01I040100718C63M01"
        l3$(112) = "ED,W011N01I04010058C631N06680,X0CN01I0401004C6318C003F80033C0,gM01I040"
        l3$(113) = "18046318CF00FEFC011C0,X08AM01I0401006318C6BEF8C67818C0,X0F2M01I0403003"
        l3$(114) = "18C6391CC63301C40,X042Q0400C028C63188C631BCEE60,IFCT062Q040060246318C4"
        l3$(115) = "6318FC0B20,E23CT062T04022318CE2318C6609E0,F11CT042T0403118C67118C633F8"
        l3$(116) = "A0,D88CT042N08K010388C63188C6319FC80,CC44T022N08L081C46318C46318C7F80,"
        l3$(117) = "C624T02O08L07162318C62318C67B80,631CT06O08L0113118CE3118C634F80,F18CT0"
        l3$(118) = "4N038002I011998FFB188C631CE,58C4gH01C8002J0BFFE6398C463188C,CC64T0C1K0"
        l3$(119) = "1C08002J0DC62318C62318C8C,C634T0C1J01C008002J01E3118C63178C70C,E31CS01"
        l3$(120) = "01I01CL02J01B188C6318CE6F,IFCS0701I06M02J0718C46718C6F7E,W0C017CF8M02J"
        l3$(121) = "058C62358C63960,W08018M04002J01C6311EC631C,W0C04N04002J0963188FE318C,W"
        l3$(122) = "0C04N04M09318C44F18CC,W0604N04M0918C63818C78,W0204N06001J098C631C067C0"
        l3$(123) = ",W0184N05801J0BC6319807E,1FEU084O08018I066318D,387V04O0E01CI0F318C680,"
        l3$(124) = "6018T084M01C3816001398C6380,4008U04M026041100F3CC631C0,4008S01P0202108"
        l3$(125) = "31366318C0,4008S02O0410300440FB318C40,6018S04N018101002801F98C620,387g"
        l3$(126) = "I020080881801FCC6310,08T01O040080883I03E63188,V06O08008048J01F318C8,3F"
        l3$(127) = "8gH01800803847800D98C64,65CS04N01I080082C600DCC636,44CP03DFI02K06L082E"
        l3$(128) = "301FE63FA,I4P0F8J06K08L082B18767F98D,64CO01F8J0FJ01N0198FE3998C780,378"
        l3$(129) = "O04M08I03N01CC6318CC63C0,04O018M08I0CJ08J0E6319C6631E0,Q078K03CF001K08"
        l3$(130) = "0040F318CE3319F0,7FCN0F8J0FC7FE26J03I040798C63198D78C0,7FCN0CJ0180C03F"
        l3$(131) = "8J02I04006C6398CC73DA0,00CM01AL04I03DN04001E338C6631AE0,00CJ04004L01K0"
        l3$(132) = "FN04I0F18C63318C70,M04008L04K07N04I078CE3198C630,004J06008K01L038I01M0"
        l3$(133) = "3C6B18CCE310,7FFJ0201L02M0CI01M01E398C67B190,60CJ0303K038Q01I02J0718C6"
        l3$(134) = "3398D0,404K018K0FN07I02I02J03DC63198C70,N038J03O01I04I02K0FE318CC630,7"
        l3$(135) = "FD8J01K04P08004I02K03F18C66310,N0BJ018P04008O02D8C633190,N07J0CQ0381P0"
        l3$(136) = "4CC631D8D8,N038I08S02J01K0746318CC7C,00CK01C804S014J01K022318C663E70,7"
        l3$(137) = "FF8gI0CJ01K04318C6731AF8,0048gI04J01K0838C63398FF4,I08gI04N01C82C6318C"
        l3$(138) = "C63380,gV01B80E318E66318C0,7FC8gI078J08207001B18C63318C60,7FC8gJ04J082"
        l3$(139) = "0B03198C63318C630,gN02J08217FF98C63398C6318,gN018I082B7DFDC6319CC63198"
        l3$(140) = ",1FgM0C901076IFEE318D66318C8,778gL0590C1FF9B07998C63318C68,64CgL05B885"
        l3$(141) = "C0781C44C6F198C630,I4gL07F8E480F7C7C6F318CC631C,64CgL0790B4BFE00C673F8"
        l3$(142) = "C66318C,778gL0391BE0FE020171AC63318C4,17gN08BB80063480D8E63198C64,gP04"
        l3$(143) = "2EI03F8806C6318CC63E,gP020CK01801F718C67F9A,3F8gM011M0600DD8C63318E,60"
        l3$(144) = "CgV0181FCC63798C6,404gW0385E7F98CC63,40CgY04F338C6631,71CgY02F1ACE3319"
        l3$(145) = ",7FF8gX0238E6F398D,hH015C63FCFE7,hH010E318C66780,hI08B18C633DF0,hI06D9"
        l3$(146) = "C63198F0,hI07CD6718CFF8,hI03E7358C7F3E,hI017F1CC6F319FD,hJ0398E6F198C6"
        l3$(147) = "3E0,hJ03FC7718CC6319C,hJ01E7BF8C66318C6,hK0F3F7C63318C66,hK0FA60FB198C"
        l3$(148) = "634,hK0FC001F8CC631C,hK078001FC66318E,hK0F2I0E63318C7,hK036I073198C63,"
        l3$(149) = "hK03CI03F8CC632,hK01J01FC6631C,hQ0F6331F8,hQ0FB19F80,hQ05D8FC,hQ07CC60,"
        l3$(150) = "hQ01E660,hR0E3C0,hR073,hR03F,hR01E,"
        l3$(151) = ","
        l3$(152) = ","
        l3$(153) = ","
        l3$(154) = ","
        l3$(155) = ","
        l3$(156) = ","
        l3$(157) = ","
        l3$(158) = ","
        l3$(159) = ","
        l3$(160) = ","
        l3$(161) = ","
        l3$(162) = ","
        l3$(163) = ","
        l3$(164) = ","
        l3$(165) = ","
        l3$(166) = ","
        l3$(167) = ","
        l3$(168) = ","
        l3$(169) = ","
        l3$(170) = ","
        l3$(171) = ","
        l3$(172) = ","
        l3$(173) = ","
        l3$(174) = ","
        l3$(175) = ","
        l3$(176) = ","
        l3$(177) = ","
        l3$(178) = ","
        l3$(179) = ","
        l3$(180) = ","
        l3$(181) = ","
        l3$(182) = ","
        l3$(183) = ","
        l3$(184) = ","
        l3$(185) = ","
        l3$(186) = ","
        l3$(187) = ","
        l3$(188) = ","
        l3$(189) = ","
        l3$(190) = ","             

        l4$(001) = "^JO"
        l4$(002) = "^XA^EG^XZ" 
        if ctrlflag$ = "Y" then ~
           l4$(002) = "^XA^EG^XB^XZ"             /* CR1051 suppress start */
        l4$(003) = hex(7e) & "DGLABEL4.GRF,11250,30,"
        l4$(004) = "R060,R040,,::R080,,Q01,R080,,:Q0180,:R080,:"
        l4$(005) = "R0C0,R080,R0C0,R060,:R020,R01L02,R01L06,R018K0C10,S0CI0E1028,S06007190"
        l4$(006) = "48,S0300C091C8,S0300C09186,S0181C05101,S01C2005801,S01C2007001,T0A2L08"
        l4$(007) = "0,T056L080,T062L080,T012L040,gH040,U08L0C0,U038K080,U018K080,U01FK080,"
        l4$(008) = "U01FJ01,V098,V050,V07J01,V018I01,V01J01,:V02J02,V04J02,V040,V04J02,:V0"
        l4$(009) = "KFC,V0C0,V040,,:::::hH07C,gW0407FFEE,gX01JF380,gW01FC3F1FE0,gU01F7FCI0"
        l4$(010) = "7FFE0,gT0C3FFE01C3FE71F,gS03E7FF003FF7F30180,gR01F3IF002731FB0060,gR07"
        l4$(011) = "FBE7E003398DF0030,gR0FCF806001FEC4EI08,gO0CFFE0E004J03I6I0F,gN01B9F8J0"
        l4$(012) = "6J01B76J0E0,gN01BCEK04K0D9CJ038,gN01BECQ0ICK06,gN01338N03FFE68L03C80,W"
        l4$(013) = "0CP013BN01EE7378M03E00E,W0CQ0FFN0FE73998M03IF180W04P01FFK03F3F7BFDD8M0"
        l4$(014) = "37C0180gN03F6K07FB319CE5N02I03,W02N0C03B2J03F3998CE77N04I02,W07N08C73A"
        l4$(015) = "I01F39IC673FM0FCI04,W05O09IFI0399CE46339EM08J04,W088N01JF80D8CE7E319CE"
        l4$(016) = "L01801F08,X08N039233FFCC67C3F8CE6L01001F08,W078N073239DE4637007E676L01"
        l4$(017) = "001DD980gL01E3C1CE7231EI07F3AM0800FFB80gL03E3C1E73B1B8J07FCM08007FFC0g"
        l4$(018) = "K0FC02C0739D8EL07CL018I0FFE0gJ01I038019CCDCM04L03J02BA0gJ01I07800CE678"
        l4$(019) = "M04L02L020gJ01I05001E73EN04L02,gJ01I0D00173B8N08L02L040gJ01001F001FFCO"
        l4$(020) = "08L02L040gJ01I0F003FFCO08L02,gQ01F3P08L02L080U01CV0CO018L02L080U034U01"
        l4$(021) = "8O01M02L080U014U09P01M02K01,U016U0CP01M02K01,U038T03Q03M02K01,U03U0EQ0"
        l4$(022) = "2M02K01,gJ04K018Q02M02K02,gJ0EK07R02M02K02,gJ08FJ0CR02M02K02,gI0119800"
        l4$(023) = "3S07CL06K02,U0CM01007802S047CK04,U0CM02I0C3CS0407EJ04K04,T01CM04I05E7F"
        l4$(024) = "8Q04003DC004K04,T07CM04I07I0CQ0CI07300CK04,T03CM08M0CQ0CJ01808,U0CM08L"
        l4$(025) = "018Q08K0808K08,S01AM02N0F8P08K0330FCI08,T06M02I01CJ0FEN018L0E007C008,T"
        l4$(026) = "08M04I01FEI013EM01Q07C10,T08M04I01CFE00103FL01R07F0,T09M08I01676003001"
        l4$(027) = "FCJ01S030,T0AL01J0133C002J07EI01S010,gG02J03BFC002J0380E03Q020,P0F8D8M"
        l4$(028) = "02J038FI02L03F82Q07020,R06N02J06C6I02M01FEP03DFA0,S08M04J066CI02O06P0E"
        l4$(029) = "00E0,R01N08J0E38I06O02O0180020,P0801N08J0B3J04O02L0200F0,P0402N08I019E"
        l4$(030) = "J04O06L07E18I040,P0202M01J03CEJ04O04L04FBJ040,P0206M01J06ECJ04O04L060E"
        l4$(031) = "J040,P020CM01J0E78J04O04L0EL040,P011N01I01F3K0CO04K0FC,P016N02I0F3EK08"
        l4$(032) = "O04J01CM080,Q0CN0201FD9CK08O0CJ0FN080,g0201C67L08O08J08N080,g0201627L0"
        l4$(033) = "8O08J08,g0201316K018O08J08M01,gH0199CK01P08J08M01,g0401CD8K01O018J08M0"
        l4$(034) = "1,g0401E7L01M01F9J018M01,g0403F7L01M01E10F001CM01,g040FFEL01M03003F801"
        l4$(035) = "C,g04019FEK01M02J0FE1CM02,g0401CC3FCI03M02K03FCM02,gH01F8007FC02M02M07"
        l4$(036) = "M02,g06037K0FF2M02M03M02,gG0833L01FEL02M03M02,gG0839M023FCJ02M02,gG083"
        l4$(037) = "D80FCI02003F8002M02M04,gG0836E7B6I02J0FFO02M04,gG08337F9BI06K01FCM02M0"
        l4$(038) = "4,gH03199CFI04U02M04,g01038DCE7I04U06M04,g010162739I04U04,g01013139DI0"
        l4$(039) = "4U04M08,g01C1I9CFI04M04M04M08,g01C18CCE7I04M04M04M08,g06C0C6673I0CM04M"
        l4$(040) = "04M08,g0880E233AI08M04M0CM08,Y010807119EI08M04M0C,Y010807DD8EI08M04M08"
        l4$(041) = "L010,Y060802CFK08M04M08L010,Y08080367K08M04M08L010,X0100801A3K08M0CM08"
        l4$(042) = "L010,X020080193K08M08M08L010,X0600800D98J08M08M08L010,W038018006CCI018"
        l4$(043) = "M08M08L010,V074001800667I01N08L018,V08I01I0331I01N08L010,U018I01I03998"
        l4$(044) = "001N08L01M020,U02J01I01CC8001N08L01M020,U02J01J0E68001CM08L01M020,U08J"
        l4$(045) = "01I0139800778K01M01M020,U08J01I039C800F38K01M01M020,g01I03CEC03B9CK01M"
        l4$(046) = "01M020,g01I0EE7C03BCCK01M030,g03007E73FFEF66K01M030,U0CJ0301F139DFE636"
        l4$(047) = "K01M02M040,U03J03FFI9CE6231EK01FCK02M040,V08K07FFCE7I38CK010IF8002M040"
        l4$(048) = ",V0CK03IF7KFCK0101FFI07EL040,V04K0362339FFE78K01I02003FFCK040,V04K0131"
        l4$(049) = "19CE663L03I02K0IF80040,V04K01998CE7233L03I02K0201IF40,V04L0IC673B3AL02"
        l4$(050) = "I02K06J03C0,V0CL0E66339FEEL02I02K060,V08L072319CEE6L02I02K040,V08L0391"
        l4$(051) = "8CE672L02I02K040,U018L01D8C6737IF8I02I02K040,U03M01EC633BFF01IFE2I02K0"
        l4$(052) = "40,T018N07318CEE78I06K06K040,gJ0398C66738I06K06K040,T06O01CC63379CI04K"
        l4$(053) = "04K040,S03Q0F631BDCCI04K04K040,S0CQ03F98FCE4I04K04K040,R01S03FC6E76I04"
        l4$(054) = "K04K040,R06T03E673AI04K04K040,R08U0F379FI04K04K0C0,Q01S03E01FDCFI04K04"
        l4$(055) = "K0C0,P03ES06JFCE78004K04K080,Q0AS0C07IF7F8004K04K080,O03U08I03F39C004K"
        l4$(056) = "04K080,O06U08J0F9CC004K04K08K080,O04T018J0DCE6004K04K08K080,O0CT01K0C6"
        l4$(057) = "76004K04K08K080,O08g0E33B004K04K08K080,O08T02K0F19F004K04K08K080,N01U0"
        l4$(058) = "2K0D8CF804K04K08K080,gJ02K0CC67804K04K08K080,N02U02K0E319C0CK0CK08K080"
        l4$(059) = ",N02U0EK0F18CE0CK08K08K080,N06U0CK098C670CJ01L08K080,N047FS0CK0CC63308"
        l4$(060) = "J03L08K080,N07IF8Q0CK0E631B88J01L08K080,N07E078Q08K0F318DC8J01L08K080,"
        l4$(061) = "Q01CP018K0B98C4E8J01L08K080,R06P018K09CC6278J01L08K080,R038P0CK0CE6I38"
        l4$(062) = "J03L08K080,R018P08K0E731998J02L08001FF80,R01AP08K0F398CD8J06K04801F808"
        l4$(063) = "0,S0EO018K0BDDEEF8J0FFE0011F0070080,S06O03L09CE6238I01LF3FK080,S02O018"
        l4$(064) = "K0CE73318I0F80C01EM080,S03P08K0E73I980078008P080,S01P08K0B39CCD827CI08"
        l4$(065) = "P080,S01P08K099CE478FFJ08P080,T08O0CK08CE723BCK08,T08O08K0C673B3E4K08,"
        l4$(066) = "T03O0CJ03B19CCF04K08P0E0,T01EM018J0398CE7F04K08P040,T01FM01K038C673804"
        l4$(067) = "K08O0380,T01EM03J07LF804K08O04,U0CM07JFE66319D804K08O04,U04M06I0377398"
        l4$(068) = "CD004K08O04,U04M06I033F9CC67004K08O04,U02J0IFCI079FCE633004K08O08,U038"
        l4$(069) = "003804J06CEE731B004K08O08,U02FE0E004J0I67398D004K08O08,U0123F8004J0637"
        l4$(070) = "39CC7004K08O04,U0100EI04J071F9CE63004K08O04,gH04J07DFFEF7B004K0C03EFK0"
        l4$(071) = "8,U02L04J04C6E739B004K0C03FFJ018,U02L04J0I6739CD004K0C04198I030,U02L04"
        l4$(072) = "J063739CE5004K0C0800D80020,U02L04J071F9CE73004K0C18007C0020,U04L04J078"
        l4$(073) = "CCE73B004K041J060020,U04L04J02C66739D006K047J050040,U07L04J033719CE700"
        l4$(074) = "6K044J024080,U07L04J01DF8CE7300CK04CJ022040,U0FL04K07CC673B1FCK0F8J011"
        l4$(075) = "040,U070078004K01E6339E302I01F8K078860,U07007E004L0E319CE403BC02M07844"
        l4$(076) = "0,U0400438047K0F18CE48006402M018240,T01C0061E7FFEJ078C675J040CN08240,T"
        l4$(077) = "01800607F11FJ04C633EJ041CM0C0140,T0180060180018I066319CJ0224L01A0180,T"
        l4$(078) = "03I06K01FI07398CCJ01E4L0120080,T03I02L0380679CEFFL04L0110080,T018002M0"
        l4$(079) = "E87FCE7FB8K04L031,T018002M0EI7EFFBFEK04L0208080,T01I02M047E67398DEK04L"
        l4$(080) = "0208180,T01F002M040FF39CC4EK04L06040C0,T01C002M0407FBFE626K04L0402060,"
        l4$(081) = "T01C602M0401FEE7336K04L041F020,T01FF02M0401FE7399CK04L041F8,T01FF02M04"
        l4$(082) = "033F39ICK04L040B8,T01CFCN060DBBCE738K04I070F8088,T01A0CN0618EFE73BL04I"
        l4$(083) = "02DF01,U01CCN023C67739FL07E7C1F806,V0ECN0266237DCEK03I03EFC04,W0C03IFC"
        l4$(084) = "006F331AFE6J01CJ01F304,W0FFE003JFB9B8C6F383IFL039C4,W0EN039CFC673IFP0C"
        l4$(085) = "84,W0FN03CE76379EJ01M03C4,W0B8M02E7331ECEJ01N082,W0FN0273B18C66K08001E"
        l4$(086) = "I062,W042M0339F8C633K0F0060F2021,W047M039CFC6F1BK08C7800F021,W079M03CE"
        l4$(087) = "763F8DK08K022A1,W079M03F7FFBFEFK08K060D0,IFCS031M0I3B9CDE3K08K0F8D080,"
        l4$(088) = "IFCS031M0319FCE733K08L041380,F39CS021M038CFE73998J08L03F2,D9CCS021M01C"
        l4$(089) = "67F39CD8J0CL0262,C674S031M0131B9CE7E8J04M0F2,E33CS0318L0118DCE73F8J0CM"
        l4$(090) = "096,F19CS0318L0F8C6E739DC03FFCL0198,D8CCS0408K0FDC62F39CJF00CL0118,CC6"
        l4$(091) = "4S0608J03CF633B9CE7CJ04L0118,C634S0E08I03EE731B9CE73CJ04I0700218,E31CS"
        l4$(092) = "0E08003E0F398CC673BCJ04I01812,FB9CR0380870E0079CC6E339FCJ04I05E3C,IFCR"
        l4$(093) = "0600IF8003CE62B19CECJ04I07008,V0401C02I02EF3398CE6CJ06I010,V0603L027B9"
        l4$(094) = "B8C6738J05J08,V0602L0339IC633F8J04EI08,V0302L079CE6E319D8I0181C070,V01"
        l4$(095) = "82L06CE72F18CD8I01E06180,W0C2L046F3BF8C678J0C07C,1FET0C2L077JFEF7F8J08"
        l4$(096) = "0,3FFT042L073DFCF631BK080,F838S042L07FCF67B19DK080,E018S042L01CE7F7F8F"
        l4$(097) = "D8,E00CR0102M0C79FEF7B3CJ040,E01CR0602L0303CEE7F19FJ020,F838R0403L0603"
        l4$(098) = "6673F8CF8I010,387R01803L0801I39EC47CJ08,08S03003K018011BBDE623EJ08,1FS"
        l4$(099) = "03001K010018DEF7F31EJ04,FF8P010E001K02001C6E7B198FI01C,E6CO01FF8001K06"
        l4$(100) = "001E273A0CCDJFA,E64O07EJ03K0C001B339E06F83FC01,E6CI018J0FEJ07CI01I01DB"
        l4$(101) = "BCE03E0EJ080,F7CJ08I0104J034I03J0ECF67J08I01C0,378N06M04001EI01BE7338I"
        l4$(102) = "08I0340,P01EK01FFC01J031E31BCI0CI0130,P03FJ07JFEFJ060718DFI0CI0110C0,F"
        l4$(103) = "FCM03K0C1E0BFCJ04039C67C004I010D80,FFCI01800E8K07I07FJ0402D633F07CI010"
        l4$(104) = "2,01CI018016K022I03FJ0602F7BDF03CI01,00CI01C01L07K078I0202398CF034I018"
        l4$(105) = "020,M0C03L0CK03CI020318C67814I018020,IF8I0E06J01EM0EI021CCE31BF8EJ0802"
        l4$(106) = "0,E0CJ063CJ03CM07I04366B18DDFEJ08060,CL067K0EN03I0CI398C4EFEJ08020,FFC"
        l4$(107) = "8I036J038N0180183B9CC6273EJ08020,FFD8I03EJ0EP060103FCE6I3BEJ0C020,M01E"
        l4$(108) = "I038P030203EEF3199EFJ04070,N0FI06R0C601E7798CCFFJ04010,00CK03F078R02C0"
        l4$(109) = "073DCC4673J0400CE0,IF8K0D8T0180039CE623798I04005F8,IFCgH08003CE7331FDC"
        l4$(110) = "I0600248,00CCgH08I0E7399FCFCI02J06,I08gH08I0F79CFFE7EI02J0180,gL0FI07F"
        l4$(111) = "CE7E73CI02K040,FFD8gH018005EEF3FF9CI02K020,gM05004E7B7FFCCI02K010,gM03"
        l4$(112) = "00673JFEEI03K010,0EgK019A77BLF8003K010,F7CgJ01BE7NFC00EK010,F7CgK0BE3F"
        l4$(113) = "FA3IFEC00E0,E6CgK0FE3DEFFBDE732FM08,E6CgK072EFC7FF773A03M08,F78gK01AFF"
        l4$(114) = "007FED9C02L018,16gM0DBC001FDCCF06K07F8,gO04FK01E67FCJ07F80,1F8gL03CL03"
        l4$(115) = "F3FCJ0E0,FFCgT01IFE039F80,E0CgU03F7JF0,C0CgW07F863J04,E0CgW07E021J04,I"
        l4$(116) = "F8gV03F021DF004,IF8gV03D821F9FC0,h01C821I06E,h01E8318003F80,hG0EC118J0"
        l4$(117) = "C0,hG07C108I07C0,hG03E108003878,hG017F0803E007F4,hG017F8E21800460,hH01"
        l4$(118) = "E73CM010,hI0E7CFM030,hI0C041FL020,hI0EC0038K020,hI078001CK010,hI0FI01C"
        l4$(119) = "L08,hI034I0EL08,hI03CI0EK010,hI01J07EJ020,hO0CI01C0,hN01EI0E,hO0F00E0,"
        l4$(120) = "hO0E0080,hO070180,hO0386,hO0184,hP0DC,hP078,"
        l4$(121) = "'"
        l4$(122) = "'"
        l4$(123) = "'"
        l4$(124) = "'"
        l4$(125) = "'"
        l4$(126) = "'"
        l4$(127) = "'"
        l4$(128) = "'"
        l4$(129) = "'"
        l4$(130) = "'"
        l4$(131) = "'"
        l4$(132) = "'"
        l4$(133) = "'"
        l4$(134) = "'"  
        l4$(134) = "'"
        l4$(135) = "'"  
        l4$(136) = "'"
        l4$(137) = "'"  
        l4$(138) = "'"
        l4$(139) = "'"  
        l4$(140) = "'"
        l4$(141) = "'"  
        l4$(142) = "'"
        l4$(143) = "'"  
        l4$(144) = "'"
        l4$(145) = "'"  
        l4$(146) = "'"
        l4$(147) = "'"  
        l4$(148) = "'"
        l4$(149) = "'"  
        l4$(150) = "'"
        l4$(151) = "'"  
        l4$(152) = "'"
        l4$(153) = "'"  
        l4$(154) = "'"
        l4$(155) = "'"  
        l4$(156) = "'"
        l4$(157) = "'"  
        l4$(158) = "'"
        l4$(159) = "'"  
        l4$(160) = "'"
        l4$(161) = "'"  
        l4$(162) = "'"
        l4$(163) = ","
        l4$(164) = ","
        l4$(165) = ","
        l4$(166) = ","
        l4$(167) = ","
        l4$(168) = ","
        l4$(169) = ","
        l4$(170) = ","
        l4$(171) = ","
        l4$(172) = ","
        l4$(173) = ","
        l4$(174) = ","
        l4$(175) = ","
        l4$(176) = ","
        l4$(177) = ","
        l4$(178) = ","
        l4$(179) = ","
        l4$(180) = ","
        l4$(181) = ","
        l4$(182) = ","
        l4$(183) = ","
        l4$(184) = ","
        l4$(185) = ","
        l4$(186) = ","
        l4$(187) = ","
        l4$(188) = ","
        l4$(189) = ","
        l4$(190) = ","   

        
        l5$(001) = "^JO"
        l5$(002) = "^XA^EG^XZ" 
        if ctrlflag$ = "Y" then ~
          l5$(002) = "^XA^EG^XB^XZ"             /* CR1051 suppress start */
        l5$(003) = hex(7e) & "DGLABEL5.GRF,10701,29,"
        l5$(004) = "R080,Q0180,Q01,,::Q02,Q04,:Q06,,:Q06,:Q02,Q"
        l5$(005) = "03,Q02,Q03,Q01,Q0180,:R080,R0CK030,R06K030,R06K0F8,R03I070E8C0,R03801F"
        l5$(006) = "CBDC0,S0C03149FC0,S0407CECE78,S060E66FF38,S070E239198,S030B11I8C,S03I9"
        l5$(007) = "8CI4,S03FIC6264,T0DC663132,T0C623189A,T023118C0E,T03998C646,T01ECC6324"
        l5$(008) = ",U03663194,U07E3188C,U03F8C660,U016C6330,U01E63198,V0631I8,V0518C48,V0"
        l5$(009) = "C8C668,V0E46338,U01A23190,U0J1880,U01988C50,U01CC4670,U03JFE0,U03,U018"
        l5$(010) = "0,,:::::hG0F,gX0FFDCC0,gW018CI60,gV01EC6231E,gU043663118FDC,gS0437F331"
        l5$(011) = "98C6E7E0,gR01E63F998CC636338,gR0F3B7B8CC6631718C,gQ0D8D8C6633118IC6380"
        l5$(012) = ",gO07BCC6C6731998C6E633E0,gM018IC6263598IC63B319DC,gM018C463131C8CI631"
        l5$(013) = "998CCF,gM018E231998C46323188CC463E0,gL02193318IC6631918CC66231B8,gM0I1"
        l5$(014) = "98CI63318D8C66I318DF20,V02Q038CC6233198C6C6331998C67FF03C0gN0E40631198"
        l5$(015) = "8C6263118IC623IFEE0gM01E223198CC46313198C646311DD8C60gM017I18CCI631998"
        l5$(016) = "DC6323199CD46C0gK060318C8C66I318IC7631B18CDE66380gL0638C446231998CI633"
        l5$(017) = "18D8C7FI31,W04O0C6331198C67E3119A8C6263318FCE,gL01631988CC63E7D98CEC6I"
        l5$(018) = "31B8C7E6,gL03318CC46633B1FCCI631998EC63F6,gL0518C462231C98C7E37318IC66"
        l5$(019) = "31FEE0gK03C8C623I1BC4C62F9D18C4662319FFE0gJ07C6C6331898EI6311FC8C62I31"
        l5$(020) = "8C7FF8gJ0C6363198C4CEI3198CEC63319F8C67FF8gJ0E31318CC62731998CC6B63198"
        l5$(021) = "IC6I398gJ0B1898C4630F188CC6639318CCI6319988gJ098IC6231998C466231898C46"
        l5$(022) = "363188CD0gJ08CI63318D8C6633119IC6231D18C4670gJ0C6I3198CE46331998DI6331"
        l5$(023) = "8D8C66330gJ0E31198CC6E23198IC7I3198C6C6331A0gJ0B188CC463I188CI631198CC"
        l5$(024) = "I63198E0U01M018C66I318CC4663191ACC66231D98C4660U048L01C633199EC6623318"
        l5$(025) = "D8E66I318IC66340U05M0163118IC6231198C6C6331998CI6331C0U08M013188C63631"
        l5$(026) = "1I8C6263118IC6633198C0gI0198CC6323198C446313588C646351988C40gI01CC6631"
        l5$(027) = "D18CC6263199IC63231D8CC4680gI01663319C8C6631318IC6631B18CCI6380gI057B1"
        l5$(028) = "18E446231898CI63318D8C66I3180gI01DD88CE223118C0C6237918C4C623199880T01"
        l5$(029) = "P0FCC7331198C6463119F8C6263918C8D,gH0100667F1988CC6323198CCFEI3198C647"
        l5$(030) = ",T01P033F7FCC46631918CCE6633998CC6363,T0AP01918C462231I8C66I31JC6631B1"
        l5$(031) = ",S01AM04001C8C623I18C446231918C46723189B,S04CM08I0B631D8C4C6331199C6C6"
        l5$(032) = "33D9F8CI6,S014Q09318FC62631988CD636319BCEE6332,gL0898C47F03188C4663131"
        l5$(033) = "8CDE67F19A,T08L02J0IC6233F18C462231898C463231F8C,gL0E663318DEC6631138I"
        l5$(034) = "C6231918C7C,T08L04J0B33199C66733189ACI63318D8C66C,gL09198CE6223B98C4E6"
        l5$(035) = "I3198C6C6334,Q0802Q0188CC463I18FC62631198CC626319C,P0802M01J01CC662319"
        l5$(036) = "88C47F03188CC463131C88,gK0366J38CC46631F58CC6623199FFC8,O02P02J0233199"
        l5$(037) = "CC6623318IC66I318CDE678,O02004M04J07118CCE6231198C646331998CI6338,g06J"
        l5$(038) = "0588C606311I8C6263118IC63F3190,P08O03I01C6631958CC62631988CC632BBF8CC5"
        l5$(039) = "0,g01I0363318IC6631318CC46631BD8CC6670,g0980073118C446231898C6E23318DC"
        l5$(040) = "C66I30,Q04N0CC01D988C6263118C0C62BI18DCC6231980,g0E7038IC6339198C64631"
        l5$(041) = "1I8C6263118CA0,P03O0B1FECI631988CC6323198C4C7F33198C660,g098C66I318CC4"
        l5$(042) = "6631918DC62631998CC6360,Y01CC6231918C4E2231I8C7631318IC6631A0,Y0166311"
        l5$(043) = "8C8C62BI18C446331819C4662318A0,Y0133198C6C6331898C6623118C8D6233118C40"
        l5$(044) = ",Y01998CC6363198C4C6331198C647331998C640,Y018CC6631318DC62631998CC6323"
        l5$(045) = "198IC6340,Y01466231898C56303188DE663I18CCI631C0,Y017F3118IC6331918C473"
        l5$(046) = "7F1889C4632318C0,Y0398CFC6I3198C646I3898C7E3B318D8C6,Y028C6677B198CC62"
        l5$(047) = "2319BC4C637F198C6C63,Y024632318FCC663I188E6263118ECC6263180,Y03E31918C"
        l5$(048) = "C7F231988C46303188C6463131880,g0718D8C6633F98CC46631918CC66231998C,g05"
        l5$(049) = "8C6C63319FCFE62I38C8C6637318IC6,g04C6263118CEC63F319CC646331C98CI6380,"
        l5$(050) = "g046313188C626311BE8C6223118C4C6233180,g0631998CC6I3198C67E3I188C60631"
        l5$(051) = "19880,g0318IC6631B98CC62671988CC6723198CC80,g018CI63318IC6631358CC4663"
        l5$(052) = "1918CC6780,gG0C6233118C46623189CC6623318C8C663380,g046311988C6233118C0"
        l5$(053) = "C623I18C4462319,g063198IC6331998C646311I8CE223118D,g098C66I318CCI63199"
        l5$(054) = "8CC62639988CC63,Y03CC6231918C463231I8C6631318CC46631,Y0I63118C8C623191"
        l5$(055) = "8C4C6231818C4622319,Y0F73198C6C63718D8C66A3118C8C623I18F,X019D98CC6363"
        l5$(056) = "1D8C6C6339198C647331898C6,X038IC6631318CC62631988CC6323198C4C62,X0CC46"
        l5$(057) = "6231898C46313188C4663I18CC62632,W01C6633118IC6631998C4E2231889C463031A"
        l5$(058) = ",W016371998CI63718IC663I18CC56231918E,W0731D8IC6I31D8CI6331898C66I318C"
        l5$(059) = "8C6,V0F9188CI631198CC6233198C4C6331198C6462,U03D88C46323188CC46311989C"
        l5$(060) = "62631198CC62230,U06CC46631918CC66A3198CC56303188C4463I1C,U0E231198C6C6"
        l5$(061) = "3319D8C66I318C8C6631318CC44,T01311I8C6263118IC6231998C646I3898C6624,T0"
        l5$(062) = "3198C446313188C6A63118C9C622311AC4C62314,T078CC62631998CC63B3198CI63I1"
        l5$(063) = "88E6063118C,T06C6631318IC6631998CC63631988CC6323198C4,T046231898CI6331"
        l5$(064) = "8IC6631B18CC46631918CC64,T0E3118C0C6233118CC662318B8C662I38C8C6630,T07"
        l5$(065) = "198C646311988C7233118C6C623I1CC4462318,T038CC6323198IC6331998CI6311I8C"
        l5$(066) = "62231188,U0C6631F98CCI63198IC633F198C4C6331198C8,U0223188CFEEI319CCI63"
        l5$(067) = "1B9DFC62671988CC68,U03118C44623FF98D4632318ACC677F358CC46638,U0398C662"
        l5$(068) = "3118CDF7231918C46623189FCC622318,U014C6331198C6C73318D8C6633158C8C7FFB"
        l5$(069) = "1188,U0183188C46631319CC6263198IC6323198C4E78,U01918C462231898D4631318"
        l5$(070) = "CCI63I18CC62638,U02C8C663I18CCE6231998C463271I8C4630310,U02E46331898C6"
        l5$(071) = "673318IC6631958CC46A319180,U04623198C4C6I31FFCE663718DCC6623B18C8D0,U0"
        l5$(072) = "871188C626311B8CC63FF1D8C6C6331198C6470,T010588C46303188EE46311BIC6263"
        l5$(073) = "1188CC62230,T0206C46631918CC6623198CC46313188C4C63I10,T0402623318C8C66"
        l5$(074) = "I318CCI631998CC62A319890,S0200231198C646I3BD8C66I318IC6631B18CC50,S040"
        l5$(075) = "031I8C622311AECC6233998C6E6331898C6630,R02I058C4463I188E626311CC8C62F3"
        l5$(076) = "118IC62310,R04I04C62631988CC6I3198C646319988C60631190,R08I0E631318CC46"
        l5$(077) = "633998CC6363198IC6323198D0,Q04I01918C8C623311AE4C6235998C6E63319D8C662"
        l5$(078) = "0,P028I0198C6463139FFC626311CC8C62B3118IC62320,U02CC636319C8CC6I3198C6"
        l5$(079) = "46319988C62631180,O04K026631B18CC46633998CC6363198IC6I3198C0,U06231898"
        l5$(080) = "C662331AIC6671B18CCI631998CC60,N01L07118C4C627I18C466235898C6EI319IC66"
        l5$(081) = "20,U0598CI6311I8C623311CC4C62B1918D4662320,N02L04CC6I3198C4C6331998CI6"
        l5$(082) = "3198C8C723311A0,U0I631998DC6263398IC6I3198C6C6331998E0,N04L0323188CC76"
        l5$(083) = "3131ICI671998CC6363198IC60,U01918C466231818C46323188CC6E31319CCI620,V0"
        l5$(084) = "F8C6633118C8C6231918C466331898D4632320,V01FFB31998C6463318D8C6633118IC"
        l5$(085) = "723191A0,Y028CI63I1IC6263198IC6I3198C6C60,M010B8N02463231I8C46313988CI"
        l5$(086) = "631199CC62620,M017FF2M01631F18CC46231998C46323188CF4631320,N0D00EM0131"
        l5$(087) = "8D8C6627318IC6631918CC6723199E0,Q03M0398CEC6331598CI63318D8C66I318CCE0"
        l5$(088) = ",Q018L028C6E63118IC6233198C6C6331998CI60,R0CL0246373188C406311988C6263"
        l5$(089) = "118IC62320,R06L02631D98CC6623198CC46I3188C64633FE0,R02L03318IC6635118C"
        l5$(090) = "CE6631B98CC63233F8E0,R03L0798CE66331CC8C66F3318ECC663FBFIC60,R018K028C"
        l5$(091) = "6A33118C44623I98CIFI3BD8C6620,S08K0246391988C6223118C8CE23391CC4C62320"
        l5$(092) = ",S08K03631D8IC6731198C64FB11988C626311A0,S04K03B18CCI635988CCE37B199IC"
        l5$(093) = "6I3198E0,Y034C6631918C462239C98C67I318IC66,Y02663518C8C623I1EC4C623191"
        l5$(094) = "8C46623,S01K0I31D8C6C6731898CI63118C8C623311E0,S018J05998CC6363598C4D6"
        l5$(095) = "733198C6C6331998FC,T0F800188CCE63131IC63735998CC6363198IC6C,T05I02C467"
        l5$(096) = "A31898C46I31C8CC6731318CCI6F8,T03I046633918CDLF18C466231898C4632380,T0"
        l5$(097) = "1I0E3319JFE63318C8C6633118IC623191,T01I0B198ECC6337198C646731998CI6331"
        l5$(098) = "8D9,W01988CI6311D8CC6223598IC6I3198C6F,W018JF23188CC463I1C8C66731198CC"
        l5$(099) = "626,U0C03CE631918CC66231988C46323188CC46312,U0B627I38D8C6673318CC46631"
        l5$(100) = "918CC662319A,V01FF19AC6C6335998C6627318D8C66I318CD,W04CC46313188C60631"
        l5$(101) = "18C8C6273118IC623,W04I631998CC6323198C44631398FDE46316,W06J38IC6671918"
        l5$(102) = "CCI631998CC732319C,W03199ACI63358C8C6637318IC6631B18DC,X08C8E623311CC4"
        l5$(103) = "46231898CI63318D8C6C,X04646311988C6223118C0C6233118C4C638,X06363198IC6"
        l5$(104) = "331198C6463119E8C62A310,T01I071B38CCI671988CC6723198CEC6339190,U0C0058"
        l5$(105) = "9AC66I358CC46633918CCEE6319C8C0,T01C00CC4E623191CC462231C88C66BB318CE4"
        l5$(106) = "70,T01C00CI63118C8C623I1DC44623F918C46238,T01C01EI3198C6C633189EC66231"
        l5$(107) = "F8C8C63E118,U0801799B8CC6367198C4C63F11B8C6C633F090,T070018FDFFE3189CC"
        l5$(108) = "46313188C4E631318D8070,T06I0C7F33B18IC6231F18C4E3A31898C78030,T04I0637"
        l5$(109) = "1998CI63318C8C6673D18IC624020,T0C0013198CCFEI3198C646331CD8CI63340,T0C"
        l5$(110) = "001188C667F1398CE7E23198C4C6I319E0,T0C0018C46323789CC47BI188CI631198CE"
        l5$(111) = "0,T0E001C663191BECE62F9988C46343188CC47020,T0C00163318D8E7EI398CC46631"
        l5$(112) = "D18CCI630,T098013198C6C63F1998C6623318C8C6633F1810,T07001988C626311FCE"
        l5$(113) = "C6231198C646331998C18,T03181CC46313388F706311I8C6623118CCFE08,T03F81I6"
        l5$(114) = "31998CC7F23198C446351188C6C7F,T03FC1I318CCE6633918CC62631D88CC63A37,T0"
        l5$(115) = "47BF1998CI6I38C8C6631318CC46633B13,T0E806C646311988E7623118C0C62711FBE"
        l5$(116) = "4E0,T046246363198IC6331198C6463IFCBF62C0,W0631B18CC76631B88CC63233I03I"
        l5$(117) = "F380,V0231BF8C66I318EFC663191CJ03E7980,W0FCC4E73FF918C466233BFFL073D80"
        l5$(118) = ",W01CI63119C8CE271FFCC46M0BD80,V021EI3198D6C6B378D8C663M07A80,V024B199"
        l5$(119) = "8CC736399CC4C6331M01B40,V010D88CC6631318CC6263198C007FI0DE0,X0CC466231"
        l5$(120) = "898C46303188CF01FDF004E0,W03C6633119ICE271918C46BFF189E0460,V01DE33199"
        l5$(121) = "8DI6B3D8C8C6639198IC1620,V010B198IC6I399FC646331898C66F3B30,IF8R018988"
        l5$(122) = "CI631198CDE223198C4C633F0190,E238R010C6631918CC66231988C46703188CFC50,"
        l5$(123) = "B118R010E3318D8CE6I31IC46631918CC67E40,9I8S08B198C6C6B319D8E6623318C8C"
        l5$(124) = "6631FC0,8C48S08988C6263918IC7E31198C6463319DC0,C628R018CC46313188C6263"
        l5$(125) = "51I8CE223118D3C0,E318R010E663199FCC67331D8CC7FBI188C73,B188T0F3318CDEE"
        l5$(126) = "635998JF639988CC622,98C8R010D998C7E6B31JC6E31318CC466322,8C68R0104C8C6"
        l5$(127) = "E33918C4662B1898C662F31C2,C638R0204646F11988C6233198C0C623199BC0,IF8R0"
        l5$(128) = "E06363198IC6731998C6467118DDF80,U01807FFD8CCI63598CEC6323598C748,U0100"
        l5$(129) = "7898C663731ICI63191ECC63,U01810E663118C8C6231B18CI4E319,V0C10B33198C6C"
        l5$(130) = "63318F8C6638118F,V0410D998CC6763398C6C6331C0CF8,V021048CC663731ACC6263"
        l5$(131) = "19980FC0,1FCT0104466231C98F46313188D0,787T0106633119IC7A31B98C468,603T"
        l5$(132) = "010731998DE763F18IC6638,C018S010798IC673B3F8FE66331C,C018R020028CI6331"
        l5$(133) = "DAECE633198C,C018R0400246323598EE57339988C4,603S08002631918DC673B19IC4"
        l5$(134) = "62,787V03318DEC66I3F8CFI631,104R02I0398C6C63319DCC67F331880,U0CJ07C62E"
        l5$(135) = "3118DCC623I98C80,6D8R08K0B1998CC633B198EE46360,CC8O03FEI080058ECC66319"
        l5$(136) = "98CC7F67FA0,8C8O0F8I018004CI63319IC6633F98C0,CD8I02J01F8I03C0066A33118"
        l5$(137) = "CC66231C98C70,6F8N04L02002391988C6273118C4C638,04N018L02003F98IC6I3998"
        l5$(138) = "CE6631C,P078K0F3C038CC676319BIC673319E,7F8M0F8I01F1FF83C66I318CDE66319"
        l5$(139) = "98CF18,7F8M0CJ030300FE6231B18CC63A31C8CC67B4,018I02I0AK01I01FB118C8C6A"
        l5$(140) = "31F19C466235C,008L02L02J07998C6C63B18D8C6633118E,O04L08J038CC6363198C6"
        l5$(141) = "C6731998C6,008I01004K02K01E6631318CC63E3598CDC62,7FFI01808K04L06231998"
        l5$(142) = "C4631F1C8C67632,008I010CJ01EM0398CI63718CDCE631918E,M09CJ02N01CC67331D" 
        l5$(143) = "8C667F318D8C6,FF98I088J04O0663D198CC6233B98C6C62,M078I018O033188CC4631"
        l5$(144) = "19E8C62632,M078I0CP01D9CC6623198EE4631B1B,M03CI08Q02E66I318CC7E631998F" 
        l5$(145) = ",00CK0E404R0163319D8C66I318IC79C,IFgH0F118IC6233998C6E63BE,0098gG0588C"
        l5$(146) = "626311CD8C62731FD,0018gG04CC6I319EC746311988CE0,gK0C6631B98CFE36319JC6" 
        l5$(147) = "30,7F9gH07B318ECC7E31B18CCI6318,7F9gI0D18C6CE3B3998CI63318C,gL01C6I3BI"
        l5$(148) = "FEC6313988C64,3EgK0F3399KF6319AIC630,7F8gJ0B3BDFFEFB3B18CCI6318,CC8gJ0" 
        l5$(149) = "BFDF701F1FD8C7EI318C,8C8gJ0JF203BCFCCF231918C6,CC8gJ0F23FEFF8DE673F18C"
        l5$(150) = "8C62,6F8gJ0733783FE77331B8C6C632,2EgK011F6003FED9D8EC63631A,gN08FC001F" 
        l5$(151) = "DC8CC6631318F,gN0418J01C4I63189FE5,7FgL022L0763FD18IC63,C18gS01F3F98C6"
        l5$(152) = "E633,C08gT07F7EFFI31980,C08gV07E671198C80,IF8gU03B1CDCE6280,gY01D8C7F3" 
        l5$(153) = "FB80,gY01DC63319BC0,h0D63118CEF8,h0FB388C6478,h079ACC633FC,h03CE6631F9"
        l5$(153) = "F,hG0FE331BD8CFD,hG07319BC4C623E0,hG07F8DC6263119C,hG03CFFEI3198C6,hG0" 
        l5$(154) = "1E7DF1998CC66,hG01F483EIC6634,hG01F8007C66231C,hG01E4003B1998C7,hH06C0"
        l5$(155) = "01D8IC63,hH078I0FCI632,hH02J0FE3231C,hM0FB191F8,hM0FD8DF80,hM02EC7C,hM" 
        l5$(156) = "03E620,hN0F320,hN071E0,hN03980,hN01F80,hO0F,"
        l5$(157) = ","
        l5$(158) = ","
        l5$(159) = ","
        l5$(160) = ","
        l5$(161) = ","
        l5$(162) = ","
        l5$(163) = ","
        l5$(164) = ","
        l5$(165) = ","
        l5$(166) = ","
        l5$(167) = ","
        l5$(168) = ","
        l5$(169) = ","
        l5$(170) = ","
        l5$(171) = ","
        l5$(172) = ","
        l5$(173) = ","
        l5$(174) = ","
        l5$(175) = ","
        l5$(176) = ","
        l5$(177) = ","
        l5$(178) = ","
        l5$(179) = ","
        l5$(180) = ","
        l5$(181) = ","
        l5$(182) = ","
        l5$(183) = ","
        l5$(184) = ","
        l5$(185) = ","
        l5$(186) = ","
        l5$(187) = ","
        l5$(188) = ","
        l5$(189) = ","
        l5$(190) = ","        

        l6$(001) = "^JO"
        l6$(002) = "^XA^EG^XZ" 
        if ctrlflag$ = "Y" then ~
           l6$(002) = "^XA^EG^XB^XZ"             /* CR1051 suppress start */
        l6$(003) = hex(7e) & "DGLABEL6.GRF,11280,30,"
        l6$(004) = "R020,R040,,::::Q01,,::Q01,R080,::R0C0,R080,"
        l6$(005) = ",:R020,:R010,R01L02,S08K08,S0CI0C0020,S06001010,S02J08008,S03J090,S018"
        l6$(006) = "04051,U020010,S01C2,T0A2L080,T042,T032,U02,T01M040,U08,,U01L080,gH080,"
        l6$(007) = "V0B0,V090,V010,V050,V010,:V01J01,V020,V040,:,:::V040,,:::::hH078,gY07F"
        l6$(008) = "DE6,gY0CC633,gW01F4631880,gV04332318C7E,gT0437F1B18C6671E,gS01E63F8D8C"
        l6$(009) = "6I30080,gS0F3F7BC4C6319A0060,gR079B18E6263188E0030,gR0D8D8C6I318C46I08"
        l6$(010) = ",gP0F7CC4C671998CI6I0E,gN0199CC626358IC6334J040,gN0198C6I31CC466319CJ0"
        l6$(011) = "30,gN019C631998C6233188CK02,gM041A6318IC633198C48L0C0,gN012318C4663198"
        l6$(012) = "CC668L01C,W08Q0318C623318CC66338M03C0,W04Q0C84633198C4633198M037C0080g"
        l6$(013) = "N0162118CC663318CC5N02,gL0C033188C4633198C6670,gM0C398C4623198CC6332M0"
        l6$(014) = "64,gM09IC623318CC46319AM080,X08N01C4631198C67E318CAO01A,gM03623188CC63"
        l6$(015) = "83D8C66O01F,gM073318C44633003C636P0CD0,gM0B198C62231CI03F1CM0800FD380g"
        l6$(016) = "L0798CC63131B8J03CCP07FF80gK0F8CC4631898EL03CL01J07FE0gJ018C662318C4C8"
        l6$(017) = "T01J021A0gJ01C6I318C607U02,gJ01631998C632CU02,gJ01318IC63198U02,gJ0118"
        l6$(018) = "C646318FO08L02L040gJ018C632318CCO08L02,gJ01C631B18C68O08L02,gJ016318D8"
        l6$(019) = "C63P08L02L080V08M01318C4C63CW02,V04M0118C626318O01M02,U014N0C631998D8O"
        l6$(020) = "01M02,U02O06318CC77X02,gJ01318C4663P02M02,gJ0198C62391P02M02,gK0CC6I38"
        l6$(021) = "9P02M02K02,gJ08F6319CC7P02M02K02,gJ01BB18DC63P0380,U04O01F8C6632P0438K"
        l6$(022) = "04,gI0200CC7E31AP0401CJ04,U04P067F7F8EP040019C004,gL033198C6T0220,T028"
        l6$(023) = "M0800398CC64U01008,U04P01CC47B4P08K0808,S011M01I0166239CP08K060F,T03Q0"
        l6$(024) = "1I31FFP08K012038,gL011998C7EV0C00380,gH04I018IC633CM01Q038,gL01CI63191"
        l6$(025) = "EL01R03D0,gH08I01637F19D007K01S010,gL0131C78E7I03CI010,P06008M02J038F0"
        l6$(026) = "3636L0ES020,gL02C601B3CM07AP038F20,gG04J046C007EO02P040060,R01N08J0E38"
        l6$(027) = "S02O018,g01CJ0B1J04V020060,P04O01CI019AJ04V078180,P02O016I038CJ04O04L0"
        l6$(028) = "4D9,g012I06CCJ04O04L0206,g033I0E78T04L06,Q01N039801B3U04K0B0,g02CE071E"
        l6$(029) = "K08T01,Q0CN0263FD8CK08T0F,g03318CC8K08O08I018,g0798C67L08O08J08,g04CC6"
        l6$(030) = "22L08O08J08,g0466314V08J08,g0633198K01U08,g07198D8K01U08,g058CC7L01M01"
        l6$(031) = "E1K08,g0463F2L01Q0FI0C,g0E319F8X07C0C,g0B18C81FR02L0F8,g098C78001F002M"
        l6$(032) = "02M03,g0FC63K03C2M02,g01E31M0F8L02,g01B19M020F8R02,g0198D00FCI02001EQ0"
        l6$(033) = "2,g018C6C336N03EO02,g01C633F1AP07CM02,g01631998FY02,g01318IC7I04,g0118"
        l6$(034) = "CI63I04,g018C62331I04U04,g01C631199I04U04,g01E3198CFI04U04,g03318CC67Q"
        l6$(035) = "04M04,g0F98C6632Q04M04,Y018CC6231AQ04,Y01CE63118EI08M04,Y063998CDK08M0"
        l6$(036) = "4M08,X01918CC67K08U08,X0398C6622K08U08,X02CCE3311K08U08,X0E66B1I98R08M"
        l6$(037) = "08,V03F22318JCR08,V0F3I18CI62R08,U01B1898C63231I01N08,U0318C4D63191980"
        l6$(038) = "01N08L010,U038C627318D8C8001N08L010,U04C630318C6C68001CT010,U0C631918C"
        l6$(039) = "62638003FT010,T01E318C9C63131800738S010,T01B18C65631998800F18K01M010,T"
        l6$(040) = "0118C623318IC80198CK010,T018C63I18CI64039C4K010,T01C631998C62I3FCE64K0"
        l6$(041) = "10,U0E318CC4631198F4636K01M020,U0318C67F3198CC6231CK01EL020,V08C62319F"
        l6$(042) = "FC66I38CK0101FCI020,V0E3198C462319DEE78S07E,V0518CC623118CC663P02K07FC"
        l6$(043) = "0,V048C6631198C66232P02N03F8,V064623188CC6J3AP020,V0623118C4663199AEL0"
        l6$(044) = "2I020,V0B1198C622318IC6L02I020,V0B88CC63I18CI62L02I02K040,U011C4663189"
        l6$(045) = "8C6323FF8J02I02K040,U021E22318C4C631B19003FE02I02K040,U0413I18C626318D"
        l6$(046) = "CDJ0206O040,U0819898C630318C4C78U040,T01008C4C631918C62638U040,T0400C6"
        l6$(047) = "26318C8C633798U040,S01I0E30318C646319DCCU040,S0800131918C622318CC64I04"
        l6$(048) = "K04K040,R01I0118C8C63I18C4636I04K040,R02I038C64631988C6271AI04K040,R08"
        l6$(049) = "I02C622318CC463358FI04K040,Q01J06631318CI6319CCFI04K040,Q0AJ0631998C62"
        l6$(050) = "73FFCC67I04K040,O01L098CI631988C627198004K04K080,U018C623318CC463358CC"
        l6$(051) = "004K04K080,O04K01C631198C6E23198C64004K04K080,U0163198CC623118CC632004"
        l6$(052) = "K04K080,O08K01318CC6631188C4631A004K04K080,U0198C6633198C462718DT080,N"
        l6$(053) = "01M0CC623198EC623398C78S080,V0663118CC6631198C638S080,V03B198C6623188C"
        l6$(054) = "C631CS080,W07FEC6I318C446318CS080,N02P016319F8C622318C4M08K080,g0B18IC"
        l6$(055) = "631398C62L01L080,N042EO098CI631898C633L01L080,N05FFC8M04C63E318C4C6319"
        l6$(056) = "8K01L080,N034038M04631918C60E318CCK01L080,R0CM0E319D8C632B18C46K01,R06"
        l6$(057) = "M0B18DEC631998C622K01,R03M098C6E6318C8463318J01,R018L08C639318C4C23199"
        l6$(058) = "8J02P0FF,S08L0C631998C62A118CD8J02O07,S06L0B18DI63198C46238J0DFE0011F0"
        l6$(059) = ",S02L098C723318CC623318I0E0,S02L0CC639198C4E31199800780,S01L0E63198CC6"
        l6$(060) = "2B188CD803C,S01L0F318CC663398C4478680,T08K0598CE633198C62239C,g08CC6A3"
        l6$(061) = "198CC63133E4,T02K0C663918CC4E3189A84,T02J01633198C662B18C4D04V040,T01E"
        l6$(062) = "I023199CC6I398C63F04,T014I0518CF6631998C633804V080,U0CI098C672319DKFD8"
        l6$(063) = "04,U04001CC633JFE46318D004U04,U040016631D98C636318C5004U04,X032318IC63"
        l6$(064) = "1F18C63004,X031IFE66318D8C633004U08,U0300798C632318C4C6319004,U02DC4EC"
        l6$(065) = "671918CI6318D004,W03FE6358D8C637318C5004,X0F231CC6C631D98C63004,X08D8C"
        l6$(066) = "631318C462319004M03EFK08,X046C671998C663118D004M04108I010,X0626358IC63"
        l6$(067) = "7188C5004M08008I020,X01131CCI631D8C463P08I04,Y0998C623318CC6233O01J060"
        l6$(068) = "020,Y0IC631198C463119O02J010040,U04I0E667198CC663188DO04K080,U03I0A335"
        l6$(069) = "8CC663718C45U040,U07001919CC66331D8C62300CK04K022040,U0700198CC623198C"
        l6$(070) = "C63131DCK0FK0110,U07003CC663118CC46318A202I01FL050840,U02002E637198CI6"
        l6$(071) = "318C4401B802M078440,U04002331DFCC6I318C648I0402M0180,T01C0031FBFFE6319"
        l6$(072) = "D8C635J040CO02,T0180019FE676318IC631EJ0418M0C,T01J0CCE33118C646318CJ02"
        l6$(073) = "24L0100180,T01I02663199FC632318C4J01C4L01I080,T01I022318CCFE33B18E7EL0"
        l6$(074) = "4L01I080,T01I03118CI6F19D8C7B38K04,T01I02CC63191CFE26398CEK04L0208080,"
        l6$(075) = "U0600266318D8C6F3318C46K04L02040C0,T01C0032318C6C631F9EC626K04L0402060"
        l6$(076) = ",U0C603918C626718EE46334K04L0405020,U0FE02D8C631318C7E2319CK04L04168,U"
        l6$(077) = "0FF026C63199CC627118CCK04L04080,T011EFE26318IC633188C48K04L08088,U08F0"
        l6$(078) = "71318CI6319BC4638K04I0201I08,T01A04D998C623318CEE233P029E01,T0118C8IC6"
        l6$(079) = "31198C46711BL04J030,W04CI63198EC623388EO0189804,W0C62FF18CC66331AFC6Q0"
        l6$(080) = "B0,W05F919CE7FF3198C662003FFL039C4,W04398CC623398DC671FFQ0484,W0C3CC66"
        l6$(081) = "311ACC56378ER03C4,W0896633198E66331CC6S082,W061A3198CC6I318C62K08I020,"
        l6$(082) = "W041918CC6631998C632K0800403,W04798C662338CDC671BK08808005021,W07BCC63"
        l6$(083) = "311AC6563D8DK08K020A0,EFF8S0312318IC631B18DE3K08K06810,E23CS03I18CI631"
        l6$(084) = "8D8C733K08L040080,B118S02198C632318C4C6319R0280,988CS021CC631919C62631"
        l6$(085) = "CDS060,8C48S01166318D8D6I398E48R070,C62CS0112318C6C73199CC7E8R0D2,E318"
        l6$(086) = "S031918C626318CC66378J0CM016,B18CS021D8C6313F8C4E331DC007E8M018,98CCT0"
        l6$(087) = "1EC6319BDC62B198DFFCP018,8C6CS061A6318DCD63398CC6CR018,C63CS0609318DE6"
        l6$(088) = "73198C662CJ04M018,E31CS080998DE23318CC6I3CJ04I01002,IF8R0380IC631198C4"
        l6$(089) = "E319B8J04I04008,V0600FEFB198CC62B18CECJ04I020,V0400E3318CC6E3398C648J0"
        l6$(090) = "40,V06023198C663B198C6328J050,V060218CC623198CC631B8J042,V03020C663118"
        l6$(091) = "CC46318D8J080,V01021633198CE62718C48J0C040,078T0C213198CC6F3358C638J08"
        l6$(092) = "0,703U0218C662339CCF631B0,6018S0421C63311BC747B18DK080,4008T021E31998C"
        l6$(093) = "E3A7F8FD0,4008S0800B18IC671F5ECE78,6018R01I098CI6B38DCD733CJ040,703S02"
        l6$(094) = "I08C63231BC4E7B19EJ020,186W0C6319D8D6263F8CF8I010,V08I0F318D8C7I39CC47"
        l6$(095) = "CJ08,1ES03J01F8C7C6319BCC623E0,7F8X03C636318CE77F31EJ04,64CR02K016I319"
        l6$(096) = "C463B198EJ04,I4P0F78001I0B1F98C6231A0CCDF13E2,44CO03EJ03I098IC7I38E047"
        l6$(097) = "81F801,678I018J07EJ07800CDI63199C603C0CJ080,37K08I01M04004723318CC66J0"
        l6$(098) = "8J080,Q06M04007F1199C4633J08I0140,P01EK01E7007198CF6231BC,7FCM03EJ07E3"
        l6$(099) = "FE278CC663318DEM01,018M03K0C0603FCC6637199C638004I010580,00CI0180068K0"
        l6$(100) = "2I03D623198CD631E01CI01,M0801L02K07198C662318C7024,00CJ0C01L08K03IC6I3"
        l6$(101) = "18C63814,7FF8I0E02K01M0C6633998C633E0CJ080,40CJ0606J01CM0E231AICE319B0"
        l6$(102) = "CJ080,M043K038M07118C646B18CDEEJ08020,M027K0CN0398CE32398C467EJ080,7FD"
        l6$(103) = "8I022J01P0CC7B1B18C6233E,M01EJ06P066318D8C63319E,M01EI03Q03B38C4C63198"
        l6$(104) = "EEJ04020,00CK0FI02R05CC626718CC7EJ04010,7FFK03901S02C6I3D8C4633J040082"
        l6$(105) = "0,00D8gG01E3199CC623398I04004F0,00C8gH0B18CC66331CD8L0248,gK0198C46331"
        l6$(106) = "9EC74N02,gK018C623398CDE36I020,7FD8gH0F6331ACC7E31CI02K040,gL01B198ECE"
        l6$(107) = "3B79CI02K020,gM078CC66B7FFCC0,gM03C4673BIFEC0,3F8gJ01F2759KF7I01K010,4"
        l6$(108) = "4CgK0BFBF703F1FEC00C0,"
        l6$(109) = "44CgK0JF203BCFE6F1,"
        l6$(110) = "648gK0F07FEFF8DE330F,"
        l6$(111) = "37gL0726"
        l6$(112) = "F83FE771A02,"
        l6$(113) = "gN013EE003FED8C02M08,"
        l6$(114) = "gO09F8001FDCC604L070,"
        l6$(115) = "1FgM043L0E63F4J0"
        l6$(116) = "7B,738gM04L0I3FCJ040,"
        l6$(117) = "40CgU0FBFCI0F,"
        l6$(118) = "40CgU01C7E7E20,"
        l6$(119) = "608gW07F,"
        l6$(120) = "7FF8gV03802"
        l6$(121) = "10,h038021020,"
        l6$(122) = "10h01C021E0FC0,"
        l6$(123) = "10h01C021I046,"
        l6$(124) = "10hG0C8K03880,"
        l6$(125) = "10hG0E81L080,"
        l6$(126) = "10hG074108"
        l6$(127) = "I0740,"
        l6$(128) = "10hG034108003830,"
        l6$(129) = "10hG017F0801C006E0,"
        l6$(130) = "10hI07062M040,"
        l6$(131) = "10H01E22CM010,"
        l6$(132) = "10hI0C7C6"
        l6$(133) = "M010,"
        l6$(134) = "10hI0C440E,"
        l6$(135) = "10hI0AI03L020,"
        l6$(136) = "10hI04I01CK010,"
        l6$(137) = "10hI0BJ0C0,"
        l6$(138) = "10hI03J040,"
        l6$(139) = "10hI018I06K010,"
        l6$(140) = "hO0EJ020,"
        l6$(141) = "10hO0C,"
        l6$(142) = "10hN01C,"
        l6$(143) = "10hO0A0060,"
        l6$(144) = "10hO0A0080,"
        l6$(145) = "10hO030080,"
        l6$(146) = "10hO0106,"
        l6$(147) = "10hO0180,"
        l6$(148) = "10hP0D0,"
        l6$(149) = "10hP070,"
        l6$(150) = ","
        l6$(151) = ","
        l6$(152) = ","
        l6$(153) = ","
        l6$(154) = ","
        l6$(155) = ","
        l6$(156) = ","
        l6$(157) = ","
        l6$(158) = ","
        l6$(159) = ","
        l6$(160) = ","
        l6$(161) = ","
        l6$(162) = ","
        l6$(163) = ","
        l6$(164) = ","
        l6$(165) = ","
        l6$(166) = ","
        l6$(167) = ","
        l6$(168) = ","
        l6$(169) = ","
        l6$(170) = ","
        l6$(171) = ","
        l6$(172) = ","
        l6$(173) = ","
        l6$(174) = ","
        l6$(175) = ","
        l6$(176) = ","
        l6$(177) = ","
        l6$(178) = ","
        l6$(179) = ","
        l6$(180) = ","
        l6$(181) = ","
        l6$(182) = ","
        l6$(183) = ","
        l6$(184) = ","
        l6$(185) = ","
        l6$(186) = ","
        l6$(187) = ","
        l6$(188) = ","
        l6$(189) = ","
        l6$(190) = ","
        
        
        l7$(001) = "^JO"
        l7$(002) = "^XA^EG^XZ" 
        if ctrlflag$ = "Y" then ~
           l7$(002) = "^XA^EG^XB^XZ"             /* CR1051 suppress start */
        l7$(003) = hex(7e) & "DGLABEL7.GRF,13695,33,"
        l7$(004) = "S01,S02,,::::S08,,::S08,S04,::S06,S04,,::S0"
        l7$(005) = "2,S01,:T0C0,T0CL02,T02L08,T03J0C0010,T018003010,U08J08106,U0CJ090,U060"
        l7$(006) = "0805080,W040011,U0704,U0284L020,U0144,V0A4,V044,:V02M010,V010,,W02L020"
        l7$(007) = ",,gJ020,W0170,W0130,X030,X0B0,X030,:X03K040,X040,X080,:,::::X080,,::::"
        l7$(008) = "::hM04,hI01782080,hI04IFE40,hG01239FC380,hG023D8C61F240,gY021CFCC63FFC"
        l7$(009) = "10F0,gX065FFC66FFC74I04,gX017FFE338I0C2003,:gV07F0DFE31984I0200180,gU0"
        l7$(010) = "1F138C618CFFC002I060,gS030730CC670C663EL058,gR0840F186635C6331F004J01,"
        l7$(011) = "gR080198E331CE31987004K0E0,gQ010018C7198C318CC38N04,gP041982C618CC618C"
        l7$(012) = "7E1808L0180,gQ01006630C6630C6C7F008M038,Y08T063186331C67006008N07C0,Y0"
        l7$(013) = "4R07C918E3198E7CT06FE0040gR0E488C718CFFEK01O0204,gQ01FC84618CFFM01O04,"
        l7$(014) = "gO0C039CE230C7CN01,gP0C384B1186EO02N0C8,gP087DF988E7P02M01,g08O01FBDFE"
        l7$(015) = "47CI03EK02P01D,gP01F3881FFJ0C0EU01F80,gP0733C806J03I0EI04Q0668,:gP0F24"
        l7$(016) = "64L04J0E004M01I07E9C0gO07DA432K018K0704Q03FFC0gM01F8CEE198J02M078M02J0"
        l7$(017) = "3FF0gM030C66B8C6J08N08M02J010D0gM03C6379C62I01O08M04,gM02E31D8636I04O0"
        l7$(018) = "8M04,gM02318DC31C0018W04,gM0218C9618FE02O01N04L020gM030C6938C7CFCO01N0"
        l7$(019) = "4,gM03C6379C643FP01N04,gM02E318C637CEP01N04L040W01O02318C6318Fg04,X08N"
        l7$(020) = "0618C6398C6Q02N04,W024N070C631CC7AQ02N04,W028N05C63186636Q02N04,W04O04"
        l7$(021) = "E318C31DCg04,gM06318C6198CQ04N04,gM0718C638E44Q04N04,:gM05CC631CE24Q04"
        l7$(022) = "N04K01,gL025F6318731CQ04N04K01,gM073B18C718CQ078,W08P01F8C798C8Q0838L0"
        l7$(023) = "8,gL0800CC7F8C68Q0801EK08,W08Q067F7FE38Q08I0CEI08,V04R033186318U011,V0"
        l7$(024) = "DN02I0398C319X08010,W08Q01CC61EDQ01L06010,U046N04I016638E7Q01L0181E,V0"
        l7$(025) = "AR01331C7FCP01M04C070,gO01B98631F8W03I0780,V04M01K0FCC318CFCN02R03C,gQ"
        l7$(026) = "07F98C661EM02S01E8,V04M02N07C676007L02U08,gS01E39EI03EJ02,R01001X0B18E"
        l7$(027) = "K0F,Q03002O08O0D8D8L038T020,:gT07CFN01E4Q078790,Q08Q01P01F8P04Q080030,"
        l7$(028) = "Q0800CN06gI04P030,gI0FQ01Y0800C0,Q02Q0FQ01X01E03,Q01Q098P01P018L01332,"
        l7$(029) = "gI088P01P018M080C,gH018CgG018L018,S08N01E6gG018K0580,gH01738O02W08,R06"
        l7$(030) = "O0118CO02V078,gH018C4O02P02K0C0,gH03E64O02P02K040,gH02734O02P02K040,gH"
        l7$(031) = "0219Cg02K040,gH030CCO04V040,gH03864O04V040,gH02E34O04N0784K040,:gH02FF"
        l7$(032) = "CO04N041F8J040,gH0218F8N04Q01EI060,gH070C41FgG07C060,gH05864I0FT08L0FC"
        l7$(033) = "0,gH04E34J01FI08N08M018,gH07F1CL03F08N080,gI0F8CN03EN080,gI0ECCO083FT0"
        l7$(034) = "10,gI0E64O08001ER010,gI0E38T03EP010,gI0F1CV03FN010,gI0988W01N010,gI08C"
        l7$(035) = "4N01N01,gI0864N01N01,gI0E34N01N01N020,gI0F1CN01W020,gI0F8EN01W020,gH01"
        l7$(036) = "8C6W02N020,:gH07E62W02N020,gH0C733W02,gG01E799N02N02,gG0336CD8M02N02N0"
        l7$(037) = "40,gG071E668M02N02N040,g0198E33CM02W040,g038C719CM02W040,g02C678CEM02W"
        l7$(038) = "040,g0E636C66V04N040,X07F2318633V04,W01F3118E319V04,W037188C718C8L04N0"
        l7$(039) = "4,W0638C4698C6CL04N04N080,W070C6238C634L04N04N080,W09863118631CL04W080"
        l7$(040) = ",V018C3188E318CL04W080,V03C718C4F18C4L04W080,V03638C6298C64L04N08N080,"
        l7$(041) = ":V0230C6318C638U08,V071863188633V08,V038C318CE31EV08,V01C718C611FW08M0"
        l7$(042) = "1,W0638C63FCEN08N0F8L01,W010C63186BF8L08N0807F8I01,W0186318E28003FEI08"
        l7$(043) = "N08I047F81,W01C318C718K01F88V07F0,X0B18C618CL0188R04K03FF,X088C630C4L0"
        l7$(044) = "188R04O07F0,X0C4631862N08R04R080,X0C2318E33N08M01J04,W017118C719V01J04"
        l7$(045) = ",W01788C618C8M08M01J04K02,W023C4630C64M0BFEK01J04K02,W043E2318632K02K0"
        l7$(046) = "3FE01J04K02,W0833118E319K02M0203P02,V0103988C718D8J02g02,:V02008C4618C"
        l7$(047) = "6CJ021Y02,U01I0C6230C633J021Y02,U04I0E31186319EI021Y02,T02I033188E318C"
        l7$(048) = "3F0021L04K018K02,T0CI0318C4718C611E021L04K018,S01J078C6218C6308F821L04"
        l7$(049) = "K018,S04J04C6310C631C46721L04K018,S08J0C631986318C631E1L04K018,R05K0C3"
        l7$(050) = "18CE318CF3FFE1L04K018,W01718C6718C7118C7DL04K018,P01L0138C6318C6308C63"
        l7$(051) = "FL04K018K04,W030C6318C631C4631FL04K018K04,P04L0386318E6319E23186L04K01"
        l7$(052) = "8K04,W02C318C7318C3118C2L04K018K04,P08L02718C6198C6188C62L04K018K04,W0"
        l7$(053) = "338C630CC670C463AY04,O01M018C6318663DC6231EY04,X0C6318E3318E31186Y04,:"
        l7$(054) = "X07B18C7198C3188C2Y04,Y07FF618CCE18C462Y04,O06Q01B0C67B0C623AR02L04,gH"
        l7$(055) = "0586331C6311ER04L04,O082FP04E3198E31886R04L04,O09FFE4N02718FC318C42R04"
        l7$(056) = "L04,O07401CN0218C6618C622R04L04,S06N070C6730C631ER040,S03N0586379C6318"
        l7$(057) = "6R040,S01CM04E31BCE318C2L08K040,T0EM04718E6318C62L08K08P01FE,T02M0618C"
        l7$(058) = "6318C63AL08K08P0E,T03M0F0C671CC631EL08J038K0507F,T018L0586358E63186L08"
        l7$(059) = "J037FCI08F80,U08L04E31CC3318C2L08I038,U08L06718E6198C62L08003E0,U04L07"
        l7$(060) = "18C630CC63AL0801E,U04L078C631C6631EL086C0,:U02L06E6338E33186L09C,gH047"
        l7$(061) = "31AC3198C2L0E4,V08K06198E618CC62L084,V0CK0B0CC630C663EK0D04X040,V07CI0"
        l7$(062) = "3186671C6331EJ01304,V028I058E33D8E319FAJ06004W01,V018I08C719CC319MFC00"
        l7$(063) = "4,W08001C618CKFC4N04W08,W08001630C7630C704N04W08,g033186331C6204N04,g0"
        l7$(064) = "31JF98E3204N04V010,W060078C718CC31E04N04,W05BC4E619C6618C04N04,Y03FE30"
        l7$(065) = "D6330C404N04,g0F3187319C6404N04,g0998E318CE3404N04,g08CC718C631C04N04N"
        l7$(066) = "03F78J010,g046619C6318C04N04N04084J020,:g06330D631CC404N04N08004J040,g"
        l7$(067) = "011987318E6404W08I010,gG08CE318C33604V01J0180040,gG0C6718C619E04V02K04"
        l7$(068) = "0080,W08I0E319C630CE04V04K02,W06I0B18D631C6704V08K01,W0E00198E7318E338"
        l7$(069) = "4N0CL088K08C080,W0E0018C7318C319E4L01DCK01EL0420,W0E003C6198C618CF4L02"
        l7$(070) = "02J07EL0141080,W04002F30DC630C67CL0401DC008M01E0880,W0800239877F1C633C"
        l7$(071) = "L08I02008N06,V0380031FEIF8E319CK01J0203N02004,V03I018FF99DC318CCK0EJ02"
        l7$(072) = "06N070,V02J0C678CC618C64K08J0189N08003,V06I02630C663FC63L08K071N08001,"
        l7$(073) = "V06I023186331FE338I09FM01N08001,V06I0318E3198EF19C001EN01,V07I038C718C"
        l7$(074) = "C37IC0036N01M01I02,:V06I02C618C661CFE4002O01M010201,V04C002630C6330C6F"
        l7$(075) = "C004O01M01010180,V0380033186319C631E078O01M0200C0C0,V018C0398E318CE719"
        l7$(076) = "B98P01M02012040,V01FC02CC718C6318D3F8P01M0205D0,V01FF026618C631CC6204P"
        l7$(077) = "01M0202,V023CFE330C631CC66204P01M040210,V051F071986318E63C208P01J02008"
        l7$(078) = "0010,V07404D8CE318C331820AU028F004,V0230C8C6718C619B0201P01K0180,Y04C6"
        l7$(079) = "318C630EE0200CS0188C010,Y0C637FC631C6C0180BET0980,Y05F98E739IF80040118"
        l7$(080) = "I03FFCL01CE10,Y0438C7318C33I040107FFR02410,Y0C3C6198C61AI0401V01E10,Y0"
        l7$(081) = "89630CC630EI0401W0408,Y061B186631C6I0401N02J02,Y04198E3318E2I04018M02I"
        l7$(082) = "08030,:Y0478C7198C32I0401N02201I068104,Y079C618CC61AI0403N02L0105,Y021"
        l7$(083) = "630C6630EI0400CM02L018E80,IFEU0313186331C6I04006M02L074080,F11EU03118E"
        l7$(084) = "3198E2L04M02M02002,F88EU0218C718CC31L04U014,EC46U021C618C6619L01V03,E6"
        l7$(085) = "22U011630C6330DM08U0380,E312U0113186319C7M078T0688,318EU03198E318CE3N0"
        l7$(086) = "8K03O0B8,F8C6U021CC718C63FI02J08I03FAO0C0,2C62V01E618C63F9I02J053FCR0C"
        l7$(087) = "0,E632U061B30C63701I02J068T0C0,E31AU060998637901I02K08J01O0C0,F18EU080"
        l7$(088) = "8CE378CCJ02K08J01J010010,IFET0780C6718C62J02J03K01J040040,X0C00FF7EC63"
        l7$(089) = "2J02J02K01J02,X0800F18C631E08002P010,:X0C0238E6318E08002J04K014,X0C021"
        l7$(090) = "C7318C208M04K01080,X070216198C6208M04K020,X03021B0CC63C0C001J04K03018,"
        l7$(091) = "Y0C20986631C0B001J04K020,07FV04208E3318C010018I020,1E38V020C7198CC01C0"
        l7$(092) = "1CI060,300CU0420E18CC6FF87016I08K020,3004V020F0C6631CC081100F80,2004U0"
        l7$(093) = "800586331C64041083080,2004T03I04E3198EB2060044078J010,300CT04I04718CC3"
        l7$(094) = "1A02002800CK08,1C18X0618C67D8F01881800FK04,041T01J078C6330C701883I018J"
        l7$(095) = "02,W06K0FE31BC630048K080,1F8gG0F18EE3100384J04J01,3BET04L058CE319I082J"
        l7$(096) = "02J01,226Q03DFI01I02C7B18D80082J01F08F880,:I2P01F8J03I02631CC780082K01"
        l7$(097) = "FC0040,326J03K03F8J078003358E63CI01K0CJ020,1BCJ01K08M040011CC331EI01K0"
        l7$(098) = "8J020,03P03N04001FC6199FJ08J08J058,R0FL01E7801C630CF6C04080,Q01FK0FE3F"
        l7$(099) = "F09E31C663604078N04008,3FEN018J0180601FF318E371A04007I04J040B0,006J03I"
        l7$(100) = "034L02J0F58C3198F84I0E01CJ040,006J01I08M08J03CC618CC784I01044J040,M010"
        l7$(101) = "01M04K01C630C66384I02024,002J01801L01M0F31C6332K01014,1FF8I01C02L02M03"
        l7$(102) = "18E339CL060CJ020,306K0C06K038M038C31AC802J020CJ020,202K087L0FN01C618C7"
        l7$(103) = "002J018EJ02002,N04FK03P0E30CE2002K07EJ020,3FE4J046K04P031C7B2002K012,N"
        l7$(104) = "03EJ018P018E31AN016,N03EI01CR0F338FN026J01802,:N01FI01T0DCC7801K038J01"
        l7$(105) = "801,006L079008T02C63601K011J0180081,3FFCgJ01E319C1K021J018004F80,0064g"
        l7$(106) = "K0B18C61K041N02640,I04gJ0198C62K0E41P010,gN018C63BK0DCL04,3FE4gK0F631D"
        l7$(107) = "08203800CJ04K02,3FE4gK01B18708205818CJ04K01,gP078C30820BFFCC0,gP03C610"
        l7$(108) = "829BEFEC0,0F8gM01F3910I7IFK02L080,1FCgM01B3DE1FFCD83EJ04,326gN0BFEFDC0"
        l7$(109) = "3C0E1I038,I2gN0JFC807BC3F03C40,226gN0F21FCBFF0061843C0,"
        l7$(110) = "3BCgN0739BE0FF0"
        l7$(111) = "10048080,"
        l7$(112) = "1B8gN011FB80031A403008M040,"
        l7$(113) = "gR087EI01FC4I01M0380,:gR040CL08I01"
        l7$(114) = "K07D8,"
        l7$(115) = "1FCgO021M030031K06,"
        l7$(116) = "306gY0E07EJ0F0,"
        l7$(117) = "202h0E179FC2,"
        l7$(118) = "202hG013C0,"
        l7$(119) = "30EhH0"
        l7$(120) = "A0041,"
        l7$(121) = "3FFChG08004101,"
        l7$(122) = "hK050041E07E,"
        l7$(123) = "hK040041I0218,"
        l7$(124) = "hK022L01C2,"
        l7$(125) = "hK01A02L02,"
        l7$(126) = "hK01D8208I01D,"
        l7$(127) = "hL0D8208001C0E0,"
        l7$(128) = "hL05FE0800EI0DC0,"
        l7$(129) = "hM040040EL02,"
        l7$(130) = "hN0E061N040"
        l7$(131) = ",hM03C22EN018,"
        l7$(132) = "hM0187C7N018,:"
        l7$(133) = "hM01844070,"
        l7$(134) = "hM01480018L020,"
        l7$(135) = "hN08J0EL018,"
        l7$(136) = "hM0"
        l7$(137) = "16J06,"
        l7$(138) = "hN068I02,"
        l7$(139) = "hN038I03L018,"
        l7$(140) = "hT078J020,"
        l7$(141) = "hT070,"
        l7$(142) = "hT0F0,"
        l7$(143) = "hT04800C0,"
        l7$(144) = "hT06801,hU"
        l7$(145) = "0C01,"
        l7$(146) = "hU040C,"
        l7$(147) = "hU06,"
        l7$(148) = "hU0320,"
        l7$(149) = "hU01E0,"
        l7$(150) = ","
        l7$(151) = ","
        l7$(152) = ","
        l7$(153) = ","
        l7$(154) = ","
        l7$(155) = ","
        l7$(156) = ","
        l7$(157) = ","
        l7$(158) = ","
        l7$(159) = ","
        l7$(160) = ","
        l7$(161) = ","
        l7$(162) = ","
        l7$(163) = ","
        l7$(164) = ","
        l7$(165) = ","
        l7$(166) = ","
        l7$(167) = ","
        l7$(168) = ","
        l7$(169) = ","
        l7$(170) = ","
        l7$(171) = ","
        l7$(172) = ","
        l7$(173) = ","
        l7$(174) = ","
        l7$(175) = ","
        l7$(176) = ","
        l7$(177) = ","
        l7$(178) = ","
        l7$(179) = ","
        l7$(180) = ","
        l7$(181) = ","
        l7$(182) = ","
        l7$(183) = ","
        l7$(184) = ","
        l7$(185) = ","
        l7$(186) = ","
        l7$(187) = ","
        l7$(188) = ","
        l7$(189) = ","
        l7$(190) = ","        

        l8$(001) = "^JO"
        l8$(002) = "^XA^EG^XZ" 
        if ctrlflag$ = "Y" then ~
           l8$(002) = "^XA^EG^XB^XZ"             /* CR1051 suppress start */
        l8$(003) = hex(7e) & "DGR:LABEL8.GRF,16488,36,"
        l8$(004) = "T010,,::T020,::T0C0,S01C0,,:S01E0,:::T020,:"
        l8$(005) = ":T030,::T010,:T018,U08,U0CL01E,::U078K03D04,U038I0F0013A,U03C0070101C2"
        l8$(006) = ",V06007016183,V0300F01E18080,V0301F01E18080,:V0303I0E10080,V0383M040,V"
        l8$(007) = "01F3M070,W0FBM070,W073N08,W038N08,:X08N08,X04M078,X038L070,Y0FL070,Y0F"
        l8$(008) = "8K070,Y0FC0,::Y0840,Y07C0,Y03C0,g04K080,g0CK080,::g080,Y07,::Y07LF,:Y0"
        l8$(009) = "MF,Y07,:,::::::hQ07C0,hM03KF0,hM0F1IFB8,hK03FF0C33DFC0,:hK03FF0C33CFC0"
        l8$(010) = ",hI07IF9E61867FFE0,h03FBIF0CF3CC30EF1F80,h0LF8679E7986F0060,gY0FFDC30F"
        l8$(011) = "F31C33CF3F003C,gX03FFEF98679EE186798I03,:gX07FFE798679E6186798I03,gT01"
        l8$(012) = "87FF0CF3CF70CF3CC30D8I02C0,gT0186F9EE39E7F8619E79878K06,gT038F3CF7DC70"
        l8$(013) = "CF30CF3CF38L070,gT010F8E79EFB8679E63867BN0E,gT0139F71CF7DF30CF3DC30FN0"
        l8$(014) = "3F0,:g06S017DF30C33CF30CF3CC30FN03F4,g07S017EF9E619E798619E7987P0FF807"
        l8$(015) = "C0g03S01F73CF3DC73CF30C33CF2P0DFC1060gT03FB8E79EFB8E79E619E7EP08J040g0"
        l8$(016) = "1P060037DF71CF7DF70CF3CC70EO078I0380g03EP01878EFBEE39EFB8619E7B86N07FJ"
        l8$(017) = "04,:g03EP01878679E6186798619E7984N07FJ04,g03FP017DC73CF3CC33DFF0C33CF4"
        l8$(018) = "N04K04,g02FQ07EFB8E19E798JFE1867CN08001E,g03FQ0C77DF71CF3DF701FFDC33CN"
        l8$(019) = "08001FC8,g01EP03E38EFBEE39EFEI03FF98CN0CI0FFBE0gQ03F79C77DF7DC7F8K0IF8"
        l8$(020) = "N04I0IFE0:gQ07F7DC70CF3CC3F8K0IF8N0CI0JF0gP0FE3BEFB8E79E7BCM03F8N08J0I"
        l8$(021) = "F0gO01F71C77DF71C33FO07N078J011E0gO01FBCFB8EFBEE3BEO07N070,gO011DF7DC7"
        l8$(022) = "7DF7DF8O07N07M010gO018C33EF38EFBFCP0FN07M010:gO018E39EFB8619F8P0CN07M0"
        l8$(023) = "10gO01F71C77DF71DFQ08N070,gO01FBCE38EFBEFCQ08N07M020Y0FO011DF79F77DFFR"
        l8$(024) = "08N07M020X01FO038E3BCFB8EFCR08N07M020X01F8N03E71C67DC7F8Q018N07M040:X0"
        l8$(025) = "1F8N03F3CC30EF3F8Q018N07M040X01F8N03FBCE38EFBF8Q01O07M040X03CO031DF79C"
        l8$(026) = "77FFR01O07M040X01P078E3BEF3FEFR01O070,gO0FF71C779F71R01O07L0380gN03IFC"
        l8$(027) = "E38FFBFR03O07L0380:X04O07IFE798F7DFR03F8M07L0380X06O07JF79CF3CFR03F8M0"
        l8$(028) = "FL0380X06O0F38FFBEFB8EER020FFL0C0,W076O0F9E71KF7ER02007F0CI08L04,W0FEN"
        l8$(029) = "018CF3EKFBER06I01FFI08L04,W0FEN038619FF9C77DER04K01C0080,:W0FEN03F30C3"
        l8$(030) = "BCF3FF8R04L030180,V027EN03F38E3BEF3FF8R04L030180,V03FCN0779E71C77DFFER"
        l8$(031) = "04L01C3FE,V03F8N078EF3CE38EIFCQ04M07E03FC,V03F8N0DC619E7DC77DIFO03CM03"
        l8$(032) = "CI0FF8,V03F8M07CF30C33EFF8EFF3FCM038S01FF8,:V037N07679E6187FFC7FE007F8"
        l8$(033) = "K038U038,V017N0I7DE71C7IF7FE003FCK038U018,S03C01FN0F38EF3CE383FBEEJ07F"
        l8$(034) = "8I0380,R0IF408M01F9C719E7B01FDF6K03FF80380,Q01FE3F8N018EF38C33E00FEFCM"
        l8$(035) = "01FF6S0FFE10,Q03E61EO038779E61BC007FF8O03CR07E07F0,:Q03F3CCO07F30CF3CF"
        l8$(036) = "8I01EQ04Q01CI010,Q03F3CCO07F38EFBCF8I01EQ04Q018I010,Q01F9F8O0779C71DEE"
        l8$(037) = "J018Q04N0C007,R0FC38O070CF38E3CJ01R04M01FF0CJ020,R01618O05867DE718J01R"
        l8$(038) = "0CN09FF8J020,R013FO03CF30EF3FK01R0CM03C07K020,R01BEO03E798619EK01R0CL0"
        l8$(039) = "7F80,:S0FCO03F0CF30FCK03R08L0EN01C0,S0FP03B0CF38FCK03R08L0CN01C0,S0CP0"
        l8$(040) = "398679E6L03R08J01F8N01C0,gJ03CF30CFCL02R08J018O01C0,gJ07E79861CL02R08J"
        l8$(041) = "01P01C0,gJ07B0CF398L02Q018J010,:gJ0798679FM02Q018J01P02,gJ0798679FM02Q"
        l8$(042) = "01K01P02,gJ07CF30FEM06O0E01K01P02,gJ07FF987N06O0IF8J01P02,gJ067IF6N06O"
        l8$(043) = "0E03FF8001E,gJ079C7IFM04N018I01FF81E,:gJ0FCF30C7FF8J04N01M0FFE,gJ0FCF3"
        l8$(044) = "081FFCJ04N01M0FFE,gJ0FE79F8I0IF004N01O0F80,gJ0FF0CFL07FFEN01O01,gK0786"
        l8$(045) = "7O0IFCK01O01,gK06F37007F8J0C03FFEI01O01,:gK0679FC1FFCJ0CJ03FF01O01,gK0"
        l8$(046) = "679FE1DFCJ0CJ01FF03O01,gK070CF7FC3CJ08L01FFO01,gK058679E61CJ08N03O07,g"
        l8$(047) = "K04F30CF3CCJ08N02O0F,gK06798619E4J08N02O0E,:gJ01F0CF30C3CJ08N02O0EO08,"
        l8$(048) = "gJ03F0CF30C3CJ08N02O0EO08,gJ03F8679E61CJ08N02O0EO08,gJ07FF30CF3CCI038N"
        l8$(049) = "02O0EO08,gJ0FF798619ECI078N02O0EO08,gI01C30CF30C3CI07O06O0E,:gI03FB867"
        l8$(050) = "9E7F8I07O06N018,gH03F3EF30CFCK07O06N01O010,:gH0718679861CK07O04N01O010"
        l8$(051) = ",gH0FCC70CF30CK07O04N01O010,gG01DE7F8679ECK07O04N01O010,:g0FFC33EF30CF"
        l8$(052) = "7K0FO04N01O010,Y0FFE618679861BCJ0CO04N010,Y0FFE6186798619EJ08O04N010,X"
        l8$(053) = "01FCF3CC70CF70C2J08O04N03O060,X038619E7F8679E63J08O0CN03O0E0,X03F30C33"
        l8$(054) = "CF30CF3FJ0FO0CN02O0E0,:X0679E6186798619FJ0FCN0CN02O0E0,X0F0CF3CC70CF30"
        l8$(055) = "C3I038EN08N02O0E0,:X0D8619E7D8679E61I07C7N08N02O0E0,W07CF30C33CF30CF3D"
        l8$(056) = "800EF3N08N02O0E0,W07E79E61BE798619FE3IF98M08N020,:X0F0CF3CFF0CF30C3IF3"
        l8$(057) = "8DEM08N020,X078619E7FFE79E61867987EM0FFM02N01,X038619E7IF79E618679878M"
        l8$(058) = "0FF8L02N01,Y0F30C33CLFDC30DF3N081JFI06N01,Y0F9E61867987OFN08I07KFN01,Y"
        l8$(059) = "0FCF3CC30CF30C3BIF8EM038I06J07JFCI01,:Y0FE19E798679E61867986M078I04L02"
        l8$(060) = "0BJF9,Y0F30C33CF30CF3CC33DFCM07J04L02K07F,Y0F30C33CF30CF3CC30DFCM07J04"
        l8$(061) = "L02L0F,Y0F9E6186798619E79FF7CM07J04L020,Y0FCF3CC30CF30C33CFB0CM07J04L0"
        l8$(062) = "20,X018E19E798679E618679IF8K07J04L060,:X03F70C33CF30CF3CC37IF1KFC7J04L"
        l8$(063) = "060,X06FBE6186798619E79FF7EK0IFJ04L060,X0679E6186798619E79F77EK0C7FJ04"
        l8$(064) = "L060,X0F0CF3CC30CF30C33CF30EK08L04L060,V01F98679E798679E61867B87K08L04"
        l8$(065) = "L040,U03FFCF71C33CF30CF3CC37FF3K08L04L040,U07F8E7BEE186798619E79F7798J"
        l8$(066) = "08L0CL040,:T01FDF73CF7CC30CF30C33CF30DEI018L0CL040,T0F9EFB8E7BE798679E"
        l8$(067) = "61867B86EI018L0CL040,S01F86798619E798679E61867B87EI018L08L040,S01FC30C"
        l8$(068) = "F30CF3CF30CF3CC3IF3FI018L0CL040,R0FFEE38EF9EE39EFB867LF79FI018L0CL040,"
        l8$(069) = "Q03IF7DF77CF7DC77DF3FCKF0CF80018L0CL040,:Q0FEE3BEFB8E7BEFB8EF9E639E7B8"
        l8$(070) = "6180018L08L040,Q0DE71C77DF71CF7DF73CF3DC73FF30C0018L08L040,Q0DF7CC33DF"
        l8$(071) = "70C33CF30CF3CC30FF30C0018L08L040,P07EFBEE38EFBE619E7B8FF9E798679E40018"
        l8$(072) = "L08L040,P0FF1DF7DC77DF3DC73DF70C33CF30CF60018L08L040,P0FBEE3BEFB8EF9EF"
        l8$(073) = "B8EFBE619E7B861E001M08L040,:O01FDF71C77DF71CF7DF73FF3CC73FF30FC01M08L0"
        l8$(074) = "C0,O018EFBCE38EFBEE39EFB8E79E7B8E79E7C01M08L0C0,O018E7BEE1867BEE186798"
        l8$(075) = "619E798679E7C01M08L0C0,O01C71DF7DC73DF7CC33CF71C33CF30CF3E01M08L040,O0"
        l8$(076) = "1F3EE3BEFB8EFBE7B8E7FEE1867B8619F01M08L040,O037DF7DC77DF71DF3DF73FF7CC"
        l8$(077) = "33FF30C381L01M0C0,:O038FFBEE38EFBEE39EFB8E7BE798E79E61C1L03M0C0,O03KF7"
        l8$(078) = "9C77DF7DC73DFF1DF3CF70CF3CE1L03M0C0,O03KF7DC30CF7DC30CFF0C33CF30CF3CE1"
        l8$(079) = "L01M0C0,O01F803FBEFB8E7BEF98679E61867B8619E79L01M080,S01FC77DF71DF7CF7"
        l8$(080) = "7CF7DC30FF30C33DL01M080,T03FB8EFBEE3BEFBFE7BEF98679E6187L01M080,:U0FDC"
        l8$(081) = "77DF7DC73DFF1CF7CF70CF3CC7L03M08J07FC,U0FEF38EFBEFB8E7BEE3BE7B8619E7BL"
        l8$(082) = "0EL0E8001IF4,U0FEFB8619EFB8679E61867B8619E7BK01EL0E8003FC,U07F7DF71CF7"
        l8$(083) = "DF37CF3CC30EF30C33FK03F3CI03FDIF80,U03F8EFBEE3BEF9FE79EFB8679E6187K0FD"
        l8$(084) = "KFE1FC,U03DF77DF7DC77DFF1CF7DF70CF3CC3I03FC0040,:U01EFB8EFBEFB8E7BEE3B"
        l8$(085) = "EFB8619E7B007FEI040,V037DC71DF7DF73CF7DC77EF30C33F0FF8J040,V038EF38E3B"
        l8$(086) = "EFB8E7BEFB8E79E61877CL040,V038EFBEE39EFB8E19E798679E6187FCL040,V01F77D"
        l8$(087) = "F71C77DFF0CF3DF70CF3CC784L040,W0FB8EFBEE38EFBEE39EFB8619E7F04L040,:W07"
        l8$(088) = "FF71DF7DF77CF7DC77FF30C3FE04L04R0180,W03FFB8E3BEFBFE7BEFB8EF9E61FC04L0"
        l8$(089) = "4R0F,X0F7DE71C77DF71CF7NFC004L04Q070,X077DF70C33DF71CF7NFC004L04Q070,X"
        l8$(090) = "078EFBEE38MFEFB8619EC004L04Q070,X07F71DF7DC7FDF3DC7FDF30C3C004L04Q070,"
        l8$(091) = ":X06FBEE3JF8EF9E7B8EF9E61C004L04Q040,X03FDF7IF7DF71CF7DF70CF3DC004L04Q"
        l8$(092) = "080,X03IFBFE38EFBEE39EFB8619EC004L04Q080,X03IFBFE18E7BEE186F98619EC004"
        l8$(093) = "L04Q080,X03KF7DCF3DF7CC3FCF30C3C004L04Q070,X03FBEE3BEFB8EFBE7B8E79E61C"
        l8$(094) = "004L04Q070,:X037DF7DC77DF71DF3DF70CF3CC004L0600IF8K080,X038EFBEE38EFBE"
        l8$(095) = "E39EFB8619EC004L0601FF98J0180,X03861DF79CF7DF7DC7FDF30C3C004L0601I0C3I"
        l8$(096) = "03,X03C71DF7DCB0CF7DC3CCF30C3C004L0601I07FI03,X02FB8E3BEFB8E7BEF98679E"
        l8$(097) = "61C004L0603I03F8001,X037DE7DC77DF71DF7CF33CF3CC004L0606K0C002,:X038EFB"
        l8$(098) = "EFB8EFBEE3BEFB8E19EC004L020CK0F806,X03C71DF7DCF7DF7DC7FDF70C3C004L0278"
        l8$(099) = "K09C04,X03F38E3BEFB8EFBEFBCE7BE61C00CL027L0C6,X03FB8E39EFB8619EFBC679E"
        l8$(100) = "61C00CL06FL0C202,X07FDF71C77DF71CF7DF30CF3D87FCK01F8L04102,X078EFFEFB8"
        l8$(101) = "EFBEE3BEF98679FBC0608003F8L01F882,:X07C71IFDIFDF7DC7FDF71C37803FC002N0"
        l8$(102) = "1F802,X0EFB8E3KFEFBEFBCE7BEE3CJ0603EN017870,X0F7DFF1LF1DF7DF73DF7F8J02"
        l8$(103) = "07CN030030,W07F0CFBCFF8EFBEE3BEFB8E7BEK038FCM01E800C,W07F8EFBCFF8EFBEE"
        l8$(104) = "39EFB8619EK03EDCM01C800C,W07DC71DF79F77DFFDC7FDF31FFL0F9CM01CI04,W07CF"
        l8$(105) = "B8E3BEFB8EFFEIFEFBIFCL01CM01C40,:W07E7DFF1C77DF71KF73IFDF8K01CM0304008"
        l8$(106) = ",W07F0EFFCE38EFB8IFEFB8FFBEFCK01CM020380C,W07F871DE79877DE71FF7DF71CF7"
        l8$(107) = "CK01CM020040E,W07FC71DF79C73DF71IFDF70C33CK01CM0200406,X0EF38E3BEF38EF"
        l8$(108) = "BELFE61BCK01CM0600602,X07F9FF1C77DF71FF7DFF8CF3DFCK01CM0607F,:X0FFCFFE"
        l8$(109) = "E38EFBEF3BEFF8E79EF8K01CM0607F,W04JFDF79C77DF7DC77DF71CF78K01CM0C03980"
        l8$(110) = ",W07EFF8E3BCF30EFBCE3IFBEE3CL01CJ0F0F803980,W07EFF8E3BEF38E7BEE1F7F9E6"
        l8$(111) = "1CL01CJ0F1F803980,W07F07E71C77DC71DF7DF7FCF3DCL01CJ0IF0040,X08F7F3EE38"
        l8$(112) = "EFBEFFBEFB8EF9F8L01FC3FC7FI080,:g0719IFC77DF7DC77DIFCF8L08J023F70080,g"
        l8$(113) = "07QFEE38EFBFE3807IFEM0F0C080,g07F71C77DC71DF79CF7DF7JF600EM01EC180,g07"
        l8$(114) = "F71C77DC71DF7DCF0DF3JFQ01EC180,g03F3CE38EFB8FFBEFF8FF9F8K0EN01C180,g03"
        l8$(115) = "F9E7DC77DE7DC77DF7EC38K0FO040,:g03EC33EFB8EFBEFB8EFBE638K0180038CC0020"
        l8$(116) = "40,g03FE1877DC71DF7DC77DF3D8K013C3CI0E02040,g03FFCC38EF30DFBEFF8FF9ECK"
        l8$(117) = "01M0606040,g03FFCE38EFB8FF9EFF87F9ECK01M06160,g03FFE79C77DE7DC77DF7FC3"
        l8$(118) = "CK01M0F0FC38,JFV01FE33CFB8EFBEFB8EFBFE1CK01M0F85C38,:JFV01F71867DC71DF"
        l8$(119) = "7DF77DF7CCK01N0FDC78,F0CFV01F7CC30EFB8E3BEFF8EFFECK01N017080,D86FV01FF"
        l8$(120) = "E79877DE7FC77FF71FF4K018N07C80,D867V01FFE79C77DF7FC73DF30E34K018N07C80"
        l8$(121) = ",CF3FV01FE3BCF38EFBEE38EFBEFFEK018N07CC0,E79FV01F71C679C71DF79F73DF7FF"
        l8$(122) = "8I01F8N05FC0,:F0CFV01F3CE30CFBIFBEFF8EFBFFBKFO09E,D867V039FE79867IF3C7"
        l8$(123) = "7FF71DJF8001O01E,CF3FV03EE33CF3FEFBEE38EFBEE3F8J018I03C0011E,E79FV07E6"
        l8$(124) = "1867FC719E79873DF7DF8K08I072001,E79FV07E71C77FC71DF79C70CF3DF8K08I0730"
        l8$(125) = "11,JFU07EFBJFEF38E3BEF38E7BFF8K08I03F87E,:7FFEU07E1KF779F73C77FF71DFF8"
        l8$(126) = "K08J060,Y07B0FFBCF30CFBFE38EFBEE3EL0CJ020,Y07DEF18679861DF79C73DF7FCL0"
        l8$(127) = "BE0,Y03CFFCC30CF30E33CF30E7BFCK0701800C0,g0CFFCE38EF38E3BEF38619FCK0F0"
        l8$(128) = "1C00C0,g061DE79C779E71C77FF31CFCK0780C1F,:g070C3BCF38CF3FE38FFBEE3CK07"
        l8$(129) = "007C0,3FFW03E61C779C619F7FC7FDF7DCK060,7FFEV03FFCC30CF30FFBFF3FE7BFCK0"
        l8$(130) = "10,601EV031DE798679E7FC7FBFF1FFCK010,601EV031DE79C77DE71C7JF0FFC0,C00F"
        l8$(131) = "V070C33CF38EF3CE3KFE3E0,:400FU07DE618779C71IFDF77FF7DFCK08,701EU0FCFFC"
        l8$(132) = "C38CF3FE3FEFB8FFBEFEK04,783EU0F61DE79C679F61E77DF7FCF7F8J02,1W01F30C33"
        l8$(133) = "CF30DF3CE38FFBEE3BF80,X01F30C33CF38FFBCE38FFBEE18780,3F8U03F9E618679C7"
        l8$(134) = "1DE79C7JFCC38J01C0,:7FCR07IF8CFFCC30EFF8E37CFB8FF3EFBFC001FE0,6C6Q01JF"
        l8$(135) = "861FE79877FE71FE7DC7E1F7F7JFC20,CC6K08K03FF0CF30FF3CF3FEF3CFB0FFBE03FE"
        l8$(136) = "0FFI010,6FEK08J01IF8679EFD8679F619E7987FDEK08J018,6FCK08J03FFD8679EFD8"
        l8$(137) = "679F71DE79C78DFK08J018,7F8P07F3CF30CFFDC30FF38E3FCF38EF8J08J03C,:R01FD"
        l8$(138) = "8679LF9C77DE71C679C7FCJ08J03380,7FCO01FCC30COF0EF3CE30CFBFFCI0CJ011CF8"
        l8$(139) = "0,7FEK08001FDE7986IF8007FF871DFF986FDF7F807CJ0103880,00EK0C003FC33CF30"
        l8$(140) = "FK0FEF30C3BCF38EFBFC1FCJ01I040,006K0F007E618679FCL0E79E618679C719FC0F4"
        l8$(141) = "J01I040,006K0F007E618679FCL0F79E71C779F71CFC0F4J01I040,004K0F80EF3CC30"
        l8$(142) = "DFM070CFBCE38CFB8E3F87CJ01I040,:7FFEJ0F80E19E79CF8M03861DFF9C67DF71FC0"
        l8$(143) = "EJ0180040,7FFEJ079FF0C33DFEN01F30E3FCF3FEFBEFF8EK080040,404K07BF9E6187"
        l8$(144) = "FCO0F9E61C679F71DF7FFEK080040,7FCEK0FCCF3CC7Q07CF3FC30CF38E3BEFEK08004"
        l8$(145) = "0,7FCEK0F8CF3CC7Q03CF3FE38EFB8E39EFEK080040,7FFEK0F8619EFCR0F19F79C77D"
        l8$(146) = "F71C7FEK080040,:O07F70C3CS03IFBCF3FEFBEE3FEK0C0060,004L03FFEFFU01F1C67"
        l8$(147) = "9FF1DF7DFFK0C0020,7FFEL0JFCV0FCC30CFB8E3BEFBK0C0013F0,7FFEgM07E79867DE"
        l8$(148) = "71C7FF8J04I0CF0,7FFEgM05E79C77DF71C7FF8J04I0858,006FgM0633CF38EFBEFF8F"
        l8$(149) = "CJ04K0F,:I0EgM07F8779FF1DIFC7FJ04L0C0,7FCEgM03FC30CFF8E3IFB8J04L020,7F"
        l8$(150) = "CEgN01F9867DFF3IFD8J06L030,gS0FCF30CNFJ038K01C,gS0FCF30ENFJ038K01C,1Fg"
        l8$(151) = "Q07F7BPFCI078K01C,:7FCgP07BRFEI0CL01C,6FEgP03LF61KF71C1C,CC6gP03KFEMFB"
        l8$(152) = "IFN02,6FEgP01F7BFF8LF7F803N02,6FEgQ077IF07KF7F002N02,7FCgQ01BFFC003FF8"
        l8$(153) = "F8F00EM01E,:04gS087FJ0FF7DC7F9CK01IF,1FgS0738M0FFBFFCK0F981,7FCgR07EN0"
        l8$(154) = "3JFCI03FC001,60EhI0JFEIFE0,C06hL0FF83040,C06hL0FF02040,:60EhL0FE03060,"
        l8$(155) = "7FFEhK07F0303IFC0,hO07D010380FE180,hO03E8102I03FF8,hO03F8102K038,hO03F"
        l8$(156) = "8182K03C,:hO01FC183J03FE,hP03E783I0FE0780,hP017F8101FCI07B08,hQ07F8187"
        l8$(157) = "N01,hQ03F04FCO010,hQ03F0EFCO010,:hR060F8FO0F0,hR067901F8M0E0,hR07FI01C"
        l8$(158) = "M060,hR078J0EM010,hR05F8I060,hR01F8I060,:hR01FJ072L010,hY03CK0E0,hY0F8"
        l8$(159) = "0,hY0FC003,hY03C004,::i0E00C,i078F0,i07D80,i01F80,iG02,"               
        l8$(160) = ","
        l8$(161) = ","
        l8$(162) = ","
        l8$(163) = ","
        l8$(164) = ","
        l8$(165) = ","
        l8$(166) = ","
        l8$(167) = ","
        l8$(168) = ","
        l8$(169) = ","
        l8$(170) = ","
        l8$(171) = ","
        l8$(172) = ","
        l8$(173) = ","
        l8$(174) = ","
        l8$(175) = ","
        l8$(176) = ","
        l8$(177) = ","
        l8$(178) = ","
        l8$(179) = ","
        l8$(180) = ","
        l8$(181) = ","
        l8$(182) = ","
        l8$(183) = ","
        l8$(184) = ","
        l8$(185) = ","
        l8$(186) = ","
        l8$(187) = ","
        l8$(188) = ","
        l8$(189) = ","
        l8$(190) = ","        
/*        l8$(141) = "^XA"  */
/*        l8$(142) = "^FO200,200^FR^XGR:LABEL8.GRF,1,1^FS" */
/*        l8$(480) = "^PQ1" */
/*        l8$(481) = "^XZ" */

        l9$(001) = "^JO"
        l9$(002) = "^XA^EG^XZ" 
        if ctrlflag$ = "Y" then ~
           l9$(002) = "^XA^EG^XB^XZ"             /* CR1051 suppress start */
        l9$(003) = hex(7e) & "DGLABEL9.GRF,13024,32,"
        l9$(004) = "S02,S04,,::::R010,,::R010,S08,::S0C,S08,,:S04,"
        l9$(005) = "S02,:S01,S01M08,T08K020,T0EI03I020,T07I04040,T01J020608,:T018I0240,"
        l9$(006) = "U0C010141,W080046,U0E08,U0508L080,U0288,U0148,V088,V04M040,V020,,W04L0"
        l9$(007) = "80,,gJ080,W02C0,W0240,X040,W0140,X040,:X04J01,X080,W01,:,::::W01,,::::"
        l9$(008) = "::hL040,hH01BC108,hH047FFE4,hG0A3CFE38,h013DC631E24,gX010E6C631FFC18F,"
        l9$(009) = "gW032FFC637FC74I040,gX0BFFE39CI0C20030,gU03F86FF31CC2I020018,gU0FC9C63"
        l9$(010) = "1867FC002I06,gR01838C66338C331EL0580,gQ0220786331AC6198F004J010,gQ0200"
        l9$(011) = "CC3198E638CC7004K0C,gQ0400C718CC631C6638N080,gO010641638C6631863F1808L"
        l9$(012) = "030,gQ040330C63318C363F008M0380,X07T031863198C63806008N07C,X03R01E48C3"
        l9$(013) = "18CC67ET06FC008gQ03A44718C67FEK01O02080,gQ07E4238C67F8L01O040,:Y08N070"
        l9$(014) = "0C6710C63EN010,X01CO030E2588637O02M01C80,X01EO021F7CC4338O02M020,X026P"
        l9$(015) = "07EEFF31EI03EK02P01A0,X032P07CC40FF8I0407U01F0,X01EO01CCE4038I018007I0"
        l9$(016) = "4Q0CD,gO03C932L02J07004M02I0FD38gN03F6919L0CK0704Q07FF8gM07C33B8CCJ01M"
        l9$(017) = "078M04J07FEgM0C719AC63J04N08M04J021AgM0E38DE631J08N08M080,gM0B0C7631BI"
        l9$(018) = "02O08M080,gM09863718EI0CW080,gM08C3258C7F81O01N08L04gM0C71A4C63F3EO01N"
        l9$(019) = "080,gM0E38DE6320F8O01N080,W01O0B0C6331BF3P01N08L08W07O09863198C7CY080,"
        l9$(020) = "W0DN018C318CC638P02N080,W068M01C718C663C8P02N080,W078M01638C6331B8P02N"
        l9$(021) = "080,W0EN0130C6318EFg080,W0CN01986318CC7Q04N080,gL03CC318C723Q04N080,gL"
        l9$(022) = "0F6718C6713Q04N08K020gL0F7B8C6398FQ04N08K020V02N013DCC6338C7Q0780,V03N"
        l9$(023) = "0118FE31CC64Q0838K01,V03N038C731FC634Q0801EJ01,:U01BN06C639FDFF1CQ08I0"
        l9$(024) = "CE001,U03BN04630CC6318CU0110,U01BN0E31866318C8W0802,V03M01B18C3318F68P"
        l9$(025) = "01L0602,U0DCM0118C7198C738P01L0183C0,U0D4M038C638CC63FFP01M0480F,U078M"
        l9$(026) = "02C630EE6318FEW03I078,U078M0663187F318C73EN02R0380,U058M0F318C41FCC639"
        l9$(027) = "0FM02S03D,U048M0998C74003E33D0038K02T01,U07M018CC63CI0F1C7I01EJ020,R07"
        l9$(028) = "803M01C6630CI058C3K0F8,Q0FCD8N02633188I06C76L01CT02,P018C68N063198F8I0"
        l9$(029) = "3E7CN0F4Q078F2,P01C638N0718CC7K0FCP04Q08006,P01E31O0D8C667W04P03,Q0F19"
        l9$(030) = "N01CC633CL08X0801C,Q058EN01C6319CL08W01E060,Q02C6N016318D8L08P08L01364"
        l9$(031) = "0,Q0266N01318C7M08P08M08180,Q023CN0318C66X08L0180,Q013O038C63CX08K058,"
        l9$(032) = "Q01EO02C637M01W080,R0CO0263F8M01V0780,gH0331O01P01K0C,gH0799O01P01K04,"
        l9$(033) = "gH04CDO01P01K04,gH0467g01K04,:gH0633O02V04,gH0719O02V04,gH058DO02N03C2"
        l9$(034) = "K04,gH05FFO02N020F8J04,gH0467EN02Q01EI06,gH0E3107Cg07C06,gH0B19I07CS04"
        l9$(035) = "L07C,gH098DK07C004N04M0180,gH0FC7M0F84N04,gH01E3N01FN04,gH01B3O040F8S0"
        l9$(036) = "1,gH019BO04I0FR01,gH018ET01FP01,gH01C7V03F8M01,gH0163W018M01,gH0131O08"
        l9$(037) = "M018,gH0119O08M018,gH018DO08M018M02,gH01C7O08V02,gH01E38N08V02,gH03318"
        l9$(038) = "V02N02,gH0F988V02N02,gG018ICV020,gG07CE64M01N020,gG0E6B36M01N02N04,g01"
        l9$(039) = "E399AM01N02N04,g06718CFM01W04,g0E18C67M01W04,:g0B0CE338L01W04,Y03986B1"
        l9$(040) = "98U04N04,X0FC8E318CCU040,W03CC4718C66U040,W06C6218C632L02N040,W0C6310D"
        l9$(041) = "631BL02N04N08,W0E31887318DL02N04N08,V01318C6318C7L02W08,V0318C6118C63L"
        l9$(042) = "02W08,V078C6309C631L02W08,V06C631856319L02N08N08,V046318E3318EU080,V0E"
        l9$(043) = "318C7118CCU080,V0718C6198C78U080,V038C630C47CV08M010,W0C63187F38M04N0F"
        l9$(044) = "CL010,W02318E31AFEL04N0803F8I010,W0318C718A001FF8004N08I047F810,W038C6"
        l9$(045) = "18C6L07C4V03F,W014630C63M044R04K03FF0,W032318631M044R04O0FF,W03118E318"
        l9$(046) = "8M04R04R08,W0388C718CCM04M01J040,W02C4618C64U01J040,W026230C632M04M01J"
        l9$(047) = "04K020,W0E31186319M05FF8J01J04K020,V01B188E318C8K08J01FF01J04K020,V031"
        l9$(048) = "8C4718C64K08L0103P020,V078C6218C636K08Y020,:U01CC6310C631BK088X020,U0E"
        l9$(049) = "C631886318C8J088X020,T07C6318C6318C67J088X020,S01E2318C6118C631FC0088K"
        l9$(050) = "02L08K020,S037118C6308C6318878088K02L080,S0E188C631846318C43E088K02L08"
        l9$(051) = "0,R01B0C46318E2318C6319C88K02L080,Q01B1866318C7318C6238C788K02L080,Q0F"
        l9$(052) = "F8E3318C6198C6338IF88K02L080,P03D8C7198C630CC631C8631E8K02L080,P078C61"
        l9$(053) = "8CC631866318C4318F8K02L08K040,P0CC630C66318E3318C6318C78K02L08K040,O01"
        l9$(054) = "E631863318C7198C6708C63L02L08K040,O013318E3198C618CC63184631L02L08K040"
        l9$(055) = ",O03198C718CC630C66318C2319L02L08K040,O038CC618C6631863319C7118DY040,O"
        l9$(056) = "06C6630C63318E3198F6388C7Y040,O046331863198C718CC630C463Y040,O063198E3"
        l9$(057) = "18CC618C663186231Y040,O0718CC718C6630C6I38C3119Y040,O058C6618C63318631"
        l9$(058) = "9EC7188DR01L040,O0CC6330C63198E318CC638C47R02L040,O0CFF1986318CC718C66"
        l9$(059) = "30C623R02L040,O0IFDCE318C6618C63F186311R02L040,O06803E718C6330C63198C3"
        l9$(060) = "189R02L040,R01F18C631986319CC718C7R02,S0F8C6318CE318DE638C63R02,S03E63"
        l9$(061) = "18C6718C6F30C631L04K02,:S01F318C6318C639986319L04K04P03FE0,S01F98C6318"
        l9$(062) = "C6318CC318DL04K04O01C0,T0FCC6318E6319C6718C7L04J01CK0507F0,T07E6318C73"
        l9$(063) = "18D6338C63L04J01BFCI08F8,T073318C6198C7318C631L04I01C0,T07198C630CC639"
        l9$(064) = "8C6319L04003F,T018CC631866318C7318DL0401E0,U0C66318E3318C6398C7L0434,U"
        l9$(065) = "063318C7198CE30CC63L04E0,U07198C618CC6B186631L0720,U038CC630C66398C331"
        l9$(066) = "9L0420,U01C6631863318C7198FK0682X04,V0FB318E3199C638CC7K09820,V07198C7"
        l9$(067) = "18CF630C67DJ03002W010,V038CC618C6731867LFE0020,V03C6630C633KF1N02W080,"
        l9$(068) = "V0163318631D8C71C1N02W080,V013198E318CC63881N020,V0118CC7IFE630C81N02V"
        l9$(069) = "01,W0CC6678C63318781N020,W0FF338C67198C301N020,W0E3FF86358CC7101N020,W"
        l9$(070) = "0B18CE31CC663901N020,W098C6718C6330D01N020,W08C6318C63198701N02N03F78J"
        l9$(071) = "01,W0C6318C6718CC301N02N04084J02,V016318E6358C67101N02N08004J0C,V01318"
        l9$(072) = "C731CC633901W08I01,:V0118C6198C6318D81V01J01800C,V018C630CC6318C781V02"
        l9$(073) = "K04010,V01C631866718C7381V04K020,V01E318E3358C639C1V08K010,V01F18C719C"
        l9$(074) = "C630CE1N06L088K08810,V01D8C618CC6318679M0EEK01EL044,V01CC770C66318C33D"
        l9$(075) = "L0101J03EL014210,V01E63BE63718C719FL0200DC004M01E110,V0I318F31DFC638CF"
        l9$(076) = "L04I02004N060,V07198C7FBFFE30C67L08I02018M020080,V078CC61FE67718633K07"
        l9$(077) = "J0203N07,V04C6730CE3318C319K04J01C48M080060,V0C6331863198FF18CK04K0388"
        l9$(078) = "M080020,V0E3198E318CC7F8CEI0478M08M080020,V0F18CC718C6637C67I0F8N08,V0"
        l9$(079) = "F8C6E18C6331BF33001B8N08L01I040,V0CC63B0C63198F3F9001P08L0102020,V0DE3"
        l9$(080) = "1986318CC71BF002P08L0101030,V07318CE318C6638C783CP08L020081C,V0318CE71"
        l9$(081) = "8C6I3C66CCQ08L020140C,V03FC6B18C63198634FCQ08L0205A,V03FE398C6318CF318"
        l9$(082) = "82Q08L02020,V047BF8E6318C6719882Q08L04022,V0A3F8C7318C6338F084Q08I0200"
        l9$(083) = "8002,V0E83C6198C6318C6085U024F0040,V0463630CC6318C6C0808P08J018,X03318"
        l9$(084) = "66318C7B80806S0184C01,X0318EFF18C63B00405F8S058,:X03FE719CE7FFE0020086"
        l9$(085) = "I01FFEL01CE1,X03C618CC6318C0020081FF8Q0241,X03E30C66318C8002008U01E1,X"
        l9$(086) = "02F1863318C78002008V04080,X0198E3198C638002008M01J020,X018C718CC630800"
        l9$(087) = "200CM01I080380,X01F618C663188002008M01101I0281040,X01E70C63318C8002018"
        l9$(088) = "M01L01050,X01F5863198C78002007M01L018E8,IFCU0FCE318CC6380020038L01L034"
        l9$(089) = "08,F11EU0EC718C66308K03M01M020020,D88CU0E618C633184K03U0140,CC46U0F70C"
        l9$(090) = "63198C4L04U030,C626U0FD86318CC74L02U038,E316U0CCE318C663CL01CT06880,F1"
        l9$(091) = "8CU0C6718C6330CM04K018N0B80,D8C6U0E318C6319FC001J04I03FDO0C,CC66T01B38"
        l9$(092) = "C6318FC4001J029FER0C,C632T019AE6318DC04001J034T0C,E31ET038E7318DE40400"
        l9$(093) = "1K04K08N0C,F18ET03C6198DE32J01K04K08I01001,IFCS01E630CC6319J01J018K08I"
        l9$(094) = "04004,W01E33FEFB18DJ01J01L08I020,W01B1FE3318C702001Q080,W0198C7198C630"
        l9$(095) = "2001J02L0A0,W01CCE18CC63102M02L084,X0C6B0C6631902M02K01,X0639863318E03"
        l9$(096) = "I08I02K018080,X0318E3198C602C008I02K01,:0FEU018C718CC6200400CI01,3C38T"
        l9$(097) = "01CE18C663200700EI03,300CT01AB0C6331BFE1C0BI04K01,200CT0319863198CF302"
        l9$(098) = "08803C,E004T0388E318CC639010840C4,200CT04CC718C6634C81802303CK080,300C"
        l9$(099) = "S01C6618C633186808014006K040,1C38S03E310C6319EC3C0440C0078J020,W063188"
        l9$(100) = "6318CC71C04418I0CJ010,W0F18C6318C6E38C024K04,1F8T0D8C6118C63B0C401C2J0"
        l9$(101) = "2K08,36CS01CC6708C63398640041J01K08,226Q0IFC63584631ECC360041K0F887C4,"
        l9$(102) = "E26P03FC4631CE2318C671E0041L0FC002,224J03K0FF62319E7118D6338FJ08J06J01"
        l9$(103) = ",33CJ01J039F3118C7188C7318C78I08J04J01,R07C7188C630C47F18C67CI04J04J02"
        l9$(104) = "80,Q01E618C467FFE2318C73DF0204,3FCN03F30C63MF8C6398D8203EN020080,3FCN0"
        l9$(105) = "3B186339F803FCC630DC6820018002J020B,006J03I0F98E3189CI03D6318663C2I070"
        l9$(106) = "0EJ02,006J010038CC718C7K0F318C331C2J0822J02,M01003C4618C7CK0718C7198C2"
        l9$(107) = "I010120,004J018066230C67L03CC638CC8K080A0,3FF8I01C0E311863CM0C630CE7L0"
        l9$(108) = "306J01,206K0C1F188E3F8M0E3186B201J0106J01,2M08798C473FN0718C31C01K0C7J"
        l9$(109) = "010020,N04F8C621FCN038C738801K03FJ01,:3FCCJ04EC6311CP0C63EC801L090,N03"
        l9$(110) = "C6319FQ0630C68N0B0,N03E318F8Q03D8E3CM013K08020,N01F18CES02F31E008J01CK"
        l9$(111) = "08010,006L07FCF8S01718D808K088J0800810,3FFCL038V0F8C6608J0108J0800478,"
        l9$(112) = "004CgJ04C63308J0208M0224,I04gJ0C6319K07208O01,gN0C318D8J06EL040,3FCCgJ"
        l9$(113) = "07D8C684081C004J04K020,3FCCgK06C6384082C0C4J04K010,gO01E3184085FFE4,gP"
        l9$(114) = "0F18840ADF7F4,1F8gM07CC883DBIF8J02L08,3FCgM06CEF0FFE6C1EJ040,"
        l9$(115) = "226gM02FF"
        l9$(116) = "7EF01E071I0180,"
        l9$(117) = "E26gM03IFE403DE1F01E2,"
        l9$(118) = "226gM03C8FE4FF8031821E,"
        l9$(119) = "33CgM01CCD"
        l9$(120) = "F03F80804404,"
        l9$(121) = "13gO047DC0018D203804M04,"
        l9$(122) = "gQ023FJ0FE2J08L038,"
        l9$(123) = "gQ0106L04J08J0"
        l9$(124) = "3D80,1FCgO088L0180188J020,"
        l9$(125) = "306gX0603FJ0F80,"
        l9$(126) = "E06gY0E0BCFC20,"
        l9$(127) = "206hG09E,30Ch"
        l9$(128) = "G0500410,"
        l9$(129) = "3FFCh040041010,:hJ028041E07E0,"
        l9$(130) = "hJ020041I02180,"
        l9$(131) = "hJ011L01C20,hK0D"
        l9$(132) = "02L020,hK0E8208I01D0,"
        l9$(133) = "hK068208001C0C,"
        l9$(134) = "hK02FE0800E001BC,"
        l9$(135) = "hL040040EL020,hM0"
        l9$(136) = "E061N04,hL03C226N01,"
        l9$(137) = "hL0187C3N01,"
        l9$(138) = "hL0184407,"
        l9$(139) = "hL01480018L02,"
        l9$(140) = "hM08J0EL01,hL0"
        l9$(141) = "16J060,hM068I020,"
        l9$(142) = "hM038I03L01,"
        l9$(143) = "hS078J02,"
        l9$(144) = "hS07,"
        l9$(145) = "hS0F,"
        l9$(146) = "hS04801C,"
        l9$(147) = "hS068020,hT0C"
        l9$(148) = "020,hT04180,"
        l9$(149) = "hT060,hT034,hT01C,"
        l9$(150) = ","
        l9$(151) = ","
        l9$(152) = ","
        l9$(153) = ","
        l9$(154) = ","
        l9$(155) = ","
        l9$(156) = ","
        l9$(157) = ","
        l9$(158) = ","
        l9$(159) = ","
        l9$(160) = ","
        l9$(161) = ","
        l9$(162) = ","
        l9$(163) = ","
        l9$(164) = ","
        l9$(165) = ","
        l9$(166) = ","
        l9$(167) = ","
        l9$(168) = ","
        l9$(169) = ","
        l9$(170) = ","
        l9$(171) = ","
        l9$(172) = ","
        l9$(173) = ","
        l9$(174) = ","
        l9$(175) = ","
        l9$(176) = ","
        l9$(177) = ","
        l9$(178) = ","
        l9$(179) = ","
        l9$(180) = ","
        l9$(181) = ","
        l9$(182) = ","
        l9$(183) = ","
        l9$(184) = ","
        l9$(185) = ","
        l9$(186) = ","
        l9$(187) = ","
        l9$(188) = ","
        l9$(189) = ","
        l9$(190) = ","

        l10$(001) = "^JO"
        l10$(002) = "^XA^EG^XZ" 
        if ctrlflag$ = "Y" then ~
           l10$(002) = "^XA^EG^XB^XZ"             /* CR1051 suppress start */
        l10$(003) = hex(7e) & "DGLABEL10.GRF,11280,30,"
        l10$(004) = "R060,R040,,::R080,,Q01,Q0180,,:Q0180,:R080,"
        l10$(005) = ":R0C0,R080,R0C0,,R060,R020,R01L02,R018K06,R018K0C10,S0CI0E1028,S060071"
        l10$(006) = "9048,S0300C091C8,S0300C09186,S0181C0D101,S01C2005801,S01C2007001,T0A2L"
        l10$(007) = "080,T056L080,T062L080,T012L040,gH040,U08L0C0,U04L080,U038K080,U01FK080"
        l10$(008) = ",U01FJ01,V098,V050,V07J01,V018I01,V01J01,:V02J02,V04J02,V040,V04J02,:V"
        l10$(009) = "0KFC,V0C0,V040,,:::::hH038,gW0403F1C4,gY0C0E03,gW01FJ0180,gW038K076E0,"
        l10$(010) = "gU03FCM031F,gS01E43FM030180,gS063C7BM020060,gR038I02M020030,gR0CJ06M02"
        l10$(011) = "I08,gO0CF78J04M04I0F,gN0190CK06M04J0E0,gN019M04M04J038,gN018U0CK06,gM0"
        l10$(012) = "418U08L0C0,W08P01V08L03C80,V01CP01V08M03E00E,W0EQ04T018M03IF180gN01U01"
        l10$(013) = "N02I03,W02N0C02U01N04I02,W07N08C2U06M0FCI04,W0F8N09CU06M08J04,W0D8N01P"
        l10$(014) = "07CJ06L01801F08,W0C8N01O01C3CI02L01001F88,W078N06O03007CO01001DD980W01"
        l10$(015) = "O08O0EI07CO0800FFB80gL03O018J07CN08007FFC0gK0FCO06L078L018I0FFE0gJ01Q0"
        l10$(016) = "CM04L03J06BA0gJ01P03N04L03L020gJ01P06N04L02,gJ01O018N08L02L040gJ01O03O"
        l10$(017) = "08L02L040gJ01O04O08L02L040gJ01N01CO08L02,V0CV03O018L02L080U01CV0CO018L"
        l10$(018) = "02L080U036U018O01M02L080U03ET01CP01M02K01,U03CT03Q03M02K01,U03U0EQ02M0"
        l10$(019) = "2K01,U02N06K018Q02M02K02,gJ0EK07R02M02K02,gI01EFJ0CR02M02K02,U0CM01BF8"
        l10$(020) = "003S07CL06K02,T01CM0313F802S047EK04,T01CM0399CC3CS0407EJ04K04,T03CM06C"
        l10$(021) = "CE5E7F8Q04003DC004K04,T07CM0CI67I0CQ0CI07300CK04,T07CM0E233J0CQ0CJ0180"
        l10$(022) = "8,T03CL01B11BI018Q08K0808K08,S01B8L01998DI018Q08K061FCJ08,S01B8L03IC7J"
        l10$(023) = "0F8P08K0330FCI08,T0FM06E663K0FEN018L0E007C008,T0FM067231K017EM01Q07E10"
        l10$(024) = ",T0BM0F3919K0103FL01R07F0,T09L01B9D8DK03001FCJ01S030,Q0100EL019CEC7K02"
        l10$(025) = "J07EI01S010,P0FDFCM0267332K02L03F82Q07820,P08CFCM063399EK02M01FEP03DFA"
        l10$(026) = "0,O01C678M0719DCCK02O06P0E00E0,O01E33N0D8CEECK06O02O0180020,P0F1BN0CC6"
        l10$(027) = "678K04O02L0200F0,P058FN0C6I38K04O06L07E18I040,P06C6M016319BL04O04L04FF"
        l10$(028) = "J040,P0266M01318DEL04O04L060EJ040,P033CM0138C6CL04O04L0EL040,P0338M011"
        l10$(029) = "C638L0CO04K0FC,P01FN020E3EM08O04J01CM080,Q0CN0203FCM08O0CJ0FN080,g02Q0"
        l10$(030) = "8O08I018N080,g02Q08O08J08N080,g02Q08O08J08,g02P018O08J08M01,gQ01P08J08"
        l10$(031) = "M01,g04P01O018J08M01,g06P01M01F9J018M01,g040FF8L01M03003F801C,g04001FE"
        l10$(032) = "K01M02J0FE1CM02,g04J03FEI03M02K03FCM02,gM07FC02M02M07M02,g06N0FF2M02M0"
        l10$(033) = "3M02,gG08N01FEL02M03M02,gG08O023FCJ02M02,gG08O02003F8002M02M04,gG08O02"
        l10$(034) = "J0FFO02M04,gG08O06K01FCM02M04,gQ04U02M04,g01P04U06M04,g01P04U04,:g01P0"
        l10$(035) = "4U04M08,g01CO04M04M04M08,:g06CO0CM04M04M08,g088O08M04M0CM08,Y0108O08M0"
        l10$(036) = "4M0C,Y0608O08M04M08L010,Y0808O08M04M08L010,X01008O08M0CM08L010,X02008O"
        l10$(037) = "08M08M08L010,X06008O08M08M08L010,W038018N018M08M08L010,V0740018N01N08L"
        l10$(038) = "018,V08I01O01N08L010,U018I01O01N08L01M020,U02J01O01N08L01M020,:U04J01O"
        l10$(039) = "01M018L01M020,U08J01O01M01M01M020,:g01O01M01M01M020,g01O03M01M030,g03O"
        l10$(040) = "03M01M030,U0CJ03O02M01M02M040,U03J03FEM02M01FCK02M040,V08K07FF8J02M010"
        l10$(041) = "IF8002M040,V04Q0FFEM01I02003FFCK040,V04S06M03I02K0IF80040,V0CS02M03I02"
        l10$(042) = "K0201IF40,V0ES02M02I02K06J03C0,V0ES02M02I02K060,V0ES02M02I02K040,U01BS"
        l10$(043) = "06M02I02K040,U039S07IF8I02I02K040,U07DS06001IFE2I02K040,T01E78Q024K027"
        l10$(044) = "EI02K040,T01E38Q024K06K06K040,T07318Q024K06K06K040,S01F998Q024K04K04K0"
        l10$(045) = "40,S0FDCD8Q024K04K04K040,R01CFE7R024K04K04K040,R076673R024K04K04K040,Q"
        l10$(046) = "01EI3BR024K04K04K040,Q01F199ER024K04K04K0C0,P03B19DCEN03E0024K04K04K0C"
        l10$(047) = "0,P0FF8CFECN06IFC4K04K04K080,O07CC6I38N08J04K04K04K080,O06E631998N08J0"
        l10$(048) = "4K04K04K08K080,O0F7318DDN018J04K04K04K08K080,O0F398C6FN01K04K04K04K08K"
        l10$(049) = "080,N0199CC637T04K04K04K08K080,N03CCE63138M02K0CK04K04K08K080,N036E731"
        l10$(050) = "998M02K0CK04K04K08K080,N0237398DCEM02K0CK04K04K08K080,N03139CC667CL02K"
        l10$(051) = "0CK04K0CK08K080,N0389CE6I3FFEJ02K08K0CK0CK08K080,N06CCE73119CE7J0EK08K"
        l10$(052) = "0CK08K08K080,N0E6E73998CE738I0CK08K0CJ01L08K080,N0EFFB9CDC6739CI0CK08K"
        l10$(053) = "08J03L08K080,N0JFCE66339DCI0CK08K08J01L08K080,N07E07E73319CECI08K08K08"
        l10$(054) = "J01L08K080,Q01F3918CE6C0018K08K08J01L08K080,R0F9D8C67380018K08K08J01L0"
        l10$(055) = "8K080,R07CFC63398I0CK08K08J03L08K080,R03E66319D8I08K08K08J02L08001FF80"
        l10$(056) = ",R01F3318CF80018K08K08J06K04801F8080,S0FDCC6338003L08K08I01LF3FK080,S0"
        l10$(057) = "6FE631980018K08K08I0F80C01EM080,S0I7318D8I08K08K080078008P080,S0I398C6"
        l10$(058) = "CI08K08K0827CI08P080,S0199CC62CI08K08K08FFJ08P080,T0DCE631CI0CK08K0BCK"
        l10$(059) = "08,T0EE7319CI08K08K0E4K08,T077398D8I0CK08K084K08,T0339CC7J0CK08J0D04K0"
        l10$(060) = "8P0E0,T01FCE66I018K08I03F04K08P040,T01FE73EI03L08I02004K08O0380,T01F73"
        l10$(061) = "98I03J07KFC004K08O04,T01F39D8I07JFE4M04K08O04,U0D9CFJ06K04M04K08O04,U0"
        l10$(062) = "4CE7J06K04M04K08O04,U066720IFCK04M04K08O08,U03B3E3804L04M04K08O08,U03F"
        l10$(063) = "FCE004L04M04K08O08,U03IF8004L04M04K08O04,U03C6EI04L04M04K08O04,U0231CI"
        l10$(064) = "04L04M04K0C03FFJ018,U0718CI04L04M04K0C04198I030,U078C7I04L04M04K0C0800"
        l10$(065) = "D80020,U06C638004L04M04K0C18007C0020,U066318004L04M04K041J060020,U0731"
        l10$(066) = "88004L04M06K047J050040,U0798C8004L04M06K04K048080,U07CC78004L04M06K044"
        l10$(067) = "J024080,U07E638004L04M0CK04CJ022040,U07733I04L04K01FCK0F8J011040,U0739"
        l10$(068) = "F8004L04K0302I01F8K078860,U0F9CFE004L04K0403BC03M078440,U0DCE638047K04"
        l10$(069) = "K0800E402M018240,T01CE761E7FFEJ04J01J040CN08240,T01E73E07F11FJ04J06J04"
        l10$(070) = "1CM0C0140,T01B39E0180038I04J0CJ0224L01A0180,T0319CEK01FI04J04J01E4L012"
        l10$(071) = "0080,T038CE6L0380640027CL04L0110080,T01C676M0E87C00788L04L031,T01319EM"
        l10$(072) = "047EI08N04L0208180,T01F8CEM040F8018N04L06040C0,T01CC66M0407E3FO04L0406"
        l10$(073) = "060,T01E632M040026P04L041F020,T01FF1AM04003EP04L041F8,T01FF8EM04003FP0"
        l10$(074) = "4L040B8,T03DFFEM060021P04K01C098,T01CFE4M060023P04I070F8088,T01A0FCM06"
        l10$(075) = "0023CO04I02DF01,U01CF8M0200234O07E7C1F806,V0ECCM0200207CM03I03EFC04,W0"
        l10$(076) = "CC3IFC00600302FCK01CJ01F304,W0FFE003IFE003004E183IFL039C4,W0FBM0200100"
        l10$(077) = "63IFP0C84,W0FBM020030060CJ01M03C4,W0FD8L02003006L01N082,W0F78L02003004"
        l10$(078) = "M08001EI062,W0F38L02003006M0F0060F2021,W0FFM0200300CM08C7800F021,W07FM"
        l10$(079) = "0200300CM08K022A1,IFCS07FM030030018L08K0F8D080,E73CS03BM03001001M08L04"
        l10$(080) = "1380,F39CS039M01001003M08L03F2,19CCS03DM01001801CL0CL0262,0CECS03FM010"
        l10$(081) = "018007L0CM076,C67CS037M010018001CK04M0F2,E33CS03B8L01I08I04K0CM0D6,F19"
        l10$(082) = "CS0398L0FI08I04003FFCL0198,18CCS06D8K07DI08I03IF00CL0118,CC6CS0678J03C"
        l10$(083) = "1I08I038J04L0118,C63CS0F38I03E01I08J08J04I0700218,E31CR01F18003E001I08"
        l10$(084) = "I018J04I01812,IFCR07D8870EI018008I03K04I05E3C,V078CIF8J08008I02K04I070"
        l10$(085) = "08,V06C7E02K08008I02K06I010,V0763AM08008O05J08,V0733AM08008I04K04EI08,"
        l10$(086) = "V039ACM0800CN0181C070,V01CE7M0E00CN01E06380,V01E63M0F006O0C07C,IFT07B9"
        l10$(087) = "L02DF07I02K080,E018S07EDL03C7058004K080,E018S0CE78K04C1C4E07C0,C008R01"
        l10$(088) = "E738K0C604420C40,E018R03F398K0C60661383CJ040,F038R073BD8J03030661F00CJ"
        l10$(089) = "020,F87S0F9FE8J06010020600FJ010,186R01CCF6F8I08010020E0018J08,U03C6737"
        l10$(090) = "E00180103A18I0CJ08,3F8R0363399F00100100E18I0CJ04,FFCP078F319DCD8020010"
        l10$(091) = "0208I03I01C,C4CO01IF98FEE780600100208I01JFA,C4CO07F99CC7673C0C00180208"
        l10$(092) = "J03FC01,E5CI018J0FFDCE67F39C1I018020CJ0EJ080,F78J08I03FIE733D9CC3J0803"
        l10$(093) = "06J08I01C0,17O0FC67739EDCE7EI0180302J08I0340,P01F6I39IFE77J03001038I0C"
        l10$(094) = "I0130,FFCM03F3199MFJ0600101EI0CI0110C0,FFCM03B98DCEFF0BFCJ04001001C004"
        l10$(095) = "I010D80,00CI01800FDCC6E778007FJ04001I0707CI0102,008I01C03CE7313FK078I0"
        l10$(096) = "2001I03034I018020,00CJ0C03673I9CK03CI020018003814I018020,IF8I0E07339CD"
        l10$(097) = "F8L0CI020018I0E1EJ08020,IF8I0E0F19CE7EM0EI02I08I070EJ08020,C0CJ067E8CE"
        l10$(098) = "77CM07I04I08I018EJ08060,M067CC673F8M03I0CI08J0FEJ08020,FFD8I0376633B8N"
        l10$(099) = "018018I08J016J08020,FFD8I03F3319EP0601J08J036J0C020,M01F918F8P0302J0CJ"
        l10$(100) = "02FJ04030,00CK0FC8CER0C6J04J03BJ04010,IFK03IFCR02CJ04J031J0400CE0,IF8K"
        l10$(101) = "0DFCS018J04J071J04005F8,00D8gH08J04J061J0600248,00C8gH08J04I0E41J02J06"
        l10$(102) = ",gL08J06003FC18I02J0180,FFD8gH0FJ02083C008I02K040,FFD8gH018I020B3B388I"
        l10$(103) = "02K020,gM05I020B7FFC8I02K010,gM03I020EIFE8I03K010,3FgK019A073KFCI03K01"
        l10$(104) = "0,E4CgK0BE317A3DDFEI0E0,C4CgK0FF39263FF3E1F18L08,E4CgK0FE3DAFF30E332FM"
        l10$(105) = "08,F78gK072EFC7F3161A03M08,37gL01AFF0039A40C02L018,gO0DBC001FDCI06K07F"
        l10$(106) = "8,1EgM04FL0CI04J07F80,FF8gL03CL0301CCJ0E0,E1CgT01EDF8039F80,C0CgU03F7J"
        l10$(107) = "F0,C0CgW05F863J04,IF8gV06E021J04,IF8gV02B021DF004,h035821F9FC0,h014821"
        l10$(108) = "I06E,hG0A8318003F80,hG0FC118J0C0,hG07C108I07C0,hG03E308003878,hG017F08"
        l10$(109) = "03E007F4,hH03F86EM060,hH01E73CM010,hI0E3CFM030,hI0C041FL020,hI0EC003CK"
        l10$(110) = "020,hI078001CK010,hI0FI01CL08,hI034I0EL08,hI03CI0EK010,hI01J07EJ020,hO"
        l10$(111) = "0CI01C0,hN01EI0E,hO0F00E0,hO0E0080,hO070180,hO0386,hO0184,hP0DC,hP078,"
        l10$(112) = ","
        l10$(113) = ","
        l10$(114) = ","
        l10$(115) = ","
        l10$(116) = ","
        l10$(117) = ","
        l10$(118) = ","
        l10$(119) = ","
        l10$(120) = ","
        l10$(121) = ","
        l10$(122) = ","
        l10$(123) = ","
        l10$(124) = ","
        l10$(125) = ","
        l10$(126) = ","
        l10$(127) = ","
        l10$(128) = ","
        l10$(129) = ","
        l10$(130) = ","
        l10$(131) = ","
        l10$(132) = ","
        l10$(133) = ","
        l10$(134) = ","
        l10$(135) = ","
        l10$(136) = ","
        l10$(137) = ","
        l10$(138) = ","
        l10$(139) = ","
        l10$(140) = ","
        l10$(141) = ","
        l10$(142) = ","
        l10$(143) = ","
        l10$(144) = ","
        l10$(145) = ","
        l10$(146) = ","
        l10$(147) = ","
        l10$(148) = ","
        l10$(149) = ","
        l10$(150) = ","
        l10$(151) = ","
        l10$(152) = ","
        l10$(153) = ","
        l10$(154) = ","
        l10$(155) = ","
        l10$(156) = ","
        l10$(157) = ","
        l10$(158) = ","
        l10$(159) = ","
        l10$(160) = ","
        l10$(161) = ","
        l10$(162) = ","
        l10$(163) = ","
        l10$(164) = ","
        l10$(165) = ","
        l10$(166) = ","
        l10$(167) = ","
        l10$(168) = ","
        l10$(169) = ","
        l10$(170) = ","
        l10$(171) = ","
        l10$(172) = ","
        l10$(173) = ","
        l10$(174) = ","
        l10$(175) = ","
        l10$(176) = ","
        l10$(177) = ","
        l10$(178) = ","
        l10$(179) = ","
        l10$(180) = ","
        l10$(181) = ","
        l10$(182) = ","
        l10$(183) = ","
        l10$(184) = ","
        l10$(185) = ","
        l10$(186) = ","
        l10$(187) = ","
        l10$(188) = ","
        l10$(189) = ","
        l10$(190) = ","

        
            init (" ") lbl$()
                                               /* Bramd Name       (10)*/
            lbl$(01%) = lb_brand$              & fs$
            if str(lb_brand$,1%,10%) = "WindowNati"  then ~
                 lbl$(01%) = "WindowNation" & fs$
            lb_brand_test$ = lb_brand$   /* pwww T E M P  */
                                               /* Series Number    (12)*/
            lbl$(02%) = lb_series$             & fs$
            lb_series_test$ = lb_series$ /* pwww T E M P  */
                                               /* Vinyl Window Sty (24)*/
            lbl$(03%) = lb_style$              & fs$
            lb_style_test$ = lb_style$   /* pwww T E M P  */
                                               /* Glass Option     (24)*/
            lbl$(04%) = lb_glass_op$           & fs$
                                               /* Res U-Factor     (04)*/
                                               /* (AWD014)             */
REM            if str(lb_resu$,2%,1%) <> "N" then                          ~
                          lbl$(05%) = "0" & str(lb_resu$,2%,3%)    & fs$~
                  else lbl$(05%) = str(lb_resu$,2%,3%) & fs$
/* (AWD015) */
            if str(lb_resu$,2%,1%) <> "-" then                          ~
                          lbl$(05%) = "0" & str(lb_resu$,2%,3%)    & fs$~
                  else lbl$(05%) = str(lb_resu$,2%,3%) & fs$

                                               /* Non-Res U-Factor (04)*/
            lbl$(06%) = str(lb_nonresu$,2%,3%) & fs$
            lb_nonresu_test$ = lb_nonresu$     /* pwww T E M P  */
                                               /* Res Heat Coeff   (04)*/
                                               /* (AWD014)             */
REM            if str(lb_resheat$,2%,1%) <> "N" then                       ~
                          lbl$(07%) = "0" & str(lb_resheat$,2%,3%) & fs$~
                  else lbl$(07%) = str(lb_resheat$,2%,3%) & fs$

/* (AWD015) */
            if str(lb_resheat$,2%,1%) <> "-" then                       ~
                          lbl$(07%) = "0" & str(lb_resheat$,2%,3%) & fs$~
                  else lbl$(07%) = str(lb_resheat$,2%,3%) & fs$
            lb_resheat_test$ = lb_resheat$    /* pwww T E M P  */

                                               /* Non-Res Heat Coef(04)*/
            lbl$(08%) = str(lb_nonresheat$,2%,3%) & fs$
                                               /* Design Pres Big  (02)*/
            lbl$(09%) = lb_dp$                  & fs$
            lb_dp_test$ = lb_dp$              /* pwww T E M P  */
                                               /* Design Pres Small(02)*/
            lbl$(10%) = lb_dp$ & "."
                                               /* Width an Height  (07)*/
            lb_x$ = hex(20) & "X" & hex(20) & hex(20)
            lbl$(11%) = lb_width$ & hex(22) & lb_x$ & lb_height$&hex(22)
            if len(lb_width$) < 2 then init(" ") lbl$(11%)
            if len(lb_width$) < 2 then lbl$(11%) = " "

                                               /* (12) Avail           */
                                               /* Seq Number       (16)*/
            lbl$(13%) = lb_seq$                 & fs$
                                               /* Dept and S.O.    (24)*/
            lbl$(14%) = lb_dep_so$              & fs$
                                               /* Load and Model   (24)*/
            lbl$(15%) = lb_ld_mod$              & fs$
                                               /* Production DTE   (24)*/
            lbl$(16%) = lb_pd_dte$              & fs$
                                               /* Glass Option 2       */
            lbl$(17%) = lb_glass_op1$           & fs$
                                               /* cpdnum  AWD021       */
            lbl$(47%) = lb_cpdnum$              & fs$
                                               /* (EWD002) - Vis Trans */
                                               /*   Res Visible    (03)*/
                                               /* (AWD014)             */
REM            if str(lb_resvisible$,2%,1%) <> "N" then                    ~
                       lbl$(18%) = "0" & str(lb_resvisible$,2%,3%) & fs$~
                else lbl$(18%) = str(lb_resvisible$,2%,3%) & fs$

/* (AWD015) */
            if str(lb_resvisible$,2%,1%) <> "-" then                    ~
                       lbl$(18%) = "0" & str(lb_resvisible$,2%,3%) & fs$~
                else lbl$(18%) = str(lb_resvisible$,2%,3%) & fs$

                                               /* (EWD002) - Vis Trans */
                                               /*   Non-res Visible(03)*/
            lbl$(19%) = str(lb_nonresvisible$,1%,4%) & fs$

                                               /* Text Line (1)        */
            lbl$(20%) = "This window has been tested in accordance with" ~
                                                      & fs$
                                               /* Text Line (2)        */
            lbl$(21%) = "AAMA/NWWDA 101 / I.S. 2-97, and has a Design  " ~
                                                      & fs$
                                               /* Text Line (3)        */
            lbl$(22%) = "Pressure of         Applies to windows up to  " ~
                                                      & fs$
                                               /* Text Line (4)        */
            lbl$(23%) = str(lbl$(11%),1%,17%) &"         in size.      " ~
                                                      & fs$
            str(lbl$(22%),14%,4%) = str(lbl$(10%),1%,4%)  /* Design Small */

                                              /* Wind Zone (EWD006)    */
                                              /* Wind Text (1)         */
            lbl$(24%) = "This fenestration product complies with the NEW" & fs$
                                              /* Wind Text (2)         */
            lbl$(25%) = "FLORIDA BUILDING CODE for residential buildings" & fs$
                                              /* Wind Text (3)         */
            lbl$(26%) = "with a mean roof height of 30 ft. or less," & fs$
                                              /* Wind Text (4)         */
            lbl$(27%) = "Exposure "& hex(22)&"B"&hex(22)&" and Wall Zone " ~
                         &hex(22)&"5."&hex(22)  & fs$
                                              /* Wind Zone (EWD007)     */
            lbl$(28%) = lb_zone$ & fs$
                                              /* Wind Pos/Neg Pressure  */
                                              /* (EWD007)               */
            lbl$(29%) = lb_pressure1$ & " / " & lb_pressure2$ & fs$
                                              /* Note may have Special  */
                                              /* Case Blank Lines       */
            if lb_pressure1$ = "      " and lb_pressure2$ = "      " then ~
          lbl$(29) = fs$
                                              /* (EWD009)               */
            lbl$(30%) = lb_txt1$ & fs$        /* No Grid, 5-8 or 1      */

            lbl$(31%) = lb_txt2$ & fs$        /* Dual Glazed            */
                                              /* (EWD009)               */
                                              /* (EWD012) Prod Barcode  */
           prt_barcode$ = str(lb_barcode$,1%,8%) & "-" & str(lb_barcode$,9%,2%) ~
                          & "-" & str(lb_barcode$,11%,4%) & "-"                 ~
                          & str(lb_barcode$,15%,4%)

           lbl$(32%) = prt_barcode$  & fs$
                                              /* (EWD012)               */
/* (AWD018) */
           trip% = 0%
           init(" ") gen_key$
           str(gen_key$,1%,9%)  = "PLANTRIPL"
           str(gen_key$,10%,2%) = str(part$,5%,2%)
           read #4, key = gen_key$, eod goto no_triple
             trip% = 1%
             lb_txt2$ = "TrplGlazed"
             lbl$(31%) = lb_txt2$ & fs$        /* Dual Glazed            */              
no_triple:
/* (\AWD018) */                                              
/* <AWD017> */
        gosub check_model
/* </AWD017> */

/* <AWD019> */
/* regions                    */
/* map 0  nada           0000 */
/* map 1  N, NC, SC & S  1111 */
/* map 2  N              1000 */
/* map 3  N & NC         1100 */
/* map 4  NC             0100 */
/* map 5  N, NC & SC     1110 */
/* map 6  NC & SC        0110 */
/* map 7  SC             0010 */
/* map 8  NC, SC & S     0111 */
/* map 9  SC & S         0011 */
/* map 10 S              0001 */

        s = 0
        sc = 0
        nc = 0
        n = 0
        d = 0
        door = 0
        U_Fac = 0.00
        SHGC  = 0.00
        map$ = "0"
        convert str(lb_resu$,2%,3%) to U_Fac, data goto bad_U
bad_U:
        convert str(lb_resheat$,2%,3%) to SHGC, data goto bad_H
bad_H:
REM   check for door???
REM        IF STR(PART$,1,3) = "311" THEN DOOR = 1
REM        IF STR(PART$,1,3) = "312" THEN DOOR = 1
REM        IF STR(PART$,1,3) = "313" THEN DOOR = 1
REM        IF STR(PART$,1,3) = "314" THEN DOOR = 1
REM        IF STR(PART$,1,3) = "315" THEN DOOR = 1
REM        IF STR(PART$,1,3) = "316" THEN DOOR = 1
REM        IF STR(PART$,1,3) = "332" THEN DOOR = 1
REM        IF STR(PART$,1,3) = "333" THEN DOOR = 1
REM        IF STR(PART$,1,3) = "334" THEN DOOR = 1
REM        IF STR(PART$,1,3) = "335" THEN DOOR = 1
REM        IF STR(PART$,1,3) = "336" THEN DOOR = 1
REM        IF STR(PART$,1,3) = "A41" THEN DOOR = 1
REM        IF STR(PART$,1,3) = "A42" THEN DOOR = 1
REM        IF STR(PART$,1,3) = "A43" THEN DOOR = 1
REM        IF DOOR = 0 THEN GOTO WINDOW
/* (AWD022) */
        if door% = 0% then goto window

/*      if U_Fac > 0.32 or SHGC > 0.30 then goto copy_map  */
        if U_Fac > 0.30 or SHGC > 0.40 then goto copy_map  /*AWD025 */
REM     if U_Fac <= 0.32 and SHGC <= 0.30 then n  = 1.....
/*      s  = 1       AWD025                                        ~
        sc = 1                                                     ~
        nc = 1                                                     ~
        n  = 1                                                     ~
        map$ = "1"                      AWD025 */
/*AWD025 + */
        if U_Fac >  0.30 or SHGC >  0.25 then goto skip_ssc
        s = 1
        sc = 1
skip_ssc:
        if U_Fac >  0.30 or SHGC > 0.40 then goto skip_nnc
        nc = 1
        n = 1
        goto skip_nnc
/*      goto copy_map    */
/*AWD025 - */

window:                           /* SR67154 */
/*      if U_Fac <= 0.60 and SHGC <= 0.27 then s  = 1~
        if U_Fac <= 0.35 and SHGC <= 0.30 then sc = 1~
        if U_Fac <= 0.32 and SHGC <= 0.40 then nc = 1 */
/*      if U_Fac <= 0.30                  then n  = 1   disable in 2016 */
/*      if U_Fac  = 0.31 and SHGC >= 0.35 then n  = 1   disable in 2016 */
/*      if U_Fac  = 0.32 and SHGC >= 0.40 then n  = 1   disable in 2016 */
/*AWD025 SR67154 + */
        if U_Fac <= 0.40 and SHGC <= 0.25 then s  = 1
        if U_Fac <= 0.30 and SHGC <= 0.25 then sc = 1
        if U_Fac <= 0.30 and SHGC <= 0.40 then nc = 1
        if U_Fac <= 0.27                  then n  = 1 /*enable in 2016 */
        if U_Fac  = 0.28 and SHGC >= 0.32 then n  = 1 /*enable in 2016 */
        if U_Fac  = 0.29 and SHGC >= 0.37 then n  = 1 /*enable in 2016 */
        if U_Fac  = 0.30 and SHGC >= 0.42 then n  = 1 /*enable in 2016 */
skip_nnc:
/*AWD025 SR67154 - */
REM     if n = 0 and nc = 0 and sc = 0 and s = 0 then map$ = "0"
        if n = 0 and nc = 0 and sc = 0 and s = 1 then map$ = "10"
        if n = 0 and nc = 0 and sc = 1 and s = 0 then map$ = "7"
        if n = 0 and nc = 0 and sc = 1 and s = 1 then map$ = "9"
        if n = 0 and nc = 1 and sc = 0 and s = 0 then map$ = "4"
        if n = 0 and nc = 1 and sc = 1 and s = 0 then map$ = "6"
        if n = 0 and nc = 1 and sc = 1 and s = 1 then map$ = "8"
        if n = 1 and nc = 0 and sc = 0 and s = 0 then map$ = "2"
        if n = 1 and nc = 1 and sc = 0 and s = 0 then map$ = "3"
        if n = 1 and nc = 1 and sc = 1 and s = 0 then map$ = "5"
        if n = 1 and nc = 1 and sc = 1 and s = 1 then map$ = "1"

copy_map:
REM     if map$ = "0" then not_qualified
         for l = 3 to 190                  /* adjusted new map size CHGIT */
           m = l + 213
           if map$ = "0" then xx$(m) = l0$(l)
           if map$ = "1" then xx$(m) = l1$(l)
           if map$ = "2" then xx$(m) = l2$(l)
           if map$ = "3" then xx$(m) = l3$(l)
           if map$ = "4" then xx$(m) = l4$(l)
           if map$ = "5" then xx$(m) = l5$(l)
           if map$ = "6" then xx$(m) = l6$(l)
           if map$ = "7" then xx$(m) = l7$(l)
           if map$ = "8" then xx$(m) = l8$(l)
           if map$ = "9" then xx$(m) = l9$(l)
           if map$ = "10" then xx$(m) = l10$(l)
         next l
         
    if map$ = "0" then not_qualified
    if lowes% = 1% then                                        ~
          xx$(413%) = "^FO563,96^FR^XGPIC1    ,1,1^FS"
    if lowes% = 1% then                                        ~
          xx$(414%) = "^FO527,320^FR^XGR:LABEL" & map$ & ".GRF,1,1^FS"    /* CHGIT */
    if lowes% <> 1% then                                        ~
          xx$(415%) = "^FO563,96^FR^XGPIC1    ,1,1^FS"
    if lowes% <> 1% then                                        ~
          xx$(416%) = "^FO527,320^FR^XGR:LABEL" & map$ & ".GRF,1,1^FS"

      goto read_loop

not_qualified:
    if lowes% = 1% then                                        ~
          xx$(413%) = "^FXNo Graphic Image^FS"
    if lowes% = 1% then                                        ~
          xx$(414%) = "^FXNo Graphic Image^FS"
    if lowes% <> 1% then                                        ~
          xx$(415%) = "^FXNo Graphic Image^FS"
    if lowes% <> 1% then                                        ~
          xx$(416%) = "^FXNo Graphic Image^FS"
/* </AWD019> */

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
                                           /* (EWD011)                   */
        if nbr_lines% < 404% then goto skip_data      /* chgit */

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
                                           /* (EWD010)                   */
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

        pgmchk
            convert nbr_lines% to nbrl$, pic(###0)
            call "SHOSTAT" ("line " & nbrl$ & xx$(nbr_lines%)) : STOP
        return
        
        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************
L55030:     FMT CH(80)

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

/* <AWD017> */
check_model
REM  str(sku_key$,01,25) = part$
/* read AWDPLNEX table @@@ */

REM   LBL$(33%) = "FLORIDA PRODUCT APPROVAL " & LB_FL$ & FS$
/* (AWD023) */   /* has to be one (1) shorter than ewdpla77! */
            str(lbl$(33%),1%,29%)  = "FL Prd Approval: " & lb_fl$ 
            str(lbl$(33%),30%,30%) = "       TDI: " & lb_tdi$ & fs$ 
            
                                    
            lbl$(34%) = "Glazing complies with ASTM E 1300" & fs$

            init(" ") gen_data$
            gen_key$ = "ELLISON05" & str(part$,1,3) & "00"
            part2$ = part$
            
            
            read #4, key >= gen_key$, using GENCODE, gen_key$,            ~
                                                gen_data$, eod goto no_code
            if str(part$,1,3) <> str(gen_key$,10,3) then goto no_code
/*IM8005*/  goto yes_code
            
no_code:    pp% = pos(lb_series$ = " ")
            gen_key$ = "FLORIDACD" & str(lb_style2$,1%, 10%) /*IM8008*/
            goto lookup_flcd   /*IM8008*/

yes_code:   x = 0.0
            y = 0.0
            for l = 1 to 16
               if str(gen_data$,l,1) = " " and x > 0 then y = l
               if str(gen_data$,l,1) = " " and x = 0 then x = l
               if x > 0 and y > 0 then l = 16
            next l
            gen_key$ = "FLORIDACD" & str(gen_data$,x+1,y - x)

lookup_flcd
            init(" ") gen_data$
REM         if lb_fl$ = " " then goto no_code
            read #4, key = gen_key$, using GENCODE, gen_key$,              ~
                                                gen_data$, eod goto no_flcode
GENCODE:    FMT CH(24), CH(30)
no_flcode:
            x = 1
            y = 1
            for l = 15 to 1 step -1
              x = l
              if str(gen_data$,l,1) > " " then l = 1
            next l
            for l = 30 to 16 step -1
              y = l - 15
              if str(gen_data$,l,1) > " " then l = 16
            next l

            lbl$(36%) = str(gen_data$,01,x) & " Glazing" & fs$   
            lbl$(35%) = str(gen_data$,16,y) & " Glazing" & fs$
            if str(gen_data$,16%,y) = " " then lbl$(35%) = lbl$(36%)
            if str(part$,11,1) = "4" then goto skipGenClear1
            if str(gen_data$,1,15) = "              " then         ~
                     lbl$(36%) = fs$
skipGenClear1:

            if str(part$,11,1) = "5" or str(part$,11,1) = "6" then ~
               skipGenClear2                     
            if str(gen_data$,16,15) = "              " then         ~
                     lbl$(35%) = fs$
skipGenClear2:
                     
REM     36^Glazing              35^Glazing
REM     38^Pane1                37^Pane1
REM     41^Airspace             42^Airspace
REM     40^Pane2                39^Pane2
REM     43^Airspace             44^Airspace
REM     45^Pane3                46^Pane3

            lbl$(37%) = "Single-Strength Annealed" & fs$
            lbl$(41%) = "Airspace" & fs$
            lbl$(42%) = "Airspace" & fs$
/* (AWD020) */
            lamn% = 0%
            lbl$(43%) = fs$
            lbl$(44%) = fs$
            lbl$(45%) = fs$
            lbl$(46%) = fs$
            if trip% = 1% then lbl$(43%) = "Airspace" & fs$
            if trip% = 1% then lbl$(44%) = "Airspace" & fs$
/* (\AWD020) */

            gen_key$ = "PLAN DBLE" & str(part$,5,2)
            read #4, key = gen_key$, using GENCODE, gen_key$,              ~
                                  gen_data$, eod goto skip_double

            lbl$(37%) = "Double-Strength Annealed" & fs$
skip_double:

            gen_key$ = "PLAN LAMN" & str(part$,5,2)
            read #4, key = gen_key$, using GENCODE, gen_key$,              ~
                                  gen_data$, eod goto skip_lamn
            lamn% = 1%
            lbl$(37%) = "Double-Strength Annealed" & fs$
skip_lamn:

            gen_key$ = "PLAN TEMP" & str(part$,5,2)
            read #4, key = gen_key$, using GENCODE, gen_key$,              ~
                                  gen_data$, eod goto skip_tempered

            lbl$(37%) = "Double-Strength Tempered" & fs$
skip_tempered:
/* (AWD020) */
            if trip% = 1% then lbl$(45%) = lbl$(37%)
            if trip% = 1% then lbl$(46%) = lbl$(37%)
/* (\AWD020) */
            lbl$(38%) = lbl$(37%)
            if lamn% = 0% then lbl$(39%) = lbl$(37%)
            if lamn% = 0% then lbl$(40%) = lbl$(37%)
            if lamn% = 1% then lbl$(39%) = "Laminated Annealed" & fs$
            if lamn% = 1% then lbl$(40%) = "Laminated Annealed" & fs$

            if str(lbl$(35%),1,3) <> fs$ then goto not_blank
            lbl$(37%) = fs$
            lbl$(39%) = fs$
            lbl$(42%) = fs$
/* (AWD020) */
            lbl$(44%) = fs$
            lbl$(46%) = fs$
/* (\AWD020) */
not_blank:  if str(lbl$(36%),1,3) <> fs$ then goto not_blank2
            lbl$(38%) = fs$
            lbl$(40%) = fs$
            lbl$(42%) = fs$
not_blank2: if str(part$,11,1) <> "4" then goto not_4
            lbl$(35%) = fs$
            lbl$(37%) = fs$
            lbl$(42%) = fs$
            lbl$(39%) = fs$
            lbl$(44%) = fs$
            lbl$(46%) = fs$            
        goto not_5_6

not_4:      if str(part$,11,1) <> "5" and                          ~
               str(part$,11,1) <> "6" then goto not_5_6
            lbl$(36%) = fs$
            lbl$(38%) = fs$
            lbl$(41%) = fs$
            lbl$(40%) = fs$
            lbl$(43%) = fs$
            lbl$(45%) = fs$

not_5_6:

        if str(part$,5,2) <> "AZ" then goto notSingleGlaze
            lbl$(41%) = fs$
            lbl$(40%) = fs$
            lbl$(43%) = fs$
            lbl$(45%) = fs$

            lbl$(42%) = fs$
            lbl$(39%) = fs$
            lbl$(44%) = fs$
            lbl$(46%) = fs$
            
notSingleGlaze:
/*<AWD024>+ */
/*      stc$ = "34" : oitc$ = "28" : goto ttemp    temp for testing */
        init (" ") oitc$ : lb_stc$ = "   " : lb_stc% = 0 
           lbl$(48%) = fs$
           if stc$ <= " " then goto no_apr
           if str(stc$,1%,1%) = "0" then goto no_apr
           for stci% = 1 to 3
               if str(stc$,stci%,1%) = " " then goto next_stci
               lb_stc% = lb_stc% + 1
               str(lb_stc$,lb_stc%,1%) = str(stc$,stci%,1%)
next_stci: next stci%
           if str(lb_stc$,3%,1%) = " " then str(lb_stc$,3%,1%) = hex(00)
           stc$ = lb_stc$
           oitc$ = "28"
ttemp:   lbl$(48%) = "STC: (" & stc$ & ")" & "  OITC: (" & oitc$ & ")" &    ~
                     " EWR: (  )" & fs$
        
no_apr:
/*<AWD024>- */
       return
       
REM     36^Glazing              35^Glazing
REM     38^Pane1                37^Pane1
REM     41^Airspace             42^Airspace
REM     40^Pane2                39^Pane2
REM     43^Airspace             44^Airspace
REM     45^Pane3                46^Pane3       
/* </AWD017> */

        REM *************************************************************~
            *      L O A D   L A B E L   F O R M A T                    *~
            *************************************************************

                                                              /* (EWD011)     */
        load_label
          init  (" ") low$()

       low$(001) = "^JO"
       low$(002) = "^XA^EG^XZ" 
       low$(  3%) = hex(7e) & "DGPIC1    ,06784,032,"
       low$(  4%) = "00,"
       low$(  5%) = "04000000000000000000040000000000000000000004,"
       low$(  6%) = "00,"
       low$(  7%) = "00,"
       low$(  8%) = "00606060606060606060600060606060606060606040006060606060606060,"
       low$(  9%) = "40E0E0E0E0E0E0E0E0E0E060E0E0E0E0E0E0E0E0E0C060E0E0E0E0E0E0E0E040"
       low$( 10%) = "01F9F9F9F9F9F9F9F9F9F079F9F9F9F9FDF9F9F9F9E071F9F9F9F9F9F9F9F880"
       low$( 11%) = "07FFFFFFFFFFFFFFFFFFFC1FFFFFFFFFFFFFFFFFFFF801FFFFFFFFFFFFFFFE,"
       low$( 12%) = "07FFFFFFFFFFFFFFFFFFFC1FFFFFFFFFFF1FFFFFFFFC07FFFFFFFFFFFFFFFE,"
       low$( 13%) = "0F9F9F9F9F9F9F9F9F9F9E1F9F9F9F9F9E1F9F9F9F9F079F9F9F9F9F9F9F9E,"
       low$( 14%) = "0F9F9F9F9F9F9F9F9F9F9C1F9F9F9F9F9E1F9F9F9FDF019F9F9F9F9F9F9F9E,"
       low$( 15%) = "07FFFFFFFFFFFFFFFFFFFC1FFFFFFFFFFE1FFFFFFFFF803FFFFFFFFFFFFFFE,"
       low$( 16%) = "07FFFFFFFFFFFFFFFFFFF87FFFFFFFFFF85FFFFFFFFFE0FFFFFFFFFFFFFFFE,"
       low$( 17%) = "41F8F0F0F0F9F0F0F0F9F079F9F9F9F9F879F9F9F9F9F061F9F9F9F9F9F9F8C0"
       low$( 18%) = "01F8000000FDC0000011F079F9F9F9F9F879F9F9F9F9F801F9F9F9F9F9F9F880"
       low$( 19%) = "07F8000000FFC000001FFC1FFFFFFFFFF87FFFFFFFFFFE07FFFFFFFFFFFFFE,"
       low$( 20%) = "07FE0E02083F82080E1FFC1FFFFFFFFFFE1FFFFFFFFFFF01FFFFFFFFFFFFFE,"
       low$( 21%) = "0F9E0E060C1F860E0E1F9E1F9F9F9F9F9E1F9F9F9F9F9F071F9F9F9F9F9F9E,"
       low$( 22%) = "0F9F9F0F9C1F8F9E9F0F9C1F9F9F9F9F9E1F9F9F9F9F9F001F9F9F9F9F9F9E,"
       low$( 23%) = "07FFFFC3FCFFCFFCFF1FFC1FFFFFFFFFFE1FFFFFFFFFFFF01FFFFFFFFFFFFE,"
       low$( 24%) = "07FFFFC3F8FFC3F8FE97F87FFFFFFFFFF87FFFFFFFFFFFF01FFFFFFFFFFFFE,"
       low$( 25%) = "41F9F9E1F8F9C1F8F891F079F9F9F9F9F879F9F9F9F9F9F041F9F9F9F9F9F8C0"
       low$( 26%) = "01F9F9C1F8F9C1F8F891F079F9F9F9F9F879F9F9F9F9F9F001F9F9F9F9F9F880"
       low$( 27%) = "07FFFFC1F8FFC3F8FF9FFC1FFFFFFFFFF01FFFFFFFFFFFFE03FFFFFFFFFFFE,"
       low$( 28%) = "07FFFFC3F8FF8FFEFF1FFC1FFFFFFFFFD81FFFFFFFFFFFFE07FFFFFFFFFFFE,"
       low$( 29%) = "0F9F9F84079F8F9E9F1F9E1F9F9F9F9F040F9F9F9F9F9F9F069F9F9F9F9F9E,"
       low$( 30%) = "0F9E1FDE07DF8FDFDF1F9C1F9F9F9F9F0F001F9F9F9F9F9F801F9F9F9F9F9E,"
       low$( 31%) = "07FE3FFC07FFDFFFFFBFFC1FFFFFFFFF07007FFFFFFFFFFFE03FFFFFFFFFFE,"
       low$( 32%) = "07F8FFF003FFF3FFFFFFF87FFFFFFFFFC280FFFFFFFFFFFFE0FFFFFFFFFFFE,"
       low$( 33%) = "41F860F9F1F9F9F9F9F9F079F9F9F9F9C1E071F9F9F9F9F9F079F9F9F9F9F8C0"
       low$( 34%) = "01F800F9F5F9F9F9FDF9F079F9F9F9F9C1E031F9F9F9F9FDF039F9F9F9F9F880"
       low$( 35%) = "07FF8003FFFFF1F1F1FFFC1FFFFFFFFFC7E301FFFFFFFFFFF01FFFFFFFFFFE,"
       low$( 36%) = "07FFF0001FFF8000001FFC1FFFFFFFFF87E3803FFFFFFFFFFE07FFFFFFFFFE,"
       low$( 37%) = "0F9F96041F9F8404041F9E1F9F9F9F9F07878C1F9F9F9F9F9F0F9F9F9F9F9E,"
       low$( 38%) = "0F9FDE1E071F8F0F0E0F9C1F9F9F9F9FC7C25E0F9F9F9F9F9F079F9F9F9F9E,"
       low$( 39%) = "07FFF0FC02FFDF1F181FFC1FFFFFFFFFC7F0FC0FFFFFFFFFFF07FFFFFFFFFE,"
       low$( 40%) = "07FFF0FFE0FFFFFFF07FF87FFFFFFFFFF1F0FF03FFFFFFFFFFC1FFFFFFFFFE,"
       low$( 41%) = "41F9F0F9E0F9F9F9F071F079F9F9F9F9F1F0F841F9F9F9F9F9E0F9F9F9F9F8C0"
       low$( 42%) = "01F9F0F8E0F9F9F841FDF079F9F9F9F9F0F079C1F9F9F9F9FDE0F9F9F9F9F880"
       low$( 43%) = "07FFF0E003FFFFE02FFFFC1FFFFFFFFFF87F07F1FFFFFFFFFFE07FFFFFFFFE,"
       low$( 44%) = "07FFF00407FFFF880FFFFC1FFFFFFFFFF81F0FD17FFFFFFFFFE03FFFFFFFFE,"
       low$( 45%) = "0F9F84071F9F9F071F9F9E1F9F9F9F9F9E0F079C1F9F9F9F9F9E1F9F9F9F9E,"
       low$( 46%) = "0F9F80031FDF9F071FDF9C1F9F9F9F9F9E0F07DC1F9F9F9F9F9A0FDF9F9F9E,"
       low$( 47%) = "07FE000FFFFFD01FFFFFFC1FFFFFFFFFFE0F81FC1FFFFFFFFFFE0FFFFFFFFE,"
       low$( 48%) = "07F801FFFFFFC0F9F9FFF87FFFFFFFFFFFE1F0FC7FFFFFFFFFFE03FFFFFFFE,"
       low$( 49%) = "41F861F9F9F9C0F1F1F9F079F9F9F9F9F9E1F0F879F9F9F9F9F841F9F9F9F8C0"
       low$( 50%) = "01F8F9F9F9F9C0000011F079F9F9F9F9F1E0F82079F9F9F9F9F801F9F9F9F880"
       low$( 51%) = "07FEFFFFFFFFC000001FFC1FFFFFFFFFFFE0F8001FFFFFFFFFFF03FFFFFFFE,"
       low$( 52%) = "07FF1F1F1FFF8E0E0E1FFC1FFFFFFFFFF8001F001FFFFFFFFFFF03FFFFFFFE,"
       low$( 53%) = "0F9F0F0F0F9F8F0F0F1F9E1F9F9F9F9F9C041F041F9F9F9F9F9F079F9F9F9E,"
       low$( 54%) = "0F9E0000001F9F9F9F9FDC1F9F9F9F9FDE000F8F1F9F9F9F9F9F035F9F9F9E,"
       low$( 55%) = "07FE000000FFFFFFFFFFFC1FFFFFFFFFFE0007FFFFFFFFFFFFFFC07FFFFFFE,"
       low$( 56%) = "07F8E0C0E0FFFFFFFFFFF87FFFFFFFFFFE8001FFFFFFFFFFFFFFE0FFFFFFFE,"
       low$( 57%) = "41F9F9E1F8F9E0606071F079F9F9F9F9F9E060F9F9F9F9F9F9F9F079F9F9F8C0"
       low$( 58%) = "01FDFDE1F8FDE0606071F079F9F9F9F9F9E000F9F9F9F9F9F9F9F07DF9F9F880"
       low$( 59%) = "07FFFFC3F8FFC000001FFC1FFFFFFFFFFFF000FFFFFFFFFFFFFFF07FFFFFFE,"
       low$( 60%) = "07FFFFC3FC3F8000001FFC1FFFFFFFFFFFFC08FFFFFFFFFFFFFFF41FFFFFFE,"
       low$( 61%) = "0F9F9F879C1F8F1E1F1F9E1F9F9F9F9F9F9F0F9F9F9F9F9F9F9F9C1F9F9F9E,"
       low$( 62%) = "0F9F9F039C1F8F9E9F0F9C1F9F9F9F9F9F8E079F9F9F9F9F9F9F9E0F9F9F9E,"
       low$( 63%) = "07FFFF03F8FFCFFCFF1FFC1FFFFFFFFFFFE005FFFFFFFFFFFFFFFC1FFFFFFE,"
       low$( 64%) = "07F8000060FFC3F8FE97F87FFFFFFFFFFF8001FFFFFFFFFFFFFFFC07FFFFFE,"
       low$( 65%) = "41F8E060E0F9C1F8F891F079F9F9F9F9F9E041F9F9F9F9F9F9F9F841F9F9F8C0"
       low$( 66%) = "01F820F001F9C1F8FC91F079F9F9F9F9FC80E1F9F9F9F9F9F9F9FC01F9F9F880"
       low$( 67%) = "07FFFFFFFFFFC3F8FF9FFC1FFFFFFFFFFF8FFFFFFFFFFFFFFFFFFE03FFFFFE,"
       low$( 68%) = "07FF1FFF1FFF8FFEFF1FFC1FFFFFFFFFFF0FFFFFFFFFFFFFFFFFFE07FFFFFE,"
       low$( 69%) = "0F9F9F9F9E1F8F9F9F1F9E1F9F9F9F9F9F8F9F9F9F9F9F9F9F9F9F079F9F9E,"
       low$( 70%) = "0F9F9F9FDE1F8FDF9F1F9C1F9F9F9F9F9F0F9F9F9F9F9F9F9F9F9F079F9F9E,"
       low$( 71%) = "07FFFFFFFCFFFFFFFFFFFC1FFFFFFFFFFF83FFFFFFFFFFFFFFFFFF83FFFFFE,"
       low$( 72%) = "07FFFFFFF8FFFFFFFFFFF87FFFFFFFFFFFE3FFFFFFFFFFFFFFFFFF81FFFFFE,"
       low$( 73%) = "41F9F9F9F8F9F1F1F1F9F079F9F9F9F9F9E0F9F9F9F9F9F9F9F9F9E0F9F9F8C0"
       low$( 74%) = "01F9F1F1F0F9C0000011F079F9F9F9F9FC8011F9F9F9F9F9F9F9F9E0FDF9F880"
       low$( 75%) = "07FFFFFFF8FFC000001FFC1FFFFFFFFFFF8007FFFFFFFFFFFFFFFFE0FFFFFE,"
       low$( 76%) = "07FE0000003F8400041FFC1FFFFFFFFFFF0C01FFFFFFFFFFFFFFFFE0FFFFFE,"
       low$( 77%) = "0F9E0606041F8E0E0E1F9E1F9F9F9F9F9F0C079F9F9F9F9F9F9F9F861F9F9E,"
       low$( 78%) = "0F9E0E0E0C1FDF9E1F0F9C1F9F9F9F9F9F0F081F9F9F9F9F9F9F9F881F9F9E,"
       low$( 79%) = "07FFFFFFFCFFFFFCFF1FFC1FFFFFFFFFFF83043FFFFFFFFFFFFFFFE83FFFFE,"
       low$( 80%) = "07FFFFFFF8FFFFF8FE97F87FFFFFFFFFFFE3007FFFFFFFFFFFFFFFE0FFFFFE,"
       low$( 81%) = "41F9F9F9F8F9F9F8F8D1F079F9F9F9F9F9E0E079F9F9F9F9F9F9F9F0F9F9F8C0"
       low$( 82%) = "01FDF9F9F8F9F9F8F891F079F9F9F9F9F9E0E039F9F9F9F9F9F9F9F0FDF9F880"
       low$( 83%) = "07FFFFFFF8FFFFF8FE1FFC1FFFFFFFFFFFF0E03FFFFFFFFFFFFFFFF83FFFFE,"
       low$( 84%) = "07FFFFFFFC3FFFF83E1FFC1FFFFFFFFFFFF0003FFFFFFFFFFFFFFFF83FFFFE,"
       low$( 85%) = "0F9F9F9F9C1F9F8E1E1F9E1F9F9F9F9F9F86041F9F9F9F9F9F9F9F9C1F9F9E,"
       low$( 86%) = "0F9F9F9F9F1F8E010E1F9C1F9F9F9F9F9F9C1E1F9F9F9F9F9F9F9FDC0F9F9E,"
       low$( 87%) = "07FFFFFFFFFFC007007FFC1FFFFFFFFFFFE41FFFFFFFFFFFFFFFFFF81FFFFE,"
       low$( 88%) = "07FFFFFFFFFFC001C07FF87FFFFFFFFFFFE01FFFFFFFFFFFFFFFFFF83FFFFE,"
       low$( 89%) = "41F8606060F9F1F9F1F9F079F9F9F9F9F9E041F9F9F9F9F9F9F9F9F879F9F8C0"
       low$( 90%) = "01F8000000F9F1F1F1FDF079F9F9F9F9FDE001F9F9F9F9F9F9F9F9F839F9F880"
       low$( 91%) = "07FE000000FFFFFFFFFFFC1FFFFFFFFFFFF001FFFFFFFFFFFFFFFFFC0FFFFE,"
       low$( 92%) = "07FFFFFF80BFFE00077FFC1FFFFFFFFFFFFC003FFFFFFFFFFFFFFFFC0FFFFE,"
       low$( 93%) = "0F9F9F9F061F9E06071F9E1F9F9F9F9F9F9E041F9F9F9F9F9F9F9F9C0F9F9E,"
       low$( 94%) = "0F9FDFDE0FDF8002001F9C1F9F9F9F9F9F9F000F9F9F9F9F9F9F9FDE0F9F9E,"
       low$( 95%) = "07FFFFFC07FFC000001FFC1FFFFFFFFFFFFF001FFFFFFFFFFFFFFFFC0FFFFE,"
       low$( 96%) = "07FFFFE07FFFC0FBF817F87FFFFFFFFFFFFFE07FFFFFFFFFFFFFFFFC0BFFFE,"
       low$( 97%) = "41F9F9E0F1F9E0F1F071F079F9F9F9F9F9F9E0F9F9F9F9F9F9F9F9F861F9F8C0"
       low$( 98%) = "01F9F001F9FDC1FDFC95F079F9F9FDF1F9F9F039F9F9F9F9F9F9F9F801F9F880"
       low$( 99%) = "07FFC03FFFFFCFF9FF87FC1FFFFFFF00FFFFF80FFFFFFFFFFFFFFFFC0BFFFE,"
       low$(100%) = "07FF801FFFFF8FFFFF8FFC1FFFFFFF003FFFFE0FFFFFFFFFFFFFFFFE0FFFFE,"
       low$(101%) = "0F9E0F9F9F9F8F9D9F0F9E1F9F9F9F0E0F9F0C0F9F9F9F9F9F9F9F9F0F9F9E,"
       low$(102%) = "0F9E0FDFDF9F8F999F0F9C1F9F9FDF0C0FDF080F9F9F9F9F9F9F9FDF0F9F9E,"
       low$(103%) = "07FE000000FF8FF8FE1FFC1FFFFFFF0301FC01FFFFFFFFFFFFFFFFFE0FFFFE,"
       low$(104%) = "07F8000000FFC000F01FF87FFFFFFFC0F07001FFFFFFFFFFFFFFFFFC03FFFE,"
       low$(105%) = "41F8606060F9E060F071F079F9F9F9E0F07041F9F9F9F9F9F9F9F9F841F9F8C0"
       low$(106%) = "01F9F1F1F1FDE000F0FDF079F9F9F9F07010E0F9F9F9F9F9F9F9F9F801F9F880"
       low$(107%) = "07FFFFFFFFFFC000F07FFC1FFFFFFFF0381080FFFFFFFFFFFFFFFFFD03FFFE,"
       low$(108%) = "07FFFFFFFFFFFFFFFFFFFC1FFFFFFFFE0700001FFFFFFFFFFFFFFFFF0FFFFE,"
       low$(109%) = "0F9F9F9F9F9F9F1F9F9F9E1F9F9F9F9E0704060F9F9F9F9F9F9F9F9F0F9F9E,"
       low$(110%) = "0F9F0F0F0F1F9F9F9F0F9C1F9F9F9F9F0000070F9F9F9F9F9F9F9FDF0F9F9E,"
       low$(111%) = "07FE000000FFFFFFF81FFC1FFFFFFFFFF8000003FFFFFFFFFFFFFFFF07FFFE,"
       low$(112%) = "07F8000000FFFFFFF017F87FFFFFFFFFF00000C3FFFFFFFFFFFFFFFD03FFFE,"
       low$(113%) = "41F870E0F0F9F9F9E079F079F9F9F9F9F8E040E1F9F9F9F9F9F9F9F841F9F8C0"
       low$(114%) = "01F870E0F0FDF9F9E0F1F079F9F9FDF9F8E000E1F9F9F9F9F9F9F9F801F9F880"
       low$(115%) = "07F87FC3F8FFFFF803FFFC1FFFFFFFFFFFF00031FFFFFFFFFFFFFFFC03FFFE,"
       low$(116%) = "07FE3FC3FC3FDF9E0FFFFC1FFFFFFFFFFFFE0011FFFFFFFFFFFFFFFF0FFFFE,"
       low$(117%) = "0F9E1F879C1F84041F9F9E1F9F9F9F9F9F9F06051F9F9F9F9F9F9F9F0F9F9E,"
       low$(118%) = "0F9E1F8F9C1F8E000F9F9C1F9F9F8E0FDFDF07011F9F9F9F9F9F9F9F0F9F9E,"
       low$(119%) = "07FE3FC3FCFFC0003FFFFC1FFFFFE00FFFFF8701FFFFFFFFFFFFFFFE0FFFFE,"
       low$(120%) = "07F87FE3F8FFFFF801FFF87FFFFFE000FFFFE1C1FFFFFFFFFFFFFFFC03FFFE,"
       low$(121%) = "41F879E1F8F9F9F841F9F079F9F9E040F9F9E1E1F9F9F9F9F9F9F9F841F9F8C0"
       low$(122%) = "01F879F1F8FDF9FDE071F079F9F9E0F035F9F0F1F9F9F9F9F9F9F9F801F9F880"
       low$(123%) = "07FFFFFFFFFFFFFFF81FFC1FFFFFE0FC07FC78FFFFFFFFFFFFFFFFFC0FFFFE,"
       low$(124%) = "07FFFFFFFFFFFFFFFE1FFC1FFFFFE83E07FE183FFFFFFFFFFFFFFFFC0FFFFE,"
       low$(125%) = "0F9F1F1F1F9F9F9F9F1F9E1F9F9F9E0F071C061F8F9F9F9F9F9F9F9E0F9F9E,"
       low$(126%) = "0F9F1F1F1F9F9F9F9F1FDC1F9F9F9E0F031C001F8FDF9F9F9F9F9F9E0F9F9E,"
       low$(127%) = "07FE000000FFFFFFFFFFFC1FFFFFFE03F010001F83FFFFFFFFFFFFFC0FFFFE,"
       low$(128%) = "07F8000000FFFFFFFFFFF87FFFFFFE01F070E03FE1FFFFFFFFFFFFF80FFFFE,"
       low$(129%) = "41F8E0E0E0F9F9F9F9F9F079F9F9F9E0F850F069E0F9F9F9F9F9F9F869F9F8C0"
       low$(130%) = "01F9F9E1F8F9F9F9F9F9F079F9F9F9F000807801E0F9F9F9F9F9F9F839F9F880"
       low$(131%) = "07FFFFC3F8FFFFFFFFFFFC1FFFFFFFF801807F0FE3FFFFFFFFFFFFF82FFFFE,"
       low$(132%) = "07FFFFC3FC3FFFFFFFFFFC1FFFFFFFFF00020F1F83FFFFFFFFFFFFFC1FFFFE,"
       low$(133%) = "0F9F9F879C1F9F9F9F9F9E1F9F9F9F9F04060F0F879F9F9F9F9F9F9C0F9F9E,"
       low$(134%) = "0F9F9FC39C1F9E1F0F1F9C1F9F9F9F9F9E0007DF83DF9F9F9F9F9F9C0F9F9E,"
       low$(135%) = "07FFFF03F8FFC07F001FFC1FFFFFFFFFFF8000FF83FFFFFFFFFFFFF83FFFFE,"
       low$(136%) = "07FFFFC1F8FFE07F007FF87FFFFFFFFFFFE060FFE0FFFFFFFFFFFFF83FFFFE,"
       low$(137%) = "41F9F9C1F0F9E0794079F079F9F9F9F9F8E060F9E0F9F9F9F9F9F9F079F9F8C0"
       low$(138%) = "01F8E020E0FDC0F82015F079F9F9FDF9FDF00039E0F9F9F9F9F9F9F0F9F9F880"
       low$(139%) = "07FE000000FFC3FE001FFC1FFFFFFFFFFFF0002FE0FFFFFFFFFFFFF83FFFFE,"
       low$(140%) = "07FE000C03FF8FFE3F0FFC1FFFFFFFFFFFFF000782FFFFFFFFFFFFE83FFFFE,"
       low$(141%) = "0F9E060E079F8F9E1F0F9E1F9F9F9F9F9F9F040F879F9F9F9F9F9F8C1F9F9E,"
       low$(142%) = "0F9F1F1F0F9F8FDE1F0FDC1F9F9FCE0FDF9F070383DF9F9F9F9F9F881F9F9E,"
       low$(143%) = "07FFFFFFFFFF8FE8FF8FFC1FFFFFF8001FFF81FFE0FFFFFFFFFFFFE0FFFFFE,"
       low$(144%) = "07FFFFFFFFFFC3F0FF87F87FFFFFF000FFFFE1F1E0FFFFFFFFFFFFE0FFFFFE,"
       low$(145%) = "41F9F9F9F9F9C0E0F051F079F9F9F04040F9E0F9E0F9F9F9F9F9F9E0F9F9F8C0"
       low$(146%) = "01FDF9F9FDF9C0E0F091F079F9F9F00000FDE0F9E0F9F9F9F9F9F9E0F9F9F880"
       low$(147%) = "07FFFFFFFFFFC001F01FFC1FFFFFF8000000F8FFE0FFFFFFFFFFFF81FFFFFE,"
       low$(148%) = "07FFFFFFFFFFFE0FFF7FFC1FFFFFFE0F00001E3FA0FFFFFFFFFFFF83FFFFFE,"
       low$(149%) = "0F9F9F9F9F9F9E0F9F1F9E1F9F9F9E0F0C041E1F869F9F9F9F9F9F879F9F9E,"
       low$(150%) = "0F9F9F9F9F9F9F1F9F1FDC1F9F9F9F03DF001E1F829F9F9F9F9F9F079F9F9E,"
       low$(151%) = "07FFFFFFFFFFFFFFFFFFFC1FFFFFFF03FF001E3FE0FFFFFFFFFFFF83FFFFFE,"
       low$(152%) = "07FFFFFFFFFFFFFFFE97F87FFFFFFFC0FFF0FE2FE0FFFFFFFFFFFE83FFFFFE,"
       low$(153%) = "41F9F9F9F9F9F9F9F8D1F079F9F9F9C0F9F0F879E0F9F9F9F9F9F8C1F9F9F8C0"
       low$(154%) = "01F9F9F9F9FDFDF9FC95F079F9F9F9E0F9F9F901E0FDF9F9F9F9F801F9F9F880"
       low$(155%) = "07FFFFFFFFFFFFFFFF9FFC1FFFFFFFF03FFFFF03E03FFFFFFFFFFC07FFFFFE,"
       low$(156%) = "07FFFFFFFFFFFFFFFF1FFC1FFFFFFFD01FFFFF0FE01FFFFFFFFFFE0FFFFFFE,"
       low$(157%) = "0F9F9F9F9F9F8F0F0E0F9E1F9F9F9F9C0F9F9F0784071F9F9F9F9E0F9F9F9E,"
       low$(158%) = "0F9F9F9F9F9F8F0F0E0F9C1F9F9F9F9C0F9F9F0780075F9F9F9F9E0F9F9F9E,"
       low$(159%) = "07FFFFFFFFFFC000001FFC1FFFFFFFFC0FFFFFC3F4001FFFFFFFFC1FFFFFFE,"
       low$(160%) = "07FFFFFFFFFFF0F0F097F87FFFFFFFFD01FFFFF1FFE000FFFFFFF07FFFFFFE,"
       low$(161%) = "41F9F9F9F9F9F0F0F0D1F079F9F9F9F841F9F9F1F9E040F9F9F9F079F9F9F8C0"
       low$(162%) = "01F9F9F9F9FDF9F9FC95F079F9F9F9F9C1F9F9F1F9F08021F9F9F079F9F9F880"
       low$(163%) = "07FFFFFFFFFFFFFFFF9FFC1FFFFFFFFF41FFFFF1FFF80007FFFFF07FFFFFFE,"
       low$(164%) = "07FFFFFFFFFFDFFFFF1FFC1FFFFFFFFFC17FFFFD7FFF88001FFFC17FFFFFFE,"
       low$(165%) = "0F9F9F9F9F9F9F9F9F0F9E1F9F9F9F9F051F9F9D1F9F8E061F9F851F9F9F9E,"
       low$(166%) = "0F9F9F9F9F9F8F1FDF0F9C1F9F9F9F9FC01F9F9C5F9FDF001F9F031F9F9F9E,"
       low$(167%) = "07FFFFFFFFFFC007FFFFFC1FFFFFFFFFD07FFFFC7FFFFE001FFF03FFFFFFFE,"
       low$(168%) = "07FFFFFFFFFFC001FFFFF87FFFFFFFFFF07FFFFC7FFFF8001FFF41FFFFFFFE,"
       low$(169%) = "41F9F9F9F9F9F040F1F9F079F9F9F9F9E0F9F9F879F9E040F9F841F9F9F9F8C0"
       low$(170%) = "01F9F9F9F9FDF000F1F9F079F9F9F9F9E0F9F9F87DF9E000F9F801F9F9F9F880"
       low$(171%) = "07FFFFFFFFFFFF1001FFFC1FFFFFFFFF01FFFFFC7FF8000FFFFE03FFFFFFFE,"
       low$(172%) = "07FFFFFFFFFFFF0E073FFC1FFFFFFFFF07FFFFFE1FFE000FFFFE0FFFFFFFFE,"
       low$(173%) = "0F9F9F9F9F9F9F1F0E0F9E1F9F9F9F9F079F9F9F1F04071F9F9E0F9F9F9F9E,"
       low$(174%) = "0F9F9F9F9F9FDF1F9E0F9C1F9F9F9F9E0F9F9F9FDC000F9F9FDA1F9F9F9F9E,"
       low$(175%) = "07FFFFFFFFFFFF1FFE1FFC1FFFFFFFFC0FFFFFFFF0001FFFFFF83FFFFFFFFE,"
       low$(176%) = "07FFFFFFFFFFFF1FC017F87FFFFFFFF02FFFFFFFE001FFFFFFE0FFFFFFFFFE,"
       low$(177%) = "41F9F9F9F9F9F9F1E071F079F9F9F9F069F9F9F9E041F9F9F9E0F9F9F9F9F8C0"
       low$(178%) = "01F9F9F9F9FDF9B000F9F079F9F9F9F039F9F9F9E0F9F9F9F9E0FDF9F9F9F880"
       low$(179%) = "07FFFFFFFFFFF000FFFFFC1FFFFFFFC0FFFFFFFFE0FFFFFFFFC1FFFFFFFFFE,"
       low$(180%) = "07FFFFFFFFFFFE001FFFFC1FFFFFFFC03FFFFFFFE0FFFFFFFF01FFFFFFFFFE,"
       low$(181%) = "0F9F9F9F9F9F840F9F9F9E1F9F9F9F069F9F0F9F869F9F9F9F079F9F9F9F9E,"
       low$(182%) = "0F9F9F9F9F9F800F9F9F9C1F9F9FDF021F9F0FDF809FDF9F9F079F9F9F9F9E,"
       low$(183%) = "07FFFFFFFFFFC2FFFFFFFC1FFFFFFF03FE0007FFE0FFFFFFFC07FFFFFFFFFE,"
       low$(184%) = "07FFFFFFFFFFC0FFFFFFF87FFFFFFF01F8E001FFE0FFFFFFFC07FFFFFFFFFE,"
       low$(185%) = "41F9F9F9F9F9F9F9F9F9F079F9F9F841E04040F9E0F9F9F9F071F9F9F9F9F8C0"
       low$(186%) = "01F9F9F9F9F9F0F0F0F1F079F9F9F8000000E0F9E0F9F9FDF039F9F9F9F9F880"
       low$(187%) = "07FFFFFFFFFFF1F1F1FFFC1FFFFFF800000080FFE0FFFFFFF03FFFFFFFFFFE,"
       low$(188%) = "07FFFFFFFFFF8000001FFC1FFFFFF800071FE03F82FFFFFF803FFFFFFFFFFE,"
       low$(189%) = "0F9F9F9F9F9F8404040F9E1F9F9F9E04071F8C1F869F9F9F841F9F9F9F9F9E,"
       low$(190%) = "0F9F9F9F9F9F8000000FDC1F9F9F9800071F881F829F9F9F801F9F9F9F9F9E,"
       low$(191%) = "07FFFFFFFFFFC600061FFC1FFFFFE8003FFFF80FE0FFFFFF80FFFFFFFFFFFE,"
       low$(192%) = "07FFFFFFFFFFFFF8FE97F87FFFFFF8FFFFFFF803E0FFFFFE03FFFFFFFFFFFE,"
       low$(193%) = "41F9F9F9F9F9F9F8F8D1F079F9F9F0F1F9F9F841E0F9F9F8C1F9F9F9F9F9F8C0"
       low$(194%) = "01F9F9F9F9F9F9F8F891F079F9F9F9F9F9F9FC01E0F9F9F001F9F9F9F9F9F880"
       low$(195%) = "07FFFFFFFFFFFFF8FF9FFC1FFFFFFFFFFFFFFF01E0FFFFF80FFFFFFFFFFFFE,"
       low$(196%) = "07FFFFFFFFFFFFFEBF1FFC1FFFFFFFFFFFFFFF0103FFFFF01FFFFFFFFFFFFE,"
       low$(197%) = "0F9F9F9F9F9F9F9E1F0F9E1F9F9F9F9F9F9F9F07079F9F9C1F9F9F9F9F9F9E,"
       low$(198%) = "0F9F9F9F9F9F9F081E1FDC1F9F9FDF9F9F9F9F80039F9F001F9F9F9F9F9F9E,"
       low$(199%) = "07FFFFFFFFFFC000001FFC1FFFFFFFFFFFFFFFF003FFFF01FFFFFFFFFFFFFE,"
       low$(200%) = "07FFFFFFFFFFC000001FF87FFFFFFFFFFFFFFFF000FFFF41FFFFFFFFFFFFFE,"
       low$(201%) = "41F9F9F9F9F9C071E0F9F079F9F9F9F9F9F9F9F040F9F841F9F9F9F9F9F9F8C0"
       low$(202%) = "01F9F9F9F9F9C0F1E0F9F079F9F9F9F9F9F9F9F000F9F801F9F9F9F9F9F9F880"
       low$(203%) = "07FFFFFFFFFFFFFFFFFFFC1FFFFFFFFFFFFFFFFE03FFF80FFFFFFFFFFFFFFE,"
       low$(204%) = "07FFFFFFFFFFFFFFFFFFFC1FFFFFFFFFFFFFFFFF03FF803FFFFFFFFFFFFFFE,"
       low$(205%) = "0F9F9F9F9F9F9F9F9F9F9E1F9F9F9F9F9F9F9F9F079F841F9F9F9F9F9F9F9E,"
       low$(206%) = "0F9F9F9F9F9F9F9F9F9F9C1F9F9F9F9F9F9F9F9F8F9F019F9F9F9F9F9F9F9E,"
       low$(207%) = "07FFFFFFFFFFFFFFFFFFFC1FFFFFFFFFFFFFFFFF83FE00FFFFFFFFFFFFFFFE,"
       low$(208%) = "07FFFFFFFFFFFFFFFFFFF87FFFFFFFFFFFFFFFFFFFF001FFFFFFFFFFFFFFFE,"
       low$(209%) = "41F9F9F9F9F9F9F9F9F9F079F9F9F9F9F9F9F9F9F1F041F9F9F9F9F9F9F9F8C0"
       low$(210%) = "01FDFDFDFDFDFDFDFDFDF07DFDFDFDFDFDFDFDFDF9E071F9FDFDFDFDFDFDFC80"
       low$(211%) = "00,"
       low$(212%) = "06060606060606060606040606060606060606060600060606060606060606,"
       low$(213%) = "04040404040404040404040404040404040404040404040404040404040404,"
       low$(214%) = "00,"
       low$(215%) = "00,"
      /*---- copy map here ----------------------*/
        low$(404%) = "^XA^PMN"              /* start point is off do not want xz */
        low$(405%) = "^MNY"
        low$(406%) = "^MMT"
        low$(407%) = "^MTT"
        low$(408%) = "^MD0"
        low$(409%) = "^LH0,0"
        low$(410%) = "^LL813"            
                                                      /* (EWD010) 6 Inch/Sec */
                                                      /* PR Label Print Speed*/
                                                      /* For NE Labels.      */

          low$(411%) = "^PR6,4,8"                     /* PR Label Print Speed*/
                                                      /* For ATRIUM Labels.  */

                                                      /* a = 3 Print Speed   */
                                                      /* b = 4 Slew  SPeed   */
                                                      /* c = 8 Back Feed Spee*/
          low$(412%) = "^JMA^COY,362"                      /* 256 + 128 Mem */
                                                      /* (-) 22k Intern*/

          low$(413%) = "^FO563,96^FR^XGPIC1    ,1,1^FS"
          low$(414%) = "^FO502,404^FR^XGLABEL2.GRF,1,1^FS"

                                                      /* U-Factor (U.S.-I-P)      */
                                                      /* (AWD014)                 */
          low$(415%) = "05^FO1138,189^CI0^A0R,71,71^FR^FD"
                                                      /* Visible Transmittance    */
                                                      /* (1)             (EWD013) */

                                                      /* (AWD014)                 */
          low$(416%) = "18^FO960,189^CI0^A0R,71,71^FR^FD"
                                                      /* Design Pressure Big      */
                                                      /* (3)             (EWD013) */
          low$(417%) = "09^FO370,175^CI0^A0R,61,61^FR^FD"
                                                      /* Text Line (3)            */
                                                      /* (4)             (EWD013) */
          low$(418%) = "22^FO410,231^CI0^A0R,22,22^FR^FD"
                                                      /* Text Line (4)            */
                                                      /* (5)             (EWD013) */
          low$(419%) = "23^FO390,231^CI0^A0R,22,22^FR^FD"
                                                      /* Wind Zone                */
          low$(420%) = "28^FO251,445^CI0^A0R,33,33^FR^FD"
                                                      /* Wind Pressure Values     */
          low$(421%) = "29^FO260,478^CI0^A0R,33,33^FR^FD"
                              
                                                      /* Sequence Number          */
          low$(422%) = "13^FO122,37^CI0^A0R,30,26^FR^FD"
                                                      /* Dept. and S.O.           */
          low$(423%) = "14^FO101,37^CI0^A0R,26,22^FR^FD"
                                                      /* Load and Model           */
          low$(424%) = "15^FO71,37^CI0^A0R,26,22^FR^FD"
                                                      /* Production Date          */
          low$(425%) = "16^FO40,37^CI0^A0R,26,22^FR^FD"
                                                      /* Required Field           */
                                                      /* Air Leakage              */
                                                      /* (2)             (EWD013) */
          low$(426%) = "19^FO961,491^CI0^A0R,71,71^FR^FD"    
                                                      /* Solar Heat Gain Coeff.   */
                                                      /* (AWD014)                 */
          low$(427%) = "07^FO1138,516^CI0^A0R,71,71^FR^FD"    
                                                      /* Special Text for label   */
          low$(428%) = "30^FO1544,240^CI0^A0R,35,33^FR^FD"
                                                     /* Grid Text (2)            */
          low$(429%) = "31^FO1544,445^CI0^A0R,35,33^FR^FD"
                                                      /* Glass Text (2)           */
          low$(430%) = "17^FO1360,260^CI0^A0R,35,33^FR^FD"
                                                      /* Glass Text (1)           */
          low$(431%) = "04^FO1390,260^CI0^A0R,35,33^FR^FD"
                                                      /* Vinyl Window Style       */
          low$(432%) = "03^FO1420,260^CI0^A0R,35,33^FR^FD"
                                                      /* Window Series            */
          low$(433%) = "02^FO1450,260^CI0^A0R,35,33^FR^FD"
                                                      /* Private Label Brand      */
          low$(434%) = "01^FO1480,260^CI0^A0R,35,33^FR^FD"

                                                      /* (6)           (EWD013)*/
          low$(435%) = "32^FO73,284^CI0^A0R,37,37^FR^FD"
                                                      /* (EWD012)              */

/* <AWD017> */
          low$(436%) = "33^FO340,5^CI0^A0R,26,26^FR^FD"
          low$(437%) = "34^FO318,5^CI0^A0R,26,26^FR^FD"
          low$(438%) = "36^FO296,5^CI0^A0R,26,26^FR^FD"
          low$(439%) = "35^FO296,380^CI0^A0R,26,26^FR^FD"
          low$(440%) = "38^FO274,5^CI0^A0R,26,26^FR^FD"
          low$(441%) = "37^FO274,380^CI0^A0R,26,26^FR^FD"
          low$(442%) = "41^FO252,5^CI0^A0R,26,26^FR^FD"
          low$(443%) = "42^FO252,380^CI0^A0R,26,26^FR^FD"
          low$(444%) = "40^FO230,5^CI0^A0R,26,26^FR^FD"
          low$(445%) = "39^FO230,380^CI0^A0R,26,26^FR^FD"
          low$(446%) = "^FO395,5^CI0^A0R,26,26^FR^FD+/-^FS"
          low$(447%) = "^FO530,57^CI0^A0R,26,26^FR^FDThis product is ENERGY STAR^FS"
          low$(448%) = "^FO500,57^CI0^A0R,26,26^FR^FDCertified in Highlighted Regions^FS"
          low$(449%) = "47^FO1330,260^CI0^A0R,35,33^FR^FD"  /* AWD021 */
          low$(450%) = "48^FO0204,5^CI0^A0R,26,26^FR^FD"   /* AWD024 */
/*AWD025 */  
          low$(451%) = "^FO1031,481^CI0^A0R,26,26^FR^FDAir Leakage (U.S./I-P)^FS"
          low$(452%) = "^FO971,504^GB0,44,06^FS"      /* IM8020 */
          low$(453%) = "^PQ1"
          low$(454%) = "^XZ"
/* </AWD017> */

          init  (" ") yy$()

          yy$(  1%) = "^JO"
          yy$(  2%) = "^XA^EG^XZ"   
          yy$(  3%) = hex(7e) & "DGPIC1    ,06784,032,"
          yy$(  4%) = "00,"
          yy$(  5%) = "04000000000000000000040000000000000000000004,"
          yy$(  6%) = "00,"
          yy$(  7%) = "00,"
          yy$(  8%) = "00606060606060606060600060606060606060606040006060606060606060,"
          yy$(  9%) = "40E0E0E0E0E0E0E0E0E0E060E0E0E0E0E0E0E0E0E0C060E0E0E0E0E0E0E0E040"
          yy$( 10%) = "01F9F9F9F9F9F9F9F9F9F079F9F9F9F9FDF9F9F9F9E071F9F9F9F9F9F9F9F880"
          yy$( 11%) = "07FFFFFFFFFFFFFFFFFFFC1FFFFFFFFFFFFFFFFFFFF801FFFFFFFFFFFFFFFE,"
          yy$( 12%) = "07FFFFFFFFFFFFFFFFFFFC1FFFFFFFFFFF1FFFFFFFFC07FFFFFFFFFFFFFFFE,"
          yy$( 13%) = "0F9F9F9F9F9F9F9F9F9F9E1F9F9F9F9F9E1F9F9F9F9F079F9F9F9F9F9F9F9E,"
          yy$( 14%) = "0F9F9F9F9F9F9F9F9F9F9C1F9F9F9F9F9E1F9F9F9FDF019F9F9F9F9F9F9F9E,"
          yy$( 15%) = "07FFFFFFFFFFFFFFFFFFFC1FFFFFFFFFFE1FFFFFFFFF803FFFFFFFFFFFFFFE,"
          yy$( 16%) = "07FFFFFFFFFFFFFFFFFFF87FFFFFFFFFF85FFFFFFFFFE0FFFFFFFFFFFFFFFE,"
          yy$( 17%) = "41F8F0F0F0F9F0F0F0F9F079F9F9F9F9F879F9F9F9F9F061F9F9F9F9F9F9F8C0"
          yy$( 18%) = "01F8000000FDC0000011F079F9F9F9F9F879F9F9F9F9F801F9F9F9F9F9F9F880"
          yy$( 19%) = "07F8000000FFC000001FFC1FFFFFFFFFF87FFFFFFFFFFE07FFFFFFFFFFFFFE,"
          yy$( 20%) = "07FE0E02083F82080E1FFC1FFFFFFFFFFE1FFFFFFFFFFF01FFFFFFFFFFFFFE,"
          yy$( 21%) = "0F9E0E060C1F860E0E1F9E1F9F9F9F9F9E1F9F9F9F9F9F071F9F9F9F9F9F9E,"
          yy$( 22%) = "0F9F9F0F9C1F8F9E9F0F9C1F9F9F9F9F9E1F9F9F9F9F9F001F9F9F9F9F9F9E,"
          yy$( 23%) = "07FFFFC3FCFFCFFCFF1FFC1FFFFFFFFFFE1FFFFFFFFFFFF01FFFFFFFFFFFFE,"
          yy$( 24%) = "07FFFFC3F8FFC3F8FE97F87FFFFFFFFFF87FFFFFFFFFFFF01FFFFFFFFFFFFE,"
          yy$( 25%) = "41F9F9E1F8F9C1F8F891F079F9F9F9F9F879F9F9F9F9F9F041F9F9F9F9F9F8C0"
          yy$( 26%) = "01F9F9C1F8F9C1F8F891F079F9F9F9F9F879F9F9F9F9F9F001F9F9F9F9F9F880"
          yy$( 27%) = "07FFFFC1F8FFC3F8FF9FFC1FFFFFFFFFF01FFFFFFFFFFFFE03FFFFFFFFFFFE,"
          yy$( 28%) = "07FFFFC3F8FF8FFEFF1FFC1FFFFFFFFFD81FFFFFFFFFFFFE07FFFFFFFFFFFE,"
          yy$( 29%) = "0F9F9F84079F8F9E9F1F9E1F9F9F9F9F040F9F9F9F9F9F9F069F9F9F9F9F9E,"
          yy$( 30%) = "0F9E1FDE07DF8FDFDF1F9C1F9F9F9F9F0F001F9F9F9F9F9F801F9F9F9F9F9E,"
          yy$( 31%) = "07FE3FFC07FFDFFFFFBFFC1FFFFFFFFF07007FFFFFFFFFFFE03FFFFFFFFFFE,"
          yy$( 32%) = "07F8FFF003FFF3FFFFFFF87FFFFFFFFFC280FFFFFFFFFFFFE0FFFFFFFFFFFE,"
          yy$( 33%) = "41F860F9F1F9F9F9F9F9F079F9F9F9F9C1E071F9F9F9F9F9F079F9F9F9F9F8C0"
          yy$( 34%) = "01F800F9F5F9F9F9FDF9F079F9F9F9F9C1E031F9F9F9F9FDF039F9F9F9F9F880"
          yy$( 35%) = "07FF8003FFFFF1F1F1FFFC1FFFFFFFFFC7E301FFFFFFFFFFF01FFFFFFFFFFE,"
          yy$( 36%) = "07FFF0001FFF8000001FFC1FFFFFFFFF87E3803FFFFFFFFFFE07FFFFFFFFFE,"
          yy$( 37%) = "0F9F96041F9F8404041F9E1F9F9F9F9F07878C1F9F9F9F9F9F0F9F9F9F9F9E,"
          yy$( 38%) = "0F9FDE1E071F8F0F0E0F9C1F9F9F9F9FC7C25E0F9F9F9F9F9F079F9F9F9F9E,"
          yy$( 39%) = "07FFF0FC02FFDF1F181FFC1FFFFFFFFFC7F0FC0FFFFFFFFFFF07FFFFFFFFFE,"
          yy$( 40%) = "07FFF0FFE0FFFFFFF07FF87FFFFFFFFFF1F0FF03FFFFFFFFFFC1FFFFFFFFFE,"
          yy$( 41%) = "41F9F0F9E0F9F9F9F071F079F9F9F9F9F1F0F841F9F9F9F9F9E0F9F9F9F9F8C0"
          yy$( 42%) = "01F9F0F8E0F9F9F841FDF079F9F9F9F9F0F079C1F9F9F9F9FDE0F9F9F9F9F880"
          yy$( 43%) = "07FFF0E003FFFFE02FFFFC1FFFFFFFFFF87F07F1FFFFFFFFFFE07FFFFFFFFE,"
          yy$( 44%) = "07FFF00407FFFF880FFFFC1FFFFFFFFFF81F0FD17FFFFFFFFFE03FFFFFFFFE,"
          yy$( 45%) = "0F9F84071F9F9F071F9F9E1F9F9F9F9F9E0F079C1F9F9F9F9F9E1F9F9F9F9E,"
          yy$( 46%) = "0F9F80031FDF9F071FDF9C1F9F9F9F9F9E0F07DC1F9F9F9F9F9A0FDF9F9F9E,"
          yy$( 47%) = "07FE000FFFFFD01FFFFFFC1FFFFFFFFFFE0F81FC1FFFFFFFFFFE0FFFFFFFFE,"
          yy$( 48%) = "07F801FFFFFFC0F9F9FFF87FFFFFFFFFFFE1F0FC7FFFFFFFFFFE03FFFFFFFE,"
          yy$( 49%) = "41F861F9F9F9C0F1F1F9F079F9F9F9F9F9E1F0F879F9F9F9F9F841F9F9F9F8C0"
          yy$( 50%) = "01F8F9F9F9F9C0000011F079F9F9F9F9F1E0F82079F9F9F9F9F801F9F9F9F880"
          yy$( 51%) = "07FEFFFFFFFFC000001FFC1FFFFFFFFFFFE0F8001FFFFFFFFFFF03FFFFFFFE,"
          yy$( 52%) = "07FF1F1F1FFF8E0E0E1FFC1FFFFFFFFFF8001F001FFFFFFFFFFF03FFFFFFFE,"
          yy$( 53%) = "0F9F0F0F0F9F8F0F0F1F9E1F9F9F9F9F9C041F041F9F9F9F9F9F079F9F9F9E,"
          yy$( 54%) = "0F9E0000001F9F9F9F9FDC1F9F9F9F9FDE000F8F1F9F9F9F9F9F035F9F9F9E,"
          yy$( 55%) = "07FE000000FFFFFFFFFFFC1FFFFFFFFFFE0007FFFFFFFFFFFFFFC07FFFFFFE,"
          yy$( 56%) = "07F8E0C0E0FFFFFFFFFFF87FFFFFFFFFFE8001FFFFFFFFFFFFFFE0FFFFFFFE,"
          yy$( 57%) = "41F9F9E1F8F9E0606071F079F9F9F9F9F9E060F9F9F9F9F9F9F9F079F9F9F8C0"
          yy$( 58%) = "01FDFDE1F8FDE0606071F079F9F9F9F9F9E000F9F9F9F9F9F9F9F07DF9F9F880"
          yy$( 59%) = "07FFFFC3F8FFC000001FFC1FFFFFFFFFFFF000FFFFFFFFFFFFFFF07FFFFFFE,"
          yy$( 60%) = "07FFFFC3FC3F8000001FFC1FFFFFFFFFFFFC08FFFFFFFFFFFFFFF41FFFFFFE,"
          yy$( 61%) = "0F9F9F879C1F8F1E1F1F9E1F9F9F9F9F9F9F0F9F9F9F9F9F9F9F9C1F9F9F9E,"
          yy$( 62%) = "0F9F9F039C1F8F9E9F0F9C1F9F9F9F9F9F8E079F9F9F9F9F9F9F9E0F9F9F9E,"
          yy$( 63%) = "07FFFF03F8FFCFFCFF1FFC1FFFFFFFFFFFE005FFFFFFFFFFFFFFFC1FFFFFFE,"
          yy$( 64%) = "07F8000060FFC3F8FE97F87FFFFFFFFFFF8001FFFFFFFFFFFFFFFC07FFFFFE,"
          yy$( 65%) = "41F8E060E0F9C1F8F891F079F9F9F9F9F9E041F9F9F9F9F9F9F9F841F9F9F8C0"
          yy$( 66%) = "01F820F001F9C1F8FC91F079F9F9F9F9FC80E1F9F9F9F9F9F9F9FC01F9F9F880"
          yy$( 67%) = "07FFFFFFFFFFC3F8FF9FFC1FFFFFFFFFFF8FFFFFFFFFFFFFFFFFFE03FFFFFE,"
          yy$( 68%) = "07FF1FFF1FFF8FFEFF1FFC1FFFFFFFFFFF0FFFFFFFFFFFFFFFFFFE07FFFFFE,"
          yy$( 69%) = "0F9F9F9F9E1F8F9F9F1F9E1F9F9F9F9F9F8F9F9F9F9F9F9F9F9F9F079F9F9E,"
          yy$( 70%) = "0F9F9F9FDE1F8FDF9F1F9C1F9F9F9F9F9F0F9F9F9F9F9F9F9F9F9F079F9F9E,"
          yy$( 71%) = "07FFFFFFFCFFFFFFFFFFFC1FFFFFFFFFFF83FFFFFFFFFFFFFFFFFF83FFFFFE,"
          yy$( 72%) = "07FFFFFFF8FFFFFFFFFFF87FFFFFFFFFFFE3FFFFFFFFFFFFFFFFFF81FFFFFE,"
          yy$( 73%) = "41F9F9F9F8F9F1F1F1F9F079F9F9F9F9F9E0F9F9F9F9F9F9F9F9F9E0F9F9F8C0"
          yy$( 74%) = "01F9F1F1F0F9C0000011F079F9F9F9F9FC8011F9F9F9F9F9F9F9F9E0FDF9F880"
          yy$( 75%) = "07FFFFFFF8FFC000001FFC1FFFFFFFFFFF8007FFFFFFFFFFFFFFFFE0FFFFFE,"
          yy$( 76%) = "07FE0000003F8400041FFC1FFFFFFFFFFF0C01FFFFFFFFFFFFFFFFE0FFFFFE,"
          yy$( 77%) = "0F9E0606041F8E0E0E1F9E1F9F9F9F9F9F0C079F9F9F9F9F9F9F9F861F9F9E,"
          yy$( 78%) = "0F9E0E0E0C1FDF9E1F0F9C1F9F9F9F9F9F0F081F9F9F9F9F9F9F9F881F9F9E,"
          yy$( 79%) = "07FFFFFFFCFFFFFCFF1FFC1FFFFFFFFFFF83043FFFFFFFFFFFFFFFE83FFFFE,"
          yy$( 80%) = "07FFFFFFF8FFFFF8FE97F87FFFFFFFFFFFE3007FFFFFFFFFFFFFFFE0FFFFFE,"
          yy$( 81%) = "41F9F9F9F8F9F9F8F8D1F079F9F9F9F9F9E0E079F9F9F9F9F9F9F9F0F9F9F8C0"
          yy$( 82%) = "01FDF9F9F8F9F9F8F891F079F9F9F9F9F9E0E039F9F9F9F9F9F9F9F0FDF9F880"
          yy$( 83%) = "07FFFFFFF8FFFFF8FE1FFC1FFFFFFFFFFFF0E03FFFFFFFFFFFFFFFF83FFFFE,"
          yy$( 84%) = "07FFFFFFFC3FFFF83E1FFC1FFFFFFFFFFFF0003FFFFFFFFFFFFFFFF83FFFFE,"
          yy$( 85%) = "0F9F9F9F9C1F9F8E1E1F9E1F9F9F9F9F9F86041F9F9F9F9F9F9F9F9C1F9F9E,"
          yy$( 86%) = "0F9F9F9F9F1F8E010E1F9C1F9F9F9F9F9F9C1E1F9F9F9F9F9F9F9FDC0F9F9E,"
          yy$( 87%) = "07FFFFFFFFFFC007007FFC1FFFFFFFFFFFE41FFFFFFFFFFFFFFFFFF81FFFFE,"
          yy$( 88%) = "07FFFFFFFFFFC001C07FF87FFFFFFFFFFFE01FFFFFFFFFFFFFFFFFF83FFFFE,"
          yy$( 89%) = "41F8606060F9F1F9F1F9F079F9F9F9F9F9E041F9F9F9F9F9F9F9F9F879F9F8C0"
          yy$( 90%) = "01F8000000F9F1F1F1FDF079F9F9F9F9FDE001F9F9F9F9F9F9F9F9F839F9F880"
          yy$( 91%) = "07FE000000FFFFFFFFFFFC1FFFFFFFFFFFF001FFFFFFFFFFFFFFFFFC0FFFFE,"
          yy$( 92%) = "07FFFFFF80BFFE00077FFC1FFFFFFFFFFFFC003FFFFFFFFFFFFFFFFC0FFFFE,"
          yy$( 93%) = "0F9F9F9F061F9E06071F9E1F9F9F9F9F9F9E041F9F9F9F9F9F9F9F9C0F9F9E,"
          yy$( 94%) = "0F9FDFDE0FDF8002001F9C1F9F9F9F9F9F9F000F9F9F9F9F9F9F9FDE0F9F9E,"
          yy$( 95%) = "07FFFFFC07FFC000001FFC1FFFFFFFFFFFFF001FFFFFFFFFFFFFFFFC0FFFFE,"
          yy$( 96%) = "07FFFFE07FFFC0FBF817F87FFFFFFFFFFFFFE07FFFFFFFFFFFFFFFFC0BFFFE,"
          yy$( 97%) = "41F9F9E0F1F9E0F1F071F079F9F9F9F9F9F9E0F9F9F9F9F9F9F9F9F861F9F8C0"
          yy$( 98%) = "01F9F001F9FDC1FDFC95F079F9F9FDF1F9F9F039F9F9F9F9F9F9F9F801F9F880"
          yy$( 99%) = "07FFC03FFFFFCFF9FF87FC1FFFFFFF00FFFFF80FFFFFFFFFFFFFFFFC0BFFFE,"
          yy$(100%) = "07FF801FFFFF8FFFFF8FFC1FFFFFFF003FFFFE0FFFFFFFFFFFFFFFFE0FFFFE,"
          yy$(101%) = "0F9E0F9F9F9F8F9D9F0F9E1F9F9F9F0E0F9F0C0F9F9F9F9F9F9F9F9F0F9F9E,"
          yy$(102%) = "0F9E0FDFDF9F8F999F0F9C1F9F9FDF0C0FDF080F9F9F9F9F9F9F9FDF0F9F9E,"
          yy$(103%) = "07FE000000FF8FF8FE1FFC1FFFFFFF0301FC01FFFFFFFFFFFFFFFFFE0FFFFE,"
          yy$(104%) = "07F8000000FFC000F01FF87FFFFFFFC0F07001FFFFFFFFFFFFFFFFFC03FFFE,"
          yy$(105%) = "41F8606060F9E060F071F079F9F9F9E0F07041F9F9F9F9F9F9F9F9F841F9F8C0"
          yy$(106%) = "01F9F1F1F1FDE000F0FDF079F9F9F9F07010E0F9F9F9F9F9F9F9F9F801F9F880"
          yy$(107%) = "07FFFFFFFFFFC000F07FFC1FFFFFFFF0381080FFFFFFFFFFFFFFFFFD03FFFE,"
          yy$(108%) = "07FFFFFFFFFFFFFFFFFFFC1FFFFFFFFE0700001FFFFFFFFFFFFFFFFF0FFFFE,"
          yy$(109%) = "0F9F9F9F9F9F9F1F9F9F9E1F9F9F9F9E0704060F9F9F9F9F9F9F9F9F0F9F9E,"
          yy$(110%) = "0F9F0F0F0F1F9F9F9F0F9C1F9F9F9F9F0000070F9F9F9F9F9F9F9FDF0F9F9E,"
          yy$(111%) = "07FE000000FFFFFFF81FFC1FFFFFFFFFF8000003FFFFFFFFFFFFFFFF07FFFE,"
          yy$(112%) = "07F8000000FFFFFFF017F87FFFFFFFFFF00000C3FFFFFFFFFFFFFFFD03FFFE,"
          yy$(113%) = "41F870E0F0F9F9F9E079F079F9F9F9F9F8E040E1F9F9F9F9F9F9F9F841F9F8C0"
          yy$(114%) = "01F870E0F0FDF9F9E0F1F079F9F9FDF9F8E000E1F9F9F9F9F9F9F9F801F9F880"
          yy$(115%) = "07F87FC3F8FFFFF803FFFC1FFFFFFFFFFFF00031FFFFFFFFFFFFFFFC03FFFE,"
          yy$(116%) = "07FE3FC3FC3FDF9E0FFFFC1FFFFFFFFFFFFE0011FFFFFFFFFFFFFFFF0FFFFE,"
          yy$(117%) = "0F9E1F879C1F84041F9F9E1F9F9F9F9F9F9F06051F9F9F9F9F9F9F9F0F9F9E,"
          yy$(118%) = "0F9E1F8F9C1F8E000F9F9C1F9F9F8E0FDFDF07011F9F9F9F9F9F9F9F0F9F9E,"
          yy$(119%) = "07FE3FC3FCFFC0003FFFFC1FFFFFE00FFFFF8701FFFFFFFFFFFFFFFE0FFFFE,"
          yy$(120%) = "07F87FE3F8FFFFF801FFF87FFFFFE000FFFFE1C1FFFFFFFFFFFFFFFC03FFFE,"
          yy$(121%) = "41F879E1F8F9F9F841F9F079F9F9E040F9F9E1E1F9F9F9F9F9F9F9F841F9F8C0"
          yy$(122%) = "01F879F1F8FDF9FDE071F079F9F9E0F035F9F0F1F9F9F9F9F9F9F9F801F9F880"
          yy$(123%) = "07FFFFFFFFFFFFFFF81FFC1FFFFFE0FC07FC78FFFFFFFFFFFFFFFFFC0FFFFE,"
          yy$(124%) = "07FFFFFFFFFFFFFFFE1FFC1FFFFFE83E07FE183FFFFFFFFFFFFFFFFC0FFFFE,"
          yy$(125%) = "0F9F1F1F1F9F9F9F9F1F9E1F9F9F9E0F071C061F8F9F9F9F9F9F9F9E0F9F9E,"
          yy$(126%) = "0F9F1F1F1F9F9F9F9F1FDC1F9F9F9E0F031C001F8FDF9F9F9F9F9F9E0F9F9E,"
          yy$(127%) = "07FE000000FFFFFFFFFFFC1FFFFFFE03F010001F83FFFFFFFFFFFFFC0FFFFE,"
          yy$(128%) = "07F8000000FFFFFFFFFFF87FFFFFFE01F070E03FE1FFFFFFFFFFFFF80FFFFE,"
          yy$(129%) = "41F8E0E0E0F9F9F9F9F9F079F9F9F9E0F850F069E0F9F9F9F9F9F9F869F9F8C0"
          yy$(130%) = "01F9F9E1F8F9F9F9F9F9F079F9F9F9F000807801E0F9F9F9F9F9F9F839F9F880"
          yy$(131%) = "07FFFFC3F8FFFFFFFFFFFC1FFFFFFFF801807F0FE3FFFFFFFFFFFFF82FFFFE,"
          yy$(132%) = "07FFFFC3FC3FFFFFFFFFFC1FFFFFFFFF00020F1F83FFFFFFFFFFFFFC1FFFFE,"
          yy$(133%) = "0F9F9F879C1F9F9F9F9F9E1F9F9F9F9F04060F0F879F9F9F9F9F9F9C0F9F9E,"
          yy$(134%) = "0F9F9FC39C1F9E1F0F1F9C1F9F9F9F9F9E0007DF83DF9F9F9F9F9F9C0F9F9E,"
          yy$(135%) = "07FFFF03F8FFC07F001FFC1FFFFFFFFFFF8000FF83FFFFFFFFFFFFF83FFFFE,"
          yy$(136%) = "07FFFFC1F8FFE07F007FF87FFFFFFFFFFFE060FFE0FFFFFFFFFFFFF83FFFFE,"
          yy$(137%) = "41F9F9C1F0F9E0794079F079F9F9F9F9F8E060F9E0F9F9F9F9F9F9F079F9F8C0"
          yy$(138%) = "01F8E020E0FDC0F82015F079F9F9FDF9FDF00039E0F9F9F9F9F9F9F0F9F9F880"
          yy$(139%) = "07FE000000FFC3FE001FFC1FFFFFFFFFFFF0002FE0FFFFFFFFFFFFF83FFFFE,"
          yy$(140%) = "07FE000C03FF8FFE3F0FFC1FFFFFFFFFFFFF000782FFFFFFFFFFFFE83FFFFE,"
          yy$(141%) = "0F9E060E079F8F9E1F0F9E1F9F9F9F9F9F9F040F879F9F9F9F9F9F8C1F9F9E,"
          yy$(142%) = "0F9F1F1F0F9F8FDE1F0FDC1F9F9FCE0FDF9F070383DF9F9F9F9F9F881F9F9E,"
          yy$(143%) = "07FFFFFFFFFF8FE8FF8FFC1FFFFFF8001FFF81FFE0FFFFFFFFFFFFE0FFFFFE,"
          yy$(144%) = "07FFFFFFFFFFC3F0FF87F87FFFFFF000FFFFE1F1E0FFFFFFFFFFFFE0FFFFFE,"
          yy$(145%) = "41F9F9F9F9F9C0E0F051F079F9F9F04040F9E0F9E0F9F9F9F9F9F9E0F9F9F8C0"
          yy$(146%) = "01FDF9F9FDF9C0E0F091F079F9F9F00000FDE0F9E0F9F9F9F9F9F9E0F9F9F880"
          yy$(147%) = "07FFFFFFFFFFC001F01FFC1FFFFFF8000000F8FFE0FFFFFFFFFFFF81FFFFFE,"
          yy$(148%) = "07FFFFFFFFFFFE0FFF7FFC1FFFFFFE0F00001E3FA0FFFFFFFFFFFF83FFFFFE,"
          yy$(149%) = "0F9F9F9F9F9F9E0F9F1F9E1F9F9F9E0F0C041E1F869F9F9F9F9F9F879F9F9E,"
          yy$(150%) = "0F9F9F9F9F9F9F1F9F1FDC1F9F9F9F03DF001E1F829F9F9F9F9F9F079F9F9E,"
          yy$(151%) = "07FFFFFFFFFFFFFFFFFFFC1FFFFFFF03FF001E3FE0FFFFFFFFFFFF83FFFFFE,"
          yy$(152%) = "07FFFFFFFFFFFFFFFE97F87FFFFFFFC0FFF0FE2FE0FFFFFFFFFFFE83FFFFFE,"
          yy$(153%) = "41F9F9F9F9F9F9F9F8D1F079F9F9F9C0F9F0F879E0F9F9F9F9F9F8C1F9F9F8C0"
          yy$(154%) = "01F9F9F9F9FDFDF9FC95F079F9F9F9E0F9F9F901E0FDF9F9F9F9F801F9F9F880"
          yy$(155%) = "07FFFFFFFFFFFFFFFF9FFC1FFFFFFFF03FFFFF03E03FFFFFFFFFFC07FFFFFE,"
          yy$(156%) = "07FFFFFFFFFFFFFFFF1FFC1FFFFFFFD01FFFFF0FE01FFFFFFFFFFE0FFFFFFE,"
          yy$(157%) = "0F9F9F9F9F9F8F0F0E0F9E1F9F9F9F9C0F9F9F0784071F9F9F9F9E0F9F9F9E,"
          yy$(158%) = "0F9F9F9F9F9F8F0F0E0F9C1F9F9F9F9C0F9F9F0780075F9F9F9F9E0F9F9F9E,"
          yy$(159%) = "07FFFFFFFFFFC000001FFC1FFFFFFFFC0FFFFFC3F4001FFFFFFFFC1FFFFFFE,"
          yy$(160%) = "07FFFFFFFFFFF0F0F097F87FFFFFFFFD01FFFFF1FFE000FFFFFFF07FFFFFFE,"
          yy$(161%) = "41F9F9F9F9F9F0F0F0D1F079F9F9F9F841F9F9F1F9E040F9F9F9F079F9F9F8C0"
          yy$(162%) = "01F9F9F9F9FDF9F9FC95F079F9F9F9F9C1F9F9F1F9F08021F9F9F079F9F9F880"
          yy$(163%) = "07FFFFFFFFFFFFFFFF9FFC1FFFFFFFFF41FFFFF1FFF80007FFFFF07FFFFFFE,"
          yy$(164%) = "07FFFFFFFFFFDFFFFF1FFC1FFFFFFFFFC17FFFFD7FFF88001FFFC17FFFFFFE,"
          yy$(165%) = "0F9F9F9F9F9F9F9F9F0F9E1F9F9F9F9F051F9F9D1F9F8E061F9F851F9F9F9E,"
          yy$(166%) = "0F9F9F9F9F9F8F1FDF0F9C1F9F9F9F9FC01F9F9C5F9FDF001F9F031F9F9F9E,"
          yy$(167%) = "07FFFFFFFFFFC007FFFFFC1FFFFFFFFFD07FFFFC7FFFFE001FFF03FFFFFFFE,"
          yy$(168%) = "07FFFFFFFFFFC001FFFFF87FFFFFFFFFF07FFFFC7FFFF8001FFF41FFFFFFFE,"
          yy$(169%) = "41F9F9F9F9F9F040F1F9F079F9F9F9F9E0F9F9F879F9E040F9F841F9F9F9F8C0"
          yy$(170%) = "01F9F9F9F9FDF000F1F9F079F9F9F9F9E0F9F9F87DF9E000F9F801F9F9F9F880"
          yy$(171%) = "07FFFFFFFFFFFF1001FFFC1FFFFFFFFF01FFFFFC7FF8000FFFFE03FFFFFFFE,"
          yy$(172%) = "07FFFFFFFFFFFF0E073FFC1FFFFFFFFF07FFFFFE1FFE000FFFFE0FFFFFFFFE,"
          yy$(173%) = "0F9F9F9F9F9F9F1F0E0F9E1F9F9F9F9F079F9F9F1F04071F9F9E0F9F9F9F9E,"
          yy$(174%) = "0F9F9F9F9F9FDF1F9E0F9C1F9F9F9F9E0F9F9F9FDC000F9F9FDA1F9F9F9F9E,"
          yy$(175%) = "07FFFFFFFFFFFF1FFE1FFC1FFFFFFFFC0FFFFFFFF0001FFFFFF83FFFFFFFFE,"
          yy$(176%) = "07FFFFFFFFFFFF1FC017F87FFFFFFFF02FFFFFFFE001FFFFFFE0FFFFFFFFFE,"
          yy$(177%) = "41F9F9F9F9F9F9F1E071F079F9F9F9F069F9F9F9E041F9F9F9E0F9F9F9F9F8C0"
          yy$(178%) = "01F9F9F9F9FDF9B000F9F079F9F9F9F039F9F9F9E0F9F9F9F9E0FDF9F9F9F880"
          yy$(179%) = "07FFFFFFFFFFF000FFFFFC1FFFFFFFC0FFFFFFFFE0FFFFFFFFC1FFFFFFFFFE,"
          yy$(180%) = "07FFFFFFFFFFFE001FFFFC1FFFFFFFC03FFFFFFFE0FFFFFFFF01FFFFFFFFFE,"
          yy$(181%) = "0F9F9F9F9F9F840F9F9F9E1F9F9F9F069F9F0F9F869F9F9F9F079F9F9F9F9E,"
          yy$(182%) = "0F9F9F9F9F9F800F9F9F9C1F9F9FDF021F9F0FDF809FDF9F9F079F9F9F9F9E,"
          yy$(183%) = "07FFFFFFFFFFC2FFFFFFFC1FFFFFFF03FE0007FFE0FFFFFFFC07FFFFFFFFFE,"
          yy$(184%) = "07FFFFFFFFFFC0FFFFFFF87FFFFFFF01F8E001FFE0FFFFFFFC07FFFFFFFFFE,"
          yy$(185%) = "41F9F9F9F9F9F9F9F9F9F079F9F9F841E04040F9E0F9F9F9F071F9F9F9F9F8C0"
          yy$(186%) = "01F9F9F9F9F9F0F0F0F1F079F9F9F8000000E0F9E0F9F9FDF039F9F9F9F9F880"
          yy$(187%) = "07FFFFFFFFFFF1F1F1FFFC1FFFFFF800000080FFE0FFFFFFF03FFFFFFFFFFE,"
          yy$(188%) = "07FFFFFFFFFF8000001FFC1FFFFFF800071FE03F82FFFFFF803FFFFFFFFFFE,"
          yy$(189%) = "0F9F9F9F9F9F8404040F9E1F9F9F9E04071F8C1F869F9F9F841F9F9F9F9F9E,"
          yy$(190%) = "0F9F9F9F9F9F8000000FDC1F9F9F9800071F881F829F9F9F801F9F9F9F9F9E,"
          yy$(191%) = "07FFFFFFFFFFC600061FFC1FFFFFE8003FFFF80FE0FFFFFF80FFFFFFFFFFFE,"
          yy$(192%) = "07FFFFFFFFFFFFF8FE97F87FFFFFF8FFFFFFF803E0FFFFFE03FFFFFFFFFFFE,"
          yy$(193%) = "41F9F9F9F9F9F9F8F8D1F079F9F9F0F1F9F9F841E0F9F9F8C1F9F9F9F9F9F8C0"
          yy$(194%) = "01F9F9F9F9F9F9F8F891F079F9F9F9F9F9F9FC01E0F9F9F001F9F9F9F9F9F880"
          yy$(195%) = "07FFFFFFFFFFFFF8FF9FFC1FFFFFFFFFFFFFFF01E0FFFFF80FFFFFFFFFFFFE,"
          yy$(196%) = "07FFFFFFFFFFFFFEBF1FFC1FFFFFFFFFFFFFFF0103FFFFF01FFFFFFFFFFFFE,"
          yy$(197%) = "0F9F9F9F9F9F9F9E1F0F9E1F9F9F9F9F9F9F9F07079F9F9C1F9F9F9F9F9F9E,"
          yy$(198%) = "0F9F9F9F9F9F9F081E1FDC1F9F9FDF9F9F9F9F80039F9F001F9F9F9F9F9F9E,"
          yy$(199%) = "07FFFFFFFFFFC000001FFC1FFFFFFFFFFFFFFFF003FFFF01FFFFFFFFFFFFFE,"
          yy$(200%) = "07FFFFFFFFFFC000001FF87FFFFFFFFFFFFFFFF000FFFF41FFFFFFFFFFFFFE,"
          yy$(201%) = "41F9F9F9F9F9C071E0F9F079F9F9F9F9F9F9F9F040F9F841F9F9F9F9F9F9F8C0"
          yy$(202%) = "01F9F9F9F9F9C0F1E0F9F079F9F9F9F9F9F9F9F000F9F801F9F9F9F9F9F9F880"
          yy$(203%) = "07FFFFFFFFFFFFFFFFFFFC1FFFFFFFFFFFFFFFFE03FFF80FFFFFFFFFFFFFFE,"
          yy$(204%) = "07FFFFFFFFFFFFFFFFFFFC1FFFFFFFFFFFFFFFFF03FF803FFFFFFFFFFFFFFE,"
          yy$(205%) = "0F9F9F9F9F9F9F9F9F9F9E1F9F9F9F9F9F9F9F9F079F841F9F9F9F9F9F9F9E,"
          yy$(206%) = "0F9F9F9F9F9F9F9F9F9F9C1F9F9F9F9F9F9F9F9F8F9F019F9F9F9F9F9F9F9E,"
          yy$(207%) = "07FFFFFFFFFFFFFFFFFFFC1FFFFFFFFFFFFFFFFF83FE00FFFFFFFFFFFFFFFE,"
          yy$(208%) = "07FFFFFFFFFFFFFFFFFFF87FFFFFFFFFFFFFFFFFFFF001FFFFFFFFFFFFFFFE,"
          yy$(209%) = "41F9F9F9F9F9F9F9F9F9F079F9F9F9F9F9F9F9F9F1F041F9F9F9F9F9F9F9F8C0"
          yy$(210%) = "01FDFDFDFDFDFDFDFDFDF07DFDFDFDFDFDFDFDFDF9E071F9FDFDFDFDFDFDFC80"
          yy$(211%) = "00,"
          yy$(212%) = "06060606060606060606040606060606060606060600060606060606060606,"
          yy$(213%) = "04040404040404040404040404040404040404040404040404040404040404,"
          yy$(214%) = "00,"
          yy$(215%) = "00,"
      /*---- copy map here ----------------------*/
          yy$(404%) = "^XA"
          yy$(405%) = "^PMN"
          yy$(406%) = "^MNY"
          yy$(407%) = "^MMT"
          yy$(408%) = "^MTT"
          yy$(409%) = "^MD0"
          yy$(410%) = "^LH0,0"
          yy$(411%) = "^LL813"
          yy$(412%) = "^PR6,4,8"                     /* PR Label Print Speed*/
          yy$(413%) = "^JMA"
          yy$(414%) = "^COY,362"               /* 256 + 128 Mem (-) 22k Intern*/
          yy$(415%) = "^FO563,96^FR^XGPIC1    ,1,1^FS"
          yy$(416%) = "^FO502,404^FR^XGLABEL2.GRF,1,1^FS"
          yy$(417%) = "05^FO1138,189^CI0^A0R,71,71^FR^FD"
          yy$(418%) = "18^FO958,189^CI0^A0R,71,71^FR^FD"
          yy$(419%) = "09^FO370,157^CI0^A0R,61,61^FR^FD"
          yy$(420%) = "22^FO414,231^CI0^A0R,22,22^FR^FD"
          yy$(421%) = "23^FO386,231^CI0^A0R,22,22^FR^FD"
          yy$(422%) = "28^FO221,445^CI0^A0R,33,33^FR^FD"
          yy$(423%) = "29^FO189,478^CI0^A0R,33,33^FR^FD"
          yy$(424%) = "13^FO122,37^CI0^A0R,30,26^FR^FD"
          yy$(425%) = "14^FO91,37^CI0^A0R,26,22^FR^FD"
          yy$(426%) = "15^FO61,37^CI0^A0R,26,22^FR^FD"
          yy$(427%) = "16^FO30,37^CI0^A0R,26,22^FR^FD"
          yy$(428%) = "19^FO961,491^CI0^A0R,71,71^FR^FD"    
          yy$(429%) = "07^FO1138,516^CI0^A0R,71,71^FR^FD"    
          yy$(430%) = "30^FO1544,240^CI0^A0R,35,33^FR^FD"
          yy$(431%) = "31^FO1544,445^CI0^A0R,35,33^FR^FD"
          yy$(432%) = "17^FO1360,240^CI0^A0R,35,33^FR^FD"
          yy$(433%) = "04^FO1390,240^CI0^A0R,35,33^FR^FD"
          yy$(434%) = "03^FO1420,240^CI0^A0R,35,33^FR^FD"
          yy$(435%) = "02^FO1450,240^CI0^A0R,35,33^FR^FD"
          yy$(436%) = "01^FO1480,240^CI0^A0R,35,33^FR^FD"
          yy$(437%) = "32^FO73,284^CI0^A0R,37,37^FR^FD"
          yy$(438%) = "33^FO340,5^CI0^A0R,26,26^FR^FD"
          yy$(439%) = "34^FO318,5^CI0^A0R,26,26^FR^FD"
          yy$(440%) = "36^FO296,5^CI0^A0R,26,26^FR^FD"
          yy$(441%) = "35^FO296,380^CI0^A0R,26,26^FR^FD"
          yy$(442%) = "38^FO274,5^CI0^A0R,26,26^FR^FD"
          yy$(443%) = "37^FO274,380^CI0^A0R,26,26^FR^FD"
          yy$(444%) = "41^FO252,5^CI0^A0R,26,26^FR^FD"
          yy$(445%) = "42^FO252,380^CI0^A0R,26,26^FR^FD"
          yy$(446%) = "40^FO230,5^CI0^A0R,26,26^FR^FD"
          yy$(447%) = "39^FO230,380^CI0^A0R,26,26^FR^FD"
          yy$(448%) = "43^FO208,5^CI0^A0R,26,26^FR^FD"
          yy$(449%) = "44^FO208,380^CI0^A0R,26,26^FR^FD"
          yy$(450%) = "45^FO186,5^CI0^A0R,26,26^FR^FD"
          yy$(451%) = "46^FO186,380^CI0^A0R,26,26^FR^FD"
          yy$(452%) = "^FO392,13^CI0^A0R,26,26^FR^FD+/-^FS"
          yy$(453%) = "^FO530,57^CI0^A0R,26,26^FR^FDThis product is ENERGY STAR^FS"
          yy$(454%) = "^FO500,57^CI0^A0R,26,26^FR^FDCertified in Highlighted Regions^FS"
          yy$(455%) = "47^FO1330,240^CI0^A0R,35,33^FR^FD" /* AWD021 */
          yy$(456%) = "48^FO0204,5^CI0^A0R,26,26^FR^FD"   /* AWD024 */
          yy$(457%) = "^FO1031,481^CI0^A0R,26,26^FR^FDAir Leakage (U.S./I-P)^FS"   
          yy$(458%) = "^FO971,504^GB0,44,06^FS"      /* IM8020 */
          yy$(459%) = "^PQ1"
          yy$(460%) = "^XZ"

/* </AWD017> */

        return                                        /* (EWD011)                 */
