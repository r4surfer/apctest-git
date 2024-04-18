        REM *************************************************************~
            *                                                           *~
            *  Program Name      - RGADATFX                             *~
            *  Creation Date     - 02/13/98                             *~
            *  Last Modified Date-                                      *~
            *  Description       - This program will change all the     *~
            *                      mm/dd/yy dates to the proper         *~
            *                      pd(11,1) format.                     *~
            *                                                           *~
            *                                                           *~
            *  Special Comments  - THIS IS A PROGRAM TO BE RAN ONCE     *~
            *                      DURING THE Y2K CONVERSION!           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/13/98 ! New Program for (EWD) - Last Mod Date    ! DJD *~
            *          !                                          !     *~
            *************************************************************

	    /* RGAHD Fields */
        dim rga_dte$8,          	 /* Original RGA Date          */~
            rga_filed_dte$8,             /* Date RGA Was Filed         */~
	    rga_mod_dte$8                /* RGA Modification Date      */

	    /* RGADT Fields */
	dim rga_prod_dte$8,		 /* Production Date	       */~
	    rga_pickup_dte$(3)8,	 /* Pickup Dates	       */~
	    rga_dt_mod_dte$		 /* Detail Mod Dates	       */

	dim hdkey$4,			 /* RGA Number Key Field       */~
	    dtKey$6 			 /* RGA Detail Key	       */

        dim f2%(12%),                    /* = 0 if the file is open    */~
            f1%(12%),                    /* = 1 if READ was successful */~
            fs%(12%),                    /* = 1 if file open, -1 if it */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(12%)20                 /* Text from file opening     */

	dim blankdate$6,		 /* Blank (empty) date         */~
	    workdate10$10,         	 /* Century date mm-dd-yyyy    */~
	    tempdate$6,			 /* Place to store PD Value    */~
	    workdate8$8			 /* Regular date mm-dd-yy      */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 02/13/98 Y2K Fix For RGA Header File    "
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
            * #01 ! APCRGAHD ! APC RGA Header Master File               *~
            * #02 ! APCRGADT ! APC RGA Detail Master File               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,  "APCRGAHD",                                      ~
                        varc,     indexed,  recsize =   80,              ~
                        keypos =   12, keylen =   4,                     ~
                        alt key  1, keypos =  10, keylen =   6,          ~
                            key  2, keypos =   1, keylen =  15

            select #2,  "APCRGADT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   12, keylen =   6,                     ~
                        alt key  1, keypos =  10, keylen =   8,          ~
                            key  2, keypos =   1, keylen =  17



            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" ( #1, fs%(1%), f2%( 1%), 100%, rslt$(1%))
	    call "OPENCHCK" ( #2, fs%(2%), f2%( 2%), 100%, rslt$(2%))
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

	    call "DATUFMTC" (blankdate$)


	    call "SHOSTAT" ("Press ENTER to fix the RGA date fields!")
            stop


	    init(" ") hdkey$, dtKey$

	    display " "

            goto detailloop
	
ReadHeadLoop:
	    read #1, hold, key > hdkey$, using APCRGAHD, hdKey$, RGA_DTE$, RGA_FILED_DTE$, ~
                                                   RGA_MOD_DTE$, ~
				      		   eod goto ProcessDetails

	
	    call "DATUNFMT" (rga_dte$)
	    call "DATUNFMT" (rga_filed_dte$)
	    call "DATUNFMT" (rga_mod_dte$)

	    put #1 using APCRGAHD, hdKey$, RGA_DTE$, RGA_FILED_DTE$, RGA_MOD_DTE$
	    rewrite #1
	    display at(1,1), "Processing (Header): " & hdKey$
	    goto ReadHeadLoop


ProcessDetails:
	    dtKey$ = " "
DetailLoop:
	    read #2, hold, key > dtkey$, using APCRGADT, dtKey$, rga_prod_dte$, ~
						 	 rga_pickup_dte$(),     ~
							 rga_dt_mod_dte$,       ~
							 eod goto EndOfFile

	
	    call "DATECONV" (rga_prod_dte$)             /* STORED AS YYMMDD */
	    call "DATECONV" (rga_pickup_dte$(1))        /* STORED AS YYMMDD */
            call "DATECONV" (rga_pickup_dte$(2))        /* STORED AS YYMMDD */
            call "DATECONV" (rga_pickup_dte$(3))        /* STORED AS YYMMDD */
	    call "DATUNFMT" (rga_dt_mod_dte$)           /* STORED AS MM-DD-YY */

	    put #2 using APCRGADT, dtKey$, rga_prod_dte$, ~
                                           rga_pickup_dte$(),     ~
                                           rga_dt_mod_dte$
	    rewrite #2
	    display at(2,1), "Processing (Detail): " & dtKey$
	    goto DetailLoop


	REM ******************************************************************
	REM * format statements for reading the dates from the Header Record *
	REM ******************************************************************
APCRGAHD:   FMT POS(12),			     /* SKIP  		  */~
		CH(4),  			     /* RGA NUMBER        */~
                POS(19),			     /* SKIP FIRST FIELDS */~
		CH(08),				     /* RGA DATE          */~
	        CH(08),				     /* FILED DATE        */~
	 	POS(43),			     /* SKIP NEXT FIELDS  */~
		CH(08)				     /* MOD DATE	  */


APCRGADT:   FMT POS(12),  			     /* SKIP		  */~
		CH(06),				     /* KEY 		  */~
                POS(167),			     /* SKIP 	          */~
		CH(08),				     /* PRODUCTION DATE   */~
	   	POS(200),			     /* SKIP		  */~
		3*CH(08),			     /* PICKUP DATES      */~
		POS(236),			     /* SKIP		  */~
		CH(08) 			     	     /* MOD DATE 	  */

EndOfFile:
	call "SHOSTAT" ("One moment please!")
	end
            
