        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  M   M   AAA   N   N  U   U   AAA   L                     *~
            *  MM MM  A   A  NN  N  U   U  A   A  L                     *~
            *  M M M  AAAAA  N N N  U   U  AAAAA  L                     *~
            *  M   M  A   A  N  NN  U   U  A   A  L                     *~
            *  M   M  A   A  N   N   UUU   A   A  LLLLL                 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * MANUAL   - The subroutine is passed a name of a program or*~
            *            subroutine. The program name is then passed    *~
            *            via dynamic link to ONLINEWP to convert &      *~
            *            display the document.                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/05/82 ! ORIGINAL                                 ! ECR *~
            * 05/04/83 ! ADDED ACCESS = PRINT TO DISPLAY GETPARM  ! ECR *~
            * 05/12/83 ! CHANGED "READFDR" TO "FIND" DOCFILE VOL. ! ECR *~
            * 11/22/83 ! THIS VERSION FOR NON-WP CUSTOMERS        ! ECR *~
            * 09/13/84 ! CHANGED TO POINT TO NEW S.E.S LIBRARY    ! LDJ *~
            *          !   (SESDB) AND ACCOMODATE NEW INTDOC01.   !     *~
            * 02/08/85 ! CORRECTED WORKVOL EXTRACT - IF NOT THERE ! LDJ *~
            *          !   WILL USE SYSTEM VOLUME.  ALSO CORRECTED!     *~
            *          !   BUG IN TESTING WHETHER INTDOC01 NEEDS  !     *~
            *          !   TO BE CLOSED OR NOT.                   !     *~
            * 10/16/85 ! Modified to no longer have a hard-coded  ! LDJ *~
            *          !   SES library - now extracted from OUTLIB!     *~
            * 01/21/86 ! Stripped to nothing more than caller to  ! LDJ *~
            *          !   ONLINEWP.                              !     *~
            * 03/08/96 ! Added Ability to handle program names    ! LDJ *~
            *          ! types by CoStar following F13.           !     *~
            * 11/06/96 ! If GUI Mode display documentation in     ! LDJ *~
            *          ! MSWORD if it exists - otherwise same.    !     *~
            * 03/05/97 ! Removed calls to GUICHENV (now inside    ! LDJ *~
            *          ! CMSLINK).  WSXIO call replaced by GETCMD.!     *~
            * 03/28/97 ! Now sends commands to SWATCH (GUI mode)  ! LDJ *~
            *          ! to turn Function Key watching off & on   !     *~
            *          ! while displaying User Doc.               !     *~
            * 10/21/97 ! More Changes to get windows docs working.! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "ZMANUAL" (program$)

	dim	caller$16,                   /* CMSFIND Argument           */~
	 	code$16,                     /* Item to display doc for    */~
                file_info$108,               /* File Infomation / Location */~
                library$8,                   /* Document Library           */~
		pf16$24,                     /* CMSLINK argument           */~
	    	rlib$8,                      /* Run library for link call  */~
	    	run$8,                       /* Program to Run             */~
                rvol$6,                      /* Run volume for link call   */~
                testfile$13,                 /* Word Document file name    */~
	    	tolow$52,                    /* Trans... replace... string */~
                volume$6,                    /* Document Volume            */~
	    	userid$3,                    /* argument for CMSLINK       */~
	    	uw$1                         /* CoStar Magic Character     */


	REM *************************************************************~
    	    *                  Release Version ID Section               *~
	    *************************************************************
	    	dim cms2v$50 : goto   L01023
	    	cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
L01023: REM *************************************************************

	select #3,"USERLCMS",varc,indexed,recsize=600,keypos=1,keylen=3

	tolow$ = "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ"
	uw$ = hex(7f)

************BEGIN *******************
	code$ = program$
	tran(code$, tolow$) replacing

******* Check for GUI Specials
        gui% = -1%  /* gui% = -25% => check regardless */
	call "CHECKGUI" addr(gui%)
        if gui% = 0% then Check_Gui_2

        guy% = 0%
        call "GETCMD" (1%,guy%,run$)
        if guy% < 0% or run$ = " " then Show_Doc
            
        call "CMSLINK" addr(#3, userid$,"R",run$, rlib$, ~
                           rvol$," ","N", pf16$, caller$, ~
                           "N", comp%, return%)
        goto exit_routine

Check_Gui_2
        gui% = -25% /* gui% = -25% => check regardless */
	call "CHECKGUI" addr(gui%)
        if gui% = 0% then Old_Style

Show_Doc
        call "SENDCMD" (uw$ & "UWCPD" & "caelus/help/SeeDoc16.EXE " & code$ & ".doc" & uw$)
        goto exit_routine

Old_Style
	testfile$ = "bcgruns.dll"
	call "EXTRACT" addr("RL", library$, "RV", volume$)
		
	call "READVTOC" addr("u",volume$,library$,testfile$,~
                              file_info$, returncode%)
            if returncode%  = 0% then Show_Doc_NT

        call "PUTPARM" addr("E", 1%, "ONLINEWP", 1%, "PROGRAM ",     ~
                           code$, 8%, hex(40), return%)

	REM *** Extract File Info ***
	testfile$ = code$ & ".doc"
	volume$ = "DOCVOL"
	library$ = "cmsdocs"
			
	call "READVTOC" addr("u",volume$,library$,testfile$,~
			      file_info$, returncode%)
	if returncode% <> 0% then Show_Old

	str(file_info$,81%) = " "
	last_char% = pos(file_info$ = hex(00))
	if last_char% > 0% then str(file_info$, last_char%) = " "

	call "PUTPARM" addr("E", 1%, "INPUT   ", 1%,~
				 "FILESPEC", file_info$, 80%,~
				 hex(40), return%)


Show_Old
        close ws
        run$ = "ONLINEWP"
        call "CMSLINK" addr(#3, userid$,"R",run$, rlib$,             ~
                     rvol$," ","N", pf16$, code$, "N", comp%, return%)
	goto exit_routine

*Show_Doc_NT
	testfile$ = "bcgruns.dll"
	call "EXTRACT" addr("RL", library$, "RV", volume$)
		
	call "READVTOC" addr("u",volume$,library$,testfile$,~
                              file_info$, returncode%)
            if returncode% <> 0% then exit_routine

Show_Doc_NT
        testfile$ = code$ & ".doc"
        volume$ = "DOCVOL"
        library$ = "cmsdocs"
        
        REM *** Extract File Info ***
        call "READVTOC" addr("u",volume$,library$,testfile$,~
                              file_info$, returncode%)
             if returncode% <> 0% then Document_Not_Found

	str(file_info$,81%) = " "
	last_char% = len(file_info$) + 1%
        str(file_info$,last_char%,1%) = hex(00)
            
        call "LINK" addr(file_info$, "D", returncode%, returncode%)
	goto exit_routine

Document_Not_Found
        call "ASKGUI" (48%,"Document Not Found (" & testfile$ & ")", ~
             "Check the DOCVOL/cmsdocs setting in CMSDEFS.ini", returncode%)

        goto exit_routine            

exit_routine
            end
