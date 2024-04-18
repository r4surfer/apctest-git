Caelus Windows Client Software version 3.0
---------------------------------------------
Copyright (c) 1996 by Caelus, Inc.
---------------------------------------------
 

I. INTRODUCTION:
----------------
The Caelus Windows Client (CWC) consists of add-ons and modifications
to the Co*Star Windows Terminal Emulation product Co*STAR (TM) which
is manufactured by Clearview Software.  The nature of these add-ons and
modifications are to enhance the usability and appearance of the
Caelus Management Systems UNIX host based product when accessed via
a personal computer running Microsoft Windows (TM).  These add-ons
work in conjunction with a new version of the Caelus Menu Presentation
program (CMSCONTR) which runs on the host.

The Installation instructions are covered in more detail in the CWC
User Manual.

II. INSTALLATION INSTRUCTIONS - FILE SERVER INSTALL:
----------------------------------------------------
If you purchased the file server version of Co*STAR you need to first
install Co*STAR and CWC on your file server.  The basic steps are:
1.  Create the server directory and share/export it.
2.  From either the server or a PC connected to the server install
    Co*STAR from the Co*STAR installation media onto the server 
    directory / network drive created in step 1 (Run Install.exe from 
    the diskette drive).
3.  From either the server or a PC connected to the server install the
    CWC server resident software from the CWC
    installation media onto the same server directory / network drive
    that Co*STAR was installed to (run setup.exe from install disk 1).
    When prompted select 'Network Server Install' on the 'Select
    Components to Install' screen.

IIIA. INSTALLATION INSTRUCTIONS - NETWORK CLIENT:
-------------------------------------------------
These instructions are for PCs connected to a LAN that will be 
running the Co*STAR/CWC software off a file server:
1.  Install Co*Star first.  From the PC, run install.exe on the
    Costar network drive / server directory created in section II
    above.  This will create a Costar directory on the local PC with
    a few files in it and a CoSTAR Program Group.
2.  Install CWC.  From the PC, run setup.exe off the CWC install
    disk 1.  When prompted select 'Network Client Install' on the 'Select
    Components to Install' screen. When prompted enter the same 
    directory that was used for the Co*STAR installation.
     
IIIB.  INSTALLATION INSTRUCTIONS - STAND-ALONE LAN OR DIALUP INSTALL:
---------------------------------------------------------------------
These instructions are for PCs that are connected to a LAN or which
will be dialing into the UNIX Host but DO NOT run the Co*STAR/CWC 
software off a file server.  All Co*STAR and CWC Software is
installed on the local PC.    
1.  First install Co*STAR from the installation media provided
    (run a:\setup.exe). It is recommended but not required that 
    you use the default directory COSTAR when prompted. 
2.  After installing CO*STAR insert the CWC installation disk 1
    and run the SETUP.EXE program located on the disk.
3.  When prompted select either 'Stand-Alone PC Install' or 
    'Dial-Up Install' on the 'Select Components to Install' screen
    depending on whether you will be connecting to the UNIX Host
    over a LAN or a modem.   When prompted enter the same 
    directory that was used for the Co*STAR installation.

IV.  INSTALLATION INSTRUCTIONS - CLEANUP:
-----------------------------------------
Perform these steps after the installation of the Caelus Windows
Client components is completed on a PC.
1.  Delete the Co*STAR Menu Group (you may want to move the readme
    and help file icons to the Caelus Windows Client Group first).
2.  Run CWC and make any terminal/connection/display setting changes
    needed.  Note:  if screen resolution is 640 x 480 or 800 x 600
    then the recommended Screen Display Font Size is 14, for higher
    resolutions we recommend Screen Display Font Size 20.  
    In all cases we recommended that you use the Costar font.
3.  The run command for costar should look something like this:
    Stand-Alone PC: c:\costar\costar.exe c:\costar\caelus\cmscontr.cnf
    Network Client: e:\costar.exe c:\costar\caelus\cmscontr.cnf
    	
V. INSTALLATION INSTRUCTIONS - UNIX HOST:
-------------------------------------------
Prerequisites:  CMS R6.04.03 or greater installed on your system. 

The costar videocap file must be installed in the wisp videocap 
directory (/cms/wisp/config/videocap/).

Directions for Individual User Setup/Configuration:

After you connect to your UNIX Host System you MUST do the
following in order to use the CMS Windows Client.

1.  Set Terminal Type (TERM) = vt220 and WISPTERM = costar
    (can be set in .profile or .login script)
2.  Run wshell and configure the terminal's psuedo blank
    rendition to be reverse video (R) and SAVE the terminal
    configuration.

You are now ready to run CMSCONTR from the CO*STAR/Caelus Windows
Client.


VI. KEYBOARD MAPPING:
---------------------
The following keys have had specific mappings assigned to them:

		CMS Function	PC Keyboard Equivalent
		------------	----------------------
Function Keys:	PF  1 - 12	F1 - F12
		PF 11 - 22	SHIFT F1 - F12
		PF 21 - 32	CONTROL F1 - F12

Miscellaneous:	ERASE key	END
		TAB		TAB
		BACKTAB		SHIFT TAB
		HOME		HOME
		
		VCQ Function	PC Keyboard Equivalent
		------------	----------------------
Function Keys:	F1 - F10	F1 - F10
---------------------------------------------------------------------

REVISION HISTORY
----------------
Changes in 3.0
	- Added New Menu System / Presentation called System Navigator.
	  Menus now located on file server or local PC.
	  No longer uses CMSCONTR on host - now uses CMSGUI 
	  (users may still opt to use CMSCONTR).
	  Menu items may now include MS Windows objects if using CMSGUI.
	- New GUI Logon application.
	- ListBox selection for GENCODES fields now automatically gets
	  focus when displayed - no longer have to use mouse to click
	  on contents of listbox.
	- Improvements to SWATCH.EXE (transform Function Keys to
	  Buttons).  Now faster, takes up less resources.
	- Change to HNYVLGUI and WCGRAPH2 - apps now displayed as
	  children of the Costar Window.
	- Changed CMSCONTR.HSF (hot spots file) to longer use Auto
	  Push Button feature - this was conflicting with SWATCH.EXE.
	- Includes new File Transfer utility - NAVFTP.EXE.
	- Added ability to view Caelus Documentation in MS Word Format
	  (documents must be located under costar/caelus/help and
	  WORDVIEW.EXE must be installed and locatable via PATH).

Changes in 2.1.0
	- Made corrections to the installation process

Changes in 2.0.5
	- Made corrections to Server Install Process
	- Color.ini now installed into ./costar directory
	- Revisions to readme.txt
        - Installation now included guinotes.doc

Changes in 2.0.4
	- Added Dialup Configuration to Setup Process
	- Other, minor changes to setup program.
	- Fixed bug in SWATCH.EXE - function key handling again
          (would blowup on 2nd screen in PIPSCAN).

Changes in 2.0.3
	- Installation Process Rewritten
	  - now supports 3 configurations
	    - Stand-Alone
	    - Server Install
	    - Server Client Install
	  - now asks for and sets Network host and IP address
	  - other minor changes
        - Minor changes to SWATCH.EXE to correct some function key
	  mishandling / bugs.

Changes in 2.0:
	- Changed PullDown Menus:
	  - If user not a database administrator then
	    following menu options removed:
	    -  Configure Terminal
	    -  Edit Templates
	    -  Edit Hotspots
	    -  Edit Toolbar
	    -  Save As
	    -  Open
	  - If Direct Run ability not present then the following
	    menu items are removed:
	    -  Send Kermit, Receive Kermit
	    -  Send Xmodem, Receive Xmodem
	    -  Kermit Packets, Kermit Protocol
	  - The following menu items are added under a new Options
	    pulldown menu"
	    -  Shell - go to Command Processor Shell (Icon preferred)
	    -  Hide & Restore Status bar at Bottom of window
	    -  Hide and Restore Caption bar at top of Window
	- Support for CoStar 1.4beta added
	- Windows 95 support added
	- New CMSUSRIN options that control use of Icons or Function
	  key buttons on menus; Whether or not to display Wallpaper
	  background on program screens.
	- Now displays last logon date/time in status bar
	  when first entering CMS.
	- Program function keys display as Command Buttons.  Users
	  may click on buttons or press corresponding function key.
	- Various dialog boxes now windows/gui standard (StartOver,
	  documentation or menus prompt, various startup error 
	  messages, ...).
	- Better support for Command Processor Shell, ILPMAN, and 
	  display when invoked/run from the menu.
	- Temporary Status Messages (SHOSTAT) now drawn with 3D box
	  around them (note - does not display well on slow 
	  connections)
	- Standard Caelus PICK functions (GETCODE/PLOWCODE) modified
	  appearance and use.
	- Data fields validated against a GENCODES table will now 
	  see a list box appear for selection of a valid code rather
	  than a Caelus PICK/PLOWCODE screen.  (Note:  must use the
	  mouse to select an entry :( )
	- New Caelus VISUAL LINK VB apps introduced.
 
------------------------------------------------------------- 

V. KNOWN ANOMALIES (GUI Mode Only):
-----------------------------------
1.  ANOMALY: When returning from the WISP shell to the CMSCONTR menu
    screen the menu pick icons are gone.
    RESOLUTION:  No fix to this problem exists yet.  Workaround is to 
    press function key 20 (Shift F10) which cause the icons to be re-
    painted on the screen.

2.  ANOMALY:  Error messages displayed during the logon process may be
    super-imposed over the Caelus Logo picture and are therefore 
    harder to read.
    RESOLUTION:  To be resolved in a future release.


Report any other bugs - oops, I mean anomalies - 
to Caelus Customer Support @ (509) 455-8566.
---------------------------------------------------------------------
05/13/96  Larry Jones