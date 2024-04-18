
# @(#) $Revision: 66.1 $      

# Default user .profile file (/bin/sh initialization).

# Set up the terminal:
	eval ` tset -s -Q -m ':?hp' `
	stty erase "^H" kill "^U" intr "^C" eof "^D"
	stty hupcl ixon ixoff
	tabs

# Set up the search paths:
	PATH=$PATH:.:/cms/currel/utility:/cms/currel/object2:/cms/wisp/bin

# Set up the shell environment:
	set -u
	trap "echo 'logout'" 0

# Set up the shell variables:
	EDITOR=vi
	export EDITOR

# Set IDSI Wisp Environment
	SHELL=/bin/sh
	WISPTERM=$TERM
	WISPCONFIG=/cms/wisp/config
	export SHELL WISPTERM WISPCONFIG
#  Limit user access to help screen and help screen options
	wusage flags set HELP=N
	wusage flags set COMMANDS=N
	wusage flags set UTILS=N

# Force user into cms
	cmscontr
	logout 
