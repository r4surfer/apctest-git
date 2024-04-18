
# @(#) $Revision: 64.2 $     


# Default user .login file ( /bin/csh initialization )

# Set up the default search paths:
set path=(. /bin /usr/bin /usr/contrib/bin /usr/local/bin /cms/currel/object2 /cms/currel/utility /cms/wisp/bin /cms/disam/bin)

#set up the terminal
eval `tset -s -Q -m ':?hp' `
stty erase "^H" kill "^U" intr "^C" eof "^D" susp "^Z" hupcl ixon ixoff tostop
tabs	

# Set up shell environment:
set noclobber
set history=20
umask 000

# Set Shared Library Path:
#setenv SHLIB_PATH /cms/currel/load1

# Set IDSI Wisp environments
setenv SHELL /usr/bin/csh
setenv WISPGID $$
setenv WISPTERM $TERM
setenv WISPCONFIG /cms/wisp/config
wusage read
# limit user access to help screen and help screen options
#wusage flags set COMMANDS=N
#wusage flags set UTILS=N
#wusage flags set HELP=N
# Force user into cms
exec cmscontr
clear
logout

