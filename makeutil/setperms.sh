#!/bin/sh

(cd .. ; pwd > ./makeutil/path.txt ; cd ./makeutil)

echo 'Set Directory Permissions (Recursive)'
echo  `cat path.txt` ' [y?]'
CURRDIR=`cat path.txt`

read RESP 

if [ "$RESP" = 'y' ]
then
	echo 'working ...'
else
	exit
fi

for FILENAME in `find $CURRDIR -print`
do
    echo "Changing owner on $FILENAME"
    chown cms $FILENAME
    chgrp cmsgroup $FILENAME
    if [ -x $FILENAME ]
    then
       chmod 555 $FILENAME
    else
       if [ -f $FILENAME ]
       then
          DIRNAME=`echo "$FILENAME" | cut -d"/" -f4`
          if [ "$DIRNAME" != "sesdbase" ]
          then
            chmod 444 $FILENAME
          else
            chmod 666 $FILENAME
          fi
       fi
    fi
    if [ -d $FILENAME ]
    then
       chmod 777 $FILENAME
    fi
done
