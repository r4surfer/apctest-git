#!/usr/bin/bash
echo "enter comment"
read comment                       
echo $comment | /usr/bin/mailx -s "Comment test" dspeight@atriumwindows.com 
