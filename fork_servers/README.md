fork_servers.pl
------------------------------
Copyright (c) 2013 - Luke Harwood, http://www.untaken.org

This software is free and is OpenSource, so feel free to do whatever you like with it.
Make sure you update the blog post http://www.untaken.org/fork-command-to-servers-via-ssh
if you do.

Little script to be able to call as many servers you like with a command. Simply
call like:

./fork_servers.pl somehost.co.uk someotherhost.com ...

It will return the results from all the hosts prepending <host>: in front.

Only requirement is that you setup SSH keys so no password is required.
