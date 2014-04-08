sld - SSH Local Development
---------------------------

This software is free and is OpenSource, so feel free to do
whatever you like with it. Make sure you update the blog post
on http://www.untaken.org/ssh-local-development-using-sshfs/
if you do :) Also check out the post for more information on 
what this is and how it can be useful.

### Install Instructions

1. Setup some SSHFS mount points to the servers where you have
   git repositories setup.

2. Install the script to ~/bin/ and then configure the variables 
   below to your needs

3. Add the following line to your .bashrc file:

   `[[ -s "$HOME/bin/sld" ]] && source "$HOME/bin/sld"`

4. Try it out. cd to a sshfs local directory and when you
   execute either ack, prove, perl, git or find, the requests
   should go to the remote server. 

