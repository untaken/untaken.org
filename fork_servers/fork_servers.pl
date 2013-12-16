#!/usr/bin/env perl

=head fork_servers.pl

Copyright (c) 2013 - Luke Harwood, http://www.untaken.org

This software is free and is OpenSource, so feel free to do whatever you like with it. 
Make sure you update the blog post <link> if you do 

Little script to be able to call as many servers you like with a command. Simply 
call like:

./fork_servers.pl somehost.co.uk someotherhost.com ...

It will return the results from all the hosts prepending <host>: in front

Only requirement is that you setup SSH keys so no password is required.

See 

=cut

my @SERVERS = qw(server1 server2 server3 server4);

my $command = $ARGV[0] or die "$0 <command>\n";
my $quoted_command = quotemeta $command;
my @children;

for my $num ( 1 .. @SERVERS ) {

    $server = $SERVERS[ $num - 1 ];
    my $pid = fork();
    if ($pid) {
    
        push( @children, $pid );
    
        # parent
    }
    elsif ( $pid == 0 ) {

        # child
        warn "ssh -tq root\@$server $command\n";
        open my $cmd, '-|', qq|ssh -tq root\@$server $quoted_command|;
        while (my $line = <$cmd>) {
            print "$server:$line";
        }

       exit(0);
   }
   else {
       die "couldn’t fork: $!\n";
   }
}

foreach (@children) {
   waitpid( $_, 0 );
}

system "reset -w";
