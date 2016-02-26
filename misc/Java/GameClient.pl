# Run Gameserver.pl before running this client
#perl perlito5.pl -Isrc5/lib -Cjava misc/Java/GameClient.pl > Main.java && javac Main.java && java Main

use strict;
use feature 'say';

package BufferedReader     { import => "java.io.BufferedReader" }
package IOException        { import => "java.io.IOException" }
package InputStream        { import => "java.io.InputStream" }
package InputStreamReader  { import => "java.io.InputStreamReader" }
package OutputStream       { import => "java.io.OutputStream" }
package PrintWriter        { import => "java.io.PrintWriter" }
package Socket             { import => "java.net.Socket" }
package ServerSocket       { import => "java.net.ServerSocket" }
package System             { import => "java.lang.System"}

package String {}

eval {
    my String $host = "localhost";
    my Socket $socket = Socket->new("localhost",2001);
    my InputStream $is = $socket->getInputStream();
    my OutputStream $os = $socket->getOutputStream();
    my InputStreamReader $inReaderSocket = InputStreamReader->new($is);
    my BufferedReader $brSocket = BufferedReader->new($inReaderSocket);

    my InputStreamReader $inReaderSystem = InputStreamReader->new(System->in);
    my BufferedReader $brSystem = BufferedReader->new($inReaderSystem);

    my PrintWriter $pw = PrintWriter->new($os,Boolean->TRUE);
    while ( Boolean->TRUE ) {
      $pw->println($brSystem->readLine());
      my $message = $brSocket->readLine();
      print "$message\n";
    }
} or die $@;