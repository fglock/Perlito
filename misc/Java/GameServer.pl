# perl perlito5.pl -Isrc5/lib -Cjava misc/Java/GameClient.pl > Main.java && javac Main.java && java Main
# Run GameClient.pl after this

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

package String {}
package Boolean {}
package Integer {}

eval {
    my ServerSocket $server = ServerSocket->new(2001);
    my Socket $socket = $server->accept();
    my InputStream $is = $socket->getInputStream();
    my OutputStream $os = $socket->getOutputStream();
    my InputStreamReader $inReader = InputStreamReader->new($is);
    my BufferedReader $br = BufferedReader->new($inReader);
    my PrintWriter $pw = PrintWriter->new($os,Boolean->TRUE);
    my $num = 55;
    my $guessedNum = 0;
    my $start = Boolean->TRUE;
    print "The game begins ....";
    while ( Boolean->TRUE ) {
      my String $messageClient = $br->readLine();
      my $message = $messageClient;
      my $op = substr($message, 0, 1);
      print "\nMessage from client: $message";
      if ($op eq "<" || $op eq ">" || $op eq "=" ){
         $guessedNum = Integer->parseInt($messageClient->substring(1));
         if ($op eq "<"){
            if ($guessedNum > $num){
              $pw->println("Yes TRUE");
            } else {
              $pw->println("Dude No!");
            }
         } elsif ( $op eq ">"){
            if ($guessedNum < $num){
              $pw->println("Yupp TRUE");
            } else {
              $pw->println("No Man!");
            }
         } else {
            if($guessedNum == 55 ) {
                $pw->println("Finally you guessed it");
            } else {
                $pw->println("Keep Trying...");
            }
          }
      }else {
        $pw->println("Guess your number between 1 to 100 ..... like '>50' or '<10' or '=91' ");
      }
    }
} or die $@;