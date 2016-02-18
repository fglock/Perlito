
# $ perl perlito5.pl -I src5/lib -Cjava misc/Java/Http.pl > Main.java

use strict;

package IOException       { import => "java.io.IOException" }
package OutputStream      { import => "java.io.OutputStream" }
package InetSocketAddress { import => "java.net.InetSocketAddress" }

package HttpExchange      { import => "com.sun.net.httpserver.HttpExchange" }
package HttpHandler       { import => "com.sun.net.httpserver.HttpHandler" }
package HttpServer        { import => "com.sun.net.httpserver.HttpServer" }

package String {}

package MyHandler {
    implements => 'HttpHandler',
    'Java::inline' => " \@Override \n",
    methods => [
        handle => {
            decl => [ "public" ],               # public method
            args => [ "HttpExchange" ],         # 1 argument
            return => "void",                   # return void
            throws => [ "IOException" ],        # throws IOException
            code => "main::action",             # implemented in Perl, see below
        },
    ],
}

sub action {
    my $self = shift;
    my HttpExchange $t = $self->to_HttpExchange();
    my String $response = "This is the response";
    $t->sendResponseHeaders(200, $response->length());
    my OutputStream $os = $t->getResponseBody();
    $os->write($response->getBytes());
    $os->close();
    return;
}

my HttpServer $server = HttpServer->create(InetSocketAddress->new(8000), 0);
$server->createContext("/test", MyHandler->new());
$server->setExecutor(undef); # creates a default executor
$server->start();

__END__
public class Test {

    static class MyHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange t) throws IOException {
            String response = "This is the response";
            t.sendResponseHeaders(200, response.length());
            OutputStream os = t.getResponseBody();
            os.write(response.getBytes());
            os.close();
        }
    }

}
