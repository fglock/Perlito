
# use Perlito5::Grammar;
use Perlito5::Eval;
use Perlito5::Dumper;
use strict;

BEGIN { say "compiling1" }

my $source = <<'SRC';
    print 1 + 3, "\n";
    sub f {
        # my $x;
        print "HERE\n";
    }
    f();
SRC

my $m;
my $ok;
say "compiling: <<< $source >>>";
eval {
    $m = Perlito5::Grammar->exp_stmts( $source, 0 );
    $ok = 1;
};
if (  !$ok
    || $m->{to} != length($source) )
{
    my $error = $@
      || ( $m->{to} != length($source)
        && "Syntax Error near " . $m->{to} )
      || "Unknown error";
    warn $error;
    exit(255);
}
my $comp_units;
if ($expand_use) {
    my $ok;
    eval {
        $comp_units =
          Perlito5::Grammar::Use::add_comp_unit( Perlito5::Match::flat($m) );
        $ok = 1;
    };
    if ( !$ok ) {
        my $error = $@
          || "Unknown error loading a module";
        warn $error;
        exit(255);
    }
}
else {
    $comp_units = Perlito5::Match::flat($m);
}

my $comp_unit = Perlito5::AST::CompUnit->new(
    name => 'main',
    body => $comp_units,
);

say Perlito5::Dumper::Dumper($comp_unit);

my $env = [
    {
        'infix:<+>' => sub {
            my ( $env, $args ) = @_;
            $args->[0]->eval($env) + $args->[1]->eval($env);
        },
        'print' => sub {
            my ( $env, $args ) = @_;
            print $_->eval($env) for @$args;
        }
    },
];

my $result = $comp_unit->eval($env);

say Perlito5::Dumper::Dumper($result);

