# pX/Common/iterator_engine_p6rule.pl - fglock
#
# experimental implementation of p6-regex parser
#
# see: iterator_engine_README

# TODO $var := (capture)

use strict;
use warnings;

use Data::Dumper;

require 'iterator_engine.pl';
require 'iterator_engine_p6rule_lib.pl';

my $namespace = 'grammar1::';

{
  package grammar1;

  use Data::Dumper;
  no warnings 'once';

sub subrule {
    my ( $code, $tail ) = $_[0] =~ /^\<(.*?)\>(.*)$/s;
    return unless defined $code;
    #print "parsing subrule $code\n";
    return { 
        bool  => 1,
        match => { code => $code },
        tail  => $tail,
        capture => [ { subrule => $code } ],
    }
}

# XXX - compile non_capturing_subrule using a rule
# XXX - set non-capture flag
sub non_capturing_subrule {
    my ( $code, $tail ) = $_[0] =~ /^\<\?(.*?)\>(.*)$/s;
    return unless defined $code;
    # print "non_capturing_subrule $code - $1\n";
    return { 
        bool  => 1,
        match => { code => $code },
        tail  => $tail,
        capture => [ { non_capturing_subrule => $code } ],
    }
}

# XXX - compile negated_subrule using a rule
sub negated_subrule {
    my ( $code, $tail ) = $_[0] =~ /^\<\!(.*?)\>(.*)$/s;
    return unless defined $code;
    # print "negated_subrule $code - $1\n";
    return { 
        bool  => 1,
        match => { code => $code },
        tail  => $tail,
        capture => [ { negated_subrule => $code } ],
    }
}

*capturing_group = 
    ruleop::concat(
        ruleop::constant( '(' ),
        ruleop::capture( 'capturing_group',
            \&rule,
        ),
        ruleop::constant( ')' )
    );

*dot = 
    ruleop::capture( 'dot', 
        ruleop::constant( '.' ),
    );

# <'literal'> literal \*
my @literals = (
    ruleop::concat(    
        ruleop::constant( "<\'" ),
        ruleop::capture( 'literal',
            ruleop::non_greedy_star( \&any ),
        ),
        ruleop::constant( "\'>" ),
    ),
    ruleop::capture( 'literal', \&word ),
    ruleop::capture( 'literal', \&escaped_char )
);

use vars qw( @rule_terms );
@rule_terms = (
    \&capturing_group,
    @literals,
    \&negated_subrule,
    \&non_capturing_subrule,
    \&subrule,
    \&dot,
    # more items are pushed later - see below 
);

# <ws>* [ <closure> | <subrule> | ... ]
*term = 
    ruleop::concat(
        \&ws_star,
        ruleop::alternation( \@rule_terms ),
        \&ws_star,
    );

# XXX - allow whitespace everywhere

# [ <term>[\*|\+] | <term> 
# note: <term>\* creates a term named 'star'
*quantifier = 
    ruleop::alternation( [
        ruleop::capture( 'star', 
            ruleop::concat(
                ruleop::capture( 'term', \&term ),
                ruleop::capture( 'literal',
                    ruleop::alternation( [
                        ruleop::constant( '??' ),
                        ruleop::constant( '?' ),
                        ruleop::constant( '*?' ),
                        ruleop::constant( '+?' ),
                        ruleop::constant( '*' ),
                        ruleop::constant( '+' ),
                    ] ),
                ),
                \&ws_star,
            ),
        ),
        \&term,
    ] );

# [ <term> [ \| <term> ]+ | <term> ]* 
# note: <term>|<term> creates a term named 'alt'
# XXX - 'alt' position is wrong
*rule = 
    ruleop::greedy_star (
        ruleop::alternation( 
          [
            ruleop::capture( 'alt', 
                ruleop::concat(
                    ruleop::capture( 'term', \&quantifier ),
                    ruleop::greedy_plus(
                        ruleop::concat(
                            ruleop::constant( '|' ),
                            ruleop::capture( 'term', \&quantifier ),
                        ),
                    ),
                ),
            ),                
            \&quantifier,
          ]
        ),
    );

# [<rule>]
*non_capturing_group = ::compile_rule( ' \[ <rule> \] ', {print_program=>0} );
push @rule_terms, \&non_capturing_group;

# { code }
    # p5 code is called using: "rule { xyz { v5; ... } }" (audreyt on #perl6)
    #   (but TimToady said it's not)
    # or: "rule { ... [:perl5:: this is p5 ] ... }"
    # or: "[:perl5(1) this is perl5 ]" (putter on #perl6)
    
# XXX - this is veeery slow - the actual implementation is in file
#       p6rule_lib.pl and uses Text::Balanced
# *code = ::compile_rule( '\{[<code>|.]*?\}' );
unshift @rule_terms, ruleop::capture( 
    'closure', \&code );
    
# $var
# -- ident, variable moved to p6rule_lib.pl
#*ident =    ::compile_rule( '[[\:\:]?<word>]+', {print_ast=>0} );
#*variable = ::compile_rule( '[\$|\%|\@]<ident>' );
#push @rule_terms, ruleop::capture( 'variable',\&variable );
unshift @rule_terms, ruleop::capture( 
    'variable', \&variable );
    #ruleop::wrap( { 
    #        before => sub { print "matching variable: $_[0]\n" },
    #        after  => sub { $_[0]->{bool} ? print "variable matched\n" : print "no variable match\n" },
    #    },
    #    \&variable
    #);

# <@var> is a run-time alternation (TimToady on #perl6)
*runtime_alternation = ::compile_rule( 
    '\< <variable> \>' );
unshift @rule_terms, ruleop::capture( 
    'runtime_alternation',\&runtime_alternation );

# $xxx := (capture)
*named_capture = ::compile_rule( 
    '\$ <ident> <?ws>? \:\= <?ws>? \( <rule> \)' );  # XXX - accept % @
unshift @rule_terms, ruleop::capture( 
    'named_capture',\&named_capture );

}

#------ match functions

=for reference - see S05 "Return values from matches"
    $/    
        the match 
        Inside a closure, refers to the current match, even if there is another
        match inside the closure
    $/()  
        just the capture - what's returned in { return ... }
    $/0
        the first submatch
Alternate names:        
    $()   
        the same as $/()
Submatches:
    $/[0] 
        the first submatch
    $0 
        the same as $/[0]
    $()<a>
        If the capture object return()-ed were a hash:
        the value $x in { return { a => $x ,} }
Named captures:
    $a := (.*?)  
        $a is something in the outer lexical scope (TimToady on #perl6)
            XXX - E05 says it is a hypothetical variable, referred as $0<a>
    $<a> := (.*?)  
        $<a> is a named capture in the match
    let $a := (.*?)
        $a is a hypothetical variable
                (\d+)     # Match and capture one-or-more digits
                { let $digits := $1 }
=cut

sub match::get {
    my $match =   $_[0];
    my $name =    $_[1];
    
    return $match->{capture}  if $name eq '$/()' || $name eq '$()';
    
    # XXX - wrong, but bug-compatible with previous versions
    return $match->{capture}  if $name eq '$/<>' || $name eq '$<>';
    
    return $match->{match}    if $name eq '$/';
    
    #print "get var $name\n";
    
    # XXX - wrong, but bug-compatible with previous versions
    # $/<0>, $/<1>
    # $<0>, $<1>
    if ( $name =~ /^ \$ \/? <(\d+)> $/x ) {
        my $num = $1;
        my $n = 0;
        for ( @{ match::get( $match, '$/()' ) } ) {
            next unless ref($_) eq 'HASH';
            if ( $num == $n ) {
                my (undef, $capture) = each %$_;
                #print "cap\n", Dumper( $capture );
                return $capture;
            }
            $n++;
        }
        return;
    }
    
    # $/()<name>
    # $()<name>
    if ( $name =~ /^ \$ \/? \( \) <(.+)> $/x ) {
        my $n = $1;
        for ( @{ match::get( $match, '$/()' ) } ) {
            next unless ref($_) eq 'HASH';
            if ( exists $_->{$n} ) {
                #print "cap\n", Dumper( $_->{$n} );
                return $_->{$n};
            }
        }
        return;
    }

    # XXX - wrong, but bug-compatible with previous versions
    # $/<name>
    # $<name>
    if ( $name =~ /^ \$ \/? <(.+)> $/x ) {
        my $n = $1;
        for ( @{ match::get( $match, '$()' ) } ) {
            next unless ref($_) eq 'HASH';
            if ( exists $_->{$n} ) {
                #print "cap\n", Dumper( $_->{$n} );
                return $_->{$n};
            }
        }
        return;
    }

    die "match variable $name is not implemented";
    # die "no submatch like $name in " . Dumper( $match->{match} );
}

sub match::str {
    my $match = $_[0];
    #print "STR: ", ref( $match ), " ", Dumper( $match ), "\n";
    return join( '', map { match::str( $_ ) } @$match )
        if ref( $match ) eq 'ARRAY';
    return join( '', map { match::str( $_ ) } values %$match )
        if ref( $match ) eq 'HASH';
    return $match;
}

#------ rule emitter

# compile_rule( $source, {flag=>value} );
#
# flags:
#   print_program=>1 - prints the generated program
#
sub compile_rule {
    local $Data::Dumper::Indent = 1;
    #print "compile_rule: $_[0]\n";
    my $match = grammar1::rule->( $_[0] );
    my $flags = $_[1];
    print "ast:\n", Dumper( $match->{capture} ) if $flags->{print_ast};
    die "syntax error in rule '$_[0]' at '" . $match->{tail} . "'\n"
        if $match->{tail};
    die "syntax error in rule '$_[0]'\n"
        unless $match->{bool};
    my $program = main::emit_rule( $match->{capture} );
    print "generated rule:\n$program" if $flags->{print_program};
    my $code = eval($program); die $@ if $@;
    return $code;
}

sub emit_rule {
    my $n = $_[0];
    my $tab = $_[1] || '    '; 
    $tab .= '  ';
    local $Data::Dumper::Indent = 0;
    #print "emit_rule: ", ref($n)," ",Dumper( $n ), "\n";

    # XXX - not all nodes are actually used

    if ( ref($n) eq '' ) {
        # XXX - this should not happen, but it does
        return '';
    }
    if ( ref( $n ) eq 'ARRAY' ) {
        my @s;
        for ( @$n ) {
            #print "emitting array item\n";
            my $tmp = emit_rule( $_, $tab );
            push @s, $tmp . "$tab ,\n" if $tmp;
        }

        # XXX XXX XXX - source-filter 
        #    temporary hacks to translate p6 to p5 -- see 'closure' node
        if (@s && $s[-1] =~ /^#return-block#(.*)/s ) {
            #print "return block\n";
            my $code = $1;
            #print "Code: $code\n";
            pop @s;
            my $program;
            if ( @s == 1 ) {
                $program = $s[0];
            }
            else {
                $program = "$tab ruleop::concat(\n" . 
                            ( join '', @s ) . 
                            "$tab )\n";
            }
            #print "program $program\n";
            my $return;
            $return = "
    sub { 
        my \$rule = \n$program    ;
        my \$match = \$rule->( \@_ );
        return unless \$match;
        my \$capture_block = sub " . $code . "; 
        #use Data::Dumper;
        #print \"capture was: \", Dumper( \$match->{capture} );
        return { 
            \%\$match,
            capture => [ \$capture_block->( \$match ) ],
        }; 
    }\n";
            return $return;
        }
        return $s[0] if @s == 1;
        return "$tab ruleop::concat(\n" . 
               ( join '', @s ) . 
               "$tab )\n";
    }
    elsif ( ref( $n ) eq 'HASH' ) 
    {
        my ( $k, $v ) = each %$n;
        # print "$tab $k => $v \n";
        return '' unless defined $v;  # XXX a bug?
        if ( $k eq 'rule' ) {
            return emit_rule( $v, $tab );
            
            #return "$tab ruleop::capture( '$k',\n" .
            #       emit_rule( $v, $tab ) . "$tab )\n";
        }        
        elsif ( $k eq 'capturing_group' ) {
            return "$tab ruleop::capture( '$k',\n" .
                   emit_rule( $v, $tab ) . "$tab )\n";
        }        
        elsif ( $k eq 'non_capturing_group' ) {
            return emit_rule( $v, $tab );
        }        
        elsif ( $k eq 'star' ) {
            local $Data::Dumper::Indent = 1;
            my $term = $v->[0]{'term'};
            #print "*** \$term:\n",Dumper $term;
            my $quantifier = $v->[1]{'literal'}[0];
            my $sub = { 
                    '*' =>'greedy_star',     
                    '+' =>'greedy_plus',
                    '*?'=>'non_greedy_star', 
                    '+?'=>'non_greedy_plus',
                    '?' =>'optional',
                    '??' =>'null_or_optional',
                }->{$quantifier};
            # print "*** \$quantifier:\n",Dumper $quantifier;
            die "quantifier not implemented: $quantifier" 
                unless $sub;
            return "$tab ruleop::$sub(\n" .
                   emit_rule( $term, $tab ) . "$tab )\n";
        }        
        elsif ( $k eq 'alt' ) {
            # local $Data::Dumper::Indent = 1;
            # print "*** \$v:\n",Dumper $v;
            my @s;
            for ( @$v ) { 
                my $tmp = emit_rule( $_, $tab );
                push @s, $tmp if $tmp;   
            }
            return "$tab ruleop::alternation( [\n" . 
                   join( '', @s ) .
                   "$tab ] )\n";
        }        
        elsif ( $k eq 'term' ) {
            return emit_rule( $v, $tab );
        }        
        elsif ( $k eq 'code' ) {
            # return "$tab # XXX code - compile '$v' ?\n";
            return "$tab $v  # XXX - code\n";  
        }        
        elsif ( $k eq 'dot' ) {
            return "$tab \\&{'${namespace}any'}\n";
        }
        elsif ( $k eq 'subrule' ) {
            #print Dumper $v;
            my $name = $v;
            $name = $namespace . $v unless $v =~ /::/;
            return "$tab ruleop::capture( '$v', \\&{'$name'} )\n";
        }
        elsif ( $k eq 'non_capturing_subrule' ) {
            #print Dumper $v;
            return "$tab \\&{'$namespace$v'}\n";
        }
        elsif ( $k eq 'negated_subrule' ) {
            #print Dumper $v;
            return "$tab ruleop::negate( \\&{'$namespace$v'} )\n";
        }
        elsif ( $k eq 'literal' ) {
            #print "literal:", Dumper($v);
            my $name = quotemeta(join('',@$v));
            #print "literal: $name\n";
            return "$tab ruleop::constant( \"$name\" )\n";
        }
        elsif ( $k eq 'variable' ) {
            #print "variable:", Dumper($v);
            my $name = match::str( $v );
            # print "var name: ", match::str( $v ), "\n";
            my $value = "sub { die 'not implemented: $name' }\n";
            $value = eval $name if $name =~ /^\$/;
            $value = join('', eval $name) if $name =~ /^\@/;

            # XXX - what hash/code interpolate to?
            # $value = join('', eval $name) if $name =~ /^\%/;

            return "$tab ruleop::constant( '" . $value . "' )\n";
        }
        elsif ( $k eq 'closure' ) {
            #print "closure: ", Dumper( $v );
            my $code = match::str( $v ); 
            
            # XXX XXX XXX - source-filter - temporary hacks to translate p6 to p5
            # $()<name>
            $code =~ s/ ([^']) \$ \( \) < (.*?) > /$1 match::get( \$_[0], '\$()<$2>' ) /sgx;
            # $<name>
            $code =~ s/ ([^']) \$ < (.*?) > /$1 match::get( \$_[0], '\$<$2>' ) /sgx;
            # $()
            $code =~ s/ ([^']) \$ \( \) /$1 match::get( \$_[0], '\$()' ) /sgx;
            #print "Code: $code\n";
            
            return "$tab sub {\n" . 
                   "$tab     $code;\n" . 
                   "$tab     return { bool => 1, tail => \$_[0] } }\n"
                unless $code =~ /return/;
            return "#return-block#" . $code;
        }
        elsif ( $k eq 'runtime_alternation' ) {
            my $code = match::str( match::get( 
                { capture => $v }, 
                '$<variable>'
            ) );
            return "$tab ruleop::alternation( \\$code )\n";
        }
        elsif ( $k eq 'named_capture' ) {
            my $name = match::str( match::get( 
                { capture => $v }, 
                '$<ident>'
            ) );
            my $program = emit_rule(
                    match::get( 
                        { capture => $v }, 
                        '$<rule>'
                    ), $tab );
            return "$tab ruleop::capture( '$name', \n" . $program . "$tab )\n";
        }
        else {
            die "unknown node: ", Dumper( $n );
        }
    }
    else 
    {
        die "unknown node: ", Dumper( $n );
    }
}

1;


__END__

random notes...

<fglock> audreyt: what is the relationship between AST and Match? (I'm compiling 
the match captures)
<audreyt> fglock: no relationship whatsoever :)
<audreyt> fglock: the Match object may carry an captured object in $/<>
<audreyt> aka $()
<audreyt> and if you are writing a Perl 6 parser, then that capture object may be 
an AST object
<audreyt> you can set the capture object by
<audreyt> rule { $() := ... }
<audreyt> or
<audreyt> rule { ... { return $capture_object } }
<audreyt> or
<audreyt> rule { ... { $() := ... } }
<audreyt> if the capture object is not set explicitly
<audreyt> then it's set to the entire match as matched by the rule.
<spinclad> so in q:code:{ say $x; {{{$a}}} } the $x is literal but the $a is 
unquoted (interpolated)? therefore the {{{ }}}'s?
<audreyt> +$/ and ~$/ resolves to +$() and ~$() respectively.

----

re: '&'

22:16 < putter> [<mail_header>&.*?^^From: (\N+).*] $<body>:=(.*)
22:20 < putter> PerlJam: what's an increased specificity example?  both 
of mine
                were assert-spec&unpack pairs.
22:22 < fglock> putter: .* would make it fail, isn't it?
22:23 < putter> you mean the .* after the (\N+) ?
22:23 < fglock> yes
22:23 < PerlJam> putter: yes, mine are similar, just that the "unpack" 
part
                 isn't necessary:    [<mail_header>&^^<'From:'>] &&
                 do_something_only_with_from_lines;
22:23 < putter> I don't believe so.  I would expect the & to force the * 
to
                backtrack.
22:24 < PerlJam> putter: btw, beware the cut-colon!  :-)  (unless I'm 
mistaken
                 that you meant for the : to be matched)

-----

re: statement_control

22:50 < putter> one uses  multi statement_control:<if> (...){...}  multi
                statement_control:<while> (...){...} etc to fill it in.
22:51 < fglock> putter: you mean, statement_control is represented by an 
array?
                (or namespace thing)
22:52 -!- pdcawley [n=pdcawley@adsl-217.146.110.1.merula.net] has joined 
#perl6
22:52 < putter> fglock: yeah, but there are some issues... like how do  
(pause)
22:52 -!- clkao [n=clkao@220-132-58-30.HINET-IP.hinet.net] has quit 
[Read
          error: 104 (Connection reset by peer)]
22:52 < PerlJam> must not have been enough svk talk here ;-)
22:52 < fglock> putter: please not that it is an array of rule - it is 
very
                flexible
22:53 < putter> statement_control is a grammatical category.  it defines 
one of
                the <statement> subrules.
22:53 < putter> (my yeah, was directed at <@subrule>, not represented by 
an
                array ;)
22:55 -!- r0n|mess [n=r0nny@p54B893E3.dip0.t-ipconnect.de] has quit 
[Connection
          timed out]
22:57 < putter> fglock: yes but.  when writing a grammar, in a first 
match wins
                engine (like | is), you carefully craft the order of the
                subrule list.  when subrules get added by 
statement_control
                defs, someone other than the human has to do the 
crafting.
                either the statement_control infrastructure assures the 
@array
                has a nice order, or <statement> can't use <@array>.
22:58 < fglock> putter: you can opt to use longest-match instead of
                ordered-match
22:58 < putter> yes
22:58 < fglock> that would be <%statement_control>
22:59 < putter> and the real parser can play games like trying to 
massage the
                @array into a trie, so it doesnt have to repeatedly 
reparse the
                same stuff the same way.
22:59 < fglock> putter: it is cached

23:02 < putter> re statement_control,
                http://dev.perl.org/perl6/doc/design/syn/S04.html has a 
little
                bit in Statement parsing.

----

re: Smart::Comments
re: defining 'if' with macros

23:08 < putter> re hash, "An interpolated hash matches the longest 
possible key
                of the hash as a literal, or fails if no key matches.", 
which
                doesnt help you distinguish between  /if <expr> <block> 
[else
                <block>]?/ and /if <expr> <block> [else <block>]?
                [wrap_around_both_branches <mumble>]?/.   comparing "if" 
and
                "if" isnt going to help.
23:09 < pmurias> Does any one think that useing Smart::Comments in
                 iterator_engine.pl would be a good idea?
23:09 < fglock> pmurias: I like Smart::Comments, but I'd like to keep it 
simple
                (that is, no unnecessary dependencies)
23:09 < putter> (there shouldn't have been a ? on the <mumble> clause)
23:10 < pmurias> It would be a dependency only for debugging :)
23:10 < pmurias> And casual users don't do that often :)
23:11 < pmurias> at least they shouldn't have to :)
23:11 < fglock> pmurias: I'll check that (you mean, disable 'use
                Smart::Comments' when not in use?)
23:11 < pmurias> the use line should be comment out by default
23:12 < fglock> putter: re /if.../ - I don't understand the mumble part, 
what
                would it be?
23:12 < pmurias> and if the debuging messages are needed you just delete 
the #
23:12 < putter> re if macro, well, you need to add a regex to 
<statement> so
                you can parse it.  and we can currently hang regexs off 
of,
                well, rule, and macros     macro statement_control:<if> 
(...)
                is parsed(/heres the regex/) {...}
23:12 < pmurias> i'll commit it tommorow if youd don't mind
23:12 < fglock> pmurias: sounds good - I'll check the pod again


23:36 < fglock> it will look like: %statement_control:<if> = rule { ... 
}

