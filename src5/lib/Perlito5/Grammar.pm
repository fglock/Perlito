package Perlito5::Grammar;

use Perlito5::Expression;
use Perlito5::Grammar::Control;
use Perlito5::Grammar::String;
use Perlito5::Grammar::Sigil;
use Perlito5::Grammar::Use;
use Perlito5::Grammar::Block;
use Perlito5::Grammar::Space;

sub word {
    substr( $_[1], $_[2], 1 ) =~ m/\w/
    ? {
        str  => $_[1],
        from => $_[2],
        to   => $_[2] + 1,
      }
    : 0;
}

sub digit {
    substr( $_[1], $_[2], 1 ) =~ m/\d/
    ? {
        str  => $_[1],
        from => $_[2],
        to   => $_[2] + 1,
      }
    : 0;
}

sub ident {
    return 
        if substr( $_[1], $_[2], 1 ) !~ m/\w/
        || substr( $_[1], $_[2], 1 ) =~ m/\d/;
    my $m = {
         str  => $_[1],
         from => $_[2],
         to   => $_[2] + 1,
       };
    $m->{to}++
        while substr( $_[1], $m->{to}, 1 ) =~ m/\w/;
    $m;
}

sub caret_char {
    my $c = substr( $_[1], $_[2], 1 );
    my $pos = $_[2];
    if ($c eq '^') {
        $pos++;
        $c = substr( $_[1], $pos, 1 );
        return 0 if $c lt 'A' || $c gt 'Z';
        $c = chr( ord($c) - ord("A") + 1 );
    }
    elsif ( $_[0]->ws($_[1], $pos) ) {
        return 0;
    }
    return 0 if $c lt "\cA" || $c gt "\cZ";
    return {
             str  => $_[1],
             from => $_[2],
             to   => $pos + 1,
             capture => $c,
           }
}

token full_ident {
    <.ident>  [ '::' <.ident> ]*
};

token namespace_before_ident {
    <.ident> <before '::'>   [ '::' <.ident> <before '::'> ]*
};
token optional_namespace_before_ident {
    | <namespace_before_ident> '::'*
        { $MATCH->{capture} = Perlito5::Match::flat($MATCH->{namespace_before_ident}) }
    | '::' <optional_namespace_before_ident>
        { 
            my $name = Perlito5::Match::flat($MATCH->{optional_namespace_before_ident});
            $MATCH->{capture} = 'main';
            $MATCH->{capture} .= '::' . $name if $name ne '';
        }
    | ''
        { $MATCH->{capture} = '' }
};


# TODO - use Perlito5::Grammar::Space->ws() instead
token ws      { <.Perlito5::Grammar::Space.ws>  };
token opt_ws  { <.Perlito5::Grammar::Space.ws>? };


token exp_stmts2 { <exp_stmts> { $MATCH->{capture} = Perlito5::Match::flat($MATCH->{exp_stmts}) } };

token exp {
    <Perlito5::Expression.exp_parse>
        { $MATCH->{capture} = Perlito5::Match::flat($MATCH->{"Perlito5::Expression.exp_parse"}) }
};

token exp2 {
    <Perlito5::Expression.exp_parse>
        { $MATCH->{capture} = Perlito5::Match::flat($MATCH->{"Perlito5::Expression.exp_parse"}) }
};

token opt_type {
    |   '::'?  <full_ident>   { $MATCH->{capture} = Perlito5::Match::flat($MATCH->{full_ident}) }
    |   ''                    { $MATCH->{capture} = '' }
};

token var_sigil     { \$ |\% |\@ |\& | \* };

token var_name      { <full_ident> | <digit> };

token var_ident {
    <var_sigil> <optional_namespace_before_ident> <var_name>
    {
        $MATCH->{capture} = Perlito5::AST::Var->new(
            sigil       => Perlito5::Match::flat($MATCH->{var_sigil}),
            namespace   => Perlito5::Match::flat($MATCH->{optional_namespace_before_ident}),
            name        => Perlito5::Match::flat($MATCH->{var_name}),
        )
    }
};

token exponent {
    [ 'e' | 'E' ]  [ '+' | '-' | '' ]  [ '_' | \d ]+
};

token val_num {
    [
    |   \. \d [ '_' | \d]*    <.exponent>?    # .10 .10e10
    |      \d [ '_' | \d]*  [ <.exponent>  |   \. <!before \. > [ '_' | \d]*  <.exponent>? ]
    ]
    {
        my $s = Perlito5::Match::flat($MATCH);
        $s =~ s/_//g;
        $MATCH->{capture} = Perlito5::AST::Val::Num->new( num => $s ) 
    }
};

token digits {
    \d+
};

token val_int {
    [ '0' ['x'|'X'] <.word>+   # XXX test for hex number
    | '0' ['b'|'B'] [ '_' | '0' | '1' ]+
    | '0' [ '_' | \d]+        # XXX test for octal number
    ]
        { $MATCH->{capture} = Perlito5::AST::Val::Int->new( int => oct(lc(Perlito5::Match::flat($MATCH))) ) }
    | \d [ '_' | \d]*
        {
            my $s = Perlito5::Match::flat($MATCH);
            $s =~ s/_//g;
            $MATCH->{capture} = Perlito5::AST::Val::Int->new( int => $s )
        }
};

token val_version {
    ['v']? <.digits> [ '.' <.digits> [ '.' <.digits> ]? ]?
};

my @PKG;
token exp_stmts {
    {
        push @PKG, $Perlito5::PKG_NAME
    }
    <Perlito5::Expression.delimited_statement>*
    { 
        $Perlito5::PKG_NAME = pop @PKG;
        $MATCH->{capture} = [ map( $_->{capture}, @{ $MATCH->{"Perlito5::Expression.delimited_statement"} } ) ]
    }
};

token args_sig {
    [ ';' | '\\' | '[' | ']' | '*' | '+' | '@' | '%' | '$' | '&' ]*
};

token prototype {
    |   <.opt_ws> \( <.opt_ws>  <args_sig>  <.opt_ws>  \)
        { $MATCH->{capture} = "" . Perlito5::Match::flat($MATCH->{args_sig}) }
    |   { $MATCH->{capture} = '*undef*' }   # default signature
};

token anon_sub_def {
    <prototype> <.opt_ws> \{ <.opt_ws> <exp_stmts> <.opt_ws>
    [   \}     | { die 'Syntax Error in anon sub' } ]
    {
        my $sig  = Perlito5::Match::flat($MATCH->{prototype});
        $sig = undef if $sig eq '*undef*';
        $MATCH->{capture} = Perlito5::AST::Sub->new(
            name  => undef, 
            namespace => undef,
            sig   => $sig, 
            block => Perlito5::Match::flat($MATCH->{exp_stmts}) 
        ) 
    }
};


=begin

=head1 NAME

Perlito5::Grammar - Grammar for Perlito

=head1 SYNOPSIS

    my $match = $source.parse;
    Perlito5::Match::flat($match);    # generated Perlito AST

=head1 DESCRIPTION

This module generates a syntax tree for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2010, 2011, 2012 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
