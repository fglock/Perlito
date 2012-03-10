use v5;

use Perlito5::AST;
use Perlito5::Dumper;

package Perlito5::Javascript;
{
    sub tab {
        my $level = shift;
        "\t" x $level
    }

    my %safe_char = (
        ' ' => 1,
        '!' => 1,
        '"' => 1,
        '#' => 1,
        '$' => 1,
        '%' => 1,
        '&' => 1,
        '(' => 1,
        ')' => 1,
        '*' => 1,
        '+' => 1,
        ',' => 1,
        '-' => 1,
        '.' => 1,
        '/' => 1,
        ':' => 1,
        ';' => 1,
        '<' => 1,
        '=' => 1,
        '>' => 1,
        '?' => 1,
        '@' => 1,
        '[' => 1,
        ']' => 1,
        '^' => 1,
        '_' => 1,
        '`' => 1,
        '{' => 1,
        '|' => 1,
        '}' => 1,
        '~' => 1,
    );

    sub escape_string {
        my $s = shift;
        my @out;
        my $tmp = '';
        return "''" if $s eq '';
        for my $i (0 .. length($s) - 1) {
            my $c = substr($s, $i, 1);
            if  (  ($c ge 'a' && $c le 'z')
                || ($c ge 'A' && $c le 'Z')
                || ($c ge '0' && $c le '9')
                || exists( $safe_char{$c} )
                )
            {
                $tmp = $tmp . $c;
            }
            else {
                push @out, "'$tmp'" if $tmp ne '';
                push @out, "String.fromCharCode(" . ord($c) . ")";
                $tmp = '';
            }
        }
        push @out, "'$tmp'" if $tmp ne '';
        return join(' + ', @out);
    }

    sub to_str {
            my $cond = shift;

            if (  $cond->isa( 'Perlito5::AST::Apply' ) && $cond->code eq 'circumfix:<( )>'
               && $cond->{"arguments"} && @{$cond->{"arguments"}}
               ) 
            {
                return to_str( $cond->{"arguments"}[0] )
            }


            if  (  ($cond->isa( 'Perlito5::AST::Val::Buf' ))
                || ($cond->isa( 'Perlito5::AST::Apply' ) 
                   && (  $cond->code eq 'substr'
                      || $cond->code eq 'join'
                      || $cond->code eq 'list:<.>'
                      )
                   )
                )
            {
                return $cond->emit_javascript;
            }
            else {
                return 'string(' . $cond->emit_javascript . ')';
            }
    }
    sub to_num {
            my $cond = shift;
            if ($cond->isa( 'Perlito5::AST::Val::Int' ) || $cond->isa( 'Perlito5::AST::Val::Num' )) {
                return $cond->emit_javascript;
            }
            else {
                return 'num(' . $cond->emit_javascript . ')';
            }
    }
    sub to_bool {
            my $cond = shift;

            if (  $cond->isa( 'Perlito5::AST::Apply' ) && $cond->code eq 'circumfix:<( )>'
               && $cond->{"arguments"} && @{$cond->{"arguments"}}
               ) 
            {
                return to_bool( $cond->{"arguments"}[0] )
            }

            # Note: 'infix:<||>' and 'infix:<&&>' can only be optimized here because we know we want "bool"
            if (  $cond->isa( 'Perlito5::AST::Apply' ) 
               && (  $cond->code eq 'infix:<&&>'
                  || $cond->code eq 'infix:<and>'
                  )
               ) 
            {
                return '(' . to_bool($cond->{"arguments"}->[0]) . ' && '
                           . to_bool($cond->{"arguments"}->[1]) . ')'
            }
            if (  $cond->isa( 'Perlito5::AST::Apply' ) 
               && (  $cond->code eq 'infix:<||>'
                  || $cond->code eq 'infix:<or>'
                  )
               ) 
            {
                return '(' . to_bool($cond->{"arguments"}->[0]) . ' || '
                           . to_bool($cond->{"arguments"}->[1]) . ')'
            }

            if  (  ($cond->isa( 'Perlito5::AST::Val::Int' ))
                || ($cond->isa( 'Perlito5::AST::Val::Num' ))
                || ($cond->isa( 'Perlito5::AST::Apply' ) 
                   && (  $cond->code eq 'prefix:<!>'
                      || $cond->code eq 'infix:<!=>'
                      || $cond->code eq 'infix:<==>'
                      || $cond->code eq 'infix:<<=>'
                      || $cond->code eq 'infix:<>=>'
                      || $cond->code eq 'infix:<>>'
                      || $cond->code eq 'infix:<<>'
                      || $cond->code eq 'infix:<eq>'
                      || $cond->code eq 'infix:<ne>'
                      || $cond->code eq 'infix:<ge>'
                      || $cond->code eq 'infix:<le>'
                      )
                   )
                )
            {
                return $cond->emit_javascript;
            }
            else {
                return 'bool(' . $cond->emit_javascript . ')';
            }
    }

    sub to_list {
        my $items = to_list_preprocess( $_[0] );
        @$items
        ? 'interpolate_array('
          .   join(', ', map( $_->emit_javascript, @$items ))
          . ')'
        : '[]'
    }

    sub to_list_preprocess {
        my @items;
        for my $item ( @{$_[0]} ) {
            if (  $item->isa( 'Perlito5::AST::Apply' ) 
               && ( $item->code eq 'circumfix:<( )>' || $item->code eq 'list:<,>' || $item->code eq 'infix:<=>>' )
               )
            {
                for my $arg ( @{ to_list_preprocess($item->arguments) } ) {
                    push( @items, $arg);
                }
            }
            else {
                push( @items, $item);
            }
        }
        return \@items;
    }
}

package Perlito5::Javascript::LexicalBlock;
{
    sub new { my $class = shift; bless {@_}, $class }
    sub block { $_[0]->{'block'} }
    sub needs_return { $_[0]->{'needs_return'} }
    sub top_level { $_[0]->{'top_level'} }
    # sub create_context ... 

    sub has_decl {
        my $self = $_[0];
        my $type = $_[1];
        for my $decl ( @{$self->{"block"}} ) {
            if (defined $decl) {
                if ($decl->isa( 'Perlito5::AST::Decl' ) && $decl->decl eq $type) {
                    return 1;
                }
                if ($decl->isa( 'Perlito5::AST::Apply' ) && $decl->code eq 'infix:<=>') {
                    my $var = $decl->arguments[0];
                    if ($var->isa( 'Perlito5::AST::Decl' ) && $var->decl eq $type) {
                        return 1;
                    }
                }
            }
        }
        return 0;
    }

    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;

        my @block;
        for (@{$self->{"block"}}) {
            if (defined($_)) {
                push @block, $_
            }
        }
        if (!@block) {
            return Perlito5::Javascript::tab($level) . 'null;';
        }
        my $out = '';
        my @str;
        my $has_local = $self->has_decl("local");
        my $create_context = $self->{"create_context"} && $self->has_decl("my");
        my $outer_pkg   = $Perlito5::PKG_NAME;
        my $outer_throw = $Perlito5::THROW;
        unshift @{ $Perlito5::VAR }, {};

        $Perlito5::THROW = 0
            if $self->{"top_level"};

        $out .= Perlito5::Javascript::tab($level) . "var local_idx = LOCAL.length;\n"
            if $has_local;
        if ($self->{"top_level"}) {
            $level++;
        }
        if ( $create_context ) {
            $out .= Perlito5::Javascript::tab($level) . "(function () {\n";
            $level++;
        }

        my $tab = Perlito5::Javascript::tab($level);
        my $last_statement;
        if ($self->{"needs_return"}) {
            $last_statement = pop @block;
        }
        for my $decl ( @block ) {
            if ( ref($decl) eq 'Perlito5::AST::Apply' && $decl->code eq 'package' ) {
                $Perlito5::PKG_NAME = $decl->{"namespace"};
            }

            if ($decl->isa( 'Perlito5::AST::Decl' )) {
                push @str, $decl->emit_javascript_init;
            }
            if ($decl->isa( 'Perlito5::AST::Apply' ) && $decl->code eq 'infix:<=>') {
                if ($decl->{"arguments"}[0]->isa( 'Perlito5::AST::Decl' )) {
                    push @str, $decl->{"arguments"}[0]->emit_javascript_init;
                }
            }

            if (!( $decl->isa( 'Perlito5::AST::Decl' ) && $decl->decl eq 'my' )) {
                push @str, $decl->emit_javascript_indented($level) . ';';
            }
        }
        if ($self->{"needs_return"} && $last_statement) {

            if ($last_statement->isa( 'Perlito5::AST::Decl' )) {
                push @str, $last_statement->emit_javascript_init;
            }
            if ($last_statement->isa( 'Perlito5::AST::Apply' ) && $last_statement->code eq 'infix:<=>') {
                if ($last_statement->{"arguments"}[0]->isa( 'Perlito5::AST::Decl' )) {
                    push @str, $last_statement->{"arguments"}[0]->emit_javascript_init;
                }
            }

            if ($last_statement->isa( 'Perlito5::AST::If' )) {
                my $cond      = $last_statement->cond;
                my $body      = $last_statement->body;
                my $otherwise = $last_statement->otherwise;
                $body         = Perlito5::Javascript::LexicalBlock->new( block => $body->stmts, needs_return => 1 );
                push @str,
                        'if ( ' . Perlito5::Javascript::to_bool( $cond ) . ' ) {' . "\n"
                        .   $body->emit_javascript_indented($level+1) . "\n"
                        . Perlito5::Javascript::tab($level) . '}';
                if ($otherwise) {
                    $otherwise = Perlito5::Javascript::LexicalBlock->new( block => $otherwise->stmts, needs_return => 1 );
                    push @str, "\n"
                        . Perlito5::Javascript::tab($level) . 'else {' . "\n"
                        .   $otherwise->emit_javascript_indented($level+1) . "\n"
                        . Perlito5::Javascript::tab($level) . '}';
                }
            }
            # elsif ( $last_statement->isa( 'Perlito5::AST::Apply' ) && $last_statement->code eq 'return' ) {
            #     push @str, 'return('
            #         .   ( $last_statement->{"arguments"} && @{$last_statement->{"arguments"}} 
            #             ? $last_statement->{"arguments"}->[0]->emit_javascript() 
            #             : 'null'
            #             )
            #         . ')'
            # }
            elsif (  $last_statement->isa( 'Perlito5::AST::For' )
                  || $last_statement->isa( 'Perlito5::AST::While' )
                  || $last_statement->isa( 'Perlito5::AST::Apply' ) && $last_statement->code eq 'goto'
                  || $last_statement->isa( 'Perlito5::AST::Apply' ) && $last_statement->code eq 'return'
                  )
            {
                push @str, $last_statement->emit_javascript_indented($level)
            }
            else {
                if ( $has_local ) {
                    push @str, 'return cleanup_local(local_idx, (' . $last_statement->emit_javascript_indented($level+1) . '));';
                }
                else {
                    push @str, 'return (' . $last_statement->emit_javascript_indented($level+1) . ');';
                }
            }
        }
        if ( $has_local ) {
            push @str, 'cleanup_local(local_idx, null);';
        }
        if ( $create_context ) {
            $level--;
            push @str, "})();";
        }
        if ($self->{"top_level"} && $Perlito5::THROW) {
            $level--;
            $out .= 
                  Perlito5::Javascript::tab($level) . "try {\n"
                .   join("\n", map($tab . $_, @str)) . "\n"
                . Perlito5::Javascript::tab($level)   . '}' . "\n"
                . Perlito5::Javascript::tab($level)   . 'catch(err) {' . "\n"
                . Perlito5::Javascript::tab($level + 1)   . 'if ( err instanceof Error ) {' . "\n"
                . Perlito5::Javascript::tab($level + 2)       . 'throw(err);' . "\n"
                . Perlito5::Javascript::tab($level + 1)   . '}' . "\n"
                . Perlito5::Javascript::tab($level + 1)   . 'else {' . "\n"
                . Perlito5::Javascript::tab($level + 2)
                    . ( $has_local
                      ? 'return cleanup_local(local_idx, err)'
                      : 'return(err)'
                      )
                    . ";\n"
                . Perlito5::Javascript::tab($level + 1)   . '}' . "\n"
                . Perlito5::Javascript::tab($level)   . '}';
        }
        else {
            $out .= join("\n", map($tab . $_, @str));
        }
        $Perlito5::PKG_NAME = $outer_pkg;
        $Perlito5::THROW    = $outer_throw
            if $self->{"top_level"};
        shift @{ $Perlito5::VAR };
        return $out;
    }

}

package Perlito5::AST::CompUnit;
{
    sub emit_javascript { 
        my $self = $_[0];
        $self->emit_javascript_indented(0) 
    }
    sub emit_javascript_indented {
        my $self = $_[0];
        my $level = $_[1];
        my $str = "(function () {\n"
            .   Perlito5::Javascript::LexicalBlock->new( block => $self->{"body"}, needs_return => 0 )->emit_javascript_indented( $level + 1 ) . "\n"
            . Perlito5::Javascript::tab($level) . "})()\n";
        return $str;
    }
    sub emit_javascript_program {
        my $comp_units = shift;
        my $str = '';
        $Perlito5::PKG_NAME = 'main';
        $Perlito5::VAR = [
            { '@_'    => { decl => 'my' },
              '$_'    => { decl => 'my' },
              '@ARGV' => { decl => 'my' },
            }
            ## TODO
            ## { '@_'    => { decl => 'our', namespace => $Perlito5::PKG_NAME },
            ##   '$_'    => { decl => 'our', namespace => $Perlito5::PKG_NAME },
            ##   '@ARGV' => { decl => 'our', namespace => $Perlito5::PKG_NAME },
            ## }
        ];
        for my $comp_unit ( @$comp_units ) {
            $str = $str . $comp_unit->emit_javascript() . "\n";
        }
        return $str;
    }
}

package Perlito5::AST::Val::Int;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        $self->{"int"} }
}

package Perlito5::AST::Val::Num;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        $self->{"num"} }
}

package Perlito5::AST::Val::Buf;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        Perlito5::Javascript::escape_string($self->{"buf"}) }
}

package Perlito5::AST::Lit::Block;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        # my $sig = 'v__';
        # if ($self->{"sig"}) {
        #     $sig = $self->{"sig"}->emit_javascript_indented( $level + 1 );
        # }


        Perlito5::AST::For->new(
                    # XXX - $_ is not declared
                    # cond  => Perlito5::AST::Var->new( sigil => '$', namespace => '', name => '_' ),
                    cond  => Perlito5::AST::Val::Int->new( int => 0 ),
                    topic => undef,
                    body  => Perlito5::AST::Lit::Block->new( stmts => $self->{"stmts"} )
                 )->emit_javascript_indented( $level );


        # return
        #       Perlito5::Javascript::tab($level) . "(function ($sig) \{\n"
        #     .   (Perlito5::Javascript::LexicalBlock->new( block => $self->{"stmts"}, needs_return => 1 ))->emit_javascript_indented( $level + 1 ) . "\n"
        #     . Perlito5::Javascript::tab($level) . '})()'
    }
}

package Perlito5::AST::Index;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;

        if (  $self->{"obj"}->isa('Perlito5::AST::Var')
           && $self->{"obj"}->sigil eq '$'
           )
        {
            my $v = Perlito5::AST::Var->new( sigil => '@', namespace => $self->{"obj"}->namespace, name => $self->{"obj"}->name );
            return $v->emit_javascript_indented($level) . '[' . $self->{"index_exp"}->emit_javascript() . ']';
        }

          '('
        .   $self->{"obj"}->emit_javascript() 
        .   ' || (' . $self->{"obj"}->emit_javascript() . ' = new ArrayRef([]))'
        . ')._array_[' . $self->{"index_exp"}->emit_javascript() . ']';
    }
}

package Perlito5::AST::Lookup;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        # my $var = $self->{"obj"}->emit_javascript;
        # return $var . '[' . $self->{"index_exp"}->emit_javascript() . ']'

        if (  $self->{"obj"}->isa('Perlito5::AST::Var')
           && $self->{"obj"}->sigil eq '$'
           )
        {
            my $v = Perlito5::AST::Var->new( sigil => '%', namespace => $self->{"obj"}->namespace, name => $self->{"obj"}->name );
            return $v->emit_javascript_indented($level) . '[' . $self->{"index_exp"}->emit_javascript() . ']';
        }

          '('
        .   $self->{"obj"}->emit_javascript() 
        .   ' || (' . $self->{"obj"}->emit_javascript() . ' = new HashRef({}))'
        . ')._hash_[' . $self->{"index_exp"}->emit_javascript() . ']';
    }
}

package Perlito5::AST::Var;
{
    my $table = {
        '$' => 'v_',
        '@' => 'List_',
        '%' => 'Hash_',
        '&' => '',
    };

    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;

        my $perl5_name = $self->perl5_name;
        # say "looking up $perl5_name";
        my $decl_type;  # my, our, local
        my $decl = $self->perl5_get_decl( $perl5_name );
        if ( $decl ) {
            # say "found ", $decl->{"decl"};
            $decl_type = $decl->{"decl"};
        }
        else {
            die "Global symbol \"$perl5_name\" requires explicit package name"
                unless $self->{"namespace"}
                    || $self->{"sigil"} eq '*';
        }

        if ( $self->{"sigil"} eq '*' ) {
            return 'NAMESPACE["' . ($self->{"namespace"} || $Perlito5::PKG_NAME) . '"]["' . $self->{"name"} . '"]';
        }
        if ( $decl_type eq 'our' ) {
            return 'NAMESPACE["' . ($self->{"namespace"} || $decl->{"namespace"}) . '"]["' . $table->{$self->{"sigil"}} . $self->{"name"} . '"]';
        }

        my $ns = '';
        if ($self->{"namespace"}) {
            $ns = 'NAMESPACE["' . $self->{"namespace"} . '"].';
        }
        $ns . $table->{$self->{"sigil"}} . $self->{"name"}
    }
    sub perl5_name {
        my $self = shift;
        $self->{"sigil"}
        . ( $self->{"namespace"}
          ? $self->{"namespace"} . '::'
          : ''
          )
        . $self->{"name"}
    }
    sub perl5_get_decl {
        my $self = shift;
        my $perl5_name = shift;
        for ( @{ $Perlito5::VAR } ) {
            return $_->{$perl5_name}
                if exists $_->{$perl5_name}
        }
        return undef;
    }
}

package Perlito5::AST::Decl;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        if ($self->{"decl"} eq 'local') {

            # TODO - add grammar support
            # if ($self->var->isa("Lookup")) {
            #     return 
            #         'set_local(' . $self->var->{"obj"}->emit_javascript() . ', '
            #                      . $self->var->{"index_exp"}->emit_javascript() . ', '
            #                      . '""); '
            #         . $self->{"var"}->emit_javascript_indented( $level );
            # }

            my $perl5_name = $self->{"var"}->perl5_name;
            # say "looking up $perl5_name";
            my $decl_namespace = '';
            my $decl = $self->{"var"}->perl5_get_decl( $perl5_name );
            if ( $decl && ($decl->{"decl"} eq 'our' || $decl->{"decl"} eq 'local')) {
                # say "found ", $decl->{"decl"}, " namespace: ", $decl->{"namespace"};
                $decl_namespace = $decl->{"namespace"};
            }

            my $ns = 'NAMESPACE["' . ($self->{"var"}{"namespace"} || $decl_namespace || $Perlito5::PKG_NAME) . '"]';

            return
                  'set_local(' . $ns . ','
                               . Perlito5::Javascript::escape_string($self->{"var"}{"name"}) . ','
                               . Perlito5::Javascript::escape_string($self->{"var"}{"sigil"}) . '); ' 
                . $self->{"var"}->emit_javascript_indented( $level );
        }
        $self->{"var"}->emit_javascript_indented( $level );
    }
    sub emit_javascript_init {
        my $self = shift;

        my $env = { decl => $self->{"decl"} };
        my $perl5_name = $self->{"var"}->perl5_name;
        if ( $self->{"decl"} ne 'my' ) {

            die "No package name allowed for variable $perl5_name in \"our\""
                if $self->{"decl"} eq 'our' && $self->{"var"}{"namespace"};

            if ( $self->{"var"}{"namespace"} eq '' ) {
                # say "looking up $perl5_name";
                my $decl_namespace = '';
                my $decl = $self->{"var"}->perl5_get_decl( $perl5_name );
                if ( $self->{"decl"} eq 'local' && $decl && ($decl->{"decl"} eq 'our' || $decl->{"decl"} eq 'local')) {
                    # say "found ", $decl->{"decl"}, " namespace: ", $decl->{"namespace"};
                    $decl_namespace = $decl->{"namespace"};
                }
                $env->{"namespace"} = $decl_namespace || $Perlito5::PKG_NAME;
            }
        }

        $Perlito5::VAR->[0]{ $perl5_name } = $env;

        if ($self->{"decl"} eq 'my') {
            my $str = "";
            $str = $str . 'var ' . $self->{"var"}->emit_javascript() . ' = ';
            if ($self->{"var"}->sigil eq '%') {
                $str = $str . '{};';
            }
            elsif ($self->{"var"}->sigil eq '@') {
                $str = $str . '[];';
            }
            else {
                $str = $str . 'null;';
            }
            return $str;
        }
        elsif ($self->{"decl"} eq 'our') {
            # TODO
            return '// our ' . $self->{"var"}->emit_javascript();
        }
        elsif ($self->{"decl"} eq 'local') {
            # TODO
            return '// local ' . $self->{"var"}->emit_javascript();
        }
        elsif ($self->{"decl"} eq 'state') {
            # TODO
            return '// state ' . $self->{"var"}->emit_javascript();
        }
        else {
            die "not implemented: Perlito5::AST::Decl '" . $self->{"decl"} . "'";
        }
    }
}

package Perlito5::AST::Proto;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        'CLASS["' . $self->{"name"} . '"]'
    }
}

package Perlito5::AST::Call;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $invocant = $self->{"invocant"}->emit_javascript;
        my $meth = $self->{"method"};

        if ( $meth eq 'postcircumfix:<[ ]>' ) {
            return 
                  '('
                .   $invocant 
                .   ' || (' . $invocant . ' = new ArrayRef([]))'
                . ')._array_[' . $self->{"arguments"}->emit_javascript() . ']';
        }
        if ( $meth eq 'postcircumfix:<{ }>' ) {
            return
                  '('
                .   $invocant 
                .   ' || (' . $invocant . ' = new HashRef({}))'
                . ')._hash_[' . $self->{"arguments"}->emit_javascript() . ']';
        }
        if  ($meth eq 'postcircumfix:<( )>')  {
            my @args = ();
            push @args, $_->emit_javascript
                for @{$self->{"arguments"}};
            return '(' . $invocant . ')([' . join(',', @args) . '])';
        }
        my @args;
        push @args, $_->emit_javascript
            for @{$self->{"arguments"}};
        return '_call_(' . $invocant . ', "' . $meth . '", [' . join(',', @args) . '])'
    }
}

package Perlito5::AST::Apply;
{

    my %op_infix_js = (
        'infix:<eq>' => ' == ',
        'infix:<ne>' => ' != ',
        'infix:<le>' => ' <= ',
        'infix:<ge>' => ' >= ',
    );
    my %op_infix_js_num = (
        'infix:<==>' => ' == ',
        'infix:<!=>' => ' != ',
        'infix:<->'  => ' - ',
        'infix:<*>'  => ' * ',
        'infix:</>'  => ' / ',
        'infix:<>>'  => ' > ',
        'infix:<<>'  => ' < ',
        'infix:<>=>' => ' >= ',
        'infix:<<=>' => ' <= ',
        'infix:<&>'  => ' & ',
        'infix:<|>'  => ' | ',
        'infix:<^>'  => ' ^ ',
        'infix:<>>>' => ' >>> ',
        'infix:<<<>' => ' << ',
    );

    sub emit_regex_javascript {
        my $op = shift;
        my $var = shift;
        my $regex = shift;

        my $str;
        my $code = $regex->{"code"};
        my $regex_args = $regex->{"arguments"};
        if ($code eq 'p5:s') {
            $str = $var->emit_javascript() 
                 . ' = ' . $var->emit_javascript() . '.replace(/' . $regex_args->[0]->{"buf"} . '/' . $regex_args->[2] . ', '
                 .  $regex_args->[1]->emit_javascript() . ')';
        }
        elsif ($code eq 'p5:m') {
            $str = $var->emit_javascript() . '.match(/' . $regex_args->[0]->{"buf"} . '/' . $regex_args->[1] . ')';
        }
        else {
            die "Error: regex emitter - unknown operator $code";
        }

        if ($op eq '=~') {
            return $str;
        }
        if ($op eq '!~') {
            return '!(' . $str . ')'
        }
        die "Error: regex emitter";
    }


    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;

        my $apply = $self->op_assign();
        if ($apply) {
            return $apply->emit_javascript_indented( $level );
        }

        my $code = $self->{"code"};

        if (ref $code ne '') {
            my @args = ();
            push @args, $_->emit_javascript
                for @{$self->{"arguments"}};
            return '(' . $self->{"code"}->emit_javascript_indented( $level ) . ')(' . join(',', @args) . ')';
        }


        if ($code eq 'infix:<=~>') {
            return emit_regex_javascript(
                '=~',
                $self->{"arguments"}->[0],
                $self->{"arguments"}->[1]
            );
        }
        if ($code eq 'infix:<!~>') {
            return emit_regex_javascript(
                '!~',
                $self->{"arguments"}->[0],
                $self->{"arguments"}->[1]
            );
        }
        if ($code eq 'p5:s') {
            return emit_regex_javascript(
                '=~',
                Perlito5::AST::Var->new( sigil => '$', namespace => '', name => '_' ),
                $self
            );
        }
        if ($code eq 'p5:m') {
            return emit_regex_javascript(
                '=~',
                Perlito5::AST::Var->new( sigil => '$', namespace => '', name => '_' ),
                $self
            );
        }


        if ($code eq 'package') {
            return 'make_package("' . $self->{"namespace"} . '")';
        }
        if ($code eq 'infix:<=>>') {
            return join(', ', map( $_->emit_javascript_indented( $level ), @{$self->{"arguments"}} ))
        }
        if (exists $op_infix_js{$code}) {
            return '(' 
                . join( $op_infix_js{$code}, map( $_->emit_javascript_indented( $level ), @{$self->{"arguments"}} ))
                . ')'
        }
        if (exists $op_infix_js_num{$code}) {
            return '(' 
                . join( $op_infix_js_num{$code}, map( Perlito5::Javascript::to_num($_), @{$self->{"arguments"}} ))
                . ')'
        }

        if ( $code eq 'prefix:<!>' ) {
            return '!( ' . Perlito5::Javascript::to_bool( $self->{"arguments"}->[0] ) . ')';
        }
        if ( $code eq 'prefix:<~>' ) {
            return '~( ' . Perlito5::Javascript::to_num( $self->{"arguments"}->[0] ) . ')';
        }
        if ( $code eq 'prefix:<->' ) {
            return '-( ' . $self->{"arguments"}->[0]->emit_javascript() . ')';
        }
        if ($code eq 'prefix:<+>') { 
            return '('  . $self->{"arguments"}->[0]->emit_javascript()  . ')' 
        }

        if ($code eq 'eval') {
            my $var_env_perl5 = Perlito5::Dumper::Dumper( $Perlito5::VAR );
            # say "at eval: ", $var_env_perl5;
            my $m = Perlito5::Expression->term_square( $var_env_perl5, 0 );
            $m = Perlito5::Expression::expand_list( $m->flat()->[2] );
            # say Perlito5::Dumper::Dumper( $m );
            my $var_env_js = '(new ArrayRef(' . Perlito5::Javascript::to_list($m) . '))';
            return
                'eval(perl5_to_js(' 
                    . Perlito5::Javascript::to_str($self->{"arguments"}->[0]) . ", "
                    . '"' . $Perlito5::PKG_NAME . '", '
                    . $var_env_js
                . '))'
        }

        if ($code eq 'undef')      {
            if ( $self->{"arguments"} && @{$self->{"arguments"}} ) {
                return '(' . $self->{"arguments"}->[0]->emit_javascript . ' = null)'
            }
            return 'null'
        }

        if ($code eq 'defined')    { return '('  . join(' ', map( $_->emit_javascript_indented( $level ), @{$self->{"arguments"}} ))    . ' != null)' }

        if ($code eq 'shift')      {
            if ( $self->{"arguments"} && @{$self->{"arguments"}} ) {
                return 'NAMESPACE["' . $Perlito5::PKG_NAME . '"].shift([' . join(', ', map( $_->emit_javascript_indented( $level ), @{$self->{"arguments"}} )) . '])'
            }
            return 'NAMESPACE["' . $Perlito5::PKG_NAME . '"].shift([List__])'
        }

        if ($code eq 'map') {
            my $fun  = $self->{"arguments"}->[0];
            my $list = $self->{"arguments"}->[1];
            return
                    '(function (a_) { '
                        . 'var out = []; '
                        . 'if ( a_ == null ) { return out; }; '
                        . 'for(var i = 0; i < a_.length; i++) { '
                            . 'var v__ = a_[i]; '
                            . 'out.push(' . $fun->emit_javascript_indented( $level ) . ')'
                        . '}; '
                        . 'return out;'
                    . ' })(' . $list->emit_javascript() . ')'
        }

        if ( $code eq 'prefix:<$>' ) {
            my $arg = $self->{"arguments"}->[0];
            return '(' . $arg->emit_javascript . ')._scalar_';
        }
        if ( $code eq 'prefix:<@>' ) {
            my $arg = $self->{"arguments"}->[0];
            return
                  '('
                .   $arg->emit_javascript_indented( $level ) 
                .   ' || (' . $arg->emit_javascript_indented( $level ) . ' = new ArrayRef([]))'
                . ')._array_';
        }
        if ( $code eq 'prefix:<%>' ) {
            my $arg = $self->{"arguments"}->[0];
            return '(' . $arg->emit_javascript_indented( $level ) . ')._hash_';
        }

        if ( $code eq 'circumfix:<[ ]>' ) {
            return '(new ArrayRef(' . Perlito5::Javascript::to_list($self->{"arguments"}) . '))'
        }
        if ( $code eq 'circumfix:<{ }>' ) {
            return '(new HashRef(array_to_hash(' . Perlito5::Javascript::to_list($self->{"arguments"}) . ')))'
        }
        if ( $code eq 'prefix:<\\>' ) {
            my $arg = $self->{"arguments"}->[0];
            if ( $arg->isa('Perlito5::AST::Var') ) {
                if ( $arg->sigil eq '@' ) {
                    return '(new ArrayRef(' . $arg->emit_javascript_indented( $level ) . '))';
                }
                if ( $arg->sigil eq '%' ) {
                    return '(new HashRef(' . $arg->emit_javascript_indented( $level ) . '))';
                }
                if ( $arg->sigil eq '&' ) {
                    if ($arg->{"namespace"}) {
                        return 'NAMESPACE["' . $arg->{"namespace"} . '"].' . $arg->{"name"};
                    }
                    else {
                        return 'NAMESPACE["' . $Perlito5::PKG_NAME . '"].' . $arg->{"name"};
                    }
                }
            }
            return '(new ScalarRef(' . $arg->emit_javascript_indented( $level ) . '))';
        }

        if ($code eq 'postfix:<++>') { return '('   . join(' ', map( $_->emit_javascript, @{$self->{"arguments"}} ))  . ')++' }
        if ($code eq 'postfix:<-->') { return '('   . join(' ', map( $_->emit_javascript, @{$self->{"arguments"}} ))  . ')--' }
        if ($code eq 'prefix:<++>')  { return '++(' . join(' ', map( $_->emit_javascript, @{$self->{"arguments"}} ))  . ')' }
        if ($code eq 'prefix:<-->')  { return '--(' . join(' ', map( $_->emit_javascript, @{$self->{"arguments"}} ))  . ')' }

        if ($code eq 'infix:<x>')  { return 'str_replicate(' . join(', ', map( $_->emit_javascript, @{$self->{"arguments"}} ))  . ')' }

        if ($code eq 'list:<.>')
        { 
            return '('  
                . join( ' + ',
                        map( Perlito5::Javascript::to_str($_), @{$self->{"arguments"}} )
                      )
                . ')' 
        }

        if ($code eq 'infix:<+>')  { return 'add' . '('  . join(', ', map( $_->emit_javascript, @{$self->{"arguments"}} ))  . ')' }

        if ($code eq 'infix:<..>') {
            return '(function (a) { '
                    . 'for (var i=' . $self->{"arguments"}->[0]->emit_javascript()
                           . ', l=' . $self->{"arguments"}->[1]->emit_javascript() . '; '
                       . 'i<=l; ++i)'
                    . '{ '
                        . 'a.push(i) '
                    . '}; '
                    . 'return a '
                . '})([])'
        }

        if (  $code eq 'infix:<&&>'
           || $code eq 'infix:<and>'
           )
        {
            return 'and' . '('
                . $self->{"arguments"}->[0]->emit_javascript() . ', '
                . 'function () { return ' . $self->{"arguments"}->[1]->emit_javascript() . '; })'
        }
        if (  $code eq 'infix:<||>'
           || $code eq 'infix:<or>'
           )
        {
            return 'or' . '('
                . $self->{"arguments"}->[0]->emit_javascript() . ', '
                . 'function () { return ' . $self->{"arguments"}->[1]->emit_javascript() . '; })'
        }
        if ($code eq 'infix:<//>') { return 'defined_or' . '('
                . $self->{"arguments"}->[0]->emit_javascript() . ', '
                . 'function () { return ' . $self->{"arguments"}->[1]->emit_javascript() . '; })'
        }

        if ($code eq 'exists') {
            my $arg = $self->{"arguments"}->[0];
            if ($arg->isa( 'Perlito5::AST::Lookup' )) {
                my $v = $arg->obj;
                if (  $v->isa('Perlito5::AST::Var')
                   && $v->sigil eq '$'
                   )
                {
                    $v = Perlito5::AST::Var->new( sigil => '%', namespace => $v->namespace, name => $v->name );
                    return '(' . $v->emit_javascript() . ').hasOwnProperty(' . ($arg->index_exp)->emit_javascript() . ')';
                }
                return '(' . $v->emit_javascript() . ')._hash_.hasOwnProperty(' . ($arg->index_exp)->emit_javascript() . ')';
            }
            if ($arg->isa( 'Perlito5::AST::Call' )) {
                if ( $arg->method eq 'postcircumfix:<{ }>' ) {
                    return '(' . $arg->invocant->emit_javascript() . ')._hash_.hasOwnProperty(' . $arg->{"arguments"}->emit_javascript() . ')';
                }
            }
        }
        if ($code eq 'ternary:<?? !!>') {
            return
                   '( ' . Perlito5::Javascript::to_bool( $self->{"arguments"}->[0] )
                 . ' ? ' . ($self->{"arguments"}->[1])->emit_javascript()
                 . ' : ' . ($self->{"arguments"}->[2])->emit_javascript()
                 . ')'
        }
        if ($code eq 'circumfix:<( )>') {
            return '(' . join(', ', map( $_->emit_javascript_indented( $level ), @{$self->{"arguments"}} )) . ')';
        }
        if ($code eq 'infix:<=>') {
            return emit_javascript_bind( $self->{"arguments"}->[0], $self->{"arguments"}->[1], $level );
        }
        if ($code eq 'return') {
            $Perlito5::THROW = 1;
            return 'throw('
                .   ( $self->{"arguments"} && @{$self->{"arguments"}} 
                    ? $self->{"arguments"}->[0]->emit_javascript() 
                    : 'null'
                    )
                . ')'
        }
        if ($code eq 'goto') {
            $Perlito5::THROW = 1;
            return 'throw((' . $self->{"arguments"}->[0]->emit_javascript() . ')([List__]))'
        }

        if ($self->{"namespace"}) {

            if (  $self->{"namespace"} eq 'JS' 
               && $code eq 'inline'
               ) 
            {
                if ( $self->{"arguments"}->[0]->isa('Perlito5::AST::Val::Buf') ) {
                    # JS::inline('var x = 123')
                    return $self->{"arguments"}[0]{"buf"};
                }
                else {
                    die "JS::inline needs a string constant";
                }
            }

            $code = 'NAMESPACE["' . $self->{"namespace"} . '"].' . $code;
        }
        else {
            $code = 'NAMESPACE["' . $Perlito5::PKG_NAME . '"].' . $code
        }
        my @args = ();
        push @args, $_->emit_javascript_indented( $level )
            for @{$self->{"arguments"}};
        $code . '([' . join(', ', @args) . '])';
    }

    sub emit_javascript_bind {
        my $parameters = shift;
        my $arguments = shift;
        my $level = shift;

        if  (   $parameters->isa( 'Perlito5::AST::Var' ) && $parameters->sigil eq '@'
            ||  $parameters->isa( 'Perlito5::AST::Decl' ) && $parameters->var->sigil eq '@'
            )
        {
            return '(' . $parameters->emit_javascript() . ' = ' . Perlito5::Javascript::to_list([$arguments]) . ')'
        }
        elsif ( $parameters->isa( 'Perlito5::AST::Var' ) && $parameters->sigil eq '%'
            ||  $parameters->isa( 'Perlito5::AST::Decl' ) && $parameters->var->sigil eq '%'
            )
        {
            return '(' . $parameters->emit_javascript() . ' = array_to_hash(' . Perlito5::Javascript::to_list([$arguments]) . '))' 
        }
        '(' . $parameters->emit_javascript_indented( $level ) . ' = ' . $arguments->emit_javascript_indented( $level ) . ')';
    }
}

package Perlito5::AST::If;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $cond = $self->{"cond"};
        my $body  = Perlito5::Javascript::LexicalBlock->new( block => $self->{"body"}->stmts, needs_return => 0, create_context => 1 );
        my $s = 'if ( ' . Perlito5::Javascript::to_bool( $cond ) . ' ) {' . "\n"
            .       $body->emit_javascript_indented( $level + 1 ) . "\n"
            . Perlito5::Javascript::tab($level) . '}';
        if ( @{ $self->{"otherwise"}->stmts } ) {
            my $otherwise = Perlito5::Javascript::LexicalBlock->new( block => $self->{"otherwise"}->stmts, needs_return => 0, create_context => 1 );
            $s = $s
                . "\n"
                . Perlito5::Javascript::tab($level) . 'else {' . "\n"
                .       $otherwise->emit_javascript_indented( $level + 1 ) . "\n"
                . Perlito5::Javascript::tab($level) . '}';
        }
        return $s;
    }
}


package Perlito5::AST::While;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $body      = Perlito5::Javascript::LexicalBlock->new( block => $self->{"body"}->stmts, needs_return => 0, create_context => 1 );
        return
           'for ( '
        .  ( $self->{"init"}     ? $self->{"init"}->emit_javascript()           . '; '  : '; ' )
        .  ( $self->{"cond"}     ? Perlito5::Javascript::to_bool( $self->{"cond"} )       . '; '  : '; ' )
        .  ( $self->{"continue"} ? $self->{"continue"}->emit_javascript()       . ' '   : ' '  )
        .  ') {' . "\n" 
            . $body->emit_javascript_indented( $level + 1 ) . "\n"
        .  Perlito5::Javascript::tab($level) . '}'
    }
}

package Perlito5::AST::For;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;

        my $cond = Perlito5::Javascript::to_list([$self->{"cond"}]);
        my $body = Perlito5::Javascript::LexicalBlock->new( block => $self->{"body"}->stmts, needs_return => 0 );
        my $sig  = 'v__';
        if ($self->{"body"}->sig()) {

            # XXX - cleanup: "for" parser throws away the variable declaration, so we need to create it again
            # mark the variable as "declared"
            my $v = $self->{"body"}->sig;
            $Perlito5::VAR->[0]{ $v->perl5_name } = { decl => 'my' };

            $sig = $v->emit_javascript_indented( $level + 1 );
        }
        'for (var i_ = 0, a_ = (' . $cond . '); i_ < a_.length ; i_++) { ' . "(function ($sig) {\n"
                . $body->emit_javascript_indented( $level + 1 ) . "\n"
        . Perlito5::Javascript::tab($level) . '})(a_[i_]) }'
    }
}

package Perlito5::AST::Sub;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;

        my $s =                     'function (List__) {' . "\n"
        .   (Perlito5::Javascript::LexicalBlock->new( block => $self->{"block"}, needs_return => 1, top_level => 1 ))->emit_javascript_indented( $level + 1 ) . "\n"
        . Perlito5::Javascript::tab($level) . '}';

        if ( $self->{"name"} ) {

            if ( $Perlito5::PKG_NAME ne $self->{"namespace"} ) {
                # diagnostics: we are migrating from emit-time namespace to parse-time namespace resolution
                die "bad sub namespace $Perlito5::PKG_NAME ne ", $self->{"namespace"};
            }

            return 'make_sub("' . $self->{"namespace"} . '", "' . $self->{"name"} . '", ' . $s . ')'
        }
        else {
            return $s;
        }
    }
}

package Perlito5::AST::Do;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $block = $self->simplify->block;
        return
              '(function () {' . "\n"
            .   (Perlito5::Javascript::LexicalBlock->new( block => $block, needs_return => 1 ))->emit_javascript_indented( $level + 1 ) . "\n"
            . Perlito5::Javascript::tab($level) . '})()'
    }
}

package Perlito5::AST::Use;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        '// use ' . $self->{"mod"} . "\n"
    }
}

=begin

=head1 NAME

Perlito5::Javascript::Emit - Code generator for Perlito Perl5-in-Javascript

=head1 SYNOPSIS

    $program->emit_javascript()  # generated Perl5 code

=head1 DESCRIPTION

This module generates Javascript code for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
