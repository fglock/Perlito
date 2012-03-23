use v5;

use Perlito5::AST;
use Perlito5::Dumper;

package Perlito5::Javascript;
{
    my $label_count = 100;
    my %label;
    sub pkg {
        $label{ $Perlito5::PKG_NAME } ||= "p5" . $label_count++
    }
    sub get_label {
        $label_count++
    }

    sub tab {
        my $level = shift;
        "\t" x $level
    }

    # prefix operators that take a "str" parameter
    our %op_prefix_js_str = (
        'prefix:<-A>' => 'p5atime',
        'prefix:<-M>' => 'p5mtime',
        'prefix:<-C>' => 'p5ctime',
        'prefix:<-s>' => 'p5size',
        'prefix:<-f>' => 'p5is_file',
        'prefix:<-d>' => 'p5is_directory',
    );

    # these operators need 2 "str" parameters
    our %op_infix_js_str = (
        'infix:<eq>' => ' == ',
        'infix:<ne>' => ' != ',
        'infix:<le>' => ' <= ',
        'infix:<ge>' => ' >= ',
    );
    # these operators need 2 "num" parameters
    our %op_infix_js_num = (
        'infix:<==>' => ' == ',
        'infix:<!=>' => ' != ',
        'infix:<+>'  => ' + ',
        'infix:<->'  => ' - ',
        'infix:<*>'  => ' * ',
        'infix:</>'  => ' / ',
        'infix:<%>'  => ' % ',
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
    # these operators always return "bool"
    our %op_to_bool = map +($_ => 1), qw(
        prefix:<!>
        infix:<!=>
        infix:<==>
        infix:<<=>
        infix:<>=>
        infix:<>>
        infix:<<>
        infix:<eq>
        infix:<ne>
        infix:<ge>
        infix:<le>
    );

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
            my $level = shift;
            my $wantarray = 'scalar';

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
                return $cond->emit_javascript($level, $wantarray);
            }
            else {
                return 'p5str(' . $cond->emit_javascript($level, $wantarray) . ')';
            }
    }
    sub to_num {
            my $cond = shift;
            my $level = shift;
            my $wantarray = 'scalar';
            if ($cond->isa( 'Perlito5::AST::Val::Int' ) || $cond->isa( 'Perlito5::AST::Val::Num' )) {
                return $cond->emit_javascript($level, $wantarray);
            }
            else {
                return 'num(' . $cond->emit_javascript($level, $wantarray) . ')';
            }
    }
    sub to_bool {
            my $cond = shift;
            my $level = shift;
            my $wantarray = 'scalar';

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
                || ($cond->isa( 'Perlito5::AST::Apply' ) && exists $op_to_bool{ $cond->code })
                )
            {
                return $cond->emit_javascript($level, $wantarray);
            }
            else {
                return 'bool(' . $cond->emit_javascript($level, $wantarray) . ')';
            }
    }

    sub to_list {
        my $items = to_list_preprocess( $_[0] );
        my $level = $_[1];
        my $wantarray = 'list';

        @$items
        ? 'interpolate_array('
          .   join(', ', map( $_->emit_javascript($level, $wantarray), @$items ))
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

    sub to_scalar {
        my $items = to_scalar_preprocess( $_[0] );
        my $level = $_[1];
        my $wantarray = 'scalar';

        # Note: v = 1,2,5  // 5

        @$items
        ?   '('
          .   join(', ', map( $_->emit_javascript($level, $wantarray), @$items ))
          . ')'
        : 'null'
    }

    sub to_scalar_preprocess {
        my @items;
        for my $item ( @{$_[0]} ) {
            if (  $item->isa( 'Perlito5::AST::Apply' ) 
               && ( $item->code eq 'list:<,>' || $item->code eq 'infix:<=>>' )
               )
            {
                for my $arg ( @{ to_scalar_preprocess($item->arguments) } ) {
                    push( @items, $arg);
                }
            }
            else {
                push( @items, $item);
            }
        }
        return \@items;
    }

    sub to_runtime_context {
        my $items = to_scalar_preprocess( $_[0] );
        my $level = $_[1];
        my $wantarray = 'runtime';

        'p5context(' 
            . '['
            .   join(', ', map( $_->emit_javascript($level, $wantarray), @$items ))
            . ']'
            . ', p5want)'
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
                if (  $decl->isa( 'Perlito5::AST::Decl' ) && $decl->decl eq $type
                   || $decl->isa( 'Perlito5::AST::Apply' ) && $decl->code eq $type
                   )
                {
                    return 1;
                }
                if ($decl->isa( 'Perlito5::AST::Apply' ) && $decl->code eq 'infix:<=>') {
                    my $var = $decl->arguments[0];
                    if (  $var->isa( 'Perlito5::AST::Decl' ) && $var->decl eq $type
                       || $decl->isa( 'Perlito5::AST::Apply' ) && $decl->code eq $type
                       ) 
                    {
                        return 1;
                    }
                }
            }
        }
        return 0;
    }


    sub emit_javascript {
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
                $Perlito5::VAR->[0]{'$_'} = { decl => 'our', namespace => $Perlito5::PKG_NAME };
                $Perlito5::VAR->[0]{'$a'} = { decl => 'our', namespace => $Perlito5::PKG_NAME };
                $Perlito5::VAR->[0]{'$b'} = { decl => 'our', namespace => $Perlito5::PKG_NAME };
            }

            if ($decl->isa( 'Perlito5::AST::Decl' )) {
                push @str, $decl->emit_javascript_init;
            }
            # TODO - local, our
            if ($decl->isa( 'Perlito5::AST::Apply' ) && $decl->code eq 'my' ) {
                for (@{$decl->{"arguments"}}) {
                    if ($_->isa( 'Perlito5::AST::Var' )) {
                        my $d = Perlito5::AST::Decl->new( decl => $decl->code, var => $_ );
                        push @str, $d->emit_javascript_init;
                    }
                }
            }
            if ($decl->isa( 'Perlito5::AST::Apply' ) && $decl->code eq 'infix:<=>') {
                my $arg = $decl->{"arguments"}[0];
                if ($arg->isa( 'Perlito5::AST::Decl' )) {
                    push @str, $arg->emit_javascript_init;
                }
                # TODO - local, our
                if ($arg->isa( 'Perlito5::AST::Apply' ) && $arg->code eq 'my' ) {
                    for (@{$arg->{"arguments"}}) {
                        if ($_->isa( 'Perlito5::AST::Var' )) {
                            my $d = Perlito5::AST::Decl->new( decl => $arg->code, var => $_ );
                            push @str, $d->emit_javascript_init;
                        }
                    }
                }
            }

            if (!( $decl->isa( 'Perlito5::AST::Decl' ) && $decl->decl eq 'my' )) {
                push @str, $decl->emit_javascript($level, 'void') . ';';
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
                        .   $body->emit_javascript($level+1) . "\n"
                        . Perlito5::Javascript::tab($level) . '}';
                if ($otherwise) {
                    $otherwise = Perlito5::Javascript::LexicalBlock->new( block => $otherwise->stmts, needs_return => 1 );
                    push @str, "\n"
                        . Perlito5::Javascript::tab($level) . 'else {' . "\n"
                        .   $otherwise->emit_javascript($level+1) . "\n"
                        . Perlito5::Javascript::tab($level) . '}';
                }
            }
            elsif ( $last_statement->isa( 'Perlito5::AST::Lit::Block' ) ) {
                my $body = Perlito5::Javascript::LexicalBlock->new( block => $last_statement->{"stmts"}, needs_return => 1 );
                push @str,
                      'for (var i_ = 0; i_ < 1 ; i_++) {' . "\n"
                    .   $body->emit_javascript( $level + 1 ) . "\n"
                    . Perlito5::Javascript::tab($level) . '}'
            }
            elsif (  $last_statement->isa( 'Perlito5::AST::For' )
                  || $last_statement->isa( 'Perlito5::AST::While' )
                  || $last_statement->isa( 'Perlito5::AST::Apply' ) && $last_statement->code eq 'goto'
                  || $last_statement->isa( 'Perlito5::AST::Apply' ) && $last_statement->code eq 'return'
                  )
            {
                push @str, $last_statement->emit_javascript($level, 'runtime');
            }
            else {
                if ( $has_local ) {
                    push @str, 'return cleanup_local(local_idx, (' . Perlito5::Javascript::to_runtime_context([$last_statement]) . '));';
                }
                else {
                    push @str, 'return (' . Perlito5::Javascript::to_runtime_context([$last_statement]) . ');';
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
        my $level = $_[1];
        my $str = "(function () {\n"
            .   Perlito5::Javascript::LexicalBlock->new( block => $self->{"body"}, needs_return => 0 )->emit_javascript( $level + 1 ) . "\n"
            . Perlito5::Javascript::tab($level) . "})()\n";
        return $str;
    }
    sub emit_javascript_program {
        my $comp_units = shift;
        $Perlito5::PKG_NAME = 'main';
        my $str = ''
                .  "var p5want = null;\n"
                .  "var " . Perlito5::Javascript::pkg . " = NAMESPACE['" . $Perlito5::PKG_NAME . "'];\n";
        $Perlito5::VAR = [
            { '@_'    => { decl => 'my' }, # XXX
              '$@'    => { decl => 'our', namespace => 'main' },
              '$^O'   => { decl => 'our', namespace => 'main' },
              '%ENV'  => { decl => 'our', namespace => 'main' },
              '%INC'  => { decl => 'our', namespace => 'main' },
              '@#'    => { decl => 'our', namespace => 'main' },
              '@ARGV' => { decl => 'our', namespace => 'main' },
              '@INC'  => { decl => 'our', namespace => 'main' },
              '$_'    => { decl => 'our', namespace => $Perlito5::PKG_NAME },
              '$a'    => { decl => 'our', namespace => $Perlito5::PKG_NAME },
              '$b'    => { decl => 'our', namespace => $Perlito5::PKG_NAME },
            }
        ];
        for my $comp_unit ( @$comp_units ) {
            $str = $str . $comp_unit->emit_javascript() . "\n";
        }
        return $str;
    }
}

package Perlito5::AST::Val::Int;
{
    sub emit_javascript {
        my $self  = shift;
        my $level = shift;
        $self->{"int"};
    }
}

package Perlito5::AST::Val::Num;
{
    sub emit_javascript {
        my $self  = shift;
        my $level = shift;
        $self->{"num"};
    }
}

package Perlito5::AST::Val::Buf;
{
    sub emit_javascript {
        my $self  = shift;
        my $level = shift;
        Perlito5::Javascript::escape_string( $self->{"buf"} );
    }
}

package Perlito5::AST::Lit::Block;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;
        my $body = Perlito5::Javascript::LexicalBlock->new( block => $self->{"stmts"}, needs_return => 0 );
        return
              'for (var i_ = 0; i_ < 1 ; i_++) {' . "\n"
            .   $body->emit_javascript( $level + 1 ) . "\n"
            . Perlito5::Javascript::tab($level) . '}'
    }
}

package Perlito5::AST::Index;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;

        if (  $self->{"obj"}->isa('Perlito5::AST::Var')
           && $self->{"obj"}->sigil eq '$'
           )
        {
            my $v = Perlito5::AST::Var->new( sigil => '@', namespace => $self->{"obj"}->namespace, name => $self->{"obj"}->name );
            return $v->emit_javascript($level) . '[' . $self->{"index_exp"}->emit_javascript() . ']';
        }

          '('
        .   $self->{"obj"}->emit_javascript() 
        .   ' || (' . $self->{"obj"}->emit_javascript() . ' = new ArrayRef([]))'
        . ')._array_[' . $self->{"index_exp"}->emit_javascript() . ']';
    }
}

package Perlito5::AST::Lookup;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;
        # my $var = $self->{"obj"}->emit_javascript;
        # return $var . '[' . $self->{"index_exp"}->emit_javascript() . ']'

        if (  $self->{"obj"}->isa('Perlito5::AST::Var')
           && $self->{"obj"}->sigil eq '$'
           )
        {
            my $v = Perlito5::AST::Var->new( sigil => '%', namespace => $self->{"obj"}->namespace, name => $self->{"obj"}->name );
            return $v->emit_javascript($level) . '[' . $self->{"index_exp"}->emit_javascript() . ']';
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

    sub emit_javascript {
        my $self = shift;
        my $level = shift;
        my $wantarray = shift;

        my $perl5_name = $self->perl5_name;
        # say "looking up $perl5_name";
        my $decl_type;  # my, our, local
        my $decl = $self->perl5_get_decl( $perl5_name );
        if ( $decl ) {
            # say "found ", $decl->{"decl"};
            $decl_type = $decl->{"decl"};
        }
        else {
            if ( !$self->{"namespace"}
               && $self->{"sigil"} ne '*' 
               )
            {
                if ( $Perlito5::STRICT ) {
                    die "Global symbol \"$perl5_name\" requires explicit package name"
                }
                # no strict - "auto-declare" the var
                $decl_type = 'our';
                $self->{"namespace"} = $Perlito5::PKG_NAME;



                my $sigil = $self->{"sigil"} eq '$#' ? '@' : $self->{"sigil"};
                my $s = 'NAMESPACE["' . $self->{"namespace"} . '"]["' . $table->{$sigil} . $self->{"name"} . '"]';

                if ($sigil eq '@') {
                    $s = $s . ' || (' . $s . ' = [])';  # init
                    $s = 'NAMESPACE[' . $s . ', "' . $self->{"namespace"} . '"]["' . $table->{$sigil} . $self->{"name"} . '"]';
                }

                if ($self->{"sigil"} eq '$#') {
                    return '(' . $s . '.length - 1)';
                }
                return $s;


            }
        }

        if ( $self->{"sigil"} eq '@' ) {
            if ( $wantarray eq 'scalar' ) {
                return $self->emit_javascript($level, 'list') . '.length';
            }
            if ( $wantarray eq 'runtime' ) {
                return '(p5want'
                    . ' ? ' . $self->emit_javascript($level, 'list')
                    . ' : ' . $self->emit_javascript($level, 'list') . '.length'
                    . ')';
            }
        }

        if ( $self->{"sigil"} eq '&' ) {
            return 'NAMESPACE["' . ($self->{"namespace"} || $Perlito5::PKG_NAME) . '"]["' . $self->{"name"} . '"]';
        }
        if ( $self->{"sigil"} eq '*' ) {
            return 'NAMESPACE["' . ($self->{"namespace"} || $Perlito5::PKG_NAME) . '"]["' . $self->{"name"} . '"]';
        }
        if ( $decl_type eq 'our' ) {

            my $sigil = $self->{"sigil"} eq '$#' ? '@' : $self->{"sigil"};
            my $s = 'NAMESPACE["' . ($self->{"namespace"} || $decl->{"namespace"}) . '"]["' . $table->{$sigil} . $self->{"name"} . '"]';

            if ($self->{"sigil"} eq '$#') {
                return '(' . $s . '.length - 1)';
            }
            return $s;
        }

        my $ns = '';
        if ($self->{"namespace"}) {
            $ns = 'NAMESPACE["' . $self->{"namespace"} . '"]';
            if ($self->{"sigil"} eq '$#') {
                return '(' . $ns . '["' . $table->{'@'} . $self->{"name"} . '"].length - 1)';
            }
            return $ns . '["' . $table->{$self->{"sigil"}} . $self->{"name"} . '"]'
        }

        if ($self->{"sigil"} eq '$#') {
            return '(' . $ns . $table->{'@'} . $self->{"name"} . '.length - 1)';
        }

        $ns . $table->{$self->{"sigil"}} . $self->{"name"}
    }
    sub perl5_name {
        my $self = shift;

        my $sigil = $self->{'sigil'};
        $sigil = '@' if $sigil eq '$#';

        $sigil
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
    sub emit_javascript {
        my $self = shift;
        my $level = shift;
        $self->{"var"}->emit_javascript( $level );
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
            # TODO - add grammar support
            # if ($self->var->isa("Lookup")) {
            #     return 
            #         'set_local(' . $self->var->{"obj"}->emit_javascript() . ', '
            #                      . $self->var->{"index_exp"}->emit_javascript() . ', '
            #                      . '""); '
            #         . $self->{"var"}->emit_javascript( $level );
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
    sub emit_javascript {
        my $self = shift;
        my $level = shift;
        return Perlito5::Javascript::pkg()
            if $self->{"name"} eq '__PACKAGE__';
        'NAMESPACE["' . $self->{"name"} . '"]'
    }
}

package Perlito5::AST::Call;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;
        my $wantarray = shift;

        my $invocant = $self->{"invocant"}->emit_javascript;
        my $meth = $self->{"method"};

        if ( $meth eq 'postcircumfix:<[ ]>' ) {
            return 
                  '('
                .   $invocant 
                .   ' || (' . $invocant . ' = new ArrayRef([]))'
                . ')._array_[' . $self->{"arguments"}->emit_javascript($level, 'list') . ']';
        }
        if ( $meth eq 'postcircumfix:<{ }>' ) {
            return
                  '('
                .   $invocant 
                .   ' || (' . $invocant . ' = new HashRef({}))'
                . ')._hash_[' . $self->{"arguments"}->emit_javascript($level, 'list') . ']';
        }
        if  ($meth eq 'postcircumfix:<( )>')  {
            return '(' . $invocant . ')(' . Perlito5::Javascript::to_list($self->{"arguments"}) . ')';
        }
        if ( ref($meth) eq 'Perlito5::AST::Var' ) {
            $meth = $meth->emit_javascript();
        }
        else {
            $meth = '"' . $meth . '"';
        }
        return '_call_(' . $invocant . ', ' 
                         . $meth . ', ' 
                         . Perlito5::Javascript::to_list($self->{"arguments"}) . ', '
                         .   ($wantarray eq 'list'   ? '1' 
                             :$wantarray eq 'scalar' ? '0' 
                             :$wantarray eq 'void'   ? 'null'
                             :                         'p5want'
                             ) 
                  . ')'
    }
}

package Perlito5::AST::Apply;
{

    sub emit_regex_javascript {
        my $op = shift;
        my $var = shift;
        my $regex = shift;

        my $str;
        my $code = $regex->{"code"};
        my $regex_args = $regex->{"arguments"};
        if ($code eq 'p5:s') {
            $str = $var->emit_javascript() 
                 . ' = p5str(' . $var->emit_javascript() . ').replace(/' . $regex_args->[0]->{"buf"} . '/' . $regex_args->[2] . ', '
                 .  $regex_args->[1]->emit_javascript() . ')';
        }
        elsif ($code eq 'p5:m') {
            $str = '(p5str(' . $var->emit_javascript() . ').match(/' . $regex_args->[0]->{"buf"} . '/' . $regex_args->[1] . ')'
                    . ' ? 1 : 0)';
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


    sub emit_function_javascript {
        my $self  = shift;
        my $level = shift;
        my $wantarray = shift;
        my $argument = shift;

        if ( $argument->isa( 'Perlito5::AST::Apply' ) && $argument->code eq 'return' ) {
            return 'function () { ' . $argument->emit_javascript($level, $wantarray) . ' }';
        }
        return 'function () { return ' . $argument->emit_javascript($level, $wantarray) . ' }';
    }


    sub emit_javascript {
        my $self  = shift;
        my $level = shift;
        my $wantarray = shift;

        my $apply = $self->op_assign();
        if ($apply) {
            return $apply->emit_javascript( $level );
        }

        my $code = $self->{"code"};

        if (ref $code ne '') {
            my @args = ();
            push @args, $_->emit_javascript
                for @{$self->{"arguments"}};
            return '(' . $self->{"code"}->emit_javascript( $level ) . ')(' . join(',', @args) . ')';
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


        if ($code eq '__PACKAGE__') {
            return '"' . $Perlito5::PKG_NAME . '"';
        }
        if ($code eq 'wantarray') {
            return 'p5want';
        }
        if ($code eq 'package') {
            return "var " . Perlito5::Javascript::pkg() . ' = make_package("' . $self->{"namespace"} . '")'
        }
        if ($code eq 'infix:<=>>') {
            return join(', ', map( $_->emit_javascript( $level ), @{$self->{"arguments"}} ))
        }
        if (exists $Perlito5::Javascript::op_infix_js_str{$code}) {
            return '(' 
                . join( $Perlito5::Javascript::op_infix_js_str{$code}, map( Perlito5::Javascript::to_str($_), @{$self->{"arguments"}} ))
                . ')'
        }
        if (exists $Perlito5::Javascript::op_infix_js_num{$code}) {
            return '(' 
                . join( $Perlito5::Javascript::op_infix_js_num{$code}, map( Perlito5::Javascript::to_num($_), @{$self->{"arguments"}} ))
                . ')'
        }
        if (exists $Perlito5::Javascript::op_prefix_js_str{$code}) {
            return $Perlito5::Javascript::op_prefix_js_str{$code} . '(' 
                . Perlito5::Javascript::to_str($self->{"arguments"}[0])
                . ')'
        }

        if ($code eq 'infix:<cmp>') {
            return 'p5cmp(' 
                . join( ', ', map( Perlito5::Javascript::to_str($_), @{$self->{"arguments"}} ))
                . ')'
        }
        if ($code eq 'infix:<<=>>') {
            return 'p5cmp(' 
                . join( ', ', map( Perlito5::Javascript::to_num($_), @{$self->{"arguments"}} ))
                . ')'
        }

        if ( $code eq 'prefix:<!>' ) {
            return '!( ' . Perlito5::Javascript::to_bool( $self->{"arguments"}->[0] ) . ')';
        }
        if ( $code eq 'prefix:<~>' ) {
            return '~( ' . Perlito5::Javascript::to_num( $self->{"arguments"}->[0] ) . ')';
        }
        if ( $code eq 'prefix:<->' ) {
            return '-( ' . $self->{"arguments"}->[0]->emit_javascript($level, 'scalar') . ')';
        }
        if ($code eq 'prefix:<+>') { 
            return '('  . $self->{"arguments"}->[0]->emit_javascript($level, $wantarray)  . ')' 
        }

        if ($code eq 'do') {
            # Note: this is "do EXPR" - look at the "Do" AST node for "do BLOCK"
            my $ast =
                Perlito5::AST::Apply->new(
                    code => 'eval',
                    namespace => '',
                    arguments => [
                       Perlito5::AST::Apply->new(
                          code => 'slurp',
                          namespace => 'Perlito5::IO',
                          arguments => $self->{"arguments"}
                        )
                    ]
                );
            return $ast->emit_javascript( $level );
        }

        if ($code eq 'eval') {
            $Perlito5::THROW = 1;   # we can return() from inside eval

            my $arg = $self->{"arguments"}->[0];
            my $eval;
            if ($arg->isa( "Perlito5::AST::Do" )) {
                $eval = $arg->emit_javascript( $level + 1, $wantarray );
            }
            else {
                my $var_env_perl5 = Perlito5::Dumper::Dumper( $Perlito5::VAR );
                # say "at eval: ", $var_env_perl5;
                my $m = Perlito5::Expression->term_square( $var_env_perl5, 0 );
                $m = Perlito5::Expression::expand_list( $m->flat()->[2] );
                # say Perlito5::Dumper::Dumper( $m );
                my $var_env_js = '(new ArrayRef(' . Perlito5::Javascript::to_list($m) . '))';
                $eval ='eval(perl5_to_js(' 
                            . Perlito5::Javascript::to_str($arg) . ", "
                            . '"' . $Perlito5::PKG_NAME . '", '
                            . $var_env_js
                        . "))";
            }

            # TODO - test return() from inside eval

            return
                "(function () {\n"
                    . "var r = null;\n"
                    . 'NAMESPACE["main"]["v_@"] = "";' . "\n"
                    . "try {\n"
                        . 'r = ' . $eval . "\n"
                    . "}\n"
                    . "catch(err) {\n"
                    .    "if ( err instanceof p5_error ) {\n"
                    .    "}\n"
                    .    "else if ( err instanceof Error ) {\n"
                    .        'NAMESPACE["main"]["v_@"] = err;' . "\n"
                    .    "}\n"
                    .    "else {\n"
                    .        "throw(err);\n"   # return() value
                    .    "}\n"
                    . "}\n"
                    . "return r;\n"
                . "})()"

        }

        if ($code eq 'undef')      {
            if ( $self->{"arguments"} && @{$self->{"arguments"}} ) {
                return '(' . $self->{"arguments"}->[0]->emit_javascript . ' = null)'
            }
            return 'null'
        }

        if ($code eq 'defined')    { return '('  . join(' ', map( $_->emit_javascript( $level ), @{$self->{"arguments"}} ))    . ' != null)' }

        if ($code eq 'shift')      {
            if ( $self->{"arguments"} && @{$self->{"arguments"}} ) {
                return Perlito5::Javascript::pkg() . '.shift([' . join(', ', map( $_->emit_javascript( $level ), @{$self->{"arguments"}} )) . '])'
            }
            return Perlito5::Javascript::pkg() . '.shift([List__])'
        }

        if ($code eq 'map') {
            my @in  = @{$self->{"arguments"}};
            my $fun  = shift @in;
            my $list = Perlito5::Javascript::to_list(\@in);

            if (ref($fun) eq 'Perlito5::AST::Lit::Block') {
                $fun = $fun->{'stmts'}
            }
            else {
                $fun = [$fun];
            }

            return 'p5map(' . Perlito5::Javascript::pkg() . ', '

                    . 'function (p5want) {' . "\n"
                    .   (Perlito5::Javascript::LexicalBlock->new( block => $fun, needs_return => 1, top_level => 0 ))->emit_javascript( $level + 1 ) . "\n"
                    . Perlito5::Javascript::tab($level) . '}, '

                    .   $list
                    . ')';
        }
        if ($code eq 'grep') {
            my @in  = @{$self->{"arguments"}};
            my $fun  = shift @in;
            my $list = Perlito5::Javascript::to_list(\@in);

            if (ref($fun) eq 'Perlito5::AST::Lit::Block') {
                $fun = $fun->{'stmts'}
            }
            else {
                $fun = [$fun];
            }

            return 'p5grep(' . Perlito5::Javascript::pkg() . ', '

                    . 'function (p5want) {' . "\n"
                    .   (Perlito5::Javascript::LexicalBlock->new( block => $fun, needs_return => 1, top_level => 0 ))->emit_javascript( $level + 1 ) . "\n"
                    . Perlito5::Javascript::tab($level) . '}, '

                    .   $list
                    . ')';
        }
        if ($code eq 'sort') {
            my @in  = @{$self->{"arguments"}};
            my $fun;
            my $list;

            if (ref($in[0]) eq 'Perlito5::AST::Lit::Block') {
                # the sort function is optional
                $fun = shift @in;
                $fun =
                      'function (p5want) {' . "\n"
                    .   (Perlito5::Javascript::LexicalBlock->new( block => $fun->{'stmts'}, needs_return => 1, top_level => 0 ))->emit_javascript( $level + 1 ) . "\n"
                    . Perlito5::Javascript::tab($level) . '}'
            }
            else {
                $fun = 'null';
            }
            $list = Perlito5::Javascript::to_list(\@in);

            return 'p5sort(' . Perlito5::Javascript::pkg() . ', '
                    .   $fun . ', '
                    .   $list
                    . ')';
        }

        if ( $code eq 'prefix:<$>' ) {
            my $arg = $self->{"arguments"}->[0];
            return '(' . $arg->emit_javascript . ')._scalar_';
        }
        if ( $code eq 'prefix:<@>' ) {
            my $arg = $self->{"arguments"}->[0];
            return
                  '('
                .   $arg->emit_javascript( $level ) 
                .   ' || (' . $arg->emit_javascript( $level ) . ' = new ArrayRef([]))'
                . ')._array_';
        }
        if ( $code eq 'prefix:<$#>' ) {
            my $arg = $self->{"arguments"}->[0];
            return
                  '(('
                .   $arg->emit_javascript( $level ) 
                .   ' || (' . $arg->emit_javascript( $level ) . ' = new ArrayRef([]))'
                . ')._array_.length - 1)';
        }
        if ( $code eq 'prefix:<%>' ) {
            my $arg = $self->{"arguments"}->[0];
            return '(' . $arg->emit_javascript( $level ) . ')._hash_';
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
                    return '(new ArrayRef(' . $arg->emit_javascript( $level ) . '))';
                }
                if ( $arg->sigil eq '%' ) {
                    return '(new HashRef(' . $arg->emit_javascript( $level ) . '))';
                }
                if ( $arg->sigil eq '&' ) {
                    if ($arg->{"namespace"}) {
                        return 'NAMESPACE["' . $arg->{"namespace"} . '"].' . $arg->{"name"};
                    }
                    else {
                        return Perlito5::Javascript::pkg() . '.' . $arg->{"name"};
                    }
                }
            }
            return '(new ScalarRef(' . $arg->emit_javascript( $level ) . '))';
        }

        if ($code eq 'postfix:<++>') { return '('   . join(' ', map( $_->emit_javascript, @{$self->{"arguments"}} ))  . ')++' }
        if ($code eq 'postfix:<-->') { return '('   . join(' ', map( $_->emit_javascript, @{$self->{"arguments"}} ))  . ')--' }
        if ($code eq 'prefix:<++>')  { return '++(' . join(' ', map( $_->emit_javascript, @{$self->{"arguments"}} ))  . ')' }
        if ($code eq 'prefix:<-->')  { return '--(' . join(' ', map( $_->emit_javascript, @{$self->{"arguments"}} ))  . ')' }

        if ($code eq 'infix:<x>')  { return 'str_replicate(' . join(', ', map( $_->emit_javascript, @{$self->{"arguments"}} ))  . ')' }

        if ($code eq 'list:<.>') { 
            return '('  
                . join( ' + ',
                        map( Perlito5::Javascript::to_str($_), @{$self->{"arguments"}} )
                      )
                . ')' 
        }
        if ($code eq 'list:<,>') { 
            return Perlito5::Javascript::to_list($self->{"arguments"}) 
        }

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
                . $self->{"arguments"}->[0]->emit_javascript($level, $wantarray) . ', '
                . $self->emit_function_javascript($level, $wantarray, $self->{"arguments"}->[1]) 
                . ')'
        }
        if (  $code eq 'infix:<||>'
           || $code eq 'infix:<or>'
           )
        {
            return 'or' . '('
                . $self->{"arguments"}->[0]->emit_javascript($level, $wantarray) . ', '
                . $self->emit_function_javascript($level, $wantarray, $self->{"arguments"}->[1]) 
                . ')'
        }
        if ($code eq 'infix:<//>') { return 'defined_or' . '('
                . $self->{"arguments"}->[0]->emit_javascript($level, $wantarray) . ', '
                . $self->emit_function_javascript($level, $wantarray, $self->{"arguments"}->[1]) 
                . ')'
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
        if ($code eq 'ternary:<? :>') {
            return
                   '( ' . Perlito5::Javascript::to_bool( $self->{"arguments"}->[0] )
                 . ' ? ' . ($self->{"arguments"}->[1])->emit_javascript($level, $wantarray)
                 . ' : ' . ($self->{"arguments"}->[2])->emit_javascript($level, $wantarray)
                 . ')'
        }
        if ($code eq 'my') {
            # TODO - bug: this is a side-effect of my($x,$y)
            return 'p5context('
                .   '[' . join(', ', map( $_->emit_javascript( $level, $wantarray ), @{$self->{"arguments"}} )) . '], ' 
                .   ($wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0)
                . ')';
        }
        if ($code eq 'circumfix:<( )>') {
            return 'p5context('
                .   '[' . join(', ', map( $_->emit_javascript( $level, $wantarray ), @{$self->{"arguments"}} )) . '], ' 
                .   ($wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0)
                . ')';
        }
        if ($code eq 'infix:<=>') {
            return emit_javascript_bind( $self->{"arguments"}->[0], $self->{"arguments"}->[1], $level );
        }
        if ($code eq 'return') {
            $Perlito5::THROW = 1;
            return 'throw('
                .   Perlito5::Javascript::to_runtime_context( $self->{"arguments"} )
                . ')'
        }
        if ($code eq 'goto') {
            $Perlito5::THROW = 1;
            return 'throw((' . $self->{"arguments"}->[0]->emit_javascript() . ')([List__, p5want]))'
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
            $code = Perlito5::Javascript::pkg() . '.' . $code
        }

        my $sig;
        {
            my $name = $self->{"code"};+            my $namespace = $self->{"namespace"};
            my $effective_name = $self->{"code"} . "::" . $self->{"namespace"};
            if ( exists $Perlito5::PROTO->{$effective_name} ) {
                $sig = $Perlito5::PROTO->{$effective_name};
            }
            elsif ( (!$namespace || $namespace eq 'CORE')
                  && exists $Perlito5::CORE_PROTO->{"CORE::$name"}
                  )
            {
                $effective_name = "CORE::$name";
                $sig = $Perlito5::CORE_PROTO->{$effective_name};
            }
        }

        if ($sig) {
            # warn "sig $effective_name $sig\n";
            my @out = ();
            my @in  = @{$self->{"arguments"} || []};

            # TODO - generate the right prototype


            if ( $sig eq '\\@@' ) {         # push, unshift
                push @out, shift(@in)->emit_javascript( $level, 'list' );
                push @out, Perlito5::Javascript::to_list(\@in);
            }
            elsif ( $sig eq '\\[@%]'        # keys, values, each
                ||  $sig eq ';\\@' ) {      # pop, shift
                push @out, shift(@in)->emit_javascript( $level, 'list' );
            }
            elsif ( $sig eq '\\@;$$@'       # splice
                ) {
                push @out, shift(@in)->emit_javascript( $level, 'list' );
                push @out, shift(@in)->emit_javascript( $level, 'scalar' ) if @in;
                push @out, shift(@in)->emit_javascript( $level, 'scalar' ) if @in;
                push @out, Perlito5::Javascript::to_list(\@in) if @in;
            }
            elsif ( $sig eq '$@' ) {        # join
                push @out, shift(@in)->emit_javascript( $level, 'scalar' );
                push @out, Perlito5::Javascript::to_list(\@in);
            }
            elsif ( $sig eq '@' ) {         # warn
                push @out, Perlito5::Javascript::to_list(\@in);
            }
            else {
                # just a list of scalars:
                #   bless      $;$ 
                #   substr     $$;$$ 
                #   length     _ 
                #   index      $$;$ 
                push @out, $_->emit_javascript( $level, 'scalar' )
                    for @in;
            }
            return $code . '([' . join(', ', @out) . '], '
                .   ($wantarray eq 'list'   ? '1' 
                    :$wantarray eq 'scalar' ? '0' 
                    :$wantarray eq 'void'   ? 'null'
                    :                         'p5want'
                    ) 
                . ')';
        }

        my @args = ();
        my $arg_list = Perlito5::Javascript::to_list_preprocess( $self->{"arguments"} );
        push @args, $_->emit_javascript( $level )
            for @$arg_list;

        my $arg_code = 
            $self->{"code"} eq 'scalar'      # scalar() is special
            ? '[' . join(', ', @args) . ']'
            : Perlito5::Javascript::to_list($arg_list);

        $code . '('
                . $arg_code . ', '
                .   ($wantarray eq 'list'   ? '1' 
                    :$wantarray eq 'scalar' ? '0' 
                    :$wantarray eq 'void'   ? 'null'
                    :                         'p5want'
                    ) 
              . ')';

    }

    sub emit_javascript_bind {
        my $parameters = shift;
        my $arguments = shift;
        my $level = shift;

        if (   $parameters->isa( 'Perlito5::AST::Apply' )
           &&  ( $parameters->code eq 'my' || $parameters->code eq 'circumfix:<( )>' )
           )
        {
            # my ($x, $y) = ...
            # ($x, $y) = ...

            my $tmp = 'tmp' . Perlito5::Javascript::get_label();
            return
              '(function () { '
                . 'var ' . $tmp . ' = ' . Perlito5::Javascript::to_list([$arguments]) . '; '
                . join( '; ',
                        map +( $_->isa('Perlito5::AST::Apply') && $_->code eq 'undef'
                             ? $tmp . '.shift()' 
                             : $_->sigil eq '$' 
                             ? $_->emit_javascript() . ' = ' . $tmp . '.shift()'
                             : $_->sigil eq '@' 
                             ? $_->emit_javascript() . ' = ' . $tmp . '; ' . $tmp . ' = []'
                             : $_->sigil eq '%' 
                             ? $_->emit_javascript() . ' = array_to_hash(' . $tmp . '); ' . $tmp . ' = []'
                             : die("not implemented")
                             ),
                             @{ $parameters->arguments }
                      )
            . ' })()'
        }

        if (   $parameters->isa( 'Perlito5::AST::Var' )  && $parameters->sigil eq '$'
           ||  $parameters->isa( 'Perlito5::AST::Decl' ) && $parameters->var->sigil eq '$'
           )
        {
            return '(' . $parameters->emit_javascript() . ' = ' . Perlito5::Javascript::to_scalar([$arguments]) . ')'
        }

        if  (   $parameters->isa( 'Perlito5::AST::Var' )  && $parameters->sigil eq '@'
            ||  $parameters->isa( 'Perlito5::AST::Decl' ) && $parameters->var->sigil eq '@'
            )
        {
            return '(' . $parameters->emit_javascript() . ' = ' . Perlito5::Javascript::to_list([$arguments]) . ')'
        }
        elsif ( $parameters->isa( 'Perlito5::AST::Var' )  && $parameters->sigil eq '%'
            ||  $parameters->isa( 'Perlito5::AST::Decl' ) && $parameters->var->sigil eq '%'
            )
        {
            return '(' . $parameters->emit_javascript() . ' = array_to_hash(' . Perlito5::Javascript::to_list([$arguments]) . '))' 
        }
        '(' . $parameters->emit_javascript( $level ) . ' = ' . $arguments->emit_javascript( $level ) . ')';
    }
}

package Perlito5::AST::If;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;
        my $cond = $self->{"cond"};
        my $body  = Perlito5::Javascript::LexicalBlock->new( block => $self->{"body"}->stmts, needs_return => 0, create_context => 1 );
        my $s = 'if ( ' . Perlito5::Javascript::to_bool( $cond ) . ' ) {' . "\n"
            .       $body->emit_javascript( $level + 1 ) . "\n"
            . Perlito5::Javascript::tab($level) . '}';
        if ( @{ $self->{"otherwise"}->stmts } ) {
            my $otherwise = Perlito5::Javascript::LexicalBlock->new( block => $self->{"otherwise"}->stmts, needs_return => 0, create_context => 1 );
            $s = $s
                . "\n"
                . Perlito5::Javascript::tab($level) . 'else {' . "\n"
                .       $otherwise->emit_javascript( $level + 1 ) . "\n"
                . Perlito5::Javascript::tab($level) . '}';
        }
        return $s;
    }
}


package Perlito5::AST::While;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;
        my $body      = Perlito5::Javascript::LexicalBlock->new( block => $self->{"body"}->stmts, needs_return => 0, create_context => 1 );
        return
           'for ( '
        .  ( $self->{"init"}     ? $self->{"init"}->emit_javascript()           . '; '  : '; ' )
        .  ( $self->{"cond"}     ? Perlito5::Javascript::to_bool( $self->{"cond"} )       . '; '  : '; ' )
        .  ( $self->{"continue"} ? $self->{"continue"}->emit_javascript()       . ' '   : ' '  )
        .  ') {' . "\n" 
            . $body->emit_javascript( $level + 1 ) . "\n"
        .  Perlito5::Javascript::tab($level) . '}'
    }
}

package Perlito5::AST::For;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;

        my $cond = Perlito5::Javascript::to_list([$self->{"cond"}]);
        if ($self->{"body"}->sig()) {
            # XXX - cleanup: "for" parser throws away the variable declaration, so we need to create it again
            # TODO - for without "my"

            # mark the variable as "declared"
            my $v = $self->{"body"}->sig;
            $Perlito5::VAR->[0]{ $v->perl5_name } = { decl => 'my' };
            my $sig = $v->emit_javascript( $level + 1 );
            return 'p5for_lex('
                    . "function ($sig) {\n"
                    .   (Perlito5::Javascript::LexicalBlock->new( block => $self->{"body"}->stmts, needs_return => 0, top_level => 0 ))->emit_javascript( $level + 1 ) . "\n"
                    . Perlito5::Javascript::tab($level) . '}, '
                    .   $cond
                    . ')'
        }
        else {
            # use $_
            return 'p5for(' . Perlito5::Javascript::pkg() . ', '
                    . 'function () {' . "\n"
                    .   (Perlito5::Javascript::LexicalBlock->new( block => $self->{"body"}->stmts, needs_return => 0, top_level => 0 ))->emit_javascript( $level + 1 ) . "\n"
                    . Perlito5::Javascript::tab($level) . '}, '
                    .   $cond
                    . ')'
        }
    }
}

package Perlito5::AST::Sub;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;

        my $s =
          'function (List__, p5want) {' . "\n"
        .   (Perlito5::Javascript::LexicalBlock->new( block => $self->{"block"}, needs_return => 1, top_level => 1 ))->emit_javascript( $level + 1 ) . "\n"
        . Perlito5::Javascript::tab($level) . '}';

        if ( $self->{"name"} ) {
            return 'make_sub("' . $self->{"namespace"} . '", "' . $self->{"name"} . '", ' . $s . ')'
        }
        else {
            return $s;
        }
    }
}

package Perlito5::AST::Do;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;
        my $block = $self->simplify->block;
        return
              '(function () {' . "\n"
            .   (Perlito5::Javascript::LexicalBlock->new( block => $block, needs_return => 1 ))->emit_javascript( $level + 1 ) . "\n"
            . Perlito5::Javascript::tab($level) . '})()'
    }
}

package Perlito5::AST::Use;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;
        Perlito5::Grammar::Use::emit_time_eval($self);
        '// ' . $self->{"code"} . ' ' . $self->{"mod"} . "\n"
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
