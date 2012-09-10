use v5;

use Perlito5::AST;
use Perlito5::Dumper;

package Perlito5::Javascript;
{
    my $label_count = 100;
    my %label;
    sub pkg {
        # this is an optimization to reduce the number of lookups
        # this breaks eval() because the variable is not always seen at runtime
        # $label{ $Perlito5::PKG_NAME } ||= "p5" . $label_count++
        'p5pkg["' . $Perlito5::PKG_NAME . '"]'
    }
    sub pkg_new_var {
        $label{ $Perlito5::PKG_NAME } = "p5" . $label_count++
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
        'prefix:<-C>' => 'p5ctime',
        'prefix:<-M>' => 'p5mtime',
        'prefix:<-d>' => 'p5is_directory',
        'prefix:<-e>' => 'p5file_exists',
        'prefix:<-f>' => 'p5is_file',
        'prefix:<-s>' => 'p5size',
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
        prefix:<not>
        exists
        defined
    );
    # these operators always return "string"
    our %op_to_str = map +($_ => 1), qw(
        substr
        join
        list:<.>
        chr
    );
    # these operators always return "num"
    our %op_to_num = map +($_ => 1), qw(
        length
        index
        ord
        oct
        infix:<->
        infix:<+>
        infix:<*>
        infix:</>
        infix:<%>
        infix:<**>
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
               && $cond->{arguments} && @{$cond->{arguments}}
               ) 
            {
                return to_str( $cond->{arguments}[0], $level )
            }

            if  (  ($cond->isa( 'Perlito5::AST::Val::Buf' ))
                || ($cond->isa( 'Perlito5::AST::Apply' )  && exists $op_to_str{ $cond->code } )
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
            if  (  $cond->isa( 'Perlito5::AST::Val::Int' ) 
                || $cond->isa( 'Perlito5::AST::Val::Num' )
                || ($cond->isa( 'Perlito5::AST::Apply' )  && exists $op_to_num{ $cond->code } )
                )
            {
                return $cond->emit_javascript($level, $wantarray);
            }
            else {
                return 'p5num(' . $cond->emit_javascript($level, $wantarray) . ')';
            }
    }
    sub to_bool {
            my $cond = shift;
            my $level = shift;
            my $wantarray = 'scalar';

            if (  $cond->isa( 'Perlito5::AST::Apply' ) && $cond->code eq 'circumfix:<( )>'
               && $cond->{arguments} && @{$cond->{arguments}}
               ) 
            {
                return to_bool( $cond->{arguments}[0], $level )
            }

            # Note: 'infix:<||>' and 'infix:<&&>' can only be optimized here because we know we want "bool"
            if (  $cond->isa( 'Perlito5::AST::Apply' ) 
               && (  $cond->code eq 'infix:<&&>'
                  || $cond->code eq 'infix:<and>'
                  )
               ) 
            {
                return '(' . to_bool($cond->{arguments}->[0], $level) . ' && '
                           . to_bool($cond->{arguments}->[1], $level) . ')'
            }
            if (  $cond->isa( 'Perlito5::AST::Apply' ) 
               && (  $cond->code eq 'infix:<||>'
                  || $cond->code eq 'infix:<or>'
                  )
               ) 
            {
                return '(' . to_bool($cond->{arguments}->[0], $level) . ' || '
                           . to_bool($cond->{arguments}->[1], $level) . ')'
            }

            if  (  ($cond->isa( 'Perlito5::AST::Val::Int' ))
                || ($cond->isa( 'Perlito5::AST::Val::Num' ))
                || ($cond->isa( 'Perlito5::AST::Apply' ) && exists $op_to_bool{ $cond->code })
                )
            {
                return $cond->emit_javascript($level, $wantarray);
            }
            else {
                return 'p5bool(' . $cond->emit_javascript($level, $wantarray) . ')';
            }
    }

    sub is_scalar {
            !$_[0]->isa( 'Perlito5::AST::Val::Int' )
         && !$_[0]->isa( 'Perlito5::AST::Val::Num' )
         && !$_[0]->isa( 'Perlito5::AST::Val::Buf' )
         && !$_[0]->isa( 'Perlito5::AST::Sub' )
         && !($_[0]->isa( 'Perlito5::AST::Var' ) && $_[0]->{sigil} eq '$')
         && !($_[0]->isa( 'Perlito5::AST::Apply' ) 
             && (  exists($op_to_str{ $_[0]->{code} })
                || exists($op_to_num{ $_[0]->{code} })
                || exists($op_to_bool{ $_[0]->{code} })
                || $_[0]->{code} eq 'prefix:<\\>'
                )
             )
    }

    sub to_list {
        my $items = to_list_preprocess( $_[0] );
        my $level = $_[1];
        my $literal_type = $_[2] || 'array';    # 'array', 'hash'

        my $wantarray = 'list';

        my $interpolate = 0;
        for (@$items) {
            $interpolate = 1
                if is_scalar($_);
        }

        if ($literal_type eq 'hash') {
            if (!$interpolate) {
                # { x : y, ... }

                my @out;
                my $printable = 1;
                my @in = @$items;
                while (@in) {
                    my $k = shift @in;
                    my $v = shift @in;
                    $k = $k->emit_javascript($level, 0);

                    $printable = 0
                        if $k =~ /[ \[]/;

                    $v = $v
                         ? $v->emit_javascript($level, 0)
                         : 'null';
                    push @out, "$k : $v";
                }

                return '{' . join(', ', @out) . '}'
                    if $printable;

            }
            return 'p5a_to_h(' . to_list($items, $level, 'array') . ')';
        }

        $interpolate
        ? ( 'p5list_to_a('
          .   join(', ', map( $_->emit_javascript($level, $wantarray), @$items ))
          . ')'
          )
        : ( '['
          .   join(', ', map( $_->emit_javascript($level, $wantarray), @$items ))
          . ']'
          )
    }

    sub to_list_preprocess {
        my @items;
        for my $item ( @{$_[0]} ) {
            if (  $item->isa( 'Perlito5::AST::Apply' ) 
               && ( $item->code eq 'circumfix:<( )>' || $item->code eq 'list:<,>' || $item->code eq 'infix:<=>>' )
               )
            {
                if ($item->isa('Perlito5::AST::Apply')
                   && $item->code eq 'infix:<=>>'
                   )
                {
                    $item->{arguments}[0] = Perlito5::AST::Lookup->autoquote( $item->{arguments}[0] );
                }

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
                if ($item->isa('Perlito5::AST::Apply')
                   && $item->code eq 'infix:<=>>'
                   )
                {
                    $item->{arguments}[0] = Perlito5::AST::Lookup->autoquote( $item->{arguments}[0] );
                }

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

        return $items->[0]->emit_javascript($level, $wantarray)
            if @$items == 1 && is_scalar($items->[0]);

        'p5context(' 
            . '['
            .   join(', ', map( $_->emit_javascript($level, $wantarray), @$items ))
            . ']'
            . ', p5want)'
    }

    sub emit_javascript_autovivify {
        my $obj = shift;
        my $level = shift;
        my $type = shift;  # 'array'/'hash'

          '(' .  $obj->emit_javascript($level)
        .   ' || (' . $obj->emit_javascript($level) . ' = ' 
                    . ( $type eq 'array' ? 'new p5ArrayRef([])' 
                      : $type eq 'hash'  ? 'new p5HashRef({})'
                      :                    'new p5ScalarRef(null)'
                      )
              . ')'
        . ')'
    }

    sub emit_function_javascript {
        my $level = shift;
        my $wantarray = shift;
        my $argument = shift;

        if ( $argument->isa( 'Perlito5::AST::Apply' ) && $argument->code eq 'return' ) {
            return 'function () { ' . $argument->emit_javascript($level, $wantarray) . ' }';
        }
        return 'function () { return ' . $argument->emit_javascript($level, $wantarray) . ' }';
    }

}

package Perlito5::Javascript::LexicalBlock;
{
    sub new { my $class = shift; bless {@_}, $class }
    sub block { $_[0]->{block} }
    sub needs_return { $_[0]->{needs_return} }
    sub top_level { $_[0]->{top_level} }
    # sub create_context ... 

    sub has_decl {
        my $self = $_[0];
        my $type = $_[1];
        for my $decl ( @{$self->{block}} ) {
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
        my $wantarray = shift;

        my @block;
        for (@{$self->{block}}) {
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
        my $create_context = $self->{create_context} && $self->has_decl("my");
        my $outer_pkg   = $Perlito5::PKG_NAME;
        my $outer_throw = $Perlito5::THROW;
        unshift @{ $Perlito5::VAR }, {};

        $Perlito5::THROW = 0
            if $self->{top_level};

        $out .= Perlito5::Javascript::tab($level) . "var local_idx = p5LOCAL.length;\n"
            if $has_local;
        if ($self->{top_level}) {
            $level++;
        }
        if ( $create_context ) {
            $out .= Perlito5::Javascript::tab($level) . "(function () {\n";
            $level++;
        }

        my $tab = Perlito5::Javascript::tab($level);
        my $last_statement;
        if ($self->{needs_return}) {
            $last_statement = pop @block;
        }
        for my $decl ( @block ) {
            if ( ref($decl) eq 'Perlito5::AST::Apply' && $decl->code eq 'package' ) {
                $Perlito5::PKG_NAME = $decl->{namespace};
                $Perlito5::VAR->[0]{'$_'} = { decl => 'our', namespace => $Perlito5::PKG_NAME };
                $Perlito5::VAR->[0]{'$a'} = { decl => 'our', namespace => $Perlito5::PKG_NAME };
                $Perlito5::VAR->[0]{'$b'} = { decl => 'our', namespace => $Perlito5::PKG_NAME };
                $Perlito5::VAR->[0]{'$AUTOLOAD'} = { decl => 'our', namespace => $Perlito5::PKG_NAME };
            }

            if ($decl->isa( 'Perlito5::AST::Decl' )) {
                push @str, $decl->emit_javascript_init;
            }
            # TODO - local, our
            if ($decl->isa( 'Perlito5::AST::Apply' ) && $decl->code eq 'my' ) {
                for (@{$decl->{arguments}}) {
                    if ($_->isa( 'Perlito5::AST::Var' )) {
                        my $d = Perlito5::AST::Decl->new( decl => $decl->code, var => $_ );
                        push @str, $d->emit_javascript_init;
                    }
                }
            }
            if ($decl->isa( 'Perlito5::AST::Apply' ) && $decl->code eq 'infix:<=>') {
                my $arg = $decl->{arguments}[0];
                if ($arg->isa( 'Perlito5::AST::Decl' )) {
                    push @str, $arg->emit_javascript_init;
                }
                # TODO - local, our
                if ($arg->isa( 'Perlito5::AST::Apply' ) && $arg->code eq 'my' ) {
                    for (@{$arg->{arguments}}) {
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

        # TODO - use the context information ($wantarray)

        if ($self->{needs_return} && $last_statement) {

            if ($last_statement->isa( 'Perlito5::AST::Decl' )) {
                push @str, $last_statement->emit_javascript_init;
            }
            if ($last_statement->isa( 'Perlito5::AST::Apply' ) && $last_statement->code eq 'infix:<=>') {
                if ($last_statement->{arguments}[0]->isa( 'Perlito5::AST::Decl' )) {
                    push @str, $last_statement->{arguments}[0]->emit_javascript_init;
                }
            }
            if  (  $last_statement->isa( 'Perlito5::AST::Apply' ) 
                && $last_statement->code eq 'return'
                && $self->{top_level}
                && @{ $last_statement->{arguments} }
                ) 
            {
                $last_statement = $last_statement->{arguments}[0];
            }

            if ($last_statement->isa( 'Perlito5::AST::If' )) {
                my $cond      = $last_statement->cond;
                my $body      = $last_statement->body;
                my $otherwise = $last_statement->otherwise;
                $body         = Perlito5::Javascript::LexicalBlock->new( block => $body->stmts, needs_return => 1 );
                push @str,
                        'if ( ' . Perlito5::Javascript::to_bool( $cond, $level+1 ) . ' ) {' . "\n"
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
                my $body = Perlito5::Javascript::LexicalBlock->new( block => $last_statement->{stmts}, needs_return => 1 );
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
                    push @str, 'return p5cleanup_local(local_idx, (' . Perlito5::Javascript::to_runtime_context([$last_statement], $level) . '));';
                }
                else {
                    push @str, 'return (' . Perlito5::Javascript::to_runtime_context([$last_statement], $level) . ');';
                }
            }
        }
        if ( $has_local ) {
            push @str, 'p5cleanup_local(local_idx, null);';
        }
        if ( $create_context ) {
            $level--;
            push @str, "})();";
        }
        if ($self->{top_level} && $Perlito5::THROW) {

            # TODO - emit error message if catched a "next/redo/last LABEL" when expecting a "return" exception

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
                      ? 'return p5cleanup_local(local_idx, err)'
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
            if $self->{top_level};
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
            .   Perlito5::Javascript::LexicalBlock->new( block => $self->{body}, needs_return => 0 )->emit_javascript( $level + 1 ) . "\n"
            . Perlito5::Javascript::tab($level) . "})()\n";
        return $str;
    }
    sub emit_javascript_program {
        my $comp_units = shift;
        $Perlito5::PKG_NAME = 'main';
        my $str = ''
                .  "var p5want = null;\n"
                .  "var " . Perlito5::Javascript::pkg_new_var() . " = p5pkg['" . $Perlito5::PKG_NAME . "'];\n";
        $Perlito5::VAR = [
            { '@_'    => { decl => 'my' }, # XXX
              '$@'    => { decl => 'our', namespace => 'main' },
              '$|'    => { decl => 'our', namespace => 'main' },
              '$^O'   => { decl => 'our', namespace => 'main' },
              '%ENV'  => { decl => 'our', namespace => 'main' },
              '%INC'  => { decl => 'our', namespace => 'main' },
              '@#'    => { decl => 'our', namespace => 'main' },
              '@ARGV' => { decl => 'our', namespace => 'main' },
              '@INC'  => { decl => 'our', namespace => 'main' },
              '$_'    => { decl => 'our', namespace => $Perlito5::PKG_NAME },
              '$a'    => { decl => 'our', namespace => $Perlito5::PKG_NAME },
              '$b'    => { decl => 'our', namespace => $Perlito5::PKG_NAME },
              '$AUTOLOAD' => { decl => 'our', namespace => $Perlito5::PKG_NAME },
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
        $self->{int};
    }
}

package Perlito5::AST::Val::Num;
{
    sub emit_javascript {
        my $self  = shift;
        my $level = shift;
        $self->{num};
    }
}

package Perlito5::AST::Val::Buf;
{
    sub emit_javascript {
        my $self  = shift;
        my $level = shift;
        Perlito5::Javascript::escape_string( $self->{buf} );
    }
}

package Perlito5::AST::Lit::Block;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;

        my $init = "";
        if ($self->{name} eq 'INIT') {
            my $tmp  = 'p5pkg.main._tmp' . Perlito5::Javascript::get_label();

            # INIT-blocks execute only once
            $init = Perlito5::Javascript::tab($level + 2) . "if ($tmp) { return }; $tmp = 1;\n";

            # TODO - make this execute before anything else

        }

        return 'p5for_lex('
                . "function () {\n"
                .   $init
                .   (Perlito5::Javascript::LexicalBlock->new( block => $self->{stmts}, needs_return => 0, top_level => 0 ))->emit_javascript($level + 2) . "\n"
                . Perlito5::Javascript::tab($level + 1) . '}, '
                .   '[0], '
                . $self->emit_javascript_continue($level) . ', '
                .   '"' . ($self->{label} || "") . '"'
                . ')'
    }
    sub emit_javascript_continue {
        my $self = shift;
        my $level = shift;

        if (!$self->{continue} || !@{ $self->{continue}{stmts} }) {
            return 'false'
        }

        return
              "function () {\n"
            .   (Perlito5::Javascript::LexicalBlock->new( block => $self->{continue}->stmts, needs_return => 0, top_level => 0 ))->emit_javascript($level + 2) . "\n"
            . Perlito5::Javascript::tab($level + 1) . '}'
    }
}

package Perlito5::AST::Index;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;

        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && $self->{obj}->sigil eq '$'
           )
        {
            my $v = Perlito5::AST::Var->new( sigil => '@', namespace => $self->{obj}->namespace, name => $self->{obj}->name );
            return $v->emit_javascript($level) . '[' 
                    . 'p5idx(' 
                        . $v->emit_javascript($level) . ','
                        . Perlito5::Javascript::to_num($self->{index_exp}, $level) 
                    . ')'
                . ']';
        }
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a[0] ==> $a->[0]
            return Perlito5::AST::Call->new(
                    method    => 'postcircumfix:<[ ]>',
                    invocant  => $self->{obj}{arguments}[0],
                    arguments => $self->{index_exp},
                )->emit_javascript($level);
        }

          Perlito5::Javascript::emit_javascript_autovivify( $self->{obj}, $level, 'array' ) . '._array_[' 
                    . 'p5idx(' 
                        . Perlito5::Javascript::emit_javascript_autovivify( $self->{obj}, $level, 'array' ) . '._array_,'
                        . Perlito5::Javascript::to_num($self->{index_exp}, $level) 
                    . ')'
                . ']';
    }
}

package Perlito5::AST::Lookup;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;
        # my $var = $self->{obj}->emit_javascript;
        # return $var . '[' . $self->{index_exp}->emit_javascript() . ']'

        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && $self->{obj}->sigil eq '$'
           )
        {
            my $v = Perlito5::AST::Var->new( sigil => '%', namespace => $self->{obj}->namespace, name => $self->{obj}->name );
            return $v->emit_javascript($level) . '[' . $self->autoquote($self->{index_exp})->emit_javascript($level) . ']';
        }
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a{0} ==> $a->{0}
            return Perlito5::AST::Call->new(
                    method    => 'postcircumfix:<{ }>',
                    invocant  => $self->{obj}{arguments}[0],
                    arguments => $self->{index_exp},
                )->emit_javascript($level);
        }

          Perlito5::Javascript::emit_javascript_autovivify( $self->{obj}, $level, 'hash' )
        . '._hash_[' . $self->autoquote($self->{index_exp})->emit_javascript($level) . ']';
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

        my $str_name = $self->{name};
        $str_name = '\\\\' if $str_name eq '\\';   # escape $\
        $str_name = '\\"' if $str_name eq '"';     # escape $"

        my $perl5_name = $self->perl5_name;
        # say "looking up $perl5_name";
        my $decl_type;  # my, our, local
        my $decl = $self->perl5_get_decl( $perl5_name );
        if ( $decl ) {
            # say "found ", $decl->{decl};
            $decl_type = $decl->{decl};
        }
        else {
            if ( !$self->{namespace}
               && $self->{sigil} ne '*' 
               )
            {
                if ( $Perlito5::STRICT ) {
                    die "Global symbol \"$perl5_name\" requires explicit package name"
                }
                # no strict - "auto-declare" the var
                $decl_type = 'our';
                $self->{namespace} = $Perlito5::PKG_NAME;

                my $sigil = $self->{sigil} eq '$#' ? '@' : $self->{sigil};
                my $s = 'p5pkg["' . $self->{namespace} . '"]["' . $table->{$sigil} . $str_name . '"]';

                if ($sigil eq '@') {
                    $s = $s . ' || (' . $s . ' = [])';  # init
                    $s = 'p5pkg[' . $s . ', "' . $self->{namespace} . '"]["' . $table->{$sigil} . $str_name . '"]';
                    if ( $self->{sigil} eq '@' && $wantarray eq 'scalar' ) {
                        $s .= '.length';
                    }
                }
                elsif ($sigil eq '%') {
                    $s = $s . ' || (' . $s . ' = {})';  # init
                    $s = 'p5pkg[' . $s . ', "' . $self->{namespace} . '"]["' . $table->{$sigil} . $str_name . '"]';
                }

                if ($self->{sigil} eq '$#') {
                    return '(' . $s . '.length - 1)';
                }
                return $s;
            }
        }

        if ( $self->{sigil} eq '@' ) {
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

        if ( $self->{sigil} eq '&' ) {
            return 'p5pkg["' . ($self->{namespace} || $Perlito5::PKG_NAME) . '"]["' . $str_name . '"]';
        }
        if ( $self->{sigil} eq '*' ) {
            return 'p5pkg["' . ($self->{namespace} || $Perlito5::PKG_NAME) . '"]["' . $str_name . '"]';
        }
        if ( $decl_type eq 'our' ) {

            my $sigil = $self->{sigil} eq '$#' ? '@' : $self->{sigil};
            my $s = 'p5pkg["' . ($self->{namespace} || $decl->{namespace}) . '"]["' . $table->{$sigil} . $str_name . '"]';

            if ($self->{sigil} eq '$#') {
                return '(' . $s . '.length - 1)';
            }
            return $s;
        }

        my $ns = '';
        if ($self->{namespace}) {
            $ns = 'p5pkg["' . $self->{namespace} . '"]';
            if ($self->{sigil} eq '$#') {
                return '(' . $ns . '["' . $table->{'@'} . $str_name . '"].length - 1)';
            }
            return $ns . '["' . $table->{$self->{sigil}} . $str_name . '"]'
        }

        if ($self->{sigil} eq '$#') {
            return '(' . $ns . $table->{'@'} . $str_name . '.length - 1)';
        }

        $ns . $table->{$self->{sigil}} . $str_name
    }
    sub perl5_name {
        my $self = shift;

        my $sigil = $self->{sigil};
        $sigil = '@' if $sigil eq '$#';

        $sigil
        . ( $self->{namespace}
          ? $self->{namespace} . '::'
          : ''
          )
        . $self->{name}
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
        $self->{var}->emit_javascript( $level );
    }
    sub emit_javascript_init {
        my $self = shift;

        my $env = { decl => $self->{decl} };
        my $perl5_name = $self->{var}->perl5_name;
        if ( $self->{decl} ne 'my' ) {

            die "No package name allowed for variable $perl5_name in \"our\""
                if $self->{decl} eq 'our' && $self->{var}{namespace};

            if ( $self->{var}{namespace} eq '' ) {
                # say "looking up $perl5_name";
                my $decl_namespace = '';
                my $decl = $self->{var}->perl5_get_decl( $perl5_name );
                if ( $self->{decl} eq 'local' && $decl && ($decl->{decl} eq 'our' || $decl->{decl} eq 'local')) {
                    # say "found ", $decl->{decl}, " namespace: ", $decl->{namespace};
                    $decl_namespace = $decl->{namespace};
                }
                $env->{namespace} = $decl_namespace || $Perlito5::PKG_NAME;
            }
        }

        $Perlito5::VAR->[0]{ $perl5_name } = $env;

        if ($self->{decl} eq 'my') {
            my $str = "";
            $str = $str . 'var ' . $self->{var}->emit_javascript() . ' = ';
            if ($self->{var}->sigil eq '%') {
                $str = $str . '{};';
            }
            elsif ($self->{var}->sigil eq '@') {
                $str = $str . '[];';
            }
            else {
                $str = $str . 'null;';
            }
            return $str;
        }
        elsif ($self->{decl} eq 'our') {
            # TODO
            return '// our ' . $self->{var}->emit_javascript();
        }
        elsif ($self->{decl} eq 'local') {
            # TODO - add grammar support
            # if ($self->var->isa("Lookup")) {
            #     return 
            #         'p5set_local(' . $self->var->{obj}->emit_javascript() . ', '
            #                      . $self->var->{index_exp}->emit_javascript() . ', '
            #                      . '""); '
            #         . $self->{var}->emit_javascript( $level );
            # }

            my $perl5_name = $self->{var}->perl5_name;
            # say "looking up $perl5_name";
            my $decl_namespace = '';
            my $decl = $self->{var}->perl5_get_decl( $perl5_name );
            if ( $decl && ($decl->{decl} eq 'our' || $decl->{decl} eq 'local')) {
                # say "found ", $decl->{decl}, " namespace: ", $decl->{namespace};
                $decl_namespace = $decl->{namespace};
            }

            my $ns = 'p5pkg["' . ($self->{var}{namespace} || $decl_namespace || $Perlito5::PKG_NAME) . '"]';

            return
                  'p5set_local(' . $ns . ','
                               . Perlito5::Javascript::escape_string($self->{var}{name}) . ','
                               . Perlito5::Javascript::escape_string($self->{var}{sigil}) . '); ' 
        }
        elsif ($self->{decl} eq 'state') {
            # TODO
            return '// state ' . $self->{var}->emit_javascript();
        }
        else {
            die "not implemented: Perlito5::AST::Decl '" . $self->{decl} . "'";
        }
    }
}

package Perlito5::AST::Proto;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;
        return Perlito5::Javascript::pkg()
            if $self->{name} eq '__PACKAGE__';
        'p5pkg["' . $self->{name} . '"]'
    }
}

package Perlito5::AST::Call;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;
        my $wantarray = shift;

        my $meth = $self->{method};

        if ( $meth eq 'postcircumfix:<[ ]>' ) {
            return Perlito5::Javascript::emit_javascript_autovivify( $self->{invocant}, $level, 'array' ) . '._array_[' 
                    . 'p5idx(' 
                        . Perlito5::Javascript::emit_javascript_autovivify( $self->{invocant}, $level, 'array' ) . '._array_,'    
                        . Perlito5::Javascript::to_num($self->{arguments}) 
                    . ')'
                . ']';
                # TODO - array slice
                # . '._array_[' . $self->{arguments}->emit_javascript($level, 'list') . ']';
        }
        if ( $meth eq 'postcircumfix:<{ }>' ) {
            return Perlito5::Javascript::emit_javascript_autovivify( $self->{invocant}, $level, 'hash' )
                . '._hash_[' . Perlito5::AST::Lookup->autoquote($self->{arguments})->emit_javascript($level, 'list') . ']';
        }

        my $invocant = $self->{invocant}->emit_javascript;
        if  ($meth eq 'postcircumfix:<( )>')  {
            return '(' . $invocant . ')(' . Perlito5::Javascript::to_list($self->{arguments}) . ')';
        }
        if ( ref($meth) eq 'Perlito5::AST::Var' ) {
            $meth = $meth->emit_javascript();
        }
        else {
            $meth = '"' . $meth . '"';
        }
        return 'p5call(' . $invocant . ', ' 
                         . $meth . ', ' 
                         . Perlito5::Javascript::to_list($self->{arguments}) . ', '
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

        if ($regex->isa('Perlito5::AST::Var')) {
            # $x =~ $regex
            $regex = { code => 'p5:m', arguments => [ $regex, '' ] };
        }

        my $str;
        my $code = $regex->{code};
        my $regex_args = $regex->{arguments};
        if ($code eq 'p5:s') {
            $str = $var->emit_javascript() 
                 . ' = p5str(' . $var->emit_javascript() . ').replace(/' . $regex_args->[0]->{buf} . '/' . $regex_args->[2] . ', '
                 .  $regex_args->[1]->emit_javascript() . ')';
        }
        elsif ($code eq 'p5:m') {

            my $ast = $regex_args->[0];
            if ($ast->isa('Perlito5::AST::Val::Buf')) {
                # constant

                 $str = '(' 
                    . 'p5str(' . $var->emit_javascript() . ')'
                    . '.match(/' . $ast->{buf} . '/' . $regex_args->[1] . ')'
                    . ' ? 1 : 0)';
            }
            else {
                # run-time interpolation

                $str = '(new RegExp('
                        . $ast->emit_javascript() . ', '
                        . '"' . $regex_args->[1] . '"'
                    . '))'
                    . '.exec('
                        . 'p5str(' . $var->emit_javascript() . ')'
                    . ')';
            }
        }
        elsif ($code eq 'p5:tr') {
            # TODO: tr/// not implemented
            $str =  
                 'p5tr(' . $var->emit_javascript() . ', ' . $regex_args->[0]->emit_javascript() . ', ' . $regex_args->[1]->emit_javascript() . ')'
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



    my %emit_js = (
        'infix:<=~>' => sub {
            my $self = $_[0];
            emit_regex_javascript( '=~', $self->{arguments}->[0], $self->{arguments}->[1] );
        },
        'infix:<!~>' => sub {
            my $self = $_[0];
            emit_regex_javascript( '!~', $self->{arguments}->[0], $self->{arguments}->[1] );
        },
        'p5:s' => sub {
            my $self = $_[0];
            emit_regex_javascript( '=~', Perlito5::AST::Var->new( sigil => '$', namespace => '', name => '_' ), $self );
        },
        'p5:m' => sub {
            my $self = $_[0];
            emit_regex_javascript( '=~', Perlito5::AST::Var->new( sigil => '$', namespace => '', name => '_' ), $self );
        },
        'p5:tr' => sub {
            my $self = $_[0];
            emit_regex_javascript( '=~', Perlito5::AST::Var->new( sigil => '$', namespace => '', name => '_' ), $self );
        },
        '__PACKAGE__' => sub {
            my $self = $_[0];
            '"' . $Perlito5::PKG_NAME . '"';
        },
        'wantarray' => sub {
            my $self = $_[0];
            'p5want';
        },
        'package' => sub {
            my $self = $_[0];
            "var " . Perlito5::Javascript::pkg_new_var() . ' = p5make_package("' . $self->{namespace} . '")';
        },
        'infix:<=>>' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
              Perlito5::AST::Lookup->autoquote($self->{arguments}[0])->emit_javascript($level)  . ', ' 
            . $self->{arguments}[1]->emit_javascript($level)
        },
        'infix:<cmp>' => sub {
            my $self = $_[0];
            'p5cmp(' . join( ', ', map( Perlito5::Javascript::to_str($_), @{ $self->{arguments} } ) ) . ')';
        },
        'infix:<<=>>' => sub {
            my $self = $_[0];
            'p5cmp(' . join( ', ', map( Perlito5::Javascript::to_num($_), @{ $self->{arguments} } ) ) . ')';
        },
        'infix:<**>' => sub {
            my $self = $_[0];
            'Math.pow(' . join( ', ', map( Perlito5::Javascript::to_num($_), @{ $self->{arguments} } ) ) . ')';
        },
        'prefix:<!>' => sub {
            my $self      = shift;
            my $level     = shift;
            '!( ' . Perlito5::Javascript::to_bool( $self->{arguments}->[0], $level ) . ')';
        },
        'prefix:<not>' => sub {
            my $self      = shift;
            my $level     = shift;
            '!( ' . Perlito5::Javascript::to_bool( $self->{arguments}->[0], $level ) . ')';
        },
        'prefix:<~>' => sub {
            my $self = $_[0];
            'p5complement( ' . Perlito5::Javascript::to_num( $self->{arguments}->[0] ) . ')';
        },
        'prefix:<->' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            '-( ' . $self->{arguments}->[0]->emit_javascript( $level, 'scalar' ) . ')';
        },
        'prefix:<+>' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            '(' . $self->{arguments}->[0]->emit_javascript( $level, $wantarray ) . ')';
        },
        'require' => sub {
            my $self = $_[0];
            'p5pkg["Perlito5::Grammar::Use"]["require"]([' 
                . Perlito5::Javascript::to_str( $self->{arguments}[0] ) . ', ' 
                . ($self->{arguments}[0]{bareword} ? 1 : 0) 
            . '])';
        },

        'prefix:<$>' => sub {
            my $self = $_[0];
            my $arg  = $self->{arguments}->[0];
            Perlito5::Javascript::emit_javascript_autovivify( $arg, $level, 'scalar' ) . '._scalar_';
        },
        'prefix:<@>' => sub {
            my $self  = $_[0];
            my $level = $_[1];
            my $arg   = $self->{arguments}->[0];
            Perlito5::Javascript::emit_javascript_autovivify( $arg, $level, 'array' ) . '._array_';
        },
        'prefix:<$#>' => sub {
            my $self  = $_[0];
            my $level = $_[1];
            my $arg   = $self->{arguments}->[0];
            '(' . Perlito5::Javascript::emit_javascript_autovivify( $arg, $level, 'array' ) . '._array_.length - 1)';
        },
        'prefix:<%>' => sub {
            my $self  = $_[0];
            my $level = $_[1];
            my $arg   = $self->{arguments}->[0];
            Perlito5::Javascript::emit_javascript_autovivify( $arg, $level, 'hash' ) . '._hash_';
        },
        'prefix:<&>' => sub {
            my $self  = $_[0];
            my $level = $_[1];
            my $arg   = $self->{arguments}->[0];
            'p5code_lookup_by_name("' . $Perlito5::PKG_NAME . '", ' . $arg->emit_javascript($level) . ')';
        },
        'circumfix:<[ ]>' => sub {
            my $self = $_[0];
            '(new p5ArrayRef(' . Perlito5::Javascript::to_list( $self->{arguments} ) . '))';
        },
        'circumfix:<{ }>' => sub {
            my $self = $_[0];
            '(new p5HashRef(' . Perlito5::Javascript::to_list( $self->{arguments}, $level, 'hash' ) . '))';
        },
        'prefix:<\\>' => sub {
            my $self  = $_[0];
            my $level = $_[1];
            my $arg   = $self->{arguments}->[0];
            if ( $arg->isa('Perlito5::AST::Var') ) {
                if ( $arg->sigil eq '@' ) {
                    return '(new p5ArrayRef(' . $arg->emit_javascript($level) . '))';
                }
                if ( $arg->sigil eq '%' ) {
                    return '(new p5HashRef(' . $arg->emit_javascript($level) . '))';
                }
                if ( $arg->sigil eq '*' ) {
                    return '(new p5GlobRef(' . $arg->emit_javascript($level) . '))';
                }
                if ( $arg->sigil eq '&' ) {
                    if ( $arg->{namespace} ) {
                        return 'p5pkg["' . $arg->{namespace} . '"].' . $arg->{name};
                    }
                    else {
                        return Perlito5::Javascript::pkg() . '.' . $arg->{name};
                    }
                }
            }
            return '(new p5ScalarRef(' . $arg->emit_javascript($level) . '))';
        },

        'postfix:<++>' => sub {
            my $self = $_[0];
            '(' . join( ' ', map( $_->emit_javascript, @{ $self->{arguments} } ) ) . ')++';
        },
        'postfix:<-->' => sub {
            my $self = $_[0];
            '(' . join( ' ', map( $_->emit_javascript, @{ $self->{arguments} } ) ) . ')--';
        },
        'prefix:<++>' => sub {
            my $self = $_[0];
            '++(' . join( ' ', map( $_->emit_javascript, @{ $self->{arguments} } ) ) . ')';
        },
        'prefix:<-->' => sub {
            my $self = $_[0];
            '--(' . join( ' ', map( $_->emit_javascript, @{ $self->{arguments} } ) ) . ')';
        },

        'infix:<x>' => sub {
            my $self = $_[0];
            'p5str_replicate(' . join( ', ', map( $_->emit_javascript, @{ $self->{arguments} } ) ) . ')';
        },

        'list:<.>' => sub {
            my $self = $_[0];
            '(' . join( ' + ', map( Perlito5::Javascript::to_str($_), @{ $self->{arguments} } ) ) . ')';
        },
        'list:<,>' => sub {
            my $self = $_[0];
            Perlito5::Javascript::to_list( $self->{arguments} );
        },

        'infix:<..>' => sub {
            my $self = $_[0];
            '(function (a) { ' . 'for (var i=' . $self->{arguments}->[0]->emit_javascript() . ', l=' . $self->{arguments}->[1]->emit_javascript() . '; ' . 'i<=l; ++i)' . '{ ' . 'a.push(i) ' . '}; ' . 'return a ' . '})([])';
        },

        'delete' => sub {
            my $self = $_[0];
            '(delete ' . $self->{arguments}[0]->emit_javascript() . ')';
        },

        'scalar' => sub {
            my $self = $_[0];
            Perlito5::Javascript::to_scalar($self->{arguments}, $level+1);
        },

        'ternary:<? :>' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            '( ' . Perlito5::Javascript::to_bool( $self->{arguments}->[0] ) . ' ? ' . ( $self->{arguments}->[1] )->emit_javascript( $level, $wantarray ) . ' : ' . ( $self->{arguments}->[2] )->emit_javascript( $level, $wantarray ) . ')';
        },
        'my' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;

            # TODO - bug: this is a side-effect of my($x,$y)
            'p5context(' . '[' . join( ', ', map( $_->emit_javascript( $level, $wantarray ), @{ $self->{arguments} } ) ) . '], ' . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 ) . ')';
        },
        'local' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;

            # TODO - bug: this is a side-effect of local($x,$y)
            'p5context(' . '[' . join( ', ', map( $_->emit_javascript( $level, $wantarray ), @{ $self->{arguments} } ) ) . '], ' . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 ) . ')';
        },
        'circumfix:<( )>' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            'p5context(' . '[' . join( ', ', map( $_->emit_javascript( $level, $wantarray ), @{ $self->{arguments} } ) ) . '], ' . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 ) . ')';
        },
        'infix:<=>' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;

            my $parameters = $self->{arguments}->[0];
            my $arguments  = $self->{arguments}->[1];

            if (   $parameters->isa( 'Perlito5::AST::Apply' )
               &&  ( $parameters->code eq 'my' || $parameters->code eq 'local' || $parameters->code eq 'circumfix:<( )>' )
               )
            {
                # my ($x, $y) = ...
                # local ($x, $y) = ...
                # ($x, $y) = ...

                my $tmp  = 'tmp' . Perlito5::Javascript::get_label();
                my $tmp2 = 'tmp' . Perlito5::Javascript::get_label();
                return
                  '(function () { '
                    . 'var ' . $tmp  . ' = ' . Perlito5::Javascript::to_list([$arguments], $level+1) . '; '
                    . 'var ' . $tmp2 . ' = ' . $tmp . '.slice(0); '
                    . join( '; ',
                            (
                            map +( $_->isa('Perlito5::AST::Apply') && $_->code eq 'undef'
                                 ? $tmp . '.shift()' 
                                 : $_->sigil eq '$' 
                                 ? $_->emit_javascript() . ' = ' . $tmp . '.shift()'
                                 : $_->sigil eq '@' 
                                 ? $_->emit_javascript() . ' = ' . $tmp . '; ' . $tmp . ' = []'
                                 : $_->sigil eq '%' 
                                 ? $_->emit_javascript() . ' = p5a_to_h(' . $tmp . '); ' . $tmp . ' = []'
                                 : die("not implemented")
                                 ),
                                 @{ $parameters->arguments }
                            ),
                            'return ' . $tmp2
                          )
                . ' })()'
            }

            if (   $parameters->isa( 'Perlito5::AST::Var' )  && $parameters->sigil eq '$'
               ||  $parameters->isa( 'Perlito5::AST::Decl' ) && $parameters->var->sigil eq '$'
               )
            {
                return '(' . $parameters->emit_javascript() . ' = ' . Perlito5::Javascript::to_scalar([$arguments], $level+1) . ')'
            }

            if  (   $parameters->isa( 'Perlito5::AST::Var' )  && $parameters->sigil eq '@'
                ||  $parameters->isa( 'Perlito5::AST::Decl' ) && $parameters->var->sigil eq '@'
                )
            {
                return '(' . $parameters->emit_javascript() . ' = ' . Perlito5::Javascript::to_list([$arguments], $level+1) . ')'
            }
            elsif ( $parameters->isa( 'Perlito5::AST::Var' )  && $parameters->sigil eq '%'
                ||  $parameters->isa( 'Perlito5::AST::Decl' ) && $parameters->var->sigil eq '%'
                )
            {
                return '(' . $parameters->emit_javascript() . ' = ' . Perlito5::Javascript::to_list([$arguments], $level+1, 'hash') . ')' 
            }
            '(' . $parameters->emit_javascript( $level ) . ' = ' . $arguments->emit_javascript( $level+1 ) . ')';

        },

        'break' => sub {
            my $self      = shift;
            my $level     = shift;
            $Perlito5::THROW = 1;
            'throw(new p5_error("break", ""))'
        },
        'next' => sub {
            my $self      = shift;
            my $level     = shift;
            $Perlito5::THROW = 1;
            my $label =  $self->{arguments}[0]{code} || "";
            'throw(new p5_error("next", "' . $label . '"))'
        },
        'last' => sub {
            my $self      = shift;
            my $level     = shift;
            $Perlito5::THROW = 1;
            my $label =  $self->{arguments}[0]{code} || "";
            'throw(new p5_error("last", "' . $label . '"))'
        },
        'redo' => sub {
            my $self      = shift;
            my $level     = shift;
            $Perlito5::THROW = 1;
            my $label =  $self->{arguments}[0]{code} || "";
            'throw(new p5_error("redo", "' . $label . '"))'
        },

        'return' => sub {
            my $self      = shift;
            my $level     = shift;
            $Perlito5::THROW = 1;
            'throw(' . Perlito5::Javascript::to_runtime_context( $self->{arguments}, $level ) . ')';
        },
        'goto' => sub {
            my $self = $_[0];
            $Perlito5::THROW = 1;
            'throw((' . $self->{arguments}->[0]->emit_javascript() . ')([List__, p5want]))';
        },

        'do' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            # Note: this is "do EXPR" - look at the "Do" AST node for "do BLOCK"
            my $ast =
                Perlito5::AST::Apply->new(
                    code => 'eval',
                    namespace => '',
                    arguments => [
                       Perlito5::AST::Apply->new(
                          code => 'slurp',
                          namespace => 'Perlito5::IO',
                          arguments => $self->{arguments}
                        )
                    ]
                );
            $ast->emit_javascript( $level );
        },

        'eval' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            $Perlito5::THROW = 1;   # we can return() from inside eval

            my $arg = $self->{arguments}->[0];
            my $eval;
            if ($arg->isa( "Perlito5::AST::Do" )) {
                # eval block

                $eval = $arg->emit_javascript( $level + 1, $wantarray );
            }
            else {
                # eval string

                my $var_env_perl5 = Perlito5::Dumper::Dumper( $Perlito5::VAR );
                # say "at eval: ", $var_env_perl5;
                my $m = Perlito5::Expression->term_square( $var_env_perl5, 0 );
                $m = Perlito5::Expression::expand_list( Perlito5::Match::flat($m)->[2] );
                # say Perlito5::Dumper::Dumper( $m );
                my $var_env_js = '(new p5ArrayRef(' . Perlito5::Javascript::to_list($m) . '))';
                $eval ='eval(perl5_to_js(' 
                            . Perlito5::Javascript::to_str($arg) . ", "
                            . '"' . $Perlito5::PKG_NAME . '", '
                            . $var_env_js . ', '
                            . '"' . $wantarray . '"'
                        . "))";
            }

            # TODO - test return() from inside eval

                "(function () {\n"
                    . "var r = null;\n"
                    . 'p5pkg["main"]["v_@"] = "";' . "\n"
                    . "try {\n"
                        . 'r = ' . $eval . "\n"
                    . "}\n"
                    . "catch(err) {\n"
                    .    "if ( err instanceof p5_error ) {\n"
                    .        'p5pkg["main"]["v_@"] = err;' . "\n"
                    .    "}\n"
                    .    "else if ( err instanceof Error ) {\n"
                    .        'p5pkg["main"]["v_@"] = err;' . "\n"
                    .    "}\n"
                    .    "else {\n"
                    .        "return(err);\n" 
                    .    "}\n"
                    . "}\n"
                    . "return r;\n"
                . "})()"

        },

        'undef' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            if ( $self->{arguments} && @{$self->{arguments}} ) {
                return '(' . $self->{arguments}->[0]->emit_javascript . ' = null)'
            }
            return 'null'
        },

        'defined' => sub { 
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            '('  . join(' ', map( $_->emit_javascript( $level ), @{$self->{arguments}} ))    . ' != null)' 
        },

        'shift' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            if ( $self->{arguments} && @{$self->{arguments}} ) {
                return Perlito5::Javascript::pkg() . '.shift([' . join(', ', map( $_->emit_javascript( $level ), @{$self->{arguments}} )) . '])'
            }
            return Perlito5::Javascript::pkg() . '.shift([List__])'
        },

        'map' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            my @in  = @{$self->{arguments}};
            my $fun  = shift @in;
            my $list = Perlito5::Javascript::to_list(\@in);

            if (ref($fun) eq 'Perlito5::AST::Lit::Block') {
                $fun = $fun->{stmts}
            }
            else {
                $fun = [$fun];
            }

            'p5map(' . Perlito5::Javascript::pkg() . ', '

                    . 'function (p5want) {' . "\n"
                    .   (Perlito5::Javascript::LexicalBlock->new( block => $fun, needs_return => 1, top_level => 0 ))->emit_javascript( $level + 1 ) . "\n"
                    . Perlito5::Javascript::tab($level) . '}, '

                    .   $list
                    . ')';
        },
        'grep' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            my @in  = @{$self->{arguments}};
            my $fun  = shift @in;
            my $list = Perlito5::Javascript::to_list(\@in);

            if (ref($fun) eq 'Perlito5::AST::Lit::Block') {
                $fun = $fun->{stmts}
            }
            else {
                $fun = [$fun];
            }

            'p5grep(' . Perlito5::Javascript::pkg() . ', '

                    . 'function (p5want) {' . "\n"
                    .   (Perlito5::Javascript::LexicalBlock->new( block => $fun, needs_return => 1, top_level => 0 ))->emit_javascript( $level + 1 ) . "\n"
                    . Perlito5::Javascript::tab($level) . '}, '

                    .   $list
                    . ')';
        },
        'sort' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            my @in  = @{$self->{arguments}};
            my $fun;
            my $list;

            if (ref($in[0]) eq 'Perlito5::AST::Lit::Block') {
                # the sort function is optional
                $fun = shift @in;
                $fun =
                      'function (p5want) {' . "\n"
                    .   (Perlito5::Javascript::LexicalBlock->new( block => $fun->{stmts}, needs_return => 1, top_level => 0 ))->emit_javascript( $level + 1 ) . "\n"
                    . Perlito5::Javascript::tab($level) . '}'
            }
            else {
                $fun = 'null';
            }
            $list = Perlito5::Javascript::to_list(\@in);

            'p5sort(' . Perlito5::Javascript::pkg() . ', '
                    .   $fun . ', '
                    .   $list
                    . ')';
        },

        'infix:<//>' => sub { 
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            'p5defined_or' . '('
                . $self->{arguments}->[0]->emit_javascript($level, 'scalar') . ', '
                . Perlito5::Javascript::emit_function_javascript($level, $wantarray, $self->{arguments}->[1]) 
                . ')'
        },

        'exists' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            my $arg = $self->{arguments}->[0];
            if ($arg->isa( 'Perlito5::AST::Lookup' )) {
                my $v = $arg->obj;
                if (  $v->isa('Perlito5::AST::Var')
                   && $v->sigil eq '$'
                   )
                {
                    $v = Perlito5::AST::Var->new( sigil => '%', namespace => $v->namespace, name => $v->name );
                    return '(' . $v->emit_javascript() . ').hasOwnProperty(' . $arg->autoquote($arg->{index_exp})->emit_javascript($level) . ')';
                }
                return '(' . $v->emit_javascript() . ')._hash_.hasOwnProperty(' . $arg->autoquote($arg->{index_exp})->emit_javascript($level) . ')';
            }
            if ($arg->isa( 'Perlito5::AST::Call' )) {
                if ( $arg->method eq 'postcircumfix:<{ }>' ) {
                    return '(' . $arg->invocant->emit_javascript() . ')._hash_.hasOwnProperty(' . Perlito5::AST::Lookup->autoquote($arg->{arguments})->emit_javascript($level) . ')';
                }
            }
        },

    );


    sub emit_javascript {
        my $self  = shift;
        my $level = shift;
        my $wantarray = shift;

        my $apply = $self->op_assign();
        if ($apply) {
            return $apply->emit_javascript( $level );
        }

        my $code = $self->{code};

        if (ref $code ne '') {
            my @args = ();
            push @args, $_->emit_javascript
                for @{$self->{arguments}};
            return '(' . $self->{code}->emit_javascript( $level ) . ')(' . join(',', @args) . ')';
        }

        return $emit_js{$code}->($self, $level, $wantarray)
            if exists $emit_js{$code};

        if (exists $Perlito5::Javascript::op_infix_js_str{$code}) {
            return '(' 
                . join( $Perlito5::Javascript::op_infix_js_str{$code}, map( Perlito5::Javascript::to_str($_), @{$self->{arguments}} ))
                . ')'
        }
        if (exists $Perlito5::Javascript::op_infix_js_num{$code}) {
            return '(' 
                . join( $Perlito5::Javascript::op_infix_js_num{$code}, map( Perlito5::Javascript::to_num($_), @{$self->{arguments}} ))
                . ')'
        }
        if (exists $Perlito5::Javascript::op_prefix_js_str{$code}) {
            return $Perlito5::Javascript::op_prefix_js_str{$code} . '(' 
                . Perlito5::Javascript::to_str($self->{arguments}[0])
                . ')'
        }

        if (  $code eq 'infix:<&&>'
           || $code eq 'infix:<and>'
           )
        {
            return 'p5and' . '('
                . $self->{arguments}->[0]->emit_javascript($level, 'scalar') . ', '
                . Perlito5::Javascript::emit_function_javascript($level, $wantarray, $self->{arguments}->[1]) 
                . ')'
        }
        if (  $code eq 'infix:<||>'
           || $code eq 'infix:<or>'
           )
        {
            return 'p5or' . '('
                . $self->{arguments}->[0]->emit_javascript($level, 'scalar') . ', '
                . Perlito5::Javascript::emit_function_javascript($level, $wantarray, $self->{arguments}->[1]) 
                . ')'
        }

        if ($self->{namespace}) {

            if (  $self->{namespace} eq 'JS' 
               && $code eq 'inline'
               ) 
            {
                if ( $self->{arguments}->[0]->isa('Perlito5::AST::Val::Buf') ) {
                    # JS::inline('var x = 123')
                    return $self->{arguments}[0]{buf};
                }
                else {
                    die "JS::inline needs a string constant";
                }
            }

            $code = 'p5pkg["' . $self->{namespace} . '"].' . $code;
        }
        else {
            $code = Perlito5::Javascript::pkg() . '.' . $code
        }

        my $sig;
        {
            my $name = $self->{code};
            my $namespace = $self->{namespace} || $Perlito5::PKG_NAME;
            my $effective_name = $namespace . "::" . $self->{code};
            if ( exists $Perlito5::PROTO->{$effective_name} ) {
                $sig = $Perlito5::PROTO->{$effective_name};
            }
            elsif ( (!$self->{namespace} || $namespace eq 'CORE')
                  && exists $Perlito5::CORE_PROTO->{"CORE::$name"}
                  )
            {
                $effective_name = "CORE::$name";
                $sig = $Perlito5::CORE_PROTO->{$effective_name};
            }
            else {
                # this subroutine was never declared
                if ($self->{bareword}) {
                    if ( $Perlito5::STRICT ) {
                        die 'Bareword "' . $name . '" not allowed while "strict subs" in use';
                    }
                    return Perlito5::Javascript::escape_string( 
                            ($self->{namespace} ? $self->{namespace} . '::' : "") . $name 
                        );
                }
            }
        }

        if (  ($self->{code} eq 'say' || $self->{code} eq 'print')
           && !$self->{namespace} 
           && $self->{bareword}
           )
        {
            # print/say have no prototype; the default argument is $_
            $self->{arguments} = [
                Perlito5::AST::Var->new( sigil => '$', namespace => '', name => '_' ),
            ];
        }

        if ($sig) {
            # warn "sig $effective_name $sig\n";
            my @out = ();
            my @in  = @{$self->{arguments} || []};

            # TODO - generate the right prototype

            my $optional = 0;
            while (length $sig) {
                my $c = substr($sig, 0, 1);
                if ($c eq ';') {
                    $optional = 1;
                }
                elsif ($c eq '$' || $c eq '_') {
                    push @out, shift(@in)->emit_javascript( $level, 'scalar' ) if @in || !$optional;
                }
                elsif ($c eq '@') {
                    push @out, Perlito5::Javascript::to_list(\@in) if @in || !$optional;
                    @in = ();
                }
                elsif ($c eq '*') {
                    if (@in || !$optional) {
                        my $arg = shift @in;
                        if ($arg->{bareword}) {
                            push @out,
                                'p5pkg["' . ($arg->{namespace} || $Perlito5::PKG_NAME) . '"]["f_' . $arg->{code} . '"]';
                        }
                        else {
                            push @out, $arg->emit_javascript( $level, 'scalar' );
                        }
                    }
                }
                elsif ($c eq '\\') {
                    if (substr($sig, 0, 2) eq '\\$') {
                        $sig = substr($sig, 1);
                        push @out, shift(@in)->emit_javascript( $level, 'scalar' ) if @in || !$optional;
                    }
                    elsif (substr($sig, 0, 2) eq '\\@'
                        || substr($sig, 0, 2) eq '\\%'
                        )
                    {
                        $sig = substr($sig, 1);
                        push @out, shift(@in)->emit_javascript( $level, 'list' ) if @in || !$optional;
                    }
                    elsif (substr($sig, 0, 5) eq '\\[@%]') {
                        $sig = substr($sig, 4);
                        push @out, shift(@in)->emit_javascript( $level, 'list' ) if @in || !$optional;
                    }
                }
                $sig = substr($sig, 1);
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
        my $arg_list = Perlito5::Javascript::to_list_preprocess( $self->{arguments} );
        push @args, $_->emit_javascript( $level )
            for @$arg_list;

        my $arg_code = 
            $self->{code} eq 'scalar'      # scalar() is special
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

}

package Perlito5::AST::If;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;
        my $cond = $self->{cond};
        my $body  = Perlito5::Javascript::LexicalBlock->new( block => $self->{body}->stmts, needs_return => 0, create_context => 1 );
        my $s = 'if ( ' . Perlito5::Javascript::to_bool($cond, $level + 1) . ' ) {' . "\n"
            .       $body->emit_javascript( $level + 1 ) . "\n"
            . Perlito5::Javascript::tab($level) . '}';
        if ( @{ $self->{otherwise}->stmts } ) {
            my $otherwise = Perlito5::Javascript::LexicalBlock->new( block => $self->{otherwise}->stmts, needs_return => 0, create_context => 1 );
            $s = $s
                . "\n"
                . Perlito5::Javascript::tab($level) . 'else {' . "\n"
                .       $otherwise->emit_javascript( $level + 1 ) . "\n"
                . Perlito5::Javascript::tab($level) . '}';
        }
        return $s;
    }
}


package Perlito5::AST::When;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;
        my $cond = $self->{cond};
        my $body  = Perlito5::Javascript::LexicalBlock->new( block => $self->{body}->stmts, needs_return => 0, create_context => 1 );

        # TODO - transform EXPR into ($_ ~~ EXPR)

        # this is a placeholder - this is wrong!
        my $expr = Perlito5::AST::Apply->new(
            code => 'infix:<==>', 
            arguments => [
                Perlito5::AST::Var->new( sigil => '$', namespace => '', name => '_' ),
                $cond
            ] 
        );

        # TODO - use a "next" exception inside a "for", but use a "break" exception inside a "given"

        my $s = 'if ( ' . Perlito5::Javascript::to_bool($expr, $level + 1) . ' ) {' . "\n"
            .       $body->emit_javascript( $level + 1 ) . "\n"

            . Perlito5::Javascript::tab($level+1) . 'throw(new p5_error("next", "' . $label . '"))'
            . Perlito5::Javascript::tab($level) . '}';
        return $s;
    }
}


package Perlito5::AST::While;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;

        my $cond = $self->{cond};

        return 'p5while('
                    . "function () {\n"
                    .   (Perlito5::Javascript::LexicalBlock->new( block => $self->{body}->stmts, needs_return => 0, top_level => 0 ))->emit_javascript($level + 2) . "\n"
                    . Perlito5::Javascript::tab($level + 1) . '}, '
                    . Perlito5::Javascript::emit_function_javascript($level, 0, $cond) . ', '
                    . Perlito5::AST::Lit::Block::emit_javascript_continue($self, $level) . ', '
                    .   '"' . ($self->{label} || "") . '"'
                    . ')'
    }
}

package Perlito5::AST::For;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;

        if (ref($self->{cond}) eq 'ARRAY') {
            # C-style for

            # TODO - catch next/last/redo
            # TODO - loop label
            # TODO - continue-block is a syntax error

            my $body      = Perlito5::Javascript::LexicalBlock->new( block => $self->{body}->stmts, needs_return => 0, create_context => 1 );
            return
               'for ( '
            .  ( $self->{cond}[0] ? $self->{cond}[0]->emit_javascript($level + 1) . '; '  : '; ' )
            .  ( $self->{cond}[1] ? $self->{cond}[1]->emit_javascript($level + 1) . '; '  : '; ' )
            .  ( $self->{cond}[2] ? $self->{cond}[2]->emit_javascript($level + 1) . ' '   : ' '  )
            .  ') {' . "\n" 
                . $body->emit_javascript( $level + 1 ) . "\n"
            .  Perlito5::Javascript::tab($level) . '}'
        }

        my $cond = Perlito5::Javascript::to_list([$self->{cond}], $level + 1);
        if ($self->{body}->sig()) {
            # XXX - cleanup: "for" parser throws away the variable declaration, so we need to create it again
            # TODO - for without "my"

            # mark the variable as "declared"
            my $v = $self->{body}->sig;
            $Perlito5::VAR->[0]{ $v->perl5_name } = { decl => 'my' };
            my $sig = $v->emit_javascript( $level + 1 );
            return 'p5for_lex('
                    . "function ($sig) {\n"
                    .   (Perlito5::Javascript::LexicalBlock->new( block => $self->{body}->stmts, needs_return => 0, top_level => 0 ))->emit_javascript($level + 2) . "\n"
                    . Perlito5::Javascript::tab($level + 1) . '}, '
                    .   $cond . ', '
                    . Perlito5::AST::Lit::Block::emit_javascript_continue($self, $level) . ', '
                    .   '"' . ($self->{label} || "") . '"'
                    . ')'
        }
        else {
            # use $_
            return 'p5for(' . Perlito5::Javascript::pkg() . ', '
                    . 'function () {' . "\n"
                    .   (Perlito5::Javascript::LexicalBlock->new( block => $self->{body}->stmts, needs_return => 0, top_level => 0 ))->emit_javascript($level + 2) . "\n"
                    . Perlito5::Javascript::tab($level + 1) . '}, '
                    .   $cond . ', '
                    . Perlito5::AST::Lit::Block::emit_javascript_continue($self, $level) . ', '
                    .   '"' . ($self->{label} || "") . '"'
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
        .   (Perlito5::Javascript::LexicalBlock->new( block => $self->{block}, needs_return => 1, top_level => 1 ))->emit_javascript( $level + 1 ) . "\n"
        . Perlito5::Javascript::tab($level) . '}';

        if ( $self->{name} ) {
            return 'p5make_sub("' . $self->{namespace} . '", "' . $self->{name} . '", ' . $s . ')'
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
        my $wantarray = shift;

        my $block = $self->simplify->block;
        return
              '(function () {' . "\n"
            .   (Perlito5::Javascript::LexicalBlock->new( block => $block, needs_return => 1 ))->emit_javascript( $level + 1, $wantarray ) . "\n"
            . Perlito5::Javascript::tab($level) . '})()'
    }
}

package Perlito5::AST::Use;
{
    sub emit_javascript {
        my $self = shift;
        my $level = shift;
        Perlito5::Grammar::Use::emit_time_eval($self);
        '// ' . $self->{code} . ' ' . $self->{mod} . "\n"
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
