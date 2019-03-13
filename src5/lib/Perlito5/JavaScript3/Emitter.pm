use v5;

use Perlito5::AST;
use Perlito5::Dumper;

package Perlito5::JavaScript3;
{
    my %label;
    sub pkg {
        # this is an optimization to reduce the number of lookups
        # this breaks eval() because the variable is not always seen at runtime
        # $label{ $Perlito5::PKG_NAME } ||= "p5" . $Perlito5::ID++
        'p5pkg["' . $Perlito5::PKG_NAME . '"]'
    }
    sub pkg_new_var {
        $label{ $Perlito5::PKG_NAME } = "p5" . $Perlito5::ID++
    }
    sub get_label {
        $Perlito5::ID++
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
        'infix:<lt>' => ' < ',
        'infix:<gt>' => ' > ',
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
        infix:<gt>
        infix:<lt>
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

            if  (  ($cond->isa( 'Perlito5::AST::Buf' ))
                || ($cond->isa( 'Perlito5::AST::Apply' )  && exists $op_to_str{ $cond->code } )
                )
            {
                return $cond->emit_javascript3($level, $wantarray);
            }
            else {
                return 'p5str(' . $cond->emit_javascript3($level, $wantarray) . ')';
            }
    }
    sub to_num {
            my $cond = shift;
            my $level = shift;
            my $wantarray = 'scalar';
            if  (  $cond->isa( 'Perlito5::AST::Int' ) 
                || $cond->isa( 'Perlito5::AST::Num' )
                || ($cond->isa( 'Perlito5::AST::Apply' )  && exists $op_to_num{ $cond->code } )
                )
            {
                return $cond->emit_javascript3($level, $wantarray);
            }
            else {
                return 'p5num(' . $cond->emit_javascript3($level, $wantarray) . ')';
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

            if  (  ($cond->isa( 'Perlito5::AST::Int' ))
                || ($cond->isa( 'Perlito5::AST::Num' ))
                || ($cond->isa( 'Perlito5::AST::Apply' ) && exists $op_to_bool{ $cond->code })
                )
            {
                return $cond->emit_javascript3($level, $wantarray);
            }
            else {
                return 'p5bool(' . $cond->emit_javascript3($level, $wantarray) . ')';
            }
    }

    sub is_scalar {
            !$_[0]->isa( 'Perlito5::AST::Int' )
         && !$_[0]->isa( 'Perlito5::AST::Num' )
         && !$_[0]->isa( 'Perlito5::AST::Buf' )
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

    sub to_value {
        # change "lvalues" to "values"
        my $v         = shift;
        my $level     = shift;
        my $wantarray = shift;

        return
             !$v
             ? 'null'                                           # null - this shouldn't happen!
             : $v->isa('Perlito5::AST::Var') && $v->sigil eq '$'
             ? $v->emit_javascript3($level, $wantarray) . '.FETCH()'     # no lvalues here
             : $v->emit_javascript3($level, $wantarray);
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
                    $k = $k->emit_javascript3($level, 0);

                    $printable = 0
                        if $k =~ /[ \[]/;   # ???

                    $v = to_value($v, $level, $wantarray);
                    push @out, "$k : $v";
                }

                return '{' . join(', ', @out) . '}'
                    if $printable;

            }
            return 'p5a_to_h(' . to_list($items, $level, 'array') . ')';
        }

        $interpolate
        ? ( 'p5list_to_a('
          .   join(', ', map( $_->emit_javascript3($level, $wantarray), @$items ))
          . ')'
          )
        : ( '['
          .   join(', ', map( to_value($_, $level, $wantarray), @$items ))
          . ']'
          )
    }

    sub to_list_preprocess {
        my @items;
        for my $item ( @{$_[0]} ) {
            if (  $item->isa( 'Perlito5::AST::Apply' ) 
               && ( $item->code eq 'circumfix:<( )>' || $item->code eq 'list:<,>' || $item->code eq 'list:<=>>' )
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
          .   join(', ', map( $_->emit_javascript3($level, $wantarray), @$items ))
          . ')'
        : 'null'
    }

    sub to_scalar_preprocess {
        my @items;
        for my $item ( @{$_[0]} ) {
            if (  $item->isa( 'Perlito5::AST::Apply' ) 
               && ( $item->code eq 'list:<,>' || $item->code eq 'list:<=>>' )
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

        return $items->[0]->emit_javascript3($level, $wantarray)
            if @$items == 1 && is_scalar($items->[0]);

        'p5context(' 
            . '['
            .   join(', ', map( $_->emit_javascript3($level, $wantarray), @$items ))
            . ']'
            . ', p5want)'
    }

    sub autoquote {
        my $index = shift;
        my $level = shift;
    
        # ok   ' sub x () { 123 } $v{x()} = 12; use Data::Dumper; print Dumper \%v '       # '123'     => 12
        # ok   ' sub x () { 123 } $v{x} = 12; use Data::Dumper; print Dumper \%v '         # 'x'       => 12
        # TODO ' sub x () { 123 } $v{main::x} = 12; use Data::Dumper; print Dumper \%v '   # '123'     => 12
        # ok   ' $v{main::x} = 12; use Data::Dumper; print Dumper \%v '                    # 'main::x' => 12
    
        if ($index->isa('Perlito5::AST::Apply')
           && $index->{bareword}
           )
        {
            my $full_name = ($index->{namespace} ? $index->{namespace} . '::' : "") . $index->{code};
            if ( !exists $Perlito5::PROTO->{$full_name} ) {
                $index = Perlito5::AST::Buf->new( 
                    buf => $full_name
                );
            }
        }
   
        return to_str($index, $level);
    }

    sub emit_javascript3_autovivify {
        my $obj = shift;
        my $level = shift;
        my $type = shift;  # 'array'/'hash'/'lvalue'
        my $wantarray = 'scalar';

        return $obj->emit_javascript3($level, $wantarray, $type);
    }

    sub emit_function_javascript3 {
        my $level = shift;
        my $wantarray = shift;
        my $argument = shift;

        if ( $argument->isa( 'Perlito5::AST::Apply' ) && $argument->code eq 'return' ) {
            return 'function () { ' . $argument->emit_javascript3($level, $wantarray) . ' }';
        }
        return 'function () { return ' . $argument->emit_javascript3($level, $wantarray) . ' }';
    }

}

package Perlito5::JavaScript3::LexicalBlock;
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
                    my $var = $decl->arguments()->[0];
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


    sub emit_javascript3 {
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
            return Perlito5::JavaScript3::tab($level) . 'null;';
        }
        my $out = '';
        my @str;
        my $has_local = $self->has_decl("local");
        my $create_context = $self->{create_context} && $self->has_decl("my");
        my $outer_pkg   = $Perlito5::PKG_NAME;
        my $outer_throw = $Perlito5::THROW;

        $Perlito5::THROW = 0
            if $self->{top_level};

        $out .= Perlito5::JavaScript3::tab($level) . "var local_idx = p5LOCAL.length;\n"
            if $has_local;
        if ($self->{top_level}) {
            $level++;
        }
        if ( $create_context ) {
            $out .= Perlito5::JavaScript3::tab($level) . "(function () {\n";
            $level++;
        }

        my $tab = Perlito5::JavaScript3::tab($level);
        my $last_statement;
        if ($self->{needs_return}) {
            $last_statement = pop @block;
        }
        for my $decl ( @block ) {
            if ( ref($decl) eq 'Perlito5::AST::Apply' && $decl->code eq 'package' ) {
                $Perlito5::PKG_NAME = $decl->{namespace};
            }

            if ($decl->isa( 'Perlito5::AST::Decl' )) {
                push @str, $decl->emit_javascript3_init;
            }
            # TODO - local, our
            if ($decl->isa( 'Perlito5::AST::Apply' ) && $decl->code eq 'my' ) {
                for (@{$decl->{arguments}}) {
                    if ($_->isa( 'Perlito5::AST::Var' )) {
                        my $d = Perlito5::AST::Decl->new( decl => $decl->code, var => $_ );
                        push @str, $d->emit_javascript3_init;
                    }
                }
            }
            if ($decl->isa( 'Perlito5::AST::Apply' ) && $decl->code eq 'infix:<=>') {
                my $arg = $decl->{arguments}[0];
                if ($arg->isa( 'Perlito5::AST::Decl' )) {
                    push @str, $arg->emit_javascript3_init;
                }
                # TODO - local, our
                if ($arg->isa( 'Perlito5::AST::Apply' ) && $arg->code eq 'my' ) {
                    for (@{$arg->{arguments}}) {
                        if ($_->isa( 'Perlito5::AST::Var' )) {
                            my $d = Perlito5::AST::Decl->new( decl => $arg->code, var => $_ );
                            push @str, $d->emit_javascript3_init;
                        }
                    }
                }
            }

            if (!( $decl->isa( 'Perlito5::AST::Decl' ) && $decl->decl eq 'my' )) {
                push @str, $decl->emit_javascript3($level, 'void') . ';';
            }
        }

        # TODO - use the context information ($wantarray)

        if ($self->{needs_return} && $last_statement) {

            if ($last_statement->isa( 'Perlito5::AST::Decl' )) {
                push @str, $last_statement->emit_javascript3_init;
            }
            if ($last_statement->isa( 'Perlito5::AST::Apply' ) && $last_statement->code eq 'infix:<=>') {
                if ($last_statement->{arguments}[0]->isa( 'Perlito5::AST::Decl' )) {
                    push @str, $last_statement->{arguments}[0]->emit_javascript3_init;
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
                $body         = Perlito5::JavaScript3::LexicalBlock->new( block => $body->stmts, needs_return => 1 );
                push @str,
                        'if ( ' . Perlito5::JavaScript3::to_bool( $cond, $level+1 ) . ' ) {' . "\n"
                        .   $body->emit_javascript3($level+1) . "\n"
                        . Perlito5::JavaScript3::tab($level) . '}';
                if ($otherwise) {
                    $otherwise = Perlito5::JavaScript3::LexicalBlock->new( block => $otherwise->stmts, needs_return => 1 );
                    push @str, "\n"
                        . Perlito5::JavaScript3::tab($level) . 'else {' . "\n"
                        .   $otherwise->emit_javascript3($level+1) . "\n"
                        . Perlito5::JavaScript3::tab($level) . '}';
                }
            }
            elsif ( $last_statement->isa( 'Perlito5::AST::Block' ) ) {
                my $body = Perlito5::JavaScript3::LexicalBlock->new( block => $last_statement->{stmts}, needs_return => 1 );
                push @str,
                      'for (var i_ = 0; i_ < 1 ; i_++) {' . "\n"
                    .   $body->emit_javascript3( $level + 1 ) . "\n"
                    . Perlito5::JavaScript3::tab($level) . '}'
            }
            elsif (  $last_statement->isa( 'Perlito5::AST::For' )
                  || $last_statement->isa( 'Perlito5::AST::While' )
                  || $last_statement->isa( 'Perlito5::AST::Apply' ) && $last_statement->code eq 'goto'
                  || $last_statement->isa( 'Perlito5::AST::Apply' ) && $last_statement->code eq 'return'
                  )
            {
                push @str, $last_statement->emit_javascript3($level, 'runtime');
            }
            else {
                if ( $has_local ) {
                    push @str, 'return p5cleanup_local(local_idx, (' . Perlito5::JavaScript3::to_runtime_context([$last_statement], $level) . '));';
                }
                else {
                    push @str, 'return (' . Perlito5::JavaScript3::to_runtime_context([$last_statement], $level) . ');';
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
                  Perlito5::JavaScript3::tab($level) . "try {\n"
                .   join("\n", map($tab . $_, @str)) . "\n"
                . Perlito5::JavaScript3::tab($level)   . '}' . "\n"
                . Perlito5::JavaScript3::tab($level)   . 'catch(err) {' . "\n"
                . Perlito5::JavaScript3::tab($level + 1)   . 'if ( err instanceof Error ) {' . "\n"
                . Perlito5::JavaScript3::tab($level + 2)       . 'throw(err);' . "\n"
                . Perlito5::JavaScript3::tab($level + 1)   . '}' . "\n"
                . Perlito5::JavaScript3::tab($level + 1)   . 'else {' . "\n"
                . Perlito5::JavaScript3::tab($level + 2)
                    . ( $has_local
                      ? 'return p5cleanup_local(local_idx, err)'
                      : 'return(err)'
                      )
                    . ";\n"
                . Perlito5::JavaScript3::tab($level + 1)   . '}' . "\n"
                . Perlito5::JavaScript3::tab($level)   . '}';
        }
        else {
            $out .= join("\n", map($tab . $_, @str));
        }
        $Perlito5::PKG_NAME = $outer_pkg;
        $Perlito5::THROW    = $outer_throw
            if $self->{top_level};
        return $out;
    }

}

package Perlito5::AST::CompUnit;
{
    sub emit_javascript3 {
        my $self = $_[0];
        my $level = $_[1];
        my $str = "(function () {\n"
            .   Perlito5::JavaScript3::LexicalBlock->new( block => $self->{body}, needs_return => 0 )->emit_javascript3( $level + 1 ) . "\n"
            . Perlito5::JavaScript3::tab($level) . "})()\n";
        return $str;
    }
    sub emit_javascript3_program {
        my ($comp_units, %options) = @_;
        $Perlito5::PKG_NAME = 'main';
        my $str;
        $str .= Perlito5::Compiler::do_not_edit("//");
        if ( $options{expand_use} ) {
            $str .= Perlito5::JavaScript3::Runtime->emit_javascript2();
            $str .= Perlito5::JavaScript3::Array->emit_javascript2();
            $str .= Perlito5::JavaScript3::CORE->emit_javascript2();
            $str .= Perlito5::JavaScript3::IO->emit_javascript2();
            $str .= Perlito5::JavaScript3::Sprintf->emit_javascript2();
        }
        $str .= "var p5want = null;\n"
             .  "var " . Perlito5::JavaScript3::pkg_new_var() . " = p5pkg['" . $Perlito5::PKG_NAME . "'];\n";
        for my $comp_unit ( @$comp_units ) {
            $str = $str . $comp_unit->emit_javascript3() . "\n";
        }
        return $str;
    }
}

package Perlito5::AST::Int;
{
    sub emit_javascript3 {
        my $self  = shift;
        my $level = shift;
        $self->{int};
    }
}

package Perlito5::AST::Num;
{
    sub emit_javascript3 {
        my $self  = shift;
        my $level = shift;
        $self->{num};
    }
}

package Perlito5::AST::Buf;
{
    sub emit_javascript3 {
        my $self  = shift;
        my $level = shift;
        Perlito5::JavaScript3::escape_string( $self->{buf} );
    }
}

package Perlito5::AST::Block;
{
    sub emit_javascript3 {
        my $self = shift;
        my $level = shift;

        my $init = "";
        if ($self->{name} eq 'INIT') {
            my $tmp  = 'p5pkg.main._tmp' . Perlito5::JavaScript3::get_label();

            # INIT-blocks execute only once
            $init = Perlito5::JavaScript3::tab($level + 2) . "if ($tmp) { return }; $tmp = 1;\n";

            # TODO - make this execute before anything else

        }

        return 'p5for_lex('
                . "function () {\n"
                .   $init
                .   (Perlito5::JavaScript3::LexicalBlock->new( block => $self->{stmts}, needs_return => 0, top_level => 0 ))->emit_javascript3($level + 2) . "\n"
                . Perlito5::JavaScript3::tab($level + 1) . '}, '
                .   '[0], '
                . $self->emit_javascript3_continue($level) . ', '
                .   '"' . ($self->{label} || "") . '"'
                . ')'
    }
    sub emit_javascript3_continue {
        my $self = shift;
        my $level = shift;

        if (!$self->{continue} || !@{ $self->{continue}{stmts} }) {
            return 'false'
        }

        return
              "function () {\n"
            .   (Perlito5::JavaScript3::LexicalBlock->new( block => $self->{continue}->stmts, needs_return => 0, top_level => 0 ))->emit_javascript3($level + 2) . "\n"
            . Perlito5::JavaScript3::tab($level + 1) . '}'
    }
}

package Perlito5::AST::Index;
{
    sub emit_javascript3 {
        my $self      = shift;
        my $level     = shift;
        my $wantarray = shift;
        my $type      = shift;  # autovivify to 'array'/'hash'

        $type = $type ? '"' . $type . '"' : 'null';

        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && $self->{obj}->sigil eq '$'
           )
        {
            my $v = Perlito5::AST::Var->new( sigil => '@', namespace => $self->{obj}->namespace, name => $self->{obj}->name );
            return $v->emit_javascript3($level) 
                    . '.aget(' . Perlito5::JavaScript3::to_num($self->{index_exp}, $level) . ', ' . $type . ')';
        }
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a[0] ==> $a->[0]
            return Perlito5::JavaScript3::emit_javascript3_autovivify( $self->{obj}{arguments}[0], $level, 'array' )
                . '.aget(' 
                        . Perlito5::JavaScript3::to_num($self->{index_exp}) . ', ' . $type
                . ')';
        }

        return Perlito5::JavaScript3::emit_javascript3_autovivify( $self->{obj}, $level, 'array' )
                . '.aget(' . Perlito5::JavaScript3::to_num($self->{index_exp}, $level) . ', ' . $type . ')';
    }
    sub emit_javascript3_set {
        my $self      = shift;
        my $arguments = shift;
        my $level     = shift;

        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && $self->{obj}->sigil eq '$'
           )
        {
            my $v = Perlito5::AST::Var->new( sigil => '@', namespace => $self->{obj}->namespace, name => $self->{obj}->name );
            return $v->emit_javascript3($level) 
                    . '.aset(' 
                        . Perlito5::JavaScript3::to_num($self->{index_exp}, $level+1) . ', ' 
                        . Perlito5::JavaScript3::to_scalar([$arguments], $level+1)
                    . ')';
        }
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a[0] ==> $a->[0]
            return Perlito5::JavaScript3::emit_javascript3_autovivify( $self->{obj}{arguments}[0], $level, 'array' )
                . '.aset(' 
                        . Perlito5::JavaScript3::to_num($self->{index_exp}) . ', '
                        . Perlito5::JavaScript3::to_scalar([$arguments], $level+1)
                . ')';
        }

        return Perlito5::JavaScript3::emit_javascript3_autovivify( $self->{obj}, $level, 'array' )
                . '.aset(' 
                    . Perlito5::JavaScript3::to_num($self->{index_exp}, $level+1) . ', ' 
                    . Perlito5::JavaScript3::to_scalar([$arguments], $level+1)
                . ')';
    }

}

package Perlito5::AST::Lookup;
{
    sub emit_javascript3 {
        my $self      = shift;
        my $level     = shift;
        my $wantarray = shift;
        my $type      = shift;  # autovivify to 'array'/'hash'

        $type = $type ? '"' . $type . '"' : 'null';

        # my $var = $self->{obj}->emit_javascript3;
        # return $var . '[' . $self->{index_exp}->emit_javascript3() . ']'

        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && $self->{obj}->sigil eq '$'
           )
        {
            my $v = Perlito5::AST::Var->new( sigil => '%', namespace => $self->{obj}->namespace, name => $self->{obj}->name );
            return Perlito5::JavaScript3::emit_javascript3_autovivify( $v, $level, 'hash' ) 
                . '.hget(' . Perlito5::JavaScript3::autoquote($self->{index_exp}, $level) . ', ' . $type . ')';
        }
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a{0} ==> $a->{0}
            return Perlito5::JavaScript3::emit_javascript3_autovivify( $self->{obj}{arguments}[0], $level, 'hash' )
                . '.hget(' . Perlito5::JavaScript3::autoquote($self->{index_exp}, $level, 'list') . ', ' . $type . ')';
        }

          Perlito5::JavaScript3::emit_javascript3_autovivify( $self->{obj}, $level, 'hash' )
        . '.hget(' . Perlito5::JavaScript3::autoquote($self->{index_exp}, $level) . ', ' . $type . ')';
    }
    sub emit_javascript3_set {
        my $self      = shift;
        my $arguments = shift;
        my $level     = shift;
        # my $var = $self->{obj}->emit_javascript3;
        # return $var . '[' . $self->{index_exp}->emit_javascript3() . ']'

        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && $self->{obj}->sigil eq '$'
           )
        {
            my $v = Perlito5::AST::Var->new( sigil => '%', namespace => $self->{obj}->namespace, name => $self->{obj}->name );
            return $v->emit_javascript3($level)
                . '.hset(' . Perlito5::JavaScript3::autoquote($self->{index_exp}, $level) . ', '
                    . Perlito5::JavaScript3::to_scalar([$arguments], $level+1)
                . ')';
        }
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a{0} ==> $a->{0}
            return Perlito5::JavaScript3::emit_javascript3_autovivify( $self->{obj}{arguments}[0], $level, 'hash' )
                . '.hset(' . Perlito5::JavaScript3::autoquote($self->{index_exp}, $level, 'list') . ', '
                    . Perlito5::JavaScript3::to_scalar([$arguments], $level+1)
                . ')';
        }

        Perlito5::JavaScript3::emit_javascript3_autovivify( $self->{obj}, $level, 'hash' )
            . '.hset(' . Perlito5::JavaScript3::autoquote($self->{index_exp}, $level) . ', '
                   . Perlito5::JavaScript3::to_scalar([$arguments], $level+1)
            . ')';
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

    sub emit_javascript3 {
        my $self = shift;
        my $level = shift;
        my $wantarray = shift;

        my $str_name = $self->{name};
        $str_name = '\\\\' if $str_name eq '\\';   # escape $\
        $str_name = '\\"' if $str_name eq '"';     # escape $"

        if ( $self->{sigil} eq '@' ) {
            if ( $wantarray eq 'scalar' ) {
                return $self->emit_javascript3($level, 'list') . '.FETCHSIZE()';
            }
            if ( $wantarray eq 'runtime' ) {
                return '(p5want'
                    . ' ? ' . $self->emit_javascript3($level, 'list')
                    . ' : ' . $self->emit_javascript3($level, 'list') . '.FETCHSIZE()'
                    . ')';
            }
        }

        if ( $self->{sigil} eq '&' ) {
            return 'p5pkg["' . ($self->{namespace} || $Perlito5::PKG_NAME) . '"]["' . $str_name . '"]';
        }
        if ( $self->{sigil} eq '*' ) {
            return 'p5pkg["' . ($self->{namespace} || $Perlito5::PKG_NAME) . '"]["' . $str_name . '"]';
        }
        my $decl_type = $self->{_decl} || 'global';
        if ( $decl_type eq 'our' ) {

            my $sigil = $self->{sigil} eq '$#' ? '@' : $self->{sigil};
            my $s = 'p5pkg["' . ($self->{namespace} || $self->{_namespace}) . '"]["' . $table->{$sigil} . $str_name . '"]';

            if ($self->{sigil} eq '$#') {
                return '(' . $s . '.FETCHSIZE() - 1)';
            }
            return $s;
        }

        my $ns = '';
        if ($self->{namespace}) {

            # if (!$decl_type) 
            {
                # this is an undeclared global
                if ($self->{sigil} eq '$#') {
                    return '(p5global("@", "' . $self->{namespace} . '", "' . $str_name . '").FETCHSIZE() - 1)';
                }
                return 'p5global("' . $self->{sigil} . '", "' . $self->{namespace} . '", "' . $str_name . '")';
            }

            # $ns = 'p5pkg["' . $self->{namespace} . '"]';
            # if ($self->{sigil} eq '$#') {
            #     return '(' . $ns . '["' . $table->{'@'} . $str_name . '"].length - 1)';
            # }
            # return $ns . '["' . $table->{$self->{sigil}} . $str_name . '"]'
        }

        if ($self->{sigil} eq '$#') {
            return '(' . $ns . $table->{'@'} . $str_name . '.FETCHSIZE() - 1)';
        }

        $ns . $table->{$self->{sigil}} . $str_name
    }
}

package Perlito5::AST::Decl;
{
    sub emit_javascript3 {
        my $self = shift;
        my $level = shift;
        $self->{var}->emit_javascript3( $level );
    }
    sub emit_javascript3_init {
        my $self = shift;
        if ($self->{decl} eq 'my') {
            my $str = "";
            $str = $str . 'var ' . $self->{var}->emit_javascript3() . ' = ';
            if ($self->{var}->sigil eq '%') {
                $str = $str . 'new p5Hash({});';
            }
            elsif ($self->{var}->sigil eq '@') {
                $str = $str . 'new p5Array([]);';
            }
            else {
                $str = $str . 'new p5Scalar(null);';
            }
            return $str;
        }
        elsif ($self->{decl} eq 'our') {

            my $str_name = $self->{var}->{name};
            $str_name = '\\\\' if $str_name eq '\\';   # escape $\
            $str_name = '\\"' if $str_name eq '"';     # escape $"

            return 'p5global("' . $self->{var}->{sigil} . '", ' 
                . '"'. ($self->{var}->{namespace} || $Perlito5::PKG_NAME) . '", ' 
                . '"'. $str_name . '")';

        }
        elsif ($self->{decl} eq 'local') {
            my $decl_namespace = $self->{var}->{_namespace};
            my $ns = 'p5pkg["' . ($self->{var}{namespace} || $decl_namespace || $Perlito5::PKG_NAME) . '"]';
            return
                  'p5set_local(' . $ns . ','
                               . Perlito5::JavaScript3::escape_string($self->{var}{name}) . ','
                               . Perlito5::JavaScript3::escape_string($self->{var}{sigil}) . '); ' 
        }
        elsif ($self->{decl} eq 'state') {
            # TODO
            return '// state ' . $self->{var}->emit_javascript3();
        }
        else {
            die "not implemented: Perlito5::AST::Decl '" . $self->{decl} . "'";
        }
    }
}

package Perlito5::AST::Call;
{
    sub emit_javascript3 {
        my $self      = shift;
        my $level     = shift;
        my $wantarray = shift;
        my $type      = shift;  # autovivify to 'array'/'hash'

        $type = $type ? '"' . $type . '"' : 'null';

        my $meth = $self->{method};

        if ( $meth eq 'postcircumfix:<[ ]>' ) {
            return Perlito5::JavaScript3::emit_javascript3_autovivify( $self->{invocant}, $level, 'array' )
                . '.aget(' . Perlito5::JavaScript3::to_num($self->{arguments}) . ', ' . $type . ')';
        }
        if ( $meth eq 'postcircumfix:<{ }>' ) {
            return Perlito5::JavaScript3::emit_javascript3_autovivify( $self->{invocant}, $level, 'hash' )
                . '.hget(' . Perlito5::JavaScript3::autoquote($self->{arguments}, $level, 'list') . ', ' . $type . ')';
        }

        my $invocant = $self->{invocant}->emit_javascript3($level, 'scalar');
        if  ($meth eq 'postcircumfix:<( )>')  {
            return 'p5code(' . $invocant . ')(' . Perlito5::JavaScript3::to_list($self->{arguments}) . ', '
                         .   ($wantarray eq 'list'   ? '1' 
                             :$wantarray eq 'scalar' ? '0' 
                             :$wantarray eq 'void'   ? 'null'
                             :                         'p5want'
                             ) 
                    . ')';
        }
        if ( ref($meth) eq 'Perlito5::AST::Var' ) {
            $meth = $meth->emit_javascript3($level, 'scalar');
        }
        else {
            $meth = '"' . $meth . '"';
        }
        return 'p5call(' . $invocant . ', ' 
                         . $meth . ', ' 
                         . Perlito5::JavaScript3::to_list($self->{arguments}) . ', '
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

    sub emit_regex_javascript3 {
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
            $str = $var->emit_javascript3() 
                 . '.assign(p5str(' . $var->emit_javascript3() . ').replace(/' . $regex_args->[0]->{buf} . '/' . $regex_args->[2]->{buf} . ', '
                 .  $regex_args->[1]->emit_javascript3() . '))';
        }
        elsif ($code eq 'p5:m') {

            my $ast = $regex_args->[0];
            if ($ast->isa('Perlito5::AST::Buf')) {
                # constant

                 $str = '(' 
                    . 'p5str(' . $var->emit_javascript3() . ')'
                    . '.match(/' . $ast->{buf} . '/' . $regex_args->[1]->{buf} . ')'
                    . ' ? 1 : 0)';
            }
            else {
                # run-time interpolation

                $str = '(new RegExp('
                        . $ast->emit_javascript3() . ', '
                        . '"' . $regex_args->[1]->{buf} . '"'
                    . '))'
                    . '.exec('
                        . 'p5str(' . $var->emit_javascript3() . ')'
                    . ')';
            }
        }
        elsif ($code eq 'p5:tr') {
            # TODO: tr/// not implemented
            $str =  
                 'p5tr(' . $var->emit_javascript3() . ', ' . $regex_args->[0]->emit_javascript3() . ', ' . $regex_args->[1]->emit_javascript3() . ')'
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
            emit_regex_javascript3( '=~', $self->{arguments}->[0], $self->{arguments}->[1] );
        },
        'infix:<!~>' => sub {
            my $self = $_[0];
            emit_regex_javascript3( '!~', $self->{arguments}->[0], $self->{arguments}->[1] );
        },
        'p5:s' => sub {
            my $self = $_[0];
            emit_regex_javascript3( '=~', Perlito5::AST::Var->new( sigil => '$', namespace => '', name => '_' ), $self );
        },
        'p5:m' => sub {
            my $self = $_[0];
            emit_regex_javascript3( '=~', Perlito5::AST::Var->new( sigil => '$', namespace => '', name => '_' ), $self );
        },
        'p5:tr' => sub {
            my $self = $_[0];
            emit_regex_javascript3( '=~', Perlito5::AST::Var->new( sigil => '$', namespace => '', name => '_' ), $self );
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
            "var " . Perlito5::JavaScript3::pkg_new_var() . ' = p5make_package("' . $self->{namespace} . '")';
        },
        'list:<=>>' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
              Perlito5::JavaScript3::autoquote($self->{arguments}[0], $level)  . ', ' 
            . $self->{arguments}[1]->emit_javascript3($level)
        },
        'infix:<cmp>' => sub {
            my $self = $_[0];
            'p5cmp(' . join( ', ', map( Perlito5::JavaScript3::to_str($_), @{ $self->{arguments} } ) ) . ')';
        },
        'infix:<<=>>' => sub {
            my $self = $_[0];
            'p5cmp(' . join( ', ', map( Perlito5::JavaScript3::to_num($_), @{ $self->{arguments} } ) ) . ')';
        },
        'infix:<**>' => sub {
            my $self = $_[0];
            'Math.pow(' . join( ', ', map( Perlito5::JavaScript3::to_num($_), @{ $self->{arguments} } ) ) . ')';
        },
        'prefix:<!>' => sub {
            my $self      = shift;
            my $level     = shift;
            '!( ' . Perlito5::JavaScript3::to_bool( $self->{arguments}->[0], $level ) . ')';
        },
        'prefix:<not>' => sub {
            my $self      = shift;
            my $level     = shift;
            '!( ' . Perlito5::JavaScript3::to_bool( $self->{arguments}->[0], $level ) . ')';
        },
        'prefix:<~>' => sub {
            my $self = $_[0];
            'p5complement( ' . Perlito5::JavaScript3::to_num( $self->{arguments}->[0] ) . ')';
        },
        'prefix:<->' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            '-( ' . $self->{arguments}->[0]->emit_javascript3( $level, 'scalar' ) . ')';
        },
        'prefix:<+>' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            '(' . $self->{arguments}->[0]->emit_javascript3( $level, $wantarray ) . ')';
        },
        'require' => sub {
            my $self = $_[0];
            my $level = $_[1];
            my $arg  = $self->{arguments}->[0];
            if ($arg->isa('Perlito5::AST::Num')) {
                # "use 5.006" -- XXX this should be tested at parse time instead
                return '1';
            }
            'p5pkg["Perlito5::Grammar::Use"].require([' 
                . Perlito5::JavaScript3::to_str( $self->{arguments}[0] ) . ', ' 
                . ($self->{arguments}[0]{bareword} ? 1 : 0) 
            . '])';
        },

        'prefix:<$>' => sub {
            my $self = $_[0];
            my $level = $_[1];
            my $arg  = $self->{arguments}->[0];
            Perlito5::JavaScript3::emit_javascript3_autovivify( $arg, $level, 'scalar' ) . '.sderef()';
        },
        'prefix:<@>' => sub {
            my $self  = $_[0];
            my $level = $_[1];
            my $arg   = $self->{arguments}->[0];
            Perlito5::JavaScript3::emit_javascript3_autovivify( $arg, $level, 'array' ) . '.aderef()';
        },
        'prefix:<$#>' => sub {
            my $self  = $_[0];
            my $level = $_[1];
            my $arg   = $self->{arguments}->[0];
            '(' . Perlito5::JavaScript3::emit_javascript3_autovivify( $arg, $level, 'array' ) . '.aderef().FETCHSIZE() - 1)';
        },
        'prefix:<%>' => sub {
            my $self  = $_[0];
            my $level = $_[1];
            my $arg   = $self->{arguments}->[0];
            Perlito5::JavaScript3::emit_javascript3_autovivify( $arg, $level, 'hash' ) . '.hderef()';
        },
        'prefix:<&>' => sub {
            my $self  = $_[0];
            my $level = $_[1];
            my $arg   = $self->{arguments}->[0];
            'p5code_lookup_by_name("' . $Perlito5::PKG_NAME . '", ' . $arg->emit_javascript3($level) . ')';
        },
        'circumfix:<[ ]>' => sub {
            my $self = $_[0];
            my $level = $_[1];
            '(new p5ArrayRef(new p5Array(' . Perlito5::JavaScript3::to_list( $self->{arguments} ) . ')))';
        },
        'circumfix:<{ }>' => sub {
            my $self = $_[0];
            my $level = $_[1];
            '(new p5HashRef(new p5Hash(' . Perlito5::JavaScript3::to_list( $self->{arguments}, $level, 'hash' ) . ')))';
        },
        'prefix:<\\>' => sub {
            my $self  = $_[0];
            my $level = $_[1];
            my $arg   = $self->{arguments}->[0];
            if ( $arg->isa('Perlito5::AST::Var') ) {
                if ( $arg->sigil eq '@' ) {
                    return '(new p5ArrayRef(' . $arg->emit_javascript3($level) . '))';
                }
                if ( $arg->sigil eq '%' ) {
                    return '(new p5HashRef(' . $arg->emit_javascript3($level) . '))';
                }
                if ( $arg->sigil eq '*' ) {
                    return '(new p5GlobRef(' . $arg->emit_javascript3($level) . '))';
                }
                if ( $arg->sigil eq '&' ) {
                    if ( $arg->{namespace} ) {
                        return 'p5pkg["' . $arg->{namespace} . '"].' . $arg->{name};
                    }
                    else {
                        return Perlito5::JavaScript3::pkg() . '.' . $arg->{name};
                    }
                }
            }
            return '(new p5ScalarRef(' . $arg->emit_javascript3($level) . '))';
        },

        'postfix:<++>' => sub {
            my $self = $_[0];
            my $level = $_[1];
            Perlito5::JavaScript3::emit_javascript3_autovivify( $self->{arguments}[0], $level, 'lvalue' )
                . '.p5postincr()';
        },
        'postfix:<-->' => sub {
            my $self = $_[0];
            my $level = $_[1];
            Perlito5::JavaScript3::emit_javascript3_autovivify( $self->{arguments}[0], $level, 'lvalue' )
                . '.p5postdecr()';
        },
        'prefix:<++>' => sub {
            my $self = $_[0];
            my $level = $_[1];
            Perlito5::JavaScript3::emit_javascript3_autovivify( $self->{arguments}[0], $level, 'lvalue' )
                . '.p5incr()';
        },
        'prefix:<-->' => sub {
            my $self = $_[0];
            my $level = $_[1];
            Perlito5::JavaScript3::emit_javascript3_autovivify( $self->{arguments}[0], $level, 'lvalue' )
                . '.p5decr()';
        },

        'infix:<x>' => sub {
            my $self = $_[0];
            'p5str_replicate(' . join( ', ', map( $_->emit_javascript3, @{ $self->{arguments} } ) ) . ')';
        },

        'list:<.>' => sub {
            my $self = $_[0];
            '(' . join( ' + ', map( Perlito5::JavaScript3::to_str($_), @{ $self->{arguments} } ) ) . ')';
        },
        'list:<,>' => sub {
            my $self = $_[0];
            Perlito5::JavaScript3::to_list( $self->{arguments} );
        },

        'infix:<..>' => sub {
            my $self = $_[0];
            '(function (a) { ' . 'for (var i=' . $self->{arguments}->[0]->emit_javascript3() . ', l=' . $self->{arguments}->[1]->emit_javascript3() . '; ' . 'i<=l; ++i)' . '{ ' . 'a.push(i) ' . '}; ' . 'return a ' . '})([])';
        },

        'delete' => sub {
            my $self = $_[0];
            '(delete ' . $self->{arguments}[0]->emit_javascript3() . ')';
        },

        'scalar' => sub {
            my $self = $_[0];
            my $level = $_[1];
            Perlito5::JavaScript3::to_scalar($self->{arguments}, $level+1);
        },

        'ternary:<? :>' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            '( ' . Perlito5::JavaScript3::to_bool( $self->{arguments}->[0] ) . ' ? ' . ( $self->{arguments}->[1] )->emit_javascript3( $level, $wantarray ) . ' : ' . ( $self->{arguments}->[2] )->emit_javascript3( $level, $wantarray ) . ')';
        },
        'my' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;

            # TODO - this is a side-effect of my($x,$y)
            'p5context(' . '[' . join( ', ', map( $_->emit_javascript3( $level, $wantarray ), @{ $self->{arguments} } ) ) . '], ' . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 ) . ')';
        },
        'our' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;

            # TODO - this is a side-effect of our($x,$y)
            'p5context(' . '[' . join( ', ', map( $_->emit_javascript3( $level, $wantarray ), @{ $self->{arguments} } ) ) . '], ' . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 ) . ')';
        },
        'local' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;

            # TODO - this is a side-effect of local($x,$y)
            'p5context(' . '[' . join( ', ', map( $_->emit_javascript3( $level, $wantarray ), @{ $self->{arguments} } ) ) . '], ' . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 ) . ')';
        },
        'circumfix:<( )>' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            'p5context(' . '[' . join( ', ', map( $_->emit_javascript3( $level, $wantarray ), @{ $self->{arguments} } ) ) . '], ' . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 ) . ')';
        },
        'infix:<=>' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;

            my $parameters = $self->{arguments}->[0];
            my $arguments  = $self->{arguments}->[1];

            if (   $parameters->isa( 'Perlito5::AST::Apply' )
               &&  $parameters->code eq 'prefix:<*>' 
               )
            {
                # *{$callpkg."::import"} = \&import;
                return 'p5set_glob('
                        . $parameters->{arguments}->[0]->emit_javascript3() . ', '
                        . Perlito5::JavaScript3::to_scalar([$arguments], $level+1)
                    . ')'
            }

            if (   $parameters->isa( 'Perlito5::AST::Apply' )
               &&  (  $parameters->code eq 'my' 
                   || $parameters->code eq 'our' 
                   || $parameters->code eq 'local' 
                   || $parameters->code eq 'circumfix:<( )>' 
                   )
               )
            {
                # my ($x, $y) = ...
                # local ($x, $y) = ...
                # ($x, $y) = ...

                my $tmp  = 'tmp' . Perlito5::JavaScript3::get_label();
                my $tmp2 = 'tmp' . Perlito5::JavaScript3::get_label();
                return
                  '(function () { '
                    . 'var ' . $tmp  . ' = ' . Perlito5::JavaScript3::to_list([$arguments], $level+1) . '; '
                    . 'var ' . $tmp2 . ' = ' . $tmp . '.slice(0); '
                    . join( '; ',
                            (
                            map +( $_->isa('Perlito5::AST::Apply') && $_->code eq 'undef'
                                 ? $tmp . '.shift()' 
                                 : $_->sigil eq '$' 
                                 ? $_->emit_javascript3() . '.assign(' . $tmp . '.shift())'
                                 : $_->sigil eq '@' 
                                 ? $_->emit_javascript3() . '.assign(' . $tmp . '); ' . $tmp . ' = []'
                                 : $_->sigil eq '%' 
                                 ? $_->emit_javascript3() . '.assign(p5a_to_h(' . $tmp . ')); ' . $tmp . ' = []'
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
                return '' . $parameters->emit_javascript3() . '.assign(' . Perlito5::JavaScript3::to_scalar([$arguments], $level+1) . ')'
            }

            if ( $parameters->isa( 'Perlito5::AST::Call' ) && $parameters->{method} eq 'postcircumfix:<[ ]>' ) {
                return Perlito5::JavaScript3::emit_javascript3_autovivify( $parameters->{invocant}, $level, 'array' )
                    . '.aset(' 
                            . Perlito5::JavaScript3::to_num($parameters->{arguments}) . ', '
                            . Perlito5::JavaScript3::to_scalar([$arguments], $level+1)
                    . ')';
            }
            elsif ( $parameters->isa( 'Perlito5::AST::Call' ) && $parameters->{method} eq 'postcircumfix:<{ }>' ) {
                return Perlito5::JavaScript3::emit_javascript3_autovivify( $parameters->{invocant}, $level, 'hash' )
                    . '.hset(' 
                            . Perlito5::JavaScript3::autoquote($parameters->{arguments}, $level) . ', '
                            . Perlito5::JavaScript3::to_scalar([$arguments], $level+1)
                    . ')';
            }
            elsif  (   $parameters->isa( 'Perlito5::AST::Index') ) {
                return $parameters->emit_javascript3_set($arguments, $level+1);
            }
            elsif  (   $parameters->isa( 'Perlito5::AST::Lookup') ) {
                return $parameters->emit_javascript3_set($arguments, $level+1);
            }
            if  (   $parameters->isa( 'Perlito5::AST::Var' )  && $parameters->sigil eq '@' ) {
                return $parameters->emit_javascript3() . '.assign(new p5Array(' . Perlito5::JavaScript3::to_list([$arguments], $level+1) . '))'
            }
            elsif  ( $parameters->isa( 'Perlito5::AST::Decl' ) && $parameters->var->sigil eq '@' ) {
                return $parameters->var->emit_javascript3() . '.assign(new p5Array(' . Perlito5::JavaScript3::to_list([$arguments], $level+1) . '))'
            }
            elsif ( $parameters->isa( 'Perlito5::AST::Var' )  && $parameters->sigil eq '%'
                ||  $parameters->isa( 'Perlito5::AST::Decl' ) && $parameters->var->sigil eq '%'
                )
            {
                return $parameters->emit_javascript3() . '.assign(new p5Hash(' . Perlito5::JavaScript3::to_list([$arguments], $level+1, 'hash') . '))' 
            }

            if ( $parameters->isa( 'Perlito5::AST::Var' )  && $parameters->sigil eq '*' ) {
                return '(' . $parameters->emit_javascript3( $level ) . ' = ' . $arguments->emit_javascript3( $level+1 ) . ')';
            }

            print Perlito5::Dumper::ast_dumper( $parameters );
            die "assignment: don't know what to do with left side isa ", ref($parameters);
            # $parameters->emit_javascript3( $level ) . '.assign(' . $arguments->emit_javascript3( $level+1 ) . ')';

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
            'throw(' . Perlito5::JavaScript3::to_runtime_context( $self->{arguments}, $level ) . ')';
        },
        'goto' => sub {
            my $self = $_[0];
            $Perlito5::THROW = 1;
            'throw((' . $self->{arguments}->[0]->emit_javascript3() . ')([List__, p5want]))';
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
                          code => 'slurp_file',
                          namespace => 'Perlito5::Grammar::Use',
                          arguments => $self->{arguments}
                        )
                    ]
                );
            $ast->emit_javascript3( $level );
        },

        'eval' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            $Perlito5::THROW = 1;   # we can return() from inside eval

            my $arg = $self->{arguments}->[0];
            my $eval;
            if ($arg->isa( "Perlito5::AST::Block" )) {
                # do BLOCK
                my $block = $arg->{stmts};
                return Perlito5::JavaScript3::emit_wrap_javascript3(
                    $level,
                    $wantarray, 
                    (Perlito5::JavaScript3::LexicalBlock->new( block => $block, needs_return => 1 ))->emit_javascript2( $level + 1, $wantarray )
                )
            }
            else {
                # eval string

                my $var_env_perl5 = Perlito5::Dumper::ast_dumper( [] );
                # print "at eval: ", $var_env_perl5, "\n";
                my $m = Perlito5::Grammar::Expression::term_square( $var_env_perl5, 0 );
                $m = Perlito5::Grammar::Expression::expand_list( Perlito5::Match::flat($m)->[2] );
                # print Perlito5::Dumper::ast_dumper( $m ), "\n";
                my $var_env_js = '(new p5ArrayRef(new p5Array(' . Perlito5::JavaScript3::to_list($m) . ')))';
                $eval ='eval(perl5_to_js(' 
                            . Perlito5::JavaScript3::to_str($arg) . ", "
                            . '"' . $Perlito5::PKG_NAME . '", '
                            . $var_env_js . ', '
                            . '"' . $wantarray . '"'
                        . "))";
            }

            # TODO - test return() from inside eval

                "(function (p5want) {\n"
                    . "var r = null;\n"
                    . 'p5pkg["main"]["v_@"].assign("");' . "\n"
                    . "try {\n"
                        . 'r = ' . $eval . "\n"
                    . "}\n"
                    . "catch(err) {\n"
                    .    "if ( err instanceof p5_error ) {\n"
                    .        'p5pkg["main"]["v_@"].assign(err);' . "\n"
                    .    "}\n"
                    .    "else if ( err instanceof Error ) {\n"
                    .        'p5pkg["main"]["v_@"].assign(err);' . "\n"
                    .    "}\n"
                    .    "else {\n"
                    .        "return(err);\n" 
                    .    "}\n"
                    . "}\n"
                    . "return r;\n"
                . "})(" 
                    .   ($wantarray eq 'list'   ? '1' 
                        :$wantarray eq 'scalar' ? '0' 
                        :$wantarray eq 'void'   ? 'null'
                        :                         'p5want'
                        ) 
                    . ")"

        },

        'undef' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            if ( $self->{arguments} && @{$self->{arguments}} ) {
                return $self->{arguments}->[0]->emit_javascript3 . '.assign(null)'
            }
            return 'null'
        },

        'shift' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            if ( $self->{arguments} && @{$self->{arguments}} ) {
                return Perlito5::JavaScript3::pkg() . '.shift([' . join(', ', map( $_->emit_javascript3( $level ), @{$self->{arguments}} )) . '])'
            }
            return Perlito5::JavaScript3::pkg() . '.shift([List__])'
        },

        'map' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            my @in  = @{$self->{arguments}};
            my $fun  = shift @in;
            my $list = Perlito5::JavaScript3::to_list(\@in);

            if (ref($fun) eq 'Perlito5::AST::Block') {
                $fun = $fun->{stmts}
            }
            else {
                $fun = [$fun];
            }

            'p5map(' . Perlito5::JavaScript3::pkg() . ', '

                    . 'function (p5want) {' . "\n"
                    .   (Perlito5::JavaScript3::LexicalBlock->new( block => $fun, needs_return => 1, top_level => 0 ))->emit_javascript3( $level + 1 ) . "\n"
                    . Perlito5::JavaScript3::tab($level) . '}, '

                    .   $list
                    . ')';
        },
        'grep' => sub {
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            my @in  = @{$self->{arguments}};
            my $fun  = shift @in;
            my $list = Perlito5::JavaScript3::to_list(\@in);

            if (ref($fun) eq 'Perlito5::AST::Block') {
                $fun = $fun->{stmts}
            }
            else {
                $fun = [$fun];
            }

            'p5grep(' . Perlito5::JavaScript3::pkg() . ', '

                    . 'function (p5want) {' . "\n"
                    .   (Perlito5::JavaScript3::LexicalBlock->new( block => $fun, needs_return => 1, top_level => 0 ))->emit_javascript3( $level + 1 ) . "\n"
                    . Perlito5::JavaScript3::tab($level) . '}, '

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

            if (ref($in[0]) eq 'Perlito5::AST::Block') {
                # the sort function is optional
                $fun = shift @in;
                $fun =
                      'function (p5want) {' . "\n"
                    .   (Perlito5::JavaScript3::LexicalBlock->new( block => $fun->{stmts}, needs_return => 1, top_level => 0 ))->emit_javascript3( $level + 1 ) . "\n"
                    . Perlito5::JavaScript3::tab($level) . '}'
            }
            else {
                $fun = 'null';
            }
            $list = Perlito5::JavaScript3::to_list(\@in);

            'p5sort(' . Perlito5::JavaScript3::pkg() . ', '
                    .   $fun . ', '
                    .   $list
                    . ')';
        },

        'infix:<//>' => sub { 
            my $self      = shift;
            my $level     = shift;
            my $wantarray = shift;
            'p5defined_or' . '('
                . $self->{arguments}->[0]->emit_javascript3($level, 'scalar') . ', '
                . Perlito5::JavaScript3::emit_function_javascript3($level, $wantarray, $self->{arguments}->[1]) 
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
                    return '(' . $v->emit_javascript3() . ').exists(' . Perlito5::JavaScript3::autoquote($arg->{index_exp}, $level) . ')';
                }
                return '(' . $v->emit_javascript3() . ').hderef().exists(' . Perlito5::JavaScript3::autoquote($arg->{index_exp}, $level) . ')';
            }
            if ($arg->isa( 'Perlito5::AST::Call' )) {
                if ( $arg->method eq 'postcircumfix:<{ }>' ) {
                    return '(' . $arg->invocant->emit_javascript3() . ').hderef().exists(' . Perlito5::JavaScript3::autoquote($arg->{arguments}, $level) . ')';
                }
            }
        },

    );


    sub emit_javascript3 {
        my $self  = shift;
        my $level = shift;
        my $wantarray = shift;

        my $apply = $self->op_assign();
        if ($apply) {
            return $apply->emit_javascript3( $level );
        }

        my $code = $self->{code};

        if (ref $code ne '') {
            my @args = ();
            push @args, $_->emit_javascript3
                for @{$self->{arguments}};
            return 'p5code(' . $self->{code}->emit_javascript3( $level ) . ')(' . join(',', @args) . ')';
        }

        return $emit_js{$code}->($self, $level, $wantarray)
            if exists $emit_js{$code};

        if (exists $Perlito5::JavaScript3::op_infix_js_str{$code}) {
            return '(' 
                . join( $Perlito5::JavaScript3::op_infix_js_str{$code}, map( Perlito5::JavaScript3::to_str($_), @{$self->{arguments}} ))
                . ')'
        }
        if (exists $Perlito5::JavaScript3::op_infix_js_num{$code}) {
            return '(' 
                . join( $Perlito5::JavaScript3::op_infix_js_num{$code}, map( Perlito5::JavaScript3::to_num($_), @{$self->{arguments}} ))
                . ')'
        }
        if (exists $Perlito5::JavaScript3::op_prefix_js_str{$code}) {
            return $Perlito5::JavaScript3::op_prefix_js_str{$code} . '(' 
                . Perlito5::JavaScript3::to_str($self->{arguments}[0])
                . ')'
        }

        if (  $code eq 'infix:<&&>'
           || $code eq 'infix:<and>'
           )
        {
            return 'p5and' . '('
                . $self->{arguments}->[0]->emit_javascript3($level, 'scalar') . ', '
                . Perlito5::JavaScript3::emit_function_javascript3($level, $wantarray, $self->{arguments}->[1]) 
                . ')'
        }
        if (  $code eq 'infix:<||>'
           || $code eq 'infix:<or>'
           )
        {
            return 'p5or' . '('
                . $self->{arguments}->[0]->emit_javascript3($level, 'scalar') . ', '
                . Perlito5::JavaScript3::emit_function_javascript3($level, $wantarray, $self->{arguments}->[1]) 
                . ')'
        }

        if ($self->{namespace}) {

            if (  $self->{namespace} eq 'JS' 
               && $code eq 'inline'
               ) 
            {
                if ( $self->{arguments}->[0]->isa('Perlito5::AST::Buf') ) {
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
            $code = Perlito5::JavaScript3::pkg() . '.' . $code
        }

        my $sig;
        my $may_need_autoload;
        {
            my $name = $self->{code};
            my $namespace = $self->{namespace} || $Perlito5::PKG_NAME;
            my $effective_name = $namespace . "::" . $self->{code};
            if ( exists $Perlito5::PROTO->{$effective_name} ) {
                $sig = $Perlito5::PROTO->{$effective_name};
            }
            elsif ( (!$self->{namespace} || $namespace eq 'CORE')
                  && Perlito5::is_core_sub("CORE::$name")
                  )
            {
                $effective_name = "CORE::$name";
                $sig = Perlito5::get_prototype_core($effective_name);
            }
            else {
                # this subroutine was never declared
                if ($self->{bareword}) {
                    # die 'Bareword "' . $name . '" not allowed while "strict subs" in use';
                    return Perlito5::JavaScript3::escape_string( 
                            ($self->{namespace} ? $self->{namespace} . '::' : "") . $name 
                        );
                }
                $may_need_autoload = 1;
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
                    push @out, shift(@in)->emit_javascript3( $level, 'scalar' ) if @in || !$optional;
                }
                elsif ($c eq '@') {
                    push @out, 'new p5Array(' . Perlito5::JavaScript3::to_list(\@in) . ')'
                        if @in || !$optional;
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
                            push @out, $arg->emit_javascript3( $level, 'scalar' );
                        }
                    }
                }
                elsif ($c eq '\\') {
                    if (substr($sig, 0, 2) eq '\\$') {
                        $sig = substr($sig, 1);
                        push @out, shift(@in)->emit_javascript3( $level, 'scalar' ) if @in || !$optional;
                    }
                    elsif (substr($sig, 0, 2) eq '\\@'
                        || substr($sig, 0, 2) eq '\\%'
                        )
                    {
                        $sig = substr($sig, 1);
                        push @out, shift(@in)->emit_javascript3( $level, 'list' ) if @in || !$optional;
                    }
                    elsif (substr($sig, 0, 5) eq '\\[@%]') {
                        $sig = substr($sig, 4);
                        push @out, shift(@in)->emit_javascript3( $level, 'list' ) if @in || !$optional;
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

        # TODO - make a list of 'lvalue' (do not use to_list_preprocess)
        #      - also when there is a $sig
        #      - see t5/01-perlito/17-hash-autovivify.t

        # TODO - find out which internals are overridable; the ones that are not overridable can be optimized

        my @args = ();
        my $arg_list = Perlito5::JavaScript3::to_list_preprocess( $self->{arguments} );
        push @args, $_->emit_javascript3( $level )
            for @$arg_list;

        my $arg_code = 
            $self->{code} eq 'scalar'      # scalar() is special
            ? '[' . join(', ', @args) . ']'
            : 'p5param_list('
              .   join(', ', map( $_->emit_javascript3($level, "list", "lvalue"), @$arg_list) )
              . ')';

        if ( $may_need_autoload ) {
            # p5call_sub(namespace, name, list, p5want)
            my $name = $self->{code};
            my $namespace = $self->{namespace} || $Perlito5::PKG_NAME;
            return 'p5call_sub('
                    . '"' . $namespace . '", '
                    . '"' . $name . '", '
                    . $arg_code . ', '
                    .   ($wantarray eq 'list'   ? '1' 
                        :$wantarray eq 'scalar' ? '0' 
                        :$wantarray eq 'void'   ? 'null'
                        :                         'p5want'
                        ) 
                 . ')';

        }

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
    sub emit_javascript3 {
        my $self = shift;
        my $level = shift;
        my $cond = $self->{cond};
        my $body  = Perlito5::JavaScript3::LexicalBlock->new( block => $self->{body}->stmts, needs_return => 0, create_context => 1 );
        my $s = 'if ( ' . Perlito5::JavaScript3::to_bool($cond, $level + 1) . ' ) {' . "\n"
            .       $body->emit_javascript3( $level + 1 ) . "\n"
            . Perlito5::JavaScript3::tab($level) . '}';
        if ( @{ $self->{otherwise}->stmts } ) {
            my $otherwise = Perlito5::JavaScript3::LexicalBlock->new( block => $self->{otherwise}->stmts, needs_return => 0, create_context => 1 );
            $s = $s
                . "\n"
                . Perlito5::JavaScript3::tab($level) . 'else {' . "\n"
                .       $otherwise->emit_javascript3( $level + 1 ) . "\n"
                . Perlito5::JavaScript3::tab($level) . '}';
        }
        return $s;
    }
}


package Perlito5::AST::When;
{
    sub emit_javascript3 {
        my $self = shift;
        my $level = shift;
        my $cond = $self->{cond};
        my $body  = Perlito5::JavaScript3::LexicalBlock->new( block => $self->{body}->stmts, needs_return => 0, create_context => 1 );

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

        my $label = '';  # TODO

        my $s = 'if ( ' . Perlito5::JavaScript3::to_bool($expr, $level + 1) . ' ) {' . "\n"
            .       $body->emit_javascript3( $level + 1 ) . "\n"

            . Perlito5::JavaScript3::tab($level+1) . 'throw(new p5_error("next", "' . $label . '"))'
            . Perlito5::JavaScript3::tab($level) . '}';
        return $s;
    }
}


package Perlito5::AST::While;
{
    sub emit_javascript3 {
        my $self = shift;
        my $level = shift;

        my $cond = $self->{cond};

        return 'p5while('
                    . "function () {\n"
                    .   (Perlito5::JavaScript3::LexicalBlock->new( block => $self->{body}->stmts, needs_return => 0, top_level => 0 ))->emit_javascript3($level + 2) . "\n"
                    . Perlito5::JavaScript3::tab($level + 1) . '}, '
                    . Perlito5::JavaScript3::emit_function_javascript3($level, 0, $cond) . ', '
                    . Perlito5::AST::Block::emit_javascript3_continue($self, $level) . ', '
                    .   '"' . ($self->{label} || "") . '"'
                    . ')'
    }
}

package Perlito5::AST::For;
{
    sub emit_javascript3 {
        my $self = shift;
        my $level = shift;

        if (ref($self->{cond}) eq 'ARRAY') {
            # C-style for

            # TODO - catch next/last/redo
            # TODO - loop label
            # TODO - continue-block is a syntax error

            my $body      = Perlito5::JavaScript3::LexicalBlock->new( block => $self->{body}->stmts, needs_return => 0, create_context => 1 );
            return
               'for ( '
            .  ( $self->{cond}[0] ? $self->{cond}[0]->emit_javascript3($level + 1) . '; '  : '; ' )
            .  ( $self->{cond}[1] ? $self->{cond}[1]->emit_javascript3($level + 1) . '; '  : '; ' )
            .  ( $self->{cond}[2] ? $self->{cond}[2]->emit_javascript3($level + 1) . ' '   : ' '  )
            .  ') {' . "\n" 
                . $body->emit_javascript3( $level + 1 ) . "\n"
            .  Perlito5::JavaScript3::tab($level) . '}'
        }

        my $cond = Perlito5::JavaScript3::to_list([$self->{cond}], $level + 1);
        if ($self->{topic}) {
            # XXX - cleanup: "for" parser throws away the variable declaration, so we need to create it again
            # TODO - for without "my"

            # mark the variable as "declared"
            my $v = $self->{topic};
            my $sig = $v->emit_javascript3( $level + 1 );
            return 'p5for_lex('
                    . "function ($sig) {\n"
                    .   (Perlito5::JavaScript3::LexicalBlock->new( block => $self->{body}->stmts, needs_return => 0, top_level => 0 ))->emit_javascript3($level + 2) . "\n"
                    . Perlito5::JavaScript3::tab($level + 1) . '}, '
                    .   $cond . ', '
                    . Perlito5::AST::Block::emit_javascript3_continue($self, $level) . ', '
                    .   '"' . ($self->{label} || "") . '"'
                    . ')'
        }
        else {
            # use $_
            return 'p5for(' . Perlito5::JavaScript3::pkg() . ', '
                    . 'function () {' . "\n"
                    .   (Perlito5::JavaScript3::LexicalBlock->new( block => $self->{body}->stmts, needs_return => 0, top_level => 0 ))->emit_javascript3($level + 2) . "\n"
                    . Perlito5::JavaScript3::tab($level + 1) . '}, '
                    .   $cond . ', '
                    . Perlito5::AST::Block::emit_javascript3_continue($self, $level) . ', '
                    .   '"' . ($self->{label} || "") . '"'
                    . ')'
        }
    }
}

package Perlito5::AST::Sub;
{
    sub emit_javascript3 {
        my $self = shift;
        my $level = shift;

        my $s =
          'function (List__, p5want) {' . "\n"
        . Perlito5::JavaScript3::tab($level+1) . 'List__ = new p5Array(List__);' . "\n"
        .   (Perlito5::JavaScript3::LexicalBlock->new( block => $self->{block}, needs_return => 1, top_level => 1 ))->emit_javascript3( $level ) . "\n"
        . Perlito5::JavaScript3::tab($level) . '}';

        if ( $self->{name} ) {
            return 'p5typeglob_set("' . $self->{namespace} . '", "' . $self->{name} . '", ' . $s . ')'
        }
        else {
            return $s;
        }
    }
}

1;

=begin

=head1 NAME

Perlito5::JavaScript3::Emit - Code generator for Perlito Perl5-in-JavaScript

=head1 SYNOPSIS

    $program->emit_javascript3()  # generated Perl5 code

=head1 DESCRIPTION

This module generates JavaScript3 code for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
