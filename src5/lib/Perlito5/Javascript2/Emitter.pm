use v5;

use Perlito5::AST;
use Perlito5::Dumper;
use strict;

package Perlito5::Javascript2;
{
    my $label_count = 100;
    my %label;
    sub pkg {
        'p5pkg["' . $Perlito5::PKG_NAME . '"]'
    }
    sub get_label {
        'tmp' . $label_count++
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
        # 'infix:<%>'  => ' % ',    # see p5modulo()
        'infix:<>>'  => ' > ',
        'infix:<<>'  => ' < ',
        'infix:<>=>' => ' >= ',
        'infix:<<=>' => ' <= ',
        'infix:<&>'  => ' & ',
        'infix:<|>'  => ' | ',
        'infix:<^>'  => ' ^ ',
        'infix:<>>>' => ' >>> ',
        # 'infix:<<<>' => ' << ',   # see p5shift_left()
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
        lc
        uc
        lcfirst
        ucfirst
        ref
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
                return $cond->emit_javascript2($level, $wantarray);
            }
            else {
                return 'p5str(' . $cond->emit_javascript2($level, $wantarray) . ')';
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
                return $cond->emit_javascript2($level, $wantarray);
            }
            else {
                return 'p5num(' . $cond->emit_javascript2($level, $wantarray) . ')';
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
                return $cond->emit_javascript2($level, $wantarray);
            }
            else {
                return 'p5bool(' . $cond->emit_javascript2($level, $wantarray) . ')';
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
                #  || $_[0]->{code} eq 'prefix:<\\>'    -- \(@a) is a list
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
                    $k = $k->emit_javascript2($level, 0);

                    $printable = 0
                        if $k =~ /[ \[]/;

                    $v = $v
                         ? $v->emit_javascript2($level, 0)
                         : 'null';
                    push @out, "$k : $v";
                }

                return '{' . join(', ', @out) . '}'
                    if $printable;

            }
            return 'p5a_to_h(' . to_list($items, $level, 'array') . ')';
        }

        $interpolate
        ? ( 'p5list_to_a(['
          .   join(', ', map( $_->emit_javascript2($level, $wantarray), @$items ))
          . '])'
          )
        : ( '['
          .   join(', ', map( $_->emit_javascript2($level, $wantarray), @$items ))
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
          .   join(', ', map( $_->emit_javascript2($level, $wantarray), @$items ))
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

        return $items->[0]->emit_javascript2($level, $wantarray)
            if @$items == 1 && is_scalar($items->[0]);

        'p5context(' 
            . '['
            .   join(', ', map( $_->emit_javascript2($level, $wantarray), @$items ))
            . ']'
            . ', p5want)'
    }

    sub to_context {
        my $wantarray = shift;
         $wantarray eq 'list'   ? '1' 
        :$wantarray eq 'scalar' ? '0' 
        :$wantarray eq 'void'   ? 'null'
        :                         'p5want'
    }

    sub autoquote {
        my $index = shift;
        my $level = shift;
    
        # ok   ' sub x () { 123 } $v{x()} = 12; use Data::Dumper; print Dumper \%v '       # '123'     => 12
        # ok   ' sub x () { 123 } $v{x} = 12; use Data::Dumper; print Dumper \%v '         # 'x'       => 12
        # TODO ' sub x () { 123 } $v{main::x} = 12; use Data::Dumper; print Dumper \%v '   # '123'     => 12
        # ok   ' $v{main::x} = 12; use Data::Dumper; print Dumper \%v '                    # 'main::x' => 12
    
        $index = Perlito5::AST::Lookup->autoquote($index);
    
        return to_str($index, $level);
    }

    sub emit_javascript2_autovivify {
        my $obj = shift;
        my $level = shift;
        my $type = shift;  # 'array'/'hash'

        if (  $obj->isa( 'Perlito5::AST::Index' )
           || $obj->isa( 'Perlito5::AST::Lookup' )
           || $obj->isa( 'Perlito5::AST::Call' )
           )
        {
            return $obj->emit_javascript2($level, 0, $type);
        }

        if ( $obj->isa( 'Perlito5::AST::Apply' ) && $obj->code eq 'prefix:<$>' ) {
            my $arg  = $obj->{arguments}->[0];
            return 'p5scalar_deref(' 
                    . $arg->emit_javascript2( $level ) . ', '
                    . Perlito5::Javascript2::escape_string($Perlito5::PKG_NAME) . ', '
                    . Perlito5::Javascript2::escape_string($type)      # autovivification type
                    . ')';
        }
 
        # TODO - Perlito5::AST::Var

          '(' .  $obj->emit_javascript2($level)
        .   ' || (' . $obj->emit_javascript2($level) . ' = ' 
                    . ( $type eq 'array' ? 'new p5ArrayRef([])' 
                      : $type eq 'hash'  ? 'new p5HashRef({})'
                      :                    'new p5ScalarRef(null)'
                      )
              . ')'
        . ')'
    }

    sub emit_javascript2_list_with_tabs {
        my ($level, $argument) = @_;
        my $tab = Perlito5::Javascript2::tab($level);
        return map { ref($_) eq 'ARRAY'
                     ? emit_javascript2_list_with_tabs($level+1, $_)
                     : $tab . $_
                   }
                   @$argument;
    }

    sub emit_func_javascript2 {
        my ($level, $wantarray, @argument) = @_;
        return join("\n", "function () {",
                          emit_javascript2_list_with_tabs($level, [
                                \@argument, "}"
                          ]));
    }

    sub emit_wrap_javascript2 {
        my ($level, $wantarray, @argument) = @_;
        return join("\n", "(function () {",
                          emit_javascript2_list_with_tabs($level, [
                                \@argument, "})()"
                          ]));
    }

    sub emit_function_javascript2 {
        my ($level, $wantarray, $argument) = @_;
        if ( $argument->isa( 'Perlito5::AST::Apply' ) && $argument->code eq 'return' ) {
            emit_func_javascript2( $level, $wantarray,
                $argument->emit_javascript2($level, $wantarray)
            );
        }
        else {
            emit_func_javascript2( $level, $wantarray,
                'return ' . $argument->emit_javascript2($level+1, $wantarray)
            );
        }
    }

    sub emit_wrap_statement_javascript2 {
        my ($level, $wantarray, $argument) = @_;
        if ($wantarray eq 'void') {
            return $argument;
        }
        emit_wrap_javascript2( $level, $wantarray, $argument )
    }

}

package Perlito5::Javascript2::LexicalBlock;
{
    sub new { my $class = shift; bless {@_}, $class }
    sub block { $_[0]->{block} }
    sub needs_return { $_[0]->{needs_return} }
    # top_level - true if this is the main block in a subroutine;
    #             false if this is an if-block or other block inside a statement.
    sub top_level { $_[0]->{top_level} }
    # sub create_context ... 

    sub has_decl {
        my $self = $_[0];
        my $type = $_[1];
        for my $decl ( @{$self->{block}} ) {
            return 1
                if grep { $_->{decl} eq $type } $decl->emit_javascript2_get_decl();
        }
        return 0;
    }


    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        my $original_level = $level;

        my @block;
        for (@{$self->{block}}) {
            if (defined($_)) {
                push @block, $_
            }
        }
        if (!@block) {
            return 'null;';
        }
        my @str;
        my $has_local = $self->has_decl("local");
        my $create_context = $self->{create_context} && $self->has_decl("my");
        my $outer_pkg   = $Perlito5::PKG_NAME;
        my $outer_throw = $Perlito5::THROW;
        unshift @{ $Perlito5::VAR }, {};

        $Perlito5::THROW = 0
            if $self->{top_level};

        if ($self->{top_level} || $create_context) {
            $level++;
        }

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

            my @var_decl = $decl->emit_javascript2_get_decl();
            for my $arg (@var_decl) {

                # my $perl5_name = $arg->{'var'}->perl5_name();
                # if ( $Perlito5::VAR->[0]->{$perl5_name} ) {
                #     # TODO - create a new context for the redeclared variable
                #     # print "redeclared $perl5_name\n"
                # }

                push @str, $arg->emit_javascript2_init($level, $wantarray);
            }

            if (!( $decl->isa( 'Perlito5::AST::Decl' ) && $decl->decl eq 'my' )) {
                push @str, $decl->emit_javascript2($level, 'void') . ';';
            }
        }

        if ($self->{needs_return} && $last_statement) {

            my @var_decl = $last_statement->emit_javascript2_get_decl();
            for my $arg (@var_decl) {

                my $perl5_name = $arg->{'var'}->perl5_name();
                if ( $Perlito5::VAR->[0]->{$perl5_name} ) {
                    # TODO - create a new context for the redeclared variable
                    # print "redeclared $perl5_name\n"
                }

                push @str, $arg->emit_javascript2_init($level, $wantarray);
            }

            if  (  $last_statement->isa( 'Perlito5::AST::Apply' ) 
                && $last_statement->code eq 'return'
                && $self->{top_level}
                && @{ $last_statement->{arguments} }
                ) 
            {
                $last_statement = $last_statement->{arguments}[0];
            }

            if    (  $last_statement->isa( 'Perlito5::AST::For' )
                  || $last_statement->isa( 'Perlito5::AST::While' )
                  || $last_statement->isa( 'Perlito5::AST::If' )
                  || $last_statement->isa( 'Perlito5::AST::Lit::Block' )
                  || $last_statement->isa( 'Perlito5::AST::Use' )
                  || $last_statement->isa( 'Perlito5::AST::Apply' ) && $last_statement->code eq 'goto'
                  || $last_statement->isa( 'Perlito5::AST::Apply' ) && $last_statement->code eq 'return'
                  )
            {
                push @str, $last_statement->emit_javascript2($level, 'runtime');
            }
            else {
                if ( $has_local ) {
                    push @str, 'return p5cleanup_local(local_idx, (' . Perlito5::Javascript2::to_runtime_context([$last_statement], $level+1) . '));';
                }
                else {
                    push @str, 'return (' . Perlito5::Javascript2::to_runtime_context([$last_statement], $level+1) . ');';
                }
            }
        }
        if ( $has_local ) {
            unshift @str, 'var local_idx = p5LOCAL.length;';
            push    @str, 'p5cleanup_local(local_idx, null);';
        }
        my $out;
        if ($self->{top_level} && $Perlito5::THROW) {

            # TODO - emit error message if catched a "next/redo/last LABEL" when expecting a "return" exception

            $level = $original_level;
            my $tab = "\n" . Perlito5::Javascript2::tab($level + 1);
            $out =                                         "try {"
                . $tab                                   .    join($tab, @str) . "\n"
                . Perlito5::Javascript2::tab($level)     . '}' . "\n"
                . Perlito5::Javascript2::tab($level)     . 'catch(err) {' . "\n"
                . Perlito5::Javascript2::tab($level + 1) .    'if ( err instanceof Error ) {' . "\n"
                . Perlito5::Javascript2::tab($level + 2)         . 'throw(err);' . "\n"
                . Perlito5::Javascript2::tab($level + 1) .    '}' . "\n"
                . Perlito5::Javascript2::tab($level + 1) .    'else {' . "\n"
                . Perlito5::Javascript2::tab($level + 2)
                    . ( $has_local
                      ? 'return p5cleanup_local(local_idx, err)'
                      : 'return(err)'
                      )
                    . ";\n"
                . Perlito5::Javascript2::tab($level + 1) .   '}' . "\n"
                . Perlito5::Javascript2::tab($level)     . '}';
        }
        elsif ( $create_context ) {
            $level = $original_level;
            my $tab = "\n" . Perlito5::Javascript2::tab($level + 1);
            $out =                                        "(function () {"
                  . $tab                               .     join($tab, @str) . "\n"
                  . Perlito5::Javascript2::tab($level) .  "})();";
        }
        else {
            $level = $original_level;
            my $tab = "\n" . Perlito5::Javascript2::tab($level);
            $out = join($tab, @str);
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
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        $wantarray = '';
        return Perlito5::Javascript2::emit_wrap_javascript2($level, $wantarray, 
            Perlito5::Javascript2::LexicalBlock->new( block => $self->{body}, needs_return => 0 )->emit_javascript2( $level + 1 )
        );
    }
    sub emit_javascript2_program {
        my $comp_units = shift;
        $Perlito5::PKG_NAME = 'main';
        my $str = ''
                .  "var p5want;\n"
                .  "var List__ = [];\n";
        $Perlito5::VAR = [
            { '@_'    => { decl => 'my',                      }, # TODO - verify
              '$@'    => { decl => 'our', namespace => 'main' },
              '$|'    => { decl => 'our', namespace => 'main' },
              '$/'    => { decl => 'our', namespace => 'main' },
              '$"'    => { decl => 'our', namespace => 'main' },
              '$,'    => { decl => 'our', namespace => 'main' },
              '$!'    => { decl => 'our', namespace => 'main' },
              '$;'    => { decl => 'our', namespace => 'main' },
              '$?'    => { decl => 'our', namespace => 'main' },
              '$['    => { decl => 'our', namespace => 'main' },
              '$^O'   => { decl => 'our', namespace => 'main' },
              '$^V'   => { decl => 'our', namespace => 'main' },
              '%ENV'  => { decl => 'our', namespace => 'main' },
              '%INC'  => { decl => 'our', namespace => 'main' },
              '%SIG'  => { decl => 'our', namespace => 'main' },
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
            $str = $str . $comp_unit->emit_javascript2() . "\n";
        }
        return $str;
    }
    sub emit_javascript2_get_decl { () }
}

package Perlito5::AST::Val::Int;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        $self->{int};
    }
    sub emit_javascript2_get_decl { () }
}

package Perlito5::AST::Val::Num;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        $self->{num};
    }
    sub emit_javascript2_get_decl { () }
}

package Perlito5::AST::Val::Buf;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        Perlito5::Javascript2::escape_string( $self->{buf} );
    }
    sub emit_javascript2_get_decl { () }
}

package Perlito5::AST::Lit::Block;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        my $body;
        if ($wantarray eq 'runtime') {
            $body = Perlito5::Javascript2::LexicalBlock->new( block => $self->{stmts}, needs_return => 1 );
        }
        else {
            $body = Perlito5::Javascript2::LexicalBlock->new( block => $self->{stmts}, needs_return => 0, top_level => 0 );
        }

        my $init = "";
        if ($self->{name} eq 'INIT') {
            my $tmp  = 'p5pkg.main.' . Perlito5::Javascript2::get_label();

            # INIT-blocks execute only once
            $init = Perlito5::Javascript2::tab($level + 2) . "if ($tmp) { return }; $tmp = 1;\n";

            # TODO - make this execute before anything else

        }

        return 'p5for_lex('
                . "function () {\n"
                .                                             $init
                . Perlito5::Javascript2::tab($level + 2) .    $body->emit_javascript2($level + 2) . "\n"
                . Perlito5::Javascript2::tab($level + 1) . '}, '
                .   '[0], '
                . $self->emit_javascript2_continue($level) . ', '
                . Perlito5::Javascript2::escape_string($self->{label} || "") . "\n"
                . Perlito5::Javascript2::tab($level) . ')'
    }
    sub emit_javascript2_continue {
        my $self = shift;
        my $level = shift;

        if (!$self->{continue} || !@{ $self->{continue}{stmts} }) {
            return 'false'
        }

        return
              "function () {\n"
            .   (Perlito5::Javascript2::LexicalBlock->new( block => $self->{continue}->stmts, needs_return => 0, top_level => 0 ))->emit_javascript2($level + 2) . "\n"
            . Perlito5::Javascript2::tab($level + 1) . '}'
    }
    sub emit_javascript2_get_decl { () }
}

package Perlito5::AST::Index;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray, $autovivification_type) = @_;
        # autovivification_type: array, hash
        my $method = $autovivification_type || 'p5aget';
        $method = 'p5aget_array' if $autovivification_type eq 'array';
        $method = 'p5aget_hash'  if $autovivification_type eq 'hash';
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<@>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && $self->{obj}->sigil eq '@'
              )
           )
        {
            # @a[10, 20]
            # @$a[0, 2] ==> @{$a}[0,2]
            return Perlito5::Javascript2::emit_wrap_javascript2($level, $wantarray, 
                    'var a = [];',
                    'var v = ' . Perlito5::Javascript2::to_list([$self->{index_exp}], $level) . ';',
                    'var src=' . $self->{obj}->emit_javascript2($level) . ';',
                    'for (var i=0, l=v.length; i<l; ++i)' . '{',
                          [ 'a.push(src.' . $method . '(v[i]))' ],
                    '}',
                    'return a', 
            )
        }
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<%>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && $self->{obj}->sigil eq '%'
              )
           )
        {
            # Perl5.20 hash slice
            # %a[10, 20]
            # %$a[0, 2] ==> %{$a}[0,2]

            # "fix" the sigil type
            my $obj = $self->{obj};
            $obj->{sigil} = '@'
                if $obj->{sigil} eq '%';
            $obj->{code} = 'prefix:<@>'
                if $obj->{code} eq 'prefix:<%>';

            return Perlito5::Javascript2::emit_wrap_javascript2($level, $wantarray, 
                    'var a = [];',
                    'var v = ' . Perlito5::Javascript2::to_list([$self->{index_exp}], $level) . ';',
                    'var src=' . $obj->emit_javascript2($level) . ';',
                    'for (var i=0, l=v.length; i<l; ++i)' . '{',
                          [ 'a.push(v[i]);',
                            'a.push(src.' . $method . '(v[i]))',
                          ],
                    '}',
                    'return a',
            )
        }
        return $self->emit_javascript2_container($level) . '.' . $method . '(' 
                        . Perlito5::Javascript2::to_num($self->{index_exp}, $level) 
                    . ')';
    }
    sub emit_javascript2_set {
        my ($self, $arguments, $level, $wantarray) = @_;
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<@>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && $self->{obj}->sigil eq '@'
              )
           )
        {
            # @a[10, 20]
            # @$a[0, 2] ==> @{$a}[0,2]
            return Perlito5::Javascript2::emit_wrap_javascript2($level, $wantarray, 
                    'var a = [];',
                    'var v = ' . Perlito5::Javascript2::to_list([$self->{index_exp}], $level) . ';',
                    'var src=' . Perlito5::Javascript2::to_list([$arguments], $level) . ";",
                    'var out=' . Perlito5::Javascript2::emit_javascript2_autovivify( $self->{obj}, $level, 'array' ) . ";",
                    'var tmp' . ";",
                    'for (var i=0, l=v.length; i<l; ++i) {',
                          [ 'tmp = src.p5aget(i);',
                            'out.p5aset(v[i], tmp);',
                            'a.push(tmp)',
                          ],
                    '}',
                    'return a',
            )
        }
        return $self->emit_javascript2_container($level) . '.p5aset(' 
                    . Perlito5::Javascript2::to_num($self->{index_exp}, $level+1) . ', ' 
                    . Perlito5::Javascript2::to_scalar([$arguments], $level+1)
                . ')';
    }
    sub emit_javascript2_set_list {
        my ($self, $level, $list) = @_;
        my $wantarray = 'list';
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<@>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && $self->{obj}->sigil eq '@'
              )
           )
        {
            # @a[10, 20]
            # @$a[0, 2] ==> @{$a}[0,2]
            return Perlito5::Javascript2::emit_wrap_javascript2($level, $wantarray, 
                    'var a = [];',
                    'var v = ' . Perlito5::Javascript2::to_list([$self->{index_exp}], $level) . ';',
                    'var out=' . Perlito5::Javascript2::emit_javascript2_autovivify( $self->{obj}, $level, 'array' ) . ";",
                    'var tmp' . ";",
                    'for (var i=0, l=v.length; i<l; ++i) {',
                          [ 'tmp = ' . $list . '.shift();',
                            'out.p5aset(v[i], tmp);',
                            'a.push(tmp)',
                          ],
                    '}',
                    'return a',
            )
        }
        return $self->emit_javascript2_container($level) . '.p5aset(' 
                    . Perlito5::Javascript2::to_num($self->{index_exp}, $level+1) . ', ' 
                    . $list . '.shift()'
                . ')';
    }
    sub emit_javascript2_container {
        my $self = shift;
        my $level = shift;
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->code eq 'circumfix:<( )>'
           && @{ $self->{obj}->arguments } == 1
           && $self->{obj}->{arguments}[0]->isa('Perlito5::AST::Apply')
           )
        {
            # say Perlito5::Dumper::Dumper $self->{obj};
            return $self->{obj}->emit_javascript2($level, 'list');
        }
        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && $self->{obj}->sigil eq '$'
           )
        {
            my $v = Perlito5::AST::Var->new( sigil => '@', namespace => $self->{obj}->namespace, name => $self->{obj}->name );
            return $v->emit_javascript2($level);
        }
        elsif (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a[0] ==> $a->[0]
            return Perlito5::Javascript2::emit_javascript2_autovivify( $self->{obj}{arguments}[0], $level, 'array' ) . '._array_';
        }
        else {
            return Perlito5::Javascript2::emit_javascript2_autovivify( $self->{obj}, $level, 'array' ) . '._array_';
        }
    }
    sub emit_javascript2_get_decl { 
        return ()
    }
}

package Perlito5::AST::Lookup;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray, $autovivification_type) = @_;
        # autovivification_type: array, hash
        my $method = $autovivification_type || 'p5hget';
        $method = 'p5hget_array' if $autovivification_type eq 'array';
        $method = 'p5hget_hash'  if $autovivification_type eq 'hash';
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<@>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && $self->{obj}->sigil eq '@'
              )
           )
        {
            # @a{ 'x', 'y' }
            # @$a{ 'x', 'y' }  ==> @{$a}{ 'x', 'y' }
            my $v;
            $v = Perlito5::AST::Var->new( sigil => '%', namespace => $self->{obj}->namespace, name => $self->{obj}->name )
                if $self->{obj}->isa('Perlito5::AST::Var');
            $v = Perlito5::AST::Apply->new( code => 'prefix:<%>', namespace => $self->{obj}->namespace, arguments => $self->{obj}->arguments )
                if $self->{obj}->isa('Perlito5::AST::Apply');
            return Perlito5::Javascript2::emit_wrap_javascript2($level, $wantarray, 
                    'var a = [];',
                    'var v = ' . Perlito5::Javascript2::to_list([$self->{index_exp}], $level) . ';',
                    'var src=' . $v->emit_javascript2($level) . ';',
                    'for (var i=0, l=v.length; i<l; ++i)' . '{',
                          [ 'a.push(src.p5hget(v[i]))' ],
                    '}',
                    'return a',
            )
        }
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<%>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && $self->{obj}->sigil eq '%'
              )
           )
        {
            # Perl5.20 hash slice
            # %a{ 'x', 'y' }
            # %$a{ 'x', 'y' }  ==> %{$a}{ 'x', 'y' }
            my $v;
            $v = Perlito5::AST::Var->new( sigil => '%', namespace => $self->{obj}->namespace, name => $self->{obj}->name )
                if $self->{obj}->isa('Perlito5::AST::Var');
            $v = Perlito5::AST::Apply->new( code => 'prefix:<%>', namespace => $self->{obj}->namespace, arguments => $self->{obj}->arguments )
                if $self->{obj}->isa('Perlito5::AST::Apply');
            return Perlito5::Javascript2::emit_wrap_javascript2($level, $wantarray, 
                    'var a = [];',
                    'var v = ' . Perlito5::Javascript2::to_list([$self->{index_exp}], $level) . ';',
                    'var src=' . $v->emit_javascript2($level) . ';',
                    'for (var i=0, l=v.length; i<l; ++i)' . '{',
                          [ 'a.push(v[i]);',
                            'a.push(src.p5hget(v[i]))',
                          ],
                    '}',
                    'return a',
            )
        }
        return $self->emit_javascript2_container($level) . '.' . $method . '('
                . Perlito5::Javascript2::autoquote($self->{index_exp}, $level)
            . ')';
    }
    sub emit_javascript2_set {
        my ($self, $arguments, $level, $wantarray) = @_;
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<@>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && $self->{obj}->sigil eq '@'
              )
           )
        {
            # @a{ 'x', 'y' }
            # @$a{ 'x', 'y' }  ==> @{$a}{ 'x', 'y' }
            my $v;
            $v = Perlito5::AST::Var->new( sigil => '%', namespace => $self->{obj}->namespace, name => $self->{obj}->name )
                if $self->{obj}->isa('Perlito5::AST::Var');
            $v = Perlito5::AST::Apply->new( code => 'prefix:<%>', namespace => $self->{obj}->namespace, arguments => $self->{obj}->arguments )
                if $self->{obj}->isa('Perlito5::AST::Apply');
            return Perlito5::Javascript2::emit_wrap_javascript2($level, $wantarray, 
                    'var a = [];',
                    'var v = ' . Perlito5::Javascript2::to_list([$self->{index_exp}], $level) . ';',
                    'var src=' . Perlito5::Javascript2::to_list([$arguments], $level) . ";",
                    'var out=' . $v->emit_javascript2($level) . ";",
                    'var tmp' . ";",
                    'for (var i=0, l=v.length; i<l; ++i)' . '{',
                          [ 'tmp = src.p5hget(i);',
                            'out.p5hset(v[i], tmp);',
                            'a.push(tmp)',
                          ],
                    '}',
                    'return a',
            )
        }
        return $self->emit_javascript2_container($level) . '.p5hset('
                    . Perlito5::Javascript2::autoquote($self->{index_exp}, $level) . ', '
                    . Perlito5::Javascript2::to_scalar([$arguments], $level+1)
            . ')';
    }
    sub emit_javascript2_set_list {
        my ($self, $level, $list) = @_;
        my $wantarray = 'list';
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<@>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && $self->{obj}->sigil eq '@'
              )
           )
        {
            # @a{ 'x', 'y' }
            # @$a{ 'x', 'y' }  ==> @{$a}{ 'x', 'y' }
            my $v;
            $v = Perlito5::AST::Var->new( sigil => '%', namespace => $self->{obj}->namespace, name => $self->{obj}->name )
                if $self->{obj}->isa('Perlito5::AST::Var');
            $v = Perlito5::AST::Apply->new( code => 'prefix:<%>', namespace => $self->{obj}->namespace, arguments => $self->{obj}->arguments )
                if $self->{obj}->isa('Perlito5::AST::Apply');
            return Perlito5::Javascript2::emit_wrap_javascript2($level, $wantarray, 
                    'var a = [];',
                    'var v = ' . Perlito5::Javascript2::to_list([$self->{index_exp}], $level) . ';',
                    'var out=' . $v->emit_javascript2($level) . ";",
                    'var tmp' . ";",
                    'for (var i=0, l=v.length; i<l; ++i)' . '{',
                          [ 'tmp = ' . $list . '.shift();',
                            'out.p5hset(v[i], tmp);',
                            'a.push(tmp)',
                          ],
                    '}',
                    'return a',
            )
        }
        return $self->emit_javascript2_container($level) . '.p5hset('
                    . Perlito5::Javascript2::autoquote($self->{index_exp}, $level) . ', '
                    . $list . '.shift()'
            . ')';
    }
    sub emit_javascript2_container {
        my $self = shift;
        my $level = shift;
        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && $self->{obj}->sigil eq '$'
           )
        {
            my $v = Perlito5::AST::Var->new( sigil => '%', namespace => $self->{obj}->namespace, name => $self->{obj}->name );
            return $v->emit_javascript2($level)
        }
        elsif (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a{0} ==> $a->{0}
            return Perlito5::Javascript2::emit_javascript2_autovivify( $self->{obj}{arguments}[0], $level, 'hash' ) . '._hash_';
        }
        else {
            return Perlito5::Javascript2::emit_javascript2_autovivify( $self->{obj}, $level, 'hash' ) . '._hash_';
        }
    }
    sub emit_javascript2_get_decl { 
        return ()
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

    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
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
                if (  $Perlito5::STRICT 
                   && $self->{name} ne '0'  # $0 @0 %0
                   && !(0 + $self->{name})  # $1 @2 %3
                ) {
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
                return $self->emit_javascript2($level, 'list') . '.length';
            }
            if ( $wantarray eq 'runtime' ) {
                return '(p5want'
                    . ' ? ' . $self->emit_javascript2($level, 'list')
                    . ' : ' . $self->emit_javascript2($level, 'list') . '.length'
                    . ')';
            }
        }

        if ( $self->{sigil} eq '::' ) {
            return Perlito5::Javascript2::escape_string( $self->{namespace} );
        }
        if ( $self->{sigil} eq '&' ) {
            return 'p5pkg["' . ($self->{namespace} || $Perlito5::PKG_NAME) . '"]["' . $str_name . '"](List__, '
                        . Perlito5::Javascript2::to_context($wantarray)
                    . ')';
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
            $ns = 'p5make_package("' . $self->{namespace} . '")';
            if ($self->{sigil} eq '$#') {
                return '(p5global_array("' . $self->{namespace} . '", "' . $str_name . '").length - 1)';
            }
            if ($self->{sigil} eq '@') {
                return 'p5global_array("' . $self->{namespace} . '", "' . $str_name . '")';
            }
            if ($self->{sigil} eq '%') {
                return 'p5global_hash("' . $self->{namespace} . '", "' . $str_name . '")';
            }
            return $ns . '["' . $table->{$self->{sigil}} . $str_name . '"]'
        }

        if ($self->{sigil} eq '$#') {
            return '(' . $ns . $table->{'@'} . $str_name . '.length - 1)';
        }

        $ns . $table->{$self->{sigil}} . $str_name
    }

    sub emit_javascript2_set {
        my ($self, $arguments, $level, $wantarray) = @_;
        my $open  = $wantarray eq 'void' ? '' : '(';
        my $close = $wantarray eq 'void' ? '' : ')';
        if ( $self->sigil eq '$' ) {
            return $open . $self->emit_javascript2() . ' = ' . Perlito5::Javascript2::to_scalar([$arguments], $level+1) . $close
        }
        if ( $self->sigil eq '@' ) {
            return $open . $self->emit_javascript2() . ' = ' . Perlito5::Javascript2::to_list([$arguments], $level+1) . $close
        }
        if ( $self->sigil eq '%' ) {
            return $open . $self->emit_javascript2() . ' = ' . Perlito5::Javascript2::to_list([$arguments], $level+1, 'hash') . $close 
        }
        if ( $self->sigil eq '*' ) {
            return 'p5typeglob_set(' 
            .   Perlito5::Javascript2::escape_string($self->{namespace} || $Perlito5::PKG_NAME) . ', '
            .   Perlito5::Javascript2::escape_string($self->{name}) . ', ' 
            .   Perlito5::Javascript2::to_scalar([$arguments], $level+1)
            . ')'
        }
        die "don't know how to assign to variable ", $self->sigil, $self->name;
    }

    sub emit_javascript2_set_list {
        my ($self, $level, $list) = @_;
        if ( $self->sigil eq '$' ) {
            return $self->emit_javascript2() . ' = ' . $list  . '.shift()'
        }
        if ( $self->sigil eq '@' ) {
            return join( ";\n" . Perlito5::Javascript2::tab($level),
                $self->emit_javascript2() . ' = ' . $list,
                $list . ' = []'
            );
        }
        if ( $self->sigil eq '%' ) {
            return join( ";\n" . Perlito5::Javascript2::tab($level),
                $self->emit_javascript2() . ' = p5a_to_h(' . $list  . ')',
                $list . ' = []'
            );
        }
        die "don't know how to assign to variable ", $self->sigil, $self->name;
    }

    sub emit_javascript2_get_decl { () }
}

package Perlito5::AST::Decl;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        $self->{var}->emit_javascript2( $level );
    }
    sub emit_javascript2_init {
        my ($self, $level, $wantarray) = @_;

        if ($self->{decl} eq 'local') {
            my $var = $self->{var};
            if ( ref($var) eq 'Perlito5::AST::Var' ) {
                my $perl5_name = $var->perl5_name;
                my $decl = $var->perl5_get_decl( $perl5_name );
                if ( $decl && ($decl->{decl} eq 'my' || $decl->{decl} eq 'state') ) {
                    die "Can't localize lexical variable $perl5_name";
                }
            }
            my $var_set;
            my $tmp_name  = Perlito5::Javascript2::get_label();
            if ( ref($var) eq 'Perlito5::AST::Var' ) {
                $var_set = $var->emit_javascript2 . ' = v_' . $tmp_name;
            }
            else {
                my $tmp = Perlito5::AST::Var->new(sigil => '$', name => $tmp_name);
                push @{ $Perlito5::VAR }, { ('$' . $tmp_name) => { decl => 'my' } };
                $var_set = $var->emit_javascript2_set($tmp);
                pop @{ $Perlito5::VAR };
            }
            return Perlito5::Javascript2::emit_wrap_javascript2($level, $wantarray, 
                     'var v_' . $tmp_name . ' = ' . $var->emit_javascript2 . ';',
                     'p5LOCAL.push(function(){ ' . $var_set . ' });',
                     'return ' . $var->emit_javascript2_set(
                                    Perlito5::AST::Apply->new( code => 'undef', arguments => [], namespace => '' ),
                                    $level+1
                                 ) . ';',
                ) . ';';
        }

        my $type = $self->{decl};
        my $env = { decl => $type };
        my $perl5_name = $self->{var}->perl5_name;
        if ( $self->{decl} ne 'my' ) {

            die "No package name allowed for variable $perl5_name in \"our\""
                if $self->{decl} eq 'our' && $self->{var}{namespace};

            if ( $self->{var}{namespace} eq '' ) {
                # say "looking up $perl5_name";
                my $decl_namespace = '';
                my $decl = $self->{var}->perl5_get_decl( $perl5_name );
                if ( $decl && $decl->{decl} eq 'our') {
                    # say "found ", $decl->{decl}, " namespace: ", $decl->{namespace};
                    $decl_namespace = $decl->{namespace};
                }
                $env->{namespace} = $decl_namespace || $Perlito5::PKG_NAME;
            }
        }

        $Perlito5::VAR->[0]{ $perl5_name } = $env;

        if ($self->{decl} eq 'my') {
            my $str = 'var ' . $self->{var}->emit_javascript2();
            if ($self->{var}->sigil eq '%') {
                $str = $str . ' = {};';
            }
            elsif ($self->{var}->sigil eq '@') {
                $str = $str . ' = [];';
            }
            else {
                $str = $str . ';';
            }
            return $str;
        }
        elsif ($self->{decl} eq 'our') {
            my $str = $self->{var}->emit_javascript2();
            if ($self->{var}->sigil eq '%') {
                $str = $str . ' = {};';
            }
            elsif ($self->{var}->sigil eq '@') {
                $str = $str . ' = [];';
            }
            else {
                return '// our ' . $str;
            }
            # return '(function () { if (typeof ' . $self->{var}->emit_javascript2() . ' == "undefined" ) { '
            #         . $str
            #         . '; return ' . $self->{var}->emit_javascript2()
            #         . '}})()';
            return 'if (typeof ' . $self->{var}->emit_javascript2() . ' == "undefined" ) { '
                    . $str
                    . '}';
        }
        elsif ($self->{decl} eq 'state') {
            # TODO
            return '// state ' . $self->{var}->emit_javascript2();
        }
        else {
            die "not implemented: Perlito5::AST::Decl '" . $self->{decl} . "'";
        }
    }
    sub emit_javascript2_set {
        my ($self, $arguments, $level, $wantarray) = @_;
        $self->var->emit_javascript2_set($arguments, $level, $wantarray);
    }
    sub emit_javascript2_set_list {
        my ($self, $level, $list) = @_;
        $self->var->emit_javascript2_set_list($level, $list);
    }
    sub emit_javascript2_get_decl {
        my $self = shift;
        return ($self);
    }
}

package Perlito5::AST::Proto;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        return Perlito5::Javascript2::pkg()
            if $self->{name} eq '__PACKAGE__';
        return $Perlito5::AST::Sub::SUB_REF // '__SUB__'
            if $self->{name} eq '__SUB__';
        'p5pkg["' . $self->{name} . '"]'
    }
    sub emit_javascript2_get_decl { () }
}

package Perlito5::AST::Call;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray, $autovivification_type) = @_;
        # autovivification_type: array, hash
        my $meth = $self->{method};

        if ( $meth eq 'postcircumfix:<[ ]>' ) {
            my $method = $autovivification_type || 'p5aget';
            $method = 'p5aget_array' if $autovivification_type eq 'array';
            $method = 'p5aget_hash'  if $autovivification_type eq 'hash';
            return Perlito5::Javascript2::emit_javascript2_autovivify( $self->{invocant}, $level, 'array' )
                . '._array_.' . $method . '(' . Perlito5::Javascript2::to_num($self->{arguments}, $level+1)
                . ')';
        }
        if ( $meth eq 'postcircumfix:<{ }>' ) {
            my $method = $autovivification_type || 'p5hget';
            $method = 'p5hget_array' if $autovivification_type eq 'array';
            $method = 'p5hget_hash'  if $autovivification_type eq 'hash';
            return Perlito5::Javascript2::emit_javascript2_autovivify( $self->{invocant}, $level, 'hash' )
                . '._hash_.' . $method . '(' . Perlito5::Javascript2::autoquote($self->{arguments}, $level+1, 'list')
                . ')';
        }
        if  ($meth eq 'postcircumfix:<( )>')  {

            my $invocant;
            if (  ref( $self->{invocant} ) eq 'Perlito5::AST::Apply' 
               && $self->{invocant}{code} eq 'prefix:<&>'
               )
            {
                my $arg   = $self->{invocant}{arguments}->[0];
                $invocant = 'p5code_lookup_by_name("' . $Perlito5::PKG_NAME . '", ' . $arg->emit_javascript2($level) . ')';
            }
            elsif (  ref( $self->{invocant} ) eq 'Perlito5::AST::Var' 
               && $self->{invocant}{sigil} eq '&'
               )
            {
                $invocant = 'p5pkg["' . ($self->{invocant}{namespace} || $Perlito5::PKG_NAME) . '"]["' . $self->{invocant}{name} . '"]';
            }
            else {
                $invocant = $self->{invocant}->emit_javascript2($level, 'scalar');
            }

            return '(' . $invocant . ')(' . Perlito5::Javascript2::to_list($self->{arguments}) . ', '
                        . Perlito5::Javascript2::to_context($wantarray)
                    . ')';
        }

        my $invocant = $self->{invocant}->emit_javascript2($level, 'scalar');
        if ( ref($meth) eq 'Perlito5::AST::Var' ) {
            $meth = $meth->emit_javascript2($level, 'scalar');
        }
        else {
            $meth = Perlito5::Javascript2::escape_string($meth);
        }
        return 'p5call(' . $invocant . ', ' 
                         . $meth . ', ' 
                         . Perlito5::Javascript2::to_list($self->{arguments}) . ', '
                         . Perlito5::Javascript2::to_context($wantarray)
                  . ')'
    }

    sub emit_javascript2_set {
        my ($self, $arguments, $level, $wantarray) = @_;
        if ( $self->{method} eq 'postcircumfix:<[ ]>' ) {
            return Perlito5::Javascript2::emit_javascript2_autovivify( $self->{invocant}, $level, 'array' )
                    . '._array_.p5aset(' 
                        . Perlito5::Javascript2::to_num($self->{arguments}, $level+1) . ', ' 
                        . Perlito5::Javascript2::to_scalar([$arguments], $level+1)
                    . ')';
        }
        if ( $self->{method} eq 'postcircumfix:<{ }>' ) {
            return Perlito5::Javascript2::emit_javascript2_autovivify( $self->{invocant}, $level, 'hash' )
                    . '._hash_.p5hset(' 
                        . Perlito5::Javascript2::autoquote($self->{arguments}, $level+1, 'list') . ', '
                        . Perlito5::Javascript2::to_scalar([$arguments], $level+1)
                    . ')';
        }
        die "don't know how to assign to method ", $self->{method};
    }
    sub emit_javascript2_set_list {
        my ($self, $level, $list) = @_;
        if ( $self->{method} eq 'postcircumfix:<[ ]>' ) {
            return Perlito5::Javascript2::emit_javascript2_autovivify( $self->{invocant}, $level, 'array' )
                    . '._array_.p5aset(' 
                        . Perlito5::Javascript2::to_num($self->{arguments}, $level+1) . ', ' 
                        . $list  . '.shift()'
                    . ')';
        }
        if ( $self->{method} eq 'postcircumfix:<{ }>' ) {
            return Perlito5::Javascript2::emit_javascript2_autovivify( $self->{invocant}, $level, 'hash' )
                    . '._hash_.p5hset(' 
                        . Perlito5::Javascript2::autoquote($self->{arguments}, $level+1, 'list') . ', '
                        . $list  . '.shift()'
                    . ')';
        }
        die "don't know how to assign to method ", $self->{method};
    }
    sub emit_javascript2_get_decl { 
        return ()
    }
}

package Perlito5::AST::Apply;
{
    sub emit_regex_javascript2 {
        my $op = shift;
        my $var = shift;
        my $regex = shift;
        my $level     = shift;
        my $wantarray = shift;

        if ($regex->isa('Perlito5::AST::Var')) {
            # $x =~ $regex
            $regex = { code => 'p5:m', arguments => [ $regex, '' ] };
        }

        my $str;
        my $code = $regex->{code};
        my $regex_args = $regex->{arguments};
        if ($code eq 'p5:s') {
            $str = $var->emit_javascript2() 
                 . ' = p5str(' . $var->emit_javascript2() . ').replace(/' . $regex_args->[0]->{buf} . '/' . $regex_args->[2]->{buf} . ', '
                 .  $regex_args->[1]->emit_javascript2() . ')';
        }
        elsif ($code eq 'p5:m') {

            my $ast = $regex_args->[0];
            if ($ast->isa('Perlito5::AST::Val::Buf')) {
                # constant
                $str = '(' 
                    . 'p5str(' . $var->emit_javascript2() . ')'
                    . '.match('
                    .   '(new RegExp('
                          . $ast->emit_javascript2() . ', '
                          . Perlito5::Javascript2::escape_string($regex_args->[1]->{buf})
                    .   '))'
                    . ')'
                    . ' ? 1 : 0)';
            }
            else {
                # run-time interpolation
                $str = '(new RegExp('
                        . $ast->emit_javascript2() . ', '
                        . Perlito5::Javascript2::escape_string($regex_args->[1]->{buf})
                    . '))'
                    . '.exec('
                        . 'p5str(' . $var->emit_javascript2() . ')'
                    . ')';
            }
        }
        elsif ($code eq 'p5:tr') {
            $str = Perlito5::Javascript2::emit_wrap_javascript2($level+1, $wantarray, 
                "var tmp = p5tr("
                    . $var->emit_javascript2() . ', '
                    . $regex_args->[0]->emit_javascript2() . ', '
                    . $regex_args->[1]->emit_javascript2() . ', '
                    . Perlito5::Javascript2::escape_string($regex_args->[2]->{buf}) . ', '
                    . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 )
                  . ");",
                $var->emit_javascript2() . " = tmp[0];",
                "return tmp[1];",
            );
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

    sub emit_javascript2_set {
        my ($self, $arguments, $level, $wantarray) = @_;
        my $code = $self->{code};
        if ($code eq 'prefix:<$>') {
            return 'p5scalar_deref_set(' 
                . Perlito5::Javascript2::emit_javascript2_autovivify( $self->{arguments}->[0], $level+1, 'scalar' ) . ', '
                . Perlito5::Javascript2::to_scalar([$arguments], $level+1)  . ', '
                . Perlito5::Javascript2::escape_string($Perlito5::PKG_NAME)
                . ')';
        }
        if ($code eq 'prefix:<*>') {
            return 'p5typeglob_deref_set(' 
                . Perlito5::Javascript2::to_scalar($self->{arguments}, $level+1) . ', '
                . Perlito5::Javascript2::to_scalar([$arguments], $level+1)       . ', '
                . Perlito5::Javascript2::escape_string($Perlito5::PKG_NAME)
                . ')';
        }
        my $open  = $wantarray eq 'void' ? '' : '(';
        my $close = $wantarray eq 'void' ? '' : ')';
        $open . $self->emit_javascript2( $level+1 ) . ' = ' . $arguments->emit_javascript2( $level+1 ) . $close;
    }

    my %emit_js = (
        'infix:<=~>' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_javascript2( '=~', $self->{arguments}->[0], $self->{arguments}->[1], $level, $wantarray );
        },
        'infix:<!~>' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_javascript2( '!~', $self->{arguments}->[0], $self->{arguments}->[1], $level, $wantarray );
        },
        'p5:s' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_javascript2( '=~', Perlito5::AST::Var->new( sigil => '$', namespace => '', name => '_' ), $self, $level, $wantarray );
        },
        'p5:m' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_javascript2( '=~', Perlito5::AST::Var->new( sigil => '$', namespace => '', name => '_' ), $self, $level, $wantarray );
        },
        'p5:tr' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_javascript2( '=~', Perlito5::AST::Var->new( sigil => '$', namespace => '', name => '_' ), $self, $level, $wantarray );
        },
        'p5:qr' => sub {
            my ($self, $level, $wantarray) = @_;
            # p5qr( $str, $modifier );
            'p5qr(' . Perlito5::Javascript2::to_str( $self->{arguments}[0] ) . ', '
                    . Perlito5::Javascript2::to_str( $self->{arguments}[1] ) . ')';
        },
        '__PACKAGE__' => sub {
            my $self = $_[0];
            Perlito5::Javascript2::escape_string($Perlito5::PKG_NAME);
        },
        '__SUB__' => sub {
            my $self = $_[0];
            $Perlito5::AST::Sub::SUB_REF // '__SUB__'
        },
        'wantarray' => sub {
            my $self = $_[0];
            'p5want';
        },
        'package' => sub {
            my $self = $_[0];
            'p5make_package("' . $self->{namespace} . '")';
        },
        'infix:<&&>' => sub {
            my ($self, $level, $wantarray) = @_;
            'p5and('
                . $self->{arguments}->[0]->emit_javascript2($level, 'scalar') . ', '
                . Perlito5::Javascript2::emit_function_javascript2($level, $wantarray, $self->{arguments}->[1]) 
                . ')'
        },
        'infix:<and>' => sub {
            my ($self, $level, $wantarray) = @_;
            'p5and('
                . $self->{arguments}->[0]->emit_javascript2($level, 'scalar') . ', '
                . Perlito5::Javascript2::emit_function_javascript2($level, $wantarray, $self->{arguments}->[1]) 
                . ')'
        },
        'infix:<||>' => sub {
            my ($self, $level, $wantarray) = @_;
            'p5or('
                . $self->{arguments}->[0]->emit_javascript2($level, 'scalar') . ', '
                . Perlito5::Javascript2::emit_function_javascript2($level, $wantarray, $self->{arguments}->[1]) 
                . ')'
        },
        'infix:<or>' => sub {
            my ($self, $level, $wantarray) = @_;
            'p5or('
                . $self->{arguments}->[0]->emit_javascript2($level, 'scalar') . ', '
                . Perlito5::Javascript2::emit_function_javascript2($level, $wantarray, $self->{arguments}->[1]) 
                . ')'
        },
        'infix:<xor>' => sub {
            my ($self, $level, $wantarray) = @_;
            'p5xor('
                . $self->{arguments}->[0]->emit_javascript2($level, 'scalar') . ', '
                . Perlito5::Javascript2::emit_function_javascript2($level, $wantarray, $self->{arguments}->[1]) 
                . ')'
        },
        'infix:<=>>' => sub {
            my ($self, $level, $wantarray) = @_;
              Perlito5::AST::Lookup->autoquote($self->{arguments}[0])->emit_javascript2($level)  . ', ' 
            . $self->{arguments}[1]->emit_javascript2($level)
        },
        'infix:<cmp>' => sub {
            my $self = $_[0];
            'p5cmp(' . join( ', ', map( Perlito5::Javascript2::to_str($_), @{ $self->{arguments} } ) ) . ')';
        },
        'infix:<<=>>' => sub {
            my $self = $_[0];
            'p5cmp(' . join( ', ', map( Perlito5::Javascript2::to_num($_), @{ $self->{arguments} } ) ) . ')';
        },
        'infix:<**>' => sub {
            my $self = $_[0];
            'Math.pow(' . join( ', ', map( Perlito5::Javascript2::to_num($_), @{ $self->{arguments} } ) ) . ')';
        },
        'infix:<<<>' => sub {
            my $self = $_[0];
            'p5shift_left(' . join( ', ', map( Perlito5::Javascript2::to_num($_), @{ $self->{arguments} } ) ) . ')';
        },
        'infix:<%>' => sub {
            my $self = $_[0];
            'p5modulo(' . join( ', ', map( Perlito5::Javascript2::to_num($_), @{ $self->{arguments} } ) ) . ')';
        },
        'prefix:<!>' => sub {
            my $self      = shift;
            my $level     = shift;
            '!( ' . Perlito5::Javascript2::to_bool( $self->{arguments}->[0], $level ) . ')';
        },
        'prefix:<not>' => sub {
            my $self      = shift;
            my $level     = shift;
            '!( ' . Perlito5::Javascript2::to_bool( $self->{arguments}->[0], $level ) . ')';
        },
        'prefix:<~>' => sub {
            my $self = $_[0];
            'p5complement( ' . Perlito5::Javascript2::to_num( $self->{arguments}->[0] ) . ')';
        },
        'prefix:<->' => sub {
            my ($self, $level, $wantarray) = @_;
            'p5negative( ' . $self->{arguments}->[0]->emit_javascript2( $level, 'scalar' ) . ')';
        },
        'prefix:<+>' => sub {
            my ($self, $level, $wantarray) = @_;
            '(' . $self->{arguments}->[0]->emit_javascript2( $level, $wantarray ) . ')';
        },
        'require' => sub {
            my ($self, $level, $wantarray) = @_;
            'p5pkg["Perlito5::Grammar::Use"]["require"]([' 
                . Perlito5::Javascript2::to_str( $self->{arguments}[0] ) . ', ' 
                . ($self->{arguments}[0]{bareword} ? 1 : 0) 
            . '])';
        },
        'prefix:<$>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg  = $self->{arguments}->[0];
            return 'p5scalar_deref(' 
                    . $arg->emit_javascript2( $level ) . ', '
                    . Perlito5::Javascript2::escape_string($Perlito5::PKG_NAME) . ', '
                    . '""'      # autovivification type
                    . ')';
        },
        'prefix:<@>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            my $s = 'p5array_deref(' 
                  . Perlito5::Javascript2::emit_javascript2_autovivify( $arg, $level, 'array' ) . ', '
                  . Perlito5::Javascript2::escape_string($Perlito5::PKG_NAME)
                  . ')';
            return $wantarray eq 'scalar'
                ? "p5num($s)"
                : $s;
        },
        'prefix:<$#>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            return '(p5array_deref(' 
                    . Perlito5::Javascript2::emit_javascript2_autovivify( $arg, $level, 'array' ) . ', '
                    . Perlito5::Javascript2::escape_string($Perlito5::PKG_NAME)
                    . ').length - 1)';
        },
        'prefix:<%>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            return 'p5hash_deref(' 
                    . Perlito5::Javascript2::emit_javascript2_autovivify( $arg, $level, 'hash' ) . ', '
                    . Perlito5::Javascript2::escape_string($Perlito5::PKG_NAME)
                    . ')';
        },
        'prefix:<&>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            'p5code_lookup_by_name("' . $Perlito5::PKG_NAME . '", ' . $arg->emit_javascript2($level) . ')([])';
        },
        'circumfix:<[ ]>' => sub {
            my ($self, $level, $wantarray) = @_;
            '(new p5ArrayRef(' . Perlito5::Javascript2::to_list( $self->{arguments} ) . '))';
        },
        'circumfix:<{ }>' => sub {
            my ($self, $level, $wantarray) = @_;
            '(new p5HashRef(' . Perlito5::Javascript2::to_list( $self->{arguments}, $level, 'hash' ) . '))';
        },
        'prefix:<\\>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if ( $arg->isa('Perlito5::AST::Apply') ) {
                # if ( $arg->{code} eq '@' ) {
                #     # TODO
                #     return '(new p5ArrayRef(' . $arg->emit_javascript2($level) . '))';
                # }
                # if ( $arg->{code} eq '%' ) {
                #     # TODO
                #     return '(new p5HashRef(' . $arg->emit_javascript2($level) . '))';
                # }
                # if ( $arg->{code} eq '*' ) {
                #     # TODO
                #     return '(new p5GlobRef(' . $arg->emit_javascript2($level) . '))';
                # }
                if ( $arg->{code} eq 'circumfix:<( )>' ) {
                    # \( @x )
                    return 'p5_list_of_refs(' . Perlito5::Javascript2::to_list( $arg->{arguments} ) . ')';
                }
                if ( $arg->{code} eq 'prefix:<&>' ) {
                    return 'p5code_lookup_by_name("' . $Perlito5::PKG_NAME . '", ' . $arg->{arguments}->[0]->emit_javascript2($level) . ')';
                }
            }
            if ( $arg->isa('Perlito5::AST::Var') ) {
                if ( $arg->sigil eq '@' ) {
                    return '(new p5ArrayRef(' . $arg->emit_javascript2($level) . '))';
                }
                if ( $arg->sigil eq '%' ) {
                    return '(new p5HashRef(' . $arg->emit_javascript2($level) . '))';
                }
                if ( $arg->sigil eq '*' ) {
                    return '(new p5GlobRef(' . $arg->emit_javascript2($level) . '))';
                }
                if ( $arg->sigil eq '&' ) {
                    if ( $arg->{namespace} ) {
                        return 'p5pkg["' . $arg->{namespace} . '"].' . $arg->{name};
                    }
                    else {
                        return Perlito5::Javascript2::pkg() . '.' . $arg->{name};
                    }
                }
            }
            return '(new p5ScalarRef(' . $arg->emit_javascript2($level) . '))';
        },

        'postfix:<++>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if  (   $arg->isa( 'Perlito5::AST::Index')
                ||  $arg->isa( 'Perlito5::AST::Lookup') 
                ||  $arg->isa( 'Perlito5::AST::Call') 
                )
            {
                return $arg->emit_javascript2($level+1, 0, 'p5postincr');
            }
            if  (   $arg->isa( 'Perlito5::AST::Var')
                &&  $arg->{sigil} eq '$'
                )
            {
                my $tmp  = Perlito5::Javascript2::get_label();
                return Perlito5::Javascript2::emit_wrap_javascript2($level, 'scalar', 
                            'var ' . $tmp . ' = ' . $arg->emit_javascript2($level) . ';',
                            $arg->emit_javascript2($level) . ' = p5incr_(' . $tmp . ');',
                            'return ' . $tmp,
                )
            }
            '(' . join( ' ', map( $_->emit_javascript2, @{ $self->{arguments} } ) ) . ')++';
        },
        'postfix:<-->' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];

            if  (   $arg->isa( 'Perlito5::AST::Index')
                ||  $arg->isa( 'Perlito5::AST::Lookup') 
                ||  $arg->isa( 'Perlito5::AST::Call') 
                )
            {
                return $arg->emit_javascript2($level+1, 0, 'p5postdecr');
            }
            if  (   $arg->isa( 'Perlito5::AST::Var')
                &&  $arg->{sigil} eq '$'
                )
            {
                my $tmp  = Perlito5::Javascript2::get_label();
                return Perlito5::Javascript2::emit_wrap_javascript2($level, 'scalar', 
                            'var ' . $tmp . ' = ' . $arg->emit_javascript2($level) . ';',
                            $arg->emit_javascript2($level) . ' = p5decr_(' . $tmp . ');',
                            'return ' . $tmp,
                )
            }

            '(' . join( ' ', map( $_->emit_javascript2, @{ $self->{arguments} } ) ) . ')--';
        },
        'prefix:<++>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if  (   $arg->isa( 'Perlito5::AST::Index')
                ||  $arg->isa( 'Perlito5::AST::Lookup') 
                ||  $arg->isa( 'Perlito5::AST::Call') 
                )
            {
                return $arg->emit_javascript2($level+1, 0, 'p5incr');
            }
            if  (   $arg->isa( 'Perlito5::AST::Var')
                &&  $arg->{sigil} eq '$'
                )
            {
                my $tmp  = Perlito5::Javascript2::get_label();
                return Perlito5::Javascript2::emit_wrap_javascript2($level, 'scalar', 
                            'var ' . $tmp . ' = ' . $arg->emit_javascript2($level) . ';',
                            $arg->emit_javascript2($level) . ' = p5incr_(' . $tmp . ');',
                            'return ' . $arg->emit_javascript2($level+1),
                )
            }
            '++(' . join( ' ', map( $_->emit_javascript2, @{ $self->{arguments} } ) ) . ')';
        },
        'prefix:<-->' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];

            if  (   $arg->isa( 'Perlito5::AST::Index')
                ||  $arg->isa( 'Perlito5::AST::Lookup') 
                ||  $arg->isa( 'Perlito5::AST::Call') 
                )
            {
                return $arg->emit_javascript2($level+1, 0, 'p5decr');
            }
            if  (   $arg->isa( 'Perlito5::AST::Var')
                &&  $arg->{sigil} eq '$'
                )
            {
                my $tmp  = Perlito5::Javascript2::get_label();
                return Perlito5::Javascript2::emit_wrap_javascript2($level, 'scalar', 
                            'var ' . $tmp . ' = ' . $arg->emit_javascript2($level) . ';',
                            $arg->emit_javascript2($level) . ' = p5decr_(' . $tmp . ');',
                            'return ' . $arg->emit_javascript2($level+1),
                )
            }

            '--(' . join( ' ', map( $_->emit_javascript2, @{ $self->{arguments} } ) ) . ')';
        },

        'infix:<x>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if ( ref($arg) eq 'Perlito5::AST::Apply' && $arg->{code} eq 'circumfix:<( )>') {
                # ($v) x $i
                return 'p5list_replicate(' . join( ', ', map( $_->emit_javascript2, @{ $self->{arguments} } ) ) . ')';
            }
            'p5str_replicate(' . join( ', ', map( $_->emit_javascript2, @{ $self->{arguments} } ) ) . ')';
        },

        'list:<.>' => sub {
            my ($self, $level, $wantarray) = @_;
            '(' . join( ' + ', map( Perlito5::Javascript2::to_str($_), @{ $self->{arguments} } ) ) . ')';
        },
        'list:<,>' => sub {
            my ($self, $level, $wantarray) = @_;
            Perlito5::Javascript2::to_list( $self->{arguments} );
        },
        'infix:<..>' => sub {
            my ($self, $level, $wantarray) = @_;
            return 'p5range(' . $self->{arguments}->[0]->emit_javascript2($level) . ', '
                              . $self->{arguments}->[1]->emit_javascript2($level) . ')';
        },

        'delete' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg = $self->{arguments}->[0];
            if ($arg->isa( 'Perlito5::AST::Lookup' )) {
                my $v = $arg->obj;
                if (  $v->isa('Perlito5::AST::Var')
                   && $v->sigil eq '$'
                   )
                {
                    $v = Perlito5::AST::Var->new( sigil => '%', namespace => $v->namespace, name => $v->name );
                    return '(delete ' . $v->emit_javascript2() . '[' . $arg->autoquote($arg->{index_exp})->emit_javascript2($level) . '])';
                }
                return '(delete ' . $v->emit_javascript2() . '._hash_[' . $arg->autoquote($arg->{index_exp})->emit_javascript2($level) . '])';
            }
            if ($arg->isa( 'Perlito5::AST::Index' )) {
                my $v = $arg->obj;
                if (  $v->isa('Perlito5::AST::Var')
                   && $v->sigil eq '$'
                   )
                {
                    $v = Perlito5::AST::Var->new( sigil => '@', namespace => $v->namespace, name => $v->name );
                    return '(delete ' . $v->emit_javascript2() . '[' . $arg->{index_exp}->emit_javascript2($level) . '])';
                }
                return '(delete ' . $v->emit_javascript2() . '._array_[' . $arg->{index_exp}->emit_javascript2($level) . '])';
            }
            if ($arg->isa( 'Perlito5::AST::Call' )) {
                if ( $arg->method eq 'postcircumfix:<{ }>' ) {
                    return '(delete ' . $arg->invocant->emit_javascript2() . '._hash_[' . Perlito5::AST::Lookup->autoquote($arg->{arguments})->emit_javascript2($level) . '])';
                }
                if ( $arg->method eq 'postcircumfix:<[ ]>' ) {
                    return '(delete ' . $arg->invocant->emit_javascript2() . '._array_[' . $arg->{arguments}->emit_javascript2($level) . '])';
                }
            }
            if (  $arg->isa('Perlito5::AST::Var')
               && $arg->sigil eq '&'
               )
            {
                die 'TODO delete &code';
                # my $name = $arg->{name};
                # my $namespace = $arg->{namespace} || $Perlito5::PKG_NAME;
                # return 'p5pkg[' . Perlito5::Javascript2::escape_string($namespace) . '].hasOwnProperty(' . Perlito5::Javascript2::escape_string($name) . ')';
            }
            if (  $arg->isa('Perlito5::AST::Apply')
               && $arg->{code} eq 'prefix:<&>'
               )
            {
                die 'TODO delete &$code';
                # my $arg2 = $arg->{arguments}->[0];
                # return 'p5sub_exists(' . Perlito5::Javascript2::to_str($arg2) . ', ' . Perlito5::Javascript2::escape_string($Perlito5::PKG_NAME) . ')';
            }
        },

        'scalar' => sub {
            my ($self, $level, $wantarray) = @_;
            Perlito5::Javascript2::to_scalar($self->{arguments}, $level+1);
        },

        'ternary:<? :>' => sub {
            my ($self, $level, $wantarray) = @_;
            '( ' . Perlito5::Javascript2::to_bool( $self->{arguments}->[0] ) . ' ? ' . ( $self->{arguments}->[1] )->emit_javascript2( $level, $wantarray ) . ' : ' . ( $self->{arguments}->[2] )->emit_javascript2( $level, $wantarray ) . ')';
        },
        'my' => sub {
            my ($self, $level, $wantarray) = @_;
            # this is a side-effect of my($x,$y)
            'p5context(' . '[' . join( ', ', map( $_->emit_javascript2( $level, $wantarray ), @{ $self->{arguments} } ) ) . '], ' . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 ) . ')';
        },
        'our' => sub {
            my ($self, $level, $wantarray) = @_;
            # this is a side-effect of my($x,$y)
            'p5context(' . '[' . join( ', ', map( $_->emit_javascript2( $level, $wantarray ), @{ $self->{arguments} } ) ) . '], ' . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 ) . ')';
        },
        'local' => sub {
            my ($self, $level, $wantarray) = @_;
            # 'local ($x, $y[10])'
            'p5context(' . '[' . join( ', ', map( $_->emit_javascript2( $level, $wantarray ), @{ $self->{arguments} } ) ) . '], ' . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 ) . ')';
        },
        'circumfix:<( )>' => sub {
            my ($self, $level, $wantarray) = @_;
            'p5context(' . '[' . join( ', ', map( $_->emit_javascript2( $level, $wantarray ), @{ $self->{arguments} } ) ) . '], ' . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 ) . ')';
        },
        'infix:<=>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $parameters = $self->{arguments}->[0];
            my $arguments  = $self->{arguments}->[1];

            if (   $parameters->isa( 'Perlito5::AST::Apply' )
               &&  ( $parameters->code eq 'my' || $parameters->code eq 'local' || $parameters->code eq 'circumfix:<( )>' )
               )
            {
                # my ($x, $y) = ...
                # local ($x, $y) = ...
                # ($x, $y) = ...

                if ( $wantarray eq 'void' ) {
                    my $tmp  = Perlito5::Javascript2::get_label();
                    return join( ";\n" . Perlito5::Javascript2::tab($level),
                            'var ' . $tmp  . ' = ' . Perlito5::Javascript2::to_list([$arguments], $level+1),
                            ( map $_->emit_javascript2_set_list($level, $tmp),
                                  @{ $parameters->arguments }
                            ),
                    );
                }

                my $tmp  = Perlito5::Javascript2::get_label();
                my $tmp2 = Perlito5::Javascript2::get_label();
                return Perlito5::Javascript2::emit_wrap_javascript2($level, $wantarray, 
                            'var ' . $tmp  . ' = ' . Perlito5::Javascript2::to_list([$arguments], $level+1) . ";",
                            'var ' . $tmp2 . ' = ' . $tmp . ".slice(0);",
                            ( map $_->emit_javascript2_set_list($level+1, $tmp) . ";",
                                  @{ $parameters->arguments }
                            ),
                            'return ' . $tmp2,
                );
            }
            return $parameters->emit_javascript2_set($arguments, $level+1, $wantarray);
        },

        'break' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            Perlito5::Javascript2::emit_wrap_statement_javascript2(
                $level,
                $wantarray, 
                'throw(new p5_error("break", ""))'
            );
        },
        'next' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            my $label =  $self->{arguments}[0]{code} || "";
            Perlito5::Javascript2::emit_wrap_statement_javascript2(
                $level,
                $wantarray, 
                'throw(new p5_error("next", "' . $label . '"))'
            );
        },
        'last' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            my $label =  $self->{arguments}[0]{code} || "";
            Perlito5::Javascript2::emit_wrap_statement_javascript2(
                $level,
                $wantarray, 
                'throw(new p5_error("last", "' . $label . '"))'
            );
        },
        'redo' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            my $label =  $self->{arguments}[0]{code} || "";
            Perlito5::Javascript2::emit_wrap_statement_javascript2(
                $level,
                $wantarray, 
                'throw(new p5_error("redo", "' . $label . '"))'
            );
        },
        'return' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            Perlito5::Javascript2::emit_wrap_statement_javascript2(
                $level,
                $wantarray, 
                'throw(' . Perlito5::Javascript2::to_runtime_context( $self->{arguments}, $level+1 ) . ')'
            );
        },
        'goto' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            Perlito5::Javascript2::emit_wrap_statement_javascript2(
                $level,
                $wantarray, 
                'throw(' . $self->{arguments}->[0]->emit_javascript2($level) . ')'
            );
        },

        'do' => sub {
            my ($self, $level, $wantarray) = @_;
            # Note: this is "do EXPR" - look at the "Do" AST node for "do BLOCK"
            my $tmp_strict = $Perlito5::STRICT;
            $Perlito5::STRICT = 0;
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
            my $js = $ast->emit_javascript2( $level );
            $Perlito5::STRICT = $tmp_strict;
            return $js;
        },

        'eval' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;   # we can return() from inside eval

            my $arg = $self->{arguments}->[0];
            my $eval;
            if ($arg->isa( "Perlito5::AST::Do" )) {
                # eval block

                $eval = $arg->emit_javascript2( $level + 1, $wantarray );
            }
            else {
                # eval string

                my $var_env_perl5 = Perlito5::Dumper::ast_dumper( $Perlito5::VAR );
                # say "at eval: ", $var_env_perl5;
                my $m = Perlito5::Grammar::Expression->term_square( $var_env_perl5, 0 );
                $m = Perlito5::Grammar::Expression::expand_list( Perlito5::Match::flat($m)->[2] );
                # say Perlito5::Dumper::ast_dumper( $m );
                my $var_env_js = '(new p5ArrayRef(' . Perlito5::Javascript2::to_list($m) . '))';
                $eval ='eval(p5pkg["Perlito5::Javascript2::Runtime"].perl5_to_js([' 
                            . Perlito5::Javascript2::to_str($arg) . ", "
                            . Perlito5::Javascript2::escape_string($Perlito5::PKG_NAME) . ', '
                            . $var_env_js . ', '
                            . Perlito5::Javascript2::escape_string($wantarray)
                        . "]))";
            }

            # TODO - test return() from inside eval

            my $context = Perlito5::Javascript2::to_context($wantarray);

            Perlito5::Javascript2::emit_wrap_javascript2($level, $wantarray,
                ( $context eq 'p5want'
                  ? ()
                  : "var p5want = " . $context . ";",
                ),
                "var r;",
                'p5pkg["main"]["v_@"] = "";',
                'p5pkg["Perlito5"]["v_STRICT"] = ' . $Perlito5::STRICT . ';',
                "try {",
                    [ 'r = ' . $eval . "",
                    ],
                "}",
                "catch(err) {",
                [  "if ( err instanceof p5_error || err instanceof Error ) {",
                     [ 'p5pkg["main"]["v_@"] = err;',
                        # try to add a stack trace
                       'try {' . "",
                           [ 'p5pkg["main"]["v_@"] = p5pkg["main"]["v_@"] + "\n" + err.stack;',
                           ],
                       '}',
                       'catch(err) { }',
                     ],
                   "}",
                   "else {",
                     [ "return(err);",
                     ],
                   "}",
                 ],
                "}",
                "return r;",
            );
        },

        'substr' => sub {
            my ($self, $level, $wantarray) = @_;
            my $length = $self->{arguments}->[2];
            if ( $length && $length->isa('Perlito5::AST::Val::Int') && $length->{int} > 0 ) {
                return Perlito5::Javascript2::to_str($self->{arguments}->[0]) 
                    . '.substr(' . Perlito5::Javascript2::to_num($self->{arguments}->[1]) . ', ' 
                                 . Perlito5::Javascript2::to_num($self->{arguments}->[2]) . ')'
            }
            my $arg_list = Perlito5::Javascript2::to_list_preprocess( $self->{arguments} );
            my $arg_code = Perlito5::Javascript2::to_list($arg_list);
            return 'CORE.substr(' 
                    . $arg_code . ', '
                    . Perlito5::Javascript2::to_context($wantarray)
                 . ')';
        },
        'undef' => sub {
            my ($self, $level, $wantarray) = @_;
            if ( $self->{arguments} && @{$self->{arguments}} ) {
                return '(' . $self->{arguments}->[0]->emit_javascript2 . ' = null)'
            }
            return 'null'
        },
        'defined' => sub { 
            my ($self, $level, $wantarray) = @_;
            my $arg = $self->{arguments}[0];
            my $invocant;
            if (  ref( $arg ) eq 'Perlito5::AST::Apply' 
               && $arg->{code} eq 'prefix:<&>'
               )
            {
                my $arg2   = $arg->{arguments}->[0];
                $invocant = 'p5code_lookup_by_name("' . $Perlito5::PKG_NAME . '", ' . $arg2->emit_javascript2($level) . ')';
            }
            elsif (  ref( $arg ) eq 'Perlito5::AST::Var' 
               && $arg->{sigil} eq '&'
               )
            {
                $invocant = 'p5pkg["' . ($arg->{namespace} || $Perlito5::PKG_NAME) . '"]["' . $arg->{name} . '"]';
            }
            else {
                $invocant = $arg->emit_javascript2($level, 'scalar');
            }
            '(' . $invocant . ' != null)' 
        },
        'shift' => sub {
            my ($self, $level, $wantarray) = @_;
            if ( $self->{arguments} && @{$self->{arguments}} ) {
                return $self->{arguments}[0]->emit_javascript2( $level ) . '.shift()'
            }
            return 'List__.shift()'
        },
        'pop' => sub {
            my ($self, $level, $wantarray) = @_;
            if ( $self->{arguments} && @{$self->{arguments}} ) {
                return $self->{arguments}[0]->emit_javascript2( $level ) . '.pop()'
            }
            return 'List__.pop()'
        },
        'unshift' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            my $v = shift @arguments;     # TODO - this argument can also be a 'Decl' instead of 'Var'

            return $v->emit_javascript2( $level ) . '.p5unshift(' . Perlito5::Javascript2::to_list(\@arguments) . ')';
        },
        'push' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            my $v = shift @arguments;     # TODO - this argument can also be a 'Decl' instead of 'Var'

            return $v->emit_javascript2( $level ) . '.p5push(' . Perlito5::Javascript2::to_list(\@arguments) . ')';
        },
        'tie' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            my $v = shift @arguments;     # TODO - this argument can also be a 'Decl' instead of 'Var'

            my $meth;
            if ( $v->isa('Perlito5::AST::Var') && $v->sigil eq '%' ) {
                $meth = 'hash';
            }
            elsif ( $v->isa('Perlito5::AST::Var') && $v->sigil eq '@' ) {
                $meth = 'array';
            }
            elsif ( $v->isa('Perlito5::AST::Var') && $v->sigil eq '$' ) {
                $meth = 'scalar';
            }
            else {
                die "tie '", ref($v), "' not implemented";
            }
            return 'p5tie_' . $meth . '(' . $v->emit_javascript2( $level ) . ', ' . Perlito5::Javascript2::to_list(\@arguments) . ')';
        },
        'untie' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            my $v = shift @arguments;     # TODO - this argument can also be a 'Decl' instead of 'Var'

            my $meth;
            if ( $v->isa('Perlito5::AST::Var') && $v->sigil eq '%' ) {
                $meth = 'hash';
            }
            elsif ( $v->isa('Perlito5::AST::Var') && $v->sigil eq '@' ) {
                $meth = 'array';
            }
            elsif ( $v->isa('Perlito5::AST::Var') && $v->sigil eq '$' ) {
                $meth = 'scalar';
            }
            else {
                die "tie '", ref($v), "' not implemented";
            }
            return 'p5untie_' . $meth . '(' . $v->emit_javascript2( $level ) . ')';
        },
        'map' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};

            my $fun;

            if ( $self->{special_arg} ) {
                # TODO - test 'special_arg' type (scalar, block, ...)
                $fun  = $self->{special_arg};
            }
            else {
                $fun  = shift @in;
            }
            my $list = Perlito5::Javascript2::to_list(\@in);

            if (ref($fun) eq 'Perlito5::AST::Lit::Block') {
                $fun = $fun->{stmts}
            }
            else {
                $fun = [$fun];
            }

            'p5map(' . Perlito5::Javascript2::pkg() . ', '
                    . 'function (p5want) {' . "\n"
                    . Perlito5::Javascript2::tab($level+1) . (Perlito5::Javascript2::LexicalBlock->new( block => $fun, needs_return => 1, top_level => 0 ))->emit_javascript2( $level + 1 ) . "\n"
                    . Perlito5::Javascript2::tab($level) . '}, '
                    .   $list
                    . ')';
        },
        'grep' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};

            my $fun;

            if ( $self->{special_arg} ) {
                # TODO - test 'special_arg' type (scalar, block, ...)
                $fun  = $self->{special_arg};
            }
            else {
                $fun  = shift @in;
            }
            my $list = Perlito5::Javascript2::to_list(\@in);

            if (ref($fun) eq 'Perlito5::AST::Lit::Block') {
                $fun = $fun->{stmts}
            }
            else {
                $fun = [$fun];
            }

            'p5grep(' . Perlito5::Javascript2::pkg() . ', '

                    . 'function (p5want) {' . "\n"
                    . Perlito5::Javascript2::tab($level+1) . (Perlito5::Javascript2::LexicalBlock->new( block => $fun, needs_return => 1, top_level => 0 ))->emit_javascript2( $level + 1 ) . "\n"
                    . Perlito5::Javascript2::tab($level) . '}, '

                    .   $list
                    . ')';
        },
        'sort' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun;
            my $list;

            if ( $self->{special_arg} ) {
                # TODO - test 'special_arg' type (scalar, block, ...)
                $fun  = $self->{special_arg};
            }
            else {
                if (ref($in[0]) eq 'Perlito5::AST::Lit::Block') {
                    # the sort function is optional
                    $fun  = shift @in;
                }
            }

            if (ref($fun) eq 'Perlito5::AST::Lit::Block') {
                # the sort function is optional
                $fun =
                      'function (p5want) {' . "\n"
                    . Perlito5::Javascript2::tab($level+1) . (Perlito5::Javascript2::LexicalBlock->new( block => $fun->{stmts}, needs_return => 1, top_level => 0 ))->emit_javascript2( $level + 1 ) . "\n"
                    . Perlito5::Javascript2::tab($level) . '}'
            }
            else {
                $fun = 'null';
            }
            $list = Perlito5::Javascript2::to_list(\@in);

            'p5sort(' . Perlito5::Javascript2::pkg() . ', '
                    .   $fun . ', '
                    .   $list
                    . ')';
        },
        'infix:<//>' => sub { 
            my ($self, $level, $wantarray) = @_;
            'p5defined_or' . '('
                . $self->{arguments}->[0]->emit_javascript2($level, 'scalar') . ', '
                . Perlito5::Javascript2::emit_function_javascript2($level, $wantarray, $self->{arguments}->[1]) 
                . ')'
        },
        'exists' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg = $self->{arguments}->[0];
            if ($arg->isa( 'Perlito5::AST::Lookup' )) {
                my $v = $arg->obj;
                if (  $v->isa('Perlito5::AST::Var')
                   && $v->sigil eq '$'
                   )
                {
                    $v = Perlito5::AST::Var->new( sigil => '%', namespace => $v->namespace, name => $v->name );
                    return '(' . $v->emit_javascript2() . ').hasOwnProperty(' . $arg->autoquote($arg->{index_exp})->emit_javascript2($level) . ')';
                }
                return '(' . $v->emit_javascript2() . ')._hash_.hasOwnProperty(' . $arg->autoquote($arg->{index_exp})->emit_javascript2($level) . ')';
            }
            if ($arg->isa( 'Perlito5::AST::Index' )) {
                my $v = $arg->obj;
                if (  $v->isa('Perlito5::AST::Var')
                   && $v->sigil eq '$'
                   )
                {
                    $v = Perlito5::AST::Var->new( sigil => '@', namespace => $v->namespace, name => $v->name );
                    return '(' . $v->emit_javascript2() . ').hasOwnProperty(' . $arg->{index_exp}->emit_javascript2($level) . ')';
                }
                return '(' . $v->emit_javascript2() . ')._array_.hasOwnProperty(' . $arg->{index_exp}->emit_javascript2($level) . ')';
            }
            if ($arg->isa( 'Perlito5::AST::Call' )) {
                if ( $arg->method eq 'postcircumfix:<{ }>' ) {
                    return '(' . $arg->invocant->emit_javascript2() . ')._hash_.hasOwnProperty(' . Perlito5::AST::Lookup->autoquote($arg->{arguments})->emit_javascript2($level) . ')';
                }
                if ( $arg->method eq 'postcircumfix:<[ ]>' ) {
                    return '(' . $arg->invocant->emit_javascript2() . ')._array_.hasOwnProperty(' . $arg->{arguments}->emit_javascript2($level) . ')';
                }
            }
            if (  $arg->isa('Perlito5::AST::Var')
               && $arg->sigil eq '&'
               )
            {
                # TODO exist() + 'my sub'
                my $name = $arg->{name};
                my $namespace = $arg->{namespace} || $Perlito5::PKG_NAME;
                return 'p5pkg[' . Perlito5::Javascript2::escape_string($namespace) . '].hasOwnProperty(' . Perlito5::Javascript2::escape_string($name) . ')';
            }
            if (  $arg->isa('Perlito5::AST::Apply')
               && $arg->{code} eq 'prefix:<&>'
               )
            {
                my $arg2 = $arg->{arguments}->[0];
                return 'p5sub_exists(' . Perlito5::Javascript2::to_str($arg2) . ', ' . Perlito5::Javascript2::escape_string($Perlito5::PKG_NAME) . ')';
            }
        },

        'prototype' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg = $self->{arguments}->[0];
            return 'p5sub_prototype(' . $arg->emit_javascript2() . ', ' . Perlito5::Javascript2::escape_string($Perlito5::PKG_NAME) . ')';
        },
        'split' => sub {
            my ($self, $level, $wantarray) = @_;
            my @js;
            push @{ $self->{arguments} }, Perlito5::AST::Val::Buf->new( buf => ' ' )
                if @{ $self->{arguments} } == 0;
            push @{ $self->{arguments} }, Perlito5::AST::Var->new( sigil => '$', namespace => '', name => '_' )
                if @{ $self->{arguments} } == 1;
            my $arg = $self->{arguments}->[0];
            if ( $arg
              && $arg->isa('Perlito5::AST::Apply')
              && $arg->{code} eq 'p5:m'
            ) {
                # first argument of split() is a regex
                push @js, 'new RegExp('
                        . $arg->{arguments}->[0]->emit_javascript2() . ', '
                        . Perlito5::Javascript2::escape_string($arg->{arguments}->[1]->{buf})
                    . ')';
                shift @{ $self->{arguments} };
            }
            return 'CORE.split('
                . '[' . join( ', ',
                    @js,
                    map( $_->emit_javascript2, @{ $self->{arguments} } ) )
                . '], '
                . Perlito5::Javascript2::to_context($wantarray)
            . ')';
        },
    );

    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        my $apply = $self->op_assign();
        if ($apply) {
            return $apply->emit_javascript2( $level );
        }

        my $code = $self->{code};

        if (ref $code ne '') {
            my @args = ();
            push @args, $_->emit_javascript2
                for @{$self->{arguments}};
            return '(' . $self->{code}->emit_javascript2( $level ) . ')(' . join(',', @args) . ')';
        }

        return $emit_js{$code}->($self, $level, $wantarray)
            if exists $emit_js{$code};

        if (exists $Perlito5::Javascript2::op_infix_js_str{$code}) {
            return '(' 
                . join( $Perlito5::Javascript2::op_infix_js_str{$code}, map { Perlito5::Javascript2::to_str($_, $level) } @{$self->{arguments}} )
                . ')'
        }
        if (exists $Perlito5::Javascript2::op_infix_js_num{$code}) {
            return '(' 
                . join( $Perlito5::Javascript2::op_infix_js_num{$code}, map { Perlito5::Javascript2::to_num($_, $level) } @{$self->{arguments}} )
                . ')'
        }
        if (exists $Perlito5::Javascript2::op_prefix_js_str{$code}) {
            return $Perlito5::Javascript2::op_prefix_js_str{$code} . '(' 
                . Perlito5::Javascript2::to_str($self->{arguments}[0])
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
            $code = Perlito5::Javascript2::pkg() . '.' . $code
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
                    # bareword doesn't call AUTOLOAD
                    return Perlito5::Javascript2::escape_string( 
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
                    push @out, shift(@in)->emit_javascript2( $level, 'scalar' ) if @in || !$optional;
                }
                elsif ($c eq '@') {
                    push @out, Perlito5::Javascript2::to_list(\@in) if @in || !$optional;
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
                            push @out, $arg->emit_javascript2( $level, 'scalar' );
                        }
                    }
                }
                elsif ($c eq '\\') {
                    if (substr($sig, 0, 2) eq '\\$') {
                        $sig = substr($sig, 1);
                        push @out, shift(@in)->emit_javascript2( $level, 'scalar' ) if @in || !$optional;
                    }
                    elsif (substr($sig, 0, 2) eq '\\@'
                        || substr($sig, 0, 2) eq '\\%'
                        )
                    {
                        $sig = substr($sig, 1);
                        push @out, shift(@in)->emit_javascript2( $level, 'list' ) if @in || !$optional;
                    }
                    elsif (substr($sig, 0, 5) eq '\\[@%]') {
                        $sig = substr($sig, 4);
                        push @out, shift(@in)->emit_javascript2( $level, 'list' ) if @in || !$optional;
                    }
                    elsif (substr($sig, 0, 6) eq '\\[$@%]') {
                        $sig = substr($sig, 5);
                        push @out, shift(@in)->emit_javascript2( $level, 'list' ) if @in || !$optional;
                    }
                }
                $sig = substr($sig, 1);
            }

            return $code . '([' . join(', ', @out) . '], '
                        . Perlito5::Javascript2::to_context($wantarray)
                . ')';
        }

        my @args = ();
        my $arg_list = Perlito5::Javascript2::to_list_preprocess( $self->{arguments} );
        push @args, $_->emit_javascript2( $level )
            for @$arg_list;

        my $arg_code = 
            $self->{code} eq 'scalar'      # scalar() is special
            ? '[' . join(', ', @args) . ']'
            : Perlito5::Javascript2::to_list($arg_list);


        if ( $may_need_autoload ) {
            # p5call_sub(namespace, name, list, p5want)
            my $name = $self->{code};
            my $namespace = $self->{namespace} || $Perlito5::PKG_NAME;
            return 'p5call_sub('
                    . Perlito5::Javascript2::escape_string($namespace) . ', '
                    . Perlito5::Javascript2::escape_string($name) . ', '
                    . $arg_code . ', '
                    . Perlito5::Javascript2::to_context($wantarray)
                 . ')';

        }

        $code . '('
                . $arg_code . ', '
                . Perlito5::Javascript2::to_context($wantarray)
              . ')';

    }

    sub emit_javascript2_set_list {
        my ($self, $level, $list) = @_;
        if ( $self->code eq 'undef' ) {
            return $list . '.shift()' 
        }
        if ( $self->code eq 'prefix:<$>' ) {
            return 'p5scalar_deref_set(' 
                . Perlito5::Javascript2::emit_javascript2_autovivify( $self->{arguments}->[0], $level+1, 'scalar' ) . ', '
                . $list . '.shift()'  . ', '
                . Perlito5::Javascript2::escape_string($Perlito5::PKG_NAME)
                . ')';
        }
        die "not implemented: assign to ", $self->code;
    }

    sub emit_javascript2_get_decl {
        my $self      = shift;
        my $code = $self->{code};
        if ($code eq 'my' || $code eq 'our' || $code eq 'state' || $code eq 'local') {
            return ( map {     ref($_) eq 'Perlito5::AST::Var'
                             ? Perlito5::AST::Decl->new(
                                 decl => $code,
                                 type => '',     # TODO - add type
                                 var  => $_,
                               )
                             : ()
                         }
                         @{ $self->{arguments} }
                   );
        }
        if ($code ne 'do' && $code ne 'eval') {
            return ( map  +( $_->emit_javascript2_get_decl ), 
                          @{ $self->{arguments} }
                   )
                if $self->{arguments};
        }
        return ()
    }
}

package Perlito5::AST::If;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        my $cond = $self->{cond};

        # extract declarations from 'cond'
        my @str;
        my $old_level = $level;
        unshift @{ $Perlito5::VAR }, {};    # new compile-time lexical frame for 'cond' variables
        # print Perlito5::Dumper::Dumper($self);
        # print Perlito5::Dumper::Dumper($self->{cond});
        if ($cond) {
            my @var_decl = $cond->emit_javascript2_get_decl();
            for my $arg (@var_decl) {
                $level = $old_level + 1;
                push @str, $arg->emit_javascript2_init($level, $wantarray);
            }
        }

        my $body =
              ref($self->{body}) ne 'Perlito5::AST::Lit::Block'
            ? $self->{body} # may be undef
            : (!@{ $self->{body}->stmts })
            ? undef
            : $wantarray eq 'runtime'
            ? Perlito5::Javascript2::LexicalBlock->new( block => $self->{body}->stmts, needs_return => 1 )
            : Perlito5::Javascript2::LexicalBlock->new( block => $self->{body}->stmts, needs_return => 0, create_context => 1 );
        my $otherwise =
              ref($self->{otherwise}) ne 'Perlito5::AST::Lit::Block'
            ? $self->{otherwise}  # may be undef
            : (!@{ $self->{otherwise}->stmts })
            ? undef
            : $wantarray eq 'runtime'
            ? Perlito5::Javascript2::LexicalBlock->new( block => $self->{otherwise}->stmts, needs_return => 1 )
            : Perlito5::Javascript2::LexicalBlock->new( block => $self->{otherwise}->stmts, needs_return => 0, create_context => 1 );
 
        my $s = 'if ( ' . Perlito5::Javascript2::to_bool($cond, $level + 1) . ' ) {';

        if ($body) {
            $s = $s . "\n"
            . Perlito5::Javascript2::tab($level + 1) . $body->emit_javascript2( $level + 1, $wantarray ) . "\n"
            . Perlito5::Javascript2::tab($level)     . '}';
        }
        else {
            $s = $s . "}";
        }

        if ($otherwise) {
            if ( @{ $otherwise->{block} } == 1 
               && ref($otherwise->{block}[0]) eq 'Perlito5::AST::If'
               )
            {
                $s = $s . "\n"
                . Perlito5::Javascript2::tab($level)     . 'else ' . $otherwise->{block}[0]->emit_javascript2( $level, $wantarray );
            }
            else {
                $s = $s . "\n"
                . Perlito5::Javascript2::tab($level)     . 'else {' . "\n"
                . Perlito5::Javascript2::tab($level + 1) .  $otherwise->emit_javascript2( $level + 1, $wantarray ) . "\n"
                . Perlito5::Javascript2::tab($level)     . '}';
            }
        }

        push @str, $s;

        if (keys %{ $Perlito5::VAR->[0] }) {
            $level = $old_level;
            shift @{ $Perlito5::VAR };  # exit scope of the 'cond' variables
            # create js scope for 'my' variables
            return 
                  ( $wantarray eq 'runtime'
                  ? "return "
                  : ""
                  )
                . Perlito5::Javascript2::emit_wrap_javascript2($level, $wantarray, @str);
        }
        else {
            shift @{ $Perlito5::VAR };  # exit scope of the 'cond' variables
            return join( "\n" . Perlito5::Javascript2::tab($level), @str );
        }

    }
    sub emit_javascript2_get_decl {
        my $self = shift;
        # NOTE - a declaration with modifier has undefined behaviour
        # return $self->{body}->emit_javascript2_get_decl
        #     if $self->{body} && ref($self->{body}) ne 'Perlito5::AST::Lit::Block';
        # return $self->{otherwise}->emit_javascript2_get_decl
        #     if $self->{otherwise} && ref($self->{otherwise}) ne 'Perlito5::AST::Lit::Block';
        return ();
    }
}


package Perlito5::AST::When;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        my $cond = $self->{cond};
        my $body  = Perlito5::Javascript2::LexicalBlock->new( block => $self->{body}->stmts, needs_return => 0, create_context => 1 );

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

        my $s = 'if ( ' . Perlito5::Javascript2::to_bool($expr, $level + 1) . ' ) {' . "\n"
            . Perlito5::Javascript2::tab($level + 1) .       $body->emit_javascript2( $level + 1 ) . "\n"

            . Perlito5::Javascript2::tab($level+1) . 'throw(new p5_error("next", "' . $label . '"))'
            . Perlito5::Javascript2::tab($level) . '}';
        return $s;
    }
    sub emit_javascript2_get_decl { () }
}


package Perlito5::AST::While;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        my $cond = $self->{cond};

        # body is 'Perlito5::AST::Do' in this construct:
        #   do { ... } while ...;
        my $do_at_least_once = ref($self->{body}) eq 'Perlito5::AST::Do' ? 1 : 0;

        my $body =
              ref($self->{body}) ne 'Perlito5::AST::Lit::Block'
            ? [ $self->{body} ]
            : $self->{body}{stmts};

        # extract declarations from 'cond'
        my @str;
        my $old_level = $level;
        unshift @{ $Perlito5::VAR }, {};    # new compile-time lexical frame for 'cond' variables
        # print Perlito5::Dumper::Dumper($self);
        # print Perlito5::Dumper::Dumper($self->{cond});
        if ($cond) {
            my @var_decl = $cond->emit_javascript2_get_decl();
            for my $arg (@var_decl) {
                $level = $old_level + 1;
                push @str, $arg->emit_javascript2_init($level, $wantarray);
            }
        }

        push @str, 'p5while('
                    . "function () {\n"
                    . Perlito5::Javascript2::tab($level + 2) .   (Perlito5::Javascript2::LexicalBlock->new( block => $body, needs_return => 0, top_level => 0 ))->emit_javascript2($level + 2) . "\n"
                    . Perlito5::Javascript2::tab($level + 1) . '}, '
                    . Perlito5::Javascript2::emit_function_javascript2($level + 1, 'void', $cond) . ', '
                    . Perlito5::AST::Lit::Block::emit_javascript2_continue($self, $level) . ', '
                    . Perlito5::Javascript2::escape_string($self->{label} || "") . ', '
                    . $do_at_least_once
                    . ')';

        if (keys %{ $Perlito5::VAR->[0] }) {
            $level = $old_level;
            shift @{ $Perlito5::VAR };  # exit scope of the 'cond' variables
            # create js scope for 'my' variables
            return Perlito5::Javascript2::emit_wrap_javascript2($level, $wantarray, @str);
        }
        else {
            shift @{ $Perlito5::VAR };  # exit scope of the 'cond' variables
            return join( "\n" . Perlito5::Javascript2::tab($level), @str );
        }
    }
    sub emit_javascript2_get_decl { () }
}

package Perlito5::AST::For;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        my $body =
              ref($self->{body}) ne 'Perlito5::AST::Lit::Block'
            ? [ $self->{body} ]
            : $self->{body}{stmts};

        # extract declarations from 'cond'
        my @str;
        my $old_level = $level;
        unshift @{ $Perlito5::VAR }, {};    # new compile-time lexical frame for 'cond' variables
        # print Perlito5::Dumper::Dumper($self);
        # print Perlito5::Dumper::Dumper($self->{cond});
        my $cond = ref( $self->{cond} ) eq 'ARRAY'
                   ? $self->{cond}
                   : [ $self->{cond} ];
        for my $expr (@$cond) {
            if ($expr) {
                my @var_decl = $expr->emit_javascript2_get_decl();
                for my $arg (@var_decl) {
                    $level = $old_level + 1;
                    push @str, $arg->emit_javascript2_init($level, $wantarray);
                }
            }
        }
        # print Perlito5::Dumper::Dumper(\@str);
        # print Perlito5::Dumper::Dumper($Perlito5::VAR);

        unshift @{ $Perlito5::VAR }, {};    # new compile-time lexical frame for the loop variable

        if (ref($self->{cond}) eq 'ARRAY') {
            # C-style for
            # TODO - loop label
            # TODO - make continue-block a syntax error
            push @str, Perlito5::Javascript2::emit_wrap_javascript2($level, $wantarray,
                'var label = "' . ($self->{label} || "") . '";',
                'for ( '
                    . ( $self->{cond}[0] ? $self->{cond}[0]->emit_javascript2($level + 1) . '; '  : '; ' )
                    . ( $self->{cond}[1] ? Perlito5::Javascript2::to_bool($self->{cond}[1], $level + 1) . '; '  : '; ' )
                    . ( $self->{cond}[2] ? $self->{cond}[2]->emit_javascript2($level + 1) . ' '   : ''  )
                  . ') {',
                  [ 'var _redo = true;',
                    'while(_redo) {',
                      [ '_redo = false;',
                        'try {',
                          [
                            Perlito5::Javascript2::LexicalBlock->new( block => $body, needs_return => 0, top_level => 0 )->emit_javascript2($level + 4),
                          ],
                        '}',
                        'catch(err) {',
                          [ 'if (err instanceof p5_error && err.v == label) {',
                              [ 'if (err.type == \'last\') { return }',
                                'else if (err.type == \'redo\') { _redo = true }',
                                'else if (err.type != \'next\') { throw(err) }',
                              ],
                            '}',
                            'else {',
                              [ 'throw(err)',
                              ],
                            '}',
                          ],
                        '}',
                      ],
                    '}',
                  ],
                '}',
            );
        }
        else {

            my $cond = Perlito5::Javascript2::to_list([$self->{cond}], $level + 1);

            my $topic;
            $topic = $self->{body}{sig}
                if ref($self->{body}) ne 'ARRAY';
            if (!$topic) {
                $topic = Perlito5::AST::Decl->new(
                            decl => 'our',
                            type => '',
                            var  => Perlito5::AST::Var->new( name => '_', namespace => '', sigil => '$' ),
                         );
            }

            my $decl = '';
            my $v = $topic;
            if ($v->{decl}) {
                $decl = $v->{decl};
                $v    = $v->{var};
            }
            my $namespace = $v->{namespace} || $Perlito5::PKG_NAME;

            my $perl5_name = $v->perl5_name;
            my $pre_declaration = $v->perl5_get_decl( $perl5_name );
            if ( $pre_declaration ) {
                # say "found ", $pre_declaration->{decl};
                $decl = $pre_declaration->{decl};
            }
            if ( !$decl && !$v->{namespace} ) {
                if ( $Perlito5::STRICT ) {
                    die "Global symbol \"$perl5_name\" requires explicit package name"
                }
                $decl = 'our';
            }

            # mark the variable as "declared"
            $Perlito5::VAR->[0]{ $perl5_name } = { decl => $decl, namespace => $namespace };

            my $s;

            if ($decl eq 'my' || $decl eq 'state') {
                my $sig = $v->emit_javascript2( $level + 1 );
                push @str,
                        'p5for_lex('
                        . "function ($sig) {\n"
                        . Perlito5::Javascript2::tab($level + 2) .   (Perlito5::Javascript2::LexicalBlock->new( block => $body, needs_return => 0, top_level => 0 ))->emit_javascript2($level + 2) . "\n"
                        . Perlito5::Javascript2::tab($level + 1) . '}, '
                        .   $cond . ', '
                        . Perlito5::AST::Lit::Block::emit_javascript2_continue($self, $level) . ', '
                        . Perlito5::Javascript2::escape_string($self->{label} || "")
                        . ')';
            }
            else {
                # use global variable or $_
                push @str,
                       'p5for(' 
                        . 'p5make_package("' . $namespace . '"), '
                        . '"v_' . $v->{name} . '", '
                        . 'function () {' . "\n"
                        . Perlito5::Javascript2::tab($level + 2) .  (Perlito5::Javascript2::LexicalBlock->new( block => $body, needs_return => 0, top_level => 0 ))->emit_javascript2($level + 2) . "\n"
                        . Perlito5::Javascript2::tab($level + 1) . '}, '
                        .   $cond . ', '
                        . Perlito5::AST::Lit::Block::emit_javascript2_continue($self, $level) . ', '
                        . Perlito5::Javascript2::escape_string($self->{label} || "")
                        . ')'
            }
        }

        shift @{ $Perlito5::VAR };  # exit scope of the loop variable

        if (keys %{ $Perlito5::VAR->[0] }) {
            $level = $old_level;
            shift @{ $Perlito5::VAR };  # exit scope of the 'cond' variables
            # create js scope for 'my' variables
            return Perlito5::Javascript2::emit_wrap_javascript2($level, $wantarray, @str);
        }
        else {
            shift @{ $Perlito5::VAR };  # exit scope of the 'cond' variables
            return join( "\n" . Perlito5::Javascript2::tab($level), @str );
        }
    }
    sub emit_javascript2_get_decl { () }
}

package Perlito5::AST::Sub;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        my $prototype = defined($self->{sig}) 
                        ? Perlito5::Javascript2::escape_string($self->{sig}) 
                        : 'null';

        my $sub_ref = Perlito5::Javascript2::get_label();
        local $Perlito5::AST::Sub::SUB_REF = $sub_ref;
        my $js_block = Perlito5::Javascript2::LexicalBlock->new( block => $self->{block}, needs_return => 1, top_level => 1 )->emit_javascript2( $level + 2 );

        my $s = Perlito5::Javascript2::emit_wrap_javascript2($level, 'scalar', 
            "var $sub_ref;",
            "$sub_ref = function (List__, p5want) {",
                [ $js_block ],
            "};",
            "$sub_ref._prototype_ = $prototype;",
            "return $sub_ref",
        );

        if ( $self->{name} ) {
            return 'p5typeglob_set("' . $self->{namespace} . '", "' . $self->{name} . '", ' . $s . ')'
        }
        else {
            return $s;
        }
    }
    sub emit_javascript2_get_decl { () }
}

package Perlito5::AST::Do;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        my $block = $self->simplify->block;
        Perlito5::Javascript2::emit_wrap_javascript2(
            $level,
            $wantarray, 
            (Perlito5::Javascript2::LexicalBlock->new( block => $block, needs_return => 1 ))->emit_javascript2( $level + 1, $wantarray )
        )
    }
    sub emit_javascript2_get_decl { () }
}

package Perlito5::AST::Use;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        Perlito5::Grammar::Use::emit_time_eval($self);
        if ($wantarray eq 'runtime') {
            return 'p5context([], p5want)';
        }
        else {
            return '// ' . $self->{code} . ' ' . $self->{mod} . "\n";
        }
    }
    sub emit_javascript2_get_decl { () }
}

=begin

=head1 NAME

Perlito5::Javascript2::Emit - Code generator for Perlito Perl5-in-Javascript2

=head1 SYNOPSIS

    $program->emit_javascript2()  # generated Perl5 code

=head1 DESCRIPTION

This module generates Javascript2 code for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
