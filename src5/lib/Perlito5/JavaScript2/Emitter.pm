use v5;

use Perlito5::AST;
use Perlito5::Dumper;
use Perlito5::JavaScript2::Apply;
use strict;

package Perlito5::JavaScript2;
{
    my %label;
    sub pkg {
        'p5pkg[' . Perlito5::JavaScript2::escape_string($Perlito5::PKG_NAME ) . ']'
    }
    sub get_label {
        'tmp' . $Perlito5::ID++
    }
    sub tab {
        my $level = shift;
        "\t" x $level
    }

    our $is_inside_subroutine;  # 'shift @_' vs. 'shift @ARGV'
    # our %JavaScript_var_name;   # 101 => 'this.env.[0]'

    # prefix operators that take a "str" parameter
    our %op_prefix_js_str = (
        'prefix:<-A>' => 'p5atime',
        'prefix:<-C>' => 'p5ctime',
        'prefix:<-M>' => 'p5mtime',
        'prefix:<-d>' => 'p5is_directory',
        'prefix:<-e>' => 'p5file_exists',
        'prefix:<-f>' => 'p5is_file',
        'prefix:<-s>' => 'p5size',
        'prefix:<-p>' => 'p5is_pipe',
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
        infix:<~~>
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

            if  (  ($cond->isa( 'Perlito5::AST::Buf' ))
                || ($cond->isa( 'Perlito5::AST::Apply' )  && exists $op_to_str{ $cond->code } )
                )
            {
                return $cond->emit_javascript2($level, $wantarray);
            }
            else {
                return 'p5str(' . $cond->emit_javascript2($level, $wantarray) . ')';
            }
    }
    sub is_num {
            my $cond = shift;
            return 1 if $cond->isa( 'Perlito5::AST::Int' )
                || $cond->isa( 'Perlito5::AST::Num' )
                || ($cond->isa( 'Perlito5::AST::Apply' )  && exists $op_to_num{ $cond->code } );
            return 0;
    }
    sub to_num {
            my $cond = shift;
            my $level = shift;
            my $wantarray = 'scalar';
            if ( is_num($cond) ) {
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

            if  (  ($cond->isa( 'Perlito5::AST::Int' ))
                || ($cond->isa( 'Perlito5::AST::Num' ))
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
            !$_[0]->isa( 'Perlito5::AST::Int' )
         && !$_[0]->isa( 'Perlito5::AST::Num' )
         && !$_[0]->isa( 'Perlito5::AST::Buf' )
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
        :$wantarray eq 'scalar' ? '""'
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
                    . Perlito5::JavaScript2::escape_string($Perlito5::PKG_NAME) . ', '
                    . Perlito5::JavaScript2::escape_string($type)      # autovivification type
                    . ')';
        }
        if ( $obj->isa( 'Perlito5::AST::Apply' ) ) {
            return $obj->emit_javascript2($level);
        }
        if ( $obj->isa( 'Perlito5::AST::Buf' ) ) {
            return $obj->emit_javascript2($level);
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
        my $tab = Perlito5::JavaScript2::tab($level);
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
        if (  $argument->isa( 'Perlito5::AST::Apply' )
           && (  $argument->code eq 'return'
              || $argument->code eq 'last'
              || $argument->code eq 'next'
              || $argument->code eq 'redo' ) )
        {
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

package Perlito5::JavaScript2::LexicalBlock;
{
    sub new { my $class = shift; bless {@_}, $class }
    sub block { $_[0]->{block} }
    # top_level - true if this is the main block in a subroutine;
    # create_context - ... 

    sub has_decl {
        my $self = $_[0];
        my $type = $_[1];
        for my $decl ( @{$self->{block}} ) {
            return 1
                if grep { $_->{decl} eq $type } $decl->emit_javascript2_get_decl();
        }
        return 0;
    }

    sub emit_javascript2_subroutine_body {
        my ($self, $level, $wantarray) = @_;
        $self->{top_level} = 1;
        my $outer_throw = $Perlito5::THROW;
        $Perlito5::THROW = 0;
        my $s = $self->emit_javascript2($level, $wantarray);
        $Perlito5::THROW    = $outer_throw;
        return $s;
    }

    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        my $original_level = $level;

        my @block;
        for my $stmt (@{$self->{block}}) {
            if (defined($stmt)) {
                push @block, $stmt;
            }
        }
        if (!@block) {
            return 'return []'      if $wantarray eq 'list';
            return 'return null'    if $wantarray eq 'scalar';
            return 'return p5want ? [] : null' if $wantarray eq 'runtime';
            return 'null;';         # void
        }
        my @str;
        my $has_local = $self->has_decl("local");
        my $has_regex = 0;
        if (grep {$_->emit_javascript2_has_regex()} @block) {
            # regex variables like '$1' are implicitly 'local'
            $has_local = 1;
            $has_regex = 1;
        }
        my $create_context = $self->{create_context} && $self->has_decl("my");
        my $outer_pkg   = $Perlito5::PKG_NAME;

        if ($self->{top_level} || $create_context) {
            $level++;
        }

        my $last_statement;
        if ($wantarray ne 'void') {
            $last_statement = pop @block;
        }
        for my $decl ( @block ) {
            if ( ref($decl) eq 'Perlito5::AST::Apply' && $decl->code eq 'package' ) {
                $Perlito5::PKG_NAME = $decl->{namespace};
            }

            my @var_decl = $decl->emit_javascript2_get_decl();
            for my $arg (@var_decl) {
                # TODO - create a new context for the redeclared variable
                push @str, $arg->emit_javascript2_init($level, $wantarray);
            }

            if (!( $decl->isa( 'Perlito5::AST::Decl' ) && $decl->decl eq 'my' )) {
                push @str, $decl->emit_javascript2($level, 'void') . ';';
            }
        }

        if ($last_statement) {

            my @var_decl = $last_statement->emit_javascript2_get_decl();
            for my $arg (@var_decl) {
                # TODO - create a new context for the redeclared variable
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
                  || $last_statement->isa( 'Perlito5::AST::Block' )
                  || $last_statement->isa( 'Perlito5::AST::Apply' ) && $last_statement->code eq 'goto'
                  || $last_statement->isa( 'Perlito5::AST::Apply' ) && $last_statement->code eq 'return'
                  )
            {
                push @str, $last_statement->emit_javascript2($level, $wantarray);
            }
            else {
                if ( $has_local ) {
                    push @str, 'return p5cleanup_local(local_idx, ('
                        . ( $wantarray eq 'runtime'
                          ? Perlito5::JavaScript2::to_runtime_context([$last_statement], $level+1)
                          : $wantarray eq 'scalar'
                          ? Perlito5::JavaScript2::to_scalar([$last_statement], $level+1)
                          : $last_statement->emit_javascript2($level, $wantarray)
                          )
                    . '));';
                }
                else {
                    push @str, 'return ('
                        . ( $wantarray eq 'runtime'
                          ? Perlito5::JavaScript2::to_runtime_context([$last_statement], $level+1)
                          : $wantarray eq 'scalar'
                          ? Perlito5::JavaScript2::to_scalar([$last_statement], $level+1)
                          : $last_statement->emit_javascript2($level, $wantarray)
                          )
                    . ');';
                }
            }
        }
        if ( $has_local ) {
            unshift @str, (
                    'var local_idx = p5LOCAL.length;',
                    ( $has_regex
                      ? ( 'var regex_tmp = p5_regex_capture;',
                          'p5LOCAL.push(function(){ p5_regex_capture = regex_tmp });',
                      )
                      : ()
                    )
                );
            push    @str, 'p5cleanup_local(local_idx, null);';
        }
        my $out;
        if ($self->{top_level} && $Perlito5::THROW) {

            # TODO - emit error message if catched a "next/redo/last LABEL" when expecting a "return" exception

            $level = $original_level;
            my $tab = "\n" . Perlito5::JavaScript2::tab($level + 1);
            $out =                                         "try {"
                . $tab                                   .    join($tab, @str) . "\n"
                . Perlito5::JavaScript2::tab($level)     . '}' . "\n"
                . Perlito5::JavaScript2::tab($level)     . 'catch(err) {' . "\n"
                . Perlito5::JavaScript2::tab($level + 1) .    'if ( err instanceof Error ) {' . "\n"
                . Perlito5::JavaScript2::tab($level + 2)         . 'throw(err);' . "\n"
                . Perlito5::JavaScript2::tab($level + 1) .    '}' . "\n"
                . Perlito5::JavaScript2::tab($level + 1) .    'else {' . "\n"
                . Perlito5::JavaScript2::tab($level + 2)
                    . ( $has_local
                      ? 'return p5cleanup_local(local_idx, err)'
                      : 'return(err)'
                      )
                    . ";\n"
                . Perlito5::JavaScript2::tab($level + 1) .   '}' . "\n"
                . Perlito5::JavaScript2::tab($level)     . '}';
        }
        elsif ( $create_context ) {
            $level = $original_level;
            my $tab = "\n" . Perlito5::JavaScript2::tab($level + 1);
            $out =                                        "(function () {"
                  . $tab                               .     join($tab, @str) . "\n"
                  . Perlito5::JavaScript2::tab($level) .  "})();";
        }
        else {
            $level = $original_level;
            my $tab = "\n" . Perlito5::JavaScript2::tab($level);
            $out = join($tab, @str);
        }
        $Perlito5::PKG_NAME = $outer_pkg;
        return $out;
    }
    sub emit_javascript2_has_regex { () }
}

package Perlito5::AST::CompUnit;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        return Perlito5::JavaScript2::emit_wrap_javascript2($level, $wantarray, 
            Perlito5::JavaScript2::LexicalBlock->new( block => $self->{body} )->emit_javascript2( $level + 1, $wantarray )
        ) . ';';
    }
    sub emit_javascript2_program {
        my ($comp_units, %options) = @_;
        $Perlito5::PKG_NAME = 'main';
        my $level = 0;
        my $wantarray = 'void';
        my $str;
        $str .= Perlito5::Compiler::do_not_edit("//");
        if ( $options{expand_use} ) {
            $str .= Perlito5::JavaScript2::Runtime->emit_javascript2();
            $str .= Perlito5::JavaScript2::Array->emit_javascript2();
            $str .= Perlito5::JavaScript2::CORE->emit_javascript2();
            $str .= Perlito5::JavaScript2::IO->emit_javascript2();
            $str .= Perlito5::JavaScript2::Sprintf->emit_javascript2();
        }
        $str .= "var p5want;\n"
             .  "var List__ = [];\n";
        for my $comp_unit ( @$comp_units ) {
            $str = $str . $comp_unit->emit_javascript2($level, $wantarray) . ";\n";
        }
        return $str;
    }
    sub emit_javascript2_get_decl { () }
    sub emit_javascript2_has_regex { () }
}

package Perlito5::AST::Int;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        $self->{int};
    }
    sub emit_javascript2_get_decl { () }
    sub emit_javascript2_has_regex { () }
}

package Perlito5::AST::Num;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        $self->{num};
    }
    sub emit_javascript2_get_decl { () }
    sub emit_javascript2_has_regex { () }
}

package Perlito5::AST::Buf;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        Perlito5::JavaScript2::escape_string( $self->{buf} );
    }
    sub emit_javascript2_get_decl { () }
    sub emit_javascript2_has_regex { () }
}

package Perlito5::AST::Block;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        my $body;
        if ($wantarray ne 'void') {
            $body = Perlito5::JavaScript2::LexicalBlock->new( block => $self->{stmts} );
        }
        else {
            $body = Perlito5::JavaScript2::LexicalBlock->new( block => $self->{stmts} );
        }

        my $init = "";
        if ($self->{name} eq 'INIT') {
            my $tmp  = 'p5pkg.main.' . Perlito5::JavaScript2::get_label();

            # INIT-blocks execute only once
            $init = Perlito5::JavaScript2::tab($level + 2) . "if ($tmp) { return }; $tmp = 1;\n";

            # TODO - make this execute before anything else

        }

        return 
                  ( $wantarray ne 'void'
                  ? "return "
                  : ""
                  )
                . 'p5block('
                . "function (v) {}, "
                . "function () {\n"
                .                                             $init
                . Perlito5::JavaScript2::tab($level + 2) .    $body->emit_javascript2($level + 2, $wantarray) . "\n"
                . Perlito5::JavaScript2::tab($level + 1) . '}, '
                .   '[0], '
                . $self->emit_javascript2_continue($level, $wantarray) . ', '
                . Perlito5::JavaScript2::escape_string($self->{label} || "") . "\n"
                . Perlito5::JavaScript2::tab($level) . ')'
    }
    sub emit_javascript2_continue {
        my $self = shift;
        my $level = shift;
        my $wantarray = shift;

        if (!$self->{continue} || !@{ $self->{continue}{stmts} }) {
            return 'false'
        }

        return
              "function () {\n"
            .   (Perlito5::JavaScript2::LexicalBlock->new( block => $self->{continue}->stmts ))->emit_javascript2($level + 2, $wantarray) . "\n"
            . Perlito5::JavaScript2::tab($level + 1) . '}'
    }
    sub emit_javascript2_get_decl { () }
    sub emit_javascript2_has_regex { () }
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
           || (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->code eq 'circumfix:<( )>'
              )
           )
        {
            # @a[10, 20]
            # @$a[0, 2] ==> @{$a}[0,2]
            # (4,5,6)[0,2]
            return 'p5list_slice('
                        . $self->{obj}->emit_javascript2($level, 'list') . ', '
                        . Perlito5::JavaScript2::to_list([$self->{index_exp}], $level) . ', '
                        . Perlito5::JavaScript2::to_context($wantarray)
                   . ')'
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

            return 'p5hash_slice('
                        . $self->{obj}->emit_javascript2($level, 'list') . ', '
                        . Perlito5::JavaScript2::to_list([$self->{index_exp}], $level) . ', '
                        . Perlito5::JavaScript2::to_context($wantarray)
                   . ')';
        }
        return $self->emit_javascript2_container($level) . '.' . $method . '(' 
                        . Perlito5::JavaScript2::to_num($self->{index_exp}, $level) 
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
            return Perlito5::JavaScript2::emit_wrap_javascript2($level, $wantarray, 
                    'var a = [];',
                    'var v = ' . Perlito5::JavaScript2::to_list([$self->{index_exp}], $level) . ';',
                    'var src=' . Perlito5::JavaScript2::to_list([$arguments], $level) . ";",
                    'var out=' . Perlito5::JavaScript2::emit_javascript2_autovivify( $self->{obj}, $level, 'array' ) . ";",
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
                    . Perlito5::JavaScript2::to_num($self->{index_exp}, $level+1) . ', ' 
                    . Perlito5::JavaScript2::to_scalar([$arguments], $level+1)
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
            return Perlito5::JavaScript2::emit_wrap_javascript2($level, $wantarray, 
                    'var a = [];',
                    'var v = ' . Perlito5::JavaScript2::to_list([$self->{index_exp}], $level) . ';',
                    'var out=' . Perlito5::JavaScript2::emit_javascript2_autovivify( $self->{obj}, $level, 'array' ) . ";",
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
                    . Perlito5::JavaScript2::to_num($self->{index_exp}, $level+1) . ', ' 
                    . $list . '.shift()'
                . ')';
    }
    sub emit_javascript2_container {
        my $self = shift;
        my $level = shift;
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # ${"Exporter::Cache"}[2]
            # $$a[0] ==> $a->[0]
            my $v = Perlito5::AST::Apply->new( %{$self->{obj}}, code => 'prefix:<@>' );
            return $v->emit_javascript2($level);
        }
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->code eq 'circumfix:<( )>'
           )
        {
            # the expression inside () returns a list
            return Perlito5::JavaScript2::to_list([$self->{obj}], $level);
        }
        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && $self->{obj}->sigil eq '$'
           )
        {
            $self->{obj}->{sigil} = '@';
            return $self->{obj}->emit_javascript2($level);
        }
        else {
            return Perlito5::JavaScript2::emit_javascript2_autovivify( $self->{obj}, $level, 'array' ) . '._array_';
        }
    }
    sub emit_javascript2_get_decl { () }
    sub emit_javascript2_has_regex { () }
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
            if ( $self->{obj}->isa('Perlito5::AST::Var') ) {
                $v = $self->{obj};
            }
            $v = Perlito5::AST::Apply->new( code => 'prefix:<%>', namespace => $self->{obj}->namespace, arguments => $self->{obj}->arguments )
                if $self->{obj}->isa('Perlito5::AST::Apply');

            return 'p5list_lookup_slice('
                        . $v->emit_javascript2($level, 'list') . ', '
                        . Perlito5::JavaScript2::to_list([$self->{index_exp}], $level) . ', '
                        . Perlito5::JavaScript2::to_context($wantarray)
                   . ')'
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
            if ( $self->{obj}->isa('Perlito5::AST::Var') ) {
                $v = $self->{obj};
            }
            $v = Perlito5::AST::Apply->new( code => 'prefix:<%>', namespace => $self->{obj}->namespace, arguments => $self->{obj}->arguments )
                if $self->{obj}->isa('Perlito5::AST::Apply');

            return 'p5hash_lookup_slice('
                        . $v->emit_javascript2($level, 'list') . ', '
                        . Perlito5::JavaScript2::to_list([$self->{index_exp}], $level) . ', '
                        . Perlito5::JavaScript2::to_context($wantarray)
                   . ')'
        }
        return $self->emit_javascript2_container($level) . '.' . $method . '('
                . Perlito5::JavaScript2::autoquote($self->{index_exp}, $level)
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
            $v = $self->{obj}
                if $self->{obj}->isa('Perlito5::AST::Var');
            $v = Perlito5::AST::Apply->new( code => 'prefix:<%>', namespace => $self->{obj}->namespace, arguments => $self->{obj}->arguments )
                if $self->{obj}->isa('Perlito5::AST::Apply');
            return Perlito5::JavaScript2::emit_wrap_javascript2($level, $wantarray, 
                    'var a = [];',
                    'var v = ' . Perlito5::JavaScript2::to_list([$self->{index_exp}], $level) . ';',
                    'var src=' . Perlito5::JavaScript2::to_list([$arguments], $level) . ";",
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
                    . Perlito5::JavaScript2::autoquote($self->{index_exp}, $level) . ', '
                    . Perlito5::JavaScript2::to_scalar([$arguments], $level+1)
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
            $v = $self->{obj}
                if $self->{obj}->isa('Perlito5::AST::Var');
            $v = Perlito5::AST::Apply->new( code => 'prefix:<%>', namespace => $self->{obj}->namespace, arguments => $self->{obj}->arguments )
                if $self->{obj}->isa('Perlito5::AST::Apply');
            return Perlito5::JavaScript2::emit_wrap_javascript2($level, $wantarray, 
                    'var a = [];',
                    'var v = ' . Perlito5::JavaScript2::to_list([$self->{index_exp}], $level) . ';',
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
                    . Perlito5::JavaScript2::autoquote($self->{index_exp}, $level) . ', '
                    . $list . '.shift()'
            . ')';
    }
    sub emit_javascript2_container {
        my $self = shift;
        my $level = shift;
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # ${"Exporter::Cache"}{x}
            # $$a{0} ==> $a->{0}
            my $v = Perlito5::AST::Apply->new( %{$self->{obj}}, code => 'prefix:<%>' );
            return $v->emit_javascript2($level);
        }
        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && $self->{obj}->sigil eq '$'
           )
        {
            # my $v = $self->{obj};   HERE

            #if ($self->{obj}{_real_sigil} ne '%') {
            #    warn Data::Dumper::Dumper($self->{obj});
            #}

            my $v = Perlito5::AST::Var->new( %{$self->{obj}}, sigil => '%' );
            return $v->emit_javascript2($level)
        }
        else {
            return Perlito5::JavaScript2::emit_javascript2_autovivify( $self->{obj}, $level, 'hash' ) . '._hash_';
        }
    }
    sub emit_javascript2_get_decl { () }
    sub emit_javascript2_has_regex { () }
}

package Perlito5::AST::Var;
{
    my $table = {
        '$' => 'v_',
        '@' => 'List_',
        '%' => 'Hash_',
        '&' => '',
    };

    sub emit_javascript2_global {
        my ($self, $level, $wantarray) = @_;
        my $str_name = $self->{name};
        my $sigil = $self->{_real_sigil} || $self->{sigil};
        my $namespace = $self->{namespace} || $self->{_namespace};
        if ($sigil eq '@' && $self->{name} eq '_' && $namespace eq 'main') {
            # XXX - optimization - @_ is a js lexical
            my $s = 'List__';
            if ($self->{sigil} eq '$#') {
                return '(' . $s . '.length - 1)';
            }
            if ( $wantarray eq 'scalar' ) {
                return $s . '.length';
            }
            if ( $wantarray eq 'runtime' ) {
                return '(p5want'
                    . ' ? ' . $s
                    . ' : ' . $s . '.length'
                    . ')';
            }
            return $s;
        }

        if ($sigil eq '$' && $self->{name} > 0) {
            # regex captures
            return 'p5_regex_capture[' . ($self->{name} - 1) . ']'
        }
        if ( $sigil eq '::' ) {

            return Perlito5::JavaScript2::pkg()
                if $self->{namespace} eq '__PACKAGE__';
            return $Perlito5::AST::Sub::SUB_REF // '__SUB__'
                if $self->{namespace} eq '__SUB__';

            return Perlito5::JavaScript2::escape_string( $namespace );
        }

        my $s = 'p5make_package(' . Perlito5::JavaScript2::escape_string($namespace ) . ')[' . Perlito5::JavaScript2::escape_string($table->{$sigil} . $str_name) . ']';
        if ( $sigil eq '*' ) {
            return $s;
        }
        if ( $sigil eq '&' ) {
            return $s . '(List__, ' . Perlito5::JavaScript2::to_context($wantarray) . ')';
        }
        if ($sigil eq '@') {
            $s = $s . ' || (' . $s . ' = [])';  # init
            $s = 'p5pkg[' . $s . ', ' . Perlito5::JavaScript2::escape_string($namespace ) . '][' . Perlito5::JavaScript2::escape_string($table->{$sigil} . $str_name) . ']';
            if ($self->{sigil} eq '$#') {
                return '(' . $s . '.length - 1)';
            }
            if ( $wantarray eq 'scalar' ) {
                return $s . '.length';
            }
        }
        elsif ($sigil eq '%') {
            $s = $s . ' || (' . $s . ' = {})';  # init
            $s = 'p5pkg[' . $s . ', ' . Perlito5::JavaScript2::escape_string($namespace ) . '][' . Perlito5::JavaScript2::escape_string($table->{$sigil} . $str_name) . ']';
        }
        return $s;
    }

    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        my $sigil = $self->{_real_sigil} || $self->{sigil};
        my $decl_type = $self->{_decl} || 'global';
        if ( $decl_type ne 'my' && $decl_type ne 'state' ) {
            return $self->emit_javascript2_global($level, $wantarray);
        }

        my $str_name = $self->{name} . "_" . $self->{_id};

        # $str_name = $Perlito5::JavaScript2::JavaScript_var_name{$self->{_id}}
        #     if exists $Perlito5::JavaScript2::JavaScript_var_name{$self->{_id}};

        if ( $sigil eq '@' ) {
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
        if ($self->{sigil} eq '$#') {
            return '(' . $table->{'@'} . $str_name . '.length - 1)';
        }
        $table->{$sigil} . $str_name
    }

    sub emit_javascript2_set {
        my ($self, $arguments, $level, $wantarray) = @_;
        my $open  = $wantarray eq 'void' ? '' : '(';
        my $close = $wantarray eq 'void' ? '' : ')';
        my $sigil = $self->{_real_sigil} || $self->{sigil};
        if ( $sigil eq '$' ) {
            return $open . $self->emit_javascript2() . ' = ' . Perlito5::JavaScript2::to_scalar([$arguments], $level+1) . $close
        }
        if ( $sigil eq '@' ) {

            if ($self->{sigil} eq '$#') {
                $self->{sigil} = '@';
                return $open . $self->emit_javascript2() . '.length = 1 + ' . Perlito5::JavaScript2::to_scalar([$arguments], $level+1) . $close
            }

            return $open . $self->emit_javascript2() . ' = ' . Perlito5::JavaScript2::to_list([$arguments], $level+1) . $close
        }
        if ( $sigil eq '%' ) {
            return $open . $self->emit_javascript2() . ' = ' . Perlito5::JavaScript2::to_list([$arguments], $level+1, 'hash') . $close 
        }
        if ( $sigil eq '*' ) {
            my $namespace = $self->{namespace} || $self->{_namespace};
            return 'p5typeglob_set(' 
            .   Perlito5::JavaScript2::escape_string($namespace) . ', '
            .   Perlito5::JavaScript2::escape_string($self->{name}) . ', ' 
            .   Perlito5::JavaScript2::to_scalar([$arguments], $level+1)
            . ')'
        }
        die "don't know how to assign to variable ", $sigil, $self->name;
    }

    sub emit_javascript2_set_list {
        my ($self, $level, $list) = @_;
        my $sigil = $self->{_real_sigil} || $self->{sigil};
        if ( $sigil eq '$' ) {
            return $self->emit_javascript2() . ' = ' . $list  . '.shift()'
        }
        if ( $sigil eq '@' ) {
            return join( ";\n" . Perlito5::JavaScript2::tab($level),
                $self->emit_javascript2() . ' = ' . $list,
                $list . ' = []'
            );
        }
        if ( $sigil eq '%' ) {
            return join( ";\n" . Perlito5::JavaScript2::tab($level),
                $self->emit_javascript2() . ' = p5a_to_h(' . $list  . ')',
                $list . ' = []'
            );
        }
        die "don't know how to assign to variable ", $sigil, $self->name;
    }

    sub emit_javascript2_get_decl { () }
    sub emit_javascript2_has_regex { () }
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
            my $var_set;
            my $tmp_name  = Perlito5::JavaScript2::get_label();
            my $id = $Perlito5::ID++;
            my $tmp = Perlito5::AST::Var->new(sigil => '$', name => $tmp_name, _decl => 'my', _id => $id );
            if ( ref($var) eq 'Perlito5::AST::Var' ) {
                $var_set = $var->emit_javascript2 . ' = ' .  $tmp->emit_javascript2;
            }
            else {
                $var_set = $var->emit_javascript2_set($tmp);
            }
            return Perlito5::JavaScript2::emit_wrap_javascript2($level, $wantarray, 
                     'var ' . $tmp->emit_javascript2 . ' = ' . $var->emit_javascript2 . ';',
                     'p5LOCAL.push(function(){ ' . $var_set . ' });',
                     'return ' . $var->emit_javascript2_set(
                                    Perlito5::AST::Apply->new( code => 'undef', arguments => [], namespace => '' ),
                                    $level+1
                                 ) . ';',
                ) . ';';
        }
        if ($self->{decl} eq 'my' || $self->{decl} eq 'state') {
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
            return 'if (typeof ' . $self->{var}->emit_javascript2() . ' == "undefined" ) { '
                    . $str
                    . '}';
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
    sub emit_javascript2_has_regex { () }
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
            return Perlito5::JavaScript2::emit_javascript2_autovivify( $self->{invocant}, $level, 'array' )
                . '._array_.' . $method . '(' . Perlito5::JavaScript2::to_num($self->{arguments}, $level+1)
                . ')';
        }
        if ( $meth eq 'postcircumfix:<{ }>' ) {
            my $method = $autovivification_type || 'p5hget';
            $method = 'p5hget_array' if $autovivification_type eq 'array';
            $method = 'p5hget_hash'  if $autovivification_type eq 'hash';
            return Perlito5::JavaScript2::emit_javascript2_autovivify( $self->{invocant}, $level, 'hash' )
                . '._hash_.' . $method . '(' . Perlito5::JavaScript2::autoquote($self->{arguments}, $level+1, 'list')
                . ')';
        }
        if  ($meth eq 'postcircumfix:<( )>')  {
            my $invocant = $self->{invocant}->emit_javascript2($level, 'scalar');
            return '(' . $invocant . ')(' . Perlito5::JavaScript2::to_list($self->{arguments}) . ', '
                        . Perlito5::JavaScript2::to_context($wantarray)
                    . ')';
        }

        my $invocant = $self->{invocant}->emit_javascript2($level, 'scalar');
        if ( ref($meth) eq 'Perlito5::AST::Var' ) {
            $meth = $meth->emit_javascript2($level, 'scalar');
        }
        else {
            $meth = Perlito5::JavaScript2::escape_string($meth);
        }
        return 'p5call(' . $invocant . ', ' 
                         . $meth . ', ' 
                         . Perlito5::JavaScript2::to_list($self->{arguments}) . ', '
                         . Perlito5::JavaScript2::to_context($wantarray)
                  . ')'
    }

    sub emit_javascript2_set {
        my ($self, $arguments, $level, $wantarray) = @_;
        if ( $self->{method} eq 'postcircumfix:<[ ]>' ) {
            return Perlito5::JavaScript2::emit_javascript2_autovivify( $self->{invocant}, $level, 'array' )
                    . '._array_.p5aset(' 
                        . Perlito5::JavaScript2::to_num($self->{arguments}, $level+1) . ', ' 
                        . Perlito5::JavaScript2::to_scalar([$arguments], $level+1)
                    . ')';
        }
        if ( $self->{method} eq 'postcircumfix:<{ }>' ) {
            return Perlito5::JavaScript2::emit_javascript2_autovivify( $self->{invocant}, $level, 'hash' )
                    . '._hash_.p5hset(' 
                        . Perlito5::JavaScript2::autoquote($self->{arguments}, $level+1, 'list') . ', '
                        . Perlito5::JavaScript2::to_scalar([$arguments], $level+1)
                    . ')';
        }
        die "don't know how to assign to method ", $self->{method};
    }
    sub emit_javascript2_set_list {
        my ($self, $level, $list) = @_;
        if ( $self->{method} eq 'postcircumfix:<[ ]>' ) {
            return Perlito5::JavaScript2::emit_javascript2_autovivify( $self->{invocant}, $level, 'array' )
                    . '._array_.p5aset(' 
                        . Perlito5::JavaScript2::to_num($self->{arguments}, $level+1) . ', ' 
                        . $list  . '.shift()'
                    . ')';
        }
        if ( $self->{method} eq 'postcircumfix:<{ }>' ) {
            return Perlito5::JavaScript2::emit_javascript2_autovivify( $self->{invocant}, $level, 'hash' )
                    . '._hash_.p5hset(' 
                        . Perlito5::JavaScript2::autoquote($self->{arguments}, $level+1, 'list') . ', '
                        . $list  . '.shift()'
                    . ')';
        }
        die "don't know how to assign to method ", $self->{method};
    }
    sub emit_javascript2_get_decl { () }
    sub emit_javascript2_has_regex { () }
}

package Perlito5::AST::If;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        my $cond = $self->{cond};

        # extract declarations from 'cond'
        my @str;
        my $old_level = $level;
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
              ref($self->{body}) ne 'Perlito5::AST::Block'
            ? $self->{body} # may be undef
            : (!@{ $self->{body}->stmts })
            ? undef
            : $wantarray ne 'void'
            ? Perlito5::JavaScript2::LexicalBlock->new( block => $self->{body}->stmts, )
            : Perlito5::JavaScript2::LexicalBlock->new( block => $self->{body}->stmts, create_context => 1 );
        my $otherwise =
              ref($self->{otherwise}) ne 'Perlito5::AST::Block'
            ? $self->{otherwise}  # may be undef
            : (!@{ $self->{otherwise}->stmts })
            ? undef
            : $wantarray ne 'void'
            ? Perlito5::JavaScript2::LexicalBlock->new( block => $self->{otherwise}->stmts )
            : Perlito5::JavaScript2::LexicalBlock->new( block => $self->{otherwise}->stmts, create_context => 1 );
 
        my $s = 'if ( ' . Perlito5::JavaScript2::to_bool($cond, $level + 1) . ' ) {';

        if ($body) {
            $s = $s . "\n"
            . Perlito5::JavaScript2::tab($level + 1) . $body->emit_javascript2( $level + 1, $wantarray ) . "\n"
            . Perlito5::JavaScript2::tab($level)     . '}';
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
                . Perlito5::JavaScript2::tab($level)     . 'else ' . $otherwise->{block}[0]->emit_javascript2( $level, $wantarray );
            }
            else {
                $s = $s . "\n"
                . Perlito5::JavaScript2::tab($level)     . 'else {' . "\n"
                . Perlito5::JavaScript2::tab($level + 1) .  $otherwise->emit_javascript2( $level + 1, $wantarray ) . "\n"
                . Perlito5::JavaScript2::tab($level)     . '}';
            }
        }

        push @str, $s;

        if (@str) {
            $level = $old_level;
            # create js scope for 'my' variables
            return 
                  ( $wantarray ne 'void'
                  ? "return "
                  : ""
                  )
                . Perlito5::JavaScript2::emit_wrap_javascript2($level, $wantarray, @str);
        }
        else {
            return join( "\n" . Perlito5::JavaScript2::tab($level), @str );
        }

    }
    sub emit_javascript2_get_decl { () }
    sub emit_javascript2_has_regex { () }
}


package Perlito5::AST::When;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        # TODO - special case when When is inside a Given block
        # TODO - special case when When is a statement modifier
        my $cond = $self->{cond};
        # extract declarations from 'cond'
        my @str;
        my $old_level = $level;
        # print Perlito5::Dumper::Dumper($self);
        # print Perlito5::Dumper::Dumper($self->{cond});
        if ($cond) {
            my @var_decl = $cond->emit_javascript2_get_decl();
            for my $arg (@var_decl) {
                $level = $old_level + 1;
                push @str, $arg->emit_javascript2_init($level, $wantarray);
            }
        }
        $cond = Perlito5::AST::Apply->new(
                'arguments' => [
                    Perlito5::AST::Var::SCALAR_ARG(),
                    $cond,
                ],
                'code' => 'infix:<~~>',
                'namespace' => '',
            );
        my $next = Perlito5::AST::Apply->new(
                'arguments' => [],
                'bareword' => 1,
                'code' => 'next',
                'namespace' => '',
            );
        my $body =
              ref($self->{body}) ne 'Perlito5::AST::Block'
            ? Perlito5::JavaScript2::LexicalBlock->new( block => [ $self->{body} ] )
            : (!@{ $self->{body}->stmts })
            ? undef
            : $wantarray ne 'void'
            ? Perlito5::JavaScript2::LexicalBlock->new( block => $self->{body}->stmts, )
            : Perlito5::JavaScript2::LexicalBlock->new( block => $self->{body}->stmts, create_context => 1 );
        push @{ $body->{block} }, $next; 
        my $s = 'if ( ' . Perlito5::JavaScript2::to_bool($cond, $level + 1) . ' ) {';

        if ($body) {
            $s = $s . "\n"
            . Perlito5::JavaScript2::tab($level + 1) . $body->emit_javascript2( $level + 1, $wantarray ) . "\n"
            . Perlito5::JavaScript2::tab($level)     . '}';
        }
        else {
            $s = $s . "}";
        }
        push @str, $s;

        if (@str) {
            $level = $old_level;
            # create js scope for 'my' variables
            return 
                  ( $wantarray ne 'void'
                  ? "return "
                  : ""
                  )
                . Perlito5::JavaScript2::emit_wrap_javascript2($level, $wantarray, @str);
        }
        else {
            return join( "\n" . Perlito5::JavaScript2::tab($level), @str );
        }
    }
    sub emit_javascript2_get_decl { () }
    sub emit_javascript2_has_regex { () }
}


package Perlito5::AST::While;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        my $cond = $self->{cond};

        # extract declarations from 'cond'
        my @str;
        my $old_level = $level;
        # print Perlito5::Dumper::Dumper($self);
        # print Perlito5::Dumper::Dumper($self->{cond});
        if ($cond) {
            my @var_decl = $cond->emit_javascript2_get_decl();
            for my $arg (@var_decl) {
                $level = $old_level + 1;
                push @str, $arg->emit_javascript2_init($level, $wantarray);
            }
        }

        # body is 'Perlito5::AST::Apply' in this construct:
        #   do { ... } while ...;
        if ( ref($self->{body}) eq 'Perlito5::AST::Apply' && $self->{body}{code} eq 'do' ) {
            push @str,
                  'do {'
                .   $self->{body}->emit_javascript2($level + 2, $wantarray) . "\n"
                . Perlito5::JavaScript2::tab($level + 1) . '} while ('
                .   Perlito5::JavaScript2::to_bool($cond, $level + 2)
                . ')';
        }
        else {
            my $body =
                  ref($self->{body}) ne 'Perlito5::AST::Block'
                ? [ $self->{body} ]
                : $self->{body}{stmts};
            push @str, 'p5while('
                    . "function () {\n"
                    . Perlito5::JavaScript2::tab($level + 2) .   (Perlito5::JavaScript2::LexicalBlock->new( block => $body ))->emit_javascript2($level + 2, $wantarray) . "\n"
                    . Perlito5::JavaScript2::tab($level + 1) . '}, '
                    . Perlito5::JavaScript2::emit_function_javascript2($level + 1, 'scalar', $cond) . ', '
                    . Perlito5::AST::Block::emit_javascript2_continue($self, $level, $wantarray) . ', '
                    . Perlito5::JavaScript2::escape_string($self->{label} || "") . ', '
                    . '0'
                    . ')';
        }

        if (@str) {
            $level = $old_level;
            # create js scope for 'my' variables
            return Perlito5::JavaScript2::emit_wrap_javascript2($level, $wantarray, @str);
        }
        else {
            return join( "\n" . Perlito5::JavaScript2::tab($level), @str );
        }
    }
    sub emit_javascript2_get_decl { () }
    sub emit_javascript2_has_regex { () }
}

package Perlito5::AST::For;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        my $body =
              ref($self->{body}) ne 'Perlito5::AST::Block'
            ? [ $self->{body} ]
            : $self->{body}{stmts};

        # extract declarations from 'cond'
        my @str;
        my $old_level = $level;
        # print Perlito5::Dumper::Dumper($self);
        # print Perlito5::Dumper::Dumper($self->{cond});
        my $cond = ref( $self->{cond} ) eq 'ARRAY'
                   ? $self->{cond}
                   : [ $self->{cond} ];
        for my $expr ( @$cond, $self->{topic} ) {
            if ($expr) {
                my @var_decl = $expr->emit_javascript2_get_decl();
                for my $arg (@var_decl) {
                    $level = $old_level + 1;
                    push @str, $arg->emit_javascript2_init($level, $wantarray);
                }
            }
        }
        # print Perlito5::Dumper::Dumper(\@str);

        if (ref($self->{cond}) eq 'ARRAY') {
            # C-style for
            # TODO - loop label
            # TODO - make continue-block a syntax error
            push @str, Perlito5::JavaScript2::emit_wrap_javascript2($level, $wantarray,
                'var label = ' . Perlito5::JavaScript2::escape_string(($self->{label} || "") ) . ';',
                'for ( '
                    . ( $self->{cond}[0] ? $self->{cond}[0]->emit_javascript2($level + 1) . '; '  : '; ' )
                    . ( $self->{cond}[1] ? Perlito5::JavaScript2::to_bool($self->{cond}[1], $level + 1) . '; '  : '; ' )
                    . ( $self->{cond}[2] ? $self->{cond}[2]->emit_javascript2($level + 1) . ' '   : ''  )
                  . ') {',
                  [ 'var _redo;',
                    'do {',
                      [ '_redo = false;',
                        'try {',
                          [
                            Perlito5::JavaScript2::LexicalBlock->new( block => $body )->emit_javascript2($level + 4, 'void'),
                          ],
                        '}',
                        'catch(err) {',
                          [ 'if (err instanceof p5_error && (err.v == label || err.v == \'\')) {',
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
                    '} while (_redo);',
                  ],
                '}',
            );
        }
        else {

            my $cond = Perlito5::JavaScript2::to_list([$self->{cond}], $level + 1);

            my $topic = $self->{topic};

            my $decl = '';
            my $v = $topic;
            if ($v->{decl}) {
                $decl = $v->{decl};
                $v    = $v->{var};
            }
            else {
                $decl = $v->{_decl} || 'global';
            }
            my $namespace = $v->{namespace} || $v->{_namespace} || $Perlito5::PKG_NAME;
            my $s;
            if ($decl eq 'my' || $decl eq 'state') {
                my $sig = $v->emit_javascript2( $level + 1 );
                push @str,
                    '(function(){ '
                        . "var $sig; "
                        . 'p5for_lex('
                            . "function (v) { $sig = v }, "
                            . "function () {\n"
                            . Perlito5::JavaScript2::tab($level + 2) .   (Perlito5::JavaScript2::LexicalBlock->new( block => $body ))->emit_javascript2($level + 2, $wantarray) . "\n"
                            . Perlito5::JavaScript2::tab($level + 1) . '}, '
                            .   $cond . ', '
                            . Perlito5::AST::Block::emit_javascript2_continue($self, $level, $wantarray) . ', '
                            . Perlito5::JavaScript2::escape_string($self->{label} || "")
                        . ') '
                    . '})()';
            }
            else {
                # use global variable or $_
                push @str,
                       'p5for(' 
                        . 'p5make_package(' . Perlito5::JavaScript2::escape_string($namespace ) . '), '
                        . '"v_' . $v->{name} . '", '
                        . 'function () {' . "\n"
                        . Perlito5::JavaScript2::tab($level + 2) .  (Perlito5::JavaScript2::LexicalBlock->new( block => $body ))->emit_javascript2($level + 2, $wantarray) . "\n"
                        . Perlito5::JavaScript2::tab($level + 1) . '}, '
                        .   $cond . ', '
                        . Perlito5::AST::Block::emit_javascript2_continue($self, $level, $wantarray) . ', '
                        . Perlito5::JavaScript2::escape_string($self->{label} || "")
                        . ')'
            }
        }

        if (@str > 1) {
            $level = $old_level;
            # create js scope for 'my' variables
            return Perlito5::JavaScript2::emit_wrap_javascript2($level, $wantarray, @str);
        }
        else {
            return join( "\n" . Perlito5::JavaScript2::tab($level), @str );
        }
    }
    sub emit_javascript2_get_decl { () }
    sub emit_javascript2_has_regex { () }
}

package Perlito5::AST::Sub;
{
    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;
        if (my $node = $self->maybe_rewrite_statevars()) {
            return $node->emit_javascript2(@_[1..$#_]);
        }
        my $prototype = defined($self->{sig}) 
                        ? Perlito5::JavaScript2::escape_string($self->{sig}) 
                        : 'null';

        my $sub_ref = Perlito5::JavaScript2::get_label();
        local $Perlito5::AST::Sub::SUB_REF = $sub_ref;
        local $Perlito5::JavaScript2::is_inside_subroutine = 1;

        # get list of captured variables, including inner blocks
        my @captured;
        for my $stmt (@{$self->{block}{stmts}}) {
            push @captured, $stmt->get_captures();
        }
        my %dont_capture = map { $_->{dont} ? ( $_->{dont} => 1 ) : () } @captured;
        my %capture = map { $_->{dont} ? ()
                          : $dont_capture{ $_->{_id} } ? ()
                          : ($_->{_decl} eq 'local' || $_->{_decl} eq 'global' || $_->{_decl} eq '') ? ()
                          : ( $_->{_id} => $_ )
                          } @captured;
        # warn Data::Dumper::Dumper(\@captured);
        # warn Data::Dumper::Dumper(\%dont_capture);
        # warn Data::Dumper::Dumper(\%capture);
        my @captures_ast  = map { $capture{$_} }
                            sort keys %capture;
        local @Perlito5::CAPTURES = @captures_ast;

        my $js_block = Perlito5::JavaScript2::LexicalBlock->new( block => $self->{block}{stmts} )->emit_javascript2_subroutine_body( $level + 2, 'runtime' );
        my @s = (
            "var $sub_ref;",
            "$sub_ref = function (List__, p5want) {",
                [ $js_block ],
            "};",
            "$sub_ref._prototype_ = $prototype;",
            "return $sub_ref",
        );

        if ( $self->{name} ) {
            my $s = Perlito5::JavaScript2::emit_wrap_javascript2($level, 'scalar', @s);
            return
                'p5typeglob_set('
                . Perlito5::JavaScript2::escape_string($self->{namespace} ) . ', '
                . Perlito5::JavaScript2::escape_string($self->{name} ) . ', '
                . $s
                . ')'
        }
        else {
            my $s = Perlito5::JavaScript2::emit_wrap_javascript2($level, 'scalar', @s);
            return $s;
        }
    }
    sub emit_javascript2_get_decl { () }
    sub emit_javascript2_has_regex { () }
}

1;

=begin

=head1 NAME

Perlito5::JavaScript2::Emit - Code generator for Perlito Perl5-in-JavaScript2

=head1 SYNOPSIS

    $program->emit_javascript2()  # generated Perl5 code

=head1 DESCRIPTION

This module generates JavaScript2 code for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
