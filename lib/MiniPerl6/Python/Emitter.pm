use v6;

class Python {
    sub tab($level) { "    " x $level }
}

class MiniPerl6::Python::LexicalBlock {
    has @.block;
    has $.needs_return;
    has $.top_level;

    my $ident;
    my @anon_block;
    sub push_stmt($block) { 
        push @anon_block, $block; 
    }
    sub get_ident {
        $ident = $ident + 1;
        return $ident;
    }

    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my @s;
        my @tmp;
        for @anon_block -> $stmt {
            @tmp.push( $stmt );
        }

        my $last_statement;
        if $.needs_return {
            $last_statement = pop @.block;
        }

        for @.block -> $stmt {
            @anon_block = [];
            my $s2 = $stmt.emit_python_indented($level);
            for @anon_block -> $stmt {
                # add anon subs
                @s.push( $stmt.emit_python_indented( $level ) );
            }
            push @s, $s2;
        }

        if $.needs_return && $last_statement {
            @anon_block = [];
            my $s2;
            if $last_statement.isa( 'If' ) {
                my $cond      = $last_statement.cond;
                my $body      = $last_statement.body;
                my $otherwise = $last_statement.otherwise;
                my $has_otherwise = $otherwise ?? 1 !! 0;
                $body      = MiniPerl6::Python::LexicalBlock.new( block => $body, needs_return => 1 );
                $otherwise = MiniPerl6::Python::LexicalBlock.new( block => $otherwise, needs_return => 1 );
                $s2 = Python::tab($level) ~ 'if ' ~ $cond.emit_python ~ ":\n" 
                    ~ $body.emit_python_indented( $level + 1 );
                if ( $has_otherwise ) {
                    $s2 = $s2 ~ "\n"
                        ~ Python::tab($level) ~ "else:\n" 
                            ~ $otherwise.emit_python_indented($level+1);
                }
            }
            elsif $last_statement.isa( 'Return' ) || $last_statement.isa( 'For' ) {
                $s2 = $last_statement.emit_python_indented( $level );
            }
            else {
                $s2 = Python::tab($level) ~ 'return ' ~ $last_statement.emit_python
            }

            for @anon_block -> $stmt {
                @s.push( $stmt.emit_python_indented( $level ) );
            }
            @s.push( $s2 ); 
        }

        @anon_block = @tmp;
        return @s.join( "\n" );
    }
}

class CompUnit {
    has $.name;
    has %.attributes;
    has %.methods;
    has @.body;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $block = MiniPerl6::Python::LexicalBlock.new( block => @.body );
        Python::tab($level) ~ 'class ' ~ $.name ~ ":\n" ~ 
            $block.emit_python_indented($level + 1) ~ "\n"
    }
}

class Val::Int {
    has $.int;
    method emit_python { $.int }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ $.int 
    }
}

class Val::Bit {
    has $.bit;
    method emit_python { $.bit }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ $.bit 
    }
}

class Val::Num {
    has $.num;
    method emit_python { $.num }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ $.num 
    }
}

class Val::Buf {
    has $.buf;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ '"""' ~ $.buf ~ '"""' 
    }
}

class Val::Undef {
    method emit_python { 'None' }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 'None' 
    }
}

class Val::Object {
    has $.class;
    has %.fields;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 
            $.class.perl ~ '(' ~ %.fields.perl ~ ')';
    }
}

class Lit::Array {
    has @.array1;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 
            '[' ~ (@.array1.>>emit_python).join(', ') ~ ']';
    }
}

class Lit::Hash {
    has @.hash1;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $fields = @.hash1;
        my @dict;
        for @$fields -> $field { 
            push @dict, (($field[0]).emit_python ~ ':' ~ ($field[1]).emit_python);
        }; 
        Python::tab($level) ~ 
            '{' ~ @dict.join(', ') ~ '}';
    }
}

class Lit::Code {
    # XXX
    1;
}

class Lit::Object {
    has $.class;
    has @.fields;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $fields = @.fields;
        my $str = '';
        for @$fields -> $field { 
            $str = $str ~ ($field[0]).emit_python ~ ' = ' ~ ($field[1]).emit_python ~ ',';
        }; 
        Python::tab($level) ~ 
            $.class ~ '( ' ~ $str ~ ' )';
    }
}

class Index {
    has $.obj;
    has $.index_exp;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 
            $.obj.emit_python ~ '[' ~ $.index_exp.emit_python ~ ']';
    }
}

class Lookup {
    has $.obj;
    has $.index_exp;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 
            $.obj.emit_python ~ '[' ~ $.index_exp.emit_python ~ ']';
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.name;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        # Normalize the sigil here into $
        # $x    => $x
        # @x    => $List_x
        # %x    => $Hash_x
        # &x    => $Code_x
        my $table = {
            '$' => '',
            '@' => 'List_',
            '%' => 'Hash_',
            '&' => 'Code_',
        };
        return Python::tab($level) ~ (
               ( $.twigil eq '.' )
            ?? ( 'self.' ~ $.name )
            !!  (    ( $.name eq '/' )
                ??   ( $table{$.sigil} ~ 'MATCH' )
                !!   ( $table{$.sigil} ~ $.name )
                )
            )
    };
    method name {
        $.name
    };
}

class Bind {
    has $.parameters;
    has $.arguments;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        if $.parameters.isa( 'Lit::Array' ) {
            
            #  [$a, [$b, $c]] = [1, [2, 3]]
            
            my $a = $.parameters.array;
            #my $b = $.arguments.array;
            my $str = "if True:\n# {\n ";
            my $i = 0;
            for @$a -> $var { 
                my $bind = Bind.new( 
                    parameters => $var, 
                    # arguments => ($b[$i]) );
                    arguments  => Index.new(
                        obj    => $.arguments,
                        index  => Val::Int.new( int => $i )
                    )
                );
                $str = $str ~ ' ' ~ $bind.emit_python ~ "\n";
                $i = $i + 1;
            };
            return $str ~ $.parameters.emit_python ~ "\n# }\n";
        };
        if $.parameters.isa( 'Lit::Hash' ) {

            #  {:$a, :$b} = { a => 1, b => [2, 3]}

            my $a = $.parameters.hash;
            my $b = $.arguments.hash;
            my $str = "if 1:\n#{\n";
            my $i = 0;
            my $arg;
            for @$a -> $var {

                $arg = Val::Undef.new();
                for @$b -> $var2 {
                    #say "COMPARE ", ($var2[0]).buf, ' eq ', ($var[0]).buf;
                    if ($var2[0]).buf eq ($var[0]).buf {
                        $arg = $var2[1];
                    }
                };

                my $bind = Bind.new( parameters => $var[1], arguments => $arg );
                $str = $str ~ ' ' ~ $bind.emit_python ~ "\n";
                $i = $i + 1;
            };
            return $str ~ $.parameters.emit_python ~ "\n# }\n";
        };

        if $.parameters.isa( 'Lit::Object' ) {

            #  Obj.new(:$a, :$b) = $obj

            my $class = $.parameters.class;
            my $a     = $.parameters.fields;
            my $b     = $.arguments;
            my $str   = 'do { ';
            my $str   = "if 1:\n# {\n";
            my $i     = 0;
            my $arg;
            for @$a -> $var {
                my $bind = Bind.new( 
                    parameters => $var[1], 
                    arguments  => Call.new( invocant => $b, method => ($var[0]).buf, arguments => [ ], hyper => 0 )
                );
                $str = $str ~ ' ' ~ $bind.emit_python ~ "\n";
                $i = $i + 1;
            };
            return $str ~ $.parameters.emit_python ~ "\n# }\n";
        };
    
        Python::tab($level) ~ 
            $.parameters.emit_python ~ ' = ' ~ $.arguments.emit;
    }
}

class Proto {
    has $.name;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 
            $.name        
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    #has $.hyper;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $invocant = $.invocant.emit;
        if     ($.method eq 'perl')
            || ($.method eq 'yaml')
            || ($.method eq 'say' )
            || ($.method eq 'join')
            || ($.method eq 'chars')
            || ($.method eq 'isa')
        { 
            if ($.hyper) {
            	return "map(lambda: Main." ~ $.method ~ "( self, " ~ (@.arguments.>>emit_python).join(', ') ~ ') , ' ~ $invocant ~ ")\n";
            }
            else {
                return "Main." ~ $.method ~ '(' ~ $invocant ~ ', ' ~ (@.arguments.>>emit_python).join(', ') ~ ')';
            }
        };

        my $meth = $.method;
        if  $meth eq 'postcircumfix:<( )>'  {
             $meth = '';  
        };
        
        my $call = '.' ~ $meth ~ '(' ~ (@.arguments.>>emit_python).join(', ') ~ ')';
        if ($.hyper) {
            Python::tab($level) ~ 
                '[ map { $_' ~ $call ~ ' } @{ ' ~ $invocant ~ ' } ]';
        }
        else {
            Python::tab($level) ~ 
                $invocant ~ $call;
        };

    }
}

class Apply {
    has $.code;
    has @.arguments;
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 
            $self.emit
    }
    method emit_python {
        
        my $code = $.code;

        if $code.isa( 'Str' ) { }
        else {
            return '(' ~ $.code.emit_python ~ ').(' ~ (@.arguments.>>emit_python).join(', ') ~ ')';
        };

        if $code eq 'self'       { return 'self' };

        if $code eq 'say'        { return 'miniperl6.python.runtime.mp6_say('   ~ (@.arguments.>>emit_python).join(', ') ~ ')' } 
        if $code eq 'print'      { return 'miniperl6.python.runtime.mp6_print(' ~ (@.arguments.>>emit_python).join(', ') ~ ')' }
        if $code eq 'warn'       { return 'miniperl6.python.runtime.mp6_warn('  ~ (@.arguments.>>emit_python).join(', ') ~ ')' }

        if $code eq 'array'      { return '[' ~ (@.arguments.>>emit_python).join(' ')    ~ ']' };

        if $code eq 'prefix:<~>' { return '("" . ' ~ (@.arguments.>>emit_python).join(' ') ~ ')' };
        if $code eq 'prefix:<!>' { return '('  ~ (@.arguments.>>emit_python).join(' ')    ~ ' ? 0 : 1)' };
        if $code eq 'prefix:<?>' { return '('  ~ (@.arguments.>>emit_python).join(' ')    ~ ' ? 1 : 0)' };

        if $code eq 'prefix:<$>' { return '${' ~ (@.arguments.>>emit_python).join(' ')    ~ '}' };
        if $code eq 'prefix:<@>' { return '@{' ~ (@.arguments.>>emit_python).join(' ')    ~ '}' };
        if $code eq 'prefix:<%>' { return '%{' ~ (@.arguments.>>emit_python).join(' ')    ~ '}' };

        if $code eq 'infix:<~>'  { return '(str('  ~ (@.arguments.>>emit_python).join(') + str(')  ~ '))' };
        if $code eq 'infix:<+>'  { return '(float('  ~ (@.arguments.>>emit_python).join(') + float(')  ~ '))' };
        if $code eq 'infix:<->'  { return '('  ~ (@.arguments.>>emit_python).join(' - ')  ~ ')' };
        if $code eq 'infix:<*>'  { return '('  ~ (@.arguments.>>emit_python).join(' * ')  ~ ')' };
        if $code eq 'infix:</>'  { return '('  ~ (@.arguments.>>emit_python).join(' / ')  ~ ')' };
        
        if $code eq 'infix:<&&>' { return '('  ~ (@.arguments.>>emit_python).join(' and ') ~ ')' };
        if $code eq 'infix:<||>' { return '('  ~ (@.arguments.>>emit_python).join(' or ') ~ ')' };
        if $code eq 'infix:<eq>' { return '(str('  ~ (@.arguments.>>emit_python).join(') == str(')  ~ '))' };
        if $code eq 'infix:<ne>' { return '(str('  ~ (@.arguments.>>emit_python).join(') != str(')  ~ '))' };
 
        if $code eq 'infix:<==>' { return '('  ~ (@.arguments.>>emit_python).join(' == ') ~ ')' };
        if $code eq 'infix:<!=>' { return '('  ~ (@.arguments.>>emit_python).join(' != ') ~ ')' };
        if $code eq 'infix:<<>'  { return '('  ~ (@.arguments.>>emit_python).join(' < ')  ~ ')' };
        if $code eq 'infix:<>>'  { return '('  ~ (@.arguments.>>emit_python).join(' > ')  ~ ')' };

        if $code eq 'ternary:<?? !!>' { 
            my $ast = 
                Do.new( 
                    block => [
                        If.new(
                            cond      => (@.arguments[0]),
                            body      => [ @.arguments[1] ],
                            otherwise => [ @.arguments[2] ],
                        ),
                    ]
                );
            return $ast.emit_python;
        }
        
        $.code ~ '(' ~ (@.arguments.>>emit_python).join(', ') ~ ')';
    }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ $self.emit_python 
    }
}

class Return {
    has $.result;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 
            'return ' ~ $.result.emit
    }
}

class If {
    has $.cond;
    has @.body;
    has @.otherwise;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $has_body = @.body ?? 1 !! 0;
        my $has_otherwise = @.otherwise ?? 1 !! 0;
        my $body_block = MiniPerl6::Python::LexicalBlock.new( block => @.body );
        my $otherwise_block = MiniPerl6::Python::LexicalBlock.new( block => @.otherwise );
        my $s = Python::tab($level) ~   'if ' ~ $.cond.emit_python ~ ":\n" 
            ~ $body_block.emit_python_indented( $level + 1 );
        if ( $has_otherwise ) {
            $s = $s ~ "\n"
                ~ Python::tab($level) ~ "else:\n" 
                    ~ $otherwise_block.emit_python_indented($level+1);
        }
        return $s;
    }
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    method emit_python {
        my $cond = $.cond;
        if   $cond.isa( 'Var' ) 
          && $cond.sigil eq '@' 
        {
            $cond = Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
        };
        'do { for my ' ~ $.topic.emit_python ~ ' ( ' ~ $cond.emit_python ~ ' ) { ' ~ (@.body.>>emit_python).join(';') ~ ' } }';
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $decl = $.decl;
        my $name = $.var.name;
        Python::tab($level)
            ~ ( ( $decl eq 'has' )
            ?? ( 'sub ' ~ $name ~ ' { ' ~
                '@_ == 1 ' ~
                    '? ( $_[0]->{' ~ $name ~ '} ) ' ~
                    ': ( $_[0]->{' ~ $name ~ '} = $_[1] ) ' ~
                '}' )
            !! ( $.type 
                ?? $.type ~ ' ' ~ $.var.emit
                !! $.var.emit_python ) );
    }
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method emit_python {
        ' print \'Signature - TODO\'; die \'Signature - TODO\'; '
    };
    method invocant {
        $.invocant
    };
    method positional {
        $.positional
    }
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method emit_python {
        my $sig = $.sig;
        my $invocant = $sig.invocant; 
        my $pos = $sig.positional;
        my $str = 'my $List__ = \@_; ';   # no strict "vars"; ';

        my $pos = $sig.positional;
        for @$pos -> $field { 
            $str = $str ~ 'my ' ~ $field.emit_python ~ '; ';
        }

        my $bind = Bind.new( 
            parameters => Lit::Array.new( array => $sig.positional ), 
            arguments  => Var.new( sigil => '@', twigil => '', name => '_' )
        );
        $str = $str ~ $bind.emit_python ~ '; ';

        'sub ' ~ $.name ~ ' { ' ~ 
          'my ' ~ $invocant.emit_python ~ ' = shift; ' ~
          $str ~
          (@.block.>>emit_python).join('; ') ~ 
        ' }'
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $sig = $.sig;
        my $pos = $sig.positional;
        my @args;
        for @$pos -> $field { 
            @args.push( $field.emit_python );
        };
        my $block = MiniPerl6::Python::LexicalBlock.new( 
                block => @.block,
                needs_return => 1 );
        Python::tab($level) ~ 'def ' ~ $.name ~ "(" ~ @args.join(", ") ~ "):\n" 
            ~ $block.emit_python_indented($level + 1) 
    }
}

class Do {
    has @.block;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $label = "_anon_" ~ MiniPerl6::Python::LexicalBlock::get_ident;
        # generate an anonymous sub in the current block
        MiniPerl6::Python::LexicalBlock::push_stmt( 
                Sub.new( 
                    name  => $label, 
                    block => @.block,
                    sig   => Sig.new( invocant => undef, positional => [], named => {} ) 
                )
            );
        # call the anonymous sub
        return Python::tab($level) ~ $label ~ "()";
    }
}

class Use {
    has $.mod;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) 
            ~ 'from ' ~ $.mod ~ 'import *'
    }
}

=begin

=head1 NAME

MiniPerl6::Python::Emit - Code generator for MiniPerl6-in-Python

=head1 SYNOPSIS

    $program.emit_python  # generated Python code

=head1 DESCRIPTION

This module generates Python code for the MiniPerl6 compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2010 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

