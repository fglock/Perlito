package main {
    use Data::Dumper;
    use strict;

    my $in = eval join( '', <> );
    print Dumper $in;

    my $rule = [ Ref => 'Perlito5::AST::Apply' ];
    my $result = TreeGrammar::scan( $rule, $in );
    print "result $result\n";

    $rule = [
        Lookup => 'name',
        [ Value => 'a' ]
    ];
    $result = TreeGrammar::scan( $rule, $in );
    print "result $result\n";

    $rule = [
        And => [
                   Lookup => 'name',
                   [ Value => 'a' ]
               ],
               [ Lookup => 'namespace' ],
               [ Action => sub { $_[0]->{HERE} = 1 } ],
    ];
    $result = TreeGrammar::scan( $rule, $in );
    print "result $result\n";
    print Dumper $in;

}

package TreeGrammar {
    use Data::Dumper;
    use strict;

    my %dispatch;
    INIT {
        %dispatch = (
            Ref    => \&Ref,       # Ref => 'Perlito5::AST::Apply'
            Lookup => \&Lookup,    # Lookup => 'namespace'
            Index  => \&Index,     # Index  => '0'
            Value  => \&Value,     # Value => '123'
            And    => \&And,
            Or     => \&Or,
            Action => \&Action,
        );
    }

    sub render {
        my ( $rule, $node ) = @_;
        return $dispatch{ $rule->[0] }->( $rule, $node );
    }

    sub scan {
        my ( $rule, $node ) = @_;
        render( $rule, $node ) if $rule;
        if ( ref($node) eq 'ARRAY' ) {
            scan( $rule, $_ ) for @$node;
        }
        elsif ( ref($node) ) {
            scan( $rule, $_ ) for values %$node;
        }
        return;
    }

    sub Action {
        my ( $rule, $node ) = @_;
        print "match: Action $node\n";
        $rule->[1]->( $node );
        return { pos => $node };
    }

    sub And {
        my ( $rule, $node ) = @_;
        my $result;
        print "match: And $node\n";
        for ( @$rule[ 1 .. $#$rule ] ) {
            $result = render( $_, $node ) or return;
        }
        print "fail\n";
        return $result;
    }

    sub Or {
        my ( $rule, $node ) = @_;
        my $result;
        for ( @$rule[ 1 .. $#$rule ] ) {
            $result = render( $_, $node ) and return $result;
        }
        return;
    }

    sub Ref {
        my ( $rule, $node ) = @_;
        return if ref($node) ne $rule->[1];
        print "match: Ref $node\n";
        return render( $rule->[2], $node ) if $rule->[2];
        print "true\n";
        return { pos => $node };
    }

    sub Lookup {
        my ( $rule, $node ) = @_;
        return
             if !ref($node)
          || ref($node) eq 'ARRAY'
          || !exists( $node->{ $rule->[1] } );
        print "match: Lookup $node\n";
        return render( $rule->[2], $node->{ $rule->[1] } ) if $rule->[2];
        return { pos => $node };
    }

    sub Index {
        my ( $rule, $node ) = @_;
        return
             if !ref($node)
          || ref($node) ne 'ARRAY'
          || !exists( $node->[ $rule->[1] ] );
        print "match: Index $node\n";
        return render( $rule->[2], $node->[ $rule->[1] ] ) if $rule->[2];
        return { pos => $node };
    }

    sub Value {
        my ( $rule, $node ) = @_;
        return if ref($node) || $node ne $rule->[1];
        print "match: Value $node\n";
        return render( $rule->[2], $node ) if $rule->[2];
        return { pos => $node };
    }

}
