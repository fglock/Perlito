package main {
    use Data::Dumper;
    use strict;

    my $in = eval join( '', <> );
    print Dumper $in;

    my $rule = [ Ref => 'Perlito5::AST::Apply' ];
    TreeGrammar::scan( $rule, $in );

    $rule = [
        Lookup => 'name',
        [ Value => 'a' ]
    ];
    TreeGrammar::scan( $rule, $in );
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
        );
    }

    sub render {
        my ( $rule, $node ) = @_;
        if ( ref($rule) ) {

            # print "Rule ", Dumper $rule;
            # print Dumper \%dispatch;
            return $dispatch{ $rule->[0] }->( $rule, $node );
        }
        return;
    }

    sub scan {
        my ( $rule, $node ) = @_;
        render( $rule, $node );
        if ( ref($node) eq 'ARRAY' ) {
            scan( $rule, $_ ) for @$node;
        }
        elsif ( ref($node) ) {
            scan( $rule, $_ ) for values %$node;
        }
        return;
    }

    sub Ref {
        my ( $rule, $node ) = @_;
        if ( ref($node) eq $rule->[1] ) {
            print "match: Ref $node\n";
            return { pos => $node };
        }
        return;
    }

    sub Lookup {
        my ( $rule, $node ) = @_;
        return if !ref($node) || ref($node) eq 'ARRAY';
        if ( exists( $node->{ $rule->[1] } ) ) {
            print "match: Lookup $node\n";
            return render( $rule->[2], $node->{ $rule->[1] } ) if $rule->[2];
            return { pos => $node };
        }
        return;
    }

    sub Index  {
        my ( $rule, $node ) = @_;
        return if !ref($node) || ref($node) ne 'ARRAY';
        if ( exists( $node->[ $rule->[1] ] ) ) {
            print "match: Index $node\n";
            return render( $rule->[2], $node->[ $rule->[1] ] ) if $rule->[2];
            return { pos => $node };
        }
        return;
    }

    sub Value {
        my ( $rule, $node ) = @_;
        return if ref($node);
        if ( $node eq $rule->[1] ) {
            print "match: Value $node\n";
            return { pos => $node };
        }
        return;
    }

}
