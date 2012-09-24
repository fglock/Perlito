# pX/Common/iterator_engine_p6rule_lib.pl - fglock
#
# native library for the experimental implementation of p6-regex parser
#
# see: iterator_engine_README


# XXX - rename the system grammar to 'Grammar'
# XXX - use method calls
#   <audreyt> because all grammars inherits from Grammar
#   <fglock> re inheritance - p5 rule calls will need to be written like methods 
#            for this to work

{
  package grammar1;

  use Text::Balanced; 

sub any { 
    return unless $_[0];
    return { 
        bool  => 1,
        match => { '.'=> substr($_[0],0,1) },
        tail  => substr($_[0],1),
        ( $_[2]->{capture} ? ( capture => [ substr($_[0],0,1) ] ) : () ),
    };
}
sub ws {
    return unless $_[0];
    return { 
        bool  => 1,
        match => { 'ws'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
        if $_[0] =~ /^(\s+)(.*)$/s;
    return;
};
sub newline {
    return unless $_[0];
    return { 
        bool  => 1,
        match => { 'newline'=> $1 },
        tail  => substr($_[0],1),
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
        if $_[0] =~ /^(\n)/s;
    return;
};
sub escaped_char {
    return unless $_[0];
    return { 
        bool  => 1,
        match => { 'escaped_char'=> $1 },
        tail  => substr($_[0],2),
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
        if $_[0] =~ /^\\(.)/s;
    return;
};
sub word { 
    return unless $_[0];
    return { 
        bool  => 1,
        match => { 'word'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
        if $_[0] =~ /^([_[:alnum:]]+)(.*)/s;
    return;
};

# ----- the following were included only for performance reasons,
# because they are too frequent and they are too slow using the basic 
# rule parser

sub code {
    return unless $_[0];
    ($extracted,$remainder) = Text::Balanced::extract_codeblock( $_[0] );
    return { 
        bool  => ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
        ( $_[2]->{capture} ? ( capture => [ $extracted ] ) : () ),
    };
}

sub literal {
    return unless $_[0];
    ($extracted,$remainder) = Text::Balanced::extract_delimited( $_[0], "'" );
    $extracted = substr( $extracted, 1, -1 );
    return { 
        bool  => ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
        ( $_[2]->{capture} ? ( capture => [ { literal => $extracted } ] ) : () ),
    };
}

sub ws_star {
    #return unless $_[0];
    return { 
        bool  => 1,
        match => { 'ws*'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
        if $_[0] =~ /^(\s*)(.*)$/s;
    return;
};

sub variable {
    #return unless $_[0];
    return { 
        bool  => 1,
        match => { 'ws*'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => $1 ) : () ),
    }
        if $_[0] =~ / ^  
            (   [ $ % @ % ]
                (?: 
                    (?:\:\:)? 
                    [_[:alnum:]]+ 
                )+
            )  
            (.*) $ /xs;
    return;
};

sub ident {
    #return unless $_[0];
    return { 
        bool  => 1,
        match => { 'ident'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ { ident => $1 } ] ) : () ),
    }
        if $_[0] =~ / ^  
            ( 
                (?: 
                    (?:\:\:)? 
                    [_[:alnum:]]+ 
                )+
            )  
            (.*) $ /xs;
    return;
};

}

1;

