use Test::More tests => 4;

{
    package Mo::Standard;
    use Mo qw(default);
    has name => 'Ayrton Senna';

    has sport => { F1 => 'F1' }, lazy => 1;
    has teams => [ 'McLaren', 'Lotus', 'Toleman', 'Williams' ], lazy => 0;
    has no_teams => sub { scalar @{ shift->teams } }, lazy => 1;
    has championships => sub { [ 1988, 1990, 1991 ] }, lazy => 0;
}
{
    package Moose::Alike;
    use Mo qw(default nonlazy);
    has name => 'Ayrton Senna';

    has sport => { F1 => 'F1' }, lazy => 1;
    has teams => [ 'McLaren', 'Lotus', 'Toleman', 'Williams' ], lazy => 0;
}
{
    package Moose::Alike::Build;
    use Mo qw(build default nonlazy);
    has name => 'Ayrton Senna';

    has sport => { F1 => 'F1' }, lazy => 1;
    has teams => [ 'McLaren', 'Lotus', 'Toleman', 'Williams' ], lazy => 0;

    sub BUILD {
        my $self = shift;
        $self->{name_at_build}  = $self->{name};
        $self->{sport_at_build} = $self->{sport};
    }
}
{
    package Moose::Alike::Builder;
    use Mo qw(build builder nonlazy);

    has name  => (builder => '_name');
    has sport => (builder => '_sport', lazy => 1);

    sub _name  { 'Ayrton Senna' }
    sub _sport { { F1 => 'F1' } }
    sub BUILD {
        my $self = shift;
        $self->{name_at_build}  = $self->{name};
        $self->{sport_at_build} = $self->{sport};
    }
}

package main;

subtest "Standard behavior" => sub {
    my $standard = Mo::Standard->new;
    is( $standard->{name}, undef,          'attributes are lazy by default' );
    is( $standard->name,   'Ayrton Senna', 'initialized lazily' );
    is( $standard->{sport}, undef, 'attr is lazy when explicitly asked' );
    is_deeply( $standard->sport, { F1 => 'F1' }, 'initialized lazily' );
    is_deeply(
        $standard->{teams},
        [ 'McLaren', 'Lotus', 'Toleman', 'Williams' ],
        'attr is non lazy when explicitly asked'
    );
    is_deeply(
        $standard->teams,
        [ 'McLaren', 'Lotus', 'Toleman', 'Williams' ],
        'accessor on non lazy'
    );
    is( $standard->no_teams, 4, 'non lazy attribute' );
    is_deeply(
        $standard->{championships},
        [ 1988, 1990, 1991 ],
        'not lazy when explicitly asked'
    );
};

subtest "Moose-like behavior (importing nonlazy)" => sub {
    my $moosey = Moose::Alike->new;
    is( $moosey->{name}, 'Ayrton Senna',
        'importing nonlazy makes attributes default to non lazy' );
    is( $moosey->name,    'Ayrton Senna', 'attribute accessor' );
    is( $moosey->{sport}, undef,          'lazy when explicitly asked' );
    is_deeply( $moosey->sport, { F1 => 'F1' }, 'lazy default' );
    is_deeply(
        $moosey->{teams},
        [ 'McLaren', 'Lotus', 'Toleman', 'Williams' ],
        'not lazy when explicitly asked'
    );
    is_deeply(
        $moosey->teams,
        [ 'McLaren', 'Lotus', 'Toleman', 'Williams' ],
        'attribute accessor'
    );
};

subtest "Moose-like behavior (importing nonlazy and build)" => sub {
    my $moosey_build = Moose::Alike::Build->new;
    is( $moosey_build->{name}, 'Ayrton Senna',
        'importing nonlazy makes attributes default to non lazy' );
    is $moosey_build->{name_at_build}, 'Ayrton Senna',
      'defaults set before BUILD is called';
    is( $moosey_build->name,    'Ayrton Senna', 'attribute accessor' );
    is( $moosey_build->{sport}, undef,          'lazy when explicitly asked' );
    is( $moosey_build->{sport_at_build},
        undef, 'lazy attr not affected during build' );
    is_deeply( $moosey_build->sport, { F1 => 'F1' }, 'lazy default' );
    is_deeply(
        $moosey_build->{teams},
        [ 'McLaren', 'Lotus', 'Toleman', 'Williams' ],
        'not lazy when explicitly asked'
    );
    is_deeply(
        $moosey_build->teams,
        [ 'McLaren', 'Lotus', 'Toleman', 'Williams' ],
        'attribute accessor'
    );
};

subtest "Moose-like behavior (importing builder, nonlazy and build)" => sub {
    my $moosey_builder = Moose::Alike::Builder->new;
    is( $moosey_builder->{name},
        'Ayrton Senna', 'importing nonlazy makes builder default to non lazy' );
    is $moosey_builder->{name_at_build}, 'Ayrton Senna',
      'builder runs before BUILD is called';
    is( $moosey_builder->name, 'Ayrton Senna', 'attribute accessor' );
    is( $moosey_builder->{sport}, undef, 'lazy when explicitly asked' );
    is( $moosey_builder->{sport_at_build},
        undef, 'lazy builder not affected during build' );
};
