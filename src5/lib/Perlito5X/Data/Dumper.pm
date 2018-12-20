package Data::Dumper;
use strict;

sub import {
    no strict 'refs';
    my $pkg     = shift;
    my $callpkg = caller(0);
    *{ $callpkg . "::Dumper" } = \&Dumper;
    return;
}

our $Indent;
our $Purity;
our $Pad;
our $Varname;
our $Useqq;
our $Terse;
our $Freezer;
our $Toaster;
our $Deepcopy;
our $Quotekeys;
our $Bless;
our $Maxdepth;
our $Pair;
our $Useperl;
our $Sortkeys;
our $Deparse;
our $Sparseseen;
our @post;

# module vars and their defaults
$Indent     = 2         unless defined $Indent;
$Purity     = 0         unless defined $Purity;
$Pad        = ""        unless defined $Pad;
$Varname    = "VAR"     unless defined $Varname;
$Useqq      = 0         unless defined $Useqq;
$Terse      = 0         unless defined $Terse;
$Freezer    = ""        unless defined $Freezer;
$Toaster    = ""        unless defined $Toaster;
$Deepcopy   = 0         unless defined $Deepcopy;
$Quotekeys  = 1         unless defined $Quotekeys;
$Bless      = "bless"   unless defined $Bless;
#$Expdepth   = 0         unless defined $Expdepth;
$Maxdepth   = 0         unless defined $Maxdepth;
$Pair       = ' => '    unless defined $Pair;
$Useperl    = 0         unless defined $Useperl;
$Sortkeys   = 0         unless defined $Sortkeys;
$Deparse    = 0         unless defined $Deparse;
$Sparseseen = 0         unless defined $Sparseseen;

#
# expects an arrayref of values to be dumped.
# can optionally pass an arrayref of names for the values.
# names must have leading $ sign stripped. begin the name with *
# to cause output of arrays and hashes rather than refs.
#
sub new {
  my($c, $v, $n) = @_;

  die "Usage:  PACKAGE->new(ARRAYREF, [ARRAYREF])"
    unless (defined($v) && (ref($v) eq 'ARRAY'));
  $n = [] unless (defined($n) && (ref($n) eq 'ARRAY'));

  my($s) = {
        level      => 0,           # current recursive depth
        indent     => $Indent,     # various styles of indenting
        pad        => $Pad,        # all lines prefixed by this string
        xpad       => "",          # padding-per-level
        apad       => "",          # added padding for hash keys n such
        sep        => "",          # list separator
        pair       => $Pair,    # hash key/value separator: defaults to ' => '
        seen       => {},          # local (nested) refs (id => [name, val])
        todump     => $v,          # values to dump []
        names      => $n,          # optional names for values []
        varname    => $Varname,    # prefix to use for tagging nameless ones
        purity     => $Purity,     # degree to which output is evalable
        useqq      => $Useqq,      # use "" for strings (backslashitis ensues)
        terse      => $Terse,      # avoid name output (where feasible)
        freezer    => $Freezer,    # name of Freezer method for objects
        toaster    => $Toaster,    # name of method to revive objects
        deepcopy   => $Deepcopy,   # dont cross-ref, except to stop recursion
        quotekeys  => $Quotekeys,  # quote hash keys
        'bless'    => $Bless,    # keyword to use for "bless"
        maxdepth   => $Maxdepth,   # depth beyond which we give up
        useperl    => $Useperl,    # use the pure Perl implementation
        sortkeys   => $Sortkeys,   # flag or filter for sorting hash keys
        deparse    => $Deparse,    # use B::Deparse for coderefs
        noseen     => $Sparseseen, # do not populate the seen hash unless necessary
       };

  if ($Indent > 0) {
    $s->{xpad} = "  ";
    $s->{sep} = "\n";
  }
  return bless($s, $c);
}

#
# reset the "seen" cache
#
sub Reset {
  my($s) = shift;
  $s->{seen} = {};
  return $s;
}

sub Indent {
  my($s, $v) = @_;
  if (defined($v)) {
    if ($v == 0) {
      $s->{xpad} = "";
      $s->{sep} = "";
    }
    else {
      $s->{xpad} = "  ";
      $s->{sep} = "\n";
    }
    $s->{indent} = $v;
    return $s;
  }
  else {
    return $s->{indent};
  }
}

sub Pair {
    my($s, $v) = @_;
    defined($v) ? (($s->{pair} = $v), return $s) : $s->{pair};
}

sub Pad {
  my($s, $v) = @_;
  defined($v) ? (($s->{pad} = $v), return $s) : $s->{pad};
}

sub Varname {
  my($s, $v) = @_;
  defined($v) ? (($s->{varname} = $v), return $s) : $s->{varname};
}

sub Purity {
  my($s, $v) = @_;
  defined($v) ? (($s->{purity} = $v), return $s) : $s->{purity};
}

sub Useqq {
  my($s, $v) = @_;
  defined($v) ? (($s->{useqq} = $v), return $s) : $s->{useqq};
}

sub Terse {
  my($s, $v) = @_;
  defined($v) ? (($s->{terse} = $v), return $s) : $s->{terse};
}

sub Freezer {
  my($s, $v) = @_;
  defined($v) ? (($s->{freezer} = $v), return $s) : $s->{freezer};
}

sub Toaster {
  my($s, $v) = @_;
  defined($v) ? (($s->{toaster} = $v), return $s) : $s->{toaster};
}

sub Deepcopy {
  my($s, $v) = @_;
  defined($v) ? (($s->{deepcopy} = $v), return $s) : $s->{deepcopy};
}

sub Quotekeys {
  my($s, $v) = @_;
  defined($v) ? (($s->{quotekeys} = $v), return $s) : $s->{quotekeys};
}

sub Bless {
  my($s, $v) = @_;
  defined($v) ? (($s->{'bless'} = $v), return $s) : $s->{'bless'};
}

sub Maxdepth {
  my($s, $v) = @_;
  defined($v) ? (($s->{'maxdepth'} = $v), return $s) : $s->{'maxdepth'};
}

sub Useperl {
  my($s, $v) = @_;
  defined($v) ? (($s->{'useperl'} = $v), return $s) : $s->{'useperl'};
}

sub Sortkeys {
  my($s, $v) = @_;
  defined($v) ? (($s->{'sortkeys'} = $v), return $s) : $s->{'sortkeys'};
}

sub Deparse {
  my($s, $v) = @_;
  defined($v) ? (($s->{'deparse'} = $v), return $s) : $s->{'deparse'};
}

sub Sparseseen {
  my($s, $v) = @_;
  defined($v) ? (($s->{'noseen'} = $v), return $s) : $s->{'noseen'};
}

sub Dump {
    my($s) = shift;
    my(@out, $val, $name);
    my($i) = 0;
    local(@post);

    $s = $s->new(@_) unless ref $s;

    # for $val (@{$s->{todump}}) {
    #   @post = ();
    #   $name = $s->{names}[$i++];
    #   $name = $s->_refine_name($name, $val, $i);
    #
    #   my $valstr;
    #   {
    #     local($s->{apad}) = $s->{apad};
    #     $s->{apad} .= ' ' x (length($name) + 3) if $s->{indent} >= 2 and !$s->{terse};
    #     $valstr = $s->_dump($val, $name);
    #   }
    #
    #   $valstr = "$name = " . $valstr . ';' if @post or !$s->{terse};
    #   my $out = $s->_compose_out($valstr, \@post);
    #
    #   push @out, $out;
    # }

    my $seen  = {};
    my $level = '    ';
    my @out;
    for my $i (0 .. $#{$s->{todump}}) {
        my $name = $s->{names}[$i];
        $name = $s->_refine_name($name, $val, $i+1);
        my $pos   = $name;
        push @out, "$pos = " . _dumper($s->{todump}[$i], $level, $seen, $pos) . ";\n";
    }
    return wantarray ? @out : join('', @out);
}

#
# non-OO style of earlier version
#
sub Dumper {
  return Data::Dumper->Dump([@_]);
}

sub _refine_name {
    my $s = shift;
    my ($name, $val, $i) = @_;
    if (defined $name) {
      if ($name =~ /^[*](.*)$/) {
        if (defined $val) {
            $name = (ref $val eq 'ARRAY') ? ( "\@" . $1 ) :
              (ref $val eq 'HASH')  ? ( "\%" . $1 ) :
              (ref $val eq 'CODE')  ? ( "\*" . $1 ) :
              ( "\$" . $1 ) ;
        }
        else {
          $name = "\$" . $1;
        }
      }
      elsif ($name !~ /^\$/) {
        $name = "\$" . $name;
      }
    }
    else { # no names provided
      $name = "\$" . $s->{varname} . $i;
    }
    return $name;
}

sub _dumper {
    my ($obj, $tab, $seen, $pos) = @_;

    return 'undef' if !defined $obj;

    my $ref = ref($obj);
    if (!$ref) {
        if (ref(\$obj) eq 'GLOB') {
            return "$obj";  # *main::x
        }
        return escape_string($obj);
    }

    my $as_string = "$obj";
    return $seen->{$as_string} if $seen->{$as_string};
    $seen->{$as_string} = $pos;
        
    my $tab1 = $tab . '    ';

    if ($ref eq 'ARRAY') {
        return '[]' unless @$obj;
        my @out;
        for my $i ( 0 .. $#$obj ) {
            my $here = $pos . '->[' . $i . ']';
            push @out, 
                $tab1,
                _dumper($obj->[$i], $tab1, $seen, $here), 
                ",\n";
        }
        return join('', "[\n", @out, $tab, ']');
    }
    elsif ($ref eq 'HASH') {
        return '{}' unless keys %$obj;
        my @out;
        for my $i ( sort keys %$obj ) {
            my $here = $pos . '->{' . $i . '}';
            push @out, 
                $tab1,
                "'$i' => ",
                _dumper($obj->{$i}, $tab1, $seen, $here), 
                ",\n";
        }
        return join('', "{\n", @out, $tab, '}');
    }
    elsif ($ref eq 'SCALAR' || $ref eq 'REF') {
        return "\\" . _dumper($$obj, $tab1, $seen, $pos);
    }
    elsif ($ref eq 'CODE') {
        # TODO
        return 'sub { "DUMMY" }';
    }
    elsif ($ref eq 'GLOB') {
        return '\\' . *$obj;    # \*main::x
    }

    local $@;
    
    my @out;
    my $res;
    $res = eval {
        # blessed ARRAY
        for my $i ( 0 .. $#$obj ) {
            my $here = $pos . '->[' . $i . ']';
            push @out, 
                $tab1,
                _dumper($obj->[$i], $tab1, $seen, $here), 
                ",\n";
        }
        join('', "bless([\n", @out, $tab, "], '$ref')");
    };
    return $res if $res;

    $res = eval {
        # blessed SCALAR
        "bless(\\" . _dumper($$obj, $tab1, $seen, $pos) . ", '$ref')";
    };
    return $res if $res;

    $res = eval {
        # blessed HASH
        for my $i ( sort keys %$obj ) {
            my $here = $pos . '->{' . $i . '}';
            push @out, 
                $tab1,
                "'$i' => ",
                _dumper($obj->{$i}, $tab1, $seen, $here), 
                ",\n";
        }
        join('', "bless({\n", @out, $tab, "}, '$ref')");
    };
    return $res if $res;

    return join('', "bless(\\" . escape_string("opaque data " . $obj) . ", '$ref')");
}

our %safe_char = (
    ' ' => 1,
    '!' => 1,
    '"' => 1,
    "'" => 1,
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
    "\n" => 1,
    map { $_ => 1 } (
            'A' .. 'Z',
            'a' .. 'z',
            '0' .. '9',
        ),
);

sub escape_string {
    my $s = shift;
    return "''" if $s eq '';
    my $v = 0+$s;
    if ($v) {
        return $v if $v eq $s && $s =~ /[0-9]/;
    }
    my @out = '"';
    for my $c ( split "", $s ) {
        if ( $c eq '\\' || $c eq '$' || $c eq '@' || $c eq '"' ) {
            push @out, '\\' . $c;
        }
        elsif ( exists( $safe_char{$c} ) ) {
            push @out, $c;
        }
        else {
            push @out, '\x{' . sprintf("%x", ord($c)) . '}';
        }
    }
    push @out, '"';
    return join('', @out);
}

sub _identity {
    # returns true if the 2 arguments point to the same reference
    "$_[0]" eq "$_[1]"
}

1;

