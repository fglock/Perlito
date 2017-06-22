package warnings::register;

our $VERSION = '1.04';
require warnings;

sub import
{
    shift;
    my @categories = @_;

    my $package = (caller(0))[0];
    warnings::register_categories($package);

    warnings::register_categories($package . "::$_") for @categories;
}
1;
__END__

=pod

=head1 NAME

warnings::register - warnings import function

=head1 SYNOPSIS

    use warnings::register;

=head1 DESCRIPTION

Creates a warnings category with the same name as the current package.

See L<warnings> for more information on this module's usage.

=cut
