package Ref::Util;

use strict;
use warnings;

use Exporter 'import';

our $VERSION     = '0.101';
our %EXPORT_TAGS = ( 'all' => [qw<
    is_ref
    is_scalarref
    is_arrayref
    is_hashref
    is_coderef
>] );
#     is_regexpref
#     is_globref
#     is_formatref
#     is_ioref
#     is_refref
# 
#     is_plain_ref
#     is_plain_scalarref
#     is_plain_arrayref
#     is_plain_hashref
#     is_plain_coderef
#     is_plain_globref
#     is_plain_formatref
#     is_plain_refref
# 
#     is_blessed_ref
#     is_blessed_scalarref
#     is_blessed_arrayref
#     is_blessed_hashref
#     is_blessed_coderef
#     is_blessed_globref
#     is_blessed_formatref
#     is_blessed_refref
# >] );

our @EXPORT_OK   = ( @{ $EXPORT_TAGS{'all'} } );

sub is_ref       { ref( $_[0] ) }
sub is_scalarref { ref( $_[0] ) eq 'SCALAR' }
sub is_arrayref  { ref( $_[0] ) eq 'ARRAY' }
sub is_hashref   { ref( $_[0] ) eq 'HASH' }
sub is_coderef   { ref( $_[0] ) eq 'CODE' }
 
1;

