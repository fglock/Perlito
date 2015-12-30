use strict;

package Byte { }

package ByteArray {
    import    => "java.util.ArrayList",
    java_type => "ArrayList<Byte>",
} 

package ByteIterator {
    import    => "java.util.Iterator",
    java_type => "Iterator<Byte>",
}

# list of raw java stuff
my ByteArray $bar = ByteArray->new();

my Byte $x = 100; # java native
my Byte $y = 65;
my Byte $z = 88;

##################################################
# TODO: copy from perl array to java arrayList
#
#   my @perlArray = (100, 200, 255);
#   map {
#       my Byte $x = $_->to_byte(); $bar->add($x);    
#   } @perlArray;
#
$bar->add($x); # <- this adds raw byte to list
$bar->add($y);
$bar->add($z);

##################################################
# TODO: provide iterator with @$bar
#   my ByteIterator $barIterator = @$bar;
my ByteIterator $barIterator = $bar->iterator();

##################################################
# TODO: automatic call to ->hasNext in boolean context
#
#   while (@$bar) { ... }
# 
#   This would obtain iterator with @$bar and then due
#   to boolean context would call hasNext().
#   That way we will be able to write idiomatic Perl
#   loops on native java Lists
while($barIterator->hasNext()) {
    
    # get byte in perl varl
    my $byteWrappedInPerlVariable = $barIterator->next();
    say $byteWrappedInPerlVariable;
   
    # get byte in native java var
    my Byte $nativeByte = $byteWrappedInPerlVariable->to_byte();
}

