use strict;

package Byte { }

package ByteList {
    import    => "java.util.ArrayList<Byte>",
} 

package ByteIterator {
    import    => "java.util.Iterator<Byte>",
}

# create java ArrayList<Byte>
my ByteList $bar = ByteList->new();

# copy perl array to java ArrayList<Byte>
my @perlArray = (100, 65, 88);
for (@perlArray) {
     my Byte $x = $_->to_byte();
     $bar->add($x);  
}

my ByteIterator $barIterator = $bar->iterator();

while($barIterator->hasNext()) {
    
    # get byte in perl varl
    my $byteWrappedInPerlVariable = $barIterator->next();
    say $byteWrappedInPerlVariable;
   
    # get byte in native java var
    my Byte $nativeByte = $byteWrappedInPerlVariable->to_byte();
}

