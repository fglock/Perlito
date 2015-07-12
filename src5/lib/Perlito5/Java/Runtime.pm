use v5;

package Perlito5::Java::Runtime;

sub emit_java {
    my ($self, %args) = @_;
    my %java_classes = %{ $args{java_classes} // {} };

    my %native_to_perl = (
        int    => 'pInt',
        double => 'pNum',
        String => 'pString',
    );
    for (values %java_classes) {
        $native_to_perl{$_->{accessor}} = "p" . $_->{accessor};
    }

    return <<'EOT'
/*
    lib/Perlito5/Java/Runtime.pm
*/

import java.util.ArrayList;
import java.util.HashMap;
EOT
        # import the Java classes
        # that were declared with
        #
        #   package MyJavaClass { Java }
        #
    . join('', ( map {
                    my $class = $_;
                    "import $class->{import};\n"
            }
            values %java_classes
      ))
    . <<'EOT'
class pCx {
    public static final int VOID   = 0;
    public static final int SCALAR = 1;
    public static final int LIST   = 2;
}
class pCORE {
    public static final pObject print(int want, pObject filehandle, pArray List__) {
        // TODO - write to filehandle
        for (int i = 0; i < List__.to_int(); i++) {
            System.out.print(List__.aget(i).to_string());
        }
        return new pInt(1);
    }
    public static final pObject say(int want, pObject filehandle, pArray List__) {
        // TODO - write to filehandle
        for (int i = 0; i < List__.to_int(); i++) {
            System.out.print(List__.aget(i).to_string());
        }
        System.out.println("");
        return new pInt(1);
    }
    public static final pObject die(int want, pArray List__) {
        for (int i = 0; i < List__.to_int(); i++) {
            System.err.print(List__.aget(i).to_string());
        }
        System.err.println("");
        System.exit(1);     // TODO
        return new pUndef();
    }
    public static final pObject die(String s) {
        // die() shortcut
        return pCORE.die(pCx.VOID, new pArray(new pString(s)));
    }
    public static final pObject ref(int want, pArray List__) {
        return List__.aget(0).ref();
    }
    public static final pObject scalar(int want, pArray List__) {
        if (List__.to_int() == 0) {
            return new pUndef();
        }
        return List__.aget(-1).scalar();
    }
}
class pObject {
    // extends java object ???
    public static final pString REF = new pString("");

    public pObject() {
    }
EOT
        # add interfaces to Java classes
        # that were declared with
        #
        #   package MyJavaClass { Java }
        #
    . join('', ( map {
                    my $class = $_;
                    my $java_class_name = $class->{accessor};
                    "    public ${java_class_name} to_${java_class_name}() {\n"
                  . "        pCORE.die(\"error .to_${java_class_name}!\");\n"
                  . "        return null;\n"
                  . "    }\n"
            }
            values %java_classes
      ))
    . <<'EOT'
    public String to_string() {
        return this.toString();
    }
    public int to_int() {
        pCORE.die("error .to_int!");
        return 0;
    }
    public pObject end_of_array_index() {
        return pCORE.die("error .to_int!");
    }
    public double to_num() {
        pCORE.die("error .to_num!");
        return 0.0;
    }
    public boolean to_bool() {
        pCORE.die("error .to_bool!");
        return true;
    }
    public pObject array_deref() {
        pCORE.die("error .array_deref!");
        return new pArray();
    }
    public pObject hash_deref() {
        pCORE.die("error .hash_deref!");
        return new pHash();
    }
    public pObject add(pObject s) {
        return this.to_num_or_int().add(s);
    }
    public boolean is_int() {
        return false;
    }
    public boolean is_num() {
        return false;
    }
    public boolean is_string() {
        return false;
    }
    public boolean is_bool() {
        return false;
    }
    public boolean is_hash() {
        return false;
    }
    public boolean is_array() {
        return false;
    }
    public pObject to_num_or_int() {
        return new pInt(0);
    }
    public pObject ref() {
        return REF;
    }
    public pObject scalar() {
        return this;
    }
    public pObject get() {
        pCORE.die("error .get!");
        return this;
    }
    public pObject aget(int i) {
        pCORE.die("error .get!");
        return this;
    }
}
class pReference extends pObject {
    public static final pString REF = new pString("REF");

    public String to_string() {
        return this.ref().to_string() + "(0x" + this.hashCode() + ")";
    }
    public pObject ref() {
        return REF;
    }
}
class pClosure extends pReference {
    public pObject env;
    public static final pString REF = new pString("CODE");

    public pClosure(pObject env) {
        this.env = env;
    }
    public pObject apply(int want, pObject... args) {
        pCORE.die("error!");
        return new pInt(0);
    }
    public pObject ref() {
        return REF;
    }
}
class pScalarRef extends pReference {
    private pScalar o;
    public static final pString REF = new pString("SCALAR");

    public pScalarRef(pScalar o) {
        this.o = o;
    }
    public pObject get() {
        return this.o;
    }
    public pObject ref() {
        return REF;
    }
}
class pArrayRef extends pReference {
    private pArray o;
    public static final pString REF = new pString("ARRAY");

    public pArrayRef(pArray o) {
        this.o = o;
    }
    public pObject get() {
        return this.o;
    }
    public pObject array_deref() {
        return this.o;
    }
    public pObject ref() {
        return REF;
    }
}
class pHashRef extends pReference {
    private pHash o;
    public static final pString REF = new pString("HASH");

    public pHashRef(pHash o) {
        this.o = o;
    }
    public pObject get() {
        return this.o;
    }
    public pHash hash_deref() {
        return this.o;
    }
    public pObject set(pHash o) {
        this.o = o;
        return this;
    }
    public pObject ref() {
        return REF;
    }
}
class pScalar extends pObject {
    private pObject o;

    // Note: several versions of pScalar()
    public pScalar() {
    }
    public pScalar(pObject o) {
        this.o = o;
    }
    public pScalar(pScalar o) {
        this.o = o.get();
    }
    public pScalar(pArray o) {
        // $a = @x
        this.o = o.scalar();
    }
    public pScalar(pHash o) {
        // $a = %x
        this.o = o.scalar();
    }

    public pObject get() {
        return this.o;
    }
    public pObject get_array() {
        // $x->[1]
        if (this.o == null) {
            this.o = new pArray();
            return this.o;
        }
        else if (this.o.is_array()) {
            return this.o;
        }
        return pCORE.die("Not an ARRAY reference");
    }
    public pObject get_hash() {
        // $x->{a}
        if (this.o == null) {
            this.o = new pHash();
        }
        else if (this.o.is_hash()) {
            return this.o;
        }
        return pCORE.die("Not a HASH reference");
    }
    public pObject array_deref() {
        // @$x doesn't autovivify
        if (this.o == null) {
            return new pArray();
        }
        else if (this.o.is_array()) {
            return this.o;
        }
        return pCORE.die("Not an ARRAY reference");
    }
    public pObject hash_deref() {
        // %$x doesn't autovivify
        if (this.o == null) {
            return new pHash();
        }
        else if (this.o.is_hash()) {
            return this.o;
        }
        return pCORE.die("Not a HASH reference");
    }

    // Note: several versions of set()
    public pObject set(pObject o) {
        this.o = o;
        return this;
    }
    public pObject set(pScalar o) {
        this.o = o.get();
        return this;
    }
    public pObject set(pArray o) {
        // $a = @x
        this.o = o.scalar();
        return this;
    }
    public pObject set(pHash o) {
        // $a = %x
        this.o = o.scalar();
        return this;
    }
EOT
    . ( join('', map {
            my $native = $_;
            my $perl   = $native_to_perl{$native};
"    public pObject set($native s) {
        this.o = new $perl(s);
        return this;
    }
"
            }
            keys %native_to_perl ))

    . <<'EOT'
    public String to_string() {
        if (this.o == null) {
            return "";
        }
        return this.o.to_string();
    }
    public int to_int() {
        if (this.o == null) {
            return 0;
        }
        return this.o.to_int();
    }
    public double to_num() {
        if (this.o == null) {
            return 0.0;
        }
        return this.o.to_num();
    }
    public boolean to_bool() {
        if (this.o == null) {
            return false;
        }
        return this.o.to_bool();
    }
    public pObject add(pObject s) {
        if (this.o == null) {
            this.o = new pInt(0);
        }
        return this.o.add(s);
    }
    public boolean is_int() {
        if (this.o == null) {
            return false;
        }
        return this.o.is_int();
    }
    public boolean is_num() {
        if (this.o == null) {
            return false;
        }
        return this.o.is_num();
    }
    public boolean is_string() {
        if (this.o == null) {
            return false;
        }
        return this.o.is_string();
    }
    public boolean is_bool() {
        if (this.o == null) {
            return false;
        }
        return this.o.is_bool();
    }
    public pObject to_num_or_int() {
        if (this.o == null) {
            this.o = new pUndef();
        }
        return this.o.to_num_or_int();
    }
    public pObject scalar() {
        return this.o;
    }
EOT
        # add "unbox" accessors to Java classes
        # that were declared with
        #
        #   package MyJavaClass { Java }
        #
    . join('', ( map {
                    my $class = $_;
                    my $java_class_name = $class->{accessor};
"    public ${java_class_name} to_${java_class_name}() {
        return this.o.to_${java_class_name}();
    }
"
            }
            values %java_classes
      ))

    . <<'EOT'
}
class pArray extends pObject {
    private ArrayList<pObject> a;
    public pArray() {
        this.a = new ArrayList<pObject>();
    }
    public pArray(pObject... args) {
        ArrayList<pObject> aa = new ArrayList<pObject>();
        for (pObject s : args) {
            if (s.is_array()) {
                // @x = ( @x, @y );
                for (int i = 0; i < s.to_int(); i++) {
                    aa.add(s.aget(i));
                }
            }
            else {
                aa.add(s);
            }
        }
        this.a = aa;
    }

    public pObject aget(pObject i) {
        int pos  = i.to_int();
        if (pos < 0) {
            pos = this.a.size() + pos;
        }
        if (pos < 0 || pos > this.a.size()) {
            return new pUndef();
        }
        return this.a.get(pos);
    }
    public pObject aget(int i) {
        int pos  = i;
        if (pos < 0) {
            pos = this.a.size() + pos;
        }
        if (pos < 0 || pos > this.a.size()) {
            return new pUndef();
        }
        return this.a.get(pos);
    }

    public pObject get_array(pObject i) {
        pObject o = this.aget(i);
        if (o == null) {
            o = new pArray();
            this.aset(i, o);
            return o;
        }
        else if (o.is_array()) {
            return o;
        }
        return pCORE.die(pCx.VOID, new pArray(new pString("Not an ARRAY reference")));
    }
    public pObject get_hash(pObject i) {
        pObject o = this.aget(i);
        if (o == null) {
            o = new pHash();
            this.aset(i, o);
            return o;
        }
        else if (o.is_hash()) {
            return o;
        }
        return pCORE.die(pCx.VOID, new pArray(new pString("Not a HASH reference")));
    }

    // Note: multiple versions of set()
    public pObject aset(pObject i, pObject v) {
        int size = this.a.size();
        int pos  = i.to_int();
        if (pos < 0) {
            pos = size + pos;
        }
        while (size < pos) {
            this.a.add( new pUndef() );
            size++;
        }
        this.a.add(pos, v.scalar());
        return v;
    }
    public pObject aset(int i, pObject v) {
        int size = this.a.size();
        int pos  = i;
        if (pos < 0) {
            pos = size + pos;
        }
        while (size < pos) {
            this.a.add( new pUndef() );
            size++;
        }
        this.a.add(pos, v.scalar());
        return v;
    }
    public pObject aset(pObject i, pScalar v) {
        int size = this.a.size();
        int pos  = i.to_int();
        if (pos < 0) {
            pos = size + pos;
        }
        while (size < pos) {
            this.a.add( new pUndef() );
            size++;
        }
        this.a.add(pos, v.get());
        return v;
    }
EOT
    . ( join('', map {
            my $native = $_;
            my $perl   = $native_to_perl{$native};
"    public pObject aset(pObject i, $native s) {
        return this.aset(i, new $perl(s));
    }
    public pObject aset(int i, $native s) {
        return this.aset(i, new $perl(s));
    }
"
            }
            keys %native_to_perl ))

    . <<'EOT'

    public String to_string() {
        // TODO
        return "" + this.hashCode();
    }
    public int to_int() {
        return this.a.size();
    }
    public pObject end_of_array_index() {
        return new pInt(this.a.size() - 1);
    }
    public double to_num() {
        return 0.0 + this.to_int();
    }
    public boolean to_bool() {
        return (this.a.size() > 0);
    }
    public pObject add(pObject s) {
        return this.to_num_or_int().add(s);
    }
    public boolean is_int() {
        return false;
    }
    public boolean is_num() {
        return false;
    }
    public boolean is_string() {
        return false;
    }
    public boolean is_bool() {
        return false;
    }
    public boolean is_array() {
        return true;
    }
    public pObject to_num_or_int() {
        return new pInt(this.to_int());
    }
    public pObject scalar() {
        return this.to_num_or_int();
    }
}
class pHash extends pObject {
    private HashMap<String, pObject> h;
    public pHash() {
        this.h = new HashMap<String, pObject>();
    }
    public pObject hget(pObject i) {
        pObject o = this.h.get(i.to_string());
        if (o == null) {
            return new pUndef();
        }
        return o;
    }
    public pObject get_array(pObject i) {
        pObject o = this.hget(i);
        if (o == null) {
            o = new pArray();
            this.hset(i, o);
            return o;
        }
        else if (o.is_array()) {
            return o;
        }
        return pCORE.die(pCx.VOID, new pArray(new pString("Not an ARRAY reference")));
    }
    public pObject get_hash(pObject i) {
        pObject o = this.hget(i);
        if (o == null) {
            o = new pHash();
            this.hset(i, o);
            return o;
        }
        else if (o.is_hash()) {
            return o;
        }
        return pCORE.die(pCx.VOID, new pArray(new pString("Not a HASH reference")));
    }

    // Note: multiple versions of set()
    public pObject hset(pObject s, pObject v) {
        this.h.put(s.to_string(), v.scalar());
        return v;
    }
    public pObject hset(String s, pObject v) {
        this.h.put(s, v.scalar());
        return v;
    }
    public pObject hset(pObject s, pScalar v) {
        this.h.put(s.to_string(), v.get());
        return v;
    }
EOT
    . ( join('', map {
            my $native = $_;
            my $perl   = $native_to_perl{$native};
"    public pObject hset(pObject s, $native v) {
        return this.hset(s, new $perl(v));
    }
    public pObject hset(String s, $native v) {
        return this.hset(s, new $perl(v));
    }
"
            }
            keys %native_to_perl ))

    . <<'EOT'

    public String to_string() {
        // TODO
        return "" + this.hashCode();
    }
    public int to_int() {
        // TODO
        return this.hashCode();
    }
    public double to_num() {
        return 0.0 + this.to_int();
    }
    public boolean to_bool() {
        return true;
    }
    public pObject add(pObject s) {
        return this.to_num_or_int().add(s);
    }
    public boolean is_int() {
        return false;
    }
    public boolean is_num() {
        return false;
    }
    public boolean is_string() {
        return false;
    }
    public boolean is_bool() {
        return false;
    }
    public boolean is_hash() {
        return true;
    }
    public pObject to_num_or_int() {
        return new pInt(this.to_int());
    }
    public pObject scalar() {
        return new pString(this.to_string());
    }
}
class pUndef extends pObject {
    public pUndef() {
    }
    public int to_int() {
        return 0;
    }
    public double to_num() {
        return 0.0;
    }
    public String to_string() {
        return "";
    }
    public boolean to_bool() {
        return false;
    }
    public boolean is_bool() {
        return false;
    }
    public pObject to_num_or_int() {
        return new pInt(0);
    }
}
class pBool extends pObject {
    private boolean i;
    public pBool(boolean i) {
        this.i = i;
    }
    public int to_int() {
        if (this.i) {
            return 1;
        }
        else {
            return 0;
        }
    }
    public double to_num() {
        if (this.i) {
            return 1.0;
        }
        else {
            return 0.0;
        }
    }
    public String to_string() {
        if (this.i) {
            return "1";
        }
        else {
            return "";
        }
    }
    public boolean to_bool() {
        return this.i;
    }
    public boolean is_bool() {
        return true;
    }
    public pObject to_num_or_int() {
        return new pInt(this.to_int());
    }
}
class pInt extends pObject {
    private int i;
    public pInt(int i) {
        this.i = i;
    }
    public int to_int() {
        return this.i;
    }
    public double to_num() {
        return (double)(this.i);
    }
    public String to_string() {
        return "" + this.i;
    }
    public boolean to_bool() {
        return this.i != 0;
    }
    public boolean is_int() {
        return true;
    }
    public pObject add(pObject s) {
        if (s.is_int()) {
            return new pInt( this.i + s.to_int() );
        }
        return s.to_num_or_int().add(this);
    }
    public pObject to_num_or_int() {
        return this;
    }
}
class pNum extends pObject {
    private double i;
    public pNum(double i) {
        this.i = i;
    }
    public int to_int() {
        return (int)(this.i);
    }
    public double to_num() {
        return this.i;
    }
    public String to_string() {
        return "" + this.i;
    }
    public boolean to_bool() {
        return this.i != 0.0;
    }
    public pNum add(pObject s) {
        return new pNum( this.i + s.to_num() );
    }
    public boolean is_num() {
        return true;
    }
    public pObject to_num_or_int() {
        return this;
    }
}
class pString extends pObject {
    private java.lang.String s;
    public pString(String s) {
        this.s = s;
    }
    public pString(char s) {
        this.s = "" + s;
    }
    public int to_int() {
        return Integer.parseInt(this.s.trim());
    }
    public double to_num() {
        return Double.parseDouble(this.s.trim());
    }
    public String to_string() {
        return this.s;
    }
    public boolean to_bool() {
        return this.s != ""
            && this.s != "0";
    }
    public boolean is_string() {
        return true;
    }
    public pObject to_num_or_int() {
        if (this.s.indexOf('.') > 0) {
            try {
                return new pNum(this.to_num());
            } catch (NumberFormatException nfe) {
                return new pInt(0);
            }
        }
        try {
            return new pInt(this.to_int());
        } catch (NumberFormatException nfe) {
            return new pInt(0);
        }
    } 
}
EOT
        # add "box" classes to Java classes
        # that were declared with
        #
        #   package MyJavaClass { Java }
        #
    . join('', ( map {
                    my $class = $_;
                    my $java_class_name = $class->{accessor};
"class p${java_class_name} extends pReference {
    public static final pString REF = new pString(\"${java_class_name}\");

    private ${java_class_name} stuff;
    // TODO - constructor with Perl parameters
    public p${java_class_name}() {
        this.stuff = new ${java_class_name}();
    }
    public p${java_class_name}(${java_class_name} stuff) {
        this.stuff = stuff;
    }
    public ${java_class_name} to_${java_class_name}() {
        return this.stuff;
    }
    public pObject ref() {
        return REF;
    }
}
"
            }
            values %java_classes
      ))

} # end of emit_javascript2()

1;

__END__

=pod

=head1 NAME

Perlito5::Java::Runtime

=head1 DESCRIPTION

Provides runtime routines for the Perlito-in-Java compiled code

=head1 AUTHORS

Flavio Soibelmann Glock

=head1 COPYRIGHT

Copyright 2015 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
