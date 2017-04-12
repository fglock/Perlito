package Perlito5::Java::Overload;

# work in progress

# See: full list of operators at
# /usr/local/lib/perl5/[VERSION]/overload/numbers.pm


1;

__END__


# Java constants


    public static final String OVERLOAD_STRING   = "(\"\"";  // (""
    public static final String OVERLOAD_NUM      = "(0+";
    public static final String OVERLOAD_ADD      = "(+";
    public static final String OVERLOAD_SUBTRACT = "(-";


# Java reference methods


    // overload
    public String toString() {
        return PlClass.overload_to_string(this).toString();
    }
    public int to_int() {
        return PlClass.overload_to_number(this).to_int();
    }
    public long to_long() {
        return PlClass.overload_to_number(this).to_long();
    }
    public PlObject add(PlObject s) {
        return PlClass.overload_add(this, s, PlCx.UNDEF);
    }
    public PlObject add2(PlObject s) {
        return PlClass.overload_add(this, s, PlCx.INT1);
    }
    public PlObject sub(PlObject s) {
        return PlClass.overload_subtract(this, s, PlCx.UNDEF);
    }
    public PlObject sub2(PlObject s) {
        return PlClass.overload_subtract(this, s, PlCx.INT1);
    }
    // end overload

# Java PlClass dispatchers

    // overload
    public static PlObject overload_to_string(PlObject o) {
        PlClass bless = o.blessed_class();
        if ( bless != null ) {
            PlObject methodCode = bless.method_lookup(PlCx.OVERLOAD_STRING, 0);
            if (methodCode.is_coderef()) {
                return methodCode.apply(PlCx.SCALAR, new PlArray(o));
            }
            // fallback
            methodCode = bless.method_lookup(PlCx.OVERLOAD_NUM, 0);
            if (methodCode.is_coderef()) {
                return methodCode.apply(PlCx.SCALAR, new PlArray(o));
            }
        }
        return new PlString(o.ref().toString() + "(0x" + Integer.toHexString(o.refaddr().to_int()) + ")");
    }
    public static PlObject overload_to_number(PlObject o) {
        PlClass bless = o.blessed_class();
        if ( bless != null ) {
            PlObject methodCode = bless.method_lookup(PlCx.OVERLOAD_NUM, 0);
            if (methodCode.is_coderef()) {
                return methodCode.apply(PlCx.SCALAR, new PlArray(o));
            }
            // fallback
            methodCode = bless.method_lookup(PlCx.OVERLOAD_STRING, 0);
            if (methodCode.is_coderef()) {
                return methodCode.apply(PlCx.SCALAR, new PlArray(o));
            }
        }
        return o.refaddr();
    }
    public static PlObject overload_add(PlObject o, PlObject other, PlObject swap) {
        PlClass bless = o.blessed_class();
        if ( bless != null ) {
            PlObject methodCode = bless.method_lookup(PlCx.OVERLOAD_ADD, 0);
            if (methodCode.is_coderef()) {
                return methodCode.apply(PlCx.SCALAR, new PlArray(o, other, swap));
            }
            // fallback
            o = PlClass.overload_to_number(o);
        }
        if (swap.to_boolean()) {
            return other.add(o);
        }
        return o.add(other);
    }
    public static PlObject overload_subtract(PlObject o, PlObject other, PlObject swap) {
        PlClass bless = o.blessed_class();
        if ( bless != null ) {
            PlObject methodCode = bless.method_lookup(PlCx.OVERLOAD_SUBTRACT, 0);
            if (methodCode.is_coderef()) {
                return methodCode.apply(PlCx.SCALAR, new PlArray(o, other, swap));
            }
            // fallback
            o = PlClass.overload_to_number(o);
        }
        if (swap.to_boolean()) {
            return other.sub(o);
        }
        return o.sub(other);
    }


