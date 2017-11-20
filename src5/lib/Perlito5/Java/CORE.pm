use v5;

package Perlito5::Java::CORE;
use strict;


my %FileFunc = (
    # open FILEHANDLE,EXPR
    # open FILEHANDLE,MODE,EXPR
    # open FILEHANDLE,MODE,EXPR,LIST
    # open FILEHANDLE,MODE,REFERENCE
    # open FILEHANDLE
    open => <<'EOT',
        int argCount = List__.to_int();
        Path path = null; 
        String mode = "";
        String s = "";
        try {
            fh.readlineBuffer = new StringBuilder();
            fh.eof = false;
            if (fh.outputStream != null) {
                fh.outputStream.close();
            }
            if (fh.reader != null) {
                fh.reader.close();
            }
            // PlCORE.say("open " + List__.toString());
            if (argCount == 0) {
                // As a shortcut a one-argument call takes the filename from the
                // global scalar variable of the same name as the filehandle
                PlCORE.die("TODO - not implemented: single argument open()");
            }
            else if (argCount == 1) {

                if (List__.aget(0).ref().str_eq(new PlString("SCALAR")).to_boolean()) {
                    PlObject o = List__.aget(0).scalar_deref("main");
                    fh.reader = new PlStringReader(o);
                    fh.reader.mark(o.toString().length());
                    fh.outputStream = null;
                    return PlCx.INT1;
                }

                // EXPR
                s = List__.aget(0).toString();
                if (s.length() > 0 && s.charAt(0) == '+') {
                    mode = mode + s.substring(0, 1);
                    s = s.substring(1);
                }
                if (s.length() > 1 && s.substring(0, 2).equals(">>")) {
                    mode = mode + s.substring(0, 2);
                    s = s.substring(2);
                }
                else if (s.length() > 0 && (s.charAt(0) == '>' || s.charAt(0) == '<')) {
                    mode = mode + s.substring(0, 1);
                    s = s.substring(1);
                }
                while (s.length() > 0 && (s.charAt(0) == ' ' || s.charAt(0) == '\t')) {
                    s = s.substring(1);
                }
            }
            else if (argCount > 1) {
                // MODE,EXPR,LIST?
                mode = List__.aget(0).toString();

                if (List__.aget(1).ref().str_eq(new PlString("SCALAR")).to_boolean()) {
                    // TODO - input stream, charset

                    PlObject o = List__.aget(1).scalar_deref("main");
                    fh.reader = new PlStringReader(o);
                    fh.reader.mark(o.toString().length());
                    fh.outputStream = null;
                    return PlCx.INT1;
                }

                s = List__.aget(1).toString();
            }

            String charset = "ISO-8859-1";
            int pos;
            pos = mode.indexOf(":raw");
            if (pos > 0) {
                charset = "ISO-8859-1";
                if ((pos + 4) > mode.length()) {
                    mode = mode.substring(0, pos).trim();
                }
                else {
                    mode = ( mode.substring(0, pos) + mode.substring(pos + 4) ).trim();
                }
            }
            pos = mode.indexOf(":bytes");
            if (pos > 0) {
                charset = "ISO-8859-1";
                if ((pos + 6) > mode.length()) {
                    mode = mode.substring(0, pos).trim();
                }
                else {
                    mode = ( mode.substring(0, pos) + mode.substring(pos + 6) ).trim();
                }
            }
            pos = mode.indexOf(":encoding(");
            if (pos > 0) {
                // extract the charset specification
                int last = mode.indexOf(")", pos);
                if (last > 0) {
                    charset = mode.substring(pos + 10, last);
                    if ((last + 1) > mode.length()) {
                        mode = mode.substring(0, pos).trim();
                    }
                    else {
                        mode = ( mode.substring(0, pos) + mode.substring(last + 1) ).trim();
                    }

                    if (charset.equals("Latin1")) {
                        charset = "ISO-8859-1";
                    }
                    if (charset.equals("utf8")) {
                        charset = "UTF-8";
                    }
                    if (charset.equals("utf16")) {
                        charset = "UTF-16";
                    }
                }
            }
            pos = mode.indexOf(":utf8");
            if (pos > 0) {
                charset = "UTF-8";
                if ((pos + 5) > mode.length()) {
                    mode = mode.substring(0, pos).trim();
                }
                else {
                    mode = ( mode.substring(0, pos) + mode.substring(pos + 5) ).trim();
                }
            }
            // PlCORE.say("charset [" + charset + "] mode [" + mode + "]");

            path = PlV.path.resolve(s);

            // save the info for binmode()
            fh.path = path;     // filename
            fh.mode = mode;     // ">", "+<"
            fh.charset = charset;   // "UTF-8"

            // PlCORE.say("path " + mode + " " + path.toString());
            if (mode.equals("<") || mode.equals("")) {
                fh.reader = Files.newBufferedReader(path, Charset.forName(charset));
                fh.outputStream = null;
            }
            else if (mode.equals(">")) {
                fh.reader = null;
                fh.outputStream = Files.newOutputStream(path, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
            }
            else if (mode.equals(">>")) {
                fh.reader = null;
                fh.outputStream = Files.newOutputStream(path, StandardOpenOption.CREATE, StandardOpenOption.APPEND, StandardOpenOption.WRITE);
            }
            else if (mode.equals("+<")) {
                // read/write
                // TODO - share the IO buffer for reads and writes
                fh.reader = Files.newBufferedReader(path, Charset.forName(charset));
                PlCORE.die("TODO - not implemented: open() mode '" + mode + "'");
            }
            else if (mode.equals("+>")) {
                // read/write, truncate first
                // TODO - share the IO buffer for reads and writes
                fh.reader = Files.newBufferedReader(path, Charset.forName(charset));
                PlCORE.die("TODO - not implemented: open() mode '" + mode + "'");
            }
            else if (mode.equals("<-")) {
                //   In the two-argument (and one-argument) form, opening "<-" or
                //   "-" opens STDIN and opening ">-" opens STDOUT.
                PlCORE.die("TODO - not implemented: open() mode '" + mode + "'");
            }
            else if (mode.equals(">-")) {
                //   In the two-argument (and one-argument) form, opening "<-" or
                //   "-" opens STDIN and opening ">-" opens STDOUT.
                PlCORE.die("TODO - not implemented: open() mode '" + mode + "'");
            }
            else if (mode.equals("|-")) {
                //   For three or more arguments if MODE is "|-", the filename is
                //   interpreted as a command to which output is to be piped, and if
                //   MODE is "-|", the filename is interpreted as a command that
                //   pipes output to us.  In the two-argument (and one-argument)
                //   form, one should replace dash ("-") with the command.  See
                //   "Using open() for IPC" in perlipc for more examples of this.
                PlCORE.die("TODO - not implemented: open() mode '" + mode + "'");
            }
            else if (mode.equals("-|")) {
                //   For three or more arguments if MODE is "|-", the filename is
                //   interpreted as a command to which output is to be piped, and if
                //   MODE is "-|", the filename is interpreted as a command that
                //   pipes output to us.  In the two-argument (and one-argument)
                //   form, one should replace dash ("-") with the command.  See
                //   "Using open() for IPC" in perlipc for more examples of this.
                PlCORE.die("TODO - not implemented: open() mode '" + mode + "'");
            }
            else {
                PlCORE.die("TODO - not implemented: open() mode '" + mode + "'");
            }
            path = path.toRealPath();
            // PlCORE.say("path " + mode + " " + path.toString());

            // success
            return PlCx.INT1;
        }
        catch(NoSuchFileException e) {
            PlV.sset("main::!", new PlString("No such file or directory"));
        }
        catch(Exception e) {
            PlV.sset("main::!", new PlStringLazyError(e));
        }
        return PlCx.UNDEF;
EOT
    close => <<'EOT',
        try {
            fh.readlineBuffer = new StringBuilder();
            fh.eof = true;
            if (fh.outputStream != null) {
                fh.outputStream.close();
            }
            if (fh.reader != null) {
                fh.reader.close();
            }

            // success
            return PlCx.INT1;
        }
        catch(IOException e) {
            PlV.sset("main::!", new PlStringLazyError(e));
        }
        return PlCx.UNDEF;
EOT
    # binmode FILEHANDLE, LAYER
    binmode => <<'EOT',
        String layer;
        int arg_count = List__.length_of_array_int();
        if (arg_count == 0) {
            // layer = ":raw";
            fh.binmode = true;
            return PlCx.INT1;
        }
        else {
            layer = List__.aget(0).toString();
        }
        return PlCORE.open(want, fh, new PlArray(
            new PlString(fh.mode + layer),
            new PlString(fh.path.toString()) 
        ));
EOT
    opendir => <<'EOT',
        try {
            String s = List__.aget(0).toString();
            Path path = PlV.path.resolve(s).toRealPath();

            fh.directoryStream = Files.newDirectoryStream(path);
            fh.directoryIterator = fh.directoryStream.iterator();

            // success
            return PlCx.INT1;
        }
        catch(IOException e) {
            PlV.sset("main::!", new PlStringLazyError(e));
        }
        return PlCx.UNDEF;
EOT
    readdir => <<'EOT',
        Iterator<Path> iter = fh.directoryIterator;
        if (want == PlCx.LIST) {
            // read all lines
            PlArray res = new PlArray();
            while (iter.hasNext()) {
                res.push(new PlString(iter.next().getFileName().toString()));
            }
            res.push(new PlString("."));
            res.push(new PlString(".."));
            return res;
        }
        if (!iter.hasNext()) {
            return PlCx.UNDEF;
        }
        Path entry = iter.next();
        return new PlString(entry.getFileName().toString());
EOT
    closedir => <<'EOT',
        try {
            fh.readlineBuffer = new StringBuilder();
            fh.eof = true;
            if (fh.directoryStream != null) {
                fh.directoryStream.close();
            }
        }
        catch(IOException e) {
            PlV.sset("main::!", new PlStringLazyError(e));
            return PlCx.UNDEF;
        }
        return PlCx.INT1;
EOT
    print => <<'EOT',
        try {
            String s = List__.toString();
            PlObject plsep = PlV.sget("main::\\");
            if (!plsep.is_undef()) {
                s = s + plsep.toString();
            }

            if (fh.binmode) {
                for (int i = 0; i < s.length(); i++) {
                    fh.outputStream.write(s.charAt(i));
                }
            }
            else {
                byte[] bytes = s.getBytes(fh.charset);
                fh.outputStream.write(bytes);
            }
            fh.outputStream.flush();
            return PlCx.INT1;
        }
        catch(Exception e) {
            PlV.sset("main::!", new PlStringLazyError(e));
            return PlCx.UNDEF;
        }
EOT
    syswrite => <<'EOT',
        int argCount = List__.to_int();
        if (argCount < 1) {
            PlCORE.die("Not enough arguments for syswrite");
        }
        if (argCount == 1) {
            return PlCORE.print(want, fh, List__);
        }
        return PlCORE.die("syswrite(FILEHANDLE,SCALAR,LENGTH,OFFSET) not implemented");
EOT
    write => <<'EOT',
        return PlCORE.die("write() not implemented");
EOT
    say => <<'EOT',
        List__.push( new PlString("\n") );
        return PlCORE.print(want, fh, List__);
EOT
    readline => <<'EOT',
        if (want == PlCx.LIST) {
            // read all lines
            PlArray res = new PlArray();
            PlObject s;
            while (!(s = PlCORE.readline(PlCx.SCALAR, fh, List__)).is_undef()) {
                res.push(s);
            }
            return res;
        }
        PlObject plsep = PlV.sget("main::/");
        boolean slurp = false;
        if (plsep.is_undef()) {
            slurp = true;
        }
        if (fh.eof) {
            if (fh.is_argv) {
                // "ARGV" is special
                PlArray argv = PlV.array_get("main::ARGV");
                PlFileHandle in = new PlFileHandle();
                if (argv.to_int() > 0) {
                    // arg list contains file name
                    PlCORE.open(PlCx.VOID, in, new PlArray(new PlString("<"), argv.shift()));
                }
                else {
                    // read from STDIN
                    fh.is_argv = false;     // clear the magic bit
                    in  = PlV.STDIN;
                }
                fh.readlineBuffer   = in.readlineBuffer;
                fh.eof              = in.eof;
                fh.outputStream     = in.outputStream;
                fh.reader           = in.reader;
            }
            if (fh.eof) {
                return PlCx.UNDEF;
            }
        }
        String sep = plsep.toString();
        StringBuilder buf = fh.readlineBuffer;
        // read from filehandle until "sep" or eof()
        int pos = slurp ? -1 : buf.indexOf(sep);
        while (pos < 0 && !fh.eof) {
            // read more
            int len = 1000;
            char[] c = new char[len];
            int num_chars = 0;
            try {
                num_chars = fh.reader.read(c, 0, len);
                if (num_chars > 0) {
                    // TODO - use: new String(bytes,"UTF-8")
                    String s = new String(c, 0, num_chars);
                    buf.append(s);
                }
            }
            catch(IOException e) {
                PlV.sset("main::!", new PlStringLazyError(e));
                return PlCx.UNDEF;
            }
            if (num_chars > 0) {
                if (!slurp) {
                    pos = buf.indexOf(sep);
                }
            }
            else {
                // eof
                fh.eof = true;
            }
        }
        String s;
        if (fh.eof || pos < 0) {
            s = buf.toString();
            fh.readlineBuffer = new StringBuilder();
            fh.eof = true;
            if (s.length() == 0) {
                return PlCx.UNDEF;
            }
        }
        else {
            pos += sep.length();
            s = buf.substring(0, pos);
            fh.readlineBuffer = new StringBuilder(buf.substring(pos));
        }
        return new PlString(s);
EOT
    # getc FILEHANDLE
    getc => <<'EOT',
        PlLvalue buf = new PlLvalue();
        PlCORE.sysread(want, fh, PlArray.construct_list_of_aliases(buf, PlCx.INT1));
        return buf;
EOT
    # read FILEHANDLE,SCALAR,LENGTH,OFFSET?
    read => <<'EOT',
        return PlCORE.sysread(want, fh, List__);
EOT
    # sysread FILEHANDLE,SCALAR,LENGTH,OFFSET?
    #   result is stored in $_[0]
    #   result may be utf8 or not
    #   returns number of bytes read
    #   set $! on error
    sysread => <<'EOT',
        int leng = List__.aget(1).to_int();
        int ofs = List__.aget(2).to_int();

        if (fh.eof) {
            return PlCx.UNDEF;
        }
        StringBuilder buf = fh.readlineBuffer;
        // read from filehandle until "len"
        int pos = buf.length();
        while (pos < leng && !fh.eof) {
            // read more
            int len = 1000;
            char[] c = new char[len];
            int num_chars = 0;
            try {
                num_chars = fh.reader.read(c, 0, len);
                if (num_chars > 0) {
                    // TODO - use: new String(bytes,"UTF-8")
                    String s = new String(c, 0, num_chars);
                    buf.append(s);
                }
            }
            catch(IOException e) {
                PlV.sset("main::!", new PlStringLazyError(e));
                return PlCx.UNDEF;
            }
            if (num_chars > 0) {
                pos = buf.length();
            }
            else {
                // eof
                fh.eof = true;
            }
        }
        String s;
        if (fh.eof || pos < leng) {
            s = buf.toString();
            fh.readlineBuffer = new StringBuilder();
            fh.eof = true;
            if (s.length() == 0) {
                return PlCx.UNDEF;
            }
        }
        else {
            s = buf.substring(0, leng);
            fh.readlineBuffer = new StringBuilder(buf.substring(leng));
        }

        leng = s.length();
        if (ofs == 0) {
            List__.aset(0, s);
        }
        else {
            die("TODO: sysread with OFFSET");
        }
        return new PlInt(leng);
EOT
    # seek FILEHANDLE,POSITION,WHENCE
    seek => <<'EOT',
        int position = List__.aget(0).to_int();
        int whence   = List__.aget(1).to_int();
        try {

            // TODO - random access files, more tests
            // See: http://stackoverflow.com/questions/262618/java-bufferedreader-back-to-the-top-of-a-text-file

            // position = 0
            if (fh.reader == null) {
                PlV.sset("main::!", new PlString("File is not open"));
                return PlCx.UNDEF;
            }
            fh.reader.reset();
            fh.readlineBuffer = new StringBuilder();
            fh.eof = false;

            if (position > 0) {
                PlCORE.read(PlCx.VOID, fh, new PlArray(PlCx.UNDEF, new PlInt(position)));
            }

        }
        catch(IOException e) {
            PlV.sset("main::!", new PlStringLazyError(e));
            return PlCx.UNDEF;
        }
        return PlCx.INT1;
EOT

);


sub emit_java {
    return <<'EOT'

class PlCORE {
EOT
    # emit all file-related functions
    . join("", map {
          "    public static final PlObject $_(int want, PlFileHandle fh, PlArray List__) {\n"
        .       $FileFunc{$_}
        . "    }\n"
        } sort keys %FileFunc
    ) . <<'EOT'

    // shortcut functions for internal use: say, warn, die
    public static final PlObject say(String s) {
        return PlCORE.say(PlCx.VOID, PlV.STDOUT, new PlArray(new PlString(s)));
    }
    public static final PlObject warn(String s) {
        return PlCORE.warn(PlCx.VOID, new PlArray(new PlString(s)));
    }
    public static final PlObject die(String s) {
        return PlCORE.die(PlCx.VOID, new PlArray(new PlString(s)));
    }

    public static final PlObject mkdir(int want, PlArray List__) {
        try {
            Path path = PlV.path.resolve(List__.aget(0).toString());
            int mask = List__.aget(1).to_int();
            Set<PosixFilePermission> perms = PerlOp.MaskToPermissions(mask);
            FileAttribute<Set<PosixFilePermission>> attr = PosixFilePermissions.asFileAttribute(perms);
            Files.createDirectory(path, attr);
            return PlCx.INT1;
        }
        catch(IOException e) {
            PlV.sset("main::!", new PlStringLazyError(e));
        }
        return PlCx.UNDEF;
    }
    public static final PlObject require(int want, PlObject file, boolean is_bareword) {
        // TODO - require-version
        if (is_bareword) {
            file = new PlString("Perlito5::Grammar::Use::modulename_to_filename").apply(PlCx.SCALAR, new PlArray(file));
        }
        return new PlString("Perlito5::Grammar::Use::require").apply(want, new PlArray(file));
    }
    public static final PlObject rmdir(int want, PlArray List__) {
        try {
            Path path = PlV.path.resolve(List__.aget(0).toString()).toRealPath();
            Files.delete(path);
            return PlCx.INT1;
        }
        catch(NoSuchFileException e) {
            PlV.sset("main::!", new PlString("No such file or directory"));
        }
        catch(DirectoryNotEmptyException e) {
            PlV.sset("main::!", new PlString("Directory not empty"));
        }
        catch(IOException e) {
            PlV.sset("main::!", new PlStringLazyError(e));
        }
        return PlCx.UNDEF;
    }
    public static final PlObject chdir(int want, PlArray List__) {
        try {
            Path path = PlV.path.resolve(List__.aget(0).toString()).toRealPath();
            PlV.path = path;
            // TODO - test that the destination is a directory
            return PlCx.INT1;
        }
        catch(NoSuchFileException e) {
            PlV.sset("main::!", new PlString("No such file or directory"));
        }
        catch(DirectoryNotEmptyException e) {
            PlV.sset("main::!", new PlString("Directory not empty"));
        }
        catch(IOException e) {
            PlV.sset("main::!", new PlStringLazyError(e));
        }
        return PlCx.UNDEF;
    }
    public static final PlObject unlink(int want, PlArray List__) {
        try {
            for (int i = 0; i < List__.to_int(); i++) {
                Path path = PlV.path.resolve(List__.aget(i).toString()).toRealPath();
                Files.delete(path);
            }
            return PlCx.INT1;
        }
        catch(NoSuchFileException e) {
            PlV.sset("main::!", new PlString("No such file or directory"));
        }
        catch(DirectoryNotEmptyException e) {
            PlV.sset("main::!", new PlString("Directory not empty"));
        }
        catch(IOException e) {
            PlV.sset("main::!", new PlStringLazyError(e));
        }
        return PlCx.UNDEF;
    }
    public static final PlObject lstat(int want, PlArray List__) {
        // TODO
        return PlCORE.stat(want, List__);
    }
    public static final PlObject stat(int want, PlArray List__) {
        // TODO - "_" filehandle
        PlArray res = new PlArray();
        PlObject arg = List__.aget(0);
        try {
            //     0 dev      device number of filesystem
            //     1 ino      inode number
            //     2 mode     file mode  (type and permissions)
            //     3 nlink    number of (hard) links to the file
            //     4 uid      numeric user ID of file's owner
            //     5 gid      numeric group ID of file's owner
            //     6 rdev     the device identifier (special files only)
            //     7 size     total size of file, in bytes
            //     8 atime    last access time in seconds since the epoch
            //     9 mtime    last modify time in seconds since the epoch
            //    10 ctime    inode change time in seconds since the epoch (*)
            //    11 blksize  preferred I/O size in bytes for interacting with the
            //                file (may vary from file to file)
            //    12 blocks   actual number of system-specific blocks allocated
            //                on disk (often, but not always, 512 bytes each)
            res.aset(7, PerlOp.p5size(arg));
            res.aset(9, PerlOp.p5mtime(arg));
            if (want == PlCx.SCALAR) {
                return PlCx.TRUE;
            }
            return res;
        }
        catch(Exception e) {
            PlV.sset("main::!", new PlStringLazyError(e));
        }
        if (want == PlCx.SCALAR) {
            return PlCx.FALSE;
        }
        return res;
    }
    public static final PlObject select(PlFileHandle fh) {
        // select FILEHANDLE
        PlFileHandle fOld = PlV.STDOUT;
        PlV.STDOUT = fh;
        return fOld;
    }
    public static final PlObject select(int want, PlArray List__) {
        int arg_count = List__.length_of_array_int();
        if (arg_count == 0) {
            // Returns the currently selected filehandle
            return PlV.STDOUT;
        }
        if (arg_count == 4) {
            if (List__.aget(0).is_undef() && List__.aget(1).is_undef() && List__.aget(2).is_undef()) {
                // You can effect a sleep of 250 milliseconds this way: select(undef, undef, undef, 0.25);
                PlCORE.sleep(want, new PlArray(List__.aget(3)));
                return PlCx.INT0;
            }
        }
        return PlCORE.die("select() not implemented");
    }
    public static final PlObject exit(int want, PlArray List__) {
        int arg = List__.aget(0).to_int();

        // Perlito5::set_global_phase("END");
        new PlStringConstant("Perlito5::set_global_phase").apply(PlCx.VOID, PlArray.construct_list_of_aliases(new PlStringConstant("END")));
        // $_->() for @Perlito5::END_BLOCK;
        for (PlObject tmp : PlArray.construct_list_of_aliases(PlV.array_get("Perlito5::END_BLOCK"))) {
            tmp.apply(PlCx.VOID, new PlArray());
        }

        System.exit(arg);
        return PlCx.UNDEF;
    }
    public static final PlObject warn(int want, PlArray List__) {
        try {
            int arg_count = List__.length_of_array_int();
            if (arg_count == 0) {
                List__.push("Warning: something's wrong");
            }
            if (arg_count != 1 || !List__.aget(0).is_ref()) {
                String s = List__.toString();
                int s_length = s.length();
                if (s_length > 0 && (s.charAt(s_length-1) == '\n' || s.charAt(s_length-1) == '\r')) {
                    // don't add file+line
                }
                else {
                    // TODO - add module name, line number
                    s = s + " at " + PlV.sget("main::0") + "\n";
                }
                List__.set(new PlArray(new PlString(s)));
            }
            if (PlV.hash_get("main::SIG").hget("__WARN__").is_coderef()) {
                // execute $SIG{__WARN__}
                // localize $SIG{__WARN__} during the call
                int tmp = PerlOp.local_length();
                PlObject c = PlV.hash_get("main::SIG").hget("__WARN__");
                PlV.hash_get("main::SIG").hget_lvalue_local("__WARN__");
                c.apply(want, List__);
                PerlOp.cleanup_local(tmp, PlCx.UNDEF);
            }
            else {
                String s = List__.toString();
                byte[] bytes = s.getBytes(PlV.STDERR.charset);
                PlV.STDERR.outputStream.write(bytes);
                PlV.STDERR.outputStream.flush();
            }
            return PlCx.INT1;
        }
        catch(Exception e) {
            PlV.sset("main::!", new PlStringLazyError(e));
            return PlCx.UNDEF;
        }
    }
    public static final PlObject die(int want, PlArray List__) {
        int arg_count = List__.length_of_array_int();
        if (arg_count == 0) {
            List__.push("Died");
        }
        if (arg_count != 1 || !List__.aget(0).is_ref()) {
            String s = List__.toString();
            int s_length = s.length();
            if (s_length > 0 && (s.charAt(s_length-1) == '\n' || s.charAt(s_length-1) == '\r')) {
                // don't add file+line
            }
            else {
                // TODO - add module name, line number
                s = s + " at " + PlV.sget("main::0") + "\n";
                // Java stack trace
                s = s + Arrays.toString(new Throwable().getStackTrace());
            }
            List__.set(new PlArray(new PlString(s)));
        }
        if (PlV.hash_get("main::SIG").hget("__DIE__").is_coderef()) {
            // execute $SIG{__DIE__}
            // localize $SIG{__DIE__} during the call
            int tmp = PerlOp.local_length();
            PlObject c = PlV.hash_get("main::SIG").hget("__DIE__");
            PlV.hash_get("main::SIG").hget_lvalue_local("__DIE__");
            c.apply(want, List__);
            PerlOp.cleanup_local(tmp, PlCx.UNDEF);
        }
        PlObject arg = List__.aget(0);
        throw new PlDieException(arg);
    }
    // public static final PlString ref(int want, PlArray List__) {
    //     return List__.aget(0).ref();
    // }
    public static final PlObject values(int want, PlObject List__) {
        return want == PlCx.LIST ? List__.values() : List__.values().scalar();
    }
    public static final PlObject keys(int want, PlObject List__) {
        return want == PlCx.LIST ? List__.keys() : List__.keys().scalar();
    }
    public static final PlObject each(int want, PlObject List__) {
        return want == PlCx.LIST ? List__.each() : List__.each().aget(0);
    }
    public static final PlObject chomp(int want, PlObject Object__) {
        String sep = PlV.sget("main::/").toString();
        int sepSize = sep.length();
        int result = 0;
        String toChomp = Object__.toString();
        if(toChomp.substring(toChomp.length() - sepSize, toChomp.length()).equals(sep)) {
            toChomp = toChomp.substring(0, toChomp.length() - sepSize);
            result += sepSize;
        }

        Object__.set(new PlString(toChomp));
            
        return new PlInt(result);
    }
    public static final PlObject chomp(int want, PlArray List__) {
        int result = 0;
        for(int i = 0; i < List__.to_int(); ++i) {
            PlObject item = List__.aget_lvalue(i);
            result += chomp(want, item).to_int();
        }

        return new PlInt(result);
    }
    public static final PlString chop(int want, PlObject Object__) {
        String str = Object__.toString();
        String returnValue = "";
        if (str.length() > 0) {
            returnValue = str.substring(str.length() -1);
            Object__.set(new PlString(str.substring(0, str.length()-1)));
        }

        return new PlString(returnValue);
    }
    public static final PlObject chop(int want, PlArray List__) {
        PlString result = PlCx.EMPTY;
        for(int i = 0; i < List__.to_int(); ++i) {
            PlObject item = List__.aget_lvalue(i);
            result = chop(want, item);
        }

        return result;
    }
    public static final PlObject scalar(int want, PlArray List__) {
        if (List__.to_int() == 0) {
            return PlCx.UNDEF;
        }
        return List__.aget(-1).scalar();
    }
    public static final PlObject splice(int want, PlArray List__) {
        PlArray res = new PlArray(List__);
        List__.a.clear();
        if (want == PlCx.LIST) {
            return res;
        }
        if (res.to_int() == 0) {
            return PlCx.UNDEF;
        }
        return res.aget(-1);
    }
    public static final PlObject split(int want, PlObject plReg, PlObject plArg, PlObject plCount) {
        if (want == PlCx.SCALAR) {
            return PlCORE.split(PlCx.LIST, plReg, plArg, plCount).length_of_array();
        }
        int limit = plCount.to_int();
        PlArray res = new PlArray();
        if (limit == 0) {
            // strip trailing empty strings and undef
            res = (PlArray)PlCORE.split(PlCx.LIST, plReg, plArg, PlCx.MIN1);
            while (res.to_int() > 0) {
                PlObject item = res.aget(-1);
                if (item.is_undef() || item.toString().length() == 0) {
                    res.pop();
                }
                else {
                    return res;
                }
            }
            return res;
        }
        String arg = plArg.toString();
        if (arg.length() == 0) {
            return res;
        }
        // make sure pattern is a RegExp
        if (plReg.is_lvalue()) {
            plReg = plReg.get();
        }

        // --- TODO ---

        if (!plReg.is_regex()) {
            String regs = plReg.toString();
            if (regs.equals(" ")) {

                // ltrim
                int i = 0;
                while (i < arg.length() && Character.isWhitespace(arg.charAt(i))) {
                    i++;
                }
                if (i > 0) {
                    arg = arg.substring(i);
                }

                plReg = PlCx.SPLIT_SPACE;
            }
            else {
                plReg = new PlRegex(regs, Pattern.MULTILINE, false);
            }
        }

        Pattern pat = ((PlRegex)plReg).p;

        //      // make sure pattern is a RegExp
        //      if (typeof pattern === "object" && (pattern instanceof RegExp)) {
        //          pattern = pattern.source;
        //      }
        //      else {
        //          pattern = p5str(pattern);
        //          if (pattern == " ") {
        //              // single space string is special
        //              pattern = "(?: |\t|\n)+";
        //              s = s.replace(/^(?: |\t|\n)+/, "");
        //          }
        //      }
        //      // add "g", "m" modifiers
        //      var flags = "g";
        //      if (pattern.substr(0, 1) == "^" || pattern.substr(-1,1) == "$") {
        //          flags = flags + "m";
        //      }
        // --- /TODO ---

        int pos = 0;
        int next = pos;
        int count = 1;
        String cap;
        Matcher matcher = pat.matcher(arg).useTransparentBounds(true);
        while (pos < arg.length() && !(limit > 0 && count >= limit) && matcher.find(next)) {
            boolean matched = true;
            if (matcher.end() == pos) {
                // pointer didn't move
                next++;
                matched = matcher.find(next);
            }
            if (matched) {
                cap = arg.substring(pos, matcher.start());
                res.push(cap);
                pos = matcher.end();
                next = pos;
                // PlCORE.say("match: match [" + cap + "] next pos " + pos);
                count++;
                for (int i = 1; i <= matcher.groupCount(); i++) {
                    cap = matcher.group(i);
                    if (cap == null) {
                        res.push(PlCx.UNDEF);
                    }
                    else {
                        res.push(cap);
                    }
                }
            }
        }
        if ( pos >= arg.length()) {
            cap = "";
        }
        else {
            cap = arg.substring(pos);
        }
        res.push(cap);
        return res;
    }
    public static final PlObject splice(int want, PlArray List__, PlObject offset) {
        int size = List__.to_int();
        int pos  = offset.to_int();
        if (pos < 0) {
            pos = List__.a.size() + pos;
        }
        if (pos < 0 || pos > List__.a.size()) {
            return PlCx.UNDEF;
        }
        PlArray res = new PlArray();
        for (int i = pos; i < size; i++) {
            res.unshift(List__.pop());
        }
        if (want == PlCx.LIST) {
            return res;
        }
        if (res.to_int() == 0) {
            return PlCx.UNDEF;
        }
        return res.aget(-1);
    }
    public static final PlObject splice(int want, PlArray List__, PlObject offset, PlObject length) {
        int size = List__.to_int();
        int pos  = offset.to_int();
        if (pos < 0) {
            pos = List__.a.size() + pos;
        }
        if (pos < 0 || pos > List__.a.size()) {
            return PlCx.UNDEF;
        }

        int last = length.to_int();
        if (last < 0) {
            last = List__.a.size() + last;
        }
        else {
            last = pos + last;
        }
        if (last < 0) {
            return PlCx.UNDEF;
        }
        if (last > size) {
            last = size;
        }

        int diff = last - pos;
        PlArray res = new PlArray();
        for (int i = pos; i < last; i++) {
            res.push(List__.a.get(i));
        }
        for (int i = pos; i < (size - diff); i++) {
            List__.a.set(i, List__.a.get(i+diff));
        }
        for (int i = 0; i < diff; i++) {
            List__.pop();
        }
        if (want == PlCx.LIST) {
            return res;
        }
        if (res.to_int() == 0) {
            return PlCx.UNDEF;
        }
        return res.aget(-1);
    }
    public static final PlObject splice(int want, PlArray List__, PlObject offset, PlObject length, PlArray list) {
        int size = List__.to_int();
        int pos  = offset.to_int();
        if (pos < 0) {
            pos = List__.a.size() + pos;
        }
        if (pos < 0 || pos > List__.a.size()) {
            return PlCx.UNDEF;
        }

        int last = length.to_int();
        if (last < 0) {
            last = List__.a.size() + last;
        }
        else {
            last = pos + last;
        }
        if (last < 0) {
            return PlCx.UNDEF;
        }
        if (last > size) {
            last = size;
        }

        int diff = last - pos;
        PlArray res = new PlArray();

        for (int i = pos; i < last; i++) {
            res.push(List__.a.get(i));
        }
        for (int i = pos; i < (size - diff); i++) {
            List__.a.set(i, List__.a.get(i+diff));
        }
        for (int i = 0; i < diff; i++) {
            List__.pop();
        }

        List__.a.addAll(pos, list.a);
        if (want == PlCx.LIST) {
            return res;
        }
        if (res.to_int() == 0) {
            return PlCx.UNDEF;
        }
        return res.aget(-1);
    }

    public static final PlObject hex(int want, PlObject List__) {
        String s = List__.toString();

        final int length = s.length();
        int c;

        for (int i = 0; i < length; i++ ) {
            c = s.codePointAt(i);
            if (c > 254) {
                PlCORE.die("Wide character in hex");
            }
        }

        if (s.startsWith("0x") || s.startsWith("0X")) {
            s = s.substring(2);
        }
        else if (s.startsWith("x") || s.startsWith("X")) {
            s = s.substring(1);
        }
        s = s.replace("_","");
        try {
            return new PlInt(Long.parseLong(s, 16));
        } catch (java.lang.NumberFormatException e) {
            // result = e.getMessage();
        }
        return new PlInt(0);
    }
    public static final PlObject oct(int want, PlObject List__) {
        String s = List__.toString();
        return new PlInt(PerlOp.oct(s));
    }
    public static final PlObject sprintf(int want, PlObject List__) {
        String format = List__.aget(0).toString();
        // "%3s"
        int length = format.length();
        int offset = 0;
        int args_max = List__.to_int();
        int args_index = 0;
        Object args[] = new Object[args_max];
        String detail = "";
        int start_detail = -1;
        for ( ; offset < length; ) {
            int c = format.codePointAt(offset);
            switch (c) {
                case '%':
                    offset++;
                    boolean scanning = true;
                    start_detail = offset;
                    for ( ; offset < length && scanning ; ) {
                        c = format.codePointAt(offset);
                        switch (c) {
                            case '%':
                                scanning = false;
                                offset++;
                                break;
                            case 'v':
                                // TODO - format value like "v1.v2.v3"
                                // replace "%v" with "%s"
                                StringBuilder sbv = new StringBuilder();
                                if (offset > 0) {
                                    sbv.append(format.substring(0, offset));
                                }
                                sbv.append("s");
                                if (offset + 1 < format.length()) {
                                    sbv.append(format.substring(offset + 1));
                                }
                                format = sbv.toString();
                                break;
                            case 'c': case 's': case 'd': case 'u': case 'o':
                            case 'x': case 'e': case 'f': case 'g':
                            case 'X': case 'E': case 'G': case 'b':
                            case 'B': case 'p': case 'n':
                            case 'i': case 'D': case 'U': case 'O': case 'F':
                                scanning = false;
                                switch (c) {
                                    case 's':
                                        args[args_index] = List__.aget(args_index+1).toString();
                                        break;
                                    case 'd': case 'o': case 'x': case 'X':
                                    case 'u': case 'b': case 'B': case 'p':
                                    case 'c':
                                        args[args_index] = List__.aget(args_index+1).to_long();

                                        if (c == 'u') {
                                            long arg = (long)(args[args_index]);
                                            arg = arg & 4294967295L; // 0xFFFFFFFF;
                                            args[args_index] = arg;
                                            StringBuilder sb = new StringBuilder();
                                            if (offset > 0) {
                                                sb.append(format.substring(0, offset));
                                            }
                                            sb.append("d");
                                            if (offset + 1 < format.length()) {
                                                sb.append(format.substring(offset + 1));
                                            }
                                            format = sb.toString();
                                            //PlCORE.say("format [" + format + "]");
                                        }

                                        break;
                                    case 'f': case 'e': case 'g':
                                    case 'E': case 'G':
                                        detail = format.substring(start_detail, offset);
                                        if (detail.equals(".") || detail.equals("+.")) {
                                            StringBuilder sb = new StringBuilder();
                                            sb.append(format.substring(0, offset));
                                            sb.append("0");
                                            sb.append(format.substring(offset));
                                            format = sb.toString();
                                        }
                                        args[args_index] = List__.aget(args_index+1).to_double();
                                        break;
                                    default:
                                        break;
                                }
                                args_index++;
                                if (args_index > args_max) {
                                    // panic
                                    offset = length;
                                }
                                offset++;
                                break;
                            default:
                                offset++;
                                break;
                        }
                    }
                    break;
                default:
                    offset++;
                    break;
            }
        }
        return new PlString(String.format(format, args));
    }
    public static final PlObject crypt(int want, PlArray List__) {
        if(List__.to_int() < 2) {
            die("Not enough arguments for crypt");
        }
        if(List__.to_int() > 2) {
            die("Too many arguments for crypt");
        }
        String plainText = List__.shift().toString();
        String salt = List__.shift().toString();

        while(salt.length() < 2) {
            salt = salt.concat(".");
        }
        
        return new PlString(PlCrypt.crypt(salt, plainText));
    }
    public static final PlObject join(int want, PlArray List__) {
        String s = List__.shift().toString();
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        for (int i = 0; i < List__.to_int(); i++) {
            String item = List__.aget(i).toString();
            if (first)
                first = false;
            else
                sb.append(s);
            sb.append(item);
        }
        return new PlString(sb.toString());
    }
    public static final PlObject reverse(int want, PlArray List__) {
        if (want == PlCx.LIST) {
            PlArray ret = PlArray.construct_list_of_aliases(List__);
            Collections.reverse(ret.a);
            return ret;
        }
        StringBuilder sb = new StringBuilder();
        if (List__.to_int() == 0) {
            sb.append( PlV.sget("main::_") );
        }
        else {
            for (int i = 0; i < List__.to_int(); i++) {
                sb.append( List__.aget(i).toString() );
            }
        }
        return new PlString(sb.reverse().toString());
    }
    public static final PlObject fc(int want,  PlObject Object__) {
        return new PlString(Object__.toString().toLowerCase());
    }
    public static final PlObject pack(int want, PlArray List__) {
        String template = List__.shift().toString();
        StringBuilder result = new StringBuilder();

        // Character mode is the default unless the format string starts with "U"
        boolean characterMode = true;
        if (template.length() > 0 && template.charAt(0) == 'U') {
            characterMode = false;
        }

        for(int i = 0; i < template.length(); ++i) {
            int size;
            if (template.length() > (i+1) && template.charAt(i+1) == '*') {
                size = List__.to_int();
            }
            else {
                size = pack_size(template, i);
            }

            switch(template.charAt(i)) {
            case 'a':
            {
                result.append(pack_a(List__.shift().toString(), size));
                break;
            }
            case 'A':
            {    
                result.append(pack_A(List__.shift().toString(), size));
                break;
            }
            case 'Z':
            {
                result.append(pack_Z(List__.shift().toString(), size));
                break;
            }
            case 'b':
            {
                result.append(pack_b(List__.shift().toString(), size));
                break;
            }
            case 'B':
            {
                result.append(pack_B(List__.shift().toString(), size));
                break;
            }
            case 'h':
            {
                result.append(pack_h(List__.shift().toString(), size));
                break;
            }
            case 'H':
            {
                result.append(pack_H(List__.shift().toString(), size));
                break;        
            }
            case 'c':
            {
                result.append(pack_c(List__.shift().toString()));
                break;        
            }
            case 'C':
            {
                if (size == 0) {
                    // C0
                    characterMode = true;
                }
                for (int j = 0; j < size; j++) {
                    result.append(pack_C(List__.shift().toString()));
                }
                break;        
            }
            case 'W':
            {
                for (int j = 0; j < size; j++) {
                    result.appendCodePoint( List__.shift().to_int() );
                }
                break;        
            }
            case 'U':
            {
                if (size == 0) {
                    // U0
                    characterMode = false;
                }
                if (characterMode) {
                    // character mode C0
                    StringBuilder sb = new StringBuilder();
                    for (int j = 0; j < size; j++) {
                        sb.appendCodePoint( List__.shift().to_int() );
                    }
                    byte[] bytes = sb.toString().getBytes(PlCx.UTF8);
                    for (byte b : bytes) {
                        int ub = b < 0 ? 256 + b : b;
                        result.appendCodePoint(ub);
                    }
                }
                else {
                    // U0 mode
                    for (int j = 0; j < size; j++) {
                        result.appendCodePoint( List__.shift().to_int() );
                    }
                }
                break;        
            }
            case 's':
            {
                result.append(pack_s(List__.shift().to_int()));
                break;        
            }
            case 'S':
            {
                result.append(pack_S(List__.shift().to_int()));
                break;        
            }
            case 'l':
            {
                result.append(pack_l(List__.shift().to_int()));
                break;        
            }
            case 'L':
            {
                result.append(pack_L(List__.shift().to_int()));
                break;        
            }
            case 'q':
            {
                result.append(pack_q(List__.shift().to_int()));
                break;        
            }
            case 'Q':
            {
                result.append(pack_Q(List__.shift().to_int()));
                break;        
            }
            case 'i':
            {
                result.append(pack_i(List__.shift().to_int()));
                break;        
            }
            case 'I':
            {
                result.append(pack_I(List__.shift().to_int()));
                break;        
            }
            case 'n':
            {
                for (int j = 0; j < size; j++) {
                    result.append(pack_n(List__.shift().to_int()));
                }
                break;        
            }
            case 'N':
            {
                result.append(pack_N(List__.shift().to_int()));
                break;        
            }
            case 'v':   
            {
                for (int j = 0; j < size; j++) {
                    result.append(pack_v(List__.shift().to_int()));
                }
                break;        
            }
            case 'V':   
            {
                result.append(pack_V(List__.shift().to_int()));
                break;        
            }
            case 'j':   
            {
                result.append(pack_j(List__.shift().to_int()));
                break;        
            }
            case 'J':   
            {
                result.append(pack_J(List__.shift().to_int()));
                break;        
            }
            case 'f':
            {
                result.append(pack_f(List__.shift().to_double()));
                break;        
            }
            case 'd':
            case 'F':
            {
                result.append(pack_d(List__.shift().to_double()));
                break;        
            }
            case 'p':
            {
                for(int k = 0; k < size; ++k) {
                    PlObject o = List__.shift();
                    if(o.is_undef()) {
                        result.append(pack_q(0));
                    
                    } else {
                        result.append(pack_p(o.toString()));
                    }
                }
            }
            case 'u':
            {
                result.append(pack_u(List__.shift().toString()));
                break;
            }
            case 'w':
            {
                String[] input = new String[size];
                for(int j = 0; j < size; ++j) {
                    input[j] = List__.shift().toString();
                }
                result.append(pack_w(input, size));
                break;        
            }
            case 'x':
            {
                result.append(pack_x(size));
                break;        
            }
            case 'X':
            {
                int length = result.length();
                result.delete(Math.max(0,length - size), length);
                break;        
            }
            case '@':
            {
                int length = result.length();
                if(size > length) {
                    result.append(new char[size - length]);
                }
                break;        
            }
            case '.':
            {
                int length = result.length();
                if(size > length) {
                    result.append(new char[size - length]);
                }
                break;        
            }
            default:
            }
        }

        return new PlString(result.toString());
    }
    public static final PlObject unpack(int want, PlArray List__) {
        String template = List__.aget(0).toString();
        String input = List__.aget(1).toString();
        PlArray result = new PlArray();
        int inputIndex = 0;

        // Character mode is the default unless the format string starts with "U"
        boolean characterMode = true;
        if (template.length() > 0 && template.charAt(0) == 'U') {
            characterMode = false;
        }

        for(int i = 0; i < template.length(); ++i) {
            int size;
            if (template.length() > (i+1) && template.charAt(i+1) == '*') {
                size = -1;
            }
            else {
                size = pack_size(template, i);
            }

            switch(template.charAt(i)) {
            case 'a':
            {
                // TODO
                // result.push(unpack_a(List__.shift().toString(), size));
                break;
            }
            case 'A':
            {
                // TODO
                // result.push(unpack_A(List__.shift().toString(), size));
                break;
            }
            case 'Z':
            {
                // TODO
                // result.push(unpack_Z(List__.shift().toString(), size));
                break;
            }
            case 'b':
            {
                // TODO
                // result.push(unpack_b(List__.shift().toString(), size));
                break;
            }

            case 'C':
            {
                if (size == 0) {
                    // C0
                    characterMode = true;
                }
                if (size < 0) {
                        while (inputIndex < input.length()) {
                            result.push( new PlInt( input.charAt(inputIndex++) & 0xFF ) );
                        }
                }
                else {
                    for (int j = 0; j < size; j++) {
                        if (inputIndex < input.length()) {
                            result.push( new PlInt( input.charAt(inputIndex++) & 0xFF ) );
                        }
                    }
                }
                break;        
            }
            case 'W':
            {
                // TODO
                // for (int j = 0; j < size; j++) {
                //     result.pushCodePoint( List__.shift().to_int() );
                // }
                break;        
            }
            case 'U':
            {
                if (size == 0) {
                    // U0
                    characterMode = false;
                    break;
                }
                if (characterMode) {
                    // character mode C0

                    if (inputIndex >= input.length()) {
                        break;
                    }

                    // decode from UTF-8 ("bytes") to internal representation
                    String s = input.substring(inputIndex);
                    char[] chars = s.toCharArray();
                    byte[] bytes = new byte[chars.length];
                    for (int j = 0; j < bytes.length; j++){
                        bytes[j] = (byte)(chars[j]);
                    }
                    String internal = new String(bytes, PlCx.UTF8);
                    int internalIndex = 0;
 
                    // unpack
                    StringBuilder sb = new StringBuilder();
                    if (size < 0) {
                            while (internalIndex < internal.length()) {
                                int ichar = internal.charAt(internalIndex++);
                                sb.appendCodePoint(ichar);
                                result.push( new PlInt(ichar) );
                            }
                    }
                    else {
                        for (int j = 0; j < size; j++) {
                            if (internalIndex < internal.length()) {
                                int ichar = internal.charAt(internalIndex++);
                                sb.appendCodePoint(ichar);
                                result.push( new PlInt(ichar) );
                            }
                        }
                    }

                    // move the input pointer by the number of "bytes" consumed (not chars)
                    byte[] bytesUsed = sb.toString().getBytes(PlCx.UTF8);
                    inputIndex += bytesUsed.length;

                    break;        
                }
                else {
                    // U0 mode
                    if (size < 0) {
                            while (inputIndex < input.length()) {
                                result.push( new PlInt( input.charAt(inputIndex++) ) );
                            }
                    }
                    else {
                        for (int j = 0; j < size; j++) {
                            if (inputIndex < input.length()) {
                                result.push( new PlInt( input.charAt(inputIndex++) ) );
                            }
                        }
                    }
                    break;        
                }
            }

            default:
            }
        }
        return result;
    }
    private static final int pack_size(String s, int pos) {
        int howMany = 0;
        while(s.length() > (pos + 1 + howMany) && java.lang.Character.isDigit(s.charAt(pos + 1 + howMany))) {
            ++howMany;
        }
        if(howMany != 0) {
            return java.lang.Integer.parseInt(s.substring(pos + 1, pos + 1 + howMany));
        }
        return 1;
    }
    private static final String pack_a(String s, int size) {
        if(s.length() >= size) {
            return s.substring(0,size);
        }
        String padding = new String(new char[size - s.length()]);
        return s + padding;    
    }
    private static final String unpack_a(String s, int size) {
        if(s.length() >= size) {
            return s.substring(0,size);
        }
        return s; 
    }
    private static final String pack_A(String s, int size) {
        if(s.length() >= size) {
            return s.substring(0,size);
        }
        String padding = new String(new char[size - s.length()]).replace('\0', ' ');
        return s + padding;    
    }
    private static final String unpack_A(String s, int size) {
        if(s.length() >= size) {
            return s.substring(0,size);
        }
        return s; 
    }
    private static final String pack_Z(String s, int size) {
        s = s.substring(0, java.lang.Math.min(size - 1, s.length()));
        return s +  new String(new char[size - s.length()]);
    }
    private static final String unpack_Z(String s, int size) {
        if(s.length() >= size) {
            return s.substring(0,size);
        }
        return s; 
    }
    private static final String pack_b(String s, int size) {
        s = s.substring(0, Math.min(size, s.length()));
        int wanted8strings = (size + 7) / 8;
        s += new String(new char[(wanted8strings * 8) - s.length()]).replace('\0', '0');
        StringBuilder input = new StringBuilder();
        for(int i = 0; i < s.length(); ++i) {
            if(s.codePointAt(i) % 2 == 1) {
                input.append("1");
            }
            else {
                input.append("0");
            }
        }
        StringBuilder result = new StringBuilder();
        s = input.toString();
        for(int i = 0; i < wanted8strings; ++i) {
            String part = s.substring(i * 8, i * 8 + 8);
            int first = java.lang.Integer.parseInt(new StringBuilder(part.substring(0,4)).reverse().toString(), 2);
            int second = java.lang.Integer.parseInt(new StringBuilder(part.substring(4,8)).reverse().toString(), 2);
            result.append(Character.toString((char)(first + second * 16)));
        }
        return result.toString();
    }
    private static final String unpack_b(String s, int size) {
        byte[] bytes = s.getBytes();
        StringBuilder result = new StringBuilder();
        byte mask = (byte)128;
        for(int i = 0; i < size; ++i) {
            byte b = bytes[i / 8];
            if((b & mask) > 0) {
                result.append("1");
            } else {
                result.append("0");
            }
            if(mask == 1) {
                mask = (byte)128;
            } else {
                mask /= 2;
            }
        }
        return result.toString();
    }
    private static final String pack_B(String s, int size) {
        s = s.substring(0, Math.min(size, s.length()));
        int wanted8strings = (size + 7) / 8;
        s += new String(new char[(wanted8strings * 8) - s.length()]).replace('\0', '0');
        StringBuilder input = new StringBuilder();
        for(int i = 0; i < s.length(); ++i) {
            if(s.codePointAt(i) % 2 == 1) {
                input.append("1");
            }
            else {
                input.append("0");
            }
        }
        StringBuilder result = new StringBuilder();
        s = input.toString();
        for(int i = 0; i < wanted8strings; ++i) {
            String part = s.substring(i * 8, i * 8 + 8);
            int ascii = java.lang.Integer.parseInt(part, 2);
            result.append(Character.toString((char)ascii));
        }
        return result.toString();
    }
    private static final String pack_h(String s, int size) {
        int index  = 0;
        if(s.length() < size * 2) {
            s += new String(new char[size * 2 - s.length()]).replace('\0', '0');
        }
        StringBuilder result = new StringBuilder();
        while(index < size) {
            String part = s.substring(index + 1, index + 2) + s.substring(index, index + 1);
            int ascii = java.lang.Integer.parseInt(part, 16);
            result.append(Character.toString((char)ascii));
            index += 2;
        }
        return result.toString();
    }
    private static final String pack_H(String s, int size) {
        int index  = 0;
        if(s.length() < size * 2) {
            s += new String(new char[size * 2 - s.length()]).replace('\0', '0');
        }
        StringBuilder result = new StringBuilder();
        while(index < size) {
            String part = s.substring(index, index + 2);
            int ascii = java.lang.Integer.parseInt(part, 16);
            result.append(Character.toString((char)ascii));
            index += 2;
        }
        return result.toString();
    }
    private static String pack_c(String s) {
        try {
            int ascii = java.lang.Integer.parseInt(s) % 128;
            return Character.toString((char)ascii);
        } catch(Exception e) {
            return "";
        }
    }
    private static String pack_C(String s) {
        try {
            int ascii = (java.lang.Integer.parseInt(s) + 256) % 256;
            return Character.toString((char)ascii);
        } catch(Exception e) {
            return "";
        }
    }
    private static String pack_number_2_string(long value, int size, boolean signed) {
        StringBuilder result = new StringBuilder();
        for(int i = 0; i < size; ++i) {
            result.append((char)((value / (int)Math.pow(2,8*i)) % 256));
        }
        return result.toString();        
    }
    private static String pack_s(long s) {
        return pack_number_2_string(s, 2, true);
    }
    private static String pack_S(long s) {
        return pack_number_2_string(s, 2, false);
    }
    public static final String pack_l(long s) {
        return pack_number_2_string(s, 4, true);
    }
    public static final String pack_L(long s) {
        return pack_number_2_string(s, 4, false);
    }
    public static final String pack_q(long s) {
        return pack_number_2_string(s, 8, true);
    }
    public static final String pack_Q(long s) {
        return pack_number_2_string(s, 8, false);
    }
    public static final String pack_i(long s) {
        return pack_number_2_string(s, 4, true);
    }
    public static final String pack_I(long s) {
        return pack_number_2_string(s, 4, false);
    }
    public static final String pack_n(long s) {
        return new StringBuilder(pack_number_2_string(s, 2, false)).reverse().toString();
    }
    public static final String pack_N(long s) {
        return new StringBuilder(pack_number_2_string(s, 4, false)).reverse().toString();
    }
    public static final String pack_v(long s) {
        return pack_number_2_string(s, 2, false);
    }
    public static final String pack_V(long s) {
        return pack_number_2_string(s, 4, false);
    }
    public static final String pack_j(long s) {
        return pack_number_2_string(s, 8, true);
    }
    public static final String pack_J(long s) {
        return pack_number_2_string(s, 8, false);
    }
    public static final String pack_f(double d) {
        float f = (float)d;
        int intBits = java.lang.Float.floatToRawIntBits(f); 
        char one = (char)(intBits / (int)Math.pow(2, 24));
        char two = (char)((intBits / (int)Math.pow(2, 16)) % 256);
        char three = (char)((intBits / (int)Math.pow(2, 8)) % 256);
        char four = (char)(intBits % 256);
        StringBuilder result = new StringBuilder();
        result.append(Character.toString(four));
        result.append(Character.toString(three));
        result.append(Character.toString(two));
        result.append(Character.toString(one));
        return result.toString();        
    }
    public static final String pack_d(double d) {
        long intBits = java.lang.Double.doubleToRawLongBits(d);
        char one =  (char)(intBits / (long)Math.pow(2, 56));
        char two = (char)((intBits / (long)Math.pow(2, 48)) % 256);
        char three = (char)((intBits / (long)Math.pow(2, 40)) % 256);
        char four = (char)((intBits / (long)Math.pow(2, 32)) % 256);
        char five = (char)((intBits / (long)Math.pow(2, 24)) % 256);
        char six = (char)((intBits / (long)Math.pow(2, 16)) % 256);
        char seven = (char)((intBits / (long)Math.pow(2, 8)) % 256);
        char eight = (char)(intBits % 256);
        StringBuilder result = new StringBuilder();
        result.append(eight);
        result.append(seven);
        result.append(six);
        result.append(five);
        result.append(four);
        result.append(three);
        result.append(two);
        result.append(one);
        return result.toString();        
    }
    private static StringBuilder pack_pointers = new StringBuilder();
    private static Map<Long, Integer> pack_pointers_size = new HashMap<Long, Integer>();
    private static final long pack_pointers_magic_value = 654321;
    public static final String pack_p(String s) {
        long pointer = pack_pointers.length() + pack_pointers_magic_value;
        pack_pointers.append(s);

        pack_pointers_size.put(pointer, s.length());
        return pack_q((long)(pointer));
    }
    public static final String pack_u(String s) {
        int index = 0;
        StringBuilder result = new StringBuilder();
        StringBuilder line = new StringBuilder();
        int tooMany = 0;
        while(s.length() > index * 3) {
            String cur = s.substring(index * 3, Math.min(index * 3 + 3, s.length()));
            while(cur.length() < 3) {
                ++tooMany;
                cur += '\0';
            }
            byte[] bytes = cur.getBytes();
            char value1 = (char)((bytes[0] >> 2) + 32);
            char value2 = (char)(((bytes[0] & 3) << 4) + (bytes[1] >> 4) + 32);
            char value3 = (char)(((bytes[1] & 15) << 2) + (bytes[2] >> 6) + 32);
            char value4 = (char)((bytes[2] & 63) + 32);

            line.append(value1);
            line.append(value2);
            line.append(value3);
            line.append(value4);
            
            if(line.length() == 60 && index != 0) {
                line.insert(0, (char)(32 + (45 - tooMany)));
                line.append("\n");
                result.append(line.toString());
                line = new StringBuilder();
            }
            ++index;
        }
        if(line.length() > 0) {
            line.insert(0, (char)(32 + ((index * 3 - tooMany) % 45)));
            line.append("\n");
            result.append(line);
        }

        return result.toString().replaceAll(" ", "`");
    }
    public static final String pack_w(String[] s, int size) {
        java.math.BigInteger max_byte = new java.math.BigInteger("128");
        StringBuilder result = new StringBuilder();
        for(int i = 0; i < size; ++i) {
            java.math.BigInteger current = new java.math.BigInteger(s[i]);
            if(current.signum() < 0) {
                throw new PlDieException(new PlString("Cannot compress negative numbers in pack"));
            }
            while(current.compareTo(max_byte) > 0) {
                int part = current.mod(max_byte).intValue();
                result.append((char) (part + 128));
                current = current.divide(max_byte);
            }
            result.append((char)current.intValue());
        }

        return result.toString();
    }
    public static final String pack_x(int size) {
        return new String(new char[size]);
    }
    public static final PlObject times(int want, PlArray List__) {
        ThreadMXBean bean = ManagementFactory.getThreadMXBean( );
        long cpu = bean.isCurrentThreadCpuTimeSupported( ) ?
            bean.getCurrentThreadCpuTime( ) : 0L;
        long user = bean.isCurrentThreadCpuTimeSupported( ) ?
            bean.getCurrentThreadUserTime( ) : 0L;
        long system = cpu - user;
        return new PlArray(
            new PlDouble( user / 1000000000.0 ),
            new PlDouble( system / 1000000000.0 ),
            new PlDouble( 0.0 ),
            new PlDouble( 0.0 )
        );
    }
    public static final PlObject localtime(int want, PlArray List__) {
        PlArray res = new PlArray();
		ZonedDateTime date;
        if (List__.to_boolean()) {
        	long arg = List__.aget(0).to_long();
            date = Instant.ofEpochSecond(arg).atZone(ZoneId.systemDefault());
        }
        else {
			date = ZonedDateTime.now();
        }
        if (want == PlCx.SCALAR) {
            return new PlString(date.format( DateTimeFormatter.RFC_1123_DATE_TIME ));
        }
        //      0    1    2     3     4    5     6     7     8
        //   ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)
		res.push(date.getSecond());
		res.push(date.getMinute());
		res.push(date.getHour());
		res.push(date.getDayOfMonth());
		res.push(date.getMonth().getValue() - 1);
		res.push(date.getYear() - 1900);
		res.push(date.getDayOfWeek().getValue());
		res.push(date.getDayOfYear() - 1);
		res.push(
            date.getZone().getRules().isDaylightSavings(date.toInstant()) ? PlCx.INT1 : PlCx.INT0
        );
        return res;
    }
    public static final PlObject gmtime(int want, PlArray List__) {
        PlArray res = new PlArray();
		ZonedDateTime date;
        if (List__.to_boolean()) {
        	long arg = List__.aget(0).to_long();
            date = Instant.ofEpochSecond(arg).atZone(ZoneId.of("UTC"));
        }
        else {
			date = ZonedDateTime.now(ZoneOffset.UTC);
        }
        if (want == PlCx.SCALAR) {
            return new PlString(date.format( DateTimeFormatter.RFC_1123_DATE_TIME ));
        }
        //      0    1    2     3     4    5     6     7     8
        //   ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)
		res.push(date.getSecond());
		res.push(date.getMinute());
		res.push(date.getHour());
		res.push(date.getDayOfMonth());
		res.push(date.getMonth().getValue() - 1);
		res.push(date.getYear() - 1900);
		res.push(date.getDayOfWeek().getValue());
		res.push(date.getDayOfYear() - 1);
		res.push(
            date.getZone().getRules().isDaylightSavings(date.toInstant()) ? PlCx.INT1 : PlCx.INT0
        );
        return res;
    }
    public static final PlObject time(int want, PlArray List__) {
        return new PlInt( (long)Math.floor(System.currentTimeMillis() * 0.001 + 0.5));
    }
    public static final PlObject sleep(int want, PlArray List__) {
        long s = ((Double)(List__.shift().to_double() * 1000)).longValue();
        try {
            TimeUnit.MILLISECONDS.sleep(s);
        } catch (InterruptedException e) {
            //Handle exception
            PlCORE.die("interrupted");
        }
        return new PlDouble(s / 1000.0);
    }
    public static final PlObject system(int want, PlArray List__) {
        // TODO - see perldoc -f system
        try {
            Process p;
            if (List__.to_int() > 1) {
                String[] args = new String[List__.to_int()];
                int i = 0;
                for (PlObject s : List__.a) {
                    args[i++] = s.toString();
                }
                p = Runtime.getRuntime().exec(args);
            }
            else {
                p = Runtime.getRuntime().exec(List__.aget(0).toString());
            }
            // String s = null;
            // BufferedReader stdInput = new BufferedReader(new InputStreamReader(p.getInputStream()));
            // BufferedReader stdError = new BufferedReader(new InputStreamReader(p.getErrorStream()));
            // System.out.println("STDOUT\n");
            // while ((s = stdInput.readLine()) != null) {
            //     System.out.println("  " + s);
            // }
            // System.out.println("STDERR\n");
            // while ((s = stdError.readLine()) != null) {
            //     System.out.println("  " + s);
            // }
            int ret = p.waitFor() * 256;
            PlV.sset("main::?", new PlInt(ret));
            return new PlInt(ret);
        } catch (InterruptedException e) {
            //Handle exception
            return PlCORE.die("interrupted");
        }
        catch (IOException e) {
            // System.out.println("IOexception: ");
            // e.printStackTrace();
            PlV.sset("main::!", new PlStringLazyError(e));
            return PlCx.MIN1;
        }
    }
    public static final PlObject qx(int want, PlArray List__) {
        // TODO - see perldoc -f qx
        try {
            String[] args = new String[List__.to_int()];
            int i = 0;
            for (PlObject s : List__.a) {
                args[i++] = s.toString();
            }
            PlArray res = new PlArray();
            String s = null;
            Process p = Runtime.getRuntime().exec(args);
            // ??? set PlCx.UTF8
            BufferedReader stdInput = new BufferedReader(new InputStreamReader(p.getInputStream()));
            // System.out.println("STDOUT\n");
            while ((s = stdInput.readLine()) != null) {
                // System.out.println("  " + s);
                res.push(s + "\n");
            }
            // BufferedReader stdError = new BufferedReader(new InputStreamReader(p.getErrorStream()));
            // System.out.println("STDERR\n");
            // while ((s = stdError.readLine()) != null) {
            //     System.out.println("  " + s);
            // }
            if (want == PlCx.LIST) {
                return res;
            }
            res.unshift(PlCx.EMPTY);
            return join(want, res);
        }
        catch (IOException e) {
            // System.out.println("IOexception: ");
            // e.printStackTrace();
            return PlCx.UNDEF;
        }
    }
    public static final PlObject caller(int wantarray, PlArray List__) {
        PlObject arg = List__.aget(0);
        boolean argDefined = !arg.is_undef();
        int item = arg.to_int();

        PlArray callerName = PlV.array_get("Perlito5::CALLER");
        if (callerName.length_of_array().to_boolean()) {
            // maybe we are inside an import() subroutine

            PlObject arr = callerName.aget(0);  // XXX this should be "item"
                                            // XXX FIXME TODO - workaround for "export to level"
            if (arr.is_arrayref()) {
                if (wantarray == PlCx.LIST) {
                    return arr.array_deref_strict();
                }
                return arr.aget(0);
            }
            // fallback to normal caller()
        };

        String fullName = "";

        // A StackTraceElement has getClassName(), getFileName(), getLineNumber() and getMethodName().
        // The last element of the array represents the bottom of the stack,
        // which is the least recent method invocation in the sequence.
        callerName = new PlArray();
        PlArray codeRef = new PlArray();
        Thread t = Thread.currentThread();
        StackTraceElement[] stackTraceElements = t.getStackTrace();
        for (StackTraceElement elem : stackTraceElements) {
            // PlCORE.say(
            //     elem.getClassName()  + " \t" +
            //     elem.getMethodName() + " \t" +
            //     elem.getFileName()   + " \t" +
            //     elem.getLineNumber()
            // );
            if (elem.getMethodName().equals("apply")) {
                // stack trace element comes from PlClosure.apply()
                // TODO - move this inner loop outside, this is very expensive
                // TODO - this code doesn't account for inner-subs - it might match an outer sub instead
                // TODO - this code doesn't account for package name changes inside a sub
                // TODO - this code skips anonymous subroutines
                // TODO - the "perlSubName" resolution is ambiguous,
                //        because a sub can be exported to several namespaces:
                //        Exporter::import and MyClass::import can point to the same Java code
                // this loop does a symbol table scan - PlV.cvar
              SCAN_SUBNAME:
                for (PlObject perlSubName : (PlArray)PlCORE.keys(PlCx.LIST, PlV.cvar)) {
                    PlObject value = PlV.cget_no_autoload(perlSubName.toString());
                    if (value.is_lvalue()) {
                        value = value.get();
                    }
                    if (value.is_coderef()) {
                        PlClosure code = (PlClosure)value;
                        if ( code.javaClassName() != null &&
                             elem.getClassName().equals(code.javaClassName()) &&
                             elem.getLineNumber() > code.firstLineNumber() &&
                             elem.getLineNumber() < code.lastLineNumber() &&
                             !code.pkg_name.startsWith("Perlito5::")
                        ) {
                            // PlCORE.say(
                            //     elem.getClassName()  + " \t" +
                            //     elem.getMethodName() + " \t" +
                            //     elem.getFileName()   + " \t" +
                            //     elem.getLineNumber()
                            // );
                            // PlCORE.say("\tPerl sub &" + perlSubName.toString());
                            callerName.push(perlSubName);
                            codeRef.push(value);
                            break SCAN_SUBNAME;
                        }
                    }
                }
            }
        }

        PlObject plCoderef = codeRef.aget(item);
        PlObject plCallerCoderef = codeRef.aget(item + 1);
        PlObject packageName = PlCx.UNDEF;
        if (plCallerCoderef.is_coderef()) {
            packageName = new PlString(((PlClosure)plCallerCoderef).pkg_name);
        }
        else if (plCoderef.is_coderef()) {
            packageName = new PlString("main");
        }

        if (wantarray != PlCx.LIST) {
			// caller() in scalar or void context
            return packageName;
        }

        PlObject plFullName = callerName.aget(item);    // "subroutine" comes from the current level
        PlObject lineNumber = PlCx.UNDEF;
        String fileName = "";
        if (plCoderef.is_coderef()) {
            lineNumber = new PlInt(((PlClosure)plCoderef).perlLineNumber());
            fileName   = ((PlClosure)plCoderef).perlFileName();
        }


        if (!argDefined) {
			// caller() in list context, without args
            return new PlArray( packageName, new PlString(fileName), lineNumber );
        }

		// caller(EXPR) in list context, with args
        // TODO - add other components
        //   #  0         1          2      3            4
        //   ($package, $filename, $line, $subroutine, $hasargs,
        //   #  5          6          7            8       9         10
        //   $wantarray, $evaltext, $is_require, $hints, $bitmask, $hinthash)
        return new PlArray( packageName, new PlString(fileName), lineNumber, plFullName );
    }
}

EOT

} # end of emit_java()

1;

__END__

=pod

=head1 NAME

Perlito5::Java::CORE

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
