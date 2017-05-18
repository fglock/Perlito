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
            if (argCount == 0) {
                PlCORE.die("TODO - not implemented: single argument open()");
            }
            else if (argCount == 1) {

                if (List__.aget(0).ref().str_eq(new PlString("SCALAR")).to_boolean()) {
                    PlObject o = List__.aget(0).scalar_deref("main");
                    InputStream is = new PlStringInputStream(o);
                    fh.reader = new BufferedReader(new InputStreamReader(is, "UTF-8"));
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
                if (s.length() > 1 && s.substring(0, 2) == ">>") {
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
                    InputStream is = new PlStringInputStream(o);
                    fh.reader = new BufferedReader(new InputStreamReader(is, "UTF-8"));
                    fh.reader.mark(o.toString().length());
                    fh.outputStream = null;
                    return PlCx.INT1;
                }

                s = List__.aget(1).toString();
            }
            path = PlV.path.resolve(s).toRealPath();
            // PlCORE.say("path " + path.toString());
            if (mode.equals("<") || mode.equals("")) {
                // TODO: charset
                fh.reader = Files.newBufferedReader(path, PlCx.UTF8);
                fh.outputStream = null;
            }
            else if (mode.equals("<:encoding(UTF-8)")) {
                fh.reader = Files.newBufferedReader(path, PlCx.UTF8);
                fh.outputStream = null;
            }
            else if (mode.equals(">")) {
                // TODO: charset
                fh.reader = null;
                fh.outputStream = new PrintStream(Files.newOutputStream(path, StandardOpenOption.CREATE));
            }
            else if (mode.equals(">>")) {
                // TODO: charset
                fh.reader = null;
                fh.outputStream = new PrintStream(Files.newOutputStream(path, StandardOpenOption.CREATE, StandardOpenOption.APPEND));
            }
            else {
                PlCORE.die("TODO - not implemented: open() mode '" + mode + "'");
            }
        }
        catch(IOException e) {
            PlV.sset("main::!", new PlString(e.getMessage()));
            return PlCx.UNDEF;
        }
        return PlCx.INT1;
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
        }
        catch(IOException e) {
            PlV.sset("main::!", new PlString(e.getMessage()));
            return PlCx.UNDEF;
        }
        return PlCx.INT1;
EOT
    opendir => <<'EOT',
        try {
            String s = List__.aget(0).toString();
            Path path = PlV.path.resolve(s).toRealPath();

            fh.directoryStream = Files.newDirectoryStream(path);
            fh.directoryIterator = fh.directoryStream.iterator();
        }
        catch(IOException e) {
            PlV.sset("main::!", new PlString(e.getMessage()));
            return PlCx.UNDEF;
        }
        return PlCx.INT1;
EOT
    readdir => <<'EOT',
        Iterator<Path> iter = fh.directoryIterator;
        if (want == PlCx.LIST) {
            // read all lines
            PlArray res = new PlArray();
            while (iter.hasNext()) {
                res.push(new PlString(iter.next().getFileName().toString()));
            }
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
            PlV.sset("main::!", new PlString(e.getMessage()));
            return PlCx.UNDEF;
        }
        return PlCx.INT1;
EOT
    print => <<'EOT',
        for (int i = 0; i < List__.to_int(); i++) {
            fh.outputStream.print(List__.aget(i).toString());
        }
        return PlCx.INT1;
EOT
    say => <<'EOT',
        for (int i = 0; i < List__.to_int(); i++) {
            fh.outputStream.print(List__.aget(i).toString());
        }
        fh.outputStream.println("");
        return PlCx.INT1;
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
                    in  = PlCx.STDIN;
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
                PlV.sset("main::!", new PlString(e.getMessage()));
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
                PlV.sset("main::!", new PlString(e.getMessage()));
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
            PlV.sset("main::!", new PlString(e.getMessage()));
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
    public static final PlObject say(String s) {
        // say() shortcut for internal use
        return PlCORE.say(PlCx.VOID, PlCx.STDOUT, new PlArray(new PlString(s)));
    }
    public static final PlObject mkdir(int want, PlArray List__) {
        try {
            Path path = PlV.path.resolve(List__.aget(0).toString()).toRealPath();
            int mask = List__.aget(1).to_int();
            Set<PosixFilePermission> perms = PerlOp.MaskToPermissions(mask);
            FileAttribute<Set<PosixFilePermission>> attr = PosixFilePermissions.asFileAttribute(perms);
            Files.createDirectory(path, attr);
            return PlCx.INT1;
        }
        catch(IOException e) {
            PlV.sset("main::!", new PlString(e.getMessage()));
        }
        return PlCx.UNDEF;
    }
    public static final PlObject require(int want, PlObject file, boolean is_bareword) {
        // TODO - require-version
        if (is_bareword) {
            file = PlV.apply("Perlito5::Grammar::Use::modulename_to_filename", PlCx.SCALAR, new PlArray(file));
        }
        return PlV.apply("Perlito5::Grammar::Use::require", want, new PlArray(file));
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
            PlV.sset("main::!", new PlString(e.getMessage()));
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
            PlV.sset("main::!", new PlString(e.getMessage()));
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
            PlV.sset("main::!", new PlString(e.getMessage()));
        }
        return PlCx.UNDEF;
    }
    public static final PlObject exit(int want, PlArray List__) {
        int arg = List__.aget(0).to_int();
        System.exit(arg);
        return PlCx.UNDEF;
    }
    public static final PlObject warn(int want, PlArray List__) {
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
            PlCx.STDERR.outputStream.println(s);
        }
        return PlCx.INT1;
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
    public static final PlObject die(String s) {
        // die() shortcut
        return PlCORE.die(PlCx.VOID, new PlArray(new PlString(s)));
    }
    public static final PlString ref(int want, PlArray List__) {
        return List__.aget(0).ref();
    }
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
                plReg = new PlRegex(regs, Pattern.MULTILINE);
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
        try {
            if (s.startsWith("0x") || s.startsWith("0X")) {
                s = s.replace("_","");
                return new PlInt(Long.parseLong(s.substring(2), 16));
            } else if (s.startsWith("x") || s.startsWith("X")) {
                s = s.replace("_","");
                return new PlInt(Long.parseLong(s.substring(1), 16));
            } else if (s.startsWith("0b") || s.startsWith("0B")) {
                s = s.replace("_","");
                return new PlInt(Long.parseLong(s.substring(2), 2));
            } else if (s.startsWith("b") || s.startsWith("B")) {
                s = s.replace("_","");
                return new PlInt(Long.parseLong(s.substring(1), 2));
            } else {
                s = s.replace("_","");
                return new PlInt(Long.parseLong(s, 8));
            }
        } catch (NumberFormatException n) {
            
        } catch (Exception e) {
            // result = e.getMessage();
        }
        return new PlInt(0);
    }
    public static final PlObject sprintf(int want, PlObject List__) {
        String format = List__.aget(0).toString();
        // "%3s"
        int length = format.length();
        int offset = 0;
        int args_max = List__.to_int();
        int args_index = 0;
        Object args[] = new Object[args_max];
        for ( ; offset < length; ) {
            int c = format.codePointAt(offset);
            switch (c) {
                case '%':
                    offset++;
                    boolean scanning = true;
                    for ( ; offset < length && scanning ; ) {
                        c = format.codePointAt(offset);
                        switch (c) {
                            case '%':
                                scanning = false;
                                offset++;
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
                                        args[args_index] = List__.aget(args_index+1).to_int();
                                        break;
                                    case 'f': case 'e': case 'g':
                                    case 'E': case 'G':
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
            PlArray ret = new PlArray(List__);
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
        String template = List__.aget(0).toString();
        StringBuilder result = new StringBuilder();
        int index = 1;
        for(int i = 0; i < template.length(); ++i) {
            switch(template.charAt(i)) {
            case 'a':
            {
                int size = pack_size(template, i);
                result.append(pack_a(List__.aget(index).toString(), size));
                ++index;
                break;
            }
            case 'A':
            {    
                int size = pack_size(template, i);
                result.append(pack_A(List__.aget(index).toString(), size));
                ++index;
                break;
            }
            case 'Z':
            {
                int size = pack_size(template, i);
                result.append(pack_Z(List__.aget(index).toString(), size));
                ++index;
                break;
            }
            case 'b':
            {
                int size = pack_size(template, i);
                result.append(pack_b(List__.aget(index).toString(), size));
                ++index;
                break;
            }
            case 'B':
            {
                int size = pack_size(template, i);
                result.append(pack_B(List__.aget(index).toString(), size));
                ++index;
                break;
            }
            case 'h':
            {
                int size = pack_size(template, i);
                result.append(pack_h(List__.aget(index).toString(), size));
                ++index;
                break;
            }
            case 'H':
            {
                int size = pack_size(template, i);
                result.append(pack_H(List__.aget(index).toString(), size));
                ++index;
                break;        
            }
            case 'c':
            {
                result.append(pack_c(List__.aget(index).toString()));
                ++index;
                break;        
            }
            case 'C':
            {
                result.append(pack_C(List__.aget(index).toString()));
                ++index;
                break;        
            }
            case 'W':
            {
                result.append(pack_W(List__.aget(index).toString()));
                ++index;
                break;        
            }
            case 's':
            {
                result.append(pack_s(List__.aget(index).toString()));
                ++index;
                break;        
            }
            case 'S':
            {
                result.append(pack_S(List__.aget(index).toString()));
                ++index;
                break;        
            }
            case 'l':
            {
                result.append(pack_l(List__.aget(index).toString()));
                ++index;
                break;        
            }
            case 'L':
            {
                result.append(pack_L(List__.aget(index).toString()));
                ++index;
                break;        
            }
            case 'q':
            {
                result.append(pack_q(List__.aget(index).toString()));
                ++index;
                break;        
            }
            case 'Q':
            {
                result.append(pack_Q(List__.aget(index).toString()));
                ++index;
                break;        
            }
            case 'i':
            {
                result.append(pack_i(List__.aget(index).toString()));
                ++index;
                break;        
            }
            case 'I':
            {
                result.append(pack_I(List__.aget(index).toString()));
                ++index;
                break;        
            }
            case 'n':
            {
                result.append(pack_n(List__.aget(index).toString()));
                ++index;
                break;        
            }
            case 'N':
            {
                result.append(pack_N(List__.aget(index).toString()));
                ++index;
                break;        
            }
            case 'v':   
            {
                result.append(pack_v(List__.aget(index).toString()));
                ++index;
                break;        
            }
            case 'V':   
            {
                result.append(pack_V(List__.aget(index).toString()));
                ++index;
                break;        
            }
            case 'j':   
            {
                result.append(pack_j(List__.aget(index).toString()));
                ++index;
                break;        
            }
            case 'J':   
            {
                result.append(pack_J(List__.aget(index).toString()));
                ++index;
                break;        
            }
            case 'f':
            {
                result.append(pack_f(List__.aget(index).to_double()));
                ++index;
                break;        
            }
            case 'd':
            case 'F':
            {
                result.append(pack_d(List__.aget(index).to_double()));
                ++index;
                break;        
            }
            case 'p':
            {
                int size = pack_size(template, i);
                for(int k = 0; k < size; ++k) {
                    if(List__.aget(index + k).is_undef()) {
                        result.append(pack_q("0"));
                    
                    } else {
                        result.append(pack_p(List__.aget(index + k).toString()));
                    }
                }
                index += i;
            }
            case 'u':
            {
                result.append(pack_u(List__.aget(index).toString()));
                ++index;
                break;
            }
            case 'w':
            {
                int size = pack_size(template, i);
                String[] input = new String[size];
                for(int j = 0; j < size; ++j) {
                    input[j] = List__.aget(index + j).toString();
                }
                result.append(pack_w(input, size));
                index += size;
                break;        
            }
            case 'x':
            {
                int size = pack_size(template, i);
                result.append(pack_x(size));
                ++index;                
                break;        
            }
            case 'X':
            {
                int size = pack_size(template, i);
                int length = result.length();
                result.delete(Math.max(0,length - size), length);
                ++index;                
                break;        
            }
            case '@':
            {
                int size = pack_size(template, i);
                int length = result.length();
                if(size > length) {
                    result.append(new char[size - length]);
                }
                ++index;                
                break;        
            }
            case '.':
            {
               int size = List__.aget(index).to_int();
                int length = result.length();
                if(size > length) {
                    result.append(new char[size - length]);
                }
                ++index;                
                break;        
            }
            default:
            }
        }

        return new PlString(result.toString());
    }
    public static final PlObject unpack(int want, PlArray List__) {
        String template = List__.aget(0).toString();
        StringBuilder result = new StringBuilder();
        int index = 1;
        for(int i = 0; i < template.length(); ++i) {
            switch(template.charAt(i)) {
            case 'a':
            {
                int size = pack_size(template, i);
                result.append(unpack_a(List__.aget(index).toString(), size));
                ++index;
                break;
            }
            case 'A':
            {
                int size = pack_size(template, i);
                result.append(unpack_A(List__.aget(index).toString(), size));
                ++index;
                break;
            }
            case 'Z':
            {
                int size = pack_size(template, i);
                result.append(unpack_Z(List__.aget(index).toString(), size));
                ++index;
                break;
            }
            case 'b':
            {
                int size = pack_size(template, i);
                result.append(unpack_b(List__.aget(index).toString(), size));
                ++index;
                break;
            }
            default:
            }
        }
        return new PlString(result.toString());
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
    private static String pack_W(String s) {
        for(int i = 0; i < s.length(); ++i) {
            if(!java.lang.Character.isDigit(s.charAt(i))) {
                s = s.substring(0, i);
                break;
            }
        }
        int value = java.lang.Integer.parseInt(s);
        StringBuilder sb = new StringBuilder();
        sb.appendCodePoint(value);
        return sb.toString();
    }
    private static String pack_number_2_string(String s, int size, boolean signed) {
        for(int i = 0; i < s.length(); ++i) {
            if(!java.lang.Character.isDigit(s.charAt(i)) && !(s.charAt(i) == '-')) {
                s = s.substring(0, i);
                break;
            }
        }
        long value = java.lang.Long.parseLong(s);
        StringBuilder result = new StringBuilder();
        for(int i = 0; i < size; ++i) {
            result.append((char)((value / (int)Math.pow(2,8*i)) % 256));
        }
        return result.toString();        
    }
    private static String pack_s(String s) {
        return pack_number_2_string(s, 2, true);
    }
    private static String pack_S(String s) {
        return pack_number_2_string(s, 2, false);
    }
    public static final String pack_l(String s) {
        return pack_number_2_string(s, 4, true);
    }
    public static final String pack_L(String s) {
        return pack_number_2_string(s, 4, false);
    }
    public static final String pack_q(String s) {
        return pack_number_2_string(s, 8, true);
    }
    public static final String pack_Q(String s) {
        return pack_number_2_string(s, 8, false);
    }
    public static final String pack_i(String s) {
        return pack_number_2_string(s, 4, true);
    }
    public static final String pack_I(String s) {
        return pack_number_2_string(s, 4, false);
    }
    public static final String pack_n(String s) {
        return new StringBuilder(pack_number_2_string(s, 2, false)).reverse().toString();
    }
    public static final String pack_N(String s) {
        return new StringBuilder(pack_number_2_string(s, 4, false)).reverse().toString();
    }
    public static final String pack_v(String s) {
        return pack_number_2_string(s, 2, false);
    }
    public static final String pack_V(String s) {
        return pack_number_2_string(s, 4, false);
    }
    public static final String pack_j(String s) {
        return pack_number_2_string(s, 8, true);
    }
    public static final String pack_J(String s) {
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
        return pack_q(new Long(pointer).toString());
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
    public static final PlObject time(int want, PlArray List__) {
        return new PlInt( (long)Math.floor(System.currentTimeMillis() * 0.001 + 0.5));
    }
    public static final PlObject sleep(int want, PlArray List__) {
        long s = (new Double(List__.shift().to_double() * 1000)).longValue();
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
            String[] args = new String[List__.to_int()];
            int i = 0;
            for (PlObject s : List__.a) {
                args[i++] = s.toString();
            }
            String s = null;
            Process p = Runtime.getRuntime().exec(args);
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
            return PlCx.INT0;
        }
        catch (IOException e) {
            // System.out.println("IOexception: ");
            // e.printStackTrace();
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
