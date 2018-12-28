use v5;

package Perlito5::JavaScript2::IO;

sub emit_javascript2 {

    return <<'EOT';
//
//
// lib/Perlito5/JavaScript2/IO.js
//
// I/O functions for "Perlito" Perl5-in-JavaScript2
//
// AUTHORS
//
// Flavio Soibelmann Glock  fglock@gmail.com
//
// COPYRIGHT
//
// Copyright 2009, 2010, 2011, 2012 by Flavio Soibelmann Glock and others.
//
// This program is free software; you can redistribute it and/or modify it
// under the same terms as Perl itself.
//
// See http://www.perl.com/perl/misc/Artistic.html

var isNode = typeof require != "undefined";
if (isNode) {

    var fs = require("fs");

    p5typeglob_set("Perlito5::IO", "print", function (filehandle, List__, p5want) {
        try {
            var v = filehandle;
            var pkg;
            if (CORE.ref([v])) {
                // looks like a filehandle
                pkg = v;
            }
            else {
                // looks like a package name
                pkg = p5make_package(v);
            }
            if (!pkg.file_handle) {
                pkg.file_handle = {};
            }
            var handle_id = pkg.file_handle.id;
            if (handle_id == 1) {
                for (var i = 0; i < List__.length; i++) {
                    process.stdout.write(p5str(List__[i]));
                }
            }
            else if (handle_id == 2) {
                for (var i = 0; i < List__.length; i++) {
                    process.stderr.write(p5str(List__[i]));
                }
            }
            else {
                for (var i = 0; i < List__.length; i++) {
                    fs.writeSync(handle_id, p5str(List__[i]));
                }
            }
            return 1;
        }
        catch(err) {
            p5pkg["main"]["v_!"] = err;
            return '';
        }
    } );

    var p5_extra_buffer_size = 100;
    p5typeglob_set("Perlito5::IO", "read", function (filehandle, List__, p5want) {
        try {
            var v = filehandle;
            var length = List__.shift();
            var pkg;
            if (CORE.ref([v])) {
                // looks like a filehandle
                pkg = v;
            }
            else {
                // looks like a package name
                pkg = p5make_package(v);
            }
            if (!pkg.file_handle) {
                pkg.file_handle = {};
            }
            var handle_id = pkg.file_handle.id;

            if (!pkg.file_handle.buffer) {
                // we don't have any data yet
                var length_wanted = length + 2 * p5_extra_buffer_size;
                var buffer = Buffer.alloc(length_wanted);
                var bytes_read = fs.readSync(handle_id, buffer, 0, length_wanted, null);
                if (bytes_read < length_wanted) {
                    pkg.file_handle.buffer_eof = 1;
                }
                pkg.file_handle.buffer = buffer;
                pkg.file_handle.buffer_start = 0;
                pkg.file_handle.buffer_end = bytes_read;
                pkg.file_handle.buffer_length = pkg.file_handle.buffer_end;
            }
            else if (pkg.file_handle.buffer_length > (length + p5_extra_buffer_size)) {
                // we have enough data
            }
            else if (!pkg.file_handle.buffer_eof) {
                // we have some data; append more data to the internal buffer
                var length_wanted = length + 2 * p5_extra_buffer_size;
                var buffer = Buffer.alloc(pkg.file_handle.buffer_length + length_wanted);
                pkg.file_handle.buffer.copy(buffer, 0, pkg.file_handle.buffer_start, pkg.file_handle.buffer_end);
                var bytes_read = fs.readSync(handle_id, buffer, pkg.file_handle.buffer_length, length_wanted, null);
                if (bytes_read < length_wanted) {
                    pkg.file_handle.buffer_eof = 1;
                }
                pkg.file_handle.buffer = buffer;
                pkg.file_handle.buffer_start = 0;
                pkg.file_handle.buffer_end = pkg.file_handle.buffer_length + bytes_read;
                pkg.file_handle.buffer_length = pkg.file_handle.buffer_end;
            }

            var s = pkg.file_handle.buffer.toString('utf-8', pkg.file_handle.buffer_start, pkg.file_handle.buffer_end).substr(0, length);

            // how many bytes we actually used
            var buffer_used = Buffer.byteLength(s, 'utf-8');

            pkg.file_handle.buffer_start = pkg.file_handle.buffer_start + buffer_used;
            pkg.file_handle.buffer_length = pkg.file_handle.buffer_length - buffer_used;

            if ( handle_id == 0) {
                // STDIN
                pkg.file_handle.buffer_eof = (s.length ? 0 : 1);
                pkg.file_handle.eof = (s.length ? 0 : 1);
            }
            else if ( pkg.file_handle.buffer_eof && pkg.file_handle.buffer_length <= 0 ) {
                pkg.file_handle.eof = 1;
            }

            return [s.length, s];
        }
        catch(err) {
            p5pkg["main"]["v_!"] = err;
            return [];
        }
    } );

    var p5ARGV = 0;
    (function (f) {
        p5typeglob_set("Perlito5::IO", "readline", f);
        p5typeglob_set("Perlito5::IO", "getline", f);
    })(function (List__, p5want) {
        var filehandle = List__.shift();

        if (p5want) {
            var out = [];
            while (1) {
                var s = p5pkg["Perlito5::IO"].readline([filehandle], 0);
                if (s == null) {
                    return out;
                }
                out.push(s);
            }
        }

        var v = filehandle;
        var pkg;
        if (CORE.ref([v])) {
            // looks like a filehandle
            pkg = v;
        }
        else {
            // looks like a package name
            pkg = p5make_package(v);
            if (v == "ARGV") {
                // ARGV is magical
                if (pkg.file_handle.id == null) {
                    if (!p5ARGV) {
                        if (p5pkg["main"]["List_ARGV"].length == 0) {
                            p5pkg["main"]["List_ARGV"].push('-');
                        }
                    }
                    p5ARGV = 1;
                    // TODO - open $ARGV[1], ...
                    var filename = p5pkg["main"]["List_ARGV"].shift();
                    CORE.open([ "ARGV", "<", filename ]) || CORE.die([ p5pkg["main"]["v_!"] ]);
                }
            }
        }
        if (!pkg.file_handle) {
            pkg.file_handle = {};
        }

        if (CORE.eof([v])) {
            return null;
        }

        var separator = p5pkg["main"]["v_/"];  // input record separator
        var buf = pkg.file_handle.readline_buffer;
        var pos;

        if (separator) {
            pos = buf.indexOf(separator);
            while ( pos < 0 && !pkg.file_handle.eof ) {
                var r = p5pkg["Perlito5::IO"].read(filehandle, [100]);
                buf = buf + r[1];
                pos = buf.indexOf(separator);
            }
        }
        else {
            // no separator
            pos = -1;
            while ( !pkg.file_handle.eof ) {
                var r = p5pkg["Perlito5::IO"].read(filehandle, [100]);
                buf = buf + r[1];
            }
        }

        if (pos < 0) {
            pkg.file_handle.readline_buffer = '';
            if (!buf.length) {
                pkg.file_handle.readline_buffer = '';
                pkg.file_handle.eof = 1;
                return null
            }
            return buf;
        }
        var s = buf.substr(0, pos + separator.length);
        pkg.file_handle.readline_buffer = buf.substr(pos + separator.length);
        if (!s.length) {
            pkg.file_handle.readline_buffer = '';
            pkg.file_handle.eof = 1;
            return null
        }
        return s;
    });

    p5typeglob_set("Perlito5::IO", "close", function (filehandle, List__, p5want) {
        try {
            var v = filehandle;
            var pkg;
            if (CORE.ref([v])) {
                // looks like a filehandle
                pkg = v;
            }
            else {
                // looks like a package name
                pkg = p5make_package(v);
            }
            if (!pkg.file_handle) {
                pkg.file_handle = {};
            }
            var handle_id = pkg.file_handle.id;
            if (handle_id == 1) {
                process.stdout.close();
            }
            else if (handle_id == 2) {
                process.stderr.close();
            }
            else {
                fs.closeSync(handle_id);
            }
            pkg.file_handle.id = null;
            return 1;
        }
        catch(err) {
            p5pkg["main"]["v_!"] = err;
            return '';
        }
    } );

    CORE.eof = function(List__) {
        try {
            var filehandle = List__.shift();
            var v = filehandle;
            var pkg;
            if (CORE.ref([v])) {
                // looks like a filehandle
                pkg = v;
            }
            else {
                // looks like a package name
                pkg = p5make_package(v);
            }
            if (!pkg.file_handle) {
                pkg.file_handle = {};
            }
            var handle_id = pkg.file_handle.id;
            if (handle_id == null) {
                return 1;  // file is not open
            }
            return pkg.file_handle.eof && pkg.file_handle.readline_buffer.length == 0;
        }
        catch(err) {
            p5pkg["main"]["v_!"] = err;
            return '';
        }
    };

    CORE.open = function(List__) {
        try {
            var filehandle = List__.shift();
            var flags = List__.shift();
            var path;
            if (List__.length) {
                path = List__.shift();
            }
            else {
                // 2-argument open
                var re = new RegExp("^([<>+|]*)(.*)$", "");
                var capture = re.exec(flags);
                flags = capture[1];
                path = capture[2];
            }
            var v = filehandle;
            var pkg;
            if (CORE.ref([v])) {
                // looks like a filehandle
                pkg = v;
            }
            else {
                // looks like a package name
                pkg = p5make_package(v);
                if (path == "-") {
                    if (flags == '>' || flags == '>>' || flags == '+>' || flags == '+>>') {
                        pkg.file_handle = p5pkg["STDOUT"].file_handle;
                    }
                    else {
                        pkg.file_handle = p5pkg["STDIN"].file_handle;
                    }
                }
            }
            if (!pkg.file_handle) {
                pkg.file_handle = {};
            }
            var handle_id = pkg.file_handle.id;
            if (handle_id != null) {
                if (handle_id < 2) {
                    return 1;   // STDIN, STDOUT, STDERR
                }
                p5pkg["Perlito5::IO"].close(filehandle, []);
            }
            if (flags == '>') {
                flags = 'w'
            }
            else if (flags == '>>') {
                flags = 'a'
            }
            else if (flags == '<' || flags == '' || flags == '<:encoding(UTF-8)') {
                flags = 'r'
            }
            else if (flags == '+>') {
                flags = 'w+'
            }
            else if (flags == '+>>') {
                flags = 'a+'
            }
            else if (flags == '+<') {
                flags = 'r+'
            }
            else {
                CORE.die([ "don't know what to do with MODE '", flags, "'" ]);
            }
            var id = fs.openSync(path, flags);
            pkg.file_handle = { id : id, readline_buffer : '' };
            return 1;
        }
        catch(err) {
            p5pkg["main"]["v_!"] = err;
            return '';
        }
    };

    var p5atime = function(s) {
        try {
            var stat = fs.statSync(s); return stat["atime"];
        }
        catch(err) {
            return '';
        }
    };
    var p5mtime = function(s) {
        try {
            var stat = fs.statSync(s); return stat["mtime"];
        }
        catch(err) {
            return '';
        }
    };
    var p5ctime = function(s) {
        try {
            var stat = fs.statSync(s); return stat["ctime"];
        }
        catch(err) {
            return '';
        }
    };
    var p5size = function(s) {
        try {
            var stat = fs.statSync(s); return stat["size"];
        }
        catch(err) {
            return '';
        }
    };
    var p5is_file = function(s) {
        try {
            var stat = fs.statSync(s); return stat.isFile() ? 1 : 0;
        }
        catch(err) {
            return '';
        }
    };
    var p5is_directory = function(s) {
        try {
            var stat = fs.statSync(s); return stat.isDirectory() ? 1 : 0;
        }
        catch(err) {
            return '';
        }
    };
    var p5file_exists = function(s) {
        return p5is_file(s) || p5is_directory(s);
    };
    var p5is_pipe = function(s) {
        try {
            var stat = fs.statSync(s);
            return stat.isFIFO() ? 1 : "";
        }
        catch(err) {
            try {
                var filehandle = s;
                var v = filehandle;
                var pkg;
                if (CORE.ref([v])) {
                    // looks like a filehandle
                    pkg = v;
                }
                else {
                    // looks like a package name
                    pkg = p5make_package(v);
                }
                if (!pkg.file_handle) {
                    pkg.file_handle = {};
                }
                var handle_id = pkg.file_handle.id;
                if (handle_id == 0) {
                    return process.stdin.isTTY ? "" : 1;
                }
                else if (handle_id == 1) {
                    return process.stdout.isTTY ? "" : 1;
                }
                else if (handle_id == 2) {
                    return process.stderr.isTTY ? "" : 1;
                }
            }
            catch(err) {
            }
        }
        return '';
    };

    CORE.binmode = function(List__) {
        try {
            // TODO
            return 1;
        }
        catch(err) {
            p5pkg["main"]["v_!"] = err;
            return '';
        }
    };

    CORE.rmdir = function(List__) {
        try {
            fs.rmdir(p5str(List__[0]));
            return 1;
        }
        catch(err) {
            p5pkg["main"]["v_!"] = err;
            return '';
        }
    };

    CORE.chdir = function(List__) {
        try {
            process.chdir(p5str(List__[0]));
            return 1;
        }
        catch(err) {
            p5pkg["main"]["v_!"] = err;
            return '';
        }
    };

    CORE.exit = function(List__) {
        process.exit(List__[0]);
    };

    CORE.rename = function(List__) {
        try {
            fs.renameSync(p5str(List__[0]), p5str(List__[1]));
            return 1;
        }
        catch(err) {
            p5pkg["main"]["v_!"] = err;
            return '';
        }
    };

    CORE.unlink = function(List__) {
        var count = 0;
        try {
            for(var i = 0; i < List__.length; i++) {
                fs.unlinkSync(p5str(List__[i]));
                count++;
            }
            return count;
        }
        catch(err) {
            p5pkg["main"]["v_!"] = err;
            return count;
        }
    };

} else {
    // not running in node.js
    p5typeglob_set("Perlito5::IO", "print", function (filehandle, List__, p5want) {
        var s = "";
        for (var i = 0; i < List__.length; i++) {
            s = s + p5str(List__[i]);
        }
        if (console && typeof console.log === 'function') {
            console.log(s);
        }
        else if (typeof write === 'function') {
            // d8 shell uses "write"
            write(s);
        }
        else if (typeof print === 'function') {
            // Rhino uses "print"
            print(s);
        }
        else {
            alert(s);
        }
        return 1;
    });
}

p5typeglob_set("Perlito5::IO", "say", function (filehandle, List__, p5want) {
    p5pkg['Perlito5::IO'].print( filehandle, List__);
    p5pkg['Perlito5::IO'].print( filehandle, ["\n"]);
    return 1;
} );
p5typeglob_set("Perlito5::IO", "printf", function (filehandle, List__, p5want) {
    p5pkg["Perlito5::IO"].print( filehandle, CORE.sprintf(List__));
    return 1;
} );

CORE.select = function(List__) {
    if (List__.length == 1) {
        var v = List__[0];
        p5pkg["Perlito5"].v_SELECT = v;
    }
    return p5pkg["Perlito5"].v_SELECT;
};

CORE.die = function(List__) {
    var i;
    var s = "";
    for (var i = 0; i < List__.length; i++) {
        s = s + p5str(List__[i]);
    }
    if (s.substr(-1, 1) != "\n") {
        try {
            if (s == "") {
                s = "Died";
            }
            s = s + " at " + p5pkg["Perlito5"].v_FILE_NAME + " line " + p5pkg["Perlito5"].v_LINE_NUMBER;
            s = s + "\n" + new Error().stack + "\n";
        }
        catch(err) { }
    }
    p5pkg["main"]["v_@"] = s;
    throw(new p5_error("die", s));
};

CORE.say = function(List__) {
    return p5pkg['Perlito5::IO'].say( 'STDOUT', List__);
};
CORE.print = function(List__) {
    return p5pkg['Perlito5::IO'].print( 'STDOUT', List__);
};
CORE.printf = function(List__) {
    return p5pkg['Perlito5::IO'].printf( 'STDOUT', List__);
};
CORE.readline = function(List__, p5want) {
    return p5pkg['Perlito5::IO'].readline(List__, p5want);
};

CORE.warn = function(List__) {
    var i;
    var s = "";
    for (var i = 0; i < List__.length; i++) {
        s = s + p5str(List__[i]);
    }
    if (s.substr(-1, 1) != "\n") {
        try {
            if (s == "") {
                s = "Warning: something's wrong";
            }
            s = s + " at " + p5pkg["Perlito5"].v_FILE_NAME + " line " + p5pkg["Perlito5"].v_LINE_NUMBER;
            s = s + "\n" + new Error().stack + "\n";
        }
        catch(err) { }
    }
    p5pkg['Perlito5::IO'].print( 'STDERR', [s]);
};


EOT
} # end of emit_javascript2()

1;

