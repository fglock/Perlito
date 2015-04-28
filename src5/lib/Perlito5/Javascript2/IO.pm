use v5;

package Perlito5::Javascript2::IO;

sub emit_javascript2 {

    return <<'EOT';
//
//
// lib/Perlito5/Javascript2/IO.js
//
// I/O functions for "Perlito" Perl5-in-Javascript2
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

    p5typeglob_set("Perlito5::IO", "print", function (List__, p5want) {
        var i;
        var filehandle = List__.shift(); // TODO - use IO::FILE
        for (var i = 0; i < List__.length; i++) {
            process.stdout.write(p5str(List__[i]));
            // TODO - fs.writeSync(1, ...
        }
        return 1;
    } );

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

    CORE.chdir = function(List__) {
        try {
            process.chdir(p5str(List__[0]));
            return 1;
        }
        catch(err) {
            return '';
        }
    };

    CORE.close = function(List__) {
        try {
            fs.closeSync(p5str(List__[0]));
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

    p5typeglob_set("Perlito5::IO", "slurp", function(List__) {
        return fs.readFileSync(List__[0],"utf8");
    });

} else {
    // not running in node.js
    p5typeglob_set("Perlito5::IO", "print", function (List__, p5want) {
        var i;
        List__.shift(); // TODO - use IO::FILE
        for (var i = 0; i < List__.length; i++) {
            write(p5str(List__[i]));
        }
        return 1;
    });
    p5typeglob_set("Perlito5::IO", "slurp", function(List__) {
        var filename = List__[0];
        if (typeof readFile == "function") {
            return readFile(filename);
        }
        if (typeof read == "function") {
            // v8
            return read(filename);
        }
        p5pkg.CORE.die(["Perlito5::IO::slurp() not implemented"]);
    });
}

CORE.select = function(List__) {
    if (List__.length == 1) {
        var v = p5pkg["Perlito5"].Hash_SpecialFilehandle[List__[0]];
        p5pkg["Perlito5"].v_SELECT = v || List__[0];
    }
    return p5pkg["Perlito5"].v_SELECT;
};

CORE.die = function(List__) {
    var i;
    var s = "";
    for (var i = 0; i < List__.length; i++) {
        s = s + p5str(List__[i]);
    }
    try {
        s = s + "\n" + new Error().stack;
    }
    catch(err) { }
    p5pkg["main"]["v_@"] = "Died: " + s;
    throw(new p5_error("die", "Died: " + s));
};

CORE.say = function(List__) {
    CORE.print(List__);
    return CORE.print(["\n"]);
};

CORE.print = function(List__) {
    var i;
    for (var i = 0; i < List__.length; i++) {
        p5pkg['Perlito5::IO'].print([ 'STDOUT', List__[i] ]);
    }
    return 1;
};

CORE.warn = function(List__) {
    var i;
    var s = "";
    for (var i = 0; i < List__.length; i++) {
        s = s + p5str(List__[i]);
    }
    try {
        s = s + "\n" + new Error().stack;
    }
    catch(err) { }
    p5pkg['Perlito5::IO'].print([ 'STDERR', "Warning: " + s + "\n"]);
};


EOT
} # end of emit_javascript2()

1;

