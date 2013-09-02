
// this is the algorithm for keeping the compile-time environment 
// incrementally set environment; and keep a pad stack

function print (s) { process.stdout.write(s) }

var prog = [

    '  var x = 3       ',

    '  print( "x=" + x + "\\n")  ',

    '  var y = 4;      ',

    '  print( "y=" + y + "\\n")  ',

    '  var z = 7;      ',

    '  y++            ',

    '  print( "y=" + y + "\\n")  ',

    '  var k = y + 1; print( "k="+ k+ "\\n")  '

];

var pad;
while (prog.length) {
    var line = prog.shift();
    cmd = '(function () { ' + 
                line + '; ' + 
                'return function (cmd) { ' + 
                    'return( eval(cmd) ) ' + 
                '} ' + 
           '}())';
    if (pad) {
        pad = pad(cmd)
    }
    else {
        pad = eval(cmd);
    }
}

