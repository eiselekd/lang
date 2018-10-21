map = { 1 : 'f1.html',
        2 : 'f2.html' };
e = "1:30 2:23 : test"

var re = /([0-9]+):([0-9]+)/;

do {
    m = re.exec(e);
    if (m) {
        console.log(m, m[1], m[2]);
        e = e.substr(m.index+m[0].length);
    }
} while (m);
