var config = require('./c'),
    express = require('express'),
    http = require('http'),
    https = require('https'),
    fs = require('fs'),
    url = require('url'),
    bodyParser = require('body-parser'),
    mongodb = require('mongodb'),
    util = require('util');

var db;
var app, server;
var state = {};
var p_state = {};

function connect(c,cont) {
    
    var _config = {};

    function dump(l) {
	if (! _config['DEBUG'])
	    return;
	l.replace(/[\n]/gi, "\\n");
	if (l.length > 256) {
	    l = l.substr(0, 256) + "..."
	}
	console.log("[h]" + l);
    }

    for (i in config)
	_config[i] = config[i];
    for (i in c) 
	_config[i] = c[i];
    
    app = express();
    
    dump(" Config used 'c.json':" + JSON.stringify(_config));

    dump( "[+] backend" );
    var PORT = _config.PORT || 10300;
    var HOST = _config.HOST || '0.0.0.0';

    app.use(bodyParser.json());
    app.use('/static', express.static('./static'));
    app.get('/', function(req, res) {
	res.end('[h][/] backend');
    });
    app.use(function(req, res, next) {
	//Allow cors
	res.header("Access-Control-Allow-Origin", "*");
	next();
    });
    app.get('/status', function(req, res) {
	_config['states-cnt'] = -1;
	_config['events-cnt'] = -1;
	states_collection.count()
	    .then(function(c) {
		_config['states-cnt'] = c;
		events_collection.count()
		    .then(function(c) {
			_config['events-cnt'] = c;
			dump("[G] GET /status : " + JSON.stringify(_config));
			res.writeHead(200, {'Content-Type': 'application/json'});
			res.write(JSON.stringify(_config));
			return res.end("\n");
		    });
	    })
	    .catch(function(err) {
		response.end('404 Not Found')
	    });
    });

    /****************************************************************************/
    /* GET:/rest/latest
     * GET:/rest/put    
     */
    app.get('/rest/latest', function(req, res) {
	dump("[G] GET /rest/latest");
	cur = states_collection.findOne(
	    {},
	    {},
	    { sort: { 'timeStamp' :-1 } },
	    function(err, item) {
		if (err) {
		    return throwError(err, res);
		}
		return res.send(item);
	    });
    });

    app.put('/rest/put', function(req, res) {
	dump("[P] PUT /rest/put : " + JSON.stringify(req.body));
	return states_collection.save(req.body, function(err) {
	    dump("[P] done");
	    return res.end("");
	    if (err) {
		return throwError(err, res);
	    } else {
	    }
	});	
    });

    var options = {
    };

    var dbconnect = function(callback) {
	var url = _config['MONGODB_CONNECTION_URL'] + _config['DB'];
	dump(" connect to " + url);
	return mongodb.MongoClient.connect(url)
	    .then(function(_db) {
		db = _db;
		var cnt = 2;
		function test_cont(err) {
		    if (--cnt || err)
			callback(err);
		}
		dump("[M] mongodb-connect");
		_db.collection('states',function(err,c) {
		    states_collection = c;
		    dump(" collection 'states': found");
		    if (_config['DORESET']) {
			dump(" collection 'states': reset ");
			c.remove()
			    .then(function() {
				test_cont();
			    });
		    } else 
			test_cont();
		});
		_db.collection('events', function(err,c) {
		    events_collection = c;
		    dump(" collection 'events': found");
		    if (_config['DORESET']) {
			dump(" collection 'events': reset ");
			c.remove()
			    .then(function() {
				test_cont();
			    });
		    } else 
			test_cont();
		});
	    })
	    .catch(function(err) {
		dump(" error :" + err);
		throw callback(err);
	    });
    };

    dbconnect(function(err) {
	server = http.createServer(app).listen(PORT, HOST, null, function() {
	    dump(util.format(' Server listening on port %d in %s mode', this.address().port, app.settings.env));
	    cont();
	});
    });

    
    return app;
}

function disconnect(c,cont) {
    server.close();
    db.close();
}

if (typeof module === 'object' && module.exports) {
    module.exports = {
	'connect' : connect,
	'disconnect' : disconnect
    };
}
