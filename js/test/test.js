// http://chaijs.com/api/bdd/#equal-section
// https://medium.com/@paul_irish/debugging-node-js-nightlies-with-chrome-devtools-7c4a1b95ae27
var assert = require('assert');
var mocha = require('mocha');
var chai = require('chai');
var expect = chai.expect;
var chaiHttp = require('chai-http');
var gendiff = require('../g.js');

debugger;

// ##############################################################################
chai.use(chaiHttp);

describe('mangodb', function() {
    var b, app, server;
    before(function(done) {
	server = require("../b0.js");
	app = server.connect({'DB' : 'unit-test', 'DORESET' : true, 'DEBUG':true}, function(a) {
	    done();
	});
    });
    after(function() {
	server.disconnect();
    });
    describe('#backend REST', function() {
	it('GET /status route', function(done) {
	    chai.request(app)
		.get("/status")
		.end(function(err,res) {
		    expect(res).to.have.status(200);
		    expect(res).to.be.json;
		    expect(res.body).to.be.a('object');
		    expect(res.body).to.have.property('PORT').eql(10300);;
		    expect(res.body).to.have.property('DB').eql('unit-test');
		    expect(res.body).to.have.property('states-cnt').eql(0);
		    expect(res.body).to.have.property('events-cnt').eql(0);
		    done();
		});
	});
	
	it('PUT /rest/put route', function(done) {
	    var state = "test"
	    chai.request(app)
		.put("/rest/put")
		.send(state)
		.end(function(err,res) {
		    expect(res).to.have.status(200);
		});
	    chai.request(app)
		.get("/status")
		.end(function(err,res) {
		    expect(res).to.have.status(200);
		    expect(res.body).to.have.property('states-cnt').eql(1);
		    expect(res.body).to.have.property('events-cnt').eql(0);
		    done();
		});
	});
    });
});
