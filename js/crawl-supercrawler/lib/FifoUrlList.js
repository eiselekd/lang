var FifoUrlList,
    Promise = require("bluebird");

var fs = require("fs");

function getHostName(url) {
    var match = url.match(/:\/\/(www[0-9]?\.)?(.[^/:]+)/i);
    if (match != null && match.length > 2 && typeof match[2] === 'string' && match[2].length > 0) {
        return match[2];
    }
    else {
        return null;
    }
}

/**
 * A simple queue for \Url objects that holds the queue in-memory and works
 * in a first-in, first-out fashion. Note that all \Url, even those that
 * are "popped", are kept in-memory because they store crawl state info, too.
 */
FifoUrlList = function () {
    if (!(this instanceof FifoUrlList)) {
        return new FifoUrlList();
    }

    this._list = [];
    this._listIndexesByUniqueId = {};
    this._nextIndex = 0;
    this._known = {};
    this._others = [];
    this._othersindex = 0;

    var a = fs.readFileSync("url.txt",'utf8');
    var ar = a.split("\n");
    for (var x in ar) {
        var h = getHostName(x);
        if (this._known[x] == undefined) {
            this._known[x] = x.length;
        } else {
            this._known[x] += x.length;
        }
    }
    
};

/**
 * Insert a \Url object into the queue. Resolves even if record currently
 * exists.
 *
 * @param  {Url} url     \Url object
 * @return {Promise}     Returns the inserted object with a promise.
 */
FifoUrlList.prototype.insertIfNotExists = function (url) {
    var uniqueId,
        currentIndex;

    uniqueId = url.getUniqueId();
    currentIndex = this._listIndexesByUniqueId[uniqueId];

    if (typeof currentIndex === "undefined") {
        //console.log(url);
        
        this._pushUrlToList(url);
    }

    return Promise.resolve(url);
};

/**
 * Insert a new URL, or update it if it already exists. This method is used
 * to update the state of a crawl.
 *
 * @param  {Url} url     \Url object
 * @return {Promise}     Returns the inserted object with a promise.
 */
FifoUrlList.prototype.upsert = function (url) {
    var uniqueId,
        self = this;

    uniqueId = url.getUniqueId();

    return this.insertIfNotExists(url).then(function () {
        var currentIndex;

        currentIndex = self._listIndexesByUniqueId[uniqueId];
        self._list[currentIndex] = url;
    });
};


/**
 * Insert a URL that isn't already in the list, i.e. update the list array
 * and the lookup object.
 *
 * @param  {Url} url    \Url object
 * @return {number}     Index of the record that has been inserted.
 */
FifoUrlList.prototype._pushUrlToList = function (url) {
    var listLength,
        uniqueId;

    listLength = this._list.length;
    uniqueId = url.getUniqueId();

    var seq = listLength-this._nextIndex;
    var idx = Math.floor(Math.random() * seq);

    var u = url._url;
    fs.appendFile('url.txt', u + "\n", function (err) {
    });
    
    var hostname = getHostName(url._url);
    if (hostname != null) {
        if (this._known[hostname] == undefined) {
            this._known[hostname] = 1;
            idx = 0;
            if (seq > 1 && Math.random() > 0.5) {
                idx = idx + 1;
            } 
            
            //console.log(hostname + ":" + listLength);
        } else {
            this._known[hostname] += url._url.length;
            if (this._known[hostname] > 16*1024) {
                if (listLength > 150000) {
                    //return listLength;
                }
                
            }
        }
    }
    
    
    this._list.splice(this._nextIndex+idx,0,url);
    
    //console.log(idx + ":" + url._url);
    
    //this._list[listLength] = url;
    this._listIndexesByUniqueId[uniqueId] = listLength;

    return listLength;
};

/**
 * Get the next URL that should be crawled. In this list, URLs are crawled
 * in a first-in, first-out fashion. They are never crawled twice, even if the
 * first request failed.
 *
 * @return {Promise} Returns the next \Url to crawl with a promise.
 */
FifoUrlList.prototype.getNextUrl = function () {
    var item;

    if (this._nextIndex >= this._list.length) {
        return Promise.reject(new RangeError("The list has been exhausted."));
    }
    
    item = this._list[this._nextIndex];
    this._nextIndex++;

    if (this._nextIndex > 4) {
        this._list.splice(0,2);
        this._nextIndex-=2;
    };
    
    return Promise.resolve(item);
};

module.exports = FifoUrlList;
