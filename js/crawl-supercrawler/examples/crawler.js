/* globals console */
var supercrawler = require("../lib"),
    crawler;
var fs = require("fs");

var a = fs.readFileSync("url.txt",'utf8');
var ar = a.split("\n");
if (ar.length > 0) {
    var u = ar[Math.floor(Math.random()*ar.length)];
}
var u = "https://www.jasminedirectory.com/"


var crawler = new supercrawler.Crawler({
  interval: 10,
  concurrentRequestsLimit: 1024 /*,
  urlList: new supercrawler.RedisUrlList({
    redis: {
      port: 6379,
      host: '127.0.0.1'
    }
  })*/
});

crawler.on("crawlurl", function (url) {
    //console.log("Crawling " + url);
});
crawler.on("urllistempty", function () {
    //console.warn("The URL queue is empty.");
});
crawler.on("handlersError", function (err) {
  console.error(err);
});
crawler.addHandler("text/html", supercrawler.handlers.htmlLinkParser(

));
crawler.addHandler(function (context) {
    console.log("Processed " + context.url);
});


crawler.getUrlList().insertIfNotExists(new supercrawler.Url({
    url: u
})).then(function () {
    crawler.start();
});
