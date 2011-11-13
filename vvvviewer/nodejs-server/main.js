var http = require('http');
var url = require('url');
var util = require('util');
var path = require('path');
var fs = require('fs');

var websitePathPattern = /^\/website(.*)$/;
var ffffoundPathPattern = /^\/ffffound-feed(.*)$/;

function CreateRelayHandler(component, callback) {
	var pattern = new RegExp("^\/" + component + "(.*)$");
	return {
		check: function(req, res) {
			  var reqpath = url.parse(req.url).pathname;
			  var testRes = pattern.exec(reqpath);
			  if (testRes) {
				  var subPath = testRes[1];
				  callback(req, res, subPath);
				  return true;
			  }	else {
				  return false;
			  }  
		}
	};
}

var dirHandlers = [
    CreateRelayHandler("website", function(req, res, subPath) {
  	  if (subPath == '' || subPath == '/') {
		  SendRedirect(res, '/website/main.html');
	  } else {
		  var fileName = "website/" + subPath;
		  var is;
		  try {
		    is = fs.createReadStream(fileName);
		  } catch (e) {
			  res.writeHead(404, {'Content-Type': 'text/html'});
			  res.end("404. Not found: " + reqpath);
			  return;
		  }
		  var contentType;
		  var ext = path.extname(subPath);
		  if (ext == '.html') {
			  contentType = 'text/html';
		  } else if (ext == '.js') {
			  contentType = 'application/javascript';
		  } else {
			  res.writeHead(500, {'Content-Type': 'text/html'});
			  res.end("500. Unknown extension: " + ext);
			  return;
		  }
		  res.writeHead(200, {'Content-Type': contentType});
		  is.pipe(res);
	  }
    }),
    CreateRelayHandler("feedburner", function(req, res, subPath) {
  	  
  	  var options = {
  		  host: 'feeds.feedburner.com',
  		  port: 80,
  		  path: subPath
  		};
  	
  		http.get(options, function(relayRes) {
  		  var respHead = {};
  		  for (var key in relayRes.headers) {
  			  if (key == 'server') {
  				  continue;
  			  }
  			  var value = relayRes.headers[key];
  			  respHead[key, value];
  		  }
  		  res.writeHead(relayRes.statusCode, respHead);
  		  relayRes.pipe(res);
  		}).on('error', function(e) {
  		  console.log("Got error: " + e.message);
  		});
	  
    })
                     ];


function SendRedirect(res, url) {
  res.writeHead(301, {'Location': url});
  res.end();
}

http.createServer(function (req, res) {
  console.log("Request for " + req.url)
  
  for (var i = 0; i < dirHandlers.length; i++) {
	  var handler = dirHandlers[i];
	  var r = handler.check(req, res);
	  if (r) {
		  return;
	  }
  }
  
  var reqpath = url.parse(req.url).pathname;
  if (reqpath == '/') {
	  SendRedirect(res, '/website/main.html');
  } else if (reqpath == '/website/main.html') {
	  res.writeHead(500);
	  res.end("500. Not found: " + path);
  } else if (path == '/hello') {
	  res.writeHead(200, {'Content-Type': 'text/plain'});
	  res.end('Hello World\n');
  } else {
	  res.writeHead(404);
	  res.end("404. Not found: " + reqpath);
  }
}).listen(1337, "127.0.0.1");
console.log('Server running at http://127.0.0.1:1337/');
