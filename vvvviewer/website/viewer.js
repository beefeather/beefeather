debugDisplay = (function() {
	var running = {};
	var key = 1;
	return {
		running: running,
		registerAsync: function(message, callback) {
			var this_key = key++;
			var data = {
				message: message,
				callback: callback
			};
			running[this_key] = data;
			return function() {
				delete running[this_key];
		        var params = [];
		        for(var n = 2; n < arguments.length; n++) params.push(arguments[n]);
				callback.call(null, params);
			};
		}
	};
})();


imageIterator = (function() {
	function createSettableFuture(name) {
		var value;
		var callbacks = [];
		return {
			set: function(val) {
				if (val === void 0) {
					throw "Error value";
				}
				if (val !== void 0) {
					throw "Value already set value";
				}
				value = val;
				for (var i = 0; i < callbacks.length; i++) {
					callbacks[i](value);
				}
			},
			isSet: function() {
				return value !== void 0;
			},
			getAsync: function(callback) {
				if (value === void 0) {
					callbacks.push(callback);
				} else {
					callback(value);
				}
			}
		};
	}
	
	function loadXhr(client, callback, syncCallback) {
		function handler() {
			if (this.readyState != 4) {
				return;
			}
			try {
				if (this.status == 200) {
					if (this.responseXML) {
						callback.success(this.responseXML);
					} else {
						callback.failure("Bad XML");
					}
				} else {
					callback.failure("Bad status: " + this.status);
				}
			} finally {
				syncCallback();
			}
		}
		
		client.onreadystatechange = handler;
		client.send();
	}
	
	function createFfffFeed() {
		return {
			nextAsync: function(callback) {
				
			}
		};
	}
	
	function createBuffer(size, feed) {
		return {
			start: function() {
				
			},
			getNextAsync: function(callback) {
				
			}
		};
	}
	
	var img_url_list = [
	    "http://lh3.googleusercontent.com/-KQt51QXqXXY/TN-iWXveZII/AAAAAAAAA9I/D6dgF-sNWR4/s720/%2525D0%25259C%2525D0%2525BE%2525D0%2525B8%2525D1%252581%2525D0%2525B5%2525D0%2525B5%2525D0%2525BD%2525D0%2525BA%2525D0%2525BE%252520%2525D0%2525A7%2525D0%2525B5%2525D1%252580%2525D0%2525B5%2525D1%252588%2525D0%2525BD%2525D1%25258F.jpg",
		"http://www.sonicsquat.com/wp-content/uploads/2010/07/zp.jpg"
	];
	var current_img_pos = -1;
	var buffer;
	
	function load() {
		function handler() {
			if (this.readyState != 4) {
				return;
			}
			if (this.status != 200) {
				alert("Bad status");
				return;
			}
			if (!this.responseXML) {
				alert("Bad XML");
				return;
			}
			parseXml(this.responseXML);
		}
		
		function parseXml(xmlDocument) {
			var items = xmlDocument.querySelectorAll("rss>channel>item");
			buffer = [];
			for (var i = 0; i < items.length; i++) {
				var link = items[i].querySelector("source");
				var text = link.attributes.url.nodeValue;
				buffer.push({
					url: text
				});
			}
		}
		
		var client = new XMLHttpRequest();
		client.onreadystatechange = handler;
		client.open("GET", "/feedburner/ffffound/everyone");
		client.send();
	}
	
	load();
	
	return {
		start: function() {
			
		},
		nextAsync: function(callback) {
			
		},
		next_old: function() {
			current_img_pos = ( current_img_pos + 1 ) % img_url_list.length;
			return {
				url: img_url_list[current_img_pos]
			};
			
		},
		next: function() {
			current_img_pos = ( current_img_pos + 1 ) % buffer.length;
			return {
				url: buffer[current_img_pos].url
			};
			
		}
	};
})();

function CreateHandler(document) {
	var imageHolderElement = document.getElementById("image-holder");
	var controlBoardElement = document.getElementById("control-board");
	
	var exposeTime = 5000;
	var blackTime = 2000;

	var stopped = false;
	var pausedCallback = null;
	function schedule(timeout, callback) {
		if (stopped) {
			pausedCallback = callback;
			return;
		}
		var callbackWrapper = function() {
			if (stopped) {
				pausedCallback = callback;
				return;
			}
			pausedCallback = null;
			callback();
		}
		setTimeout(callbackWrapper, timeout);
	}
	function Step1Callback() {
		changeImage();
		layoutImage();
		
		var imageElement = imageHolderElement.querySelector("IMG");
		imageElement.style.opacity = 1;
		
		function Step2Callback() {
			imageElement.style.opacity = 0;
			schedule(blackTime, Step1Callback);
		}
		schedule(exposeTime, Step2Callback);
		
	}
	
	function TimerManagerFactory(delay) {
		var scheduled = false;
		function Callback() {
			scheduled = false;
			if (obj.handler) {
				obj.handler();
			}
		}
		var obj = {
			schedule: function() {
				if (scheduled) {
					return;
				}
				scheduled = true;
				setTimeout(Callback, delay);
			},
			handler: null
		};
		return obj;
	}	
	
	function changeImage() {
		var url = imageIterator.next().url;
		imageHolderElement.innerHTML = "<img style='opacity: 0; -webkit-transition: opacity 2s ease-in-out' src='" + url + "'>";
		var imageElement = imageHolderElement.querySelector("IMG");
		return imageElement;
	}
	
	function layoutImage() {
		var imageElement = imageHolderElement.querySelector("IMG");
		if (imageElement.height == 0 || imageElement.width == 0) {
			return;
		}
		var originalHeight;
		var originalWidth;
		if (imageElement.vvvviewerOriginalHeight) {
			originalHeight = imageElement.vvvviewerOriginalHeight;
			originalWidth = imageElement.vvvviewerOriginalWidth;
		} else {
			originalHeight = imageElement.height;
			originalWidth = imageElement.width;
			imageElement.vvvviewerOriginalHeight = originalHeight;
			imageElement.vvvviewerOriginalWidth = originalWidth;
		}
		var heightFactor = imageHolderElement.clientHeight / originalHeight; 
		var widthFactor = imageHolderElement.clientWidth / originalWidth;
		var newHeight;
		var newWidth;
		if (heightFactor > widthFactor) {
		    newHeight = widthFactor * originalHeight;
		    newWidth = imageHolderElement.clientWidth;
	    } else {
		    newHeight = imageHolderElement.clientHeight;
		    newWidth = heightFactor * originalWidth;
		}
		imageElement.height = newHeight;
		imageElement.width = newWidth;
	}
	
	var fadeTimerManager = TimerManagerFactory(100);
	
	function stopPlayer() {
		stopped = true;
	}
	
	function startPlayer() {
		stopped = false;
		if (pausedCallback) {
			schedule(0, pausedCallback);
		} else {
			schedule(0, Step1Callback);
		}
	}

	
	return {
		HandleTestButton : function() {
			var imageElement = imageHolderElement.querySelector("IMG");
			imageElement.style.transition = "opacity .5s ease-in-out";
			var test = imageElement.style.transition;
		},
		LayoutImageButton : layoutImage,
		ChangeImageButton : changeImage,
		TransparencyButton : function(value) {
			var imageElement = imageHolderElement.querySelector("IMG");
			imageElement.style.opacity = value;
		},
		FadeOutButton : function(value) {
			var imageElement = imageHolderElement.querySelector("IMG");
			
			function Handler() {
				var currentOpacity = imageElement.style.opacity;
				if (currentOpacity === undefined || currentOpacity == "") {
					currentOpacity = 1;
				} else {
					currentOpacity = Number(currentOpacity);
				}
				if (currentOpacity <= 0) {
					return;
				}
				currentOpacity = Math.floor(currentOpacity * 100) / 100;
				var newOpacity = currentOpacity - 0.1;
				imageElement.style.opacity = newOpacity;
				fadeTimerManager.schedule();
			}
			
			fadeTimerManager.handler = Handler;
			fadeTimerManager.schedule();
		},
		ExposeButton : function(value) {
			var imageElement = imageHolderElement.querySelector("IMG");
			imageElement.style.opacity = 1;
			
			function Callback() {
			    imageElement.style.opacity = 0;
			}
			
			setTimeout(Callback, 5000);
		},
		ShowControlsButton : function(value) {
			controlBoardElement.style.display = 'block';
			setTimeout(function() {
   			    controlBoardElement.style.opacity = 1;
			}, 10);
		},
		CloseControlsButton : function(value) {
			controlBoardElement.style.opacity = 0;
			setTimeout(function() {
			    controlBoardElement.style.display = 'none';
			}, 1000);
		},
		StopButton: stopPlayer,
		StartButton: startPlayer
	};
}
