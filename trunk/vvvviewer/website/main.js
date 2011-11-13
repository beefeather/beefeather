function CreateHandler(document) {
	var imageHolderElement = document.getElementById("image-holder");
	var controlBoardElement = document.getElementById("control-board");
	
	var img_url_list = [
	    "http://lh3.googleusercontent.com/-KQt51QXqXXY/TN-iWXveZII/AAAAAAAAA9I/D6dgF-sNWR4/s720/%2525D0%25259C%2525D0%2525BE%2525D0%2525B8%2525D1%252581%2525D0%2525B5%2525D0%2525B5%2525D0%2525BD%2525D0%2525BA%2525D0%2525BE%252520%2525D0%2525A7%2525D0%2525B5%2525D1%252580%2525D0%2525B5%2525D1%252588%2525D0%2525BD%2525D1%25258F.jpg",
		"http://www.sonicsquat.com/wp-content/uploads/2010/07/zp.jpg"
	];
	var current_img_pos = -1;

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
	
	var fadeTimerManager = TimerManagerFactory(100);
	
	return {
		HandleTestButton : function() {
			var imageElement = imageHolderElement.querySelector("IMG");
			imageElement.style.transition = "opacity .5s ease-in-out";
			var test = imageElement.style.transition;
		},
		LayoutImageButton : function() {
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
		},
		ChangeImageButton : function() {
			current_img_pos = ( current_img_pos + 1 ) % img_url_list.length;
			imageHolderElement.innerHTML = "<img style='opacity: 0; -webkit-transition: opacity 2s ease-in-out' src='" + img_url_list[current_img_pos] + "'>";
			var imageElement = imageHolderElement.querySelector("IMG");
			return imageElement;
		},
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
			alert("Hello");
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
		}
	};
}
