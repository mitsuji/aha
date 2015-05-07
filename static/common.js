
var webSocketUtil = function() {

    return {
	
	//
	// WebSoket生成
	//
	webSocket: function (path) {
	    var uri = 'ws://' + window.location.host + path;
	    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
	    return new Socket(uri);
	}
	
    };
    
}();

