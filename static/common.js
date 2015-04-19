
var cookieUtil = function() {

    return {
	//
	// cookie 取得
	//
	cookie: function( key ) {
	    var keyStart = document.cookie.indexOf( key + '=' );
	    if( keyStart != -1 ) {
		var valStart = keyStart + key.length + 1; 
		var valEnd   = document.cookie.indexOf( ';', valStart );
		if( valEnd = -1 ) {
		    valEnd = document.cookie.length;
		}
		return document.cookie.substring(valStart,valEnd);
	    } else {
		return null;
	    }
	},

	//
	// cookie 設定
	//
	setCookie : function ( key, val ) {
	    document.cookie = key + '=' + val;
	}
	
    };
    
}();


var webSocketUtil = function() {

    var private_getHost = function() {
	var h = window.location.hostname;
	if (h == '') h = 'localhost';
	return h;
    };
    
    return {
	
	//
	// カレントホスト取得
	//
	getHost: function() {
	    return private_getHost();
	},
	
	//
	// WebSoket生成
	//
	webSocket: function (path, port) {
	    var uri = 'ws://' + private_getHost() + ':' + port + path;
	    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
	    return new Socket(uri);
	}
	
    };
    
}();

