//
// reporter.html 用 UIコントローラ
//
$(document).ready(function () {

    //
    // 表示リセット
    //
    setConnect(false);
    $('#ahaCount').text(0);


    //
    // boarder key が指定されていないときは何もしない
    //
    var url = document.location.href;
    var posQuery = url.indexOf('?');
    if( posQuery >= 0 ) {

	var bk = url.substr( posQuery +1 );
	console.log(cookieUtil.cookie('rk:' + bk));
	//
	// Cookie に当該のboardのセッションIDがあればURLに渡してレジューム
	//
	var url;
	if(cookieUtil.cookie('rk:' + bk) == null) {
	    url = '/reporter?bk=' + bk
	} else {
	    url = '/reporter?bk=' + bk + "&rk=" + cookieUtil.cookie('rk:' + bk);
	}


	//
	// WebSocketクライアントの実装
	//
	var ws = webSocketUtil.webSocket(url);
	
	ws.onopen = function() {
	    setConnect(true);
	};
	
	ws.onclose = function(event) {
	    setConnect(false);
	};
    
	ws.onmessage = function(event) {
	    console.log('onmessage: ' + event.data );
	    eval (" var json = " + event.data + ";" ); // [TODO] must be danger
	    if(json.type == 'rk') {
		cookieUtil.setCookie('rk:' + bk, json.content);
	    } else if(json.type == 'ahaCount') {
		$('#ahaCount').text(json.content);
	    } else if(json.type == 'reset') {
		$('#ahaCount').text(0);
	    }
	};


	//
	// AHAボタン押下
	//
	$('#aha')[0].addEventListener("touchend",function(e){
            ws.send('aha');
	},false);
	
    }


    //
    // 接続表示
    //
    function setConnect( flag ) {
	if( flag ) {
	    $('#connectionStatus .ng').hide();
	    $('#connectionStatus .ok').show();
	} else {
	    $('#connectionStatus .ok').hide();
	    $('#connectionStatus .ng').show();
	}
    }

    
});


