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
	
	if(cookieUtil.cookie('rk:' + bk) == null) {
	    startNewReporter(bk);
	} else {
	    startExistingReporter(bk);
	}
	
    }


    function startNewReporter(bk) {
	$.post(
	    'http://' + window.location.host + '/addReporter',
	    {bpk: bk},
	    function (data){
		cookieUtil.setCookie('rk:' + data.bpk ,data.rsk);
		startWebsocket(data);
	    },
	    "json"
	);
    }
    

    function startExistingReporter(bk) {
	$.post(
	    'http://' + window.location.host + '/getReporter',
	    {bpk:bk, rsk: cookieUtil.cookie('rk:' + bk)},
	    function (data){
		cookieUtil.setCookie('rk:' + data.bpk, data.rsk);
		startWebsocket(data);
	    },
	    "json"
	).fail(function(){
	    startNewReporter(bk);
	});
    }

    
    function startWebsocket( data ) {
	console.log(data);

	//
	// 初期表示
	//
	
	// pk
	// caption
	// total
	$('#ahaCount').text(data.ahaCount);


	//
	// WebSocketクライアントの実装
	//
	var ws = webSocketUtil.webSocket('/reporter?bk=' + data.bpk + "&rk=" + data.rsk);
	
	ws.onopen = function() {
	    setConnect(true);
	};
	
	ws.onclose = function(event) {
	    setConnect(false);
	};
    
	ws.onmessage = function(event) {
	    console.log('onmessage: ' + event.data );
	    eval (" var json = " + event.data + ";" ); // [TODO] must be danger
	    if(json.type == 'ahaCount') {
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
	
//	$('#aha').click(function(e){
//            ws.send('aha');
//	});

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


