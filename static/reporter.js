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
	console.log('localStorage:rk: ' + localStorage.getItem('rk:' + bk));
	
	if(localStorage.getItem('rk:' + bk) == null) {
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
		if(data.success) {
		    localStorage.setItem('rk:' + data.content.bpk, data.content.rsk)
		    startWebsocket(data);
		} else {
		    alert("error:" + data.error_code + ":" + data.message);  // board　が存在しないなど
		}
	    },
	    "json"
	);
    }
    

    function startExistingReporter(bk) {
	$.post(
	    'http://' + window.location.host + '/getReporter',
	    {bpk:bk, rsk: localStorage.getItem('rk:' + bk)},
	    function (data){
		if(data.success) {
		    localStorage.setItem('rk:' + data.content.bpk, data.content.rsk)
		    startWebsocket(data);
		} else {
		    startNewReporter(bk);
		}
	    },
	    "json"
	);
    }

    
    function startWebsocket( data ) {
	console.log(data);

	//
	// 初期表示
	//
	
	// pk
	// caption
	// total
	$('#ahaCount').text(data.content.ahaCount);


	//
	// WebSocketクライアントの実装
	//
	var ws = webSocketUtil.webSocket('/reporter?bk=' + data.content.bpk + "&rk=" + data.content.rsk);
	
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


