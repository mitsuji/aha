//
// board.html 用 UIコントローラ
//
$(document).ready(function () {

    //
    // 表示リセット
    //
    setConnect(false);
    $('#total').text('0');
    
    console.log('cookie:bk: ' + cookieUtil.cookie('bk'));

    if(cookieUtil.cookie('bk') == null) {
	startNewBoard();
    } else {
	startExistingBoard();
    }

    
    function startNewBoard() {
	$.post(
	    'http://' + window.location.host + '/addBoard',
	    {pk:"mitsujitest1",caption:"mitsuji test1"},
	    function (data){
		cookieUtil.setCookie('bk',data.sk);
		startWebsocket(data);
	    },
	    "json"
	);
    }

    
    function startExistingBoard() {
	$.post(
	    'http://' + window.location.host + '/getBoard',
	    {sk: cookieUtil.cookie('bk')},
	    function (data){
		cookieUtil.setCookie('bk',data.sk);
		startWebsocket(data);
	    },
	    "json"
	).fail(function(){
	    startNewBoard();
	});
    }
    

    function startWebsocket( data ) {
	console.log(data);

	//
	// 初期表示
	//
	var slave_url = 'http://' + window.location.host + '/reporter.html?' + data.pk ;
	var qr_url = 'https://chart.googleapis.com/chart';
	qr_url += '?chs=300x300&cht=qr&chl=' + slave_url;
	$('#slaveAddress').text(slave_url);
	$('#qr').attr('src',qr_url);

	// pk
	// caption
	$('#total').text(data.total);
	

	//
	// WebSocketクライアントの実装
	//
	var ws = webSocketUtil.webSocket('/board?bk=' + data.sk);
	
	ws.onopen = function() {
	    setConnect(true);
	};
	
	ws.onclose = function(event) {
	    setConnect(false);
	};
	
	ws.onmessage = function(event) {
	    console.log('onmessage: ' + event.data );
	    eval (" var json = " + event.data + ";" ); // [TODO] must be danger
	    if(json.type == 'total') {
		$('#total').text(json.content);
	    } else if(json.type == 'reset') {
		$('#total').text('0');
	    }
	};

	
	//
	// リセットボタン押下
	//
	$('#reset').click(function () {
            ws.send('reset');
	});
	
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

