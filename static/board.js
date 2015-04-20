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
    //
    // Cookie にセッションIDがあればURLに渡してレジューム
    //
    var url;
    if(cookieUtil.cookie('bk') == null) {
	url = '/board';
    } else {
	url = '/board?bk=' + cookieUtil.cookie('bk');
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
	if( json.type == 'bk') {
	    cookieUtil.setCookie('bk',json.bsk);
	    var slave_url = 'http://' + window.location.host + '/reporter.html?' + json.bpk ;
	    var qr_url = 'https://chart.googleapis.com/chart';
	    qr_url += '?chs=300x300&cht=qr&chl=' + slave_url;
	    $('#slaveAddress').text(slave_url);
	    $('#qr').attr('src',qr_url);
	} else if(json.type == 'total') {
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

