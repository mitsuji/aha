//
// reporter.html 用 UIコントローラ
//
$(document).ready(function () {

    //
    // 表示リセット
    //
    setConnect(false);
    setCaption('');
    setTotal('0');
    setAha('0');


    var boardPublicKey = function() {
	var url = location.href;
	var posQuery = url.indexOf('?');
	if( posQuery >= 0 ) {
	    return url.substr( posQuery +1 );
	} else {
	    return null;
	}
    }();

    
    //
    // boarderPublicKey が指定されていないときは何もしない
    //
    if( boardPublicKey != null ) {

	console.log('boardPublicKey: ' + boardPublicKey );
	console.log('localStorage: rk: ' + localStorage.getItem('rk:' + boardPublicKey));

	connect(boardPublicKey);
	
    }
    

    function connect( boardPublicKey ) {

	//
	// WebSocketクライアントの実装
	//

	var wsurl = '/reporter?board_public_key=' + boardPublicKey;
	if( localStorage.getItem('rk:' + boardPublicKey) != null)
	{
	    wsurl += '&reporter_key=' + localStorage.getItem('rk:' + boardPublicKey);
	}
	
	var ws = webSocketUtil.webSocket( wsurl );
	ws.onopen = function() {
	    setConnect(true);
	};
	
	ws.onclose = function(event) {
	    setConnect(false);
	};
    
	ws.onmessage = function(event) {
	    console.log('onmessage: ' + event.data );
	    var json = JSON.parse(event.data);
	    if(json.type == 'reporter') {
		localStorage.setItem('rk:' + json.content.board_public_key, json.content.reporter_key)
		setCaption(json.content.board_caption)
	    } else if(json.type == 'aha') {
		setAha(json.content);
	    } else if(json.type == 'total_aha') {
		setTotal(json.content);
	    } else if(json.type == 'reset') {
		setTotal('0');
		setAha('0');
	    }
	};


	//
	// AHAボタン押下
	//
	if ( 'ontouchend' in window ) {
	    $('#aha')[0].addEventListener('touchend',function(e){
		ws.send('aha');
	    },false);
	} else {
	    $('#aha').click(function(e){
		ws.send('aha');
	    });
	}

	$(this).gShake(function() { 
	    ws.send('aha');
	});
    }

    
    //
    // 画面表示
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

    function setCaption( caption ) {
	$('#caption').text( caption );
    }
    
    function setTotal( total ) {
	$('#totalCount').text(total);
    }
    
    function setAha( aha ) {
	$('#ahaCount').text(aha);
    }
    
});


