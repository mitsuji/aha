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


    //
    // boarder key が指定されていないときは何もしない
    //
    var url = document.location.href;
    var posQuery = url.indexOf('?');
    if( posQuery >= 0 ) {

	var bk = url.substr( posQuery +1 );
	console.log('localStorage: rk: ' + localStorage.getItem('rk:' + bk));
	
	if(localStorage.getItem('rk:' + bk) != null) {
	    resume(bk);
	} else {
	    create(bk);
	}
	
    }


    function resume( bk ) {
	$.post(
	    'http://' + location.host + '/get_reporter',
	    {bpk: bk, rsk: localStorage.getItem('rk:' + bk)},
	    function (data){
		if(data.success) {
		    localStorage.setItem('rk:' + data.content.board_public_key, data.content.reporter_key)
		    connect(data.content);
		} else {
		    switch(data.error_code) {
		    case 10001:
		    case 10002:
			alert("error: " + data.error_code + ": " + data.message);
			break;
		    default:
			create(bk);
			break;
		    }
		}
	    },
	    "json"
	);
    }

    
    function create( bk ) {
	$.post(
	    'http://' + location.host + '/add_reporter',
	    {bpk: bk},
	    function (data){
		if(data.success) {
		    localStorage.setItem('rk:' + data.content.board_public_key, data.content.reporter_key)
		    connect(data.content);
		} else {
		    alert("error: " + data.error_code + ": " + data.message);
		}
	    },
	    "json"
	);
    }
    

    function connect( content ) {
	console.log(content);

	//
	// WebSocketクライアントの実装
	//
	var ws = webSocketUtil.webSocket('/reporter?bk=' + content.board_public_key + "&rk=" + content.reporter_key);
	
	ws.onopen = function() {
	    setConnect(true);
	    setCaption(content.board_caption)
	    setTotal(content.board_total_aha)
	    setAha(content.aha)
	};
	
	ws.onclose = function(event) {
	    setConnect(false);
	};
    
	ws.onmessage = function(event) {
	    console.log('onmessage: ' + event.data );
	    eval ("var json = " + event.data + ";" ); // [TODO] must be danger
	    if(json.type == 'aha') {
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
	$('#aha')[0].addEventListener("touchend",function(e){
            ws.send('aha');
	},false);
	
//	$('#aha').click(function(e){
//            ws.send('aha');
//	});

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


