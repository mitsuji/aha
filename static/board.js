//
// board.html 用 UIコントローラ
//
$(document).ready(function () {

    //
    // 表示リセット
    //
    setConnect(false);
    $('#total').text('0');
    
    console.log('localStorage:bk: ' + localStorage.getItem('bk'));

    if(localStorage.getItem('bk') == null) {
	startNewBoard();
    } else {
	startExistingBoard();
    }

    
    function startNewBoard() {
	$.post(
	    'http://' + window.location.host + '/addBoard',
	    {pk:"mitsujitest1",caption:"mitsuji test1"},
	    function (data){
		if(data.success) {
		    localStorage.setItem('bk',data.content.sk);
		    startWebsocket(data);
		} else {
		    alert("error:" + data.error_code + ":" + data.message); // publicKey の重複など
		}
	    },
	    "json"
	);
    }

    
    function startExistingBoard() {
	$.post(
	    'http://' + window.location.host + '/getBoard',
	    {sk: localStorage.getItem('bk')},
	    function (data){
		if(data.success) {
		    localStorage.setItem('bk',data.content.sk);
		    startWebsocket(data);
		} else {
		    startNewBoard();
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
	var slave_url = 'http://' + window.location.host + '/reporter.html?' + data.content.pk ;
	var qr_url = 'https://chart.googleapis.com/chart';
	qr_url += '?chs=300x300&cht=qr&chl=' + slave_url;
	$('#slaveAddress').text(slave_url);
	$('#qr').attr('src',qr_url);

	// pk
	// caption
	$('#total').text(data.content.total);
	

	//
	// WebSocketクライアントの実装
	//
	var ws = webSocketUtil.webSocket('/board?bk=' + data.content.sk);
	
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


    var dialog, form;
    
    dialog = $( "#dialog-form" ).dialog({
      autoOpen: false,
      height: 400,
      width: 400,
      modal: true,
      buttons: {
        "Create a board": function(){ console.log("go!!")},
        Cancel: function() {
          dialog.dialog( "close" );
        }
      },
      close: function() {
        form[ 0 ].reset();
        allFields.removeClass( "ui-state-error" );
      }
    });
 
    form = dialog.find( "form" ).on( "submit", function( event ) {
      event.preventDefault();
	console.log("go!!");
    });
 
    $( "#create-user" ).button().on( "click", function() {
      dialog.dialog( "open" );
    });
    
//    dialog.dialog( "open" );
    
});

