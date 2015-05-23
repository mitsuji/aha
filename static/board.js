//
// board.html 用 UIコントローラ
//
$(document).ready(function () {

    //
    // 表示リセット
    //
    setConnect(false);
    setCaption('');
    setTotal('0');
    
    console.log('localStorage: bk: ' + localStorage.getItem('bk'));

    if(localStorage.getItem('bk') != null) {
	resume();
    } else {
	create();
    }

    
    function resume() {
	$.post(
	    'http://' + location.host + '/get_board',
	    {sk: localStorage.getItem('bk')},
	    function (data){
		if(data.success) {
		    localStorage.setItem('bk',data.content.secret_key);
		    connect(data.content);
		} else {
		    switch(data.error_code) {
		    case 10001:
			alert("error: " + data.error_code + ": " + data.message);
			break;
		    default:
			create();
			break;
		    }
		}
	    },
	    "json"
	);
    }
    

    function create() {
	$.post(
	    'http://' + location.host + '/add_board',
	    {pk: "mitsujitest1", caption: "mitsuji test1"},
	    function (data){
		if(data.success) {
		    localStorage.setItem('bk',data.content.secret_key);
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
	var ws = webSocketUtil.webSocket('/board?bk=' + content.secret_key);
	
	ws.onopen = function() {
	    setConnect(true);
	    setCaption(content.caption);
	    setTotal(content.total_aha);
	    setQr(content.public_key);
	};
	
	ws.onclose = function(event) {
	    setConnect(false);
	};
	
	ws.onmessage = function(event) {
	    console.log('onmessage: ' + event.data);
	    eval ("var json = " + event.data + ";" ); // [TODO] must be danger
	    if(json.type == 'total_aha') {
		setTotal(json.content);
	    } else if(json.type == 'reset') {
		setTotal('0');
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
	$('#total').text( total );
    }

    function setQr( public_key ) {
	var reporter_url = 'http://' + location.host + '/reporter.html?' + public_key;
	var qr_url = 'https://chart.googleapis.com/chart';
	qr_url += '?chs=300x300&cht=qr&chl=' + reporter_url;
	$('#reporterAddress').text(reporter_url);
	$('#qr').attr('src',qr_url);
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

