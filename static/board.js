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

    //
    // dialog object (Singleton)
    //
    var boardDialog = function( fOnSubmit ) {

	var canClose;
	var divElem = $('#create-board-dialog');
	
	divElem.dialog({
	    autoOpen: false,
	    height: 400,
	    width: 400,
	    modal: true,
	    buttons: {
		'Create a Board': submit
	    },
	    beforeClose: function(event) {
		if(!canClose) {
		    event.preventDefault();
		}
	    }
	});
    
	divElem.find('form').on('submit', function( event ) {
	    event.preventDefault();
	    submit();
	});

	function submit() {
	    // validation
	    var publicKey = $('#newPublicKey').val();
	    var caption   = $('#newCaption').val(); 
	    fOnSubmit(publicKey,caption);
	}

	//
	// public methods
	//
	return {
	    open: function () {
		divElem.dialog('open');
		canClose = false;
	    },
	    close: function () {
		canClose = true;
		divElem.dialog('close');
	    }
	};
	
    }(onCreate);


    
    console.log('localStorage: bk: ' + localStorage.getItem('bk'));
    
    if(localStorage.getItem('bk') != null) {
	resume();
    } else {
	create();
    }

    
    function resume() {
	$.post(
	    'http://' + location.host + '/get_board',
	    {secret_key: localStorage.getItem('bk')},
	    function (data){
		if(data.success) {
		    localStorage.setItem('bk',data.content.secret_key);
		    connect(data.content);
		} else {
		    switch(data.error_code) {
		    case 10001:
			alert('error: ' + data.error_code + ': ' + data.message);
			break;
		    default:
			create();
			break;
		    }
		}
	    },
	    'json'
	);
    }
    


    function create() {
	boardDialog.open();
    }
    
    function onCreate( publicKey, caption ) {
	$.post(
	    'http://' + location.host + '/add_board',
	    {public_key: publicKey, caption: caption},
	    function (data){
		if(data.success) {
		    boardDialog.close();
		    localStorage.setItem('bk',data.content.secret_key);
		    connect(data.content);
		} else {
		    alert('error: ' + data.error_code + ': ' + data.message);
		}
	    },
	    'json'
	);
    }

    
    function connect( content ) {
	console.log(content);

	//
	// WebSocketクライアントの実装
	//
	var ws = webSocketUtil.webSocket('/board?secret_key=' + content.secret_key);
	
	ws.onopen = function() {
	    setConnect(true);
	    setCaption(content.caption);
	    setTotal(content.total_aha);
	    setReporterAddress(content.public_key);
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

    
    function getReporterAddress( publicKey ) {
	return 'http://' + location.host + '/reporter.html?' + publicKey;
    }
    
    function setReporterAddress( publicKey ) {
	var reporter_url = getReporterAddress(publicKey);
	$('#reporterAddress').text(reporter_url);
    }

    function setQr( publicKey ) {
	var reporter_url = getReporterAddress(publicKey);
	var qr_url = 'https://chart.googleapis.com/chart';
	qr_url += '?chs=300x300&cht=qr&chl=' + reporter_url;
	$('#qr').attr('src',qr_url);
    }


    
});

