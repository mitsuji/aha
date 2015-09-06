//
// admin.html 用 UIコントローラ
//
$(document).ready(function () {

    //
    // dialog object (Singleton)
    //
    var boardDialog = function( fOnSubmit ) {

	var divElem = $('#create-board-dialog');
	
	divElem.dialog({
	    autoOpen: false,
	    height: 400,
	    width: 400,
	    modal: true,
	    buttons: {
		'Create a Board': submit
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
	    },
	    close: function () {
		divElem.dialog('close');
	    }
	};
	
    }(onCreate);


    $('#addButton').on('click',function( event ){
	boardDialog.open();
    });

    
    function onCreate( publicKey, caption ) {
	$.post(
	    'http://' + location.host + '/add_board',
	    {public_key: publicKey, caption: caption},
	    function (data){
		if(data.success) {
		    boardDialog.close();

		    var boardKeys;
		    if(localStorage.getItem('boardKeys') != null){
			boardKeys = JSON.parse(localStorage.getItem('boardKeys'));
		    } else {
			boardKeys = {};
		    }

		    boardKeys[data.content.public_key] = data.content.secret_key;

		    localStorage.setItem('boardKeys',JSON.stringify(boardKeys));

		    load();
		    
		} else {
		    alert('error: ' + data.type);
		}
	    },
	    'json'
	);
    }
    

    function load() {
	
	jQuery.each($(".boardItem"),function(){
	    this.remove();
	})
	
	var boardKeys;
	if(localStorage.getItem('boardKeys') != null){
	    boardKeys = JSON.parse(localStorage.getItem('boardKeys'));
	} else {
	    boardKeys = {};
	}

	for (var publicKey in boardKeys ) {
	    var secretKey = boardKeys[publicKey];

	    $.post(
		'http://' + location.host + '/get_board',
		{secret_key: secretKey},
		function (data){
		    if(data.success) {
			onBoard(data.content);
		    } else {
			alert('error: ' + data.type);
		    }
		},
		'json'
	    );
	}
	
    }

    function onBoard( content ) {
	var item = $('#boardTemplate').clone(false);
	item.removeAttr('id');
	item.addClass('boardItem');
	item.css('display','table-row');
	item.find('.publicKey').text(content.public_key);
	item.find('.caption').text(content.caption);
	item.find('.launchButton').on('click',function(){
	    location.href = '/viewer.html?'+ content.public_key;
	});
	item.find('.deleteButton').on('click',function(){
	    alert("delete: " + content.secret_key);
	});
	$('#boardTable').append(item);
    }

    load();
    
});

