function dialogMessage(msg) {
    $('#dialog').dialog({
        modal: true,
        buttons: {
            Ok: function () {
                $(this).dialog('close');
            }
        }
    }).html('<p>' + msg + '<p>');
}

/* Sends the shutdown command to Scurry. This will kill the process. */
function sendShutdown() {
    var button = $('#button-shutdown'),
        txid = $.uuid();

    $.ajax({
        data: { 'txid': txid, 'event': 'shutdown' },
        success: function (data) {
            if (data && "OK" == data.responseCode) {
                dialogMessage("Success!");
            } else {
                dialogMessage("Failure!");
            }
        }
    });
}

/* Setup binds */
function setupBinds() {
    $('#button-shutdown').click(sendShutdown);
}

/* Kick things off plz */
function startup() {
    $.ajaxSetup( {
        url: 'sq',
        dataType: 'json'
    });
    setupBinds();
}
