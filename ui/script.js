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

function sendStart() {
    var txid = $.uuid();

    $.ajax({
        data: { 'txid': txid, 'event': 'start' },
        success: function (data) {
            if (data && "OK" == data.responseCode) {
                dialogMessage("Success!");
            } else {
                dialogMessage("Failure!");
            }
        }
    });
}

/* Sends the shutdown command to Scurry. This will kill the process. */
function sendShutdown() {
    var txid = $.uuid();

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

/* Gets Scurry's internal state. */
function getState(callback) {
    var txid = $.uuid();

    $.ajax({
        data: { 'txid': txid, 'query': 'state' },
        success: callback
    });
}

/* Setup binds */
function setupBinds() {
    $('#button-start').click(sendStart);
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
