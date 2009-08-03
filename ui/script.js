$.postJSON = function (url, data, callback) {
    $.post(url, data, callback, "json");
};

// inet_ntoa
function ntoa(i) {
    var pi = parseInt(i);

    if(isNaN(pi)) {
        throw("Not an integer!");
    } else {
        var q0 = (pi & 0xFF000000) >> 24,
            q1 = (pi & 0x00FF0000) >> 16,
            q2 = (pi & 0x0000FF00) >> 8,
            q3 = (pi & 0x000000FF) >> 0;
        return q0 + "." + q1 + "." + q2 + "." + q3;
    }
}

function aton(s) {
    var q = s.split(".");

    return (parseInt(q[0]) * Math.pow(256,3)) +
           (parseInt(q[1]) * Math.pow(256,2)) +
           (parseInt(q[2]) * Math.pow(256,1)) +
           (parseInt(q[3]) * Math.pow(256,0));
}

$(document).ready(function () {
});
