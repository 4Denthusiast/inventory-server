var items = {};
var listenList = {};
var itemViews = {};
var itemListenerRequest = null;
var newItemHandler = null;
var username = new URL(document.location).searchParams.get("name");

function launchListener() {
    if (itemListenerRequest) {
        itemListenerRequest.abort();
    }
    var request = new XMLHttpRequest();
    itemListenerRequest = request;
    request.open("POST", "listen-updates?name="+username);
    request.responseType = "json";
    request.onreadystatechange = function() {
        if (request.readyState != XMLHttpRequest.DONE) return;
        if (request.status == 422) {
            alert("Unrecognised username. Please consult a GM.");
        } else if (request.status == 0) {
            alert("Data loading error. Probably the server is down, or maybe you lost your internet connectiion.");
        } else if (request.status != 200) {
            alert("Unexpected error encountered when loading item updates from the server: "+request.status+" "+request.statusText+" "+request.response);
        } else {
            for (var i in request.response) {
                var response = request.response[i];
                // The response arrives in the format [item ID, item, version number of this item].
                var oldItem = items[response[0]];
                items[response[0]] = response[1];
                listenList[response[0]] = response[2];
                newItemHandler(response[0], oldItem, response[1]);
            }
        }
        launchListener();
    }
    request.setRequestHeader("Content-Type", "application/json");
    request.send(encodeListenList());
}

launchListener();

function sendUpdatedListeningList() {
    var request = new XMLHttpRequest();
    request.open("POST", "change-listening?name="+username);
    request.setRequestHeader("Content-Type", "application/json");
    request.send(encodeListenList());
}

function encodeListenList() {
    var encoded = []
    for (var i in listenList)
        encoded.push([parseInt(i), listenList[i]]);
    return JSON.stringify(encoded);
}
