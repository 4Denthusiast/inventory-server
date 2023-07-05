const gm = true;

tabButtons = document.getElementsByClassName("tab-button")

function selectTab() {
    for (var i=0; i<tabButtons.length; i++) {
        var tab = document.getElementById(tabButtons[i].value);
        tab.style.display = tabButtons[i].checked ? "" : "none";
    }
}

for (var i=0; i<tabButtons.length; i++)
    tabButtons[i].addEventListener("change", selectTab);

selectTab();

function createLocation() {
    var message = new XMLHttpRequest();
    message.open("POST", "/create-location");
    message.send(document.getElementById("new-location-name").value);
}

newItemHandler = function(id, oldItem, item) {
    if (item.location) alert(item.name);
}
