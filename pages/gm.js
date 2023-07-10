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

specificNewItemHandler = function(id, oldItem, newItem) {
    var viewType = null;
    var whereToAdd = null;
    var expandedViewLocation = null;
    if (newItem.location && (!oldItem || !oldItem.location)) {
        viewType = "shortInList";
        whereToAdd = "location-list";
        expandedViewLocation = "locations";
    } //TODO: Add the player and recipe cases.
    if (viewType) {
        var newView = {
            type:viewType,
            id:id,
            whereToPutColumn:document.getElementById(expandedViewLocation)
        };
        createView(newView);
        document.getElementById(whereToAdd).appendChild(newView.element);
    }
}

visibleAttributes.splice(1,0,"..");
