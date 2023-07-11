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

function addExtraControls(id, nameElement) {
    var button = document.createElement("button");
    button.className = "add-attribute-button item-button";
    button.title = "add attribute";
    button.textContent = "+";
    nameElement.insertAdjacentElement("afterend", button);
    button.addEventListener("click", function(event) {
        event.stopPropagation();
        setInteractionState("add-attribute", id, true);
        document.getElementById("add-attribute-details").showModal();
    });
}

var activeAddAttInput = null;
function updateAddAttTypeInput() {
    var type = document.getElementById("add-att-type-input").value;
    var control = null;
    switch (type) {
        case "name":
        case "desc":
        case "text":
            control = "text";
            break;
        case "container":
        case "components":
            control = "none";
    }
    document.getElementById("add-att-submit").disabled = control === null;
    activeAddAttInput = null;
    if (control) {
        var inputContainer = document.getElementById("add-att-input-container");
        for (var i = 0; i < inputContainer.children.length; i++) {
            inputContainer.children[i].hidden = true;
        }
        if (control != "none") {
            activeAddAttInput = document.getElementById("add-att-"+control+"-input");
            activeAddAttInput.hidden = false;
        }
    }
}
updateAddAttTypeInput(); // The type input may have some initial value due to the browser keeping the old value, so this is needed to ensure consistency.

function addAttSubmit() {
    var type = document.getElementById("add-att-type-input").value;
    var value = activeAddAttInput ? activeAddAttInput.value : [];
    var data = {};
    data[type] = value;
    var message = new XMLHttpRequest();
    message.open("POST", "/add-attribute");
    message.setRequestHeader("Content-Type", "application/json");
    message.send(JSON.stringify([interactionStateData,data]));
    setInteractionState("none");
    document.getElementById("add-attribute-details").close();
}

function addAttCancel() {
    setInteractionState("none");
    document.getElementById("add-attribute-details").close();
}
