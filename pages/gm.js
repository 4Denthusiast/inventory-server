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
    if (newItem.location && (!oldItem || !oldItem.location)) {
        addListView(id, "location-list", "locations");
    }
    // TODO: add the recipe case.
}

specificCheckPCView = function(view) {
    if (view.level == playerCharacters[view.player].length - 2 && playerCharacters[view.player][view.level + 1].limit) {
        if (!view.dependentView) {
            view.dependentView = addListView(view.id, "players-list", "players");
        }
    } else {
        if (view.dependentView) {
            removeView(view.dependentView);
            delete view.dependentView;
        }
    }
}

function addListView(id, whereToAdd, expandedViewLocation) {
    var newView = {
        type:"shortInList",
        id:id,
        whereToPutColumn:document.getElementById(expandedViewLocation)
    };
    createView(newView);
    document.getElementById(whereToAdd).appendChild(newView.element);
    return newView;
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
        setInteractionState("add-attribute", "", id, true);
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
        case "spawnPoint":
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
    sendToServer("add-attribute", [interactionStateData, data]);
    setInteractionState("none");
    document.getElementById("add-attribute-details").close();
}

function createContent(id, attached) {
    return function(event) {
        if (event.shiftKey) {
            setInteractionState("createCommodity", "", {id:id, attached:attached}, true);
            document.getElementById("create-commodity-details").showModal();
        } else {
            sendToServer("create-item", [id, attached]);
        }
    }
}

function createCommoditySubmit() {
    sendToServer("create-commodity", [interactionStateData.id, interactionStateData.attached, document.getElementById("create-commodity-type-input").value, document.getElementById("create-commodity-quantity-input").valueAsNumber]);
    setInteractionState("none");
    document.getElementById("create-commodity-details").close();
}
