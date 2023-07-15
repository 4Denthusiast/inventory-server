var items = {};
var listenList = {};
var itemViews = {};
var itemListenerRequest = null;
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
    request.send(JSON.stringify(encodeListenList()));
}

launchListener();

function sendToServer(actionName, data) {
    var request = new XMLHttpRequest();
    request.open("POST", actionName+"?name="+username);
    request.setRequestHeader("Content-Type", "application/json");
    request.send(JSON.stringify(data));
}

function sendUpdatedListeningList() {
    sendToServer("change-listening", encodeListenList());
}

function encodeListenList() {
    var encoded = []
    for (var i in listenList)
        encoded.push([parseInt(i), listenList[i]]);
    return encoded;
}

function newItemHandler(id, oldItem, newItem) {
    if (!itemViews[id]) {
        itemViews[id] = [];
    }
    for (var viewI in itemViews[id]) {
        var view = itemViews[id][viewI];
        if (!newItem) {
            alert("A watched item disappeared. The code to deal with this hasn't been written yet.");//TODO
        } else if (view.loading) {
            view.element.replaceWith(createView(view, newItem));
        } else {
            view.nameElement.textContent = newItem.name || "[Unnamed Item]";
            if (view.expanded) {
                updateAttributeViews(view, oldItem, newItem);
            }
        }
    }
    specificNewItemHandler(id, oldItem, newItem);
}
var specificNewItemHandler = null; // To be set by player.js or gm.js

function updateAttributeViews(view) {
    //TODO: Check there are no cases where this can be called with a null item.
    const item = items[view.id];
    var lastExistingNode = null;
    view.attBeingDisplayed = view.attBeingDisplayed || {};
    var visibleAtts = visibleAttributes;
    if (gm) {
        visibleAtts = [].concat(visibleAtts);
        for (var att in item) {
            if (!visibleAtts.includes(att) && att != "name") {
                visibleAtts.push(att);
            }
        }
    }
    for (var attI = visibleAtts.length-1; attI >= 0; attI --) {
        var att = visibleAtts[attI];
        var attView = view.attElements[att];
        if (attView && !item[att]) {
            view.attListElement.removeChild(attView);
            delete view.attElements[att];
        } else if (!attView && item[att]) {
            var newAttView = createAttView(att, item[att], view.id, item, view.type);
            view.attListElement.insertBefore(newAttView, lastExistingNode);
            view.attElements[att] = newAttView;
            lastExistingNode = newAttView;
        } else if (attView && item[att]) {
            if (!deepEquals(view.attBeingDisplayed[att], item[att])) {
                if (updateAttView[att]) {
                    updateAttView[att](view.attElements[att], item[att], view.id, item, view.type);
                } else {
                    var newAttView = createAttView(att, item[att], view.id, item, view.type);
                    attView.replaceWith(newAttView);
                    view.attElements[att] = newAttView;
                }
            }
            lastExistingNode = view.attElements[att];
        } // The other case, that there neither was nor is this attribute, needs no update.
        view.attBeingDisplayed[att] = item[att];
    }
}

function deepEquals(a, b) {
    // JSON.stringify has some odd cases like {a:1,b:2} not equalling {b:2,a:1}, but it's fine for this purpose where we just want to check whether the two values were equal on the server before being stringified and parsed.
    return JSON.stringify(a) == JSON.stringify(b);
}

function createView(view) {
    if (!itemViews[view.id])
        itemViews[view.id] = [];
    if (!itemViews[view.id].includes(view))
        itemViews[view.id].push(view);
    view.loading = !items[view.id];
    var rootElement;
    if (view.loading) {
        rootElement = document.createElement("div");
        rootElement.className = "item-view";
        rootElement.textContent = "Loading...";
    } else {
        var enclosingElement = document.createElement("div"); //An extra outer element must be used because setting outerHTML doesn't work on an element that isn't actually in the DOM tree.
        switch(view.type) {
            case "shortInList":
                enclosingElement.innerHTML = '<div class="item-view"><div class="item-view-header"><div class="item-view-name"></div><button class="expand-button item-button" title="expand item view"><img src="/icons/right-arrow.svg" alt="expand"/></button></div></div>';
                break;
            case "content":
                enclosingElement.innerHTML = '<div class="item-view"><div class="item-view-header"><div class="item-view-name"></div><button class="remove-button item-button" title="move item"><img src="/icons/hand.png" alt="move"/></button><button class="expand-button item-button" title="expand item view"><img src="/icons/down-arrow.svg" alt="expand"/></button></div><ul class="attribute-list" hidden></ul></div>';
                break;
            case "component":
                enclosingElement.innerHTML = '<div class="item-view"><div class="item-view-header"><div class="item-view-name"></div><button class="remove-button item-button" title="remove component"><img src="/icons/knife.svg" alt="remove"/></button><button class="expand-button item-button" title="expand item view"><img src="/icons/down-arrow.svg" alt="expand"/></button></div><ul class="attribute-list" hidden></ul></div>';
                break;
            case "root":
                enclosingElement.innerHTML = '<div class="item-view root-item-view column"><div class="item-view-header"><h2 class="item-view-name col-title"></h2></div><ul class="attribute-list"></ul></div>';
                break;
        }
        rootElement = enclosingElement.children[0];
    }
    view.element = rootElement;
    rootElement.itemView = view;
    rootElement.addEventListener("click", clickView(view));
    if (!view.loading) {
        function getElement(name) {
            return rootElement.getElementsByClassName(name)[0];
        }
        view.nameElement = getElement("item-view-name");
        view.nameElement.textContent = items[view.id].name || "[Unnamed Item]";
        makeAttViewEditable(view.nameElement, "name", view.id);
        if (gm) {
            addExtraControls(view.id, view.nameElement);
        }
        var expandButton = getElement("expand-button");
        if (expandButton) {
            expandButton.addEventListener("click", expandItemView(view));
        }
        var removeButton = getElement("remove-button");
        if (removeButton) {
            removeButton.addEventListener("click", moveItem(view.id, view.type == "component"));
        }
        view.attListElement = getElement("attribute-list");
        view.attElements = {};
        view.expanded = view.type == "root";
        if (view.expanded) {
            updateAttributeViews(view);
        }
    }
    return rootElement;
}

function createCommodityView(data, attached, id) {
    var enclosingElement = document.createElement("div"); //An extra outer element must be used because setting outerHTML doesn't work on an element that isn't actually in the DOM tree.
    if(attached) {
        enclosingElement.innerHTML = '<div class="item-view commodity-view"><div class="item-view-header"><div class="item-view-name commodity-name"></div><button class="remove-button item-button" title="remove some of this component, or hold shift to remove all of it."><img src="/icons/knife.svg" alt="remove"/></button></div></div>';
    } else {
        enclosingElement.innerHTML = '<div class="item-view"><div class="item-view-header"><div class="item-view-name"></div><button class="remove-button item-button" title="move some of this, or hold shift to move all of it."><img src="/icons/hand.png" alt="move"/></button></div></div>';
    }
    var rootElement = enclosingElement.children[0];
    function getElement(name) {
        return rootElement.getElementsByClassName(name)[0];
    }
    getElement("item-view-name").textContent = data[0]+", "+addUnits(data[1], data[0]);
    var removeButton = getElement("remove-button");
    if (removeButton) {
        removeButton.addEventListener("click", moveCommodity(data, attached, id));
    }
    return rootElement;
}

function addUnits(value, commodityType) {
    const decomposed = decomposeUnits(value, commodityType);
    return new Intl.NumberFormat("en-GB", {maximumSignificantDigits: 3}).format(decomposed.mantissa) + decomposed.unit;
}

function decomposeUnits(value, commodityType) {
    var baseUnit = {focus:"", craving:"", wisdom:""}[commodityType];
    // I'm more likely to add ad-hoc physical commodity types than any other sort, so grams is the default.
    if (baseUnit === undefined) {
        baseUnit = "g";
    }
    var exponent = value == 0 ? 0 : Math.floor(Math.log10(value)/3);
    exponent = Math.max(-10, Math.min(10, exponent));
    const prefixFactor = Math.pow(1000,exponent);
    const mantissa = value/prefixFactor;
    if (baseUnit == "g" && exponent >= 2) {
        baseUnit = "t"; //Mg would just be confusing.
        exponent -= 2;
    }
    const prefix = ["q","r","y","z","a","f","p","n","μ","m","","k","M","G","T","P","E","Z","Y","R","Q"][exponent+10];
    return {mantissa:mantissa, unit:prefix+baseUnit, factor:prefixFactor};
}

var visibleAttributes = ["desc", "text", "container"];

const updateAttView = {};

function createAttView(type, ...args) {
    if (createAttViewTypes[type]) {
        return createAttViewTypes[type](...args);
    } else {
        const element = document.createElement("span");
        element.className = "attribute-view default-att-view";
        element.textContent = type;
        return element;
    }
}

const createAttViewTypes = {
    desc:function(desc, id) {
        const element = document.createElement("p");
        element.className = "attribute-view";
        element.textContent = desc;
        makeAttViewEditable(element, "desc", id);
        return element;
    },
    text:function(text, id) {
        const element = document.createElement("p");
        element.className = "attribute-view text-attribute";
        element.textContent = text;
        makeAttViewEditable(element, "text", id);
        return element;
    },
    container:createContainerView(false),
    components:createContainerView(true)
};

function createContainerView(attached) {
    return function(content, id, item, view) {
        const outerElement = document.createElement("div");
        outerElement.className = "list-attribute";
        outerElement.appendChild(document.createElement("h3"));
        outerElement.children[0].textContent = attached ? "Components:" : "Contents:";
        if (gm) {
            outerElement.children[0].innerHTML += '<button class="add-content-button item-button" title="Add item here, or shift-click to add commodity.">+</button>';
            outerElement.children[0].children[0].addEventListener("click", createContent(id, attached));
        }
        const listElement = document.createElement("ul");
        var needsNewItems = false;
        for (var i in content) {
            var data = content[i].contents;
            if (content[i].tag == "Commodity") {
                listElement.appendChild(createCommodityView(data, attached, id));
            } else { //content[i].tag == "ItemRef"
                var innerView = {type:attached?"component":"content", id:data};
                createView(innerView);
                listElement.appendChild(innerView.element);
                if (innerView.loading) {
                    needsNewItems = true;
                    listenList[data] = -1;
                }
            }
        }
        outerElement.appendChild(listElement);
        if (needsNewItems) {
            sendUpdatedListeningList();
        }
        return outerElement;
    }
}

function expandItemView(view) {
    return function(event) {
        event.stopPropagation();
        var expandButtonImg = event.currentTarget.children[0];
        if (view.type == "shortInList") {
            if (!view.expandedView) {
                expandButtonImg.src = "/icons/left-arrow.svg";
                var colView = {type:"root", id:view.id};
                var col = createView(colView);
                view.whereToPutColumn.appendChild(col);
                view.expandedView = colView;
            } else {
                expandButtonImg.src = "/icons/right-arrow.svg";
                removeView(view.expandedView);
                delete view.expandedView;
            }
        } else {
            view.expanded = !view.expanded;
            if (view.expanded) {
                expandButtonImg.src = "/icons/up-arrow.svg";
                view.attListElement.hidden = false;
                updateAttributeViews(view, items[view.id]);
            } else {
                expandButtonImg.src = "/icons/down-arrow.svg";
                view.attListElement.hidden = true;
            }
        }
    }
}

function removeView(view, recursed) {
    const id = view.id;
    var initialListenList;
    if (!recursed) {
        initialListenList = JSON.stringify(listenList);
    }
    view.element.remove();
    findAndDestroyChildren(view.attListElement);
    if (itemViews[id].length == 1) {
        delete items[id];
        delete listenList[id];
        delete itemViews[id];
    } else {
        for (var i in itemViews[id]) {
            if (itemViews[id][i] === view) {
                itemViews[id].splice(i,1);
                break;
            }
        }
    }
    if (!recursed && initialListenList !== JSON.stringify(listenList)) {
        sendUpdatedListeningList();
    }
}

function findAndDestroyChildren(element) {
    if (element.itemView) {
        removeView(element.itemView, true);
    } else {
        for (var i = 0; i < element.children.length; i++) {
            findAndDestroyChildren(element.children[i]);
        }
    }
}

var interactionState = "none";
var interactionStateData;
var interactionStateIsId = false;

function setInteractionState(state, description, data, isId) {
    function getIds(x) {
        if (Object.hasOwn(x,"id")) {
            x = x.id;
        }
        if (!(x instanceof Array)) {
            x = [x];
        }
        return x;
    }
    if (interactionStateIsId) {
        var ids = getIds(interactionStateData);
        for (var i in ids) {
            var id = ids[i];
            for (var j in itemViews[id]) {
                itemViews[id][j].element.classList.remove("item-highlighted");
            }
        }
    }
    if (isId) {
        var ids = getIds(data);
        for (var i in ids) {
            var id = ids[i];
            for (var j in itemViews[id]) {
                itemViews[id][j].element.classList.add("item-highlighted");
            }
        }
    }
    interactionState = state;
    interactionStateData = data;
    interactionStateIsId = isId;
    document.getElementById("mode-description-text").textContent = description;
    document.getElementById("mode-description").style.visibility = description ? "visible" : "hidden";
}

function clickView(view) {
    return function(event) {
        switch (interactionState) {
            case "none":
                return;
            case "movingItem":
                var id = interactionStateData;
                setInteractionState("none");
                if (view.id == id) break;
                sendToServer("move-item", [id, view.id]);
                break;
            case "movingCommodity":
                var data = interactionStateData;
                setInteractionState("none");
                if (view.id == data.id && !data.attached) break;
                sendToServer("move-commodity", [data.id, data.type, data.quantity, data.attached, view.id]);
                break;
        }
    }
}

var attachedWarningShown = false;
function moveItem(id, attached) {
    return function(event) {
        event.stopPropagation();
        if (attached && !attachedWarningShown) {
            alert("Removing a part of something may require a tool, such as a knife, screwdriver or saw. This website doesn't know what counts as a suitable tool, so you'll need to apply this rule yourself.");
            attachedWarningShown = true;
        }
        setInteractionState("movingItem", "Click on another item to move the selected one into it.", id, true);
    }
}

function moveCommodity(data, attached, id) {
    return function(event) {
        event.stopPropagation();
        if (attached && !attachedWarningShown) {
            alert("Removing a part of something may require a tool, such as a knife, screwdriver or saw. This website doesn't know what counts as a suitable tool, so you'll need to apply this rule yourself.");
            attachedWarningShown = true;
        }
        setInteractionState("movingCommodity", "Click on another item to move the selected substance into it.", {id:id, type:data[0], quantity:data[1], attached:attached}, true);
        if (!event.shiftKey) {
            var decomposed = decomposeUnits(data[1], data[0]);
            var inputElements = [document.getElementById("commodity-amount-slider"), document.getElementById("commodity-amount-number")];
            for (var i in inputElements) {
                inputElements[i].max = decomposed.mantissa;
                inputElements[i].value = decomposed.mantissa;
            }
            document.getElementById("commodity-amount-context").textContent = decomposed.unit + " / " +decomposed.mantissa + decomposed.unit;
            document.getElementById("commodity-amount-dialog").showModal();
        }
    }
}

function commodityAmountSubmit() {
    var factor = decomposeUnits(interactionStateData.quantity).factor;
    interactionStateData.quantity = document.getElementById("commodity-amount-slider").value * factor;
    document.getElementById("commodity-amount-dialog").close();
}

function dialogCancel() {
    setInteractionState("none");
    var dialogs = document.getElementsByTagName("dialog");
    for (var i = 0; i < dialogs.length; i++) {
        dialogs[i].close();
    }
}

// TODO: Check that this doesn't mess up if focus is lost due to an update arriving from the server.
function makeAttViewEditable(element, attribute, id) {
    element.contentEditable = "true";
    element.addEventListener("focusout", function(event) {
        if (element.textContent != items[id][attribute]) {
            var data = {};
            data[attribute] = element.textContent;
            sendToServer("add-attribute", [id, data]);
        }
    });
}
