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

function newItemHandler(id, oldItem, newItem) {
    if (!itemViews[id]) {
        itemViews[id] = [];
    }
    for (var viewI in itemViews[id]) {
        var view = itemViews[id][viewI];
        if (!newItem) {
            alert("A watched item disappeared. The code to deal with this hasn't been written yet.");//TODO
        } else if (view.loading) {
            var newElement = createView(view, newItem);
            view.element.replaceWith(newElement);
            view.element = newElement;
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
    for (var attI = visibleAttributes.length-1; attI >= 0; attI --) {
        var att = visibleAttributes[attI];
        var attView = view.attElements[att];
        if (attView && !item[att]) {
            view.attListElement.removeChild(attView);
            delete view.attElements[att];
        } else if (!attView && item[att]) {
            var newAttView = createAttView[att](item[att], item, view.type, view.id);
            view.attListElement.insertBefore(newAttView, lastExistingNode);
            view.attElements[att] = newAttView;
            lastExistingNode = newAttView;
        } else if (attView && item[att]) {
            if (!deepEquals(view.attBeingDisplayed[att], item[att])) {
                if (updateAttView[att]) {
                    updateAttView[att](view.attElements[att], item[att], item, view.type, view.id);
                } else {
                    var newAttView = createAttView[att](item[att], item, view.type, view.id);
                    attView.replace(newAttView);
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
    itemViews[view.id].push(view);
    view.loading = !items[view.id];
    var rootElement;
    if (view.loading) {
        rootElement = document.createElement("div");
        rootElement.className = "itemView";
        rootElement.textContent = "Loading...";
    } else {
        var enclosingElement = document.createElement("div"); //An extra outer element must be used because setting outerHTML doesn't work on an element that isn't actually in the DOM tree.
        switch(view.type) {
            case "shortInList":
                enclosingElement.innerHTML = '<div class="itemView"><div class="itemViewHeader"><div class="itemViewName"></div><button class="expandButton itemButton"><img src="/icons/right-arrow.svg" alt="expand"/></button></div></div>';
                break;
            case "content":
                enclosingElement.innerHTML = '<div class="itemView"><div class="itemViewHeader"><div class="itemViewName"></div><button class="removeButton itemButton"><img src="/icons/hand.png" alt="move"/></button><button class="expandButton itemButton"><img src="/icons/down-arrow.svg" alt="expand"/></button></div><ul class="attributeList" hidden></ul></div>';
                break;
            case "component":
                enclosingElement.innerHTML = '<div class="itemView"><div class="itemViewHeader"><div class="itemViewName"></div><button class="removeButton itemButton"><img src="/icons/knife.png" alt="remove"/></button><button class="expandButton itemButton"><img src="/icons/down-arrow.svg" alt="expand"/></button></div><ul class="attributeList" hidden></ul></div>';
                break;
            case "root":
                enclosingElement.innerHTML = '<div class="itemView rootItemView column"><div class="itemViewHeader"><div class="itemViewName"></div></div><ul class="attributeList"></ul></div>';
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
        view.nameElement = getElement("itemViewName");
        view.nameElement.textContent = items[view.id].name || "[Unnamed Item]";
        var expandButton = getElement("expandButton");
        if (expandButton) {
            expandButton.addEventListener("click", expandItemView(view));
        }
        var removeButton = getElement("removeButton");
        if (removeButton) {
            removeButton.addEventListener("click", moveItem(view.id, view.type == component));
        }
        view.attListElement = getElement("attributeList");
        view.attElements = {};
        view.expanded = view.type == "root";
        if (view.expanded) {
            updateAttributeViews(view);
        }
    }
    return rootElement;
}

var visibleAttributes = ["description"];

const createAttView = {
    description:function(desc) {
        const element = document.createElementById("div");
        element.className = "attributeView";
        element.textContent = desc;
    }
};

function expandItemView(view) {
    return function(event) {
        view.expanded = !view.expanded;
        var expandButtonImg = event.currentTarget.children[0];
        if (view.expanded) {
            if (view.type == "shortInList") {
                expandButtonImg.src = "/icons/left-arrow.svg";
                var colView = {type:"root", id:view.id};
                var col = createView(colView);
                view.whereToPutColumn.appendChild(col);
                view.expandedView = colView;
            } else {
                expandButtonImg.src = "/icons/up-arrow.svg";
                view.attListElement.hidden = false;
                updateAttributeViews(view, items[view.id]);
            }
        } else {
            if (view.type == "shortInList") {
                expandButtonImg.src = "/icons/right-arrow.svg";
                removeView(view.expandedView);
                delete view.expandedView;
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

function clickView(view) {
    return function(event) {
        switch (interactionState) {
            case "none":
                return;
            case "movingItem":
                alert("Placeholder: moving item "+interactionStateData); //TODO
                interactionState = "none";
                break;
        }
    }
}

var attachedWarningShown = false;
function moveItem(id, attached) {
    if (attached && !attachedWarningShown) {
        alert("Removing a part of something may require a tool, such as a knife, screwdriver or saw. This website doesn't know what counts as a suitable tool, so you'll need to apply this rule yourself.");
        attachedWarningShown = true;
    }
    interactionState = "movingItem";
    interactionStateData = id;
}
