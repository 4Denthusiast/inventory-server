const gm = false; //There are some things it is more convenient to put in the common script and alter depending on who's using it.

function specificNewItemHandler(id, oldItem, newItem) {
}

function specificCheckPCView(view) {
    if (view.level == playerCharacters[view.player].length - 2 && playerCharacters[view.player][view.level + 1].limit) {
        if (!view.dependentView) {
            view.dependentView = {type:"self",id:view.id};
            createView(view.dependentView);
            document.getElementById("self-column").appendChild(view.dependentView.element);
            
            view.inventoryView = {type:"inventory",id:view.id};
            createView(view.inventoryView);
            document.getElementById("inventory-column").appendChild(view.inventoryView.element);
        }
    } else {
        removeViewOf(document.getElementById("self-column"), view);
        removeViewOf(document.getElementById("inventory-column"), view);
    }
    if (view.limit) {
        if (!view.dependentView) {
            view.dependentView = {type:"root", id:view.id};
            createView(view.dependentView)
            document.getElementById("surroundings-column").appendChild(view.dependentView.element);
        }
    } else {
        removeViewOf(document.getElementById("surroundings-column"), view);
    }
}

function removeViewOf(column, chainView) {
    for (var i = column.children.length - 1; i >= 0; i--) {
        var element = column.children[i];
        if (element.itemView && element.itemView.id == chainView.id) {
            if (element.itemView == chainView.dependentView) {
                delete chainView.dependentView;
            }
            if (element.itemView == chainView.inventoryView) {
                delete chainView.inventoryView; //I'm not sure this step is actually necessary, but it's probably a good idea to tidy this up just in case.
            }
            removeView(element.itemView);
        }
    }
}
