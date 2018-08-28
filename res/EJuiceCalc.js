// globals in this file
var baseUrl = '';

// utility functions
function get(url, resolve, reject) { xhr(url, 'GET', null, resolve, reject); }
function post(url, data, resolve, reject) { xhr(url, 'POST', data, resolve, reject); }
function xhr(urlPath, method, data, resolve, reject) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, baseUrl + urlPath, true);
    xhr.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
    xhr.onreadystatechange = function() {
        if(xhr.readyState === XMLHttpRequest.DONE) {
            if(xhr.status === 200) {
                var obj = JSON.parse(xhr.responseText);
                if(obj.success) {
                    resolve(obj.content);
                } else {
                    reject(obj.message);
                }
            } else {
                reject("Network error.");
            }
        }
    }
    if(data != null) {
        xhr.send(JSON.stringify(data));
    } else {
        xhr.send();
    }
}
function urlToPath(urlString) {
    var s
    if (urlString.startsWith("file:///")) {
        var k = urlString.charAt(9) === ':' ? 8 : 7
        s = urlString.substring(k)
    } else {
        s = urlString
    }
    return decodeURIComponent(s);
}
// End utility functions

// Interface to Haskell.
function haskellInit(resolve, reject) { get('/init', resolve, reject); }
function haskellOpen(filePath, resolve, reject) { post('/open', { filePath: filePath }, resolve, reject); }
function haskellSave(filePath, inputData, resolve, reject) { post('/save', { filePath: filePath, inputData: inputData }, resolve, reject); }
function haskellFilePath(resolve, reject) { get('/filepath', resolve, resolve); }
function haskellCalculate(inputData, resolve, reject) { post('/calculate', inputData, resolve, reject); }
function haskellVersion(resolve, reject) { get('/version', resolve, reject); };
function haskellExit(resolve, reject) { get('/exit', resolve, reject); }
// End interface to Haskell.

// Getters and setters for GUI elements.
function getBatchSize() { return batchSize.value; }
function setBatchSize(value) { batchSize.value = value; }
function getFlavors() {
    var result = [];
    for(var i = 0; i<flavorModel.count; i++) {
        var row = flavorModel.get(i);
        result[result.length] = {
            name: row.name,
            percentage: row.percentage,
            isVg: row.vg === 'Yes' ? true : false
        };
    }
    return result;
}
function setFlavors(flavors) {
    flavorModel.clear()
    for(var i = 0; i<flavors.length; i++) {
        var flavor = flavors[i]
        flavorModel.append({
            name: flavor.name,
            percentage: flavor.percentage,
            vg: flavor.isVg === true ? 'Yes' : 'No'
        })
    }
}
function getBaseNicotineName() { return 'Nicotine Base' }
function getBaseNicotineStrength() { return baseNicotineStrength.value; }
function setBaseNicotineStrength(value) { baseNicotineStrength.value = value; }
function getBaseNicotineRatioPg() { return baseRatioPg.value; }
function getBaseNicotineRatioVg() { return baseRatioVg.value; }
function setBaseNicotineRatio(pg, vg) {
    baseRatioPg.value = pg
    baseRatioVg.value = vg
}
function getTargetNicotineName() { return 'Target Liquid'; }
function getTargetNicotineStrength() { return targetNicotineStrength.value; }
function setTargetNicotineStrength(value) { targetNicotineStrength.value = value; }
function getTargetNicotineRatioPg() { return targetRatioPg.value; }
function getTargetNicotineRatioVg() { return targetRatioVg.value; }
function setTargetNicotineRatio(pg, vg) {
    targetRatioPg.value = pg
    targetRatioVg.value = vg
}
function getInputData() {
    return {
        batchSize: getBatchSize(),
        flavors: getFlavors(),
        baseLiquid: {
            ratio: {
                pg: getBaseNicotineRatioPg(),
                vg: getBaseNicotineRatioVg()
            },
            amount: null,
            name: getBaseNicotineName(),
            nicotine: getBaseNicotineStrength()
        },
        targetLiquid: {
            ratio: {
                pg: getTargetNicotineRatioPg(),
                vg: getTargetNicotineRatioVg()
            },
            amount: null,
            name: getTargetNicotineName(),
            nicotine: getTargetNicotineStrength()
        }
    };
}
function setInputData(obj) {
    setBatchSize(obj.batchSize)
    setFlavors(obj.flavors)
    setBaseNicotineStrength(obj.baseLiquid.nicotine)
    setBaseNicotineRatio(obj.baseLiquid.ratio.pg, obj.baseLiquid.ratio.vg)
    setTargetNicotineStrength(obj.targetLiquid.nicotine)
    setTargetNicotineRatio(obj.targetLiquid.ratio.pg, obj.targetLiquid.ratio.vg)
}
function setRecipe(recipe) {
    resultModel.clear();
    if(recipe !== null) {
        var liquids = recipe.liquids;
        for(var i = 0; i<liquids.length; i++) {
            var liquid = liquids[i];
            resultModel.append({
                name: liquid.name,
                ml: liquid.amount.toFixed(2)
            });
        }
        resultModel.append({
            name: 'Sum',
            ml: recipe.sum.toFixed(2)
        });
    }
}
// End getters and setters

// This function MUST be called before functions in this file should be used.
function initialize(host, port) {
    baseUrl = 'http://' + host + ':' + port
    haskellInit(function(arr) {
        var resDir = arr[0];
        var inputData = arr[1];
        // set icon
        qmllb.setWindowIcon(main, resDir + 'icon2.png')
        // set gui components based on input data
        main.recalculateOnChange = false;
        setInputData(inputData);
        main.recalculateOnChange = true;
        haskellFilePath(function(filePath) {
            if(filePath != null) {
                titleExtra = filePath;
            }
            calculate();
        }, function() {
            // error
        })
    }, function() {
        // Error
    })
}
function calculate() {
    var inputData = getInputData();
    haskellCalculate(inputData, function(recipe) {
        setRecipe(recipe);
    }, function() {
        // error
    });
}

// GUI functions
// Menu
function menuFileOpen() {
    var component = Qt.createComponent("OpenDialog.qml");
    var dialog = component.createObject(main);
    dialog.acceptedCallback = function() {
        var filePath = urlToPath(dialog.fileUrl.toString());
        haskellOpen(filePath, function(inputData) {
            if(inputData != null) {
                main.recalculateOnChange = false;
                setInputData(inputData);
                main.recalculateOnChange = true;
                main.titleExtra = filePath;
                calculate();
            }
        }, function() {})
    }
}
function menuFileSaveHelper(filePath, resolve, reject) {
    haskellSave(filePath, getInputData(), function() {
        titleExtra = filePath
        resolve();
    }, function() {
        reject();
    });
}
function menuFileSave() {
    // get the filepath and save
    haskellFilePath(function(filePath) {
        // check if file was already opened/saved before
        if(filePath === null) {
            // show dialog to select file path
            menuFileSaveAs(function() {
                // TODO: success!
            }, function() {
                // TODO: error!
            });
        } else {
            menuFileSaveHelper(filePath, function() {
                // TODO: success!
            }, function() {
                // TODO: error!
            });
        }
    }, function() {
        // TODO: error!
    });
}
function menuFileSaveAs(resolve, reject) {
    var component = Qt.createComponent("SaveDialog.qml")
    var dialog = component.createObject(main)
    dialog.acceptedCallback = function() {
        var filePath = EJuiceCalc.urlToPath(dialog.fileUrl.toString())
        menuFileSaveHelper(filePath, function() {
            if(resolve != null) {
                resolve();
            } else {
                // TODO: success!
            }
        }, function() {
            if(reject != null) {
                reject();
            } else {
                // TODO: error!
            }
        })
    }
}
function menuFileQuit() {
    haskellExit(function() {
        Qt.quit();
    }, function() {
        // TODO: error
    })
}
function menuHelpHelp() {
    Qt.openUrlExternally("https://szibele.com/");
}
function menuHelpAbout() {
    haskellVersion(function(version) {
        Qt.createComponent("AboutDialog.qml").createObject(main).version = version;
    }, function() {
        // TODO: error
    })
}
// Flavor
function flavorAdd() {
    var component = Qt.createComponent("FlavorDialog.qml");
    var dialog    = component.createObject(main);
    dialog.acceptedCallback = function() {
        flavorModel.append({
            name: dialog.flavorName,
            percentage: dialog.flavorPercentage,
            vg: dialog.flavorIsVg ? 'Yes' : 'No'
        });
        calculate();
    }
}
function flavorRemove() {
    if(flavorView.selection.count > 0) {
        flavorView.selection.forEach(function(rowIndex) { flavorModel.remove(rowIndex); });
        calculate();
    }
}