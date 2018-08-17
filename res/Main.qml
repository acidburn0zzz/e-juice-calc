import QtQuick 2.0
import QtQuick.Controls 2.2
import QtQuick.Controls 1.4
import QtQuick.Layouts 1.1
import QtQml.Models 2.1

ApplicationWindow {
    id: main
    visible: true
    title: titleOriginal + (titleExtra != '' ? ' - ' + titleExtra : '');
    width: 400
    height: 250

    Component.onCompleted: {
        init()
    }

    property var init: function() {
        decodeInputData(haskellInit())
        var filePath = JSON.parse(haskellGetFilePath())
        if(filePath != null) {
            titleExtra = filePath
        }
        calculate()
    }
    property var titleOriginal: 'E-Juice-Calc'
    property var titleExtra: ''
    property var getBatchSize: function() {
        return batchSize.value;
    }
    property var setBatchSize: function(value) {
        batchSize.value = value;
    }
    property var getFlavors: function() {
        var result = [];

        for(var i = 0; i<flavorModel.count; i++) {
            var row = flavorModel.get(i);
            result[result.length] = {
                flavorName: row.name,
                flavorPercentage: row.percentage,
                flavorIsVg: row.vg === 'Yes' ? true : false
            };
        }

        return result;
    }
    property var setFlavors: function(flavors) {
        flavorModel.clear()
        for(var i = 0; i<flavors.length; i++) {
            var flavor = flavors[i]
            flavorModel.append({
                name: flavor.flavorName,
                percentage: flavor.flavorPercentage,
                vg: flavor.flavorIsVg === true ? 'Yes' : 'No'
            })
        }
    }
    property var getBaseNicotineName: function() {
        return 'Nicotine Base'
    }
    property var getBaseNicotineStrength: function() {
        return baseNicotineStrength.value;
    }
    property var setBaseNicotineStrength: function(value) {
        baseNicotineStrength.value = value;
    }
    property var getBaseNicotineRatioPg: function() {
        return baseRatioPg.value;
    }
    property var getBaseNicotineRatioVg: function() {
        return baseRatioVg.value;
    }
    property var setBaseNicotineRatio: function(pg, vg) {
        baseRatioPg.value = pg
        baseRatioVg.value = vg
    }
    property var getTargetNicotineName: function() {
        return 'Target Liquid';
    }
    property var getTargetNicotineStrength: function() {
        return targetNicotineStrength.value;
    }
    property var setTargetNicotineStrength: function(value) {
        targetNicotineStrength.value = value;
    }
    property var getTargetNicotineRatioPg: function() {
        return targetRatioPg.value;
    }
    property var getTargetNicotineRatioVg: function() {
        return targetRatioVg.value;
    }
    property var setTargetNicotineRatio: function(pg, vg) {
        targetRatioPg.value = pg
        targetRatioVg.value = vg
    }
    property var encodeInputData: function() {
        return JSON.stringify({
            inputDataBatchSize: main.getBatchSize(),
            inputDataFlavors: getFlavors(),
            inputDataBaseLiquid: {
                liquidRatio: {
                    liquidRatioPg: getBaseNicotineRatioPg(),
                    liquidRatioVg: getBaseNicotineRatioVg()
                },
                liquidAmount: null,
                liquidName: getBaseNicotineName(),
                liquidNicotine: getBaseNicotineStrength()
            },
            inputDataTargetLiquid: {
                liquidRatio: {
                    liquidRatioPg: getTargetNicotineRatioPg(),
                    liquidRatioVg: getTargetNicotineRatioVg()
                },
                liquidAmount: null,
                liquidName: getTargetNicotineName(),
                liquidNicotine: getTargetNicotineStrength()
            }
        });
    }
    property var decodeInputData: function(str) {
        var obj = JSON.parse(str)
        setBatchSize(obj.inputDataBatchSize)
        setFlavors(obj.inputDataFlavors)
        setBaseNicotineStrength(obj.inputDataBaseLiquid.liquidNicotine)
        setBaseNicotineRatio(obj.inputDataBaseLiquid.liquidRatio.liquidRatioPg, obj.inputDataBaseLiquid.liquidRatio.liquidRatioVg)
        setTargetNicotineStrength(obj.inputDataTargetLiquid.liquidNicotine)
        setTargetNicotineRatio(obj.inputDataTargetLiquid.liquidRatio.liquidRatioPg, obj.inputDataTargetLiquid.liquidRatio.liquidRatioVg)
    }
    property var decodeRecipe: function(str) {
        resultModel.clear();
        var recipe = JSON.parse(str);
        if(recipe !== null) {
            var liquids = recipe.recipeLiquids;
            for(var i = 0; i<liquids.length; i++) {
                var liquid = liquids[i];
                resultModel.append({
                    name: liquid.recipeLiquidName,
                    ml: liquid.recipeLiquidAmount.toFixed(2)
                });
            }
            resultModel.append({
                name: 'Sum',
                ml: recipe.recipeSum.toFixed(2)
            });
        }
    }
    property var calculate: function() {
        decodeRecipe(haskellCalc(encodeInputData())); // call Haskell function "haskellCalc"
    }
    // Utility functions
    property var urlToPath: function(urlString) {
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

    menuBar: MenuBar {
        Menu {
            title: 'File'
            MenuItem {
                text: 'Open File...'
                shortcut: 'Ctrl+O'
                onTriggered: {
                    var component = Qt.createComponent("OpenDialog.qml")
                    var dialog = component.createObject(main)
                    dialog.acceptedCallback = function() {
                        var filePath = urlToPath(dialog.fileUrl.toString())
                        var data = haskellOpen(filePath)
                        if(data != null) {
                            decodeInputData(data)
                            titleExtra = filePath
                            calculate()
                        }
                    }
                }
            }
            MenuSeparator {}
            MenuItem {
                text: 'Save'
                shortcut: 'Ctrl+S'
                onTriggered: {
                    var fp = JSON.parse(haskellGetFilePath())
                    var saveCallback = function(filePath) {
                        if(haskellSave(filePath, encodeInputData())) {
                            titleExtra = filePath
                            console.log('successfully saved to ' + filePath)
                        } else {
                            console.log('failed to save to ' + filePath)
                        }
                    }
                    // check if file was already opened/saved before
                    if(fp === null) {
                        // show dialog to select file path
                        var component = Qt.createComponent("SaveDialog.qml")
                        var dialog = component.createObject(main)
                        dialog.acceptedCallback = function() {
                            saveCallback(urlToPath(dialog.fileUrl.toString()))
                        }
                    } else {
                        saveCallback(fp)
                    }
                }
            }
            MenuItem {
                text: 'Save As...'
                shortcut: 'Ctrl+Shift+S'
                onTriggered: {
                    var filePath = haskellGetFilePath()
                    var saveCallback = function(filePath) {
                        if(haskellSave(filePath, encodeInputData())) {
                            titleExtra = filePath
                            console.log('successfully saved to ' + filePath)
                        } else {
                            console.log('failed to save to ' + filePath)
                        }
                    }
                    var component = Qt.createComponent("SaveDialog.qml")
                    var dialog = component.createObject(main)
                    dialog.acceptedCallback = function() {
                        filePath = urlToPath(dialog.fileUrl.toString())
                        saveCallback(filePath)
                    }
                }
            }
            MenuSeparator {}
            MenuItem {
                text: 'Quit'
                onTriggered: Qt.quit()
            }
        }
        Menu {
            title: 'Help'
            MenuItem {
                text: 'Help'
                onTriggered: Qt.openUrlExternally("https://szibele.com/");
            }
            MenuSeparator {}
            MenuItem {
                text: 'About'
                onTriggered: Qt.createComponent("AboutDialog.qml").createObject(main).version = haskellGetVersion()
            }
        }
    }
    ColumnLayout {
        spacing: 10
        anchors.fill: parent
        GroupBox {
            title: 'Batch'
            Layout.fillWidth: true
            ColumnLayout {
                width: parent.width
                Label {
                    text: 'Enter the amount of liquid you would like to make'
                }
                SpinBox  {
                    id: batchSize
                    value: 100
                    minimumValue: 10
                    maximumValue: 100000
                    suffix: ' ml'
                    horizontalAlignment: Qt.AlignRight
                    Layout.fillWidth: true
                    onValueChanged: { calculate() }
                }
            }
        }
        GroupBox {
            title: 'Flavoring'
            Layout.fillWidth: true
            ColumnLayout {
                width: parent.width
                ListModel {
                    id: flavorModel
                }
                TableView {
                    id: flavorView
                    model: flavorModel
                    selectionMode: SelectionMode.SingleSelection
                    Layout.fillWidth: true
                    Layout.preferredHeight: 110
                    TableViewColumn { id: flavorViewColName; role: 'name'; title: 'Flavor'; width: flavorView.width - flavorViewColPercentage.width - flavorViewColVg.width - 5 }
                    TableViewColumn { id: flavorViewColVg; role: 'vg'; title: 'VG'; width: 50; horizontalAlignment: Qt.AlignRight }
                    TableViewColumn { id: flavorViewColPercentage; role: 'percentage'; title: '%'; width: 50; horizontalAlignment: Qt.AlignRight }
                }
                RowLayout {
                    Layout.alignment: Qt.AlignRight
                    Button {
                        text: 'add'
                        onClicked: {
                            var component = Qt.createComponent("FlavorDialog.qml")
                            var dialog    = component.createObject(main)
                            dialog.acceptedCallback = function() {
                                flavorModel.append({
                                    name: dialog.flavorName,
                                    percentage: dialog.flavorPercentage,
                                    vg: dialog.flavorIsVg ? 'Yes' : 'No'
                                })
                                calculate()
                            }
                        }
                    }
                    Button {
                        text: 'remove'
                        onClicked: function() {
                            if(flavorView.selection.count > 0) {
                                flavorView.selection.forEach(function(rowIndex) { flavorModel.remove(rowIndex); });
                                calculate();
                            }
                        }
                    }
                }
            }
        }
        RowLayout {
            spacing: 10
            GroupBox {
                title: 'Nicotine base liquid'
                Layout.preferredWidth: parent.width / 2
                ColumnLayout {
                    anchors.fill: parent
                    Label { text: 'Nicotine strength (mg/ml)' }
                    SpinBox {
                        id: baseNicotineStrength
                        minimumValue: 0
                        maximumValue: 500  // TODO: what is the maximum possible mg/ml?
                        suffix: ' mg'
                        value: 20
                        horizontalAlignment: Qt.AlignRight
                        Layout.fillWidth: true
                        onValueChanged: { calculate() }
                    }
                    GridLayout {
                        columns: 2
                        Layout.fillWidth: true
                        Label { text: 'VG' }
                        Label { text: 'PG' }
                        SpinBox {
                            id: baseRatioVg
                            minimumValue: 0
                            maximumValue: 100
                            suffix: ' %'
                            value: 0
                            horizontalAlignment: Qt.AlignRight
                            onValueChanged: { baseRatioPg.value = 100 - this.value; calculate() }
                        }
                        SpinBox {
                            id: baseRatioPg
                            minimumValue: 0
                            maximumValue: 100
                            suffix: ' %'
                            value: 0
                            horizontalAlignment: Qt.AlignRight
                            onValueChanged: { baseRatioVg.value = 100 - this.value; calculate() }
                        }
                    }
                }
            }
            GroupBox {
                title: 'Target mixture'
                Layout.fillWidth: true
                ColumnLayout {
                    anchors.fill: parent
                    Label { text: 'Nicotine strength (mg/ml)' }
                    SpinBox {
                        id: targetNicotineStrength
                        minimumValue: 0
                        maximumValue: 500  // TODO: what is the maximum possible mg/ml?
                        suffix: ' mg'
                        value: 3
                        horizontalAlignment: Qt.AlignRight
                        Layout.fillWidth: true
                        onValueChanged: { calculate() }
                    }
                    GridLayout {
                        columns: 2
                        Layout.fillWidth: true
                        Label { text: 'VG' }
                        Label { text: 'PG' }
                        SpinBox {
                            id: targetRatioVg
                            minimumValue: 0
                            maximumValue: 100
                            suffix: ' %'
                            value: 100-targetRatioPg.value
                            horizontalAlignment: Qt.AlignRight
                            onValueChanged: { targetRatioPg.value = 100 - this.value; calculate() }
                        }
                        SpinBox {
                            id: targetRatioPg
                            minimumValue: 0
                            maximumValue: 100
                            suffix: ' %'
                            value: 100-targetRatioVg.value
                            horizontalAlignment: Qt.AlignRight
                            onValueChanged: { targetRatioVg.value = 100 - this.value; calculate() }
                        }
                    }
                }
            }
        }
        GroupBox {
            title: 'Result'
            Layout.fillWidth: true
            ColumnLayout {
                anchors.fill: parent
                ListModel {
                    id: resultModel
                }
                TableView {
                    id: resultView
                    model: resultModel
                    Layout.fillWidth: true
                    Layout.preferredHeight: 200
                    TableViewColumn { id: resultViewColName; role: 'name'; title: 'Liquid'; width: resultView.width - resultViewColMl.width - 20 }
                    TableViewColumn { id: resultViewColMl; role: 'ml'; title: 'ml'; width: 100 }
                }
            }
        }
    }
}
