import QtQuick 2.0
import QtQuick.Controls 2.2
import QtQuick.Controls 1.4
import QtQuick.Layouts 1.1
import QtQml.Models 2.1

import "EJuiceCalc.js" as EJuiceCalc

ApplicationWindow {
    id: main
    visible: true
    title: titleOriginal + (titleExtra != '' ? ' - ' + titleExtra : '');
    width: 400
    height: 250

    Component.onCompleted: {
        EJuiceCalc.initialize(qmllb.getHost(), qmllb.getPort());
    }
    onClosing: {
        EJuiceCalc.menuFileQuit();
    }

    property var titleOriginal: 'E-Juice-Calc'
    property var titleExtra: ''

    menuBar: MenuBar {
        Menu {
            title: 'File'
            MenuItem {
                text: 'Open File...'
                shortcut: 'Ctrl+O'
                onTriggered: EJuiceCalc.menuFileOpen()
            }
            MenuSeparator {}
            MenuItem {
                text: 'Save'
                shortcut: 'Ctrl+S'
                onTriggered: EJuiceCalc.menuFileSave()
            }
            MenuItem {
                text: 'Save As...'
                shortcut: 'Ctrl+Shift+S'
                onTriggered: EJuiceCalc.menuFileSaveAs()
            }
            MenuSeparator {}
            MenuItem {
                text: 'Quit'
                shortcut: 'Ctrl+Q'
                onTriggered: EJuiceCalc.menuFileQuit();
            }
        }
        Menu {
            title: 'Help'
            MenuItem {
                text: 'Help'
                onTriggered: EJuiceCalc.menuHelpHelp();
            }
            MenuSeparator {}
            MenuItem {
                text: 'About'
                onTriggered: EJuiceCalc.menuHelpAbout();
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
                    onValueChanged: { EJuiceCalc.calculate() }
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
                        onClicked: EJuiceCalc.flavorAdd()
                    }
                    Button {
                        text: 'remove'
                        onClicked: EJuiceCalc.flavorRemove()
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
                        onValueChanged: EJuiceCalc.calculate()
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
                            onValueChanged: { baseRatioPg.value = 100 - this.value; EJuiceCalc.calculate(); }
                        }
                        SpinBox {
                            id: baseRatioPg
                            minimumValue: 0
                            maximumValue: 100
                            suffix: ' %'
                            value: 0
                            horizontalAlignment: Qt.AlignRight
                            onValueChanged: { baseRatioVg.value = 100 - this.value; EJuiceCalc.calculate(); }
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
                        onValueChanged: EJuiceCalc.calculate()
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
                            onValueChanged: { targetRatioPg.value = 100 - this.value; EJuiceCalc.calculate(); }
                        }
                        SpinBox {
                            id: targetRatioPg
                            minimumValue: 0
                            maximumValue: 100
                            suffix: ' %'
                            value: 100-targetRatioVg.value
                            horizontalAlignment: Qt.AlignRight
                            onValueChanged: { targetRatioVg.value = 100 - this.value; EJuiceCalc.calculate(); }
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
