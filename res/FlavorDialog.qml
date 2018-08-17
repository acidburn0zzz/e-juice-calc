import QtQuick 2.3
import QtQuick.Controls 1.2
import QtQuick.Dialogs 1.2
import QtQuick.Layouts 1.1

Dialog {
    id: flavorDialog
    visible: true
    title: "Add new flavor"
    width: 300
    modality: Qt.ApplicationModal
    standardButtons: StandardButton.Ok | StandardButton.Cancel

    property var flavorName: ''
    property var flavorPercentage: 0
    property var flavorIsVg: false
    property var acceptedCallback: function() {}

    onAccepted: {
        flavorName = name.text
        flavorPercentage = percentage.value
        flavorIsVg = isVg.checked
        acceptedCallback()
    }
    Component.onCompleted: {
        name.focus = true;
    }

    ColumnLayout {
        anchors {
            left: parent.left
            right: parent.right
        }
        Label { text: 'Name' }
        TextField {
            id: name
            anchors {
                left: parent.left
                right: parent.right
            }
        }
        Label { text: 'Percentage' }
        SpinBox  {
            id: percentage
            minimumValue: 1
            maximumValue: 100
            suffix: ' %'
            value: 3
            horizontalAlignment: Qt.AlignRight
            anchors {
                left: parent.left
                right: parent.right
            }
        }
        CheckBox {
            id: isVg
            text: 'Flavor is VG based'
            checked: false
        }
    }
}
