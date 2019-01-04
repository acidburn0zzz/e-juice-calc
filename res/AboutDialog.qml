import QtQuick 2.3
import QtQuick.Controls 1.2
import QtQuick.Dialogs 1.2
import QtQuick.Layouts 1.1

Dialog {
    id: aboutDialog
    visible: true
    title: "About"
    modality: Qt.ApplicationModal
    standardButtons: StandardButton.Ok

    property var version

    ColumnLayout {
        anchors {
            left: parent.left
            right: parent.right
        }
        Label {
            text: 'E-Juice Calc'
            font.bold: true
        }
        Label {
            text: 'Version: ' + version
        }
        Label {
            text: 'License: GNU GPLv2'
        }
        Label { text: 'Copyright (C) 2018-2019 Richard Szibele' }
    }
}
