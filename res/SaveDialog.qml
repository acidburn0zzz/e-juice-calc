import QtQuick 2.2
import QtQuick.Dialogs 1.0

FileDialog {
    id: saveDialog
    folder: shortcuts.home // TODO: XDG
    selectExisting: false
    nameFilters: [ "E-Juice Calc files (*.ejc)", "All files (*)" ]

    property var acceptedCallback: function() {}
    property var rejectedCallback: function() {}

    onAccepted: acceptedCallback()
    onRejected: rejectedCallback()

    Component.onCompleted: visible = true
}
