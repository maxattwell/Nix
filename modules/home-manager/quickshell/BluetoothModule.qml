import QtQuick
import QtQuick.Controls
import Quickshell
import Quickshell.Bluetooth

Rectangle {
  property bool hovered: false
  property var panelWindow
  property int barHeight
  property var radioModule
  property alias bluetoothPopup: bluetoothPopup
  property alias bluetoothHideAnim: bluetoothHideAnim
  
  width: bluetoothText.width + 20
  height: 26
  y: 7
  color: bluetoothPopup.visible ? "#3c3836" : (hovered ? "#d5c4a1" : "#ebdbb2")
  radius: 7

  Text {
    id: bluetoothText
    anchors.centerIn: parent
    text: Bluetooth.defaultAdapter && Bluetooth.defaultAdapter.enabled ? "󰂯" : "󰂲"
    font.family: "Overpass Nerd Font"
    font.pixelSize: 14
    color: bluetoothPopup.visible ? "#a89984" : (parent.parent.hovered ? "#282828" : "#3c3836")
  }

  MouseArea {
    anchors.fill: parent
    hoverEnabled: true
    onEntered: parent.hovered = true
    onExited: parent.hovered = false
    onClicked: {
      if (bluetoothPopup.visible) {
        bluetoothHideAnim.start()
      } else {
        if (radioModule && radioModule.radioPopup && radioModule.radioPopup.visible) {
          radioModule.radioHideAnim.start()
          bluetoothDelayTimer.start()
        } else {
          bluetoothPopup.visible = true
        }
      }
    }
  }

  Timer {
    id: bluetoothDelayTimer
    interval: 220
    onTriggered: bluetoothPopup.visible = true
  }

  PopupWindow {
    id: bluetoothPopup
    anchor.window: panelWindow
    anchor.rect.x: panelWindow.width
    anchor.rect.y: barHeight
    implicitWidth: 300
    implicitHeight: 350
    color: "transparent"

    Rectangle {
      id: bluetoothRect
      anchors.fill: parent
      color: "#fbf1c7"
      radius: 10
      transform: Translate { id: bluetoothTransform; y: -bluetoothRect.height }

      PropertyAnimation {
        id: bluetoothShowAnim
        target: bluetoothTransform
        property: "y"
        from: -bluetoothRect.height
        to: 0
        duration: 250
        easing.type: Easing.OutCubic
      }

      PropertyAnimation {
        id: bluetoothHideAnim
        target: bluetoothTransform
        property: "y"
        from: 0
        to: -bluetoothRect.height
        duration: 200
        easing.type: Easing.InCubic
        onFinished: bluetoothPopup.visible = false
      }

      Component.onCompleted: {
        bluetoothPopup.onVisibleChanged.connect(function() {
          if (bluetoothPopup.visible) {
            bluetoothRect.visible = true
            bluetoothShowAnim.start()
          }
        })
      }

      component BluetoothButton: Rectangle {
        property string text: ""
        property bool isActive: false
        property bool isEnabled: true
        property bool hovered: false
        
        signal clicked()
        
        width: implicitWidth
        height: 30
        color: {
          if (!isEnabled) return "#ebdbb2"
          if (isActive) return "#3c3836"
          if (hovered) return "#d5c4a1"
          return "#ebdbb2"
        }
        radius: 8
        opacity: isEnabled ? 1.0 : 0.5
        
        Text {
          anchors.centerIn: parent
          text: parent.text
          font.family: "Overpass Nerd Font"
          font.pixelSize: 12
          color: {
            if (!parent.isEnabled) return "#3c3836"
            if (parent.isActive) return "#a89984"
            if (parent.hovered) return "#282828"
            return "#3c3836"
          }
        }

        MouseArea {
          anchors.fill: parent
          enabled: parent.isEnabled
          hoverEnabled: true
          onEntered: parent.hovered = true
          onExited: parent.hovered = false
          onClicked: parent.clicked()
        }
      }

      Column {
        anchors.fill: parent
        anchors.margins: 10
        spacing: 10

        Column {
          id: adapterControls
          width: parent.width
          spacing: 8

          Text {
            text: "Bluetooth"
            font.family: "Overpass Nerd Font"
            font.pixelSize: 14 + 2
            color: "#282828"
            font.bold: true
          }

          Row {
            width: parent.width
            spacing: 10

            BluetoothButton {
              implicitWidth: 60
              text: Bluetooth.defaultAdapter && Bluetooth.defaultAdapter.enabled ? "ON" : "OFF"
              isActive: Bluetooth.defaultAdapter && Bluetooth.defaultAdapter.enabled
              onClicked: {
                if (Bluetooth.defaultAdapter) {
                  Bluetooth.defaultAdapter.enabled = !Bluetooth.defaultAdapter.enabled
                }
              }
            }

            BluetoothButton {
              implicitWidth: 80
              text: Bluetooth.defaultAdapter && Bluetooth.defaultAdapter.discovering ? "Scanning..." : "Scan"
              isActive: Bluetooth.defaultAdapter && Bluetooth.defaultAdapter.discovering
              isEnabled: Bluetooth.defaultAdapter && Bluetooth.defaultAdapter.enabled
              onClicked: {
                if (Bluetooth.defaultAdapter) {
                  Bluetooth.defaultAdapter.discovering = !Bluetooth.defaultAdapter.discovering
                }
              }
            }
          }
        }

        Text {
          text: "Devices"
          font.family: "Overpass Nerd Font"
          font.pixelSize: 14
          color: "#282828"
          visible: Bluetooth.defaultAdapter && Bluetooth.defaultAdapter.devices.length > 0
        }

        ScrollView {
          id: deviceScrollView
          width: parent.width
          height: Math.min(250, contentHeight)
          contentHeight: deviceList.height
          clip: true
          
          ScrollBar.vertical.policy: ScrollBar.AsNeeded
          ScrollBar.horizontal.policy: ScrollBar.AlwaysOff

          Column {
            id: deviceList
            width: deviceScrollView.width
            spacing: 5

            Repeater {
              model: Bluetooth.defaultAdapter ? Bluetooth.defaultAdapter.devices : []
              delegate: Rectangle {
                required property var modelData
                width: deviceList.width - 20
                height: 50
                color: deviceMouseArea.containsMouse ? "#d5c4a1" : "#ebdbb2"
                radius: 7

                Row {
                  anchors.left: parent.left
                  anchors.leftMargin: 10
                  anchors.verticalCenter: parent.verticalCenter
                  spacing: 10

                  Text {
                    text: modelData.connected ? "󰂱" : (modelData.paired ? "󰂯" : "󰂲")
                    font.family: "Overpass Nerd Font"
                    font.pixelSize: 14
                    color: modelData.connected ? "#282828" : "#3c3836"
                  }

                  Column {
                    anchors.verticalCenter: parent.verticalCenter
                    
                    Text {
                      text: modelData.name || modelData.address
                      font.family: "Overpass Nerd Font"
                      font.pixelSize: 12 + 1
                      color: "#3c3836"
                    }

                    Text {
                      text: {
                        if (modelData.pairing) return "Pairing..."
                        if (modelData.state === 1) return "Connecting..."
                        if (modelData.state === 2) return "Disconnecting..."
                        return modelData.paired ? "Paired" : "Available"
                      }
                      font.family: "Overpass Nerd Font"
                      font.pixelSize: 12 - 1
                      color: "#282828"
                      opacity: 0.8
                    }
                  }
                }

                Row {
                  anchors.right: parent.right
                  anchors.rightMargin: 10
                  anchors.verticalCenter: parent.verticalCenter
                  spacing: 5

                  BluetoothButton {
                    implicitWidth: 60
                    height: 25
                    text: modelData.connected ? "Disconnect" : "Connect"
                    isActive: true
                    visible: modelData.paired
                    onClicked: {
                      if (modelData.connected) {
                        modelData.disconnect()
                      } else {
                        modelData.connect()
                      }
                    }
                  }

                  BluetoothButton {
                    implicitWidth: 40
                    height: 25
                    text: modelData.paired ? "Forget" : "Pair"
                    isActive: !modelData.paired
                    onClicked: {
                      if (modelData.paired) {
                        modelData.forget()
                      } else {
                        modelData.pair()
                      }
                    }
                  }
                }

                MouseArea {
                  id: deviceMouseArea
                  anchors.fill: parent
                  hoverEnabled: true
                  z: -1
                }
              }
            }

            Text {
              text: "No devices found"
              font.family: "Overpass Nerd Font"
              font.pixelSize: 12
              color: "#282828"
              opacity: 0.7
              anchors.horizontalCenter: parent.horizontalCenter
              visible: !Bluetooth.defaultAdapter || Bluetooth.defaultAdapter.devices.length === 0
            }
          }
        }
      }
    }
  }
}