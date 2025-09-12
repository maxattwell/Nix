import Quickshell
import Quickshell.Io
import Quickshell.Hyprland
import Quickshell.Bluetooth
import QtQuick
import QtQuick.Controls

Variants {
  model: Quickshell.screens;

  delegate: Component {
    PanelWindow {
      id: panelWindow
      required property var modelData
      screen: modelData

      anchors {
        top: true
        left: true
        right: true
      }

      implicitHeight: 37
      color: "transparent"

      WorkspaceModule {
        screen: modelData
      }

      MetricsModule {
        // Center module for system metrics
      }

      Rectangle {
        id: rightModule
        height: parent.height - 2
        width: rightRow.width + 20
        anchors.right: parent.right
        anchors.rightMargin: 2
        y: 2
        color: "#fbf1c7"
        radius: 10

        Row {
          id: rightRow
          anchors.centerIn: parent
          spacing: 1

          BluetoothModule {
            id: bluetoothModule
            panelWindow: panelWindow
            barHeight: 37
            radioModule: radioModule
          }

          RadioModule {
            id: radioModule
            panelWindow: panelWindow
            barHeight: 37
            bluetoothModule: bluetoothModule
          }

          ClockModule {
            // Clock display
          }
        }
      }
    }
  }
}