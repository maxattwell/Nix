import Quickshell
import Quickshell.Io
import Quickshell.Hyprland
import Quickshell.Bluetooth
import QtQuick
import QtQuick.Controls

Variants {
  // Theme variables
  readonly property color bgColor: "#fbf1c7"
  readonly property color fgColor: "#3c3836"
  readonly property color accentColor: "#ebdbb2"
  readonly property color accentColorHover: "#d5c4a1"
  readonly property color textColor: "#3c3836"
  readonly property color textColorHover: "#282828"
  readonly property color fgColorHover: "#32302f"
  readonly property color clockColor: "#282828"
  readonly property color focusedTextColor: "#a89984"
  
  readonly property int barHeight: 37
  readonly property int moduleHeight: 35
  readonly property int buttonHeight: 26
  readonly property int moduleRadius: 10
  readonly property int buttonRadius: 8
  readonly property int smallRadius: 7
  readonly property int moduleMargin: 2
  readonly property int modulePadding: 20
  readonly property int buttonPadding: 20
  readonly property int textPadding: 8
  readonly property int clockPadding: 10
  readonly property int workspaceWidth: 30
  
  readonly property string fontFamily: "Overpass Nerd Font"
  readonly property int fontSize: 14
  readonly property int smallFontSize: 12
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

      implicitHeight: barHeight
      color: "transparent"


      Rectangle {
        id: leftModule
        height: parent.height - moduleMargin
        width: workspacesRow.width + modulePadding
        x: moduleMargin
        y: moduleMargin
        color: bgColor
        radius: moduleRadius

        Row {
          id: workspacesRow
          anchors.centerIn: parent
          spacing: 1

          Repeater {
            model: Hyprland.workspaces
            delegate: Rectangle {
              required property var modelData
              
              // Get the monitor for this screen
              property var currentMonitor: Hyprland.monitorFor(screen)
              property bool hovered: false
              
              // Show workspaces that belong to this monitor and are typically in use
              visible: modelData.monitor && currentMonitor && modelData.monitor.id === currentMonitor.id && modelData.id >= 1 && modelData.id <= 10
              
              width: visible ? workspaceWidth : 0
              height: buttonHeight
              color: modelData.focused ? fgColor : (hovered ? accentColorHover : accentColor)
              radius: buttonRadius
              
              Text {
                anchors.centerIn: parent
                text: modelData.id
                font.family: fontFamily
                font.pixelSize: smallFontSize
                color: modelData.focused ? focusedTextColor : (hovered ? textColorHover : textColor)
              }

              MouseArea {
                anchors.fill: parent
                hoverEnabled: true
                onEntered: parent.hovered = true
                onExited: parent.hovered = false
                onClicked: Hyprland.dispatch("workspace " + modelData.id)
              }
            }
          }
        }
      }

      Rectangle {
        id: centerModule
        height: parent.height - (moduleMargin * 2)
        width: metricsRow.width + modulePadding
        anchors.centerIn: parent
        y: moduleMargin
        color: bgColor
        radius: moduleRadius

        Row {
          id: metricsRow
          anchors.centerIn: parent
          spacing: 0

          Text {
            id: diskUsage
            padding: textPadding
            color: textColor
            font.family: fontFamily
            font.pixelSize: fontSize
            
            Process {
              id: diskProc
              command: ["df", "-h", "/"]
              running: true
              
              stdout: StdioCollector {
                onStreamFinished: {
                  var lines = this.text.trim().split('\n')
                  if (lines.length > 1) {
                    var usage = lines[1].split(/\s+/)[4].replace('%', '')
                    diskUsage.text = usage + "% "
                  }
                }
              }
            }

            Timer {
              interval: 30000
              running: true
              repeat: true
              onTriggered: diskProc.running = true
            }
          }

          Text {
            id: memoryUsage
            padding: textPadding
            color: textColor
            font.family: fontFamily
            font.pixelSize: fontSize
            
            Process {
              id: memProc
              command: ["free"]
              running: true
              
              stdout: StdioCollector {
                onStreamFinished: {
                  var lines = this.text.trim().split('\n')
                  if (lines.length > 1) {
                    var parts = lines[1].split(/\s+/)
                    var total = parseInt(parts[1])
                    var used = parseInt(parts[2])
                    var percent = Math.round((used / total) * 100)
                    memoryUsage.text = percent + "% "
                  }
                }
              }
            }

            Timer {
              interval: 5000
              running: true
              repeat: true
              onTriggered: memProc.running = true
            }
          }

          Text {
            id: cpuUsage
            padding: textPadding
            color: textColor
            font.family: fontFamily
            font.pixelSize: fontSize
            
            Process {
              id: cpuProc
              command: ["sh", "-c", "top -bn1 | grep 'Cpu(s)' | awk '{print $2}' | cut -d'%' -f1"]
              running: true
              
              stdout: StdioCollector {
                onStreamFinished: {
                  var usage = parseFloat(this.text.trim())
                  cpuUsage.text = Math.round(usage) + "% "
                }
              }
            }

            Timer {
              interval: 2000
              running: true
              repeat: true
              onTriggered: cpuProc.running = true
            }
          }
        }
      }

      Rectangle {
        id: rightModule
        height: parent.height - moduleMargin
        width: rightRow.width + modulePadding
        anchors.right: parent.right
        anchors.rightMargin: moduleMargin
        y: moduleMargin
        color: bgColor
        radius: moduleRadius

        Row {
          id: rightRow
          anchors.centerIn: parent
          spacing: 1

          Rectangle {
            property bool hovered: false
            width: bluetoothText.width + buttonPadding
            height: buttonHeight
            y: 7
            color: bluetoothPopup.visible ? fgColor : (hovered ? accentColorHover : accentColor)
            radius: smallRadius

            Text {
              id: bluetoothText
              anchors.centerIn: parent
              text: Bluetooth.defaultAdapter && Bluetooth.defaultAdapter.enabled ? "󰂯" : "󰂲"
              font.family: fontFamily
              font.pixelSize: fontSize
              color: bluetoothPopup.visible ? focusedTextColor : (parent.parent.hovered ? textColorHover : textColor)
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
                  // Close radio popup if it's open, then open bluetooth after delay
                  if (radioPopup.visible) {
                    radioHideAnim.start()
                    bluetoothDelayTimer.start()
                  } else {
                    bluetoothPopup.visible = true
                  }
                }
              }
            }

            Timer {
              id: bluetoothDelayTimer
              interval: 220 // Slightly longer than hide animation (200ms)
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
                color: bgColor
                radius: moduleRadius
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

                // Reusable Button Component
                component BluetoothButton: Rectangle {
                  property string text: ""
                  property bool isActive: false
                  property bool isEnabled: true
                  property bool hovered: false
                  
                  signal clicked()
                  
                  width: implicitWidth
                  height: 30
                  color: {
                    if (!isEnabled) return accentColor
                    if (isActive) return fgColor
                    if (hovered) return accentColorHover
                    return accentColor
                  }
                  radius: buttonRadius
                  opacity: isEnabled ? 1.0 : 0.5
                  
                  Text {
                    anchors.centerIn: parent
                    text: parent.text
                    font.family: fontFamily
                    font.pixelSize: smallFontSize
                    color: {
                      if (!parent.isEnabled) return textColor
                      if (parent.isActive) return focusedTextColor
                      if (parent.hovered) return textColorHover
                      return textColor
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

                // Adapter controls
                Column {
                  id: adapterControls
                  width: parent.width
                  spacing: 8

                  Text {
                    text: "Bluetooth"
                    font.family: fontFamily
                    font.pixelSize: fontSize + 2
                    color: clockColor
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

                // Device list
                Text {
                  text: "Devices"
                  font.family: fontFamily
                  font.pixelSize: fontSize
                  color: clockColor
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
                        color: deviceMouseArea.containsMouse ? accentColorHover : accentColor
                        radius: smallRadius

                        Row {
                          anchors.left: parent.left
                          anchors.leftMargin: 10
                          anchors.verticalCenter: parent.verticalCenter
                          spacing: 10

                          Text {
                            text: modelData.connected ? "󰂱" : (modelData.paired ? "󰂯" : "󰂲")
                            font.family: fontFamily
                            font.pixelSize: fontSize
                            color: modelData.connected ? clockColor : textColor
                          }

                          Column {
                            anchors.verticalCenter: parent.verticalCenter
                            
                            Text {
                              text: modelData.name || modelData.address
                              font.family: fontFamily
                              font.pixelSize: smallFontSize + 1
                              color: textColor
                            }

                            Text {
                              text: {
                                if (modelData.pairing) return "Pairing..."
                                if (modelData.state === 1) return "Connecting..."
                                if (modelData.state === 2) return "Disconnecting..."
                                return modelData.paired ? "Paired" : "Available"
                              }
                              font.family: fontFamily
                              font.pixelSize: smallFontSize - 1
                              color: textColorHover
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
                      font.family: fontFamily
                      font.pixelSize: smallFontSize
                      color: textColorHover
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

          Rectangle {
            property bool hovered: false
            width: radioText.width + buttonPadding
            height: buttonHeight
            y: 7
            color: radioPopup.visible ? fgColor : (hovered ? accentColorHover : accentColor)
            radius: smallRadius

            Text {
              id: radioText
              anchors.centerIn: parent
              text: ""
              font.family: fontFamily
              font.pixelSize: fontSize
              color: radioPopup.visible ? focusedTextColor : (parent.parent.hovered ? textColorHover : textColor)
            }

            MouseArea {
              anchors.fill: parent
              hoverEnabled: true
              onEntered: parent.hovered = true
              onExited: parent.hovered = false
              onClicked: {
                if (radioPopup.visible) {
                  radioHideAnim.start()
                } else {
                  // Close bluetooth popup if it's open, then open radio after delay
                  if (bluetoothPopup.visible) {
                    bluetoothHideAnim.start()
                    radioDelayTimer.start()
                  } else {
                    radioPopup.visible = true
                  }
                }
              }
            }

            Timer {
              id: radioDelayTimer
              interval: 220 // Slightly longer than hide animation (200ms)
              onTriggered: radioPopup.visible = true
            }

            PopupWindow {
              id: radioPopup
              anchor.window: panelWindow
              anchor.rect.x: panelWindow.width
              anchor.rect.y: barHeight
              implicitWidth: 280
              implicitHeight: 300
              color: "transparent"

              Rectangle {
                id: radioRect
                anchors.fill: parent
                color: bgColor
                radius: moduleRadius
                transform: Translate { id: radioTransform; y: -radioRect.height }

                PropertyAnimation {
                  id: radioShowAnim
                  target: radioTransform
                  property: "y"
                  from: -radioRect.height
                  to: 0
                  duration: 250
                  easing.type: Easing.OutCubic
                }

                PropertyAnimation {
                  id: radioHideAnim
                  target: radioTransform
                  property: "y"
                  from: 0
                  to: -radioRect.height
                  duration: 200
                  easing.type: Easing.InCubic
                  onFinished: radioPopup.visible = false
                }

                Component.onCompleted: {
                  radioPopup.onVisibleChanged.connect(function() {
                    if (radioPopup.visible) {
                      radioRect.visible = true
                      radioShowAnim.start()
                    }
                  })
                }

                // Reusable Radio Button Component
                component RadioButton: Rectangle {
                  property string text: ""
                  property bool isActive: false
                  property bool hovered: false
                  
                  signal clicked()
                  
                  width: implicitWidth
                  height: 35
                  color: {
                    if (isActive) return fgColor
                    if (hovered) return accentColorHover
                    return accentColor
                  }
                  radius: buttonRadius
                  
                  Text {
                    anchors.centerIn: parent
                    text: parent.text
                    font.family: fontFamily
                    font.pixelSize: smallFontSize
                    color: {
                      if (parent.isActive) return focusedTextColor
                      if (parent.hovered) return textColorHover
                      return textColor
                    }
                  }

                  MouseArea {
                    anchors.fill: parent
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

                  Text {
                    text: "Radio Stations"
                    font.family: fontFamily
                    font.pixelSize: fontSize + 2
                    color: clockColor
                    font.bold: true
                  }

                  ScrollView {
                    id: radioScrollView
                    width: parent.width
                    height: Math.min(220, contentHeight)
                    contentHeight: radioList.height
                    clip: true
                    
                    ScrollBar.vertical.policy: ScrollBar.AsNeeded
                    ScrollBar.horizontal.policy: ScrollBar.AlwaysOff

                    Column {
                      id: radioList
                      width: radioScrollView.width
                      spacing: 5

                      RadioButton {
                        implicitWidth: radioList.width - 20
                        text: "George FM"
                        onClicked: {
                          console.log("George FM clicked")
                          radioStopProc.running = true
                          georgeTimer.start()
                          radioHideAnim.start()
                        }
                      }

                      RadioButton {
                        implicitWidth: radioList.width - 20
                        text: "Loca FM"
                        onClicked: {
                          console.log("Loca FM clicked")
                          radioStopProc.running = true
                          locaTimer.start()
                          radioHideAnim.start()
                        }
                      }

                      RadioButton {
                        implicitWidth: radioList.width - 20
                        text: "RNE Radio 5"
                        onClicked: {
                          console.log("RNE Radio 5 clicked")
                          radioStopProc.running = true
                          rne5Timer.start()
                          radioHideAnim.start()
                        }
                      }

                      RadioButton {
                        implicitWidth: radioList.width - 20
                        text: "RNE Exterior"
                        onClicked: {
                          console.log("RNE Exterior clicked")
                          radioStopProc.running = true
                          rneExtTimer.start()
                          radioHideAnim.start()
                        }
                      }

                      RadioButton {
                        implicitWidth: radioList.width - 20
                        text: "Stop all stations"
                        isActive: true
                        onClicked: {
                          console.log("Stop all stations clicked")
                          radioStopProc.running = true
                          radioHideAnim.start()
                        }
                      }
                    }
                  }
                }
              }
            }

            // Process definitions for each radio station
            Process {
              id: radioStopProc
              command: ["pkill", "vlc"]
              onRunningChanged: console.log("radioStopProc running:", running)
              stdout: StdioCollector {
                onStreamFinished: console.log("radioStopProc stdout:", this.text)
              }
              stderr: StdioCollector {
                onStreamFinished: console.log("radioStopProc stderr:", this.text)
              }
            }

            Process {
              id: radioGeorgeProc
              command: ["sh", "-c", "cvlc 'https://digitalstreams.mediaworks.nz/george_net/playlist.m3u8' &"]
              onRunningChanged: console.log("radioGeorgeProc running:", running)
              stdout: StdioCollector {
                onStreamFinished: console.log("radioGeorgeProc stdout:", this.text)
              }
              stderr: StdioCollector {
                onStreamFinished: console.log("radioGeorgeProc stderr:", this.text)
              }
            }

            Process {
              id: radioLocaProc
              command: ["sh", "-c", "cvlc 'https://s3.we4stream.com:2020/stream/locafm' &"]
              onRunningChanged: console.log("radioLocaProc running:", running)
              stdout: StdioCollector {
                onStreamFinished: console.log("radioLocaProc stdout:", this.text)
              }
              stderr: StdioCollector {
                onStreamFinished: console.log("radioLocaProc stderr:", this.text)
              }
            }

            Process {
              id: radioRne5Proc
              command: ["sh", "-c", "cvlc 'https://rtvelivestream.rtve.es/rtvesec/rne/rne_r5_madrid_main.m3u8' &"]
              onRunningChanged: console.log("radioRne5Proc running:", running)
              stdout: StdioCollector {
                onStreamFinished: console.log("radioRne5Proc stdout:", this.text)
              }
              stderr: StdioCollector {
                onStreamFinished: console.log("radioRne5Proc stderr:", this.text)
              }
            }

            Process {
              id: radioRneExtProc
              command: ["sh", "-c", "cvlc 'https://rtvelivestream.akamaized.net/rtvesec/rne/rne_re_main.m3u8' &"]
              onRunningChanged: console.log("radioRneExtProc running:", running)
              stdout: StdioCollector {
                onStreamFinished: console.log("radioRneExtProc stdout:", this.text)
              }
              stderr: StdioCollector {
                onStreamFinished: console.log("radioRneExtProc stderr:", this.text)
              }
            }

            // Timers to delay starting radio after stopping
            Timer {
              id: georgeTimer
              interval: 500
              onTriggered: {
                console.log("Starting George FM after delay")
                radioGeorgeProc.startDetached()
              }
            }

            Timer {
              id: locaTimer
              interval: 500
              onTriggered: {
                console.log("Starting Loca FM after delay")
                radioLocaProc.startDetached()
              }
            }

            Timer {
              id: rne5Timer
              interval: 500
              onTriggered: {
                console.log("Starting RNE Radio 5 after delay")
                radioRne5Proc.startDetached()
              }
            }

            Timer {
              id: rneExtTimer
              interval: 500
              onTriggered: {
                console.log("Starting RNE Exterior after delay")
                radioRneExtProc.startDetached()
              }
            }
          }

          Text {
            id: clock
            padding: clockPadding
            color: clockColor
            font.family: fontFamily
            font.pixelSize: fontSize

            Process {
              id: dateProc
              command: ["date", "+%a %d %b     %I:%M  %p"]
              running: true

              stdout: StdioCollector {
                onStreamFinished: clock.text = "󰃭 " + this.text.trim()
              }
            }

            Timer {
              interval: 1000
              running: true
              repeat: true
              onTriggered: dateProc.running = true
            }
          }
        }
      }
    }
  }
}
