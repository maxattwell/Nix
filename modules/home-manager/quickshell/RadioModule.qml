import QtQuick
import QtQuick.Controls
import Quickshell
import Quickshell.Io

Rectangle {
  property bool hovered: false
  property var panelWindow
  property int barHeight
  property var bluetoothModule
  property alias radioPopup: radioPopup
  property alias radioHideAnim: radioHideAnim
  
  width: radioText.width + 20
  height: 26
  y: 7
  color: radioPopup.visible ? "#3c3836" : (hovered ? "#d5c4a1" : "#ebdbb2")
  radius: 7

  Text {
    id: radioText
    anchors.centerIn: parent
    text: ""
    font.family: "Overpass Nerd Font"
    font.pixelSize: 14
    color: radioPopup.visible ? "#a89984" : (parent.parent.hovered ? "#282828" : "#3c3836")
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
        if (bluetoothModule && bluetoothModule.bluetoothPopup && bluetoothModule.bluetoothPopup.visible) {
          bluetoothModule.bluetoothHideAnim.start()
          radioDelayTimer.start()
        } else {
          radioPopup.visible = true
        }
      }
    }
  }

  Timer {
    id: radioDelayTimer
    interval: 220
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
      color: "#fbf1c7"
      radius: 10
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

      component RadioButton: Rectangle {
        property string text: ""
        property bool isActive: false
        property bool hovered: false
        
        signal clicked()
        
        width: implicitWidth
        height: 35
        color: {
          if (isActive) return "#3c3836"
          if (hovered) return "#d5c4a1"
          return "#ebdbb2"
        }
        radius: 8
        
        Text {
          anchors.centerIn: parent
          text: parent.text
          font.family: "Overpass Nerd Font"
          font.pixelSize: 12
          color: {
            if (parent.isActive) return "#a89984"
            if (parent.hovered) return "#282828"
            return "#3c3836"
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
          font.family: "Overpass Nerd Font"
          font.pixelSize: 14 + 2
          color: "#282828"
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
