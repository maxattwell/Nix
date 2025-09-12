import QtQuick
import Quickshell.Io

Text {
  id: clock
  padding: 10
  color: "#282828"
  font.family: "Overpass Nerd Font"
  font.pixelSize: 14

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
