import QtQuick
import Quickshell.Io

Rectangle {
  id: centerModule
  height: parent.height - 4
  width: metricsRow.width + 20
  anchors.centerIn: parent
  y: 2
  color: "#fbf1c7"
  radius: 10

  Row {
    id: metricsRow
    anchors.centerIn: parent
    spacing: 0

    Text {
      id: diskUsage
      padding: 8
      color: "#3c3836"
      font.family: "Overpass Nerd Font"
      font.pixelSize: 14
      
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
      padding: 8
      color: "#3c3836"
      font.family: "Overpass Nerd Font"
      font.pixelSize: 14
      
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
      padding: 8
      color: "#3c3836"
      font.family: "Overpass Nerd Font"
      font.pixelSize: 14
      
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
