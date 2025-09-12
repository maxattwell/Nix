import QtQuick
import Quickshell.Hyprland

Rectangle {
  id: leftModule
  property var screen
  
  // Theme constants
  readonly property color bgColor: "#fbf1c7"
  readonly property color fgColor: "#3c3836"
  readonly property color accentColor: "#ebdbb2"
  readonly property color accentColorHover: "#d5c4a1"
  readonly property color textColor: "#3c3836"
  readonly property color textColorHover: "#282828"
  readonly property color focusedTextColor: "#a89984"
  readonly property int moduleMargin: 2
  readonly property int modulePadding: 20
  readonly property int buttonHeight: 26
  readonly property int buttonRadius: 8
  readonly property int workspaceWidth: 30
  readonly property string fontFamily: "Overpass Nerd Font"
  readonly property int smallFontSize: 12
  
  height: parent.height - moduleMargin
  width: workspacesRow.width + modulePadding
  x: moduleMargin
  y: moduleMargin
  color: bgColor
  radius: 10

  Row {
    id: workspacesRow
    anchors.centerIn: parent
    spacing: 1

    Repeater {
      model: Hyprland.workspaces
      delegate: Rectangle {
        required property var modelData
        
        property var currentMonitor: Hyprland.monitorFor(screen)
        property bool hovered: false
        property bool hasWindows: modelData.toplevels && modelData.toplevels.length > 0
        property bool isActiveOnThisMonitor: currentMonitor && currentMonitor.activeWorkspace && currentMonitor.activeWorkspace.id === modelData.id
        
        visible: modelData.monitor && currentMonitor && modelData.monitor.id === currentMonitor.id && modelData.id >= 1 && modelData.id <= 10

        width: visible ? workspaceWidth : 0
        height: buttonHeight
        color: isActiveOnThisMonitor ? fgColor : (hovered ? accentColorHover : accentColor)
        radius: buttonRadius
        
        Text {
          anchors.centerIn: parent
          text: modelData.id
          font.family: fontFamily
          font.pixelSize: smallFontSize
          color: isActiveOnThisMonitor ? focusedTextColor : (hovered ? textColorHover : textColor)
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
