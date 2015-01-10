Layout
=======

## Setup

* Main  - Application Startup and Input/Output configuration
* EventInputs - Input/Output data declaration
* Paths - Access to the Data directory

## Event Generators

#### Common

* Dialogs

#### Aui

* AuiManager
* Notebook
* SourceEditor

#### Workspace
* CurrentWorkspace - This module configures Discrete data that maintains the current state of the workspace. It also broadcasts events to notify UI elements when the state changes.
* CurrentWorkspaceData - data definitions of the WorkspaceState. EventInputs,CurrentWorkspace and workspaceBrowser all depend on this module.
* WorkspaceBrowser - This module configures UI elements to display the current state of the workspace.

#### Misc

* MapEditor - A custom control I was playing with that won't be needed in the finished IDE.

