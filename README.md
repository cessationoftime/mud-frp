mud-frp
=======

== Git Clone with BuildWrapper submodule

git clone --recursive https://github.com/cessationoftime/mud-frp.git

MUD game editor using functional reactive programming

== Troubleshooting

* Seg fault - if using a casted c++ object.  Use wxHaskell's "kindof" function to determine (it will be logged) if it is the kind you expect. A seg fault will occur if you call a c++ function with the wrong kind of object.
* loggingFunctions
 	* logError
 	* logFatalError
 	* logWarning
 	* logMessage - displays a popup window with the message
 	* logVerbose
 	* logStatus
 	* logSysError
 	* logDebug
 	* logTrace
