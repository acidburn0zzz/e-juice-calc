# E-Juice-Calc

E-Juice-Calc is a free and open source application to help with mixing your own e-juice.

![Alt text](/res/Screenshot_1.png?raw=true "The main window of e-juice-calc.")

The features include:

* calculate a target liquid given a base liquid and flavors
* ability to specify nicotine content in the base and target liquid
* save and load support
* auto-load of the last saved setup

## Building

You will need to have stack and the Qt5 development files installed, to be able to build q-juice-calc.

To build e-juice-calc, run the following in the root directory:
````
$ stack build
````

You will also have to build qml-language-bridge, which resides in the subdirectory qml-language-bridge.
For build instructions visit the <a href="https://gitlab.com/rszibele/qml-language-bridge">qml-language-bridge repository</a>.

Once both e-juice-calc and qml-language-bridge have been installed, copy both the binaries
and the res folder into a new folder called deploy.

As of 21.08.2018 e-juice-calc only supports loading the QML resources from the path `res/` relative to the executable `ejuicecalc`. This will 
likely change in future versions.