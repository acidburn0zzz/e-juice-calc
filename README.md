# E-Juice-Calc

E-Juice-Calc is a free and open source application to help with mixing your own e-juice.

![Alt text](/res/Screenshot_1.png?raw=true "The main window of e-juice-calc.")

The features include:

* calculate a target liquid given a base liquid and flavors
* ability to specify nicotine content in the base and target liquid
* save and load support
* auto-load of the last saved setup

## Installation

### GNU/Linux (all flavors)

To install E-Juice-Calc, you will need to install it through Flatpak.

```
$ flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
$ flatpak install flathub com.szibele.e-juice-calc
```

### Windows

Currently N/A

### Mac OS

Currently N/A

## How to Run

You can run it through the menu entry, as Flatpak creates it automatically.
You can however, also run it through the command line:

```
$ flatpak run com.szibele.e-juice-calc
```

## Build Instructions

You will need to have stack installed, to be able to build q-juice-calc.

To build e-juice-calc, run the following in the root directory:
```
$ stack build
```

You will also have to build qml-language-bridge, which resides in the subdirectory qml-language-bridge.
For build instructions visit the <a href="https://gitlab.com/rszibele/qml-language-bridge">qml-language-bridge repository</a>.

Once both e-juice-calc and qml-language-bridge have been installed, copy both the binaries
and the res folder into a new folder called deploy. You can now run the executable `ejuicecalc`.

On GNU/Linux e-juice-calc searches for the Main.qml file in the following locations, ordered by preference:
* /app/share/e-juice-calc/res/Main.qml
* /usr/share/e-juice-calc/res/Main.qml
* /usr/local/share/e-juice-calc/res/Main.qml
* res/Main.qml
