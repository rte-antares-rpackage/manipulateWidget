<!-- Copyright © 2016 RTE Réseau de transport d’électricité --->

# manipulateWidget 0.11.0 (2020-03-21)

## New features
* New comparison mode: the UI generated with `manipulateWidget()` has now new controls to enter in comparison mode, choose the number of charts, the layout and the variables to compare.
* `compareOptions()` gains a new argument `allowCompare` to hide/show the new controls presented above.
* New function `mwTranslations()` to translate UI elements.
* UI has been slightly reworked.

## Bug Fixes
* The number of unnecesary updates has been decreased resulting in better performance.
* Sometimes, there were some conflict when a parameter in `manipulateWidget()` had the same name as a variable in the global environment.
* `mwSelect(multiple = TRUE)` was not updating charts when selection was empty.
* `staticPlot()` was evaluating expressions in the wrong environment.


# manipulateWidget 0.10.0 (2018-05-30)

## New features
* UI has now a button to save the current chart in a PNG file. `manipulateWidget`gains a new parameter ".exportBtn" to show or hide this button.

## Bugfixes
* Passing `.saveBtn` using module
* Fix reset widget after saving in .html

# manipulateWidget 0.9.0 (2018-01-29)

## New features
* Can add a label to `mwGroup`
* new ``mwSelectize`` input
* add ``.showCompare``

## Bugfixes
* Loss of scrollbar using `shiny` with `fluidPage` and `manipulateWidget`
* Fix Handle shiny tag objects with HTML dependencies
* Preserve the class of widgets that are passed to combineWidgets

# manipulateWidget 0.8.0 (2017-11-27)

## New features
* `manipulateWidget()` has a new parameter `.updateBtnInit`. In case of update button `.updateBtn`, you can decide to render graphics on init or not.
* UI has now a button to save the current chart in an HTML file (thanks to Benoit Thieurmel).`manipulateWidget`gains a new parameter ".saveBtn" to show or hide this button.
* `manipulateWidget()` has a new parameter ".runApp". If it is false, then the function returns an object of class `MWController` that can be modified using command line instructions. This is useful to write tests for UIs created with `manipulateWidget()`.
* `manipulateWidget` interfaces can now be included in shiny applications thanks to the two new functions `mwModule()` and `mwModuleUI()`.
* A new virtual input called `mwSharedValue` has been introduced. It can be used to avoid repeating the same computations when inputs and output use a common intermediary value. It can also be used when
`manipulateWidget()` is used in a shiny application to send data from the main application to the module.
* `manipulateWidget()` now only updates the dependant inputs and outputs when user changes the value of an input. This can lead to important performance improvement in complicated applications.
* `mwModule()` now return `controller` value, with possibility to use new `clear()` method
* add `header`, `footer` and `fluidRow` arguments to `mwModuleUI()`

## Bugfixes
* When a UI contained dynamic inputs, output was sometimes updated before inputs, which could lead to some errors.
* Opening the same application in two browsers (or tabs) resulted in strange results. 


# manipulateWidget 0.7.0 (2017-06-08)

## Breaking changes
* `manipulateWidget()` has lost all arguments that were used to customize the UI. Parameters `.controlPos`, `.tabColumns` and `.compareLayout` do not exist anymore.

## New features
* `manipulateWidget()` now creates a more compact and elegant user interface.
* It is now possible to compare more than two charts. `manipulateWidget()` has a new argument `.compareOpts` to control the number of charts and their position.
* Argument `.compare` of `manipulateWidget` can now be a character vector. 

# manipulateWidget 0.6.0 (2017-05-24)

## Breaking changes
* `manipulateWidget()` now has a simpler API to show, hide and update inputs dynamically. Parameters `.display` and `.updateInputs` have been removed.
* Functions `mwUI()` and `mwControlsUI()` have been removed.

## New Features
* `manipulateWidget()` gains a new parameter `.return` to modify the object returned by the function.
* `manipulateWidget()` has two new arguments `.width` and `.height` to control size of the UI in Rmarkdown documents with option `runtime: shiny`
* New function `mwGroup` can be used to create groups of input.


## Bug fixes
* Select inputs have had a buggy behavior in some settings.
* Labels of inputs were incorrect in comparison mode.

# manipulateWidget 0.5.1 (2017-01-23)

## New Features

* Variable `.id` is now available when evaluating the initial properties of the input controls. This can be useful in comparison mode, for instance to set different choices for a select input. 

## Bug fixes

* Fixed a scope problem occuring when manipulateWidget was used inside a function and parameter `.updateInputs` was used.
* Fixed a crash that could occur when parameters `.compare` and `.updateInputs` were used together.


# manipulateWidget 0.5.0 (2017-01-18)

## New Features

* `manipulateWidget()` can now be used in a R Markdown document with shiny runtime. Input controls are included in the final document so end users can play with their values directly. (contribution by JJ. Allaire)
* `manipulateWidget()` has two new arguments `.compare` and `.compareLayout` to create a comparison interface. When `.compare` is set, two charts are outputed with some common and some individual input controls (see vignette).
* Now, input controls generated by `manipulateWidget()` can be dynamically updated thanks to the new argument ".updateInputs".
* New functions `staticImage()` and `staticPlot()` to include in a combine widget a static image or a static plot created with base functions, ggplot2, etc.
* In `combinedWidgets`objects, individual widgets are stored in a property called `widgets`, so users can now access them and modify them.


# manipulateWidget 0.4 (2016-12-16)

## Breaking changes

* Function `combineWidgets()` has been entirely rewritten and now produces a htmlwidget that can be included as is in documents or shiny applications. The general behavior is the same, but some parameters have changed.

## New features

* `manipulateWidget()` can now update an already rendered widget instead of overwriting it each time the user changes an input. This leads to better performance and user experience. Look at the documentation of manipulateWidget for further information.

## Bug fixes
* `manipulateWidget()` now preserves the order of the initial value of select inputs.
* `manipulateWidget()` now automatically finds the correct render and output functions. This solves in particular sizing problems.

# manipulateWidget 0.3 (2016-10-06)

* add a file LICENSE and copyright to sources files

# manipulateWidget 0.2 (2016-09-27)

## New features

* New functions `mwUI()` and `mwControlsUI()` have been added to let the user easily reuse the user interface generated by the package but with different server logic.
* User can now easily create group of inputs in function manipulate widget. In the UI, these inputs are grouped in a panel that can be collapsed/opened by clicking on its name.

## Bug fixes

* Many useless but worrying warning messages have been removed.
