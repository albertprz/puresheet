# puresheet

## Description

Spreadsheet management application implemented on [Halogen](https://github.com/purescript-halogen/purescript-halogen) 
aiming to provide an extensible, expressive and simple to use platform
for data representation, visualization, analysis and exploration available
for use for enterprise, personal & educational purposes.

## Project Goals

- [:heavy_check_mark:] Provide a no frills, minimalistic GUI supporting all basic spreadsheet functionality regarding navigation, cell management, formula evaluation & automatic cell updates.

- [:heavy_check_mark:] Expose a high level pure functional dynamic formula language interpreted at the browser, with expresiveness similar to the term level language in Haskell or Purescript, albeit with familiar syntax and idioms to popular spreadhsheet applications and mainstream languages.

- [:heavy_check_mark:] Expose a prelude library with commonly used functions and combinators, loaded at startup.

- [:heavy_check_mark:] Support formula edition with syntax highlighting and function signatures for the current function at the cursor.

- Include Opt-in automatic formatting for formulas when evaluated.

- Support auto completion for imported and module aliased global functions.

- Expose a view to query (possibly in a Hoogle / Pursuit fashion), view, update and upload new global functions & operators on a per module basis. This view would surface the same editing capatibilities as the formula box in the main spreadsheet view.

- Expose customization options for navigation, keybindings, spreadsheet behaviour as well as display and themes configuration.

- Enable the use of formulas for filtering & sorting rows.

- Support authenticated persistence & retrieval from a catalog of private spreadsheets via a future backend.

- Support Import & Export of spreadsheets from and to Excel and Google Sheets with the proper mapping of cell values & formulas.


## Screenshots

![](assets/screenshot.png)

## Local Deployment

First, clone the repository:

```console
$ git clone https://github.com/albertprz/puresheet
$ cd puresheet
```

Then, install the following dependencies via `npm`:

```console
$ npm install -g purescript spago parcel
```

Finally build the project and launch it in localhost:


```console
$ npm run build
$ npm run serve
```
    
## Keybindings

<center>

| Key Combination | Action |
| --------------- | ------ |
| &uarr; &darr; &larr; &rarr; | Cell navigation |
| `h` `j` `k` `l` | Cell navigation (Vim like) |
| `Tab` | Go to next cell |
| `Shift` + `Tab` | Go to previous cell |
| `Enter` | Edit cell value |
| `Control` + `Enter` | Edit formula |
| `Shift` + `Navigation` | Cell selection |
| `Backspace` | Delete cell/s |
| `Control` + `G` | Go to cell |
| `Control` + `A` | Select all cells |
| `Control` + `C` | Copy cells/s |
| `Control` + `V` | Paste cells/s |
| `Control` + `X` | Cut cells/s |

</center>

Note: `Control` bindings also match the `Command` key on Mac OS.


## Available functions

Please see the [Prelude module](lib/Prelude.pursh) loaded at startup.
