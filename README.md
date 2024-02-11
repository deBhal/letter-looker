# Letter Looker

This app is intended to help craft bracelets for an upcoming Taylor Swift
concert from a collection of available letters.

1. Paste available letters into the text box at the top and phrases that can be
made out of those letters will be moved up into the "Available Phrases" area
2. Click on available phrases to add them to the list of selected phrases and remove the required letters from the list of letters

*NOTE*: Don't type in available letters directly.

The app has no persistence, so be sure to keep your list of available
letters in some other document to avoid losing them if you reset or close the web page

## Build Instructions

Run the following command from the root of this project:

```bash
elm make src/Main.elm --output=elm.js
```

Then open `index.html` in your browser!

## Dev

`elm-live -H --port 3000 src/Main.elm`


Originally based on https://evancz.github.io/elm-todomvc
