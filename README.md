# Tableau Editor

In this repository is maintained the code to my bachelor thesis.


Here you can find other material to my bachelor thesis: https://nitrajka.github.io/

### Development server:
```
elm-live src/Editor.elm src/Tableau.elm src/Zipper.elm src/Errors.elm src/Validate.elm src/Helpers/Helper.elm src/Helpers/Rules.elm src/Helpers/Exporting/Ports.elm src/Helpers/Exporting/Json/Encode.elm src/Helpers/Exporting/Json/Decode.elm --open --pushstate --output=elm.js
```
* Access app at `http://localhost:8000/`


### Build & bundle for prod:
```
make
```

* Files are saved into the `/build` folder
* To check it, open `build/index.html`

When switching between prod and development don't forget to uncomment the correct links in header of `index.html` file.
