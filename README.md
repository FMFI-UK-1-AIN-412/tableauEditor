# Tableau Editor

In this repository is maintained the code to my bachelor thesis.
A live version is available at https://fmfi-uk-1-ain-412.github.io/tableauEditor/ .

Here we can find other materials to my bachelor thesis: https://nitrajka.github.io/ .
We don't need to install anything to see the build application in a browser. Just open `/build/index.html` in a browser.
To run the application in development mode or build it we need to have elm installed.

We install elm using a package manager for JS packages. Since `npm` had problems installing Elm,
we will use `yarn`.
Install yarn according to the tutorial https://yarnpkg.com/lang/en/docs/install/#mac-stable . Be careful to choose the flag `--without-node` if you use
`nvm` or `n` or have node already installed. Then run:
```
$ yarn global add elm elm-live elm-make
```
Now you can run the command from section `Development server` to live access the app in a browser.

### Development server:
```
elm-live src/Editor.elm src/Tableau.elm src/Zipper.elm src/Errors.elm src/Validate.elm src/Helpers/Helper.elm src/Helpers/Rules.elm src/Helpers/Exporting/Ports.elm src/Helpers/Exporting/Json/Encode.elm src/Helpers/Exporting/Json/Decode.elm --open --pushstate --output=elm.js
```
* Access app at `http://localhost:8000/`


### Build & bundle for prod:
```
make
```

* Files are build and saved into the `/build` folder
* To check it, open `build/index.html` in the browser.

When switching between prod and development environments don't forget to uncomment the correct links for styles in header of `index.html` file.