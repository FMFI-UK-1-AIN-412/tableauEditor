const fs = require('fs/promises');
const {exec} = require('child_process');

function cmd(command) {
    return new Promise((resolve, reject) => {
        console.log(command);
        const proc = exec(command);
        proc.stdout.on('data', function (data) {process.stdout.write(data)});
        proc.stderr.on('data', function (data) {process.stderr.write(data)});
        proc.on('exit', function (code) {code === 0 ? resolve(0) : reject(code)});
    });
}

(async () => {
    // if exists, remove './lib/dist' folder
    await fs.rm('./lib/dist', {
        force: true,
        recursive: true,
    });
    // compile elm app
    await cmd("npx elm make src/Editor.elm --output lib/dist/elm-editor.js");
    // compile react component
    await cmd("npx babel --extensions '.jsx' src --out-dir lib/dist");
    // isolate css files
    await cmd("npx isolate-css-cli -p tableaueditor-Obry4K9MqH -c -o lib/dist/static/css -u 2 src/static");
    // copy font files used by isolated css
    await cmd("npx copyfiles -u 3 src/static/webfonts/* lib/dist/static/webfonts");
})()
