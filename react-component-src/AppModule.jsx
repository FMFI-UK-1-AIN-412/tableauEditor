import React, { useEffect, useRef } from 'react';
import '../../src/static/main.css'
const Elm = require('./elm-editor.js').Elm;

function AppModule() {
  const rootElement = useRef(null);
  useEffect(() => {
    Elm.Editor.init({
      node: rootElement.current,
      flags: null
    });
  })
  return (
    <div ref={rootElement}></div>
  )
}

export default AppModule
