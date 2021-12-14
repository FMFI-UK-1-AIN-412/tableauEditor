import React, { useEffect, useRef } from 'react';
const Elm = require('./elm-editor.js').Elm;

function AppModule() {
  const rootElement = useRef(null);
  useEffect(() => {
    Elm.Editor.init({
      node: rootElement, 
      flags: null 
    });
  })
  return (
    <div ref={rootElement}></div>
  )
}

export default AppModule
