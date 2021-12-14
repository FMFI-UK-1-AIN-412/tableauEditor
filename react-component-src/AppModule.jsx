import React, { useEffect, useRef } from 'react';
import ElmComponent from 'react-elm-components'
const Elm = require('./elm-editor.js').Elm;

function AppModule() {
  return (
    <ElmComponent src={Elm.Editor} flags={null} />
  )
}

export default AppModule
