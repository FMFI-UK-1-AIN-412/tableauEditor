import React, { useEffect, useRef } from 'react';
import ElmComponent from 'react-elm-components'
import '../../src/static/main.css'
const Elm = require('./elm-editor.js').Elm;

function prepare(initialState) {
  const instance = {
    initialState,
    state: null
  };
  const getState = (instance) => instance.state;
  return {
    instance: instance,
    getState,
  }
}

function AppComponent(props) {
  const instance = props.instance;
  const setupPorts = (ports) => {
    ports.cache.subscribe( (state) => {
      instance.state = state;
      props.onStateChange();
    })
  };
  return (
    <ElmComponent src={Elm.Editor} flags={instance.initialState} ports={setupPorts}/>
  );
}

export {
  prepare,
  AppComponent
}
