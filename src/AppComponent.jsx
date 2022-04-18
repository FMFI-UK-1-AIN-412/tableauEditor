import React, { useEffect, useRef } from 'react';
import ElmComponent from 'react-elm-components'
import './static/css/main.iso.css';
import './static/css/all.iso.css';
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
    <div className="tableaueditor-Obry4K9MqH">
      <ElmComponent src={Elm.MainEmbeddable} flags={instance.initialState} ports={setupPorts}/>
    </div>
  );
}

export default {
  prepare,
  AppComponent
}
