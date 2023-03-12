import React, { useEffect, useRef } from 'react';
import ElmComponent from 'react-elm-components'
import './static/css/main.iso.css';
import './static/css/all.iso.css';
const Elm = require('./elm-editor.js').Elm;

function prepare(initialState) {
  const instance = {
    initialState,
    state: null,
    triggerStateUpdate: null
  };
  const getState = (instance) => {
    if (instance.triggerStateUpdate) {
      console.log("State before triggering update", instance.state);
      instance.triggerStateUpdate();
      console.log("State after triggering update", instance.state);
    }
    return instance.state;
  }
  return {
    instance: instance,
    getState,
  }
}

function AppComponent(props) {
  const instance = props.instance;
  const setupPorts = (ports) => {
    ports.onChange.subscribe( () => {
      props.onStateChange();
    });
    ports.onStore.subscribe( (state) => {
      instance.state = state;
    })
    instance.triggerStateUpdate = () =>
      ports.storeTrigger.send(null);
  };
  return (
    <div className={`tableaueditor-Obry4K9MqH${
      props.isEdited ? '' : ' viewMode'
    }`}>
      <ElmComponent src={Elm.MainEmbeddable} flags={instance.initialState} ports={setupPorts}/>
    </div>
  );
}

export default {
  prepare,
  AppComponent
}
