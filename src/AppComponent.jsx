import React from 'react';
import ElmComponent from 'react-elm-components'
import './static/css/main.iso.css';
import './static/css/all.iso.css';
const Elm = require('./elm-editor.js').Elm;

function prepare(initialState) {
  if (typeof(initialState) == "string") {
    // Backwards compatibility
    // State is now a JSON value, but was stringified JSON before
    try {
      initialState = JSON.parse(initialState);
    } catch (_) {
      initialState = null;
    }
  }
  const instance = {
    initialState,
    state: initialState,
    triggerStateUpdate: null
  };
  const getState = (instance) => {
    if (instance.triggerStateUpdate) {
      // console.log("Triggering tableau editor state update");
      instance.triggerStateUpdate();
      // console.log("Triggered tableau editor state update");
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
      // console.log("Tableau editor has changed");
      props.onStateChange();
    });
    ports.onStore.subscribe( (state) => {
      // console.log("Tableau editor sent state to store");
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
