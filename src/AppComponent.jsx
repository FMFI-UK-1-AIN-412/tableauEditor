import React, { useCallback, useContext, useMemo } from 'react';
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

function AppComponent({instance, onStateChange, isEdited, proof, updateProofVerdict}) {
  const setupPorts = (ports) => {
    ports.onChange.subscribe( ({proofVerdict, initial}) => {
      console.log("Tableau editor has changed ", proofVerdict, initial);
      if (instance.updateProofVerdict !== undefined) {
        instance.updateProofVerdict(proofVerdict)
      }
      if (!initial) {
        // initial onChange call only checks proof verdict 
        instance.onStateChange();
      }
    });
    ports.onStore.subscribe( (state) => {
      // console.log("Tableau editor sent state to store");
      instance.state = state;
    })
    instance.triggerStateUpdate = () =>
      ports.storeTrigger.send(null);

    instance.ports = ports;
  };

  // Functions updateProofVerdict and onChange may change on rerenders,
  // so its values are stored inside instance, so port subscribe handler 
  // above, which is just called only once, calls up-to-date versions
  instance.updateProofVerdict = updateProofVerdict;
  instance.onStateChange = onStateChange;

  useMemo(() => {
    if (instance.ports !== undefined && proof !== undefined) {
      const contextData = {
        axioms: proof.axioms.map(f => f.formula),
        provedTheorems: proof.theorems.filter(f => f.prooved).map(f => f.formula),
        newTheorem: proof.newTheorem.formula,
      }
      console.log('Sending proof', contextData)
      instance.ports.updateContext.send(contextData)
    }
  }, [proof, instance.ports])

  return (
    <div className='tableaueditor-Obry4K9MqH'>
      <div className={isEdited ? '' : 'viewMode'}>
        <ElmComponent src={Elm.MainEmbeddable} flags={instance.initialState} ports={setupPorts}/>
      </div>
    </div>
  );
}

export default {
  prepare,
  AppComponent
}
