interface AppProps {
  instance: any,
  isEdited: boolean,
  onStateChange: () => void
}

interface PrepareResult {
  instance: any,
  getState: (instance: any) => any,
}

export function prepare(initialState: any): PrepareResult
export function AppComponent(props: AppProps): JSX.Element
