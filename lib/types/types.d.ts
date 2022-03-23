interface AppProps {
  instance: any,
  onStateChange: () => void
}

interface PrepareResult {
  instance: any,
  getState: (instance: any) => any,
}

export function prepare(initialState: any): PrepareResult
export function AppComponent(props: AppProps): JSX.Element