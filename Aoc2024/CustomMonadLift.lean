def liftIO (t : ExceptT String Id α) : IO α := do
  match t with
  | .ok r => EStateM.Result.ok r
  | .error e => EStateM.Result.error e

instance : MonadLift (ExceptT String Id) IO where
  monadLift := liftIO
