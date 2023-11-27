let haskellCi =
      https://raw.githubusercontent.com/sorki/github-actions-dhall/main/haskell-ci.dhall

let defSteps = haskellCi.defaultCabalSteps

in  haskellCi.generalCi
      ( defSteps
        with extraSteps.pre
             =
              defSteps.extraSteps.pre
            # [ haskellCi.BuildStep.Name
                  { name = "Install zeromq3-dev"
                  , run = "sudo apt install zeromq3-dev"
                  }
              ]
      )
      haskellCi.DhallMatrix::{=}
