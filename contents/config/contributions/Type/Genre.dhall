let Genre_ =
      < Haskell : {}
      | Cpp : {}
      | JavaScript : {}
      | Rust : {}
      | Go : {}
      | Dhall : {}
      >

in  let genreHandler =
          { Haskell = λ(_ : {}) → "Haskell"
          , Cpp = λ(_ : {}) → "C++"
          , JavaScript = λ(_ : {}) → "JavaScript"
          , Rust = λ(_ : {}) → "Rust"
          , Go = λ(_ : {}) → "Go"
          , Dhall = λ(_ : {}) → "Dhall"
          }

    in  { Genre = Genre_, genreToText = λ(g : Genre_) → merge genreHandler g }
