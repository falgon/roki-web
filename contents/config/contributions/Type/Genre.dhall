let Genre_ =
      < Haskell : {}
      | Cpp : {}
      | JavaScript : {}
      | Rust : {}
      | Go : {}
      | Dhall : {}
      | Python : {}
      >

in  let genreHandler =
          { Haskell = λ(_ : {}) → "Haskell"
          , Cpp = λ(_ : {}) → "C++"
          , JavaScript = λ(_ : {}) → "JavaScript"
          , Rust = λ(_ : {}) → "Rust"
          , Go = λ(_ : {}) → "Go"
          , Dhall = λ(_ : {}) → "Dhall"
          , Python = λ(_ : {}) → "Python"
          }

    in  { Genre = Genre_, genreToText = λ(g : Genre_) → merge genreHandler g }
