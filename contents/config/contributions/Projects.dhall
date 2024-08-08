let g = ./Type/Genre.dhall

in    [ { projName = "htcc"
        , lang = g.genreToText (g.Genre.Haskell {=})
        , projLink = "https://github.com/falgon/htcc"
        , summary = "A full scratch, tiny C language compiler."
        }
      , { projName = "hmgit"
        , lang = g.genreToText (g.Genre.Haskell {=})
        , projLink = "https://github.com/falgon/hmgit"
        , summary = "A full scratch, minimal Git implementation."
        }
      , { projName = "mpl-lazyk"
        , lang = g.genreToText (g.Genre.Cpp {=})
        , projLink = "https://github.com/falgon/mpl-lazyk"
        , summary = "Compile-time Lazy K interpreter with C++17 template metaprogramming (monadic implementation)."
        }
      , { projName = "roki-web"
        , lang = g.genreToText (g.Genre.Haskell {=})
        , projLink = "https://github.com/falgon/roki-web"
        , summary = "The implementation of this site."
        }
      , { projName = "jpezy"
        , lang = g.genreToText (g.Genre.Cpp {=})
        , projLink = "https://github.com/falgon/jpezy"
        , summary = "A full scratch, JPEG encoder and decoder implementation."
        }
      , { projName = "network-basal"
        , lang = g.genreToText (g.Genre.Haskell {=})
        , projLink = "https://github.com/falgon/network-basal"
        , summary =
            "Simple implementation of ping by full scratch from Ethernet frame."
        }
      , { projName = "Srook C++ Libraries"
        , lang = g.genreToText (g.Genre.Cpp {=})
        , projLink = "https://github.com/falgon/SrookCppLibraries"
        , summary =
            "Modern-style (C++17) containers, iterators, memory managements and some utilities."
        }
      , { projName = "PlayLinearAlgebra"
        , lang = g.genreToText (g.Genre.Haskell {=})
        , projLink = "https://github.com/falgon/PlayLinearAlgebra"
        , summary =
            "My playground about linear algebra. It includes a least squares plotter. It works by solving by LU decomposition and solving by pseudo (Moore-Penrose) inverse matrix respectively."
        }
      , { projName = "edcc"
        , lang = g.genreToText (g.Genre.Go {=})
        , projLink = "https://github.com/falgon/edcc"
        , summary =
            "Simple and tiny comprehensive management tool for distributed compilation using distcc on AWS EC2."
        }
      , { projName = "bsimplified"
        , lang = g.genreToText (g.Genre.Haskell {=})
        , projLink = "https://github.com/falgon/bsimplified"
        , summary =
            "The simple and pure implementation of Quine-McCluskey method, Petrick's method and parsing of Boolean formula."
        }
      , { projName = "ElgamalEncryptionHs"
        , lang = g.genreToText (g.Genre.Haskell {=})
        , projLink = "https://github.com/falgon/ElgamalEncryptionHs"
        , summary =
            "The rustic implementation of ElGamal encryption encoder and its decoder."
        }
      ]
    : List ./Type/Project.dhall
