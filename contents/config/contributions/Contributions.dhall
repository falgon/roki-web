let g = ./Type/Genre.dhall

in    [ { text = "cpprefjp: Fix typo #388"
        , link =
            "https://github.com/cpprefjp/site/pull/388/commits/5652d53580b10e357f1093b907b90021dc04ebc1"
        , genre = g.genreToText (g.Genre.Cpp {=})
        , date = { yyyy = 2017, mm = 1, dd = 9 }
        }
      , { text = "cppreference: Fix typo"
        , link =
            "http://en.cppreference.com/mwiki/index.php?title=cpp/language/class_template_argument_deduction&diff=prev&oldid=92071"
        , genre = g.genreToText (g.Genre.Cpp {=})
        , date = { yyyy = 2017, mm = 4, dd = 4 }
        }
      , { text = "cpprefjp: Fix typo 8c9490"
        , link =
            "https://github.com/cpprefjp/site/commit/8c09490285f735480bfacec2037fa7d94742b370"
        , genre = g.genreToText (g.Genre.Cpp {=})
        , date = { yyyy = 2017, mm = 7, dd = 6 }
        }
      , { text = "MDN web docs Revision 1279273 of parseInt()"
        , link =
            "https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/parseInt\$compare?locale=ja&to=1279273&from=1105781"
        , genre = g.genreToText (g.Genre.JavaScript {=})
        , date = { yyyy = 2017, mm = 7, dd = 29 }
        }
      , { text = "P0052 unique_resource implementation: Fix assert typo"
        , link = "https://github.com/PeterSommerlad/SC22WG21_Papers/pull/4"
        , genre = g.genreToText (g.Genre.Cpp {=})
        , date = { yyyy = 2017, mm = 8, dd = 16 }
        }
      , { text = "cpprefjp: Fix typo 336fa5e"
        , link =
            "https://github.com/cpprefjp/site/commit/6bb217759bd852629c2e24cd8285bf8925719ad4"
        , genre = g.genreToText (g.Genre.Cpp {=})
        , date = { yyyy = 2017, mm = 8, dd = 26 }
        }
      , { text =
            "Boost C++ libraries predef: fix typo s/BOOST_ARCH_PARISK/BOOST_ARCH_PARISC"
        , link = "https://github.com/boostorg/predef/pull/63"
        , genre = g.genreToText (g.Genre.Cpp {=})
        , date = { yyyy = 2017, mm = 10, dd = 11 }
        }
      , { text = "P0051 - C++ generic overload function: removed extra comma"
        , link = "https://github.com/viboes/std-make/pull/33"
        , genre = g.genreToText (g.Genre.Cpp {=})
        , date = { yyyy = 2017, mm = 11, dd = 13 }
        }
      , { text =
            "P0053 - C++ Synchronized Buffered Ostream: fixed to return *this in __basic_syncbuf::operator= and changed the variable name so that the declaration of 'syncbuf' does not shadow a global declaration(line: 324)."
        , link = "https://github.com/PeterSommerlad/SC22WG21_Papers/pull/5"
        , genre = g.genreToText (g.Genre.Cpp {=})
        , date = { yyyy = 2017, mm = 11, dd = 23 }
        }
      , { text =
            "meta-cpp/clang: Fix to enable compilation with clang and gcc other than Visual C++."
        , link = "https://github.com/meta-cpp/clang/pull/1"
        , genre = g.genreToText (g.Genre.Cpp {=})
        , date = { yyyy = 2019, mm = 6, dd = 11 }
        }
      , { text = "restyled-io/restylers: Add dhall format"
        , link = "https://github.com/restyled-io/restylers/pull/96"
        , genre = g.genreToText (g.Genre.Dhall {=})
        , date = { yyyy = 2020, mm = 9, dd = 11 }
        }
      ]
    : List ./Type/Contribute.dhall
