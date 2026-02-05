let t = ./Type/Favorite.dhall

in    [ { text = "アナと雪の女王"
        , category = "works"
        , link = Some "https://www.disney.co.jp/fc/anayuki"
        }
      , { text = "ズートピア"
        , category = "works"
        , link = Some "https://www.disney.co.jp/fc/zootopia"
        }
      , { text = "メリー・ポピンズ"
        , category = "works"
        , link = Some
            "https://www.disneyplus.com/ja-jp/browse/entity-f63db666-b097-4c61-99c1-b778de2d4ae1"
        }
      , { text = "魔法にかけられて"
        , category = "works"
        , link = Some "https://www.disney.co.jp/fc/live-action/enchanted"
        }
      , { text = "ミッキーマウス！"
        , category = "works"
        , link = Some
            "https://www.disneyplus.com/ja-jp/browse/entity-9ef52e33-d305-4043-ad90-a1b718ec8f65"
        }
      , { text = "ワンス・アポン・ア・スタジオ -100年の思い出-"
        , category = "works"
        , link = Some
            "https://www.disneyplus.com/ja-jp/browse/entity-ed8f308d-b613-48b4-aefc-e20f0c84fc64"
        }
      , { text = "スノーギース"
        , category = "characters"
        , link = Some "https://www.disney.co.jp/fc/anayuki/character/snowgies"
        }
      , { text = "エルサ"
        , category = "characters"
        , link = Some "https://www.disney.co.jp/fc/anayuki/character/elsa"
        }
      , { text = "ジュディ・ホップス"
        , category = "characters"
        , link = Some "https://www.disney.co.jp/fc/zootopia/character/judy"
        }
      , { text = "ミッキーのマジカルミュージックワールド"
        , category = "park-contents"
        , link = Some "https://www.tokyodisneyresort.jp/tdl/show/detail/895/"
        }
      , { text = "クラブマウスビート"
        , category = "park-contents"
        , link = Some "https://www.tokyodisneyresort.jp/tdl/show/detail/965/"
        }
      , { text = "ミッキーのフィルハーマジック"
        , category = "park-contents"
        , link = Some
            "https://www.tokyodisneyresort.jp/tdl/attraction/detail/167/"
        }
      , { text = "スター・ツアーズ"
        , category = "park-contents"
        , link = Some
            "https://www.tokyodisneyresort.jp/tdl/attraction/detail/183/"
        }
      , { text = "ビッグサンダー・マウンテン"
        , category = "park-contents"
        , link = Some
            "https://www.tokyodisneyresort.jp/tdl/attraction/detail/160/"
        }
      , { text = "アナとエルサのフローズンジャーニー"
        , category = "park-contents"
        , link = Some
            "https://www.tokyodisneyresort.jp/tds/attraction/detail/255/"
        }
      , { text = "タワー・オブ・テラー"
        , category = "park-contents"
        , link = Some
            "https://www.tokyodisneyresort.jp/tds/attraction/detail/243/"
        }
      , { text = "ウエスタンリバー鉄道"
        , category = "park-contents"
        , link = Some
            "https://www.tokyodisneyresort.jp/tdl/attraction/detail/154/"
        }
      ]
    : List t
