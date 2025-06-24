let t = ./Type/Favorite.dhall

in    [ { text = "美女と野獣"
        , category = "works"
        , link = Some
            "https://disney.fandom.com/wiki/Beauty_and_the_Beast_(1991_film)"
        }
      , { text = "アラジン", category = "works", link = None Text }
      , { text = "リトル・マーメイド"
        , category = "works"
        , link = Some
            "https://disney.fandom.com/wiki/The_Little_Mermaid_(1989_film)"
        }
      , { text = "ライオン・キング", category = "works", link = None Text }
      , { text = "トイ・ストーリー"
        , category = "works"
        , link = Some "https://disney.fandom.com/wiki/Toy_Story"
        }
      , { text = "スプラッシュ・マウンテン"
        , category = "attractions"
        , link = Some "https://disney.fandom.com/wiki/Splash_Mountain"
        }
      , { text = "スペース・マウンテン", category = "attractions", link = None Text }
      , { text = "ホーンテッド・マンション"
        , category = "attractions"
        , link = Some "https://disney.fandom.com/wiki/Haunted_Mansion"
        }
      , { text = "ピーター・パン", category = "attractions", link = None Text }
      , { text = "イッツ・ア・スモールワールド"
        , category = "attractions"
        , link = Some "https://disney.fandom.com/wiki/It%27s_a_Small_World"
        }
      ]
    : List t
