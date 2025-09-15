let Hotel =
      { hotelCode : Text
      , stays : Natural
      , details : List Text
      , hotelColor : Text
      }

in  [ { hotelCode = "FSH"
      , stays = 4
      , details =
        [ "ファンタジーシャトー", "　スプリングスサイド: バルアル", "　ローズコートサイド: スーペリアx2, スーペリア・アルコーヴ" ]
      , hotelColor = "#854454"
      }
    , { hotelCode = "DHM"
      , stays = 4
      , details = [ "スイート: ハバテラx2, ピアバル", "ポルトパラディーゾ: スーペリアルームハーバービュー" ]
      , hotelColor = "#8A7501"
      }
    , { hotelCode = "TDH"
      , stays = 4
      , details = [ "キャラ: 美女野獣, シンデレラ", "スーペリア: コーナールーム, パークグランドビュー" ]
      , hotelColor = "#B95C00"
      }
    , { hotelCode = "TSH"
      , stays = 3
      , details = [ "スタンダードルーム x3" ]
      , hotelColor = "#C28A02"
      }
    ]
