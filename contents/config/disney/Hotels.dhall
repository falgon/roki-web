-- ホテルの詳細情報を階層的に表現する型（シンプル版）
-- 子要素はテキストのリストとして表現
let HotelDetail =
      < HDText : Text | HDNode : { hdLabel : Text, hdChildren : List Text } >

let Hotel =
      { hotelCode : Text
      , stays : Natural
      , details : List HotelDetail
      , hotelColor : Text
      }

in  [ { hotelCode = "FSH"
      , stays = 4
      , details =
        [ HotelDetail.HDText "ファンタジーシャトー"
        , HotelDetail.HDNode { hdLabel = "スプリングスサイド", hdChildren = [ "バルアル" ] }
        , HotelDetail.HDNode
            { hdLabel = "ローズコートサイド"
            , hdChildren = [ "スーペリア ×2", "スーペリア・アルコーヴ" ]
            }
        ]
      , hotelColor = "#854454"
      }
    , { hotelCode = "DHM"
      , stays = 4
      , details =
        [ HotelDetail.HDNode
            { hdLabel = "スイート", hdChildren = [ "ハバテラ ×2", "ピアバル" ] }
        , HotelDetail.HDNode
            { hdLabel = "ポルトパラディーゾ", hdChildren = [ "スーペリアルームハーバービュー" ] }
        ]
      , hotelColor = "#8A7501"
      }
    , { hotelCode = "TDH"
      , stays = 4
      , details =
        [ HotelDetail.HDNode
            { hdLabel = "キャラ", hdChildren = [ "美女野獣", "シンデレラ" ] }
        , HotelDetail.HDNode
            { hdLabel = "スーペリア", hdChildren = [ "コーナールーム", "パークグランドビュー" ] }
        ]
      , hotelColor = "#B95C00"
      }
    , { hotelCode = "TSH"
      , stays = 3
      , details = [ HotelDetail.HDText "スタンダードルーム ×3" ]
      , hotelColor = "#C28A02"
      }
    ]
