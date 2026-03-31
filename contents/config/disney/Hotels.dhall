-- 任意の深さのホテル詳細をChurchエンコードで表現する
let HotelDetailF =
      λ(r : Type) →
        < HDText : Text | HDNode : { hdLabel : Text, hdChildren : List r } >

let HotelDetail = ∀(r : Type) → (HotelDetailF r → r) → r

let foldHotelDetail =
      λ(r : Type) →
      λ(alg : HotelDetailF r → r) →
      λ(detail : HotelDetail) →
        detail r alg

let listMap =
      λ(a : Type) →
      λ(b : Type) →
      λ(f : a → b) →
      λ(xs : List a) →
        List/fold
          a
          xs
          (List b)
          (λ(x : a) → λ(acc : List b) → [ f x ] # acc)
          ([] : List b)

let listConcat =
      λ(a : Type) →
      λ(xss : List (List a)) →
        List/fold
          (List a)
          xss
          (List a)
          (λ(xs : List a) → λ(acc : List a) → xs # acc)
          ([] : List a)

let prependToEach =
      λ(prefix : Text) →
      λ(paths : List (List Text)) →
        listMap
          (List Text)
          (List Text)
          (λ(path : List Text) → [ prefix ] # path)
          paths

let detailToPaths
    : HotelDetail → List (List Text)
    = foldHotelDetail
        (List (List Text))
        ( λ(detailF : HotelDetailF (List (List Text))) →
            merge
              { HDText = λ(text : Text) → [ [ text ] ]
              , HDNode =
                  λ ( node
                    : { hdLabel : Text, hdChildren : List (List (List Text)) }
                    ) →
                    let childPaths = listConcat (List Text) node.hdChildren

                    in  prependToEach node.hdLabel childPaths
              }
              detailF
        )

let detailsToPaths =
      λ(details : List HotelDetail) →
        listConcat
          (List Text)
          (listMap HotelDetail (List (List Text)) detailToPaths details)

let makeText
    : Text → HotelDetail
    = λ(text : Text) →
      λ(r : Type) →
      λ(alg : HotelDetailF r → r) →
        alg ((HotelDetailF r).HDText text)

let makeNode
    : Text → List HotelDetail → HotelDetail
    = λ(label : Text) →
      λ(children : List HotelDetail) →
      λ(r : Type) →
      λ(alg : HotelDetailF r → r) →
        let mappedChildren =
              listMap
                HotelDetail
                r
                (λ(child : HotelDetail) → child r alg)
                children

        in  alg
              ( (HotelDetailF r).HDNode
                  { hdLabel = label, hdChildren = mappedChildren }
              )

let Hotel =
      { hotelCodeRaw : Text
      , staysRaw : Natural
      , detailsRaw : List (List Text)
      , hotelColorRaw : Text
      }

in  [ { hotelCodeRaw = "FSH"
      , staysRaw = 6
      , detailsRaw =
          detailsToPaths
            [ makeNode
                "ファンタジーシャトー"
                [ makeNode "スプリングスサイド" [ makeText "バルアル" ]
                , makeNode
                    "ローズコートサイド"
                    [ makeText "スーペリア ×3", makeText "スーペリア・アルコーヴ ×2" ]
                ]
            ]
      , hotelColorRaw = "#854454"
      }
    , { hotelCodeRaw = "DHM"
      , staysRaw = 5
      , detailsRaw =
          detailsToPaths
            [ makeNode
                "スイート"
                [ makeText "ハバグラ", makeText "ハバテラ ×2", makeText "ピアバル" ]
            , makeNode "ポルトパラディーゾ" [ makeText "スーペリアルームハーバービュー" ]
            ]
      , hotelColorRaw = "#8A7501"
      }
    , { hotelCodeRaw = "TDH"
      , staysRaw = 5
      , detailsRaw =
          detailsToPaths
            [ makeNode "キャラ" [ makeText "美女野獣", makeText "シンデレラ" ]
            , makeNode "スーペリア" [ makeText "コーナールーム ×2", makeText "パークグランドビュー" ]
            ]
      , hotelColorRaw = "#B95C00"
      }
    , { hotelCodeRaw = "TSH"
      , staysRaw = 3
      , detailsRaw = detailsToPaths [ makeText "スタンダードルーム ×3" ]
      , hotelColorRaw = "#C28A02"
      }
    ]
