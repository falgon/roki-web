-- 任意の深さのホテル詳細をChurchエンコードで表現する
let HotelDetailF =
      λ(r : Type) →
        < HDStay : { stayLabel : Text, stayCount : Natural }
        | HDNode : { hdLabel : Text, hdChildren : List r }
        >

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

let DetailRaw = { detailPathRaw : List Text, stayCountRaw : Natural }

let prependToEach =
      λ(prefix : Text) →
      λ(details : List DetailRaw) →
        listMap
          DetailRaw
          DetailRaw
          ( λ(detail : DetailRaw) →
              { detailPathRaw = [ prefix ] # detail.detailPathRaw
              , stayCountRaw = detail.stayCountRaw
              }
          )
          details

let detailToPaths
    : HotelDetail → List DetailRaw
    = foldHotelDetail
        (List DetailRaw)
        ( λ(detailF : HotelDetailF (List DetailRaw)) →
            merge
              { HDStay =
                  λ(stay : { stayLabel : Text, stayCount : Natural }) →
                    [ { detailPathRaw = [ stay.stayLabel ]
                      , stayCountRaw = stay.stayCount
                      }
                    ]
              , HDNode =
                  λ ( node
                    : { hdLabel : Text, hdChildren : List (List DetailRaw) }
                    ) →
                    let childDetails = listConcat DetailRaw node.hdChildren

                    in  prependToEach node.hdLabel childDetails
              }
              detailF
        )

let detailsToPaths =
      λ(details : List HotelDetail) →
        listConcat
          DetailRaw
          (listMap HotelDetail (List DetailRaw) detailToPaths details)

let makeStay
    : Text → Natural → HotelDetail
    = λ(label : Text) →
      λ(count : Natural) →
      λ(r : Type) →
      λ(alg : HotelDetailF r → r) →
        alg ((HotelDetailF r).HDStay { stayLabel = label, stayCount = count })

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
      { hotelCodeRaw : Text, detailsRaw : List DetailRaw, hotelColorRaw : Text }

in    [ { hotelCodeRaw = "FSH"
        , detailsRaw =
            detailsToPaths
              [ makeNode
                  "ファンタジーシャトー"
                  [ makeNode "スプリングスサイド" [ makeStay "バルアル" 1 ]
                  , makeNode
                      "ローズコートサイド"
                      [ makeStay "スーペリア" 3, makeStay "スーペリア・アルコーヴ" 2 ]
                  , makeNode "ベイエリアサイド" [ makeStay "スーペリア・アルコーヴ" 2 ]
                  ]
              ]
        , hotelColorRaw = "#854454"
        }
      , { hotelCodeRaw = "DHM"
        , detailsRaw =
            detailsToPaths
              [ makeNode
                  "スイート"
                  [ makeStay "ハバグラ" 1, makeStay "ハバテラ" 2, makeStay "ピアバル" 1 ]
              , makeNode "ポルトパラディーゾ" [ makeStay "スーペリアルームハーバービュー" 1 ]
              ]
        , hotelColorRaw = "#8A7501"
        }
      , { hotelCodeRaw = "TDH"
        , detailsRaw =
            detailsToPaths
              [ makeNode "キャラ" [ makeStay "美女野獣" 1, makeStay "シンデレラ" 1 ]
              , makeNode
                  "スタンダード"
                  [ makeStay "スーペリアルーム" 1
                  , makeStay "コーナールーム" 2
                  , makeStay "スーペリア・アルコーヴ（パークグランドビュー）" 1
                  ]
              , makeNode "コンシェルジュ" [ makeStay "バルコニールーム パークグランドビュー" 1 ]
              ]
        , hotelColorRaw = "#B95C00"
        }
      , { hotelCodeRaw = "TSH"
        , detailsRaw = detailsToPaths [ makeStay "スタンダードルーム" 3 ]
        , hotelColorRaw = "#C28A02"
        }
      ]
    : List Hotel
