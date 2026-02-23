let Offer =
      { offerId : Text
      , offerTitle : Text
      , offerUrl : Text
      , offerCtaLabel : Text
      , offerDescription : Optional Text
      , offerUtmCampaign : Optional Text
      , offerIsActive : Bool
      }

in  [ { offerId = "demo-disney-hotel-offer"
      , offerTitle = "舞浜周辺ホテルの最新プランを見る"
      , offerUrl = "https://www.tokyodisneyresort.jp/hotel.html"
      , offerCtaLabel = "詳細を見る"
      , offerDescription = Some "サンプル掲載: 送客導線の表示確認用"
      , offerUtmCampaign = Some "demo_disney_offer"
      , offerIsActive = True
      }
    ]
