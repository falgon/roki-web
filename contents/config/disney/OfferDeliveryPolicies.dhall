let OfferDeliveryPolicy =
      { policyOfferId : Text
      , policyPartnerId : Text
      , policyPriority : Double
      , policyTargetShare : Double
      , policyMinShare : Double
      , policyMaxShare : Double
      , policyHistoricalImpressions : Natural
      , policyHistoricalClicks : Natural
      , policyExplorationWeight : Double
      }

in  [ { policyOfferId = "demo-disney-hotel-offer"
      , policyPartnerId = "tokyo-disney-resort"
      , policyPriority = 0.6
      , policyTargetShare = 0.5
      , policyMinShare = 0.2
      , policyMaxShare = 0.8
      , policyHistoricalImpressions = 0
      , policyHistoricalClicks = 0
      , policyExplorationWeight = 0.2
      }
    ]
