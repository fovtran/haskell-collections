{-# LANGUAGE TypeApplications, OverloadedStrings, OverloadedLabels,
    TypeOperators, DataKinds, FlexibleContexts #-}

import Labels.Explore

main =
  runResourceT $
  httpSource "http://chrisdone.com/ontime.csv.zip" responseBody .|
  zipEntryConduit "ontime.csv" .|
  fromCsvConduit
    @("fl_date" := Day, "tail_num" := String)
    (set #downcase True csv) .|
  dropConduit 10 .|
  takeConduit 5 .>
  tableSink
  
  fromCsvConduit
    @("fl_date" := Day, "tail_num" := String)
    csv

	main =
  runResourceT $
  fileSource "ontime.csv" .|
  fromCsvConduit
    @("fl_date" := Day, "tail_num" := String)
    (set #downcase True csv) .|
  dropConduit 10 .|
  takeConduit 5 .>
  tableSink