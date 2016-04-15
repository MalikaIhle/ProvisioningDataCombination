


SELECT 
tblCaptures.BirdID, 
tblCaptures.CaptureDate, 
[tblCaptures]![CaptureDate]-[sys_EggAndHatchDates]![HatchDate] AS AgeatCapture, 
tblMeasurements.Mass, tblMeasurements.Tarsus, 
sys_RearingAndNatalBroods.RearingBrood

FROM ((tblCaptures 
INNER JOIN sys_EggAndHatchDates ON tblCaptures.BirdID = sys_EggAndHatchDates.BirdID) 
INNER JOIN sys_RearingAndNatalBroods ON tblCaptures.BirdID = sys_RearingAndNatalBroods.BirdID) 
INNER JOIN tblMeasurements ON tblCaptures.CaptureRef = tblMeasurements.CaptureRef

WHERE ((([tblCaptures]![CaptureDate]-[sys_EggAndHatchDates]![HatchDate])=11) 
AND ((sys_EggAndHatchDates.HatchDate)>#1/1/2004#) AND ((tblCaptures.Stage)=2))
ORDER BY tblCaptures.BirdID;



-- usys_qBroodHatchDate

SELECT usys_qBroodsWithHatchlings.BroodRef, 

IIf([usys_qBroodHatchDatesFromTable].[HatchDate] Is Not Null,
[usys_qBroodHatchDatesFromTable].[HatchDate],
[usys_qBroodEggDate].[LayDate]+GetDBOption("AvgIncPeriod")) AS HatchDate, 

FROM (usys_qBroodsWithHatchlings 
LEFT JOIN usys_qBroodHatchDatesFromTable ON usys_qBroodsWithHatchlings.BroodRef = usys_qBroodHatchDatesFromTable.BroodRef) 
LEFT JOIN usys_qBroodEggDate ON usys_qBroodsWithHatchlings.BroodRef = usys_qBroodEggDate.BroodRef;



-- query 2: day 11
-- query 1: day >10
-- usys_broodhatchdate to developp
-- query 5: left join q1 on q2 with birdID q2 = NULL > birds that were only measure older than age 11
-- query 6: rearing brood that are involve in both query 2 and 5 > where chicks of a same brood were measure at age 11 AND others at age above 11

