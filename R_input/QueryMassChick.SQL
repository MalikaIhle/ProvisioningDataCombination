-- Malika Ihle 20160405
-- MODIFIED sys_ChickMassStats --
-- House sparrow DB --




-- this is a query that is trying to get avg mass of chicks at different age class, per social/rearing brood.

-- usys_qChickMassStatsComplete

SELECT tblBroods.BroodRef, 										
usys_qChickMassAllStats.[AvgMassDay11 - 14], 
usys_qChickMassAllStats.[MinMassDay11 - 14], 
usys_qChickMassAllStats.[MaxMassDay11 - 14], 
usys_qChickMassAllStats.[CVMassDay11 - 14], 
usys_qChickMassAllStats.[AvgAgeDay11 - 14]

FROM ((tblBroods 
INNER JOIN usys_qRearingBroodSize ON tblBroods.BroodRef = usys_qRearingBroodSize.RearingBrood) 
INNER JOIN usys_qBroodEggDate ON tblBroods.BroodRef = usys_qBroodEggDate.BroodRef) 
INNER JOIN usys_qChickMassAllStats ON tblBroods.BroodRef = usys_qChickMassAllStats.RearingBrood;

		
		-- usys_qRearingBroodSize
		SELECT IIf([FosterBrood] Is Null,[BroodRef],[FosterBrood]) AS RearingBrood, 
		Count(*) AS RearBroodSize, 
		Min([XFosterRef]) Is Not Null AS IsFosterBrood							-- isn't the fosterRef null ? yes it cross fostered (-1) or no (0) (just need one chick to be crossfostered for that)
		
		FROM tblBirdID 
		LEFT JOIN tblFosterBroods ON tblBirdID.BirdID = tblFosterBroods.BirdID
		
		WHERE (((tblBirdID.LastStage)>1) AND ((tblBirdID.BroodRef) Is Not Null)) -- does not count chicks that hatched but were never seen alive (=chigg)
		
		GROUP BY IIf([FosterBrood] Is Null,[BroodRef],[FosterBrood]);

		
		
		-- usys_qChickMassAllStats									-- give per rearing brood all the stats per age class obtained from the pivot tables
		SELECT usys_qChickMassAvgMass.RearingBrood, 
		

		usys_qChickMassAvgMass.[11 - 14] AS [AvgMassDay11 - 14], 
		usys_qChickMassMinMass.[11 - 14] AS [MinMassDay11 - 14], 
		usys_qChickMassMaxMass.[11 - 14] AS [MaxMassDay11 - 14], 
		usys_qChickMassCVMass.[11 - 14] AS [CVMassDay11 - 14], 
		usys_qChickMassAvgClassAge.[11 - 14] AS [AvgAgeDay11 - 14]
		
		FROM (((usys_qChickMassAvgMass 
		INNER JOIN usys_qChickMassMaxMass ON usys_qChickMassAvgMass.RearingBrood = usys_qChickMassMaxMass.RearingBrood) 
		INNER JOIN usys_qChickMassMinMass ON usys_qChickMassAvgMass.RearingBrood = usys_qChickMassMinMass.RearingBrood) 
		INNER JOIN usys_qChickMassAvgClassAge ON usys_qChickMassAvgMass.RearingBrood = usys_qChickMassAvgClassAge.RearingBrood) 
		INNER JOIN usys_qChickMassCVMass ON usys_qChickMassAvgMass.RearingBrood = usys_qChickMassCVMass.RearingBrood;

		
		
				
				-- usys_qChickMassAvgMass
				
				TRANSFORM First(usys_qChickMassMeanSdMeasureDay.AvgMass) AS FirstOfAvgMass				-- value: Avg chick mass per rearing brood per age class
				SELECT usys_qChickMassMeanSdMeasureDay.RearingBrood
				FROM usys_qChickMassMeanSdMeasureDay
				GROUP BY usys_qChickMassMeanSdMeasureDay.RearingBrood									-- row heading
				PIVOT usys_qChickMassMeanSdMeasureDay.ClassName;										-- column heading
			
			
				-- usys_qChickMassMinMass
				
				TRANSFORM First(usys_qChickMassMeanSdMeasureDay.MinMass) AS FirstOfMinMass				-- value: Min chick mass per rearing brood per age class
				SELECT usys_qChickMassMeanSdMeasureDay.RearingBrood
				FROM usys_qChickMassMeanSdMeasureDay
				GROUP BY usys_qChickMassMeanSdMeasureDay.RearingBrood									-- row heading
				PIVOT usys_qChickMassMeanSdMeasureDay.ClassName;										-- column heading

				
				-- usys_qChickMassMaxMass
		
				TRANSFORM First(usys_qChickMassMeanSdMeasureDay.MaxMass) AS FirstOfMaxMass				-- value: Max chick mass per rearing brood per age class
				SELECT usys_qChickMassMeanSdMeasureDay.RearingBrood
				FROM usys_qChickMassMeanSdMeasureDay
				GROUP BY usys_qChickMassMeanSdMeasureDay.RearingBrood									-- row heading
				PIVOT usys_qChickMassMeanSdMeasureDay.ClassName;										-- column heading
				
				
				-- usys_qChickMassAvgClassAge

				TRANSFORM First(usys_qChickMassMeanSdMeasureDay.AvgClassAge) AS FirstOfAvgClassAge		-- value: Avg Age per rearing brood per age class --?
				SELECT usys_qChickMassMeanSdMeasureDay.RearingBrood
				FROM usys_qChickMassMeanSdMeasureDay
				GROUP BY usys_qChickMassMeanSdMeasureDay.RearingBrood									-- row heading
				PIVOT usys_qChickMassMeanSdMeasureDay.ClassName;										-- column heading

				
				-- usys_qChickMassCVMass			

				TRANSFORM First([SdMass]/[AvgMass]) AS Expr1											-- value: CV chick mass per rearing brood per age class
				SELECT usys_qChickMassMeanSdMeasureDay.RearingBrood
				FROM usys_qChickMassMeanSdMeasureDay
				GROUP BY usys_qChickMassMeanSdMeasureDay.RearingBrood									-- row heading
				PIVOT usys_qChickMassMeanSdMeasureDay.ClassName;										-- column heading
				
				


							-- usys_qChickMassMeanSdMeasureDay											-- this will calculate things for social brood per Age Class

							SELECT usys_qRearingBrood.RearingBrood, 									-- this will calculate things for social brood
							usys_qRelativeChickMassClassesForCaptures.MassClass AS MeasureDay, 			-- the day of measurement 'd3, d7, d10, d14' even if their age was a bit different
							First(usys_tblRelativeChickMassClasses.Class) AS ClassName, 				-- give the actual age range for that type of measurement e.g.:'d14'=11-14
							Avg(tblMeasurements.Mass) AS AvgMass, 										-- the avg mass of a chick in this social brood at this age class
							StDev(tblMeasurements.Mass) AS SdMass, 
							Min(tblMeasurements.Mass) AS MinMass, 
							Max(tblMeasurements.Mass) AS MaxMass, 
							Count(tblMeasurements.Mass) AS n, 											-- the number of chicks in this social brood (~between 1 and 10)
							Avg(usys_qRelativeChickMassClassesForCaptures.Age) AS AvgClassAge			-- the avg age of the chicks of this social brood when measured at 'd3', 'd7'...

							FROM (((usys_qRearingBrood 
							INNER JOIN tblCaptures ON usys_qRearingBrood.BirdID = tblCaptures.BirdID) 
							INNER JOIN usys_qRelativeChickMassClassesForCaptures ON tblCaptures.CaptureRef = usys_qRelativeChickMassClassesForCaptures.CaptureRef) 
							INNER JOIN usys_tblRelativeChickMassClasses ON usys_qRelativeChickMassClassesForCaptures.MassClass = usys_tblRelativeChickMassClasses.MassDay) 
							INNER JOIN tblMeasurements ON tblCaptures.CaptureRef = tblMeasurements.CaptureRef

							WHERE (((usys_qRearingBrood.RearingBrood) Is Not Null) AND ((usys_tblRelativeChickMassClasses.Display)=True))
							GROUP BY usys_qRearingBrood.RearingBrood, usys_qRelativeChickMassClassesForCaptures.MassClass
							HAVING (((Count(tblMeasurements.Mass))>0));



									-- usys_qRearingBrood
									
									SELECT tblBirdID.BirdID, IIf([FosterBrood] Is Null,[BroodRef],[FosterBrood]) AS RearingBrood
									FROM tblBirdID LEFT JOIN tblFosterBroods ON tblBirdID.BirdID = tblFosterBroods.BirdID
									WHERE (((tblBirdID.BroodRef) Is Not Null));
									
									
									-- usys_tblRelativeChickMassClasses
									MassDay		Class	Display
										1	     <=1	  No
										3	    2 - 3	  Yes
										7	    4 - 7	  Yes
										10	    8 - 10	  No
										14	   11 - 14	  Yes
										
									
									
									-- usys_qRelativeChickMassClassesForCaptures

									SELECT tblCaptures.CaptureRef, 
									Min(usys_tblRelativeChickMassClasses.MassDay) AS MassClass, 
									First(tblCaptures.CaptureDate-[HatchDate])+1 AS Age            -- true as query grouped per CaptureRef only for chick captures

									FROM usys_tblRelativeChickMassClasses, tblCaptures 
									INNER JOIN usys_qBirdEggHatchDates ON tblCaptures.BirdID = usys_qBirdEggHatchDates.BirdID

									WHERE (((([tblCaptures].[CaptureDate]-[HatchDate])+1)<=[MassDay]) AND ((tblCaptures.Stage)<3)) -- select chick captures per age class
									GROUP BY tblCaptures.CaptureRef;
									

									
									
									
									
									
									
									