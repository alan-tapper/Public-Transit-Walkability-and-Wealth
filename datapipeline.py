DISTANCE_CUTOFF_MILES = 1
WALK_TIME_CUTOFF_SECONDS = 600

transit_ids = getTransitIds()
block_ids = getBlockIds()

# create “blockinfo.csv” with schema (block_id, transit_id, walk_time, walk_score)

for transit_id in transit_ids:

	blocks_within_one_mile = getBlocksWithinDistanceCutoff(transit_id)

	for block in blocks_within_one_mile:

		walk_time = getWalkTime(block_id, transit_id)

		if walk_time <= WALK_TIME_CUTOFF_SECONDS:
			
			walk_score = getWalkScore(block_id)		
	
			# write (block_id, transit_id, walk_time, walk_score) to “blockinfo.csv”


def getTransitIds():
	

			
