SELECT precinct, vote_count, vote_percent, geometry 
FROM precinct_result
INNER JOIN ?geometry_table
USING(precinct)
WHERE year         = ?year
  AND election     = ?election
  AND jurisdiction = ?jurisdiction
  AND position     = ?position
  AND candidate    = ?candidate;
