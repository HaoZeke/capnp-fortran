`sendResultsTo` yourself runs the call and returns `resultsSentElsewhere`,
and a later `takeFromOtherQuestion` returns those held results. A take
from any other answer aborts. A third party tail call aborts the connection.
