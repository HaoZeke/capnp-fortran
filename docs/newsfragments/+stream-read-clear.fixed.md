Stream readers free the target message before reading, so a failed read
leaves an empty message and no stale root from an earlier read.
