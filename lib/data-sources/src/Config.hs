


data ConnectionStrings = ConnectionStrings
  { pgReadConnectionString ::
	, pgWriteConnectionString
	, redisReadConnectionString
	, redisWriteConnectionString
	, rabbitConnectionString
	, elasticSearchConnectionString
	}
