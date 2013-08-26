{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Database.Redis.Simple (
  runRedis,
  auth,
  echo,
  ping,
  quit,
  select,
  del,
  dump,
  exists,
  expire,
  expireAt,
  keys,
  migrate,
  move,
  objectRefCount,
  objectEncoding,
  objectIdleTime,
  persist,
  pExpire,
  pExpireAt,
  pTtl,
  randomKey,
  rename,
  renameNx,
  restore,
  sort,
  sortStore,
  ttl,
  getType,
  hDel,
  hExists,
  hGet,
  hGetAll,
  hIncrBy,
  hIncrByFloat,
  hKeys,
  hLen,
  hMGet,
  hMSet,
  hSet,
  hSetNX,
  hVals,
  blPop,
  brPop,
  brPopLPush,
  lIndex,
  lInsertBefore,
  lInsertAfter,
  lLen,
  lPop,
  lPush,
  lPushX,
  lRange,
  lRem,
  lSet,
  lTrim,
  rPop,
  rPopLPush,
  rPush,
  rPushX,
  eval,
  evalSha,
  scriptExists,
  scriptFlush,
  scriptKill,
  scriptLoad,
  bgRewriteAof,
  bgSave,
  configGet,
  configResetStat,
  configSet,
  dbSize,
  debugObject,
  flushAll,
  flushDb,
  info,
  lastSave,
  save,
  slaveOf,
  slowLogGet,
  slowLogLen,
  slowLogReset,
  time,
  sAdd,
  sCard,
  sDiff,
  sDiffStore,
  sInter,
  sInterStore,
  sIsMember,
  sMembers,
  sMove,
  sPop,
  sRandMember,
  sRem,
  sUnion,
  sUnionStore,
  zAdd,
  zCard,
  zCount,
  zIncrBy,
  zInterStore,
  zInterStoreWeights,
  zRange,
  zRangeWithScores,
  zRangeByScore,
  zRangeByScoreWithScores,
  zRangeByScoreLimit,
  zRangeByScoreWithScoresLimit,
  zRank,
  zRem,
  zRemRangeByRank,
  zRemRangeByScore,
  zRevRange,
  zRevRangeWithScores,
  zRevRangeByScore,
  zRevRangeByScoreWithScores,
  zRevRangeByScoreLimit,
  zRevRangeByScoreWithScoresLimit,
  zRevRank,
  zScore,
  zUnionStore,
  zUnionStoreWeights,
  append,
  bitCount,
  bitCountRange,
  bitOpAnd,
  bitOpOr,
  bitOpXor,
  bitOpNot,
  decr,
  decrBy,
  get,
  getBit,
  getRange,
  getSet,
  incr,
  incrBy,
  incrByFloat,
  mGet,
  mSet,
  mSetNx,
  pSetEx,
  set,
  setBit,
  setEx,
  setNx,
  setRange,
  strLen,
  watch,
  unwatch,
  multiExec,
  publish,
  pubSub,
  sendRequest,
  R.Connection,
  R.ConnectInfo(..),
  R.defaultConnectInfo,
  R.PortID(..),
  R.connect,
  R.Status(..),
  R.ConnectionLostException,
  R.RedisResult(..),
  R.Reply,
  R.PubSub(..),
  R.Message(..),
  R.TxResult(..),
  Redis(..),
  RedisTx(..)
) where
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.ByteString (ByteString)
import qualified Database.Redis as R

class WrappedRedis m mi r | m -> mi r where
  wrap :: mi (r a) -> m a
  unwrap :: m a -> mi (r a)

newtype Redis a = Redis { fromRedis :: EitherT R.Reply R.Redis a }
  deriving (Functor, Applicative, Monad, MonadIO)

newtype RedisTx a = RedisTx { fromRedisTx :: R.RedisTx (R.Queued a) }

instance Functor RedisTx where
  fmap f m = RedisTx $ fmap (fmap f) $ fromRedisTx m

instance Applicative RedisTx where
  pure = RedisTx . pure . pure
  l <*> r = RedisTx $ (fromRedisTx l >>= \txl -> fromRedisTx r >>= \txr -> return $ txl <*> txr)

instance WrappedRedis Redis R.Redis (Either R.Reply) where
  wrap = Redis . EitherT
  unwrap = runEitherT . fromRedis

instance WrappedRedis RedisTx R.RedisTx R.Queued where
  wrap = RedisTx
  unwrap = fromRedisTx

runRedis :: R.Connection -> Redis a -> IO (Either R.Reply a)
runRedis conn m = R.runRedis conn $ runEitherT $ fromRedis m

auth :: ByteString -> Redis R.Status
auth = Redis . EitherT . R.auth

echo :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m ByteString
echo = wrap . R.echo

ping :: (R.RedisCtx mi r, WrappedRedis m mi r) => m R.Status
ping = wrap R.ping

quit :: (R.RedisCtx mi r, WrappedRedis m mi r) => m R.Status
quit = wrap R.quit

select :: (R.RedisCtx mi r, WrappedRedis m mi r) => Integer -> m R.Status
select = wrap . R.select

del :: (R.RedisCtx mi r, WrappedRedis m mi r) => [ByteString] -> m Integer
del = wrap . R.del

dump :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m ByteString
dump = wrap . R.dump

exists :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m Bool
exists = wrap . R.exists

expire :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> m Bool
expire k = wrap . R.expire k

expireAt :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> m Bool
expireAt k = wrap . R.expireat k

keys :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m [ByteString]
keys = wrap . R.keys

migrate :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> ByteString -> Integer -> Integer -> m R.Status
migrate host port key dest timeout = wrap $ R.migrate host port key dest timeout

move :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> m Bool
move k = wrap . R.move k

objectRefCount :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m Integer
objectRefCount = wrap . R.objectRefcount

objectEncoding :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m ByteString
objectEncoding = wrap . R.objectEncoding

objectIdleTime :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m Integer
objectIdleTime = wrap . R.objectIdletime

persist :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m Bool
persist = wrap . R.persist

pExpire :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> m Bool
pExpire k = wrap . R.pexpire k

pExpireAt :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> m Bool
pExpireAt k = wrap . R.pexpireat k

pTtl :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m Integer
pTtl = wrap . R.pttl

randomKey :: (R.RedisCtx mi r, WrappedRedis m mi r) => m (Maybe ByteString)
randomKey = wrap R.randomkey

rename :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> m R.Status
rename k = wrap . R.rename k

renameNx :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> m Bool
renameNx k = wrap . R.renamenx k

restore :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> ByteString -> m R.Status
restore k t v = wrap $ R.restore k t v

sort :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> R.SortOpts -> m [ByteString]
sort k = wrap . R.sort k

sortStore :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> R.SortOpts -> m Integer
sortStore k dest o = wrap $ R.sortStore k dest o

ttl :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m Integer
ttl = wrap . R.ttl

getType :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m R.RedisType
getType = wrap . R.getType

hDel :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> [ByteString] -> m Integer
hDel k = wrap . R.hdel k

hExists :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> m Bool
hExists k = wrap . R.hexists k

hGet :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> m (Maybe ByteString)
hGet k = wrap . R.hget k

hGetAll :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m [(ByteString, ByteString)]
hGetAll = wrap . R.hgetall

hIncrBy :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> Integer -> m Integer
hIncrBy k f i = wrap $ R.hincrby k f i

hIncrByFloat :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> Double -> m Double
hIncrByFloat k f i = wrap $ R.hincrbyfloat k f i

hKeys :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m [ByteString]
hKeys = wrap . R.hkeys

hLen :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m Integer
hLen = wrap . R.hlen

hMGet :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> [ByteString] -> m [Maybe ByteString]
hMGet k = wrap . R.hmget k

hMSet :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> [(ByteString, ByteString)] -> m R.Status
hMSet k = wrap . R.hmset k

hSet :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> ByteString -> m Bool
hSet k f v = wrap $ R.hset k f v

hSetNX :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> ByteString -> m Bool
hSetNX k f v = wrap $ R.hsetnx k f v

hVals :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m [ByteString]
hVals = wrap . R.hvals

blPop :: (R.RedisCtx mi r, WrappedRedis m mi r) => [ByteString] -> Integer -> m (Maybe (ByteString, ByteString))
blPop ks = wrap . R.blpop ks

brPop :: (R.RedisCtx mi r, WrappedRedis m mi r) => [ByteString] -> Integer -> m (Maybe (ByteString, ByteString))
brPop ks = wrap . R.brpop ks

brPopLPush :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> Integer -> m (Maybe ByteString)
brPopLPush src dest t = wrap $ R.brpoplpush src dest t

lIndex :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> m (Maybe ByteString)
lIndex k = wrap . R.lindex k

lInsertBefore :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> ByteString -> m Integer
lInsertBefore k p v = wrap $ R.linsertBefore k p v

lInsertAfter :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> ByteString -> m Integer
lInsertAfter k p v = wrap $ R.linsertAfter k p v

lLen :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m Integer
lLen = wrap . R.llen

lPop :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m (Maybe ByteString)
lPop = wrap . R.lpop

lPush :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> [ByteString] -> m Integer
lPush k = wrap . R.lpush k

lPushX :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> m Integer
lPushX k = wrap . R.lpushx k

lRange :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> Integer -> m [ByteString]
lRange k l r = wrap $ R.lrange k l r

lRem :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> ByteString -> m Integer
lRem k c v = wrap $ R.lrem k c v

lSet :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> ByteString -> m R.Status
lSet k i v = wrap $ R.lset k i v

lTrim :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> Integer -> m R.Status
lTrim k l r = wrap $ R.ltrim k l r

rPop :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m (Maybe ByteString)
rPop = wrap . R.rpop

rPopLPush :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> m (Maybe ByteString)
rPopLPush k = wrap . R.rpoplpush k

rPush :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> [ByteString] -> m Integer
rPush k = wrap . R.rpush k

rPushX :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> m Integer
rPushX k = wrap . R.rpushx k

eval :: (R.RedisCtx mi r, WrappedRedis m mi r, R.RedisResult a) => ByteString -> [ByteString] -> [ByteString] -> m a
eval s ks args = wrap $ R.eval s ks args

evalSha :: (R.RedisCtx mi r, WrappedRedis m mi r, R.RedisResult a) => ByteString -> [ByteString] -> [ByteString] -> m a
evalSha s ks args = wrap $ R.evalsha s ks args

scriptExists :: (R.RedisCtx mi r, WrappedRedis m mi r) => [ByteString] -> m [Bool]
scriptExists = wrap . R.scriptExists

scriptFlush :: (R.RedisCtx mi r, WrappedRedis m mi r) => m R.Status
scriptFlush = wrap R.scriptFlush

scriptKill :: (R.RedisCtx mi r, WrappedRedis m mi r) => m R.Status
scriptKill = wrap R.scriptKill

scriptLoad :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m ByteString
scriptLoad = wrap . R.scriptLoad

bgRewriteAof :: (R.RedisCtx mi r, WrappedRedis m mi r) => m R.Status
bgRewriteAof = wrap R.bgrewriteaof

bgSave :: (R.RedisCtx mi r, WrappedRedis m mi r) => m R.Status
bgSave = wrap R.bgsave

configGet :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m [(ByteString, ByteString)]
configGet = wrap . R.configGet

configResetStat :: (R.RedisCtx mi r, WrappedRedis m mi r) => m R.Status
configResetStat = wrap R.configResetstat

configSet :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> m R.Status
configSet param val = wrap $ R.configSet param val

dbSize :: (R.RedisCtx mi r, WrappedRedis m mi r) => m Integer
dbSize = wrap R.dbsize

debugObject :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m ByteString
debugObject = wrap . R.debugObject

flushAll :: (R.RedisCtx mi r, WrappedRedis m mi r) => m R.Status
flushAll = wrap R.flushall

flushDb :: (R.RedisCtx mi r, WrappedRedis m mi r) => m R.Status
flushDb = wrap R.flushdb

info :: (R.RedisCtx mi r, WrappedRedis m mi r) => m ByteString
info = wrap R.info

lastSave :: (R.RedisCtx mi r, WrappedRedis m mi r) => m Integer
lastSave = wrap R.lastsave

save :: (R.RedisCtx mi r, WrappedRedis m mi r) => m R.Status
save = wrap R.save

slaveOf :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> m R.Status
slaveOf k = wrap . R.slaveof k

slowLogGet :: (R.RedisCtx mi r, WrappedRedis m mi r) => Integer -> m [R.Slowlog]
slowLogGet = wrap . R.slowlogGet

slowLogLen :: (R.RedisCtx mi r, WrappedRedis m mi r) => m Integer
slowLogLen = wrap R.slowlogLen

slowLogReset :: (R.RedisCtx mi r, WrappedRedis m mi r) => m R.Status
slowLogReset = wrap R.slowlogReset

time :: (R.RedisCtx mi r, WrappedRedis m mi r) => m (Integer, Integer)
time = wrap R.time

sAdd :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> [ByteString] -> m Integer
sAdd k = wrap . R.sadd k

sCard :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m Integer
sCard = wrap . R.scard

sDiff :: (R.RedisCtx mi r, WrappedRedis m mi r) => [ByteString] -> m [ByteString]
sDiff = wrap . R.sdiff

sDiffStore :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> [ByteString] -> m Integer
sDiffStore k = wrap . R.sdiffstore k

sInter :: (R.RedisCtx mi r, WrappedRedis m mi r) => [ByteString] -> m [ByteString]
sInter = wrap . R.sinter

sInterStore :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> [ByteString] -> m Integer
sInterStore k = wrap . R.sinterstore k

sIsMember :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> m Bool
sIsMember k = wrap . R.sismember k

sMembers :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m [ByteString]
sMembers = wrap . R.smembers

sMove :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> ByteString -> m Bool
sMove k f t = wrap $ R.smove k f t

sPop :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m (Maybe ByteString)
sPop = wrap . R.spop

sRandMember :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m (Maybe ByteString)
sRandMember = wrap . R.srandmember

sRem :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> [ByteString] -> m Integer
sRem k = wrap . R.srem k

sUnion :: (R.RedisCtx mi r, WrappedRedis m mi r) => [ByteString] -> m [ByteString]
sUnion = wrap . R.sunion

sUnionStore :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> [ByteString] -> m Integer
sUnionStore k = wrap . R.sunionstore k

zAdd :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> [(Double, ByteString)] -> m Integer
zAdd k = wrap . R.zadd k

zCard :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m Integer
zCard = wrap . R.zcard

zCount :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Double -> Double -> m Integer
zCount k l h = wrap $ R.zcount k l h

zIncrBy :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> ByteString -> m Double
zIncrBy k i v = wrap $ R.zincrby k i v

zInterStore :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> [ByteString] -> R.Aggregate -> m Integer
zInterStore k vs a = wrap $ R.zinterstore k vs a

zInterStoreWeights :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> [(ByteString, Double)] -> R.Aggregate -> m Integer
zInterStoreWeights k vws a = wrap $ R.zinterstoreWeights k vws a

zRange :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> Integer -> m [ByteString]
zRange k l r = wrap $ R.zrange k l r

zRangeWithScores :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> Integer -> m [(ByteString, Double)]
zRangeWithScores k l r = wrap $ R.zrangeWithscores k l r

zRangeByScore :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Double -> Double -> m [ByteString]
zRangeByScore k l h = wrap $ R.zrangebyscore k l h

zRangeByScoreWithScores :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Double -> Double -> m [(ByteString, Double)]
zRangeByScoreWithScores k l h = wrap $ R.zrangebyscoreWithscores k l h

zRangeByScoreLimit :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Double -> Double -> Integer -> Integer -> m [ByteString]
zRangeByScoreLimit k max min offset count = wrap $ R.zrangebyscoreLimit k max min offset count

zRangeByScoreWithScoresLimit :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Double -> Double -> Integer -> Integer -> m [(ByteString, Double)]
zRangeByScoreWithScoresLimit k max min offset count = wrap $ R.zrangebyscoreWithscoresLimit k max min offset count

zRank :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> m (Maybe Integer)
zRank k = wrap . R.zrank k

zRem :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> [ByteString] -> m Integer
zRem k = wrap . R.zrem k

zRemRangeByRank :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> Integer -> m Integer
zRemRangeByRank k o c = wrap $ R.zremrangebyrank k o c

zRemRangeByScore :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Double -> Double -> m Integer
zRemRangeByScore k l h = wrap $ R.zremrangebyscore k l h

zRevRange :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> Integer -> m [ByteString]
zRevRange k o c = wrap $ R.zrevrange k o c

zRevRangeWithScores :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> Integer -> m [(ByteString, Double)]
zRevRangeWithScores k l r = wrap $ R.zrevrangeWithscores k l r

zRevRangeByScore :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Double -> Double -> m [ByteString]
zRevRangeByScore k l h = wrap $ R.zrevrangebyscore k l h

zRevRangeByScoreWithScores :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Double -> Double -> m [(ByteString, Double)]
zRevRangeByScoreWithScores k l h = wrap $ R.zrevrangebyscoreWithscores k l h

zRevRangeByScoreLimit :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Double -> Double -> Integer -> Integer -> m [ByteString]
zRevRangeByScoreLimit k l r o c = wrap $ R.zrevrangebyscoreLimit k l r o c

zRevRangeByScoreWithScoresLimit :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Double -> Double -> Integer -> Integer -> m [(ByteString, Double)]
zRevRangeByScoreWithScoresLimit k l r o c = wrap $ R.zrevrangebyscoreWithscoresLimit k l r o c

zRevRank :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> m (Maybe Integer)
zRevRank k = wrap . R.zrevrank k

zScore :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> m (Maybe Double)
zScore k = wrap . R.zscore k

zUnionStore :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> [ByteString] -> R.Aggregate -> m Integer
zUnionStore dest ks a = wrap $ R.zunionstore dest ks a

zUnionStoreWeights :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> [(ByteString, Double)] -> R.Aggregate -> m Integer
zUnionStoreWeights k vws a = wrap $ R.zunionstoreWeights k vws a

append :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> m Integer
append k = wrap . R.append k

bitCount :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m Integer
bitCount = wrap . R.bitcount

bitCountRange :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> Integer -> m Integer
bitCountRange k l r = wrap $ R.bitcountRange k l r

bitOpAnd :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> [ByteString] -> m Integer
bitOpAnd k = wrap . R.bitopAnd k

bitOpOr :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> [ByteString] -> m Integer
bitOpOr k = wrap . R.bitopOr k

bitOpXor :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> [ByteString] -> m Integer
bitOpXor k = wrap . R.bitopXor k

bitOpNot :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> m Integer
bitOpNot k = wrap . R.bitopNot k

decr :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m Integer
decr = wrap . R.decr

decrBy :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> m Integer
decrBy k = wrap . R.decrby k

get :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m (Maybe ByteString)
get = wrap . R.get

getBit :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> m Integer
getBit k = wrap . R.getbit k

getRange :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> Integer -> m ByteString
getRange k f t = wrap $ R.getrange k f t

getSet :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> m (Maybe ByteString)
getSet k v = wrap $ R.getset k v

incr :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m Integer
incr = wrap . R.incr

incrBy :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> m Integer
incrBy k = wrap . R.incrby k

incrByFloat :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Double -> m Double
incrByFloat k = wrap . R.incrbyfloat k

mGet :: (R.RedisCtx mi r, WrappedRedis m mi r) => [ByteString] -> m [Maybe ByteString]
mGet = wrap . R.mget

mSet :: (R.RedisCtx mi r, WrappedRedis m mi r) => [(ByteString, ByteString)] -> m R.Status
mSet = wrap . R.mset

mSetNx :: (R.RedisCtx mi r, WrappedRedis m mi r) => [(ByteString, ByteString)] -> m Bool
mSetNx = wrap . R.msetnx

pSetEx :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> ByteString -> m R.Status
pSetEx k t v = wrap $ R.psetex k t v

set :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> m R.Status
set k v = wrap $ R.set k v

setBit :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> ByteString -> m Integer
setBit k i v = wrap $ R.setbit k i v

setEx :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> ByteString -> m R.Status
setEx k t v = wrap $ R.setex k t v

setNx :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> m Bool
setNx k v = wrap $ R.setnx k v

setRange :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> Integer -> ByteString -> m Integer
setRange k i v = wrap $ R.setrange k i v

strLen :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> m Integer
strLen = wrap . R.strlen

watch :: [ByteString] -> Redis R.Status
watch = wrap . R.watch

unwatch :: Redis R.Status
unwatch = wrap R.unwatch

multiExec :: RedisTx a -> Redis (R.TxResult a)
multiExec m = Redis $ EitherT $ fmap Right $ R.multiExec $ fromRedisTx m

publish :: (R.RedisCtx mi r, WrappedRedis m mi r) => ByteString -> ByteString -> m Integer
publish chan msg = wrap $ R.publish chan msg

pubSub :: R.PubSub -> (R.Message -> IO R.PubSub) -> Redis ()
pubSub p f = Redis $ EitherT $ fmap Right $ R.pubSub p f

sendRequest :: (R.RedisCtx mi r, WrappedRedis m mi r, R.RedisResult a) => [ByteString] -> m a
sendRequest = wrap . R.sendRequest
