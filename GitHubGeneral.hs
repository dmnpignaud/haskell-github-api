module GitHubGeneral where

import Network.HTTP
import Network.Curl 
import Control.Applicative ((<$>))
import Control.Monad (liftM,liftM2)

type UserName = String
type Password = String
type JSONResult = String
type AuthTuple = CurlOption
                 
urlBase :: String
urlBase = "https://api.github.com/"

authentification :: UserName -> Password -> AuthTuple
authentification u p =  (CurlUserPwd (u ++ ":" ++ p))                  

request4User :: UserName -> String -> IO (String)
request4User u suffix = request ("users/" ++ u ++ suffix2) []
  where suffix2 
          | suffix == "" = ""
          | otherwise = "/" ++ suffix
                       
-- TODO : suppress the parameters                        
request :: String -> [CurlOption] ->  IO (String)
request s c = snd `liftM` (requestFullResp s c)

requestFullResp :: String -> [CurlOption] ->  IO (CurlCode,String)
requestFullResp suffix opts = curlGetString (urlBase ++ suffix) opts 
--request2 :: UserName -> String                       
--request2 suffix = simpleHTTP(getRequest ("https://api.github.com/" ++ suffix)) >>= fmap (take 100) .getResponseBody

--parameters :
-- the authentification tuple
-- other options
-- the end of the URL request
--return the JSON result
requestFullRespWithAuth :: AuthTuple -> [CurlOption] -> String -> IO (CurlCode,String)
requestFullRespWithAuth auth opts suffix = requestFullResp suffix (auth:opts)


getStatus4Req :: AuthTuple -> [CurlOption] -> String -> IO (Int,String)
getStatus4Req auth opts suffix = liftM2 (,) (liftM respStatus response) (liftM respStatusLine response)
  where response = curlGetResponse (urlBase ++ suffix) (auth:opts)


-- "[
--   {
--     \"url\": \"https://api.github.com/users/kennethreitz\",
--     \"login\": \"kennethreitz\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/2eccc4005572c1e2b12a9c00580bc86f?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 119893
--   },
--   {
--     \"url\": \"https://api.github.com/users/chickenkiller\",
--     \"login\": \"chickenkiller\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/936431ecc527fd7e349d4abc9ff0e7c1?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 334805
--   },
--   {
--     \"url\": \"https://api.github.com/users/jmettraux\",
--     \"login\": \"jmettraux\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/8d96626e52beb1ff90f57a8e189e1e6f?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 3624
--   },
--   {
--     \"url\": \"https://api.github.com/users/lrvick\",
--     \"login\": \"lrvick\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/ce3b7bc97b5adbdc2cae4d231e4ebad8?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 69200
--   },
--   {
--     \"url\": \"https://api.github.com/users/dpflug\",
--     \"login\": \"dpflug\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/a02257f4090a7a576bf6d0bf29b7e06a?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 108501
--   },
--   {
--     \"url\": \"https://api.github.com/users/michaelficarra\",
--     \"login\": \"michaelficarra\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/24fb0088507077f7852adaec4f6b679f?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 218840
--   },
--   {
--     \"url\": \"https://api.github.com/users/pitr\",
--     \"login\": \"pitr\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/ba162f227525e37d91a86ef11789cc98?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 102791
--   },
--   {
--     \"url\": \"https://api.github.com/users/kmikzjh\",
--     \"login\": \"kmikzjh\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/546848e15fb9f1b5c69df8dcfe592b1f?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 654483
--   },
--   {
--     \"url\": \"https://api.github.com/users/chok\",
--     \"login\": \"chok\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/f8cb3b58603afcae01da439fddfcb462?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 271343
--   },
--   {
--     \"url\": \"https://api.github.com/users/jowido\",
--     \"login\": \"jowido\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/1adf3461720000516be077b6acf83577?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 566182
--   },
--   {
--     \"url\": \"https://api.github.com/users/jdoliner\",
--     \"login\": \"jdoliner\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/591a6387ecff42a07c2910aef552ad3a?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 43867
--   },
--   {
--     \"url\": \"https://api.github.com/users/alanzeino\",
--     \"login\": \"alanzeino\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/1b946c50db41d21a43e657ebc9acb16f?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 470797
--   },
--   {
--     \"url\": \"https://api.github.com/users/henkisdabro\",
--     \"login\": \"henkisdabro\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/535c8a0f2f0b2684ca9db67a08075240?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 477513
--   },
--   {
--     \"url\": \"https://api.github.com/users/chrisforbes\",
--     \"login\": \"chrisforbes\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/41e73ee1d1d259aa232965a2795a549f?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 128877
--   },
--   {
--     \"url\": \"https://api.github.com/users/jj56\",
--     \"login\": \"jj56\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/80a4c048bac773c59bfc3303df4fc55f?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 691552
--   },
--   {
--     \"url\": \"https://api.github.com/users/JoseRibeiro\",
--     \"login\": \"JoseRibeiro\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/dde66d31c9b436ea13a4bd7ef5c2d9ce?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 108473
--   },
--   {
--     \"url\": \"https://api.github.com/users/enguerran\",
--     \"login\": \"enguerran\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/2e43fe3376aaf887aad755de94942fa0?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 701648
--   },
--   {
--     \"url\": \"https://api.github.com/users/cebe\",
--     \"login\": \"cebe\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/2ebfe57beabd0b9f8eb9ded1237a275d?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 189796
--   },
--   {
--     \"url\": \"https://api.github.com/users/At-sushi\",
--     \"login\": \"At-sushi\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/19b529d811ac83b7558f02689635d4bc?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 720265
--   },
--   {
--     \"url\": \"https://api.github.com/users/DouglasDuteil\",
--     \"login\": \"DouglasDuteil\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/1e7cd3d5b060997af752aee10d724da1?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 730511
--   },
--   {
--     \"url\": \"https://api.github.com/users/dingzhihu\",
--     \"login\": \"dingzhihu\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/c0124c188367280ab829fd02a6565420?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 707494
--   },
--   {
--     \"url\": \"https://api.github.com/users/cindybubbles\",
--     \"login\": \"cindybubbles\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/d38d2ee521ea17602f2b16a245ab6557?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 677270
--   },
--   {
--     \"url\": \"https://api.github.com/users/rvhani\",
--     \"login\": \"rvhani\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/de9d42ff025a577679da0594ed124e3f?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 740282
--   },
--   {
--     \"url\": \"https://api.github.com/users/AgentD\",
--     \"login\": \"AgentD\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/c885ddf91d54dd0735096d8e8f93be0b?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 746041
--   },
--   {
--     \"url\": \"https://api.github.com/users/ctshryock\",
--     \"login\": \"ctshryock\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/dfb3948650131e4f0385c3328187cfca?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 61721
--   },
--   {
--     \"url\": \"https://api.github.com/users/kedusi\",
--     \"login\": \"kedusi\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/45d3413e3f8b8702741dd6eb7b650e3a?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 796895
--   },
--   {
--     \"url\": \"https://api.github.com/users/fantcha\",
--     \"login\": \"fantcha\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/e599dcfbebe84cacddc2e63aadcefc6f?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 800418
--   },
--   {
--     \"url\": \"https://api.github.com/users/jiska\",
--     \"login\": \"jiska\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/6edaeca6d206ade350bc34b74ecc07e1?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 210861
--   },
--   {
--     \"url\": \"https://api.github.com/users/mech4\",
--     \"login\": \"mech4\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/a15a6dcec0816e065c91ccce6f3b1c16?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 822375
--   },
--   {
--     \"url\": \"https://api.github.com/users/axiomsofchoice\",
--     \"login\": \"axiomsofchoice\",
--     \"avatar_url\": \"https://secure.gravatar.com/avatar/bb35b04c0e61548a328493ecf1417f52?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png\",
--     \"id\": 136502
--   }
-- ]"
