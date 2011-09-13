module GitHubUsers where 

import Control.Monad (liftM)
import GitHubGeneral
import JsonParsing

-- List users following another user
getFollowing :: UserName -> IO()
getFollowing u = request4User u "following" >>= (printRequest "login")

-- List followers of a user
getFollowers :: UserName -> IO()
getFollowers u = request4User u "followers" >>= (printRequest "login")

getNbFollowers :: UserName -> IO()
getNbFollowers u = request4User u "" >>= (printRequest "followers")

getNbFollowing :: UserName -> IO()
getNbFollowing u = request4User u "" >>= (printRequest "following")


{--------------------}
{-- REQUIRING AUTH --}
{--------------------}

-- Check if you are following a user
amIFollowing :: AuthTuple -> UserName -> IO ()
amIFollowing auth u = liftM (hasfound . fst) (getStatus4Req auth [] ("user/following/" ++ u)) >>= putStrLn . show
  where hasfound nb 
          | nb == 204 = True
          | otherwise = False

getKeys :: AuthTuple -> IO ()
getKeys auth = request "user/keys/" [auth] 
               >>= (printRequest "key")

-- unexpected end of input
getKeyByID :: AuthTuple -> Int -> IO String
getKeyByID auth id = request ("user/keys/" ++ (show id)) [auth] 
                     >>= return . head . (getStrVal "key") 

getEmails :: AuthTuple -> IO [String]
getEmails auth = request "user/emails" [auth] >>= return . (getStrVal "")
                 