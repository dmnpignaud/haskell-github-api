module GitHubUsers where 

import Control.Monad (liftM)
import Numeric (readInt)

import GitHubGeneral
import JsonParsing

                
-- List users following another user
getFollowing :: UserName -> IO [String]
getFollowing u = u `getLogins` "following"

-- List followers of a user
getFollowers :: UserName -> IO [String]
getFollowers u = u `getLogins` "followers"

getNbFollowers :: UserName -> IO Int
getNbFollowers u =  u `getNb` "followers"

getNbFollowing :: UserName -> IO Int
getNbFollowing u = u `getNb` "following"
  
-- TODO : do we really need helpers ?
getLogins :: UserName -> String -> IO [String]
getLogins user relation = liftM (getStrVal "login") 
                 (request4User user relation)
                          
getNb :: UserName -> String -> IO Int
getNb user relation = liftM (rInt . head . (getStrVal relation))
                   (request4User user "")
  where rInt a = read (a)::Int


{--------------------}
{-- REQUIRING AUTH --}
{--------------------}

-- Check if you are following a user
amIFollowing :: AuthTuple -> UserName -> IO Bool
amIFollowing auth u = liftM (hasfound . fst) (getStatus4Req auth [] ("user/following/" ++ u))
  where hasfound nb 
          | nb == 204 = True
          | otherwise = False

getKeys :: AuthTuple -> IO [GitHubKey]
getKeys auth = liftM jsonToKeys 
               (request "user/keys" [auth])

getKeyByID :: AuthTuple -> Int -> IO GitHubKey
getKeyByID auth id = liftM (head . jsonToKeys) (request ("user/keys/" ++ (show id)) [auth] )
                     
getEmails :: AuthTuple -> IO [String]
getEmails auth = liftM (getStrVal "")
                 (request "user/emails" [auth])                 