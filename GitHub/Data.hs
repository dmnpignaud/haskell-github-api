module GitHub.Data 
       (  
         GitHubUser(..)
       , GitHubRepo(..)
       , GitHubKey(..)
       ) where
  
  

import Data.Time
import System.Locale

type Url = String
type UserName = String

data GitHubUser = GHUser {
  login :: UserName,
  user_name :: String,
  user_id ::Int,
  avatarUrl :: Url,
  user_url :: Url,
  company :: String,
  blog :: Url,
  location :: String,
  email :: String,
  hireable :: Bool,
  bio :: String, 
  publicRepos :: Int,
  publicGists :: Int,
  followers :: Int,
  following :: Int,
  user_htmlUrl :: Url,
  user_createdAt :: UTCTime,
  user_type :: String -- TODO : maybe it should be an enum
} deriving (Show)

data GitHubPrivUser = GHPrivUser{
  totalPrivateRepos :: Int,
  ownedPrivateRepos :: Int,
  privateGists :: Int,
  diskUsage :: Int,
  collaborators :: Int,
  planName :: String, 
  planSpace :: Int,
  planCollaborators :: Int,
  planPrivateRepos :: Int
  }
data GitHubRepo = GHRepo {
  updatedAt :: UTCTime,
  pushedAt :: UTCTime,
  forks :: Int,
  cloneUrl :: Url,
  repo_createdAt ::UTCTime,
  description :: String,
  svnUrl :: Url,
  htmlUrl :: Url,
  masterBranch :: String, -- coming soon : GitHubBranch
  fork :: Bool,
  sshUrl :: Url,
  openIssues :: Int,
  language :: String,
  private :: Bool,
  size :: Int,
  owner :: GitHubUser,
  homepage :: String,
  repo_name :: String,
  repoGitUrl :: Url,
  watchers :: Int,
  repo_id :: Int,
  repo_url :: Url  
  } deriving (Show)

data GitHubKey = GHKey {
  key :: String,
  title :: String,
  key_id :: Int
  } deriving (Show)



