module GitHubRepos where

import Control.Monad (liftM)
import GitHubGeneral
import GitHub.Data(GitHubRepo)
import JsonParsing

-- List repositories for the authenticated user
getMyRepos :: AuthTuple -> IO (ParsEither [GitHubRepo])--[String]
getMyRepos auth = liftM jsonToRepos             
                (request "user/repos" [auth])