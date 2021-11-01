module AuthenticationLogic where


import Execute
import qualified Exceptions as Ex
import Types
import Handlers
import App.Database
import Execute.Utils

import Test.Hspec

emulatedLogic :: Spec
emulatedLogic = describe "emulated authentication logic tests" $ do
    authorizedLogic
    adminPermissionsLogic



authorizedLogic :: Spec
authorizedLogic = do
  let def = defaultAuthHandler
      noUser = def {getUserByToken = noUserByToken }
      okUser = def {getUserByToken = userByToken }
  describe "Authorization logic" $ do
    it "throws unauthorized exception if no token supplied" $ do
        (withAuth def defaultLogger Nothing >>= maybeUserToUser def defaultLogger)
            `shouldThrow` unauthorizedSelector
--    it "throws unauthorized exception if no token supplied" $ do
--        (withAuth nouser Nothing >>= getUser nouser)
--            `shouldThrow` unauthorizedSelector
    it "throws unauthorized exception if no user is returned" $ do
        (withAuth noUser defaultLogger (Just $ Token "asdas") >>= maybeUserToUser def defaultLogger)
            `shouldThrow` unauthorizedSelector

    it "returns a user if one with such token exists" $ do
        (withAuth okUser defaultLogger (Just $ Token "token1") >>= maybeUserToUser def defaultLogger)
            `shouldReturn` defaultUser
    it "throws undefined on undefined token" $ do
        (withAuth def defaultLogger undefined >>= maybeUserToUser def defaultLogger)
            `shouldThrow` anyErrorCall



adminPermissionsLogic :: Spec
adminPermissionsLogic = do
  let def = defaultAuthHandler
  describe "Permissions logic" $ do
    it "throws forbidden exception if no token supplied" $ do
        withAuthAdmin def defaultLogger Nothing
            `shouldThrow` forbiddenSelector
    it "throws forbidden exception if no user found with this token" $ do
        withAuthAdmin def { getUserByToken = noUserByToken } defaultLogger (Just $ Token "asdasdasda")
            `shouldThrow` forbiddenSelector
    it "throws forbidden exception if user is not admin" $ do
        withAuthAdmin def { getUserByToken = notAdminByToken } defaultLogger (Just $ Token "asdasdasda")
            `shouldThrow` forbiddenSelector
    it "returns unit-type value if user is admin" $ do
        withAuthAdmin def { getUserByToken = adminByToken } defaultLogger (Just $ Token "asdasdasda")
            `shouldReturn` ()

    it "throws undefined on undefined token" $ do
        withAuthAdmin def { getUserByToken = noUserByToken } defaultLogger undefined
            `shouldThrow` anyErrorCall

forbiddenSelector = serverErrorSelector Ex.Forbidden
unauthorizedSelector = serverErrorSelector Ex.Unauthorized

serverErrorSelector :: Ex.ServerException -> Selector Ex.ServerException
serverErrorSelector e = (== e)


